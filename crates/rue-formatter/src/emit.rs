use rue_parser::{SyntaxKind, T};

use crate::{
    FormatError,
    analysis::{DelimiterStyle, ItemBoundary, Layout},
    document::Doc,
    ordering::ImportGroupPlan,
    token_stream::{Comment, CommentPlacement, Gap, TokenId, TokenSpan, TokenStream},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Separator {
    None,
    Space,
    Soft,
    Hard,
    Empty,
}

impl Separator {
    fn after(self, gap: &Gap) -> Self {
        if !gap.ends_line() {
            return self;
        }
        match self {
            Self::Empty => Self::Hard,
            _ => Self::None,
        }
    }
}

pub struct Formatter<'a> {
    stream: &'a TokenStream,
    layout: Layout,
    consumed_tokens: usize,
    consumed_comments: usize,
}

impl<'a> Formatter<'a> {
    pub fn new(stream: &'a TokenStream, layout: Layout) -> Self {
        Self {
            stream,
            layout,
            consumed_tokens: 0,
            consumed_comments: 0,
        }
    }

    pub fn format(mut self) -> Result<Doc, FormatError> {
        let items = self.layout.document.items.clone();
        let header = self.layout.document.header.clone();
        let footer = self.layout.document.footer.clone();
        let mut docs = vec![self.gap_doc(&header, Separator::None)];

        for (position, item) in items.iter().enumerate() {
            let separator = if position == 0 {
                Separator::None
            } else {
                let previous = &items[position - 1];
                let requested = match (previous.import_group, item.import_group) {
                    (Some(left), Some(right)) if left == right => Separator::Hard,
                    (None, None)
                        if previous.compact_group.is_some()
                            && previous.compact_group == item.compact_group
                            && (item.span.start().index() == 0
                                || self.stream.gap_before(item.span.start()).newlines <= 1) =>
                    {
                        Separator::Hard
                    }
                    _ => Separator::Empty,
                };
                requested.after(&previous.trailing)
            };
            let mut leading = item.leading.clone();
            if position == 0
                && let Some(first) = leading.comments.first_mut()
            {
                first.newlines_before = 0;
                first.placement = CommentPlacement::Leading;
            }
            docs.push(self.movable_leading_doc(&leading, separator));
            docs.push(self.span(item.span)?);
            docs.push(self.gap_doc(&item.trailing, Separator::None));
        }
        docs.push(self.gap_doc(&footer, Separator::None));

        if self.consumed_tokens != self.stream.len() {
            return Err(FormatError::Internal(format!(
                "emitted {} of {} significant tokens",
                self.consumed_tokens,
                self.stream.len()
            )));
        }
        if self.consumed_comments != self.stream.comment_count {
            return Err(FormatError::Internal(format!(
                "emitted {} of {} comments",
                self.consumed_comments, self.stream.comment_count
            )));
        }
        Ok(Doc::concat(docs))
    }

    fn span(&mut self, span: TokenSpan) -> Result<Doc, FormatError> {
        self.span_inner(span, true)
    }

    fn span_inner(&mut self, span: TokenSpan, allow_outer_group: bool) -> Result<Doc, FormatError> {
        let exact_group = self
            .layout
            .facts(span.start())
            .group_end
            .is_some_and(|end| self.stream.boundary_after(end) == span.end());
        if exact_group {
            if !self
                .layout
                .facts(span.start())
                .continuation_operators
                .is_empty()
            {
                return Ok(self.binary_span(span)?.group());
            }
            if allow_outer_group && self.layout.facts(span.start()).is_conditional_group() {
                return Ok(self.span_inner(span, false)?.group());
            }
            if allow_outer_group {
                return self.grouped_span(span);
            }
        }

        let mut docs = Vec::new();
        let mut index = span.start();
        while index.index() < span.end().index() {
            let mut emitted_separator = false;
            let facts = self.layout.facts(index);
            let atom_end = if let Some(group_end) = facts.group_end
                && group_end.index() < span.end().index()
                && !(index == span.start() && self.stream.boundary_after(group_end) == span.end())
            {
                let next = self.stream.next_token(group_end).ok_or_else(|| {
                    FormatError::Internal(
                        "group end has no following token inside formatting span".to_string(),
                    )
                })?;
                let gap = self.stream.gap_before(next);
                if !facts.continuation_operators.is_empty()
                    && self
                        .layout
                        .facts(next)
                        .delimiter_style
                        .is_some_and(|style| style == DelimiterStyle::Block)
                    && gap.comments.is_empty()
                    && gap.newlines <= 1
                {
                    docs.push(
                        Doc::concat([
                            self.binary_span(
                                self.stream.span(index, self.stream.boundary_before(next))?,
                            )?,
                            Doc::if_break(Doc::hard_line(), Doc::space()),
                        ])
                        .group(),
                    );
                    emitted_separator = true;
                } else {
                    docs.push(self.grouped_span(
                        self.stream.span(index, self.stream.boundary_before(next))?,
                    )?);
                }
                group_end
            } else if let Some(close) = facts.pair {
                if close.index() >= span.end().index() {
                    return Err(FormatError::Internal(
                        "delimiter pair crosses formatting span".to_string(),
                    ));
                }
                docs.push(self.delimited(index, close)?);
                close
            } else {
                docs.push(self.token(index));
                index
            };

            let next_boundary = self.stream.boundary_after(atom_end);
            if next_boundary.index() < span.end().index() {
                index = self.stream.token_at(next_boundary).ok_or_else(|| {
                    FormatError::Internal(
                        "formatting span contains a non-token boundary".to_string(),
                    )
                })?;
                if !emitted_separator {
                    let separator = self.separator(atom_end, index);
                    docs.push(self.gap_doc(self.stream.gap_before(index), separator));
                }
            } else {
                break;
            }
        }
        Ok(Doc::concat(docs))
    }

    fn grouped_span(&mut self, span: TokenSpan) -> Result<Doc, FormatError> {
        if self.layout.facts(span.start()).is_conditional_group() {
            return Ok(self.span_inner(span, false)?.group());
        }
        if !self
            .layout
            .facts(span.start())
            .continuation_operators
            .is_empty()
        {
            return Ok(self.binary_span(span)?.group());
        }
        Ok(self.span_inner(span, false)?.indent().group())
    }

    fn binary_span(&mut self, span: TokenSpan) -> Result<Doc, FormatError> {
        let operators = self
            .layout
            .facts(span.start())
            .continuation_operators
            .clone();
        let Some(first_operator) = operators.first() else {
            return self.span_inner(span, false);
        };

        let mut docs = vec![self.span_inner(
            self.stream.span(
                span.start(),
                self.stream.boundary_before(first_operator.token),
            )?,
            false,
        )?];
        let mut segments = Vec::with_capacity(operators.len());
        for (position, operator) in operators.iter().enumerate() {
            let operand_start = self.stream.next_token(operator.token).ok_or_else(|| {
                FormatError::Internal("binary operator has no operand token".to_string())
            })?;
            let segment_end = operators
                .get(position + 1)
                .map_or(span.end(), |next| self.stream.boundary_before(next.token));
            let operator_doc = self.token(operator.token);
            let after_operator =
                self.gap_doc(self.stream.gap_before(operand_start), Separator::Space);
            let operand = self.span_inner(self.stream.span(operand_start, segment_end)?, false)?;
            let segment = Doc::concat([operator_doc, after_operator, operand]);
            let fill = is_comparison_operator(self.stream.token(operator.token).kind)
                && self.stream.gap_before(operator.token).comments.is_empty();
            let before_operator = if fill {
                Doc::Nil
            } else {
                self.gap_doc(self.stream.gap_before(operator.token), Separator::Soft)
            };
            segments.push(BinarySegment {
                depth: operator.depth,
                fill,
                before_operator,
                segment,
            });
        }
        binary_segments(&segments, 0..segments.len(), 0, 0, &mut docs);
        Ok(Doc::concat(docs))
    }

    fn delimited(&mut self, open: TokenId, close: TokenId) -> Result<Doc, FormatError> {
        let facts = self.layout.facts(open);
        let style = facts.delimiter_style.unwrap_or(DelimiterStyle::Group);
        let supports_trailing_comma = facts.supports_trailing_comma();
        let open_doc = self.token(open);
        let close_doc = self.token(close);
        let inner_start = self.stream.next_token(open).ok_or_else(|| {
            FormatError::Internal("opening delimiter has no following token".to_string())
        })?;

        if inner_start == close {
            let gap = self.gap_doc_with_comments(
                self.stream.gap_before(inner_start),
                if self.stream.gap_before(inner_start).comments.is_empty() {
                    Separator::None
                } else if style == DelimiterStyle::Block {
                    Separator::Hard
                } else {
                    Separator::Soft
                },
                true,
                false,
            );
            return Ok(Doc::concat([open_doc, gap, close_doc]).group());
        }

        if let Some(plan) = self.layout.import_groups.get(&open).cloned() {
            return self.import_group(open_doc, close_doc, &plan);
        }

        let previous = self.stream.previous_token(close).ok_or_else(|| {
            FormatError::Internal("closing delimiter has no preceding token".to_string())
        })?;
        let source_trailing_comma =
            supports_trailing_comma && self.stream.token(previous).kind == T![,];
        let suppress_broken_comma = style == DelimiterStyle::Hug;
        let inner_end = if source_trailing_comma {
            self.stream.boundary_before(previous)
        } else {
            self.stream.boundary_before(close)
        };
        let inner = self.span(self.stream.span(inner_start, inner_end)?)?;
        let comma = if source_trailing_comma {
            let leading = self.gap_doc(self.stream.gap(inner_end), Separator::None);
            self.consumed_tokens += 1;
            Doc::concat([
                leading,
                if suppress_broken_comma {
                    Doc::Nil
                } else {
                    Doc::if_break(Doc::text(","), Doc::Nil)
                },
            ])
        } else if supports_trailing_comma && !suppress_broken_comma {
            Doc::if_break(Doc::text(","), Doc::Nil)
        } else {
            Doc::Nil
        };
        let leading_gap = self.stream.gap_before(inner_start);
        let trailing_gap = self.stream.gap_before(close);
        let leading_comment_starts_line = leading_gap
            .comments
            .first()
            .is_some_and(|comment| comment.newlines_before > 0 || comment.multiline);
        let trailing_comment_ends_line = trailing_gap.comments.last().is_some_and(|comment| {
            comment.kind == SyntaxKind::LineComment
                || comment.multiline
                || trailing_gap.newlines > 0
        });
        let delimiter_separator = match style {
            DelimiterStyle::Block => Separator::Hard,
            DelimiterStyle::Braced | DelimiterStyle::ConditionalBraced => Separator::Soft,
            DelimiterStyle::Group
            | DelimiterStyle::Fill
            | DelimiterStyle::FillBraced
            | DelimiterStyle::Hug
            | DelimiterStyle::Vertical => Separator::None,
        };
        let leading = if leading_gap.comments.is_empty() {
            separator_doc(delimiter_separator)
        } else {
            self.gap_doc_with_comments(leading_gap, delimiter_separator, true, false)
        };
        // Comments before the closing delimiter belong to the contents, so they're
        // indented with them; only the final line break is at the delimiter's level.
        let (trailing_comments, trailing) = if trailing_gap.comments.is_empty() {
            (Doc::Nil, separator_doc(delimiter_separator))
        } else {
            self.closing_gap_docs(trailing_gap, delimiter_separator)
        };

        Ok(match style {
            DelimiterStyle::Block | DelimiterStyle::Braced | DelimiterStyle::ConditionalBraced => {
                Doc::concat([
                    open_doc,
                    Doc::concat([leading, inner, comma, trailing_comments]).indent(),
                    trailing,
                    close_doc,
                ])
                .group_if(style == DelimiterStyle::Braced)
            }
            DelimiterStyle::Fill | DelimiterStyle::FillBraced | DelimiterStyle::Hug => {
                let space_inside = style == DelimiterStyle::FillBraced;
                let closing_space = if space_inside { Doc::space() } else { Doc::Nil };
                let flat_inner = Doc::concat([
                    leading.clone(),
                    inner.clone(),
                    trailing_comments.clone(),
                    trailing.clone(),
                    closing_space,
                    close_doc.clone(),
                ]);
                let broken_inner = Doc::concat([
                    leading,
                    inner,
                    comma,
                    trailing_comments,
                    trailing,
                    Doc::concat([Doc::hard_line(), close_doc]).outdent(),
                ]);
                Doc::concat([
                    open_doc,
                    Doc::fill_choice(flat_inner, broken_inner, space_inside, 1),
                ])
                .group()
            }
            DelimiterStyle::Group | DelimiterStyle::Vertical => Doc::concat([
                open_doc,
                Doc::concat([
                    if leading_comment_starts_line {
                        Doc::Nil
                    } else {
                        Doc::if_break(Doc::hard_line(), Doc::Nil)
                    },
                    leading,
                    inner,
                    comma,
                    trailing_comments,
                ])
                .indent(),
                trailing,
                if trailing_comment_ends_line {
                    Doc::Nil
                } else {
                    Doc::if_break(Doc::hard_line(), Doc::Nil)
                },
                close_doc,
            ])
            .group(),
        })
    }

    fn import_group(
        &mut self,
        open: Doc,
        close: Doc,
        plan: &ImportGroupPlan,
    ) -> Result<Doc, FormatError> {
        let opening = self.gap_doc(&plan.opening, Separator::None);
        let before_items = if plan.opening.ends_line() {
            Doc::Nil
        } else {
            Doc::if_break(Doc::hard_line(), Doc::Nil)
        };
        let mut docs = Vec::new();
        for (position, item) in plan.items.iter().enumerate() {
            let mut leading = item.leading.clone();
            if position == 0
                && let Some(first) = leading.comments.first_mut()
            {
                first.newlines_before = 0;
            }
            let separator = if position == 0 {
                Separator::None
            } else {
                Separator::Soft.after(&plan.items[position - 1].trailing)
            };
            docs.push(self.movable_leading_doc(&leading, separator));
            docs.push(self.span(item.span)?);
            docs.push(self.gap_doc(&item.before_comma, Separator::None));
            if item.comma.is_some() {
                self.consumed_tokens += 1;
            }
            if position + 1 < plan.items.len() || !item.before_comma.comments.is_empty() {
                docs.push(Doc::text(","));
            } else {
                docs.push(Doc::if_break(Doc::text(","), Doc::Nil));
            }
            let suppress_final_line = position + 1 == plan.items.len()
                && plan.closing.comments.is_empty()
                && item.trailing.ends_line();
            docs.push(self.gap_doc_with_final_line(
                &item.trailing,
                Separator::None,
                !suppress_final_line,
            ));
        }
        docs.push(self.gap_doc_with_final_line(&plan.closing, Separator::None, false));
        Ok(Doc::concat([
            open,
            Doc::concat([opening, before_items, Doc::concat(docs)]).indent(),
            Doc::if_break(Doc::hard_line(), Doc::Nil),
            close,
        ])
        .group())
    }

    fn token(&mut self, id: TokenId) -> Doc {
        self.consumed_tokens += 1;
        Doc::text(self.stream.token(id).text.clone())
    }

    fn separator(&self, previous: TokenId, next: TokenId) -> Separator {
        let left = self.stream.token(previous);
        let right = self.stream.token(next);
        if let Some(boundary) = self.layout.facts(previous).item_boundary {
            return match boundary {
                ItemBoundary::Document => Separator::Empty,
                ItemBoundary::Module | ItemBoundary::Block => Separator::Hard,
            };
        }
        if left.kind == T![;] {
            return Separator::Hard;
        }
        if left.kind == T![,] {
            return Separator::Soft;
        }
        if right.kind == T![::]
            && self.layout.facts(next).is_absolute_path_start()
            && !no_space_after(left.kind)
        {
            return Separator::Space;
        }
        if no_space_after(left.kind) || no_space_before(right.kind) {
            return Separator::None;
        }
        if SyntaxKind::BINARY_OPS.contains(&left.kind)
            && !self.layout.facts(previous).is_generic()
            && !self.layout.facts(previous).is_prefix_operator()
        {
            return Separator::Soft;
        }
        if right.kind == T!['('] && self.layout.facts(next).is_attached_opener() {
            return Separator::None;
        }
        if left.kind == T!['}'] && right.kind == T![else] {
            return Separator::Space;
        }
        if ((left.kind == T![<]
            || right.kind == T![<]
            || right.kind == T![>]
            || (left.kind == T![>] && right.kind == T!['(']))
            && (self.layout.facts(previous).is_generic() || self.layout.facts(next).is_generic()))
            || (SyntaxKind::PREFIX_OPS.contains(&left.kind)
                && self.layout.facts(previous).is_prefix_operator())
        {
            Separator::None
        } else {
            Separator::Space
        }
    }

    fn movable_leading_doc(&mut self, gap: &Gap, requested: Separator) -> Doc {
        if gap.comments.is_empty() {
            separator_doc(requested)
        } else {
            self.gap_doc(gap, requested)
        }
    }

    /// Splits the gap before a closing delimiter into its comments and the line
    /// break that follows them, so they can be emitted at different indentation.
    fn closing_gap_docs(&mut self, gap: &Gap, requested: Separator) -> (Doc, Doc) {
        let comments = self.gap_doc_with_comments(gap, requested, false, true);
        let last = gap.comments.last().expect("comments are not empty");
        let final_line = if gap.newlines > 1 {
            Doc::empty_line()
        } else if last.kind == SyntaxKind::LineComment || last.multiline || gap.newlines > 0 {
            Doc::hard_line()
        } else {
            // An inline block comment is followed by a space before the delimiter.
            Doc::space()
        };
        (comments, final_line)
    }

    fn gap_doc(&mut self, gap: &Gap, requested: Separator) -> Doc {
        self.gap_doc_with_comments(gap, requested, false, false)
    }

    fn gap_doc_with_final_line(
        &mut self,
        gap: &Gap,
        requested: Separator,
        include_final_line: bool,
    ) -> Doc {
        self.gap_doc_with_comments(gap, requested, false, !include_final_line)
    }

    fn gap_doc_with_comments(
        &mut self,
        gap: &Gap,
        requested: Separator,
        ignore_trailing: bool,
        suppress_final_line: bool,
    ) -> Doc {
        if gap.comments.is_empty() {
            if gap.newlines > 1 && requested != Separator::None {
                return Doc::empty_line();
            }
            return separator_doc(requested);
        }

        let mut docs = Vec::new();
        for (index, comment) in gap.comments.iter().enumerate() {
            self.consumed_comments += 1;
            let before = if index == 0 {
                comment_separator(comment, requested, ignore_trailing)
            } else {
                comment_separator(comment, Separator::Hard, false)
            };
            docs.push(before);
            docs.push(Doc::text(comment.text.clone()));
            let followed_inline = match gap.comments.get(index + 1) {
                Some(next) => next.newlines_before == 0,
                None => gap.newlines == 0 && !suppress_final_line,
            };
            if comment.kind == SyntaxKind::BlockComment && !comment.multiline && followed_inline {
                docs.push(Doc::space());
            }
        }
        let last = gap.comments.last().expect("comments are not empty");
        if suppress_final_line {
            // The enclosing delimiter emits the required line at its own
            // indentation, avoiding spaces before the closing token.
        } else if gap.newlines > 1 {
            docs.push(Doc::empty_line());
        } else if last.kind == SyntaxKind::LineComment || last.multiline || gap.newlines > 0 {
            docs.push(Doc::hard_line());
        }
        Doc::concat(docs)
    }
}

trait GroupIf {
    fn group_if(self, condition: bool) -> Self;
}

impl GroupIf for Doc {
    fn group_if(self, condition: bool) -> Self {
        if condition { self.group() } else { self }
    }
}

struct BinarySegment {
    depth: usize,
    fill: bool,
    before_operator: Doc,
    segment: Doc,
}

/// Emits binary operator segments, indenting each by its precedence tier. Comparison operators
/// are fills that only break when they don't fit, so the operators in their right operand are
/// nested inside the fill and lose that comparison's tier of indentation when it stays flat.
/// Indentation levels are absolute within the binary expression, and `reference` is the level
/// that the emitted docs are already indented to.
fn binary_segments(
    segments: &[BinarySegment],
    range: std::ops::Range<usize>,
    reference: usize,
    flat_comparisons: usize,
    docs: &mut Vec<Doc>,
) {
    let mut index = range.start;
    while index < range.end {
        let segment = &segments[index];
        let level = segment.depth + 1 - flat_comparisons;
        if !segment.fill {
            docs.push(shift_levels(
                Doc::concat([segment.before_operator.clone(), segment.segment.clone()]),
                level,
                reference,
            ));
            index += 1;
            continue;
        }

        // Operators are left associative, so the right operand is exactly the following
        // operators with higher precedence
        let end = (index + 1..range.end)
            .find(|&next| segments[next].depth <= segment.depth)
            .unwrap_or(range.end);
        let flat_level = if index == 0 && segment.depth == 0 {
            0
        } else {
            1usize.saturating_sub(flat_comparisons)
        };
        let mut flat = vec![segment.segment.clone()];
        binary_segments(
            segments,
            index + 1..end,
            flat_level,
            flat_comparisons + 1,
            &mut flat,
        );
        let mut broken = vec![segment.segment.clone()];
        binary_segments(
            segments,
            index + 1..end,
            level,
            flat_comparisons,
            &mut broken,
        );

        // The left operand is exactly the preceding operators with higher precedence. If any of
        // them break with the chain, the comparison breaks too so both operands share a tier
        if segments[..index]
            .iter()
            .rev()
            .take_while(|previous| previous.depth > segment.depth)
            .any(|previous| !previous.fill)
        {
            docs.push(shift_levels(
                Doc::concat([Doc::soft_line(), Doc::concat(broken)]),
                level,
                reference,
            ));
            index = end;
            continue;
        }

        docs.push(shift_levels(
            Doc::fill_choice(
                Doc::concat(flat),
                Doc::concat(broken),
                true,
                level - flat_level,
            ),
            flat_level,
            reference,
        ));
        index = end;
    }
}

fn shift_levels(mut doc: Doc, level: usize, reference: usize) -> Doc {
    if level >= reference {
        return indent_levels(doc, level - reference);
    }
    for _ in level..reference {
        doc = doc.outdent();
    }
    doc
}

fn indent_levels(mut doc: Doc, levels: usize) -> Doc {
    for _ in 0..levels {
        doc = doc.indent();
    }
    doc
}

fn separator_doc(separator: Separator) -> Doc {
    match separator {
        Separator::None => Doc::Nil,
        Separator::Space => Doc::space(),
        Separator::Soft => Doc::soft_line(),
        Separator::Hard => Doc::hard_line(),
        Separator::Empty => Doc::empty_line(),
    }
}

fn comment_separator(comment: &Comment, requested: Separator, ignore_trailing: bool) -> Doc {
    if comment.placement == CommentPlacement::Trailing && !ignore_trailing {
        return Doc::space();
    }
    if comment.newlines_before > 1 {
        return Doc::empty_line();
    }
    if comment.newlines_before > 0 || comment.multiline {
        return Doc::hard_line();
    }
    separator_doc(requested)
}

fn no_space_after(kind: SyntaxKind) -> bool {
    matches!(kind, T!['('] | T!['['] | T![::] | T![.] | T![...] | T![,])
}

fn no_space_before(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        T![')'] | T![']'] | T![,] | T![;] | T![::] | T![.] | T![:]
    )
}

fn is_comparison_operator(kind: SyntaxKind) -> bool {
    matches!(kind, T![==] | T![!=] | T![<] | T![>] | T![<=] | T![>=])
}
