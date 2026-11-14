use rue_parser::{SyntaxKind, T};

use crate::{
    FormatError, FormatOptions,
    analysis::{DelimiterStyle, ItemBoundary, Layout},
    document::Doc,
    ordering::{CompactGroup, DocumentItem, ImportGroupPlan},
    renderer::render,
    token_stream::{Comment, CommentPlacement, Gap, TokenId, TokenSpan, TokenStream},
    trivia::split_between,
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
    /// The separator that's still needed after the comments in `gap`, which end with a line break
    /// or a blank line of their own.
    fn after(self, gap: &Gap) -> Self {
        if !gap.ends_line() {
            return self;
        }
        match self {
            // A gap whose comments are followed by a blank line already provides it
            Self::Empty if gap.newlines <= 1 => Self::Hard,
            _ => Self::None,
        }
    }
}

pub struct Formatter<'a> {
    stream: &'a TokenStream,
    layout: Layout,
    options: &'a FormatOptions,
    consumed_tokens: usize,
    consumed_comments: usize,
}

impl<'a> Formatter<'a> {
    pub fn new(stream: &'a TokenStream, layout: Layout, options: &'a FormatOptions) -> Self {
        Self {
            stream,
            layout,
            options,
            consumed_tokens: 0,
            consumed_comments: 0,
        }
    }

    pub fn format(mut self) -> Result<Doc, FormatError> {
        let items = self.layout.document.items.clone();
        let header = self.layout.document.header.clone();
        let footer = self.layout.document.footer.clone();
        let mut docs = vec![self.gap_doc(&header, Separator::None)];

        // Sorting can move the comments around a blank line that ended an import group into the
        // middle of one, where the blank line would split the group on the next format
        let same_import_group = |left: &DocumentItem, right: &DocumentItem| {
            left.import_group.is_some() && left.import_group == right.import_group
        };
        let trailing_gaps = items
            .iter()
            .enumerate()
            .map(|(position, item)| {
                let mut trailing = item.trailing.clone();
                if items
                    .get(position + 1)
                    .is_some_and(|next| same_import_group(item, next))
                {
                    remove_blank_lines(&mut trailing);
                }
                trailing
            })
            .collect::<Vec<_>>();

        let mut formatted_items = Vec::with_capacity(items.len());
        for (item, trailing) in items.iter().zip(&trailing_gaps) {
            formatted_items.push(self.document_item(item, trailing)?);
        }

        for (position, item) in items.iter().enumerate() {
            let separator = if position == 0 {
                Separator::None
            } else {
                let previous = &items[position - 1];
                let previous_group = formatted_items[position - 1].compact_group;
                let requested = match (previous.import_group, item.import_group) {
                    (Some(left), Some(right)) if left == right => Separator::Hard,
                    (None, None)
                        if previous_group.is_some()
                            && previous_group == formatted_items[position].compact_group
                            && (item.span.start().index() == 0
                                || self.stream.gap_before(item.span.start()).newlines <= 1) =>
                    {
                        Separator::Hard
                    }
                    _ => Separator::Empty,
                };
                requested.after(&trailing_gaps[position - 1])
            };
            let mut leading = item.leading.clone();
            if position > 0 && same_import_group(&items[position - 1], item) {
                remove_blank_lines(&mut leading);
            }
            if let Some(first) = leading.comments.first_mut() {
                if position == 0 {
                    first.newlines_before = 0;
                    first.placement = CommentPlacement::Leading;
                } else if trailing_gaps[position - 1].ends_line() {
                    // The previous item's trailing comments already end its line. They're only
                    // followed by these comments in the source if the items weren't sorted, and
                    // then the line break between them was already given to the trailing comments.
                    first.newlines_before = first.newlines_before.saturating_sub(1);
                }
            }
            docs.push(self.movable_leading_doc(&leading, separator));
            docs.push(formatted_items[position].item.clone());
            docs.push(formatted_items[position].trailing.clone());
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

    /// Formats a top-level item and the comments after it on the same line, and works out which
    /// compact group it's in. A constant or type alias is only in one if it's formatted on one
    /// line, not counting its attributes, which are always on lines of their own. This is decided
    /// from the formatted item rather than the source, so that formatting it again gives the same
    /// result.
    fn document_item(
        &mut self,
        item: &DocumentItem,
        trailing: &Gap,
    ) -> Result<FormattedItem, FormatError> {
        if !matches!(
            item.compact_group,
            Some(CompactGroup::Constant | CompactGroup::TypeAlias)
        ) {
            return Ok(FormattedItem {
                item: self.span(item.span)?,
                trailing: self.gap_doc(trailing, Separator::None),
                compact_group: item.compact_group,
            });
        }

        let attributes = if item.body_start == item.span.start() {
            Doc::Nil
        } else {
            let last_attribute_token =
                self.stream.previous_token(item.body_start).ok_or_else(|| {
                    FormatError::Internal("item body has no preceding attribute token".to_string())
                })?;
            Doc::concat([
                self.span(self.stream.span(
                    item.span.start(),
                    self.stream.boundary_before(item.body_start),
                )?)?,
                self.gap_between(last_attribute_token, item.body_start),
            ])
        };
        let body = self.span(self.stream.span(item.body_start, item.span.end())?)?;
        let trailing = self.gap_doc(trailing, Separator::None);
        // Trailing comments can make the item break, so they're rendered with it
        let single_line = !render(&Doc::concat([body.clone(), trailing.clone()]), self.options)
            .trim_end()
            .contains('\n');
        Ok(FormattedItem {
            item: Doc::concat([attributes, body]),
            trailing,
            compact_group: item.compact_group.filter(|_| single_line),
        })
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
                    docs.push(self.gap_between(atom_end, index));
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
        let delimiter_separator = match style {
            DelimiterStyle::Block => Separator::Hard,
            DelimiterStyle::Braced | DelimiterStyle::ConditionalBraced => Separator::Soft,
            DelimiterStyle::Group
            | DelimiterStyle::Fill
            | DelimiterStyle::FillBraced
            | DelimiterStyle::Hug
            | DelimiterStyle::Vertical => Separator::None,
        };
        let comma = if source_trailing_comma {
            let gap = self.stream.gap(inner_end);
            let comment_ends_line = gap.comments.is_empty() || gap.ends_line();
            // An inline comment before the comma is followed by the comma when the list is broken,
            // and otherwise by what follows a comment before the closing delimiter
            let leading = self.gap_doc_with_final_line(gap, Separator::None, comment_ends_line);
            let without_comma = if !comment_ends_line && delimiter_separator == Separator::None {
                Doc::space()
            } else {
                Doc::Nil
            };
            self.consumed_tokens += 1;
            Doc::concat([
                leading,
                if suppress_broken_comma {
                    without_comma
                } else {
                    Doc::if_break(Doc::text(","), without_comma)
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
            // A blank line after trailing comments is only kept before leading comments, as in
            // `split_between`, which sorting can move away
            let mut trailing = item.trailing.clone();
            if plan
                .items
                .get(position + 1)
                .is_some_and(|next| next.leading.comments.is_empty())
            {
                trailing.newlines = trailing.newlines.min(1);
            }
            docs.push(self.gap_doc_with_final_line(
                &trailing,
                Separator::None,
                !suppress_final_line,
            ));
        }
        docs.push(self.gap_doc_with_final_line(&plan.closing, Separator::None, false));
        // The line after a comment that ends the last item's line is left out above, so that it's
        // emitted here outside the indentation, but it's still needed when the group would
        // otherwise fit on one line
        let closing_line = if plan
            .items
            .last()
            .is_some_and(|item| item.trailing.ends_line())
        {
            Doc::hard_line()
        } else {
            Doc::if_break(Doc::hard_line(), Doc::Nil)
        };
        Ok(Doc::concat([
            open,
            Doc::concat([opening, before_items, Doc::concat(docs)]).indent(),
            closing_line,
            close,
        ])
        .group())
    }

    fn token(&mut self, id: TokenId) -> Doc {
        self.consumed_tokens += 1;
        Doc::text(&*self.stream.token(id).text)
    }

    fn separator(&self, previous: TokenId, next: TokenId) -> Separator {
        let left = self.stream.token(previous);
        let right = self.stream.token(next);
        if let Some(boundary) = self.layout.facts(previous).item_boundary {
            return match boundary {
                ItemBoundary::Document | ItemBoundary::ModuleInnerAttributes => Separator::Empty,
                ItemBoundary::Module | ItemBoundary::Block => Separator::Hard,
            };
        }
        if left.kind == T![;] || self.layout.facts(previous).is_attribute_end() {
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

    /// The separator and comments between two adjacent tokens.
    fn gap_between(&mut self, previous: TokenId, next: TokenId) -> Doc {
        let separator = self.separator(previous, next);
        let gap = self.stream.gap_before(next);
        if separator == Separator::Empty {
            self.item_gap_doc(gap)
        } else {
            self.gap_doc(gap, separator)
        }
    }

    /// Separates two items with a blank line, keeping comments on the same line as the first item
    /// attached to it, the same way as items in a document.
    fn item_gap_doc(&mut self, gap: &Gap) -> Doc {
        let (trailing, leading) = split_between(gap);
        Doc::concat([
            self.gap_doc_with_final_line(&trailing, Separator::None, false),
            self.movable_leading_doc(&leading, Separator::Empty),
        ])
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
        } else if last.kind == SyntaxKind::LineComment
            || last.multiline
            || gap.newlines > 0
            || requested == Separator::Hard
        {
            // A block's closing brace is always on its own line, even after an inline comment
            Doc::hard_line()
        } else if requested == Separator::Soft {
            // Braces that fit on one line keep the space before the closing brace, and otherwise
            // it goes on its own line
            Doc::soft_line()
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
        let mut spaced = false;
        for (index, comment) in gap.comments.iter().enumerate() {
            self.consumed_comments += 1;
            let before = if index == 0
                && requested == Separator::Empty
                && comment.newlines_before == 1
                && gap.newlines <= 1
            {
                // Comments directly before an item that's separated from the previous one by a
                // blank line belong to it, so they go after the blank line
                Doc::empty_line()
            } else if index == 0 {
                comment_separator(comment, requested, ignore_trailing)
            } else if spaced {
                // The previous comment is already followed by a space on the same line
                Doc::Nil
            } else {
                comment_separator(comment, Separator::Hard, false)
            };
            docs.push(before);
            docs.push(Doc::text(&*comment.text));
            let followed_inline = match gap.comments.get(index + 1) {
                Some(next) => next.newlines_before == 0,
                None => gap.newlines == 0 && !suppress_final_line,
            };
            spaced =
                comment.kind == SyntaxKind::BlockComment && !comment.multiline && followed_inline;
            if spaced {
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

struct FormattedItem {
    item: Doc,
    trailing: Doc,
    compact_group: Option<CompactGroup>,
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

/// Limits the line breaks in a gap to one, so it has no blank lines.
fn remove_blank_lines(gap: &mut Gap) {
    gap.newlines = gap.newlines.min(1);
    for comment in &mut gap.comments {
        comment.newlines_before = comment.newlines_before.min(1);
    }
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
    if comment.newlines_before > 0
        || (comment.multiline && comment.placement == CommentPlacement::Trailing)
    {
        return Doc::hard_line();
    }
    separator_doc(requested)
}

fn no_space_after(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        T!['('] | T!['['] | T![::] | T![.] | T![...] | T![,] | T![#]
    )
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
