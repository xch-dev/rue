use std::{collections::HashMap, fmt::Write};

use rue_ast::{AstDocument, AstFunctionItem, AstItem, AstNode};
use rue_parser::{SyntaxKind, SyntaxNode, T};

use crate::{
    FormatError,
    token_stream::{Gap, TokenId, TokenSpan, TokenStream, significant_tokens},
    trivia::{split_between, split_file_header, split_group_opening},
};

#[derive(Debug, Clone)]
pub struct DocumentItem {
    pub span: TokenSpan,
    pub import_group: Option<usize>,
    pub compact_group: Option<CompactGroup>,
    pub leading: Gap,
    pub trailing: Gap,
    pub identity_key: String,
    inner_attributes: bool,
    sort_key: String,
    original_index: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompactGroup {
    Constant,
    TypeAlias,
    ExternDeclaration,
}

#[derive(Debug)]
pub struct DocumentPlan {
    pub header: Gap,
    pub items: Vec<DocumentItem>,
    pub footer: Gap,
}

#[derive(Debug, Clone)]
pub struct ImportPathItem {
    pub span: TokenSpan,
    pub comma: Option<TokenId>,
    pub leading: Gap,
    pub before_comma: Gap,
    pub trailing: Gap,
    pub identity_key: String,
    sort_key: String,
    original_index: usize,
}

#[derive(Debug, Clone)]
pub struct ImportGroupPlan {
    pub opening: Gap,
    pub items: Vec<ImportPathItem>,
    pub closing: Gap,
}

pub fn plan_document(
    document: &AstDocument,
    stream: &TokenStream,
) -> Result<DocumentPlan, FormatError> {
    let mut items = Vec::new();
    let mut next_import_group = 0;
    let mut previous_was_import = false;

    // The inner attributes at the start of the file are kept together as a single unit, which
    // always stays first so that items (such as imports) aren't sorted above them
    let inner_attributes = document.inner_attributes().collect::<Vec<_>>();
    if let (Some(first), Some(last)) = (inner_attributes.first(), inner_attributes.last()) {
        let start = node_span(first.syntax(), stream)?.start();
        let last = significant_tokens(last.syntax()).last().ok_or_else(|| {
            FormatError::Internal("inner attribute has no significant tokens".to_string())
        })?;
        let last = stream.token_id_at_offset(usize::from(last.text_range().start()))?;
        items.push(DocumentItem {
            span: stream.span_through(start, last)?,
            import_group: None,
            compact_group: None,
            leading: Gap::default(),
            trailing: Gap::default(),
            identity_key: inner_attributes
                .iter()
                .map(|attribute| canonical_node_key(attribute.syntax()))
                .collect(),
            inner_attributes: true,
            sort_key: String::new(),
            original_index: 0,
        });
    }

    for (original_index, item) in document.items().enumerate() {
        let span = item_span(item.syntax(), stream)?;
        // An import with a misplaced inner attribute isn't sorted, since moving it to the start of
        // the file would turn the attribute into one that applies to the whole file
        let is_import = matches!(&item, AstItem::ImportItem(_))
            && !item.attributes().any(|attribute| attribute.is_inner());
        let import_group = is_import.then(|| {
            if !previous_was_import
                || (span.start().index() > 0 && gap_has_blank_line(stream.gap_before(span.start())))
            {
                next_import_group += 1;
            }
            next_import_group
        });
        previous_was_import = is_import;

        let compact_group = match item.syntax().kind() {
            SyntaxKind::ConstantItem if is_single_line(&item) => Some(CompactGroup::Constant),
            SyntaxKind::TypeAliasItem if is_single_line(&item) => Some(CompactGroup::TypeAlias),
            SyntaxKind::FunctionItem
                if AstFunctionItem::cast(item.syntax().clone()).is_some_and(|function| {
                    function.extern_kw().is_some() && function.body().is_none()
                }) =>
            {
                Some(CompactGroup::ExternDeclaration)
            }
            _ => None,
        };
        // Attributes don't affect the order of items, so they're left out of the sort key
        let key_tokens = item
            .syntax()
            .descendants_with_tokens()
            .filter_map(rowan::NodeOrToken::into_token)
            .filter(|token| {
                !token.kind().is_trivia()
                    && !token
                        .parent_ancestors()
                        .any(|node| node.kind() == SyntaxKind::Attribute)
            })
            .collect::<Vec<_>>();
        let path_key = key_tokens
            .iter()
            .filter(|token| !matches!(token.kind(), T![import] | T![export] | T![;]))
            .map(rue_parser::SyntaxToken::text)
            .collect::<String>();
        let keyword_key = key_tokens.first().map_or("", rue_parser::SyntaxToken::text);

        items.push(DocumentItem {
            span,
            import_group,
            compact_group,
            leading: Gap::default(),
            trailing: Gap::default(),
            identity_key: canonical_node_key(item.syntax()),
            inner_attributes: false,
            sort_key: format!("{path_key}\0{keyword_key}"),
            original_index,
        });
    }

    if items.is_empty() {
        return Ok(DocumentPlan {
            header: stream.first_gap().clone(),
            items,
            footer: Gap::default(),
        });
    }

    let (header, first_leading) = split_file_header(stream.gap_before(items[0].span.start()));
    items[0].leading = first_leading;
    for index in 1..items.len() {
        let (trailing, leading) = split_between(stream.gap_before(items[index].span.start()));
        items[index - 1].trailing = trailing;
        items[index].leading = leading;
    }
    let last = items
        .last_mut()
        .expect("non-empty document plan has a final item");
    let (trailing, footer) = split_between(stream.final_gap());
    last.trailing = trailing;

    items.sort_by(|left, right| {
        right
            .inner_attributes
            .cmp(&left.inner_attributes)
            .then_with(|| match (left.import_group, right.import_group) {
                (Some(left_group), Some(right_group)) => left_group
                    .cmp(&right_group)
                    .then_with(|| left.sort_key.cmp(&right.sort_key))
                    .then_with(|| left.original_index.cmp(&right.original_index)),
                (Some(_), None) => std::cmp::Ordering::Less,
                (None, Some(_)) => std::cmp::Ordering::Greater,
                (None, None) => left.original_index.cmp(&right.original_index),
            })
    });

    Ok(DocumentPlan {
        header,
        items,
        footer,
    })
}

pub fn plan_import_groups(
    root: &SyntaxNode,
    stream: &TokenStream,
) -> Result<HashMap<TokenId, ImportGroupPlan>, FormatError> {
    let mut groups = HashMap::new();
    for node in root
        .descendants()
        .filter(|node| node.kind() == SyntaxKind::ImportPathSegment)
    {
        let Some(open_token) = node
            .children_with_tokens()
            .filter_map(rowan::NodeOrToken::into_token)
            .find(|token| token.kind() == T!['{'])
        else {
            continue;
        };
        let open = stream.token_id_at_offset(usize::from(open_token.text_range().start()))?;
        let close = stream.matching_brace(open)?;
        let paths: Vec<_> = node
            .children()
            .filter(|child| child.kind() == SyntaxKind::ImportPath)
            .collect();
        if paths.is_empty() {
            continue;
        }

        let mut items = Vec::new();
        for (original_index, path) in paths.iter().enumerate() {
            let span = node_span(path, stream)?;
            let comma = stream
                .token_at(span.end())
                .filter(|id| stream.token(*id).kind == T![,]);
            let sort_key = stream
                .tokens_in(span)
                .iter()
                .map(|token| token.text.as_str())
                .collect();
            let before_comma = comma
                .map(|comma| stream.gap_before(comma).clone())
                .unwrap_or_default();
            items.push(ImportPathItem {
                span,
                comma,
                leading: Gap::default(),
                before_comma,
                trailing: Gap::default(),
                identity_key: canonical_node_key(path),
                sort_key,
                original_index,
            });
        }

        let (opening, first_leading) =
            split_group_opening(stream.gap_before(items[0].span.start()));
        items[0].leading = first_leading;
        for index in 1..items.len() {
            let boundary = stream.gap_before(items[index].span.start());
            let (trailing, leading) = split_between(boundary);
            items[index - 1].trailing = trailing;
            items[index].leading = leading;
        }
        let final_boundary = stream.gap_before(close);
        let (trailing, closing) = split_between(final_boundary);
        items
            .last_mut()
            .expect("non-empty import group has a final path")
            .trailing = trailing;

        items.sort_by(|left, right| {
            left.sort_key
                .cmp(&right.sort_key)
                .then_with(|| left.original_index.cmp(&right.original_index))
        });
        groups.insert(
            open,
            ImportGroupPlan {
                opening,
                items,
                closing: closing.dangling(),
            },
        );
    }
    Ok(groups)
}

fn item_span(node: &SyntaxNode, stream: &TokenStream) -> Result<TokenSpan, FormatError> {
    let span = node_span(node, stream)?;
    let Some(semicolon) = stream
        .token_at(span.end())
        .filter(|id| stream.token(*id).kind == T![;])
    else {
        return Ok(span);
    };
    stream.span_through(span.start(), semicolon)
}

fn node_span(node: &SyntaxNode, stream: &TokenStream) -> Result<TokenSpan, FormatError> {
    let mut tokens = significant_tokens(node);
    let first = tokens.next().ok_or_else(|| {
        FormatError::Internal(format!("{:?} node has no significant tokens", node.kind()))
    })?;
    let last = tokens.last().unwrap_or_else(|| first.clone());
    let start = stream.token_id_at_offset(usize::from(first.text_range().start()))?;
    let last = stream.token_id_at_offset(usize::from(last.text_range().start()))?;
    stream.span_through(start, last)
}

/// Whether an item is on a single line, not counting its attributes, which are always formatted
/// on their own lines.
fn is_single_line(item: &AstItem) -> bool {
    !item
        .syntax()
        .children_with_tokens()
        .skip_while(|element| element.kind().is_trivia() || element.kind() == SyntaxKind::Attribute)
        .any(|element| element.to_string().contains('\n'))
}

fn gap_has_blank_line(gap: &crate::token_stream::Gap) -> bool {
    gap.newlines > 1
        || gap
            .comments
            .iter()
            .any(|comment| comment.newlines_before > 1)
}

fn canonical_node_key(node: &SyntaxNode) -> String {
    let mut key = format!("n:{:?}[", node.kind());
    let mut sorted_paths = if node.kind() == SyntaxKind::ImportPathSegment {
        let mut paths: Vec<_> = node
            .children()
            .filter(|child| child.kind() == SyntaxKind::ImportPath)
            .map(|child| canonical_node_key(&child))
            .collect();
        paths.sort();
        paths.into_iter()
    } else {
        Vec::new().into_iter()
    };

    for element in node.children_with_tokens() {
        match element {
            rowan::NodeOrToken::Node(child)
                if node.kind() == SyntaxKind::ImportPathSegment
                    && child.kind() == SyntaxKind::ImportPath =>
            {
                key.push_str(
                    &sorted_paths
                        .next()
                        .expect("each import path has a canonical identity"),
                );
            }
            rowan::NodeOrToken::Node(child) => key.push_str(&canonical_node_key(&child)),
            rowan::NodeOrToken::Token(token)
                if !token.kind().is_trivia() && token.kind() != T![,] =>
            {
                write!(&mut key, "t:{:?}:{};", token.kind(), token.text())
                    .expect("writing to a String cannot fail");
            }
            rowan::NodeOrToken::Token(_) => {}
        }
    }
    key.push(']');
    key
}
