use rue_ast::{AstDocument, AstExpr, AstModuleItem, AstNode, AstNodeKind};
use rue_parser::{SyntaxKind, SyntaxNode, T};

use crate::{
    FormatError,
    ordering::{DocumentPlan, ImportGroupPlan, plan_document, plan_import_groups},
    token_stream::{TokenId, TokenStream, significant_tokens},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DelimiterStyle {
    Block,
    Braced,
    ConditionalBraced,
    Group,
    Fill,
    FillBraced,
    Hug,
    Vertical,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SingleArgumentLayout {
    Hug,
    Vertical,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ItemBoundary {
    Document,
    Module,
    ModuleInnerAttributes,
    Block,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ContinuationOperator {
    pub token: TokenId,
    pub depth: usize,
}

#[derive(Debug, Clone, Default)]
pub struct TokenFacts {
    pub pair: Option<TokenId>,
    pub delimiter_style: Option<DelimiterStyle>,
    pub item_boundary: Option<ItemBoundary>,
    pub group_end: Option<TokenId>,
    pub continuation_operators: Vec<ContinuationOperator>,
    flags: TokenFlags,
}

#[derive(Debug, Clone, Copy, Default)]
struct TokenFlags(u8);

impl TokenFlags {
    const TRAILING_COMMA: u8 = 1 << 0;
    const GENERIC: u8 = 1 << 1;
    const PREFIX_OPERATOR: u8 = 1 << 2;
    const ATTACHED_OPENER: u8 = 1 << 3;
    const ABSOLUTE_PATH_START: u8 = 1 << 4;
    const CONDITIONAL_GROUP: u8 = 1 << 5;
    const ATTRIBUTE_END: u8 = 1 << 6;

    fn insert(&mut self, flag: u8) {
        self.0 |= flag;
    }

    fn contains(self, flag: u8) -> bool {
        self.0 & flag != 0
    }
}

impl TokenFacts {
    pub fn supports_trailing_comma(&self) -> bool {
        self.flags.contains(TokenFlags::TRAILING_COMMA)
    }

    pub fn is_generic(&self) -> bool {
        self.flags.contains(TokenFlags::GENERIC)
    }

    pub fn is_prefix_operator(&self) -> bool {
        self.flags.contains(TokenFlags::PREFIX_OPERATOR)
    }

    pub fn is_attached_opener(&self) -> bool {
        self.flags.contains(TokenFlags::ATTACHED_OPENER)
    }

    pub fn is_attribute_end(&self) -> bool {
        self.flags.contains(TokenFlags::ATTRIBUTE_END)
    }

    pub fn is_absolute_path_start(&self) -> bool {
        self.flags.contains(TokenFlags::ABSOLUTE_PATH_START)
    }

    pub fn is_conditional_group(&self) -> bool {
        self.flags.contains(TokenFlags::CONDITIONAL_GROUP)
    }
}

#[derive(Debug)]
pub struct Layout {
    facts: Vec<TokenFacts>,
    pub document: DocumentPlan,
    pub import_groups: std::collections::HashMap<TokenId, ImportGroupPlan>,
}

impl Layout {
    pub fn new(document: &AstDocument, stream: &TokenStream) -> Result<Self, FormatError> {
        let mut facts = vec![TokenFacts::default(); stream.len()];
        pair_delimiters(stream, &mut facts)?;
        analyze_nodes(document.syntax(), stream, &mut facts)?;
        analyze_boundaries(document, stream, &mut facts)?;
        analyze_expression_groups(document.syntax(), stream, &mut facts)?;

        Ok(Self {
            facts,
            document: plan_document(document, stream)?,
            import_groups: plan_import_groups(document.syntax(), stream)?,
        })
    }

    pub fn facts(&self, token: TokenId) -> &TokenFacts {
        &self.facts[token.index()]
    }
}

fn pair_delimiters(stream: &TokenStream, facts: &mut [TokenFacts]) -> Result<(), FormatError> {
    let mut stack: Vec<(SyntaxKind, TokenId)> = Vec::new();
    for (id, token) in stream.token_ids() {
        match token.kind {
            T!['('] | T!['['] | T!['{'] => stack.push((token.kind, id)),
            T![')'] | T![']'] | T!['}'] => {
                let Some((open, open_id)) = stack.pop() else {
                    return Err(FormatError::Internal(
                        "closing delimiter has no opener".to_string(),
                    ));
                };
                if !delimiters_match(open, token.kind) {
                    return Err(FormatError::Internal(
                        "mismatched delimiters in valid syntax".to_string(),
                    ));
                }
                facts[open_id.index()].pair = Some(id);
            }
            _ => {}
        }
    }
    if !stack.is_empty() {
        return Err(FormatError::Internal(
            "opening delimiter has no closer".to_string(),
        ));
    }
    Ok(())
}

fn analyze_nodes(
    root: &SyntaxNode,
    stream: &TokenStream,
    facts: &mut [TokenFacts],
) -> Result<(), FormatError> {
    for node in root.descendants() {
        let kind = ast_node_kind(&node)?;
        // Intentionally exhaustive: adding a typed AST node requires an
        // explicit formatter analysis policy before this crate can compile.
        match kind {
            AstNodeKind::Block => {
                if let Some(id) = direct_or_descendant_token(&node, T!['{'], stream)? {
                    facts[id.index()].delimiter_style = Some(
                        expression_block_style(&node, id, stream, facts)
                            .unwrap_or(DelimiterStyle::Block),
                    );
                }
            }
            AstNodeKind::ModuleItem | AstNodeKind::StructItem => {
                if let Some(id) = direct_or_descendant_token(&node, T!['{'], stream)? {
                    facts[id.index()].delimiter_style = Some(DelimiterStyle::Block);
                    if kind == AstNodeKind::StructItem {
                        facts[id.index()].flags.insert(TokenFlags::TRAILING_COMMA);
                    }
                }
            }
            AstNodeKind::StructInitializerExpr | AstNodeKind::StructBinding => {
                if let Some(id) = direct_or_descendant_token(&node, T!['{'], stream)? {
                    facts[id.index()].delimiter_style = Some(DelimiterStyle::Braced);
                    facts[id.index()].flags.insert(TokenFlags::TRAILING_COMMA);
                }
            }
            AstNodeKind::PairExpr
            | AstNodeKind::PairType
            | AstNodeKind::PairBinding
            | AstNodeKind::ListExpr
            | AstNodeKind::ListType
            | AstNodeKind::ListBinding
            | AstNodeKind::ImportPathSegment => {
                if let Some(token) = node
                    .children_with_tokens()
                    .filter_map(rowan::NodeOrToken::into_token)
                    .find(|token| matches!(token.kind(), T!['('] | T!['['] | T!['{']))
                {
                    let id = token_id(&token, stream)?;
                    facts[id.index()].flags.insert(TokenFlags::TRAILING_COMMA);
                    if matches!(kind, AstNodeKind::PairExpr | AstNodeKind::ListExpr)
                        && sole_list_item_expression_kind(&node)
                            .is_some_and(is_transparent_expression_wrapper)
                        && !delimiter_contains_comments(id, stream, facts)
                    {
                        facts[id.index()].delimiter_style = Some(DelimiterStyle::Fill);
                    }
                }
            }
            AstNodeKind::GroupExpr => {
                if let Some(id) = direct_or_descendant_token(&node, T!['('], stream)?
                    && sole_direct_expression_kind(&node)
                        .is_some_and(is_transparent_expression_wrapper)
                    && !delimiter_contains_comments(id, stream, facts)
                {
                    facts[id.index()].delimiter_style = Some(DelimiterStyle::Fill);
                }
            }
            AstNodeKind::GenericParameters | AstNodeKind::GenericArguments => {
                let direct_tokens: Vec<_> = node
                    .children_with_tokens()
                    .filter_map(rowan::NodeOrToken::into_token)
                    .filter(|token| !token.kind().is_trivia())
                    .collect();
                for token in
                    significant_tokens(&node).filter(|token| is_generic_punctuation(token.kind()))
                {
                    let id = token_id(&token, stream)?;
                    facts[id.index()].flags.insert(TokenFlags::GENERIC);
                }
                if let (Some(open), Some(close)) = (direct_tokens.first(), direct_tokens.last())
                    && open.kind() == T![<]
                    && close.kind() == T![>]
                {
                    let open = token_id(open, stream)?;
                    let close = token_id(close, stream)?;
                    facts[open.index()].flags.insert(TokenFlags::TRAILING_COMMA);
                    facts[open.index()].pair = Some(close);
                }
            }
            AstNodeKind::PrefixExpr => {
                if let Some(token) = significant_tokens(&node)
                    .find(|token| SyntaxKind::PREFIX_OPS.contains(&token.kind()))
                {
                    let id = token_id(&token, stream)?;
                    facts[id.index()].flags.insert(TokenFlags::PREFIX_OPERATOR);
                }
            }
            AstNodeKind::FunctionItem
            | AstNodeKind::FunctionCallExpr
            | AstNodeKind::LambdaExpr
            | AstNodeKind::LambdaType => {
                if let Some(token) = node
                    .children_with_tokens()
                    .filter_map(rowan::NodeOrToken::into_token)
                    .find(|token| token.kind() == T!['('])
                {
                    let id = token_id(&token, stream)?;
                    facts[id.index()].flags.insert(TokenFlags::ATTACHED_OPENER);
                    facts[id.index()].flags.insert(TokenFlags::TRAILING_COMMA);
                    if kind == AstNodeKind::FunctionCallExpr
                        && let Some(argument_layout) = single_argument_layout(&node)
                    {
                        facts[id.index()].delimiter_style = Some(
                            if argument_layout == SingleArgumentLayout::Hug
                                && !delimiter_contains_comments(id, stream, facts)
                            {
                                DelimiterStyle::Hug
                            } else {
                                DelimiterStyle::Vertical
                            },
                        );
                    }
                }
            }
            AstNodeKind::PathExpr | AstNodeKind::PathType | AstNodeKind::ImportPath => {
                if let Some(token) = significant_tokens(&node).next()
                    && token.kind() == T![::]
                {
                    let id = token_id(&token, stream)?;
                    facts[id.index()]
                        .flags
                        .insert(TokenFlags::ABSOLUTE_PATH_START);
                }
            }
            AstNodeKind::Document
            | AstNodeKind::FunctionParameter
            | AstNodeKind::ConstantItem
            | AstNodeKind::TypeAliasItem
            | AstNodeKind::StructField
            | AstNodeKind::ImportItem
            | AstNodeKind::LiteralType
            | AstNodeKind::UnionType
            | AstNodeKind::GroupType
            | AstNodeKind::ListTypeItem
            | AstNodeKind::LambdaParameter
            | AstNodeKind::LetStmt
            | AstNodeKind::ExprStmt
            | AstNodeKind::IfStmt
            | AstNodeKind::ReturnStmt
            | AstNodeKind::AssertStmt
            | AstNodeKind::RaiseStmt
            | AstNodeKind::DebugStmt
            | AstNodeKind::PathSegment
            | AstNodeKind::StructInitializerField
            | AstNodeKind::LiteralExpr
            | AstNodeKind::ConstExpr
            | AstNodeKind::ListItem
            | AstNodeKind::BinaryExpr
            | AstNodeKind::GuardExpr
            | AstNodeKind::CastExpr
            | AstNodeKind::FieldAccessExpr
            | AstNodeKind::NamedBinding
            | AstNodeKind::ListBindingItem
            | AstNodeKind::StructFieldBinding => {}
            AstNodeKind::Attribute => {
                for token in node
                    .children_with_tokens()
                    .filter_map(rowan::NodeOrToken::into_token)
                    .filter(|token| !token.kind().is_trivia())
                {
                    let id = token_id(&token, stream)?;
                    match token.kind() {
                        T!['('] => {
                            facts[id.index()].flags.insert(TokenFlags::ATTACHED_OPENER);
                            facts[id.index()].flags.insert(TokenFlags::TRAILING_COMMA);
                        }
                        // The `!` of an inner attribute is attached to the brackets like a
                        // prefix operator, as in `#![allow(unused_import)]`
                        T![!] => facts[id.index()].flags.insert(TokenFlags::PREFIX_OPERATOR),
                        T!['['] => facts[id.index()].delimiter_style = Some(DelimiterStyle::Fill),
                        T![']'] => facts[id.index()].flags.insert(TokenFlags::ATTRIBUTE_END),
                        _ => {}
                    }
                }
            }
            AstNodeKind::IfExpr => {
                if !is_inline_conditional(&node) {
                    continue;
                }
                let mut tokens = significant_tokens(&node);
                let first = tokens.next().ok_or_else(|| {
                    FormatError::Internal(
                        "conditional expression has no significant token".to_string(),
                    )
                })?;
                let last = tokens.last().unwrap_or_else(|| first.clone());
                let start = token_id(&first, stream)?;
                facts[start.index()].group_end = Some(token_id(&last, stream)?);
                facts[start.index()]
                    .flags
                    .insert(TokenFlags::CONDITIONAL_GROUP);
            }
        }
    }
    Ok(())
}

fn single_argument_layout(call: &SyntaxNode) -> Option<SingleArgumentLayout> {
    let kind = sole_list_item_expression_kind(call)?;
    Some(if is_transparent_expression_wrapper(kind) {
        SingleArgumentLayout::Hug
    } else {
        SingleArgumentLayout::Vertical
    })
}

fn sole_list_item_expression_kind(node: &SyntaxNode) -> Option<AstNodeKind> {
    let mut items = node
        .children()
        .filter(|child| AstNodeKind::of(child) == Some(AstNodeKind::ListItem));
    let item = items.next()?;
    if items.next().is_some() {
        return None;
    }
    sole_direct_expression_kind(&item)
}

fn sole_direct_expression_kind(node: &SyntaxNode) -> Option<AstNodeKind> {
    let mut expressions = node.children().filter_map(AstExpr::cast);
    let expression = expressions.next()?;
    if expressions.next().is_some() {
        return None;
    }
    AstNodeKind::of(expression.syntax())
}

fn is_transparent_expression_wrapper(kind: AstNodeKind) -> bool {
    matches!(
        kind,
        AstNodeKind::FunctionCallExpr
            | AstNodeKind::StructInitializerExpr
            | AstNodeKind::ConstExpr
            | AstNodeKind::GroupExpr
            | AstNodeKind::PairExpr
            | AstNodeKind::ListExpr
            | AstNodeKind::Block
    )
}

fn expression_block_style(
    node: &SyntaxNode,
    open: TokenId,
    stream: &TokenStream,
    facts: &[TokenFacts],
) -> Option<DelimiterStyle> {
    if delimiter_contains_comments(open, stream, facts)
        || node.parent().is_some_and(|parent| {
            matches!(
                parent.kind(),
                SyntaxKind::FunctionItem | SyntaxKind::ModuleItem | SyntaxKind::IfStmt
            ) || (parent.kind() == SyntaxKind::IfExpr && !is_inline_conditional(&parent))
        })
    {
        return None;
    }

    let mut children = node.children();
    let expression = children.next().and_then(AstExpr::cast)?;
    if children.next().is_some() {
        return None;
    }
    let kind = AstNodeKind::of(expression.syntax())?;
    if node
        .parent()
        .is_some_and(|parent| parent.kind() == SyntaxKind::IfExpr)
    {
        return Some(DelimiterStyle::ConditionalBraced);
    }
    Some(if is_transparent_expression_wrapper(kind) {
        DelimiterStyle::FillBraced
    } else {
        DelimiterStyle::Braced
    })
}

fn is_inline_conditional(node: &SyntaxNode) -> bool {
    node.ancestors()
        .take_while(|ancestor| ancestor.kind() == SyntaxKind::IfExpr)
        .any(|conditional| {
            conditional.children_with_tokens().any(|element| {
                element
                    .as_token()
                    .is_some_and(|token| token.kind() == T![inline])
            })
        })
}

fn delimiter_contains_comments(open: TokenId, stream: &TokenStream, facts: &[TokenFacts]) -> bool {
    let Some(close) = facts[open.index()].pair else {
        return false;
    };
    ((open.index() + 1)..=close.index()).any(|index| {
        stream
            .boundary(index)
            .is_ok_and(|boundary| !stream.gap(boundary).comments.is_empty())
    })
}

fn analyze_boundaries(
    document: &AstDocument,
    stream: &TokenStream,
    facts: &mut [TokenFacts],
) -> Result<(), FormatError> {
    for item in document.items() {
        let Some(token) = significant_tokens(item.syntax()).last() else {
            return Err(FormatError::Internal(
                "document item has no significant token".to_string(),
            ));
        };
        let mut id = token_id(&token, stream)?;
        if let Some(next) = stream
            .next_token(id)
            .filter(|next| stream.token(*next).kind == T![;])
        {
            id = next;
        }
        facts[id.index()].item_boundary = Some(ItemBoundary::Document);
    }

    for (kind, boundary) in [
        (SyntaxKind::ModuleItem, ItemBoundary::Module),
        (SyntaxKind::Block, ItemBoundary::Block),
    ] {
        for container in document
            .syntax()
            .descendants()
            .filter(|node| node.kind() == kind)
        {
            for item in container.children() {
                if let Some(token) = significant_tokens(&item).last() {
                    let id = token_id(&token, stream)?;
                    if facts[id.index()].item_boundary.is_none() {
                        facts[id.index()].item_boundary = Some(boundary);
                    }
                }
            }

            // Inner attributes at the start of a module are separated from its items by a blank
            // line, like they are at the start of a file
            if let Some(module) = AstModuleItem::cast(container)
                && let Some(last) = module.inner_attributes().last()
                && last.syntax().next_sibling().is_some()
                && let Some(token) = significant_tokens(last.syntax()).last()
            {
                let id = token_id(&token, stream)?;
                facts[id.index()].item_boundary = Some(ItemBoundary::ModuleInnerAttributes);
            }
        }
    }
    Ok(())
}

fn analyze_expression_groups(
    root: &SyntaxNode,
    stream: &TokenStream,
    facts: &mut [TokenFacts],
) -> Result<(), FormatError> {
    for node in root
        .descendants()
        .filter(|node| matches!(node.kind(), SyntaxKind::BinaryExpr | SyntaxKind::UnionType))
        .filter(|node| {
            node.parent()
                .is_none_or(|parent| parent.kind() != node.kind())
        })
    {
        let mut tokens = significant_tokens(&node);
        let first = tokens.next().ok_or_else(|| {
            FormatError::Internal("binary expression has no significant token".to_string())
        })?;
        let last = tokens.last().unwrap_or_else(|| first.clone());
        let start = token_id(&first, stream)?;
        let end = token_id(&last, stream)?;
        facts[start.index()].group_end = Some(
            facts[start.index()]
                .group_end
                .map_or(end, |current| current.max(end)),
        );

        let mut operators = Vec::new();
        if node.kind() == SyntaxKind::BinaryExpr {
            collect_binary_operators(&node, None, 0, &mut operators)?;
        } else {
            collect_union_operators(&node, &mut operators);
        }
        facts[start.index()].continuation_operators = operators
            .into_iter()
            .map(|operator| {
                Ok(ContinuationOperator {
                    token: stream.token_id_at_offset(operator.offset)?,
                    depth: operator.depth,
                })
            })
            .collect::<Result<Vec<_>, _>>()?;
    }
    Ok(())
}

fn direct_or_descendant_token(
    node: &SyntaxNode,
    kind: SyntaxKind,
    stream: &TokenStream,
) -> Result<Option<TokenId>, FormatError> {
    significant_tokens(node)
        .find(|token| token.kind() == kind)
        .map(|token| token_id(&token, stream))
        .transpose()
}

fn token_id(token: &rue_parser::SyntaxToken, stream: &TokenStream) -> Result<TokenId, FormatError> {
    stream.token_id_at_offset(usize::from(token.text_range().start()))
}

fn ast_node_kind(node: &SyntaxNode) -> Result<AstNodeKind, FormatError> {
    AstNodeKind::of(node).ok_or_else(|| {
        FormatError::Internal(format!(
            "syntax node {:?} has no typed AST node kind",
            node.kind()
        ))
    })
}

#[derive(Debug, Clone, Copy)]
struct OperatorOffset {
    offset: usize,
    depth: usize,
}

fn collect_binary_operators(
    node: &SyntaxNode,
    parent_precedence: Option<u8>,
    parent_depth: usize,
    operators: &mut Vec<OperatorOffset>,
) -> Result<(), FormatError> {
    let operator = node
        .children_with_tokens()
        .filter_map(rowan::NodeOrToken::into_token)
        .find(|token| SyntaxKind::BINARY_OPS.contains(&token.kind()))
        .ok_or_else(|| {
            FormatError::Internal("binary expression has no direct operator".to_string())
        })?;
    let precedence = operator
        .kind()
        .binary_binding_power()
        .ok_or_else(|| FormatError::Internal("binary operator has no precedence".to_string()))?
        .0;
    let depth =
        parent_precedence.map_or(0, |parent| parent_depth + usize::from(precedence != parent));

    for element in node.children_with_tokens() {
        match element {
            rowan::NodeOrToken::Node(child) if child.kind() == SyntaxKind::BinaryExpr => {
                collect_binary_operators(&child, Some(precedence), depth, operators)?;
            }
            rowan::NodeOrToken::Token(token) if SyntaxKind::BINARY_OPS.contains(&token.kind()) => {
                operators.push(OperatorOffset {
                    offset: usize::from(token.text_range().start()),
                    depth,
                });
            }
            _ => {}
        }
    }
    Ok(())
}

fn collect_union_operators(node: &SyntaxNode, operators: &mut Vec<OperatorOffset>) {
    for element in node.children_with_tokens() {
        match element {
            rowan::NodeOrToken::Node(child) if child.kind() == SyntaxKind::UnionType => {
                collect_union_operators(&child, operators);
            }
            rowan::NodeOrToken::Token(token) if token.kind() == T![|] => {
                operators.push(OperatorOffset {
                    offset: usize::from(token.text_range().start()),
                    depth: 0,
                });
            }
            _ => {}
        }
    }
}

fn is_generic_punctuation(kind: SyntaxKind) -> bool {
    matches!(kind, T![<] | T![>] | T![,])
}

fn delimiters_match(open: SyntaxKind, close: SyntaxKind) -> bool {
    matches!(
        (open, close),
        (T!['('], T![')']) | (T!['['], T![']']) | (T!['{'], T!['}'])
    )
}
