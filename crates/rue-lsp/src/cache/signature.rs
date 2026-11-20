//! The signature of the function that's being called at the cursor, and which of its parameters
//! the argument at the cursor is for. A struct initializer shows the struct's fields the same way.

use std::collections::HashMap;
use std::ops::Range;

use rowan::{TextRange, TextSize, TokenAtOffset};
use rue_ast::{AstExpr, AstFunctionCallExpr, AstNode, AstStructInitializerExpr};
use rue_compiler::{Compiler, CompletionContext, SyntaxCall, SyntaxItemKind};
use rue_hir::Symbol;
use rue_parser::{SyntaxNode, T};
use rue_types::Type;

use super::{Cache, FieldHoverInfo, Scopes, field_declarations};

/// How many parts of each type are written in a signature. A type that was inferred can be far too
/// large to show, or even to write, in full.
const TYPE_PARTS: usize = 64;

/// The signatures that a call could be to, of which there's more than one for some builtins.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Signatures {
    pub signatures: Vec<Signature>,
    /// The signature that the arguments given so far are for.
    pub active: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Signature {
    pub label: String,
    /// The part of the label that each parameter is, as byte offsets.
    pub parameters: Vec<Range<usize>>,
    /// The parameter that the argument at the cursor is for.
    pub active_parameter: Option<usize>,
}

impl Signature {
    /// A signature whose parameters are written between a prefix and a suffix, with commas
    /// between them.
    fn new(prefix: &str, parameters: &[impl AsRef<str>], suffix: &str, argument: usize) -> Self {
        let mut label = prefix.to_string();
        let mut ranges = Vec::new();

        for (i, parameter) in parameters.iter().enumerate() {
            if i > 0 {
                label.push_str(", ");
            }

            let start = label.len();
            label.push_str(parameter.as_ref());
            ranges.push(start..label.len());
        }

        label.push_str(suffix);

        // An argument past the last parameter is shown as being for it even if it doesn't take
        // the rest of them, since the protocol has no way to say that it isn't for any of them
        let active_parameter = ranges.len().checked_sub(1).map(|last| argument.min(last));

        Self {
            label,
            parameters: ranges,
            active_parameter,
        }
    }

    /// Whether the signature has a parameter for the argument, which the last one is for every
    /// argument from it on if it takes the rest of them.
    fn takes(&self, argument: usize) -> bool {
        argument < self.parameters.len()
            || self
                .parameters
                .last()
                .is_some_and(|last| self.label[last.clone()].starts_with("..."))
    }
}

impl Cache<Compiler> {
    pub fn signature_help(&mut self, scopes: &Scopes, index: usize) -> Option<Signatures> {
        let offset = TextSize::from(u32::try_from(index).ok()?);

        // The token before the cursor is inside of everything that the cursor is
        let token = match self.tokens_at(offset) {
            TokenAtOffset::None => return None,
            TokenAtOffset::Single(token) | TokenAtOffset::Between(token, _) => token,
        };

        // The innermost call or struct initializer that the cursor is between the delimiters of
        token.parent_ancestors().find_map(|node| {
            if let Some(call) = AstFunctionCallExpr::cast(node.clone()) {
                let argument = argument_index(&node, T!['('], T![')'], offset)?;
                return Some(self.call_signatures(scopes, &call, argument));
            }

            if let Some(initializer) = AstStructInitializerExpr::cast(node.clone()) {
                argument_index(&node, T!['{'], T!['}'], offset)?;
                return Some(self.struct_signature(scopes, &initializer, offset));
            }

            None
        })?
    }

    fn call_signatures(
        &mut self,
        scopes: &Scopes,
        call: &AstFunctionCallExpr,
        argument: usize,
    ) -> Option<Signatures> {
        // A function that's named by a path is shown with its name
        let name = match call.expr()? {
            AstExpr::PathExpr(path) => path.segments().last().and_then(|segment| segment.name()),
            _ => None,
        };

        let prefix = name
            .as_ref()
            .map_or_else(|| "fn".to_string(), |name| format!("fn {}", name.text()));

        // Builtins are checked on their own, so they have signatures rather than a type
        if let Some(name) = &name
            && let Some(builtin) = self
                .items_in(name.text_range())
                .find_map(|kind| match kind {
                    SyntaxItemKind::SymbolReference(symbol) => match self.ctx.symbol(*symbol) {
                        Symbol::Builtin(builtin) => Some(*builtin),
                        _ => None,
                    },
                    _ => None,
                })
        {
            let signatures = builtin
                .signatures()
                .iter()
                .map(|signature| builtin_signature(&prefix, signature, argument))
                .collect::<Vec<_>>();

            // The first signature that has a parameter for the argument at the cursor
            let active = signatures
                .iter()
                .position(|signature| signature.takes(argument))
                .unwrap_or(signatures.len().saturating_sub(1));

            return Some(Signatures { signatures, active });
        }

        // If the call was compiled more than once, the last time is the one that was kept
        let SyntaxCall { function, generics } = self
            .items_in(call.syntax().text_range())
            .filter_map(|kind| match kind {
                SyntaxItemKind::Call(call) => Some(call),
                _ => None,
            })
            .last()?
            .clone();

        let inferred = generics.into_iter().collect::<HashMap<_, _>>();

        // The types are shown as they're declared, with what each generic was inferred as next to
        // it, rather than with the generics replaced in each type. A type with them replaced is a
        // copy that isn't named by anything, which can take very long to write out in full
        let mut prefix = prefix;

        for (i, &generic) in function.generics.iter().enumerate() {
            prefix.push_str(if i == 0 { "<" } else { ", " });
            let name = self.bounded_type_name(scopes, generic, TYPE_PARTS);
            prefix.push_str(&name);

            // A generic can be inferred as itself by a recursive call, or as a generic of the
            // caller's with the same name, which says nothing
            if let Some(&ty) = inferred.get(&generic) {
                let inferred = self.bounded_type_name(scopes, ty, TYPE_PARTS);

                if inferred != name {
                    prefix.push_str(" = ");
                    prefix.push_str(&inferred);
                }
            }

            if i == function.generics.len() - 1 {
                prefix.push('>');
            }
        }

        prefix.push('(');

        let len = function.params.len();

        let parameters = function
            .params
            .iter()
            .enumerate()
            .map(|(i, param)| {
                let ty = self.bounded_type_name(scopes, param.ty, TYPE_PARTS);

                let spread = if !function.nil_terminated && i == len - 1 {
                    "..."
                } else {
                    ""
                };

                format!("{spread}{}: {ty}", param.name)
            })
            .collect::<Vec<_>>();

        let suffix = format!(
            ") -> {}",
            self.bounded_type_name(scopes, function.ret, TYPE_PARTS)
        );

        Some(Signatures {
            signatures: vec![Signature::new(&prefix, &parameters, &suffix, argument)],
            active: 0,
        })
    }

    /// The fields of the struct that's initialized, as if they were the parameters of a function.
    fn struct_signature(
        &mut self,
        scopes: &Scopes,
        initializer: &AstStructInitializerExpr,
        offset: TextSize,
    ) -> Option<Signatures> {
        let name = initializer.path()?.syntax().text().to_string();
        let name = name.trim();

        let (ty, specified) = self
            .items_in(initializer.syntax().text_range())
            .filter_map(|kind| match kind {
                SyntaxItemKind::CompletionContext(CompletionContext::StructFields {
                    ty,
                    specified_fields: Some(specified),
                }) => Some((*ty, specified.clone())),
                _ => None,
            })
            .last()?;

        let (mut fields, nil_terminated) = self.struct_field_types(ty)?;

        if fields.is_empty() {
            return None;
        }

        // A generic struct whose generic arguments are missing or wrong has fields whose types
        // couldn't be worked out, so they're shown as they're declared, like a call's are
        if fields
            .iter()
            .any(|&(_, ty)| rue_types::contains_unresolved(self.ctx.types(), ty))
            && let Type::Struct(declared) = self.ctx.ty(ty)
            && let Some((declared, _)) = self.struct_field_types(declared.semantic)
        {
            fields = declared;
        }

        let fields = fields
            .into_iter()
            .map(|(name, ty)| FieldHoverInfo {
                name,
                type_name: self.bounded_type_name(scopes, ty, TYPE_PARTS),
            })
            .collect::<Vec<_>>();

        // The field that the cursor is in, or else the first one that hasn't been given a value
        // yet, which is the one to write next
        let active = initializer
            .fields()
            .find(|field| field.syntax().text_range().contains_inclusive(offset))
            .and_then(|field| field.name())
            .and_then(|name| fields.iter().position(|field| field.name == name.text()))
            .or_else(|| {
                fields
                    .iter()
                    .position(|field| !specified.contains(&field.name))
            })
            .unwrap_or(0);

        let declarations = field_declarations(&fields, nil_terminated);

        Some(Signatures {
            signatures: vec![Signature::new(
                &format!("{name} {{ "),
                &declarations,
                " }",
                active,
            )],
            active: 0,
        })
    }

    /// The items of the file that queries are about with exactly the span, in the order they were
    /// added.
    fn items_in(&self, span: TextRange) -> impl Iterator<Item = &SyntaxItemKind> {
        self.syntax_map
            .items()
            .filter(move |item| item.span == span && item.source_kind == self.source.kind)
            .map(|item| &item.kind)
    }
}

/// How many arguments are before the offset, if it's between the delimiters of the node (or
/// after the opening one, if the closing one is missing).
fn argument_index(
    node: &SyntaxNode,
    open: rue_parser::SyntaxKind,
    close: rue_parser::SyntaxKind,
    offset: TextSize,
) -> Option<usize> {
    let tokens = || {
        node.children_with_tokens()
            .filter_map(rowan::NodeOrToken::into_token)
    };

    let open = tokens().find(|token| token.kind() == open)?;

    if offset < open.text_range().end() {
        return None;
    }

    if tokens()
        .filter(|token| token.kind() == close)
        .last()
        .is_some_and(|close| offset > close.text_range().start())
    {
        return None;
    }

    Some(
        tokens()
            .filter(|token| token.kind() == T![,] && token.text_range().end() <= offset)
            .count(),
    )
}

/// The signature of a builtin, which is written like the rest of a function's declaration after
/// its name, such as `(value: Bytes, start: Int) -> Bytes`.
fn builtin_signature(prefix: &str, signature: &str, argument: usize) -> Signature {
    let mut parameters = Vec::new();
    let mut depth = 0usize;
    let mut start = 1;
    let mut end = signature.len();

    // The parameters are between the outermost parentheses, separated by the commas that aren't
    // inside of a type
    for (i, c) in signature.char_indices() {
        match c {
            '(' | '<' | '[' => depth += 1,
            ')' | '>' | ']' => {
                depth = depth.saturating_sub(1);

                if depth == 0 {
                    end = i;
                    break;
                }
            }
            ',' if depth == 1 => {
                parameters.push(signature[start..i].trim());
                start = i + 1;
            }
            _ => {}
        }
    }

    let last = signature.get(start..end).unwrap_or_default().trim();

    if !last.is_empty() {
        parameters.push(last);
    }

    Signature::new(
        &format!("{prefix}("),
        &parameters,
        signature.get(end..).unwrap_or_default(),
        argument,
    )
}
