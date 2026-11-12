use std::collections::HashMap;

use rue_ast::{
    AstBlock, AstFunctionItem, AstFunctionParameter, AstLambdaExpr, AstLetStmt, AstNode,
};
use rue_diagnostic::{Lint, Note, Source, SrcLoc};
use rue_parser::{SyntaxNode, SyntaxToken};

use crate::{LintContext, LintInput, lints::naming::binding_names, srcloc};

pub(crate) const LINTS: &[Lint] = &[Lint::ShadowedBinding];

/// Reports bindings and parameters that shadow another binding or parameter with the same name,
/// which is in scope where they're declared.
pub(crate) fn check(cx: &mut LintContext<'_>, input: &LintInput<'_>) {
    for &(source, document) in &input.documents {
        let mut walker = Walker {
            cx,
            source,
            scopes: Vec::new(),
        };

        walker.walk(document.syntax());
    }
}

struct Walker<'a, 'b> {
    cx: &'a mut LintContext<'b>,
    source: &'a Source,
    /// The names declared in each scope that encloses the current node, from the outermost to
    /// the innermost.
    scopes: Vec<HashMap<String, SrcLoc>>,
}

impl Walker<'_, '_> {
    fn walk(&mut self, node: &SyntaxNode) {
        if let Some(function) = AstFunctionItem::cast(node.clone()) {
            // Bindings in other items aren't in scope, so each function starts again
            let scopes = std::mem::take(&mut self.scopes);
            self.walk_function(
                function.parameters(),
                function.body().map(|body| body.syntax().clone()),
            );
            self.scopes = scopes;
        } else if let Some(lambda) = AstLambdaExpr::cast(node.clone()) {
            self.walk_function(
                lambda.parameters(),
                lambda.body().map(|body| body.syntax().clone()),
            );
        } else if AstBlock::cast(node.clone()).is_some() {
            self.scopes.push(HashMap::new());
            self.walk_children(node);
            self.scopes.pop();
        } else if let Some(stmt) = AstLetStmt::cast(node.clone()) {
            // The value is evaluated before the binding is in scope
            if let Some(value) = stmt.value() {
                self.walk(value.syntax());
            }

            if self.scopes.is_empty() {
                self.scopes.push(HashMap::new());
            }

            for name in stmt
                .binding()
                .iter()
                .flat_map(|binding| binding_names(binding, true))
            {
                self.declare(&name);
            }
        } else {
            self.walk_children(node);
        }
    }

    fn walk_children(&mut self, node: &SyntaxNode) {
        for child in node.children() {
            self.walk(&child);
        }
    }

    fn walk_function(
        &mut self,
        parameters: impl Iterator<Item = AstFunctionParameter>,
        body: Option<SyntaxNode>,
    ) {
        self.scopes.push(HashMap::new());

        for parameter in parameters {
            for name in parameter
                .binding()
                .iter()
                .flat_map(|binding| binding_names(binding, true))
            {
                self.declare(&name);
            }
        }

        if let Some(body) = body {
            self.walk(&body);
        }

        self.scopes.pop();
    }

    fn declare(&mut self, name: &SyntaxToken) {
        let text = name.text();
        let srcloc = srcloc(self.source, name.text_range());

        if !text.starts_with('_')
            && let Some(previous) = self
                .scopes
                .iter()
                .rev()
                .find_map(|scope| scope.get(text))
                .cloned()
            && let Some(diagnostic) = self.cx.report(
                Lint::ShadowedBinding,
                srcloc.clone(),
                format!("`{text}` shadows an earlier binding with the same name"),
            )
        {
            diagnostic
                .notes
                .push(Note::at(previous, "The earlier binding is declared here"));
        }

        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(text.to_string(), srcloc);
        }
    }
}
