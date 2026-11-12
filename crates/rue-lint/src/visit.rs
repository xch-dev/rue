use std::collections::HashSet;

use rue_diagnostic::Lint;
use rue_hir::{Block, Declaration, Hir, HirId, Statement, Symbol, SymbolId};

use crate::LintContext;

/// A lint pass that runs over the HIR of the documents being linted. Every pass runs during the
/// same walk, which visits every function, constant and struct field default declared in them
/// (including lambdas), and calls each pass for each HIR node and statement once.
///
/// The type and location of the expression that a node was compiled from are available from
/// [`Database::hir_type`](rue_hir::Database::hir_type) and
/// [`Database::hir_srcloc`](rue_hir::Database::hir_srcloc). Nodes that the compiler generated in
/// other ways don't have either, so passes should skip them.
pub trait HirLintPass {
    /// The lints the pass can report, so that the pass can be skipped if they're all allowed.
    fn lints(&self) -> &'static [Lint];

    /// Called for each function and lambda, before its body.
    fn check_function(&mut self, _cx: &mut LintContext<'_>, _symbol: SymbolId, _body: HirId) {}

    /// Called for each function and lambda, after its body.
    fn check_function_post(&mut self, _cx: &mut LintContext<'_>, _symbol: SymbolId, _body: HirId) {}

    /// Called for each HIR node, before the nodes inside of it.
    fn check_hir(&mut self, _cx: &mut LintContext<'_>, _hir: HirId, _node: &Hir) {}

    /// Called for each HIR node, after the nodes inside of it.
    fn check_hir_post(&mut self, _cx: &mut LintContext<'_>, _hir: HirId, _node: &Hir) {}

    /// Called for each statement of a block, before the nodes inside of it. The block is the
    /// innermost of [`LintContext::parents`].
    fn check_statement(&mut self, _cx: &mut LintContext<'_>, _block: &Block, _index: usize) {}

    /// Called once after the whole HIR has been walked, for passes that report what they found
    /// across every declaration.
    fn finish(&mut self, _cx: &mut LintContext<'_>) {}
}

/// Where the walk over the HIR currently is, which passes can read from the [`LintContext`].
#[derive(Debug, Default, Clone)]
pub(crate) struct WalkState {
    pub(crate) declaration: Option<Declaration>,
    pub(crate) functions: Vec<SymbolId>,
    pub(crate) parents: Vec<HirId>,
}

/// Runs the HIR lint passes whose lints could be reported, in a single walk over the HIR.
pub(crate) fn run_hir_passes<'p>(
    cx: &mut LintContext<'_>,
    mut passes: Vec<Box<dyn HirLintPass + 'p>>,
) {
    passes.retain(|pass| pass.lints().iter().any(|&lint| cx.is_enabled(lint)));

    if passes.is_empty() {
        return;
    }

    let mut walker = Walker {
        passes,
        visited: HashSet::new(),
    };

    for (declaration, root) in roots(cx) {
        cx.walk = WalkState {
            declaration: Some(declaration),
            ..WalkState::default()
        };

        match root {
            Root::Function(symbol) => walker.visit_function(cx, symbol),
            Root::Hir(hir) => walker.visit_hir(cx, hir),
        }
    }

    cx.walk = WalkState::default();

    for pass in &mut walker.passes {
        pass.finish(cx);
    }
}

enum Root {
    Function(SymbolId),
    Hir(HirId),
}

/// Every function, constant and struct field default declared in the documents being linted.
/// Lambdas aren't declarations, so they're visited from the HIR they're declared in.
fn roots(cx: &LintContext<'_>) -> Vec<(Declaration, Root)> {
    let db = cx.db();
    let mut roots = Vec::new();

    for declaration in db.relevant_declarations() {
        match declaration {
            Declaration::Symbol(symbol) => match db.symbol(symbol) {
                Symbol::Function(_) => roots.push((declaration, Root::Function(symbol))),
                Symbol::Constant(constant) => {
                    roots.push((declaration, Root::Hir(constant.value.hir)));
                }
                // Bindings are visited from their `let` statement, and the rest don't have HIR
                _ => {}
            },
            Declaration::Type(ty) => {
                roots.extend(
                    db.default_fields(ty)
                        .map(|(_, value)| (declaration, Root::Hir(value.hir))),
                );
            }
        }
    }

    roots
}

struct Walker<'p> {
    passes: Vec<Box<dyn HirLintPass + 'p>>,
    visited: HashSet<HirId>,
}

impl Walker<'_> {
    fn visit_function(&mut self, cx: &mut LintContext<'_>, symbol: SymbolId) {
        let Symbol::Function(function) = cx.db().symbol(symbol) else {
            return;
        };

        let body = function.body;

        // The body is only visited once, so the function is too
        if self.visited.contains(&body) {
            return;
        }

        cx.walk.functions.push(symbol);

        for pass in &mut self.passes {
            pass.check_function(cx, symbol, body);
        }

        self.visit_hir(cx, body);

        for pass in &mut self.passes {
            pass.check_function_post(cx, symbol, body);
        }

        cx.walk.functions.pop();
    }

    fn visit_hir(&mut self, cx: &mut LintContext<'_>, hir: HirId) {
        // A few nodes that the compiler generates are shared, such as the `nil` of a `return`
        // without a value, so each node is only visited once
        if !self.visited.insert(hir) {
            return;
        }

        let node = cx.db().hir(hir).clone();

        for pass in &mut self.passes {
            pass.check_hir(cx, hir, &node);
        }

        cx.walk.parents.push(hir);
        self.visit_children(cx, node.clone());
        cx.walk.parents.pop();

        for pass in &mut self.passes {
            pass.check_hir_post(cx, hir, &node);
        }
    }

    fn visit_children(&mut self, cx: &mut LintContext<'_>, node: Hir) {
        match node {
            Hir::Unresolved
            | Hir::Nil
            | Hir::String(_)
            | Hir::Int(_)
            | Hir::Bytes(_)
            | Hir::Bool(_)
            | Hir::Reference(_)
            | Hir::InfinityG1
            | Hir::InfinityG2 => {}
            // The result of a const expression is generated, so only the expression is visited
            Hir::Const(expr) => self.visit_hir(cx, expr.value),
            Hir::List(list) => self.visit_all(cx, list.items),
            // Default values are visited from the struct declaration instead
            Hir::Struct(initializer) => self.visit_all(
                cx,
                initializer
                    .fields
                    .into_iter()
                    .filter(|field| !field.is_default)
                    .map(|field| field.value),
            ),
            Hir::FieldAccess(access) => self.visit_hir(cx, access.value),
            Hir::Cast(hir) => self.visit_hir(cx, hir),
            Hir::Guard(guard) => self.visit_hir(cx, guard.value),
            Hir::Block(block) => self.visit_block(cx, &block),
            Hir::Lambda(symbol) => self.visit_function(cx, symbol),
            Hir::Pair(first, rest) => self.visit_all(cx, [first, rest]),
            Hir::If(condition, then, else_, _) => self.visit_all(cx, [condition, then, else_]),
            Hir::FunctionCall(call) => {
                self.visit_hir(cx, call.function);
                self.visit_all(cx, call.args);
            }
            Hir::Unary(_, hir) | Hir::ClvmOp(_, hir) => self.visit_hir(cx, hir),
            Hir::Binary(_, left, right) => self.visit_all(cx, [left, right]),
            Hir::CoinId(a, b, c)
            | Hir::Modpow(a, b, c)
            | Hir::Secp256K1Verify(a, b, c)
            | Hir::Secp256R1Verify(a, b, c) => self.visit_all(cx, [a, b, c]),
            Hir::Substr(hir, start, end) => {
                self.visit_all(cx, [hir, start]);
                self.visit_all(cx, end);
            }
            Hir::G1Map(data, dst) | Hir::G2Map(data, dst) => {
                self.visit_hir(cx, data);
                self.visit_all(cx, dst);
            }
            Hir::BlsPairingIdentity(args) => self.visit_all(cx, args),
            Hir::BlsVerify(sig, args) => {
                self.visit_hir(cx, sig);
                self.visit_all(cx, args);
            }
        }
    }

    fn visit_all(&mut self, cx: &mut LintContext<'_>, hirs: impl IntoIterator<Item = HirId>) {
        for hir in hirs {
            self.visit_hir(cx, hir);
        }
    }

    fn visit_block(&mut self, cx: &mut LintContext<'_>, block: &Block) {
        for (index, statement) in block.statements.iter().enumerate() {
            for pass in &mut self.passes {
                pass.check_statement(cx, block, index);
            }

            match statement {
                Statement::Let(symbol, _) => {
                    if let Symbol::Binding(binding) = cx.db().symbol(*symbol) {
                        let value = binding.value.hir;
                        self.visit_hir(cx, value);
                    }
                }
                Statement::Expr(statement) => self.visit_hir(cx, statement.hir),
                Statement::If(statement) => {
                    self.visit_all(cx, [statement.condition, statement.then]);
                }
                Statement::Return(hir, _)
                | Statement::Assert(hir, _)
                | Statement::Debug(hir, _) => {
                    self.visit_hir(cx, *hir);
                }
                Statement::Raise(hir, _) => self.visit_all(cx, *hir),
            }
        }

        self.visit_all(cx, block.body);
    }
}

#[cfg(test)]
mod tests {
    use std::{cell::RefCell, rc::Rc};

    use indexmap::IndexMap;
    use rue_diagnostic::LintLevels;
    use rue_hir::{Database, FunctionKind, FunctionSymbol, Scope};
    use rue_types::BuiltinTypes;

    use crate::ScopedLintLevels;

    use super::*;

    /// Records where each hook is called, and what the context says at that point.
    struct Recorder(Rc<RefCell<Vec<String>>>);

    impl Recorder {
        fn record(&self, cx: &LintContext<'_>, event: &str) {
            let db = cx.db();

            let function = cx
                .enclosing_function()
                .map_or("-".to_string(), |symbol| db.debug_symbol(symbol));

            let declaration = match cx.enclosing_declaration() {
                Some(Declaration::Symbol(symbol)) => db.debug_symbol(symbol),
                _ => "-".to_string(),
            };

            let parent = cx.parent().map_or("-".to_string(), |hir| db.debug_hir(hir));

            self.0.borrow_mut().push(format!(
                "{event} (in {function} of {declaration}, {} parents, parent {parent})",
                cx.parents().len()
            ));
        }
    }

    impl HirLintPass for Recorder {
        fn lints(&self) -> &'static [Lint] {
            &[Lint::UnusedValue]
        }

        fn check_function(&mut self, cx: &mut LintContext<'_>, symbol: SymbolId, _body: HirId) {
            self.record(cx, &format!("enter {}", cx.db().debug_symbol(symbol)));
        }

        fn check_function_post(&mut self, cx: &mut LintContext<'_>, symbol: SymbolId, _: HirId) {
            self.record(cx, &format!("exit {}", cx.db().debug_symbol(symbol)));
        }

        fn check_hir(&mut self, cx: &mut LintContext<'_>, hir: HirId, _node: &Hir) {
            self.record(cx, &format!("hir {}", cx.db().debug_hir(hir)));
        }

        fn check_hir_post(&mut self, cx: &mut LintContext<'_>, hir: HirId, _node: &Hir) {
            self.record(cx, &format!("post {}", cx.db().debug_hir(hir)));
        }
    }

    fn function(db: &mut Database, types: &BuiltinTypes, name: &str, body: HirId) -> SymbolId {
        let scope = db.alloc_scope(Scope::new(None));

        db.alloc_symbol(Symbol::Function(FunctionSymbol {
            name: Some(rue_diagnostic::Name::new(name, None)),
            ty: types.unresolved,
            scope,
            vars: Vec::new(),
            parameters: IndexMap::new(),
            nil_terminated: true,
            return_type: types.unresolved,
            body,
            kind: FunctionKind::Sequential,
        }))
    }

    #[test]
    fn test_walk_context() {
        let mut db = Database::new();
        let types = BuiltinTypes::new(db.types_mut());

        // fn main() { if true { fn() => 1 } else { nil } }
        let one = db.alloc_hir(Hir::Int(1.into()));
        let lambda = function(&mut db, &types, "lambda", one);
        let lambda = db.alloc_hir(Hir::Lambda(lambda));
        let condition = db.alloc_hir(Hir::Bool(true));
        let nil = db.alloc_hir(Hir::Nil);
        let if_expr = db.alloc_hir(Hir::If(condition, lambda, nil, false));
        let main = function(&mut db, &types, "main", if_expr);
        db.add_relevant_declaration(Declaration::Symbol(main));

        let log = Rc::new(RefCell::new(Vec::new()));

        let (levels, _) = ScopedLintLevels::new(LintLevels::default(), &[]);
        let mut cx = LintContext::new(&mut db, &types, levels);
        run_hir_passes(&mut cx, vec![Box::new(Recorder(log.clone()))]);

        let parent = "if true { lambda<0> } else { nil }";

        assert_eq!(
            log.take(),
            vec![
                "enter main<1> (in main<1> of main<1>, 0 parents, parent -)".to_string(),
                format!("hir {parent} (in main<1> of main<1>, 0 parents, parent -)"),
                format!("hir true (in main<1> of main<1>, 1 parents, parent {parent})"),
                format!("post true (in main<1> of main<1>, 1 parents, parent {parent})"),
                format!("hir lambda<0> (in main<1> of main<1>, 1 parents, parent {parent})"),
                format!("enter lambda<0> (in lambda<0> of main<1>, 2 parents, parent lambda<0>)"),
                "hir 1 (in lambda<0> of main<1>, 2 parents, parent lambda<0>)".to_string(),
                "post 1 (in lambda<0> of main<1>, 2 parents, parent lambda<0>)".to_string(),
                "exit lambda<0> (in lambda<0> of main<1>, 2 parents, parent lambda<0>)".to_string(),
                format!("post lambda<0> (in main<1> of main<1>, 1 parents, parent {parent})"),
                format!("hir nil (in main<1> of main<1>, 1 parents, parent {parent})"),
                format!("post nil (in main<1> of main<1>, 1 parents, parent {parent})"),
                format!("post {parent} (in main<1> of main<1>, 0 parents, parent -)"),
                "exit main<1> (in main<1> of main<1>, 0 parents, parent -)".to_string(),
            ]
        );
    }
}
