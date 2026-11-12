use std::collections::{HashMap, HashSet};

use rue_diagnostic::{Applicability, Lint, Name, Note, Suggestion};
use rue_hir::{Database, Declaration, FunctionKind, Hir, HirId, Statement, Symbol, SymbolId};

use crate::{HirLintPass, LintContext, LintInput, text::keyword_before};

/// The number of HIR nodes that an inline function's body can be duplicated by (its size, for
/// each use after the first) before it's reported by `large_inline`.
const LARGE_INLINE_DUPLICATION: usize = 200;

/// The smallest body that `large_inline` reports, however many times it's used, since inlining a
/// small function is usually cheaper than calling it.
const LARGE_INLINE_SIZE: usize = 40;

/// Reports inline constants and bindings that are only used once, which the compiler would
/// inline anyway, and large inline functions that are used in several places.
#[derive(Debug)]
pub(crate) struct Inlining {
    entrypoints: HashSet<Declaration>,
    /// Where each symbol is referenced.
    references: HashMap<SymbolId, Vec<Reference>>,
    /// The `inline let` bindings, along with the function they're declared in.
    bindings: Vec<(SymbolId, Option<SymbolId>)>,
}

#[derive(Debug, Clone, Copy)]
struct Reference {
    function: Option<SymbolId>,
    /// Whether the reference is inside of an inline function, or a struct field's default value,
    /// either of which can be evaluated in more than one place.
    duplicated: bool,
}

impl Inlining {
    pub(crate) fn new(input: &LintInput<'_>) -> Self {
        Self {
            entrypoints: input.entrypoints.clone(),
            references: HashMap::new(),
            bindings: Vec::new(),
        }
    }
}

impl HirLintPass for Inlining {
    fn lints(&self) -> &'static [Lint] {
        &[Lint::RedundantInline, Lint::LargeInline]
    }

    fn check_hir(&mut self, cx: &mut LintContext<'_>, _hir: HirId, node: &Hir) {
        let Hir::Reference(symbol) = *node else {
            return;
        };

        let db = cx.db();

        let duplicated = matches!(cx.enclosing_declaration(), Some(Declaration::Type(_)))
            || cx.walk.functions.iter().any(|&function| {
                matches!(
                    db.symbol(function),
                    Symbol::Function(function) if function.kind == FunctionKind::Inline
                )
            });

        self.references.entry(symbol).or_default().push(Reference {
            function: cx.enclosing_function(),
            duplicated,
        });
    }

    fn check_statement(&mut self, cx: &mut LintContext<'_>, block: &rue_hir::Block, index: usize) {
        if let Statement::Let(symbol, _) = block.statements[index]
            && let Symbol::Binding(binding) = cx.db().symbol(symbol)
            && binding.inline
        {
            self.bindings.push((symbol, cx.enclosing_function()));
        }
    }

    fn finish(&mut self, cx: &mut LintContext<'_>) {
        let mut reports = Vec::new();

        for &(symbol, function) in &self.bindings {
            let Symbol::Binding(binding) = cx.db().symbol(symbol) else {
                continue;
            };

            // A binding used in a lambda is captured by it rather than inlined, so `inline` can
            // change how it's evaluated
            if let Some(name) = binding.name.clone()
                && let [reference] = self.references(symbol)
                && reference.function == function
            {
                reports.push(Report::Redundant(name, "binding", "let"));
            }
        }

        for declaration in cx.db().relevant_declarations() {
            let Declaration::Symbol(symbol) = declaration else {
                continue;
            };

            if self.entrypoints.contains(&declaration) {
                continue;
            }

            match cx.db().symbol(symbol) {
                Symbol::Constant(constant) if constant.inline => {
                    if let Some(name) = constant.name.clone()
                        && let [reference] = self.references(symbol)
                        && !reference.duplicated
                    {
                        reports.push(Report::Redundant(name, "constant", "const"));
                    }
                }
                Symbol::Function(function) if function.kind == FunctionKind::Inline => {
                    let uses = self.references(symbol).len();

                    let Some(name) = function.name.clone() else {
                        continue;
                    };

                    if uses < 2 {
                        continue;
                    }

                    let size = body_size(cx.db(), function.body);

                    if size >= LARGE_INLINE_SIZE && size * (uses - 1) >= LARGE_INLINE_DUPLICATION {
                        reports.push(Report::Large(name, uses));
                    }
                }
                _ => {}
            }
        }

        for report in reports {
            report.emit(cx);
        }
    }
}

impl Inlining {
    fn references(&self, symbol: SymbolId) -> &[Reference] {
        self.references.get(&symbol).map_or(&[], Vec::as_slice)
    }
}

enum Report {
    /// An inline constant or binding that's only used once, along with what it is and the keyword
    /// that comes after `inline`.
    Redundant(Name, &'static str, &'static str),
    /// A large inline function, along with the number of places it's used.
    Large(Name, usize),
}

impl Report {
    fn emit(self, cx: &mut LintContext<'_>) {
        match self {
            Self::Redundant(name, kind, keyword) => {
                let Some(srcloc) = name.srcloc().cloned() else {
                    return;
                };

                let Some(diagnostic) = cx.report(
                    Lint::RedundantInline,
                    srcloc.clone(),
                    format!(
                        "Unnecessary `inline`, since {kind} `{}` is only used once, so it's inlined anyway",
                        name.text()
                    ),
                ) else {
                    return;
                };

                if let Some(inline) = keyword_before(&srcloc, "inline", keyword) {
                    diagnostic.suggestions.push(Suggestion::replace(
                        "Remove `inline`",
                        inline,
                        "",
                        Applicability::MachineApplicable,
                    ));
                }
            }
            Self::Large(name, uses) => {
                let Some(srcloc) = name.srcloc().cloned() else {
                    return;
                };

                let Some(diagnostic) = cx.report(
                    Lint::LargeInline,
                    srcloc,
                    format!(
                        "Inline function `{}` is large, and its body is duplicated in each of the {uses} places it's used",
                        name.text()
                    ),
                ) else {
                    return;
                };

                diagnostic.notes.push(Note::new(
                    "Removing `inline` would define it once and call it instead, which is usually smaller",
                ));
            }
        }
    }
}

/// The number of HIR nodes in a function's body, including the values of its bindings.
fn body_size(db: &Database, body: HirId) -> usize {
    let mut visited = HashSet::new();
    let mut stack = vec![body];

    while let Some(hir) = stack.pop() {
        if !visited.insert(hir) {
            continue;
        }

        match db.hir(hir).clone() {
            Hir::Unresolved
            | Hir::Nil
            | Hir::String(_)
            | Hir::Int(_)
            | Hir::Bytes(_)
            | Hir::Bool(_)
            | Hir::Reference(_)
            | Hir::Lambda(_)
            | Hir::InfinityG1
            | Hir::InfinityG2 => {}
            Hir::Const(expr) => stack.extend(expr.result.or(Some(expr.value))),
            Hir::List(list) => stack.extend(list.items),
            Hir::Struct(initializer) => {
                stack.extend(initializer.fields.into_iter().map(|field| field.value));
            }
            Hir::FieldAccess(access) => stack.push(access.value),
            Hir::Guard(guard) => stack.push(guard.value),
            Hir::Cast(hir) | Hir::Unary(_, hir) | Hir::ClvmOp(_, hir) => stack.push(hir),
            Hir::Block(block) => {
                for statement in block.statements {
                    match statement {
                        Statement::Let(symbol, _) => {
                            if let Symbol::Binding(binding) = db.symbol(symbol) {
                                stack.push(binding.value.hir);
                            }
                        }
                        Statement::Expr(statement) => stack.push(statement.hir),
                        Statement::If(statement) => {
                            stack.extend([statement.condition, statement.then]);
                        }
                        Statement::Return(hir, _)
                        | Statement::Assert(hir, _)
                        | Statement::Debug(hir, _) => stack.push(hir),
                        Statement::Raise(hir, _) => stack.extend(hir),
                    }
                }
                stack.extend(block.body);
            }
            Hir::Pair(a, b) | Hir::Binary(_, a, b) => stack.extend([a, b]),
            Hir::If(a, b, c, _)
            | Hir::CoinId(a, b, c)
            | Hir::Modpow(a, b, c)
            | Hir::Secp256K1Verify(a, b, c)
            | Hir::Secp256R1Verify(a, b, c) => stack.extend([a, b, c]),
            Hir::FunctionCall(call) => {
                stack.push(call.function);
                stack.extend(call.args);
            }
            Hir::Substr(a, b, c) => {
                stack.extend([a, b]);
                stack.extend(c);
            }
            Hir::G1Map(a, b) | Hir::G2Map(a, b) => {
                stack.push(a);
                stack.extend(b);
            }
            Hir::BlsPairingIdentity(args) => stack.extend(args),
            Hir::BlsVerify(a, args) => {
                stack.push(a);
                stack.extend(args);
            }
        }
    }

    visited.len()
}
