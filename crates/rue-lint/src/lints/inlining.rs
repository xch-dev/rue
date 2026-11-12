use std::collections::{HashMap, HashSet};

use rue_diagnostic::{Applicability, Lint, Name, Note, Suggestion};
use rue_hir::{
    Database, Declaration, DependencyGraph, FunctionKind, Hir, HirId, Statement, Symbol, SymbolId,
};

use rue_ast::{AstBinding, AstConstantItem, AstLetStmt, AstNode};
use rue_diagnostic::SrcLoc;

use crate::{
    HirLintPass, LintContext, LintInput, NameKey,
    cost::{COST_PER_BYTE, call_overhead},
    measure::{program_graph, program_size},
    name_key,
    text::keyword_locations,
};

/// Reports inline constants and bindings that are only used once, which the compiler would
/// inline anyway, and large inline functions that are used in several places.
#[derive(Debug)]
pub(crate) struct Inlining<'a> {
    input: &'a LintInput<'a>,
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

impl<'a> Inlining<'a> {
    pub(crate) fn new(input: &'a LintInput<'a>) -> Self {
        Self {
            input,
            references: HashMap::new(),
            bindings: Vec::new(),
        }
    }
}

impl HirLintPass for Inlining<'_> {
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
        let mut large = Vec::new();

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
                reports.push(Report::Redundant(name, "binding"));
            }
        }

        for declaration in cx.db().relevant_declarations() {
            let Declaration::Symbol(symbol) = declaration else {
                continue;
            };

            if self.input.entrypoints.contains(&declaration) {
                continue;
            }

            match cx.db().symbol(symbol) {
                Symbol::Constant(constant) if constant.inline => {
                    if let Some(name) = constant.name.clone()
                        && let [reference] = self.references(symbol)
                        && !reference.duplicated
                    {
                        reports.push(Report::Redundant(name, "constant"));
                    }
                }
                Symbol::Function(function) if function.kind == FunctionKind::Inline => {
                    let uses = self.references(symbol).len();

                    if uses >= 2
                        && let Some(name) = function.name.clone()
                    {
                        large.push((symbol, name, uses, function.parameters.len()));
                    }
                }
                _ => {}
            }
        }

        // Measuring the programs requires code that compiled without errors
        if !self.input.has_errors && !large.is_empty() && cx.is_enabled(Lint::LargeInline) {
            let min_savings = self.input.options.large_inline_min_savings.max(1);
            let mut programs = None;

            for (symbol, name, uses, parameters) in large {
                // The programs aren't measured for functions where the lint is allowed
                if !name
                    .srcloc()
                    .is_some_and(|srcloc| cx.is_enabled_at(Lint::LargeInline, srcloc))
                {
                    continue;
                }

                // The inline functions that use it are inlined along with it
                let inline_users = self
                    .references(symbol)
                    .iter()
                    .filter_map(|reference| reference.function)
                    .filter(|&function| {
                        matches!(
                            cx.db().symbol(function),
                            Symbol::Function(function) if function.kind == FunctionKind::Inline
                        )
                    })
                    .collect::<HashSet<_>>();

                let programs = programs
                    .get_or_insert_with(|| Programs::new(cx.db(), self.input.programs(cx.db())));

                let savings =
                    programs.removing_inline_saves(cx, symbol, &inline_users, uses, parameters);

                if savings >= min_savings {
                    reports.push(Report::Large(name, uses, savings));
                }
            }
        }

        // Finding the keywords walks every document, so it's only done if there's something to
        // report
        if reports.is_empty() {
            return;
        }

        let keywords = inline_keywords(self.input);

        for report in reports {
            report.emit(cx, &keywords);
        }
    }
}

impl Inlining<'_> {
    fn references(&self, symbol: SymbolId) -> &[Reference] {
        self.references.get(&symbol).map_or(&[], Vec::as_slice)
    }
}

enum Report {
    /// An inline constant or binding that's only used once, along with what it is.
    Redundant(Name, &'static str),
    /// A large inline function, along with the number of places it's used, and the estimated cost
    /// that removing `inline` would save.
    Large(Name, usize, u64),
}

impl Report {
    fn emit(self, cx: &mut LintContext<'_>, keywords: &HashMap<NameKey, SrcLoc>) {
        match self {
            Self::Redundant(name, kind) => {
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

                if let Some(inline) = name_key(&name).and_then(|key| keywords.get(&key)) {
                    diagnostic.suggestions.push(Suggestion::replace(
                        "Remove `inline`",
                        inline.clone(),
                        "",
                        Applicability::MachineApplicable,
                    ));
                }
            }
            Self::Large(name, uses, savings) => {
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

                diagnostic.notes.push(Note::new(format!(
                    "Removing `inline` would define it once and call it instead, which saves an estimated cost of {savings}"
                )));
            }
        }
    }
}

/// The programs that are compiled from the entrypoints (besides tests), which are measured to
/// find out what removing `inline` from a function would save.
struct Programs {
    /// Each program, along with its dependencies.
    graphs: Vec<(SymbolId, DependencyGraph)>,
    /// The size of each program as it is, which is the same for every function.
    sizes: HashMap<SymbolId, Option<u64>>,
}

impl Programs {
    fn new(db: &Database, programs: &[SymbolId]) -> Self {
        Self {
            graphs: programs
                .iter()
                .map(|&program| (program, program_graph(db, program)))
                .collect(),
            sizes: HashMap::new(),
        }
    }

    /// The cost that removing `inline` from a function would save, from the difference in the
    /// size of each program that uses it, less the cost of calling it in each place it's used.
    fn removing_inline_saves(
        &mut self,
        cx: &mut LintContext<'_>,
        symbol: SymbolId,
        inline_users: &HashSet<SymbolId>,
        uses: usize,
        parameters: usize,
    ) -> u64 {
        let mut saved_bytes: i64 = 0;

        for (program, graph) in &self.graphs {
            let program = *program;

            // A function that's only used once in a program is inlined whether it's marked as
            // `inline` or not, unless it's used by an inline function that's inlined in several
            // places, so the program is only measured if it could change
            if graph.references(symbol) < 2
                && !inline_users.iter().any(|&user| graph.references(user) > 0)
            {
                continue;
            }

            let before = *self
                .sizes
                .entry(program)
                .or_insert_with(|| program_size(cx.db_mut(), program));

            let after = with_kind(cx.db_mut(), symbol, FunctionKind::BinaryTree, |db| {
                program_size(db, program)
            });

            let (Some(before), Some(after)) = (before, after) else {
                return 0;
            };

            saved_bytes += before.cast_signed() - after.cast_signed();
        }

        let Ok(saved_bytes) = u64::try_from(saved_bytes) else {
            return 0;
        };

        saved_bytes
            .saturating_mul(COST_PER_BYTE)
            .saturating_sub(call_overhead(parameters).runtime * uses as u64)
    }
}

/// Runs the closure with the function changed to the given kind, and changes it back afterward.
fn with_kind<T>(
    db: &mut Database,
    symbol: SymbolId,
    kind: FunctionKind,
    f: impl FnOnce(&mut Database) -> T,
) -> T {
    let Symbol::Function(function) = db.symbol_mut(symbol) else {
        return f(db);
    };

    let original = std::mem::replace(&mut function.kind, kind);
    let result = f(db);

    if let Symbol::Function(function) = db.symbol_mut(symbol) {
        function.kind = original;
    }

    result
}

/// The location of the `inline` keyword (and the space after it) of each inline constant and
/// binding with a name, by the location of the name.
fn inline_keywords(input: &LintInput<'_>) -> HashMap<NameKey, SrcLoc> {
    keyword_locations(input, |node| {
        if let Some(item) = AstConstantItem::cast(node.clone()) {
            Some((item.inline()?, item.name()?))
        } else if let Some(stmt) = AstLetStmt::cast(node.clone()) {
            match stmt.binding()? {
                AstBinding::NamedBinding(binding) => Some((stmt.inline()?, binding.name()?)),
                _ => None,
            }
        } else {
            None
        }
    })
}
