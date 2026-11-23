use std::{
    cmp::Reverse,
    collections::{HashMap, HashSet},
    fmt, mem,
    ops::{Deref, DerefMut, Range},
    panic::Location,
    path::{Path, PathBuf},
    rc::Rc,
    sync::Arc,
};

use num_bigint::{BigInt, Sign};
use rowan::{TextRange, TextSize};
use rue_ast::{AstDocument, AstNode};
use rue_diagnostic::{
    Applicability, Diagnostic, DiagnosticKind, DiagnosticSeverity, Name, Note, Source, SourceKind,
    SrcLoc, Suggestion,
};
use rue_hir::{
    Builtins, Constraint, Database, DatabaseCheckpoint, Declaration, HirId, ImportId, ImportUser,
    Item, Items, Mappings, Namespace, Scope, ScopeId, Symbol, SymbolId, TypePath, Value,
    merge_mappings, replace_type,
};
use rue_lint::{
    EnclosedPreludeReference, ImpreciseNarrowing, LintInput, LintSites, LintSitesCheckpoint,
};
use rue_lir::{ClvmTarget, bigint_atom};
use rue_options::CompilerOptions;
use rue_parser::{SyntaxElement, SyntaxKind, SyntaxNode, SyntaxToken};
use rue_types::{CallSolver, Check, CheckError, Comparison, Inference, TooComplex, Type, TypeId};

use crate::{
    DeferredLambda, ElementLambda, FileTree, LambdaParameter, LiteralValue, SyntaxItem,
    SyntaxItemKind, SyntaxMap, narrowed_with_references,
};

/// How the type expected of the expression being compiled is used, which the expressions it's
/// passed down to inherit.
#[derive(Debug, Default, Clone, Copy)]
struct ExpectedTypeUse {
    /// Whether it's only a hint, which the value doesn't have to be assignable to (see
    /// `Compiler::with_expected_type_hint`).
    hinted: bool,
    /// Whether it could still be widened before the value is checked against it (see
    /// `Compiler::with_expected_type_provisional`).
    provisional: bool,
    /// Whether it's only for the generics of the calls in it that nothing else gives a type (see
    /// `Compiler::with_expected_type_fallback`).
    fallback: bool,
}

/// What [`Compiler::checkpoint`] saves.
#[derive(Debug, Clone)]
pub(crate) struct Checkpoint {
    db: DatabaseCheckpoint,
    diagnostics: usize,
    syntax_items: usize,
    lint_sites: LintSitesCheckpoint,
    named_types: usize,
    unchecked_type_items: usize,
    pending_defaults: usize,
    scope_stack: Vec<(TextSize, ScopeId)>,
    declaration_stack: Vec<Declaration>,
    return_stack: Vec<(Option<ReturnTarget>, usize)>,
    return_count: usize,
    call_frames: Vec<CallFrame>,
    expected: ExpectedTypeUse,
    argument_requirement: Option<(TextRange, TypeId)>,
    undo: usize,
    uninferred_parameters: usize,
    named_generics: Vec<NamedGeneric>,
    compiling_default: Option<(TypeId, String)>,
    first_placeholder: Option<&'static Location<'static>>,
    cancelled: bool,
}

#[derive(Debug, Clone)]
pub struct Compiler {
    options: CompilerOptions,
    source: Source,
    diagnostics: Vec<Diagnostic>,
    db: Database,
    syntax_map: SyntaxMap,
    scope_stack: Vec<(TextSize, ScopeId)>,
    /// The directory modules that the file being compiled is in.
    module_stack: Vec<SymbolId>,
    /// The module of the file being compiled, and the scope outside of it (with the prelude), if
    /// a file is being compiled.
    file: Option<(SymbolId, ScopeId)>,
    /// The inline modules that the code being compiled is in, in the file being compiled.
    inline_modules: Vec<SymbolId>,
    /// The scope with the sibling modules of each file, keyed by the scope of the file.
    sibling_scopes: HashMap<ScopeId, ScopeId>,
    /// The items that failed imports bind their names to (see [`Self::is_error_item`]).
    error_items: HashSet<Item>,
    builtins: Builtins,
    field_types: HashMap<TypeId, Vec<TypeId>>,
    declared_variants: HashMap<TypeId, Vec<Option<EnumVariant>>>,
    inferred_fields: HashSet<(TypeId, String)>,
    /// The fields of each struct that have a default value, which is known before the default
    /// values are compiled.
    declared_defaults: HashSet<(TypeId, String)>,
    /// The field whose default value is being compiled, if any.
    compiling_default: Option<(TypeId, String)>,
    /// The defaults that each default value uses, by the field it belongs to.
    default_uses: HashMap<(TypeId, String), Vec<(TypeId, String)>>,
    /// The fields of struct initializers in default values whose own defaults hadn't been
    /// compiled yet (see `resolve_pending_defaults`).
    pending_defaults: Vec<PendingDefault>,
    declaration_stack: Vec<Declaration>,
    registered_scopes: HashSet<ScopeId>,
    external_programs: HashMap<PathBuf, Vec<u8>>,
    return_stack: Vec<(Option<ReturnTarget>, usize)>,
    return_count: usize,
    /// The calls whose generics are being inferred from their arguments, innermost last.
    call_frames: Vec<CallFrame>,
    /// The parameters of the lambdas being compiled whose types weren't inferred, as the generic
    /// each was left as and its name (see [`Compiler::name_uninferred_generics`]).
    uninferred_parameters: Vec<(TypeId, String)>,
    /// The generics that weren't inferred yet in the types that were named for the next error.
    named_generics: Vec<NamedGeneric>,
    /// The errors that show generics of a call that weren't inferred by then, in the order
    /// they're in the diagnostics (see [`Compiler::errors_since`]).
    uninferred_errors: Vec<UninferredError>,
    /// The diagnostics, by where they are, that follow from an error that's reported, and so
    /// aren't themselves (see [`Compiler::suppress_uninferred_errors_since`]).
    suppressed: HashSet<usize>,
    /// How the type expected of the expression being compiled is used.
    expected: ExpectedTypeUse,
    /// A type that the call argument at the range has to be assignable to however the enclosing
    /// call's generics are solved, if there is one (see `Compiler::with_argument_requirement`).
    argument_requirement: Option<(TextRange, TypeId)>,
    lint_sites: LintSites,
    /// The function of each `for` expression that makes a list which ends with nil, by the node
    /// of its call, so that a list it's spread in can have it end with the items after it (see
    /// `end_list_loop_with`).
    list_loops: HashMap<HirId, SymbolId>,
    ambiguous_imports: HashSet<(ImportId, Declaration)>,
    /// Every type a path has named, in the order they were compiled, so that what's compiled between
    /// two points can be told to name a type (see `LambdaBody::names_types`).
    named_types: Vec<TypeId>,
    /// The symbols whose type is overridden in any scope, so that looking up the type of any other
    /// symbol doesn't need to walk every enclosing scope (and each `let` statement opens one).
    overridden_symbols: HashSet<SymbolId>,
    /// The types that narrowing gave values whose types keep the references of untyped lambda
    /// parameters' types, each with the parts of the type before it was narrowed that make it up,
    /// which keep them (see `narrowed_with_references`).
    narrowed_references: HashMap<TypeId, TypeId>,
    /// The type items compiled since their contractiveness was last rechecked, which happens once
    /// every type item in a tree has been compiled (see `recheck_contractive_types`).
    unchecked_type_items: Vec<TypeId>,
    cancel: Option<Cancel>,
    cancelled: bool,
    /// Where the first placeholder for code that couldn't be compiled was made (see
    /// [`Self::unresolved`]), to check that it's covered by an error before generating code.
    first_placeholder: Option<&'static Location<'static>>,
    /// What was added to the sets and maps that can be added to for something from before a
    /// checkpoint, in order, for [`Self::restore`] to take out again.
    undo: Vec<Undo>,
    /// How many lambdas that feed themselves are being compiled in steps (see
    /// `compile_lambda_feeding_itself`).
    feeding_depth: usize,
}

/// Something added to one of the compiler's sets or maps (see `Compiler::undo`).
#[derive(Debug, Clone)]
enum Undo {
    OverriddenSymbol(SymbolId),
    /// The type, and what it was recorded for before.
    NarrowedReference(TypeId, Option<TypeId>),
    InferredField((TypeId, String)),
    /// The field whose default value uses another.
    DefaultUse((TypeId, String)),
}

/// Says whether a compile should stop partway, because nothing needs its result anymore (for
/// example, because the language server's files were edited while it ran). It's asked between
/// phases and before each declaration's body is compiled, so it should be cheap.
#[derive(Clone)]
pub struct Cancel(Arc<dyn Fn() -> bool + Send + Sync>);

impl Cancel {
    pub fn new(cancelled: impl Fn() -> bool + Send + Sync + 'static) -> Self {
        Self(Arc::new(cancelled))
    }
}

impl fmt::Debug for Cancel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("Cancel")
    }
}

/// A call whose generics are being inferred from its arguments.
#[derive(Debug, Clone)]
struct CallFrame {
    generics: Vec<TypeId>,
    /// The argument being compiled, if any.
    argument: Option<ArgumentFrame>,
}

/// An argument of a call being compiled, while the call's generics are inferred. The elements of
/// a pair or list in it add what they infer to `infer`, so that a later element with a lambda in
/// it, or a call with no arguments, or a pair or list with one in it, can be compiled against it
/// (#350, see `refine_element_expected`).
#[derive(Debug, Clone)]
pub struct ArgumentFrame {
    /// The candidates of the arguments compiled before this one, and of this one's elements so far.
    pub infer: Inference,
    /// The same, but with what the elements infer from their types with the caller's generics that
    /// their lambdas' untyped parameters were compiled as in place of the callee's renamed ones (see
    /// `restored`) as those (see `unrestore_lambdas`), so that it only names a caller's generic
    /// where that's the caller's own. It's only used to tell which those are (see
    /// `own_generics_of`).
    pub own_infer: Inference,
    pub solver: Rc<CallSolver>,
    /// The callee's renamed generics (see `rename_generics_in_scope`), and the caller's generics
    /// they were renamed from, to restore them again after refining an element. It's only filled
    /// when `restored` isn't empty.
    pub renamed: HashMap<TypeId, TypeId>,
    /// The caller's generics that the argument's expected type has in place of the callee's
    /// renamed generics that weren't inferred yet (see `restore_renamed`), each with the generic it
    /// stands for. It's only those that nothing else in the expected type gave, only for a pair or
    /// list argument with a lambda in it, and only while the pairs and lists it's made of are
    /// compiled (see `compile_element`).
    pub restored: HashMap<TypeId, TypeId>,
    /// The caller's generics in `restored` that are the caller's own in the type of an untyped
    /// parameter of a lambda among those elements, by the parameter's reference, rather than the
    /// renamed ones they stand for, since what the elements before it infer for the renamed
    /// generics in its expected type names them (see `own_infer` and `own_generics_of`).
    pub own_generics: Vec<(TypeId, Vec<TypeId>)>,
    /// What the element about to be compiled was refined from (see `refine_element_expected`): the
    /// type it was expected to have with the callee's renamed generics in it, and what `own_infer`
    /// infers for those of them that the refinement gives a type, which a lambda's untyped
    /// parameters get their own generics from (see `own_generics_of`). The element takes it when
    /// it's compiled (see `compile_element`).
    pub element_refinement: Option<(TypeId, HashMap<TypeId, TypeId>)>,
    /// Whether the lambdas among the elements of the pairs and lists the argument is made of are
    /// compiled with their untyped parameters as references (see `compile_lambda`), which is only
    /// while those pairs and lists are compiled (see `compile_element`).
    pub wrap_lambdas: bool,
    /// The untyped parameters of those lambdas (see `compile_lambda`).
    pub lambda_parameters: Vec<LambdaParameter>,
    /// The lambdas among those elements that are only compiled once no argument still to be
    /// compiled can add to the generics their parameters are expected to have, by where they are,
    /// with the type expected of each, before it's substituted (see `DeferredLambda`).
    pub to_defer: HashMap<TextRange, ElementLambda>,
    /// The placeholders left for those lambdas, as they're compiled.
    pub deferred: Vec<DeferredLambda>,
}

/// A variant of an enum, as it was declared.
#[derive(Debug, Clone, Copy)]
pub struct EnumVariant {
    /// The variant's struct type.
    pub ty: TypeId,
    /// The constant for the variant, if it has no fields.
    pub constant: Option<SymbolId>,
}

/// A field of a struct initializer in a default value, whose own default value hadn't been compiled
/// yet, so it's filled in once every default value has been (see `resolve_pending_defaults`).
#[derive(Debug, Clone)]
pub struct PendingDefault {
    /// The struct initializer.
    pub hir: HirId,
    /// The index of the field in the struct initializer.
    pub index: usize,
    /// The struct whose default value it is.
    pub ty: TypeId,
    pub name: String,
    /// Where the struct initializer is, and the scope it's in, for reporting errors.
    pub srcloc: SrcLoc,
    pub scope: ScopeId,
    /// The field whose default value the struct initializer is in.
    pub user: Option<(TypeId, String)>,
}

/// The function or lambda body that a `return` statement exits from.
#[derive(Debug, Clone)]
pub struct ReturnTarget {
    /// The declared return type, if there is one.
    pub expected_type: Option<TypeId>,
    /// The types of every `return` statement compiled for this target so far.
    pub types: Vec<TypeId>,
}

impl Deref for Compiler {
    type Target = Database;

    fn deref(&self) -> &Self::Target {
        &self.db
    }
}

impl DerefMut for Compiler {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.db
    }
}

impl Compiler {
    pub fn new(mut options: CompilerOptions) -> Self {
        // Some lints only warn by default with `hard_fork_2`, which can be set after the levels
        options.lints.apply_target_defaults(options.hard_fork_2);

        let mut db = Database::new();

        let builtins = Builtins::new(&mut db);

        let mut ctx = Self {
            options,
            source: Source::new(Arc::from(""), SourceKind::Std("<placeholder>".to_string())),
            diagnostics: Vec::new(),
            db,
            syntax_map: SyntaxMap::new(),
            scope_stack: vec![(TextSize::from(0), builtins.scope)],
            module_stack: Vec::new(),
            file: None,
            inline_modules: Vec::new(),
            sibling_scopes: HashMap::new(),
            error_items: HashSet::new(),
            builtins,
            field_types: HashMap::new(),
            declared_variants: HashMap::new(),
            inferred_fields: HashSet::new(),
            declared_defaults: HashSet::new(),
            compiling_default: None,
            default_uses: HashMap::new(),
            pending_defaults: Vec::new(),
            declaration_stack: Vec::new(),
            registered_scopes: HashSet::new(),
            external_programs: HashMap::new(),
            return_stack: Vec::new(),
            return_count: 0,
            call_frames: Vec::new(),
            uninferred_parameters: Vec::new(),
            named_generics: Vec::new(),
            uninferred_errors: Vec::new(),
            suppressed: HashSet::new(),
            expected: ExpectedTypeUse::default(),
            argument_requirement: None,
            lint_sites: LintSites::default(),
            list_loops: HashMap::new(),
            ambiguous_imports: HashSet::new(),
            named_types: Vec::new(),
            overridden_symbols: HashSet::new(),
            narrowed_references: HashMap::new(),
            unchecked_type_items: Vec::new(),
            cancel: None,
            cancelled: false,
            first_placeholder: None,
            undo: Vec::new(),
            feeding_depth: 0,
        };

        if options.std {
            let tree = FileTree::load_std(&mut ctx).unwrap();
            tree.compile_impl(&mut ctx, false);
            let prelude_file = tree
                .find(&SourceKind::Std("prelude.rue".to_string()))
                .unwrap();
            let prelude_scope = ctx.module(prelude_file.module).scope;

            let prelude = ctx.alloc_child_scope();

            for namespace in Namespace::ALL {
                for (name, item) in ctx.scope(prelude_scope).exported_items(namespace) {
                    ctx.scope_mut(prelude).insert_item(name, item, false);
                }
            }

            ctx.push_scope(prelude, prelude_file.document.syntax().text_range().start());
        }

        ctx
    }

    pub fn source(&self) -> &Source {
        &self.source
    }

    /// What the programs are compiled for, which decides the flags they're run with.
    pub fn target(&self) -> ClvmTarget {
        clvm_target(&self.options)
    }

    pub fn options(&self) -> &CompilerOptions {
        &self.options
    }

    pub(crate) fn external_program(&self, path: &Path) -> Option<&[u8]> {
        self.external_programs.get(path).map(Vec::as_slice)
    }

    pub(crate) fn cache_external_program(&mut self, path: PathBuf, program: Vec<u8>) {
        self.external_programs.insert(path, program);
    }

    /// Sets what's asked whether to stop compiling partway. A compile that stopped is incomplete,
    /// and only good for being thrown away.
    pub fn set_cancel(&mut self, cancel: Option<Cancel>) {
        self.cancel = cancel;
    }

    /// Whether the compile should stop. Once it should, it stays that way, so that every phase
    /// after it stops too.
    pub fn is_cancelled(&mut self) -> bool {
        if !self.cancelled
            && let Some(cancel) = &self.cancel
        {
            self.cancelled = (cancel.0)();
        }

        self.cancelled
    }

    pub fn set_source(&mut self, source: Source) {
        self.source = source;
    }

    pub fn scope_stack(&self) -> Vec<ScopeId> {
        self.scope_stack.iter().map(|(_, scope)| *scope).collect()
    }

    pub fn syntax_map(&self) -> &SyntaxMap {
        &self.syntax_map
    }

    pub fn add_syntax(&mut self, kind: SyntaxItemKind, span: TextRange) {
        self.add_syntax_for_source(kind, span, self.source.kind.clone());
    }

    pub fn add_syntax_for_source(
        &mut self,
        kind: SyntaxItemKind,
        span: TextRange,
        source_kind: SourceKind,
    ) {
        self.syntax_map
            .add_item(SyntaxItem::new(kind, span, source_kind));
    }

    /// Takes the diagnostics reported so far, in the order they appear in each file. Files are in
    /// the order their first diagnostic was reported.
    pub fn take_diagnostics(&mut self) -> Vec<Diagnostic> {
        let mut diagnostics = mem::take(&mut self.diagnostics);
        self.uninferred_errors.clear();

        // The errors that follow from one that a generic wasn't inferred because of
        let suppressed = mem::take(&mut self.suppressed);

        if !suppressed.is_empty() {
            let mut index = 0;

            diagnostics.retain(|_| {
                index += 1;
                !suppressed.contains(&(index - 1))
            });
        }

        // A diagnostic that shows a type with a part that couldn't be compiled only follows from
        // the error that was reported for that part, as long as there is one that doesn't
        if diagnostics.iter().any(is_root_error) {
            diagnostics.retain(|diagnostic| !mentions_unresolved(diagnostic));
        }

        let mut files = HashMap::new();

        for diagnostic in &diagnostics {
            let next = files.len();
            files
                .entry(diagnostic.srcloc.source.kind.clone())
                .or_insert(next);
        }

        // Lints are ordered by where they're reported and then by lint, so that the order doesn't
        // depend on which pass reports them. Other diagnostics come before lints at the same
        // location, and the sort is stable, so they stay in the order they were reported.
        diagnostics.sort_by_cached_key(|diagnostic| {
            (
                files[&diagnostic.srcloc.source.kind],
                diagnostic.srcloc.span.start,
                diagnostic.srcloc.span.end,
                diagnostic.lint(),
            )
        });

        diagnostics
    }

    /// Where the compile is, to go back to with [`Self::restore`] after compiling something that
    /// isn't kept, which takes as long as what was compiled since, not as what was compiled before:
    /// the diagnostics, the syntax map, the lint sites and what the database records only grow, so
    /// it's where each of them ends (see `rue_hir::DatabaseCheckpoint`), what's added to the sets and
    /// maps that hold things from before is taken out again (see `Undo`), and the state that
    /// compiling an expression changes and puts back, which is only as large as what's being
    /// compiled, is copied.
    ///
    /// Every field is named here, so that one added later doesn't compile until it's said how it
    /// goes back (see [`Self::restore`]).
    pub(crate) fn checkpoint(&self) -> Checkpoint {
        let Self {
            // Settings, and what doesn't depend on the code being compiled
            options: _,
            source: _,
            builtins: _,
            cancel: _,
            external_programs: _,
            // Only grow, so they go back to where they ended
            diagnostics,
            db,
            syntax_map,
            lint_sites,
            named_types,
            unchecked_type_items,
            pending_defaults,
            uninferred_parameters,
            undo,
            // Indexed by where they are in the diagnostics, which `restore` keeps those before
            uninferred_errors: _,
            suppressed: _,
            // Keyed by the scopes and nodes made since, which `restore` clears
            registered_scopes: _,
            list_loops: _,
            // Copied, since compiling an expression changes them and puts them back
            scope_stack,
            declaration_stack,
            return_stack,
            return_count,
            call_frames,
            expected,
            argument_requirement,
            named_generics,
            compiling_default,
            first_placeholder,
            cancelled,
            // Added to for something from before, so what's added is logged in `undo`
            inferred_fields: _,
            default_uses: _,
            overridden_symbols: _,
            narrowed_references: _,
            // Only changed while compiling items and imports, which a lambda's body has none of,
            // or put back by what changes them before it returns (`feeding_depth` around a lambda
            // that's compiled in steps, which is where checkpoints are taken)
            module_stack: _,
            file: _,
            inline_modules: _,
            sibling_scopes: _,
            error_items: _,
            field_types: _,
            declared_variants: _,
            declared_defaults: _,
            ambiguous_imports: _,
            feeding_depth: _,
        } = self;

        Checkpoint {
            db: db.checkpoint(),
            diagnostics: diagnostics.len(),
            syntax_items: syntax_map.len(),
            lint_sites: lint_sites.checkpoint(),
            named_types: named_types.len(),
            unchecked_type_items: unchecked_type_items.len(),
            pending_defaults: pending_defaults.len(),
            scope_stack: scope_stack.clone(),
            declaration_stack: declaration_stack.clone(),
            return_stack: return_stack.clone(),
            return_count: *return_count,
            call_frames: call_frames.clone(),
            expected: *expected,
            argument_requirement: *argument_requirement,
            undo: undo.len(),
            uninferred_parameters: uninferred_parameters.len(),
            named_generics: named_generics.clone(),
            compiling_default: compiling_default.clone(),
            first_placeholder: *first_placeholder,
            cancelled: *cancelled,
        }
    }

    /// Goes back to the checkpoint (see [`Self::checkpoint`]).
    pub(crate) fn restore(&mut self, checkpoint: &Checkpoint) {
        let hirs = self.db.hirs_since(&checkpoint.db);
        for scope in self.db.scopes_since(&checkpoint.db) {
            self.registered_scopes.remove(&scope);
        }
        for hir in &hirs {
            self.list_loops.remove(hir);
        }
        self.lint_sites.restore(&checkpoint.lint_sites, &hirs);
        self.db.restore(&checkpoint.db);
        self.diagnostics.truncate(checkpoint.diagnostics);

        // What's kept of the errors by where they are in the diagnostics, which are in that order
        let errors = self
            .uninferred_errors
            .partition_point(|error| error.index < checkpoint.diagnostics);
        self.uninferred_errors.truncate(errors);
        if !self.suppressed.is_empty() {
            self.suppressed
                .retain(|&index| index < checkpoint.diagnostics);
        }
        self.uninferred_parameters
            .truncate(checkpoint.uninferred_parameters);
        self.named_generics.clone_from(&checkpoint.named_generics);
        self.compiling_default
            .clone_from(&checkpoint.compiling_default);
        self.syntax_map.truncate(checkpoint.syntax_items);
        self.named_types.truncate(checkpoint.named_types);
        self.unchecked_type_items
            .truncate(checkpoint.unchecked_type_items);
        self.pending_defaults.truncate(checkpoint.pending_defaults);
        self.scope_stack.clone_from(&checkpoint.scope_stack);
        self.declaration_stack
            .clone_from(&checkpoint.declaration_stack);
        self.return_stack.clone_from(&checkpoint.return_stack);
        self.return_count = checkpoint.return_count;
        self.call_frames.clone_from(&checkpoint.call_frames);
        self.expected = checkpoint.expected;
        self.argument_requirement = checkpoint.argument_requirement;
        for undo in self.undo.split_off(checkpoint.undo).into_iter().rev() {
            match undo {
                Undo::OverriddenSymbol(symbol) => {
                    self.overridden_symbols.remove(&symbol);
                }
                Undo::NarrowedReference(narrowed, Some(raw)) => {
                    self.narrowed_references.insert(narrowed, raw);
                }
                Undo::NarrowedReference(narrowed, None) => {
                    self.narrowed_references.remove(&narrowed);
                }
                Undo::InferredField(field) => {
                    self.inferred_fields.remove(&field);
                }
                Undo::DefaultUse(user) => {
                    if let Some(uses) = self.default_uses.get_mut(&user) {
                        uses.pop();

                        if uses.is_empty() {
                            self.default_uses.remove(&user);
                        }
                    }
                }
            }
        }
        self.first_placeholder = checkpoint.first_placeholder;
        self.cancelled = checkpoint.cancelled;
    }

    /// The number of diagnostics reported so far, which [`Self::diagnostics_since_mut`] takes.
    pub fn diagnostic_count(&self) -> usize {
        self.diagnostics.len()
    }

    /// The diagnostics that have been reported since there were `count` of them, so that notes
    /// can be added to them.
    pub fn diagnostics_since_mut(&mut self, count: usize) -> &mut [Diagnostic] {
        &mut self.diagnostics[count..]
    }

    /// The diagnostics that have been reported since there were `count` of them.
    pub(crate) fn diagnostics_since(&self, count: usize) -> &[Diagnostic] {
        &self.diagnostics[count..]
    }

    /// Where the errors are that have been reported since there were `count` diagnostics and
    /// that a generic of a call might not have been inferred because of. That leaves out the
    /// errors that follow from another one: those that show a type with a part that couldn't be
    /// compiled (see [`Self::take_diagnostics`]), and those that show a generic that wasn't
    /// inferred, which are there because of that rather than the other way around.
    pub(crate) fn errors_since(&self, count: usize) -> Vec<Range<usize>> {
        self.diagnostics
            .iter()
            .enumerate()
            .skip(count)
            .filter(|(index, diagnostic)| {
                is_root_error(diagnostic) && self.uninferred_error(*index).is_err()
            })
            .map(|(_, diagnostic)| diagnostic.srcloc.span.clone())
            .collect()
    }

    /// Where the error at the index is among the ones that show a generic that wasn't inferred,
    /// or where it would be.
    fn uninferred_error(&self, index: usize) -> Result<usize, usize> {
        self.uninferred_errors
            .binary_search_by_key(&index, |error| error.index)
    }

    /// Records that the errors reported since there were `count` diagnostics show the generics
    /// of a call, which weren't inferred (see [`Self::errors_since`]).
    pub(crate) fn mark_uninferred_errors_since(&mut self, count: usize, generics: &[TypeId]) {
        for index in count..self.diagnostics.len() {
            match self.uninferred_error(index) {
                Ok(position) => self.uninferred_errors[position].generics.extend(generics),
                Err(position) => self.uninferred_errors.insert(
                    position,
                    UninferredError {
                        index,
                        generics: generics.to_vec(),
                        settled: false,
                    },
                ),
            }
        }
    }

    /// Leaves out the errors reported since there were `count` diagnostics that only show
    /// generics which weren't inferred because of another error that's reported, given those
    /// generics. They're there because the generics weren't inferred, so that error is the one
    /// to fix, as `map(missing, fn(x) => x + 1)` only needs `missing` declared, rather than a
    /// type for `x`.
    ///
    /// An error that a call in the arguments left as it is stays, if it shows that call's
    /// generics (see [`Self::settle_uninferred_errors_since`]).
    pub(crate) fn suppress_uninferred_errors_since(&mut self, count: usize, generics: &[TypeId]) {
        let start = self.uninferred_error(count).unwrap_or_else(|start| start);

        for error in &self.uninferred_errors[start..] {
            if !error.settled
                && error
                    .generics
                    .iter()
                    .all(|generic| generics.contains(generic))
            {
                self.suppressed.insert(error.index);
            }
        }
    }

    /// Records that the call whose generics are given is done with the errors reported since
    /// there were `count` diagnostics that show any of them: it reported that a generic couldn't
    /// be inferred, or left them out with another error. Every call of a function has the same
    /// generics, so a call that this one is an argument of, or is in a lambda that's passed to,
    /// would otherwise take an error about this call's generic to be about its own.
    pub(crate) fn settle_uninferred_errors_since(&mut self, count: usize, generics: &[TypeId]) {
        let start = self.uninferred_error(count).unwrap_or_else(|start| start);

        for error in &mut self.uninferred_errors[start..] {
            if error
                .generics
                .iter()
                .any(|generic| generics.contains(generic))
            {
                error.settled = true;
            }
        }
    }

    pub(crate) fn feeding_depth(&self) -> usize {
        self.feeding_depth
    }

    pub(crate) fn set_feeding_depth(&mut self, depth: usize) {
        self.feeding_depth = depth;
    }

    /// Whether any of the diagnostics reported since there were `count` of them is an error.
    pub fn has_errors_since(&self, count: usize) -> bool {
        self.diagnostics[count..]
            .iter()
            .any(|diagnostic| diagnostic.severity == DiagnosticSeverity::Error)
    }

    pub fn has_errors(&self) -> bool {
        self.diagnostics
            .iter()
            .any(|diagnostic| diagnostic.severity == DiagnosticSeverity::Error)
    }

    /// The placeholder for code that couldn't be compiled. It doesn't report an error of its own,
    /// so every use of it must be covered by an error that has been or will be reported, since
    /// its type is assignable to and from every type and its value can't be generated.
    #[track_caller]
    pub fn unresolved(&mut self) -> Value {
        self.note_placeholder();
        self.builtins.unresolved.clone()
    }

    /// Records that a placeholder for code that couldn't be compiled was made here, other than
    /// with [`Self::unresolved`] (see [`Self::first_placeholder`]).
    #[track_caller]
    pub(crate) fn note_placeholder(&mut self) {
        self.first_placeholder.get_or_insert(Location::caller());
    }

    /// Where the first placeholder for code that couldn't be compiled was made, if any. Code is
    /// only generated without errors, so there shouldn't be one by then.
    pub fn first_placeholder(&self) -> Option<&'static Location<'static>> {
        self.first_placeholder
    }

    pub fn builtins(&self) -> &Builtins {
        &self.builtins
    }

    /// Reports an error, which is returned so that notes and suggestions can be added to it.
    pub fn diagnostic(
        &mut self,
        node: &impl GetTextRange,
        kind: DiagnosticKind,
    ) -> &mut Diagnostic {
        let srcloc = self.srcloc(node);
        self.diagnostic_at(srcloc, kind)
    }

    pub(crate) fn push_unchecked_type_item(&mut self, ty: TypeId) {
        self.unchecked_type_items.push(ty);
    }

    pub(crate) fn take_unchecked_type_items(&mut self) -> Vec<TypeId> {
        std::mem::take(&mut self.unchecked_type_items)
    }

    pub fn diagnostic_name(&mut self, name: &Name, kind: DiagnosticKind) {
        if let Some(srcloc) = name.srcloc().cloned() {
            self.diagnostic_at(srcloc, kind);
        }
    }

    /// Reports an error, which is returned so that notes and suggestions can be added to it. Lints
    /// are reported separately, by the passes in [`rue_lint`].
    pub fn diagnostic_at(&mut self, srcloc: SrcLoc, kind: DiagnosticKind) -> &mut Diagnostic {
        debug_assert!(
            kind.lint().is_none(),
            "lints must be reported by a lint pass, so that their level is respected"
        );

        let mut diagnostic = Diagnostic::new(srcloc, kind, DiagnosticSeverity::Error);

        // A type can be named without an error being reported for it, so the generics that were
        // named only count for an error that shows them
        if !self.named_generics.is_empty() {
            let message = diagnostic.kind.to_string();
            let mut shown = Vec::new();

            for generic in mem::take(&mut self.named_generics) {
                if shows_type(&message, &generic.name) {
                    shown.push(generic.id);
                    diagnostic.notes.extend(generic.note);
                }
            }

            if !shown.is_empty() {
                self.uninferred_errors.push(UninferredError {
                    index: self.diagnostics.len(),
                    generics: shown,
                    settled: false,
                });
            }
        }

        self.diagnostics.push(diagnostic);
        self.diagnostics
            .last_mut()
            .expect("the diagnostic was just pushed")
    }

    /// How many parameters there are whose types weren't inferred, which is what
    /// [`Self::truncate_uninferred_parameters`] takes once the lambda they're of is compiled.
    pub(crate) fn uninferred_parameter_count(&self) -> usize {
        self.uninferred_parameters.len()
    }

    /// Adds a parameter of the lambda being compiled that has no type written and whose type
    /// wasn't inferred, so it's left as (or as a type with) a generic of the function that the
    /// lambda is passed to.
    pub(crate) fn push_uninferred_parameter(&mut self, generic: TypeId, name: &str) {
        let known = self
            .uninferred_parameters
            .iter()
            .any(|(other, parameter)| *other == generic && parameter == name);

        if !known {
            self.uninferred_parameters.push((generic, name.to_string()));
        }
    }

    /// Leaves as many of the parameters whose types weren't inferred as there were before a
    /// lambda was compiled. The generics that were named in it aren't for an error after it.
    pub(crate) fn truncate_uninferred_parameters(&mut self, count: usize) {
        self.uninferred_parameters.truncate(count);
        self.named_generics.clear();
    }

    /// Keeps the generics in a type that's named for the next error that are of a call whose
    /// generics are still being inferred. Nothing is known about one of those by then, so the
    /// error follows from it not being inferred where the type is used, as
    /// ``Cannot use `+` operator with `T` and `1` `` does in the body of a lambda whose parameter
    /// was left as the `T` of the function that's called. If it's what a parameter was left as,
    /// the error gets a note that says so, since it otherwise only names a generic that the code
    /// doesn't.
    fn name_uninferred_generics(&mut self, ty: TypeId) {
        let inferred = self.inferred_generics();

        if inferred.is_empty() {
            return;
        }

        let scope = self.last_scope_id();

        for generic in rue_types::contained_generics(self.db.types_mut(), ty, inferred) {
            let name = self.db.type_name_in(scope, generic);

            // One parameter is enough to say where a generic comes from
            if self.named_generics.iter().any(|other| other.name == name) {
                continue;
            }

            // A call in a lambda that's passed to the same function has the same generics, so
            // more than one lambda's parameter can have been left as one
            let parameters = self
                .uninferred_parameters
                .iter()
                .filter(|(other, _)| *other == generic)
                .map(|(_, parameter)| format!("`{parameter}`"))
                .collect::<Vec<_>>();

            let note = match parameters.split_last() {
                None => None,
                Some((parameter, [])) => Some(format!(
                    "`{name}` is a generic of the function that's called, since the type of the parameter {parameter} wasn't inferred here; specify the parameter's type explicitly"
                )),
                Some((last, rest)) => Some(format!(
                    "`{name}` is a generic of the function that's called, since the types of the parameters {} and {last} weren't inferred here; specify their types explicitly",
                    rest.join(", ")
                )),
            };

            self.named_generics.push(NamedGeneric {
                id: generic,
                name,
                note: note.map(Note::new),
            });
        }
    }

    pub fn srcloc(&self, node: &impl GetTextRange) -> SrcLoc {
        let range = node.text_range();
        let span: Range<usize> = range.start().into()..range.end().into();
        SrcLoc::new(self.source.clone(), span)
    }

    /// The location of a node without any whitespace or comments at its start or end, which the
    /// parser can include in it. This is exactly the code that was written, so it can be replaced
    /// by a suggestion.
    pub fn trimmed_srcloc(&self, node: &SyntaxNode) -> SrcLoc {
        self.srcloc(&trimmed_range(node))
    }

    /// Like [`Self::trimmed_srcloc`], but without the attributes at the start of the node, which
    /// an `if` or `match` statement includes, since they aren't part of its code.
    pub fn code_srcloc(&self, node: &SyntaxNode) -> SrcLoc {
        let range = trimmed_range(node);

        let start = node
            .children_with_tokens()
            .find(|element| !element.kind().is_trivia() && element.kind() != SyntaxKind::Attribute)
            .map_or(range.start(), |element| match element {
                SyntaxElement::Node(node) => trimmed_range(&node).start(),
                SyntaxElement::Token(token) => token.text_range().start(),
            });

        self.srcloc(&TextRange::new(
            start.clamp(range.start(), range.end()),
            range.end(),
        ))
    }

    pub fn local_name(&self, token: &SyntaxToken) -> Name {
        let srcloc = self.srcloc(token);
        Name::new(token.text().to_string(), Some(srcloc))
    }

    pub fn extend_diagnostics(&mut self, diagnostics: Vec<Diagnostic>) {
        self.diagnostics.extend(diagnostics);
    }

    pub fn alloc_child_scope(&mut self) -> ScopeId {
        let parent_scope = self.last_scope_id();
        self.alloc_scope(Scope::new(Some(parent_scope)))
    }

    pub fn push_scope(&mut self, scope: ScopeId, start: TextSize) {
        self.scope_stack.push((start, scope));
    }

    pub fn pop_scope(&mut self, end: TextSize) {
        let (start, scope) = self.scope_stack.pop().unwrap();

        if !self.registered_scopes.insert(scope) {
            return;
        }

        self.add_syntax(SyntaxItemKind::Scope(scope), TextRange::new(start, end));
    }

    pub fn last_scope(&self) -> &Scope {
        let scope = *self.scope_stack.last().unwrap();
        self.scope(scope.1)
    }

    pub fn last_scope_mut(&mut self) -> &mut Scope {
        let scope = *self.scope_stack.last().unwrap();
        self.scope_mut(scope.1)
    }

    pub fn last_scope_id(&self) -> ScopeId {
        self.scope_stack.last().unwrap().1
    }

    pub fn push_module(&mut self, module: SymbolId) {
        self.module_stack.push(module);
    }

    pub fn pop_module(&mut self) {
        self.module_stack.pop().unwrap();
    }

    /// The modules that `super` refers to from the code being compiled, from the outermost to the
    /// innermost, so that each `super` pops one. In an inline module, `super` is the module it's
    /// in. In a file, it's the parent of the file's directory, since the file can already refer to
    /// the other files in its directory by name.
    pub fn parent_module_stack(&self) -> Vec<SymbolId> {
        let mut stack = self.module_stack[..self.module_stack.len().saturating_sub(1)].to_vec();

        if let Some((_, parents)) = self.inline_modules.split_last() {
            stack.extend(self.file.map(|(module, _)| module));
            stack.extend_from_slice(parents);
        }

        stack
    }

    /// The module that `project` refers to from the code being compiled: the module that the
    /// project's entrypoint points to (its directory), or the file itself if it isn't in a
    /// directory (such as a program compiled from a single file). The standard library is its own
    /// project, whose module is its directory.
    pub fn project_root(&self) -> SymbolId {
        self.module_stack
            .first()
            .copied()
            .unwrap_or_else(|| self.file.expect("code is compiled in a file").0)
    }

    /// Starts compiling a file, whose module is in the given scope.
    pub fn enter_file(&mut self, module: SymbolId, outer_scope: ScopeId) {
        self.file = Some((module, outer_scope));
    }

    pub fn exit_file(&mut self) {
        self.file = None;
    }

    pub fn push_inline_module(&mut self, module: SymbolId) {
        self.inline_modules.push(module);
    }

    pub fn pop_inline_module(&mut self) {
        self.inline_modules.pop().unwrap();
    }

    /// The module that the code being compiled is in, inline or a file.
    pub fn current_module(&self) -> SymbolId {
        self.inline_modules.last().copied().unwrap_or_else(|| {
            let (module, _) = self.file.expect("code is compiled in a file");
            module
        })
    }

    /// The scope that an inline module's scope is in. An inline module only sees its own items and
    /// imports, and the prelude, like a file, so its scope is in the one outside of the file.
    pub fn inline_module_parent_scope(&self) -> ScopeId {
        let (_, outer_scope) = self.file.expect("inline modules are declared in a file");
        outer_scope
    }

    /// Records a lint site if a name written in an inline module (as the start of a path) resolves
    /// outside of the file, to the prelude or a builtin, while a module that encloses it has a
    /// different item with the same name in the namespace. Before inline modules stopped seeing the
    /// items of the modules they're in, the name meant the enclosing module's item.
    pub fn record_enclosed_prelude_reference(&mut self, namespace: Namespace, name: &SyntaxToken) {
        if self.inline_modules.is_empty() || !self.source.kind.check_unused() {
            return;
        }

        // The nearest enclosing module with the name, and how many `super`s name it. This is the
        // same stack that `super` pops, from the innermost module, and only the inline modules
        // and the file (not the file's directories) enclose the code.
        let stack = self.parent_module_stack();
        let Some((depth, enclosing, import)) = stack
            .iter()
            .rev()
            .take(self.inline_modules.len())
            .enumerate()
            .find_map(|(index, &module)| {
                let scope = self.module(module).scope;
                let (item, import) = self.module_member(scope, namespace, name.text())?;

                // A failed import's name has already been reported
                if self.is_error_item(item) {
                    return Some(None);
                }

                Some(Some((index + 1, item, import)))
            })
            .flatten()
        else {
            return;
        };

        // Whether the name resolves in the scope outside of the inline module or one enclosing it,
        // rather than in the module or the code in it, and whether that's the builtins
        let outer_scope = self.inline_module_parent_scope();
        let mut outside = false;

        let Some((resolved, scope)) = self.lookup_in(self.last_scope_id(), |id, scope| {
            outside |= id == outer_scope;

            scope.item(namespace, name.text()).map(|item| (item, id))
        }) else {
            return;
        };

        // An enclosing module with the same item (such as by importing it from the prelude) means
        // the same thing
        if !outside || resolved == enclosing {
            return;
        }

        let srcloc = match import {
            Some(import) => match &self.import(import).items {
                Items::Named(name) | Items::All(name) => name.srcloc().cloned(),
            },
            None => self
                .declaration_name(enclosing.into())
                .and_then(|name| name.srcloc().cloned()),
        };

        let reference = EnclosedPreludeReference {
            name: self.local_name(name),
            builtin: scope == self.builtins.scope,
            depth,
            enclosing: srcloc,
            imported: import.is_some(),
            field_shorthand: name
                .parent()
                .is_some_and(|parent| parent.kind() == SyntaxKind::StructInitializerField),
        };

        self.lint_sites.enclosed_prelude_references.push(reference);
    }

    /// Records the scope with a file's sibling modules, which are found through `super` from an
    /// inline module in the file like the file's own items.
    pub fn add_sibling_scope(&mut self, file_scope: ScopeId, sibling_scope: ScopeId) {
        self.sibling_scopes.insert(file_scope, sibling_scope);
    }

    /// The item with a name that a module (by its scope) has, including a file's sibling modules,
    /// and the import that brought it into the module, if any. This is what `super::name` finds.
    pub fn module_member(
        &self,
        scope: ScopeId,
        namespace: Namespace,
        name: &str,
    ) -> Option<(Item, Option<ImportId>)> {
        self.scope(scope)
            .item_with_import(namespace, name)
            .or_else(|| self.sibling_item(scope, namespace, name))
    }

    /// The sibling module with a name, if a scope is a file's and the name is in a namespace that
    /// modules are in. An item of the file with the name hides the sibling, so this is only looked
    /// up once the file doesn't have one.
    pub fn sibling_item(
        &self,
        scope: ScopeId,
        namespace: Namespace,
        name: &str,
    ) -> Option<(Item, Option<ImportId>)> {
        let siblings = self.sibling_scopes.get(&scope)?;
        self.scope(*siblings).item_with_import(namespace, name)
    }

    /// The names and items that a module (by its scope) has in a namespace, including a file's
    /// sibling modules. This is what `super::name` can name.
    pub fn module_members(&self, scope: ScopeId, namespace: Namespace) -> Vec<(String, Item)> {
        let module = self.scope(scope);
        let mut items = module.items(namespace);

        // An item of the file with the name hides the sibling (see [`Self::sibling_item`])
        items.extend(
            self.sibling_items(scope, namespace)
                .into_iter()
                .filter(|(name, _)| module.item(namespace, name).is_none()),
        );

        items
    }

    /// The names and items of a file's sibling modules in a namespace, if a scope is a file's.
    pub fn sibling_items(&self, scope: ScopeId, namespace: Namespace) -> Vec<(String, Item)> {
        let Some(&siblings) = self.sibling_scopes.get(&scope) else {
            return Vec::new();
        };

        self.scope(siblings).items(namespace)
    }

    /// Looks up a name in one namespace in a scope and then each enclosing scope.
    pub fn resolve_item_in(
        &self,
        scope: ScopeId,
        namespace: Namespace,
        name: &str,
    ) -> Option<(Item, Option<ImportId>)> {
        self.lookup_in(scope, |_, scope| scope.item_with_import(namespace, name))
    }

    /// Records an item that a failed import binds its name to (see [`Self::is_error_item`]).
    pub fn add_error_item(&mut self, item: Item) {
        self.error_items.insert(item);
    }

    /// Whether an item is one that a failed import binds its name to, in place of what it would
    /// have brought in. The import's failure has been reported, so a use of the name is silently
    /// unresolved, and imports of it bind it too.
    pub fn is_error_item(&self, item: Item) -> bool {
        self.error_items.contains(&item)
    }

    /// The first result of `find` in a scope and then each enclosing scope.
    pub(crate) fn lookup_in<T>(
        &self,
        scope: ScopeId,
        mut find: impl FnMut(ScopeId, &Scope) -> Option<T>,
    ) -> Option<T> {
        let mut current = Some(scope);

        while let Some(scope) = current {
            if let Some(found) = find(scope, self.scope(scope)) {
                return Some(found);
            }
            current = self.scope(scope).parent();
        }

        None
    }

    /// The name of a type for the error that's reported next, which is taken to follow from a
    /// generic not being inferred if the type has one that wasn't (see
    /// [`Self::name_uninferred_generics`]). A name for anything else is [`Self::type_name_in`].
    pub fn type_name(&mut self, ty: TypeId) -> String {
        self.name_uninferred_generics(ty);
        let current = self.last_scope_id();
        self.type_name_in(current, ty)
    }

    pub fn type_name_in(&mut self, scope: ScopeId, ty: TypeId) -> String {
        self.db.type_name_in(scope, ty)
    }

    pub fn symbol_name(&self, symbol: SymbolId) -> String {
        let mut current = Some(self.last_scope_id());

        while let Some(scope) = current {
            if let Some(name) = self.scope(scope).symbol_name(symbol) {
                return name.to_string();
            }
            current = self.scope(scope).parent();
        }

        self.symbol(symbol)
            .name()
            .map_or_else(|| "{unknown}".to_string(), |name| name.text().to_string())
    }

    pub fn symbol_type(&self, symbol: SymbolId) -> TypeId {
        let current = self.last_scope_id();
        self.symbol_type_in(current, symbol)
    }

    pub fn symbol_type_in(&self, scope: ScopeId, symbol: SymbolId) -> TypeId {
        let mut current = self.overridden_symbols.contains(&symbol).then_some(scope);

        while let Some(scope) = current {
            if let Some(ty) = self.scope(scope).symbol_override_type(symbol) {
                return ty;
            }
            current = self.scope(scope).parent();
        }

        match self.symbol(symbol) {
            Symbol::Unresolved | Symbol::Module(_) | Symbol::Builtin(_) => {
                self.builtins().unresolved.ty
            }
            Symbol::Function(function) => function.ty,
            Symbol::Parameter(parameter) => parameter.ty,
            Symbol::Constant(constant) => constant.value.ty,
            Symbol::Binding(binding) => binding.value.ty,
        }
    }

    pub fn push_mappings(
        &mut self,
        mappings: HashMap<SymbolId, HashMap<Vec<TypePath>, TypeId>>,
        start: TextSize,
    ) -> usize {
        let scope = self.alloc_child_scope();

        let builtins = self.builtins.types;

        for (symbol, paths) in mappings {
            let mut ty = self.symbol_type(symbol);

            let mut paths = paths.into_iter().collect::<Vec<_>>();
            paths.sort_by_key(|(path, _)| (Reverse(path.len()), path.last().copied()));

            // The same narrowing of the parts that keep the references of untyped lambda
            // parameters' types, for a value that was narrowed before (see
            // `narrowed_with_references`)
            let original = self.with_narrowed_references(ty);
            let has_raw = original != ty;
            let mut raw = original;

            for (path, replacement) in paths {
                ty = replace_type(&mut self.db, &builtins, ty, replacement, &path);

                if has_raw {
                    raw = replace_type(&mut self.db, &builtins, raw, replacement, &path);
                }
            }

            if !has_raw {
                raw = ty;
            }

            narrowed_with_references(self, original, raw, ty);

            self.scope_mut(scope).override_symbol_type(symbol, ty);
            if self.overridden_symbols.insert(symbol) {
                self.undo.push(Undo::OverriddenSymbol(symbol));
            }
        }

        let index = self.scope_stack.len();
        self.push_scope(scope, start);
        index
    }

    /// Records that narrowing gave a value the type, made up of the parts of the type before it was
    /// narrowed in `raw`, which keep the references of untyped lambda parameters' types that the
    /// type sees through (see `narrowed_with_references`). They're the same type.
    pub fn add_narrowed_reference(&mut self, narrowed: TypeId, raw: TypeId) {
        let before = self.narrowed_references.insert(narrowed, raw);
        self.undo.push(Undo::NarrowedReference(narrowed, before));
    }

    /// The type that narrowing gave a value as the parts that make it up before they were
    /// substituted, which keep the references in them, if it's one (see `add_narrowed_reference`),
    /// or the type itself.
    pub fn with_narrowed_references(&self, ty: TypeId) -> TypeId {
        self.narrowed_references.get(&ty).copied().unwrap_or(ty)
    }

    pub fn merge_mappings(&mut self, a: &Mappings, b: &Mappings) -> Mappings {
        let builtins = self.builtins.types;
        merge_mappings(&mut self.db, &builtins, a, b)
    }

    /// Enters a context where `return` statements exit to the given target. If the target
    /// is `None`, then `return` statements are not allowed (such as in expression blocks).
    pub fn push_return_target(&mut self, target: Option<ReturnTarget>) {
        self.return_stack.push((target, self.return_count));
    }

    /// Leaves the current return context. The return count is restored, since `return`
    /// statements inside of it can't exit any of the blocks that enclose it.
    pub fn pop_return_target(&mut self) -> Option<ReturnTarget> {
        let (target, return_count) = self
            .return_stack
            .pop()
            .expect("return target stack is unbalanced");
        self.return_count = return_count;
        target
    }

    /// Enters the arguments of a call whose generics are being inferred from them.
    pub fn push_call_frame(&mut self, generics: Vec<TypeId>) {
        self.call_frames.push(CallFrame {
            generics,
            argument: None,
        });
    }

    pub fn pop_call_frame(&mut self) {
        self.named_generics.clear();
        self.call_frames
            .pop()
            .expect("call frame stack is unbalanced");
    }

    /// The generics of the enclosing calls that are still being inferred.
    pub fn inferred_generics(&self) -> Vec<TypeId> {
        self.call_frames
            .iter()
            .flat_map(|frame| frame.generics.iter().copied())
            .collect()
    }

    /// The generics of the innermost call being inferred, if there is one.
    pub fn call_generics(&self) -> Option<&[TypeId]> {
        self.call_frames
            .last()
            .map(|frame| frame.generics.as_slice())
    }

    /// Starts compiling an argument of the innermost call being inferred (or stops, with `None`).
    pub fn set_argument_frame(&mut self, argument: Option<ArgumentFrame>) {
        self.call_frames
            .last_mut()
            .expect("an argument is compiled inside a call")
            .argument = argument;
    }

    /// Compiles with an expected type that's only a hint, which the value doesn't have to be
    /// assignable to, or one that it does. A hint is the type of a cast, of an argument whose
    /// parameter type references the generics of the call being inferred, which can still widen
    /// it, or of an element of a pair or list that a member of its expected type, such as `Any`,
    /// could hold as another pair (see `holds_other_pairs`). The expressions it's passed down to,
    /// such as a pair's elements or an `if`'s branches, inherit it, until one is compiled with an
    /// expected type of its own, such as a `let` annotation.
    pub fn with_expected_type_hint<T>(
        &mut self,
        hint: bool,
        compile: impl FnOnce(&mut Self) -> T,
    ) -> T {
        let outer = mem::replace(&mut self.expected.hinted, hint);
        let result = compile(self);
        self.expected.hinted = outer;
        result
    }

    /// Whether the type expected of the expression being compiled is only a hint (see
    /// `Compiler::with_expected_type_hint`).
    pub fn is_expected_type_hint(&self) -> bool {
        self.expected.hinted
    }

    /// Compiles with an expected type that could still be widened before the value is checked
    /// against it, or one that can't. That's the type of an argument whose parameter type
    /// references the generics of the call being inferred: it's what they're inferred as so far,
    /// and the argument is checked against what they're finally inferred as, which a later
    /// argument can widen. Even when the value has to be assignable to it (it isn't a hint), a
    /// call in it isn't narrowed to satisfy it (see `CallSolver::solve_narrowed`), since the
    /// wider type could be what the program needs, only for what the enclosing call requires of
    /// it however it's solved (see `Compiler::with_argument_requirement`). It's inherited like a
    /// hint, until an expression is compiled with an expected type of its own: a `let`
    /// annotation, a lambda's return type, a struct's field, or the argument of a parameter that
    /// doesn't reference the call's generics.
    pub fn with_expected_type_provisional<T>(
        &mut self,
        provisional: bool,
        compile: impl FnOnce(&mut Self) -> T,
    ) -> T {
        let outer = mem::replace(&mut self.expected.provisional, provisional);
        let result = compile(self);
        self.expected.provisional = outer;
        result
    }

    /// Whether the type expected of the expression being compiled could still be widened before
    /// it's checked (see `Compiler::with_expected_type_provisional`).
    pub fn is_expected_type_provisional(&self) -> bool {
        self.expected.provisional
    }

    /// Compiles a call argument with a type that it has to be assignable to however the enclosing
    /// call's generics are solved, if there is one. A call that's the argument itself is narrowed
    /// for it (see `CallSolver::solve_narrowed`) when what it's solved as otherwise isn't
    /// assignable to it, though its expected type is provisional
    /// (`Compiler::with_expected_type_provisional`): the program would be rejected either way.
    pub fn with_argument_requirement<T>(
        &mut self,
        requirement: Option<(TextRange, TypeId)>,
        compile: impl FnOnce(&mut Self) -> T,
    ) -> T {
        let outer = mem::replace(&mut self.argument_requirement, requirement);
        let result = compile(self);
        self.argument_requirement = outer;
        result
    }

    /// The type that the expression at the range has to be assignable to, if it's a call argument
    /// that has one (see `Compiler::with_argument_requirement`).
    pub fn argument_requirement(&self, range: TextRange) -> Option<TypeId> {
        self.argument_requirement
            .filter(|&(at, _)| at == range)
            .map(|(_, ty)| ty)
    }

    /// Compiles with an expected type that's only for the generics of the calls in the expression
    /// that nothing else gives a type, or one that's for all of them. It's inherited by the
    /// expressions it's passed down to, and an argument of a call is compiled with its own.
    pub fn with_expected_type_fallback<T>(
        &mut self,
        fallback: bool,
        compile: impl FnOnce(&mut Self) -> T,
    ) -> T {
        let outer = mem::replace(&mut self.expected.fallback, fallback);
        let result = compile(self);
        self.expected.fallback = outer;
        result
    }

    /// Whether the type expected of the expression being compiled is only a fallback (see
    /// `Compiler::with_expected_type_fallback`).
    pub fn is_expected_type_fallback(&self) -> bool {
        self.expected.fallback
    }

    /// Takes the argument of the innermost call being inferred that's being compiled, if there is
    /// one, to be put back with `Compiler::set_argument_frame`.
    pub fn take_argument_frame(&mut self) -> Option<ArgumentFrame> {
        self.call_frames.last_mut()?.argument.take()
    }

    /// The argument of the innermost call being inferred that's being compiled, if there is one.
    pub fn argument_frame_mut(&mut self) -> Option<&mut ArgumentFrame> {
        self.call_frames.last_mut()?.argument.as_mut()
    }

    pub fn return_target_mut(&mut self) -> Option<&mut ReturnTarget> {
        self.return_stack
            .last_mut()
            .and_then(|(target, _)| target.as_mut())
    }

    /// The number of `return` statements compiled so far that could exit the blocks in
    /// the current return context, which is used to tell whether a block may have exited
    /// early.
    pub fn return_count(&self) -> usize {
        self.return_count
    }

    pub fn increment_return_count(&mut self) {
        self.return_count += 1;
    }

    /// Records the type, location and scope of a compiled expression in the database, so that
    /// they're available after compilation (such as to lints). The builtin values (such as `nil`) share
    /// a HIR node, so the value is given a node of its own first, which is returned.
    pub fn record_value(&mut self, node: &SyntaxNode, mut value: Value) -> Value {
        if self.builtins.is_shared_hir(value.hir) {
            let hir = self.db.hir(value.hir).clone();
            value = value.with_hir(self.db.alloc_hir(hir));
        }

        let srcloc = self.code_srcloc(node);
        self.db.set_hir_type(value.hir, value.ty);
        self.db.set_hir_srcloc(value.hir, srcloc);
        self.db.set_hir_scope(value.hir, self.last_scope_id());

        value
    }

    /// Records the function that a `for` expression which makes a list is compiled to, by the
    /// node of its call.
    pub(crate) fn record_list_loop(&mut self, call: HirId, function: SymbolId) {
        self.list_loops.insert(call, function);
    }

    /// The function of the `for` expression that the node is the call of, if it makes a list
    /// that still ends with nil. It's only given once, since the list can only be made to end
    /// with something else once: a list that starts with a `for` it has ended is that same node,
    /// and a list that it's spread in must not end it again.
    pub(crate) fn take_list_loop(&mut self, call: HirId) -> Option<SymbolId> {
        self.list_loops.remove(&call)
    }

    /// The sites recorded for lints while compiling, which are checked by [`rue_lint`] once
    /// everything has been compiled.
    pub fn lint_sites_mut(&mut self) -> &mut LintSites {
        &mut self.lint_sites
    }

    /// Runs the lint passes over the sites recorded so far and the given documents.
    pub fn run_lints(
        &mut self,
        documents: Vec<(&Source, &AstDocument)>,
        entrypoints: &HashSet<Declaration>,
    ) {
        let input = LintInput::new(
            mem::take(&mut self.lint_sites),
            documents,
            entrypoints,
            self.has_errors(),
            &self.options,
        );

        let diagnostics = rue_lint::run_lints(
            &mut self.db,
            &self.builtins.types,
            self.options.lints,
            &input,
        );

        self.diagnostics.extend(diagnostics);
    }

    /// Discards the sites recorded so far without linting them, such as for the standard library.
    pub fn discard_lint_sites(&mut self) {
        self.lint_sites = LintSites::default();
    }

    pub fn mapping_checkpoint(&self) -> usize {
        self.scope_stack.len()
    }

    pub fn revert_mappings(&mut self, index: usize, end: TextSize) {
        while self.scope_stack.len() > index {
            self.pop_scope(end);
        }
    }

    pub fn is_assignable(&mut self, from: TypeId, to: TypeId) -> bool {
        let comparison = rue_types::compare(self.db.types_mut(), &self.builtins.types, from, to);
        comparison == Comparison::Assign
    }

    /// Like `is_assignable`, but returns `TooComplex` if comparing the types took too many steps.
    pub fn try_is_assignable(&mut self, from: TypeId, to: TypeId) -> Result<bool, TooComplex> {
        let comparison =
            rue_types::compare_limited(self.db.types_mut(), &self.builtins.types, from, to)?;
        Ok(comparison == Comparison::Assign)
    }

    /// Reports that comparing the types took too many steps (see `rue_types::compare_limited`).
    pub fn too_complex(&mut self, node: &impl GetTextRange, from: TypeId, to: TypeId) {
        let from = self.type_name(from);
        let to = self.type_name(to);
        self.diagnostic(node, DiagnosticKind::TypeComparisonTooComplex(from, to));
    }

    pub fn is_castable(&mut self, from: TypeId, to: TypeId) -> bool {
        let comparison = rue_types::compare(self.db.types_mut(), &self.builtins.types, from, to);
        matches!(comparison, Comparison::Assign | Comparison::Cast)
    }

    pub fn assign_type(&mut self, node: &impl GetTextRange, from: TypeId, to: TypeId) {
        self.compare_type(node, from, to, to, false);
    }

    /// Like [`Self::assign_type`], but an error names `shown` as the type that the value can't be
    /// assigned to, which is `to` as it's written before it's substituted with placeholders.
    pub(crate) fn assign_type_shown_as(
        &mut self,
        node: &impl GetTextRange,
        from: TypeId,
        to: TypeId,
        shown: TypeId,
    ) {
        self.compare_type(node, from, to, shown, false);
    }

    pub fn cast_type(&mut self, node: &impl GetTextRange, from: TypeId, to: TypeId) {
        self.compare_type(node, from, to, to, true);
    }

    pub fn guard_type(&mut self, node: &impl GetTextRange, from: TypeId, to: TypeId) -> Constraint {
        let Some(check) = self.runtime_check(node, from, to) else {
            return Constraint::new(Check::Impossible);
        };

        if check == Check::Impossible {
            let from_name = self.type_name(from);
            let to_name = self.type_name(to);
            self.diagnostic(node, DiagnosticKind::IncompatibleGuard(from_name, to_name));
        }

        let srcloc = self.srcloc(node);
        let else_id = self.subtract_type(&srcloc, from, to);

        Constraint::new(check).with_else(else_id)
    }

    /// The values of `from` that aren't a `to`, which is what's left after a type guard or `match`
    /// arm at the given location fails. A struct with too many combinations of its fields left to
    /// narrow it by is merged instead, which is recorded for the `imprecise_narrowing` lint.
    pub fn subtract_type(&mut self, srcloc: &SrcLoc, from: TypeId, to: TypeId) -> TypeId {
        let subtracted =
            rue_types::subtract_detailed(self.db.types_mut(), &self.builtins.types, from, to);

        for struct_id in subtracted.merged_structs {
            let current = self.last_scope_id();
            let struct_name = self.type_name_in(current, struct_id);
            self.lint_sites
                .imprecise_narrowings
                .push(ImpreciseNarrowing {
                    srcloc: srcloc.clone(),
                    struct_name,
                });
        }

        subtracted.type_id
    }

    /// The check that a value of type `from` needs at runtime to tell whether it's a `to`, or
    /// `None` if it can't be checked, which has been reported. A check that's impossible, since the
    /// types don't overlap, isn't reported here.
    pub fn runtime_check(
        &mut self,
        node: &impl GetTextRange,
        from: TypeId,
        to: TypeId,
    ) -> Option<Check> {
        let error = match rue_types::check(self.db.types_mut(), &self.builtins.types, from, to) {
            Ok(check) => return Some(check),
            Err(CheckError::DepthExceeded) => DiagnosticKind::TypeCheckDepthExceeded,
            Err(CheckError::TooLarge) => DiagnosticKind::TypeCheckTooLarge,
            Err(CheckError::RecursiveType) => DiagnosticKind::RecursiveTypeCheck,
            Err(CheckError::FunctionType) => DiagnosticKind::FunctionTypeCheck,
            Err(CheckError::GenericType) => DiagnosticKind::GenericTypeCheck,
            Err(CheckError::TooComplex) => {
                self.too_complex(node, from, to);
                return None;
            }
        };

        self.diagnostic(node, error);
        None
    }

    pub fn check_condition(&mut self, node: &impl GetTextRange, ty: TypeId) {
        // An error has already been reported, and the type is compatible with everything
        if self.is_unresolved(ty) {
            return;
        }

        // Conditions that are always `true` or `false` are reported by the `constant_condition`
        // lint, but they still need to be a `Bool` regardless of whether that lint is enabled
        self.assign_type(node, ty, self.builtins().types.bool);
    }

    /// Collects inference candidates for the generics in `to`, without reporting diagnostics.
    /// The value must be checked against the substituted type afterward.
    pub fn infer_type(&mut self, from: TypeId, to: TypeId, infer: &mut Inference) {
        rue_types::compare_with_inference(
            self.db.types_mut(),
            &self.builtins.types,
            from,
            to,
            Some(infer),
        );
    }

    /// Collects inference candidates for the generics in `ty` from the type it's expected to be.
    /// Collects candidates for the generics in the type from the type it's expected to be
    /// assignable to (see `rue_types::infer_from_expected`), and returns whether that finished
    /// within the step limit.
    pub fn infer_from_expected(
        &mut self,
        ty: TypeId,
        expected: TypeId,
        infer: &mut Inference,
    ) -> bool {
        rue_types::infer_from_expected(
            self.db.types_mut(),
            &self.builtins.types,
            ty,
            expected,
            infer,
        )
    }

    /// A solver for a call's generics, given what was inferred from the type expected of it, and
    /// from a narrower one, if the call is an argument widened to its enclosing call's expected
    /// type (see `CallSolver::with_narrower_expected`).
    pub fn call_solver(
        &mut self,
        expected: Option<Inference>,
        narrower: Option<&Inference>,
    ) -> CallSolver {
        let solver = CallSolver::new(self.db.types_mut(), &self.builtins.types, expected);

        match narrower {
            Some(narrower) => {
                solver.with_narrower_expected(self.db.types_mut(), &self.builtins.types, narrower)
            }
            None => solver,
        }
    }

    /// What the arguments of a call compiled so far infer on their own (see
    /// `CallSolver::solve_arguments`).
    pub fn solve_arguments(
        &mut self,
        solver: &CallSolver,
        infer: &Inference,
    ) -> HashMap<TypeId, TypeId> {
        solver.solve_arguments(self.db.types_mut(), &self.builtins.types, infer)
    }

    /// The mappings that the next argument's expected type is substituted with (see
    /// `CallSolver::argument_mappings`).
    pub fn argument_mappings(
        &mut self,
        solver: &CallSolver,
        infer: &Inference,
        inferred: &HashMap<TypeId, TypeId>,
        bound_mapped: bool,
    ) -> HashMap<TypeId, TypeId> {
        solver.argument_mappings(
            self.db.types_mut(),
            &self.builtins.types,
            infer,
            inferred,
            bound_mapped,
        )
    }

    /// Widens the mappings to the bounds the call's expected type gives on its own (see
    /// `CallSolver::widen_to_expected`).
    pub fn widen_to_expected(
        &mut self,
        solver: &CallSolver,
        infer: &Inference,
        mappings: &mut HashMap<TypeId, TypeId>,
    ) {
        solver.widen_to_expected(self.db.types_mut(), &self.builtins.types, infer, mappings);
    }

    /// The mappings a call is checked with, once its arguments have been compiled (see
    /// `CallSolver::solve`).
    pub fn solve_call(&mut self, solver: &CallSolver, infer: Inference) -> HashMap<TypeId, TypeId> {
        solver.solve(self.db.types_mut(), &self.builtins.types, infer)
    }

    /// The mappings the call is checked with when it narrows its generics (see
    /// `CallSolver::solve_narrowed`).
    pub fn solve_call_narrowed(
        &mut self,
        solver: &CallSolver,
        infer: Inference,
    ) -> HashMap<TypeId, TypeId> {
        solver.solve_narrowed(self.db.types_mut(), &self.builtins.types, infer)
    }

    /// The name of the type that a value can't be assigned to, which is `to`. If it's shown as
    /// another type, that's the parameter of a call as it's declared, whose generics are the
    /// callee's rather than ones that a type in the arguments was left as.
    fn shown_type_name(&mut self, to: TypeId, shown: TypeId) -> String {
        if shown == to {
            self.type_name(to)
        } else {
            let scope = self.last_scope_id();
            self.type_name_in(scope, shown)
        }
    }

    /// A hex literal is bytes rather than an integer, so when one is assigned where an `Int` is
    /// expected, this explains why and suggests the number that was probably meant, which is the
    /// unsigned value of its bytes: `255` for `0xff`, which is `-1` as an `Int`.
    fn hex_literal_int_help(&mut self, srcloc: &SrcLoc, to: TypeId) -> Option<(Note, Suggestion)> {
        // The literal can be in parentheses, or be all that's in a block (such as a function's
        // body, which is what's checked against its return type), which the suggestion keeps
        let text = srcloc.text();
        let mut start = 0;
        let mut trimmed = text;

        loop {
            let inner = trimmed.trim_start();
            start += trimmed.len() - inner.len();
            trimmed = inner.trim_end();

            let inner = [('(', ')'), ('{', '}')]
                .into_iter()
                .find_map(|(open, close)| trimmed.strip_prefix(open)?.strip_suffix(close));

            match inner {
                Some(inner) => {
                    start += 1;
                    trimmed = inner;
                }
                None => break,
            }
        }

        let digits = trimmed.strip_prefix("0x")?;

        if digits.is_empty() || !digits.chars().all(|c| c.is_ascii_hexdigit() || c == '_') {
            return None;
        }

        let int = self.builtins.types.int;

        if !matches!(
            rue_types::compare_limited(self.db.types_mut(), &self.builtins.types, int, to),
            Ok(Comparison::Assign)
        ) {
            return None;
        }

        let LiteralValue::Bytes(bytes) = LiteralValue::decode_text(SyntaxKind::Hex, trimmed) else {
            return None;
        };

        if bytes.is_empty() {
            return None;
        }

        let unsigned = BigInt::from_bytes_be(Sign::Plus, &bytes);
        let signed = BigInt::from_signed_bytes_be(&bytes);

        let cast = if bigint_atom(signed.clone()) != bytes {
            format!("has a leading zero byte, so it isn't equal to the `Int` `{unsigned}`")
        } else if signed == unsigned {
            format!("is the `Int` `{signed}`")
        } else {
            format!("is the negative number `{signed}`, since integers are signed")
        };

        let note = Note::new(format!(
            "Hex literals are bytes rather than integers; `{trimmed} as Int` {cast}"
        ));

        let start = srcloc.span.start + start;
        let replaced = SrcLoc::new(srcloc.source.clone(), start..start + trimmed.len());

        let suggestion = Suggestion::replace(
            format!("Write the number `{unsigned}` instead"),
            replaced,
            unsigned.to_string(),
            Applicability::MaybeIncorrect,
        );

        Some((note, suggestion))
    }

    fn compare_type(
        &mut self,
        node: &impl GetTextRange,
        from: TypeId,
        to: TypeId,
        shown: TypeId,
        cast: bool,
    ) {
        let Ok(comparison) =
            rue_types::compare_limited(self.db.types_mut(), &self.builtins.types, from, to)
        else {
            self.too_complex(node, from, shown);
            return;
        };

        match comparison {
            Comparison::Assign => {}
            Comparison::Cast => {
                if !cast {
                    let from = self.type_name(from);
                    let to_name = self.shown_type_name(to, shown);
                    let srcloc = self.srcloc(node);
                    let help = self.hex_literal_int_help(&srcloc, to);
                    let diagnostic =
                        self.diagnostic_at(srcloc, DiagnosticKind::UnassignableType(from, to_name));

                    if let Some((note, suggestion)) = help {
                        diagnostic.notes.push(note);
                        diagnostic.suggestions.push(suggestion);
                    }
                }
            }
            Comparison::Invalid => {
                let can_guard =
                    match rue_types::check(self.db.types_mut(), &self.builtins.types, from, to) {
                        Ok(check) => check != Check::Impossible,
                        // A check exists, even though the guard would report that it's too large
                        Err(CheckError::TooLarge) => true,
                        Err(
                            CheckError::DepthExceeded
                            | CheckError::RecursiveType
                            | CheckError::FunctionType
                            | CheckError::GenericType
                            | CheckError::TooComplex,
                        ) => false,
                    };

                let from = self.type_name(from);
                let to = self.shown_type_name(to, shown);

                if can_guard {
                    if cast {
                        self.diagnostic(node, DiagnosticKind::UnguardedCast(from, to));
                    } else {
                        self.diagnostic(node, DiagnosticKind::UnguardedAssignment(from, to));
                    }
                } else if cast {
                    self.diagnostic(node, DiagnosticKind::IncompatibleCast(from, to));
                } else {
                    self.diagnostic(node, DiagnosticKind::IncompatibleType(from, to));
                }
            }
        }
    }

    /// Stores the types of a struct's fields in declaration order, so that their default values
    /// can be compiled against them once all types have been resolved.
    pub fn insert_field_types(&mut self, ty: TypeId, field_types: Vec<TypeId>) {
        self.field_types.insert(ty, field_types);
    }

    pub fn field_types(&self, ty: TypeId) -> Vec<TypeId> {
        self.field_types[&ty].clone()
    }

    /// Stores the variants of an enum, with `None` for each variant (in declaration order) that has
    /// no name or a duplicate name, so they line up with the variants in the syntax tree.
    pub(crate) fn insert_declared_variants(
        &mut self,
        ty: TypeId,
        variants: Vec<Option<EnumVariant>>,
    ) {
        self.declared_variants.insert(ty, variants);
    }

    pub(crate) fn declared_variants(&self, ty: TypeId) -> &[Option<EnumVariant>] {
        &self.declared_variants[&ty]
    }

    /// Whether the variant is declared without braces, which makes it a constant as well as a type.
    pub fn is_unit_variant(&self, variant: TypeId) -> bool {
        self.enum_of(Declaration::Type(variant))
            .and_then(|ty| self.declared_variants.get(&ty))
            .is_some_and(|variants| {
                variants
                    .iter()
                    .flatten()
                    .any(|declared| declared.ty == variant && declared.constant.is_some())
            })
    }

    /// Marks a struct field whose type was inferred from its default value rather than declared.
    pub fn insert_inferred_field(&mut self, ty: TypeId, name: String) {
        if self.inferred_fields.insert((ty, name.clone())) {
            self.undo.push(Undo::InferredField((ty, name)));
        }
    }

    pub fn is_inferred_field(&self, ty: TypeId, name: &str) -> bool {
        self.inferred_fields.contains(&(ty, name.to_string()))
    }

    /// Marks a struct field that has a default value, before the default values are compiled.
    pub(crate) fn insert_declared_default(&mut self, ty: TypeId, name: String) {
        self.declared_defaults.insert((ty, name));
    }

    pub(crate) fn has_declared_default(&self, ty: TypeId, name: &str) -> bool {
        self.declared_defaults.contains(&(ty, name.to_string()))
    }

    pub(crate) fn set_compiling_default(&mut self, field: Option<(TypeId, String)>) {
        self.compiling_default = field;
    }

    pub(crate) fn compiling_default(&self) -> Option<(TypeId, String)> {
        self.compiling_default.clone()
    }

    /// Records that the default value being compiled, if any, uses the given field's default.
    pub(crate) fn record_default_use(&mut self, ty: TypeId, name: &str) {
        if let Some(user) = self.compiling_default.clone() {
            self.default_uses
                .entry(user.clone())
                .or_default()
                .push((ty, name.to_string()));
            self.undo.push(Undo::DefaultUse(user));
        }
    }

    pub(crate) fn default_uses(&self, field: &(TypeId, String)) -> &[(TypeId, String)] {
        self.default_uses.get(field).map_or(&[], Vec::as_slice)
    }

    pub(crate) fn push_pending_default(&mut self, pending: PendingDefault) {
        self.pending_defaults.push(pending);
    }

    pub(crate) fn take_pending_defaults(&mut self) -> Vec<PendingDefault> {
        mem::take(&mut self.pending_defaults)
    }

    pub fn push_declaration(&mut self, declaration: Declaration) {
        if let Some(last) = self.declaration_stack.last() {
            self.db.add_declaration(*last, declaration);
        }

        self.declaration_stack.push(declaration);

        if self.source.kind.check_unused() {
            self.db.add_relevant_declaration(declaration);
        }
    }

    pub fn pop_declaration(&mut self) {
        self.declaration_stack.pop().unwrap();
    }

    /// Records that a path named the type (see `named_types_between`).
    pub fn name_type(&mut self, ty: TypeId) {
        self.named_types.push(ty);
    }

    /// How many types paths have named so far, to give to `named_types_between`.
    pub fn named_type_count(&self) -> usize {
        self.named_types.len()
    }

    /// The types paths named from `start` up to `end`, both of which are from `named_type_count`.
    pub fn named_types_between(&self, start: usize, end: usize) -> &[TypeId] {
        &self.named_types[start..end]
    }

    pub fn reference(&mut self, reference: Declaration, import: Option<ImportId>) {
        if let Some(last) = self.declaration_stack.last() {
            self.db.add_reference(*last, reference);
        }

        if let Some(import) = import {
            let user = self
                .declaration_stack
                .last()
                .map_or(ImportUser::Other, |&last| ImportUser::Declaration(last));

            self.db.add_import_reference(import, reference, user);
        }
    }

    /// Records that a glob import brought a declaration into a scope that another glob import
    /// brings a different declaration with the same name into, or that it brought in a
    /// declaration that was already ambiguous, so using it is ambiguous. Returns whether it
    /// wasn't already recorded.
    pub fn add_ambiguous_import(&mut self, import: ImportId, declaration: Declaration) -> bool {
        self.ambiguous_imports.insert((import, declaration))
    }

    /// Whether a name that refers to a declaration through this import is ambiguous.
    pub fn is_ambiguous_import(&self, import: Option<ImportId>, declaration: Declaration) -> bool {
        import.is_some_and(|import| self.ambiguous_imports.contains(&(import, declaration)))
    }

    /// Records an import that's checked for being unused, unless it's in the standard library,
    /// as with declarations.
    pub fn add_relevant_import(&mut self, import: ImportId) {
        if self.source.kind.check_unused() {
            self.db.add_relevant_import(import);
        }
    }

    pub fn declaration_span(&mut self, declaration: Declaration, span: TextRange) {
        self.add_syntax(
            match declaration {
                Declaration::Symbol(symbol) => SyntaxItemKind::SymbolDeclaration(symbol),
                Declaration::Type(ty) => SyntaxItemKind::TypeDeclaration(ty),
            },
            span,
        );
    }

    pub fn reference_span(&mut self, reference: Declaration, span: TextRange) {
        let reference = self.db.reference_target(reference);
        self.add_syntax(SyntaxItemKind::reference(reference), span);
    }

    pub fn is_unresolved(&mut self, ty: TypeId) -> bool {
        let semantic = rue_types::unwrap_semantic(self.db.types_mut(), ty, true);
        matches!(self.ty(semantic), Type::Unresolved)
    }
}

/// An error that shows generics of a call that weren't inferred by the time it was reported.
#[derive(Debug, Clone)]
struct UninferredError {
    /// Where the error is in the diagnostics.
    index: usize,
    generics: Vec<TypeId>,
    /// Whether the call that the generics are of is done with it.
    settled: bool,
}

/// A generic of a call that wasn't inferred yet, in a type that was named for an error.
#[derive(Debug, Clone)]
struct NamedGeneric {
    id: TypeId,
    name: String,
    /// What the error says about it, if it's what a parameter of a lambda was left as.
    note: Option<Note>,
}

/// Whether the diagnostic is an error that doesn't only follow from another one. A lint that's
/// an error isn't one that a type couldn't be compiled because of.
fn is_root_error(diagnostic: &Diagnostic) -> bool {
    diagnostic.severity == DiagnosticSeverity::Error
        && diagnostic.kind.lint().is_none()
        && !mentions_unresolved(diagnostic)
}

/// Whether a diagnostic's message shows the type with the given name, on its own or as part of
/// another type. Types are shown in backticks.
pub(crate) fn shows_type(message: &str, name: &str) -> bool {
    message
        .split('`')
        .skip(1)
        .step_by(2)
        .any(|code| names_type(code, name))
}

/// Whether the name of a type has the type with the given name in it, as `(T, Int)` has `T`.
pub(crate) fn names_type(ty: &str, name: &str) -> bool {
    let is_part_of_name = |char: char| char.is_alphanumeric() || char == '_';

    ty.match_indices(name).any(|(start, _)| {
        !ty[..start].ends_with(is_part_of_name)
            && !ty[start + name.len()..].starts_with(is_part_of_name)
    })
}

/// Whether the diagnostic shows a type with a part that couldn't be compiled, which is written as
/// [`rue_types::UNRESOLVED`]. A string in a type is written in quotes, so it isn't that.
fn mentions_unresolved(diagnostic: &Diagnostic) -> bool {
    let message = diagnostic.kind.to_string();
    let mut in_string = false;
    let mut chars = message.char_indices();

    while let Some((index, char)) = chars.next() {
        match char {
            '\\' if in_string => {
                chars.next();
            }
            '"' => in_string = !in_string,
            _ if !in_string && message[index..].starts_with(rue_types::UNRESOLVED) => {
                return true;
            }
            _ => {}
        }
    }

    false
}

/// The range of a node without any whitespace or comments at its start or end.
pub fn trimmed_range(node: &SyntaxNode) -> TextRange {
    let range = node.text_range();

    let mut first = node.first_token();
    while let Some(token) = first.as_ref().filter(|token| token.kind().is_trivia()) {
        first = token
            .next_token()
            .filter(|next| next.text_range().end() <= range.end());
    }

    let mut last = node.last_token();

    while let Some(token) = last.as_ref().filter(|token| token.kind().is_trivia()) {
        last = token
            .prev_token()
            .filter(|prev| prev.text_range().start() >= range.start());
    }

    match (first, last) {
        (Some(first), Some(last)) if first.text_range().start() <= last.text_range().end() => {
            TextRange::new(first.text_range().start(), last.text_range().end())
        }
        _ => range,
    }
}

pub trait GetTextRange {
    fn text_range(&self) -> TextRange;
}

impl GetTextRange for TextRange {
    fn text_range(&self) -> TextRange {
        *self
    }
}

impl GetTextRange for SyntaxNode {
    fn text_range(&self) -> TextRange {
        self.text_range()
    }
}

impl GetTextRange for SyntaxToken {
    fn text_range(&self) -> TextRange {
        self.text_range()
    }
}

/// What programs compiled with the options are compiled for, which decides the flags they're run
/// with (see [`ClvmTarget::flags`]).
pub fn clvm_target(options: &CompilerOptions) -> ClvmTarget {
    ClvmTarget {
        operand_limits: options.operand_limits,
        hard_fork_2: options.hard_fork_2,
    }
}
