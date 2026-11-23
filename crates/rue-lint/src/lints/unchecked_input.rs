//! Reports where a program relies on the length of a value from the solution that its type
//! declares but nothing checks.
//!
//! The parameter types of `main` are assumptions about the solution, so a `Bytes32` parameter can
//! be an atom of any length. Most code doesn't depend on that, since an operation that needs the
//! right length fails if it isn't, and a spend either succeeds as a whole or fails. Two kinds of
//! code do depend on it, and are reported:
//!
//! 1. **Ambiguous concatenations.** Concatenating bytes can only be split back one way if at most
//!    one of the parts has an unknown length. When the types say that's the case, but some of the
//!    parts with a known length are unchecked values from the solution, a spender can move bytes
//!    from one part to the next and get the same result, as in the bug in the first version of CATs, where bytes moved
//!    from a coin's puzzle hash into its amount without changing its coin id.
//! 2. **Deferred conditions.** A value is put in a field of a condition that consensus checks the
//!    length of (such as the puzzle hash of `CreateCoin`), but the condition is hashed with
//!    `tree_hash` rather than output, as in a puzzle that a later spend runs. Consensus only checks
//!    the field once that spend outputs the condition, and then that spend fails instead of this
//!    one.
//!
//! Values are followed through bindings, field access, pairs, lists and structs, and into the
//! functions they're passed to, using a summary of each function that's found by iterating to a
//! fixed point, so that recursion is handled. The results of calling function values and `extern
//! fn`s are unchecked too.
//!
//! A value's length is checked by a check that's always evaluated in its function, before or
//! after the use, since a spend fails as a whole: an `assert` of a type guard that checks the
//! length, or of equality with a value of known length, and `coinid` and the BLS operations,
//! which fail for the wrong length. A type guard on a branch checks the length inside the branch,
//! and so does narrowing a parameter, if the narrowing checks it. These all fail at runtime, so
//! the optimizer keeps them wherever the program evaluates them. Reading a field or doing
//! arithmetic says nothing about a length, so it never counts as a check.
//!
//! Values that a `for` loop or a lambda captures from the function around it aren't followed,
//! so a use of one inside of the loop isn't reported.

use std::collections::{BTreeMap, BTreeSet, HashMap};

use rue_diagnostic::{Lint, Note, SourceKind, SrcLoc};
use rue_hir::{
    BinaryOp, Declaration, FieldKind, FunctionCall, FunctionKind, Hir, HirId, Statement, Symbol,
    SymbolId, UnaryOp,
};
use rue_lir::ClvmOp;
use rue_types::Type;

use crate::{LintContext, LintInput, check_has_length};

pub(crate) const LINTS: &[Lint] = &[Lint::UncheckedInput];

/// Paths into a value are cut off after this many fields, so that recursion over a list reaches a
/// fixed point. A shorter path covers everything inside of it, so this only loses precision.
const MAX_PATH: usize = 3;

/// The most times the summaries are recomputed before giving up on a fixed point.
const MAX_ITERATIONS: usize = 32;

/// Where an unchecked value comes from.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Root {
    /// A parameter of the function being analyzed, by index. For `main`, it's the solution.
    Param(usize),
    /// The result of calling a function value or an `extern fn`.
    Call(HirId),
}

/// Part of an unchecked value: the value itself, or a field inside of it.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Origin {
    root: Root,
    path: Vec<String>,
}

impl Origin {
    /// Whether one of the checked origins is this one, or contains it.
    fn checked_by(&self, checked: &BTreeSet<Origin>) -> bool {
        checked
            .iter()
            .any(|check| check.root == self.root && self.path.starts_with(&check.path))
    }

    fn with_field(mut self, name: &str) -> Self {
        if self.path.len() < MAX_PATH {
            self.path.push(name.to_string());
        }
        self
    }
}

/// What's known about a value.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
struct Flow {
    /// The unchecked values that this value is, or is made of, without being changed.
    direct: BTreeSet<Origin>,
    /// The unchecked values inside of this value that are in a field of a condition that relies on
    /// their length, along with the node of the field's value.
    deferred: BTreeMap<Origin, HirId>,
    /// Whether the value is exactly its `direct` origin, so that reading a field of it reads the
    /// same field of the origin.
    exact: bool,
    /// The flow of each field, if the value is a struct initializer.
    fields: Option<BTreeMap<String, Flow>>,
}

impl Flow {
    fn origin(origin: Origin) -> Self {
        Self {
            direct: BTreeSet::from([origin]),
            exact: true,
            ..Self::default()
        }
    }

    /// The unchecked values at the path inside of this value.
    fn select(&self, path: &[String]) -> BTreeSet<Origin> {
        let Some((name, rest)) = path.split_first() else {
            return self.direct.clone();
        };

        if let Some(field) = self.fields.as_ref().and_then(|fields| fields.get(name)) {
            return field.select(rest);
        }

        if self.exact {
            return self
                .direct
                .iter()
                .map(|origin| {
                    path.iter()
                        .fold(origin.clone(), |origin, name| origin.with_field(name))
                })
                .collect();
        }

        self.direct.clone()
    }

    /// The flow of a field of this value.
    fn field(&self, name: &str) -> Flow {
        if let Some(field) = self.fields.as_ref().and_then(|fields| fields.get(name)) {
            return field.clone();
        }

        if self.exact {
            return Flow {
                direct: self
                    .direct
                    .iter()
                    .map(|origin| origin.clone().with_field(name))
                    .collect(),
                deferred: self.deferred.clone(),
                exact: true,
                fields: None,
            };
        }

        self.clone()
    }

    fn union(&mut self, other: Flow) {
        self.exact = false;
        self.fields = None;
        self.direct.extend(other.direct);
        for (origin, site) in other.deferred {
            self.deferred.entry(origin).or_insert(site);
        }
    }
}

/// A concatenation whose operands are only of known length if some unchecked values are checked.
#[derive(Debug, Clone, PartialEq, Eq)]
struct Concat {
    site: HirId,
    /// The number of operands whose type doesn't have a known length.
    unknown: usize,
    /// For each other operand that's unchecked, the values it's made of.
    operands: Vec<BTreeSet<Origin>>,
    /// The calls that the concatenation was reached through, from the outermost.
    calls: Vec<HirId>,
}

impl Concat {
    /// Removes the values that are checked, and returns whether it's still ambiguous.
    fn retain_unchecked(&mut self, checked: impl Fn(&Origin) -> bool) -> bool {
        for operand in &mut self.operands {
            operand.retain(|origin| !checked(origin));
        }
        self.operands.retain(|operand| !operand.is_empty());
        ambiguous(self.unknown, &self.operands)
    }
}

/// Whether a concatenation can be split more than one way: at most one operand has an unknown
/// length by its type, but counting the unchecked operands, at least two do. The same value in
/// several operands counts once, since it has the same length in each, and so does a value that
/// can be one of two operands, depending on a condition.
fn ambiguous(unknown: usize, operands: &[BTreeSet<Origin>]) -> bool {
    let values = operands.iter().flatten().collect::<BTreeSet<_>>().len();
    let unchecked = if values == 1 { 1 } else { operands.len() };
    unknown <= 1 && unknown + unchecked >= 2
}

/// An unchecked value in a deferred condition that's hashed.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct Deferred {
    origin: Origin,
    /// The value of the condition's field.
    site: HirId,
}

/// What a function does with its parameters, in terms of [`Root::Param`], and with the results of
/// the function values it calls.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
struct Summary {
    returns: Flow,
    /// The values whose length is checked whenever the function returns.
    checked: BTreeSet<Origin>,
    concats: Vec<Concat>,
    /// Values in deferred conditions that are hashed, with the calls they were hashed through.
    deferred: BTreeMap<Deferred, Vec<HirId>>,
    /// The values that are hashed as a whole, so the deferred conditions inside of them are too.
    hashed: BTreeSet<Origin>,
}

pub(crate) fn check(cx: &mut LintContext<'_>, input: &LintInput<'_>) {
    let mut summaries = HashMap::new();

    for main in mains(cx, input) {
        summarize(cx, input, main, &mut summaries);
        report(cx, main, &summaries[&main]);
    }
}

/// The `main` functions of the files being compiled, in a fixed order.
fn mains(cx: &LintContext<'_>, input: &LintInput<'_>) -> Vec<SymbolId> {
    let mut mains: Vec<SymbolId> = input
        .entrypoints
        .iter()
        .filter_map(|declaration| match *declaration {
            Declaration::Symbol(symbol) => Some(symbol),
            Declaration::Type(_) => None,
        })
        .filter(|&symbol| {
            matches!(
                cx.db().symbol(symbol),
                Symbol::Function(function)
                    if function.name.as_ref().is_some_and(|name| name.text() == "main")
            )
        })
        .collect();

    mains.sort();
    mains
}

/// Computes the summaries of the function and every function it calls, until they stop changing.
fn summarize(
    cx: &mut LintContext<'_>,
    input: &LintInput<'_>,
    function: SymbolId,
    summaries: &mut HashMap<SymbolId, Summary>,
) {
    let mut functions = vec![function];

    for _ in 0..MAX_ITERATIONS {
        let mut changed = false;
        let mut index = 0;

        while let Some(&function) = functions.get(index) {
            index += 1;

            let mut analysis = Analysis::new(cx, input, summaries, function);
            let summary = analysis.run();

            for callee in analysis.callees {
                if !functions.contains(&callee) {
                    functions.push(callee);
                    changed = true;
                }
            }

            if summaries.get(&function) != Some(&summary) {
                summaries.insert(function, summary);
                changed = true;
            }
        }

        if !changed {
            break;
        }
    }
}

/// The calls a concatenation is first reached through from `main`, and the names of its unchecked
/// values.
type ReportedConcat = (Vec<HirId>, BTreeSet<String>);

fn report(cx: &mut LintContext<'_>, main: SymbolId, summary: &Summary) {
    // Each concatenation is reported once where it's reached from `main`, naming every unchecked
    // value in it
    let mut concats: BTreeMap<(HirId, Option<HirId>), ReportedConcat> = BTreeMap::new();

    for concat in &summary.concats {
        let (_, names) = concats
            .entry((concat.site, concat.calls.first().copied()))
            .or_insert_with(|| (concat.calls.clone(), BTreeSet::new()));

        for origin in concat.operands.iter().flatten() {
            names.insert(origin_name(cx, main, origin));
        }
    }

    for (site, (calls, names)) in concats.into_iter().map(|((site, _), value)| (site, value)) {
        let names = names.into_iter().collect::<Vec<_>>();
        let (values, are) = if names.len() == 1 {
            ("value", "is")
        } else {
            ("values", "are")
        };

        let message = format!(
            "The {values} {} {are} from the solution, but nothing checks {}, so the concatenation can be split into different values that give the same bytes",
            join_names(&names),
            if names.len() == 1 {
                "its length"
            } else {
                "their lengths"
            },
        );

        report_at(
            cx,
            site,
            &calls,
            message,
            "The concatenation is here",
            "Declare it with a type that doesn't have a fixed length, such as `Bytes`, and check it with `assert value is Bytes32`; or compute a coin id with `coinid`, which checks its arguments",
        );
    }

    let mut deferred: BTreeMap<(HirId, Option<HirId>, String), &[HirId]> = BTreeMap::new();

    for (value, calls) in &summary.deferred {
        let name = origin_name(cx, main, &value.origin);
        deferred
            .entry((value.site, calls.first().copied(), name))
            .or_insert(calls);
    }

    for ((site, _, name), calls) in deferred {
        let message = format!(
            "The value `{name}` is from the solution, but nothing checks its length, and it's put in a condition that's hashed rather than output, so the condition is only checked when a later spend outputs it, which then fails"
        );

        report_at(
            cx,
            site,
            calls,
            message,
            "The condition's field is here",
            "Declare it with a type that doesn't have a fixed length, such as `Bytes`, and check it with `assert value is Bytes32`",
        );
    }
}

/// Reports the diagnostic at the outermost call in `main` that reaches the site, or at the site
/// itself if it's in `main`, so that it can be allowed there.
fn report_at(
    cx: &mut LintContext<'_>,
    site: HirId,
    calls: &[HirId],
    message: String,
    site_note: &str,
    help: &str,
) {
    let Some(site) = cx.db().hir_srcloc(site).cloned() else {
        return;
    };

    let srcloc = calls
        .first()
        .and_then(|call| cx.db().hir_srcloc(*call).cloned())
        .unwrap_or_else(|| site.clone());

    if let Some(diagnostic) = cx.report(Lint::UncheckedInput, srcloc, message) {
        if !calls.is_empty() {
            diagnostic.notes.push(Note::at(site, site_note));
        }
        diagnostic.notes.push(Note::new(help));
    }
}

fn join_names(names: &[String]) -> String {
    let names = names
        .iter()
        .map(|name| format!("`{name}`"))
        .collect::<Vec<_>>();

    match names.as_slice() {
        [] => String::new(),
        [name] => name.clone(),
        [rest @ .., last] => format!("{} and {last}", rest.join(", ")),
    }
}

/// How an unchecked value is written in a diagnostic: the parameter of `main` and the fields read
/// from it, or the call it's the result of.
fn origin_name(cx: &LintContext<'_>, main: SymbolId, origin: &Origin) -> String {
    let root = match origin.root {
        Root::Param(index) => {
            let Symbol::Function(function) = cx.db().symbol(main) else {
                unreachable!("`main` is a function");
            };

            function
                .parameters
                .get(index)
                .and_then(|param| cx.db().symbol(*param).name())
                .map_or_else(|| "_".to_string(), |name| name.text().to_string())
        }
        // A call written over several lines is shown on one
        Root::Call(call) => cx.db().hir_srcloc(call).map_or_else(
            || "_".to_string(),
            |srcloc| {
                srcloc
                    .text()
                    .split_whitespace()
                    .collect::<Vec<_>>()
                    .join(" ")
            },
        ),
    };

    // The fields are named up to the first item of a list or pair, which can be any of its items
    std::iter::once(root)
        .chain(
            origin
                .path
                .iter()
                .take_while(|name| *name != "first" && *name != "rest")
                .cloned(),
        )
        .collect::<Vec<_>>()
        .join(".")
}

/// Whether the name was declared in the standard library.
fn is_std(name: Option<&rue_diagnostic::Name>) -> bool {
    name.and_then(|name| name.srcloc())
        .is_some_and(|srcloc: &SrcLoc| matches!(srcloc.source.kind, SourceKind::Std(_)))
}

/// The analysis of one function, which produces its summary.
struct Analysis<'c, 'a, 'i> {
    cx: &'c mut LintContext<'a>,
    input: &'c LintInput<'i>,
    summaries: &'c HashMap<SymbolId, Summary>,
    parameters: Vec<SymbolId>,
    body: HirId,
    summary: Summary,
    /// The functions that are called, whose summaries are needed.
    callees: Vec<SymbolId>,
    bindings: HashMap<SymbolId, Flow>,
    /// The values checked by the conditions of the branches being analyzed.
    branch_checks: Vec<BTreeSet<Origin>>,
    /// The flow of each node, along with whether it's always evaluated.
    flows: HashMap<(HirId, bool), Flow>,
}

impl<'c, 'a, 'i> Analysis<'c, 'a, 'i> {
    fn new(
        cx: &'c mut LintContext<'a>,
        input: &'c LintInput<'i>,
        summaries: &'c HashMap<SymbolId, Summary>,
        function: SymbolId,
    ) -> Self {
        let Symbol::Function(symbol) = cx.db().symbol(function) else {
            unreachable!("only functions are analyzed");
        };

        let parameters = symbol.parameters.clone();
        let body = symbol.body;

        Self {
            cx,
            input,
            summaries,
            parameters,
            body,
            summary: Summary::default(),
            callees: Vec::new(),
            bindings: HashMap::new(),
            branch_checks: Vec::new(),
            flows: HashMap::new(),
        }
    }

    fn run(&mut self) -> Summary {
        let returns = self.flow(self.body, true);
        self.summary.returns.union(returns);

        // A check counts wherever it is in the function, even after the use, since if it fails,
        // the whole spend does
        let checked = self.summary.checked.clone();

        self.summary
            .concats
            .retain_mut(|concat| concat.retain_unchecked(|origin| origin.checked_by(&checked)));

        // The same concatenation reached through different calls (or around a recursive call
        // again) is kept once, with the first calls it was reached through
        let mut seen = Vec::new();
        self.summary.concats.retain(|concat| {
            let key = (concat.site, concat.operands.clone());
            if seen.contains(&key) {
                false
            } else {
                seen.push(key);
                true
            }
        });

        self.summary
            .deferred
            .retain(|deferred, _| !deferred.origin.checked_by(&checked));

        std::mem::take(&mut self.summary)
    }

    fn known_length(&mut self, hir: HirId) -> Option<usize> {
        let ty = self.cx.db().hir_type(hir)?;
        self.cx.known_atom_length(ty)
    }

    fn checked_in_branch(&self, origin: &Origin) -> bool {
        self.branch_checks
            .iter()
            .any(|checked| origin.checked_by(checked))
    }

    /// Records that the values are checked, if the check is always evaluated.
    fn check(&mut self, flow: Flow, always: bool) {
        if always {
            self.summary.checked.extend(flow.direct);
        }
    }

    /// The flow of the node. If `always` is set, the node is evaluated whenever the function
    /// returns, so the checks in it count everywhere in the function.
    fn flow(&mut self, hir: HirId, always: bool) -> Flow {
        if let Some(flow) = self.flows.get(&(hir, always)) {
            return flow.clone();
        }

        let flow = self.compute_flow(hir, always);
        self.flows.insert((hir, always), flow.clone());
        flow
    }

    fn compute_flow(&mut self, hir: HirId, always: bool) -> Flow {
        let node = self.cx.db().hir(hir).clone();

        match node {
            Hir::Reference(symbol) => self.reference(hir, symbol, always),
            Hir::FieldAccess(access) => {
                let value = self.flow(access.value, always);

                match access.kind {
                    FieldKind::Path(_) => value.field(&access.name),
                    FieldKind::Length => Flow::default(),
                }
            }
            // The items of a `for` loop are read with `first` and `rest`
            Hir::Unary(op @ (UnaryOp::First | UnaryOp::Rest), value) => {
                let value = self.flow(value, always);

                if value.exact {
                    value.field(if op == UnaryOp::First {
                        "first"
                    } else {
                        "rest"
                    })
                } else {
                    value
                }
            }
            Hir::Cast(value) => self.flow(value, always),
            Hir::Pair(first, rest) => {
                let mut flow = self.flow(first, always);
                flow.union(self.flow(rest, always));
                flow
            }
            Hir::List(list) => {
                let mut flow = Flow::default();
                for item in list.items {
                    flow.union(self.flow(item, always));
                }
                flow
            }
            Hir::Struct(initializer) => {
                let condition = self.condition_type(hir);
                let mut flow = Flow::default();
                let mut fields = BTreeMap::new();

                for field in &initializer.fields {
                    let value = self.flow(field.value, always);

                    if let Some(ty) = condition
                        && !field.is_default
                        && self.field_has_length(ty, &field.name)
                    {
                        for origin in &value.direct {
                            flow.deferred.entry(origin.clone()).or_insert(field.value);
                        }
                    }

                    fields.insert(field.name.clone(), value.clone());
                    flow.union(value);
                }

                if let Some(tail) = initializer.tail_value() {
                    flow.union(self.flow(tail, always));
                } else {
                    flow.fields = Some(fields);
                }

                flow
            }
            Hir::Block(block) => self.block(&block.statements, block.body, always),
            Hir::If(condition, then, else_, _) => {
                self.flow(condition, always);

                let checks = self.condition_checks(condition);
                self.branch_checks.push(checks);
                let mut flow = self.flow(then, false);
                self.branch_checks.pop();

                flow.union(self.flow(else_, false));
                flow
            }
            Hir::FunctionCall(call) => self.call(hir, &call, always),
            Hir::Binary(BinaryOp::Concat, ..) => {
                let mut operands = Vec::new();
                flatten_concat(self.cx.db(), hir, &mut operands);
                self.concat(hir, &operands, always);
                Flow::default()
            }
            Hir::ClvmOp(ClvmOp::Concat | ClvmOp::Sha256, list) => {
                if let Hir::List(items) = self.cx.db().hir(list).clone() {
                    self.concat(hir, &items.items, always);
                } else {
                    self.flow(list, always);
                }
                Flow::default()
            }
            Hir::Binary(BinaryOp::And | BinaryOp::Or, left, right) => {
                self.flow(left, always);
                self.flow(right, false);
                Flow::default()
            }
            Hir::CoinId(parent, puzzle_hash, amount) => {
                let parent = self.flow(parent, always);
                let puzzle_hash = self.flow(puzzle_hash, always);
                self.flow(amount, always);
                self.check(parent, always);
                self.check(puzzle_hash, always);
                Flow::default()
            }
            Hir::Binary(BinaryOp::G1Add | BinaryOp::G1Subtract, left, right) => {
                let left = self.flow(left, always);
                let right = self.flow(right, always);
                self.check(left, always);
                self.check(right, always);
                Flow::default()
            }
            Hir::Unary(UnaryOp::G1Negate, value) => {
                let value = self.flow(value, always);
                self.check(value, always);
                Flow::default()
            }
            Hir::BlsVerify(signature, public_keys_and_messages) => {
                let signature = self.flow(signature, always);
                self.check(signature, always);
                for (index, value) in public_keys_and_messages.into_iter().enumerate() {
                    let value = self.flow(value, always);
                    // The public keys and messages alternate
                    if index % 2 == 0 {
                        self.check(value, always);
                    }
                }
                Flow::default()
            }
            Hir::Lambda(_) | Hir::Const(_) => Flow::default(),
            _ => {
                for child in node.children().collect::<Vec<_>>() {
                    self.flow(child, always);
                }
                Flow::default()
            }
        }
    }

    fn reference(&mut self, hir: HirId, symbol: SymbolId, always: bool) -> Flow {
        if let Some(index) = self.parameters.iter().position(|&param| param == symbol) {
            let Symbol::Parameter(parameter) = self.cx.db().symbol(symbol) else {
                return Flow::default();
            };
            let declared = parameter.ty;

            // A parameter that's been narrowed to a type with a known length is checked, if the
            // narrowing checks the length
            if let Some(ty) = self.cx.db().hir_type(hir)
                && self.cx.known_atom_length(ty).is_some()
                && self.cx.known_atom_length(declared).is_none()
                && self.cx.narrowing_checks_length(declared, ty)
            {
                return Flow::default();
            }

            return Flow::origin(Origin {
                root: Root::Param(index),
                path: Vec::new(),
            });
        }

        match self.cx.db().symbol(symbol).clone() {
            Symbol::Binding(binding) if binding.inline => self.flow(binding.value.hir, always),
            Symbol::Binding(binding) => {
                if let Some(flow) = self.bindings.get(&symbol) {
                    return flow.clone();
                }
                let flow = self.flow(binding.value.hir, false);
                self.bindings.insert(symbol, flow.clone());
                flow
            }
            _ => Flow::default(),
        }
    }

    fn block(&mut self, statements: &[Statement], body: Option<HirId>, always: bool) -> Flow {
        let body = rue_hir::reachable_body(statements, body);

        for statement in rue_hir::reachable_statements(statements) {
            match statement {
                Statement::Let(symbol, _) => {
                    let Symbol::Binding(binding) = self.cx.db().symbol(*symbol).clone() else {
                        continue;
                    };

                    if binding.inline {
                        continue;
                    }

                    // A binding is evaluated where it's referenced, unless it may fail, in which
                    // case it's evaluated here for its effect
                    let evaluated =
                        always && self.input.failures().may_be_effect(self.cx.db(), *symbol);
                    let flow = self.flow(binding.value.hir, evaluated);
                    self.bindings.insert(*symbol, flow);
                }
                Statement::Expr(statement) => {
                    self.flow(statement.hir, always);
                }
                Statement::Control(statement) => {
                    self.flow(statement.hir, always);
                }
                Statement::If(statement) => {
                    self.flow(statement.condition, always);

                    let checks = self.condition_checks(statement.condition);
                    self.branch_checks.push(checks);
                    self.flow(statement.then, false);
                    self.branch_checks.pop();
                }
                Statement::Return(value, _) => {
                    let flow = self.flow(*value, always);
                    self.summary.returns.union(flow);
                }
                Statement::Assert(condition, ..) => {
                    self.flow(*condition, always);

                    if always {
                        let checks = self.condition_checks(*condition);
                        self.summary.checked.extend(checks);
                    }
                }
                Statement::Raise(..) | Statement::Debug(..) => {}
            }
        }

        body.map(|body| self.flow(body, always)).unwrap_or_default()
    }

    /// The values whose length is checked if the condition is true.
    fn condition_checks(&mut self, condition: HirId) -> BTreeSet<Origin> {
        match self.cx.db().hir(condition).clone() {
            Hir::Guard(guard) => {
                let checks_length = self.cx.known_atom_length(guard.ty).is_some()
                    && guard.value_uses() > 0
                    && check_has_length(&guard.runtime_check());

                if checks_length {
                    self.flow(guard.value, false).direct
                } else {
                    BTreeSet::new()
                }
            }
            Hir::Binary(BinaryOp::Eq, left, right) => {
                let mut checks = BTreeSet::new();

                for (value, other) in [(left, right), (right, left)] {
                    let other_flow = self.flow(other, false);

                    // Equal to a value whose length is known and that isn't itself unchecked
                    if self.known_length(other).is_some()
                        && other_flow
                            .direct
                            .iter()
                            .all(|origin| origin.checked_by(&self.summary.checked))
                    {
                        checks.extend(self.flow(value, false).direct);
                    }
                }

                checks
            }
            Hir::Binary(BinaryOp::And, left, right) => {
                let mut checks = self.condition_checks(left);
                checks.extend(self.condition_checks(right));
                checks
            }
            _ => BTreeSet::new(),
        }
    }

    fn concat(&mut self, site: HirId, operands: &[HirId], always: bool) {
        let mut unknown = 0;
        let mut unchecked = Vec::new();

        for &operand in operands {
            let flow = self.flow(operand, always);

            // A cast to `Bytes` doesn't change how long the value is
            let value = self.cx.db().without_casts(operand);

            if self.known_length(value).is_none() {
                unknown += 1;
                continue;
            }

            let origins: BTreeSet<Origin> = flow
                .direct
                .into_iter()
                .filter(|origin| !self.checked_in_branch(origin))
                .collect();

            if !origins.is_empty() {
                unchecked.push(origins);
            }
        }

        if ambiguous(unknown, &unchecked) {
            self.summary.concats.push(Concat {
                site,
                unknown,
                operands: unchecked,
                calls: Vec::new(),
            });
        }
    }

    fn call(&mut self, hir: HirId, call: &FunctionCall, always: bool) -> Flow {
        let args: Vec<Flow> = call
            .args
            .iter()
            .map(|&arg| self.flow(arg, always))
            .collect();

        let function = self.cx.db().without_casts(call.function);
        let callee = match self.cx.db().hir(function) {
            Hir::Reference(symbol) => match self.cx.db().symbol(*symbol) {
                Symbol::Function(function) if function.kind != FunctionKind::External => {
                    Some((*symbol, function.clone()))
                }
                _ => None,
            },
            _ => None,
        };

        // The result of a function value or an `extern fn` is unchecked, like the solution
        let Some((callee, function)) = callee else {
            self.flow(call.function, always);

            return Flow::origin(Origin {
                root: Root::Call(hir),
                path: Vec::new(),
            });
        };

        // The standard library's `tree_hash` is where conditions are hashed instead of output
        if is_std(function.name.as_ref())
            && function
                .name
                .as_ref()
                .is_some_and(|name| name.text() == "tree_hash")
        {
            if let Some(arg) = args.first() {
                self.hash(hir, arg);
            }
            return Flow::default();
        }

        if !self.callees.contains(&callee) {
            self.callees.push(callee);
        }

        let Some(summary) = self.summaries.get(&callee).cloned() else {
            return Flow::default();
        };

        // The arguments that are passed for each parameter, which are the rest of them for a rest
        // parameter
        let fixed = function.fixed_parameters().len();
        let arguments_for = |root: Root| -> Vec<&Flow> {
            match root {
                Root::Param(index) if index < fixed || function.nil_terminated => {
                    args.get(index).into_iter().collect()
                }
                Root::Param(_) => args.iter().skip(fixed).collect(),
                Root::Call(_) => Vec::new(),
            }
        };

        // An origin of the callee, in terms of the caller
        let translate = |origin: &Origin| -> BTreeSet<Origin> {
            match origin.root {
                Root::Param(_) => arguments_for(origin.root)
                    .into_iter()
                    .flat_map(|arg| arg.select(&origin.path))
                    .collect(),
                Root::Call(_) => BTreeSet::from([origin.clone()]),
            }
        };

        let mut returns = Flow::default();

        for origin in &summary.returns.direct {
            returns.direct.extend(translate(origin));

            // Deferred conditions in the arguments are returned with them
            for arg in arguments_for(origin.root) {
                for (origin, site) in &arg.deferred {
                    returns.deferred.entry(origin.clone()).or_insert(*site);
                }
            }
        }

        for (origin, site) in &summary.returns.deferred {
            for origin in translate(origin) {
                returns.deferred.entry(origin).or_insert(*site);
            }
        }

        if always {
            for origin in &summary.checked {
                self.summary.checked.extend(translate(origin));
            }
        }

        for concat in &summary.concats {
            let mut concat = Concat {
                site: concat.site,
                unknown: concat.unknown,
                operands: concat
                    .operands
                    .iter()
                    .map(|operand| operand.iter().flat_map(&translate).collect())
                    .collect(),
                calls: std::iter::once(hir)
                    .chain(concat.calls.iter().copied())
                    .collect(),
            };

            if concat.retain_unchecked(|origin| self.checked_in_branch(origin)) {
                self.summary.concats.push(concat);
            }
        }

        for (deferred, calls) in &summary.deferred {
            for origin in translate(&deferred.origin) {
                if self.checked_in_branch(&origin) {
                    continue;
                }

                let calls = std::iter::once(hir).chain(calls.iter().copied()).collect();
                self.summary
                    .deferred
                    .entry(Deferred {
                        origin,
                        site: deferred.site,
                    })
                    .or_insert(calls);
            }
        }

        for origin in &summary.hashed {
            for arg in arguments_for(origin.root) {
                self.hash(hir, arg);
            }
        }

        returns
    }

    /// Records that the value is hashed, so the deferred conditions in it are too.
    fn hash(&mut self, call: HirId, value: &Flow) {
        self.summary.hashed.extend(value.direct.iter().cloned());

        for (origin, &site) in &value.deferred {
            if self.checked_in_branch(origin) {
                continue;
            }

            self.summary
                .deferred
                .entry(Deferred {
                    origin: origin.clone(),
                    site,
                })
                .or_insert_with(|| vec![call]);
        }
    }

    /// The type of the struct, if it's one of the standard library's conditions.
    fn condition_type(&mut self, hir: HirId) -> Option<rue_types::TypeId> {
        let ty = self.cx.db().hir_type(hir)?;
        let semantic = self.cx.unwrap_semantic(ty);

        let Type::Struct(structure) = self.cx.db().ty(semantic) else {
            return None;
        };

        let is_condition = is_std(structure.name.as_ref())
            && structure
                .name
                .as_ref()
                .is_some_and(|name| name.text().starts_with("Condition::"));

        is_condition.then_some(ty)
    }

    /// Whether consensus relies on the length of the field, because its type has a known length.
    fn field_has_length(&mut self, ty: rue_types::TypeId, name: &str) -> bool {
        self.cx
            .struct_field_type(ty, name)
            .is_some_and(|field| self.cx.known_atom_length(field).is_some())
    }
}

fn flatten_concat(db: &rue_hir::Database, hir: HirId, operands: &mut Vec<HirId>) {
    match db.hir(hir) {
        Hir::Binary(BinaryOp::Concat, left, right) => {
            flatten_concat(db, *left, operands);
            flatten_concat(db, *right, operands);
        }
        _ => operands.push(hir),
    }
}
