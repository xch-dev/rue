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
//! length, or of equality with a value of known length, and `coinid` and the BLS and secp
//! operations, which fail for the wrong length. A type guard that checks the length checks it
//! inside the branch it guards, and after a branch that never falls through if it's negated
//! (`if !(value is Bytes32) { raise; }`). These all fail at runtime, so the optimizer keeps them
//! wherever the program evaluates them. Reading a field or doing arithmetic says nothing about a
//! length, so it never counts as a check, and neither does narrowing that only rules out other
//! members of a union (`if value is nil { return ...; }`).
//!
//! Values that a `for` loop or a lambda captures from the function around it aren't followed,
//! so a use of one inside of the loop isn't reported.

use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};

use rue_diagnostic::{Lint, Note, SourceKind, SrcLoc};
use rue_hir::{
    BinaryOp, Declaration, FieldKind, FunctionCall, FunctionKind, Hir, HirId, Statement, Symbol,
    SymbolId, UnaryOp,
};
use rue_lir::ClvmOp;
use rue_types::{Check, Type};

use crate::{LintContext, LintInput};

pub(crate) const LINTS: &[Lint] = &[Lint::UncheckedInput];

/// Paths into a value keep this many fields before the last one, so that recursion over a list
/// reaches a fixed point. The fields in between are replaced by [`CUT`], so a path that's cut still
/// ends in the field it reads, and a check of one field doesn't cover the fields next to it.
const MAX_PATH: usize = 3;

/// What replaces the fields in the middle of a path that's too long.
const CUT: &str = "...";

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
        if self.path.len() > MAX_PATH {
            self.path.pop();
        } else if self.path.len() == MAX_PATH {
            self.path.push(CUT.to_string());
        }
        self.path.push(name.to_string());
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
    /// The flow of each field, if the value is a struct initializer, a pair or a list (whose
    /// fields are `first` and `rest`).
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
        path.iter()
            .fold(self.clone(), |flow, name| flow.field(name))
            .direct
    }

    /// The flow of a field of this value, or of the item or the rest of it if it's a pair
    /// (`first` and `rest`).
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

    /// A pair of the two values, whose fields are `first` and `rest`.
    fn pair(first: Flow, rest: Flow) -> Self {
        let fields = BTreeMap::from([
            ("first".to_string(), first.clone()),
            ("rest".to_string(), rest.clone()),
        ]);

        let mut flow = first;
        flow.union(rest);
        flow.fields = Some(fields);
        flow
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

/// The calls a concatenation is first reached through from `main`, and its unchecked values.
type ReportedConcat<'s> = (Vec<HirId>, BTreeSet<&'s Origin>);

fn report(cx: &mut LintContext<'_>, main: SymbolId, summary: &Summary) {
    // Each concatenation is reported once where it's reached from `main`, naming every unchecked
    // value in it
    let mut concats: BTreeMap<(HirId, Option<HirId>), ReportedConcat<'_>> = BTreeMap::new();

    for concat in &summary.concats {
        let (_, origins) = concats
            .entry((concat.site, concat.calls.first().copied()))
            .or_insert_with(|| (concat.calls.clone(), BTreeSet::new()));

        origins.extend(concat.operands.iter().flatten());
    }

    for (site, (calls, origins)) in concats.into_iter().map(|((site, _), value)| (site, value)) {
        // Several items of the same list have the same name, so they're counted by name
        let mut counts: BTreeMap<(String, Part), usize> = BTreeMap::new();
        for origin in &origins {
            *counts.entry(origin_name(cx, main, origin)).or_default() += 1;
        }

        let names = counts
            .into_iter()
            .map(|((path, part), count)| part.describe(&path, count))
            .collect::<Vec<_>>();

        let message = if origins.len() == 1 {
            format!(
                "{} is from the solution, but nothing checks its length, so the concatenation can be split into different values that give the same bytes",
                join_names(&names),
            )
        } else {
            format!(
                "{} are from the solution, but nothing checks their lengths, so the concatenation can be split into different values that give the same bytes",
                join_names(&names),
            )
        };

        report_at(
            cx,
            site,
            &calls,
            &message,
            "The concatenation is here",
            "Declare it with a type that doesn't have a fixed length, such as `Bytes`, and check it with `assert value is Bytes32`; or compute a coin id with `coinid`, which checks its arguments",
        );
    }

    let mut deferred: BTreeMap<(HirId, Option<HirId>, String), &[HirId]> = BTreeMap::new();

    for (value, calls) in &summary.deferred {
        let (path, part) = origin_name(cx, main, &value.origin);
        let name = part.describe(&path, 1);
        deferred
            .entry((value.site, calls.first().copied(), name))
            .or_insert(calls);
    }

    for ((site, _, name), calls) in deferred {
        let message = format!(
            "{name} is from the solution, but nothing checks its length, and it's put in a condition that's hashed rather than output, so the condition is only checked when a later spend outputs it, which then fails"
        );

        report_at(
            cx,
            site,
            calls,
            &message,
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
    message: &str,
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

    let message = capitalize(message);

    if let Some(diagnostic) = cx.report(Lint::UncheckedInput, srcloc, message) {
        if !calls.is_empty() {
            diagnostic.notes.push(Note::at(site, site_note));
        }
        diagnostic.notes.push(Note::new(help));
    }
}

fn capitalize(text: &str) -> String {
    let mut chars = text.chars();
    chars
        .next()
        .map(|first| first.to_uppercase().chain(chars).collect())
        .unwrap_or_default()
}

fn join_names(names: &[String]) -> String {
    match names {
        [] => String::new(),
        [name] => name.clone(),
        [rest @ .., last] => format!("{} and {last}", rest.join(", ")),
    }
}

/// How an unchecked value is written in a diagnostic: the parameter of `main` and the fields read
/// from it, or the call it's the result of. The path stops at an item of a list or pair, since it
/// can be any of them, or at a field that's too deep to name, and which part of it the value is
/// is returned along with it.
fn origin_name(cx: &LintContext<'_>, main: SymbolId, origin: &Origin) -> (String, Part) {
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
        Root::Call(call) => cx.db().hir_srcloc(call).map_or_else(
            || "_".to_string(),
            |srcloc| rue_hir::one_line(srcloc.text()),
        ),
    };

    let fields = origin
        .path
        .iter()
        .take_while(|name| !matches!(name.as_str(), "first" | "rest" | CUT));

    let part = match origin.path.get(fields.clone().count()).map(String::as_str) {
        None => Part::Whole,
        Some(CUT) => Part::Inside,
        Some(_) => Part::Item,
    };

    let path = std::iter::once(root)
        .chain(fields.cloned())
        .collect::<Vec<_>>()
        .join(".");

    (path, part)
}

/// Which part of the named value an unchecked value is.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Part {
    Whole,
    /// An item of a list or pair.
    Item,
    /// A field too deep inside of it to name.
    Inside,
}

impl Part {
    fn describe(self, path: &str, count: usize) -> String {
        match (self, count) {
            (Self::Whole, _) => format!("`{path}`"),
            (Self::Item, 1) => format!("an item of `{path}`"),
            (Self::Item, _) => format!("items of `{path}`"),
            (Self::Inside, 1) => format!("a value inside of `{path}`"),
            (Self::Inside, _) => format!("values inside of `{path}`"),
        }
    }
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
    /// The values checked by the conditions of the branches being analyzed, and by the negated
    /// conditions of the branches that left the blocks being analyzed (`if !(value is Bytes32) {
    /// return ...; }`), which hold for the rest of their block.
    branch_checks: Vec<BTreeSet<Origin>>,
    /// The flow of each node, along with whether it's always evaluated, for nodes outside of
    /// checking branches. Inside of one, a node such as the value of an `inline let` would be
    /// remembered without the concatenations that the branch's checks hide.
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
        let mut seen = HashSet::new();
        self.summary
            .concats
            .retain(|concat| seen.insert((concat.site, concat.operands.clone())));

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
        if !self.branch_checks.is_empty() {
            return self.compute_flow(hir, always);
        }

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
            Hir::Reference(symbol) => self.reference(symbol, always),
            Hir::FieldAccess(access) => {
                let value = self.flow(access.value, always);

                match access.kind {
                    FieldKind::Path(_) => value.field(&access.name),
                    FieldKind::Length => Flow::default(),
                }
            }
            // The items of a `for` loop are read with `first` and `rest`
            Hir::Unary(op @ (UnaryOp::First | UnaryOp::Rest), value) => {
                self.flow(value, always).field(if op == UnaryOp::First {
                    "first"
                } else {
                    "rest"
                })
            }
            Hir::Cast(value) => self.flow(value, always),
            Hir::Pair(first, rest) => {
                let first = self.flow(first, always);
                let rest = self.flow(rest, always);
                Flow::pair(first, rest)
            }
            // A list is nested pairs, which end in nil or in the last item if it's spread
            Hir::List(list) => {
                let mut items: Vec<Flow> = list
                    .items
                    .iter()
                    .map(|&item| self.flow(item, always))
                    .collect();

                let end = if list.nil_terminated {
                    Flow::default()
                } else {
                    items.pop().unwrap_or_default()
                };

                items
                    .into_iter()
                    .rev()
                    .fold(end, |rest, item| Flow::pair(item, rest))
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
                let mut flow = self.branch(condition, then);
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
            Hir::ClvmOp(ClvmOp::Concat | ClvmOp::Sha256 | ClvmOp::Keccak256, list) => {
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
                self.checked_operands(&[parent, puzzle_hash], always);
                self.flow(amount, always);
                Flow::default()
            }
            // Operations on points fail if a point isn't valid, which includes its length
            Hir::Binary(
                BinaryOp::G1Add | BinaryOp::G1Subtract | BinaryOp::G2Add | BinaryOp::G2Subtract,
                left,
                right,
            ) => {
                self.checked_operands(&[left, right], always);
                Flow::default()
            }
            Hir::Binary(BinaryOp::G1Multiply | BinaryOp::G2Multiply, point, scalar) => {
                self.checked_operands(&[point], always);
                self.flow(scalar, always);
                Flow::default()
            }
            Hir::Unary(UnaryOp::G1Negate | UnaryOp::G2Negate, point) => {
                self.checked_operands(&[point], always);
                Flow::default()
            }
            Hir::ClvmOp(
                ClvmOp::G1Add
                | ClvmOp::G1Subtract
                | ClvmOp::G2Add
                | ClvmOp::G2Subtract
                | ClvmOp::BlsPairingIdentity,
                points,
            ) => {
                self.checked_operands(&[points], always);
                Flow::default()
            }
            Hir::BlsPairingIdentity(points) => {
                self.checked_operands(&points, always);
                Flow::default()
            }
            // The public keys and messages alternate, and only the keys have a fixed length
            Hir::BlsVerify(signature, public_keys_and_messages) => {
                self.checked_operands(&[signature], always);
                for (index, value) in public_keys_and_messages.into_iter().enumerate() {
                    if index % 2 == 0 {
                        self.checked_operands(&[value], always);
                    } else {
                        self.flow(value, always);
                    }
                }
                Flow::default()
            }
            Hir::ClvmOp(ClvmOp::BlsVerify, args) => {
                if let Hir::Pair(signature, pairs) = *self.cx.db().hir(args) {
                    self.checked_operands(&[signature], always);
                    self.flow(pairs, always);
                } else {
                    self.flow(args, always);
                }
                Flow::default()
            }
            // The secp operations fail unless the message hash and the signature have the right
            // length, but the key can also be uncompressed, which is longer
            Hir::Secp256K1Verify(public_key, message, signature)
            | Hir::Secp256R1Verify(public_key, message, signature) => {
                self.flow(public_key, always);
                self.checked_operands(&[message, signature], always);
                Flow::default()
            }
            Hir::Keccak256Verify(value, hash) => {
                self.flow(value, always);
                self.checked_operands(&[hash], always);
                Flow::default()
            }
            Hir::Guard(guard) => {
                self.flow(guard.value, always);
                Flow::default()
            }
            Hir::Lambda(_)
            | Hir::Const(_)
            | Hir::Unresolved
            | Hir::Nil
            | Hir::String(_)
            | Hir::Int(_)
            | Hir::Bytes(_)
            | Hir::Bool(_)
            | Hir::InfinityG1
            | Hir::InfinityG2 => Flow::default(),
            // Everything else computes a new value from its operands, without checking a length
            Hir::Unary(..)
            | Hir::Binary(..)
            | Hir::Substr(..)
            | Hir::G1Map(..)
            | Hir::G2Map(..)
            | Hir::Modpow(..)
            | Hir::ClvmOp(..) => {
                for child in node.children().collect::<Vec<_>>() {
                    self.flow(child, always);
                }
                Flow::default()
            }
        }
    }

    /// Evaluates the operands of an operation that fails unless each of them has the right
    /// length, so they're checked if it's always evaluated.
    fn checked_operands(&mut self, operands: &[HirId], always: bool) {
        for &operand in operands {
            let flow = self.flow(operand, always);
            self.check(flow, always);
        }
    }

    fn reference(&mut self, symbol: SymbolId, always: bool) -> Flow {
        // A parameter that's narrowed is still unchecked: the guard that narrowed it may only
        // have ruled out other members of a union (`is nil`), so only the guards themselves count
        // as checks (see `condition_checks`)
        if let Some(index) = self.parameters.iter().position(|&param| param == symbol) {
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
            // A parameter of another function is one that a `for` loop or a lambda captures, which
            // isn't followed
            Symbol::Parameter(_)
            | Symbol::Constant(_)
            | Symbol::Function(_)
            | Symbol::Builtin(_)
            | Symbol::Module(_)
            | Symbol::Unresolved => Flow::default(),
        }
    }

    fn block(&mut self, statements: &[Statement], body: Option<HirId>, always: bool) -> Flow {
        let body = rue_hir::reachable_body(statements, body);
        let statements = rue_hir::reachable_statements(statements);

        // The checks of conditions whose branches leave the block, which hold for the rest of it
        let branch_checks = self.branch_checks.len();
        let flow = self.statements(statements, body, always);
        self.branch_checks.truncate(branch_checks);
        flow
    }

    fn statements(&mut self, statements: &[Statement], body: Option<HirId>, always: bool) -> Flow {
        for (index, statement) in statements.iter().enumerate() {
            let returned = self.statement(statement, always);

            // After a statement that can return, the rest of the block isn't always evaluated, but
            // the checks in it still hold for the uses in it
            if let Some(empty) = returned
                && always
            {
                let rest = &statements[index + 1..];
                return self.region(&empty, |analysis| analysis.statements(rest, body, true));
            }
        }

        body.map(|body| self.flow(body, always)).unwrap_or_default()
    }

    /// Analyzes part of a function that's only evaluated some of the time, as if it was always
    /// evaluated. The checks in it then cover the uses in it, but not the rest of the function,
    /// except for checks of the items of a value that's empty whenever the part isn't evaluated,
    /// such as a list in `if list is nil { return ...; }`: there are no items then, so the items
    /// that exist are always checked. This is how recursion over a list checks each item.
    fn region(
        &mut self,
        empty: &BTreeSet<Origin>,
        analyze: impl FnOnce(&mut Self) -> Flow,
    ) -> Flow {
        let checked = self.summary.checked.clone();
        let concats = self.summary.concats.len();
        let deferred = std::mem::take(&mut self.summary.deferred);

        let flow = analyze(self);

        let region_checked = std::mem::replace(&mut self.summary.checked, checked);

        let mut region_concats = self.summary.concats.split_off(concats);
        region_concats.retain_mut(|concat| {
            concat.retain_unchecked(|origin| origin.checked_by(&region_checked))
        });
        self.summary.concats.extend(region_concats);

        let region_deferred = std::mem::replace(&mut self.summary.deferred, deferred);
        for (value, calls) in region_deferred {
            if !value.origin.checked_by(&region_checked) {
                self.summary.deferred.entry(value).or_insert(calls);
            }
        }

        // The items of a value that's empty whenever the region isn't evaluated are checked where
        // they exist
        self.summary
            .checked
            .extend(region_checked.into_iter().filter(|origin| {
                empty.iter().any(|empty| {
                    empty.root == origin.root
                        && origin.path.len() > empty.path.len()
                        && origin.path.starts_with(&empty.path)
                })
            }));

        flow
    }

    /// Analyzes the branch that's taken if the condition is true, with the condition's checks.
    fn branch(&mut self, condition: HirId, then: HirId) -> Flow {
        let checks = self.condition_checks(condition);
        self.branch_checks.push(checks);
        let flow = self.flow(then, false);
        self.branch_checks.pop();
        flow
    }

    /// Analyzes a statement. If it can return from the function, this returns the values that are
    /// empty whenever it does.
    fn statement(&mut self, statement: &Statement, always: bool) -> Option<BTreeSet<Origin>> {
        match statement {
            Statement::Let(symbol, _) => {
                let Symbol::Binding(binding) = self.cx.db().symbol(*symbol).clone() else {
                    return None;
                };

                if !binding.inline {
                    // A binding is evaluated where it's referenced, unless it may fail, in which
                    // case it's evaluated here for its effect
                    let evaluated =
                        always && self.input.failures().may_be_effect(self.cx.db(), *symbol);
                    let flow = self.flow(binding.value.hir, evaluated);
                    self.bindings.insert(*symbol, flow);
                }

                None
            }
            Statement::Expr(statement) => {
                self.flow(statement.hir, always);
                None
            }
            Statement::Control(statement) => {
                self.flow(statement.hir, always);
                statement.may_return.then(BTreeSet::new)
            }
            Statement::If(statement) => {
                self.flow(statement.condition, always);
                self.branch(statement.condition, statement.then);

                let may_return = rue_hir::control_may_return(self.cx.db(), statement.then);

                // If the branch never falls through, the condition is false for the rest of the
                // block, as in `if !(value is Bytes32) { raise; }`
                if rue_hir::control_fallthroughs(self.cx.db(), statement.then) == 0 {
                    let checks = self.negated_condition_checks(statement.condition);

                    if always && !may_return {
                        self.summary.checked.extend(checks.iter().cloned());
                    }

                    self.branch_checks.push(checks);
                }

                may_return.then(|| self.empty_if(statement.condition))
            }
            Statement::Return(value, _) => {
                let flow = self.flow(*value, always);
                self.summary.returns.union(flow);
                None
            }
            Statement::Assert(condition, ..) => {
                self.flow(*condition, always);

                if always {
                    let checks = self.condition_checks(*condition);
                    self.summary.checked.extend(checks);
                }

                None
            }
            Statement::Raise(..) | Statement::Debug(..) => None,
        }
    }

    /// The values that are empty atoms if the condition is true, such as the list in `list is nil`.
    fn empty_if(&mut self, condition: HirId) -> BTreeSet<Origin> {
        match self.cx.db().hir(condition).clone() {
            Hir::Guard(guard) if self.cx.known_atom_length(guard.ty) == Some(0) => {
                self.flow(guard.value, false).direct
            }
            _ => BTreeSet::new(),
        }
    }

    /// The values whose length is checked if the condition is false.
    fn negated_condition_checks(&mut self, condition: HirId) -> BTreeSet<Origin> {
        match *self.cx.db().hir(condition) {
            Hir::Unary(UnaryOp::Not, condition) => self.condition_checks(condition),
            Hir::Binary(BinaryOp::Or, left, right) => {
                let mut checks = self.negated_condition_checks(left);
                checks.extend(self.negated_condition_checks(right));
                checks
            }
            _ => BTreeSet::new(),
        }
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
        let callee = rue_hir::resolve_function(self.cx.db(), call.function).and_then(|symbol| {
            match self.cx.db().symbol(symbol) {
                Symbol::Function(function) if function.kind != FunctionKind::External => {
                    Some((symbol, function.clone()))
                }
                _ => None,
            }
        });

        // The arguments of an `inline fn` are only evaluated where its body references them
        let args_always = always
            && callee
                .as_ref()
                .is_none_or(|(_, function)| function.kind != FunctionKind::Inline);

        let args: Vec<Flow> = call
            .args
            .iter()
            .map(|&arg| self.flow(arg, args_always))
            .collect();

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

        let summaries = self.summaries;
        let Some(summary) = summaries.get(&callee) else {
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

/// Whether a guard's runtime check rejects every atom of the wrong length, on every way it can
/// pass.
fn check_has_length(check: &Check) -> bool {
    match check {
        Check::Atom(_) | Check::Impossible => true,
        Check::And(checks) => checks.iter().any(check_has_length),
        Check::Or(checks) => checks.iter().all(check_has_length),
        Check::None | Check::IsAtom { .. } | Check::IsPair { .. } | Check::Pair(..) => false,
    }
}
