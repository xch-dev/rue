//! Reports where a program relies on the length of a value from the solution that its type
//! declares but nothing checks.
//!
//! The parameter types of an entrypoint (`main`, or an export of the main file, which is compiled
//! as a program of its own) are assumptions about the solution, so a `Bytes32` parameter can be
//! an atom of any length. In what follows, `main` stands for any entrypoint. Most code doesn't depend on that, since an operation that needs the
//! right length fails if it isn't, and a spend either succeeds as a whole or fails. Two kinds of
//! code do depend on it, and are reported:
//!
//! 1. **Ambiguous concatenations.** Concatenating bytes can only be split back one way if at most
//!    one of the parts has an unknown length. When the types say that's the case, but some of the
//!    parts with a known length are unchecked values from the solution, a spender can move bytes
//!    from one part to the next and get the same result, as in the bug in the first version of
//!    CATs, where bytes moved from a coin's puzzle hash into its amount without changing its coin
//!    id.
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
//! after the use, since a spend fails as a whole. After a statement that can return, a check only
//! covers the uses in the rest of its block, except that a check of the items of a list that the
//! statement returns for when it's empty (`if list is nil { return ...; }`) covers every item, so
//! that recursion over a list checks each of them. The checks are: an `assert` of a type guard that
//! checks the length, or of equality with a value of known length that isn't itself unchecked;
//! `coinid`'s parent and puzzle hash; the points of the BLS operations; the message hash and
//! signature of the secp operations; and the hash of `keccak256_verify`. A type guard that checks
//! the length checks it inside the branch it guards, and after a branch that never falls through
//! if it's negated (`if !(value is Bytes32) { raise; }`). These all fail at runtime, so the
//! optimizer keeps them wherever the program evaluates them. Reading a field or doing arithmetic
//! says nothing about a length, so it never counts as a check, and neither does narrowing that
//! only rules out other members of a union (`if value is nil { return ...; }`). A check only counts
//! for a value that's exactly one unchecked value, not one of several (`if flag { a } else { b }`).
//! A parameter of `main` marked `#[trusted]` is checked by whoever provides it, such as a curried
//! value that the coin's creator chose, so it's treated as checked, along with everything inside of
//! it, and the warnings suggest marking the parameters they're about.
//!
//! Values that a `for` loop or a lambda captures from the function around it aren't followed,
//! so a use of one inside of the loop isn't reported. Items of a list deeper than the paths keep
//! (see [`MAX_PATH`]) share a path, so a check of one of them counts for the others at the same
//! depth.

use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};

use rowan::{TextRange, TextSize};
use rue_ast::{AstFunctionItem, AstNode, TRUSTED_ATTRIBUTE};
use rue_diagnostic::{Applicability, Lint, Note, SourceKind, SrcLoc, Suggestion};
use rue_hir::{
    BinaryOp, FieldKind, FunctionCall, FunctionKind, Hir, HirId, Statement, Symbol, SymbolId,
    UnaryOp,
};
use rue_lir::ClvmOp;
use rue_types::{Check, Type};

use crate::{LintContext, LintInput, srcloc};

pub(crate) const LINTS: &[Lint] = &[Lint::UncheckedInput];

/// Paths into a value keep this many fields before the last one, so that recursion over a list
/// reaches a fixed point. The fields in between are replaced by [`CUT`], so a path that's cut still
/// ends in the field it reads, and a check of one field doesn't cover the fields next to it.
const MAX_PATH: usize = 3;

/// What replaces the fields in the middle of a path that's too long.
const CUT: &str = "...";

/// The most fields, at every depth, that a value keeps track of, so that values built from wide
/// structs many times over stay small too.
const MAX_FIELDS: usize = 256;

/// The most times the summaries are recomputed before giving up on a fixed point.
const MAX_ITERATIONS: usize = 32;

/// Where an unchecked value comes from.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Root {
    /// A parameter of the function being analyzed, by index. For `main`, it's the solution.
    Param(usize),
    /// The result of calling a function value or an `extern fn` (`call`), which is reached
    /// through a call in the function being analyzed (`site`), and through a call in that call's
    /// function (`via`), so that the results of the same call reached through different calls are
    /// different values. Calls further in aren't told apart.
    Call {
        site: HirId,
        via: HirId,
        call: HirId,
    },
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

    /// The parameter that this is part of, if it's from a parameter.
    fn parameter(&self) -> Option<usize> {
        match self.root {
            Root::Param(index) => Some(index),
            Root::Call { .. } => None,
        }
    }

    /// Whether this is a field inside of the other origin.
    fn is_inside(&self, other: &Origin) -> bool {
        self.root == other.root
            && self.path.len() > other.path.len()
            && self.path.starts_with(&other.path)
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

    /// The flow of the value at the path inside of this value.
    fn at(&self, path: &[String]) -> Flow {
        // Fields are followed without copying the rest of the value
        let mut flow = self;
        let mut rest = path;

        while let Some((name, after)) = rest.split_first()
            && let Some(field) = flow.fields.as_ref().and_then(|fields| fields.get(name))
        {
            flow = field;
            rest = after;
        }

        rest.iter()
            .fold(flow.clone(), |flow, name| flow.field(name))
    }

    /// The unchecked value that this value is exactly, if any, which is what a check of it checks.
    /// A value that's exact is always exactly one.
    fn exact(&self) -> Option<&Origin> {
        self.direct.first().filter(|_| self.exact)
    }

    /// The flow of a field of this value, or of the item or the rest of it if it's a pair
    /// (`first` and `rest`).
    fn field(mut self, name: &str) -> Flow {
        if let Some(field) = self.fields.as_mut().and_then(|fields| fields.remove(name)) {
            return field;
        }

        if self.exact {
            self.direct = self
                .direct
                .into_iter()
                .map(|origin| origin.with_field(name))
                .collect();
            return self;
        }

        // A field of a value that's one of several, or of a struct with a spread tail, can be any of
        // them
        self.fields = None;
        self
    }

    /// A pair of the two values, whose fields are `first` and `rest`.
    fn pair(first: Flow, rest: Flow) -> Self {
        Self::with_fields(BTreeMap::from([
            ("first".to_string(), first),
            ("rest".to_string(), rest),
        ]))
    }

    /// A value made of the fields. The fields are only kept as deep as paths are (see
    /// [`MAX_PATH`]), and only up to [`MAX_FIELDS`] in all, so that values built from the same value
    /// many times over, as in `let b = (a, a); let c = (b, b);`, stay small. Past that, a value is
    /// the union of its fields, like a value whose fields aren't known.
    fn with_fields(mut fields: BTreeMap<String, Flow>) -> Self {
        let mut flow = Flow::default();
        for field in fields.values_mut() {
            field.limit_depth(MAX_PATH);
            flow.union(field);
        }

        if fields.values().map(Flow::size).sum::<usize>() <= MAX_FIELDS {
            flow.fields = Some(fields);
        }

        flow
    }

    /// The number of fields that are kept track of, at every depth, including this value.
    fn size(&self) -> usize {
        1 + self
            .fields
            .iter()
            .flat_map(BTreeMap::values)
            .map(Flow::size)
            .sum::<usize>()
    }

    /// Forgets the fields more than the depth below this value.
    fn limit_depth(&mut self, depth: usize) {
        let Some(fields) = &mut self.fields else {
            return;
        };

        if depth == 0 {
            self.fields = None;
        } else {
            for field in fields.values_mut() {
                field.limit_depth(depth - 1);
            }
        }
    }

    /// A value that's one of the two values. If they're both structs with the same fields, or one
    /// is and the other is exactly an unchecked value, each field is one of the two fields, so
    /// reading one doesn't read the others.
    fn either(mut first: Flow, mut second: Flow) -> Self {
        // An unchecked value has the fields of the struct it's merged with
        if first.exact
            && let Some(fields) = &second.fields
        {
            first.fields = Some(first.fields_named(fields.keys()));
        } else if second.exact
            && let Some(fields) = &first.fields
        {
            second.fields = Some(second.fields_named(fields.keys()));
        }

        match (first.fields.take(), second.fields.take()) {
            (Some(first_fields), Some(second_fields))
                if first_fields.keys().eq(second_fields.keys()) =>
            {
                // The fields have the same names, in the same order
                let mut flow = Self::with_fields(
                    first_fields
                        .into_iter()
                        .zip(second_fields)
                        .map(|((name, first), (_, second))| (name, Self::either(first, second)))
                        .collect(),
                );

                // A condition's unchecked values are on the struct, not on its fields
                flow.defer_all(first.deferred);
                flow.defer_all(second.deferred);
                flow
            }
            _ => {
                first.union(&second);
                first
            }
        }
    }

    /// The flows of the fields of this value with the names.
    fn fields_named<'n>(&self, names: impl Iterator<Item = &'n String>) -> BTreeMap<String, Flow> {
        names
            .map(|name| (name.clone(), self.clone().field(name)))
            .collect()
    }

    /// Adds unchecked values in deferred conditions, keeping the first site of each.
    fn defer_all(&mut self, deferred: impl IntoIterator<Item = (Origin, HirId)>) {
        for (origin, site) in deferred {
            self.deferred.entry(origin).or_insert(site);
        }
    }

    fn union(&mut self, other: &Flow) {
        self.exact = false;
        self.fields = None;
        self.direct.extend(other.direct.iter().cloned());
        self.defer_all(
            other
                .deferred
                .iter()
                .map(|(origin, &site)| (origin.clone(), site)),
        );
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
    /// The outermost call that the concatenation was reached through, if it's in another
    /// function.
    call: Option<HirId>,
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

/// Where a deferred condition is hashed.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Hashed {
    /// The outermost call it was hashed through.
    call: HirId,
    /// Whether it's hashed in this function, rather than in a function that's called.
    here: bool,
}

/// What a function does with its parameters, in terms of [`Root::Param`], and with the results of
/// the function values it calls. Parameters marked `#[trusted]` are as unchecked as any other here,
/// since this is what the function does when it's called (see [`check`]).
#[derive(Debug, Clone, Default, PartialEq, Eq)]
struct Summary {
    returns: Flow,
    /// The values whose length is checked whenever the function returns.
    checked: BTreeSet<Origin>,
    concats: Vec<Concat>,
    /// Values in deferred conditions that are hashed, with where they were hashed.
    deferred: BTreeMap<Deferred, Hashed>,
    /// The values that are hashed as a whole, so the deferred conditions inside of them are too.
    hashed: BTreeSet<Origin>,
}

pub(crate) fn check(cx: &mut LintContext<'_>, input: &LintInput<'_>) {
    let mut summaries = HashMap::new();

    for main in entrypoints(cx, input) {
        summarize(cx, input, main, &mut summaries);

        // The summary of `main` is also what it does when it's called, by itself or by another
        // file's `main`, where its parameters aren't trusted, so if any are marked `#[trusted]`,
        // it's analyzed again with them trusted for what's reported
        let Symbol::Function(function) = cx.db().symbol(main) else {
            unreachable!("`main` is a function");
        };

        if function
            .parameters
            .iter()
            .any(|&parameter| cx.db().is_trusted_parameter(parameter))
        {
            let summary = Analysis::new(cx, input, &summaries, main, true).run();
            report(cx, input, main, &summary);
        } else {
            report(cx, input, main, &summaries[&main]);
        }
    }
}

/// The entrypoints of the files being compiled, in a fixed order: the functions compiled as
/// programs of their own (each file's `main`, and the main file's exports), whose parameters are
/// the solution. The exports of other files are library functions that files import from each
/// other, which are followed from where they're called instead. An `extern fn` has no body.
fn entrypoints(cx: &LintContext<'_>, input: &LintInput<'_>) -> Vec<SymbolId> {
    let mut entrypoints: Vec<SymbolId> = input
        .programs
        .iter()
        .copied()
        .filter(|&symbol| {
            matches!(
                cx.db().symbol(symbol),
                Symbol::Function(function) if function.kind != FunctionKind::External
            )
        })
        .collect();

    entrypoints.sort();
    entrypoints
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

            let mut analysis = Analysis::new(cx, input, summaries, function, false);
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

fn report(cx: &mut LintContext<'_>, input: &LintInput<'_>, main: SymbolId, summary: &Summary) {
    // Each concatenation is reported once where it's reached from `main`, naming every unchecked
    // value in it
    let mut concats: BTreeMap<(HirId, Option<HirId>), BTreeSet<&Origin>> = BTreeMap::new();

    for concat in &summary.concats {
        concats
            .entry((concat.site, concat.call))
            .or_default()
            .extend(concat.operands.iter().flatten());
    }

    for ((site, call), origins) in concats {
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

        let suggestions = trusted_suggestions(
            cx,
            input,
            main,
            origins.iter().filter_map(|o| o.parameter()),
        );

        report_at(
            cx,
            suggestions,
            site,
            call,
            &message,
            "The concatenation is here",
            "Declare it with a type that doesn't have a fixed length, such as `Bytes`, and check it with `assert value is Bytes32`; or compute a coin id with `coinid`, which checks its arguments",
        );
    }

    let mut deferred: BTreeSet<(HirId, HirId, String, Option<usize>)> = BTreeSet::new();

    for (value, &Hashed { call, .. }) in &summary.deferred {
        let (path, part) = origin_name(cx, main, &value.origin);
        deferred.insert((
            value.site,
            call,
            part.describe(&path, 1),
            value.origin.parameter(),
        ));
    }

    for (site, call, name, parameter) in deferred {
        let message = format!(
            "{name} is from the solution, but nothing checks its length, and it's put in a condition that's hashed rather than output, so the condition is only checked when a later spend outputs it, which then fails"
        );

        let suggestions = trusted_suggestions(cx, input, main, parameter);

        report_at(
            cx,
            suggestions,
            site,
            Some(call),
            &message,
            "The condition's field is here",
            "Declare it with a type that doesn't have a fixed length, such as `Bytes`, and check it with `assert value is Bytes32`",
        );
    }
}

/// Suggestions to mark the parameters of `main` as trusted, for values that whoever creates the
/// coin or runs the puzzle checks, such as curried values or values that an outer puzzle computes.
/// Whether that's true is up to the author, so they're never applied automatically.
fn trusted_suggestions(
    cx: &LintContext<'_>,
    input: &LintInput<'_>,
    main: SymbolId,
    parameters: impl IntoIterator<Item = usize>,
) -> Vec<Suggestion> {
    parameters
        .into_iter()
        .collect::<BTreeSet<_>>()
        .into_iter()
        .filter_map(|index| {
            let start = parameter_start(cx, input, main, index)?;
            let name = parameter_name(cx, main, index);
            Some(Suggestion::replace(
                format!(
                    "If `{name}` is curried, or supplied by an outer puzzle that checks it, mark it as trusted"
                ),
                start,
                format!("#[{TRUSTED_ATTRIBUTE}] "),
                Applicability::MaybeIncorrect,
            ))
        })
        .collect()
}

/// Reports the diagnostic at the outermost call in `main` that reaches the site, or at the site
/// itself if it's in `main`, so that it can be allowed there.
fn report_at(
    cx: &mut LintContext<'_>,
    suggestions: Vec<Suggestion>,
    site: HirId,
    call: Option<HirId>,
    message: &str,
    site_note: &str,
    help: &str,
) {
    let Some(site) = cx.db().hir_srcloc(site).cloned() else {
        return;
    };

    let srcloc = call
        .and_then(|call| cx.db().hir_srcloc(call).cloned())
        .unwrap_or_else(|| site.clone());

    let message = capitalize(message);

    if let Some(diagnostic) = cx.report(Lint::UncheckedInput, srcloc, message) {
        if call.is_some() {
            diagnostic.notes.push(Note::at(site, site_note));
        }
        diagnostic.notes.push(Note::new(help));
        diagnostic.suggestions.extend(suggestions);
    }
}

/// How a parameter of `main` is written in a diagnostic: its name, or its pattern if it's
/// destructured, as the function's type records it.
fn parameter_name(cx: &LintContext<'_>, main: SymbolId, index: usize) -> String {
    let Symbol::Function(function) = cx.db().symbol(main) else {
        unreachable!("`main` is a function");
    };

    let Type::Function(ty) = cx.db().ty(function.ty) else {
        return format!("parameter {}", index + 1);
    };

    ty.params
        .get(index)
        .map(|parameter| rue_hir::one_line(&parameter.name))
        .filter(|name| !name.is_empty())
        .unwrap_or_else(|| format!("parameter {}", index + 1))
}

/// Where to insert an attribute before a parameter of `main`, which is found in the syntax tree
/// from where `main` is named.
fn parameter_start(
    cx: &LintContext<'_>,
    input: &LintInput<'_>,
    main: SymbolId,
    index: usize,
) -> Option<SrcLoc> {
    let Symbol::Function(function) = cx.db().symbol(main) else {
        return None;
    };

    let name = function.name.as_ref()?.srcloc()?;

    let &(source, document) = input
        .documents
        .iter()
        .find(|(source, _)| source.kind == name.source.kind)?;

    let parameter = document
        .syntax()
        .token_at_offset(TextSize::try_from(name.span.start).ok()?)
        .right_biased()?
        .parent_ancestors()
        .find_map(AstFunctionItem::cast)?
        .parameters()
        .nth(index)?;

    let start = parameter.syntax().text_range().start();
    Some(srcloc(source, TextRange::empty(start)))
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
        Root::Param(index) => parameter_name(cx, main, index),
        // The result of a call in another function is named after the call in `main` that it's
        // reached through, since the same call can be reached more than once
        Root::Call { site, .. } => cx.db().hir_srcloc(site).map_or_else(
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
            (Self::Whole, 1) => format!("`{path}`"),
            // Results of calls in other functions, reached through the same call in `main`
            (Self::Whole, _) => format!("results of `{path}`"),
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
    /// Whether the parameters marked `#[trusted]` are trusted, which is only the case for the
    /// `main` being reported, not where it's called.
    trusts_parameters: bool,
    body: HirId,
    summary: Summary,
    /// The functions that are called, whose summaries are needed.
    callees: Vec<SymbolId>,
    /// Whether the function has a `return` statement, so its value is one of several.
    returns: bool,
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
        trusts_parameters: bool,
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
            trusts_parameters,
            body,
            summary: Summary::default(),
            callees: Vec::new(),
            returns: false,
            bindings: HashMap::new(),
            branch_checks: Vec::new(),
            flows: HashMap::new(),
        }
    }

    fn run(&mut self) -> Summary {
        // The value of a function without a `return` statement is its body, which may be exactly an
        // unchecked value, or a struct of them
        let body = self.flow(self.body, true);
        if self.returns {
            self.summary.returns.union(&body);
        } else {
            self.summary.returns = body;
        }

        // A check counts wherever it is in the function, even after the use, since if it fails,
        // the whole spend does
        let checked = &self.summary.checked;

        self.summary
            .concats
            .retain_mut(|concat| concat.retain_unchecked(|origin| origin.checked_by(checked)));

        // The same concatenation reached through different calls (or around a recursive call
        // again) is kept once, where it's written in this function if it is, or else with the
        // first call it was reached through
        self.summary
            .concats
            .sort_by_key(|concat| concat.call.is_some());
        let mut seen = HashSet::new();
        self.summary
            .concats
            .retain(|concat| seen.insert((concat.site, concat.operands.clone())));

        self.summary
            .deferred
            .retain(|deferred, _| !deferred.origin.checked_by(checked));

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
            Hir::Cast(inner) => {
                let mut flow = self.flow(inner, always);

                // Fields are named by the type they're read through, so a cast to a struct or pair
                // with another layout loses track of them
                if self.changes_layout(inner, hir) {
                    flow.exact = false;
                    flow.fields = None;
                }

                flow
            }
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
                let mut fields = BTreeMap::new();
                let mut deferred = BTreeMap::new();

                for field in &initializer.fields {
                    let value = self.flow(field.value, always);

                    if let Some(ty) = condition
                        && !field.is_default
                        && self.field_has_length(ty, &field.name)
                    {
                        for origin in &value.direct {
                            deferred.entry(origin.clone()).or_insert(field.value);
                        }
                    }

                    fields.insert(field.name.clone(), value);
                }

                let mut flow = Flow::with_fields(fields);

                // The rest of a struct that spreads another one is that value as it is, so the
                // fields aren't known
                if let Some(tail) = initializer.tail_value() {
                    let tail = self.flow(tail, always);
                    flow.union(&tail);
                }

                flow.defer_all(deferred);
                flow
            }
            Hir::Block(block) => self.block(&block.statements, block.body, always),
            Hir::If(condition, then, else_, _) => {
                self.flow(condition, always);
                let then = self.branch(condition, then);
                let else_ = self.flow(else_, false);
                Flow::either(then, else_)
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
                // Each item of a list that's written out is checked, or a list passed as a whole
                if let Hir::List(list) = self.cx.db().hir(points).clone() {
                    self.checked_operands(&list.items, always);
                } else {
                    self.checked_operands(&[points], always);
                }
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
            | Hir::Program(_)
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
            if always {
                self.summary.checked.extend(flow.exact().cloned());
            }
        }
    }

    fn reference(&mut self, symbol: SymbolId, always: bool) -> Flow {
        // A parameter that's narrowed is still unchecked: the guard that narrowed it may only
        // have ruled out other members of a union (`is nil`), so only the guards themselves count
        // as checks (see `condition_checks`)
        if let Some(index) = self.parameters.iter().position(|&param| param == symbol) {
            // The value of a parameter of `main` marked `#[trusted]` is checked by whoever
            // provides it
            if self.trusts_parameters && self.cx.db().is_trusted_parameter(symbol) {
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
        for (value, hashed) in region_deferred {
            if !value.origin.checked_by(&region_checked) {
                self.record_deferred(value, hashed);
            }
        }

        // The items of a value that's empty whenever the region isn't evaluated are checked where
        // they exist
        self.summary.checked.extend(
            region_checked
                .into_iter()
                .filter(|origin| empty.iter().any(|empty| origin.is_inside(empty))),
        );

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
                self.summary.returns.union(&flow);
                self.returns = true;
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
            Hir::Guard(guard) if self.cx.known_atom_length(guard.ty) == Some(0) => self
                .flow(guard.value, false)
                .exact()
                .cloned()
                .into_iter()
                .collect(),
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
                    self.flow(guard.value, false)
                        .exact()
                        .cloned()
                        .into_iter()
                        .collect()
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
                        checks.extend(self.flow(value, false).exact().cloned());
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
                call: None,
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
                root: Root::Call {
                    site: hir,
                    via: hir,
                    call: hir,
                },
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
                Root::Call { .. } => Vec::new(),
            }
        };

        // An origin of the callee, in terms of the caller. The result of a call in the callee is
        // reached through this call.
        let translate = |origin: &Origin| -> Flow {
            match origin.root {
                // A single argument keeps its exactness and fields
                Root::Param(_) => arguments_for(origin.root)
                    .into_iter()
                    .map(|arg| arg.at(&origin.path))
                    .reduce(|mut flow, arg| {
                        flow.union(&arg);
                        flow
                    })
                    .unwrap_or_default(),
                Root::Call { site, call, .. } => Flow::origin(Origin {
                    root: Root::Call {
                        site: hir,
                        via: site,
                        call,
                    },
                    path: origin.path.clone(),
                }),
            }
        };

        let mut returns = translate_flow(&summary.returns, &translate);

        // `unchecked_cast` returns its argument as it is, but read through another type, which can
        // have another layout, like a cast
        if function
            .name
            .as_ref()
            .is_some_and(|name| name.text() == "unchecked_cast" && name.srcloc().is_none())
            && let [arg] = call.args.as_slice()
            && self.changes_layout(*arg, hir)
        {
            returns.exact = false;
            returns.fields = None;
        }

        // A check of a parameter checks the argument, if it's exactly one unchecked value
        if always {
            for origin in &summary.checked {
                self.summary
                    .checked
                    .extend(translate(origin).exact().cloned());
            }
        }

        for concat in &summary.concats {
            let mut concat = Concat {
                site: concat.site,
                unknown: concat.unknown,
                operands: concat
                    .operands
                    .iter()
                    .map(|operand| {
                        operand
                            .iter()
                            .flat_map(|origin| translate(origin).direct)
                            .collect()
                    })
                    .collect(),
                call: Some(hir),
            };

            if concat.retain_unchecked(|origin| self.checked_in_branch(origin)) {
                self.summary.concats.push(concat);
            }
        }

        for deferred in summary.deferred.keys() {
            for origin in translate(&deferred.origin).direct {
                self.defer(origin, deferred.site, hir, false);
            }
        }

        // The part of each argument that the callee hashes is hashed
        for origin in &summary.hashed {
            for arg in arguments_for(origin.root) {
                self.hash(hir, &arg.at(&origin.path));
            }
        }

        returns
    }

    /// Records that the value is hashed, so the deferred conditions in it are too.
    fn hash(&mut self, call: HirId, value: &Flow) {
        self.summary.hashed.extend(value.direct.iter().cloned());

        for (origin, &site) in &value.deferred {
            self.defer(origin.clone(), site, call, true);
        }
    }

    /// Records an unchecked value in a deferred condition that's hashed, reached through the
    /// call, unless the branch being analyzed checks it. A condition that this function hashes
    /// (`here`) is reported where it does, rather than where a call reaches it again, such as a
    /// recursive call.
    fn defer(&mut self, origin: Origin, site: HirId, call: HirId, here: bool) {
        if self.checked_in_branch(&origin) {
            return;
        }

        self.record_deferred(Deferred { origin, site }, Hashed { call, here });
    }

    /// Records a deferred value, keeping where it's first hashed in this function if it is, and
    /// otherwise the first call it's reached through.
    fn record_deferred(&mut self, deferred: Deferred, hashed: Hashed) {
        let entry = self.summary.deferred.entry(deferred).or_insert(hashed);

        if hashed.here && !entry.here {
            *entry = hashed;
        }
    }

    /// Whether a cast from the value to the cast's type reads its fields through another struct or
    /// pair, whose fields can have the same names in other places. A cast to an alias, or to another
    /// instance of the same generic struct, has the same layout, so it keeps the fields.
    fn changes_layout(&mut self, value: HirId, cast: HirId) -> bool {
        let (Some(from), Some(to)) = (self.cx.db().hir_type(value), self.cx.db().hir_type(cast))
        else {
            return false;
        };

        (self.cx.has_pairs(from) || self.cx.has_pairs(to))
            && self.cx.field_layout(from) != self.cx.field_layout(to)
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

/// A flow of a callee, in terms of the caller, given how each of the callee's origins translates.
/// A value that's exactly one origin is exactly what that origin translates to, and the fields of a
/// struct are translated one by one.
fn translate_flow(flow: &Flow, translate: &impl Fn(&Origin) -> Flow) -> Flow {
    let mut result = if let Some(origin) = flow.exact() {
        translate(origin)
    } else if let Some(fields) = &flow.fields {
        Flow::with_fields(
            fields
                .iter()
                .map(|(name, field)| (name.clone(), translate_flow(field, translate)))
                .collect(),
        )
    } else {
        let mut result = Flow::default();
        for origin in &flow.direct {
            result.union(&translate(origin));
        }
        result
    };

    for (origin, &site) in &flow.deferred {
        result.defer_all(
            translate(origin)
                .direct
                .into_iter()
                .map(|origin| (origin, site)),
        );
    }

    result
}
