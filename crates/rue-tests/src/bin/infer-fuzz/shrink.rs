//! Shrinks a failing program to a small reproduction, and describes the shape of the calls whose
//! generic arguments were removed.
//!
//! Shrinking is a greedy delta reduction over the program's structure: it keeps the generic
//! arguments of sites that aren't needed for the failure, removes bindings, functions and
//! recursion guards, and replaces expressions with simpler ones of the same type, as long as the
//! explicit program still compiles and the variant still fails in the same way.

use std::cell::Cell;
use std::collections::BTreeSet;

use crate::generate::legalize;
use crate::program::{Expr, ExprKind, Function, Named, Program, Root, Strip, Ty, Variant};

/// Shrinks the program and variant, keeping each change for which `fails` is still true. Gives
/// up after `budget` calls of `fails`.
pub fn shrink(
    mut program: Program,
    mut variant: Variant,
    budget: usize,
    fails: &mut impl FnMut(&Program, &Variant) -> bool,
) -> (Program, Variant) {
    let calls = Cell::new(0);
    let mut check = |program: &Program, variant: &Variant| {
        calls.set(calls.get() + 1);
        calls.get() <= budget && still_removable(program, variant) && fails(program, variant)
    };

    loop {
        let mut changed = false;

        // Keep the generic arguments of each site that isn't needed for the failure
        for site in variant.stripped.keys().copied().collect::<Vec<_>>() {
            let mut candidate = variant.clone();
            candidate.stripped.remove(&site);
            if !candidate.stripped.is_empty() && check(&program, &candidate) {
                variant = candidate;
                changed = true;
            }
        }

        // Return nothing from `main`, or a single binding
        let mut returns = Vec::new();
        if !matches!(program.main.body.kind, ExprKind::Nil) {
            returns.push(ExprKind::Nil);
        }
        if !matches!(program.main.body.kind, ExprKind::Nil | ExprKind::Var(_)) {
            returns.extend(
                program
                    .main
                    .lets
                    .iter()
                    .map(|binding| ExprKind::Var(binding.name.clone())),
            );
        }
        for kind in returns {
            let mut candidate = program.clone();
            candidate.main.body = Expr::new(kind, Ty::Any);
            if check(&candidate, &variant) {
                program = candidate;
                changed = true;
                break;
            }
        }

        // Remove each binding, recursion guard and function
        for index in (0..program.main.lets.len()).rev() {
            let mut candidate = program.clone();
            candidate.main.lets.remove(index);
            if check(&candidate, &variant) {
                program = candidate;
                changed = true;
            }
        }

        for index in (0..program.functions.len()).rev() {
            let mut candidate = program.clone();
            candidate.functions.remove(index);
            if check(&candidate, &variant) {
                program = candidate;
                changed = true;
                continue;
            }

            if program.functions[index].recursion.is_some() {
                let mut candidate = program.clone();
                let recursion = candidate.functions[index].recursion.as_mut().unwrap();
                if recursion.inner.take().is_some() && check(&candidate, &variant) {
                    program = candidate;
                    changed = true;
                }

                let mut candidate = program.clone();
                candidate.functions[index].recursion = None;
                if check(&candidate, &variant) {
                    program = candidate;
                    changed = true;
                }
            }
        }

        // Replace each expression with a simpler one of the same type
        let mut index = 0;
        while index < count_exprs(&program) {
            let expr = nth(&program, index).unwrap();
            let mut replaced = false;
            if !is_simple(expr) {
                for replacement in [
                    simple(&expr.ty),
                    Expr::new(ExprKind::Unchecked, expr.ty.clone()),
                ] {
                    let mut candidate = program.clone();
                    *nth_mut(&mut candidate, index).unwrap() = replacement;
                    if check(&candidate, &variant) {
                        program = candidate;
                        changed = true;
                        replaced = true;
                        break;
                    }
                }
            }
            if !replaced {
                index += 1;
            }
        }

        if !changed || calls.get() > budget {
            break;
        }
    }

    // Sites that were removed along with their expressions no longer matter
    let sites: BTreeSet<_> = program.sites().into_iter().collect();
    variant.stripped.retain(|site, _| sites.contains(site));

    (program, variant)
}

/// Whether every generic argument the variant removes still has something to be inferred from.
fn still_removable(program: &Program, variant: &Variant) -> bool {
    legalize(program, variant) == *variant
}

/// Whether the expression is as simple as what [`simple`] would replace it with. Pairs and typed
/// lambdas count when their parts are simple, since `simple` builds those for pair and function
/// types, and replacing one with an identical copy would never end.
fn is_simple(expr: &Expr) -> bool {
    match &expr.kind {
        ExprKind::Var(_)
        | ExprKind::Decrement(_)
        | ExprKind::Int(_)
        | ExprKind::Bytes
        | ExprKind::Bool
        | ExprKind::Nil
        | ExprKind::Unchecked => true,
        ExprKind::Pair(first, rest) => is_simple(first) && is_simple(rest),
        ExprKind::Lambda {
            typed: true, body, ..
        } => is_simple(body),
        _ => false,
    }
}

/// The simplest expression of a type that doesn't take generic arguments.
fn simple(ty: &Ty) -> Expr {
    let kind = match ty {
        Ty::Int => ExprKind::Int(1),
        Ty::Lit(value) => ExprKind::Int(*value),
        Ty::Bytes => ExprKind::Bytes,
        Ty::Bool => ExprKind::Bool,
        Ty::Nil | Ty::Opt(_) | Ty::List(_) => ExprKind::Nil,
        Ty::Pair(first, rest) => ExprKind::Pair(Box::new(simple(first)), Box::new(simple(rest))),
        Ty::Fn(params, ret) => ExprKind::Lambda {
            params: params
                .iter()
                .enumerate()
                .map(|(index, param)| (format!("s{index}"), param.clone()))
                .collect(),
            typed: true,
            body: Box::new(simple(ret)),
        },
        // The simplest member, as a value of the whole union
        Ty::Union(members) => {
            return Expr {
                ty: ty.clone(),
                ..simple(&members[0])
            };
        }
        Ty::Named(Named::Maybe | Named::Tree, _) => ExprKind::Nil,
        Ty::Boxed(_)
        | Ty::Named(Named::Entry, _)
        | Ty::GenFn(..)
        | Ty::Rigid(_)
        | Ty::Var(_)
        | Ty::Any => ExprKind::Unchecked,
    };
    Expr::new(kind, ty.clone())
}

fn count_exprs(program: &Program) -> usize {
    program.exprs().len()
}

fn nth(program: &Program, index: usize) -> Option<&Expr> {
    program.exprs().get(index).copied()
}

/// The expression at the index in the order of [`Program::exprs`].
fn nth_mut(program: &mut Program, mut index: usize) -> Option<&mut Expr> {
    fn find<'a>(expr: &'a mut Expr, index: &mut usize) -> Option<&'a mut Expr> {
        if *index == 0 {
            return Some(expr);
        }
        *index -= 1;
        for child in expr.children_mut() {
            if let Some(found) = find(child, index) {
                return Some(found);
            }
        }
        None
    }

    let Program { functions, main } = program;
    for function in functions.iter_mut().chain([main]) {
        for expr in function.exprs_mut() {
            if let Some(found) = find(expr, &mut index) {
                return Some(found);
            }
        }
    }
    None
}

/// Describes each call whose generic arguments the variant removes, such as
/// `map(ref:duplicate!, lit) as let: lit`, sorted and joined with `; `. A `!` marks an
/// argument whose own generic arguments are removed.
pub fn shape(program: &Program, variant: &Variant) -> String {
    let mut shapes = BTreeSet::new();
    for function in program.functions_and_main() {
        let recursive = function.recursion.is_some();
        for (expr, root) in function.roots() {
            let context = match root {
                Root::GuardLet => "unannotated let in recursion guard".to_string(),
                Root::GuardReturn => "return in recursion guard".to_string(),
                Root::Let(Some(ty)) => format!("let: {}", ty_kind(ty)),
                Root::Let(None) => "unannotated let".to_string(),
                Root::Return => "return".to_string(),
            };
            describe_sites(
                program,
                expr,
                &context,
                function,
                recursive,
                variant,
                &mut shapes,
            );
        }
    }
    shapes.into_iter().collect::<Vec<_>>().join("; ")
}

fn describe_sites(
    program: &Program,
    expr: &Expr,
    context: &str,
    function: &Function,
    recursive: bool,
    variant: &Variant,
    shapes: &mut BTreeSet<String>,
) {
    if let Some(site) = expr.site()
        && let Some(strip) = variant.stripped.get(&site)
    {
        let mut shape = describe(program, expr, variant);
        if let Strip::Placeholders(_) = strip {
            shape.push_str(" with placeholders");
        }
        if let ExprKind::Call {
            function: callee, ..
        } = &expr.kind
            && *callee == function.name
        {
            shape.push_str(" (recursive)");
        } else if recursive {
            shape.push_str(" (in a recursive function)");
        }
        shapes.insert(format!("{shape} as {context}"));
    }

    let child_context = match &expr.kind {
        ExprKind::Call { function, .. } => format!("arg of {}", callee_name(program, function)),
        ExprKind::CallLocal { .. } => "arg of a local function".to_string(),
        ExprKind::Lambda { .. } => "lambda body".to_string(),
        ExprKind::BoxLit { .. } => "field".to_string(),
        ExprKind::First(_) | ExprKind::Rest(_) | ExprKind::Field(_) => "projected".to_string(),
        _ => context.to_string(),
    };

    for child in expr.children() {
        describe_sites(
            program,
            child,
            &child_context,
            function,
            recursive,
            variant,
            shapes,
        );
    }
}

/// A catalog function's name, or `g(<signature>)` for a random function, whose name means
/// nothing across programs.
fn callee_name(program: &Program, name: &str) -> String {
    match program.function(name) {
        Some(function)
            if name.starts_with('g') && name[1..].chars().all(|c| c.is_ascii_digit()) =>
        {
            let ty = function.ty().print(&function.generics);
            format!("g<{}>{}", function.generics.join(", "), &ty[2..])
        }
        _ => name.to_string(),
    }
}

fn describe(program: &Program, expr: &Expr, variant: &Variant) -> String {
    let mark = |expr: &Expr| if variant.strips(expr) { "!" } else { "" };

    match &expr.kind {
        ExprKind::Var(_) | ExprKind::Decrement(_) => "var".to_string(),
        ExprKind::Int(_) => "lit".to_string(),
        ExprKind::Bytes => "bytes".to_string(),
        ExprKind::Bool => "bool".to_string(),
        ExprKind::Nil => "nil".to_string(),
        ExprKind::Pair(..) => "pair".to_string(),
        ExprKind::List(_) => "list".to_string(),
        ExprKind::First(_) | ExprKind::Rest(_) | ExprKind::Field(_) => "projection".to_string(),
        ExprKind::BoxLit { value, .. } => {
            format!("Box {{ {} }}", describe(program, value, variant))
        }
        ExprKind::Call { function, args, .. } => format!(
            "{}({})",
            callee_name(program, function),
            args.iter()
                .map(|arg| {
                    let text = describe_arg(program, arg, variant);
                    format!("{text}{}", mark(arg))
                })
                .collect::<Vec<_>>()
                .join(", ")
        ),
        ExprKind::CallLocal { .. } => "local call".to_string(),
        ExprKind::FnRef { function, .. } => format!("ref:{}", callee_name(program, function)),
        ExprKind::Lambda { typed: true, .. } => "typed lambda".to_string(),
        ExprKind::Lambda { typed: false, .. } => "lambda".to_string(),
        ExprKind::IdentityLambda => "generic lambda".to_string(),
        ExprKind::Unchecked => "cast".to_string(),
        ExprKind::EntryLit { key, value, .. } => format!(
            "Entry {{ {}, {} }}",
            describe(program, key, variant),
            describe(program, value, variant)
        ),
        ExprKind::Key(_) => "projection".to_string(),
        ExprKind::Apply(..) => "local call".to_string(),
        ExprKind::Wrapped { call, .. } => {
            format!(
                "{} for a generic function type",
                describe(program, call, variant)
            )
        }
    }
}

/// An argument, with nested calls named but not expanded.
fn describe_arg(program: &Program, expr: &Expr, variant: &Variant) -> String {
    match &expr.kind {
        ExprKind::Call { function, .. } => format!("call:{}", callee_name(program, function)),
        _ => describe(program, expr, variant),
    }
}

fn ty_kind(ty: &Ty) -> &'static str {
    if ty.contains_fn() {
        "function"
    } else if ty.contains_lit() {
        "literal"
    } else if ty.contains_union() {
        "union"
    } else {
        "plain"
    }
}

/// Features of the calls whose generic arguments the variant removes, which point at the
/// inference rules a failure involves, such as `recursive call` (#334) or `untyped lambda for a
/// bare generic`. Shrunk failures with the same diagnostic are grouped by these.
pub fn tags(program: &Program, variant: &Variant) -> Vec<String> {
    let mut tags = BTreeSet::new();
    let stripped = |expr: &Expr| variant.strips(expr);

    for function in program.functions_and_main() {
        for (expr, root) in function.roots() {
            if let Root::Let(Some(ty)) = root
                && stripped(expr)
            {
                if ty.contains_lit() {
                    tags.insert("expected literal");
                } else if ty.contains_union() {
                    tags.insert("expected union");
                }
            }

            expr.walk(&mut |expr| {
                if !stripped(expr) {
                    return;
                }
                if let ExprKind::Call {
                    function: callee,
                    args,
                    ..
                } = &expr.kind
                {
                    if *callee == function.name {
                        tags.insert("recursive call");
                    }
                    let Some(callee) = program.function(callee) else {
                        return;
                    };
                    if has_cycle(callee) {
                        tags.insert("cycle of generics");
                    }
                    for ((_, param), arg) in callee.params.iter().zip(args) {
                        call_arg_tags(program, param, arg, &stripped, false, &mut tags);
                    }
                }
                if variant.stripped.get(&expr.site().unwrap()) != Some(&Strip::All) {
                    tags.insert("placeholders");
                }
            });
        }
    }

    tags.into_iter().map(ToString::to_string).collect()
}

fn call_arg_tags(
    program: &Program,
    param: &Ty,
    arg: &Expr,
    stripped: &impl Fn(&Expr) -> bool,
    nested: bool,
    tags: &mut BTreeSet<&'static str>,
) {
    match (&arg.kind, param) {
        (ExprKind::Lambda { typed: false, .. }, Ty::Var(_)) => {
            tags.insert("untyped lambda for a bare generic");
        }
        (ExprKind::Lambda { typed: false, .. }, _) => {
            tags.insert(if nested {
                "untyped lambda in a compound argument"
            } else {
                "untyped lambda"
            });
        }
        (ExprKind::Lambda { typed: true, .. }, _) => {
            tags.insert("typed lambda");
        }
        (ExprKind::IdentityLambda, _) => {
            tags.insert("generic lambda");
        }
        (ExprKind::Pair(first, rest), Ty::Pair(first_param, rest_param)) => {
            call_arg_tags(program, first_param, first, stripped, true, tags);
            call_arg_tags(program, rest_param, rest, stripped, true, tags);
        }
        (ExprKind::Call { .. }, _) if stripped(arg) => {
            tags.insert("nested call");
        }
        (ExprKind::FnRef { function, .. }, _) => {
            let Some(function) = program.function(function) else {
                return;
            };
            if function.generics.is_empty() {
                if function.params.iter().any(|(_, ty)| ty.contains_lit()) {
                    tags.insert("function with a literal parameter");
                }
                return;
            }
            if stripped(arg) {
                tags.insert("generic function argument");
            }
            let in_params: BTreeSet<usize> = function
                .params
                .iter()
                .flat_map(|(_, ty)| ty.vars())
                .collect();
            if (0..function.generics.len()).any(|generic| !in_params.contains(&generic)) {
                tags.insert("generic function with a generic only in its return type");
            }
            if function
                .params
                .iter()
                .any(|(_, ty)| !matches!(ty, Ty::Var(_)) && !ty.vars().is_empty())
            {
                tags.insert("generic function with a compound parameter");
            }
        }
        _ => {}
    }
}

/// Whether the function's parameters relate its generics in a cycle, as in
/// `cycle<A, B>(forward: fn(a: A) -> B, backward: fn(b: B) -> A)`.
fn has_cycle(function: &Function) -> bool {
    let count = function.generics.len();
    let mut edges = vec![BTreeSet::new(); count];
    for (_, param) in &function.params {
        if let Ty::Fn(params, ret) = param {
            for from in params.iter().flat_map(Ty::vars) {
                edges[from].extend(ret.vars());
            }
        }
    }
    (0..count).any(|start| {
        let mut seen = BTreeSet::new();
        let mut stack: Vec<usize> = edges[start].iter().copied().collect();
        while let Some(next) = stack.pop() {
            if next == start {
                return true;
            }
            if seen.insert(next) {
                stack.extend(edges[next].iter().copied());
            }
        }
        false
    })
}
