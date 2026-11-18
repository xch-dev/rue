//! Writes random programs from a seed.
//!
//! A program is a few functions taken from a catalog of the shapes that the inference tests use
//! (`map`, `chain`, `cycle`, `duplicate`, `first_of`, ...), a few random generic functions, and a
//! `main` that binds the results of some calls and returns them. Every expression is built for a
//! type that's known up front, so every generic argument can be written out. Bodies are built the
//! same way, so generic functions also call each other, and some call themselves.

use std::collections::{BTreeMap, BTreeSet};

use crate::program::{
    Expr, ExprKind, Function, Let, Named, Program, Recursion, SiteId, Strip, Ty, Variant,
};

/// A small deterministic PRNG (`SplitMix64`), as in `rue-types/tests/memo.rs`.
#[derive(Debug, Clone)]
pub struct Rng(u64);

impl Rng {
    pub fn new(seed: u64) -> Self {
        Self(seed)
    }

    fn next(&mut self) -> u64 {
        self.0 = self.0.wrapping_add(0x9e37_79b9_7f4a_7c15);
        let mut z = self.0;
        z = (z ^ (z >> 30)).wrapping_mul(0xbf58_476d_1ce4_e5b9);
        z = (z ^ (z >> 27)).wrapping_mul(0x94d0_49bb_1331_11eb);
        z ^ (z >> 31)
    }

    pub fn below(&mut self, n: usize) -> usize {
        (self.next() % n as u64) as usize
    }

    pub fn chance(&mut self, percent: usize) -> bool {
        self.below(100) < percent
    }

    pub fn range(&mut self, low: usize, high: usize) -> usize {
        low + self.below(high - low + 1)
    }

    fn int(&mut self, low: usize, high: usize) -> i64 {
        i64::try_from(self.range(low, high)).unwrap()
    }

    /// Picks an index with probability proportional to its weight.
    fn weighted(&mut self, weights: &[usize]) -> usize {
        let mut pick = self.below(weights.iter().sum());
        for (index, &weight) in weights.iter().enumerate() {
            if pick < weight {
                return index;
            }
            pick -= weight;
        }
        unreachable!()
    }
}

/// A function in the catalog: its name, generics, parameters and return type, with types written
/// as Rue source.
struct Template {
    name: &'static str,
    generics: &'static [&'static str],
    params: &'static [(&'static str, &'static str)],
    ret: &'static str,
}

const fn template(
    name: &'static str,
    generics: &'static [&'static str],
    params: &'static [(&'static str, &'static str)],
    ret: &'static str,
) -> Template {
    Template {
        name,
        generics,
        params,
        ret,
    }
}

/// Generic functions whose shapes the inference tests use (#146, #265, #283, #284, #285).
const CATALOG: &[Template] = &[
    template("identity", &["T"], &[("value", "T")], "T"),
    template("duplicate", &["T"], &[("value", "T")], "(T, T)"),
    template("first_of", &["T"], &[("pair", "(T, T)")], "T"),
    template("optional", &["T"], &[("value", "T")], "T | nil"),
    template("boxed", &["T"], &[("value", "T")], "Box<T>"),
    template("unbox", &["T"], &[("value", "Box<T>")], "T"),
    template("singleton", &["T"], &[("value", "T")], "List<T>"),
    template(
        "head_or",
        &["T"],
        &[("list", "List<T>"), ("default", "T")],
        "T",
    ),
    template("either", &["T"], &[("first", "T"), ("second", "T")], "T"),
    template(
        "or_else",
        &["T"],
        &[("value", "T | nil"), ("default", "T")],
        "T",
    ),
    template("pairs", &["T"], &[], "(T, T)"),
    template("swap", &["A", "B"], &[("pair", "(A, B)")], "(B, A)"),
    template(
        "map",
        &["A", "B"],
        &[("function", "fn(a: A) -> B"), ("value", "A")],
        "B",
    ),
    template(
        "map_reversed",
        &["A", "B"],
        &[("value", "A"), ("function", "fn(a: A) -> B")],
        "B",
    ),
    template(
        "chain",
        &["A", "B", "C"],
        &[
            ("first", "fn(a: A) -> B"),
            ("second", "fn(b: B) -> C"),
            ("value", "A"),
        ],
        "C",
    ),
    template(
        "cycle",
        &["A", "B"],
        &[
            ("forward", "fn(a: A) -> B"),
            ("backward", "fn(b: B) -> A"),
            ("value", "A"),
        ],
        "B",
    ),
    template(
        "run",
        &["A", "B"],
        &[
            ("first", "fn(a: A) -> B"),
            ("second", "fn(b: B) -> Int"),
            ("value", "A"),
        ],
        "Int",
    ),
    template(
        "feed",
        &["A", "B"],
        &[("first", "fn(a: A) -> B"), ("second", "fn(b: B) -> Int")],
        "fn(a: A) -> Int",
    ),
    template(
        "compose",
        &["A", "B"],
        &[("function", "fn(a: A) -> B")],
        "fn(a: A) -> B",
    ),
    template("call", &["B"], &[("function", "fn() -> B")], "B"),
    template("make", &["A", "B"], &[], "fn(a: A) -> B"),
    template(
        "apply",
        &["T"],
        &[("pair", "(T, fn(value: T) -> Int)")],
        "Int",
    ),
    template(
        "same",
        &["T"],
        &[("function", "fn(value: T) -> T"), ("produce", "fn() -> T")],
        "T",
    ),
    template(
        "map_list",
        &["T", "U"],
        &[("list", "List<T>"), ("function", "fn(value: T) -> U")],
        "List<U>",
    ),
];

/// Non-generic functions, including ones whose parameters are literal types (#337).
const HELPERS: &[Template] = &[
    template("five_only", &[], &[("value", "5")], "Int"),
    template("increment", &[], &[("value", "Int")], "Int"),
    template("maybe", &[], &[("value", "Int")], "Int | nil"),
    template("five", &[], &[], "5"),
];

/// The generator versions. A version only ever adds shapes, behind `self.version` checks that
/// draw nothing from the random number generator in older versions, so a seed gives the same
/// program in every version it existed in (`version_1_programs_never_change` pins version 1).
///
/// - **1**: the shapes of the first run (#341).
/// - **2**: adds generic structs and aliases, including a recursive one (`Entry<K, V>`,
///   `Maybe<T>`, `Tree<T>`), unions of function types, of generic structs and of literals,
///   generic function types in bindings' annotations and functions' return types (#340, #359),
///   longer chains and cycles of generics, `std`'s `merge_list`, and the `expected` variant,
///   which removes only the generic arguments that the expected type alone gives.
pub const VERSIONS: [u32; 2] = [1, 2];

/// Functions that generator version 2 adds to the catalog.
const CATALOG_V2: &[Template] = &[
    template("make3", &["A", "B", "C"], &[], "fn(a: A) -> fn(b: C) -> B"),
    template("constant", &["A", "B"], &[("value", "B")], "fn(a: A) -> B"),
    template("pair_fn", &["T"], &[], "fn(a: T) -> (T, T)"),
    template("none", &["T"], &[], "T | nil"),
    template("empty", &["T"], &[], "List<T>"),
    template("wrap_maybe", &["T"], &[("value", "T")], "Maybe<T>"),
    template(
        "unwrap_maybe",
        &["T"],
        &[("value", "Maybe<T>"), ("default", "T")],
        "T",
    ),
    template("leaf", &["T"], &[("value", "T")], "Tree<T>"),
    template(
        "tree_value",
        &["T"],
        &[("tree", "Tree<T>"), ("default", "T")],
        "T",
    ),
    template(
        "entry",
        &["K", "V"],
        &[("key", "K"), ("value", "V")],
        "Entry<K, V>",
    ),
    template("key_of", &["K", "V"], &[("entry", "Entry<K, V>")], "K"),
    template("value_of", &["K", "V"], &[("entry", "Entry<K, V>")], "V"),
    template("box_or_pair", &["T"], &[("value", "Box<T> | (T, T)")], "T"),
    template(
        "box_or_entry",
        &["K", "V"],
        &[("value", "Box<K> | Entry<K, V>")],
        "K",
    ),
    template(
        "call_either",
        &["T"],
        &[("function", "(fn(a: Int) -> T) | (fn(a: Bytes) -> T)")],
        "T",
    ),
    template(
        "chain4",
        &["A", "B", "C", "D"],
        &[
            ("first", "fn(a: A) -> B"),
            ("second", "fn(b: B) -> C"),
            ("third", "fn(c: C) -> D"),
            ("value", "A"),
        ],
        "D",
    ),
    template(
        "cycle3",
        &["A", "B", "C"],
        &[
            ("first", "fn(a: A) -> B"),
            ("second", "fn(b: B) -> C"),
            ("third", "fn(c: C) -> A"),
            ("value", "A"),
        ],
        "C",
    ),
    // From the standard library, so it isn't printed, and the program is compiled with it
    template(
        "merge_list",
        &["T"],
        &[("a", "List<T>"), ("b", "List<T>")],
        "List<T>",
    ),
];

/// Parses a type written as Rue source, with the given generics.
pub fn parse_ty(text: &str, generics: &[&str]) -> Ty {
    let mut parser = TyParser {
        text: text.as_bytes(),
        pos: 0,
        generics,
    };
    let ty = parser.union();
    assert_eq!(parser.pos, text.len(), "trailing text in type `{text}`");
    ty
}

struct TyParser<'a> {
    text: &'a [u8],
    pos: usize,
    generics: &'a [&'a str],
}

impl TyParser<'_> {
    fn skip_space(&mut self) {
        while self.text.get(self.pos) == Some(&b' ') {
            self.pos += 1;
        }
    }

    fn eat(&mut self, token: &str) -> bool {
        self.skip_space();
        if self.text[self.pos..].starts_with(token.as_bytes()) {
            self.pos += token.len();
            true
        } else {
            false
        }
    }

    fn expect(&mut self, token: &str) {
        assert!(self.eat(token), "expected `{token}` at {}", self.pos);
    }

    fn word(&mut self) -> String {
        self.skip_space();
        let start = self.pos;
        while self
            .text
            .get(self.pos)
            .is_some_and(|c| c.is_ascii_alphanumeric() || *c == b'_')
        {
            self.pos += 1;
        }
        String::from_utf8(self.text[start..self.pos].to_vec()).unwrap()
    }

    fn union(&mut self) -> Ty {
        let mut members = vec![self.atom()];
        while self.eat("|") {
            members.push(self.atom());
        }
        match members.as_slice() {
            [only] => only.clone(),
            [inner, Ty::Nil] => Ty::opt(inner.clone()),
            _ => Ty::Union(members),
        }
    }

    fn atom(&mut self) -> Ty {
        if self.eat("(") {
            let first = self.union();
            if self.eat(")") {
                return first;
            }
            self.expect(",");
            let rest = self.union();
            self.expect(")");
            return Ty::pair(first, rest);
        }

        if self.eat("fn(") {
            let mut params = Vec::new();
            while !self.eat(")") {
                self.word();
                self.expect(":");
                params.push(self.union());
                self.eat(",");
            }
            self.expect("->");
            return Ty::func(params, self.union());
        }

        let word = self.word();

        if let Ok(value) = word.parse() {
            return Ty::Lit(value);
        }

        match word.as_str() {
            "Int" => Ty::Int,
            "Bytes" => Ty::Bytes,
            "Bool" => Ty::Bool,
            "nil" => Ty::Nil,
            "List" | "Box" => {
                self.expect("<");
                let inner = self.union();
                self.expect(">");
                if word == "List" {
                    Ty::list(inner)
                } else {
                    Ty::boxed(inner)
                }
            }
            "Maybe" | "Tree" | "Entry" => {
                let named = Named::ALL
                    .into_iter()
                    .find(|named| named.name() == word)
                    .unwrap();
                self.expect("<");
                let mut args = vec![self.union()];
                while self.eat(",") {
                    args.push(self.union());
                }
                self.expect(">");
                assert_eq!(args.len(), named.arity(), "arguments of `{word}`");
                Ty::named(named, args)
            }
            _ => Ty::Var(
                self.generics
                    .iter()
                    .position(|generic| *generic == word)
                    .unwrap_or_else(|| panic!("unknown type `{word}`")),
            ),
        }
    }
}

/// Matches `pattern`, whose `Ty::Var`s are the generics being solved for, against `target`.
fn unify(pattern: &Ty, target: &Ty, solution: &mut [Option<Ty>]) -> bool {
    match (pattern, target) {
        (Ty::Var(index), _) => {
            if let Some(bound) = &solution[*index] {
                bound == target
            } else {
                solution[*index] = Some(target.clone());
                true
            }
        }
        (Ty::Pair(a, b), Ty::Pair(c, d)) => unify(a, c, solution) && unify(b, d, solution),
        (Ty::Opt(a), Ty::Opt(b)) | (Ty::List(a), Ty::List(b)) | (Ty::Boxed(a), Ty::Boxed(b)) => {
            unify(a, b, solution)
        }
        (Ty::Fn(a, r), Ty::Fn(b, s)) => {
            a.len() == b.len()
                && a.iter().zip(b).all(|(a, b)| unify(a, b, solution))
                && unify(r, s, solution)
        }
        (Ty::Union(a), Ty::Union(b)) => {
            a.len() == b.len() && a.iter().zip(b).all(|(a, b)| unify(a, b, solution))
        }
        (Ty::Named(n, a), Ty::Named(m, b)) => {
            n == m && a.iter().zip(b).all(|(a, b)| unify(a, b, solution))
        }
        _ => pattern == target,
    }
}

/// What's in scope while building an expression.
#[derive(Debug, Clone, Default)]
struct Scope {
    vars: Vec<(String, Ty)>,
    /// The generics of the enclosing function, which are rigid inside its body.
    rigid: Vec<String>,
}

impl Scope {
    fn with(&self, name: String, ty: Ty) -> Self {
        let mut scope = self.clone();
        scope.vars.push((name, ty));
        scope
    }
}

/// A generated program, and the variants to check.
#[derive(Debug, Clone)]
pub struct Generated {
    /// The generator version that made it.
    pub version: u32,
    pub program: Program,
    pub variants: Vec<(String, Variant)>,
}

pub struct Generator {
    rng: Rng,
    /// Which generator version this is (see [`VERSIONS`]).
    version: u32,
    functions: Vec<Function>,
    next_site: SiteId,
    next_name: usize,
    /// Whether expressions may call the program's generic functions. Catalog functions get
    /// plain bodies, such as `function(value)`.
    calls: bool,
}

/// How deep expressions are built in `main`, and in function bodies.
const MAIN_DEPTH: u32 = 3;
const BODY_DEPTH: u32 = 2;

impl Generator {
    /// A generator of version 1, which tests use for the shapes of the first run.
    #[cfg(test)]
    pub fn new(seed: u64) -> Self {
        Self::versioned(seed, 1)
    }

    pub fn versioned(seed: u64, version: u32) -> Self {
        Self {
            rng: Rng::new(seed),
            version,
            functions: Vec::new(),
            next_site: 0,
            next_name: 0,
            calls: true,
        }
    }

    pub fn generate(mut self) -> Generated {
        let v2 = self.version >= 2;
        let all: Vec<&Template> = if v2 {
            CATALOG.iter().chain(CATALOG_V2).collect()
        } else {
            CATALOG.iter().collect()
        };
        let mut catalog = all.clone();
        // Version 2's catalog is larger, so it picks more, for related functions to still meet
        let count = if v2 {
            self.rng.range(5, 11)
        } else {
            self.rng.range(4, 9)
        };
        let mut picked = Vec::new();
        for _ in 0..count {
            picked.push(catalog.remove(self.rng.below(catalog.len())));
        }
        // Keep the catalog's order, so related functions are declared in a familiar order
        picked.sort_by_key(|template| all.iter().position(|other| other.name == template.name));

        for template in picked {
            let function = self.template_function(template);
            self.functions.push(function);
        }

        for template in HELPERS {
            if self.rng.chance(50) {
                let function = self.template_function(template);
                self.functions.push(function);
            }
        }

        for index in 0..self.rng.range(0, 3) {
            let function = self.random_function(index);
            self.functions.push(function);
        }

        let main = self.main();

        let program = Program {
            functions: self.functions,
            main,
        };

        let variants = variants(&mut self.rng, &program, v2);

        Generated {
            version: self.version,
            program,
            variants,
        }
    }

    fn fresh(&mut self, prefix: &str) -> String {
        self.next_name += 1;
        format!("{prefix}{}", self.next_name)
    }

    fn site(&mut self) -> SiteId {
        let site = self.next_site;
        self.next_site += 1;
        site
    }

    fn template_function(&mut self, template: &Template) -> Function {
        let generics: Vec<String> = template.generics.iter().map(ToString::to_string).collect();
        let mut params: Vec<(String, Ty)> = template
            .params
            .iter()
            .map(|(name, ty)| ((*name).to_string(), parse_ty(ty, template.generics)))
            .collect();
        let ret = parse_ty(template.ret, template.generics);
        // Some higher-order functions call themselves, as `map` does in #334
        let recursive = !generics.is_empty()
            && params.iter().any(|(_, ty)| matches!(ty, Ty::Fn(..)))
            && self.rng.chance(15);
        if recursive {
            params.push(("depth".to_string(), Ty::Int));
        }
        self.function(
            template.name.to_string(),
            generics,
            params,
            ret,
            false,
            recursive,
        )
    }

    /// A generic function with a random signature. Some of them call themselves.
    fn random_function(&mut self, index: usize) -> Function {
        let generic_count = self.rng.range(1, 3);
        let generics: Vec<String> = ["A", "B", "C"][..generic_count]
            .iter()
            .map(ToString::to_string)
            .collect();

        let mut params: Vec<(String, Ty)> = (0..self.rng.range(1, 3))
            .map(|param| (format!("p{param}"), self.pattern(generic_count, 2)))
            .collect();

        // Most generics should be in a parameter, but some are only in the return type, so they
        // can only be inferred from the expected type
        for generic in 0..generic_count {
            if !params.iter().any(|(_, ty)| ty.contains_var(generic)) && self.rng.chance(80) {
                let param = self.rng.below(params.len());
                let ty = std::mem::replace(&mut params[param].1, Ty::Nil);
                params[param].1 = Ty::pair(Ty::Var(generic), ty);
            }
        }

        let ret = self.pattern(generic_count, 2);
        let recursive = self.rng.chance(30);
        if recursive {
            params.push(("depth".to_string(), Ty::Int));
        }

        self.function(format!("g{index}"), generics, params, ret, true, recursive)
    }

    /// A random type over the generics `Var(0)` to `Var(count - 1)`.
    fn pattern(&mut self, count: usize, depth: u32) -> Ty {
        if self.version >= 2 && depth > 0 && self.rng.chance(15) {
            let kind = self.rng.below(4);
            let first = self.pattern(count, depth - 1);
            return match kind {
                0 => Ty::named(Named::Maybe, vec![first]),
                1 => Ty::named(Named::Tree, vec![first]),
                2 => Ty::named(Named::Entry, vec![first, self.pattern(count, depth - 1)]),
                _ => union_of(vec![first, self.pattern(count, depth - 1)]),
            };
        }
        let weights: &[usize] = if depth == 0 {
            &[60, 25, 0, 0, 0, 0, 0]
        } else {
            &[40, 15, 15, 8, 5, 4, 10]
        };
        match self.rng.weighted(weights) {
            0 => Ty::Var(self.rng.below(count)),
            1 => self.leaf(&Scope::default()),
            2 => Ty::pair(
                self.pattern(count, depth - 1),
                self.pattern(count, depth - 1),
            ),
            3 => Ty::opt(self.pattern(count, depth - 1)),
            4 => Ty::list(self.pattern(count, depth - 1)),
            5 => Ty::boxed(self.pattern(count, depth - 1)),
            _ => Ty::func(
                vec![self.pattern(count, depth - 1)],
                self.pattern(count, depth - 1),
            ),
        }
    }

    fn leaf(&mut self, scope: &Scope) -> Ty {
        let rigid = if scope.rigid.is_empty() { 0 } else { 25 };
        match self.rng.weighted(&[35, 15, 3, 8, 4, 3, rigid]) {
            0 => Ty::Int,
            1 => Ty::Lit(5),
            2 => Ty::Lit(7),
            3 => Ty::Bytes,
            4 => Ty::Bool,
            5 => Ty::Nil,
            _ => Ty::Rigid(scope.rigid[self.rng.below(scope.rigid.len())].clone()),
        }
    }

    /// A random type that can be named in the scope, for a generic argument or a binding.
    fn ty(&mut self, scope: &Scope, depth: u32) -> Ty {
        if self.version >= 2 && depth > 0 && self.rng.chance(20) {
            return self.ty_v2(scope, depth);
        }
        if depth == 0 || self.rng.chance(60) {
            return self.leaf(scope);
        }
        match self.rng.weighted(&[35, 25, 12, 10, 12]) {
            0 => Ty::pair(self.ty(scope, depth - 1), self.ty(scope, depth - 1)),
            1 => {
                let inner = self.ty(scope, depth - 1);
                if matches!(inner, Ty::Opt(_) | Ty::Nil) {
                    inner
                } else {
                    Ty::opt(inner)
                }
            }
            2 => Ty::list(self.ty(scope, depth - 1)),
            3 => Ty::boxed(self.ty(scope, depth - 1)),
            _ => Ty::func(vec![self.ty(scope, depth - 1)], self.ty(scope, depth - 1)),
        }
    }

    /// A call whose type is a generic function type, as `make()` is for `fn<T>(a: T) -> (T, T)`
    /// (#340) or `make3()` for `fn<T>(a: T) -> fn<U>(b: U) -> (T, U)` (#359), with the generic
    /// function type. Each single-parameter function type in the callee's return type whose
    /// parameter is one of its generics becomes a generic function type, up to two deep. The
    /// innermost return type is built from their generics. The call's own arguments mustn't
    /// name them, since the bare call is outside the wrappers.
    fn wrapped(&mut self, scope: &Scope) -> Option<(Ty, Expr)> {
        let candidates: Vec<usize> = (0..self.functions.len())
            .filter(|&index| {
                let function = &self.functions[index];
                !function.generics.is_empty() && matches!(function.ret, Ty::Fn(..))
            })
            .collect();
        if candidates.is_empty() {
            return None;
        }
        let index = candidates[self.rng.below(candidates.len())];
        let function = self.functions[index].clone();

        let mut solution: Vec<Option<Ty>> = vec![None; function.generics.len()];
        let mut layers: Vec<String> = Vec::new();
        let mut ret = function.ret.clone();
        while let Ty::Fn(params, inner) = &ret {
            let [Ty::Var(generic)] = params.as_slice() else {
                break;
            };
            if layers.len() == 2 || solution[*generic].is_some() {
                break;
            }
            let name = ["T", "U"][layers.len()].to_string();
            solution[*generic] = Some(Ty::Rigid(name.clone()));
            layers.push(name);
            ret = (**inner).clone();
        }
        if layers.is_empty() {
            return None;
        }

        let rigid = Scope {
            vars: Vec::new(),
            rigid: layers.clone(),
        };
        for generic in ret.vars() {
            if solution[generic].is_none() {
                let first = Ty::Rigid(layers[0].clone());
                let last = Ty::Rigid(layers[layers.len() - 1].clone());
                solution[generic] = Some(match self.rng.below(4) {
                    0 => first,
                    1 => Ty::pair(first, last),
                    2 => Ty::opt(last),
                    _ => self.ty(&rigid, 1),
                });
            }
        }
        let generic_args: Vec<Ty> = solution
            .into_iter()
            .map(|arg| arg.unwrap_or_else(|| self.ty(scope, 1)))
            .collect();

        let names_layer =
            |ty: &Ty| ty.any(&|ty| matches!(ty, Ty::Rigid(name) if layers.contains(name)));
        if function
            .params
            .iter()
            .any(|(_, param)| names_layer(&param.subst(&generic_args)))
        {
            return None;
        }

        let call = self.call_to(index, generic_args.clone(), scope, 1);
        let ExprKind::Call {
            function: name,
            generic_args,
            args,
            ..
        } = call.kind
        else {
            unreachable!("call_to makes calls");
        };
        // The wrapper is the site, so the call has none of its own
        let call = Expr::new(
            ExprKind::Call {
                site: None,
                function: name,
                generic_args: generic_args.clone(),
                args,
            },
            call.ty,
        );

        let mut annotation = ret.subst(&generic_args);
        for name in layers.iter().rev() {
            annotation = Ty::GenFn(
                vec![name.clone()],
                vec![Ty::Rigid(name.clone())],
                Box::new(annotation),
            );
        }
        let wrappers = layers
            .iter()
            .map(|name| {
                let param = self.fresh("w");
                (vec![name.clone()], vec![(param, Ty::Rigid(name.clone()))])
            })
            .collect();

        let site = self.site();
        let wrapped = ExprKind::Wrapped {
            site,
            layers: wrappers,
            call: Box::new(call),
        };
        Some((annotation.clone(), Expr::new(wrapped, annotation)))
    }

    /// The kinds of types that generator version 2 adds.
    fn ty_v2(&mut self, scope: &Scope, depth: u32) -> Ty {
        match self.rng.weighted(&[20, 15, 15, 10, 15, 10, 15]) {
            // An expected literal union, such as `let x: 5 | 7 = ...`
            0 => Ty::Union(vec![Ty::Lit(5), Ty::Lit(7)]),
            1 => {
                let first = self.ty(scope, depth - 1);
                let second = self.ty(scope, depth - 1);
                union_of(vec![first, second])
            }
            2 => Ty::named(Named::Maybe, vec![self.ty(scope, depth - 1)]),
            3 => Ty::named(Named::Tree, vec![self.ty(scope, depth - 1)]),
            4 => {
                let key = self.ty(scope, depth - 1);
                let value = self.ty(scope, depth - 1);
                Ty::named(Named::Entry, vec![key, value])
            }
            // A union of generic structs
            5 => {
                let inner = self.ty(scope, depth - 1);
                let value = self.ty(scope, depth - 1);
                Ty::Union(vec![
                    Ty::boxed(inner.clone()),
                    Ty::named(Named::Entry, vec![inner, value]),
                ])
            }
            // A union of function types
            _ => {
                let ret = self.ty(scope, depth - 1);
                Ty::Union(vec![
                    Ty::func(vec![Ty::Int], ret.clone()),
                    Ty::func(vec![Ty::Bytes], ret),
                ])
            }
        }
    }

    /// Builds a function's body, with the function's generics rigid inside it. Catalog functions
    /// get plain bodies (`function(value)`); random ones may call the functions before them. The
    /// arguments of a recursion guard's calls may call them in both, as in `map(duplicate, 1)`
    /// inside `map` (#334).
    fn function(
        &mut self,
        name: String,
        generics: Vec<String>,
        params: Vec<(String, Ty)>,
        ret: Ty,
        calls: bool,
        recursive: bool,
    ) -> Function {
        let scope = Scope {
            vars: params
                .iter()
                .map(|(name, ty)| (name.clone(), ty.rigid(&generics)))
                .collect(),
            rigid: generics.clone(),
        };

        let mut function = Function {
            name,
            generics,
            params,
            ret,
            recursion: None,
            lets: Vec::new(),
            body: Expr::new(ExprKind::Nil, Ty::Nil),
        };

        if recursive {
            function.recursion = Some(self.recursion(&function, &scope));
        }

        let ret = function.ret.rigid(&function.generics);
        self.calls = calls;
        function.body = self.expr(&ret, &scope, BODY_DEPTH, true);
        self.calls = true;
        function
    }

    /// The recursion guard of a function that calls itself: an optional call with other generic
    /// arguments (`map(duplicate, 1)` inside `map`, #334), and a call with its own generics.
    fn recursion(&mut self, function: &Function, scope: &Scope) -> Recursion {
        let own: Vec<Ty> = function
            .generics
            .iter()
            .map(|name| Ty::Rigid(name.clone()))
            .collect();

        let returned = self.self_call(function, scope, own);

        let inner = self.rng.chance(60).then(|| {
            let args = self.instantiate(function, vec![None; function.generics.len()], scope);
            self.self_call(function, scope, args)
        });

        Recursion { inner, returned }
    }

    fn self_call(&mut self, function: &Function, scope: &Scope, generic_args: Vec<Ty>) -> Expr {
        let args: Vec<Expr> = function
            .params
            .iter()
            .map(|(name, ty)| {
                if name == "depth" {
                    Expr::new(ExprKind::Decrement(name.clone()), Ty::Int)
                } else {
                    self.expr(&ty.subst(&generic_args), scope, 1, true)
                }
            })
            .collect();

        let site = self.site();

        let ty = function.ret.subst(&generic_args);
        Expr::new(
            ExprKind::Call {
                site: Some(site),
                function: function.name.clone(),
                generic_args,
                args,
            },
            ty,
        )
    }

    /// `main` binds a few values, and returns all of them, so the output depends on each.
    fn main(&mut self) -> Function {
        let scope = Scope::default();
        let mut lets = Vec::new();
        let mut vars = Vec::new();
        let mut parts = Vec::new();

        for _ in 0..self.rng.range(1, 4) {
            let annotated = self.rng.chance(60);
            let local = Scope {
                vars: vars.clone(),
                rigid: Vec::new(),
            };

            // Most bindings are a call to a generic function, with random generic arguments
            let generic: Vec<usize> = (0..self.functions.len())
                .filter(|&index| !self.functions[index].generics.is_empty())
                .collect();
            let (goal, value) = if !generic.is_empty() && self.rng.chance(80) {
                let index = generic[self.rng.below(generic.len())];
                let function = self.functions[index].clone();
                let generic_args =
                    self.instantiate(&function, vec![None; function.generics.len()], &scope);
                let value = self.call_to(index, generic_args, &local, MAIN_DEPTH);
                // Sometimes the binding's type is wider than the call's
                let goal = match &value.ty {
                    Ty::Lit(_) if self.rng.chance(30) => Ty::Int,
                    ty @ (Ty::Int | Ty::Pair(..)) if self.rng.chance(15) => Ty::opt(ty.clone()),
                    ty => ty.clone(),
                };
                (goal, value)
            } else {
                let goal = self.ty(&scope, 2);
                let value = self.expr(&goal, &local, MAIN_DEPTH, annotated);
                (goal, value)
            };
            let name = format!("v{}", lets.len());
            // An unannotated binding's inferred type can differ from the explicit program's,
            // such as `Int` for `5`, so it's only returned, as `Any`, and not used elsewhere
            if annotated {
                vars.push((name.clone(), goal.clone()));
            }
            parts.push(Expr::new(ExprKind::Var(name.clone()), Ty::Any));
            lets.push(Let {
                name,
                annotation: annotated.then_some(goal),
                value,
            });
        }

        // A binding with a generic function type, either a call that's given it, or a call of a
        // function that returns one
        if self.version >= 2 && self.rng.chance(40) {
            let returned = self.rng.chance(35);
            if let Some((annotation, wrapped)) = self.wrapped(&scope) {
                let name = format!("v{}", lets.len());
                let value = if returned {
                    let helper = format!("generic_function{}", lets.len());
                    self.functions.push(Function {
                        name: helper.clone(),
                        generics: Vec::new(),
                        params: Vec::new(),
                        ret: annotation.clone(),
                        recursion: None,
                        lets: Vec::new(),
                        body: wrapped,
                    });
                    let call = ExprKind::Call {
                        site: None,
                        function: helper,
                        generic_args: Vec::new(),
                        args: Vec::new(),
                    };
                    Expr::new(call, annotation.clone())
                } else {
                    wrapped
                };
                // A function's code differs between a wrapper and the call it wraps, so it's
                // called rather than returned
                parts.push(applied(&name, &annotation));
                lets.push(Let {
                    name,
                    annotation: Some(annotation),
                    value,
                });
            }
        }

        let mut parts = parts.into_iter().rev();
        let mut body = parts.next().unwrap();
        for first in parts {
            body = Expr::new(ExprKind::Pair(Box::new(first), Box::new(body)), Ty::Any);
        }

        Function {
            name: "main".to_string(),
            generics: Vec::new(),
            params: Vec::new(),
            ret: Ty::Any,
            recursion: None,
            lets,
            body,
        }
    }

    /// An expression of type `goal`. `expected` says whether the position has an expected type,
    /// such as an annotated binding or an argument, which inference can use.
    fn expr(&mut self, goal: &Ty, scope: &Scope, depth: u32, expected: bool) -> Expr {
        if depth > 0
            && self.calls
            && self.rng.chance(50)
            && let Some(call) = self.call(goal, scope, depth)
        {
            return call;
        }

        if self.rng.chance(60)
            && let Some(found) = self.in_scope(goal, scope, depth)
        {
            return found;
        }

        let child_depth = depth.saturating_sub(1);

        let kind = match goal {
            Ty::Int => ExprKind::Int(self.rng.int(1, 9)),
            Ty::Lit(value) => ExprKind::Int(*value),
            Ty::Bytes => ExprKind::Bytes,
            Ty::Bool => ExprKind::Bool,
            Ty::Nil => ExprKind::Nil,
            Ty::Pair(first, rest) => ExprKind::Pair(
                Box::new(self.expr(first, scope, child_depth, expected)),
                Box::new(self.expr(rest, scope, child_depth, expected)),
            ),
            Ty::Opt(inner) => {
                if self.rng.chance(30) {
                    ExprKind::Nil
                } else {
                    let expected = expected && !goal.is_function_union();
                    return Expr {
                        ty: goal.clone(),
                        ..self.expr(inner, scope, depth, expected)
                    };
                }
            }
            Ty::List(inner) => {
                let expected = expected && !goal.is_function_union();
                match self.rng.below(3) {
                    0 => ExprKind::Nil,
                    count => ExprKind::List(
                        (0..count)
                            .map(|_| self.expr(inner, scope, child_depth, expected))
                            .collect(),
                    ),
                }
            }
            Ty::Boxed(inner) => {
                let value = self.expr(inner, scope, child_depth, true);
                // A struct literal's generic arguments can't be inferred, so they're kept
                ExprKind::BoxLit {
                    site: self.site(),
                    arg: (**inner).clone(),
                    value: Box::new(value),
                }
            }
            Ty::Fn(params, ret) => {
                return self.function_value(goal, params, ret, scope, depth, expected);
            }
            Ty::Rigid(_) => {
                return self
                    .in_scope(goal, scope, depth)
                    .unwrap_or_else(|| Expr::new(ExprKind::Unchecked, goal.clone()));
            }
            Ty::Union(members) => {
                let member = members[self.rng.below(members.len())].clone();
                let expected = expected && !goal.is_function_union();
                return Expr {
                    ty: goal.clone(),
                    ..self.expr(&member, scope, depth, expected)
                };
            }
            Ty::Named(Named::Maybe, args) => {
                if self.rng.chance(30) {
                    ExprKind::Nil
                } else {
                    let expected = expected && !goal.is_function_union();
                    return Expr {
                        ty: goal.clone(),
                        ..self.expr(&args[0], scope, depth, expected)
                    };
                }
            }
            Ty::Named(Named::Tree, args) => {
                if child_depth > 0 && self.rng.chance(40) {
                    let expected = expected && !goal.is_function_union();
                    let value = self.expr(&args[0], scope, child_depth, expected);
                    let left = self.expr(goal, scope, child_depth - 1, expected);
                    let right = self.expr(goal, scope, child_depth - 1, expected);
                    let branches = Expr::new(
                        ExprKind::Pair(Box::new(left), Box::new(right)),
                        Ty::pair(goal.clone(), goal.clone()),
                    );
                    ExprKind::Pair(Box::new(value), Box::new(branches))
                } else {
                    ExprKind::Nil
                }
            }
            Ty::Named(Named::Entry, args) => {
                let key = self.expr(&args[0], scope, child_depth, true);
                let value = self.expr(&args[1], scope, child_depth, true);
                // A struct literal's generic arguments can't be inferred, so they're kept
                ExprKind::EntryLit {
                    site: self.site(),
                    args: args.clone(),
                    key: Box::new(key),
                    value: Box::new(value),
                }
            }
            // Only bindings and return types have generic function types, and they're built
            // with `wrapped`
            Ty::GenFn(..) => ExprKind::Unchecked,
            Ty::Var(_) | Ty::Any => unreachable!("expressions have concrete types"),
        };

        Expr::new(kind, goal.clone())
    }

    /// A parameter or binding of the goal type, or one that leads to it: a pair's field, a
    /// box's value, or a function's result.
    fn in_scope(&mut self, goal: &Ty, scope: &Scope, depth: u32) -> Option<Expr> {
        let mut direct = Vec::new();
        let mut functions = Vec::new();

        for (name, ty) in &scope.vars {
            let var = Expr::new(ExprKind::Var(name.clone()), ty.clone());
            if ty == goal {
                direct.push(var);
                continue;
            }
            match ty {
                Ty::Pair(first, _) if **first == *goal => {
                    direct.push(Expr::new(ExprKind::First(Box::new(var)), goal.clone()));
                }
                Ty::Pair(_, rest) if **rest == *goal => {
                    direct.push(Expr::new(ExprKind::Rest(Box::new(var)), goal.clone()));
                }
                Ty::Boxed(inner) if **inner == *goal => {
                    direct.push(Expr::new(ExprKind::Field(Box::new(var)), goal.clone()));
                }
                Ty::Named(Named::Entry, args) if args[0] == *goal => {
                    direct.push(Expr::new(ExprKind::Key(Box::new(var)), goal.clone()));
                }
                Ty::Named(Named::Entry, args) if args[1] == *goal => {
                    direct.push(Expr::new(ExprKind::Field(Box::new(var)), goal.clone()));
                }
                Ty::Fn(params, ret) if **ret == *goal => {
                    functions.push((name.clone(), params.clone()));
                }
                _ => {}
            }
        }

        if !direct.is_empty() {
            let pick = self.rng.below(direct.len());
            return Some(direct.swap_remove(pick));
        }

        if functions.is_empty() || depth == 0 {
            return None;
        }

        let (name, params) = functions.swap_remove(self.rng.below(functions.len()));
        let args = params
            .iter()
            .map(|param| self.expr(param, scope, depth - 1, true))
            .collect();
        Some(Expr::new(ExprKind::CallLocal { name, args }, goal.clone()))
    }

    /// A call to one of the program's functions whose return type can be the goal, or a type
    /// assignable to it.
    fn call(&mut self, goal: &Ty, scope: &Scope, depth: u32) -> Option<Expr> {
        // A value of a narrower type, which inference may pick instead of the goal (#284)
        let target = match goal {
            Ty::Opt(inner) if self.rng.chance(30) => (**inner).clone(),
            Ty::Int if self.rng.chance(20) => Ty::Lit(5),
            _ => goal.clone(),
        };

        let candidates: Vec<(usize, Vec<Option<Ty>>)> = self
            .functions
            .iter()
            .enumerate()
            .filter_map(|(index, function)| {
                let mut solution = vec![None; function.generics.len()];
                unify(&function.ret, &target, &mut solution).then_some((index, solution))
            })
            .collect();

        if candidates.is_empty() {
            return None;
        }

        let (index, solution) = candidates[self.rng.below(candidates.len())].clone();
        let function = self.functions[index].clone();
        let generic_args = self.instantiate(&function, solution, scope);

        Some(self.call_to(index, generic_args, scope, depth))
    }

    /// Picks the generic arguments of a call, given the ones that are already known. Most shapes
    /// that inference handles come from the functions passed as arguments, such as `map` with
    /// `duplicate` or `first_of`, so the types of some function parameters are taken from one of
    /// the program's functions first. Some calls also use one type for all remaining generics,
    /// as `run::<5, 5>(identity, five_only, 5)` does.
    fn instantiate(
        &mut self,
        function: &Function,
        mut solution: Vec<Option<Ty>>,
        scope: &Scope,
    ) -> Vec<Ty> {
        let fn_params: Vec<Ty> = function
            .params
            .iter()
            .filter(|(_, ty)| matches!(ty, Ty::Fn(..)))
            .map(|(_, ty)| ty.clone())
            .collect();

        for param in fn_params {
            if !self.rng.chance(65) {
                continue;
            }
            let shared = self.ty(scope, 1);
            let candidates: Vec<Vec<Option<Ty>>> = self
                .functions
                .iter()
                .filter_map(|other| {
                    let args = vec![shared.clone(); other.generics.len()];
                    let mut candidate = solution.clone();
                    unify(&param, &other.ty().subst(&args), &mut candidate).then_some(candidate)
                })
                .collect();
            if !candidates.is_empty() {
                solution.clone_from(&candidates[self.rng.below(candidates.len())]);
            }
        }

        let shared = self.rng.chance(35).then(|| self.ty(scope, 1));
        solution
            .into_iter()
            .map(|arg| arg.unwrap_or_else(|| shared.clone().unwrap_or_else(|| self.ty(scope, 1))))
            .collect()
    }

    /// A call to the function with the index, with the given generic arguments.
    fn call_to(&mut self, index: usize, generic_args: Vec<Ty>, scope: &Scope, depth: u32) -> Expr {
        let function = self.functions[index].clone();

        let args: Vec<Expr> = function
            .params
            .iter()
            .map(|(name, ty)| {
                if name == "depth" {
                    Expr::new(ExprKind::Int(self.rng.int(1, 2)), Ty::Int)
                } else {
                    self.expr(
                        &ty.subst(&generic_args),
                        scope,
                        depth.saturating_sub(1),
                        true,
                    )
                }
            })
            .collect();

        let site = (!function.generics.is_empty()).then(|| self.site());

        let ty = function.ret.subst(&generic_args);

        Expr::new(
            ExprKind::Call {
                site,
                function: function.name,
                generic_args,
                args,
            },
            ty,
        )
    }

    /// A value of a function type: a reference to one of the program's functions, or a lambda.
    fn function_value(
        &mut self,
        goal: &Ty,
        params: &[Ty],
        ret: &Ty,
        scope: &Scope,
        depth: u32,
        expected: bool,
    ) -> Expr {
        if self.rng.chance(70) {
            let candidates: Vec<(usize, Vec<Option<Ty>>)> = self
                .functions
                .iter()
                .enumerate()
                .filter_map(|(index, function)| {
                    let mut solution = vec![None; function.generics.len()];
                    unify(&function.ty(), goal, &mut solution).then_some((index, solution))
                })
                .collect();

            if !candidates.is_empty() {
                let (index, solution) = candidates[self.rng.below(candidates.len())].clone();
                let function = self.functions[index].clone();
                let generic_args: Vec<Ty> = solution
                    .into_iter()
                    .map(|arg| arg.unwrap_or_else(|| self.ty(scope, 1)))
                    .collect();
                let site = (!function.generics.is_empty()).then(|| self.site());
                return Expr::new(
                    ExprKind::FnRef {
                        site,
                        function: function.name,
                        generic_args,
                    },
                    goal.clone(),
                );
            }
        }

        if params.len() == 1 && params[0] == *ret && self.rng.chance(15) {
            return Expr::new(ExprKind::IdentityLambda, goal.clone());
        }

        // An untyped lambda needs an expected type for its parameters
        let typed = !expected || self.rng.chance(40);
        let mut inner = scope.clone();
        let params: Vec<(String, Ty)> = params
            .iter()
            .map(|param| {
                let name = self.fresh("x");
                inner = inner.with(name.clone(), param.clone());
                (name, param.clone())
            })
            .collect();
        let body = self.expr(ret, &inner, depth.saturating_sub(1), false);

        Expr::new(
            ExprKind::Lambda {
                params,
                typed,
                body: Box::new(body),
            },
            goal.clone(),
        )
    }
}

/// For each site, which of its generic arguments may be removed, so that inference has something
/// to infer each of them from: an argument that gives the generic a type, or the expected type,
/// for a generic in the return type. An untyped lambda doesn't give its parameters a type, and
/// `nil` gives none. Removing any other argument would be a correct rejection.
///
/// A call whose generic arguments the variant removes only gives a type to the generics of the
/// call it's passed to if its own generics are inferred from its arguments. In
/// `first_of(pairs())`, nothing gives `T` a type.
pub fn removable_sites(program: &Program, variant: &Variant) -> BTreeMap<SiteId, Vec<bool>> {
    removable_sites_with(program, variant, true)
}

/// Which generic arguments may be removed, as [`removable_sites`] says, counting expected types
/// only if `use_expected` is true. The `expected` variant compares the two, to find the sites
/// that only their expected type gives anything.
pub fn removable_sites_with(
    program: &Program,
    variant: &Variant,
    use_expected: bool,
) -> BTreeMap<SiteId, Vec<bool>> {
    let mut walk = Removable {
        program,
        variant,
        use_expected,
        masks: BTreeMap::new(),
    };
    for function in program.functions_and_main() {
        for (expr, root) in function.roots() {
            walk.expr(expr, root.has_expected_type());
        }
    }
    walk.masks
}

/// Reduces what the variant removes to what [`removable_sites`] allows, keeping the other
/// arguments of a site as placeholders. Keeping one argument can make others removable again:
/// in `map(fn(x) => x, nil)`, keeping `A` gives the lambda's `x` a type, and so `B` one, and in
/// `g(identity)`, keeping either site's arguments makes the other's removable. So it keeps one
/// argument at a time, in order. It only ever keeps more, so it stops.
pub fn legalize(program: &Program, variant: &Variant) -> Variant {
    let mut variant = variant.clone();
    'fix: loop {
        let removable = removable_sites(program, &variant);
        for (&site, strip) in &variant.stripped {
            // A site that shrinking removed along with its expression doesn't matter
            let Some(mask) = removable.get(&site) else {
                continue;
            };
            let mut removed = removed_mask(strip, mask.len());
            let Some(kept) = (0..mask.len()).find(|&generic| removed[generic] && !mask[generic])
            else {
                continue;
            };
            removed[kept] = false;
            if removed.iter().any(|&removed| removed) {
                variant.stripped.insert(site, Strip::Placeholders(removed));
            } else {
                variant.stripped.remove(&site);
            }
            continue 'fix;
        }
        return variant;
    }
}

/// Which generic arguments of a site a strip removes.
fn removed_mask(strip: &Strip, len: usize) -> Vec<bool> {
    match strip {
        Strip::All => vec![true; len],
        Strip::Placeholders(mask) => mask.clone(),
    }
}

struct Removable<'a> {
    program: &'a Program,
    variant: &'a Variant,
    use_expected: bool,
    masks: BTreeMap<SiteId, Vec<bool>>,
}

impl Removable<'_> {
    /// Whether the position of an expression has an expected type follows how
    /// `Generator::expr` builds it: arguments and fields have one, lambda bodies don't, and a
    /// union with a function type in it (`F | nil`, whose value is the `F` expression itself, or
    /// `List<F>`, which passes it on to its items) gives a lambda no parameter types.
    fn expr(&mut self, expr: &Expr, expected: bool) {
        let expected = self.use_expected && expected && !expr.ty.is_function_union();

        // A shrunk program can call a function it no longer declares. It doesn't compile, so its
        // sites don't matter.
        match &expr.kind {
            ExprKind::Call {
                site: Some(site),
                function,
                args,
                ..
            } => {
                if let Some(function) = self.program.function(function) {
                    let witnessed = Witnessed::new(
                        self.program,
                        self.variant,
                        function,
                        *site,
                        args,
                        expected,
                        true,
                    );
                    self.masks.insert(*site, witnessed.generics);
                    // The generics of a function passed to the call get a type through it
                    self.masks.extend(witnessed.references);
                }
            }
            ExprKind::FnRef {
                site: Some(site),
                function,
                ..
            } => {
                // Unless the call it's passed to has already given its mask
                if !self.masks.contains_key(site)
                    && let Some(function) = self.program.function(function)
                {
                    let mut solution = vec![None; function.generics.len()];
                    unify(&function.ty(), &expr.ty, &mut solution);
                    let mask = solution
                        .iter()
                        .map(|arg| expected && arg.is_some())
                        .collect();
                    self.masks.insert(*site, mask);
                }
            }
            // A struct literal's generic arguments can't be inferred, so they're kept
            ExprKind::BoxLit { site, .. } => {
                self.masks.insert(*site, vec![false]);
            }
            ExprKind::EntryLit { site, .. } => {
                self.masks.insert(*site, vec![false, false]);
            }
            // The wrapper's call is removed as a whole, when the expected generic function type
            // and the arguments give each of the callee's generics a type
            ExprKind::Wrapped { site, call, .. } => {
                if let ExprKind::Call { function, args, .. } = &call.kind
                    && let Some(function) = self.program.function(function)
                {
                    let witnessed = Witnessed::new(
                        self.program,
                        self.variant,
                        function,
                        *site,
                        args,
                        self.use_expected,
                        true,
                    );
                    let all = witnessed.generics.iter().all(|&removable| removable);
                    self.masks.insert(*site, vec![all; function.generics.len()]);
                }
            }
            _ => {}
        }

        let child_expected = match &expr.kind {
            ExprKind::Call { .. }
            | ExprKind::CallLocal { .. }
            | ExprKind::BoxLit { .. }
            | ExprKind::EntryLit { .. }
            | ExprKind::Apply(..) => true,
            ExprKind::Lambda { .. } => false,
            _ => expected,
        };
        for child in expr.children() {
            self.expr(child, child_expected);
        }
    }
}

/// What a call's arguments give a type to.
///
/// A function passed with its own generic arguments removed, such as `duplicate` in
/// `map(duplicate, 42)`, or a generic lambda, gives nothing a type by itself: its generics are
/// inferred from the parameter it's passed to. Each one links parts of the parameter's type to
/// parts of its own type (`A` to `T`, and `B` to `(T, T)`), and types flow through the links
/// both ways until nothing changes: `42` gives `A` a type, so `T` gets one, and so `B` does. In
/// `g(identity)` with `g<A>(f: fn(a: A) -> A)`, nothing does.
struct Witnessed {
    /// Which of the call's generics could have their arguments removed: those that get a type
    /// when every other generic whose argument the variant keeps is known.
    generics: Vec<bool>,
    /// For each function passed with removed generic arguments, which of its generics get a
    /// type.
    references: BTreeMap<SiteId, Vec<bool>>,
}

/// The parts of a parameter's type and of a function's type, passed for it, that line up.
#[derive(Clone)]
struct Link {
    site: Option<SiteId>,
    count: usize,
    pairs: Vec<(Ty, Ty)>,
}

/// An untyped lambda, whose body gives its return type a type only once the call's generics in
/// its parameter types have one, as the compiler compiles it after the other arguments.
#[derive(Clone)]
struct Lambda<'a> {
    params: BTreeSet<usize>,
    ret: Ty,
    body: &'a Expr,
}

/// What an argument gives a type to through the call, rather than directly.
#[derive(Clone)]
struct Facts<'a> {
    links: Vec<Link>,
    lambdas: Vec<Lambda<'a>>,
    /// Whether a typed lambda's parameter annotations give generics a type. They're only upper
    /// bounds, so a nested call such as `cycle(fn(a: 5) => nil, fn(b: (5, 5) | nil) => 5, 5)`
    /// infers its `B` as `nil` from the body, and its type only passes `nil` on.
    annotations: bool,
}

impl Facts<'_> {
    fn new(annotations: bool) -> Self {
        Self {
            links: Vec::new(),
            lambdas: Vec::new(),
            annotations,
        }
    }
}

impl Witnessed {
    fn new(
        program: &Program,
        variant: &Variant,
        function: &Function,
        site: SiteId,
        args: &[Expr],
        expected: bool,
        annotations: bool,
    ) -> Self {
        let count = function.generics.len();
        let removed = variant
            .stripped
            .get(&site)
            .map_or_else(|| vec![false; count], |strip| removed_mask(strip, count));

        // What the arguments give a type to directly, and the expected type
        let mut direct = BTreeSet::new();
        if expected {
            direct.extend(function.ret.vars());
        }
        let mut facts = Facts::new(annotations);
        for ((_, param), arg) in function.params.iter().zip(args) {
            witness(program, variant, param, arg, &mut direct, &mut facts);
        }

        let kept = |except: Option<usize>| {
            let mut known = direct.clone();
            known
                .extend((0..count).filter(|&generic| !removed[generic] && Some(generic) != except));
            known
        };

        let generics = (0..count)
            .map(|generic| {
                solve(program, variant, facts.clone(), kept(Some(generic)))
                    .0
                    .contains(&generic)
            })
            .collect();

        let (_, links) = solve(program, variant, facts, kept(None));
        let references = links
            .into_iter()
            .filter_map(|(link, determined)| {
                let mask = (0..link.count)
                    .map(|generic| determined.contains(&generic))
                    .collect();
                link.site.map(|site| (site, mask))
            })
            .collect();

        Self {
            generics,
            references,
        }
    }
}

/// Lets types flow through the links, and into the lambdas whose parameters get them, from the
/// call's generics that are known, until nothing changes. Returns the call's generics that get a
/// type, and each link with the generics of its function that get one.
fn solve(
    program: &Program,
    variant: &Variant,
    mut facts: Facts<'_>,
    mut known: BTreeSet<usize>,
) -> (BTreeSet<usize>, Vec<(Link, BTreeSet<usize>)>) {
    let mut determined = vec![BTreeSet::new(); facts.links.len()];
    loop {
        let mut changed = false;

        for (link, determined) in facts.links.iter().zip(&mut determined) {
            for (param, own) in &link.pairs {
                if own.vars().is_subset(determined) {
                    for generic in param.vars() {
                        changed |= known.insert(generic);
                    }
                }
                if param.vars().is_subset(&known) {
                    for generic in own.vars() {
                        changed |= determined.insert(generic);
                    }
                }
            }
        }

        let (ready, waiting): (Vec<_>, Vec<_>) = std::mem::take(&mut facts.lambdas)
            .into_iter()
            .partition(|lambda| lambda.params.is_subset(&known));
        facts.lambdas = waiting;
        for lambda in ready {
            changed = true;
            let mut found = Facts::new(facts.annotations);
            witness(
                program,
                variant,
                &lambda.ret,
                lambda.body,
                &mut known,
                &mut found,
            );
            determined.resize(determined.len() + found.links.len(), BTreeSet::new());
            facts.links.extend(found.links);
            facts.lambdas.extend(found.lambdas);
        }

        if !changed {
            return (known, facts.links.into_iter().zip(determined).collect());
        }
    }
}

/// Lines up the parts of two types, down to where either one is a generic.
fn align(param: &Ty, own: &Ty, pairs: &mut Vec<(Ty, Ty)>) {
    match (param, own) {
        (Ty::Var(_), _) | (_, Ty::Var(_)) => pairs.push((param.clone(), own.clone())),
        (Ty::Pair(a, b), Ty::Pair(c, d)) => {
            align(a, c, pairs);
            align(b, d, pairs);
        }
        (Ty::Opt(a), Ty::Opt(b)) | (Ty::List(a), Ty::List(b)) | (Ty::Boxed(a), Ty::Boxed(b)) => {
            align(a, b, pairs);
        }
        (Ty::Fn(a, r), Ty::Fn(b, s)) if a.len() == b.len() => {
            for (a, b) in a.iter().zip(b) {
                align(a, b, pairs);
            }
            align(r, s, pairs);
        }
        _ => {}
    }
}

/// What an argument gives the generics in its parameter's type. The parts of a pair or list
/// are matched with the parts of the parameter's type, and a lambda's body with its return type.
/// Elsewhere, the generics in the parameter's type get a type only if the argument's type is
/// [`known`].
fn witness<'a>(
    program: &Program,
    variant: &Variant,
    pattern: &Ty,
    arg: &'a Expr,
    witnessed: &mut BTreeSet<usize>,
    facts: &mut Facts<'a>,
) {
    // `nil` gives nothing to `T | nil`, and only `nil` to `T`, which a nested call such as
    // `or_else(nil, nil)` then passes on as its type, whatever its explicit type is. So `nil`
    // and anything of type `nil` count as giving nothing a type.
    if matches!(arg.kind, ExprKind::Nil) || arg.ty == Ty::Nil {
        return;
    }

    match (pattern, &arg.kind) {
        (Ty::Pair(first, rest), ExprKind::Pair(first_arg, rest_arg)) => {
            witness(program, variant, first, first_arg, witnessed, facts);
            witness(program, variant, rest, rest_arg, witnessed, facts);
        }
        (Ty::List(inner), ExprKind::List(items)) => {
            for item in items {
                witness(program, variant, inner, item, witnessed, facts);
            }
        }
        // A value passed for `T | nil` is one of `T`'s, and the same for `Maybe<T>`
        (Ty::Opt(inner), _) => witness(program, variant, inner, arg, witnessed, facts),
        (Ty::Named(Named::Maybe, args), _) => {
            witness(program, variant, &args[0], arg, witnessed, facts);
        }
        // A value passed for another union could be any member's, so it only gives a type to the
        // generics in all of them
        (Ty::Union(members), _) => {
            if known(program, variant, arg) {
                let mut common = members[0].vars();
                for member in &members[1..] {
                    common = common.intersection(&member.vars()).copied().collect();
                }
                witnessed.extend(common);
            }
        }
        (Ty::Fn(params, ret), ExprKind::Lambda { typed, body, .. }) => {
            // A typed lambda's annotations give its parameters' generics a type. An untyped
            // lambda's parameters get theirs from the call, so its body is only looked at once
            // they have one.
            if *typed {
                if facts.annotations {
                    witnessed.extend(params.iter().flat_map(Ty::vars));
                }
                witness(program, variant, ret, body, witnessed, facts);
            } else {
                facts.lambdas.push(Lambda {
                    params: params.iter().flat_map(Ty::vars).collect(),
                    ret: (**ret).clone(),
                    body,
                });
            }
        }
        (
            _,
            ExprKind::FnRef {
                site: Some(site),
                function,
                generic_args,
            },
        ) if variant.stripped.contains_key(site) => {
            let Some(function) = program.function(function) else {
                return;
            };
            // The generic arguments that are kept are types, and the removed ones stay generics
            let removed = removed_mask(&variant.stripped[site], function.generics.len());
            let args: Vec<Ty> = generic_args
                .iter()
                .enumerate()
                .map(|(generic, arg)| {
                    if removed[generic] {
                        Ty::Var(generic)
                    } else {
                        arg.clone()
                    }
                })
                .collect();
            let mut pairs = Vec::new();
            align(pattern, &function.ty().subst(&args), &mut pairs);
            facts.links.push(Link {
                site: Some(*site),
                count: function.generics.len(),
                pairs,
            });
        }
        (_, ExprKind::IdentityLambda) => {
            let mut pairs = Vec::new();
            align(pattern, &Ty::func(vec![Ty::Var(0)], Ty::Var(0)), &mut pairs);
            facts.links.push(Link {
                site: None,
                count: 1,
                pairs,
            });
        }
        _ => {
            if known(program, variant, arg) {
                witnessed.extend(pattern.vars());
            }
        }
    }
}

/// Whether an expression's type is known without inferring it from where it's used: not `nil` or
/// an untyped lambda, and not a call whose removed generic arguments its own arguments don't give
/// a type to (as in `first_of(pairs())`), not counting typed lambdas' parameter annotations.
fn known(program: &Program, variant: &Variant, expr: &Expr) -> bool {
    if expr.ty == Ty::Nil {
        return false;
    }
    let known = |expr: &Expr| known(program, variant, expr);
    match &expr.kind {
        ExprKind::Nil | ExprKind::IdentityLambda | ExprKind::Lambda { typed: false, .. } => false,
        ExprKind::Var(_)
        | ExprKind::CallLocal { .. }
        | ExprKind::Decrement(_)
        | ExprKind::Int(_)
        | ExprKind::Bytes
        | ExprKind::Bool
        | ExprKind::Unchecked
        | ExprKind::BoxLit { .. }
        | ExprKind::EntryLit { .. }
        | ExprKind::Apply(..)
        | ExprKind::Wrapped { .. } => true,
        ExprKind::First(inner)
        | ExprKind::Rest(inner)
        | ExprKind::Field(inner)
        | ExprKind::Key(inner) => known(inner),
        ExprKind::Pair(first, rest) => known(first) && known(rest),
        ExprKind::List(items) => !items.is_empty() && items.iter().all(known),
        ExprKind::Lambda { body, .. } => known(body),
        ExprKind::FnRef { site, .. } => {
            site.is_none_or(|site| !variant.stripped.contains_key(&site))
        }
        ExprKind::Call {
            site,
            function,
            args,
            ..
        } => {
            let Some(site) = site.filter(|site| variant.stripped.contains_key(site)) else {
                return true;
            };
            let Some(function) = program.function(function) else {
                return false;
            };
            // Its type is only known if its removed generics are inferred from its arguments,
            // rather than from the expected type
            let removed = removed_mask(&variant.stripped[&site], function.generics.len());
            let inner = Witnessed::new(program, variant, function, site, args, false, false);
            function
                .ret
                .vars()
                .into_iter()
                .all(|generic| !removed[generic] || inner.generics[generic])
        }
    }
}

/// A union of two types, which is just the type if they're the same, and `T | nil` if one is
/// `nil`.
fn union_of(members: Vec<Ty>) -> Ty {
    match members.as_slice() {
        [first, second] if first == second => first.clone(),
        [inner, Ty::Nil] | [Ty::Nil, inner] => Ty::opt(inner.clone()),
        _ => Ty::Union(members),
    }
}

/// A binding with a generic function type, called with `1` for each parameter of each generic
/// function type, outermost first, such as `v0(1)(1)`.
fn applied(name: &str, ty: &Ty) -> Expr {
    let mut expr = Expr::new(ExprKind::Var(name.to_string()), ty.clone());
    let mut ty = ty.clone();
    while let Ty::GenFn(_, params, ret) = ty {
        let args = params
            .iter()
            .map(|_| Expr::new(ExprKind::Int(1), Ty::Int))
            .collect();
        expr = Expr::new(ExprKind::Apply(Box::new(expr), args), Ty::Any);
        ty = *ret;
    }
    expr
}

/// The variants to check: every removable generic argument removed, a random subset of them,
/// and a single site. Arguments that can't be removed at a site are kept, and the others are
/// replaced with `_`.
fn variants(rng: &mut Rng, program: &Program, v2: bool) -> Vec<(String, Variant)> {
    let removable = &removable_sites(program, &Variant::default());
    let sites: Vec<SiteId> = program
        .sites()
        .into_iter()
        .filter(|site| removable[site].iter().any(|&removable| removable))
        .collect();

    if sites.is_empty() {
        return Vec::new();
    }

    let strip = |site: SiteId| {
        let mask = &removable[&site];
        if mask.iter().all(|&removable| removable) {
            Strip::All
        } else {
            Strip::Placeholders(mask.clone())
        }
    };

    let mut variants: Vec<(String, Variant)> = Vec::new();
    let mut add = |name: &str, variant: Variant| {
        let variant = legalize(program, &variant);
        if !variant.stripped.is_empty() && variants.iter().all(|(_, other)| *other != variant) {
            variants.push((name.to_string(), variant));
        }
    };

    add(
        "all",
        Variant {
            stripped: sites.iter().map(|&site| (site, strip(site))).collect(),
        },
    );

    let mut some = Variant::default();
    for &site in &sites {
        if rng.chance(50) {
            let mut how = strip(site);
            // Sometimes write `_` for each argument instead of removing them
            if how == Strip::All && rng.chance(25) {
                how = Strip::Placeholders(vec![true; removable[&site].len()]);
            }
            some.stripped.insert(site, how);
        }
    }
    add("some", some);

    let site = sites[rng.below(sites.len())];
    add(
        "one",
        Variant {
            stripped: [(site, strip(site))].into(),
        },
    );

    // The sites whose generic arguments only the expected type can give, such as
    // `let x: Int | nil = none();`, with nothing else removed
    if v2 {
        let without_expected = removable_sites_with(program, &Variant::default(), false);
        let expected = Variant {
            stripped: sites
                .iter()
                .filter(|site| {
                    without_expected
                        .get(site)
                        .is_some_and(|mask| mask.iter().all(|&removable| !removable))
                })
                .map(|&site| (site, strip(site)))
                .collect(),
        };
        add("expected", expected);
    }

    variants
}
