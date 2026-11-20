//! The programs the fuzzer generates, and how they're printed as Rue source.
//!
//! Every place that takes generic arguments (a call, a reference to a generic function, or a
//! struct literal) is a [`Site`] with an id. The explicit program writes out every site's
//! arguments. A [`Variant`] says which sites have them removed, or replaced with `_`.

use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Write;

/// A type, as the generator sees it.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Ty {
    Int,
    Bytes,
    Bool,
    Nil,
    /// A literal integer type, such as `5`.
    Lit(i64),
    Pair(Box<Ty>, Box<Ty>),
    /// `T | nil`.
    Opt(Box<Ty>),
    List(Box<Ty>),
    /// The generic struct `Box<T>`, with a single field `value: T`.
    Boxed(Box<Ty>),
    /// A function type, such as `fn(a: Int) -> Int`.
    Fn(Vec<Ty>, Box<Ty>),
    /// The generic with this index of the function whose signature contains it.
    Var(usize),
    /// A generic that's in scope, by name, such as the enclosing function's `A`.
    Rigid(String),
    /// `Any`, which `main` returns, so that the types of its unannotated bindings don't matter.
    Any,
    /// A union of two or more types other than `T | nil`, such as `5 | 7` or
    /// `(fn(a: Int) -> T) | (fn(a: Bytes) -> T)` (generator version 2).
    Union(Vec<Ty>),
    /// A generic alias or struct the program declares (generator version 2).
    Named(Named, Vec<Ty>),
    /// A generic function type, such as `fn<T>(a: T) -> (T, T)`, whose generics are `Rigid`
    /// inside it. Only bindings' annotations and functions' return types have one (generator
    /// version 2).
    GenFn(Vec<String>, Vec<Ty>, Box<Ty>),
}

/// The generic aliases and structs that generator version 2 declares when a program uses them.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Named {
    /// `type Maybe<T> = T | nil;`
    Maybe,
    /// `type Tree<T> = nil | (T, (Tree<T>, Tree<T>));`, a recursive alias.
    Tree,
    /// `struct Entry<K, V> { key: K, value: V }`
    Entry,
}

impl Named {
    pub const ALL: [Named; 3] = [Named::Maybe, Named::Tree, Named::Entry];

    pub fn name(self) -> &'static str {
        match self {
            Named::Maybe => "Maybe",
            Named::Tree => "Tree",
            Named::Entry => "Entry",
        }
    }

    pub fn arity(self) -> usize {
        match self {
            Named::Maybe | Named::Tree => 1,
            Named::Entry => 2,
        }
    }

    fn declaration(self) -> &'static str {
        match self {
            Named::Maybe => "type Maybe<T> = T | nil;\n",
            Named::Tree => "type Tree<T> = nil | (T, (Tree<T>, Tree<T>));\n",
            Named::Entry => "struct Entry<K, V> {\n    key: K,\n    value: V,\n}\n",
        }
    }
}

impl Ty {
    pub fn pair(first: Ty, rest: Ty) -> Ty {
        Ty::Pair(Box::new(first), Box::new(rest))
    }

    pub fn opt(inner: Ty) -> Ty {
        Ty::Opt(Box::new(inner))
    }

    pub fn list(inner: Ty) -> Ty {
        Ty::List(Box::new(inner))
    }

    pub fn boxed(inner: Ty) -> Ty {
        Ty::Boxed(Box::new(inner))
    }

    pub fn func(params: Vec<Ty>, ret: Ty) -> Ty {
        Ty::Fn(params, Box::new(ret))
    }

    /// Replaces each `Var(i)` with `args[i]`.
    pub fn subst(&self, args: &[Ty]) -> Ty {
        self.map_vars(&|index| args[index].clone())
    }

    /// Replaces each `Var(i)` with the rigid generic `names[i]`, which is how a function's own
    /// generics look inside its body.
    pub fn rigid(&self, names: &[String]) -> Ty {
        self.map_vars(&|index| Ty::Rigid(names[index].clone()))
    }

    fn map_vars(&self, f: &impl Fn(usize) -> Ty) -> Ty {
        match self {
            Ty::Var(index) => f(*index),
            Ty::Pair(first, rest) => Ty::pair(first.map_vars(f), rest.map_vars(f)),
            Ty::Opt(inner) => Ty::opt(inner.map_vars(f)),
            Ty::List(inner) => Ty::list(inner.map_vars(f)),
            Ty::Boxed(inner) => Ty::boxed(inner.map_vars(f)),
            Ty::Fn(params, ret) => Ty::func(
                params.iter().map(|param| param.map_vars(f)).collect(),
                ret.map_vars(f),
            ),
            Ty::Union(members) => {
                Ty::Union(members.iter().map(|member| member.map_vars(f)).collect())
            }
            Ty::Named(named, args) => {
                Ty::Named(*named, args.iter().map(|arg| arg.map_vars(f)).collect())
            }
            Ty::GenFn(generics, params, ret) => Ty::GenFn(
                generics.clone(),
                params.iter().map(|param| param.map_vars(f)).collect(),
                Box::new(ret.map_vars(f)),
            ),
            _ => self.clone(),
        }
    }

    /// The direct parts of the type.
    fn children(&self) -> Vec<&Ty> {
        match self {
            Ty::Pair(first, rest) => vec![first, rest],
            Ty::Opt(inner) | Ty::List(inner) | Ty::Boxed(inner) => vec![inner],
            Ty::Fn(params, ret) | Ty::GenFn(_, params, ret) => {
                params.iter().chain([&**ret]).collect()
            }
            Ty::Union(members) | Ty::Named(_, members) => members.iter().collect(),
            _ => vec![],
        }
    }

    /// Whether `f` holds for the type or any type inside of it.
    pub fn any(&self, f: &impl Fn(&Ty) -> bool) -> bool {
        f(self) || self.children().into_iter().any(|child| child.any(f))
    }

    /// The generics `Var(i)` in the type.
    pub fn vars(&self) -> BTreeSet<usize> {
        let mut vars = BTreeSet::new();
        let mut stack = vec![self];
        while let Some(ty) = stack.pop() {
            if let Ty::Var(index) = ty {
                vars.insert(*index);
            }
            stack.extend(ty.children());
        }
        vars
    }

    pub fn contains_var(&self, index: usize) -> bool {
        self.any(&|ty| *ty == Ty::Var(index))
    }

    pub fn contains_fn(&self) -> bool {
        self.any(&|ty| matches!(ty, Ty::Fn(..)))
    }

    pub fn contains_lit(&self) -> bool {
        self.any(&|ty| matches!(ty, Ty::Lit(_)))
    }

    pub fn contains_union(&self) -> bool {
        self.any(&|ty| {
            matches!(
                ty,
                Ty::Opt(_) | Ty::List(_) | Ty::Union(_) | Ty::Named(Named::Maybe | Named::Tree, _)
            )
        })
    }

    /// Whether this is a union with a function type in it, such as `F | nil` or `List<F>`,
    /// which doesn't give a lambda the types of its parameters. Removability counts such a
    /// position as having no expected type, which is version 1's rule, kept so that its variants
    /// don't change.
    pub fn is_function_union(&self) -> bool {
        matches!(self, Ty::Opt(inner) | Ty::List(inner) if matches!(**inner, Ty::Fn(..)))
    }

    /// Whether a lambda anywhere in a value of this type gets no parameter types from it, so
    /// the generator writes their types out. Besides [`is_function_union`](Self::is_function_union),
    /// generator version 2's unions give a lambda no parameter types anywhere inside them, nor
    /// does a `Maybe` or `Tree` of a type with a function in it: `Box<F> | (F, F)` doesn't give
    /// a lambda in the pair a type. The position still has an expected type for anything else.
    pub fn gives_lambdas_no_types(&self) -> bool {
        match self {
            Ty::Union(_) => true,
            Ty::Named(Named::Maybe | Named::Tree, args) => args[0].contains_fn(),
            _ => self.is_function_union(),
        }
    }

    /// The type as Rue source, naming the generics of the function it belongs to.
    pub fn print(&self, generics: &[String]) -> String {
        let mut out = String::new();
        self.write(&mut out, generics, false);
        out
    }

    /// Writes the type. `in_union` is true where a union or a function type needs parentheses to
    /// bind as intended: as a function's return type, or as a member of another union.
    fn write(&self, out: &mut String, generics: &[String], in_union: bool) {
        match self {
            Ty::Int => out.push_str("Int"),
            Ty::Bytes => out.push_str("Bytes"),
            Ty::Bool => out.push_str("Bool"),
            Ty::Nil => out.push_str("nil"),
            Ty::Lit(value) => write!(out, "{value}").unwrap(),
            Ty::Pair(first, rest) => {
                out.push('(');
                first.write(out, generics, false);
                out.push_str(", ");
                rest.write(out, generics, false);
                out.push(')');
            }
            Ty::Opt(inner) => {
                if in_union {
                    out.push('(');
                }
                inner.write(out, generics, true);
                out.push_str(" | nil");
                if in_union {
                    out.push(')');
                }
            }
            Ty::List(inner) => {
                out.push_str("List<");
                inner.write(out, generics, false);
                out.push('>');
            }
            Ty::Boxed(inner) => {
                out.push_str("Box<");
                inner.write(out, generics, false);
                out.push('>');
            }
            Ty::Fn(params, ret) | Ty::GenFn(_, params, ret) => {
                if in_union {
                    out.push('(');
                }
                match self {
                    Ty::GenFn(names, ..) => write!(out, "fn<{}>(", names.join(", ")).unwrap(),
                    _ => out.push_str("fn("),
                }
                for (index, param) in params.iter().enumerate() {
                    if index > 0 {
                        out.push_str(", ");
                    }
                    write!(out, "{}: ", param_name(index)).unwrap();
                    param.write(out, generics, false);
                }
                out.push_str(") -> ");
                ret.write(out, generics, true);
                if in_union {
                    out.push(')');
                }
            }
            Ty::Any => out.push_str("Any"),
            Ty::Union(members) => {
                if in_union {
                    out.push('(');
                }
                for (index, member) in members.iter().enumerate() {
                    if index > 0 {
                        out.push_str(" | ");
                    }
                    member.write(out, generics, true);
                }
                if in_union {
                    out.push(')');
                }
            }
            Ty::Named(named, args) => {
                out.push_str(named.name());
                out.push('<');
                for (index, arg) in args.iter().enumerate() {
                    if index > 0 {
                        out.push_str(", ");
                    }
                    arg.write(out, generics, false);
                }
                out.push('>');
            }
            Ty::Var(index) => out.push_str(&generics[*index]),
            Ty::Rigid(name) => out.push_str(name),
        }
    }
}

/// The name of a parameter in a function type, which doesn't affect assignability.
fn param_name(index: usize) -> char {
    (b'a' + index as u8) as char
}

/// A place that takes generic arguments.
pub type SiteId = usize;

/// One generic lambda of a `Wrapped` call: its generics, and its parameters with their types.
pub type Layer = (Vec<String>, Vec<(String, Ty)>);

/// An expression, along with the type it has in the explicit program.
#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub ty: Ty,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    /// A parameter or binding.
    Var(String),
    /// `depth - 1`, for the recursion guard.
    Decrement(String),
    Int(i64),
    Bytes,
    Bool,
    Nil,
    Pair(Box<Expr>, Box<Expr>),
    List(Vec<Expr>),
    First(Box<Expr>),
    Rest(Box<Expr>),
    Field(Box<Expr>),
    /// `Box::<T> { value: ... }`.
    BoxLit {
        site: SiteId,
        arg: Ty,
        value: Box<Expr>,
    },
    /// A call to a generic or non-generic function declared in the program.
    Call {
        site: Option<SiteId>,
        function: String,
        generic_args: Vec<Ty>,
        args: Vec<Expr>,
    },
    /// A call to a parameter or binding with a function type.
    CallLocal {
        name: String,
        args: Vec<Expr>,
    },
    /// A reference to a declared function, such as `duplicate::<Int>`.
    FnRef {
        site: Option<SiteId>,
        function: String,
        generic_args: Vec<Ty>,
    },
    /// A lambda, with each parameter's type written out or left to the expected type.
    Lambda {
        params: Vec<(String, Ty)>,
        typed: bool,
        body: Box<Expr>,
    },
    /// The generic lambda `fn<V>(value: V) => value`.
    IdentityLambda,
    /// `unchecked_cast::<T>(nil)`, for a value of a type nothing else can produce, such as the
    /// result of a function whose generic is only in its return type.
    Unchecked,
    /// `Entry::<K, V> { key: ..., value: ... }` (generator version 2).
    EntryLit {
        site: SiteId,
        args: Vec<Ty>,
        key: Box<Expr>,
        value: Box<Expr>,
    },
    /// `entry.key` (generator version 2).
    Key(Box<Expr>),
    /// A call of an expression's value, such as `function(1)(2)` (generator version 2).
    Apply(Box<Expr>, Vec<Expr>),
    /// A call whose type is a generic function type that's expected of it, such as `make()` for a
    /// `fn<T>(a: T) -> (T, T)`. No explicit generic arguments can produce a generic function
    /// type, so the explicit program wraps the call in a generic lambda for each generic
    /// function type, `fn<T>(w1: T) => make::<T, (T, T)>()(w1)`, and the site's variant is the
    /// bare call `make()`. The call itself has no site (generator version 2).
    Wrapped {
        site: SiteId,
        /// The generics and parameters of each lambda, outermost first.
        layers: Vec<Layer>,
        call: Box<Expr>,
    },
}

impl Expr {
    pub fn new(kind: ExprKind, ty: Ty) -> Self {
        Self { kind, ty }
    }

    /// The direct subexpressions, in order.
    pub fn children(&self) -> Vec<&Expr> {
        match &self.kind {
            ExprKind::Pair(first, rest) => vec![first, rest],
            ExprKind::List(items) => items.iter().collect(),
            ExprKind::First(inner) | ExprKind::Rest(inner) | ExprKind::Field(inner) => {
                vec![inner]
            }
            ExprKind::BoxLit { value, .. } => vec![value],
            ExprKind::Call { args, .. } | ExprKind::CallLocal { args, .. } => args.iter().collect(),
            ExprKind::Lambda { body, .. } => vec![body],
            ExprKind::EntryLit { key, value, .. } => vec![key, value],
            ExprKind::Key(inner) => vec![inner],
            ExprKind::Apply(function, args) => [&**function].into_iter().chain(args).collect(),
            ExprKind::Wrapped { call, .. } => vec![call],
            _ => vec![],
        }
    }

    pub fn children_mut(&mut self) -> Vec<&mut Expr> {
        match &mut self.kind {
            ExprKind::Pair(first, rest) => vec![first, rest],
            ExprKind::List(items) => items.iter_mut().collect(),
            ExprKind::First(inner) | ExprKind::Rest(inner) | ExprKind::Field(inner) => {
                vec![inner]
            }
            ExprKind::BoxLit { value, .. } => vec![value],
            ExprKind::Call { args, .. } | ExprKind::CallLocal { args, .. } => {
                args.iter_mut().collect()
            }
            ExprKind::Lambda { body, .. } => vec![body],
            ExprKind::EntryLit { key, value, .. } => vec![key, value],
            ExprKind::Key(inner) => vec![inner],
            ExprKind::Apply(function, args) => {
                let mut children: Vec<&mut Expr> = vec![function];
                children.extend(args.iter_mut());
                children
            }
            ExprKind::Wrapped { call, .. } => vec![call],
            _ => vec![],
        }
    }

    /// Calls `f` with this expression and each one inside of it, in preorder.
    pub fn walk<'a>(&'a self, f: &mut impl FnMut(&'a Expr)) {
        f(self);
        for child in self.children() {
            child.walk(f);
        }
    }

    pub fn site(&self) -> Option<SiteId> {
        match &self.kind {
            ExprKind::BoxLit { site, .. }
            | ExprKind::EntryLit { site, .. }
            | ExprKind::Wrapped { site, .. } => Some(*site),
            ExprKind::Call { site, .. } | ExprKind::FnRef { site, .. } => *site,
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Let {
    pub name: String,
    pub annotation: Option<Ty>,
    pub value: Expr,
}

/// The recursion guard at the start of a recursive function's body:
///
/// ```rue
/// if depth > 0 {
///     let _inner = f::<Int>(..., depth - 1);
///     return f::<A>(..., depth - 1);
/// }
/// ```
#[derive(Debug, Clone)]
pub struct Recursion {
    /// A recursive call with other generic arguments, whose result is unused.
    pub inner: Option<Expr>,
    /// A recursive call with the function's own generics, whose result is returned.
    pub returned: Expr,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub generics: Vec<String>,
    /// The parameters, with types that reference the generics as `Ty::Var`.
    pub params: Vec<(String, Ty)>,
    pub ret: Ty,
    pub recursion: Option<Recursion>,
    pub lets: Vec<Let>,
    pub body: Expr,
}

/// Where a function's top-level expression is, which says whether it has an expected type.
#[derive(Debug, Clone, Copy)]
pub enum Root<'a> {
    /// `let _inner = ...;` in the recursion guard, which has no expected type.
    GuardLet,
    /// `return ...;` in the recursion guard.
    GuardReturn,
    /// A binding, with its annotation if it has one.
    Let(Option<&'a Ty>),
    /// The function's result.
    Return,
}

impl Root<'_> {
    pub fn has_expected_type(self) -> bool {
        match self {
            Root::GuardLet => false,
            Root::GuardReturn | Root::Return => true,
            Root::Let(annotation) => annotation.is_some(),
        }
    }
}

impl Function {
    /// The function's top-level expressions, with where each one is, in the order they're
    /// printed.
    pub fn roots(&self) -> Vec<(&Expr, Root<'_>)> {
        let mut roots = Vec::new();
        if let Some(recursion) = &self.recursion {
            if let Some(inner) = &recursion.inner {
                roots.push((inner, Root::GuardLet));
            }
            roots.push((&recursion.returned, Root::GuardReturn));
        }
        for binding in &self.lets {
            roots.push((&binding.value, Root::Let(binding.annotation.as_ref())));
        }
        roots.push((&self.body, Root::Return));
        roots
    }

    /// The function's type, with its generics as `Ty::Var`.
    pub fn ty(&self) -> Ty {
        Ty::func(
            self.params.iter().map(|(_, ty)| ty.clone()).collect(),
            self.ret.clone(),
        )
    }

    pub fn exprs(&self) -> Vec<&Expr> {
        self.roots().into_iter().map(|(expr, _)| expr).collect()
    }

    /// The same expressions as [`exprs`](Self::exprs), in the same order.
    pub fn exprs_mut(&mut self) -> Vec<&mut Expr> {
        let mut exprs = Vec::new();
        if let Some(recursion) = &mut self.recursion {
            exprs.extend(&mut recursion.inner);
            exprs.push(&mut recursion.returned);
        }
        exprs.extend(self.lets.iter_mut().map(|binding| &mut binding.value));
        exprs.push(&mut self.body);
        exprs
    }
}

#[derive(Debug, Clone)]
pub struct Program {
    /// The functions other than `main`, in the order they're declared.
    pub functions: Vec<Function>,
    pub main: Function,
    /// The sites of generator version 3's probe calls, whose generics nothing gives a type (see
    /// `Generator::probe`).
    pub probes: BTreeSet<SiteId>,
}

/// How a site's generic arguments are written in a variant.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Strip {
    /// Removed entirely, as in `map(duplicate, 42)`.
    All,
    /// The arguments marked `true` are replaced with `_`, as in `map::<_, Int>(...)`.
    Placeholders(Vec<bool>),
}

/// A program with some of its generic arguments removed.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Variant {
    pub stripped: BTreeMap<SiteId, Strip>,
}

impl Variant {
    /// Whether the expression is a site whose generic arguments this variant removes.
    pub fn strips(&self, expr: &Expr) -> bool {
        expr.site()
            .is_some_and(|site| self.stripped.contains_key(&site))
    }
}

impl Program {
    pub fn functions_and_main(&self) -> impl Iterator<Item = &Function> {
        self.functions.iter().chain([&self.main])
    }

    /// The functions that the probe calls whose generic arguments the variant removes call (see
    /// `Generator::probe`).
    pub fn probe_callees(&self, variant: &Variant) -> BTreeSet<&str> {
        let mut callees = BTreeSet::new();
        for expr in self.exprs() {
            expr.walk(&mut |expr| {
                if let ExprKind::Call {
                    site: Some(site),
                    function,
                    ..
                } = &expr.kind
                    && self.probes.contains(site)
                    && variant.stripped.contains_key(site)
                {
                    callees.insert(function.as_str());
                }
            });
        }
        callees
    }

    pub fn function(&self, name: &str) -> Option<&Function> {
        self.functions.iter().find(|function| function.name == name)
    }

    pub fn exprs(&self) -> Vec<&Expr> {
        let mut exprs = Vec::new();
        for function in self.functions_and_main() {
            for expr in function.exprs() {
                expr.walk(&mut |expr| exprs.push(expr));
            }
        }
        exprs
    }

    /// The sites in the program, in the order they're printed.
    pub fn sites(&self) -> Vec<SiteId> {
        self.exprs().into_iter().filter_map(Expr::site).collect()
    }

    /// Whether any type in the program, including the generic arguments it writes out, matches.
    fn uses(&self, matches: &impl Fn(&Ty) -> bool) -> bool {
        let any = |ty: &Ty| ty.any(matches);
        self.functions_and_main().any(|function| {
            function.params.iter().any(|(_, ty)| any(ty))
                || any(&function.ret)
                || function
                    .lets
                    .iter()
                    .any(|binding| binding.annotation.as_ref().is_some_and(any))
        }) || self.exprs().iter().any(|expr| {
            any(&expr.ty)
                || match &expr.kind {
                    ExprKind::Call { generic_args, .. } | ExprKind::FnRef { generic_args, .. } => {
                        generic_args.iter().any(any)
                    }
                    ExprKind::EntryLit { args, .. } => args.iter().any(any),
                    ExprKind::BoxLit { arg, .. } => any(arg),
                    ExprKind::Wrapped { layers, .. } => layers
                        .iter()
                        .any(|(_, params)| params.iter().any(|(_, ty)| any(ty))),
                    _ => false,
                }
        })
    }

    /// Whether the program calls or references a function from the standard library, so it must
    /// be compiled with it.
    pub fn uses_std(&self) -> bool {
        self.exprs().iter().any(|expr| {
            matches!(&expr.kind, ExprKind::Call { function, .. } | ExprKind::FnRef { function, .. }
                if is_std(function))
        })
    }

    /// The program as Rue source, with the generic arguments that the variant strips removed.
    pub fn print(&self, variant: &Variant) -> String {
        let mut out = String::new();

        if self.uses(&|ty| matches!(ty, Ty::Boxed(_))) {
            out.push_str("struct Box<T> {\n    value: T,\n}\n\n");
        }

        for named in Named::ALL {
            if self.uses(&|ty| matches!(ty, Ty::Named(other, _) if *other == named)) {
                out.push_str(named.declaration());
                out.push('\n');
            }
        }

        // A function from the standard library is in the program only so that calls know its
        // signature
        for function in self.functions_and_main() {
            if is_std(&function.name) {
                continue;
            }
            print_function(&mut out, function, variant);
            out.push('\n');
        }

        out.pop();
        out
    }
}

/// The standard library's generic functions that generator version 2 calls.
pub const STD_FUNCTIONS: &[&str] = &["merge_list"];

pub fn is_std(name: &str) -> bool {
    STD_FUNCTIONS.contains(&name)
}

fn print_function(out: &mut String, function: &Function, variant: &Variant) {
    let generics = &function.generics;
    write!(out, "fn {}", function.name).unwrap();

    if !generics.is_empty() {
        write!(out, "<{}>", generics.join(", ")).unwrap();
    }

    out.push('(');
    for (index, (name, ty)) in function.params.iter().enumerate() {
        if index > 0 {
            out.push_str(", ");
        }
        write!(out, "{name}: {}", ty.print(generics)).unwrap();
    }
    writeln!(out, ") -> {} {{", function.ret.print(generics)).unwrap();

    let printer = Printer { generics, variant };

    if let Some(recursion) = &function.recursion {
        out.push_str("    if depth > 0 {\n");
        if let Some(inner) = &recursion.inner {
            writeln!(out, "        let _inner = {};", printer.expr(inner)).unwrap();
        }
        writeln!(out, "        return {};", printer.expr(&recursion.returned)).unwrap();
        out.push_str("    }\n\n");
    }

    for binding in &function.lets {
        write!(out, "    let {}", binding.name).unwrap();
        if let Some(annotation) = &binding.annotation {
            write!(out, ": {}", annotation.print(generics)).unwrap();
        }
        writeln!(out, " = {};", printer.expr(&binding.value)).unwrap();
    }

    if !function.lets.is_empty() {
        out.push('\n');
    }

    writeln!(out, "    {}", printer.expr(&function.body)).unwrap();
    out.push_str("}\n");
}

struct Printer<'a> {
    /// The generics of the function the expression is in.
    generics: &'a [String],
    variant: &'a Variant,
}

impl Printer<'_> {
    fn expr(&self, expr: &Expr) -> String {
        match &expr.kind {
            ExprKind::Var(name) => name.clone(),
            ExprKind::Decrement(name) => format!("{name} - 1"),
            ExprKind::Int(value) => value.to_string(),
            ExprKind::Bytes => "\"a\"".to_string(),
            ExprKind::Bool => "true".to_string(),
            ExprKind::Nil => "nil".to_string(),
            ExprKind::Pair(first, rest) => format!("({}, {})", self.expr(first), self.expr(rest)),
            ExprKind::List(items) => format!(
                "[{}]",
                items
                    .iter()
                    .map(|item| self.expr(item))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            ExprKind::First(inner) => format!("{}.first", self.operand(inner)),
            ExprKind::Rest(inner) => format!("{}.rest", self.operand(inner)),
            ExprKind::Field(inner) => format!("{}.value", self.operand(inner)),
            ExprKind::BoxLit { site, arg, value } => format!(
                "Box{} {{ value: {} }}",
                self.generic_args(Some(*site), std::slice::from_ref(arg)),
                self.expr(value)
            ),
            ExprKind::Call {
                site,
                function,
                generic_args,
                args,
            } => format!(
                "{function}{}({})",
                self.generic_args(*site, generic_args),
                self.args(args)
            ),
            ExprKind::CallLocal { name, args } => format!("{name}({})", self.args(args)),
            ExprKind::FnRef {
                site,
                function,
                generic_args,
            } => format!("{function}{}", self.generic_args(*site, generic_args)),
            ExprKind::Lambda {
                params,
                typed,
                body,
            } => {
                let params = params
                    .iter()
                    .map(|(name, ty)| {
                        if *typed {
                            format!("{name}: {}", ty.print(self.generics))
                        } else {
                            name.clone()
                        }
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("fn({params}) => {}", self.expr(body))
            }
            ExprKind::IdentityLambda => "fn<V>(value: V) => value".to_string(),
            ExprKind::Unchecked => {
                format!("unchecked_cast::<{}>(nil)", expr.ty.print(self.generics))
            }
            ExprKind::EntryLit {
                site,
                args,
                key,
                value,
            } => format!(
                "Entry{} {{ key: {}, value: {} }}",
                self.generic_args(Some(*site), args),
                self.expr(key),
                self.expr(value)
            ),
            ExprKind::Key(inner) => format!("{}.key", self.operand(inner)),
            ExprKind::Apply(function, args) => {
                format!("{}({})", self.operand(function), self.args(args))
            }
            ExprKind::Wrapped { site, layers, call } => {
                if self.variant.stripped.contains_key(site) {
                    // Shrinking can replace the call with something simpler
                    return match &call.kind {
                        ExprKind::Call { function, args, .. } => {
                            format!("{function}({})", self.args(args))
                        }
                        _ => self.expr(call),
                    };
                }
                let mut text = String::new();
                for (generics, params) in layers {
                    let params: Vec<String> = params
                        .iter()
                        .map(|(name, ty)| format!("{name}: {}", ty.print(self.generics)))
                        .collect();
                    write!(
                        text,
                        "fn<{}>({}) => ",
                        generics.join(", "),
                        params.join(", ")
                    )
                    .unwrap();
                }
                text.push_str(&self.expr(call));
                for (_, params) in layers {
                    let names: Vec<&str> = params.iter().map(|(name, _)| name.as_str()).collect();
                    write!(text, "({})", names.join(", ")).unwrap();
                }
                text
            }
        }
    }

    /// An expression that a field access is applied to, in parentheses unless it's a simple one.
    fn operand(&self, expr: &Expr) -> String {
        let text = self.expr(expr);
        match &expr.kind {
            ExprKind::Var(_)
            | ExprKind::Call { .. }
            | ExprKind::CallLocal { .. }
            | ExprKind::First(_)
            | ExprKind::Rest(_)
            | ExprKind::Field(_)
            | ExprKind::Key(_)
            | ExprKind::Apply(..)
            | ExprKind::Pair(..) => text,
            _ => format!("({text})"),
        }
    }

    fn args(&self, args: &[Expr]) -> String {
        args.iter()
            .map(|arg| self.expr(arg))
            .collect::<Vec<_>>()
            .join(", ")
    }

    fn generic_args(&self, site: Option<SiteId>, args: &[Ty]) -> String {
        if args.is_empty() {
            return String::new();
        }

        let strip = site.and_then(|site| self.variant.stripped.get(&site));

        let printed: Vec<String> = match strip {
            Some(Strip::All) => return String::new(),
            Some(Strip::Placeholders(mask)) => args
                .iter()
                .zip(mask)
                .map(|(arg, &placeholder)| {
                    if placeholder {
                        "_".to_string()
                    } else {
                        arg.print(self.generics)
                    }
                })
                .collect(),
            None => args.iter().map(|arg| arg.print(self.generics)).collect(),
        };

        format!("::<{}>", printed.join(", "))
    }
}
