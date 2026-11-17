//! The programs the fuzzer generates, and how they're printed as Rue source.
//!
//! Every place that takes generic arguments (a call, a reference to a generic function, or a
//! struct literal) is a [`Site`] with an id. The explicit program writes out every site's
//! arguments. A [`Variant`] says which sites have them removed, or replaced with `_`.

use std::cell::RefCell;
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
            _ => self.clone(),
        }
    }

    /// The generics `Var(i)` in the type.
    pub fn vars(&self) -> BTreeSet<usize> {
        let vars = RefCell::new(BTreeSet::new());
        self.map_vars(&|index| {
            vars.borrow_mut().insert(index);
            Ty::Var(index)
        });
        vars.into_inner()
    }

    pub fn contains_var(&self, index: usize) -> bool {
        match self {
            Ty::Var(other) => *other == index,
            Ty::Pair(first, rest) => first.contains_var(index) || rest.contains_var(index),
            Ty::Opt(inner) | Ty::List(inner) | Ty::Boxed(inner) => inner.contains_var(index),
            Ty::Fn(params, ret) => {
                params.iter().any(|param| param.contains_var(index)) || ret.contains_var(index)
            }
            _ => false,
        }
    }

    pub fn contains_boxed(&self) -> bool {
        match self {
            Ty::Boxed(_) => true,
            Ty::Pair(first, rest) => first.contains_boxed() || rest.contains_boxed(),
            Ty::Opt(inner) | Ty::List(inner) => inner.contains_boxed(),
            Ty::Fn(params, ret) => params.iter().any(Ty::contains_boxed) || ret.contains_boxed(),
            _ => false,
        }
    }

    pub fn contains_fn(&self) -> bool {
        match self {
            Ty::Fn(..) => true,
            Ty::Pair(first, rest) => first.contains_fn() || rest.contains_fn(),
            Ty::Opt(inner) | Ty::List(inner) | Ty::Boxed(inner) => inner.contains_fn(),
            _ => false,
        }
    }

    pub fn contains_lit(&self) -> bool {
        match self {
            Ty::Lit(_) => true,
            Ty::Pair(first, rest) => first.contains_lit() || rest.contains_lit(),
            Ty::Opt(inner) | Ty::List(inner) | Ty::Boxed(inner) => inner.contains_lit(),
            Ty::Fn(params, ret) => params.iter().any(Ty::contains_lit) || ret.contains_lit(),
            _ => false,
        }
    }

    pub fn contains_union(&self) -> bool {
        match self {
            Ty::Opt(_) | Ty::List(_) => true,
            Ty::Pair(first, rest) => first.contains_union() || rest.contains_union(),
            Ty::Boxed(inner) => inner.contains_union(),
            Ty::Fn(params, ret) => params.iter().any(Ty::contains_union) || ret.contains_union(),
            _ => false,
        }
    }

    /// The type as Rue source, naming the generics of the function it belongs to.
    pub fn print(&self, generics: &[String]) -> String {
        let mut out = String::new();
        self.write(&mut out, generics, false);
        out
    }

    /// Writes the type, with parentheses around a union where it would otherwise bind wrongly
    /// (as a function's return type, inside another union, or as a list's element type).
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
            Ty::Fn(params, ret) => {
                if in_union {
                    out.push('(');
                }
                out.push_str("fn(");
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
            ExprKind::BoxLit { site, .. } => Some(*site),
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

impl Function {
    /// The function's type, with its generics as `Ty::Var`.
    pub fn ty(&self) -> Ty {
        Ty::func(
            self.params.iter().map(|(_, ty)| ty.clone()).collect(),
            self.ret.clone(),
        )
    }

    pub fn exprs(&self) -> Vec<&Expr> {
        let mut exprs = Vec::new();
        if let Some(recursion) = &self.recursion {
            exprs.extend(&recursion.inner);
            exprs.push(&recursion.returned);
        }
        exprs.extend(self.lets.iter().map(|binding| &binding.value));
        exprs.push(&self.body);
        exprs
    }

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

impl Program {
    pub fn functions_and_main(&self) -> impl Iterator<Item = &Function> {
        self.functions.iter().chain([&self.main])
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

    fn uses_box(&self) -> bool {
        self.functions_and_main().any(|function| {
            function.params.iter().any(|(_, ty)| ty.contains_boxed())
                || function.ret.contains_boxed()
                || function.lets.iter().any(|binding| {
                    binding
                        .annotation
                        .as_ref()
                        .is_some_and(Ty::contains_boxed)
                })
        }) || self.exprs().iter().any(|expr| {
            expr.ty.contains_boxed()
                || matches!(&expr.kind, ExprKind::Call { generic_args, .. } | ExprKind::FnRef { generic_args, .. } if generic_args.iter().any(Ty::contains_boxed))
        })
    }

    /// The program as Rue source, with the generic arguments that the variant strips removed.
    pub fn print(&self, variant: &Variant) -> String {
        let mut out = String::new();

        if self.uses_box() {
            out.push_str("struct Box<T> {\n    value: T,\n}\n\n");
        }

        for function in self.functions_and_main() {
            print_function(&mut out, function, variant);
            out.push('\n');
        }

        out.pop();
        out
    }
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
