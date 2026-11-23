//! Decompiles a program into the [IR](crate::ir), by evaluating its code symbolically: the
//! environment of each function (and each inline body) is a [`Value`] whose structure is known
//! as far as the code that builds it shows, so that reading a path from it can be resolved to a
//! name, a constant, or a value computed earlier.
//!
//! Functions that the debug metadata describes are decompiled once each, and each call to one is
//! shown as a call. The parts of a function's environment that the metadata doesn't name (such as
//! captured functions and constants) are hidden, and when every call passes the same constant for
//! one, the body uses that constant. That's only sound if every call is known, so it's found by
//! iterating to a fixed point, and a function whose code is used in any other way (passed as a
//! value to code that isn't decompiled, stored in data, or run with an environment that's only
//! known at runtime) keeps its hidden values unknown.

use std::collections::{HashMap, HashSet};
use std::mem;

use rue_clvm::operator_name;

use crate::ir::{
    Block, Expr, ExprKind, Function, FunctionId, FunctionKind, KnownSlot, Root, Statement, Value,
    VarId,
};
use crate::program::{
    APPLY, CONS, IF, Layout, NodeId, NodeKind, Path, Program, QUOTE, RAISE, WHOLE_ENV, atom_path,
};
use crate::stack::{Deep, grow_stack};

/// How many times the analysis is repeated before it gives up on finding hidden values.
const MAX_PASSES: usize = 32;

/// How large a structure that's read from an environment can be before it's bound to a variable,
/// which keeps a structure that's built from copies of itself from being shown (and walked) in
/// full, which would take exponential time.
const MAX_STRUCTURE: usize = 64;

/// What's known about a hidden part of a function's environment, from every call to it.
#[derive(Debug, Clone, Copy)]
enum Fact {
    /// No call has been seen.
    Unset,
    Const(NodeId),
    Unknown,
}

/// A function that's known before the analysis.
#[derive(Debug, Clone)]
struct FunctionInfo {
    name: String,
    node: NodeId,
    kind: FunctionKind,
    layout: Option<Layout>,
}

impl FunctionInfo {
    fn hidden(&self) -> &[Path] {
        self.layout
            .as_ref()
            .map_or(&[], |layout| layout.hidden.as_slice())
    }
}

/// The result of decompiling a program.
#[derive(Debug)]
pub(crate) struct Built {
    pub functions: Vec<Function>,
    pub entry: FunctionId,
    /// The function that each body of a function is, including copies of it.
    pub function_of: HashMap<NodeId, FunctionId>,
    /// Whether each node of the program is part of the decompiled code.
    pub covered: Vec<bool>,
    /// Whether the hidden values had to be given up on, since the analysis didn't settle.
    pub gave_up: bool,
}

pub(crate) fn build(program: &Program) -> Built {
    let (infos, function_of, entry) = described_functions(program);

    let mut facts = per_hidden(&infos, Fact::Unset);
    let mut reads = per_hidden(&infos, false);

    for _ in 0..MAX_PASSES {
        let mut pass = Pass::run(program, &infos, &function_of, &facts, &reads, entry);

        let mut new_facts = mem::take(&mut pass.observed);
        for (id, escaped) in pass.escaped.iter().enumerate() {
            if *escaped {
                new_facts[id].fill(Fact::Unknown);
            }
        }

        let mut new_reads = reads.clone();
        for (all, new) in new_reads.iter_mut().zip(&pass.reads) {
            for (all, new) in all.iter_mut().zip(new) {
                *all |= *new;
            }
        }

        if same_facts(program, &facts, &new_facts) && new_reads == reads {
            return pass.finish(entry, false);
        }

        facts = new_facts;
        reads = new_reads;
    }

    // Without hidden values, nothing depends on how functions are called
    let facts = per_hidden(&infos, Fact::Unknown);
    let reads = per_hidden(&infos, true);
    Pass::run(program, &infos, &function_of, &facts, &reads, entry).finish(entry, true)
}

/// A value for each hidden part of each function's environment.
fn per_hidden<T: Clone>(infos: &[FunctionInfo], value: T) -> Vec<Vec<T>> {
    infos
        .iter()
        .map(|info| vec![value.clone(); info.hidden().len()])
        .collect()
}

fn same_facts(program: &Program, a: &[Vec<Fact>], b: &[Vec<Fact>]) -> bool {
    a.iter().zip(b).all(|(a, b)| {
        a.iter().zip(b).all(|(a, b)| match (a, b) {
            (Fact::Unset, Fact::Unset) | (Fact::Unknown, Fact::Unknown) => true,
            (Fact::Const(a), Fact::Const(b)) => program.same_value(*a, *b),
            _ => false,
        })
    })
}

fn join(program: &Program, fact: &mut Fact, value: Option<NodeId>) {
    *fact = match (*fact, value) {
        (Fact::Unknown, _) | (_, None) => Fact::Unknown,
        (Fact::Unset, Some(value)) => Fact::Const(value),
        (Fact::Const(known), Some(value)) if program.same_value(known, value) => Fact::Const(known),
        (Fact::Const(_), Some(_)) => Fact::Unknown,
    };
}

/// The functions that the debug metadata describes, the function each annotated node is the body
/// of (the same function for each copy of a body), and the function the whole program is (which
/// is added if the program isn't the body of one).
fn described_functions(
    program: &Program,
) -> (Vec<FunctionInfo>, HashMap<NodeId, FunctionId>, FunctionId) {
    let mut infos = Vec::<FunctionInfo>::new();
    let mut function_of = HashMap::new();
    let mut canonical = HashMap::<(usize, [u8; 32]), FunctionId>::new();
    let mut names = HashMap::<String, usize>::new();

    // In order of position, so that names are numbered the way they're read
    for node in program.subtree(program.root) {
        let Some(index) = program.nodes[node].function else {
            continue;
        };

        if program.pair(node).is_none() {
            continue;
        }

        let id = *canonical
            .entry((index, program.hashes[node]))
            .or_insert_with(|| {
                let metadata = program.functions.get(index);
                let base = metadata
                    .and_then(|function| program.string(function.name))
                    .unwrap_or("<unknown>")
                    .to_string();
                let count = names.entry(base.clone()).or_insert(0);
                *count += 1;
                let name = if *count == 1 {
                    base
                } else {
                    format!("{base}#{count}")
                };

                infos.push(FunctionInfo {
                    name,
                    node,
                    kind: FunctionKind::Described,
                    layout: metadata.and_then(|function| Layout::new(program, function)),
                });
                infos.len() - 1
            });

        function_of.insert(node, id);
    }

    let entry = if let Some(&id) = function_of.get(&program.root) {
        id
    } else {
        infos.push(FunctionInfo {
            name: "program".to_string(),
            node: program.root,
            kind: FunctionKind::Program,
            layout: None,
        });
        infos.len() - 1
    };

    (infos, function_of, entry)
}

/// The state of a function whose body is being decompiled.
struct Frame {
    id: FunctionId,
    /// The hidden parts of the environment that are known.
    known: Vec<KnownSlot>,
    vars: Vec<String>,
    /// The structures that variables are bound to, which reads see through to the values in them
    /// that aren't pairs.
    structures: HashMap<VarId, Value>,
    reserved: HashSet<String>,
}

impl Frame {
    /// A new variable, named so that it doesn't shadow a parameter.
    fn new_var(&mut self) -> VarId {
        let mut number = self.vars.len() + 1;
        let name = loop {
            let name = format!("v{number}");
            if !self.reserved.contains(&name) {
                break name;
            }
            number += 1;
        };
        self.reserved.insert(name.clone());
        self.vars.push(name);
        self.vars.len() - 1
    }
}

/// Where statements are added as the code of a block is decompiled.
struct Scope {
    env: Value,
    statements: Vec<Statement>,
}

struct Pass<'a> {
    program: &'a Program,
    /// The functions that the metadata describes, then the program if it's added.
    described: &'a [FunctionInfo],
    /// The functions that are found to be run from constants, which never have a layout.
    anonymous: Vec<FunctionInfo>,
    anonymous_of: HashMap<NodeId, FunctionId>,
    function_of: &'a HashMap<NodeId, FunctionId>,
    facts: &'a [Vec<Fact>],
    reads_before: &'a [Vec<bool>],
    observed: Vec<Vec<Fact>>,
    escaped: Vec<bool>,
    reads: Vec<Vec<bool>>,
    covered: Vec<bool>,
    exposed: HashSet<NodeId>,
    /// The parts of variables bound to structures that have been used as data.
    touched: HashSet<(FunctionId, VarId, Path)>,
    bodies: Vec<Function>,
}

impl<'a> Pass<'a> {
    fn run(
        program: &'a Program,
        described: &'a [FunctionInfo],
        function_of: &'a HashMap<NodeId, FunctionId>,
        facts: &'a [Vec<Fact>],
        reads_before: &'a [Vec<bool>],
        entry: FunctionId,
    ) -> Self {
        let mut pass = Self {
            program,
            described,
            anonymous: Vec::new(),
            anonymous_of: HashMap::new(),
            function_of,
            facts,
            reads_before,
            observed: facts
                .iter()
                .map(|facts| vec![Fact::Unset; facts.len()])
                .collect(),
            escaped: vec![false; facts.len()],
            reads: facts.iter().map(|facts| vec![false; facts.len()]).collect(),
            covered: vec![false; program.nodes.len()],
            exposed: HashSet::new(),
            touched: HashSet::new(),
            bodies: Vec::new(),
        };

        // The program is run with an environment that's only known at runtime
        pass.escaped[entry] = true;

        // Anonymous functions are added as they're found
        let mut id = 0;
        while id < pass.described.len() + pass.anonymous.len() {
            let function = pass.function(id);
            pass.bodies.push(function);
            id += 1;
        }

        pass
    }

    fn finish(self, entry: FunctionId, gave_up: bool) -> Built {
        let mut function_of = self.function_of.clone();
        function_of.extend(&self.anonymous_of);

        Built {
            functions: self.bodies,
            function_of,
            entry,
            covered: self.covered,
            gave_up,
        }
    }

    fn info(&self, id: FunctionId) -> &FunctionInfo {
        self.described
            .get(id)
            .unwrap_or_else(|| &self.anonymous[id - self.described.len()])
    }

    /// The hidden parts of the function's environment (anonymous functions have none).
    fn hidden(&self, id: FunctionId) -> &'a [Path] {
        let described: &'a [FunctionInfo] = self.described;
        described.get(id).map_or(&[], FunctionInfo::hidden)
    }

    /// What's known about each hidden part of the function's environment.
    fn facts(&self, id: FunctionId) -> &'a [Fact] {
        let facts: &'a [Vec<Fact>] = self.facts;
        facts.get(id).map_or(&[], Vec::as_slice)
    }

    fn function(&mut self, id: FunctionId) -> Function {
        let info = self.info(id).clone();

        let known = info
            .hidden()
            .iter()
            .zip(self.facts(id))
            .filter_map(|(path, fact)| match fact {
                Fact::Const(value) => Some(KnownSlot {
                    path: path.clone(),
                    value: *value,
                }),
                _ => None,
            })
            .collect();

        let mut frame = Frame {
            id,
            known,
            vars: Vec::new(),
            structures: HashMap::new(),
            reserved: info
                .layout
                .iter()
                .flat_map(|layout| layout.named.iter().map(|slot| slot.name.clone()))
                .collect(),
        };
        let mut scope = Scope {
            env: Value::Ref(Root::Env, Vec::new()),
            statements: Vec::new(),
        };

        let value = self.expr(&mut frame, &mut scope, info.node);
        let body = finish_block(self.program, scope.statements, value);

        Function {
            name: info.name,
            kind: info.kind,
            node: info.node,
            layout: info.layout,
            known: frame.known,
            reads_hidden: self.reads_before.get(id).cloned().unwrap_or_default(),
            body,
            vars: frame.vars,
        }
    }

    fn cover(&mut self, node: NodeId) {
        self.covered[node] = true;
    }

    fn cover_subtree(&mut self, node: NodeId) {
        for node in self.program.subtree(node) {
            self.covered[node] = true;
        }
    }

    /// Covers the pairs and the nil at the end of an argument list.
    fn cover_list(&mut self, mut node: NodeId) {
        loop {
            self.covered[node] = true;
            match self.program.pair(node) {
                Some((_, rest)) => node = rest,
                None => break,
            }
        }
    }

    /// The part of the value at the path, with the constant for a hidden part of the function's
    /// environment that's known, or `None` if the path goes into a computed part or through an
    /// atom constant (which fails at runtime).
    fn resolve(&self, frame: &Frame, value: &Value, path: &[bool]) -> Option<Value> {
        let mut value = value
            .resolve(self.program, &frame.known, path)?
            .into_owned();

        // A part of a structure that a variable is bound to is itself if it isn't a pair
        while let Value::Ref(Root::Var(var), path) = &value
            && let Some(structure) = frame.structures.get(var)
            && let Some(part) = structure.resolve(self.program, &frame.known, path)
            && !matches!(*part, Value::Cons(..))
        {
            value = part.into_owned();
        }

        Some(value)
    }

    /// Marks the value as used as data, which reads whatever parts of the environment it
    /// includes, and lets any function whose code it includes be called in unknown ways.
    fn touch(&mut self, frame: &Frame, value: &Value) {
        match value {
            Value::Const(node) => self.expose(*node),
            Value::Ref(Root::Env, path) => {
                for (index, (hidden, fact)) in self
                    .hidden(frame.id)
                    .iter()
                    .zip(self.facts(frame.id))
                    .enumerate()
                {
                    if hidden.starts_with(path) || path.starts_with(hidden) {
                        self.reads[frame.id][index] = true;
                    }

                    // Reading more than the hidden part includes its value
                    if let Fact::Const(known) = fact
                        && hidden.starts_with(path)
                        && path.len() < hidden.len()
                    {
                        self.expose(*known);
                    }
                }
            }
            // A structure's parts are used where the variable is, each at most once, since using
            // one again changes nothing
            Value::Ref(Root::Var(var), path) => {
                if !frame.structures.contains_key(var)
                    || !self.touched.insert((frame.id, *var, path.clone()))
                {
                    return;
                }
                if let Some(part) = frame
                    .structures
                    .get(var)
                    .and_then(|structure| structure.at(self.program, path))
                {
                    let part = part.into_owned();
                    self.touch(frame, &part);
                }
            }
            Value::Nil | Value::Expr(_) => {}
            Value::Cons(first, rest, _) => grow_stack(|| {
                self.touch(frame, first);
                self.touch(frame, rest);
            }),
        }
    }

    /// Marks the functions whose code is in the constant as escaping.
    fn expose(&mut self, node: NodeId) {
        if !self.program.contains_function[node] || !self.exposed.insert(node) {
            return;
        }

        for node in self.program.subtree(node) {
            if let Some(&id) = self.function_of.get(&node) {
                self.escaped[id] = true;
            }
        }
    }

    fn value_expr(&mut self, frame: &Frame, value: Value, node: NodeId) -> Expr {
        self.touch(frame, &value);
        Expr {
            kind: ExprKind::Value(value),
            node,
        }
    }

    fn raw(&mut self, frame: &Frame, scope: &Scope, code: NodeId) -> Expr {
        self.cover_subtree(code);
        self.expose(code);
        let env = scope.env.clone();
        self.touch(frame, &env);
        Expr {
            kind: ExprKind::Raw { code, env },
            node: code,
        }
    }

    fn expr(&mut self, frame: &mut Frame, scope: &mut Scope, code: NodeId) -> Expr {
        grow_stack(|| self.expr_inner(frame, scope, code))
    }

    fn expr_inner(&mut self, frame: &mut Frame, scope: &mut Scope, code: NodeId) -> Expr {
        let program = self.program;

        let (op, args) = match program.kind(code) {
            NodeKind::Atom(atom) => {
                self.cover(code);
                let Some(path) = atom_path(atom) else {
                    return self.value_expr(frame, nil_code(atom, code), code);
                };
                return match self.resolve(frame, &scope.env, &path) {
                    Some(value) => self.value_expr(frame, value, code),
                    None => self.raw(frame, scope, code),
                };
            }
            &NodeKind::Pair(op, args) => (op, args),
        };

        let Some(op_atom) = program.atom(op) else {
            return self.raw(frame, scope, code);
        };

        if op_atom == QUOTE {
            self.cover(code);
            self.cover(op);
            self.cover_subtree(args);
            return self.value_expr(frame, Value::Const(args), code);
        }

        let Some(operands) = program.list(args) else {
            return self.raw(frame, scope, code);
        };

        match (op_atom, operands.as_slice()) {
            (APPLY, &[callee, env]) => {
                self.cover(code);
                self.cover(op);
                self.cover_list(args);
                self.apply(frame, scope, callee, env, code)
            }
            (CONS, &[_, _]) => {
                let value = self.sym(frame, scope, code);
                self.value_expr(frame, value, code)
            }
            (APPLY, _) => self.raw(frame, scope, code),
            _ if operator_name(op_atom).is_some() => {
                self.cover(code);
                self.cover(op);
                self.cover_list(args);

                // From the last operand to the first, as they're evaluated
                let mut args = operands
                    .iter()
                    .rev()
                    .map(|&operand| self.expr(frame, scope, operand))
                    .collect::<Vec<_>>();
                args.reverse();

                Expr {
                    kind: ExprKind::Op {
                        op,
                        args: Deep::new(args),
                    },
                    node: code,
                }
            }
            _ => self.raw(frame, scope, code),
        }
    }

    /// The value of code as a structure, with the parts that it computes as expressions.
    fn sym(&mut self, frame: &mut Frame, scope: &mut Scope, code: NodeId) -> Value {
        grow_stack(|| self.sym_inner(frame, scope, code))
    }

    fn sym_inner(&mut self, frame: &mut Frame, scope: &mut Scope, code: NodeId) -> Value {
        let program = self.program;

        match program.kind(code) {
            NodeKind::Atom(atom) => {
                self.cover(code);
                let Some(path) = atom_path(atom) else {
                    return nil_code(atom, code);
                };
                match self.resolve(frame, &scope.env, &path) {
                    Some(value) => read(frame, scope, value, code),
                    None => Value::Expr(Deep::new(self.raw(frame, scope, code))),
                }
            }
            &NodeKind::Pair(op, args) => {
                if program.atom(op) == Some(QUOTE) {
                    self.cover(code);
                    self.cover(op);
                    self.cover_subtree(args);
                    return Value::Const(args);
                }

                if program.atom(op) == Some(CONS)
                    && let Some(&[first, rest]) = program.list(args).as_deref()
                {
                    self.cover(code);
                    self.cover(op);
                    self.cover_list(args);
                    let rest = self.sym(frame, scope, rest);
                    let first = self.sym(frame, scope, first);
                    return Value::cons(first, rest);
                }

                Value::Expr(Deep::new(self.expr(frame, scope, code)))
            }
        }
    }

    /// The program that quoted code is, if it's `(q . PROGRAM)`.
    fn quoted(&self, code: NodeId) -> Option<NodeId> {
        let (op, value) = self.program.pair(code)?;
        (self.program.atom(op) == Some(QUOTE)).then_some(value)
    }

    /// Covers quoted code, other than the value it quotes.
    fn cover_quote(&mut self, code: NodeId) {
        self.cover(code);
        if let Some((op, _)) = self.program.pair(code) {
            self.cover(op);
        }
    }

    fn apply(
        &mut self,
        frame: &mut Frame,
        scope: &mut Scope,
        callee: NodeId,
        env: NodeId,
        node: NodeId,
    ) -> Expr {
        let program = self.program;

        // `(a (q . PROGRAM) ENV)`
        if let Some(body) = self.quoted(callee) {
            self.cover_quote(callee);
            let env = self.sym(frame, scope, env);
            return self.known(frame, scope, body, env, true, node);
        }

        // `(a (i CONDITION (q . THEN) (q . ELSE)) ENV)`, where a branch can also be nil
        if let Some((op, args)) = program.pair(callee)
            && program.atom(op) == Some(IF)
            && let Some(&[condition, then, otherwise]) = program.list(args).as_deref()
            && [then, otherwise]
                .iter()
                .all(|&branch| program.is_nil(branch) || self.quoted(branch).is_some())
        {
            self.cover(callee);
            self.cover(op);
            self.cover_list(args);

            // The environment is evaluated before the condition
            let env = if program.atom(env) == Some(WHOLE_ENV) {
                self.cover(env);
                scope.env.clone()
            } else {
                let env = self.sym(frame, scope, env);
                bind(frame, scope, env)
            };
            let condition = self.expr(frame, scope, condition);

            let then = self.branch(frame, then, env.clone(), node);
            let otherwise = self.branch(frame, otherwise, env, node);

            return Expr {
                kind: ExprKind::If {
                    condition: Deep::new(condition),
                    then,
                    otherwise,
                },
                node,
            };
        }

        // A program that's a constant in the environment
        if let Some(path) = program.atom(callee).and_then(atom_path)
            && let Some(Value::Const(body)) = self.resolve(frame, &scope.env, &path)
        {
            self.cover(callee);
            let env = self.sym(frame, scope, env);
            return self.known(frame, scope, body, env, false, node);
        }

        // The environment is evaluated before the program
        let env = self.sym(frame, scope, env);
        let program_expr = self.expr(frame, scope, callee);
        self.touch(frame, &env);

        Expr {
            kind: ExprKind::Run {
                program: Deep::new(program_expr),
                env,
            },
            node,
        }
    }

    fn branch(&mut self, frame: &mut Frame, branch: NodeId, env: Value, node: NodeId) -> Block {
        let mut scope = Scope {
            env: env.clone(),
            statements: Vec::new(),
        };

        // A branch is quoted code, or nil
        let value = if let Some(body) = self.quoted(branch) {
            self.cover_quote(branch);
            self.known(frame, &mut scope, body, env, true, node)
        } else {
            self.cover(branch);
            self.value_expr(frame, Value::Const(branch), branch)
        };

        finish_block(self.program, scope.statements, value)
    }

    /// Runs a program that's known with the environment, which is a call if it's a function, and
    /// is otherwise inline (if it's quoted where it's run) or an anonymous function.
    fn known(
        &mut self,
        frame: &mut Frame,
        scope: &mut Scope,
        body: NodeId,
        env: Value,
        inline: bool,
        node: NodeId,
    ) -> Expr {
        let program = self.program;

        if let Some(atom) = program.atom(body) {
            // An atom reads from the environment it's run with
            self.cover(body);
            let env = bind(frame, scope, env);
            let Some(path) = atom_path(atom) else {
                return self.value_expr(frame, nil_code(atom, body), body);
            };
            if let Some(value) = self.resolve(frame, &env, &path) {
                return self.value_expr(frame, value, body);
            }
            let saved = mem::replace(&mut scope.env, env);
            let expr = self.raw(frame, scope, body);
            scope.env = saved;
            return expr;
        }

        if let Some(&id) = self.function_of.get(&body) {
            if self.info(id).node != body {
                // A copy of the body of a function that's shown elsewhere
                self.cover_subtree(body);
            }

            // A computed value that has more than one part of the environment in it is bound
            // first, so that each part can be shown
            let crossing = self.info(id).layout.as_ref().is_some_and(|layout| {
                let mut leaves = Vec::new();
                computed_paths(&env, &mut Vec::new(), &mut leaves);
                leaves.iter().any(|leaf| {
                    layout
                        .named
                        .iter()
                        .map(|slot| &slot.path)
                        .chain(&layout.hidden)
                        .any(|slot| slot.starts_with(leaf) && leaf.len() < slot.len())
                })
            });

            let env = if crossing {
                bind(frame, scope, env)
            } else {
                env
            };

            return self.call(frame, id, env, node);
        }

        if inline {
            let env = bind(frame, scope, env);
            let saved = mem::replace(&mut scope.env, env);
            let expr = self.expr(frame, scope, body);
            scope.env = saved;
            return expr;
        }

        let id = if let Some(&id) = self.anonymous_of.get(&body) {
            id
        } else {
            self.anonymous.push(FunctionInfo {
                name: format!("anonymous#{}", self.anonymous.len() + 1),
                node: body,
                kind: FunctionKind::Anonymous,
                layout: None,
            });
            let id = self.described.len() + self.anonymous.len() - 1;
            self.anonymous_of.insert(body, id);
            id
        };

        self.call(frame, id, env, node)
    }

    fn call(&mut self, frame: &Frame, id: FunctionId, env: Value, node: NodeId) -> Expr {
        let described: &'a [FunctionInfo] = self.described;
        let Some(layout) = described.get(id).and_then(|info| info.layout.as_ref()) else {
            self.touch(frame, &env);
            return Expr {
                kind: ExprKind::Call { function: id, env },
                node,
            };
        };

        for slot in &layout.named {
            match self.resolve(frame, &env, &slot.path) {
                Some(value) => self.touch(frame, &value),
                None => self.touch(frame, &env),
            }
        }

        for (index, path) in layout.hidden.iter().enumerate() {
            let value = self.resolve(frame, &env, path);

            // A hidden part of this function's environment that no call has been seen for yet
            // says nothing yet, and if none ever is, neither function is run
            if let Some(Value::Ref(Root::Env, passed)) = value.as_ref()
                && self
                    .hidden(frame.id)
                    .iter()
                    .zip(self.facts(frame.id))
                    .any(|(hidden, fact)| matches!(fact, Fact::Unset) && passed.starts_with(hidden))
            {
                continue;
            }

            let constant = match value.as_ref() {
                Some(Value::Const(node)) => Some(*node),
                _ => None,
            };
            join(self.program, &mut self.observed[id][index], constant);

            // Until a slot is known, values passed for it are assumed to be what it'll be
            let matches = match (self.facts(id)[index], constant) {
                (Fact::Unset, _) => true,
                (Fact::Const(known), Some(value)) => self.program.same_value(known, value),
                _ => false,
            };
            if !matches && self.reads_before[id][index] {
                self.touch(frame, value.as_ref().unwrap_or(&env));
            }
        }

        Expr {
            kind: ExprKind::Call { function: id, env },
            node,
        }
    }
}

/// The value that's read from an environment, which is bound to a variable if it's a large
/// structure.
fn read(frame: &mut Frame, scope: &mut Scope, value: Value, node: NodeId) -> Value {
    if !matches!(value, Value::Cons(_, _, shape) if !shape.computed && shape.size > MAX_STRUCTURE) {
        return value;
    }

    let var = frame.new_var();
    frame.structures.insert(var, value.clone());
    scope.statements.push(Statement::Let {
        var,
        value: Deep::new(Expr {
            kind: ExprKind::Value(value),
            node,
        }),
    });
    Value::Ref(Root::Var(var), Vec::new())
}

/// Binds the computed parts of the value to variables, in the order they're evaluated (the rest
/// of a pair first), with `let`s before the statement, and returns the value that refers to them
/// instead. Everything but the branches of an `if` is evaluated strictly, so this only changes
/// which of several failures is reported when the value is part of a larger expression.
fn bind(frame: &mut Frame, scope: &mut Scope, value: Value) -> Value {
    match value {
        Value::Expr(expr) => {
            let var = frame.new_var();
            scope.statements.push(Statement::Let { var, value: expr });
            Value::Ref(Root::Var(var), Vec::new())
        }
        // Only the parts with computed values in them are rebuilt
        Value::Cons(first, rest, shape) if shape.computed => grow_stack(|| {
            let rest = bind(frame, scope, rest.into_inner());
            let first = bind(frame, scope, first.into_inner());
            Value::cons(first, rest)
        }),
        value => value,
    }
}

/// The value of an atom of code that doesn't read from the environment, which is nil. Nil itself
/// is shown as its own node, and an atom of zero bytes evaluates to nil too.
fn nil_code(atom: &[u8], code: NodeId) -> Value {
    if atom.is_empty() {
        Value::Const(code)
    } else {
        Value::Nil
    }
}

fn computed_paths(value: &Value, path: &mut Path, paths: &mut Vec<Path>) {
    match value {
        Value::Expr(_) => paths.push(path.clone()),
        Value::Cons(first, rest, shape) if shape.computed => grow_stack(|| {
            path.push(false);
            computed_paths(first, path, paths);
            path.pop();
            path.push(true);
            computed_paths(rest, path, paths);
            path.pop();
        }),
        Value::Cons(..) | Value::Const(_) | Value::Nil | Value::Ref(..) => {}
    }
}

/// A block of the statements and the value, where an `if` that fails unless its condition holds
/// (an `assert`) at the end is merged into it.
fn finish_block(program: &Program, mut statements: Vec<Statement>, mut value: Expr) -> Block {
    loop {
        match value.kind {
            ExprKind::If {
                condition,
                then,
                otherwise,
            } if is_raise(program, &otherwise) => {
                let ExprKind::Op { args, .. } = otherwise.value.into_inner().kind else {
                    unreachable!("the branch is a raise");
                };
                statements.push(Statement::Assert {
                    condition,
                    raise: args,
                    node: value.node,
                });
                statements.extend(then.statements);
                value = then.value.into_inner();
            }
            kind => {
                return Block {
                    statements,
                    value: Deep::new(Expr {
                        kind,
                        node: value.node,
                    }),
                };
            }
        }
    }
}

/// Whether the block is only a `raise` (`x`).
fn is_raise(program: &Program, block: &Block) -> bool {
    block.statements.is_empty()
        && matches!(&block.value.kind, ExprKind::Op { op, .. } if program.atom(*op) == Some(RAISE))
}
