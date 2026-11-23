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
    Block, Expr, ExprKind, Function, FunctionId, KnownSlot, Root, Statement, Value, VarId,
};
use crate::program::{Layout, NodeId, NodeKind, Path, Program, atom_path, is_prefix};

/// How many times the analysis is repeated before it gives up on finding hidden values.
const MAX_PASSES: usize = 32;

const QUOTE: &[u8] = &[1];
const APPLY: &[u8] = &[2];
const IF: &[u8] = &[3];
const CONS: &[u8] = &[4];
const RAISE: &[u8] = &[8];

/// What's known about a hidden part of a function's environment, from every call to it.
#[derive(Debug, Clone, Copy)]
enum Fact {
    /// No call has been seen.
    Unset,
    Const(NodeId),
    Unknown,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FunctionKind {
    /// A function that the debug metadata describes.
    Described,
    /// A program that's run from a constant, which the debug metadata doesn't describe.
    Anonymous,
    /// The whole program, if it isn't the body of a function.
    Program,
}

/// A function that's known before the analysis.
#[derive(Debug, Clone)]
struct FunctionInfo {
    name: String,
    node: NodeId,
    kind: FunctionKind,
    layout: Option<Layout>,
}

/// The result of decompiling a program.
#[derive(Debug)]
pub struct Built {
    pub functions: Vec<Function>,
    pub kinds: Vec<FunctionKind>,
    pub entry: FunctionId,
    /// The function that each body of a function is, including copies of it.
    pub function_of: HashMap<NodeId, FunctionId>,
    /// Whether each node of the program is part of the decompiled code.
    pub covered: Vec<bool>,
    /// Whether the hidden values had to be given up on, since the analysis didn't settle.
    pub gave_up: bool,
}

pub fn build(program: &Program) -> Built {
    let (infos, function_of, entry) = described_functions(program);

    let hidden = |infos: &[FunctionInfo]| {
        infos
            .iter()
            .map(|info| info.layout.as_ref().map_or(0, |layout| layout.hidden.len()))
            .collect::<Vec<_>>()
    };
    let sizes = hidden(&infos);
    let mut facts: Vec<Vec<Fact>> = sizes.iter().map(|&n| vec![Fact::Unset; n]).collect();
    let mut reads: Vec<Vec<bool>> = sizes.iter().map(|&n| vec![false; n]).collect();

    for _ in 0..MAX_PASSES {
        let pass = Pass::run(program, infos.clone(), &function_of, &facts, &reads, entry);

        let mut new_facts = pass.observed.clone();
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
    let facts = sizes
        .iter()
        .map(|&n| vec![Fact::Unknown; n])
        .collect::<Vec<_>>();
    let reads = sizes.iter().map(|&n| vec![true; n]).collect::<Vec<_>>();
    Pass::run(program, infos, &function_of, &facts, &reads, entry).finish(entry, true)
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
        (Fact::Const(known), Some(value)) => {
            if program.same_value(known, value) {
                Fact::Const(known)
            } else {
                Fact::Unknown
            }
        }
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
    vars: Vec<String>,
    reserved: HashSet<String>,
}

/// Where statements are added as the code of a block is decompiled.
struct Scope {
    env: Value,
    statements: Vec<Statement>,
}

struct Pass<'a> {
    program: &'a Program,
    infos: Vec<FunctionInfo>,
    function_of: &'a HashMap<NodeId, FunctionId>,
    anonymous: HashMap<NodeId, FunctionId>,
    facts: &'a [Vec<Fact>],
    reads_before: &'a [Vec<bool>],
    observed: Vec<Vec<Fact>>,
    escaped: Vec<bool>,
    reads: Vec<Vec<bool>>,
    covered: Vec<bool>,
    exposed: HashSet<NodeId>,
    bodies: Vec<Option<Function>>,
}

impl<'a> Pass<'a> {
    fn run(
        program: &'a Program,
        infos: Vec<FunctionInfo>,
        function_of: &'a HashMap<NodeId, FunctionId>,
        facts: &'a [Vec<Fact>],
        reads_before: &'a [Vec<bool>],
        entry: FunctionId,
    ) -> Self {
        let observed = facts
            .iter()
            .map(|facts| vec![Fact::Unset; facts.len()])
            .collect();
        let reads = facts.iter().map(|facts| vec![false; facts.len()]).collect();
        let mut escaped = vec![false; facts.len()];

        // The program is run with an environment that's only known at runtime
        escaped[entry] = true;

        let mut pass = Self {
            program,
            infos,
            function_of,
            anonymous: HashMap::new(),
            facts,
            reads_before,
            observed,
            escaped,
            reads,
            covered: vec![false; program.nodes.len()],
            exposed: HashSet::new(),
            bodies: Vec::new(),
        };

        // Anonymous functions are added as they're found
        let mut id = 0;
        while id < pass.infos.len() {
            let function = pass.function(id);
            pass.bodies.push(Some(function));
            id += 1;
        }

        pass
    }

    fn finish(self, entry: FunctionId, gave_up: bool) -> Built {
        let mut function_of = self.function_of.clone();
        function_of.extend(&self.anonymous);

        Built {
            kinds: self.infos.iter().map(|info| info.kind).collect(),
            functions: self
                .bodies
                .into_iter()
                .map(|body| body.expect("every function is built"))
                .collect(),
            function_of,
            entry,
            covered: self.covered,
            gave_up,
        }
    }

    fn function(&mut self, id: FunctionId) -> Function {
        let info = self.infos[id].clone();

        let mut frame = Frame {
            id,
            vars: Vec::new(),
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

        let known = info
            .layout
            .as_ref()
            .map(|layout| {
                layout
                    .hidden
                    .iter()
                    .zip(self.facts.get(id).map_or(&[][..], Vec::as_slice))
                    .filter_map(|(path, fact)| match fact {
                        Fact::Const(value) => Some(KnownSlot {
                            path: path.clone(),
                            value: *value,
                        }),
                        _ => None,
                    })
                    .collect()
            })
            .unwrap_or_default();

        Function {
            name: info.name,
            node: info.node,
            layout: info.layout,
            known,
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

    fn new_var(&mut self, frame: &mut Frame) -> VarId {
        let mut number = frame.vars.len() + 1;
        let name = loop {
            let name = format!("v{number}");
            if !frame.reserved.contains(&name) {
                break name;
            }
            number += 1;
        };
        frame.reserved.insert(name.clone());
        frame.vars.push(name);
        frame.vars.len() - 1
    }

    /// The hidden parts of the function's environment, and what's known about them.
    fn hidden(&self, id: FunctionId) -> Vec<(Path, Fact)> {
        self.infos[id]
            .layout
            .as_ref()
            .map(|layout| {
                layout
                    .hidden
                    .iter()
                    .cloned()
                    .zip(self.facts.get(id).cloned().unwrap_or_default())
                    .collect()
            })
            .unwrap_or_default()
    }

    /// A reference to the function's environment, which is the constant every call passes if the
    /// path is in a hidden part that's known.
    fn env_ref(&self, frame: &Frame, path: Path) -> Value {
        for (hidden, fact) in self.hidden(frame.id) {
            if let Fact::Const(value) = fact
                && is_prefix(&hidden, &path)
                && let Some(part) = self.program.descend(value, &path[hidden.len()..])
            {
                return Value::Const(part);
            }
        }

        Value::Ref(Root::Env, path)
    }

    /// The part of the value at the path, or `None` if the path goes through an atom constant
    /// (which fails at runtime). The value must not have computed parts, other than at the path
    /// itself or inside of it.
    fn resolve(&self, frame: &Frame, value: &Value, path: &[bool]) -> Option<Value> {
        let mut value = value;
        let mut rest = path;

        loop {
            let Some((&step, remaining)) = rest.split_first() else {
                return Some(value.clone());
            };

            match value {
                Value::Cons(first, second) => {
                    value = if step { second } else { first };
                    rest = remaining;
                }
                Value::Const(node) => {
                    return self.program.descend(*node, rest).map(Value::Const);
                }
                Value::Ref(Root::Env, prefix) => {
                    let mut path = prefix.clone();
                    path.extend_from_slice(rest);
                    return Some(self.env_ref(frame, path));
                }
                Value::Ref(Root::Var(var), prefix) => {
                    let mut path = prefix.clone();
                    path.extend_from_slice(rest);
                    return Some(Value::Ref(Root::Var(*var), path));
                }
                Value::Expr(_) => unreachable!("an environment's computed parts are bound first"),
            }
        }
    }

    /// Marks the value as used as data, which reads whatever parts of the environment it
    /// includes, and lets any function whose code it includes be called in unknown ways.
    fn touch(&mut self, frame: &Frame, value: &Value) {
        match value {
            Value::Const(node) => self.expose(*node),
            Value::Ref(Root::Env, path) => {
                for (index, (hidden, fact)) in self.hidden(frame.id).into_iter().enumerate() {
                    if is_prefix(&hidden, path) || is_prefix(path, &hidden) {
                        self.reads[frame.id][index] = true;
                    }

                    // Reading more than the hidden part includes its value
                    if let Fact::Const(known) = fact
                        && is_prefix(path, &hidden)
                        && path.len() < hidden.len()
                    {
                        self.expose(known);
                    }
                }
            }
            Value::Ref(Root::Var(_), _) | Value::Expr(_) => {}
            Value::Cons(first, rest) => {
                self.touch(frame, first);
                self.touch(frame, rest);
            }
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

    fn raw(&mut self, frame: &Frame, scope: &mut Scope, code: NodeId) -> Expr {
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
        stacker::maybe_grow(256 * 1024, 4 * 1024 * 1024, || {
            self.expr_inner(frame, scope, code)
        })
    }

    fn expr_inner(&mut self, frame: &mut Frame, scope: &mut Scope, code: NodeId) -> Expr {
        let program = self.program;

        let (op, args) = match program.kind(code) {
            NodeKind::Atom(atom) => {
                self.cover(code);
                let Some(path) = atom_path(atom) else {
                    return self.value_expr(frame, Value::Const(code), code);
                };
                return match self.resolve(frame, &scope.env.clone(), &path) {
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

        let known = op_atom != APPLY && operator_name(op_atom).is_some();

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
            _ if known => {
                self.cover(code);
                self.cover(op);
                self.cover_list(args);
                let args = operands
                    .iter()
                    .map(|&operand| self.expr(frame, scope, operand))
                    .collect();
                Expr {
                    kind: ExprKind::Op { op, args },
                    node: code,
                }
            }
            _ => self.raw(frame, scope, code),
        }
    }

    /// The value of code as a structure, with the parts that it computes as expressions.
    fn sym(&mut self, frame: &mut Frame, scope: &mut Scope, code: NodeId) -> Value {
        let program = self.program;

        match program.kind(code) {
            NodeKind::Atom(atom) => {
                self.cover(code);
                let Some(path) = atom_path(atom) else {
                    return Value::Const(code);
                };
                match self.resolve(frame, &scope.env.clone(), &path) {
                    Some(value) => value,
                    None => Value::Expr(Box::new(self.raw(frame, scope, code))),
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
                    let first = self.sym(frame, scope, first);
                    let rest = self.sym(frame, scope, rest);
                    return Value::Cons(Box::new(first), Box::new(rest));
                }

                Value::Expr(Box::new(self.expr(frame, scope, code)))
            }
        }
    }

    /// The program that quoted code is, if it's `(q . PROGRAM)`.
    fn quoted(&self, code: NodeId) -> Option<NodeId> {
        let (op, value) = self.program.pair(code)?;
        (self.program.atom(op) == Some(QUOTE)).then_some(value)
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
            self.cover(callee);
            self.cover(program.pair(callee).unwrap().0);
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
            return self.lazy_if(frame, scope, condition, then, otherwise, env, node);
        }

        // A program that's a constant in the environment
        if let Some(path) = program.atom(callee).and_then(atom_path)
            && let Some(Value::Const(body)) = self.resolve(frame, &scope.env.clone(), &path)
        {
            self.cover(callee);
            let env = self.sym(frame, scope, env);
            return self.known(frame, scope, body, env, false, node);
        }

        let program_expr = self.expr(frame, scope, callee);
        let env = self.sym(frame, scope, env);
        self.touch(frame, &env);

        Expr {
            kind: ExprKind::Run {
                program: Box::new(program_expr),
                env,
            },
            node,
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn lazy_if(
        &mut self,
        frame: &mut Frame,
        scope: &mut Scope,
        condition: NodeId,
        then: NodeId,
        otherwise: NodeId,
        env: NodeId,
        node: NodeId,
    ) -> Expr {
        let condition = self.expr(frame, scope, condition);

        let env = if self.program.atom(env) == Some(&[1]) {
            self.cover(env);
            scope.env.clone()
        } else {
            let env = self.sym(frame, scope, env);
            self.bind(frame, scope, env)
        };

        self.if_expr(frame, condition, then, otherwise, env, node)
    }

    #[allow(clippy::too_many_arguments)]
    fn if_expr(
        &mut self,
        frame: &mut Frame,
        condition: Expr,
        then: NodeId,
        otherwise: NodeId,
        env: Value,
        node: NodeId,
    ) -> Expr {
        let then = self.branch(frame, then, env.clone(), node);
        let otherwise = self.branch(frame, otherwise, env, node);

        Expr {
            kind: ExprKind::If {
                condition: Box::new(condition),
                then,
                otherwise,
            },
            node,
        }
    }

    fn branch(&mut self, frame: &mut Frame, branch: NodeId, env: Value, node: NodeId) -> Block {
        let mut scope = Scope {
            env: env.clone(),
            statements: Vec::new(),
        };

        self.cover(branch);

        let value = match self.quoted(branch) {
            Some(body) => {
                self.cover(self.program.pair(branch).unwrap().0);
                self.known(frame, &mut scope, body, env, true, node)
            }
            None => self.value_expr(frame, Value::Const(branch), branch),
        };

        finish_block(self.program, scope.statements, value)
    }

    /// Runs a program that's known with the environment, which is a call if it's a function, and
    /// is otherwise inline (if it's quoted where it's run) or an anonymous function.
    #[allow(clippy::too_many_arguments)]
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
            let env = self.bind(frame, scope, env);
            let Some(path) = atom_path(atom) else {
                return self.value_expr(frame, Value::Const(body), body);
            };
            return match self.resolve(frame, &env, &path) {
                Some(value) => self.value_expr(frame, value, body),
                None => {
                    let saved = mem::replace(&mut scope.env, env);
                    let expr = self.raw(frame, scope, body);
                    scope.env = saved;
                    expr
                }
            };
        }

        if let Some(&id) = self.function_of.get(&body) {
            if self.infos[id].node != body {
                // A copy of the body of a function that's shown elsewhere
                self.cover_subtree(body);
            }

            // A computed value that has more than one part of the environment in it is bound
            // first, so that each part can be shown
            let crossing = self.infos[id].layout.as_ref().is_some_and(|layout| {
                let mut leaves = Vec::new();
                computed_paths(&env, &mut Vec::new(), &mut leaves);
                leaves.iter().any(|leaf| {
                    layout
                        .named
                        .iter()
                        .map(|slot| &slot.path)
                        .chain(&layout.hidden)
                        .any(|slot| is_prefix(leaf, slot) && leaf.len() < slot.len())
                })
            });

            let env = if crossing {
                self.bind(frame, scope, env)
            } else {
                env
            };

            return self.call(frame, id, env, node);
        }

        if inline {
            let env = self.bind(frame, scope, env);
            let saved = mem::replace(&mut scope.env, env);
            let expr = self.expr(frame, scope, body);
            scope.env = saved;
            return expr;
        }

        let id = match self.anonymous.get(&body) {
            Some(&id) => id,
            None => {
                let number = self
                    .infos
                    .iter()
                    .filter(|info| info.kind == FunctionKind::Anonymous)
                    .count()
                    + 1;
                self.infos.push(FunctionInfo {
                    name: format!("anonymous#{number}"),
                    node: body,
                    kind: FunctionKind::Anonymous,
                    layout: None,
                });
                let id = self.infos.len() - 1;
                self.anonymous.insert(body, id);
                id
            }
        };

        self.call(frame, id, env, node)
    }

    fn call(&mut self, frame: &mut Frame, id: FunctionId, env: Value, node: NodeId) -> Expr {
        match self.infos[id].layout.clone() {
            Some(layout) => {
                for slot in &layout.named {
                    match self.resolve_leafy(frame, &env, &slot.path) {
                        Some(value) => self.touch(frame, &value),
                        None => self.touch(frame, &env),
                    }
                }

                for (index, (path, fact)) in self.hidden(id).into_iter().enumerate() {
                    let value = self.resolve_leafy(frame, &env, &path);

                    // A hidden part of this function's environment that no call has been seen for
                    // yet says nothing yet, and if none ever is, neither function is run
                    if let Some(Value::Ref(Root::Env, passed)) = &value
                        && self.hidden(frame.id).iter().any(|(hidden, fact)| {
                            matches!(fact, Fact::Unset) && is_prefix(hidden, passed)
                        })
                    {
                        continue;
                    }

                    let constant = match &value {
                        Some(Value::Const(node)) => Some(*node),
                        _ => None,
                    };
                    join(self.program, &mut self.observed[id][index], constant);

                    // Until a slot is known, values passed for it are assumed to be what it'll be
                    let matches = match (fact, constant) {
                        (Fact::Unset, _) => true,
                        (Fact::Const(known), Some(value)) => self.program.same_value(known, value),
                        _ => false,
                    };
                    if !matches && self.reads_before[id][index] {
                        match &value {
                            Some(value) => self.touch(frame, value),
                            None => self.touch(frame, &env),
                        }
                    }
                }
            }
            None => self.touch(frame, &env),
        }

        Expr {
            kind: ExprKind::Call { function: id, env },
            node,
        }
    }

    /// Like [`resolve`](Self::resolve), but the value can have computed parts, and the path
    /// can't go into one.
    fn resolve_leafy(&self, frame: &Frame, value: &Value, path: &[bool]) -> Option<Value> {
        match (value, path.split_first()) {
            (_, None) => Some(value.clone()),
            (Value::Cons(first, rest), Some((&step, remaining))) => {
                self.resolve_leafy(frame, if step { rest } else { first }, remaining)
            }
            (Value::Expr(_), Some(_)) => None,
            _ => self.resolve(frame, value, path),
        }
    }

    /// Binds the computed parts of the value to variables in order, with `let`s before the
    /// statement, and returns the value that refers to them instead. Everything but the branches
    /// of an `if` is evaluated strictly (by `clvmr`, from the last operand to the first), so this
    /// only changes which of several failures is reported.
    fn bind(&mut self, frame: &mut Frame, scope: &mut Scope, value: Value) -> Value {
        match value {
            Value::Expr(expr) => {
                let var = self.new_var(frame);
                scope.statements.push(Statement::Let { var, value: *expr });
                Value::Ref(Root::Var(var), Vec::new())
            }
            Value::Cons(first, rest) => {
                let first = self.bind(frame, scope, *first);
                let rest = self.bind(frame, scope, *rest);
                Value::Cons(Box::new(first), Box::new(rest))
            }
            value => value,
        }
    }
}

fn computed_paths(value: &Value, path: &mut Path, paths: &mut Vec<Path>) {
    match value {
        Value::Expr(_) => paths.push(path.clone()),
        Value::Cons(first, rest) => {
            path.push(false);
            computed_paths(first, path, paths);
            path.pop();
            path.push(true);
            computed_paths(rest, path, paths);
            path.pop();
        }
        Value::Const(_) | Value::Ref(..) => {}
    }
}

/// A block of the statements and the value, where an `if` that fails unless its
/// condition holds (an `assert`) at the end is merged into it.
fn finish_block(program: &Program, mut statements: Vec<Statement>, mut value: Expr) -> Block {
    loop {
        match value.kind {
            ExprKind::If {
                condition,
                then,
                otherwise,
            } if raise_args(program, &otherwise).is_some() => {
                let raise = raise_args(program, &otherwise).unwrap_or_default();
                statements.push(Statement::Assert {
                    condition: *condition,
                    raise,
                    node: value.node,
                });
                statements.extend(then.statements);
                value = *then.value;
            }
            kind => {
                return Block {
                    statements,
                    value: Box::new(Expr {
                        kind,
                        node: value.node,
                    }),
                };
            }
        }
    }
}

/// The arguments of the `raise` (`x`) that the block is, if it's only that.
fn raise_args(program: &Program, block: &Block) -> Option<Vec<Expr>> {
    if !block.statements.is_empty() {
        return None;
    }

    match &block.value.kind {
        ExprKind::Op { op, args } if program.atom(*op) == Some(RAISE) => Some(args.clone()),
        _ => None,
    }
}
