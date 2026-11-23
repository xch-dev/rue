//! Renders the [IR](crate::ir) as Rue-like pseudocode, with the source location of each line.

use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::fmt::Write;

use chialisp::compiler::debug_metadata::ParameterConstraint;
use rue_clvm::operator_name;

use crate::Explanation;
use crate::ir::{
    Block, Expr, ExprKind, Function, FunctionId, FunctionKind, Root, Statement, Value,
};
use crate::program::{APPLY, CONS, NodeId, Path, QUOTE, WHOLE_ENV, path_number};
use crate::stack::grow_stack;

/// How many nodes a constant can have to be shown as a Rue value rather than as CLVM.
const MAX_LITERAL_NODES: usize = 16;

/// The column that source locations are aligned to, unless a line is longer.
const MAX_COMMENT_COLUMN: usize = 72;

const INDENT: &str = "    ";

/// How long a line can be before it's wrapped.
const MAX_WIDTH: usize = 100;

/// How deeply blocks are nested at most before the rest isn't shown, which keeps a pathological
/// program from producing output that's quadratic (or worse) in its size.
const MAX_NESTING: usize = 64;

/// How many times a line is wrapped inside of itself at most, which keeps wrapping a deeply nested
/// line from taking quadratic time.
const MAX_WRAP_DEPTH: usize = 32;

/// Precedences, from the loosest to the tightest.
const PREC_IF: u8 = 0;
const PREC_COMPARE: u8 = 3;
const PREC_ADD: u8 = 4;
const PREC_MUL: u8 = 5;
const PREC_UNARY: u8 = 6;
const PREC_POSTFIX: u8 = 7;
const PREC_ATOM: u8 = 8;

struct Line {
    indent: usize,
    text: String,
    node: Option<NodeId>,
}

/// How large a constant can be (in bytes, for an atom, or in nodes) before it's given a name and
/// shown once if it's used more than once.
const MAX_REPEATED: usize = 64;

/// The names that functions and constants are shown with.
#[derive(Default)]
struct Names {
    functions: HashMap<FunctionId, String>,
    /// The name of the code of each function (by the version that knows nothing about its
    /// environment), which is the name of the version that's shown for it.
    code: HashMap<FunctionId, String>,
    constants: HashMap<[u8; 32], String>,
}

/// What a first rendering refers to, which decides what's shown and how it's named.
#[derive(Default)]
struct Record {
    /// The functions that are called, and the functions whose code is used as a value.
    functions: Vec<FunctionId>,
    /// The functions whose code is named in comments.
    comments: HashSet<FunctionId>,
    /// The large constants, in the order they're first used, with how many times each is.
    constants: Vec<([u8; 32], NodeId)>,
    counts: HashMap<[u8; 32], usize>,
}

pub(crate) fn render(explanation: &Explanation) -> String {
    let (shown, record) = shown_functions(explanation);
    let mut used = reserved_names(explanation, &shown);
    let mut names = function_names(explanation, &shown, &mut used);

    // Large constants that are used more than once are named, and shown once
    let mut definitions = Vec::new();
    let mut number = 0;
    for (hash, node) in record.constants {
        if record.counts[&hash] < 2 {
            continue;
        }
        let name = loop {
            number += 1;
            let name = format!("C{number}");
            if !used.contains(&name) {
                break name;
            }
        };
        definitions.push((name.clone(), node));
        names.constants.insert(hash, name);
    }

    let mut lines = Vec::new();

    let typed = shown.iter().any(|&id| {
        explanation.functions[id]
            .layout
            .as_ref()
            .is_some_and(|layout| {
                layout
                    .named
                    .iter()
                    .any(|slot| constraint(&slot.constraint).is_some())
            })
    });
    if typed {
        lines.push(Line {
            indent: 0,
            text:
                "// The types of parameters are what the debug metadata says about their declared \
                   types, which the program doesn't check"
                    .to_string(),
            node: None,
        });
    }

    for (name, node) in &definitions {
        let mut printer = printer(explanation, explanation.entry, &names, None, &mut lines);
        let text = format!("const {name} = {}", printer.literal(*node));
        printer.line(0, &text, None);
    }

    for (index, &id) in shown.iter().enumerate() {
        if index > 0 || !definitions.is_empty() {
            lines.push(Line {
                indent: 0,
                text: String::new(),
                node: None,
            });
        }

        if id == explanation.entry && explanation.functions[id].kind == FunctionKind::Described {
            lines.push(Line {
                indent: 0,
                text: format!("// The program is the body of `{}`", names.functions[&id]),
                node: None,
            });
        }

        printer(explanation, id, &names, None, &mut lines).function();
    }

    let width = lines
        .iter()
        .filter(|line| {
            line.node
                .is_some_and(|node| explanation.program.nodes[node].span.is_some())
        })
        .map(|line| line.indent * INDENT.len() + line.text.chars().count())
        .filter(|&width| width <= MAX_COMMENT_COLUMN)
        .max()
        .unwrap_or(0);

    let mut output = String::new();

    for line in lines {
        let mut text = format!("{}{}", INDENT.repeat(line.indent), line.text);

        if let Some(location) = line.node.and_then(|node| location(explanation, node)) {
            let padding = width.saturating_sub(text.chars().count());
            write!(text, "{}  // {location}", " ".repeat(padding)).unwrap();
        }

        output.push_str(text.trim_end());
        output.push('\n');
    }

    output
}

fn printer<'a>(
    explanation: &'a Explanation,
    id: FunctionId,
    names: &'a Names,
    record: Option<&'a RefCell<Record>>,
    lines: &'a mut Vec<Line>,
) -> Printer<'a> {
    Printer {
        explanation,
        id,
        function: &explanation.functions[id],
        names,
        record,
        in_comment: false,
        depth: 0,
        lines,
    }
}

/// The functions to show, in order: the program, the functions it calls or uses the code of (and
/// so on), and the functions the metadata describes that none of the others are versions of.
/// Along with what they refer to.
fn shown_functions(explanation: &Explanation) -> (Vec<FunctionId>, Record) {
    let functions = &explanation.functions;
    let names = Names::default();
    let record = RefCell::new(Record::default());

    let mut shown = HashSet::new();
    let mut versions = HashMap::<FunctionId, usize>::new();
    let mut queue = vec![explanation.entry];

    loop {
        while let Some(id) = queue.pop() {
            if !shown.insert(id) {
                continue;
            }
            *versions.entry(functions[id].generic).or_default() += 1;
            printer(explanation, id, &names, Some(&record), &mut Vec::new()).function();
            queue.append(&mut record.borrow_mut().functions);
        }

        // A function that's named in a comment is shown as it is unless exactly one version of
        // it is, which the name is then for, and a function the metadata describes is shown if no
        // version of it is
        let shown_versions = |id: &FunctionId| versions.get(id).copied().unwrap_or(0);
        queue = record
            .borrow()
            .comments
            .iter()
            .copied()
            .filter(|id| !shown.contains(id) && shown_versions(id) != 1)
            .collect();
        if queue.is_empty() {
            queue = (0..functions.len())
                .filter(|id| {
                    functions[*id].kind == FunctionKind::Described
                        && functions[*id].generic == *id
                        && shown_versions(id) == 0
                })
                .collect();
        }

        if queue.is_empty() {
            break;
        }
        queue.sort_unstable_by(|a, b| b.cmp(a));
    }

    // The program first, then each function with its versions
    let mut shown = shown.into_iter().collect::<Vec<_>>();
    shown.sort_by_key(|&id| {
        (
            id != explanation.entry,
            functions[id].generic,
            functions[id].call,
            id,
        )
    });
    (shown, record.into_inner())
}

/// Names that functions and constants aren't given, since they're names of parameters or
/// variables, or are used for parts of environments.
fn reserved_names(explanation: &Explanation, shown: &[FunctionId]) -> HashSet<String> {
    let mut reserved =
        HashSet::from(["env".to_string(), "solution".to_string(), "nil".to_string()]);
    for &id in shown {
        let function = &explanation.functions[id];
        reserved.extend(function.vars.iter().cloned());
        if let Some(layout) = &function.layout {
            reserved.extend(layout.named.iter().map(|slot| slot.name.clone()));
        }
    }
    reserved
}

/// The name of each function that's shown, and of the code of each function: its own name, unless
/// another function that's shown has it (or it's the name of a value), in which case it's numbered.
/// The names are added to the ones that are used.
fn function_names(
    explanation: &Explanation,
    shown: &[FunctionId],
    used: &mut HashSet<String>,
) -> Names {
    let functions = &explanation.functions;
    let mut names = Names::default();
    let mut numbers = HashMap::<&str, usize>::new();

    for &id in shown {
        let base = functions[id].name.as_str();
        let number = numbers.entry(base).or_insert(1);
        let name = loop {
            let name = if *number == 1 {
                base.to_string()
            } else {
                format!("{base}#{number}")
            };
            *number += 1;
            if !used.contains(&name) && !is_generated(&name) {
                break name;
            }
        };
        used.insert(name.clone());
        names.functions.insert(id, name);
    }

    let shown_set = shown.iter().copied().collect::<HashSet<_>>();
    for &id in shown {
        let generic = functions[id].generic;
        if generic == id || !shown_set.contains(&generic) {
            names
                .code
                .entry(generic)
                .or_insert_with(|| names.functions[&id].clone());
        }
    }

    names
}

/// Whether the name is one that variables or constants are given.
fn is_generated(name: &str) -> bool {
    name.strip_prefix(['v', 'C']).is_some_and(|number| {
        !number.is_empty() && number.bytes().all(|byte| byte.is_ascii_digit())
    })
}

/// Where the node is in the source, along with its label if it has one.
fn location(explanation: &Explanation, node: NodeId) -> Option<String> {
    let program = &explanation.program;
    let node = &program.nodes[node];
    let span = program.spans.get(node.span?)?;
    let file = program.files.get(span.file)?;

    let mut location = if span.start_line == span.end_line {
        format!(
            "{}:{}:{}-{}",
            file.path, span.start_line, span.start_column, span.end_column
        )
    } else {
        format!(
            "{}:{}:{}-{}:{}",
            file.path, span.start_line, span.start_column, span.end_line, span.end_column
        )
    };

    if let Some(label) = node.label.and_then(|label| program.string(label)) {
        write!(location, " ({label})").unwrap();
    }

    Some(location)
}

struct Printer<'a> {
    explanation: &'a Explanation,
    id: FunctionId,
    function: &'a Function,
    names: &'a Names,
    /// What the rendering refers to, if it's only to find that out.
    record: Option<&'a RefCell<Record>>,
    /// Whether what's being rendered is a comment.
    in_comment: bool,
    /// How deeply the lines are nested in blocks inside of expressions.
    depth: usize,
    lines: &'a mut Vec<Line>,
}

impl Printer<'_> {
    fn line(&mut self, indent: usize, text: &str, node: Option<NodeId>) {
        // Nothing is shown when only finding out what's referred to
        if self.record.is_some() {
            return;
        }

        // A multi-line expression is indented along with its first line, and long lines are
        // wrapped, other than comments (whose other lines would look like code)
        let mut node = node;
        for part in text.split('\n') {
            let leading = part.len() - part.trim_start_matches(' ').len();
            let width = if part[leading..].starts_with("//") {
                usize::MAX
            } else {
                MAX_WIDTH.saturating_sub(indent * INDENT.len() + leading)
            };
            for wrapped in wrap(&part[leading..], width, MAX_WRAP_DEPTH) {
                self.lines.push(Line {
                    indent,
                    text: format!("{}{wrapped}", &part[..leading]),
                    node: node.take(),
                });
            }
        }
    }

    fn function(&mut self) {
        let function = self.function;

        let name = self.function_name(self.id);
        let header = match function.kind {
            FunctionKind::Program => "program(solution) {".to_string(),
            FunctionKind::Anonymous => format!("fn {name}(env) {{"),
            FunctionKind::Described => {
                let parameters = match &function.layout {
                    Some(layout) => layout
                        .named
                        .iter()
                        .map(|slot| match constraint(&slot.constraint) {
                            Some(ty) => format!("{}: {ty}", slot.name),
                            None => slot.name.clone(),
                        })
                        .collect::<Vec<_>>()
                        .join(", "),
                    None => "env".to_string(),
                };
                format!("fn {name}({parameters}) {{")
            }
        };
        self.line(0, &header, Some(function.node));

        // A nil that every call passes is the end of the parameter list, which isn't worth
        // pointing out
        for known in function
            .known
            .iter()
            .filter(|known| !self.explanation.program.is_nil(known.value))
        {
            // The name of code that's only run with this environment can be that of the version
            // that's run, but code that's used as data is named as it is
            self.in_comment = !known.exposed;
            let value = self.constant(known.value);
            self.in_comment = false;
            self.line(
                1,
                &format!("// {} is always {value}", self.env_name(&known.path)),
                None,
            );
        }

        self.block(1, &function.body);
        self.line(0, "}", None);
    }

    /// The statements of the block, then its value, each on lines of its own.
    fn block(&mut self, indent: usize, block: &Block) {
        if self.depth + indent > MAX_NESTING {
            self.line(indent, "... // nested too deeply to show", None);
            return;
        }

        for statement in &block.statements {
            match statement {
                Statement::Let { var, value } => {
                    let text = format!(
                        "let {} = {}",
                        self.function.vars[*var],
                        self.expr(value, PREC_IF)
                    );
                    self.line(indent, &text, Some(value.node));
                }
                Statement::Assert {
                    condition,
                    raise,
                    node,
                } => {
                    let condition = self.expr(condition, PREC_IF + 1);
                    let text = if raise.is_empty() {
                        format!("assert {condition}")
                    } else {
                        format!(
                            "assert {condition} else raise({})",
                            self.args(raise.as_slice())
                        )
                    };
                    self.line(indent, &text, Some(*node));
                }
            }
        }

        self.tail(indent, &block.value);
    }

    fn tail(&mut self, indent: usize, value: &Expr) {
        if let ExprKind::If {
            condition,
            then,
            otherwise,
        } = &value.kind
        {
            let condition = self.expr(condition, PREC_IF + 1);
            self.line(indent, &format!("if {condition} {{"), Some(value.node));
            self.block(indent + 1, then);

            // `else if` for an `if` that's all of the other branch
            let mut otherwise = otherwise;
            loop {
                if otherwise.statements.is_empty()
                    && let ExprKind::If {
                        condition,
                        then,
                        otherwise: next,
                    } = &otherwise.value.kind
                {
                    let condition = self.expr(condition, PREC_IF + 1);
                    self.line(
                        indent,
                        &format!("}} else if {condition} {{"),
                        Some(otherwise.value.node),
                    );
                    self.block(indent + 1, then);
                    otherwise = next;
                    continue;
                }

                self.line(indent, "} else {", None);
                self.block(indent + 1, otherwise);
                break;
            }

            self.line(indent, "}", None);
            return;
        }

        let text = self.expr(value, PREC_IF);
        self.line(indent, &text, Some(value.node));
    }

    /// An expression as text, which can span several lines (for a block inside of it). It's in
    /// parentheses if it binds more loosely than `min`.
    fn expr(&self, expr: &Expr, min: u8) -> String {
        let (text, prec) = grow_stack(|| self.expr_prec(expr));
        if prec < min {
            format!("({text})")
        } else {
            text
        }
    }

    fn expr_prec(&self, expr: &Expr) -> (String, u8) {
        match &expr.kind {
            ExprKind::Value(Value::Expr(expr)) => self.expr_prec(expr),
            ExprKind::Value(value) => (self.value(value), PREC_ATOM),
            ExprKind::Op { op, args } => self.op(*op, args.as_slice()),
            ExprKind::If {
                condition,
                then,
                otherwise,
            } => {
                let condition = self.expr(condition, PREC_IF + 1);
                let then = self.block_text(then);
                let otherwise = self.block_text(otherwise);
                (format!("if {condition} {then} else {otherwise}"), PREC_IF)
            }
            ExprKind::Call { function, env } => (self.call(*function, env), PREC_ATOM),
            ExprKind::Run { program, env } => {
                let program = self.expr(program, PREC_POSTFIX);
                (format!("{program}({})", self.list_args(env)), PREC_POSTFIX)
            }
            ExprKind::Raw { code, env } => {
                let code = self.explanation.program.disassemble(*code);
                let text = match env {
                    Value::Ref(Root::Env, path) if path.is_empty() => format!("clvm!({code})"),
                    env => format!("clvm!({code}, env = {})", self.value(env)),
                };
                (text, PREC_ATOM)
            }
        }
    }

    /// A block inside of an expression, which is on one line if it's only a value.
    fn block_text(&self, block: &Block) -> String {
        let mut lines = Vec::new();
        let mut printer = Printer {
            explanation: self.explanation,
            id: self.id,
            function: self.function,
            names: self.names,
            record: self.record,
            in_comment: self.in_comment,
            depth: self.depth + 1,
            lines: &mut lines,
        };

        // The value is only rendered once, since it can have blocks in it in turn
        if printer.depth < MAX_NESTING
            && block.statements.is_empty()
            && !matches!(block.value.kind, ExprKind::If { .. })
        {
            let value = printer.expr(&block.value, PREC_IF);
            if !value.contains('\n') {
                return format!("{{ {value} }}");
            }
            printer.line(1, &value, Some(block.value.node));
        } else {
            printer.block(1, block);
        }

        let mut text = "{".to_string();
        for line in lines {
            text.push('\n');
            text.push_str(&INDENT.repeat(line.indent));
            text.push_str(&line.text);
        }
        text.push_str("\n}");
        text
    }

    fn args(&self, args: &[Expr]) -> String {
        args.iter()
            .map(|arg| self.expr(arg, PREC_IF))
            .collect::<Vec<_>>()
            .join(", ")
    }

    fn op(&self, op: NodeId, args: &[Expr]) -> (String, u8) {
        let program = &self.explanation.program;
        let name = program
            .atom(op)
            .and_then(operator_name)
            .expect("an operator has a name");

        let infix = |symbol: &str, prec: u8, associative: bool| {
            let mut text = String::new();
            for (index, arg) in args.iter().enumerate() {
                if index > 0 {
                    write!(text, " {symbol} ").unwrap();
                }
                let min = if index == 0 && associative {
                    prec
                } else {
                    prec + 1
                };
                text.push_str(&self.expr(arg, min));
            }
            (text, prec)
        };

        match (name, args) {
            ("+", [_, _, ..]) => infix("+", PREC_ADD, true),
            ("-", [_, _, ..]) => infix("-", PREC_ADD, true),
            ("*", [_, _, ..]) => infix("*", PREC_MUL, true),
            ("/", [_, _]) => infix("/", PREC_MUL, true),
            ("%", [_, _]) => infix("%", PREC_MUL, true),
            (">", [_, _]) => infix(">", PREC_COMPARE, false),
            ("=", [_, _]) => infix("==", PREC_COMPARE, false),
            ("not", [arg]) => (format!("!{}", self.expr(arg, PREC_UNARY)), PREC_UNARY),
            ("f", [arg]) => (
                format!("{}.first", self.expr(arg, PREC_POSTFIX)),
                PREC_POSTFIX,
            ),
            ("r", [arg]) => (
                format!("{}.rest", self.expr(arg, PREC_POSTFIX)),
                PREC_POSTFIX,
            ),
            _ => {
                let name = match name {
                    "+" => "add",
                    "-" => "sub",
                    "*" => "mul",
                    "/" => "div",
                    "%" => "mod",
                    ">" => "gt",
                    ">s" => "gt_bytes",
                    "=" => "eq",
                    "f" => "first",
                    "r" => "rest",
                    "c" => "cons",
                    "l" => "listp",
                    "i" => "select",
                    "x" => "raise",
                    name => name,
                };
                (format!("{name}({})", self.args(args)), PREC_ATOM)
            }
        }
    }

    /// A call, with an argument for each named part of the callee's environment and each hidden
    /// part whose value the callee doesn't know, or with the whole environment if it isn't laid
    /// out that way.
    fn call(&self, id: FunctionId, env: &Value) -> String {
        if let Some(record) = self.record {
            record.borrow_mut().functions.push(id);
        }

        let callee = &self.explanation.functions[id];
        let name = self.function_name(id);
        match self.call_args(callee, env) {
            Some(args) => format!("{name}({})", args.join(", ")),
            None => format!("{name}(env: {})", self.value(env)),
        }
    }

    fn function_name(&self, id: FunctionId) -> String {
        self.names
            .functions
            .get(&id)
            .cloned()
            .unwrap_or_else(|| self.explanation.functions[id].name.clone())
    }

    /// The name of a function's code, which is used as a value.
    fn code_name(&self, id: FunctionId) -> String {
        let generic = self.explanation.functions[id].generic;
        if let Some(record) = self.record {
            let mut record = record.borrow_mut();
            if self.in_comment {
                record.comments.insert(generic);
            } else {
                record.functions.push(generic);
            }
        }

        self.names
            .code
            .get(&generic)
            .cloned()
            .unwrap_or_else(|| self.explanation.functions[generic].name.clone())
    }

    fn call_args(&self, callee: &Function, env: &Value) -> Option<Vec<String>> {
        let layout = callee.layout.as_ref()?;
        let mut args = Vec::new();

        for slot in &layout.named {
            let value = self.resolve(env, &slot.path)?;
            args.push(format!("{}: {}", slot.name, self.value(&value)));
        }

        for (index, path) in layout.hidden.iter().enumerate() {
            let value = self.resolve(env, path)?;

            // Only calls that pass what the callee knows are to it, but that's checked anyway
            let known = callee.known.iter().any(|known| {
                &known.path == path
                    && matches!(value, Value::Const(node) if self.explanation.program.same_value(node, known.value))
            });
            let unread =
                !callee.reads_hidden.get(index).copied().unwrap_or(true) && !value.has_computed();
            if known || unread {
                continue;
            }

            args.push(format!("env@{}: {}", path_number(path), self.value(&value)));
        }

        Some(args)
    }

    /// The part of a value at a path, with the constant for a part of the environment that's
    /// known.
    fn resolve(&self, value: &Value, path: &[bool]) -> Option<Value> {
        value
            .resolve(&self.explanation.program, &self.function.known, path)
            .map(Cow::into_owned)
    }

    /// The items of the list that a program is run with, as the arguments of a call, with
    /// whatever ends the list spread (`...rest`) if it isn't nil.
    fn list_args(&self, env: &Value) -> String {
        let (mut items, tail) = self.items(env);
        if let Some(tail) = tail {
            items.push(format!("...{tail}"));
        }
        items.join(", ")
    }

    /// The items of a list, and whatever ends it if it isn't nil. The items of a constant are
    /// only shown separately if it's small.
    fn items(&self, value: &Value) -> (Vec<String>, Option<String>) {
        let program = &self.explanation.program;
        let mut items = Vec::new();
        let mut value = value;

        while let Value::Cons(first, rest, _) = value {
            items.push(self.value(first));
            value = rest;

            // A closure at the end of a list is shown as one
            if let Some(closure) = self.closure(value) {
                return (items, Some(closure));
            }
        }

        let tail = match value {
            &Value::Const(mut node) => {
                if !program.contains_function[node] && program.sizes[node] <= MAX_LITERAL_NODES {
                    while let Some((first, rest)) = program.pair(node) {
                        items.push(self.constant(first));
                        node = rest;
                    }
                }
                (!program.is_nil(node)).then(|| self.constant(node))
            }
            Value::Nil => None,
            value => Some(self.value(value)),
        };

        (items, tail)
    }

    /// The first and rest of a value that's known to be a pair, borrowed from it where they can
    /// be.
    fn parts<'v>(&self, value: Cow<'v, Value>) -> Option<(Cow<'v, Value>, Cow<'v, Value>)> {
        let constant = |node: NodeId| {
            self.explanation.program.pair(node).map(|(first, rest)| {
                (
                    Cow::Owned(Value::Const(first)),
                    Cow::Owned(Value::Const(rest)),
                )
            })
        };

        match value {
            Cow::Borrowed(Value::Cons(first, rest, _)) => {
                Some((Cow::Borrowed(&**first), Cow::Borrowed(&**rest)))
            }
            Cow::Owned(Value::Cons(first, rest, _)) => Some((
                Cow::Owned(first.into_inner()),
                Cow::Owned(rest.into_inner()),
            )),
            Cow::Borrowed(&Value::Const(node)) | Cow::Owned(Value::Const(node)) => constant(node),
            _ => None,
        }
    }

    fn is_nil(&self, value: &Value) -> bool {
        match value {
            Value::Nil => true,
            Value::Const(node) => self.explanation.program.is_nil(*node),
            _ => false,
        }
    }

    fn is_atom(&self, value: &Value, atom: &[u8]) -> bool {
        matches!(value, Value::Const(node) if self.explanation.program.atom(*node) == Some(atom))
    }

    fn value(&self, value: &Value) -> String {
        grow_stack(|| self.value_inner(value))
    }

    fn value_inner(&self, value: &Value) -> String {
        match value {
            Value::Const(node) => self.constant(*node),
            Value::Nil => "nil".to_string(),
            Value::Ref(Root::Env, path) => self.env_name(path),
            Value::Ref(Root::Var(var), path) => project(&self.function.vars[*var], path),
            Value::Cons(..) => self.closure(value).unwrap_or_else(|| self.list(value)),
            Value::Expr(expr) => self.expr(expr, PREC_IF),
        }
    }

    /// A pair or list, in Rue's syntax.
    fn list(&self, value: &Value) -> String {
        match self.items(value) {
            (items, None) => format!("[{}]", items.join(", ")),
            (items, Some(tail)) if items.len() == 1 => format!("({}, {tail})", items[0]),
            (items, Some(tail)) if items.is_empty() => tail,
            (items, Some(tail)) => format!("[{}, ...{tail}]", items.join(", ")),
        }
    }

    /// A closure that the code builds at runtime, which is the program
    /// `(a (q . FUNCTION) (c (q . CAPTURE) ... 1))` (or `()` at the end, if it doesn't take
    /// arguments).
    fn closure(&self, value: &Value) -> Option<String> {
        let (apply, rest) = self.parts(Cow::Borrowed(value))?;
        if !self.is_atom(&apply, APPLY) {
            return None;
        }

        let (quoted, rest) = self.parts(rest)?;
        let (quote, function) = self.parts(quoted)?;
        if !self.is_atom(&quote, QUOTE) {
            return None;
        }
        let Value::Const(function) = *function else {
            return None;
        };
        let &id = self.explanation.function_of.get(&function)?;

        let (mut env, rest) = self.parts(rest)?;
        if !self.is_nil(&rest) {
            return None;
        }

        let mut captures = Vec::new();
        let mut path = Vec::new();

        let takes_arguments = loop {
            if self.is_atom(&env, WHOLE_ENV) {
                break true;
            }
            if self.is_nil(&env) {
                break false;
            }

            let (cons, rest) = self.parts(env)?;
            if !self.is_atom(&cons, CONS) {
                return None;
            }
            let (capture, rest) = self.parts(rest)?;
            let (next, end) = self.parts(rest)?;
            if !self.is_nil(&end) {
                return None;
            }

            // A capture is quoted, or nil code for nil (as Rue writes it), so that a closure that's
            // shown is only one value: `(q)` would be shown the same way as `()`
            let capture = if self.is_nil(&capture) {
                capture
            } else {
                let (quote, value) = self.parts(capture)?;
                if !self.is_atom(&quote, QUOTE) || self.is_nil(&value) {
                    return None;
                }
                value
            };

            let mut capture_path = path.clone();
            capture_path.push(false);
            captures.push((capture_path, capture));
            path.push(true);
            env = next;
        };

        let callee = &self.explanation.functions[id];
        let fields = captures
            .iter()
            .map(|(path, capture)| {
                let name = callee
                    .layout
                    .as_ref()
                    .and_then(|layout| layout.named.iter().find(|slot| &slot.path == path))
                    .map_or_else(
                        || format!("env@{}", path_number(path)),
                        |slot| slot.name.clone(),
                    );
                format!("{name}: {}", self.value(capture))
            })
            .collect::<Vec<_>>();

        let suffix = if takes_arguments {
            ""
        } else {
            ", without arguments"
        };

        Some(format!(
            "{}{{{}{suffix}}}",
            self.code_name(id),
            fields.join(", ")
        ))
    }

    fn constant(&self, node: NodeId) -> String {
        let program = &self.explanation.program;

        if let Some(&id) = self.explanation.function_of.get(&node) {
            return self.code_name(id);
        }

        let large = program.atom(node).map_or(program.sizes[node], <[u8]>::len) > MAX_REPEATED;
        if large {
            let hash = program.hashes[node];
            if let Some(name) = self.names.constants.get(&hash) {
                return name.clone();
            }
            if let Some(record) = self.record {
                let mut record = record.borrow_mut();
                let count = record.counts.entry(hash).or_insert(0);
                *count += 1;
                if *count == 1 {
                    record.constants.push((hash, node));
                }
                return "C".to_string();
            }
        }

        self.literal(node)
    }

    /// A constant as it's written, other than a function, which is shown by its name.
    fn literal(&self, node: NodeId) -> String {
        let program = &self.explanation.program;

        if program.is_nil(node) {
            return "nil".to_string();
        }

        if program.atom(node).is_some() {
            return program.disassemble_value(node);
        }

        if !program.contains_function[node] && program.sizes[node] <= MAX_LITERAL_NODES {
            return self.list(&Value::Const(node));
        }

        format!("clvm!(q . {})", program.disassemble(node))
    }

    /// The name of a part of the environment: the parameter it's in, or its path.
    fn env_name(&self, path: &Path) -> String {
        let base = match self.function.kind {
            FunctionKind::Program => "solution",
            FunctionKind::Described | FunctionKind::Anonymous => "env",
        };

        if let Some(layout) = &self.function.layout
            && let Some(slot) = layout
                .named
                .iter()
                .find(|slot| path.starts_with(&slot.path))
        {
            return project(&slot.name, &path[slot.path.len()..]);
        }

        if path.is_empty() {
            base.to_string()
        } else {
            format!("{base}@{}", path_number(path))
        }
    }
}

/// Wraps a line that's longer than the width, by breaking it before the loosest operators of a
/// chain of them, or by putting each item of its longest bracketed group on a line of its own
/// (and wrapping those in turn, up to the depth). Text in string literals and CLVM is left alone.
fn wrap(text: &str, width: usize, depth: usize) -> Vec<String> {
    if depth == 0 || text.chars().count() <= width {
        return vec![text.to_string()];
    }

    // A chain of operators is broken before each of its loosest ones
    if let Some(parts) = operator_parts(text) {
        let mut lines = Vec::new();
        for (index, part) in parts.iter().enumerate() {
            if index == 0 {
                lines.extend(wrap(part, width, depth - 1));
            } else {
                let inner_width = width.saturating_sub(INDENT.len());
                for line in wrap(part, inner_width, depth - 1) {
                    lines.push(format!("{INDENT}{line}"));
                }
            }
        }
        return lines;
    }

    let Some((open, close, items)) = longest_group(text) else {
        return vec![text.to_string()];
    };

    // Arguments, items and captures are separated by commas, but blocks and parenthesized values
    // (which have a single item) aren't
    let named = text[..open]
        .chars()
        .next_back()
        .is_some_and(|char| char.is_alphanumeric() || "_>)]}@#".contains(char));
    let separator = match text[open..].chars().next() {
        Some('[') => ",",
        Some('(') if named || items.len() > 1 => ",",
        Some('{') if named => ",",
        _ => "",
    };

    let mut lines = vec![text[..=open].to_string()];
    let inner_width = width.saturating_sub(INDENT.len());
    for item in items {
        let item = format!("{}{separator}", text[item.0..item.1].trim());
        for line in wrap(&item, inner_width, depth - 1) {
            lines.push(format!("{INDENT}{line}"));
        }
    }
    lines.push(text[close..].to_string());
    lines
}

/// The characters of the text that aren't in string literals, with their byte offsets.
fn outside_strings(text: &str) -> impl Iterator<Item = (usize, char)> + '_ {
    let mut in_string = false;
    let mut escaped = false;

    text.char_indices().filter(move |&(_, char)| {
        if in_string {
            if escaped {
                escaped = false;
            } else if char == '\\' {
                escaped = true;
            } else if char == '"' {
                in_string = false;
            }
            false
        } else if char == '"' {
            in_string = true;
            false
        } else {
            true
        }
    })
}

/// The text split before each of the loosest binary operators outside of brackets and strings,
/// if there are any (and it isn't an `if` or an `assert`).
fn operator_parts(text: &str) -> Option<Vec<&str>> {
    const OPERATORS: [(&str, u8); 7] = [
        (" == ", PREC_COMPARE),
        (" > ", PREC_COMPARE),
        (" + ", PREC_ADD),
        (" - ", PREC_ADD),
        (" * ", PREC_MUL),
        (" / ", PREC_MUL),
        (" % ", PREC_MUL),
    ];

    let mut found = Vec::new();
    let mut depth = 0usize;

    for (offset, char) in outside_strings(text) {
        match char {
            '(' | '[' | '{' => depth += 1,
            ')' | ']' | '}' => depth = depth.saturating_sub(1),
            // The operators of an `if` or an `assert` are in its parts, which are wrapped instead
            _ if depth == 0
                && (offset == 0 || text[..offset].ends_with(' '))
                && ["if ", "else ", "assert "]
                    .iter()
                    .any(|keyword| text[offset..].starts_with(keyword)) =>
            {
                return None;
            }
            ' ' if depth == 0 => {
                if let Some(&(_, prec)) = OPERATORS
                    .iter()
                    .find(|(operator, _)| text[offset..].starts_with(operator))
                {
                    found.push((offset, prec));
                }
            }
            _ => {}
        }
    }

    let loosest = found.iter().map(|&(_, prec)| prec).min()?;
    let mut parts = Vec::new();
    let mut start = 0;
    for (offset, _) in found.into_iter().filter(|&(_, prec)| prec == loosest) {
        parts.push(&text[start..offset]);
        start = offset + 1;
    }
    parts.push(&text[start..]);
    Some(parts)
}

/// The outermost bracketed group with the most text in it, as the byte offsets of its brackets
/// and the ranges of its items (which are separated by commas at its top level).
#[allow(clippy::type_complexity)]
fn longest_group(text: &str) -> Option<(usize, usize, Vec<(usize, usize)>)> {
    let mut best: Option<(usize, usize, Vec<(usize, usize)>)> = None;
    let mut depth = 0;
    let mut start = 0;
    let mut items = Vec::new();
    let mut item_start = 0;
    let mut raw = false;

    for (offset, char) in outside_strings(text) {
        match char {
            '(' | '[' | '{' => {
                if depth == 0 {
                    // CLVM is shown exactly as it is, so it isn't wrapped
                    raw = text[..offset].ends_with("clvm!");
                    start = offset;
                    item_start = offset + 1;
                    items.clear();
                }
                depth += 1;
            }
            ')' | ']' | '}' if depth > 0 => {
                depth -= 1;
                if depth == 0 {
                    if item_start < offset && !text[item_start..offset].trim().is_empty() {
                        items.push((item_start, offset));
                    }
                    if best
                        .as_ref()
                        .is_none_or(|(open, close, _)| close - open < offset - start)
                        && !items.is_empty()
                        && !raw
                    {
                        best = Some((start, offset, items.clone()));
                    }
                }
            }
            ',' if depth == 1 => {
                items.push((item_start, offset));
                item_start = offset + 1;
            }
            _ => {}
        }
    }

    best
}

/// A part of a named value, by the fields of pairs.
fn project(name: &str, path: &[bool]) -> String {
    let mut text = name.to_string();
    for &step in path {
        text.push_str(if step { ".rest" } else { ".first" });
    }
    text
}

fn constraint(constraint: &ParameterConstraint) -> Option<String> {
    Some(match constraint {
        ParameterConstraint::Integer => "Int".to_string(),
        ParameterConstraint::Atom => "Atom".to_string(),
        ParameterConstraint::Bytes(32) => "Bytes32".to_string(),
        ParameterConstraint::Bytes(length) => format!("Bytes({length})"),
        ParameterConstraint::Pair => "Pair".to_string(),
        ParameterConstraint::ProperList => "List".to_string(),
        ParameterConstraint::Unknown | ParameterConstraint::Union(_) => return None,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn wrapping_keeps_the_commas_between_items() {
        // A closure's captures are separated by commas, and a block's value isn't followed by one
        assert_eq!(
            wrap("f(g{alpha: a, beta: b}, h)", 12, MAX_WRAP_DEPTH),
            [
                "f(",
                "    g{",
                "        alpha: a,",
                "        beta: b,",
                "    },",
                "    h,",
                ")"
            ]
        );
        assert_eq!(
            wrap("if c { value_value } else { nil }", 12, MAX_WRAP_DEPTH),
            ["if c {", "    value_value", "} else { nil }"]
        );

        // Commas in strings don't separate anything
        assert_eq!(
            wrap("raise(\"a, b, c, d\")", 10, MAX_WRAP_DEPTH),
            ["raise(", "    \"a, b, c, d\",", ")"]
        );
    }

    #[test]
    fn wrapping_leaves_clvm_and_parentheses_alone() {
        // CLVM isn't wrapped, and a parenthesized value doesn't become a pair
        assert_eq!(
            wrap("f(clvm!((100 (q . 1) (q . 2))), alpha)", 20, MAX_WRAP_DEPTH),
            ["f(", "    clvm!((100 (q . 1) (q . 2))),", "    alpha,", ")"]
        );
        assert_eq!(
            wrap("(alpha + beta + gamma).first", 16, MAX_WRAP_DEPTH),
            [
                "(",
                "    alpha",
                "        + beta",
                "        + gamma",
                ").first"
            ]
        );
        assert_eq!(
            wrap("(alpha, beta_gamma)", 16, MAX_WRAP_DEPTH),
            ["(", "    alpha,", "    beta_gamma,", ")"]
        );
    }

    #[test]
    fn wrapping_breaks_chains_of_operators() {
        // Before the loosest operators outside of brackets and strings
        assert_eq!(
            wrap(
                "alpha * beta + f(gamma + delta) - \"a + b\"",
                24,
                MAX_WRAP_DEPTH
            ),
            ["alpha * beta", "    + f(gamma + delta)", "    - \"a + b\""]
        );
        assert_eq!(
            wrap("let v1 = alpha == beta * gamma", 20, MAX_WRAP_DEPTH),
            ["let v1 = alpha", "    == beta * gamma"]
        );

        // But not in an `if` or an `assert`, whose parts are wrapped instead
        assert_eq!(
            wrap(
                "let v1 = if alpha > beta { gamma } else { delta }",
                30,
                MAX_WRAP_DEPTH
            ),
            [
                "let v1 = if alpha > beta {",
                "    gamma",
                "} else { delta }"
            ]
        );
        assert_eq!(
            wrap("assert alpha > beta else raise(gamma)", 30, MAX_WRAP_DEPTH),
            ["assert alpha > beta else raise(", "    gamma,", ")"]
        );

        // And each part is wrapped in turn, to what's left of the width
        assert_eq!(
            wrap("x + f(alpha, beta)", 12, MAX_WRAP_DEPTH),
            ["x", "    + f(", "        alpha,", "        beta,", "    )"]
        );
        assert_eq!(
            wrap("x + f(alpha)", 11, MAX_WRAP_DEPTH),
            ["x", "    + f(", "        alpha,", "    )"]
        );
    }
}
