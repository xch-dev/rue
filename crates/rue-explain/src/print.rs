//! Renders the [IR](crate::ir) as Rue-like pseudocode, with the source location of each line.

use std::fmt::Write;

use chialisp::compiler::debug_metadata::ParameterConstraint;
use rue_clvm::operator_name;

use crate::Explanation;
use crate::build::FunctionKind;
use crate::ir::{Block, Expr, ExprKind, Function, FunctionId, Root, Statement, Value};
use crate::program::{NodeId, NodeKind, Path, is_prefix, path_number};

/// How many nodes a constant can have to be shown as a Rue value rather than as CLVM.
const MAX_LITERAL_NODES: usize = 16;

/// The column that source locations are aligned to, unless a line is longer.
const MAX_COMMENT_COLUMN: usize = 72;

const INDENT: &str = "    ";

/// How long a line can be before it's wrapped.
const MAX_WIDTH: usize = 100;

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

pub fn render(explanation: &Explanation) -> String {
    let mut lines = Vec::new();

    let mut order = vec![explanation.entry];
    order.extend((0..explanation.functions.len()).filter(|&id| id != explanation.entry));

    if explanation.gave_up {
        lines.push(Line {
            indent: 0,
            text: "// The values of hidden parts of environments couldn't be worked out, so none are shown".to_string(),
            node: None,
        });
    }

    for (index, &id) in order.iter().enumerate() {
        if index > 0 {
            lines.push(Line {
                indent: 0,
                text: String::new(),
                node: None,
            });
        }

        if id == explanation.entry && explanation.kinds[id] == FunctionKind::Described {
            lines.push(Line {
                indent: 0,
                text: format!(
                    "// The program is the body of `{}`",
                    explanation.functions[id].name
                ),
                node: None,
            });
        }

        Printer {
            explanation,
            function: &explanation.functions[id],
            kind: explanation.kinds[id],
            lines: &mut lines,
        }
        .function();
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
    function: &'a Function,
    kind: FunctionKind,
    lines: &'a mut Vec<Line>,
}

impl Printer<'_> {
    fn line(&mut self, indent: usize, text: String, node: Option<NodeId>) {
        // A multi-line expression is indented along with its first line, and long lines are
        // wrapped
        let mut node = node;
        for part in text.split('\n') {
            let leading = part.len() - part.trim_start_matches(' ').len();
            let width = MAX_WIDTH.saturating_sub(indent * INDENT.len() + leading);
            for wrapped in wrap(&part[leading..], width) {
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

        let header = match self.kind {
            FunctionKind::Program => "program(solution) {".to_string(),
            FunctionKind::Anonymous => format!("fn {}(env) {{", function.name),
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
                format!("fn {}({parameters}) {{", function.name)
            }
        };
        self.line(0, header, Some(function.node));

        for known in &function.known {
            let value = self.constant(known.value);
            self.line(
                1,
                format!("// {} is always {value}", self.env_name(&known.path)),
                None,
            );
        }

        self.block(1, &function.body);
        self.line(0, "}".to_string(), None);
    }

    /// The statements of the block, then its value, each on lines of its own.
    fn block(&mut self, indent: usize, block: &Block) {
        for statement in &block.statements {
            match statement {
                Statement::Let { var, value } => {
                    let text = format!(
                        "let {} = {}",
                        self.function.vars[*var],
                        self.expr(value, PREC_IF)
                    );
                    self.line(indent, text, Some(value.node));
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
                        format!("assert {condition} else raise({})", self.args(raise))
                    };
                    self.line(indent, text, Some(*node));
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
            self.line(indent, format!("if {condition} {{"), Some(value.node));
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
                        format!("}} else if {condition} {{"),
                        Some(otherwise.value.node),
                    );
                    self.block(indent + 1, then);
                    otherwise = next;
                    continue;
                }

                self.line(indent, "} else {".to_string(), None);
                self.block(indent + 1, otherwise);
                break;
            }

            self.line(indent, "}".to_string(), None);
            return;
        }

        let text = self.expr(value, PREC_IF);
        self.line(indent, text, Some(value.node));
    }

    /// An expression as text, which can span several lines (for a block inside of it). It's in
    /// parentheses if it binds more loosely than `min`.
    fn expr(&self, expr: &Expr, min: u8) -> String {
        let (text, prec) = self.expr_prec(expr);
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
            ExprKind::Op { op, args } => self.op(*op, args),
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
        if block.statements.is_empty() && !matches!(block.value.kind, ExprKind::If { .. }) {
            let value = self.expr(&block.value, PREC_IF);
            if !value.contains('\n') {
                return format!("{{ {value} }}");
            }
        }

        let mut lines = Vec::new();
        let mut printer = Printer {
            explanation: self.explanation,
            function: self.function,
            kind: self.kind,
            lines: &mut lines,
        };
        printer.block(1, block);

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

    fn call(&self, id: FunctionId, env: &Value) -> String {
        let callee = &self.explanation.functions[id];

        let Some(layout) = &callee.layout else {
            return format!("{}(env: {})", callee.name, self.value(env));
        };

        let mut args = Vec::new();

        for slot in &layout.named {
            let Some(value) = self.resolve(env, &slot.path) else {
                return format!("{}(env: {})", callee.name, self.value(env));
            };
            args.push(format!("{}: {}", slot.name, self.value(&value)));
        }

        for (index, path) in layout.hidden.iter().enumerate() {
            let Some(value) = self.resolve(env, path) else {
                return format!("{}(env: {})", callee.name, self.value(env));
            };

            let known = callee.known.iter().any(|known| {
                &known.path == path
                    && matches!(value, Value::Const(node) if self.explanation.program.same_value(node, known.value))
            });
            let unread =
                !callee.reads_hidden.get(index).copied().unwrap_or(true) && !has_computed(&value);

            if known || unread {
                continue;
            }

            args.push(format!("env@{}: {}", path_number(path), self.value(&value)));
        }

        format!("{}({})", callee.name, args.join(", "))
    }

    /// The part of a value at a path, with the constant for a part of the environment that's
    /// known.
    fn resolve(&self, value: &Value, path: &[bool]) -> Option<Value> {
        let value = resolve(self.explanation, value, path)?;
        Some(match value {
            Value::Ref(Root::Env, path) => self.env_value(path),
            value => value,
        })
    }

    /// A part of the environment, which is the constant every call passes if it's known.
    fn env_value(&self, path: Path) -> Value {
        for known in &self.function.known {
            if is_prefix(&known.path, &path)
                && let Some(part) = self
                    .explanation
                    .program
                    .descend(known.value, &path[known.path.len()..])
            {
                return Value::Const(part);
            }
        }
        Value::Ref(Root::Env, path)
    }

    /// The items of the list that a program is run with, as the arguments of a call, with
    /// whatever ends the list spread (`...rest`) if it isn't nil.
    fn list_args(&self, env: &Value) -> String {
        let mut items = Vec::new();
        let mut value = env.clone();

        while let Some((first, rest)) = self.parts(&value) {
            items.push(self.value(&first));
            value = rest;
        }

        if !self.is_nil(&value) {
            items.push(format!("...{}", self.value(&value)));
        }

        items.join(", ")
    }

    /// The first and rest of a value that's known to be a pair.
    fn parts(&self, value: &Value) -> Option<(Value, Value)> {
        match value {
            Value::Cons(first, rest) => Some(((**first).clone(), (**rest).clone())),
            Value::Const(node) => self
                .explanation
                .program
                .pair(*node)
                .map(|(first, rest)| (Value::Const(first), Value::Const(rest))),
            _ => None,
        }
    }

    fn is_nil(&self, value: &Value) -> bool {
        matches!(value, Value::Const(node) if self.explanation.program.is_nil(*node))
    }

    fn is_atom(&self, value: &Value, atom: &[u8]) -> bool {
        matches!(value, Value::Const(node) if self.explanation.program.atom(*node) == Some(atom))
    }

    fn value(&self, value: &Value) -> String {
        match value {
            Value::Const(node) => self.constant(*node),
            Value::Ref(Root::Env, path) => self.env_name(path),
            Value::Ref(Root::Var(var), path) => project(&self.function.vars[*var], path),
            Value::Cons(..) => self.closure(value).unwrap_or_else(|| self.list(value)),
            Value::Expr(expr) => self.expr(expr, PREC_IF),
        }
    }

    /// A pair or list, in Rue's syntax.
    fn list(&self, value: &Value) -> String {
        let mut items = Vec::new();
        let mut value = value.clone();

        while let Value::Cons(first, rest) = value {
            items.push(self.value(&first));
            value = *rest;
        }

        if self.is_nil(&value) {
            format!("[{}]", items.join(", "))
        } else if items.len() == 1 {
            format!("({}, {})", items[0], self.value(&value))
        } else {
            format!("[{}, ...{}]", items.join(", "), self.value(&value))
        }
    }

    /// A closure that the code builds at runtime, which is the program
    /// `(a (q . FUNCTION) (c (q . CAPTURE) ... 1))` (or `()` at the end, if it doesn't take
    /// arguments).
    fn closure(&self, value: &Value) -> Option<String> {
        let (apply, rest) = self.parts(value)?;
        if !self.is_atom(&apply, &[2]) {
            return None;
        }

        let (quoted, rest) = self.parts(&rest)?;
        let (quote, function) = self.parts(&quoted)?;
        if !self.is_atom(&quote, &[1]) {
            return None;
        }
        let Value::Const(function) = function else {
            return None;
        };
        let &id = self.explanation.function_of.get(&function)?;

        let (mut env, rest) = self.parts(&rest)?;
        if !self.is_nil(&rest) {
            return None;
        }

        let mut captures = Vec::new();
        let mut path = Vec::new();

        let takes_arguments = loop {
            if self.is_atom(&env, &[1]) {
                break true;
            }
            if self.is_nil(&env) {
                break false;
            }

            let (cons, rest) = self.parts(&env)?;
            if !self.is_atom(&cons, &[4]) {
                return None;
            }
            let (capture, rest) = self.parts(&rest)?;
            let (next, end) = self.parts(&rest)?;
            if !self.is_nil(&end) {
                return None;
            }

            let capture = if self.is_nil(&capture) {
                capture
            } else {
                let (quote, value) = self.parts(&capture)?;
                if !self.is_atom(&quote, &[1]) {
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

        Some(format!("{}{{{}{suffix}}}", callee.name, fields.join(", ")))
    }

    fn constant(&self, node: NodeId) -> String {
        let program = &self.explanation.program;

        if let Some(&id) = self.explanation.function_of.get(&node) {
            return self.explanation.functions[id].name.clone();
        }

        if program.is_nil(node) {
            return "nil".to_string();
        }

        if program.atom(node).is_some() {
            return program.disassemble_value(node);
        }

        if !program.contains_function[node] && program.subtree(node).len() <= MAX_LITERAL_NODES {
            return self.literal(node);
        }

        format!("clvm!(q . {})", program.disassemble(node))
    }

    /// A small constant in Rue's syntax.
    fn literal(&self, node: NodeId) -> String {
        let program = &self.explanation.program;

        match program.kind(node) {
            NodeKind::Atom(atom) if atom.is_empty() => "nil".to_string(),
            NodeKind::Atom(_) => program.disassemble_value(node),
            NodeKind::Pair(..) => {
                let mut items = Vec::new();
                let mut node = node;
                while let Some((first, rest)) = program.pair(node) {
                    items.push(self.literal(first));
                    node = rest;
                }
                if program.is_nil(node) {
                    format!("[{}]", items.join(", "))
                } else if items.len() == 1 {
                    format!("({}, {})", items[0], self.literal(node))
                } else {
                    format!("[{}, ...{}]", items.join(", "), self.literal(node))
                }
            }
        }
    }

    /// The name of a part of the environment: the parameter it's in, or its path.
    fn env_name(&self, path: &Path) -> String {
        let base = match self.kind {
            FunctionKind::Program => "solution",
            FunctionKind::Described | FunctionKind::Anonymous => "env",
        };

        if let Some(layout) = &self.function.layout
            && let Some(slot) = layout.named.iter().find(|slot| is_prefix(&slot.path, path))
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

/// Wraps a line that's longer than the width, by putting each item of its longest bracketed
/// group on a line of its own (and wrapping those in turn). Text in string literals is left alone.
fn wrap(text: &str, width: usize) -> Vec<String> {
    if text.chars().count() <= width {
        return vec![text.to_string()];
    }

    let Some((open, close, items)) = longest_group(text) else {
        return vec![text.to_string()];
    };

    let mut lines = vec![text[..=open].to_string()];
    let inner_width = width.saturating_sub(INDENT.len());
    // Items of a block aren't separated by commas
    let separator = if text[open..].starts_with('{') {
        ""
    } else {
        ","
    };
    for item in items {
        let item = format!("{}{separator}", text[item.0..item.1].trim());
        for line in wrap(&item, inner_width) {
            lines.push(format!("{INDENT}{line}"));
        }
    }
    lines.push(text[close..].to_string());
    lines
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
    let mut in_string = false;
    let mut escaped = false;

    for (offset, char) in text.char_indices() {
        if in_string {
            if escaped {
                escaped = false;
            } else if char == '\\' {
                escaped = true;
            } else if char == '"' {
                in_string = false;
            }
            continue;
        }

        match char {
            '"' => in_string = true,
            '(' | '[' | '{' => {
                if depth == 0 {
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

fn has_computed(value: &Value) -> bool {
    match value {
        Value::Expr(_) => true,
        Value::Cons(first, rest) => has_computed(first) || has_computed(rest),
        Value::Const(_) | Value::Ref(..) => false,
    }
}

/// The part of a value at a path, if it doesn't go into a computed part or through an atom.
pub(crate) fn resolve(explanation: &Explanation, value: &Value, path: &[bool]) -> Option<Value> {
    let Some((&step, rest)) = path.split_first() else {
        return Some(value.clone());
    };

    match value {
        Value::Cons(first, second) => resolve(explanation, if step { second } else { first }, rest),
        Value::Const(node) => explanation.program.descend(*node, path).map(Value::Const),
        Value::Ref(root, prefix) => {
            let mut full = prefix.clone();
            full.extend_from_slice(path);
            Some(Value::Ref(*root, full))
        }
        Value::Expr(_) => None,
    }
}
