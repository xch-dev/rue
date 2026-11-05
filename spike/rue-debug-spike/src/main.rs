//! SPIKE: emit chialisp PR #579 debug sidecars from Rue and render captured failure stacks.
//!
//! Usage: rue-debug-spike <case.rue> <env> [release]

use std::{collections::HashMap, path::Path};

use chialisp::compiler::debug_metadata::{
    DebugFunction, DebugMetadata, DebugMetadataCollection, DebugNode, DebugParameter,
    DebugSourceFile, DebugSourceSpan, ParameterConstraint, SerializedFrame, StackFrameStyle,
};
use clvmr::{Allocator, NodePtr, SExp, run_program_with_diagnostics, serde::node_to_bytes};
use rue_compiler::{Compiler, FileTree, normalize_path};
use rue_diagnostic::SrcLoc;
use rue_hir::{Environment, Symbol, SymbolId};
use rue_lir::{CLVM_FLAGS, DEBUG_TAGGING, DebugDialect, DebugTagging, path_to_atom};
use rue_options::CompilerOptions;
use sha2::{Digest, Sha256};

const TAB_WIDTH: usize = 8;

#[derive(Default)]
struct Builder {
    files: Vec<DebugSourceFile>,
    file_indices: HashMap<String, usize>,
    strings: Vec<String>,
    string_indices: HashMap<String, usize>,
    spans: Vec<DebugSourceSpan>,
    span_indices: HashMap<(usize, usize, usize, usize, usize), usize>,
    functions: Vec<DebugFunction>,
    function_indices: HashMap<usize, usize>,
}

impl Builder {
    fn string(&mut self, value: &str) -> usize {
        if let Some(&index) = self.string_indices.get(value) {
            return index;
        }
        self.strings.push(value.to_string());
        self.string_indices
            .insert(value.to_string(), self.strings.len() - 1);
        self.strings.len() - 1
    }

    fn span(&mut self, srcloc: &SrcLoc, base: &Path) -> usize {
        let path = srcloc.source.kind.display(base);
        let file = *self.file_indices.entry(path.clone()).or_insert_with(|| {
            self.files.push(DebugSourceFile {
                path,
                source: srcloc.source.text.to_string(),
            });
            self.files.len() - 1
        });
        let text = &srcloc.source.text;
        let (start_line, start_column) = line_column(text, srcloc.span.start);
        let (end_line, end_column) = line_column(text, srcloc.span.end);
        let key = (file, start_line, start_column, end_line, end_column);
        if let Some(&index) = self.span_indices.get(&key) {
            return index;
        }
        self.spans.push(DebugSourceSpan {
            file,
            start_line,
            start_column,
            end_line,
            end_column,
        });
        self.span_indices.insert(key, self.spans.len() - 1);
        self.spans.len() - 1
    }
}

/// 1-based line and tab-expanded (width 8) display column, as #579 requires.
fn line_column(text: &str, offset: usize) -> (usize, usize) {
    let before = &text[..offset];
    let line = before.matches('\n').count() + 1;
    let line_start = before.rfind('\n').map_or(0, |i| i + 1);
    let mut column = 1;
    for ch in text[line_start..offset].chars() {
        if ch == '\t' {
            column = (column + TAB_WIDTH) & !(TAB_WIDTH - 1);
        } else {
            column += 1;
        }
    }
    (line, column)
}

fn type_constraint(type_name: &str) -> ParameterConstraint {
    match type_name {
        "Int" => ParameterConstraint::Integer,
        "Bytes32" => ParameterConstraint::Bytes(32),
        "PublicKey" => ParameterConstraint::Bytes(48),
        "Signature" => ParameterConstraint::Bytes(96),
        "Bytes" | "String" | "true | false" | "Atom" => ParameterConstraint::Atom,
        name if name.starts_with("List<") => ParameterConstraint::ProperList,
        name if name.starts_with('(') => ParameterConstraint::Pair,
        _ => ParameterConstraint::Unknown,
    }
}

struct Ctx<'a> {
    compiler: &'a mut Compiler,
    base: &'a Path,
    builder: Builder,
    tagging: DebugTagging,
    /// Printed alongside the traceback: what each parameter's declared type was.
    declared_types: Vec<String>,
}

impl Ctx<'_> {
    fn parameter_tree(&mut self, env: &Environment, path: num_bigint::BigUint) -> DebugParameter {
        match env {
            Environment::Nil => DebugParameter::Nil,
            Environment::Pair(first, rest) => DebugParameter::Pair(
                Box::new(self.parameter_tree(first, path.clone() * 2u8)),
                Box::new(self.parameter_tree(rest, path * 2u8 + 1u8)),
            ),
            Environment::Leaf(symbol) => self.leaf(*symbol, &path_to_atom(&path)),
        }
    }

    fn leaf(&mut self, symbol: SymbolId, path: &[u8]) -> DebugParameter {
        let (name, ty) = match self.compiler.symbol(symbol).clone() {
            Symbol::Parameter(parameter) => (
                self.compiler
                    .debug
                    .param_names
                    .get(&symbol)
                    .cloned()
                    .unwrap_or_else(|| "<param>".to_string()),
                Some(parameter.ty),
            ),
            Symbol::Binding(binding) => (
                binding
                    .name
                    .map_or("<binding>".to_string(), |n| n.text().to_string()),
                Some(binding.value.ty),
            ),
            // Captured functions and constants are compiler plumbing; hide them.
            _ => return DebugParameter::Nil,
        };
        let type_name = ty.map_or("?".to_string(), |ty| {
            rue_types::stringify(self.compiler.types_mut(), ty)
        });
        self.declared_types.push(format!("{name}: {type_name}"));
        DebugParameter::Name {
            name: self.builder.string(&name),
            path: path.to_vec(),
            constraint: type_constraint(&type_name),
        }
    }

    fn function(&mut self, lir_index: usize) -> usize {
        if let Some(&index) = self.builder.function_indices.get(&lir_index) {
            return index;
        }
        let (name, env) = if let Some((symbol, env)) =
            self.compiler.debug.function_roots.get(&lir_index).cloned()
        {
            (self.function_name(symbol), env)
        } else {
            let env = self.compiler.debug.scope_roots[&lir_index].clone();
            ("<let scope>".to_string(), env)
        };
        let parameters = self.parameter_tree(&env, 1u8.into());
        let function = DebugFunction {
            name: self.builder.string(&name),
            left_env: false,
            parameters,
        };
        self.builder.functions.push(function);
        let index = self.builder.functions.len() - 1;
        self.builder.function_indices.insert(lir_index, index);
        index
    }

    fn function_name(&self, symbol: SymbolId) -> String {
        match self.compiler.symbol(symbol) {
            Symbol::Function(function) => function
                .name
                .as_ref()
                .map_or("<lambda>".to_string(), |n| n.text().to_string()),
            _ => unreachable!(),
        }
    }

    fn tree(&mut self, allocator: &Allocator, node: NodePtr, inherited: Option<usize>) -> DebugNode {
        let span = self
            .tagging
            .node_spans
            .get(&node)
            .and_then(|lir| self.compiler.debug.lir_spans.get(lir).cloned())
            .map(|srcloc| self.builder.span(&srcloc, self.base))
            .or(inherited);
        match allocator.sexp(node) {
            SExp::Atom => DebugNode::Atom {
                span,
                label: None,
                function: None,
                value: allocator.atom(node).as_ref().to_vec(),
            },
            SExp::Pair(left, right) => {
                let function = self
                    .tagging
                    .node_functions
                    .get(&node)
                    .copied()
                    .map(|lir| self.function(lir));
                DebugNode::Pair {
                    span,
                    label: None,
                    function,
                    left: Box::new(self.tree(allocator, left, span)),
                    right: Box::new(self.tree(allocator, right, span)),
                }
            }
        }
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let path = Path::new(&args[1]);
    let env_text = &args[2];
    let release = args.get(3).is_some_and(|a| a == "release");

    let options = if release {
        CompilerOptions::default()
    } else {
        CompilerOptions::debug()
    };
    let base = path.parent().unwrap().canonicalize().unwrap();

    let mut compiler = Compiler::new(options);
    let tree = FileTree::compile_file(&mut compiler, path).unwrap();
    for diagnostic in compiler.take_diagnostics() {
        eprintln!("diagnostic: {}", diagnostic.message(&base));
    }

    DEBUG_TAGGING.set(Some(DebugTagging::default()));
    let mut allocator = Allocator::new();
    let kind = normalize_path(path).unwrap();
    let program = tree
        .main(&mut compiler, &mut allocator, &kind, base.clone())
        .unwrap()
        .expect("main");
    let tagging = DEBUG_TAGGING.take().unwrap();
    let program_bytes = node_to_bytes(&allocator, program).unwrap();

    let mut ctx = Ctx {
        compiler: &mut compiler,
        base: &base,
        builder: Builder::default(),
        tagging,
        declared_types: Vec::new(),
    };
    let shadow = ctx.tree(&allocator, program, None);
    let declared_types = std::mem::take(&mut ctx.declared_types);
    let builder = ctx.builder;
    let metadata = DebugMetadata {
        program_sha256: Sha256::digest(&program_bytes).into(),
        files: builder.files,
        strings: builder.strings,
        spans: builder.spans,
        functions: builder.functions,
        tree: shadow,
    };
    let sidecar = metadata.encode().unwrap();
    metadata.verify_program(&program_bytes).unwrap();
    let out = path.with_extension(if release { "release.sym.clvm" } else { "sym.clvm" });
    std::fs::write(&out, &sidecar).unwrap();

    println!(
        "program: {}",
        rue_clvm::disassemble_program(&allocator, program)
    );
    println!(
        "program bytes: {}, sidecar bytes: {}, spans: {}, functions: {}",
        program_bytes.len(),
        sidecar.len(),
        metadata.spans.len(),
        metadata.functions.len()
    );
    for (i, function) in metadata.functions.iter().enumerate() {
        println!(
            "function[{i}] {} params={:?}",
            metadata.strings[function.name],
            function.parameters
        );
    }
    println!("declared types: {}", declared_types.join(", "));

    probe_forward_compat(&sidecar);

    let env = rue_clvm::assemble(&mut allocator, env_text).unwrap();
    let dialect = DebugDialect::new(CLVM_FLAGS, false);
    let failure = match run_program_with_diagnostics(
        &mut allocator,
        &dialect,
        program,
        env,
        11_000_000_000,
        64,
    ) {
        Ok(reduction) => {
            println!(
                "UNEXPECTED SUCCESS: {}",
                rue_clvm::disassemble_value(&allocator, reduction.1)
            );
            return;
        }
        Err(failure) => failure,
    };
    let frames: Vec<SerializedFrame> = failure
        .frames
        .iter()
        .map(|frame| SerializedFrame {
            program: node_to_bytes(&allocator, frame.program).unwrap(),
            environment: node_to_bytes(&allocator, frame.environment).unwrap(),
        })
        .collect();
    println!(
        "error: {:?}  raw frames: {}  truncated: {}",
        failure.error,
        frames.len(),
        failure.truncated
    );

    let mut collection = DebugMetadataCollection::default();
    collection.insert(&sidecar).unwrap();
    for style in [StackFrameStyle::Python, StackFrameStyle::Lisp] {
        println!("----- {style:?} -----");
        match collection.format_captured_stack(&frames, failure.truncated, style) {
            Ok(text) => println!("{text}"),
            Err(error) => println!("RENDER ERROR: {error}"),
        }
    }
}

/// Try the additive changes a newer producer would make, and see whether a v1 reader accepts them.
fn probe_forward_compat(sidecar: &[u8]) {
    use clvmr::serde::node_from_bytes;
    fn list_items(a: &Allocator, mut node: NodePtr) -> Vec<NodePtr> {
        let mut items = Vec::new();
        while let SExp::Pair(first, rest) = a.sexp(node) {
            items.push(first);
            node = rest;
        }
        items
    }
    fn make_list(a: &mut Allocator, items: &[NodePtr]) -> NodePtr {
        let mut node = NodePtr::NIL;
        for &item in items.iter().rev() {
            node = a.new_pair(item, node).unwrap();
        }
        node
    }
    type Mutation = fn(&mut Allocator, &mut Vec<NodePtr>);
    let mutations: [(&str, Mutation); 6] = [
        ("extra trailing root field (e.g. producer info)", |a, root| {
            let extra = a.new_atom(b"rue 0.11.0").unwrap();
            root.push(extra);
        }),
        ("version 2", |a, root| root[1] = a.new_small_number(2).unwrap()),
        ("extra field on a shadow node (e.g. inlined-from)", |a, root| {
            let mut node = list_items(a, root[8]);
            let extra = a.new_small_number(1).unwrap();
            node.push(extra);
            root[8] = make_list(a, &node);
        }),
        ("extra field on a span entry (e.g. byte offsets)", |a, root| {
            let spans = list_items(a, root[6]);
            let mut first = list_items(a, spans[0]);
            let extra = a.new_small_number(0).unwrap();
            first.push(extra);
            let mut spans = spans.clone();
            spans[0] = make_list(a, &first);
            root[6] = make_list(a, &spans);
        }),
        ("extra field on a function record (e.g. definition span)", |a, root| {
            let functions = list_items(a, root[7]);
            let mut first = list_items(a, functions[0]);
            let extra = a.new_small_number(1).unwrap();
            first.push(extra);
            let mut functions = functions.clone();
            functions[0] = make_list(a, &first);
            root[7] = make_list(a, &functions);
        }),
        ("extra: unknown constraint tag 99 on first named parameter", |a, root| {
            // Rewrite the first `(1 name path (tag ...))` parameter record found in the function table.
            fn rewrite(a: &mut Allocator, node: NodePtr) -> Option<NodePtr> {
                let SExp::Pair(..) = a.sexp(node) else { return None };
                let items = list_items(a, node);
                if items.len() >= 4 && a.atom(items[0]).as_ref() == [1] {
                    if let SExp::Pair(..) = a.sexp(items[3]) {
                        let tag = a.new_small_number(99).unwrap();
                        let constraint = make_list(a, &[tag]);
                        let mut items = items.clone();
                        items[3] = constraint;
                        return Some(make_list(a, &items));
                    }
                }
                for (i, &item) in items.iter().enumerate() {
                    if let Some(new) = rewrite(a, item) {
                        let mut items = items.clone();
                        items[i] = new;
                        return Some(make_list(a, &items));
                    }
                }
                None
            }
            let functions = list_items(a, root[7]);
            let mut first = list_items(a, functions[0]);
            first[2] = rewrite(a, first[2]).expect("named parameter");
            let mut functions = functions.clone();
            functions[0] = make_list(a, &first);
            root[7] = make_list(a, &functions);
        }),
    ];
    for (label, mutate) in mutations {
        let mut a = Allocator::new();
        let node = node_from_bytes(&mut a, sidecar).unwrap();
        let mut root = list_items(&a, node);
        mutate(&mut a, &mut root);
        let node = make_list(&mut a, &root);
        let bytes = node_to_bytes(&a, node).unwrap();
        let result = DebugMetadata::decode(&bytes).map(|decoded| {
            let constraints: Vec<String> = decoded
                .functions
                .first()
                .map(|function| collect_constraints(&function.parameters))
                .unwrap_or_default();
            format!("accepted (first function constraints: {})", constraints.join(", "))
        });
        println!("probe: {label}: {result:?}");
    }
}

fn collect_constraints(parameter: &DebugParameter) -> Vec<String> {
    match parameter {
        DebugParameter::Nil => Vec::new(),
        DebugParameter::Name { constraint, .. } => vec![constraint.to_string()],
        DebugParameter::Pair(left, right) => {
            let mut result = collect_constraints(left);
            result.extend(collect_constraints(right));
            result
        }
    }
}
