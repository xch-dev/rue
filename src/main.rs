use std::fs;

use anyhow::Result;
use clvm_tools_rs::classic::clvm_tools::binutils::{assemble, disassemble};
use clvmr::{Allocator, ChiaDialect, run_program};
use id_arena::Arena;
use rue_ast::{AstDocument, AstNode};
use rue_compiler::{Compiler, compile_document, declare_document};
use rue_hir::{DependencyGraph, Scope, lower_main};
use rue_lexer::Lexer;
use rue_lir::codegen;
use rue_mir::lower_mir;
use rue_parser::Parser;
use serde::{Deserialize, Serialize};
use walkdir::WalkDir;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct TestCase {
    program: Option<String>,
    solution: Option<String>,
    output: Option<String>,
    errors: Vec<String>,
}

fn main() -> Result<()> {
    for entry in WalkDir::new("tests") {
        let entry = entry?;

        if let Some(name) = entry.file_name().to_str().unwrap().strip_suffix(".rue") {
            if !entry
                .path()
                .parent()
                .unwrap()
                .join(format!("{name}.yaml"))
                .try_exists()?
            {
                println!("Skipping {name} because it doesn't have a test case");
            }
            continue;
        };

        let Some(name) = entry.file_name().to_str().unwrap().strip_suffix(".yaml") else {
            continue;
        };

        let mut test_case: TestCase = serde_yml::from_str(&fs::read_to_string(entry.path())?)?;
        let original = test_case.clone();

        let source =
            fs::read_to_string(entry.path().parent().unwrap().join(format!("{name}.rue")))?;

        let tokens = Lexer::new(&source).collect::<Vec<_>>();
        let parser = Parser::new(&source, tokens);
        let result = parser.parse();

        let mut errors = Vec::new();

        for error in result.errors {
            errors.push(error.message(&source));
        }

        let ast = AstDocument::cast(result.node).unwrap();

        let mut ctx = Compiler::new();

        let scope = ctx.alloc_scope(Scope::new());
        let declarations = declare_document(&mut ctx, scope, &ast);
        compile_document(&mut ctx, scope, &ast, declarations);

        for error in ctx.errors() {
            errors.push(error.message(&source));
        }

        let symbol = ctx.scope(scope).symbol("main").unwrap();
        let graph = DependencyGraph::build(&ctx, symbol);
        let mut mir_arena = Arena::new();
        let mir = lower_main(&ctx, &mut mir_arena, &graph, symbol);

        let mut allocator = Allocator::new();
        let mut arena = Arena::new();
        let lir = lower_mir(&mir_arena, &mut arena, mir, None);
        let ptr = codegen(&arena, &mut allocator, lir)?;

        let env = assemble(&mut allocator, test_case.solution.as_ref().unwrap()).unwrap();

        let output = run_program(&mut allocator, &ChiaDialect::new(0), ptr, env, u64::MAX)
            .unwrap()
            .1;

        test_case.program = Some(disassemble(&allocator, ptr, None));
        test_case.output = Some(disassemble(&allocator, output, None));
        test_case.errors = errors;

        if test_case != original {
            println!("{name} failed, updated test case");
            fs::write(entry.path(), serde_yml::to_string(&test_case)?)?;
        }
    }

    Ok(())
}
