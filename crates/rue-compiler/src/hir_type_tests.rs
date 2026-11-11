use std::sync::Arc;

use expect_test::{Expect, expect};
use rue_diagnostic::{Source, SourceKind};
use rue_hir::{Hir, HirId, Statement, Symbol};
use rue_options::CompilerOptions;

use crate::{Compiler, File, FileTree};

/// Compiles the source, and lists every HIR node in the body of `main` along with its type and
/// the source text it was compiled from.
fn check(source: &str, expect: &Expect) {
    let mut ctx = Compiler::new(CompilerOptions {
        std: false,
        ..CompilerOptions::default()
    });

    let file = File::new(
        &mut ctx,
        "main".to_string(),
        Source::new(Arc::from(source), SourceKind::File("main.rue".to_string())),
    );
    let module = file.module;

    FileTree::File(file).compile(&mut ctx);

    assert!(!ctx.has_errors(), "{:?}", ctx.take_diagnostics());

    let main = ctx.scope(ctx.module(module).scope).symbol("main").unwrap();

    let Symbol::Function(function) = ctx.symbol(main).clone() else {
        panic!("main isn't a function");
    };

    let mut lines = Vec::new();
    list_types(&mut ctx, function.body, 0, &mut lines);

    expect.assert_eq(&lines.join("\n"));
}

fn list_types(ctx: &mut Compiler, hir: HirId, depth: usize, lines: &mut Vec<String>) {
    let ty = ctx
        .hir_type(hir)
        .map_or("-".to_string(), |ty| ctx.type_name(ty));

    let text = ctx.hir_srcloc(hir).map_or("-".to_string(), |srcloc| {
        let text = &srcloc.source.text[srcloc.span.clone()];
        format!("`{}`", text.split_whitespace().collect::<Vec<_>>().join(" "))
    });

    let hir_kind = match ctx.hir(hir) {
        Hir::Block(_) => "{ ... }".to_string(),
        _ => ctx.debug_hir(hir),
    };

    lines.push(format!("{}{hir_kind}: {ty} at {text}", "  ".repeat(depth)));

    let children = match ctx.hir(hir).clone() {
        Hir::Block(block) => block
            .statements
            .iter()
            .flat_map(|statement| match statement {
                Statement::Let(symbol) => match ctx.symbol(*symbol) {
                    Symbol::Binding(binding) => vec![binding.value.hir],
                    _ => vec![],
                },
                Statement::Expr(statement) => vec![statement.hir],
                Statement::If(statement) => vec![statement.condition, statement.then],
                Statement::Return(hir) | Statement::Assert(hir, _) | Statement::Debug(hir, _) => {
                    vec![*hir]
                }
                Statement::Raise(hir, _) => hir.iter().copied().collect(),
            })
            .chain(block.body)
            .collect(),
        Hir::Pair(first, rest) => vec![first, rest],
        Hir::If(condition, then, else_, _) => vec![condition, then, else_],
        Hir::FunctionCall(call) => [call.function].into_iter().chain(call.args).collect(),
        Hir::Unary(_, hir) => vec![hir],
        Hir::Binary(_, left, right) => vec![left, right],
        _ => vec![],
    };

    for child in children {
        list_types(ctx, child, depth + 1, lines);
    }
}

#[test]
fn records_expression_types_and_locations() {
    check(
        r#"
        fn double(value: Int) -> Int {
            value * 2
        }

        fn main(value: Int, list: List<Int>) -> Int {
            let doubled = double(value);
            assert doubled > 0;

            if list is (Int, List<Int>) {
                return list.first;
            }

            let flag = true;

            if flag && nil == nil {
                (doubled + 1) as Int
            } else {
                -doubled
            }
        }
        "#,
        &expect![[r#"
            { ... }: Int at `{ let doubled = double(value); assert doubled > 0; if list is (Int, List<Int>) { return list.first; } let flag = true; if flag && nil == nil { (doubled + 1) as Int } else { -doubled } }`
              double<30>(value<33>): Int at `double(value)`
                double<30>: fn(value: Int) -> Int at `double`
                value<33>: Int at `value`
              (doubled<35> > 0): Bool at `doubled > 0`
                doubled<35>: Int at `doubled`
                0: 0 at `0`
              (listp list<34>): Bool at `list is (Int, List<Int>)`
                list<34>: List<Int> at `list`
              { ... }: Never at `{ return list.first; }`
                (first list<34>): Int at `list.first`
                  list<34>: (Int, List<Int>) at `list`
              true: true at `true`
              if (flag<36> && (nil == nil)) { (doubled<35> + 1) } else { (- doubled<35>) }: Int at `if flag && nil == nil { (doubled + 1) as Int } else { -doubled }`
                (flag<36> && (nil == nil)): Bool at `flag && nil == nil`
                  flag<36>: true at `flag`
                  (nil == nil): Bool at `nil == nil`
                    nil: nil at `nil`
                    nil: nil at `nil`
                { ... }: Int at `{ (doubled + 1) as Int }`
                  (doubled<35> + 1): Int at `(doubled + 1) as Int`
                    doubled<35>: Int at `doubled`
                    1: 1 at `1`
                { ... }: Int at `{ -doubled }`
                  (- doubled<35>): Int at `-doubled`
                    doubled<35>: Int at `doubled`"#]],
    );
}
