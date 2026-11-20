use expect_test::{Expect, expect};
use rue_hir::{Hir, HirId, Statement, Symbol};

use crate::Compiler;
use crate::test_utils::compile;

/// Compiles the source, and lists every HIR node in the body of `main` along with its type and
/// the source text it was compiled from.
fn check(source: &str, expect: &Expect) {
    let mut compiled = compile(source);
    compiled.assert_no_errors();

    let body = compiled.function_body("main");

    let mut lines = Vec::new();
    list_types(&mut compiled.ctx, body, 0, &mut lines);

    expect.assert_eq(&lines.join("\n"));
}

fn list_types(ctx: &mut Compiler, hir: HirId, depth: usize, lines: &mut Vec<String>) {
    let ty = ctx
        .hir_type(hir)
        .map_or("-".to_string(), |ty| ctx.type_name(ty));

    let text = ctx.hir_srcloc(hir).map_or("-".to_string(), |srcloc| {
        let text = &srcloc.source.text[srcloc.span.clone()];
        format!(
            "`{}`",
            text.split_whitespace().collect::<Vec<_>>().join(" ")
        )
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
                Statement::Let(symbol, _) => match ctx.symbol(*symbol) {
                    Symbol::Binding(binding) => vec![binding.value.hir],
                    _ => vec![],
                },
                Statement::Expr(statement) => vec![statement.hir],
                Statement::Control(statement) => vec![statement.hir],
                Statement::If(statement) => vec![statement.condition, statement.then],
                Statement::Return(hir, _)
                | Statement::Assert(hir, _)
                | Statement::Debug(hir, _) => {
                    vec![*hir]
                }
                Statement::Raise(hir, _) => hir.iter().copied().collect(),
            })
            .chain(block.body)
            .collect(),
        Hir::Pair(first, rest) => vec![first, rest],
        Hir::List(list) => list.items,
        Hir::Struct(initializer) => initializer
            .fields
            .into_iter()
            .map(|field| field.value)
            .collect(),
        Hir::FieldAccess(access) => vec![access.value],
        Hir::Cast(hir) => vec![hir],
        Hir::Guard(guard) => vec![guard.value],
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
        struct Point {
            x: Int,
            y: Int = 0,
        }

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
            let point = Point { x: [doubled, 2].first };

            if flag && nil == nil {
                (doubled + point.y) as Int
            } else {
                -doubled
            }
        }
        "#,
        &expect![[r#"
            { ... }: Int at `{ let doubled = double(value); assert doubled > 0; if list is (Int, List<Int>) { return list.first; } let flag = true; let point = Point { x: [doubled, 2].first }; if flag && nil == nil { (doubled + point.y) as Int } else { -doubled } }`
              double<31>(value<34>): Int at `double(value)`
                double<31>: fn(value: Int) -> Int at `double`
                value<34>: Int at `value`
              (doubled<36> > 0): Bool at `doubled > 0`
                doubled<36>: Int at `doubled`
                0: 0 at `0`
              (list<35> is _): Bool at `list is (Int, List<Int>)`
                list<35>: List<Int> at `list`
              { ... }: Never at `{ return list.first; }`
                list<35>.first: Int at `list.first`
                  list<35>: (Int, List<Int>) at `list`
              true: true at `true`
              { x: [doubled<36>, 2].first, y: 0 }: Point at `Point { x: [doubled, 2].first }`
                [doubled<36>, 2].first: Int at `[doubled, 2].first`
                  [doubled<36>, 2]: (Int, (2, nil)) at `[doubled, 2]`
                    doubled<36>: Int at `doubled`
                    2: 2 at `2`
                0: 0 at `0`
              if (flag<37> && (nil == nil)) { ((doubled<36> + point<38>.y) as _) } else { (- doubled<36>) }: Int at `if flag && nil == nil { (doubled + point.y) as Int } else { -doubled }`
                (flag<37> && (nil == nil)): Bool at `flag && nil == nil`
                  flag<37>: true at `flag`
                  (nil == nil): Bool at `nil == nil`
                    nil: nil at `nil`
                    nil: nil at `nil`
                { ... }: Int at `{ (doubled + point.y) as Int }`
                  ((doubled<36> + point<38>.y) as _): Int at `(doubled + point.y) as Int`
                    (doubled<36> + point<38>.y): Int at `(doubled + point.y)`
                      doubled<36>: Int at `doubled`
                      point<38>.y: Int at `point.y`
                        point<38>: Point at `point`
                { ... }: Int at `{ -doubled }`
                  (- doubled<36>): Int at `-doubled`
                    doubled<36>: Int at `doubled`"#]],
    );
}

#[test]
fn call_in_a_pair_expected_to_be_any_keeps_its_type() {
    // The `Any` member could hold the pair as any other pair, so the first element's expected `5`
    // is only a hint, and the call isn't solved again for it (#362, step 13e)
    check(
        "
        fn map<A, B>(function: fn(a: A) -> B, value: A) -> B {
            function(value)
        }

        fn main() -> Any {
            let pair: (5, Int) | Any = (map(fn(x) => x, 5), 1);
            pair
        }
        ",
        &expect![[r#"
            { ... }: (5, Int) | Any at `{ let pair: (5, Int) | Any = (map(fn(x) => x, 5), 1); pair }`
              (map<31>(<37>, 5), 1): (Int, 1) at `(map(fn(x) => x, 5), 1)`
                map<31>(<37>, 5): Int at `map(fn(x) => x, 5)`
                  map<31>: fn<A, B>(function: fn(a: A) -> B, value: A) -> B at `map`
                  <37>: fn(x: Int) -> Int at `fn(x) => x`
                  5: 5 at `5`
                1: 1 at `1`
              pair<35>: (5, Int) | Any at `pair`"#]],
    );
}

#[test]
fn call_in_a_list_expected_to_be_any_keeps_its_type() {
    // The same for a list's items
    check(
        "
        fn map<A, B>(function: fn(a: A) -> B, value: A) -> B {
            function(value)
        }

        fn main() -> Any {
            let list: List<5> | Any = [map(fn(x) => x, 5)];
            list
        }
        ",
        &expect![[r#"
            { ... }: List<5> | Any at `{ let list: List<5> | Any = [map(fn(x) => x, 5)]; list }`
              [map<31>(<37>, 5)]: (Int, nil) at `[map(fn(x) => x, 5)]`
                map<31>(<37>, 5): Int at `map(fn(x) => x, 5)`
                  map<31>: fn<A, B>(function: fn(a: A) -> B, value: A) -> B at `map`
                  <37>: fn(x: Int) -> Int at `fn(x) => x`
                  5: 5 at `5`
              list<35>: List<5> | Any at `list`"#]],
    );
}
