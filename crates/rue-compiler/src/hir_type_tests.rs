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
                statement => statement.hirs().collect(),
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
              double<33>(value<36>): Int at `double(value)`
                double<33>: fn(value: Int) -> Int at `double`
                value<36>: Int at `value`
              (doubled<38> > 0): Bool at `doubled > 0`
                doubled<38>: Int at `doubled`
                0: 0 at `0`
              (list<37> is _): Bool at `list is (Int, List<Int>)`
                list<37>: List<Int> at `list`
              { ... }: Never at `{ return list.first; }`
                list<37>.first: Int at `list.first`
                  list<37>: (Int, List<Int>) at `list`
              true: true at `true`
              { x: [doubled<38>, 2].first, y: 0 }: Point at `Point { x: [doubled, 2].first }`
                [doubled<38>, 2].first: Int at `[doubled, 2].first`
                  [doubled<38>, 2]: (Int, (2, nil)) at `[doubled, 2]`
                    doubled<38>: Int at `doubled`
                    2: 2 at `2`
                0: 0 at `0`
              if (flag<39> && (nil == nil)) { ((doubled<38> + point<40>.y) as _) } else { (- doubled<38>) }: Int at `if flag && nil == nil { (doubled + point.y) as Int } else { -doubled }`
                (flag<39> && (nil == nil)): Bool at `flag && nil == nil`
                  flag<39>: true at `flag`
                  (nil == nil): Bool at `nil == nil`
                    nil: nil at `nil`
                    nil: nil at `nil`
                { ... }: Int at `{ (doubled + point.y) as Int }`
                  ((doubled<38> + point<40>.y) as _): Int at `(doubled + point.y) as Int`
                    (doubled<38> + point<40>.y): Int at `(doubled + point.y)`
                      doubled<38>: Int at `doubled`
                      point<40>.y: Int at `point.y`
                        point<40>: Point at `point`
                { ... }: Int at `{ -doubled }`
                  (- doubled<38>): Int at `-doubled`
                    doubled<38>: Int at `doubled`"#]],
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
              (map<33>(<39>, 5), 1): (Int, 1) at `(map(fn(x) => x, 5), 1)`
                map<33>(<39>, 5): Int at `map(fn(x) => x, 5)`
                  map<33>: fn<A, B>(function: fn(a: A) -> B, value: A) -> B at `map`
                  <39>: fn(x: Int) -> Int at `fn(x) => x`
                  5: 5 at `5`
                1: 1 at `1`
              pair<37>: (5, Int) | Any at `pair`"#]],
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
              [map<33>(<39>, 5)]: (Int, nil) at `[map(fn(x) => x, 5)]`
                map<33>(<39>, 5): Int at `map(fn(x) => x, 5)`
                  map<33>: fn<A, B>(function: fn(a: A) -> B, value: A) -> B at `map`
                  <39>: fn(x: Int) -> Int at `fn(x) => x`
                  5: 5 at `5`
              list<37>: List<5> | Any at `list`"#]],
    );
}
