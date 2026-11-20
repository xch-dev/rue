//! Enums: they compile to the same program as the tagged union of structs they stand for.

use crate::test_utils::compile;

/// The disassembled `main` of the source, which must compile without errors.
fn main_program(source: &str) -> String {
    compile(source).main_program()
}

/// Uses a value of `Value` in every way the program can, so that the comparison covers creating
/// each variant, narrowing to it, and reading its fields.
const USES: &str = "
fn main(value: Value, flag: Bool) -> (Int, (Value, (Value, Value))) {
    let total = if value is Empty {
        0
    } else if value is Single {
        value.first
    } else {
        value.first + value.second
    };
    (total, (EMPTY_VALUE, (Single { first: 1 }, Double { first: 2, second: if flag { 3 } else { 4 } })))
}
";

#[test]
fn an_enum_compiles_like_the_structs_it_stands_for() {
    let structs = format!(
        "
inline const EMPTY: 0 = 0;
inline const SINGLE: 1 = 1;
inline const DOUBLE: 7 = 7;

struct Empty {{ kind = EMPTY }}
struct Single {{ kind = SINGLE, first: Int }}
struct Double {{ kind = DOUBLE, first: Int, second: Int }}

type Value = Empty | Single | Double;

inline const EMPTY_VALUE: Empty = Empty {{}};
{USES}"
    );

    let enums = format!(
        "
enum Value {{
    Empty,
    Single {{ first: Int }},
    Double = 7 {{ first: Int, second: Int }},
}}

import Value::*;

inline const EMPTY_VALUE: Empty = Empty;
{USES}"
    );

    assert_eq!(main_program(&enums), main_program(&structs));
}

#[test]
fn an_enum_without_fields_compiles_like_its_discriminants() {
    let atoms = "
type Mode = 0 | 1 | 5;

fn main(mode: Mode) -> (Bool, Mode) {
    (mode == 5 || mode == 1, 1)
}
";

    let enums = "
enum Mode {
    Locked,
    Unlocked,
    Open = 5,
}

fn main(mode: Mode) -> (Bool, Mode) {
    (mode is Mode::Open || mode is Mode::Unlocked, Mode::Unlocked)
}
";

    assert_eq!(main_program(enums), main_program(atoms));
}

#[test]
fn a_match_on_an_enum_compiles_like_its_if_chain() {
    let shape = "
enum Shape {
    Empty,
    Circle { radius: Int },
    Rect = 10 { width: Int, height: Int },
}
";

    let chain = format!(
        "{shape}
fn main(shape: Shape) -> Int {{
    if shape is Shape::Empty {{
        0
    }} else if shape is Shape::Circle {{
        shape.radius
    }} else {{
        shape.width * shape.height
    }}
}}
"
    );

    let matched = format!(
        "{shape}
fn main(shape: Shape) -> Int {{
    match shape {{
        Shape::Empty => 0,
        Shape::Circle => shape.radius,
        Shape::Rect => shape.width * shape.height,
    }}
}}
"
    );

    assert_eq!(main_program(&matched), main_program(&chain));
}
