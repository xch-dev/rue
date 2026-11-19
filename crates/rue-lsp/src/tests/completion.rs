use expect_test::{Expect, expect};

use super::TestProject;

#[allow(clippy::needless_pass_by_value)]
fn check(source: &str, expect: Expect) {
    let project = TestProject::new(&[("main.rue", source)]);
    expect.assert_eq(&project.completions());
}

#[test]
fn test_complete_locals() {
    check(
        r#"
const LIMIT: Int = 10;

fn main(amount: Int) -> Int {
    let total = amount + 1;
    $0
}

fn helper() -> Int {
    1
}
"#,
        expect![[r#"
            super [Module]
            project [Module]
            total [Variable] Int
            amount [Variable] Int
            LIMIT [Constant] Int
            main [Function] fn(amount: Int) -> Int
            helper [Function] fn() -> Int
            unchecked_cast [Function] fn<T>(value: Any) -> T
            sha256 [Function] fn(value: Bytes) -> Bytes32
            sha256_inline [Function] fn(value: Bytes) -> Bytes32
            keccak256 [Function] fn(value: Bytes) -> Bytes32
            keccak256_inline [Function] fn(value: Bytes) -> Bytes32
            concat [Function] fn(values: List<Bytes>) -> Bytes
            coinid [Function] fn(parent_coin_id: Bytes, puzzle_hash: Bytes, amount: Int) -> Bytes32
            substr [Function] fn(value: Bytes, start: Int) -> Bytes
            sum [Function] fn(values: List<Int>) -> Int
            difference [Function] fn(values: List<Int>) -> Int
            product [Function] fn(values: List<Int>) -> Int
            divmod [Function] fn(numerator: Int, denominator: Int) -> (Int, Int)
            modpow [Function] fn(base: Int, exponent: Int, modulus: Int) -> Int
            any [Function] fn(values: List<Bool>) -> Bool
            all [Function] fn(values: List<Bool>) -> Bool
            pubkey_for_exp [Function] fn(exponent: Int | Bytes32) -> PublicKey
            g1_sum [Function] fn(values: List<PublicKey>) -> PublicKey
            g1_difference [Function] fn(values: List<PublicKey>) -> PublicKey
            g1_map [Function] fn(data: Bytes) -> PublicKey
            g2_sum [Function] fn(values: List<Signature>) -> Signature
            g2_difference [Function] fn(values: List<Signature>) -> Signature
            g2_map [Function] fn(data: Bytes) -> Signature
            bls_pairing_identity [Function] fn(g1: PublicKey, g2: Signature, ...) -> nil
            bls_verify [Function] fn(signature: Signature, public_key: PublicKey, message: Bytes, ...) -> nil
            secp256k1_verify [Function] fn(public_key: K1PublicKey, message_hash: Bytes32, signature: K1Signature) -> nil
            secp256r1_verify [Function] fn(public_key: R1PublicKey, message_hash: Bytes32, signature: R1Signature) -> nil
            INFINITY_G1 [Constant] PublicKey
            INFINITY_G2 [Constant] Signature
        "#]],
    );
}

#[test]
fn test_complete_partial_identifier() {
    check(
        r#"
fn main(amount: Int) -> Int {
    let total = amount + 1;
    let other = 2;
    tot$0
}
"#,
        expect![[r#"
            total [Variable] Int
        "#]],
    );
}

#[test]
fn test_complete_types() {
    check(
        r#"
struct Point {
    x: Int,
    y: Int,
}

type Price = Int;

fn main(value: P$0) -> Int {
    1
}
"#,
        expect![[r#"
            project [Module]
            Point [Struct]
            Price [Struct] Int
            PublicKey [Struct]
            super [Module]
            K1PublicKey [Struct]
            R1PublicKey [Struct]
        "#]],
    );
}

#[test]
fn test_complete_empty_type() {
    check(
        r#"
struct Point {
    x: Int,
    y: Int,
}

fn main(value: $0) -> Int {
    1
}
"#,
        expect![[r#"
            super [Module]
            project [Module]
            Point [Struct]
            Atom [Struct]
            Bytes [Struct]
            Bytes32 [Struct]
            String [Struct]
            PublicKey [Struct]
            Signature [Struct]
            K1PublicKey [Struct]
            K1Signature [Struct]
            R1PublicKey [Struct]
            R1Signature [Struct]
            Int [Struct]
            Bool [Struct]
            Any [Struct]
            List [Struct] nil | (T, List<T>)
            AlternatingList [Struct] nil | (A, (B, AlternatingList<A, B>))
        "#]],
    );
}

#[test]
fn test_complete_struct_fields() {
    check(
        r#"
struct Point {
    x: Int,
    y: Int,
}

fn main(point: Point) -> Int {
    point.$0
}
"#,
        expect![[r#"
            x [Field] Int
            y [Field] Int
        "#]],
    );
}

#[test]
fn test_complete_struct_initializer_fields() {
    check(
        r#"
struct Point {
    x: Int,
    y: Int,
    z: Int,
}

fn main() -> Point {
    Point { x: 1, $0 }
}
"#,
        expect![[r#"
            y [Field] Int
            z [Field] Int
        "#]],
    );
}

#[test]
fn test_complete_pair_fields() {
    check(
        r#"
fn main(pair: (Int, Bytes)) -> Int {
    pair.$0
}
"#,
        expect![[r#"
            first [Field] Int
            rest [Field] Bytes
        "#]],
    );
}

#[test]
fn test_complete_atom_fields() {
    check(
        r#"
fn main(value: Bytes) -> Int {
    value.$0
}
"#,
        expect![[r#"
            length [Field] Int
        "#]],
    );
}

#[test]
fn test_complete_module_exports() {
    check(
        r#"
mod inner {
    export const NUM: Int = 42;
    export fn double(num: Int) -> Int {
        num * 2
    }
    const PRIVATE: Int = 1;
}

fn main() -> Int {
    inner::$0
}
"#,
        expect![[r#"
            NUM [Constant] Int
            double [Function] fn(num: Int) -> Int
        "#]],
    );
}

#[test]
fn test_complete_after_single_colon() {
    check(
        r#"
fn main() -> Int {
    let value:$0 Int = 1;
    value
}
"#,
        expect![[r#"
            <none>
        "#]],
    );
}

#[test]
fn test_complete_start_of_file() {
    check(
        r#"$0
fn main() -> Int {
    1
}
"#,
        expect![[r#"
            <none>
        "#]],
    );
}

#[test]
fn test_complete_empty_return_type() {
    check(
        r#"
struct Point {
    x: Int,
}

fn main() -> $0 {
    1
}
"#,
        expect![[r#"
            super [Module]
            project [Module]
            Point [Struct]
            Atom [Struct]
            Bytes [Struct]
            Bytes32 [Struct]
            String [Struct]
            PublicKey [Struct]
            Signature [Struct]
            K1PublicKey [Struct]
            K1Signature [Struct]
            R1PublicKey [Struct]
            R1Signature [Struct]
            Int [Struct]
            Bool [Struct]
            Any [Struct]
            List [Struct] nil | (T, List<T>)
            AlternatingList [Struct] nil | (A, (B, AlternatingList<A, B>))
        "#]],
    );
}

#[test]
fn test_complete_empty_let_type() {
    check(
        r#"
struct Point {
    x: Int,
}

fn main() -> Int {
    let value: $0 = 1;
    value
}
"#,
        expect![[r#"
            super [Module]
            project [Module]
            Point [Struct]
            Atom [Struct]
            Bytes [Struct]
            Bytes32 [Struct]
            String [Struct]
            PublicKey [Struct]
            Signature [Struct]
            K1PublicKey [Struct]
            K1Signature [Struct]
            R1PublicKey [Struct]
            R1Signature [Struct]
            Int [Struct]
            Bool [Struct]
            Any [Struct]
            List [Struct] nil | (T, List<T>)
            AlternatingList [Struct] nil | (A, (B, AlternatingList<A, B>))
        "#]],
    );
}

#[test]
fn test_complete_empty_cast_type() {
    check(
        r#"
struct Point {
    x: Int,
}

fn main(value: Any) -> Int {
    value as $0
}
"#,
        expect![[r#"
            super [Module]
            project [Module]
            Point [Struct]
            Atom [Struct]
            Bytes [Struct]
            Bytes32 [Struct]
            String [Struct]
            PublicKey [Struct]
            Signature [Struct]
            K1PublicKey [Struct]
            K1Signature [Struct]
            R1PublicKey [Struct]
            R1Signature [Struct]
            Int [Struct]
            Bool [Struct]
            Any [Struct]
            List [Struct] nil | (T, List<T>)
            AlternatingList [Struct] nil | (A, (B, AlternatingList<A, B>))
        "#]],
    );
}

#[test]
fn test_complete_empty_type_alias() {
    check(
        r#"
struct Point {
    x: Int,
}

type Alias = $0;
"#,
        expect![[r#"
            super [Module]
            project [Module]
            Point [Struct]
            Alias [Struct] {unresolved}
            Atom [Struct]
            Bytes [Struct]
            Bytes32 [Struct]
            String [Struct]
            PublicKey [Struct]
            Signature [Struct]
            K1PublicKey [Struct]
            K1Signature [Struct]
            R1PublicKey [Struct]
            R1Signature [Struct]
            Int [Struct]
            Bool [Struct]
            Any [Struct]
            List [Struct] nil | (T, List<T>)
            AlternatingList [Struct] nil | (A, (B, AlternatingList<A, B>))
        "#]],
    );
}

#[test]
fn test_complete_struct_initializer_value() {
    // A colon in a struct initializer is followed by an expression, so types aren't suggested.
    // TODO: Suggest expressions here, which also have no completion context until one is typed.
    check(
        r#"
struct Point {
    x: Int,
}

fn main(num: Int) -> Point {
    Point { x: $0 }
}
"#,
        expect![[r#"
            <none>
        "#]],
    );
}

#[test]
fn test_complete_function_signature_names_types() {
    // The detail of a function is its signature as hover shows it, with the names of its types.
    check(
        r#"
type Amount = Int;

fn main(flag: Bool) -> Amount {
    con$0
}

fn convert<T>(flag: Bool, value: T, amount: Amount) -> T {
    value
}
"#,
        expect![[r#"
            convert [Function] fn<T>(flag: Bool, value: T, amount: Amount) -> T
            concat [Function] fn(values: List<Bytes>) -> Bytes
            coinid [Function] fn(parent_coin_id: Bytes, puzzle_hash: Bytes, amount: Int) -> Bytes32
        "#]],
    );
}

#[test]
fn test_complete_a_module_and_a_function_with_the_same_name() {
    // Modules have their own namespace, so both are offered
    check(
        r#"
mod helper {
    export const NUM: Int = 1;
}

fn helper() -> Int {
    helper::NUM
}

fn main() -> Int {
    help$0
}
"#,
        expect![[r#"
            helper [Function] fn() -> Int
            helper [Module]
        "#]],
    );
}

#[test]
fn test_complete_module_exports_with_a_module_and_a_function_with_the_same_name() {
    check(
        r#"
mod outer {
    export mod helper {}

    export fn helper() -> Int {
        1
    }
}

fn main() -> Int {
    outer::$0
}
"#,
        expect![[r#"
            helper [Function] fn() -> Int
            helper [Module]
        "#]],
    );
}

#[test]
fn test_complete_in_an_inline_module() {
    // An inline module doesn't see the items of the file it's in, only its own and the prelude
    check(
        r#"
fn outer_helper() -> Int {
    1
}

mod inner {
    fn inner_helper() -> Int {
        2
    }

    fn main() -> Int {
        _helper$0
    }
}
"#,
        expect![[r#"
            inner_helper [Function] fn() -> Int
        "#]],
    );
}

#[test]
fn test_complete_super_members() {
    // `super` names the module that contains the code, so its private items are offered too
    check(
        r#"
const PRIVATE: Int = 1;

export const PUBLIC: Int = 2;

mod inner {
    fn main() -> Int {
        super::$0
    }
}
"#,
        expect![[r#"
            super [Module]
            PRIVATE [Constant] Int
            PUBLIC [Constant] Int
            inner [Module]
        "#]],
    );
}

#[test]
fn test_complete_project_members() {
    // `project` names the project's directory from anywhere in it, even an inline module, so its
    // files are offered (rather than the items of the file), but not `super`, since the root has
    // no parent
    check(
        r#"
const PRIVATE: Int = 1;

mod outer {
    mod inner {
        fn main() -> Int {
            project::$0
        }
    }
}
"#,
        expect![[r#"
            main [Module]
        "#]],
    );
}

#[test]
fn test_complete_project_members_in_a_project() {
    // In a project, `project` is the root directory, whose files are offered, and only the exported
    // items of a file after it
    let project = TestProject::new(&[
        (
            "main.rue",
            r#"
fn main() -> Int {
    project::utils::$0
}
"#,
        ),
        (
            "utils.rue",
            "export const ONE: Int = 1;\nconst TWO: Int = 2;\n",
        ),
    ]);
    expect![[r#"
        ONE [Constant] Int
    "#]]
    .assert_eq(&project.completions());

    let project = TestProject::new(&[
        (
            "main.rue",
            r#"
fn main() -> Int {
    project::$0
}
"#,
        ),
        ("utils.rue", "export const ONE: Int = 1;\n"),
    ]);
    expect![[r#"
        main [Module]
        utils [Module]
    "#]]
    .assert_eq(&project.completions());
}

#[test]
fn test_complete_super_members_in_a_type() {
    // A file's sibling files and private types are offered after `super::` in a type too
    let project = TestProject::new(&[
        (
            "main.rue",
            r#"
type Private = Int;

mod inner {
    fn main() -> Int {
        let x: super::$0 = 1;
        x
    }
}
"#,
        ),
        ("utils.rue", "export const ONE: Int = 1;\n"),
    ]);
    expect![[r#"
        super [Module]
        inner [Module]
        utils [Module]
        Private [Struct] Int
    "#]]
    .assert_eq(&project.completions());
}
