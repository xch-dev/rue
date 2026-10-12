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
            LIMIT [Constant] Int
            main [Function] fn(amount: Int) -> Int
            helper [Function] fn() -> Int
            amount [Variable] Int
            total [Variable] Int
            unchecked_cast [Function] fn(value: Any) -> T
            sha256 [Function] {unresolved}
            sha256_inline [Function] {unresolved}
            keccak256 [Function] {unresolved}
            keccak256_inline [Function] {unresolved}
            concat [Function] {unresolved}
            coinid [Function] {unresolved}
            substr [Function] {unresolved}
            sum [Function] {unresolved}
            difference [Function] {unresolved}
            product [Function] {unresolved}
            divmod [Function] {unresolved}
            modpow [Function] {unresolved}
            any [Function] {unresolved}
            all [Function] {unresolved}
            pubkey_for_exp [Function] {unresolved}
            g1_sum [Function] {unresolved}
            g1_difference [Function] {unresolved}
            g1_map [Function] {unresolved}
            g2_sum [Function] {unresolved}
            g2_difference [Function] {unresolved}
            g2_map [Function] {unresolved}
            bls_pairing_identity [Function] {unresolved}
            bls_verify [Function] {unresolved}
            secp256k1_verify [Function] {unresolved}
            secp256r1_verify [Function] {unresolved}
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
    // There's no type node to provide a completion context until something has been typed.
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
            <none>
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
