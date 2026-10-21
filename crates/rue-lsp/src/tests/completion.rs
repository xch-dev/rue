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
