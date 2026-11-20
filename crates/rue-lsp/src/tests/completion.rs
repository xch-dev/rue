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
            keccak256_verify [Function] fn(value: Bytes, hash: Bytes32) -> nil
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
fn test_complete_index_fields() {
    check(
        r#"
fn main(value: Bytes) -> Int {
    value[0].$0
}
"#,
        expect![[r#"
            length [Field] Int
        "#]],
    );
}

#[test]
fn test_complete_slice_fields() {
    check(
        r#"
fn main(value: Bytes) -> Int {
    value[1..].$0
}
"#,
        expect![[r#"
            length [Field] Int
        "#]],
    );
}

#[test]
fn test_complete_fields_in_slice_start() {
    check(
        r#"
struct Range {
    start: Int,
    end: Int,
}

fn main(value: Bytes, range: Range) -> Bytes {
    value[range.$0..range.end]
}
"#,
        expect![[r#"
            start [Field] Int
            end [Field] Int
        "#]],
    );
}

#[test]
fn test_complete_in_empty_index() {
    check(
        r#"
fn main(value: Bytes, index: Int) -> Bytes {
    value[$0]
}
"#,
        expect![[r#"
            super [Module]
            project [Module]
            value [Variable] Bytes
            index [Variable] Int
            main [Function] fn(value: Bytes, index: Int) -> Bytes
            unchecked_cast [Function] fn<T>(value: Any) -> T
            sha256 [Function] fn(value: Bytes) -> Bytes32
            sha256_inline [Function] fn(value: Bytes) -> Bytes32
            keccak256 [Function] fn(value: Bytes) -> Bytes32
            keccak256_inline [Function] fn(value: Bytes) -> Bytes32
            concat [Function] fn(values: List<Bytes>) -> Bytes
            coinid [Function] fn(parent_coin_id: Bytes, puzzle_hash: Bytes, amount: Int) -> Bytes32
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
            keccak256_verify [Function] fn(value: Bytes, hash: Bytes32) -> nil
            INFINITY_G1 [Constant] PublicKey
            INFINITY_G2 [Constant] Signature
        "#]],
    );
}

#[test]
fn test_complete_slice_end() {
    check(
        r#"
fn main(value: Bytes, index: Int) -> Bytes {
    value[index..$0]
}
"#,
        expect![[r#"
            super [Module]
            project [Module]
            value [Variable] Bytes
            index [Variable] Int
            main [Function] fn(value: Bytes, index: Int) -> Bytes
            unchecked_cast [Function] fn<T>(value: Any) -> T
            sha256 [Function] fn(value: Bytes) -> Bytes32
            sha256_inline [Function] fn(value: Bytes) -> Bytes32
            keccak256 [Function] fn(value: Bytes) -> Bytes32
            keccak256_inline [Function] fn(value: Bytes) -> Bytes32
            concat [Function] fn(values: List<Bytes>) -> Bytes
            coinid [Function] fn(parent_coin_id: Bytes, puzzle_hash: Bytes, amount: Int) -> Bytes32
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
            keccak256_verify [Function] fn(value: Bytes, hash: Bytes32) -> nil
            INFINITY_G1 [Constant] PublicKey
            INFINITY_G2 [Constant] Signature
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
fn test_complete_module_exports_skip_failed_imports() {
    // A failed import's name is bound to an error item, which isn't anything that could be used,
    // even when the import is exported
    check(
        r#"
mod inner {
    export nope::Gone;
    export const NUM: Int = 42;
}

fn main() -> Int {
    inner::$0
}
"#,
        expect![[r#"
            NUM [Constant] Int
        "#]],
    );
}

#[test]
fn test_complete_types_skip_failed_imports() {
    check(
        r#"
import nope::Missing;

fn main(value: Mis$0) -> Int {
    1
}
"#,
        expect![[r#"
            <none>
        "#]],
    );
}

#[test]
fn test_complete_scope_skips_failed_imports() {
    check(
        r#"
import nope::Missing;

fn main() -> Int {
    Mis$0
}
"#,
        expect![[r#"
            <none>
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
fn test_complete_super_members_skip_sibling_files_hidden_by_failed_imports() {
    // A failed import of `utils` binds the name in the file to an error item, which hides the
    // sibling file, so neither is offered after `super::`
    let project = TestProject::new(&[
        (
            "main.rue",
            r#"
import nope::utils;

mod inner {
    fn main() -> Int {
        super::$0
    }
}
"#,
        ),
        ("utils.rue", "export const ONE: Int = 1;\n"),
    ]);
    expect![[r#"
        super [Module]
        inner [Module]
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

#[test]
fn test_complete_match_arm_pattern() {
    check(
        r#"
struct Point {
    x: Int,
}

type Shape = Point | nil;

fn main(value: Shape) -> Int {
    match value {
        Po$0
    }
}
"#,
        expect![[r#"
            Point [Struct]
            project [Module]
        "#]],
    );
}

#[test]
fn test_complete_empty_match_arm_pattern() {
    check(
        r#"
struct Point {
    x: Int,
}

fn main(value: Point | nil) -> Int {
    match value {
        nil => 0,
        $0
    }
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
fn test_complete_match_arm_value() {
    check(
        r#"
struct Point {
    x: Int,
}

fn main(value: Point | nil) -> Int {
    match value {
        Point => val$0,
        nil => 0,
    }
}
"#,
        expect![[r#"
            value [Variable] Point
        "#]],
    );
}

#[test]
fn test_complete_empty_match_arm_value() {
    check(
        r#"
fn main(value: Int | nil) -> Int {
    let other = 1;
    match value {
        Int => $0
    }
}
"#,
        expect![[r#"
            super [Module]
            project [Module]
            other [Variable] 1
            value [Variable] Int | nil
            main [Function] fn(value: Int | nil) -> Int
            unchecked_cast [Function] fn<T>(value: Any) -> T
            sha256 [Function] fn(value: Bytes) -> Bytes32
            sha256_inline [Function] fn(value: Bytes) -> Bytes32
            keccak256 [Function] fn(value: Bytes) -> Bytes32
            keccak256_inline [Function] fn(value: Bytes) -> Bytes32
            concat [Function] fn(values: List<Bytes>) -> Bytes
            coinid [Function] fn(parent_coin_id: Bytes, puzzle_hash: Bytes, amount: Int) -> Bytes32
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
            keccak256_verify [Function] fn(value: Bytes, hash: Bytes32) -> nil
            INFINITY_G1 [Constant] PublicKey
            INFINITY_G2 [Constant] Signature
        "#]],
    );
}

#[test]
fn test_complete_narrowed_fields_in_match_arm() {
    check(
        r#"
struct Point {
    x: Int,
    y: Int,
}

fn main(value: Point | nil) -> Int {
    match value {
        nil => 0,
        Point => value.$0,
    }
}
"#,
        expect![[r#"
            x [Field] Int
            y [Field] Int
        "#]],
    );
}

#[test]
fn test_complete_match_arm_binding_fields() {
    check(
        r#"
struct Point {
    x: Int,
    y: Int,
}

fn main(value: Point | nil) -> Int {
    match value {
        nil => 0,
        Point { x, $0 } => x,
    }
}
"#,
        expect![[r#"
            y [Field] Int
        "#]],
    );
}

#[test]
fn test_complete_empty_match_arm_binding_fields() {
    check(
        r#"
enum Shape {
    Empty,
    Rect { width: Int, height: Int },
}

fn main(shape: Shape) -> Int {
    match shape {
        Shape::Empty => 0,
        Shape::Rect { $0 } => 1,
    }
}
"#,
        expect![[r#"
            width [Field] Int
            height [Field] Int
        "#]],
    );
}

#[test]
fn test_complete_partial_match_arm_binding_field() {
    check(
        r#"
enum Shape {
    Empty,
    Rect { width: Int, height: Int },
}

fn main(shape: Shape) -> Int {
    match shape {
        Shape::Empty => 0,
        Shape::Rect { he$0 } => 1,
    }
}
"#,
        expect![[r#"
            height [Field] Int
        "#]],
    );
}

#[test]
fn test_complete_match_arm_binding_in_value() {
    check(
        r#"
struct Point {
    x: Int,
    y: Int,
}

fn main(value: Point | nil) -> Int {
    match value {
        nil => 0,
        Point { x: horizontal, y } => hor$0,
    }
}
"#,
        expect![[r#"
            horizontal [Variable] Int
        "#]],
    );
}

#[test]
fn test_complete_nothing_for_match_arm_binding_names() {
    // The names in an arm's binding are new, so nothing is offered for them, like for the names
    // in a `let`
    let source = r#"
struct Point {
    x: Int,
    y: Int,
}

fn main(value: Point | (Int, Int) | nil, count: Int) -> Int {
    match value {
        Point as point => point.x,
        (Int, Int) as (first, rest) => first + rest,
        nil => count,
    }
}
"#;

    check(
        &source.replacen("Point as point", "Point as p$0", 1),
        expect![[r#"
            <none>
        "#]],
    );
    check(
        &source.replacen("(first, rest)", "(c$0, rest)", 1),
        expect![[r#"
            <none>
        "#]],
    );
    check(
        &source.replacen("(first, rest)", "(first, $0)", 1),
        expect![[r#"
            <none>
        "#]],
    );
    check(
        "fn main(list: List<Int>, count: Int) -> Int {\n    match list {\n        nil => count,\n        _ as [c$0, ...rest] => 1,\n    }\n}\n",
        expect![[r#"
            <none>
        "#]],
    );
    // Right after the `as`, before there's a binding
    check(
        "fn main(list: List<Int>, count: Int) -> Int {\n    match list {\n        nil => count,\n        _ as $0 => 1,\n    }\n}\n",
        expect![[r#"
            <none>
        "#]],
    );
    check(
        "fn main(list: List<Int>, count: Int) -> Int {\n    match list {\n        nil => count,\n        _ as $0\n",
        expect![[r#"
            <none>
        "#]],
    );
    check(
        "fn main(value: Int, count: Int) -> Int {\n    let c$0 = value;\n    count\n}\n",
        expect![[r#"
            <none>
        "#]],
    );
}

#[test]
fn test_complete_let_binding_fields() {
    check(
        r#"
struct Point {
    x: Int,
    y: Int,
}

fn main(value: Point) -> Int {
    let { x, $0 } = value;
    x
}
"#,
        expect![[r#"
            y [Field] Int
        "#]],
    );
}

#[test]
fn test_complete_enum_variants() {
    check(
        r#"
enum Shape {
    Empty,
    Circle { radius: Int },
}

fn main() -> Shape {
    Shape::$0
}
"#,
        expect![[r#"
            Empty [EnumMember] Shape::Empty
            Circle [EnumMember]
        "#]],
    );
}

#[test]
fn test_complete_enum_variant_fields() {
    check(
        r#"
enum Shape {
    Empty,
    Circle { radius: Int },
}

fn main(shape: Shape::Circle) -> Int {
    shape.$0
}
"#,
        expect![[r#"
            radius [Field] Int
        "#]],
    );
}

#[test]
fn test_complete_enum_variant_initializer_fields() {
    check(
        r#"
enum Shape {
    Empty,
    Circle { radius: Int, diameter: Int },
}

fn main() -> Shape {
    Shape::Circle { radius: 1, $0 }
}
"#,
        expect![[r#"
            diameter [Field] Int
        "#]],
    );
}

#[test]
fn test_complete_enum_in_a_type() {
    check(
        r#"
enum Shape {
    Empty,
}

fn main(shape: Sha$0) -> Int {
    0
}
"#,
        expect![[r#"
            Shape [Enum] Shape::Empty
        "#]],
    );
}

#[test]
fn test_complete_enum_variant_types() {
    check(
        r#"
enum Shape {
    Empty,
    Circle { radius: Int },
}

fn main(shape: Shape::$0) -> Int {
    0
}
"#,
        expect![[r#"
            Empty [EnumMember]
            Circle [EnumMember]
        "#]],
    );
}

#[test]
fn test_complete_match_arm_condition() {
    // The arm's bindings and what's in scope are completed in its condition
    check(
        r#"
struct Point {
    x: Int,
    y: Int,
}

fn main(value: Point | nil) -> Int {
    match value {
        nil => 0,
        Point { x: horizontal, y } if hor$0 => y,
        Point => 1,
    }
}
"#,
        expect![[r#"
            horizontal [Variable] Int
        "#]],
    );

    // The fields of the value as the arm narrows it
    check(
        r#"
struct Point {
    x: Int,
    y: Int,
}

fn main(value: Point | nil) -> Int {
    match value {
        nil => 0,
        Point if value.$0 => 1,
        Point => 2,
    }
}
"#,
        expect![[r#"
            x [Field] Int
            y [Field] Int
        "#]],
    );
}

#[test]
fn test_complete_empty_match_arm_condition() {
    // Right after the `if`, with and without the rest of the arm
    check(
        "fn main(value: Int | nil, limit: Int) -> Int {\n    match value {\n        nil => 0,\n        Int as number if $0 => 1,\n        Int => 2,\n    }\n}\n",
        expect![[r#"
            super [Module]
            project [Module]
            number [Variable] Int
            value [Variable] Int
            limit [Variable] Int
            main [Function] fn(value: Int | nil, limit: Int) -> Int
            unchecked_cast [Function] fn<T>(value: Any) -> T
            sha256 [Function] fn(value: Bytes) -> Bytes32
            sha256_inline [Function] fn(value: Bytes) -> Bytes32
            keccak256 [Function] fn(value: Bytes) -> Bytes32
            keccak256_inline [Function] fn(value: Bytes) -> Bytes32
            concat [Function] fn(values: List<Bytes>) -> Bytes
            coinid [Function] fn(parent_coin_id: Bytes, puzzle_hash: Bytes, amount: Int) -> Bytes32
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
            keccak256_verify [Function] fn(value: Bytes, hash: Bytes32) -> nil
            INFINITY_G1 [Constant] PublicKey
            INFINITY_G2 [Constant] Signature
        "#]],
    );

    check(
        "fn main(value: Int | nil, limit: Int) -> Int {\n    match value {\n        nil => 0,\n        Int as number if $0\n",
        expect![[r#"
            super [Module]
            project [Module]
            number [Variable] Int
            value [Variable] Int
            limit [Variable] Int
            main [Function] fn(value: Int | nil, limit: Int) -> Int
            unchecked_cast [Function] fn<T>(value: Any) -> T
            sha256 [Function] fn(value: Bytes) -> Bytes32
            sha256_inline [Function] fn(value: Bytes) -> Bytes32
            keccak256 [Function] fn(value: Bytes) -> Bytes32
            keccak256_inline [Function] fn(value: Bytes) -> Bytes32
            concat [Function] fn(values: List<Bytes>) -> Bytes
            coinid [Function] fn(parent_coin_id: Bytes, puzzle_hash: Bytes, amount: Int) -> Bytes32
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
            keccak256_verify [Function] fn(value: Bytes, hash: Bytes32) -> nil
            INFINITY_G1 [Constant] PublicKey
            INFINITY_G2 [Constant] Signature
        "#]],
    );
}

/// Which of the names are offered at the cursor, and which of `Int` (a type) and `sha256` (a
/// value) are, which shows whether types or values are completed there.
fn offered(source: &str, names: &[&str]) -> String {
    let project = TestProject::new(&[("main.rue", source)]);
    let completions = project.completions();

    names
        .iter()
        .chain(&["Int", "sha256"])
        .filter(|name| {
            completions
                .lines()
                .any(|line| line.split_whitespace().next() == Some(**name))
        })
        .copied()
        .collect::<Vec<_>>()
        .join(" ")
}

/// The item of a `for` is in scope in the filter and the body, and the result of its fold only in
/// the body. Neither is in scope in the list or in the fold's value, and the fold's type is a
/// type.
#[test]
fn test_complete_in_for() {
    let names = ["item", "total", "items"];

    let list = "fn main(items: List<Int>) -> Int { for item in $0 fold total = 0 { total } }";
    assert_eq!(offered(list, &names), "items sha256");

    // Before the list is written
    let no_list = "fn main(items: List<Int>) -> Int { for item in $0 }";
    assert_eq!(offered(no_list, &names), "items sha256");

    let filter =
        "fn main(items: List<Int>) -> Int { for item in items if $0 fold total = 0 { total } }";
    assert_eq!(offered(filter, &names), "item items sha256");

    let ty = "fn main(items: List<Int>) -> Int { for item in items fold total: $0 = 0 { total } }";
    assert_eq!(offered(ty, &names), "Int");

    let value = "fn main(items: List<Int>) -> Int { for item in items fold total = $0 { total } }";
    assert_eq!(offered(value, &names), "items sha256");

    let body = "fn main(items: List<Int>) -> Int { for item in items fold total = 0 { $0 } }";
    assert_eq!(offered(body, &names), "item total items sha256");

    let after =
        "fn main(items: List<Int>) -> Int { for item in items fold total = 0 { total } + $0 }";
    assert_eq!(offered(after, &names), "items sha256");
}

#[test]
fn test_complete_struct_initializer_fields_after_spread() {
    // The fields that aren't written yet are suggested, though the spread provides them.
    check(
        r#"
struct Point {
    x: Int,
    y: Int,
    z: Int,
}

fn main(point: Point) -> Point {
    Point { ...point, x: 1, $0 }
}
"#,
        expect![[r#"
            y [Field] Int
            z [Field] Int
        "#]],
    );
}

#[test]
fn test_complete_struct_initializer_spread() {
    // A spread is followed by an expression, rather than the name of a field.
    check(
        r#"
struct Point {
    x: Int,
}

fn main(point: Point) -> Point {
    Point { ...$0 }
}
"#,
        expect![[r#"
            super [Module]
            project [Module]
            point [Variable] Point
            main [Function] fn(point: Point) -> Point
            Point [Struct]
            unchecked_cast [Function] fn<T>(value: Any) -> T
            sha256 [Function] fn(value: Bytes) -> Bytes32
            sha256_inline [Function] fn(value: Bytes) -> Bytes32
            keccak256 [Function] fn(value: Bytes) -> Bytes32
            keccak256_inline [Function] fn(value: Bytes) -> Bytes32
            concat [Function] fn(values: List<Bytes>) -> Bytes
            coinid [Function] fn(parent_coin_id: Bytes, puzzle_hash: Bytes, amount: Int) -> Bytes32
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
            keccak256_verify [Function] fn(value: Bytes, hash: Bytes32) -> nil
            INFINITY_G1 [Constant] PublicKey
            INFINITY_G2 [Constant] Signature
        "#]],
    );
}

#[test]
fn test_complete_struct_initializer_spread_value() {
    check(
        r#"
struct Point {
    x: Int,
}

fn main(point: Point) -> Point {
    Point { ...po$0 }
}
"#,
        expect![[r#"
            point [Variable] Point
            Point [Struct]
            modpow [Function] fn(base: Int, exponent: Int, modulus: Int) -> Int
            project [Module]
            product [Function] fn(values: List<Int>) -> Int
            pubkey_for_exp [Function] fn(exponent: Int | Bytes32) -> PublicKey
        "#]],
    );
}

#[test]
fn test_complete_struct_initializer_spread_field() {
    check(
        r#"
struct Point {
    x: Int,
}

struct Line {
    start: Point,
    end: Point,
}

fn main(line: Line) -> Point {
    Point { ...line.$0 }
}
"#,
        expect![[r#"
            start [Field] Point
            end [Field] Point
        "#]],
    );
}
