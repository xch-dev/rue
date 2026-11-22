//! Items declared in the standard library. Hover shows them like any other item, but the standard
//! library is compiled into the server and has no file the editor can open, so its declarations
//! and the references inside of it are left out of definitions and references.

use expect_test::{Expect, expect};

use super::TestProject;

#[allow(clippy::needless_pass_by_value)]
fn check(source: &str, expect: Expect) {
    let project = TestProject::with_manifest(
        Some("[compiler]\nentrypoint = \".\"\n"),
        &[("main.rue", source)],
    );

    expect.assert_eq(&project.queries());
}

#[test]
fn test_std_function() {
    check(
        r#"
fn main() -> Int {
    len$0gth([1, 2])
}
"#,
        expect![[r#"
            hover:
            fn length<T>(list: List<T>) -> Int
            definitions:
            <none>
            references:
            main.rue:2:4-2:10
        "#]],
    );
}

#[test]
fn test_std_function_through_module_path() {
    check(
        r#"
fn main() -> Bool {
    std::utils::deep_eq$0ual(1, 1)
}
"#,
        expect![[r#"
            hover:
            fn deep_equal(a: Any, b: Any) -> Bool
            definitions:
            <none>
            references:
            main.rue:2:16-2:26
        "#]],
    );
}

#[test]
fn test_std_module() {
    check(
        r#"
fn main() -> Bool {
    std::uti$0ls::deep_equal(1, 1)
}
"#,
        expect![[r#"
            hover:
            mod utils
            definitions:
            <none>
            references:
            main.rue:2:9-2:14
        "#]],
    );
}

#[test]
fn test_std_condition_variant() {
    check(
        r#"
fn main(puzzle_hash: Bytes32) -> CreateCoin {
    Create$0Coin { puzzle_hash, amount: 1 }
}
"#,
        expect![[r#"
            hover:
            CreateCoin = 51 { puzzle_hash: Bytes32, amount: Int, ...memos: Memos | nil }
            definitions:
            <none>
            references:
            main.rue:1:33-1:43
            main.rue:2:4-2:14
        "#]],
    );
}

#[test]
fn test_std_struct_field() {
    check(
        r#"
fn main(condition: CreateCoin) -> Int {
    condition.amo$0unt
}
"#,
        expect![[r#"
            hover:
            amount: Int
            definitions:
            <none>
            references:
            main.rue:2:14-2:20
        "#]],
    );
}

#[test]
fn test_std_condition_enum() {
    check(
        r#"
fn main(condition: Condi$0tion) -> Bool {
    condition is CreateCoin
}
"#,
        expect![[r#"
            hover:
            enum Condition {
                Remark = 1 { ...rest: Any },
                AggSigParent = 43 { public_key: PublicKey, message: Bytes },
                AggSigPuzzle = 44 { public_key: PublicKey, message: Bytes },
                AggSigAmount = 45 { public_key: PublicKey, message: Bytes },
                AggSigPuzzleAmount = 46 { public_key: PublicKey, message: Bytes },
                AggSigParentAmount = 47 { public_key: PublicKey, message: Bytes },
                AggSigParentPuzzle = 48 { public_key: PublicKey, message: Bytes },
                AggSigUnsafe = 49 { public_key: PublicKey, message: Bytes },
                AggSigMe = 50 { public_key: PublicKey, message: Bytes },
                CreateCoin = 51 { puzzle_hash: Bytes32, amount: Int, ...memos: Memos | nil },
                ReserveFee = 52 { amount: Int },
                CreateCoinAnnouncement = 60 { message: Bytes },
                AssertCoinAnnouncement = 61 { id: Bytes32 },
                CreatePuzzleAnnouncement = 62 { message: Bytes },
                AssertPuzzleAnnouncement = 63 { id: Bytes32 },
                AssertConcurrentSpend = 64 { coin_id: Bytes32 },
                AssertConcurrentPuzzle = 65 { puzzle_hash: Bytes32 },
                SendMessage = 66 { mode: Int, message: Bytes, ...receiver: List<Bytes | Int> },
                ReceiveMessage = 67 { mode: Int, message: Bytes, ...sender: List<Bytes | Int> },
                AssertMyCoinId = 70 { coin_id: Bytes32 },
                AssertMyParentId = 71 { parent_id: Bytes32 },
                AssertMyPuzzleHash = 72 { puzzle_hash: Bytes32 },
                AssertMyAmount = 73 { amount: Int },
                AssertMyBirthSeconds = 74 { seconds: Int },
                AssertMyBirthHeight = 75 { height: Int },
                AssertEphemeral = 76 {},
                AssertSecondsRelative = 80 { seconds: Int },
                AssertSecondsAbsolute = 81 { seconds: Int },
                AssertHeightRelative = 82 { height: Int },
                AssertHeightAbsolute = 83 { height: Int },
                AssertBeforeSecondsRelative = 84 { seconds: Int },
                AssertBeforeSecondsAbsolute = 85 { seconds: Int },
                AssertBeforeHeightRelative = 86 { height: Int },
                AssertBeforeHeightAbsolute = 87 { height: Int },
                Softfork = 90 { cost: Int, ...args: List<Any> },
            }
            definitions:
            <none>
            references:
            main.rue:1:19-1:28
        "#]],
    );
}

#[test]
fn test_std_constant() {
    check(
        r#"
fn main() -> Int {
    CREATE_CO$0IN
}
"#,
        expect![[r#"
            hover:
            inline const CREATE_COIN: 51
            definitions:
            <none>
            references:
            main.rue:2:4-2:15
        "#]],
    );
}

#[test]
fn test_std_completion() {
    let project = TestProject::with_manifest(
        Some("[compiler]\nentrypoint = \".\"\n"),
        &[(
            "main.rue",
            r#"
fn main() -> Int {
    leng$0
}
"#,
        )],
    );

    expect![[r#"
        length [Function] fn<T>(list: List<T>) -> Int
    "#]]
    .assert_eq(&project.completions());
}
