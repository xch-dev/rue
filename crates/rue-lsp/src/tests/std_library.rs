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
fn main() -> List<Int> {
    merge_li$0st([1, 2], [3])
}
"#,
        expect![[r#"
            hover:
            fn merge_list<T>(a: List<T>, b: List<T>) -> List<T>
            definitions:
            <none>
            references:
            main.rue:2:4-2:14
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
fn test_std_struct() {
    check(
        r#"
fn main(puzzle_hash: Bytes32) -> CreateCoin {
    Create$0Coin { puzzle_hash, amount: 1 }
}
"#,
        expect![[r#"
            hover:
            struct CreateCoin {
                opcode: 51,
                puzzle_hash: Bytes32,
                amount: Int,
                ...memos: Memos | nil,
            }
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
fn test_std_type_alias() {
    check(
        r#"
fn main(condition: Condi$0tion) -> Bool {
    condition is CreateCoin
}
"#,
        expect![[r#"
            hover:
            type Condition = Remark | AggSigParent | AggSigPuzzle | AggSigAmount | AggSigPuzzleAmount | AggSigParentAmount | AggSigParentPuzzle | AggSigUnsafe | AggSigMe | CreateCoin | ReserveFee | CreateCoinAnnouncement | AssertCoinAnnouncement | CreatePuzzleAnnouncement | AssertPuzzleAnnouncement | AssertConcurrentSpend | AssertConcurrentPuzzle | SendMessage | ReceiveMessage | AssertMyCoinId | AssertMyParentId | AssertMyPuzzleHash | AssertMyAmount | AssertMyBirthSeconds | AssertMyBirthHeight | AssertEphemeral | AssertSecondsRelative | AssertSecondsAbsolute | AssertHeightRelative | AssertHeightAbsolute | AssertBeforeSecondsRelative | AssertBeforeSecondsAbsolute | AssertBeforeHeightRelative | AssertBeforeHeightAbsolute | Softfork
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
fn main() -> List<Int> {
    merge_l$0
}
"#,
        )],
    );

    expect![[r#"
        merge_list [Function] fn<T>(a: List<T>, b: List<T>) -> List<T>
    "#]]
    .assert_eq(&project.completions());
}
