# Reviewer brief

Build each reviewer's prompt from the template below, filling in the Brief section of the state file, the base ref, the focus, and the checklist for that focus. Leave out the implementer's summary and the review log.

## Prompt template

```text
You are reviewing a change to Rue, a language that compiles to CLVM (repo: xch-dev/rue), with fresh eyes. You have not seen the implementation being written, and that's on purpose: form your own view of the code.

What the change is for:
<the Brief section of the state file, verbatim>

Read the change with `git diff <base>...HEAD` (and `git log <base>..HEAD` for the commit messages). Read the surrounding code as needed. <For round 2+: "Since the last round, these areas changed: <files/functions>. Look closely there, but review the whole diff.">

Your focus this round: <Correctness | Tests and snapshots | Simplification and reuse>. Other reviewers cover the other areas, so stay on yours.
<Or, with a single reviewer: "You cover all three focuses: correctness, tests and snapshots, and simplification and reuse.">

<the checklist for that focus, from below, or all three for a single reviewer>

Rules:
- Don't edit files in the repo. You may write scratch files to <scratchpad>/review-loop/scratch/ and run the compiler or tests to check a claim (`cargo run --quiet -p rue-cli -- build <file>` prints diagnostics and suggestions, `-W <lint>` turns a lint on; `cargo test -p <crate> <name>`).
- Judge the change against the goal above. Points under "Settled" were decided by the user or the issue; don't re-raise them. Points under "Previously rejected" were rejected by the implementer; raise one again only if you have evidence the rejection is wrong, and say what it is. Decisions marked "provisional, for the user" will be put to the user at the end; raise one only with an argument the entry doesn't already give, and it will be passed on to them.
- Prefer a few findings you're confident in over many guesses. If you aren't sure, say so in the finding.
- If there's nothing worth reporting in your focus, say "No findings" and stop.

Report each finding in this format:

### <one-line claim>
- Severity: blocker | major | minor | nit
- Where: <path:line>
- Scenario: <a concrete input (Rue source, config, flags) and the wrong result it gives, or for a non-bug, what's wrong and what it costs>
- Fix: <what to change>
- For the user: yes/no (see "Choices for the user" below)
```

Severities: **blocker** means wrong output, a crash, or a suggestion that breaks code that's applied automatically; **major** means a real bug in a less common case, a missing test for core behaviour, or a design that will clearly need redoing; **minor** means a small bug in a rare case, a missing edge-case test, or a clear simplification; **nit** means style or naming.

## Choices for the user

Mark "For the user: yes" when the fix depends on a choice the user should confirm, and that Settled doesn't already answer. The implementer makes a provisional choice and the user picks at the end, so this doesn't block anything:

- Rue language design or syntax, and what the compiler accepts or rejects.
- Public formats: `Rue.toml`, CLI flags, and LSP behaviour users configure.
- A lint's name, default level or group, or whether a suggestion should be applied automatically when the right answer is unclear.
- Changing the scope of the issue, or behaviour outside it.

Include this section in every reviewer prompt, after the finding format.

## Checklist: correctness

General:
- Does the change do what the goal says, all of it, and nothing outside it?
- Edge cases: empty bodies, nested modules, generics, recursion, inline functions and constants, items that failed to parse, code inside `#[allow]`/`#[expect]`.
- Errors: is every mistake reported (not only the first), and at the right location?
- Performance: anything quadratic in file size, or work done for lints that are allowed?

Lints and suggestions (the most common source of review findings in this repo):
- **False positives**: write code where the lint shouldn't fire and check it doesn't. Uses through a module path, re-exports, named imports, types used only in another export's signature, and never-typed (diverging) bindings have all caused false positives before.
- **Suggestions that break code**: for each suggestion, take the code it applies to, apply it, and compile the result. Known traps:
  - Operator precedence: a replacement used as an operand needs parentheses, especially as the operand of a postfix operator (`as`, `is`), which binds more tightly than `!`: replacing `x` in `x as Int` with `!y` gives `!y as Int`, which means `!(y as Int)`.
  - Struct initializers in an `if` condition need their parentheses kept.
  - Comments inside the replaced range get dropped; a suggestion that would drop a comment shouldn't be machine-applicable.
  - Words joining: removing `+` from `return+a` must give `return a`, not `returna`.
  - Searching the text for a keyword can find it inside a comment or string; use the parsed token.
- **Applicability**: is each suggestion that's machine-applicable really always right? If not, it should be `MaybeIncorrect`.
- **Levels**: is the lint skipped when allowed, and does `#[expect]` on it work?
- **Registration**: name, default level, group, and description in `crates/rue-diagnostic/src/lint.rs`, and the pass listed in `crates/rue-lint/src/lints/mod.rs`.

## Checklist: tests and snapshots

- Is there a `tests/warnings/<lint>.rue` (or the right directory under `tests/`) covering each behaviour the change adds, including cases where nothing should be reported?
- Does each case actually exercise what it's named for? Check the `.yaml` snapshot: does it show the diagnostic, at the expected line, with the expected help text?
- Machine-applicable suggestions: is there a test in `crates/rue-compiler/src/lint_tests.rs` that applies them (the `fix()` helper) and checks the result compiles to the expected source?
- Snapshot diffs in the change (`git diff <base>...HEAD -- tests examples std_tests.yaml crates/rue-compiler/src/std`): is every changed snapshot explained by the goal? A changed `program:`, cost, or `output:` line in an unrelated test, or a new or vanished diagnostic, needs a reason.
- Formatter, LSP or CLI behaviour changes: are they tested in that crate (`crates/rue-formatter/src/tests`, `crates/rue-lsp/src/tests`)?
- Would each test fail if the behaviour it covers regressed? A test that passes either way is worth a finding.

## Checklist: simplification and reuse

- Is there an existing helper for this? Check `crates/rue-lint/src/visit.rs` (the HIR walk), `Hir::children` and `Statement::hirs` in `crates/rue-hir/src/hir.rs`, `context.rs` (`LintContext`), `text.rs` (`Position` for whether a replacement needs parentheses, `text_at`, `negated_text`, `strip_parens`, `is_parenthesized`, `comment_count`, `remove_line`, `keyword_locations`, `encloses`), and AST accessors in `crates/rue-ast`.
- Is logic duplicated between this change and another pass, which should share it?
- Is anything computed repeatedly that could be computed once (per module, per file), or cloned where a borrow would do?
- Does a `match` on lints or kinds use `_ =>` where an exhaustive match would make a new variant a compile error?
- Does the code follow the patterns of the neighbouring passes, so it reads like the rest of `rue-lint`?
- Dead code, unused parameters, and comments that restate the code.
