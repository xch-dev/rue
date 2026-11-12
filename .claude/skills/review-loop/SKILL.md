---
name: review-loop
description: Take a piece of Rue work (an issue, a lint, a bug fix, a PR's follow-up) all the way to a clean, reviewed, CI-passing branch, running rounds of independent code review and fixing what they find, without the user having to ask for each review pass. Use this whenever the user asks to implement an issue "with reviews", to "drive this to completion", "review until clean", "keep reviewing and fixing", "take this through review", or hands over an issue or task in the Rue repo and expects a finished, reviewed PR rather than a first draft, even if they don't say "review loop".
---

# Review loop

The user's usual workflow on Rue was: an agent implements something, then the user asks for a review pass, the agent fixes the findings, and this repeats until the work is clean. This skill runs that loop for them. The history shows what those rounds catch (`git log --oneline origin/main | grep -i review`): lint suggestions that produce broken code, missing snapshot cases, duplicated logic that should share a helper, and behaviour that drifted from what the issue asked for.

The loop is: **write the brief → implement → review → verify and fix → repeat until clean or out of rounds → final checks → commit, PR and report.**

## 1. Write the state file before touching code

The state file is the memory of the loop. Reviewers start with fresh context, and after a few rounds the implementer's own context is long, so without it later rounds re-raise settled points or undo earlier decisions. Write it to the session scratchpad (not the repo), at `<scratchpad>/review-loop/<branch-slug>.md`, and keep it current: every review round reads it, and every fix round updates it.

Read the source first: `gh issue view <n> --comments` or `gh pr view <n> --comments`, plus the code it touches. Then write:

```markdown
# <short title>

## Brief (shared with reviewers)
- Goal: <what this change must achieve, in the user's terms>
- Serves: <issue/PR link>
- Base: <base ref, e.g. origin/main, or the parent branch in a gh stack>
- Branch: <rigidity/...>
- Decisions: <each decision and why: "suggestion is MaybeIncorrect, because rewriting a comparison with comments in it can drop them">
- Out of scope: <things deliberately left alone, and why>
- Settled: <points already raised in review and resolved; reviewers shouldn't re-raise them without new evidence>

## Open questions for the user
- <only things the user has to decide; see section 4>

## Review log (implementer only)
### Round 1
- [confirmed, major] <finding> -> fixed in <commit>, regression test <where>
- [rejected] <finding> -> <why it's wrong, with the evidence>
- [deferred] <finding> -> <why, and where it's recorded>
```

Only the **Brief** goes to reviewers. The review log holds the implementer's verdicts, and giving those to reviewers would anchor them on the implementer's view of the code. When a rejected finding is truly settled, add a one-line summary to **Settled** so the next round doesn't spend effort on it, but phrase it as a fact about the design ("`export` on a nested module's items counts as a use"), not as an argument.

## 2. Implement

Do the work on a `rigidity/` branch (lowercase; never `Rigidity/`). For a new lint, the usual places are:

- Register it in `crates/rue-diagnostic/src/lint.rs` (the `lints!` macro: name, default level, description; add it to a `lint_groups!` group if it belongs in one).
- Write the pass in `crates/rue-lint/src/lints/<area>.rs` and list it in `crates/rue-lint/src/lints/mod.rs` (`INPUT_PASSES` for whole-input passes, `hir_passes` for the HIR walk). The `every_lint_is_reported_by_one_pass` test fails if you forget.
- Use what's already there before writing helpers: `crates/rue-lint/src/visit.rs` (HIR traversal), `context.rs` (`LintContext::report`, levels, type queries), `text.rs` (source text, parentheses, negation, comments, keyword locations). Several past review rounds were about logic duplicated next to an existing helper.
- Add a snapshot test `tests/warnings/<lint>.rue` (starting with `#![warn(<lint>)]` if it isn't on by default) and generate its `.yaml` (see section 5). Cover the cases where the lint should *not* fire, not only where it should.
- If the lint has machine-applicable suggestions, add a case to `crates/rue-compiler/src/lint_tests.rs` that applies them with `fix()` and checks the result is the expected source.

Commit the implementation before the first review round, so each round's fixes are a separate, reviewable diff.

## 3. Review rounds

Each round, launch three reviewers **in parallel** with the Agent tool (`general-purpose`, `run_in_background: false`), one per focus:

1. **Correctness**: wrong results, missed or false-positive diagnostics, crashes, and suggestions that produce broken or different code.
2. **Tests and snapshots**: whether the tests would catch a regression in each behaviour the change adds, and whether snapshot diffs are intended.
3. **Simplification and reuse**: duplicated logic, existing helpers that should be used, needless work, structure that doesn't match the surrounding code.

For a small diff (roughly under 100 changed lines), one reviewer covering all three focuses is enough; three reviewers on a ten-line change mostly produce nits.

Build each reviewer's prompt from [references/reviewer-brief.md](references/reviewer-brief.md): it holds the prompt template, the Rue-specific checklist for each focus, and the format findings come back in. Give reviewers the Brief section, the base ref, and the focus. Don't give them your summary of the change, your reasoning about why it's correct, or the review log; they should read the diff (`git diff <base>...HEAD`) and form their own view. That independence is what makes a review round worth running.

The built-in `code-review` skill is an alternative for the correctness focus on a large diff (`/code-review high`), but it doesn't see the Brief, so check its findings against the goal and the settled points yourself.

## 4. Verify, then fix

Reviewers are sometimes confidently wrong. Before acting on a finding, check it:

- For a claimed bug, reproduce it: write the Rue snippet the finding describes to the scratchpad and run it (`cargo run --quiet -p rue-cli -- build <file>` prints the diagnostics, suggestions and compiled program; `-W <lint>` turns a lint on), or add the failing test first. A finding you can't reproduce is rejected unless you can see from the code that it's right.
- For a claimed missing test, check whether an existing case already covers it.
- For a simplification, check that the suggested helper does what the reviewer thinks.

Then mark each finding in the review log as **confirmed**, **rejected** (with the evidence), or **deferred** (real but out of scope, with where it's recorded). Fix every confirmed blocker and major finding, and minor ones when they're cheap. Each confirmed correctness finding gets a regression test, so the next round (and the future) can see it stay fixed. Nits are optional; never start another round just for them.

**Stop and ask the user** (AskUserQuestion, or record it under *Open questions* and stop the loop if you can't ask) when a finding needs a decision only they can make, rather than picking one yourself:

- Rue language design or syntax, and what the compiler accepts or rejects.
- Public formats: `Rue.toml`, CLI flags, LSP behaviour users configure.
- A lint's name, default level, or group, and whether a suggestion is applied automatically when the right answer is unclear.
- Changing the scope of the issue, or a fix that would change behaviour outside it.

After fixing, run the quick checks for what you touched (`cargo test -p <crate>`, `cargo run -p rue-tests --release -- <filter>`), update the Brief's Decisions and Settled lists, and commit. Name the commit after what changed ("Keep comments in suggestions"), or "Address review findings in <area>" with a bulleted body when one round fixes several small things; see section 6.

### When to stop

- **Clean**: a round with no confirmed findings at major or above. Stop there.
- **Out of rounds**: stop after round 3 even if findings remain, and list them in the report. If round 3's fixes were more than trivial, first run one last round with only the correctness reviewer, to check the fixes themselves. A change that still has major findings after three rounds usually has a design problem that another round won't fix, and the user should look.
- **Blocked**: a finding needs a user decision (above).

Every round after the first reviews the whole diff against the base, not only the latest fix commit, because fixes break things elsewhere. Tell those reviewers which areas changed since the last round so they look closely there.

## 5. Final checks

Run these in this order, matching `.github/workflows/build.yml`, and fix anything they report:

```bash
cargo clippy --all-features --workspace --all-targets -- -D warnings
cargo fmt --all -- --check
cargo run --quiet --package rue-cli -- fmt 'examples/**/*.rue' 'tests/**/*.rue' 'crates/rue-compiler/src/std/**/*.rue' --check
cargo machete
cargo run -p rue-tests --release        # regenerates snapshots in tests/ and examples/
git status --short tests examples       # read every changed .yaml
cargo nextest run --all-features --workspace   # or: cargo test --all-features --workspace
```

- `rue fmt --check` with no paths fails with "No project found"; use the globbed form above.
- `cargo run -p rue-tests` rewrites snapshots in place. Read each changed `.yaml` and make sure every change is intended: a new diagnostic in an unrelated test, a changed `program:` or cost line, or a vanished diagnostic usually means a regression. The `rue-tests` unit test compares without updating, so it fails if snapshots weren't regenerated.
- Use nextest if `cargo nextest --version` works, otherwise `cargo test`.
- Commit regenerated snapshots with the change that caused them (amend or fix up that commit if it's still unpushed), not as a separate "update snapshots" commit.
- If you touched `crates/rue-wasm`, also run `cargo clippy --package rue-wasm --target wasm32-unknown-unknown --all-targets`.

## 6. Commits, PR and report

**Commits.** One focused commit per logical change. Messages follow the git log: a plain sentence as the subject ("Keep suggestions from joining words to the code around them"), no prefix like `feat:`, and a body of plain sentences or `-` bullets when there's more to say. No AI attribution of any kind: no `Co-Authored-By`, no "Generated with" line.

**PR.** Push and open a draft PR (`gh pr create --draft --base <base>`) with a title in the same style and a body that says what changed and why, links the issue (`Closes #n` when it does), and lists any open questions. For stacked work, use `gh stack` (`gh stack init`, `gh stack add`, `gh stack submit`) rather than setting PR bases by hand.

**Report.** End with this, in the reply to the user:

```markdown
## <title> — <PR link>

<two or three sentences: what was done, and what the user should look at first>

### Review rounds
- Round 1: <n> findings, <n> confirmed. Caught: <the substantive ones, one line each>
- Round 2: ...
- Stopped because: <clean / out of rounds / needs your decision>

### Rejected findings
- <finding> — <why it was rejected>

### Left for you
- <decisions only the user can make, with the options and your recommendation>
- <confirmed findings left unfixed, if the loop ran out of rounds>

### Checks
<each final check and whether it passed; note snapshot changes and that they're intended>
```

Keep the state file until the user has seen the report; it's the place to look if they ask why something was decided.
