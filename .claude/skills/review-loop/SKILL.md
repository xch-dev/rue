---
name: review-loop
description: Take a piece of Rue work (an issue, a lint, a bug fix, a follow-up to an open PR) all the way to a reviewed draft PR that passes the same checks as CI, running rounds of independent code review and fixing what they find, without the user having to ask for each review pass. Use this whenever the user asks to implement an issue "with reviews", to "drive this to completion", "review until clean", "keep reviewing and fixing", "take this through review", or hands over a task in the Rue repo and expects a finished, reviewed PR rather than a first draft, even if they don't say "review loop". Not for a one-off review of an existing diff with no fix loop; use code-review for that.
---

# Review loop

The user's usual workflow on Rue was: an agent implements something, then the user asks for a review pass, the agent fixes the findings, and this repeats until the work is clean. This skill runs that loop for them. Past rounds caught lint suggestions that produce broken code (see commits f74792dc, 949133b0, df02c590, 5d038af8 and e777d6f3), false positives, missing test cases, and logic duplicated next to an existing helper (`git log --oneline origin/main | grep -i review`).

The loop is: **scope and write the brief → implement → review → verify and fix → repeat until a stop condition → final checks → commit, PR and report.**

## 1. Scope the work and write the state file

Read the source first: `gh issue view <n> --comments` or `gh pr view <n> --comments`, plus the code it touches.

**Tracking issues.** If the issue lists several independent items (like #235), don't put them all on one branch. Ask the user which items to do, unless they already said. Give each item its own branch, state file and loop, stack the branches with `gh stack`, and link the issue with "Part of #n" rather than "Closes #n".

**Open choices.** If the source leaves open anything on the reviewer brief's "needs the user" list (language design, public formats, a lint's name, level or group, scope), ask before implementing, since changing it after review costs a rewrite. #235's `bool_bitwise_operator`, for example, leaves open whether it's `allow` by default or only fires on expensive operands. If you can't ask, decide as in section 5.

**Follow-ups to an open PR.** Do what the user asked; if they didn't say, continue on their own `rigidity/` branches and stack on anyone else's. Set Base to the commit the follow-up starts from, so reviewers see only the new work.

- *Continuing on the PR's branch* (`gh pr checkout <n>`): keep its branch name, and push to that PR in section 7.
- *Stacking on top*: if the PR is already in a stack, `gh stack checkout <n>`; otherwise adopt its branch with `gh stack init <pr-branch>`. Then `gh stack add rigidity/<slug>`. The new branch gets its own draft PR in section 7, linking the original.

The state file is the memory of the loop. Reviewers start with fresh context, and after a few rounds the implementer's own context is long, so without it later rounds re-raise settled points or undo earlier decisions. Write it to the session scratchpad (not the repo), at `<scratchpad>/review-loop/<branch-slug>.md`. Every review round reads it, and every fix round updates it.

```markdown
# <short title>

## Brief (shared with reviewers)
- Goal: <what this change must achieve, in the user's terms>
- Serves: <issue/PR link>
- Base: <origin/main, the parent branch in a gh stack, or the commit a PR follow-up starts from>
- Branch: <rigidity/...>
- Decisions: <each decision and why, noting who made it: the issue, the user, the implementer, or "provisional, pending the user" (section 5). Include intended changes to unrelated snapshots and why.>
- Out of scope: <things deliberately left alone, and why>
- Settled: <points the user or the issue decided; not open for review>
- Previously rejected: <findings the implementer rejected in earlier rounds: the claim and the evidence>

## Open questions for the user

## Review log (implementer only)
### Round 1
- [confirmed, major] <finding> -> fixed in <commit>, regression test <where>
- [rejected] <finding> -> <evidence>
- [deferred] <finding> -> <why, and where it's recorded>
```

## 2. Implement

Work on a `rigidity/` branch (lowercase; never `Rigidity/`), unless you're continuing someone else's PR branch. For a bug fix, write the failing regression test first. For a new lint, the usual places are:

- Register it in `crates/rue-diagnostic/src/lint.rs` (the `lints!` macro: name, default level, description; add it to a `lint_groups!` group if it belongs in one). Take the name and level from the issue if it gives them; otherwise follow the neighbouring lints and record the choice under Decisions.
- Write the pass in `crates/rue-lint/src/lints/<area>.rs` and list it in `crates/rue-lint/src/lints/mod.rs` (`INPUT_PASSES` for whole-input passes, `hir_passes` for the HIR walk). The `every_lint_is_reported_by_one_pass` test fails if you forget.
- Use what's already there before writing helpers: `crates/rue-lint/src/visit.rs` (the HIR walk), `context.rs` (`LintContext::report`, levels, type queries), `text.rs` (source text, parentheses, negation, comments, keyword locations), and `Hir::children` / `Statement::hirs` in `crates/rue-hir/src/hir.rs`.
- Add a snapshot test `tests/warnings/<lint>.rue` (starting with `#![warn(<lint>)]` if it isn't on by default). Cover the cases where the lint should *not* fire, not only where it should.
- If the lint has machine-applicable suggestions, add a case to `crates/rue-compiler/src/lint_tests.rs` that applies them with `fix()` and checks the result is the expected source.

Before each review round, including the first, regenerate every snapshot so reviewers see the real diff, including changes to unrelated tests:

```bash
cargo run -p rue-tests --release
git status --short
```

This rewrites the `.yaml` snapshots under `tests/` and `examples/` and `std_tests.yaml`, and reformats `.rue` files in `tests/`, `examples/` and `crates/rue-compiler/src/std/`. It exits 1 whenever it changed anything ("Test failed, updated yaml file" or "Formatting differed, updated <path>"), including the first run of a new test; run it again, and it should exit 0. Read each changed snapshot: a new diagnostic in an unrelated test, a changed `program:`, `output:` or cost line, or a vanished diagnostic usually means a regression. Fix unintended changes before the round starts; record intended changes to unrelated tests under Decisions, so reviewers don't flag them again. Commit snapshots with the change that caused them.

Commit before the first review round, so each round's fixes are a separate diff.

## 3. Review rounds

Each round, launch three reviewers **in parallel** with the Agent tool (`general-purpose`, `run_in_background: false`), one per focus:

1. **Correctness**: wrong results, missed or false-positive diagnostics, crashes, and suggestions that produce broken or different code.
2. **Tests and snapshots**: whether the tests would catch a regression in each behaviour the change adds, and whether snapshot diffs are intended.
3. **Simplification and reuse**: duplicated logic, existing helpers that should be used, needless work, structure that doesn't match the surrounding code.

For a small diff (roughly under 100 changed lines), one reviewer covering all three focuses is enough; three reviewers on a ten-line change mostly produce nits.

Build each prompt from [references/reviewer-brief.md](references/reviewer-brief.md), which holds the template, the Rue checklist for each focus, the severities, and when a finding needs the user. Give reviewers the Brief section, the base ref, and the focus, and nothing else: not your summary of the change, your reasoning about why it's correct, or the review log. They read the diff (`git diff <base>...HEAD`) and form their own view, and that independence is what makes a round worth running. From round 2 they still review the whole diff against the base, since fixes break things elsewhere; name the areas that changed since the last round so they look closely there.

The built-in `code-review` skill is an alternative for the correctness focus on a large diff (`/code-review high`), but it doesn't see the Brief, so check its findings against the goal and the Settled points yourself.

## 4. Verify, then fix

Reviewers are sometimes confidently wrong. Before acting on a finding, check it:

- For a claimed bug, reproduce it: write the Rue snippet the finding describes to the scratchpad and run it (`cargo run --quiet -p rue-cli -- build <file>` prints the diagnostics, suggestions and compiled program; `-W <lint>` turns a lint on), or add the failing test first. A finding you can't reproduce is rejected unless you can see from the code that it's right.
- For a claimed missing test, check whether an existing case already covers it.
- For a simplification, check that the suggested helper does what the reviewer thinks.
- For a finding marked "needs a user decision", it's confirmed if the choice really is open: neither the source nor Settled answers it. Handle it as in section 5.

Mark each finding in the review log as **confirmed**, **rejected** (with the evidence), or **deferred** (real but out of scope, with where it's recorded), and add rejected ones to the Brief's Previously rejected list. Fix every confirmed blocker and major finding, and minor ones when they're cheap. Each confirmed correctness finding gets a regression test. Nits are optional.

Then run the quick checks for what you touched (`cargo test -p <crate>`, and `cargo run -p rue-tests --release -- <filter>` while iterating), regenerate every snapshot as in section 2, update the Brief, and commit (section 7).

## 5. When to stop

- **Clean**: a round with no confirmed findings at major or above. Stop there; never start a round just for minor findings or nits.
- **Out of rounds**: stop after round 3 even if findings remain, and list them in the report. If round 3's fixes were more than trivial, first run one more round with only the correctness reviewer, to check the fixes; fix what it confirms and report it, without another round. A change that still has major findings after three rounds usually has a design problem that another round won't fix, and the user should look.

Whatever the stop, still run the final checks and open or update the draft PR, so the user has the work in hand.

**Decisions during the loop.** These don't end the loop. When a confirmed finding of minor or above needs a decision only the user can make (the list is in the reviewer brief), ask with AskUserQuestion, giving the options and your recommendation, then record the answer under Settled and carry on. If you can't ask (a non-interactive run), take the more conservative option, record it under Decisions as "provisional, pending the user: X, because Y" so later reviewers don't raise it again, and list it under Open questions; the draft PR and the report carry the question.

## 6. Final checks

These are the main checks in `.github/workflows/build.yml`, plus `-D warnings` on clippy (stricter than CI, as the user wants) and a final snapshot regeneration. Fix anything they report:

```bash
cargo clippy --all-features --workspace --all-targets -- -D warnings
cargo fmt --all -- --check
cargo run --quiet --package rue-cli -- fmt 'examples/**/*.rue' 'tests/**/*.rue' 'crates/rue-compiler/src/std/**/*.rue' --check
cargo machete
cargo run -p rue-tests --release; git status --short   # as in section 2; exits 1 if it rewrote anything
cargo nextest run --all-features --workspace   # if `cargo nextest --version` works; otherwise cargo test --all-features --workspace
```

- A bare `rue fmt --check` fails with "No project found"; use the globbed form.
- The test suite includes `rue-tests`' own unit test, which compares snapshots without updating them, so it fails if they weren't regenerated.
- `cargo clippy --package rue-wasm --target wasm32-unknown-unknown --all-targets` if you changed `wasm/` or an API it uses (`rue-compiler`, `rue-diagnostic`).
- CI also runs `cargo ws publish --publish-as-is --allow-dirty --dry-run` (run it if you changed a `Cargo.toml`) and the `wasm/` and `playground/` pnpm builds (if you changed `wasm/`). Say in the report which of these you didn't run.
- If these checks led to changes beyond formatting, say so in the report, since no reviewer saw them.

## 7. Commits, PR and report

**Commits.** One focused commit per logical change. Messages follow the git log: a plain sentence as the subject that says what changed ("Keep comments in suggestions"), or "Address review findings in <area>" with a `-` bulleted body when one round fixes several small things. No prefix like `feat:`. No AI attribution of any kind: no `Co-Authored-By`, no "Generated with" line.

**PR.** Push and open a draft PR with `gh pr create --draft --base <branch>`, where the branch is Base without the `origin/` prefix (`main`, not `origin/main`), or push to the existing PR when continuing on its branch. The title follows the same style; the body says what changed and why, links the issue ("Closes #n", or "Part of #n" for a tracking issue) or the PR it follows up, and lists any open questions. For stacked work, use `gh stack` rather than setting PR bases by hand: `gh stack submit --auto` creates draft PRs with generated titles, so set each title and body afterwards with `gh pr edit <n> --title ... --body ...`, and don't pass `--open`, which marks them ready for review.

**Report.** End with this, in the reply to the user:

```markdown
## <title> — <PR link>

<two or three sentences: what was done, and what the user should look at first>

### Review rounds
- Round 1: <n> findings, <n> confirmed. Caught: <the substantive ones, one line each>
- Round 2: ...
- Stopped because: <clean / out of rounds>

### Rejected findings
- <finding> — <why it was rejected>

### Left for you
- <decisions only the user can make, with the options and your recommendation>
- <confirmed findings left unfixed, if the loop ran out of rounds>

### Checks
<each final check and whether it passed; note snapshot changes and that they're intended>
```

Keep the state file until the user has seen the report; it's the place to look if they ask why something was decided.
