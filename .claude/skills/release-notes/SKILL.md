---
name: release-notes
description: Write or update the GitHub release notes for a Rue release in the format the releases use now (a summary sentence, Highlights grouped by area, an Upgrading section, and the pull request list folded away at the end). Use this whenever the user asks to write, draft, update, refresh or fix release notes or a changelog for Rue, to "add the new PRs to the release notes", to prepare notes for the next version, or to rewrite an older release's notes in the current style, even if they only say "update the release". Not for bumping the version or publishing a release.
---

# Release notes

Rue's release notes live in the body of each GitHub release; there is no changelog file. Since 0.8.5 they all follow one format, written for someone who uses Rue rather than someone who works on it. This skill writes notes in that format for a new release, or brings existing notes up to date after more pull requests merge.

Read the latest release before writing, so the notes match what's published now and not just this description of it: `gh release view --json body --jq .body`. Read 0.12.0 or 0.11.0 for a large release and 0.8.5 for a small one.

## What you may and may not do

Pushing a version tag publishes every crate to crates.io (`.github/workflows/build.yml`), and publishing a GitHub release creates its tag. So:

- **Never publish a release, create a tag or push one.** The user does that. New notes go into a draft release, which has no tag until it's published.
- **Never pass `--draft=false`, and never run `gh release create` without `--draft`.**
- **A published release's notes are public.** Editing them is fine when the user asked for that release to be changed. Show what changed afterwards.

## 1. Find the release and collect its pull requests

Work out which release the notes are for:

- the version the user named;
- otherwise a draft release, if there is one (`gh release list --json tagName,isDraft`);
- otherwise the next version. The version in `Cargo.toml` on `origin/main` is the next one if it's ahead of the latest release. If it isn't, the version hasn't been bumped yet: use the next minor version (0.12.0 → 0.13.0) and say that you assumed it.

Then run the script, which only reads from GitHub:

```bash
.claude/skills/release-notes/scripts/release-prs.sh <tag> <scratchpad>/release-notes/<tag>
```

It takes the previous tag and the target branch as optional third and fourth arguments (the latest published release and `main` by default). It writes:

- `footer.md`: the end of the notes, ready to append as it is;
- `prs.tsv`: number, author and title of every pull request in the release;
- `prs/<n>.md`: each pull request's title and description;
- `current.md`: the notes the release has now, if it exists;
- `new.txt`: the pull requests that `current.md` doesn't list yet.

When `new.txt` is empty and the user didn't ask for a rewrite, the notes are up to date. Say so and stop.

## 2. Read the pull requests

The titles aren't enough. Read each description in `new.txt` (all of them for new notes) to learn what changed for a user: the syntax, the command or flag, the error message, the default, what used to happen. Descriptions end with a risk assessment and sometimes "Decisions for you"; the first is a useful summary and the second isn't for users.

For a large release (more than about 40 pull requests to read), split the numbers between a few subagents and have each return, per pull request: the area, what a user sees change in one or two sentences with the exact names involved, whether existing code or output is affected, and whether it's internal only. Give them this section and the "what belongs" list below. Write the notes yourself from what they return.

Several pull requests often make up one change (a feature, then fixes to it, then review follow-ups). Describe the result as it is at the release, not each step. A bug that was introduced and fixed within the same release was never released, so leave it out.

**Check claims you can check.** Descriptions go stale when later pull requests change the same thing. For a count, a default, a flag name or a piece of syntax, look at the code or run the tool (`cargo run -p rue-cli -- lints`, `--help`) rather than trusting the description. 0.12.0's "about 40 lints" came from the tool, not from adding up the pull requests.

## 3. Write the notes

### Structure

```markdown
Rue <version> <what the release does, in one or two sentences>.

## Highlights

### <Area>

<An optional sentence introducing the area.>

- **<Label>:** <what changed>.
- **<Label>:** <what changed>.

### <Area>

- ...

## Upgrading

- **<Label>:** <what may need attention>.

<footer.md, unchanged>
```

- **Summary:** starts with "Rue <version>" and names the two or three changes that matter most, in plain words. It can say what kind of release it is ("is mostly a soundness release").
- **Highlights:** `###` sections by area, most important first. Use the names earlier releases used where they fit: Language, Type checking, Lints, Compiler, Compiler and optimizer, CLVM output, Parser and formatter, Formatter, Language server, Robustness and performance, CLI, Examples. Add a new one (Playground, Standard library) when a release needs it. A section with one bullet usually belongs in another section.
- **Bullets:** a bold label ending in a colon, then the text starting in lower case (unless it starts with code or a name). The label names the thing: a feature (`` **`rue fmt`:** ``), a topic (**Recursive types:**) or a short statement (**Early returns are checked:**). A section of plain fixes can drop the labels and start each bullet with "Fixed", as 0.12.0's Formatter section does. A bullet with several parts introduces them with a colon and nests them, each ending in a semicolon and the last in a full stop.
- **Upgrading:** what someone moving from the previous version may have to act on: new errors, new warnings, changed output or tree hashes, changed defaults, removed or renamed things, changed text formats. Each bullet says what to do when there's something to do. The usual labels are **New errors:**, **New warnings:**, **Compiled output:** and **Dependencies:**. Every release so far has had this section; if nothing at all needs attention, leave it out rather than writing "nothing".
- **Footer:** `footer.md` as the script wrote it. It holds the folded "All pull requests" list, GitHub's "New Contributors" section when there is one, and the "Full Changelog" link. Don't edit, reorder or trim it; every pull request stays in the list, including the ones the highlights leave out.
- **Small releases:** with only a few changes, drop the "Highlights" heading and the area sections and put the bullets straight after the summary, as 0.8.5 does. Use sections once there are two or more areas with something to say.

### What belongs in the highlights

- **In:** anything a user can see. New syntax, builtins, lints, commands and flags; fixed bugs and crashes; changed errors, warnings and messages; changed output, cost or size; speed-ups they'd notice; language server and formatter behaviour; new examples.
- **Out:** tests, CI, refactors, internal data structures, dependency bumps from Dependabot, skills and other tooling for working on Rue, and the version bump. They stay in the pull request list only.
- **Sometimes:** an internal change with a visible effect goes in as its effect ("lowering and name resolution are now linear in long chains of bindings, which used to take minutes"), and a dependency change goes in when it changes behaviour or matters to people who build on the crates ("the Chia crates were updated to their latest versions", "the server moved to `tower-lsp-server`").

### How it reads

- **Say what changed for the user.** Name the behaviour, not the mechanism: "a single syntax error no longer causes errors for the rest of the file", not "the parser recovers at synchronisation tokens".
- **Give the before when it explains the fix.** "Before, only the final expression was checked, so an early `return` could return a value of the wrong type."
- **Show the code.** Put syntax, names, flags, commands, files and error names in backticks, and give a short example where one makes the change clear: "as in `wrap::<_, Bytes>(1)`", "`(q 2 3)` is now `(1 2 3)`".
- **Present tense, plain words.** "now" for new behaviour, "no longer" for removed behaviour, "Fixed" for a bug. No marketing words, no "we", no thanks, no emoji.
- **Group, don't list everything.** Many small fixes in one place become one bullet that names the most important ones ("fixed several optimizations that made release builds behave differently from debug builds, including ...").
- **No pull request numbers, links or authors in the highlights.** The footer carries them.
- **Lists in a sentence** have no comma before "and".

## 4. Updating existing notes

When `current.md` exists, keep what's there and fold the pull requests in `new.txt` into it. The existing wording has usually been read and edited by the user, so don't rewrite bullets that the new pull requests don't touch.

- Add each visible change to the section it belongs in, or add a section in the right place by importance.
- When a new pull request changes something an existing bullet describes, edit that bullet to say what's true now.
- Add to Upgrading whatever the new changes need, and revisit the summary sentence if the release's weight has shifted.
- Replace everything from `<details>` to the end with the new `footer.md`.

Rewriting an older release in the current format (the user asks for this by version) is the same as writing new notes, with that release's tag and the one before it as the previous tag. Keep facts from the old notes that the pull requests don't state.

## 5. Check and save

Write the notes to `<scratchpad>/release-notes/<tag>/notes.md`, then check them:

- every pull request in `prs.tsv` is in the footer's list, and the list is in the script's order;
- every pull request that changes something a user can see is covered by a bullet, and nothing in the highlights is internal only;
- every change that can break or alter existing code or output is under Upgrading;
- the names, flags, counts and examples are right as of the release, not as of the pull request that introduced them;
- it reads like the latest release beside it.

Then save:

```bash
# a new release: always a draft, with no tag created
gh release create <tag> --draft --target main --title <tag> --notes-file <notes.md>

# an existing draft, or a published release the user asked to change
gh release edit <tag> --notes-file <notes.md>
```

The title is the bare version, like the tag. Check the result with `gh release view <tag> --json isDraft,url` and confirm a new release is still a draft.

## 6. Report

Tell the user:

- the release's URL, and whether it's a draft;
- which sections you added or changed (for an update, the bullets, not the whole text);
- anything you assumed, such as the version number;
- anything you couldn't settle from the pull requests and the code, with what you wrote in the meantime;
- that publishing the release is theirs to do, and that it creates the tag and publishes the crates, so the version bump has to be merged first.
