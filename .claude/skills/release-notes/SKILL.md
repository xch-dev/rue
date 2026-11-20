---
name: release-notes
description: Rewrite the description of an existing GitHub release of Rue into the release notes format the releases use now (a summary sentence, Highlights grouped by area, an Upgrading section, and the pull request list folded away at the end). Use this whenever the user asks to write, update, refresh or fix the release notes, release description or changelog of a Rue release they have already created, to "do the release notes for 0.13.0", to "add the new PRs to the release notes", or to redo an older release's notes in the current style, even if they only say "update the release". It only edits the description of a release that exists; it never creates a release, a tag or a changelog file, and never bumps the version.
---

# Release notes

Rue's release notes live in the description of each GitHub release; there is no changelog file. The user tags and creates the release themselves, usually with GitHub's generated "What's Changed" list as its description. This skill replaces that description with notes in the format every release since 0.8.5 follows, written for someone who uses Rue rather than someone who works on it. It also brings notes it wrote earlier up to date.

Read the latest release that has written notes before writing, so the result matches what's published now and not just this description of it. 0.12.0 and 0.11.0 are large releases and 0.8.5 is a small one: `gh release view 0.12.0 --json body --jq .body`.

## What you may and may not do

The only thing this skill changes is the description of a release that already exists, with `gh release edit <tag> --notes-file`.

- **Never create a release or a tag, and never publish a draft.** Pushing a version tag publishes every crate to crates.io (`.github/workflows/build.yml`), and that's the user's step. If the release doesn't exist, say so and stop; offer the notes as text if they'd help.
- **Never change anything else about the release:** not its title, tag, target, draft or prerelease state, or assets. Pass `gh release edit` no flag other than `--notes-file`.
- **Don't add a changelog file** or put the notes anywhere in the repository.

The user asked for the description to change, so edit it without asking again, and show what changed afterwards.

## 1. Find the release and collect its pull requests

The release is the one the user named, or otherwise the latest one (`gh release view --json tagName,isDraft`; `gh release list` also shows drafts). Then run the script, which only reads from GitHub:

```bash
.claude/skills/release-notes/scripts/release-prs.sh <tag> <scratchpad>/release-notes/<tag>
```

The previous tag is an optional third argument; by default it's the published release before this one. The script writes:

- `footer.md`: the end of the notes, ready to append as it is;
- `prs.tsv`: number, author and title of every pull request in the release;
- `prs/<n>.md`: each pull request's title and description;
- `current.md`: the description the release has now;
- `new.txt`: the pull requests the notes don't cover yet.

Its last line says which of three states the release is in:

- **Not in the notes format yet** (empty, or GitHub's generated list): write the notes from every pull request. Keep anything the user wrote by hand in `current.md` that isn't part of the generated list.
- **Has written notes:** `new.txt` lists only the pull requests missing from them; see section 4. When it's empty and the user didn't ask for a rewrite, the notes are up to date. Say so and stop.
- **No release for the tag:** stop, as above.

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

## 4. Updating notes that are already written

When the release already has notes in this format, keep what's there and fold the pull requests in `new.txt` into it. The existing wording has usually been read and edited by the user, so don't rewrite bullets that the new pull requests don't touch.

- Add each visible change to the section it belongs in, or add a section in the right place by importance.
- When a new pull request changes something an existing bullet describes, edit that bullet to say what's true now.
- Add to Upgrading whatever the new changes need, and revisit the summary sentence if the release's weight has shifted.
- Replace everything from `<details>` to the end with the new `footer.md`.

When the user asks for an older release to be redone in the current format, write it from scratch like a release with no notes, and keep facts from the old description that the pull requests don't state.

## 5. Check and save

Write the notes to `<scratchpad>/release-notes/<tag>/notes.md` (a working copy only; it isn't kept), then check them:

- every pull request in `prs.tsv` is in the footer's list, and the list is in the script's order;
- every pull request that changes something a user can see is covered by a bullet, and nothing in the highlights is internal only;
- every change that can break or alter existing code or output is under Upgrading;
- the names, flags, counts and examples are right as of the release, not as of the pull request that introduced them;
- it reads like the latest release beside it.

Then replace the release's description:

```bash
gh release edit <tag> --notes-file <scratchpad>/release-notes/<tag>/notes.md
```

Check the result with `gh release view <tag> --json isDraft,url,body` and confirm the description is the new one and the release's draft state is what it was.

## 6. Report

Tell the user:

- the release's URL;
- which sections you wrote, or for an update which bullets you added or changed;
- anything from the old description you kept or dropped;
- anything you couldn't settle from the pull requests and the code, with what you wrote in the meantime.
