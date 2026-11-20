#!/usr/bin/env bash
# Collects what a Rue release's notes are written from.
#
#   release-prs.sh <tag> <out-dir> [previous-tag] [target]
#
# <tag> is the release being written (it doesn't have to exist yet), [previous-tag] defaults to the
# latest published release, and [target] to main. Writes into <out-dir>:
#
#   footer.md    the end of the notes: the "All pull requests" block, new contributors and the
#                changelog link, exactly as they go into the release
#   prs.tsv      number, author and title of every pull request in the release, in merge order
#   prs/<n>.md   each pull request's title, labels and description
#   current.md   the notes the release has now, if it already exists (draft or published)
#   new.txt      the pull request numbers that current.md doesn't list yet (all of them when
#                there's no release)
#
# Only reads from GitHub. It never creates, edits or publishes a release.
set -euo pipefail

if [ $# -lt 2 ]; then
    sed -n '2,17p' "$0" | sed 's/^# \{0,1\}//'
    exit 2
fi

tag=$1
out=$2
previous=${3:-}
target=${4:-main}

repo=$(gh repo view --json nameWithOwner --jq .nameWithOwner)

if [ -z "$previous" ]; then
    # The latest published release other than the one being written.
    previous=$(gh api "repos/$repo/releases" --paginate \
        --jq '.[] | select(.draft | not) | .tag_name' | grep -vx "$tag" | head -n 1)
fi

mkdir -p "$out/prs"

generated=$(gh api "repos/$repo/releases/generate-notes" \
    -f tag_name="$tag" -f previous_tag_name="$previous" -f target_commitish="$target" --jq .body)

# GitHub's notes are a "What's Changed" list, an optional "New Contributors" section and the
# changelog link. The release keeps all three, with the list folded away.
list=$(printf '%s\n' "$generated" | grep '^\* .* by @[^ ]* in https://.*/pull/[0-9][0-9]*$' || true)
rest=$(printf '%s\n' "$generated" | awk '/^## New Contributors/ || /^\*\*Full Changelog\*\*/ { on = 1 } on')

{
    printf '<details>\n<summary>All pull requests</summary>\n\n'
    printf '%s\n' "$list"
    printf '\n</details>\n\n'
    printf '%s\n' "$rest"
} > "$out/footer.md"

: > "$out/prs.tsv"
printf '%s\n' "$list" | while IFS= read -r line; do
    [ -n "$line" ] || continue
    number=${line##*/pull/}
    author=$(printf '%s\n' "$line" | sed -E 's/.* by @([^ ]+) in https:.*/\1/')
    title=$(printf '%s\n' "$line" | sed -E 's/^\* (.*) by @[^ ]+ in https:.*/\1/')
    printf '%s\t%s\t%s\n' "$number" "$author" "$title" >> "$out/prs.tsv"
    if [ ! -s "$out/prs/$number.md" ]; then
        gh pr view "$number" --repo "$repo" --json number,title,labels,body --jq \
            '"# #\(.number) \(.title)\nLabels: \([.labels[].name] | join(", "))\n\n\(.body)"' \
            > "$out/prs/$number.md"
    fi
done

rm -f "$out/current.md"
if gh release view "$tag" --repo "$repo" --json body --jq .body > "$out/current.raw" 2>/dev/null; then
    # Notes edited on GitHub come back with CRLF line endings.
    tr -d '\r' < "$out/current.raw" > "$out/current.md"
    rm -f "$out/current.raw"
    cut -f1 "$out/prs.tsv" | while IFS= read -r number; do
        grep -q "/pull/$number\$" "$out/current.md" || echo "$number"
    done > "$out/new.txt"
    state="existing notes in current.md"
else
    rm -f "$out/current.raw"
    cut -f1 "$out/prs.tsv" > "$out/new.txt"
    state="no release for $tag yet"
fi

total=$(wc -l < "$out/prs.tsv" | tr -d ' ')
new=$(wc -l < "$out/new.txt" | tr -d ' ')
echo "$repo $previous...$tag (target $target): $total pull requests, $new not in the notes yet; $state"
