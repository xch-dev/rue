# Rue × chialisp #579 debug sidecar: gap report

Spike: make Rue emit the #579 sidecar, run failing programs with `run_program_with_diagnostics`, render them with
`format_captured_stack`, and record where the format cannot express what Rue needs.

- **Currently tested:** chialisp `1ba4c714a3bb52f5f808d5f4586db22c9ce24650` (#579 head, "Use published clvmr
  0.22.0"), plus Rue #188 (Bram's main-wrapper source tag). chialisp now uses clvmr 0.22.0 from crates.io; the
  Rue side stays on the `05e31346` patch because `clvm-traits` still requires clvmr 0.21, and the spike only
  passes serialized bytes to chialisp, so the two versions coexist. Every output in `spike/out/` was regenerated.
- **Previously tested:** `69599b8c`, `014255b9` and, originally, `d65c2f9f`. The original findings are kept below
  the re-test sections.

All tracebacks below are verbatim Python-style output. The full output for each case (both styles, the program,
the function records and the probe results) is in `spike/out/*.txt`. Run `spike/run.sh` to regenerate it.

## Re-test against `1ba4c714` (with Rue #188)

Two changes affect the Rue traces: chialisp `4f61b787` collapses a verified curry wrapper into its body frame, and
Rue #188 tags the main capture/setup wrapper with the main body's span (no function or parameters, since its
environment differs from the body's). The emitter needed no changes. Probe results are unchanged.

- **N4a fixed.** The leading `in <unknown:hash>` frame is gone from every Python trace. The wrapper now renders
  as an unnamed expression frame at `main`'s body (e.g. `nested_assert.rue`, line 1, column 29, the `{`). Each
  sidecar has one more span (e.g. recursion 1616 → 1628 bytes).
- **N4b fixed.** Closures render one `in <lambda>` frame, keeping the `# bound:` annotation and captured values
  (closure_capture, closure_no_params). The three recursive `total` calls remain.
- **N1 unchanged.** Identical bodies still render `in <ambiguous: first | second>`. Bram accepts ambiguity as the
  limit for #579; resolving it needs call-target provenance.
- **H2 unchanged for Rue.** `4f61b787` fixes Chialisp's `*macros*` locations (frames now point at the user's
  `if`), but Rue frames still use the body root's span. A `definition_span` is still a follow-up.
- The Lisp style labels every unnamed expression frame `(<unknown:hash>)`, including the now-located wrapper.
  This was already true of other expression frames and isn't new.
- Everything else (H1, H3a, H4, H6, N2, N3) is byte-identical to `69599b8c`.

## Re-test against `69599b8c`

The only change is in the decoder, and the Rue emitter needed no changes. **Every traceback is byte-identical to
`014255b9`.** The only lines that changed in `spike/out/*.txt` are the `probe:` lines.

The probe now has a sixth mutation: it replaces the first named parameter's constraint with an unknown tag `(99)`.
It also prints the constraints of the decoded first function. In every case (nested_assert shown):

```
probe: extra trailing root field (e.g. producer info): Ok("accepted (first function constraints: integer)")
probe: version 2: Err("unsupported debug metadata version 2")
probe: extra field on a shadow node (e.g. inlined-from): Ok("accepted (first function constraints: integer)")
probe: extra field on a span entry (e.g. byte offsets): Ok("accepted (first function constraints: integer)")
probe: extra field on a function record (e.g. definition span): Ok("accepted (first function constraints: integer)")
probe: extra: unknown constraint tag 99 on first named parameter: Ok("accepted (first function constraints: unknown)")
```

This is E1, implemented as asked. Details, from the source and the PR's own tests:
- Every record now accepts trailing fields: the root, coordinates, files, spans, functions, parameters,
  constraints and shadow nodes.
- Unknown constraint tags decode as `Unknown`, and a union with an unknown member collapses to `Unknown`.
- The version stays 1 and still rejects 2. Under the new policy the version only changes for breaking changes,
  which covers the major/minor split this report asked for.
- Still rejected:
  - missing required fields;
  - unknown *structural* tags (shadow node kind, parameter kind);
  - a different magic.
- Extension fields are dropped on decode: `DebugMetadata` has nowhere to keep them. A v1 reader can't use a newer
  producer's extensions, but it no longer rejects the sidecar. That's the property that matters before merge.

**Consequence:** every additive row in the proposed-changes table is now additive after merge. The ones that
stay breaking change the meaning of an existing field or tag: replacing line/column with byte offsets, renaming
the magic, changing the identity hash, or adding a new shadow-node or parameter kind instead of a field.

## Re-test against `014255b9`

The Rue emitter needed **no changes**. The public types (`DebugMetadata`, `DebugNode`, `DebugParameter`,
`DebugFunction`, `ParameterConstraint`) and the encoding are unchanged. The only public API change is the new
`FrameMatch::Ambiguous` variant.

**Format, checked by the decoder probe:** still version 1, magic `CHIALISP_DEBUG`, coordinates `(1 8)`, and exact
field counts everywhere. All five probes are rejected with the same errors as before, in every case
(`grep probe out/*.txt`). The changes are all in the renderer:
- frames that match more than one shadow node are reported as `Ambiguous`;
- the search is scoped to the enclosing frame's subtree, falling back to the whole tree;
- curries ending in `()` are accepted;
- an empty tail parameter is no longer labeled `bound`;
- a destructured parameter that receives an atom now renders its pattern instead of failing.

### Status of every report item

| ID | Before (`d65c2f9`) | After (`014255b9`) | Evidence |
|---|---|---|---|
| H1 inlining | confirmed | **unchanged** | `out/inline_fail.txt` identical: line 2 call site still missing, inline body still anonymous |
| H2 function location | confirmed | **unchanged** | `in outer` still points at the `{` (5:29); `in inner` at the first assert (11:5) |
| H3 calling convention | not a blocker | **unchanged (not a blocker)** | `closure_capture`, `nested_assert`, `struct_*` outputs identical |
| H3a declaration order | open | **unchanged** | `total(f, items)` still lists `items` before `f` |
| H3b `bound` on empty tail | renderer bug | **fixed** | `in countdown  # bound: acc` → `in countdown` (recursion*, typed_param_values) |
| H3c `()`-tail curry | unknown frame | **fixed** | `in <unknown:a723…>` → `in <lambda>  # bound: x` with `x = 3` (closure_no_params) |
| H3d `Nil` = skip slot | undocumented | **unchanged** | spec text not touched |
| H4 types | confirmed | **unchanged** | still `amount = "hell"`, `memo = 26729`, `enabled = ()` |
| H5 strict decoder | confirmed | **unchanged in `014255b9`; fixed in `69599b8c`** | at `014255b9`, 5/5 probes rejected; at `69599b8c`, the 4 additive probes and an unknown constraint tag are accepted and version 2 is rejected by design |
| H6a tab columns | confirmed | **unchanged** | Rue `7:3`, sidecar still `line 7, column 16` (recursion_tabs) |
| H6b producer / magic | confirmed | **unchanged** | magic `CHIALISP_DEBUG`, 9-field root, no producer field (probe 1) |
| H6c identity hash | confirmed | **unchanged** | still SHA-256 of serialized bytes (`program_sha256`) |
| N1 identical subtrees | misattributed | **partly fixed** | identical bodies: `in second … b = -1` → `in <ambiguous: first \| second>` (no longer wrong, still unresolved); identical sub-expression in distinct bodies: leaf line 11 → line 6 (resolved) |
| N2 `let` locals | only by faking a function | **unchanged** | `out/nested_assert_let_scopes.txt` identical |
| N3 as-pattern parameters | not expressible | **unchanged** | `DebugParameter` still `Nil`/`Name`/`Pair`; struct_destructure identical |
| N4a `<unknown:hash>` plumbing frame | renderer issue | **unchanged; fixed by Rue #188 + `4f61b787`** | was the first line of nested_assert, closure_capture, recursion, …; see "Re-test against `1ba4c714`" |
| N4b closure frame shown twice | renderer issue | **unchanged, now wider; fixed in `4f61b787`** | at `014255b9`, parameterless closures also rendered `in <lambda>` twice (closure_no_params); one frame each at `1ba4c714` |
| N5 size | informational | **unchanged** | sidecar byte counts identical (e.g. recursion 1616) |
| E1 tolerant decoder | requested | **not done in `014255b9`; done in `69599b8c`** | see "Re-test against `69599b8c`" |
| Table rows 1, 2, 3a, 4, 6a–6c, N2, N3 | requested | **not done** (still true at `69599b8c`, but now additive after merge where the table says "no" under "With E1") | no new fields or tags in the encoder or decoder |
| Table rows 3b, 3c | requested | **done** | see H3b, H3c |
| Table row N1 (report ambiguity) | requested | **done**, plus scoped search | see N1 |
| Table row N4 | requested | **done by `1ba4c714`** | see N4a, N4b |

### N1 in detail

**Identical function bodies** (`duplicate_bodies.rue`, release, `first(x)` fails with `x = -1`):

Before:
```
File "duplicate_bodies.rue", line 2, column 5
  first(x) + second(y)
  ^^^^^^^^
File "duplicate_bodies.rue", line 11, column 5, in second
  assert b > 0;
  ^^^^^^^^^^^^^
  b = -1
```
After:
```
File "duplicate_bodies.rue", line 2, column 5
  first(x) + second(y)
  ^^^^^^^^
in <ambiguous: first | second>
  arguments = [-1]
in <ambiguous>
  arguments = [-1]
```

This is honest now, but still unresolved, and the leaf `assert` frame has lost its location along with its name.

The enclosing-frame scope can't help here, because of how Rue lays out functions. A callee's body isn't inside
its caller's subtree. All function bodies are quoted into `main`'s capture wrapper, `(c (c (q . A) (q . B)) 1)`,
and `main`'s body only reaches them through environment paths (`(a 6 …)`). So the search finds nothing in the
caller's scope, falls back to the whole tree, and finds both bodies. Chialisp's `defun`s have the same shape
(they live in the left environment), so this probably affects Chialisp too.

**What would resolve it:**
- *Renderer:* follow the parent frame. Take the path the caller applied (`6`), resolve it in the caller's
  environment, and find which quoted shadow subtree was bound into that slot.
- *Format:* a per-node "bound at" link from each function record to the capture slot that carries it.

**Identical sub-expression inside distinct bodies** (new case `duplicate_subexpr.rue`, release; `first(-1, 5)`
fails). The bodies differ, but both contain the identical raise `(q 8)`.

Before (`d65c2f9`, rebuilt for this comparison):
```
File "duplicate_subexpr.rue", line 6, column 5, in first
  assert a > 0;
  ...
File "duplicate_subexpr.rue", line 11, column 5
  assert a > 0;
```
After:
```
File "duplicate_subexpr.rue", line 6, column 5, in first
  assert a > 0;
  ...
File "duplicate_subexpr.rue", line 6, column 5
  assert a > 0;
```

The leaf frame used to land in `second` (line 11). With the enclosing `first` body as its scope, it now resolves
correctly.

### New problems introduced by the update

- **Ambiguous frames lose their location.** An ambiguous leaf renders as a bare `in <ambiguous>` with no
  file or line, even when every candidate shares a function or is in the same file. Before, it showed one
  candidate's location, which could be wrong. Showing every candidate location, or the span they have in
  common, would keep some information.
- **Ambiguous frames list arguments as a raw positional list** (`arguments = [-1]`), even when every candidate's
  parameter tree agrees on the names (here `a` and `b` have the same shape and constraint).
- **N4b now also affects parameterless closures**, a consequence of fixing H3c.

---

# Original findings (`d65c2f9`)

The sections below are the original analysis, unchanged except for status markers. Wherever they quote output
that the update changed, the re-test section above has the new output.

## Summary (as of `d65c2f9`)

| # | Hypothesis | Verdict for Rue |
|---|---|---|
| 1 | No inlined-from chain | **Confirmed.** The inlined call site disappears completely; the inline body shows up as an anonymous expression frame. |
| 2 | Function records have no definition span | **Confirmed (milder than Chialisp).** Frames point at the body's `{` or at its first statement, depending on how the body is shaped. Rue has the definition span (`FunctionSymbol.name` carries a `SrcLoc`) but the format has nowhere to put it. |
| 3 | Positional binding doesn't fit Rue's layout | **Not confirmed as a blocker.** If the parameter tree mirrors the real environment tree, with `Nil` in the slots to hide, positional binding equals structural binding for every Rue layout tested. Three problems remain: parameters display in environment order, not declaration order; a renderer bug labels empty tail parameters `bound`; and closures without parameters aren't recognized as curries. |
| 4 | Constraints can't carry Rue types | **Confirmed.** `Int` displays as `"hell"`, `String` as `26729`, `false` and `0` as `()`, structs without field names, and function values as raw CLVM. |
| 5 | Strict decoder, no room to grow | **Confirmed by probe.** All five additive changes tried are rejected by the v1 reader. |
| 6 | Smaller issues | **Confirmed.** A tab-indented line: Rue says `7:3`, the sidecar stores `7:16`. The magic is Chialisp-specific with no producer field, and identity is a hash of the serialized bytes. |

New gaps: structurally identical subtrees get misattributed (a failure in `first` is reported as `second`); `let`
locals can't be shown; destructured parameters can't name both the whole value and its parts; plus some renderer
issues (spurious `<unknown:…>` frames, closure frames shown twice).

**The most important request is E1, a forward-compatible decoder, before #579 merges.** (Done in `69599b8c`;
see the re-test section.) As written, every change
below is breaking after merge, because a v1 reader rejects any sidecar with an extra field. With E1 in place, most
of them become additive.

## Step 1: what Rue's compiler keeps

**Spans.** The pipeline is AST (rowan, byte `TextRange`) → HIR → LIR → CLVM.
- HIR nodes carry no spans. The exceptions are `Statement::Assert/Raise/Debug(…, SrcLoc)` and `ConstExpr`.
- LIR and codegen carry none.
- In debug builds the only source positions that reach the output are the `"assertion failed at file:line:col"`
  strings inside `raise`.
- Function names keep a `SrcLoc` (`Name`), so definition spans exist.

Since this isn't "no spans at all", I added side tables to thread them through, marked `// SPIKE`:
- `compile_expr` and `compile_block` record `HirId → SrcLoc`; the innermost expression wins.
- `Lowerer::lower_hir` tags every LIR node it allocates with the HIR node's span, unless an inner node already
  tagged it.
- `assert` and `raise` tag their `If` and `Raise` nodes with the statement span.
- `optimize` records which original node each optimized node came from, so release builds keep their spans.
- `codegen` maps LIR tags onto CLVM pairs.
- Atoms get their parent's span. clvmr interns small atoms by value, so they can't be told apart by `NodePtr`.

**Environment layout** (`rue-hir/src/lower.rs`, `environment.rs`):
- A normal `fn` (`FunctionKind::BinaryTree`): `env = (cap_k . … (cap_1 . PARAMS))`.
  - `PARAMS` is a **balanced binary tree ordered by reference count**. It is not a list, and not in declaration
    order: `fn total(f, items)` becomes `(items . f)`. With two parameters, the last one is the environment's
    *tail*, not a list element.
  - Each capture group is also a tree.
  - Captures are mostly other functions referenced by path. A recursive function captures itself.
- `extern fn` and closures/lambdas use sequential parameters: a nil-terminated list, or an improper tail for
  `...rest`.
- `main` takes the solution as a plain list. Its captures are bound by nested `(a (q . BODY) (c GROUP 1))`
  wrappers around the body.
- Closures are runtime curries: `(a (q . BODY) (c (q . cap1) … 1))`, with captures first. A closure **with no
  parameters** uses a `()` tail instead of `1`.
- Every `let` group is its own apply: `(a (q . REST) (c BINDINGS 1))`. So locals live in frames that are not
  function frames.
- A function value with captures is a curried closure. Captured inline arguments are captured by value.

**Inlining.**
- `inline fn` is always inlined.
- With `auto_inline`, which is the default and is turned off by `CompilerOptions::debug()`, Rue also inlines any
  non-recursive function referenced once whose parameters are each referenced at most once. Constants and `let`
  bindings referenced once are inlined the same way.
- Inline arguments are substituted as HIR and lowered in the caller's environment. A parameter can therefore turn
  into a constant or an arbitrary expression, with no environment slot.

## Method

- Scratch crate `spike/rue-debug-spike`. It has its own workspace and lockfile, because Rue's lock conflicts with
  chialisp's `sha2`/`serde_json` versions.
- It depends on `chialisp` from git at the PR commit and patches `clvmr` to `05e31346` with `pre-eval`. Only one
  clvmr ends up in the tree.
- The sidecar is built directly from the PR's `DebugMetadata`/`DebugNode`/`DebugParameter` types and encoded with
  `DebugMetadata::encode`. `verify_program` passes on every case.
- Function records: one per non-inlined function body root, with `left_env = false`. The parameter tree is Rue's
  actual `Environment` for that body:
  - `Leaf(param)` → `Name`, with its real path and a constraint derived from the Rue type.
  - Captured functions → `Nil`, so they stay hidden.
  - `Pair` → `Pair`.
- Programs are compiled with `CompilerOptions::debug()`. A few runs use release options (`release` in the case
  name).

## Hypotheses

### 1. Inlining: confirmed (unchanged in `014255b9`)

`inline_fail.rue`: `let y = checked_sub(x, 10);` on line 2 calls `inline fn checked_sub`, which raises.

```
File "inline_fail.rue", line 1, column 24, in main
  fn main(x: Int) -> Int {
                         ^
  x = 5
File "inline_fail.rue", line 6, column 46
  inline fn checked_sub(a: Int, b: Int) -> Int {
                                               ^
File "inline_fail.rue", line 8, column 9
  raise "underflow";
  ^^^^^^^^^^^^^^^^^^
```

- Line 2, the call site, appears nowhere.
- The inline body has no name and no arguments, and `a`/`b` can't be shown.
- Each node has exactly one span, so the emitter has to pick the callee's span or the call site's; it can't give
  both.
- An `inline` function record attached to the inlined body root doesn't help either. `b` is the constant `10`, and
  `DebugParameter` can only point at environment paths.

The same thing happens without `inline` whenever release builds auto-inline a function that is referenced once.

**Smallest fix:** an optional `inlined_at` index on shadow nodes, pointing into a new table of inline sites
`(call_span, function, parent_site)`. The renderer turns each site into a synthetic frame. Optionally, also give
each inline site argument bindings, each either `(path P)` or `(const V)`.

### 2. Function location: confirmed (unchanged in `014255b9`)

A function frame's location is the span of its body's root node, and that depends on the body's shape:

- `in outer` → `line 5, column 29`, the `{` of `fn outer(value: Int) -> Int {`, because the body starts with a `let`.
- `in inner` → `line 11, column 5`, `assert total > 100;`, because the body's root is the assert.
- `in main` → `line 2, column 5`, the first expression `outer(amount) + 1`.

This is better than Chialisp's `*macros*` `if`, but it's inconsistent. It also can't be fixed on the emitter side:
moving the root node's span to the definition loses that node's own expression location.

**Smallest fix:** an optional `definition_span` on `DebugFunction`.

### 3. Calling convention: not confirmed as a blocker (3b and 3c fixed in `014255b9`)

The parameter tree mirrors the real environment and uses `Nil` for hidden slots. `top_level_parameters` plus
tail reconstruction then turns out to be equivalent to structural destructuring. Every layout tested bound
correctly:
- binary-tree parameters, including a tail parameter;
- several capture groups (`left_env` isn't needed);
- curried closures, where the curry's bound arguments come first, exactly like Rue's captures;
- main's capture wrapper.

`closure_capture.rue` (closure captures `limit`, called through `fn total(f, items)`):

```
File "closure_capture.rue", line 9, column 60, in total
  fn total(f: fn(item: Int) -> Int, items: List<Int>) -> Int {
                                                             ^
  items = (30 4)
  f = (2 (1 2 (...) 1) (4 (...) 1))
...
File "closure_capture.rue", line 3, column 9, in <lambda>  # bound: limit
  assert item < limit;
  ^^^^^^^^^^^^^^^^^^^^
  limit = 10
  item = 30
File "closure_capture.rue", line 3, column 9, in <lambda>
  assert item < limit;
  ^^^^^^^^^^^^^^^^^^^^
  limit = 10
  item = 30
```

Problems found:

- **3a. Display order is environment order.** `total(f, items)` shows `items` before `f`, because Rue orders the
  tree by reference count. Positional binding can't reorder. **Fix:** an optional declaration ordinal on `Name`.
  Alternatively, bind by the stored paths (they're already in the record) and list formals in declaration order.
- **3b. Renderer bug: an empty tail parameter is labeled `bound`.** In `materialize_frame_arguments`,
  `remaining.iter().all(Bound) && runtime_tail.is_none()` is vacuously true when nothing is left. Every Rue
  binary-tree function whose last parameter is the nil tail hits this:
  ```
  File "recursion.rue", line 5, column 39, in countdown  # bound: acc
    n = 3
    acc = ()
  ```
  `acc` is a runtime argument here; it's labeled `bound` only because it's `0`. Renderer fix only.
- **3c. Curry recognition is hard-coded to `(c (q . X) … 1)`.** Rue's closure without parameters ends in `()`, so
  its wrapper frame is unknown (`closure_no_params.rue`):
  ```
  in <unknown:a72366285622bb61ff75effd4a68e9f7d069dc8311b6126a7abecf9a544f9764>
  File "closure_no_params.rue", line 3, column 9, in <lambda>
  ```
  **Fix:** either the renderer also accepts a `()` tail, or Rue emits `1`, which is harmless for a closure that
  takes no parameters. Not a format change.
- **3d.** `Nil` means "consume a slot, bind nothing". That's what lets Rue hide plumbing, but the spec should say
  so explicitly, so readers don't treat it as "expects nil".

### 4. Types: confirmed (unchanged in `014255b9`)

`typed_param.rue` with `amount: Int = 1751477356`, `memo: String = "hi"`, `enabled: Bool = false`:

```
File "typed_param.rue", line 6, column 5, in validate  # bound: enabled
  assert enabled;
  ^^^^^^^^^^^^^^^
  owner = 0xaaaa…aaaa
  target = 0xbbbb…bbbb
  amount = "hell"
  memo = 26729
  enabled = ()
```

Other cases show the same problem:
- `Int` zero renders as `()` (`n = ()` in `recursion.rue`).
- Structs have no field names: `payment = (0x859f… 100)` instead of `Payment { puzzle_hash: …, amount: 100 }`.
- Function values render as raw CLVM: `f = (2 (1 2 (...) 1) (4 (...) 1))`.
- `Bool` (`true | false`), structs, function types and generics all map to `unknown`.
- `Bytes32` and `PublicKey` map to `bytes[32]`/`bytes[48]`, but those tags only appear in Lisp style and never
  change how a value is displayed.

**Smallest fix:** an optional `type` string index on each `Name`, used for display, plus a display hint from an
open set (`int`, `bytes`, `string`, `bool`, `pubkey`, `fn`, …) that the renderer uses to format the value.
Struct field decoding can come later through the same extension point, e.g. a type table of
`(name, ((field . path) …))`.

### 5. No room to grow: confirmed (unchanged in `014255b9`; fixed in `69599b8c`)

The probe runs in every case (`probe:` lines in `out/*.txt`). It changes a real Rue sidecar and decodes it with
the v1 reader:

```
probe: extra trailing root field (e.g. producer info): Err("not Chialisp structural debug metadata")
probe: version 2: Err("unsupported debug metadata version 2")
probe: extra field on a shadow node (e.g. inlined-from): Err("unknown or malformed debug shadow node")
probe: extra field on a span entry (e.g. byte offsets): Err("malformed debug span entry")
probe: extra field on a function record (e.g. definition span): Err("malformed debug function record")
```

Constraint tags are also a closed set (unknown tag → error), and the coordinate list must be exactly `(1 8)`.

**Fix (E1):**
- Decoders accept *at least* N fields on every record (root, file, span, function, parameter, shadow node) and
  ignore the rest.
- Unknown constraint tags decode as `Unknown`.
- Version is split into a major number, which rejects, and a minor number, which is accepted.
- Optionally, a trailing root `extensions` alist `((key . value) …)`.

This only helps if it lands **before merge**: readers already deployed can't be retrofitted.

### 6. Smaller issues: confirmed (unchanged in `014255b9`)

- **6a. Columns.** In `recursion_tabs.rue`, `\t\traise "hit zero";` is reported by Rue as
  `raise called at recursion_tabs.rue:7:3`, while the sidecar stores and renders `line 7, column 16`. Columns are
  tab-expanded character counts: not bytes (Rue, rowan), and not UTF-16 (LSP). The full source is embedded, so
  **fix:** store byte offsets `(file, start_byte, end_byte)` and let renderers compute display columns. Adding
  them alongside line/column is additive with E1; replacing line/column is breaking.
- **6b. Producer.** The magic is `CHIALISP_DEBUG` and there's no producer field. A Rue sidecar is a "Chialisp"
  sidecar. **Fix:** an optional `(producer "rue" "0.11.0")` field (additive with E1). Renaming the magic to
  something neutral is breaking.
- **6c. Program identity.** The identity is SHA-256 of the serialized bytes. The collection re-serializes frames
  canonically, so a producer that hashes back-reference-compressed bytes never gets an identity match and falls
  back to structural matching. Chia tooling identifies puzzles by tree hash. **Fix:** use the tree hash as the
  identity, or add it alongside. Changing what the existing field means is breaking; adding a second field is
  additive with E1.

## New gaps

### N1. Identical subtrees are misattributed (partly fixed in `014255b9`)

`exact_tree_match` returns the first matching subtree in depth-first order. In release builds `first` and
`second` compile to byte-identical bodies, `(2 (i (> 1 ()) (q 18 1 1) (q 8)) 1)`. In `duplicate_bodies.rue`
with `x = -1`, **`first(x)` fails**:

```
File "duplicate_bodies.rue", line 2, column 5
  first(x) + second(y)
  ^^^^^^^^
File "duplicate_bodies.rue", line 11, column 5, in second
  assert b > 0;
  ^^^^^^^^^^^^^
  b = -1
```

The function, line and parameter name are all wrong. The collection already treats a match shared between
*sidecars* as ambiguous, but doesn't check for ambiguity *within* one sidecar. Small helpers, generic
instantiations and identical expressions make this common in Rue release builds.

**Smallest fix (renderer only):** when several shadow nodes match with different function or span, render them
as ambiguous (`first | second`) rather than picking one.

**Real fix (outside the format):** let frames carry their position, e.g. the path of the node inside its parent
frame's program, so the renderer can follow the call chain.

### N2. `let` locals can't be shown (unchanged in `014255b9`)

Each Rue `let` group is a CLVM apply frame `(BINDINGS . PARENT_ENV)`. The only way to name its bindings is to
pretend the scope is a function (`SPIKE_LET_SCOPES=1`, `out/nested_assert_let_scopes.txt`):

```
File "nested_assert.rue", line 5, column 29, in outer
  value = 10
File "nested_assert.rue", line 7, column 5, in <let scope>
  inner(doubled, value) + doubled
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  doubled = 20
  value = 10
```

That works, but the result reads as an extra call. **Fix:** an optional `kind` on `DebugFunction`
(`function` | `scope` | `inline`). The renderer folds `scope` frames into the enclosing function frame, as
locals. The same field would cover hypothesis 1's inline sites.

### N3. A destructured parameter can't name both the whole and its parts (unchanged in `014255b9`)

`DebugParameter` is `Name` *or* `Pair`. Rue patterns like `{ x, ...point }`, and the common need to show
`payment = …` alongside `amount = 100`, need a name with sub-structure (an as-pattern). Chialisp's `@` patterns
can't be expressed either. The spike emitted the whole value:

```
File "struct_param.rue", line 15, column 5, in by_destructure
  assert puzzle_hash != sha256("burn");
  { puzzle_hash, amount } = (0x859f… 100)
```

**Fix:** an optional `inner: DebugParameter` on `Name`, additive with E1.

### N4. Renderer issues (no format change; unchanged in `014255b9`, and the duplicate-frame issue now also affects parameterless closures)

- A node that matches exactly but has no span, function or label renders as `in <unknown:hash>`, which looks
  like a symbolization failure. Rue's main capture wrapper produces this as the first line of almost every
  trace. Emitters can work around it with a `label`; better would be to skip such frames, or have a `hidden`
  flag.
- A closure call renders twice: the curried wrapper frame and then the body frame both show `in <lambda>` with
  the same location and arguments. Identical frames are collapsed only when the raw program and environment are
  identical; collapsing on (function, span, rendered arguments) would merge these.

### N5. Size (informational; unchanged in `014255b9`)

The shadow tree repeats every node and atom value, and each file's full source is embedded. A 142-byte program
gives a 1616-byte sidecar (`recursion.rue`); the cases here range from 1.2 to 3.1 KB. That's fine for local
debugging. It's only worth revisiting if sidecars get published next to puzzles.

## Proposed changes, and whether they break after #579 merges

Status in `014255b9`: 3b, 3c and N1 are done in the renderer. Nothing else is done, E1 included, so the "without E1" column still applies.

Status in `69599b8c`: **E1 is done**, so the "With E1" column now applies. Everything marked "no" there can be added after merge without breaking v1 readers. One caveat: a change that needs a new shadow-node or parameter *kind* (rather than a field) is still breaking, because unknown structural tags are still rejected. N3 should therefore be done as a field on `Name`, as proposed, not as a new parameter kind.

"With E1" means E1 lands before merge.

| ID | Change | Breaking after merge without E1 | With E1 |
|---|---|---|---|
| **E1** | Tolerant decoder: ignore extra fields, unknown tags → `Unknown`, major/minor version | **done in `69599b8c`** (the version now only changes for breaking changes) | n/a |
| 1 | `inlined_at` per node plus an inline-site table (call span, function, parent site, optional argument bindings) | yes | no |
| 2 | `definition_span` on `DebugFunction` | yes | no |
| 3a | Declaration ordinal on `Name`, or bind by stored paths | yes | no (bind-by-path changes reader behavior only) |
| 3b | Empty tail parameter labeled `Runtime`, not `Bound` | no (renderer) | no |
| 3c | Treat a `()` tail as a curry | no (renderer) | no |
| 3d | Document `Nil` as "skip slot" | no (spec text) | no |
| 4 | `type` string plus display hint on `Name`; later a type table | yes | no |
| 6a | Byte offsets on spans | yes (replacing columns is breaking even with E1) | no if added alongside |
| 6b | `producer` field / neutral magic | yes | no for producer; magic rename is still breaking |
| 6c | Tree hash as identity | yes (changes field meaning) | no if added alongside |
| N1 | Report ambiguous matches inside one sidecar | no (renderer) | no |
| N2 | `kind` on `DebugFunction` (`function`/`scope`/`inline`) | yes | no |
| N3 | `inner` sub-pattern on `Name` | yes | no |
| N4 | Skip unlabeled plumbing frames; better collapsing of duplicate frames | no (renderer) | no |

## What's in the spike

- Rue compiler (spike-only, `// SPIKE` comments):
  - `rue-hir` `database.rs` (`DebugTables`) and `lower.rs`
  - `rue-compiler` `compile/expr.rs`, `compile/block.rs` and `file.rs`
  - `rue-lir` `codegen.rs` (`DEBUG_TAGGING`) and `optimize.rs` (`OPT_TRACE`). `OPT_TRACE` is always on and never
    cleared, so it leaks in long-running processes such as the LSP. That's fine for a spike.
- `spike/rue-debug-spike/src/main.rs`: sidecar builder, runner, renderer and forward-compatibility probe.
- `spike/cases/*.rue`: the test programs, including `duplicate_subexpr.rue`, added for the re-test. `spike/out/*.txt`: rendered output. `spike/run.sh`: regenerates `out/`.
