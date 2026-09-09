# Root identity: resolved `--start-from` roots do not survive declaration ordering

## Observation

Selecting an inherent method as a Charon root with its user-facing path
(`--start-from=rootid::View::refine`) translates the method, its receiver type
and its callees, and then produces an LLBC crate that contains no items at all.
The same crate selected by a free function (`rootid::free`) keeps every item,
and selected by the type (`rootid::View`) keeps only the struct. Declaration
order does not change any of these results; the two cells in `cells/` declare
the type and the impl in both orders and measure identically.

This is the tiny form of the M0 DAE observation in `dev/ud07-extraction-gt77Yb/`
(explicit `Type::method` selection captures 79 functions but orders only two
types).

## Owner chain

Charon pinned source `/nix/store/2058cyr4b25jmx5809k7i36ijfh4cflw-source/charon`:

1. `src/bin/charon-driver/translate/translate_crate.rs:1076-1084`. A
   `StartFrom::Pattern` is resolved by rustc path resolution
   (`ctx.resolve_path`) to `DefId`s and each is enqueued through
   `enqueue_module_item`. The resolved identities are used only to enqueue;
   nothing records them. Impl patterns that are not the first path element are
   refused here (`method-impl-spelling` records, exit 2).
2. `src/options.rs:629-641` `StartFrom::matches`. The same option is
   re-evaluated against `ItemMeta.name`, Charon's own `Name`, through
   `NameMatcher`. This is the second spelling: the inherent method's `Name` is
   `rootid::{impl View}::refine`, an `Impl(ImplElem::Ty)` element that the
   pattern `rootid::View::refine` (an `Ident` element) never matches.
3. `src/name_matcher/mod.rs:270-273`. `(PatElem::Impl, PathElem::Impl(ImplElem::Ty))`
   is a literal `// TODO` returning `false`, and `(Ident, Impl(..))` falls to
   the `_ => false` arm, so no pattern spelling can name an inherent-impl item
   through `NameMatcher` at all.
4. `src/transform/add_missing_info/reorder_decls.rs:374-391`
   `compute_declarations_graph`. Seeds the reachability walk from builtins plus
   `StartFrom::matches` over all items, then keeps only what the walk reaches.
   With the method unseeded, nothing is reached, and the ordered declaration
   list is empty. `StartFrom::matches` has no other consumer.
5. `src/export/multi_target.rs:753-757` `cleanup_post_merge` runs the same
   pass on merged, deserialized crates, where no rustc-resolved identity exists.

## Measurements

Tool: `/tmp/rumoca-demand-retention-QVu1kr/charon` (sha256 `10ca62cd...`,
the retention-lane build of the pinned source), preset `aeneas`,
`--error-on-warnings`, edition 2024, crate `rootid`. Full argv, environment,
exit codes and log hashes are in `run-evidence.json` (10 records, 0
expected/observed mismatches).

| cell | selection | stage | exit | functions | structs |
|---|---|---|---|---|---|
| order-type-impl | `rootid::View::refine` | ULLBC (pre-reorder) | 0 | len, refine | View |
| order-type-impl | `rootid::View::refine` | LLBC (post-reorder) | 0 | none | none |
| order-type-impl | `rootid::{impl rootid::View}::refine` | LLBC | 2 (resolver refusal) | | |
| order-type-impl | `rootid::free` | LLBC | 0 | free, len, refine | View |
| order-type-impl | `rootid::View` | LLBC | 0 | none | View |
| order-impl-type | same five selections | | identical to the rows above | | |

The ULLBC row is the proof that resolution succeeded: `refine` and its callee
`len` are translated. The LLBC row is the failure: the ordering pass, not
translation, erases them.

## Fix: identity-carrying roots

Declared in the ledger before the producer edit; implemented as `root-identity.patch`
(six files) on the retention-lane composed source. Nothing outside the patch
changes Charon.

Typed metadata: `TransformCtx.roots: Vec<ItemId>` in `src/transform/ctx.rs:14`,
the `ItemId`s registered for the items that `--start-from` selected, in
registration order, deduplicated. It lives on `TransformCtx`, which is not
serialized: no field on `TranslatedCrate`, no LLBC/JSON change, no
`charon-ml` regeneration, no Rumoca codec change.

Producer: `translate_crate.rs:1076-1112`. Each of the three `StartFrom` arms
already calls `enqueue_module_item(def_id)`; the item it registers is
recoverable from `id_map: HashMap<TransItemSource, ItemId>`
(`translate_ctx.rs:52`). The enqueue returns (or the caller looks up) that
`ItemId` and pushes it onto the root list. When `enqueue_module_item` redirects
a defaultless trait method to its trait, the trait's id is the root, which is
the item actually translated. Roots are collected for `Pattern`, `Attribute`
and `Pub` alike, so no arm keeps a name-based fallback.

Forced consumers, all compile-enforced by the new field having no default:

1. `translate_crate.rs:1149`: the driver constructor supplies the collected
   roots.
2. `export/multi_target.rs:29`: the merge constructor supplies the union of
   `ItemId`s present in each input crate's `ordered_decls`, mapped through the
   merger's id remap. Each input was already root-filtered by its own
   translation, so this is the identity-carrying seed for that path.
3. `reorder_decls.rs:374-391`: `deps.unprocessed` is seeded from `ctx.roots`
   plus builtins as today. A recorded root that no longer exists raises
   `register_error!` ("selected root {id} was removed by an earlier pass before
   declaration ordering"): counted into `error_count`, `has_errors` on the
   export, nonzero exit under `--error-on-warnings` (charon-driver
   `main.rs:121-122`). Nothing is filtered silently. The `StartFrom::matches`
   call is deleted.
4. `options.rs:629-641`: `StartFrom::matches` is removed. Its sole consumer is
   gone, and removing it makes any consumer this packet missed a compile error
   rather than a silent second spelling.

Not proposed: a wildcard pattern, a Rumoca-side wrapper, an allowance for
roots to vanish, or any change to the `NameMatcher` grammar. The
`name_matcher` TODO at `:270` stays as it is; once ordering carries identities
it is no longer on this path. Aeneas-side `-keep-function` (the campaign patch
`infra/verification/aeneas/keep-function-roots.patch`) still spells names and
is a separate lane.

Expected post-fix measurement on a candidate build: the `method-user-spelling`
LLBC rows become functions `len, refine`, struct `View`; every control row is
unchanged.


## Passes that remove items before ordering

Read, not inferred. `transform/ctx.rs:171,184` and
`normalize/partial_monomorphization.rs:547` remove an item temporarily and
restore the same id. `finish_translation/filter_invisible_trait_impls.rs:21`
removes invisible trait impls permanently. `remove_unused_methods` runs at
translation end, before roots leave the translate context. None of them
redirects an item to a new identity, so there is no redirect table to carry;
if any of them removes a selected root, the refusal above fires and names the
id.

## Merge path accessor

`CrateMerger::add_one` drives each input crate through `RemapIdsVisitor`
(`multi_target.rs:100-147`, slot-count offsets on every declaration id) before
the destructure at `:149-166` that discarded `ordered_decls`. The ids inside
`ordered_decls` are therefore merged-crate ids at that point; the patch
collects every id of every `DeclarationGroup` (`group_item_ids`) into
`CrateMerger.roots`. `ItemDeduplicator::apply_merge_decisions` (`:654-704`)
now returns the non-canonical-to-canonical remap it drives over the crate, and
`dedup` applies the same map to the roots and deduplicates them. This path is exercised by
the merge-path evidence below.

## Candidate measurements

Binary sha256 `9426f9b6` (full hash in `run-evidence.json`), built from the
composed source of charon `10ca62cd` plus the patch, cargo dev profile.

Tiny cells (`logs/candidate/`, receipt rows tagged `candidate`): the
`method-user-spelling` LLBC rows now order `len`, `refine` and `View`; the
impl-spelling rows still exit 2 at the resolver; `free-control` and
`type-only` are unchanged. Both declaration orders identical.

DAE root (the M0 selection), run from `crates/rumoca-ir-dae` with a scratch
`CARGO_TARGET_DIR` and `-- --locked`:

```
charon cargo --preset=aeneas --error-on-warnings \
  --start-from=rumoca_ir_dae::model::view::DaeView::variable_refinement --print-llbc
```

| tool | functions ordered | types | traits | impls | exit |
|---|---:|---:|---:|---:|---:|
| predecessor `10ca62cd` | 0 | 2 | 0 | 0 | 0 |
| candidate | 58 | 145 | 12 | 17 | 0 |

`variable_refinement` is among the candidate's functions; every item the
predecessor ordered is ordered by the candidate. Neither run emitted a Charon
warning.

Not yet exercised: the refusal path (no fixture removes a selected root) and
the merge path.


## Open

- No cell exercises the refusal; constructing one needs a selected root that
  `filter_invisible_trait_impls` removes.
- The predecessor's two ordered DAE types are `str` and one opaque builtin
  (`logs/dae/predecessor.llbc.log`): the `is_builtin()` seeds, not root matches. The candidate's pre-reorder print holds 61
  functions, 175 types, 36 traits and 33 impls against 58, 145, 12 and 17
  ordered: the remainder is translated but unreachable from the root, which
  the walk has always filtered.

## Mutation control

`mutations/ignore-roots.patch` keeps the refusal check but seeds ordering from
builtins only. Built from the candidate source, it orders nothing for every
selection, including `rootid::free`, which the candidate and the predecessor
both order in full (`logs/mutant-ignore-roots/`, receipt rows tagged
`mutant-ignore-roots`). The free-control row alone distinguishes it.

## Suites and rebuilds

`cargo test --test cyclic_retention --test specialized_traits` on the
candidate: 3 and 4 tests passed, exit 0. That invocation rebuilt the bins from
the same source; the cells rerun on the rebuilt binary produced byte-identical
logs and the DAE root rerun gave the same 58/145/12/17, so the receipt names
the preserved (rebuilt) binary and records the first build's hash beside it.
The campaign test `tests/specialized_traits.rs` constructs a `TransformCtx`
directly and is the third forced consumer; it seeds every item as a root
because no ordering runs there.

## Container roots (second candidate build)

Independent review found the omitted case: a selection that resolves to a
module or an inherent impl registers no item (`register_no_enqueue` returns
`None` for `Module | InherentImpl`), and the items such a container registers
while it is translated (`register_module` in `translate_items.rs`) were
ordinary enqueues. Executed on the first candidate build, the default
invocation (no `--start-from`, Charon's own `crate` pattern) and
`--start-from=rootid` ordered nothing. The fix: `enqueue_root` remembers a
selected container in `TranslateCtx.root_containers`, and `register_module`
enqueues a remembered container's items through `enqueue_contained_item`,
which records them as roots and remembers nested containers in turn. New
controls in `logs/candidate2/` and receipt rows tagged `candidate2`: no
selection and module selection now order `free`, `len`, `refine`, `View` in
both cell orders; `{impl rootid::View}` is refused by the resolver on both
tools ("does not support inherent impls"), so that path is covered by the
module row. Both binaries are hashed from here on: the launcher did not change
between builds, the driver did.

## Strict checks and the remaining forced consumers

`cargo clippy --all-targets -- -D warnings` on the candidate source found two
more campaign tests that construct `TransformCtx` directly,
`tests/box_copy.rs` (three sites) and `tests/box_metadata.rs`; they seed every
item as a root, as `specialized_traits.rs` does, and clippy's `question_mark`
rewrite was applied in `enqueue_module_item`. With those in the patch: clippy
exit 0; `cargo test --test cyclic_retention --test specialized_traits
--test box_copy --test box_metadata` 3, 4, 24 and 7 passed, exit 0. The
sixteen cell rows rerun on that build (`candidate3` in the receipt) are
byte-identical to the `candidate2` logs. Two `candidate2` impl-spelling rows
were first measured with the pattern unquoted (a CLI parse error); they were
re-measured quoted and the receipt says so.

## Retained artifact

`build.nix` evaluates `package.nix` (chained on
`associated-demand-retention`) with the pinned Aeneas flake, the same way the
Aeneas candidates are built:

```
nix build --impure --no-link --print-out-paths --cores 4 --max-jobs 1 \
  --file infra/verification/charon/candidates/root-identity/build.nix
```

The store path, once built, is recorded under `retained` in the receipt and
every row is rerun on it; until then all candidate rows are provisional
measurements on preserved cargo builds.

## Merge path: the defect the upstream suite caught

The nix check phase runs Charon's `tests/cargo.rs`; on the candidate it failed
`multi-targets` (the function compiled only for the non-host target was
missing from the merged crate) while the base chain passed that test.
Reproduced by hand on the fixture with two targets: whichever crate was merged
second lost its roots. Cause: `DeclarationGroup` is in the visitor `skip` list
(`src/ast/visitor.rs:60`), so `RemapIdsVisitor` never renumbers the ids inside
`ordered_decls`, and the ids captured there were un-offset for every crate
after the first. Fix: `add_one` applies the same slot-count offsets by hand
before recording. After the fix both functions survive in all three target
orders (`logs/merge/`, receipt `merge_path`), and the cargo suite passes
locally with the stale per-project targets removed.

The same nix check phase fails 8 other tests on the base chain and on the
candidate alike, with a rustc panic in `charon-driver`
(`rustc_hir/src/stable_hash_impls.rs:35`, `OwnerInfo.opt_hash` is `None` for
binaries when no crate hash is needed). That failure is inherited from the
chain and is not addressed here; the receipt records the nix result as it is.

## Snapshot suites bless unless `IN_CI=1`

Charon's `tests/cargo.rs` (and the ui suite) write the actual output over the
expected `.out` file when `IN_CI` is unset (`tests/util/mod.rs:29-43`); only
the nix check phase, which sets it, compares. Every local pass of that suite
before this was discovered was therefore vacuous, and one blessed file leaked
into a patch identity that was discarded. The receipt's
`cargo_suite_in_ci` row is the real local comparison: 11 of 11 with the
candidate, expected files byte-identical to upstream.

## Revision: what a selection denotes, carried by identity (candidate7)

The ui suite in real comparison mode (`IN_CI=1`) exposed that the container
path was not the identity of a path selection: the old `crate` pattern was a
name-prefix match over every local item, closures, their synthesized impls,
promoted constants and definitions nested in bodies included, while the
container path recorded only what modules and inherent impls register. On
`closure-as-fn.rs` under `--preset=tests` the base printed 18 items and the
frozen candidate4 printed 7; on the root-identity tree alone the ui suite
failed the inherited 18 plus 63. Explicit method or function roots (the M0
commands, the cells, the DAE root) were unaffected.

The container and body-owner mechanisms are replaced by one rule at the single
registration point (`register_no_enqueue`):

- A path pattern records the definitions it resolved to
  (`selected_def_ids`); every registered item whose owner chain
  (`hax::DefId::parent`) reaches one of them is a root. That is what a path
  denotes: its contents, transitively, including synthesized items.
- A `Pub` or attribute selection records the definitions it matched
  (`selected_exact_def_ids`); every registered instance of one of them is a
  root (a hax monomorphization instance included), nothing under them.
- `explicit_roots` holds the definitions the selection named; their absence at
  ordering is refused. Derived roots consumed by a pass are simply not
  ordered. Disposition recorded for review: a named global consumed by
  constant inlining is not refused (`issue-1233-foreign-static.rs` names a
  foreign static that upstream's own expectation drops).
- A pattern naming a required trait method enqueues the trait for translation
  but selects nothing, matching upstream's expectation (`filtering/start_from.rs`).
- Partial monomorphization copies inherit root status from their originals
  (`create_pending_instantiation`): a pass-owned redirection carried by
  identity.
- The merge path seeds from each target's ordered local items
  (`item_meta.is_local`, a serialized fact), not from every ordered item, so a
  foreign item an input ordered is ordered again only if still reachable.
- Ordering seeds in item-id order, as the name seed did, so the computed
  order does not depend on registration order.

Measurements for this revision are the rows tagged `candidate7` in the receipt.

### Candidate9 measurements

Frozen copy: `/home/jgoppert/rumoca-checkpoints/2026-09-08-root-identity-candidate9-SeWFxh/` (launcher 1fe5c890, driver fd1277b6, patch 3c5fa90f, DAE
LLBC, M0 composition LLBC and manifest, `sha256sums`).

| control | candidate4 | candidate9 |
|---|---|---|
| `closure-as-fn.rs`, tests preset | 7 items | byte-identical to the base, 18 items |
| ui suite, `IN_CI=1` | 81 failed (63 own) | 18 failed, the inherited set, none new |
| sixteen cells | as recorded | byte-identical logs |
| multi-target fixture, three orders | both functions | both functions |
| `multi-target/sysroot.rs` (foreign explicit selection across four targets) | not measured | passes |
| DAE root | 58 / 145 / 12 / 17 | 58 / 145 / 12 / 18 (plus the closure's `Destruct` impl) |
| three-root M0 composition | 580 groups, 127 functions, 11 opaque | 581 groups, same 127 functions, same 11 opaque |
| in-tree suites, clippy | green | green |
| cargo suite | inherited 8 | inherited 8, `multi-targets` ok |

One expectation update is carried in the patch, `tests/ui/filtering/start_from.out`:
the test selects four items spelled through `std` re-exports, which Charon
names under `core`; the name seed matched the `std` spelling against `core`
names and dropped the selected items, and upstream's expectation encodes that.
Identity keeps them; the added lines are exactly those four items and their
carriers (`logs/candidate9/start_from-expectation.diff`).

## Candidate10: mutually recursive roots through the merge path

Codex's counterexample (`dev/root9-cycle-review-e5d7Uz`, retained here as
`cells/cycle.rs`): two selected functions that call each other keep both
bodies under the ordinary invocation and lose both under
`--targets=x86_64-unknown-linux-gnu`, which takes the merge path, with exit 0
and `has_errors` false on candidate9. `dependency_sources` chose the vertices
with no incoming edge from the ordered set; every vertex of a source cycle has
one from its peer, so the cycle was never seeded. Candidate10 seeds the source
strongly connected components of the graph restricted to the ordered ids
(`petgraph::algo::tarjan_scc`; a component with no incoming edge from another
component is a source), which keeps the previous behaviour on acyclic inputs.
The regression is retained as `tests/merge_roots.rs`, which runs the launcher
both ways on the counterexample and requires the same two functions.

This reconstructs the selection from the dependency structure, as before. The
recorded selection itself does not survive the per-target artifact today; an
explicit root carrier in the artifact would be a public schema change and is
declared, not made, in the channel.

Frozen candidate10 tools: `/home/jgoppert/rumoca-checkpoints/2026-09-08-root-identity-candidate10-JkMEbJ/bin` (hashes in `run-evidence.json`,
`candidate10`). Suites on this tree: rows `candidate10-suites` (six suites green) and
`candidate10-ui` (exactly the inherited 18).

## Candidate11: the selection is carried by identity

Codex's adoption contract: carry the actual selected roots in the per-target
artifact and remap them through merge and deduplication, instead of inferring
the selection from the dependency graph. `TranslatedCrate` now has `roots`
(the selection, its containers' members and the copies partial
monomorphization made) and `explicit_roots` (the exactly named selection),
written from the transform context when the artifact is made. The merge no
longer reconstructs anything: the id remap that visits every reference visits
the carried lists too, the deduplicator remaps both lists, and the merged
artifact carries the result. `dependency_sources` is gone. The three
controls in `cells/merge_roots.rs` (mutually recursive, nonrecursive, explicit
selection) compare emitted functions, carried roots and explicit roots between
the ordinary and the merge invocation.

This is the declared public schema change: two fields on the crate, each an
array of item ids. The charon-ml readers are regenerated, not hand-written, in
the trait-identity tree and travel with the record-chain-consumers candidate.

Frozen candidate11 tools: `/home/jgoppert/rumoca-checkpoints/2026-09-08-root-identity-candidate11-yAGndw/bin` (hashes in `run-evidence.json`,
`candidate11`). Six suites green; ui exactly the inherited 18 (rows
`candidate11-suites`, `candidate11-ui`).

## Candidate12: cleanup seeded from the carried roots; consumed roots accounted for

Codex's candidate11 counterexample (`cells/trait-root.rs`): a selected
default trait method survives the ordinary route and is lost through
`--targets`, with the refusal printed as a warning, exit 0 and
`has_errors` false. Two causes. Post-merge cleanup removed default methods
nothing mentions without seeding reachability from the carried roots; it now
adds every carried function root to the reachability root. And the merge ran
its cleanup in a fresh error context it never joined: it now joins into
`has_errors`, and the launcher fails on a merged crate with errors, as a
single-target translation does. The launcher exit is mechanism-reviewed only:
the direct `merge()` control exercises `has_errors`, and no launcher error
control has run, since after the fix no real input reaches the post-merge
refusal.

Consumed-root disposition, the declared contract: `ConsumedRoot { root,
consumer, pass }` is recorded by the passes that take a root away
(`anon_const_to_call`: a promoted constant into its initializer;
`inline_selected_functions`: the initializer into its callers), carried in
the artifact as `consumed_roots`, remapped through merge and deduplication.
The refusal accepts an absent explicit root only with a recorded disposition;
the global-kind exemption is gone. `cells/promo.rs` shows both dispositions
on both routes. `group_item_ids` is removed. Six controls in
`cells/merge_roots.rs`, including the direct error-state test on `merge()`.

Two more corrections found by the ui suite on the first candidate12 build,
both retained as receipt rows: the cleanup seeds reachability from the exactly
selected roots only, since seeding roots by ancestry kept a default method the
lazy scheme removes; an exact root is the selection's own item kind, so the
vtable made from a selected impl is a root under it, not the selection; and an
explicitly selected item the opacity rules make invisible records the
disposition `opacity: invisible` instead of being refused.

Frozen candidate12 tools: `/home/jgoppert/rumoca-checkpoints/2026-09-08-root-identity-candidate12-ma0gw7/bin` (hashes in `run-evidence.json`,
`candidate12`; two earlier freezes renamed `-superseded`). Six suites green
with the six merge controls; ui exactly the inherited 18 (rows
`candidate12-merge-roots-test`, `candidate12-suites`, `candidate12-ui`).

## Candidate13: closed dispositions issued at the consumption site

Codex's candidate12 review: formatting red on three files, and the
disposition record was an open struct with a free pass label and an optional
consumer, so an artifact could carry a made-up disposition. Candidate13 is
rustfmt clean and makes `ConsumedRoot` a closed enum whose variants name the
identities each transfer involves: `AnonConstToCall { root, initializer }`
(a promoted constant into its initializer), `Inlined { root, into }` (issued
at the inlining site, one record per host function; nothing is recorded at the
pass's construction), and `Invisible { root }` (an exactly selected item the
opacity rules hide). The merge remaps each variant's identities through
deduplication. The disposition control now requires the promoted constant's
record to name its initializer and an inlining record from that initializer
into the selected function, equal on both routes. An artifact consumer can
check that a disposition's identities exist; the transfer itself remains the
producer's claim.

The first candidate13 freeze named the constant variant's field `initializer`,
an OCaml keyword, so the generated charon-ml readers did not parse; the field
is `init_fun` and the freeze was redone (the first is retained as superseded).

Frozen candidate13 tools: `/home/jgoppert/rumoca-checkpoints/2026-09-08-root-identity-candidate13-VpVbYO/bin` (hashes in `run-evidence.json`,
`candidate13`). Six suites green; ui exactly the inherited 18; rustfmt clean
(rows `candidate13-suites`, `candidate13-ui`, `candidate13-fmt`).

Trust limit, stated plainly: disposition metadata is a raw producer claim.
The closed variants fix the representation (the reader rejects an unknown
pass or a missing field), but a consumer of an arbitrary serialized artifact
cannot validate that a recorded identity performed the transfer; an edited
artifact carrying `Inlined { root, into: absent }` passes the raw merge with
`has_errors` false, as Codex's boundary harness shows. What candidate13
claims is producer-recorded accounting of selected roots under the real
translation and merge, not a checked witness and not a missing-root integrity
proof for arbitrary artifacts.

## Candidate14: strict gate

Codex's candidate13 review passed the original controls and found strict
Clippy red on an unused `mut` in the dedup remap closure; removing it exposed
a second lint in the merge test helper (`&PathBuf` where `&Path` will do).
Both fixed, no semantic change; strict Clippy and rustfmt are clean on both
trees. Frozen candidate14 tools: `/home/jgoppert/rumoca-checkpoints/2026-09-08-root-identity-candidate14-vEbJNG/bin` (hashes in `run-evidence.json`,
`candidate14`); every candidate13 row rerun on them, six suites green, ui
exactly the inherited 18.
