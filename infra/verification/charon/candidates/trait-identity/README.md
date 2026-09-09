# Trait identity: a lifted trait declaration keeps its declared identity

## Defect

The chain's associated-type normalization (specialized-trait-normalization,
associated-demand-retention) lifts the associated types of traits that
upstream Charon skips as self-referential, `core::ops::try_trait::Try` among
them. The lifted trait gains parameters, every impl of it references it with
the lifted arguments, and Aeneas's builtin pattern for the Std model
`core::result::{core::ops::try_trait::Try<core::result::Result<@T, @E>>}::branch`
(declared arity, Self only) stops matching; the extractor mints a specialized
name and an opaque axiom. Bisected in `dev/branch-binding-notes-SJqbKK/`:
the binding-correct Charon prints `impl Try for Result<T, E>`, the chain
prints `impl Try<T, Result<Infallible, E>, Result<T, E>> for Result<T, E>`,
with or without the lifting option, and every Aeneas binary in the store
behaves the same on each LLBC.

## Change (`trait-identity.patch`, four files)

Both name matchers already translate an instantiated item back to its
original generics before matching: Rust `NameMatcher::matches_with_generics`
(`name_matcher/mod.rs:67-100`) and OCaml `match_name_with_generics`
(`charon-ml/src/NameMatcher.ml:455-465`) strip a trailing
`PathElem::Instantiated(binder)` and apply the binder. The producer never
recorded one for lifting. Now, in `expand_associated_types.rs`, when lifting
appends parameters to a trait declaration, the declaration's name receives
`Instantiated(Binder { params: <new generics>, args: <identity of the original
prefix> })`, the existing element (`ast/meta/names.rs:41`, `Name::instantiate`).
The impl arm of both matchers resolves an impl through its trait's name, so
every impl and method of the lifted trait matches at the declared arity while
the LLBC keeps the complete lifted arguments. No new type, no serialization
change (the element already round-trips through charon-ml and Aeneas, which
strips it for Lean naming), no builtin special case, no matcher relaxation.

Two consequences of the element on an un-copied item, both handled in the
patch: `Name::binder_is_generics_extension` recognizes a binder whose
arguments are the identity of a parameter prefix; the name printer omits such
elements at both join sites (a lifted item is not a copy, so its printed name
is unchanged), and `Name::instantiate` replaces such a record when a genuine
instantiation (partial monomorphization runs after lifting) copies the item,
instead of composing with it, since composing folded copies that differ only
in the lifted parameters onto one Lean name (measured as an Aeneas name clash
on `core.ops.function.FnOnce`). One forced consumer: the campaign test helper
`tests/util/mod.rs::trait_name` read the name's last element; it now reads
through `as_slice_uninstantiated()`.

Scope: trait declarations only. A binder on every lifted item (functions,
types, impls) is retained as the named rejected control
(`logs/rejected-general-binder/`): it binds `branch` on the reproducer but
the composition dies in `Substitute.instantiate_name_generics` and 108 items
carry binders.

## Measurements (`run-evidence.json`)

Reproducer `controls/try_branch.rs` (two functions, `?` on `Result`), the
combined tree (root-identity candidate plus this patch), M0 Aeneas
`j64xdd5b`: `branch` binds to `core.result.Result.Insts.CoreOpsTry.branch`,
zero opaque entries; `Types.lean` and `Funs.lean` typecheck against the pinned
Std with warnings as errors; `logs/lean/Laws.lean` proves an Err-path law
(`twice_err`) and an Ok-path law (`twice_ok`) over the generated `twice`
through the Std `branch` and `from_residual` definitions.

Three-root M0 composition on the same tree: Aeneas exits 0 and `branch` is
bound, but 24 of 140 entries are opaque against candidate4's 11 of 127. The
newly opaque entries are the impl methods of lifted builtin traits (`Deref`,
`SliceIndex`, `Iterator`, `IntoIterator`, `TryFrom`), and Aeneas now warns
that `FromIterator`/`IntoIterator`/`Iterator` form a mutually recursive
extracted group, which candidate4 never emitted because those traits bound to
the Std builtin model. Cause: Aeneas's chain lookup for names ending in
`PeInstantiated` (the specialized-names patch's `find_with_generics_opt`)
compares a bare builtin trait pattern against the binder's arguments and
refuses on count, the same rule Codex measured for `Option`. The counterpart
this lane needs, in that Aeneas lane: a name whose trailing binder is a
generics extension resolves as the original identity for a bare pattern,
exactly as the printer treats it. Not implemented here.

Suites on the combined tree: the four in-tree suites pass (3, 4, 24, 7); the
cargo suite fails the inherited 8; the ui suite fails 81, which is the root
identity tree's own set (inherited 18 plus 63 of root-identity's, under repair
in that packet); this patch adds no ui difference (measured on the composed
tree without root-identity: exactly the inherited 18).

## Retained artifact

`package.nix` chains on `root-identity/package.nix`; `build.nix` is the usual
wrapper. Not built: root-identity itself has no store path yet.

## Rebased on root-identity candidate9

The tree above was the pre-repair root-identity source. Rebased on candidate9
(receipt rows `rebased-*`): reproducer binds the same way; the composition emits
with Aeneas exit 0 (the `FnOnce` name clash of a build that lacked the
instantiation-merge rule is gone), `branch` bound, the same 24 opaque entries
through the Aeneas lookup interaction; in-tree suites green; ui suite exactly
the inherited 18 on top of candidate9, zero new. A stale patch file once
dropped the merge rule during the rebase; the patch here is regenerated from
the final tree.

## Record chain (revision `record-chain`)

The rebased revision replaced a trait's extension record when partial
monomorphization instantiated the lifted trait, which lost the provenance
fact: a copy's name mapped to the lifted arity and never back to the declared
identity. This revision keeps both facts as a chain of records
(`names.rs`: `instantiate` pushes a new record when the last record is an
extension record; `as_slice_uninstantiated` strips every trailing record;
`name_matcher/mod.rs` folds every trailing record last to first). The probe in
`controls/chain_probe.rs` lists every item with records and each record's
shape (receipt row `record-chain-probe`, logs in `logs/record-chain/`).

Law through the actual matcher (rows `record-chain-law-*`): the reproducer
binds the Std `branch` in the polymorphic and `--monomorphize-mut` runs and
mints two distinct copies under `--monomorphize`, the pre-existing boundary.

Counterexample, retained (rows `record-chain-composition-*`): the three-root
composition emits (Charon exit 0) and the M0 Aeneas stops with the assertion
in `Substitute.instantiate_name_generics` while naming the blanket
`impl FnOnce for &mut F`, whose trait is the copied `FnOnce` (TraitDecl 30,
records ext then copy). The cause is in the consumers, each of which folds
one record: the M0 Aeneas impl-naming site
(`specialized-impl-names/complete-trait-arguments.patch`) pops the last record
with the full trait arguments, then strips the self type, and the pattern
conversion in charon-ml `NameMatcher.name_with_generics_to_pattern_aux`
pops the next record against the self-stripped arguments. The replace build
carried an identical copy record for the same impl and named it, so the
single-record fold is what the M0 consumer tolerates. Two textual rewrites of
the composed LLBC isolate this (rows `record-chain-experiment-A/B`): with the
extension record under each copy removed, the same Aeneas emits with exit 0;
with the chain order swapped it fails identically. The Charon side is
consistent: the copy record's parameters equal the copy's generics and its
arguments are at the lifted arity the extension record maps back.

Consumer sites that assume a single record, all outside this packet:
charon-ml `NameMatcher.ml` `match_name_with_generics` (461),
`name_with_generics_to_pattern_aux` (934), the mid-path arm that drops a
record (956), `match_name_with_generics_prefix` (1464),
`find_with_generics_opt` (1483); Aeneas `complete-trait-arguments.patch`
(impl naming) and `iterator-library-boundary/specialized-names.patch`
(`instantiated_name_generics`). `LlbcAstUtils.ml:109` already strips every
record. The contract proposed for them is in the channel post of this
revision; nothing in those consumers is edited here.

Suites: the four in-tree suites pass; the ui suite is exactly the inherited
18, zero new. Frozen tools: `charon` 139ee55f, `charon-driver` a08728bd
(copies taken before the probe run, which relinks `target/debug`).

## Explicit record role (revision `explicit-role`)

Codex rejected the shape classifier: it ignored trait evidence, so a copy that
substitutes only a trait implementation would have passed as an extension.
The role is now carried by the schema. `PathElem::Extended` (last variant, so
the earlier postcard tags are unchanged) is constructed only by the
associated-type lifting through `Name::extend`; `Name::instantiate` always
constructs `Instantiated`, appends after an `Extended` record and composes
into an `Instantiated` one. `as_slice_uninstantiated` strips both; the matcher
folds both last to first; the printer shows nothing for `Extended` and the
arguments of `Instantiated`. charon-ml's generated files gain `PeExtended`
from `generate-asts` (row `explicit-role-generate-asts`).

Laws through the actual matcher, `tests/record_chain.rs` (retained as
`controls/record_chain.rs`): the extension keeps the original identity for a
bare pattern and for the declared arity and prints bare; an identity copy is a
copy (stripped, prints its arguments, matches through the fold); a copy whose
arguments differ from the identity only in trait evidence is a copy; a copy of
a copy of an extension composes into one copy record over the extension and
matches at the declared arity with the composed arguments; and the producer
itself emits the chain on the blanket `impl FnOnce for &mut F` specialized at a
generic closure type under `--monomorphize-mut` (rows `explicit-role-*`).

The consumers (charon-ml, Aeneas) are the separate candidate
`infra/verification/aeneas/candidates/record-chain-consumers/`. This tree also
carries root-identity candidate10 (source-component seeding of merge roots).

The revision is unchanged by root-identity candidates 11 and 12 (the carried
root and disposition schema): the carrier files are identical in both trees.
The binaries measured by the consumer candidate are the explicit-role tree on
candidate12 (`tool.explicit_role_on_candidate12`); suites on that tree are
rows `explicit-role-on-candidate12-*`.
