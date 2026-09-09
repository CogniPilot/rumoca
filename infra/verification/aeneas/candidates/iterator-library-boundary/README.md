# Iterator source and specialized identity

Unadopted candidate; review requested. No production Rumoca change, live pin,
proof allowance, golden admission, or additional UnitDerivative relation.
The accepted count remains 1/18. SPEC_0033 §§2a/4/6 governs this bounded cut;
SPEC_0037 §§2/6 (DRAFT) supplies the experimental proof-boundary vocabulary.

## First divergence

The actual DAE admission root reaches iterator `map`, `sum`, `next` and `len`.
Source inclusion of `Iterator::map` and `Map::new` requires the actual `Map`
type body too. Including that body resolves the initial opaque-field error;
no interpreter change is needed for construction. Including source `next`
then reaches `Option::map` with a mutable callback reference. Charon's
`--monomorphize-mut` distinguishes `FnOnce` from `FnOnce<&mut F>` because the
latter must return the callback's updated state. The predecessor Aeneas
name-only lookup instead identifies both with the generic builtin trait.

`PeInstantiated` already contains the substitution under its remaining
parameter binder. Charon's name matcher, given no call-site arguments, does
not instantiate a nonempty binder. The patch supplies the existing binder's
identity arguments, rather than guessing a signature or changing Rust. The
same identity is retained in three consumers: generated Lean names, builtin
lookup, and emitted Rust model attributes. Non-specialized lookup is unchanged.

The third consumer matters: an intermediate candidate generated a distinct
Lean trait but registered it under generic `FnOnce`. `MetadataCheck.lean`
inspects the actual extraction registry. It accepts the corrected specialized
pattern and rejects that intermediate candidate (4aa7f5/1), even though the
intermediate generated Lean type-checks. This is not a comment-text test.

The patch changes `ExtractName.ml` and `TranslateCore.ml`, not the symbolic
interpreter. `package.nix` composes the existing method-constraints predecessor.
No Rust source renaming, manually edited LLBC, or hand-written replacement of
the iterator implementation is used.

## Source-bound fixture and observations

`charon/fixtures/iterator-specialization.rs` has two deliberately separate
callers. `mapped_pair` owns its callback counter and returns the first two
`(value, counter)` results. `mapped_next` borrows its counter from the enclosing
function; it remains an explicit unsupported translation control, not removed
to make the candidate appear complete. The `CountedItem` alias names the
observed pair and avoids a complex public tuple spelling; it changes no runtime
representation. The standalone Cargo manifest also compiles the pre-existing
DAE library fixture by path, without copying it or adding a compiler crate.

Final focused results and exact commands are in `run-evidence.json`:

| Check | Observation |
|---|---|
| Native fixture tests | 3 existing + 5 new pass; empty, singleton, prefix/state and overflow cases |
| Strict Clippy and formatting | Pass; no lint suppressions |
| Owned caller: Charon, Aeneas, generated Lean | All pass |
| `option_map_state` | Universal relation for actual source-extracted specialized `Option::map` and arbitrary callback state |
| `mapped_pair_contract` | Actual Rust caller equals the authored two-element prefix/count specification for every represented slice and U32 counter, including checked-add failure |
| Registry identity | Specialized mutable `FnOnce` has the specialized pattern; generic identity remains unique |
| Rust callback mutation `+1` to `+2` | Native oracle fails; mutant still translates and Lean-checks; unchanged universal theorem fails |
| Concrete mutant witness | Kernel checks that the mutant returns count 6, not required count 5, for input `[7]` and initial count 4 |
| Borrowed capture | Charon/Aeneas exit 0, generated Lean exits 1 on backward-result/trait field mismatches; still unsupported |

`IteratorLaws.lean` and `MutationWitness.lean` guard exact axiom sets. They use
only `propext`, `Quot.sound`, and (for the slice/caller relations) Lean's
`Classical.choice`, not `sorryAx` or a new semantic axiom. The specification is
authored; the implementation is generated from Rust. The mutant file imports
the same generated module name in a separate directory, so the unchanged law
is checked against the mutated implementation, not a stale positive import.
The retained source mutation is `mutations/callback-count.patch`.

These are functional-translation claims. They trust the pinned Rust/Charon/
Aeneas toolchain and Aeneas's existing Slice iteration, U32 checked arithmetic,
reference abstraction and result-monad definitions. Source-extracting Map and
Option does not prove the underlying slice model, allocation, Rust destructor
effects, or arbitrary borrowed closure captures. No Rust branding is weakened.

## Standard-library fact production and actual DAE

The standalone Cargo manifest also enables the existing collecting driver to
observe standard-library compilations through Cargo `-Zbuild-std`. The pinned
Miri-built sysroot alone does not contain the required collected closure facts.
No new Charon transport code was added by this cut.

Earlier observed diagnostic results, not whole-root admission:

- Source map/new and sum dispatch reduce the actual DAE opaque functions from
  9 to 7. Source `usize::sum` then exposes its real fold dependencies; a larger
  reached inventory is not a regression or permission to omit them.
- Collector-backed core emits 485 closure-fact records. With both newly built
  core sidecars temporarily withheld, the identical extraction refuses with
  `DependencyNotBuiltByCollector` (bb2f10/101). Both are restored and their
  hashes match; restored extraction passes (a21eeb/0). Nothing was deleted.
- Actual DAE with collector-built std passes Charon (2a9a1e/0) and Aeneas
  (e4f0e1/0), including source `usize::sum`. Whole Lean still needs the reached
  Iterator/ExactSizeIterator fold/len interfaces and other declared externals.
- Adding mutable monomorphization and source `next` to that same DAE root
  panics in Charon's explicit-only SelfId substitution (5e01ee/101, backtrace
  577ada/101). The packet retains the exact reproducing command. No partial
  output from that failing extraction is admitted as proof input.

The broader sysroot/MIR, auxiliary-crate and canonical adoption checks in the
method-constraints transport packet remain open. This diagnostic does not
replace them.

## Reproduction and limits

Build the candidate from the repository root:

```console
nix build --impure --no-link --print-out-paths --file infra/verification/aeneas/candidates/iterator-library-boundary/package.nix --max-jobs 1 --cores 4
```

The recorded binary is
`/nix/store/gww53xmql0p910qphay62rvf2vqg0zf3-ocaml5.2.1-aeneas-0.1.0/bin/aeneas`.
The receipt includes the complete fixture extraction, native, Lean and mutation
commands, pinned search paths, observed terminal statuses, and source hashes.
The copied mutation is diagnostic output; its maintained patch and all authored
code live here or in the sibling fixture directory, not only in target or tmp.

Whole upstream regressions, live adoption and full DAE proof admission have not
run successfully. `cargo xtask verify quick/full` and remote CI were not rerun
for this unadopted tools-only cut. Separately, the current pinned production
UnitDerivative tracer passes all five tests (515f57/0), and ParameterDecay's
compile/simulation check passes (948492/0). Those preserve the working spine;
they are not translator-proof or broad-gate credit.
