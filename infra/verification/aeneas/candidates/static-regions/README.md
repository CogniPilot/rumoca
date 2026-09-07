# Static-region translator candidate

Review snapshot, **not adopted** by Rumoca's toolchain or proof driver.
Preservation does not accept the translator's semantics.

The subsequent [static-owner lifecycle checkpoint](lifecycle.md) composes the
reviewed capture producer with a two-file interpreter correction. Its separate
`lifecycle.nix` recipe and evidence do not change this snapshot's adoption status.

## Retained inputs

- `static-regions.patch`: the 27-file implementation delta against the
  Aeneas source prepared by `../../package.nix`, before its root-retention
  patch. It adds typed static ownership and the stored-region/kind summary.
- `contextual-keep-function-roots.patch`: the existing root-retention
  behavior with context-anchored insertion hunks.
- `package.nix`: pinned, relative-path reconstruction; no scratch source
  tree is an input. The candidate has a distinct, content-bound tool identity.
- `fixtures/`: original Rust controls, including unsupported positive
  cases. Opaque types are translator controls, not Rumoca proof assumptions.
- `proofs/`: authored laws over **generated** Lean, not a handwritten
  replacement implementation.
- `mutations/closed-constant.patch`: changes the closed-field result to a
  different constant without duplicating the fixture.

Predecessor: Rumoca `91723491379e006776bbaaa7a24465217265d6e1`;
Aeneas `f9a8e338188447c77f31246892cb9a7a742e58ef`.
The adopted package also pins the Charon patches used by these controls.
Rebase explicitly if the predecessor changes; this is not a compatibility path.

## Reconstruction

From the repository root:

```console
nix build --impure --no-link --print-out-paths --cores 4 --max-jobs 1 --file infra/verification/aeneas/candidates/static-regions/package.nix
nix eval --impure --raw --expr '(import ./infra/verification/aeneas/candidates/static-regions/package.nix).src'
```

The second command identifies the reconstructed source before applying the
root-retention patch. Its NAR digest at preservation was
`sha256-UvUVVORjjPrfeCkboWm87AAD9T1sk/ocdrPaRoQHqaA=`, equal to the original
candidate's complete source tree, including symlink identities.

Native controls use the pinned Rust nightly with `rustc --test --edition=2021`.
Charon extraction uses `rustc --preset=aeneas --mir=optimized --start-from`
with the exact function root and explicit `--dest-file`; Aeneas uses
`-backend lean -checks -sequential -no-progress-bar -print-error-emitters`.
Keep generated LLBC, Lean, oleans, binaries and logs in a build-output directory,
never beside these inputs. These isolated upstream controls are not a
replacement for the repository's `cargo xtask verify quick/full` gates.

For the authored laws, extract the whole `static_owner_controls` crate into
`StaticOwnerControls.llbc`, `static_region_generics::closed_static` into
`ClosedStatic.llbc`, and `static_region_brands::mixed_mut_identity` into
`Brand-mixedMutIdentity.llbc`. Check the resulting three modules and then
`static-owner-laws.lean` and `stored-region-laws.lean` with pinned Lean
`-j4 -M4096 -DwarningAsError=true`, using fresh generated-module imports.
The counterexample file is only for the generated **mutated** ClosedStatic
module. Apply the mutation to an output-directory copy, not to this fixture.

## Evidence and remaining work

Before preservation: 35 positive generated modules checked, two additional
positive roots (`mixed_mut`, `mixed_mut_write`) refused mutable-reference
copy interpretation, and two opaque-negative controls refused as expected.
The full upstream translator corpus passed 118 tasks and 26 native tests;
existing Lean/Coq/F* corpus outputs were unchanged. These are finite regression
results, not a translator-preservation theorem.

Actual DAE extensive translation improved from seven diagnostics at four sites
to six at three, but remains rejected. Static lifetime lifecycle cases and
closure fact transport remain open. No additional UnitDerivative relation,
golden admission, full-gate result, or rustc correctness claim follows.

Preservation checks and exact pending adoption boundaries are in
[preservation.md](preservation.md). The candidate is deliberately not imported
by the live package. Adopt only after the separate semantic review and normal
pilot, driver, production-tracer and integration gates.
