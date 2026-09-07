# Captured-argument producer candidate

Unadopted implementation, not imported by the live package. The candidate
preserves instantiated `ClosureArgs` through item-reference construction and
solves the declaration's capture-region parameters from the actual capture
types. A separate region binder isolates those parameters from parent generic
arguments; successful matching requires exact equality after substitution.
Unsupported or inconsistent instantiations produce a translation error.

Build with the pinned upstream dependencies and adopted box-borrow patch:

```console
nix build --impure --no-link --print-out-paths --cores 4 --max-jobs 1 --file infra/verification/charon/candidates/capture-arguments/package.nix
```

Use the resulting Charon on the sibling
`../../fixtures/closure-lifetimes/two-region-iterator.rs` controls. This recipe
does not consume scratch sources under `target/`. Generated outputs belong in
a fresh build directory. The recipe consumes `capture-arguments.patch`; the
temporary diagnostic has been removed. Unit tests and actual Rust-to-IR
integration tests are embedded in the patch. The initial implementation
translated all four iterator roots, the whole iterator file and a cross-crate
caller through Aeneas with its signature-guessing pass removed. Those modules
Lean-check, but retain an external `Iterator.map.default` axiom, so this does
not establish their end-to-end semantics or a new Rumoca Lean proof.

Use the companion [Aeneas candidate](../../../aeneas/candidates/capture-arguments/README.md)
on LLBC from this candidate Charon. Its bundled Charon is still the adopted
one, not this producer. Claude independently reproduced the whole-iterator
and higher-ranked fixture results and inspected the fifteen changed upstream
snapshots. The final source, added constructor controls, full test suite and
strict Clippy have passed their candidate gate, and Claude accepted the source
on 2026-09-07. The new actual Rust-to-IR test fails against adopted Charon and
passes against the candidate. Live tool pins have not been changed.

The real `admit_dae_profile` extraction through `rumoca-phase-solve`, with
`rumoca_ir_dae`, `rumoca_ir_solve` and `rumoca_core` included, completes.
With extensive Aeneas checks, its capture-related caller and signature errors
are gone: seven diagnostics at four sites become five at two sites. The
remaining sites are the separate static-reference operation and four
closure-method outlives failures. A matched sequential old-tool run took
90.99 seconds, versus 93.14 seconds for the candidate; these single runs
are diagnostic observations, not a performance benchmark.

The earlier diagnostic passed the configured upstream tests (440 UI cases,
two ignored). Its first/second capture controls translated their known lifetimes
to different Charon region parameters, including when the second is loaded
from a dependency. `external-iterator.rs` exercises that dependency path;
compile its provider with `-Zalways-encode-mir -Zmir-opt-level=0` and the
**same sysroot** as the consuming Charon, then pass the rlib via `--extern`.
Its native test checks first/second capture choice and nonempty/empty slices.
Those observations establish the first-divergence precondition, not the
correctness of this new implementation. Higher-ranked `binder-controls.rs`
also reaches the separate closure-method outlives issue; account for its
signature and method diagnostics independently.
