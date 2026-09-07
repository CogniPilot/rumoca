# Closure method contract investigation

This is an observational candidate, not an adopted producer fix. The reviewed
capture-arguments candidate remains unchanged. Rumoca brands and all Aeneas
checks remain intact.

The first question is where the relation between a captured lifetime and a
closure's output is lost. `signature-probe.patch` logs the raw Rust signature,
the unclosed signature and captured types, then the post-un-erasure Rust and
Hax signatures. It changes no types, substitutions, predicates or binders.
Tracing is diagnostic evidence, never a production source of lifetime facts.

`probe.nix` consumes the same explicit Aeneas flake/system as the paired
candidate. It returns an **unwrapped** Charon build. Invoke it under the
upstream Charon wrapper's exact pinned Rust toolchain and full-MIR sysroots.
Configured upstream tests and strict Clippy remain enabled.

The existing `../capture-arguments/binder-controls.rs` is the minimal source
reproduction. The actual DAE `DaeView::variables` additionally returns an
invariant, phantom-only `VariableId` and a mixed borrowed/branded
`VariableView`. A repair of covariant references alone is insufficient.

No successful translation, formal proof, or adoption is claimed here.

## Measured distinctions

The probe confirms both unerased and erased forms of the same closure. On
actual dependency-derived `DaeView::variables`, an enclosing identity signature
contains `VariableId<'dae>` and `VariableView<'dae>` beside the same captured
`DaeView<'dae>`. The generated method's declaration instead receives erased
occurrences, which the visitor replaces independently. Hax preserves the
non-erased input when it is supplied. Thus the missing contract is not simply
an absent metadata field or an erasure performed by this visitor alone.

Do not generalize a closure declaration from one returned hidden type:
`declared-vs-required.rs` demonstrates that even an enclosing identity signature
can record both captures at the shorter lifetime although their original
constraint graph keeps them distinct.

`constraint-probe.rs` compares public SCC reachability with solved outlives
values. It uses public borrow-checker consumer APIs and returns the original
checker result, without modifying rustc. The retained
`../closure-regions/src/lib.rs` now uses actual paths, not solved-value inclusion.
Its source-bound discriminator and replacement mutation check both pass.

`invariant-output.rs` supplies the real-borrow-plus-invariant-brand controls.
The companion `invariant-checks.rs`, imported by the common consumer probe,
checks every structural slot pair rather than only the failing output. Native
tests check data/layout; the extracted constraint graph checks variance and
region separation. Both are needed: a successful Aeneas run alone could miss
an unnecessarily restrictive equality.

The upstream context is [Charon issue 1040](https://github.com/AeneasVerif/charon/issues/1040#issuecomment-4682187415):
recovering and transporting closure requirements is a known gap. This work
does not add another single-lifetime substitution rule.
