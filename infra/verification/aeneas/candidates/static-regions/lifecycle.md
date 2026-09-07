# Static-owner lifecycle checkpoint

Reviewed 2026-09-07. **Retained candidate, not adopted.** This extends the
preserved static-region candidate and the paired closure-capture producer. It
does not complete live-content static joins or closure-method constraints.

## Mechanism

`static-lifecycle.patch` changes two upstream interpreter files:

- The existing whole-context shared-borrow liveness pass also collects dead
  loan records from permanent static owners, without ending those owners or
  running a backward continuation. Ordinary frozen input abstractions keep
  their existing treatment: their callers can still own invisible borrows.
- Shared reborrowing substitutes variable regions through the typed region
  set. Static ownership is preserved, not converted to a fresh variable region.
  The consuming `require_variable_ids` operation and its other callers remain.
- The common `destructure_abs` constructor rejects changing permanent ownership
  into an ordinary or endable abstraction. Both branch joins and loop
  preparation reach this constructor. A later join-only check was insufficient
  and has been removed, not retained as a second check.

The first divergence was reproduced with a loop calling a static-array selector,
without carrying an array of references in loop state. The predecessor created
two endable static owners from one frozen owner. The corrected accumulation
path retains its original frozen owner. Returning a live static selection from
the loop still reaches an explicit unsupported-construction error, before any
endable static owner exists. That positive Rust case is unfinished support,
not a passing translation.

## Reconstruction and checks

From the repository root:

```console
nix build --impure --no-link --print-out-paths --cores 4 --max-jobs 1 --file infra/verification/aeneas/candidates/static-regions/lifecycle.nix
```

The recipe has one upstream Aeneas pin and imports its matching Charon candidate.
It applies the preserved static patch, then this lifecycle patch, then root
retention and removal of the capture-signature guessing pass. Identify **both**
executables when recording a run: Aeneas's version identifies its source patches,
not the separately bundled Charon's identity. The reviewed build was
`/nix/store/5hfqp8q0vx2pk77r6f4mm99a615kjvwj-ocaml5.2.1-aeneas-0.1.0/bin`.

Use the pinned Rust toolchain to run both new fixture files with `rustc --test`.
Lint both the library and `--test` configurations; linting only the library
misses the test module. Strict flags were `-Dwarnings -Dclippy::all
-Dclippy::too_many_lines -Dclippy::excessive_nesting`, without suppressions.

Charon source-root runs use `rustc --preset=aeneas --mir=optimized`, explicit
`--start-from` and `--dest-file`. Aeneas runs use `-backend lean -checks
-strict-joins -sequential -no-progress-bar -print-error-emitters`; use
`-log InterpJoin` to inspect the actually reached joins. A tail-return branch
does not exercise continuation joining, and non-strict fallback can hide a
failed join by duplicating the continuation.

For `proofs/static-loop-laws.lean`, extract both
`static_owner_loop::accumulate_static` and
`static_owner_loop::accumulate_ordinary` into `StaticOwnerAccumulate.llbc`.
Translate to `StaticOwnerAccumulate.lean`. Compile that generated module to an
olean with pinned `lake env lean --root=<generated-directory> -j4 -M4096
-DwarningAsError=true -o <generated-directory>/StaticOwnerAccumulate.olean
<generated-directory>/StaticOwnerAccumulate.lean`, then check the authored laws
with that directory on `LEAN_PATH`. The four laws check both selected constants,
one arbitrary accumulation step, and the complete generated static/ordinary
loop-function equivalence for all counts under a successful-selection premise.
Each axiom guard permits exactly `propext`, `Classical.choice`, and `Quot.sound`.

Apply `mutations/static-loop-subtraction.patch` to an output-directory **copy**
of the Rust fixture. Re-extract and compile the mutated generated module, then
check the unchanged laws against it. Compilation must succeed and the laws must
fail. Do not mutate the retained fixture or replace the laws to follow the mutant.

## Recorded evidence

| Check | Result on final candidate |
| --- | --- |
| New native fixtures | 4 loop tests and 6 asymmetric tests pass |
| Formatting and strict Clippy including tests | Pass |
| Loop translator roots | Five translate; `retain_static` refuses at the common constructor |
| Asymmetric translator controls | Two pre-existing nested-borrow refusals, including the ordinary analogue |
| Refused loop join trace | Zero endable static owners; original owner identity retained |
| Existing source-root replay | Charon 50/50; Aeneas 41 positive and 9 explicit refusals |
| Generated Lean from that replay | All 41 positive modules check |
| Full pinned upstream `make -j4 test` | Pass: 151 invocations across 112 source targets, 20 native tests |
| Existing upstream generated Lean/Coq/F* directories | Byte-identical |
| Four generated-code laws and axiom guards | Pass |
| Rust addition-to-subtraction mutation | Extracts and Lean-compiles; unchanged laws fail on arithmetic and axiom guard |
| Fresh actual DAE source replay | Charon succeeds; Aeneas refuses with 4 diagnostics at 1 interpreter site, 92.67 s |

The nine source-root refusals comprise three existing lifecycle nested-borrow
roots, two mutable-reference-copy roots, three unsupported global profiles,
and one opaque-static-mutable control. Opaque positive controls use deliberate
external type axioms and are not Rumoca proof premises. Expected-failure corpus
cases are included in the upstream invocation count, not relabelled as positive.

Fresh DAE root:
`rumoca_phase_solve::scalar_constant_derivative_refinement::admit_dae_profile`,
with `rumoca_ir_dae`, `rumoca_ir_solve`, and `rumoca_core` included. Adopted tools
report 7 diagnostics at 4 sites; capture-only tools report 5 at 2; this combined
candidate reports 4 at 1. Partial Lean output is not successful translation.

Disposable replay evidence was recorded under
`/tmp/rumoca-static-capture-integration-gwafhB`; all authored sources, patches,
recipes and laws are retained here. The upstream corpus used its existing
`make test` harness, not a new project shell script. Repository
`cargo xtask verify quick/full`, live-pin integration, the canonical pilot,
and whole-core translation were not validated by this checkpoint.

Claude accepted retention at 2026-09-07T20:54:42Z, binding patch SHA-256
`f4a7ac4625751e3a36cccc53e6fb6ba2fb63867c3a40495f23a4f6dfffdae00c`
and proof SHA-256
`a5493b3b3128c535ac45c83815a009963a8f921fc9e22a2802364158285c468f`.
The independent review confirmed the common constructor, final refusal trace,
owner identity and static/ordinary generated-loop shape. The aggregate corpus
and replay counts were run by Codex, not independently reproduced by Claude.
No translator-preservation theorem, additional UnitDerivative relation, or
golden admission follows: the campaign remains at **1/18** relations.

## Remaining translator work

Live-content static joins, registration/deduplication and monotonic-growth
invariants, and independent mixed-type static-projector review remain open.
The next DAE fix belongs in Charon: declaration-owned closure constraints,
correct binder/caller substitution, and authenticated cross-crate transport.
Required constraint paths alone are not a complete callable contract. The
remaining Aeneas closure-method lifetime guessing pass must ultimately be
removed, not expanded to repair missing producer information.
