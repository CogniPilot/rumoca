# Whole native assignment programs with typed calls

This is a component admission, not a production SLAM migration or an MSL Tier 1
result. The unchanged `RigidPointRegistration` Modelica model retains its full
14,400-pair capacity, all geometric rejection predicates, and its bounded Horn
function owners. The original and historic Rumoca checkouts remain untouched.

## First divergent owner

The original native schedule refusal was in
`refresh/native_assignment/scalar.rs::reads`: its first unsupported operation
was the compact `TensorLoad` of 43,200 P cells preceding the typed `PureCall`.
The canonical model contains twelve programs, each eleven operations, including
three-/nine-cell result ranges. The function body itself already had an admitted
checked typed-call backend. The complete model needed generic assignment proof,
call-table linkage and tuple storage rather than a different Horn implementation.

The native assignment owner now admits bounded primal tensor inputs and checked
primal call sites. Complete input-coordinate dependence participates in range
ordering. Scalar stages additionally refuse a call whose complete input tuple
depends on that stage's unknown target, even when its returned value is constant:
numerical failure can depend on an otherwise unused input. Existing exact
isolators, canonical source-prefix replay, complete output coverage, disjoint
targets and cycle rejection remain mandatory. Directional calls and effectful
operations remain refused.

## Target boundary

`compile_native_assignment_schedule_with_calls_wasm` consumes the issued native
schedule and the matching model-level `SolvePureCallTable`. The shared typed
emitter supplies one helper per reachable owner, preserving its lazy regions,
finite loops, snapshot/storage proof, arithmetic and source-bound fault records.
The new adapter packs whole typed arguments from scalar-register values and
scatters complete returned tuples. It does not lower a function body again.

The v3 ABI is
`eval_assignments(yPtr, pPtr, time, scratchPtr, reservedZero) -> status:i32`.
All Y/P/scratch intervals must be aligned, disjoint and in unshared `env.memory`.
Status 1 rejects invalid intervals; status 2 rejects invalid scalar-to-Integer
input conversion; subsequent statuses retain the exact typed owner's provenance.
Fault source identities serialize as decimal strings, avoiding JavaScript u64
rounding. P is immutable. Stages write a scratch Y image; the complete external
Y tuple is copied only after every stage succeeds. The register arena is a
separate private module memory. Call-free programs retain their original v2 ABI.

One successful complete tuple can be reused across result stages only for an
identical issued site and complete immutable P-span/time/constant input
coordinate. Constants compare their IEEE bits. Y-dependent arguments remain
fresh. Memo flags reset at each invocation; no result survives a dispatch.
Image/range extents become runtime loops, not cloned source operations.

Assertion-predicate output interfaces, directional calls, external tables,
stateful models and unsupported coupled systems remain explicit refusals. This
does not authorize a nonlinear solver, numerical host fallback or fabricated
owner certificate.

## Evidence

The unchanged full source admitted twelve native stages and passed actual Wasmi
execution for 14,400 independently constructed transformed pairs, translation
and RMS below 1e-9 m, domain rejection, recovery, and unchanged P bytes. Focused
regressions compare full-domain complete tuples against canonical typed
evaluation, exercise Integer/Boolean transfer and late checked failure, refuse
alias/alignment/bounds violations, and reject a mismatched call table. A counted
target `pow` import independently observes one execution for three equal P
coordinates and a separate execution for a Y-dependent call. A negative owner
fixture proves that constant results cannot erase input-dependent call failure.

The app owner's actual Chromium consumer also passed nine full-capacity
geometry/mask/domain/NaN/recovery/reset cases, four raw ABI fault cases preserving
all Y/P bytes, six corrupt metadata refusals, and source/artifact reload. Twenty
warmed complete-model calls averaged 8.995 ms, excluding sensors, input copies,
matching, rendering and the SLAM pipeline. This is target observation rather
than a general throughput claim.

The tested whole-model module SHA256 is
`70b84c03bd56a2d5682b5c0459465323be9fb323e1f59d07502b58bf2ee3edd6`;
the Modelica source SHA256 is
`3f6e29a83d5fffab183bb5a3179eb972da96aef97578a7528d5106fba106fd02`.
The production app pin is unchanged. The MSL canary timed out during its harness
build, before any model phases; that supplies no model-conformance result.

The final combined native regression command passed 539 unit/integration tests
and nine doctests across `rumoca-bind-wasm`, `rumoca-exec-wasm` and
`rumoca-ir-solve`, with `rumoca-bind-wasm/native-assignments` enabled. Strict
all-target/all-feature Clippy passed for the same three packages. The final
command required an additional dependency feature-union rebuild: compilation
finished in 8m59s and the whole bounded process ended at 563.83 seconds on two
pinned CPUs. An attached monitor observed 2,050,028 KiB peak aggregate RSS only
from its attachment onward and 48,816,772 KiB minimum host available memory.
The shared target occupied approximately 13 GiB including earlier artifacts;
this does not satisfy or claim the separate earlier 8 GiB cache budget. No
shared cache artifacts were deleted.
