# Native typed pure-call backend (review draft)

This work is isolated on `slam-native-functions`, based on
`03e6b59a47f1adc48488b89e4c462c5fa50f3aaf`. It adds a bytes-only compiler
consumer of a checked `SolvePureCallTable` and its matching
`SolvePureCallSite`. A narrow DAE compaction repair also retains source-ordered
Real and Integer accumulator loops. Solve wire construction, native assignment v1/v2 and
the application compiler pin remain unchanged.

## Current execution contract

`compile_pure_call_wasm(table, site)` retains the selected owner ID, its issued
source identity, ordered typed input/output interface, and failure spans.
Compilation validates the interface with the existing table owner. It does not
identify calls by source name, matching function text, or observed values.

The module imports unshared wasm32 `env.memory`, plus the existing `env.pow`
math intrinsic when an admitted reachable owner uses source Power, and exports
`eval_typed_call(inputPtr, outputPtr, scratchPtr) -> i32`. Each tuple is packed
in declared value order, row-major, using eight-byte cells: Binary64 bits,
signed i64 Integer, or canonical 0/1 Boolean. The compiled layout declares
every buffer's byte length. The caller allocates scratch once and may reuse it
between invocations; this helper has no result cache or ambient state.

Buffer alignment, memory bounds, and pairwise nonoverlap are checked before any
write. Integer/Boolean inputs are checked before body execution. Every output
slot is private scratch storage. Only a complete successful body publishes the
whole output tuple to the caller. A positive return code selects an immutable
fault record with owner, enclosing region path, region-local operation ordinal
(when applicable), operation class,
failure kind, and original source span. A failed invocation can dirty private
scratch but leaves input/output buffers unchanged. A subsequent call executes
all initialization and operations again.

Assertion-predicate output interfaces are refused before frame planning,
including reachable nested callees. A Boolean payload does not establish
source assertion severity, termination, or action semantics. Ordinary Boolean
result outputs remain supported.

## Admitted straight-line operations

- Typed constant, input/local/output load/store; complete aggregate copies.
- Elementwise f64 add/subtract/multiply/divide and ordered comparisons.
- f64 negate/abs/sign/sqrt/floor/ceiling/truncate.
- Checked Integer negate/abs/sign/add/subtract/multiply/min/max/comparisons,
  preserving both i64 overflow failure and the declared Integer domain.
- Boolean not/and/or/equality and complete typed selection.
- Integer-to-Real and checked Real-to-Integer truncation/floor conversion.
- Fill, explicit aggregate construction, static projection, dynamic one-based
  row-major projection/update with per-axis bounds failure.

Register-flow queries use the canonical IR visitors. Input loads alias only
read-only caller inputs. Mutable-slot loads copy a snapshot. A functional
element update can forward only a consumed private SSA aggregate; a live older
snapshot, method-local slot, output slot, or input buffer is never consumed as
that mutable register tape. Consequently a straight-line consumed update chain
requires one private aggregate allocation and no aggregate copy per write.
This does not yet solve aggregate carried-loop forwarding.

## Structured control profile

The bytes-only consumer additionally admits checked lazy `Conditional` regions
and finite `Fold` regions with the checked `StructuredIndexDomain` binders and a
scalar or tensor carried tuple. Nested admitted regions keep distinct private frames and
runtime counters. The selected branch alone executes; both branches must still
compile within the declared operation profile. The source-issued Fold domain
supplies the exact finite iteration count and every axis lower bound, signed
step and attained final tuple. Runtime traversal preserves canonical last-axis
fastest order without a backend rank cap. The
emitter never invents a bound, enumerates tuples, or expands statement copies
with domain size. A zero domain returns the initial tuple without executing its
transition. The binder is never incremented after the last iteration, including
at signed i64 endpoints.

Each transition receives a complete carried snapshot and immutable capture
snapshot in private input slots. Only its completed output tuple replaces the
carried values. Private slot loads retain their issuance-time SSA values, and
input-slot loads cannot be consumed into mutable update storage merely because
the region input frame is private. All region outputs remain private until the
top-level success publication. Runtime failures preserve the exact source span,
top-level issued owner, and `(enclosing operation ordinal, branch ordinal)` path;
the Fold transition has branch ordinal zero. This path disambiguates repeated
region-local operation ordinals.

The original one-binder admission boundary is historical; finite multibinder
Fold is now admitted through the same checked domain. Captures can
be tensors and are copied once before each Fold starts, outside its runtime
loop. A 14,400-cell immutable raster and three-cell carried accumulator match
both the canonical evaluator and an independent ordered recurrence, including
cancellation and signed zero. This module is 1058 bytes with 115400 scratch
bytes. Full-raster carried tuples are also covered by alias, empty-domain and
late bounds-failure/atomic recovery controls. Nested tensor Fold/Conditional
controls preserve outer snapshots and skip a faulting unselected branch.
No source-language
loop-bound, dynamic-range, While, break or alias construction rules are changed.

The first aggregate profile uses sound whole-tuple copying. Each iteration
copies the old carried tuple into private transition inputs and the completed
output tuple back, with any body copies required by SSA. Scratch is allocated
once per checked region, not per iteration. Fold invariant captures do not incur per-iteration copying at the Fold entry.
Nested Conditional captures still copy on each branch activation in this
baseline; this can copy full rasters inside a Fold. Carrying an N-cell raster for N iterations still
costs O(N²) copied bytes; this is not a fast raster-update profile. Only the
existing consumed-private-SSA UpdateElement witness permits storage reuse.
No carried-frame forwarding or new alias/liveness witness is inferred.

Unsupported operations fail compilation explicitly. Map, linear solve,
unimplemented tensor operations, other transcendental functions, Integer
Scale/Power and Binary32 remain outside this profile. Source-issued Real
Scale, Identity, canonical Real Min/Max, reachable nested Call and Real Power
through the existing target intrinsic are now admitted.
There is no backend-invented iteration bound, Modelica scalarization, ODE
wrapper, application WAT, or special case for a vision algorithm.

Assertion-predicate output interfaces also fail compilation before frame
planning. The error retains the exact owner, output ordinal, output kind and
original call/interface span. A raw Boolean predicate can be false while the
canonical payload evaluator returns successfully; that is not source-assertion
execution. This table interface does not carry assertion severity, original
assertion message/span or a checked continuation contract. The backend does
not infer them from store instructions or source names. Both true and false
predicate interfaces are refused until that contract exists. Ordinary Boolean
result outputs, including false, still execute normally. Negative admission
and ordinary-result Wasmi controls passed in the 44-test boundary gate
(28 typed-call tests). Fatal/warning source assertions remain unadmitted.

## Historical baseline evidence

The preceding straight-line baseline passed: `cargo test -p rumoca-exec-wasm
--all-features` runs 25 tests, including all nine typed-call fixtures below.
`cargo clippy -p rumoca-exec-wasm --all-targets --all-features -- -D warnings`
and the workspace formatter check pass. Initial builds exposed private-method
visibility and excessive-nesting lint errors; visibility and helper structure
were corrected before the final executable gates. The numerical contract was
retained. Final checks used two native build cores, two Cargo jobs and a private
cache with an 8 GiB sampled owned-RSS limit. The complete scoped test command
finished in 19.72 seconds at 552,352 KiB peak owned RSS; final strict Clippy
finished in 0.489 seconds at 340,348 KiB. Logs and source hashes are recorded in
`slam_web/dev/rumoca-native-typed-call-verification.json`.

These baseline Wasmi/reference comparisons remain regression gates:

- Actual Wasmi execution versus the canonical typed evaluator for a complete
  14,400-cell aggregate plus Real/Integer/Boolean outputs, changing inputs and
  deliberately poisoned reused scratch.
- Late conversion failure after private output stores, dynamic index failures,
  exact fault provenance, unchanged public outputs, and recovery.
- A 100-update 14,400-cell chain with a bounded scratch allocation; a live old
  snapshot remains distinct. Mutable method-local load snapshots are separate.
- Integer overflow/domain controls, canonical Boolean input refusal, trap-free
  conversion boundary checks, signed zero and NaN sign, and comparison oracles.
- Alignment/overlap/out-of-memory-span guards, unsupported math, foreign
  interface rejection, and retained owner/interface metadata.

Manual typed-IR fixtures and helper presence do not establish full source
admission. The historical control stage below did not admit unchanged
FeatureSelection or Horn; the later Horn component stage at the end of this
record establishes its source-issued owner separately.
The first structured-control extension passes all eleven new Wasmi/reference
tests, including finite/descending/empty domains, exact full tensor-capture
order, carried snapshots, lazy faults, nested region paths, i64 binder endpoints,
nested-counter isolation, empty faulting transitions and typed wider-profile
refusals. The complete scoped crate suite passes 36 tests; scoped all-targets,
all-features strict Clippy passes. The bounded all-crate command finished in
24.96 seconds at 596,784 KiB peak owned RSS; Clippy finished in 16.84 seconds at
723,604 KiB. The final result is recorded separately in
`slam_web/dev/rumoca-native-typed-control-verification.json`.

These initial control fixtures are manually constructed, constructor-checked
typed IR. The source-sized gate below now establishes one additional finite
Modelica profile. The worktree-wide architecture gate and fixed MSL
canary/cohort verification are not established by this isolated executor suite.

## Full source-sized ordered accumulator proof

The unchanged `tests/suite_typed_calls/OrderedFrameFold.mo` fixture declares
14,400 Real inputs plus an initial accumulator and final conversion input.
The reviewed943 producer rewrites the source seed/loop to Map plus Reduce.
For 14,400 entries repeating `[1e16,1,-1e16,3]`, initial `7.25`, the literal
recurrence produces `14407.0` while that table produces `14406.25`.
An overflow control also produces source `+Infinity` versus table `MAX`.
Those exact historical issued tables and their explicit native Map refusals
remain fixtures. Signed-zero controls are separate from the demonstrated
rounding/overflow discrepancy.

The DAE compactor now identifies resolved Real accumulator types, including
aliases and arrays, and retains their original ordered loops recursively.
No reassociation certificate is available. All 315 DAE library tests pass.
A minimal external Rust producer compiles the exact Modelica source on this
isolated branch and obtains its constructor-checked PureCall table through
`lower_solve_package`. The canonical typed table contains one Fold, no Map or
Reduce. The executor then consumes the deserialized exact issued table and
matching site; production compilation does not inspect source names or text.

Actual Wasmi execution agrees bit-for-bit with the canonical evaluator and
an independent literal recurrence for all 14,400 values, baseline/parameter
gain 2/subtraction source variants, cancellation and negative-zero controls.
The fixed native overflow control agrees with source `+Infinity`. A late
invalid Integer conversion retains the exact source span, unchanged public
outputs and immutable inputs; reused poisoned scratch recovers successfully.
The modules are 1004/1005/1004 bytes, with 115216 input bytes, 16 output bytes and
115320 scratch bytes. They export the existing checked typed-call ABI.
All 41 executor tests pass, including 25 typed-call tests; combined executor/DAE
all-targets/all-features strict Clippy passes. The added compiled-layout export
helper is separately rechecked. Hashes, provenance and bounded logs live in
`slam_web/dev/rumoca-native-source-order-verification.json`.

Integer seed reassociation was independently characterized before its separate
repair: source seed `-MAX` and terms `[MAX,1,0,…]` produce 1, while the reviewed
typed Reduce faults at its exact range span. Resolved Integer accumulators now
retain the source order too; aliases/arrays and nested/standalone loops are
covered. Ordered first-match scans remain a separate profile. The unchanged
14,400-input Integer source now issues one Fold and executes in Wasmi: result 1,
exact checked overflow on a changed seed, unchanged public output, original
nested fault provenance and successful recovery. Its module is 904 bytes,
with 115208 input bytes and 115272 scratch bytes. The combined scoped gate
passes 42 executor tests and 317 DAE tests. One former structural regression
required zero folds and a tensor reduction; it now checks the exact original
domain, update and loop spans plus the source-ordered carried result. The
independent complete arithmetic oracle remains mandatory.
Source-issued standalone owner execution still does not admit the complete
model schedule, FeatureSelection, Horn registration, full SLAM or a 10× target.

The later aggregate gate uses the unchanged 14,400-input Modelica
`OrderedTensorFrameFold.mo`, including its tensor-carried previous-total field.
Both the original source and edited protected gain2 compile into an issued
one-binder tensor Fold, then execute bit-identically against an independent
literal recurrence and the canonical checked evaluator. Dyadic/cancellation
and signed-zero controls, exact late conversion fault source span, atomic
32-byte publication, immutable inputs and recovery pass. Source-issued
modules are 1640/1641 bytes with 115216 input, 32 output and 115512 scratch
bytes. Their verified late fault status is11 at source bytes506..525; safe
manifests retain that IntegerConversion kind and opaque source identity.
All49 executor tests pass (33 typed calls); the34 projection regressions and
strict all-target/all-feature executor/eval-dae/phase-dae Clippy pass.

A bounded1353-sample native profile (zero lost) located98.15% exclusive CPU
in two ordered dependency duplicate scans. One shared ordered inventory now
retains first-occurrence Vec order and full Scalar/RecordField keys with
membership-only HashSet lookup. No graph closure, source ownership or fault
rule changes. The unchanged tensor source previously timed out in Solve
lowering after60 seconds; after this fix its complete owner production is
21.309 seconds. This is a component preparation result on a shared machine,
not a whole-pipeline throughput claim. FullFeatureSelection still has its
actual ToDae dependent-domain refusal; unchanged Horn had a phase-identified
Solve-lowering timeout; after the capture fix it still did not finish within60
seconds (compile4.490s, peak owned RSS1.18GiB), so no Horn owner artifact is
claimed. Raw profiles, source/producer/module digests, historical
failed-fixture evidence and limits are recorded separately in
`slam_web/dev/rumoca-native-aggregate-fold-verification.json`.

The frozen tensor modules also pass actual Chromium execution against the
independent complete source recurrence, including IEEE overflow, exact source
fault status11/span506..525, input/guard immutability, atomic publication and
recovery. One thousand calls including input copy, native execution and owned
output copy average0.3332/0.3232ms for original/gain2. This standalone component
benchmark is outside the production pin and does not measure SLAM. Browser
evidence is in `slam_web/dev/rumoca-source-typed-call-browser-verification.json`.

At that historical aggregate stage, the next owner/backend work was nested
Call and witnessed aggregate-carried
storage forwarding, with exact activation/source identity. Complete source
admission remains separate from these constructed-table storage controls.
Dependent/While execution needs its constructor-issued DAE/Solve proof. A
separate compiler-owned model schedule must eventually issue one typed call and
publish its complete result/assertion tuple; evaluating residuals as assignments
is not a substitute. Full source and complete numerical acceptance remain
pending.

The subsequent preparation stage completed actual source-to-Solve
owner production. Its Modelica SHA256 is
`3f6e29a83d5fffab183bb5a3179eb972da96aef97578a7528d5106fba106fd02`;
capacity remains 14,400. The traced producer completed in 54.148s with
1,121,640KiB peak owned RSS and issued three checked owners. Preparation is
separate from native registration execution: the actual issued table is refused
at finite two-binder Fold operations (source bytes 606..609) and scalar Real Max
(bytes 5176..5195). These are the first native capability boundaries, not an
exhaustive admission claim. Real Min/Max must preserve the canonical evaluator's
NaN and signed-zero behavior; multibinder transitions must preserve canonical
point order, simultaneous carried versions and source fault order.

The preparation changes preserve original graph nodes and ordered edges.
Successful parameter-only fragments retain exact expression/field/scalar and
lexical-context identities; their completed ordered inventories share storage.
A narrow owned-binder literal-update profile checks and shares the complete
parameter-guard sweep while retaining every scalar node and its exact self edge.
Unproved shapes retain the full walker. Construction-local strongly connected
components evaluate exact closures successor-first and intern only completed
sets. They introduce no IR owner and drop no dependency. The traced full graph
retains 144,050 nodes and 1,296,194 edges; 14,520,816,200 logical closure occurrences
share 57,618 completed sets with 820,805 retained parameter IDs.

The independent original dense fixed point covers all 512 three-node graphs,
cycles, repeated/self edges, disconnected captures and complete key identity.
Differential controls compare public callback order, function summaries,
per-node closures, ordered edges and exact failed-address diagnostics. A deep
14,400-node storage control retains one completed shared capture rather than
historical union sets. All 72 evaluator library tests and strict all-target,
all-feature evaluator Clippy pass. Temporary tracing probes are removed before
the final source freeze. The final producer completes the unchanged source in
53.266s with 1,126,104KiB peak owned RSS. Its 622,622-byte raw issued table is
byte-identical to the traced result, and its exact native refusal gate passes
again. Earlier timeouts and the 8GiB RSS-limit stop remain
recorded in `slam_web/dev/rumoca-native-registration-projection-verification.json`.
This preparation proof does not establish full registration numerics,
FeatureSelection, the production model schedule, full SLAM or the 10× target.

## Full Horn source-issued owner: numerical admission, copying baseline

The unchanged 14,400-point source now issues three owners and its complete
standalone FitRigidPointPairs owner executes in Wasmi and a Chromium dedicated
worker. The exact source SHA above is unchanged. Generic finite-rank Fold,
Real Min/Max, Scale/Identity and nested Call preserve original domain order,
carried tuple snapshots, activation and source fault provenance. Reachable
assertion-predicate interfaces remain refused.

A discovered aggregate emitter defect copied only one cell from an
aggregate-valued row. Exact complete row-byte copies now preserve nested
Horn and rotation matrices. The failing full proper-transform fixture and
binary digest are retained; an independent nested matrix bit-copy regression
covers negative zero, NaN payloads and immutable inputs.

Source Power retains the compiler target intrinsic env.pow. Replacing literal
squares by multiplication failed a one-ULP canonical counterexample and was
removed. Wasmi uses canonical same-host Rust powf. Chromium Math.pow is
qualified by independent finite geometry; portable host bit parity is not
claimed. There are six Power intrinsic calls per full registration invocation.

The complete native typed-call suite passes46 tests, including complete
208-byte canonical comparisons and independent full-domain transforms,
rank/centroids/cost, planar versus collinear/coincident geometry, proper
reflection handling, invalid-pair/count gates and recovery. All224
eval-Solve library tests pass after immutable TypedValue payloads began sharing
Arc storage; functional updates still create distinct complete payloads.
Scoped all-target/all-feature strict Clippy and formatter checks pass.

Actual Chromium worker execution passes21 full-capacity geometry/refusal
cases, immutable806432-byte inputs, guards, complete208-byte result and
atomic ABI failure/recovery. The frozen module is72877 bytes, with16037760
scratch bytes; its SHA256 is
74b09a10216e3876e51aed093c18000588c6eb4ba47d8a46583759100613e901.
Ten complete input-copy/execution/owned-output calls average7430.100ms:
0.100ms input,7429.990ms execution and0.010ms owned output. This is far from
90Hz or10x and is not a SLAM throughput result. The first100-call benchmark
timed out; reducing benchmark repetition to10 retained every full-size
numerical case and the unchanged Modelica source.

The current sound-copying baseline still copies complete Conditional captures
on every activation and whole carried masks on each iteration. Even the two
mask boundary copies alone require3317760000 bytes for14400 iterations of a
14400-cell Boolean tuple. Source-level geometry, correspondence production,
model schedule integration, source edit admission, FeatureSelection and full
SLAM remain separate gates. Frozen manifests, exact source/producer/table/
module/binary hashes, failed history, numerical results and resource limits
are recorded in slam_web/dev/rumoca-native-horn-backend-verification.json.
The preparation report remains historical with its original refusals intact.

## Checked immutable region captures

The next target-local storage change borrows exact immutable parent capture
ranges for both Fold and Conditional inputs. A checked child input is read-only
and has the exact capture byte width. Borrowed scratch ends before all child
private allocations; the existing consumed-update rule cannot mutate it. The
parent is suspended through the synchronous child. Carried/binder inputs retain
independent private snapshots. No activation, ownership or source bound changes.

Five focused controls and the complete47-test typed suite pass, including
complete canonical equality for every full14400 registration case. Strict
all-target/all-feature executor Clippy, formatter and browser TypeScript pass.
The full-source gate now requires native admission of every issued owner;
capability refusals cannot pass that numerical gate.

The new frozen module is64868 bytes, SHA256
974b7996dd6f485399fbfc5c2ac958ec98dded5d11e5f50b67185f6d70b81287,
with1285200 scratch bytes and the same806432/208 input/output ABI. Actual
Chromium worker passes all21 full-size geometry/refusal cases and atomic
failure/recovery. Ten complete calls average305.890ms:0.040ms input,
305.840ms execution and0.010ms owned output. The earlier7429.990ms execution
baseline stays frozen. These shared-machine component observations do not
establish whole-pipeline speed,90Hz or10x.

A finalized9991-sample dedicated worker profile places91.50% exclusive time
in the main registration WASM function, with0.08% combined in its Eigen
helpers. It cannot attribute individual WASM instructions to copying. The
two carried-mask snapshots still require at least3317760000 bytes/call;
any further reuse needs a separate exact old-SSA/slot-read/alias lifetime
proof. Source/table/module/binary hashes, history, guards and timings are in
slam_web/dev/rumoca-native-horn-capture-borrows-verification.json.

## Exact old-value lifetimes and private carried inputs

A subsequent storage profile borrows each Fold's fresh private carried target
for its transition input. Invariant captures remain immutable across every
iteration. A functional update consumes that private allocation only after
all exact old Input-slot loads, duplicate captures and old SSA-register reads
finish. Conditional capture permission uses the same complete old-value
lifetime; public inputs never receive permission. Region outputs still retain
private snapshots and successful complete-tuple commit in this stage.

Five focused lifetime controls and all 49 typed-call tests pass, including
full 14,400 old-output snapshots, duplicate/later slot and SSA reads, nested
invariant lifetime, and complete canonical registration tuples. Scoped strict
all-target/all-feature Clippy and formatter pass. The final frozen module is
63,243 bytes, SHA256
d23be1a3ec4624951ffe165230446a9387ed86575ae4c614e087b8be924d1f6a,
with 1,051,952 scratch bytes. The unchanged 806,432/208-byte ABI remains atomic.

Actual Chromium worker passes all 21 full-capacity cases. Ten complete calls
average 236.370 ms (236.340 ms execution), versus 263.970 ms at the intervening
private-carried-input stage and 305.890 ms with immutable captures alone.
Those separate frozen component observations are not full-SLAM throughput.
A 7,733-sample worker profile places 89.24% exclusive time in the main WASM
function without identifying individual copy instructions. The carried-mask
backedge alone still moves at least 1,658,880,000 bytes/call. Any output alias
requires a separate completed-return and simultaneous-tuple proof. Records,
source/module/binary hashes and historical failures are in
slam_web/dev/rumoca-native-horn-carried-lifetimes-verification.json.

## Completed region returns and same-ordinal backedges

The completed-return profile retains a separate private root output tuple.
A region output can share its single Store source only when that output has
no loads and all remaining operations are output stores. Every shared store
is then an exact same-range no-op, while retained outputs use disjoint private
snapshots. No later computation, nested region or fault can mutate a shared
returned source. Conditional results share storage only when both arms return
the identical checked allocation; mutation permission requires both arms'
permission. Fold output sharing is restricted to its same-ordinal carried
allocation. Cross-ordinal tuple swaps retain complete snapshots.

Frame planning proceeds in source operation order, so later SSA plans use the
completed conditional result allocation. Existing old-SSA, duplicate capture,
late slot-read and invariant lifetime checks still govern functional updates.
Caller inputs and invariant captures never become mutable. Public output
publication remains atomic after complete success, including fault recovery.
No IR, owner, source dimension, activation or numerical expression changes.

Three additional focused full 14,400 controls and all 52 typed-call tests pass.
The controls cover an entire 14,400-iteration mask update, simultaneous full
array swaps and an old output followed by a nested private write. Their
expected bytes are independently constructed and checked against canonical
evaluation. Existing full source cases retain complete 208-byte canonical
comparison. Scoped all-target/all-feature strict Clippy and formatter pass.
The actual unchanged Horn module emits no 115,200-byte MemoryCopy instruction
inside any loop, removing the remaining minimum 1,658,880,000-byte mask backedge
traffic. Smaller tensor copies and initial/final full-array copies remain.

The frozen module is 60,579 bytes, SHA256
ddbc07532694de1e20da33c945304597a3744f8226e23f1f6739178a18e90782.
Input/output/scratch remain 806,432/208/1,051,952 bytes; unused conservative
allocations have not been compacted. Actual Chromium worker passes all 21
full-size cases, immutable inputs/guards and atomic ABI failure/recovery.
Ten warmed complete input/execution/owned-output calls average 8.690 ms:
0.110 ms input, 8.560 ms execution and 0.020 ms owned output. The first proper
transform call took 56 ms including cold/JIT effects. Six env.pow calls per
invocation preserve source Power; browser Math.pow carries the prior qualification.

The 8.69 ms measurement is a standalone registration component, with matched
points as inputs. It does not establish visual odometry, shared model schedule,
production pin integration, full SLAM, 90 Hz whole-pipeline or 10× performance.
Sequential shared-machine observations and a finalized 1,022-sample worker
profile are preserved in
slam_web/dev/rumoca-native-horn-returned-storage-verification.json.
All earlier module directories and failed/source-preparation records remain
historical evidence. Native source-edit admission remains a separate gate;
runtime parameter variation is covered.
