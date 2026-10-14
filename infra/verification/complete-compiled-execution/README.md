# Required compiled execution: focused evidence

Code revision: `0c049bcbc6020738f1391fe18bd67e43939e093d`, based on
`c15dc75182dd029177e4cabc4a3953ed6df563ce`. CPU scalar arithmetic remains
isolated in PR #435 and is not included in this branch.

Requested compiled runtimes now prepare mandatory source-bound value, selected
row, JVP, lifecycle, delay, target-query and event kernels before construction
succeeds. Optional fast specializations may decline only when the prepared
compiled base owns the operation. Explicit interpreter execution remains a
reference product. Reverse sensitivity has no compiled producer here: its
reference artifact can only be prepared from a reference runtime, and a
compiled request fails before evaluation or output writes.

The acceptance boundary follows SPEC_0007/0040's separate canonical and derived
products, SPEC_0008's required `Result` propagation, and SPEC_0033's unchanged
checkers. Unsupported backend capability is a preparation failure, not evidence
that every legal Modelica model can currently be compiled for every target.

## Runtime regressions

Release library suites passed after upstream integration: 728 solver, 233 sim
and 12 opt tests (973 total). Ten solver doctests passed, including eight
compile-fail cases. Three-package library/test clippy with warnings denied,
format and whitespace checks passed. The canary artifact build then exposed
two exhaustive-match omissions for `RequiredExecutionBackendMissing` in the
worker. Both typed classifiers were updated, with regression assertions;
all 32 worker library tests passed. This compiler failure occurred before
any canary model attempt.

Focused controls cover supported compiled execution and explicit reference
execution alongside cold lowering refusal, selected-row isolation and output
atomicity, initialization, delays, target queries, prepared torn composites,
foreign call-table rejection, immutable runtime metadata and reverse-product
preparation rejection. Package suites are not workspace or cohort evidence.

## Frozen physical drone

`physics.json` retains the source digest, full trace digest, timings and backend
receipts from the native-host harness. It loaded the captured 855-source
workspace, compiled `LabQuadrotor`, and compared explicit Interpreter with
NativeRequired over 24 advances of 0.01 seconds. Forward/yaw inputs changed at
ticks 9 and 17. All 371 named values per tick were compared by binary64 bits:
8,904 values matched, with no declined Cranelift kernels.

This is one native-host run. It does not establish browser lowering, whole-app
real-time performance, or a statistically measured speedup. The raw full trace
and source capture remain in the task's scratch profiling directory.

## Current SLAM native constructor

`slam-admission.json` records the public `prepare_native_program` constructor
used by slam_web's build worker. The source was captured from all 59 files in
the app's `d435-native` manifest order: 762,329 bytes, SHA-256
`f49ee1da9d4697f62f0168c0cc5dc05a81de7d62f117c2302384bbe58200a23e`.
This digest matches the app's published source pin; its working checkout was
dirty, so its Git HEAD alone does not identify this source.

Each of the four entrypoints received one attempt under a 60-second parent
watchdog. All refused before producing a runnable artifact. Reset refused
native schedule admission; Initialize, Step and Intervals exceeded the default
64 MiB whole-program scratch capacity. Their exact requested byte counts and
elapsed times are retained. No published module was substituted and no guest
execution was claimed.

A same-source comparison through raw native lowering and retained FMI
construction identified Reset's exact action: the authored assertion in
`RGBDGraphProcessing.Empty` validating a fresh localization state, assembled
source bytes `671655..671661`, line 12913. Both paths have one Assert action,
owner 30/predicate 160, with a matching call site and checked assertion flow,
but no assigned-read predicates. This rules out table lineage loss or a
binding-only schedule omission. SOLVE-C93 explicitly refuses this ordinary
authored action in the current stateless native profile. The assertion and
capacity checkers were kept intact.

## Canary and construction-module follow-up

The fixed20 after run at `00dac0f28` compiled all20 models and completed16
simulations, with4 typed failures and0 timeouts. Its initial comparator refusal
(missing pinned OMC Services) remains in `canary-after-initial.json`. Official
repository dependency setup was then run, and the existing traces were compared
without repeating Rumoca model attempts:15 compared,15 high,0 minor/deviation,
0 missing,1 existing typed reference-policy exclusion. `canary-delta.json` retains the paired c15dc7518 baseline: all counts
have delta0, with the exact same four failures and one reference exclusion.
Neither run is full-cohort evidence.

CI's architecture scan caught `solve_runtime.rs` at2094 lines. Construction was
moved verbatim into its focused module at `d5d35b3cb`, leaving the runtime1406
lines and construction694. Normalizing only the required internal visibility,
the constructor body matched SHA-256
`b26543d07437aee661d70d0da9cc35343435c7b61b80e567bc504198146aef46`
before/after. Four-package release libraries passed1012 tests
(12 opt,240 sim,728 solver,32 worker), four-package library/test clippy passed,
and local review-scan with `--fail-on-forbidden` passed. No scanner waiver was
added.

## Limits

Full workspace, documentation, all-feature clippy, coverage, full MSL,
ModelicaTest and pinned modelica_models compatibility gates have not been run
for this tip. There is no GATE_OK, full-cohort parity or complete-browser
lowering claim. The SLAM refusals expose remaining target capabilities; they
are not successful SLAM execution or a performance result.

The audit at `65244a9d1` identified a cold-source gap: ordinary event messages
without an assertion projection evaluate numeric Conversion values and dynamic
String options through `eval_row_with_context`. Compiled warnings, prints and
action requests reach that renderer. Those message programs are now in the required inventory at `6982e013b`,
as documented below. Overall source closure remains unproven because other
clock and coupled-event paths are still under audit. Call-scoped assertion projections use checked
invocation observations and are a distinct source path.

## Numeric messages and named reads follow-up

Commit `6982e013b` prepares every unprojected message Conversion value and
present formatting-option program before compiled runtime construction succeeds.
A mandatory execution owner retains the immutable source model; numeric requests
must select an actual action/conversion/field from that owner. Absent authored
options retain the existing Modelica defaults. Projected assertion messages still
render checked captures. Shared formatting and validation were moved intact to
`event_messages.rs`; reference execution uses the same formatter. Source-operation
faults preserve their category/span, and no partial warning is published.

The negative cold-message constructor regression failed before this change:
exactly one test ran and the backend's refused numeric warning program was
accepted despite its unique source span. It passes after preparation is required.
Accepted controls cover all four numeric fields, foreign-action refusal, runtime
clones, malformed scalar output counts, each unsupported dynamic option, and
source-fault publication. A real Cranelift Modelica warning control matched
explicit reference formatting on cold activation and retained once-per-site
reporting.

Named visible reads also bypassed prepared code. The before regression returned
value2 but recorded0 compiled calls rather than1. It now uses the existing
lifecycle program. Broad tests caught an unchecked reference short-input panic
in the first routing change; the checked reference entry was preserved, without
changing its existing error-span assertion.

Validation:476 evaluator tests and12 opt /241 sim tests passed in the broader
run; that run failed1 solver test at the reference short-input regression. After
correction,691 default-feature solver tests passed, including the new bypass
control and unchanged error-span control. Four-package library/test clippy
(eval, solver, sim, binding with sim-rk45), format/whitespace, and the committed
forbidden-architecture review scan passed. Four binding options/trace tests and
one host periodic compiled-receipt test passed. Browser guest execution and the
new fixed-canary delta remain pending for this tip.

Commit `a308734eb` updates the browser policy contracts to require native by
default while explicitly accepting `auto` and `interpreter`, and rejecting an
unknown policy. The stateless periodic-clock receipt requires a compiled engine,
no refusal, and no declined kernels. This reflects the intended policy change;
no production checker or malformed-input rejection was relaxed.

The subsequent cold-path work below closes the identified clock and coupled-event
calls. Overall source closure and landing remain unproven.

## Cold runtime dispatch follow-up

Before-fix tests exposed three clock dispatch bypasses, three refresh bypasses,
four coupled-event derivative/admission failures, two additional coupled primal
failures, two delay-root bypasses, and four public deadline-scheduling bypasses.
These tests observed missing compiled calls or swallowed compiled faults; equal
numeric results alone did not establish compiled execution.

Clock initialization, deadlines, and pulse release now select prepared programs
and their issued output offsets. The shared condition-memory seeding algorithm
remains single-source, with verification explicitly selecting reference execution.
All present coupled-event derivative artifacts are prepared eagerly; genuine
absent derivative capability remains a typed refusal. Coupled residuals,
derivatives, refresh calculations, delay roots, and dynamic deadline scheduling
use prepared code. Public standalone reference scheduling retains the same shared
deadline and tolerance logic. Missing mandatory selected outputs are errors;
optional backend hooks still use their already prepared compiled base kernels.

After these changes, all 756 release solver library tests with `native-wasm`
passed, and solver all-target clippy passed with warnings denied. The first broad
run exposed an obsolete partial-kernel fixture expecting interpretation after
declining a required selected output; its expectation now requires an error.
Another added fixture attempted a zero-output program which the existing source
constructor correctly rejects; the corrected test asserts that exact refusal
and original source span. Neither production checker was weakened.

The frozen LabQuadrotor control again matched all 8904 values bit-for-bit across
24 input-driven steps. NativeRequired recorded Cranelift with no refusal or
declined kernels. Mean advance time in this single host run was 0.312 ms native
and 21.068 ms reference; preparation was 0.911 s and 0.125 s respectively. This is
neither whole-SLAM nor browser performance evidence. `cold-path-physics.json`
retains the source pin, trace digest, receipts, and timing summary.

`cold-path-validation.json` identifies the tested source snapshot and before/after
log digests. The fresh message canary at d42b2e6c0 is retained separately in
`canary-message-followup.json`: 19 compiled/19 balanced, 16 simulations, three
unchanged solver failures, plus an AutomaticSeed worker startup timeout before
model compilation. The 15 compared traces have no deviating channels. This is
an incomplete capability gate, not a green delta or full-cohort result. No model
attempt was retried or excluded.

Remaining substantive gaps are visible: projected function messages have no
native invocation capture/report producer in the scalar backend, and native table
callbacks currently turn checked failures into NaN. CI caught the former at the
exact invocation-observation checker. Required kernels existing in the inventory
also does not prevent a future direct reference evaluator call at the Rust type
boundary; sealed execution owners and read-only analysis views are under design.
Fresh browser/SLAM and pre-landing gates remain pending. These results do not
establish complete lowering for every source program or permission to land.

## Native table fault follow-up

A native row containing a PureCall bypasses reference validation. Its before-fix
control accepted valid table interpolation, but returned success for a missing
table which the reference evaluator rejected. Native table callbacks now return
checked status and write their result only on success. The emitter propagates
that status before reading the private result cell. Invocation-scoped typed
fault storage restores outer table/fault state across nested calls and unwinding.

The table fault is classified as `NativeSourceFault::ExternalTable` and mapped
to a source operation fault, retaining checked details and normal source-owner
span wrapping. The existing validation mirror remains enabled; its checked table
failures receive the same source category. All 164 Cranelift library tests,
three simulation table controls and both packages' all-target clippy passed.
`native-table-faults.json` retains the source/log digests and exact validation
scope. The projected warning product and type-level execution-owner work remain
unfinished, so this is not a whole-runtime closure claim.
