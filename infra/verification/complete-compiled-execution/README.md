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

## Limits

Full workspace, documentation, all-feature clippy, coverage, full MSL,
ModelicaTest and pinned modelica_models compatibility gates have not been run
for this tip. There is no GATE_OK, full-cohort parity or complete-browser
lowering claim. The SLAM refusals expose remaining target capabilities; they
are not successful SLAM execution or a performance result.
