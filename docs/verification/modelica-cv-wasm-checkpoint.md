# Modelica vision / SolveIR WASM integration checkpoint

This branch is rebased onto main revision 5a65f88eebec9128c71f09c22ba4e02180e17d50. It collects the existing reusable compiler work required by the browser Modelica vision application. It is a review and CI checkpoint; full connected RGB-D SLAM is not working yet.

Implemented compiler areas include compact array/stencil expression WASM emission, typed pure-call WASM execution, certified native mixed assignments and one checked whole-program schedule, source-owned dynamic gather faults, conditional dependency projection, compact function-loop preparation, and exact tensor storage views when an optional Flat template is disabled. The binding exports executable bytes and layout/fault metadata without requiring browser-side compiler IR decoding.

The array-storage fix preserves the original Flat physical row view for whole-array, scalar binder and binder-prefix families. Three ownership-validator tests, four independent analytic trajectory tests (including both prefix iteration directions), and the unchanged each-start regression passed on the preserved pre-rebase source. Post-rebase testing is pending at this publication checkpoint. Older component evidence in this directory is historical and does not certify this branch tip.

The unfinished normalized-function/branch-region production migration remains in a separate private overlay and is not included here. The standalone generated-local/normalization groundwork present here does not imply production cutover. Persistent landmark/keyframe ownership, verified geometric loop admission, complete connected localization, full browser integration, and throughput qualification remain outstanding. This checkpoint does not claim 10x realtime, F32 image transport, or a complete FMI LS WASM FMU.

Raw historical profiler traces are retained outside this PR's changeset. CI and new current-source checks must qualify this revision. The fixed 20-model canary delta, full workspace checks, coverage/pre-landing gate, full MSL/ModelicaTest gates and pinned modelica_models gate are pending. No cohort parity or merge-readiness claim is made.
