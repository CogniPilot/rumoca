# Flight corpus source adjudication at 5e6

All eight unchanged flight pins passed using compiler source
`5e6f9cb9afec4caf992a370db78c1a5b342d5336` and the correctly identified external
corpus revision `3d6204e7a907e452e08c46cd63352d6365089a38`, tree
`89d8e8f31aa6f3f472184c92a3c67d8b111b8021`. All 747 file hashes and sizes matched
the archived Git snapshot before and after execution. No model, input,
expectation or native tolerance changed. The preserved release CLI reported
`5e6f9cb9afec`; the exact CLI/xtask ELF hashes are in the receipt.

The existing corpus owner ran five codegen rows and three simulation rows in
53.798 seconds with four workers. Navigation estimator GALEC/embedded-C, UKF,
Controller, Cubs2 OuterLoop, Waypoint RK/BDF and Cubs2 Takeoff all passed. The
whole unchanged schema-2 manifest still supplied each row's 192.857-second
deadline. The 600-second outer guardian completed without a stop, peaking at
2,988,040 KiB aggregate RSS plus swap. The copied artifacts and CSVs are retained
outside the parse cache.

```sh
cargo xtask verify corpus-pin --models-root <immutable-3d6204e7-root> \
  --msl-root <pinned-MSL-4.1.0-root> --rumoca-binary <sealed-5e6-CLI> --jobs 4 \
  --only flight/navigation-estimator/galec-production \
  --only flight/navigation-estimator/embedded-c-galec \
  --only flight/ukf-estimator/galec-production \
  --only flight/controller/galec-production \
  --only flight/cubs2-outer-loop/galec-production \
  --only flight/waypoint-mission/rk-like \
  --only flight/waypoint-mission/bdf --only flight/cubs2-takeoff/rk-like
```

The compatibility CI revision `a41f7c0c00b55c1bf54f03c9b66b901ba8e43c6f` is a
separate corpus/gate. It does not contain the pinned UKF class, changes the
Controller composition, and starts Cubs2 at zero rather than the numerical
pin's 0.1 metres. The correct revision restores that original declared start
and reproduces every unchanged Cubs2 pin. These were input-revision mismatches,
not evidence for changing the numerical baseline or compiler. Canonical
`target/verification/corpus-config.json` must select the numerical source
separately; the compatibility checkout must remain independently pinned.
Durable automatic immutable provisioning is a separate tool-owner change.

OpenModelica `a96aa1a-cmake` (the realized unstable-2026-07-21 tool) simulated the
same unchanged Cubs2 model, horizon 0.02, two output intervals and DASSL. Its
first reference at solver tolerance `1e-6` succeeded but missed the elevator
endpoint: `0.01720248923576681` versus pin `0.017205550449028235`, a `3.06121e-6`
error outside the unchanged `1.17206e-6` bound. That discrepancy remains in the
receipt; it is not reported as a strict reference pass.

Two separately recorded tighter references reused the identical generated ELF
and initialization XML. Runtime `-tolerance=1e-8` and `-tolerance=1e-10` were
confirmed by solver logs; neither changed Rumoca or any pin bound. Elevator
endpoints were `0.01720529546393665` and `0.01720533367942436`, respectively.
Their difference was `3.82155e-8`, and the tighter endpoint differs from the pin
by `2.16770e-7`. All six original endpoint pins passed at both precisions.
Position changes between the tighter references were `2.85305e-10` (x) and
`5.56438e-12` (z). These measurements support an underresolved first reference;
they do not establish bit identity or whole-observable parity.

Earlier attempted `-override=tolerance=…` commands warned that tolerance was not
a model variable and did not alter solver precision. Their immutable logs are
retained and excluded from convergence evidence. The corrected precision
controls finish before the guardian's first periodic resource sample, so their
zero/null peak readings mean unobserved usage, not measured zero memory.

[The portable receipt](flight-source-adjudication-5e6.json) binds the immutable
source, original pin manifest, eight actual row commands/results, all CSVs,
reference ELF/XML, precision logs and resource records. Raw evidence is under
`$HOME/scratch/rumoca/workshop427-final-rebind-5e6f-20261012/flight-adjudication-3d6204e7`.
This focused result does not qualify all 28 corpus rows, the separate
compatibility gate, a full MSL cohort, canonical quick/full, coverage or a
later source tip.
