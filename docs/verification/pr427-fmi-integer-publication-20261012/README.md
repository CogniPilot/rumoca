# FMI Integer publication qualification

The original issue #411 modulo source executes correctly through FMI 2 and FMI 3, Model Exchange and Co-Simulation. A separate variant changes only `mod(` to `div(`: its exact settled internal result is 1286742750677285, outside the existing public Int32 domain. The repaired getter returns FMI Error and preserves the entire output array. It does not expose wide division as a supported public Integer.

At historical source `5e6f9cb9`, the lifecycle-settled FMI 3 ME probe returned OK and -2147483648 for that same value. The retained counterexample identifies the publication conversion as the first loss, after exact internal computation. Earlier probes without event settlement do not establish this fault.

The fix uses one shared finite/integral/signed-domain predicate, settles once, validates every requested value, then publishes with pure reads. Int64 validation uses the half-open upper bound 2^63. The public XML Integer profile and internal typed arithmetic remain unchanged. This follows the active pipeline, crate-boundary, and counterexample requirements in SPEC0007, SPEC0029, and SPEC0033, with MLS 3.7 §3.7.2 and the FMI 2.0.5/3.0.2 Integer ABI. SPEC0038 is a draft.

Clean source `347549f1a6b33963c273171e61841843383abb7d` passed nine exact tests with zero failures or ignored tests. The publication selector executes 48 actual lifecycle cases: 28 original modulo checks, four wide-division atomic refusals, and 16 signed Int32 boundary checks across both FMI versions and interfaces. Direct helper checks cover nonfinite/fractional values, signed64 boundaries, and Enumeration domains; these are not authored nonfinite producer simulations.

Supporting controls include three duplicate-name helper tests, public Enumeration getters, lifecycle/tensor Real and Boolean behavior, typed variables, quotient semantics, and Integer array-size metadata. Official XSD, pinned FMPy 0.3.30, pinned VDM, importer source builds, and copied-archive rejection controls remain mandatory. The duplicate-name helper now restricts changes to the second supported direct ModelVariables child, including Enumeration.

All-target/all-feature Clippy for rumoca and phase-codegen passed, and the final `cargo fmt --all -- --check` passed at that same clean source. The machine receipt binds each command, guard, source manifest, raw log, and sealed test executable without embedding process histories. Each child keeps the original 600-second, 4-GiB resident-plus-swap, 8-GiB cache, 16-GiB host-reserve, and 8-GiB scratch-reserve limits with four jobs/threads.

Earlier failures remain separate: a missing feature yielded zero tests; the first correctly enabled attempt failed the old Float64-only duplicate-name helper; the captured dirty mixed-kind helper then passed all six runtime selectors. A local inlined-template UBSan diagnostic passed but is not the production registry proof. These receipts are retained rather than overwritten.

This focused snapshot does not establish a later Root quick/full or GitHub CI verdict, nor resolve the separate issue #403 short-circuit policy question.
