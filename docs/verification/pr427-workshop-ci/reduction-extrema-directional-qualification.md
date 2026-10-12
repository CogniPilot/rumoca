# Compact typed extrema directional qualification

The checked Minimum/Maximum reduction lacked a directional owner. The original
Frame/Dim diagnostic also measured the first refused typed operation as Real
`Reduce Maximum` before scalar inline fallback failed at an exact Integer read.
The canonical producer now keeps its original Reduce once and constructs one
ranked Fold tangent. It starts with the actual first cell, skips the first
comparison, and shares the scalar min/max tangent rule. No extremum identity,
pointwise compiler expansion, function-body relowering, wire field, or checker
exception was added. Narrow coordinate profiles still decline the derivative;
zero extents remain refused by `SolveValueType::tensor` before primal admission.

The owners follow SPEC_0007 and SOLVE-C51/C56 in SPEC_0040, compact-domain
construction in SPEC_0032, crate boundaries in SPEC_0029, and the before-proof
process in SPEC_0033. Scalar tie tangents retain the existing <=/>= policy;
independent finite differences apply only away from ties. The canonical primal
bit results remain unchanged, including signed zero and nonfinite controls.

| Source | Actual qualification |
| --- | --- |
| 8e606849c | Eval2 FAIL and IR1 FAIL, unavailable directional owner. Cargo's first attempt stopped after Eval2; IR1 ran separately. |
| a8c7b541 | New Eval2 + IR1 PASS. |
| 526341020 | Full Eval328 + IR611 PASS (939 total); both packages all-target/all-feature Clippy with `-D warnings` PASS. |
| dd3e8034d | Final `cargo fmt --all -- --check` PASS; original Frame/Dim2 PASS, continuation7 PASS, runtime slices7 PASS/1 FAIL. |

`526341020` retains every original continuation model and numerical oracle,
replacing obsolete inline-path census assertions with typed-owner/structured-
region assertions. `dd3e8034d` changes only test-module registration ordering.
The original selected-row slice model still refuses with a checked call
argument/slot interface error; its output assertion was not reached. This is an
explicit unresolved first-owner residual, not a passing numerical trace.

All model/test attempts used the unchanged 600-second, 4-GiB aggregate RSS plus
swap, 8-GiB cache and scratch reserve, and 16-GiB host-available guards, with
4/4/4 concurrency. No retry or cap increase was used. Raw logs, guardians,
tracked-source snapshots, and the exact suite ELF remain in the owned scratch
attempt directory; the adjacent portable JSON binds their hashes, commands,
counts and limits. The first BEFORE snapshot was taken during its already-
started immutable build; terminal matching confirms no source mutation.
Full CI, every original slice case, forced-native sample parity, and OMC parity
are not claimed by this receipt.
