# Retained workshop and issue validation status

Read-only source snapshot: `e5388bd4bf2c08e4c20d29751bd3fedb95cb8fb6`,
2026-10-11. This is the local workshop candidate, **not complete PR421+427
composition**. The [evidence manifest](workshop-bug-status-retained-20261011.json)
binds each reported attempt to its frozen recipe, exact source, raw log and
Guardian SHA256. Artifact paths are relative to
`$HOME/scratch/rumoca/slam-integration-cycle-20261010`.

GitHub still has exactly nine open issues of type Bug: 403, 405, 411, 418, 422–426.
No newly appeared Bug issue or reproduction was found. PR427 remains open at
`32f75cfa310ac2f1a4b12c874093038203de1b0c`; PR421 remains open at
`f84a4304ece1a101e2f9a28275b1ed76fefdf87e`. Local fixes are not merged-fix or
issue-closure evidence. No production edit, build, issue closure or publication
was performed for this document.

## Original issues

The original public runtime cohort has ten tests:403 contributes three and 411
two; 405/418/422/423/426 each contribute one. All ten passed at source
`1447ed716d44cc3b020935f38bfb16372bab7fda`. Its larger 14-test selection actually
returned **12 passed / 2 failed / 0 ignored**. The two broader Integer failures
are not omitted or relabeled as an all-green gate.

| Issue | Measured original evidence | Remaining qualification |
|---|---|---|
| [403](https://github.com/CogniPilot/rumoca/issues/403) | Profile observation, explicit-if protection and active-invalid-index fault all pass at 1447. | MLS does not require And/Or short-circuiting; preserve that corrected oracle. Refresh final source. |
| [405](https://github.com/CogniPilot/rumoca/issues/405) | Original interactive input schedule switches at each write at 1447. | Refresh final source; no new event fix implied. |
| [411](https://github.com/CogniPilot/rumoca/issues/411) | Original wide Integer main-DAE mod/div both pass at 1447. Broader comparison/call evidence below. | Final source refresh and the separately failing structured outer-family case. |
| [418](https://github.com/CogniPilot/rumoca/issues/418) | Original explicit-algebraic B model renders public JAX at 1447. | Actual original exported JAX execution/JIT/forward/reverse AD remains unrun in these receipts. |
| [422](https://github.com/CogniPilot/rumoca/issues/422) | Original U4 passes all 11 native samples at 1447, one checked owner, Cranelift receipts without refusal/decline. | Actual GNU static-PIE original plus canonical memcmp unit pairing remains unrun. Ordinary native relocation is not static-PIE proof. |
| [423](https://github.com/CogniPilot/rumoca/issues/423) | Original C13 analytic runtime passes at 1447; original compiler regression passes at 673. | Refresh final source. Existing quotient repair is retained; do not duplicate it. |
| [424](https://github.com/CogniPilot/rumoca/issues/424) | Four compiler controls pass at 673, including original precise shape rejection and the reported valid reverse-order/value case. | Three parameter/Boolean dimension controls are integrated but unrun in these receipts; seven424 tests/nine whole compiler-module tests. |
| [425](https://github.com/CogniPilot/rumoca/issues/425) | No original38 controlled memory/quiescence result. | Integrated 6393 memory+semantic tests remain unrun here. No peak-memory improvement or 20/30/38 scaling claim. |
| [426](https://github.com/CogniPilot/rumoca/issues/426) | Original G1 full 11-sample analytic runtime passes at 1447; compact compiler regression passes at 673. | Refresh final source; preserve active-invalid-index controls. |

Source 673 is `673ce6ec74418fb0043d3ad7354e0ceb2c7543da`; its whole original
compiler module actually passed **6 / 0 / 0 ignored**. Original 424's authored
`m[3,2]` with `for i in1:3,j in1:2` has constructor shape [2,3] and is rejected
by pinned OMC as well. That does not invalidate the issue's conforming
reverse-order, parameter-range and Boolean cases. The original source stays
unchanged. See [the typed controls](issue424-typed-constructor-controls.md).

## Broader controls and tracked examples

| Frozen attempt / source | Actual result | Scope |
|---|---|---|
| `pr427-comparison-controls-after-e690-20261011` / `e69072c2a831f227099f5cd6eed585acb0f6a878` | 3 pass, 0 fail, 0 ignored; exit0 | Exact enclosing Integer comparisons, mixed Real promotion and native-owner/full-session control. |
| `pr427-descendant-call-and-assertion-after-20261011` / `ca4962550e4bc9be1077f0ad633471866b956312` | Separate gates: descendant calls 3, branch assertions 5, assertion scopes 3, message projection 9; all pass, 0 ignored, exit0 | Live-input descendant call/full traces, active/inactive assertions and declaring message frames. Does not prove every structured family. |
| `pr427-tracked-examples-after-ca496-20261011` / same ca496 | 5 pass,2 fail, 0 ignored; Guardian/xtask exit1, Cargo suite exit101 | All seven tests ran. This attempt was a semantic failure, **not a resource stop**. |
| `pr427-foreign-lhs-binder-before-d62-20261011` / `d62ecd954bb936a49c3ef23128c41ef4d4a44493` | 0 pass,1 fail, 0 ignored; exit101 | Checked B1c family incorrectly accepts a foreign same-spelling LHS binder with constant RHS. This is a constructor before witness. |

Five tracked-example tests pass: representative scalar, Ball free-fall, Circuit
RC charging, scalar/tensor exponential decay and local switched-RLC response.
The aggregate compilation test logs balanced success for 14 of 15 tracked model
entries. Both failing tests include `KalmanFilterStepTest` ToDAE **ED019**:
`Modelica.Math.Matrices.LAPACK.dgetrf` passes protected local `m` to its external
body without a checked owner. Kalman's authored numeric initialization
assertions were not executed: compilation failed first. No full-workshop
success or external-function repair is inferred.

The foreign-LHS before test panics at its intended assertion, not during
compilation. The existing source-provenance repair and issued WASM fixture
refresh are present in the snapshot; their complete after qualification remains
separate from this before receipt. See
[the retained producer plan](pr427-main-dae-integer/structured-binder-provenance-after.md).
Partial/current running gates must retain their own receipts rather than
inherit any earlier source's result.

## Mandatory next selections

The [selector inventory](workshop-bug-validation-ca496.md) records PR workflow
reachability and exact commands. Ordinary workspace CI selects registered
compiler/runtime controls. Original 418 actual JAX is selected by the PR JAX
job (`--require-external-tools`); original 422 GNU static-PIE is selected by the
Linux PR script. Missing results are qualification gaps, not missing wiring.

Keep the original ten runtime tests separate from 425's independently mandatory
semantic test. Final refresh must select exactly 10 original runtime tests,
9 compiler module tests (7 with the424 prefix), 1 original 418 JAX test, the two
static-PIE tests separately, and each 425 test separately. Both 425 tests need
ordinary default features when paired with the actual CLI memory witness;
fmu-packaging alone does not build the required CLI. The 2 GiB address-space
limit is not a peak-RSS measurement. Original tracked examples remain a
seven-test gate, including both failing Kalman-dependent tests.

These selections and the structured-provenance/wire/current issued WASM gates
are pending complete final-source qualification. No issue is closed on a
render-only, compiler-only, ordinary-native or unmerged-source result.
