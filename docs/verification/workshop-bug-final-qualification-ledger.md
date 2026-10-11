# Workshop Bug qualification ledger

Read-only source inspection snapshot:
`9694c8fdf7ae32fb485c1fbf3e2f10858043e581`. This local PR427 workshop
candidate is not complete PR421 composition and is not a merged fix.
The fresh GitHub issue-type query still finds exactly nine open Bugs:
403, 405, 411, 418, 422–426. Published PR427 remains
`21d64ecaa9d205b671598991ee8c4d2f556fed6c`; PR421 remains
`f84a4304ece1a101e2f9a28275b1ed76fefdf87e`. No new Bug reproduction was found.
No issue comment, closure, publication, Cargo run, or cache mutation was made
for this ledger.

## Original reports and retained evidence

The [earlier manifest](workshop-bug-status-retained-20261011.json) binds the
1447 runtime and 673 compiler receipts to exact source, recipe, logs and
guardians. Its old pending labels remain historical. None of those receipts is
promoted to a test of 9694. The ongoing focused activation gates likewise have
no completed outcome in this ledger.

| Issue | Existing owner / measured evidence | Remaining original qualification |
|---|---|---|
| [403](https://github.com/CogniPilot/rumoca/issues/403) | All three original profile/protection/fault controls pass at 1447. MLS §3.3 permits eager And/Or operands; explicit-if protection remains required. | Final-source three-test refresh. No unsupported must-short-circuit oracle or new duplicate fix. |
| [405](https://github.com/CogniPilot/rumoca/issues/405) | Existing input-event repair, with original scheduled writes passing at 1447. Earlier main history uses squash `976a5141541`, not false direct ancestry to the unsquashed fix. | Final-source scheduled-write refresh. |
| [411](https://github.com/CogniPilot/rumoca/issues/411) | Main-DAE exact Integer quotient repair `13e1581d0`; original wide-literal mod/div pass at 1447. Broader descendant/comparison controls have separate retained receipts. | Final original mod/div refresh and broader affected owner controls; typed-function support alone is insufficient proof for this report. No PR421 repair is assumed from its title or source claim. |
| [418](https://github.com/CogniPilot/rumoca/issues/418) | Existing public JAX exact algebraic-assignment emission; original B renders at 1447. | Execute the original generated module with JIT, forward and reverse AD. The unrelated five-model export passes do not discharge this exact original gate. |
| [422](https://github.com/CogniPilot/rumoca/issues/422) | Canonical JIT host registration `4f02a729c`; original U4 native 11-sample curve and receipts pass at 1447, plus the canonical relocation control has actual ordinary-native evidence. | Original U4 and canonical memcmp unit in verified GNU static-PIE executables. Dynamic/native success does not prove Linux release linkage. |
| [423](https://github.com/CogniPilot/rumoca/issues/423) | Existing parameter-divisor admission retains the parameter read and structured binder; original C13 curve passes at 1447 and compiler admission at 673. | Final-source compiler/curve refresh plus retained zero-divisor/override/continuous-root controls. No second quotient evaluator. |
| [424](https://github.com/CogniPilot/rumoca/issues/424) | Resolved iterator provenance and compact parameter shape repair `c5bac3a7a`; all seven dimension/value controls pass at b4, including parameter extents and Boolean masks. | Final-source seven controls plus genuine iterator scope controls. Original C3 is correctly shape-rejected by Rumoca and OMC; its reported reverse-order variant is conforming and must remain positive. |
| [425](https://github.com/CogniPilot/rumoca/issues/425) | Construction-issued projection demand repair: actual original38 memory PASS1 under unchanged 2 GiB child AS/Rayon1, and independent state/native PASS1 at 87c. | Fresh final CI and landing. These measured model gates do not establish 20/30/38 scaling, 100k-state feasibility, or a general peak-memory guarantee. |
| [426](https://github.com/CogniPilot/rumoca/issues/426) | Existing inactive-if-arm scalar projection fix; original G1 compiler pass at 673 and complete 11-sample analytic runtime pass at 1447. | Final-source compiler/runtime refresh; active invalid accesses must still fault. No duplicate stencil fix. |

1447 is `1447ed716d44cc3b020935f38bfb16372bab7fda`; 673 is
`673ce6ec74418fb0043d3ad7354e0ceb2c7543da`; b4 is
`b4e38503723816d0529168c46ac1b027ca1ea73d`; 87c is
`87c16773ef1be4c03f8c61f78e0aed387b2058f8`.
The seven #424 results are in
`pr427-reviewed-compact-and-shape-after-b4-20261011/parameter-dimensions-and-masks-7.log`
under `$HOME/scratch/rumoca/slam-integration-cycle-20261010`: seven passed,
zero failed/ignored, no resource stop. This supersedes the actual e538
four-pass/three-fail outcome for those controls, not its preserved history.
The [Euler original-model receipt](user-guide-ci/euler425-original-memory-after-87c.md)
binds both successful model gates and preserves the d65 failure/timeout.

## Required final selections

These source-derived counts distinguish original reports from broader controls.
All commands remain pending final-source execution here. Use existing bounded
counted recipes with jobs/Rayon/test workers four; do not treat a zero-test exit
as qualification.

```sh
# Ten original runtime tests; exclude independently qualified expensive Euler.
cargo test --locked --offline --jobs 4 -p rumoca --test suite_core \
  reported_bug_runtime::issue_ -- --nocapture --skip issue_425

# Nine compiler tests, of which seven are the #424 shape/value controls.
cargo test --locked --offline --jobs 4 -p rumoca-compile --lib \
  reported_bug_regressions:: -- --nocapture

# One original B export: import, JIT, forward AD and reverse AD.
cargo test --locked --jobs 4 -p rumoca --no-default-features \
  --features template-runtime-tests --test suite_template_runtime \
  jax_ode_target_handles_reported_explicit_algebraic_assignments -- --nocapture

# Two independent static-PIE test executables, one exact test each.
bash .github/scripts/qualify-issue422-static-pie.sh "$OWNED_EVIDENCE_DIRECTORY"
```

The ten-test runtime command uses default features, including the actual
CLI/native path. The original B JAX command uses the existing JAX CI feature
profile and required test-target feature `template-runtime-tests`. The test
directly starts Python and requires installed JAX;
its mandatory PR JAX workflow selects it through
`cargo xtask verify template-runtimes --backend jax --require-external-tools`.
That strict option belongs to xtask, not a Cargo feature. The static-PIE script is
selected by the Linux PR job, checks ELF64/DYN, absence of INTERP/NEEDED, and
retains executable hashes; its U4 run requires all 11 native receipt lines.
These are missing measured outcomes, not missing CI wiring. Both #425 tests
are registered in ordinary default-feature CI and have independent exact
selectors in the Euler receipt. There is no new test or production fix here.

## Independent workshop evidence and documentation picks

At `51398d4ab2e8ce900ebe2326e53f564f1ef152da`, original Glycol with its
viscosity probe passed one test in 14.89 seconds; the independent nested
exponential interface passed one in 10.34 seconds. These are supplemental
workshop evidence, not any of the nine original Bug reports and not a full
guide/CI result. Raw attempt:
`pr427-original-glycol-log-remainder-51398-20261012` under the same scratch root.
The reciprocal quotient test's later oracle correction is test-only; its final
numeric qualification is pending the current counted run.

Remaining pick order after the already integrated 53f source/test chain:
`0c9398a4ea9190268f7e61c34be4fa37b20efabc`,
`1f56fd9956b775de3acae7350301b5fd43e7ab26`,
`f01e8045d609166cf18ba40c0b24e0155e26420b`, then this ledger.
The 0c/1f/f01 chain adds/updates only the two Euler receipt documents and
corrects current native guide coverage from 19 to 20. At inspected 9694, their
existing touched files have no changes relative to that chain's 53f base, and
the newly added receipt paths are absent: no source conflict was identified.
The historical c851 19-test failure scope remains unchanged. No unconditional
all-guide, full-workshop, final-CI, merged-fix, or issue-closure claim is made.
