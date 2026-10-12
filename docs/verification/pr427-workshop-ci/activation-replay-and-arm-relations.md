# Activation replay and arm-local state relations

Both original failures were executed at `5b82462ea`, before these repairs.
No AFTER Cargo run was performed in this agent lane.

| Original control | First failure | Raw log SHA256 |
|---|---|---|
| `nested_model_arm_function_interval_survives_structural_replay` | Structural reconstruction replaced a constructor-issued parent guard Conditional by its selected operand; canonical guard replay refused the changed operation | `ed7a4af9235f794c6c91609babfc4265a6670b6ee9ecea719b5d3c8e85cf6031` |
| `real_quotient_nested_first_true_arms_protect_later_conditions_and_ratios` | ToDAE ED020 at the later condition `mod(1/time,7.0)>0`: ordinary state-relation publication had no arm-owned admission path | `e2ec54360661d784b62f328883cc6d24481dff6b9b36c9cbfb24b8956d571878` |

The raw logs are in the owned integration cycle under
`pr427-integrated-activation-after-5b824-20261012/activation-structural-one.log`
and `pr427-integrated-activation-public-remainder-5b824-20261012/activation-public-four.log`.
The latter selected four tests: inactive undefined ratio and selected undefined
ratio controls passed; nested first-true and the separately repaired structured
root certificate failed. This relation repair does not claim that separate fix.

The structural repair projects the exact generated operation identities from
`ModelConditionalView`. It preserves its canonical Not and parent guards while
ordinary conditional folding remains enabled. No condition text, span identity,
or arbitrary Boolean is activation authority.

The relation repair uses the already issued expression's ModelArm. During live
construction the arm must be lexically visible; outside a live arm its exact
conditional must already be closed, as required by whole-model replay. A
function construction boundary rejects an arm relation. Existing closed
coordinate checks still reject function or domain captures at this scalar
boundary. Root observation and Boolean relation reads derive the same guard;
live observation evaluates the relation inside the selected region, whereas
ordinary reads preserve relation buffering. Strict JSON and bincode replay
re-run admission. Existing DAE47/Solve78 fields carry this authority; no new wire
field, older-wire adapter, or schema relabel is introduced.

This implements MLS §3.6.5 first-true conditional evaluation and §8.5 event
observation through the existing owners, under accepted SPEC0007 / DAE-C22,
SPEC0032 compact-domain rules, and SPEC0029 shared-owner boundaries. SPEC0036
and SPEC0037 remain DRAFT and are not asserted as accepted requirements.

Initial focused qualification commands (ordinary CI-selected suites):

```sh
cargo test --locked --offline -p rumoca-phase-structural --lib dae_transform::tests::model_conditionals:: -- --nocapture
# 3 tests; includes unchanged original, literal replacement, ordinary folding.
cargo test --locked --offline -p rumoca-ir-dae --lib tests::model_conditionals:: -- --nocapture
# 7 tests; includes exact guard/open/foreign admission and current wire replay.
cargo test --locked --offline -p rumoca --test suite_core reported_bug_runtime::main_dae_integer::real_quotient_ -- --nocapture
# 4 unchanged original activation tests.
cargo test --locked --offline -p rumoca --test suite_core model_arm_relation_protects_inactive_arithmetic_without_a_quotient -- --nocapture
# 1 independent source model, Interpreter and Auto, all 11 sampled integrals.
```

The independent non-quotient source keeps its authored conditional and square
root. Its full integral is `min(t,0.5) + 2*max(t-0.5,0)`; it must protect the
negative square root while inactive and activate the later branch at its
boundary. It is supplementary coverage, not a rewrite of the original failure.

Guard dependency followup (source description): the canonical model owner
projects the relation and its exact activation guard together. Scalar relation
admission checks both roots for closed coordinates. B.1c construction checks
both roots against the same issued producer prefix. Clock ownership, continuous
root classification and condition-memory scheduling inspect both roots,
including a `previous` coordinate occurring only in the guard. This preserves
MLS §16.5 partition ownership and §16.8.1 clocked event semantics; it introduces
no new domain expansion or wire field. Structured numeric quotient families
retain their separate domain-aware admission.

Original test-only commit `a8286477f320128f42353dd01a456de7cc627cfb`
is based on the unchanged dependency implementation in `3fb67e6a`. Its tests
keep independent positive controls: the same B.1c producers in correct order
with strict bincode roundtrip, and a constant primitive whose guard alone owns
the issued 0.1 clock. ROOT executed their BEFORE at `98a307a4e`: IR selected two
tests, with the binder control passing and the producer-prefix control failing;
the clock test failed because Previous escaped its clock schedule. The binder
control already fails closed because expression insertion merges the active
guard's binder domain into each arm expression. It is retained protection, not
a newly established missing proof. Integrated AFTER results are recorded below.

| BEFORE log | SHA256 | Outcome |
|---|---|---|
| `pr427-arm-guard-dependencies-before-98a3-20261012/arm-guard-construction-before-two.log` | `cc41be2029d61ce8a0038e81867f64593a4ad88299c57018a41eff633126f304` | 1 pass / 1 fail |
| `pr427-arm-guard-clock-before-98a3-20261012/arm-guard-clock-before-one.log` | `9530624009ffc3ebe57167def1d917dff23eaf81d5df48cec4319afe718c60cf` | 0 pass / 1 fail |

```sh
cargo test --locked --offline -p rumoca-ir-dae --lib tests::model_conditionals::guard_dependencies:: -- --nocapture
# Exactly 2: guard-domain refusal, and guard discrete-producer-prefix proof.
cargo test --locked --offline -p rumoca-phase-solve --lib tests::clocks::arm_relation_previous_guard_owns_its_condition_memory_clock -- --exact --nocapture
# Exactly 1: no continuous roots, one condition-memory row on its issued clock.
```

The complete model-conditional IR selector contains 9 tests after this test-only
commit. The earlier 7-test command above records the initial repair's scope.

The clock followup visits every reachable expression and condition once using
the existing IR expression traversal. It expands each exact arm guard and
checked condition coordinate, including nested Relation/Discrete operands.
Ancestor guards are explicit operands of the issued parent-guard Conditional.
The conflict checker consumes every found clock rather than a first-clock
summary. Periodic condition activation lanes alone are not reclassified as
clock-partition ownership. No arithmetic evaluator or expression grammar is
duplicated. The original mixed-clock control is test-only commit
`336cfe02f5b3aadd77f8c202d39c7cf1ea4f7cd7`. Its first attempt at
`7b8e3971ce61e3568f21c5cd471092705ce60edb` failed compilation because the
frozen view called a method implemented only on constructor storage: no test
executed. Compile-only repair `2bf99fa42463b68831dcd8fddd0598052452af30`
shares one projection function between both storage views.

At `9a47e85b0ef58263b1a3070b58a40008d176f67d`, the unchanged mixed-clock
control executed and failed: lowering reported `previous coordinate escaped
its owning clock schedule`, rather than rejecting both clocks at the condition
ownership proof. This establishes the first-owner masking defect separately
from the earlier compile failure. Integrated AFTER results are recorded below.

[The exact BEFORE receipt](arm-relation-guard-before.json) binds all four
attempts to their source commits, counted recipes, raw log and guardian hashes,
commands, observed Rust summaries and resource measurements. The executed
BEFORE results are IR 1 pass / 1 fail, clock 0 pass / 1 fail, and mixed clock
0 pass / 1 fail. The compile-only failure has no observed test summary.

At integrated source `379f3d56ad90e1f6046275c3af752672cdc42706`, ROOT
executed the full IR DAE library (218), full phase-Solve library (185), full
structural library (266), and four unchanged public quotient activation tests.
All 673 tests passed with zero ignored tests. The next, supplementary square
root test failed at its 11-row assertion because the actual trace contained
12 rows. That source did not print timestamps or values, and the failure
preceded numerical assertions and the second execution policy. The final
20-test guide gate was not executed in this fail-fast attempt.
[The integrated AFTER receipt](arm-relation-guard-after.json) binds these five
executed gates and the unrun gate to their exact source, commands, raw logs,
count receipts and unchanged resource guards. This is no whole-guide or remote
CI qualification.

The square root test correction retains the original model and tolerances,
prints the actual trace, requires every original grid point, and checks all
recorded integrals and nonboundary branch outputs. Extra observations must
match the source event at 0.5 under the canonical timestamp predicate; extras
at any other grid point are refused. The actual publication path is
`rumoca_sim::diffsol::simulate_prepared` → the common FMI-ME session. Its
`apply_located_root` publishes the checked event-left observation and then the
settled observation. `MeTraceRecorder::decide` retains distinct timestamp bits;
its exact-coordinate role policy controls replacement. The similarly named
`timeline::merge_output_times_with_event_observations` has no production caller
in this source. This explains the applicable publication contract, and does
not establish the actual twelfth row's legitimacy. ROOT then executed the corrected test at `1117563e41982a014bbd733b9d1cbbf64c557b23`: one Rust test
passed under both Interpreter and Auto. Both actual traces contained the same
12 finite ordered observations, including `0.49999999999999994` and `0.5`,
with `y=1` on the former and `y=2` on the latter. Every recorded `x` satisfied
the unchanged independent integral tolerance, every requested grid point was
present, and all nonboundary branch outputs matched. The supplemental receipt
records the exact arrays and raw evidence hashes; this qualifies this test,
not the separately pending guide scenarios or remote CI.
