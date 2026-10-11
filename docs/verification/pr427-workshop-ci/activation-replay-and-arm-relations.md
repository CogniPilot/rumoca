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

Pending exact qualification commands (ordinary CI-selected suites):

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
