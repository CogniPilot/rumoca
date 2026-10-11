# Workshop Bug validation inventory

Source audit: `ca4962550e4bc9be1077f0ad633471866b956312`.
Queued test-only candidates: #424 `2d360d6d6638a2fd436d7f29c9de4fd6773a12f6`
and #425 `6393d8d5b525379fa11ebdeea73817aa97bca34f`.
This is the local workshop candidate, not complete PR421+427 composition.
No build, cleanup, external publication or issue closure was performed for
this inventory. Earlier measured results retain their own source identities.

## Ordinary PR coverage

`.github/workflows/ci.yml` triggers on pull requests into main. Its Test matrix
runs `cargo xtask verify workspace`, which selects workspace nextest with normal
default features and the existing MSL feature. rumoca-compile unit tests and
rumoca's registered suite_core therefore run without new selector wiring.
The memory test is Linux-only; its real CLI requires default rumoca features,
including native-allocator. The two queued candidates become reachable through
these existing suites once integrated; they are not present at ca496 yet.

Runtime selectors below are prefixed by `reported_bug_runtime::` in rumoca's
`suite_core`. Compiler selectors are prefixed by
`session::tests::reported_bug_regressions::` in rumoca-compile's libtest.

| Issue | Original/control selectors already present | Remaining actual qualification |
|---|---|---|
| 403 | `issue_403_boolean_operand_order_is_a_profile_observation`; `issue_403_explicit_if_control_guarantees_inactive_operand_protection`; `issue_403_active_invalid_index_keeps_its_fault` | All three passed at1447. Refresh final source; no mandatory And/Or laziness claim. |
| 405 | `issue_405_original_input_schedule_switches_at_each_write` | Passed at1447. Refresh final source; broader event variants are separate controls. |
| 411 | `issue_411_original_main_dae_integer_mod_retains_wide_literal`; `issue_411_main_dae_integer_div_retains_wide_literal` | Originals passed at1447. Broader Integer owner/admission controls have separate outcomes below. |
| 418 | `issue_418_original_explicit_algebraic_model_renders_public_jax_target` | Rendering passed at1447. Actual original JAX execution/AD remains unrun. |
| 422 | `issue_422_original_array_function_runs_without_native_fallback` | Native U4 passed at1447: one checked owner,11 Cranelift samples. Actual GNU static PIE remains unrun. |
| 423 | Runtime `issue_423_original_iterator_mod_simulates_its_analytic_solution`; compiler `issue_423_iterator_mod_parameter_stays_in_its_domain` | Runtime passed at1447; compiler passed at673. Refresh final source. Parameter override/event-owner variants are separate scope. |
| 424 | Compiler `issue_424_original_constructor_rejects_incompatible_declared_shape`; `issue_424_conforming_constructor_has_last_iterator_outermost`; `issue_424_reported_reverse_order_constructor_has_conforming_dimensions_and_values`; `issue_424_partially_unknown_declaration_keeps_mismatched_known_axis` | Compiler controls passed at673. Original C3 rejection matches pinned OMC; it does not invalidate the issue's conforming variants. Three new typed-dimension controls are queued/unrun. |
| 425 | No original38 controlled gates at ca496; candidate6393 registers the two selectors below | Both memory and exact quiescence/native gates remain unrun. No memory-improvement or20/30/38 scaling claim. |
| 426 | Runtime `issue_426_original_inactive_neighbor_simulates_its_analytic_solution`; compiler `issue_426_inactive_neighbor_preserves_the_structured_family` | Runtime passed at1447; compiler passed at673. Refresh final source; active-invalid-index control is separate. |

The original public runtime cohort is exactly ten tests:403 has three controls,
411 has two, and405/418/422/423/426 each have one. Keep the new425 semantic
gate separate from that cohort when filtering after integration:

```sh
cargo test --locked --offline --jobs 4 -p rumoca \
  --no-default-features --features fmu-packaging --test suite_core \
  reported_bug_runtime::issue_ -- \
  --skip reported_bug_runtime::issue_425_ --nocapture --test-threads=4
```

This local cohort command must execute ten tests, zero ignored. The skip only
separates the independently required425 gate; ordinary workspace CI skips none.
Compiler module coverage is six tests at ca496 and nine after queued424 is
integrated. Its issue424 prefix alone selects four now and seven afterwards.

## Original418 JAX execution is wired

PR job `template-runtime-tests` has a JAX matrix member using ci-template-python.
It installs diffrax0.7.2/JAX0.9.2/NumPy2.4.3/SciPy1.17.1, activates that environment,
and runs `cargo xtask verify template-runtimes --backend jax --require-external-tools`.
The JAX group in `crates/xtask/src/verify_cmd.rs` selects `jax_` with feature
template-runtime-tests. That includes the exact original-model test:

```sh
cargo test --locked --offline --jobs 4 -p rumoca \
  --no-default-features --features template-runtime-tests \
  --test suite_template_runtime \
  backend_template_runtime_regression::jax_ode_target_handles_reported_explicit_algebraic_assignments \
  -- --exact --nocapture --test-threads=4
```

Exactly one test must execute. Its real Python/JAX subprocess cannot skip;
it imports the original B export, JITs RHS and checks forward/reverse AD.
The JAX group also selects the existing state-only control, selected-region
value/AD controls and actual workshop examples. Their passes cannot substitute
for the original B test's missing actual receipt. No wiring hole was found.

## Original422 GNU static PIE is wired

The Linux Test job explicitly invokes
`.github/scripts/qualify-issue422-static-pie.sh` under a600-second outer timeout,
with `if: runner.os == 'Linux' && !cancelled()`. Artifact upload uses always().
The helper builds two test-profile executables through cargo rustc, using
`+crt-static`, PIC and `-static-pie`; it binds the compiler/source/input hashes
and validates ELF64/x86-64/ET_DYN with no INTERP or NEEDED. It then executes:

- rumoca-exec-cranelift libtest:
  `emit::host_runtime::tests::issue_422_memcmp_relocation_needs_no_process_symbol_export`
  (exactly one test; process lookup prohibited).
- rumoca suite_core with no-default-features,fmu-packaging:
  `reported_bug_runtime::issue_422_original_array_function_runs_without_native_fallback`
  (exactly one test; original U4,11 native samples).

Both actual static artifacts and executions remain unrun. The earlier dynamic
memcmp test and U4 simulation are not static-PIE proof. No wiring hole was found.
The helper additionally requires U4 to retain at least one checked owner.
That matches actual1447 count1, but is a stricter acceptance condition than the
general original model: legitimate constant folding could yield count0 while
the independently paired memcmp test still proves relocation. This is a future
optimization fragility, not missing CI selection or a reason to add tests.

## Broader controls and queued gates

Broader411 suites are selected by ordinary workspace CI, separately from the
two original quotient tests:

- `reported_bug_runtime::main_dae_integer::`: five controls covering signed
  arithmetic, exact publication, divisor/overflow faults and iterator owners.
- `reported_bug_runtime::integer_quotient_comparison::`: three controls covering
  enclosing comparisons, numeric promotion and strict native/session ownership.
- `reported_bug_runtime::integer_call_comparison::`: three controls covering
  live descendant calls and assertion activation/provenance.

The enclosing comparison group passed at e690. The descendant-call group3,
call branch/assertions5, assertion scopes3 and message projection9 passed at
ca496. The owned comprehension and corrected supported-divisor control each
passed at4907. The structured outer-iterator positive still lacked an after
receipt at this audit; do not replace that gap with a scalar-only pass.
These facts do not constitute a fresh original-ten cohort pass at ca496.

Queued424 adds three compiler tests, all unrun:
`issue_424_parameter_extents_preserve_both_constructor_axis_orders`,
`issue_424_boolean_parameter_masks_preserve_axis_order_and_exact_values`, and
`issue_424_reported_boolean_binding_accepts_conforming_parameter_dimensions`.
Both parameter orders have independently specified exact matrix/mask values;
the ordinary Boolean control proves compilation/shape only. Its source does
not reuse the invalid original dimensions or require Boolean laziness.

Queued425's smallest commands each execute one test, zero ignored, sequentially:

```sh
cargo test --locked --offline --jobs 4 -p rumoca --test suite_core \
  process_allocator_address_space::issue_425_original_38_bench_fits_a_two_gib_address_space \
  -- --exact --nocapture
cargo test --locked --offline --jobs 4 -p rumoca --test suite_core \
  reported_bug_runtime::issue_425_original_38_preserves_quiescent_states_and_native_receipts \
  -- --exact --nocapture
```

Use defaults for both to reuse the actual CLI link. The semantic gate alone
can use no-default-features,fmu-packaging. The memory gate issues child
RLIMIT_AS2097152KiB/Rayon1 and the original sim bench options, checking11 points
and reported preparation timings. It is not peak RSS; retain an independent
GNU time/guardian receipt for that measurement. Semantic checks cover5776
states at11 times, exact quiescence and strict native receipts. Original30
source stays byte-identical; only nx/ny become38. The3346-byte derived source
SHA256 is `1bd65c874537a23acd43cde3d5d1879d84bf1b1c2affa685df1646d84e8c6a5d`.

At1447 native browser8/editor1/public-renderer1 passed. Tracked examples stopped
at the scratch floor before tests; the new ca496 example gate was still live
when this inventory was prepared. Final example/source qualification must use
its own terminal receipt. Open PR limit remains two; no publication or closure
is authorized by this inventory.
