//! Umbrella binary for the feature-independent `rumoca` integration suites.
//!
//! Every member below is an ordinary module under `tests/suite_core/`. They
//! used to be individual integration-test targets, whose repeated whole-
//! compiler links dominated memory and disk use. Grouping them preserves one
//! binary per feature set while retaining normal Rust module resolution.
//!
//! Where a new test file goes:
//! - needs a feature beyond the crate defaults -> the umbrella for that
//!   feature (`suite_galec_fmu`, `suite_template_runtime`, `suite_msl_sim`,
//!   `suite_examples_smoke`);
//! - a repo-inspection policy/budget check that links no `rumoca` library ->
//!   `suite_gates`, which stays a small binary so `xtask verify quick` can run
//!   the fast gates without linking the compiler;
//! - anything else -> a normal module below `tests/suite_core/`, declared at
//!   the top of this file.
//!
//! A module must NOT join an umbrella if it mutates process-global state
//! (`std::env::set_var`, `set_current_dir`, a process-wide singleton), because
//! libtest runs the tests of one binary as threads of one process. Such a file
//! stays a standalone `[[test]]` target with a comment saying why.
//!
//! Test paths gain the module prefix (`cli_emit::emit_modelica_stages_render`);
//! libtest filters are substring matches over the full path, so filtering by
//! bare test name still works, and `<file_stem>::` selects one member file.

mod history_operator_checked;

mod algorithm_parameter_range;
mod array_der_coupling_test;
mod array_subscript_test;
mod backend_executor_differential;
mod balance_diagnostic;
mod cli_diagnostic_rendering;
mod cli_emit;
mod cli_fmt_lint;
mod cli_target_acceptance;
mod clocked_coincident_exchange;
mod clocked_sample_regression;
mod connection_normalization_golden;
mod enumeration_compact_range_test;
mod fmi_me_host_divergence;
mod for_eq_array_test;
mod for_loop_element_derivative_alias;
mod forward_param_jacobian_test;
mod function_assertion_message_projection;
mod function_conditional_sequence_test;
mod function_inner_index_slice_compaction;
mod function_input_shadow_state_test;
mod function_loop_carried_record;
mod function_loop_carried_scalar;
mod function_loop_reduction_checked;
mod function_loop_snapshot_test;
mod function_noelse_if_test;
mod function_output_shadow_state_test;
mod function_projection_array_shape_test;
mod function_proven_branch_test;
mod function_quotient_sim;
mod function_record_array_test;
mod function_record_call_owner;
mod function_return_checked;
mod function_slice_compaction_rank_position;
mod function_spd_loop_compaction;
mod function_staged_record_update_test;
mod gear_loop_regression;
mod homotopy_branch_selection;
mod index_reduction_manifold;
mod initial_algorithm_test;
mod initial_value_alias_transfer;
mod initialization_ordering;
mod integer_builtin_checked;
mod interface_flow_balance;
mod jacobian_admission_battery;
mod jacobian_finite_difference;
mod jacobian_refusal_diagnostic;
mod jacobian_refused_forms;
mod jacobian_standard_modelica;
mod mixed_record_equation;
mod mlir_verification_wiring;
mod mod_propagation_test;
mod model_algorithm_continuous;
mod nested_class_shadowing_test;
mod nested_record_function_redeclaration;
mod neural_ode_tensor_solve_ir;
mod omc_differential_semantics;
mod overdetermined_connection_loop;
mod override_promoted_array_mask;
mod periodic_source_counter_regression;
mod pipeline_test;
mod prepared_vectors_refresh;
mod replaceable_function_redeclare;
mod required_tool_markers;
mod semi_linear_zero_flow;
// quadrotor_se23_regression_test.rs lives in `suite_heavy_solve`
// (required-features = ["heavy-solve-tests"]): until the compact Solve
// function-fold owner lands, its guards grind through ~139 MB of
// scalarized IR for minutes instead of failing fast.
mod record_array_member_slice_test;
mod record_connector_equation_test;
mod reverse_vjp_test;
mod scoped_import_flatten;
mod solve_model_round_trip;
mod state_select_enclosing_constant_test;
mod steady_adjoint_test;
mod steady_state_sensitivity_test;
mod structured_event_roots;
mod structured_family_corner_lowering;
mod terminate_when_regression;
mod tiered_models;
mod time_event_when_activation;
mod verification_surface_wiring;
