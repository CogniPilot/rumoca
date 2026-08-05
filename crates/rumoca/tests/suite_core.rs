//! Umbrella binary for the feature-independent `rumoca` integration suites.
//!
//! Every member below is an ordinary integration-test file that used to be its
//! own `[[test]]` target. Each such target statically links the whole compiler,
//! so ~60 of them made linking — not codegen — the dominant memory and disk
//! cost of `cargo test -p rumoca`, and the parallel links spiked peak RSS hard
//! enough to trip the OOM killer. Including the files as `#[path]` modules here
//! leaves every test file untouched in place while collapsing the link phase to
//! one binary per group (`Cargo.toml` sets `autotests = false` and lists the
//! umbrellas explicitly).
//!
//! Where a new test file goes:
//! - needs a feature beyond the crate defaults -> the umbrella for that
//!   feature (`suite_galec_fmu`, `suite_template_runtime`, `suite_msl_sim`,
//!   `suite_examples_smoke`);
//! - a repo-inspection policy/budget check that links no `rumoca` library ->
//!   `suite_gates`, which stays a small binary so `xtask verify quick` can run
//!   the fast gates without linking the compiler;
//! - anything else -> here.
//!
//! A file must NOT join an umbrella if it mutates process-global state
//! (`std::env::set_var`, `set_current_dir`, a process-wide singleton), because
//! libtest runs the tests of one binary as threads of one process. Such a file
//! stays a standalone `[[test]]` target with a comment saying why.
//!
//! Test paths gain the module prefix (`cli_emit::emit_modelica_stages_render`);
//! libtest filters are substring matches over the full path, so filtering by
//! bare test name still works, and `<file_stem>::` selects one member file.

// Shared helpers of the GALEC family, declared once for the whole binary so
// `cli_target_embedded_c_galec` and `galec_equivalence` share one copy
// (`clippy::duplicate_mod` rejects loading a file as a module twice, and a
// shared copy is what we want anyway). Every item is used by at least one
// member, keeping the zero-`allow` dead-code discipline intact.
#[path = "galec_cli_support/cc.rs"]
mod cc_support;
#[path = "galec_cli_support/cli.rs"]
mod cli_support;

#[path = "array_der_coupling_test.rs"]
mod array_der_coupling_test;
#[path = "array_subscript_test.rs"]
mod array_subscript_test;
#[path = "balance_diagnostic.rs"]
mod balance_diagnostic;
#[path = "cli_diagnostic_rendering.rs"]
mod cli_diagnostic_rendering;
#[path = "cli_emit.rs"]
mod cli_emit;
#[path = "cli_fmt_lint.rs"]
mod cli_fmt_lint;
#[path = "cli_target_embedded_c_galec.rs"]
mod cli_target_embedded_c_galec;
#[path = "clocked_sample_regression.rs"]
mod clocked_sample_regression;
#[path = "connection_normalization_golden.rs"]
mod connection_normalization_golden;
#[path = "enumeration_compact_range_test.rs"]
mod enumeration_compact_range_test;
#[path = "fmi_me_host_divergence.rs"]
mod fmi_me_host_divergence;
#[path = "for_eq_array_test.rs"]
mod for_eq_array_test;
#[path = "for_loop_element_derivative_alias.rs"]
mod for_loop_element_derivative_alias;
#[path = "forward_param_jacobian_test.rs"]
mod forward_param_jacobian_test;
#[path = "function_conditional_sequence_test.rs"]
mod function_conditional_sequence_test;
#[path = "function_input_shadow_state_test.rs"]
mod function_input_shadow_state_test;
#[path = "function_loop_reduction_checked.rs"]
mod function_loop_reduction_checked;
#[path = "function_loop_snapshot_test.rs"]
mod function_loop_snapshot_test;
#[path = "function_noelse_if_test.rs"]
mod function_noelse_if_test;
#[path = "function_output_shadow_state_test.rs"]
mod function_output_shadow_state_test;
#[path = "function_projection_array_shape_test.rs"]
mod function_projection_array_shape_test;
#[path = "function_proven_branch_test.rs"]
mod function_proven_branch_test;
#[path = "function_record_array_test.rs"]
mod function_record_array_test;
#[path = "function_return_checked.rs"]
mod function_return_checked;
#[path = "function_spd_loop_compaction.rs"]
mod function_spd_loop_compaction;
#[path = "function_staged_record_update_test.rs"]
mod function_staged_record_update_test;
#[path = "galec_equivalence.rs"]
mod galec_equivalence;
#[path = "gear_loop_regression.rs"]
mod gear_loop_regression;
#[path = "homotopy_branch_selection.rs"]
mod homotopy_branch_selection;
#[path = "index_reduction_manifold.rs"]
mod index_reduction_manifold;
#[path = "initial_algorithm_test.rs"]
mod initial_algorithm_test;
#[path = "initial_value_alias_transfer.rs"]
mod initial_value_alias_transfer;
#[path = "initialization_ordering.rs"]
mod initialization_ordering;
#[path = "integer_builtin_checked.rs"]
mod integer_builtin_checked;
#[path = "interface_flow_balance.rs"]
mod interface_flow_balance;
#[path = "mlir_verification_wiring.rs"]
mod mlir_verification_wiring;
#[path = "mod_propagation_test.rs"]
mod mod_propagation_test;
#[path = "model_algorithm_continuous.rs"]
mod model_algorithm_continuous;
#[path = "msl_table_regression.rs"]
mod msl_table_regression;
#[path = "nested_class_shadowing_test.rs"]
mod nested_class_shadowing_test;
#[path = "nested_record_function_redeclaration.rs"]
mod nested_record_function_redeclaration;
#[path = "neural_ode_tensor_solve_ir.rs"]
mod neural_ode_tensor_solve_ir;
#[path = "omc_differential_semantics.rs"]
mod omc_differential_semantics;
#[path = "overdetermined_connection_loop.rs"]
mod overdetermined_connection_loop;
#[path = "override_promoted_array_mask.rs"]
mod override_promoted_array_mask;
#[path = "periodic_source_counter_regression.rs"]
mod periodic_source_counter_regression;
#[path = "pipeline_test.rs"]
mod pipeline_test;
#[path = "prepared_vectors_refresh.rs"]
mod prepared_vectors_refresh;
#[path = "semi_linear_zero_flow.rs"]
mod semi_linear_zero_flow;
// quadrotor_se23_regression_test.rs lives in `suite_heavy_solve`
// (required-features = ["heavy-solve-tests"]): until the compact Solve
// function-fold owner lands, its guards grind through ~139 MB of
// scalarized IR for minutes instead of failing fast.
#[path = "record_array_member_slice_test.rs"]
mod record_array_member_slice_test;
#[path = "record_connector_equation_test.rs"]
mod record_connector_equation_test;
#[path = "reverse_vjp_test.rs"]
mod reverse_vjp_test;
#[path = "scoped_import_flatten.rs"]
mod scoped_import_flatten;
#[path = "solve_model_round_trip.rs"]
mod solve_model_round_trip;
#[path = "state_select_enclosing_constant_test.rs"]
mod state_select_enclosing_constant_test;
#[path = "steady_adjoint_test.rs"]
mod steady_adjoint_test;
#[path = "steady_state_sensitivity_test.rs"]
mod steady_state_sensitivity_test;
#[path = "structured_family_corner_lowering.rs"]
mod structured_family_corner_lowering;
#[path = "terminate_when_regression.rs"]
mod terminate_when_regression;
#[path = "tiered_models.rs"]
mod tiered_models;
#[path = "time_event_when_activation.rs"]
mod time_event_when_activation;
#[path = "verification_surface_wiring.rs"]
mod verification_surface_wiring;

// `support.rs` declares shared compile helpers and no tests of its own. It is
// `pub` here so the umbrella keeps compiling it exactly as its former
// standalone target did, without an unused-code exemption.
#[path = "support.rs"]
pub mod support;
