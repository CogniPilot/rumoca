//! DAE-level tree-walk expression evaluator for Rumoca.
//!
//! Provides the interpreter, automatic differentiation dual numbers,
//! floating-point trait abstractions, and statement evaluators for
//! `rumoca_core::Expression` trees.

pub mod constant;
pub mod dual;
pub mod eval;
pub mod initial_assignments;
pub mod sim_float;
pub mod statement;
pub(crate) mod trace;

pub use eval::{
    EvalError, EvalRuntimeState, IMPLICIT_CLOCK_ACTIVE_ENV_KEY, INIT_HOMOTOPY_LAMBDA_KEY,
    MODELICA_COMPLEX_CONSTANTS, MODELICA_CONSTANTS, VarEnv, build_env, build_env_with_runtime,
    build_runtime_parameter_tail_env, build_runtime_parameter_tail_env_with_runtime,
    can_broadcast_start_value, clear_pre_values, clear_pre_values_in_env_runtime,
    clear_runtime_state, clear_runtime_state_in_env_runtime, collect_user_functions,
    collect_var_dims, collect_var_starts, deterministic_automatic_global_seed, eval_array_values,
    eval_condition_as_root, eval_const_expr, eval_expr,
    eval_function_call_pub_dae as eval_function_call_pub,
    eval_selected_function_output_pub_dae as eval_selected_function_output_pub, get_pre_value,
    get_pre_value_from_env, infer_clock_timing_seconds, is_runtime_special_function_name,
    is_runtime_special_function_short_name, lift_env, map_var_to_env, modelica_strings_hash_string,
    refresh_env_solver_and_parameter_values,
    resolve_function_call_outputs_pub_dae as resolve_function_call_outputs_pub, restore_pre_values,
    restore_pre_values_in_env_runtime, restore_pre_values_in_runtime, seed_pre_values_from_env,
    seed_pre_values_in_env_runtime, set_array_entries, set_pre_value, set_pre_value_in_env,
    set_pre_value_in_runtime, snapshot_pre_values, snapshot_pre_values_from_env,
    snapshot_pre_values_from_runtime, start_expr_is_nonnumeric, try_build_env,
    try_build_env_with_runtime,
    try_build_partial_runtime_parameter_tail_env_with_declared_slots_and_runtime,
    try_build_partial_runtime_parameter_tail_env_with_runtime,
    try_build_runtime_parameter_tail_env,
    try_build_runtime_parameter_tail_env_with_declared_slots_and_runtime,
    try_build_runtime_parameter_tail_env_with_runtime, try_refresh_env_solver_and_parameter_values,
};
pub use initial_assignments::{
    InitialAssignment, initial_assignment_from_equation, initial_assignments_from_equation,
};
