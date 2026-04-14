//! Runtime evaluation over DAE expressions.

pub mod dual;
pub mod eval;
pub mod sim_float;
pub mod statement;

pub use eval::{
    IMPLICIT_CLOCK_ACTIVE_ENV_KEY, INIT_HOMOTOPY_LAMBDA_KEY, MODELICA_COMPLEX_CONSTANTS,
    MODELICA_CONSTANTS, VarEnv, build_env, build_runtime_parameter_tail_env, clear_pre_values,
    collect_user_functions, collect_var_dims, collect_var_starts,
    eval_array_values_dae as eval_array_values,
    eval_condition_as_root_dae as eval_condition_as_root, eval_const_expr_dae as eval_const_expr,
    eval_expr_dae as eval_expr, eval_function_call_pub_dae as eval_function_call_pub,
    eval_projected_function_output_pub_dae as eval_projected_function_output_pub, get_pre_value,
    infer_clock_timing_seconds_dae as infer_clock_timing_seconds, is_runtime_special_function_name,
    is_runtime_special_function_short_name, lift_env, map_var_to_env,
    refresh_env_solver_and_parameter_values,
    resolve_function_call_outputs_pub_dae as resolve_function_call_outputs_pub, restore_pre_values,
    seed_pre_values_from_env, set_array_entries, set_pre_value, snapshot_pre_values,
};
