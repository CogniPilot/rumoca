//! Lower DAE data into solver-facing IR, plus the runtime that consumes it.
//!
//! Three concerns under one roof:
//!
//! - **Lowering passes** (`layout`, `lower`, `ad`): take a `dae::Dae` and produce
//!   `ir-solve` row IR — variable layout, residual rows, Jacobian-vector rows,
//!   discrete RHS, root conditions.
//! - **Tree-walk interpreter** (`eval`, `dual`, `sim_float`, `statement`): always
//!   available; evaluates `dae::Expression` trees directly during simulation,
//!   with `Dual<f64>`-based AD and pre-value snapshots.
//! - **JIT compilers** (`cranelift`, `wasm`, feature-gated): translate the row IR
//!   into machine code (cranelift) or wasm bytecode. JIT-emitted code calls
//!   back into the interpreter for table lookups and special functions.

use std::collections::HashMap;

use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

// Lowering passes (DAE → ir-solve rows).
pub mod ad;
pub mod layout;
pub mod lower;

// Runtime evaluator (interpreter + AD).
pub mod dual;
pub mod eval;
pub mod sim_float;
pub mod statement;

// JIT backends (feature-gated).
#[cfg(feature = "cranelift")]
pub mod cranelift;
#[cfg(feature = "wasm")]
pub mod wasm;

pub use ad::{lower_initial_residual_ad, lower_residual_ad};
pub use layout::{build_var_layout, build_var_layout_with_solver_len};
pub use lower::{
    LowerError, LoweredExpression, lower_discrete_rhs, lower_expression,
    lower_expression_rows_from_expressions,
    lower_expression_rows_from_expressions_with_runtime_metadata,
    lower_initial_expression_rows_from_expressions,
    lower_initial_expression_rows_from_expressions_with_runtime_metadata, lower_initial_residual,
    lower_residual, lower_root_conditions,
};

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

#[cfg(feature = "cranelift")]
pub use cranelift::{
    Backend, CompileError, CompiledExpressionRows, CompiledJacobianV, CompiledResidual,
    compile_discrete_rhs, compile_expression_row_block, compile_expressions,
    compile_expressions_with_solver_len, compile_initial_expressions,
    compile_initial_expressions_with_solver_len, compile_initial_jacobian_v,
    compile_initial_jacobian_v_with_solver_len, compile_initial_residual,
    compile_initial_residual_with_solver_len, compile_jacobian_row_block, compile_jacobian_v,
    compile_jacobian_v_with_solver_len, compile_residual, compile_residual_row_block,
    compile_residual_with_solver_len, compile_root_conditions,
    compile_root_conditions_with_solver_len,
};
#[cfg(feature = "wasm")]
pub use wasm::{
    CompiledExpressionRowsWasm, CompiledJacobianVWasm, CompiledResidualWasm, WasmCompileError,
    compile_discrete_rhs_wasm, compile_expression_row_block_wasm, compile_expressions_wasm,
    compile_initial_expressions_wasm, compile_initial_jacobian_v_wasm,
    compile_initial_residual_wasm, compile_jacobian_row_block_wasm, compile_jacobian_v_wasm,
    compile_residual_row_block_wasm, compile_residual_wasm, compile_root_conditions_wasm,
};

pub fn lower_solve_layout(dae_model: &dae::Dae, solver_len: usize) -> solve::SolveLayout {
    let parameter_count = scalar_count(dae_model.parameters.values());
    let input_scalar_names = collect_scalar_names(dae_model.inputs.iter());
    let discrete_real_scalar_names = collect_scalar_names(dae_model.discrete_reals.iter());
    let discrete_valued_scalar_names = collect_scalar_names(dae_model.discrete_valued.iter());
    let compiled_parameter_len = parameter_count
        + input_scalar_names.len()
        + discrete_real_scalar_names.len()
        + discrete_valued_scalar_names.len();

    solve::SolveLayout {
        solver_maps: build_solver_name_index_maps(dae_model, solver_len),
        parameter_count,
        compiled_parameter_len,
        input_scalar_names,
        discrete_real_scalar_names,
        discrete_valued_scalar_names,
    }
}

pub fn lower_solve_problem(dae_model: &dae::Dae) -> Result<solve::SolveProblem, LowerError> {
    let layout = build_var_layout(dae_model);
    let solver_len = layout.y_scalars();
    Ok(solve::SolveProblem {
        residual: solve::RowBlock::new(lower_residual(dae_model, &layout)?),
        jacobian_v: solve::RowBlock::new(lower_residual_ad(dae_model, &layout)?),
        initial_residual: solve::RowBlock::new(lower_initial_residual(dae_model, &layout)?),
        initial_jacobian_v: solve::RowBlock::new(lower_initial_residual_ad(dae_model, &layout)?),
        root_conditions: solve::RowBlock::new(lower_root_conditions(dae_model, &layout)?),
        discrete_rhs: solve::RowBlock::new(lower_discrete_rhs(dae_model, &layout)?),
        solve_layout: lower_solve_layout(dae_model, solver_len),
        layout,
    })
}

pub fn solver_vector_names(dae_model: &dae::Dae, n_total: usize) -> Vec<String> {
    lower_solve_layout(dae_model, n_total).solver_maps.names
}

pub fn build_solver_name_index_maps(
    dae_model: &dae::Dae,
    y_len: usize,
) -> solve::SolverNameIndexMaps {
    let solver_names = collect_solver_names(dae_model, y_len);
    let mut name_to_idx: HashMap<String, usize> = solver_names
        .iter()
        .enumerate()
        .map(|(idx, name)| (name.clone(), idx))
        .collect();
    insert_solver_name_aliases(dae_model, y_len, &mut name_to_idx);
    let mut base_to_indices: HashMap<String, Vec<usize>> = HashMap::new();
    for (idx, name) in solver_names.iter().enumerate() {
        let base = dae::component_base_name(name).unwrap_or_else(|| name.to_string());
        base_to_indices.entry(base).or_default().push(idx);
    }

    solve::SolverNameIndexMaps {
        names: solver_names,
        name_to_idx,
        base_to_indices,
    }
}

fn scalar_count<'a>(vars: impl Iterator<Item = &'a dae::Variable>) -> usize {
    vars.map(dae::Variable::size).sum()
}

fn var_scalar_names(name: &str, var: &dae::Variable) -> Vec<String> {
    let size = var.size();
    if size <= 1 {
        return vec![name.to_string()];
    }
    (1..=size).map(|idx| format!("{name}[{idx}]")).collect()
}

fn collect_scalar_names<'a>(
    vars: impl Iterator<Item = (&'a dae::VarName, &'a dae::Variable)>,
) -> Vec<String> {
    vars.flat_map(|(name, var)| var_scalar_names(name.as_str(), var))
        .collect()
}

fn collect_solver_names(dae_model: &dae::Dae, solver_len: usize) -> Vec<String> {
    let mut names = collect_scalar_names(
        dae_model
            .states
            .iter()
            .chain(dae_model.algebraics.iter())
            .chain(dae_model.outputs.iter()),
    );
    names.truncate(solver_len);
    names
}

fn insert_solver_name_aliases(
    dae_model: &dae::Dae,
    solver_len: usize,
    name_to_idx: &mut HashMap<String, usize>,
) {
    let mut offset = 0usize;
    for (name, var) in dae_model
        .states
        .iter()
        .chain(dae_model.algebraics.iter())
        .chain(dae_model.outputs.iter())
    {
        let size = var.size();
        if size == 0 {
            continue;
        }
        if offset >= solver_len {
            break;
        }

        let visible_size = size.min(solver_len - offset);
        if size > 1 {
            name_to_idx
                .entry(name.as_str().to_string())
                .or_insert(offset);
        }
        for flat_idx in 0..visible_size {
            if let Some(subs) = flat_index_to_subscripts(flat_idx, &var.dims)
                && subs.len() > 1
            {
                name_to_idx
                    .entry(format_subscript_key(name.as_str(), &subs))
                    .or_insert(offset + flat_idx);
            }
        }
        offset += size;
    }
}

fn flat_index_to_subscripts(flat_index: usize, dims: &[i64]) -> Option<Vec<usize>> {
    if dims.is_empty() {
        return None;
    }
    let mut dims_usize = Vec::with_capacity(dims.len());
    for &d in dims {
        let dim = usize::try_from(d).ok()?;
        if dim == 0 {
            return None;
        }
        dims_usize.push(dim);
    }

    let mut remainder = flat_index;
    let mut subs_rev = Vec::with_capacity(dims_usize.len());
    for dim in dims_usize.iter().rev().copied() {
        subs_rev.push((remainder % dim) + 1);
        remainder /= dim;
    }
    if remainder != 0 {
        return None;
    }
    subs_rev.reverse();
    Some(subs_rev)
}

fn format_subscript_key(name: &str, subs: &[usize]) -> String {
    let mut key = String::from(name);
    key.push('[');
    for (idx, sub) in subs.iter().enumerate() {
        if idx > 0 {
            key.push(',');
        }
        key.push_str(&sub.to_string());
    }
    key.push(']');
    key
}

#[cfg(test)]
mod tests {
    use super::*;

    fn array_var(name: &str, dims: &[i64]) -> dae::Variable {
        dae::Variable {
            name: dae::VarName::new(name),
            dims: dims.to_vec(),
            ..Default::default()
        }
    }

    #[test]
    fn solver_name_index_maps_include_matrix_aliases() {
        let mut dae_model = dae::Dae::default();
        dae_model
            .algebraics
            .insert(dae::VarName::new("M"), array_var("M", &[3, 4]));

        let maps = build_solver_name_index_maps(&dae_model, 12);

        assert_eq!(
            maps.names,
            (1..=12).map(|idx| format!("M[{idx}]")).collect::<Vec<_>>()
        );
        assert_eq!(maps.name_to_idx.get("M"), Some(&0));
        assert_eq!(maps.name_to_idx.get("M[1,1]"), Some(&0));
        assert_eq!(maps.name_to_idx.get("M[1,2]"), Some(&1));
        assert_eq!(maps.name_to_idx.get("M[2,1]"), Some(&4));
        assert_eq!(maps.name_to_idx.get("M[3,4]"), Some(&11));
        assert_eq!(
            solve::solver_idx_for_target("M[2,1]", &maps.name_to_idx),
            Some(4)
        );
        assert_eq!(
            maps.base_to_indices.get("M").cloned().unwrap_or_default(),
            (0..12).collect::<Vec<_>>()
        );
    }

    #[test]
    fn solver_name_index_aliases_respect_truncated_solver_len() {
        let mut dae_model = dae::Dae::default();
        dae_model
            .algebraics
            .insert(dae::VarName::new("M"), array_var("M", &[2, 2]));

        let maps = build_solver_name_index_maps(&dae_model, 3);

        assert_eq!(
            maps.names,
            vec!["M[1]".to_string(), "M[2]".to_string(), "M[3]".to_string()]
        );
        assert_eq!(maps.name_to_idx.get("M[1,1]"), Some(&0));
        assert_eq!(maps.name_to_idx.get("M[1,2]"), Some(&1));
        assert_eq!(maps.name_to_idx.get("M[2,1]"), Some(&2));
        assert_eq!(maps.name_to_idx.get("M[2,2]"), None);
    }
}
