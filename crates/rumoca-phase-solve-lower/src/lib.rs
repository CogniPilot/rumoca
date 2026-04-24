//! Lower DAE data into solver-facing IR.

use std::collections::HashMap;

use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

pub mod ad;
pub mod layout;
pub mod lower;

pub use ad::{lower_initial_residual_ad, lower_residual_ad};
pub use layout::build_var_layout;
pub use lower::{
    LowerError, LoweredExpression, lower_discrete_rhs, lower_expression,
    lower_expression_rows_from_expressions,
    lower_expression_rows_from_expressions_with_runtime_metadata,
    lower_initial_expression_rows_from_expressions,
    lower_initial_expression_rows_from_expressions_with_runtime_metadata, lower_initial_residual,
    lower_residual, lower_root_conditions,
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
    let name_to_idx = solver_names
        .iter()
        .enumerate()
        .map(|(idx, name)| (name.clone(), idx))
        .collect();
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
