//! State-space linearization `x' = A x + B u`, `y = C x + D u` at an operating
//! point, assembled from the Solve directional-derivative rows.
//!
//! `A` is the state Jacobian the Jacobian inspector reports. `B` is the
//! derivative rows differentiated along each declared input's parameter slot,
//! so the path through the algebraic projection is included. `C` and `D`
//! differentiate each declared output along the states and the inputs through
//! the same projection tangent. Every entry is an automatic directional
//! derivative of the lowered rows; each is exact.

use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;
use rumoca_solver::{AlgebraicLinearization, SimOptions, SolveRuntime};

use super::diagnostics::SimulationDiagnosticError;
use super::overrides::{
    input_scalar_names, inputs_without_default, lower_for_simulation_with_overrides,
};
use super::probe::{EVAL_AT_REFRESH_MAX_ITERS, EVAL_AT_REFRESH_TOL, resolve_probe_state};

/// A linearized model with named axes.
#[derive(Debug, Clone)]
pub struct Linearization {
    pub t: f64,
    pub states: Vec<String>,
    pub inputs: Vec<String>,
    pub outputs: Vec<String>,
    /// Operating state, aligned with `states`.
    pub state_values: Vec<f64>,
    /// Operating input values, aligned with `inputs`.
    pub input_values: Vec<f64>,
    /// `a[row][col] = ∂(der(state_row))/∂(state_col)`.
    pub a: Vec<Vec<f64>>,
    /// `b[row][col] = ∂(der(state_row))/∂(input_col)`.
    pub b: Vec<Vec<f64>>,
    /// `c[row][col] = ∂(output_row)/∂(state_col)`.
    pub c: Vec<Vec<f64>>,
    /// `d[row][col] = ∂(output_row)/∂(input_col)`.
    pub d: Vec<Vec<f64>>,
}

/// Lower `model` and linearize it at the named state overrides and time `t`.
pub fn linearization_for_dae(
    model: &dae::Dae,
    opts: &SimOptions,
    state_overrides: &[(String, f64)],
    t: f64,
) -> Result<Linearization, SimulationDiagnosticError> {
    // A named input fixes the operating input; every other name is a state.
    let input_names = input_scalar_names(model);
    let (input_overrides, state_overrides): (Vec<_>, Vec<_>) = state_overrides
        .iter()
        .cloned()
        .partition(|(name, _)| input_names.contains(name));
    // An input with no default has no operating value until one is named.
    let missing: Vec<String> = inputs_without_default(model)
        .into_iter()
        .filter(|name| !input_overrides.iter().any(|(given, _)| given == name))
        .collect();
    if !missing.is_empty() {
        return Err(SimulationDiagnosticError::InvalidOverride {
            message: format!(
                "the operating point needs a value for input `{}`: give it with `--at {}=<value>`",
                missing[0], missing[0]
            ),
        });
    }
    let opts = SimOptions {
        initial_inputs: input_overrides,
        ..opts.clone()
    };
    let solve_model = lower_for_simulation_with_overrides(model, &opts)?;
    let (state, states) =
        resolve_probe_state(&solve_model, &state_overrides, "--inspect linearize --at")?;
    let runtime = SolveRuntime::new(&solve_model).map_err(SimulationDiagnosticError::from)?;
    let settle = rumoca_solver::AlgebraicSettle {
        tol: EVAL_AT_REFRESH_TOL,
        max_iters: EVAL_AT_REFRESH_MAX_ITERS,
    };
    let params = &solve_model.parameters;
    let lin = AlgebraicLinearization { t, params, settle };

    let jacobian = runtime.eval_state_jacobian(t, &state, params, settle);
    if let Some(error) = jacobian.error {
        return Err(SimulationDiagnosticError::Solver(error));
    }
    let layout = &solve_model.problem.solve_layout;
    let inputs: Vec<(String, usize)> = layout
        .input_scalar_names()
        .iter()
        .filter_map(|name| Some((name.clone(), layout.input_parameter_index(name)?)))
        .collect();
    let n = states.len();
    let b = input_columns(&runtime, &solve_model, lin, &state, &inputs)?;
    let outputs = output_slots(&solve_model)?;
    let (c, d) = output_rows(&runtime, lin, &state, &inputs, &outputs)?;
    Ok(Linearization {
        t,
        states,
        inputs: inputs.iter().map(|(name, _)| name.clone()).collect(),
        outputs: outputs.iter().map(|(name, _)| name.clone()).collect(),
        state_values: state,
        input_values: inputs.iter().map(|(_, slot)| params[*slot]).collect(),
        a: jacobian.matrix,
        b: transpose(&b, n),
        c,
        d,
    })
}

fn transpose(columns: &[Vec<f64>], rows: usize) -> Vec<Vec<f64>> {
    (0..rows)
        .map(|row| columns.iter().map(|column| column[row]).collect())
        .collect()
}

/// `∂(der)/∂u_j` for each input, as columns.
fn input_columns(
    runtime: &SolveRuntime,
    model: &solve::SolveModel,
    lin: AlgebraicLinearization<'_>,
    state: &[f64],
    inputs: &[(String, usize)],
) -> Result<Vec<Vec<f64>>, SimulationDiagnosticError> {
    let layout = &model.problem.layout;
    let y_scalars = layout.y_scalars();
    let mut seed = vec![0.0; y_scalars + layout.p_scalars()];
    let mut columns = Vec::with_capacity(inputs.len());
    for (_, slot) in inputs {
        seed[y_scalars + slot] = 1.0;
        let mut column = vec![0.0; state.len()];
        let outcome = runtime.eval_full_jacobian_v_ad_into(lin, state, &seed, &mut column);
        seed[y_scalars + slot] = 0.0;
        outcome.map_err(SimulationDiagnosticError::from)?;
        columns.push(column);
    }
    Ok(columns)
}

/// Where each declared output lives: a solver variable, or an input slot it
/// aliases. An output with no runtime slot was folded to a constant, whose
/// derivative is zero.
#[derive(Clone, Copy)]
enum OutputSlot {
    Solver(usize),
    Parameter(usize),
    Constant,
}

fn output_slots(
    model: &solve::SolveModel,
) -> Result<Vec<(String, OutputSlot)>, SimulationDiagnosticError> {
    model
        .variable_meta
        .iter()
        .filter(|meta| meta.role == "output")
        .map(|meta| {
            let slot = match model.problem.layout.binding(&meta.name) {
                Some(solve::ScalarSlot::Y { index, .. }) => OutputSlot::Solver(index),
                Some(solve::ScalarSlot::P { index, .. }) => OutputSlot::Parameter(index),
                Some(solve::ScalarSlot::Constant(_)) => OutputSlot::Constant,
                Some(solve::ScalarSlot::Time) | None => {
                    return Err(SimulationDiagnosticError::RuntimePreparation {
                        message: format!("output `{}` has no checked storage slot", meta.name),
                        span: None,
                    });
                }
            };
            Ok((meta.name.clone(), slot))
        })
        .collect()
}

/// The output Jacobians `(C, D)`.
type OutputMatrices = (Vec<Vec<f64>>, Vec<Vec<f64>>);

/// `C` and `D`: each output differentiated along every state and every input
/// through the algebraic projection's tangent.
fn output_rows(
    runtime: &SolveRuntime,
    lin: AlgebraicLinearization<'_>,
    state: &[f64],
    inputs: &[(String, usize)],
    outputs: &[(String, OutputSlot)],
) -> Result<OutputMatrices, SimulationDiagnosticError> {
    let n = state.len();
    let mut tangent = vec![0.0; runtime.solver_count];
    let mut c = vec![vec![0.0; n]; outputs.len()];
    let mut d = vec![vec![0.0; inputs.len()]; outputs.len()];
    let mut direction = vec![0.0; n];
    for col in 0..n {
        direction[col] = 1.0;
        let outcome =
            runtime.project_state_tangent_to_solver_y(lin, state, &direction, None, &mut tangent);
        direction[col] = 0.0;
        outcome.map_err(SimulationDiagnosticError::from)?;
        for (row, (_, slot)) in outputs.iter().enumerate() {
            if let OutputSlot::Solver(index) = slot {
                c[row][col] = tangent[*index];
            }
        }
    }
    for (col, (_, input_slot)) in inputs.iter().enumerate() {
        runtime
            .project_state_tangent_to_solver_y(
                lin,
                state,
                &direction,
                Some(*input_slot),
                &mut tangent,
            )
            .map_err(SimulationDiagnosticError::from)?;
        for (row, (_, slot)) in outputs.iter().enumerate() {
            d[row][col] = match slot {
                OutputSlot::Solver(index) => tangent[*index],
                OutputSlot::Parameter(index) if index == input_slot => 1.0,
                OutputSlot::Parameter(_) | OutputSlot::Constant => 0.0,
            };
        }
    }
    Ok((c, d))
}
