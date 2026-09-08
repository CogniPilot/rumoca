use crate::compile::compile_derivative_rhs_with_opts;
use crate::compiled::CompiledMlirResidual;
use crate::error::MlirError;
use crate::options::MlirBackendOptions;
use rumoca_ir_solve::{
    PureExplicitLayoutDisposition, PureExplicitStateCount, SolveLayout, SolveModel,
};
use std::sync::Arc;

/// An ODE whose derivative function has been JIT-compiled via the MLIR execution adapter.
///
/// Wraps a `SolveModel` together with the compiled `eval_derivative`
/// shared library.  Currently supports pure explicit ODE models (no algebraics).
pub struct CompiledOdeModel {
    compiled: CompiledMlirResidual,
    parameters: Vec<f64>,
    pure_explicit_state: PureExplicitStateCount,
    pub initial_y: Vec<f64>,
    pub visible_names: Vec<String>,
}

impl CompiledOdeModel {
    /// Evaluate `xdot = f(y, p, t)` using the MLIR-compiled function.
    pub fn eval_state_derivatives(&self, t: f64, y: &[f64]) -> Result<Vec<f64>, MlirError> {
        let mut out = vec![0.0; self.pure_explicit_state.get()];
        self.compiled.call(y, &self.parameters, t, &mut out)?;
        Ok(out)
    }

    pub fn state_count(&self) -> usize {
        self.pure_explicit_state.get()
    }
}

/// Compile the `derivative_rhs` of `model` via the MLIR toolchain with default options.
///
/// Requires `mlir-opt-18`, `mlir-translate-18`, `llc-18`, `clang-18` on `$PATH`.
pub fn build_ode_model(
    model: Arc<SolveModel>,
    model_name: &str,
) -> Result<CompiledOdeModel, MlirError> {
    build_ode_model_with_opts(model, model_name, &MlirBackendOptions::default())
}

/// Compile the `derivative_rhs` of `model` with explicit backend options.
pub fn build_ode_model_with_opts(
    model: Arc<SolveModel>,
    model_name: &str,
    opts: &MlirBackendOptions,
) -> Result<CompiledOdeModel, MlirError> {
    let pure_explicit_state = require_ode_layout(model.problem().solve_layout())?;
    let compiled = compile_derivative_rhs_with_opts(Arc::clone(&model), model_name, opts)?;
    Ok(CompiledOdeModel {
        compiled,
        parameters: model.parameters().to_vec(),
        pure_explicit_state,
        initial_y: model.initial_y().to_vec(),
        visible_names: model.visible_names().map(str::to_string).collect(),
    })
}

fn require_ode_layout(layout: &SolveLayout) -> Result<PureExplicitStateCount, MlirError> {
    match layout.pure_explicit_state_disposition() {
        PureExplicitLayoutDisposition::Supported(capability) => Ok(capability),
        PureExplicitLayoutDisposition::Unsupported(diagnostic) => Err(MlirError::InvalidInput {
            operation: "build_ode_model",
            message: format!(
                "ODE integration requires a positive pure explicit state-only Y layout: {diagnostic}"
            ),
        }),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ode_layout_accepts_only_positive_pure_explicit_state_storage() {
        let layout = |state, algebraic, output, y| SolveLayout {
            state_scalar_count: state,
            algebraic_scalar_count: algebraic,
            output_scalar_count: output,
            solver_maps: rumoca_ir_solve::SolverNameIndexMaps {
                names: (0..y).map(|index| format!("y{index}")).collect(),
                ..Default::default()
            },
            ..Default::default()
        };
        require_ode_layout(&layout(2, 0, 0, 2))
            .expect("positive pure explicit layout is supported");
        for (state, algebraic, output, y) in
            [(0, 0, 0, 0), (1, 1, 0, 2), (1, 0, 1, 2), (1, 0, 0, 2)]
        {
            let error = require_ode_layout(&layout(state, algebraic, output, y))
                .expect_err("unsupported integration layout must fail before compilation");
            assert!(matches!(
                error,
                MlirError::InvalidInput {
                    operation: "build_ode_model",
                    ..
                }
            ));
        }
    }
}
