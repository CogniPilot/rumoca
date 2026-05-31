use crate::compile::compile_derivative_rhs_with_opts;
use crate::compiled::CompiledMlirResidual;
use crate::error::MlirError;
use crate::options::MlirBackendOptions;
use rumoca_ir_solve::SolveModel;

/// An ODE whose derivative function has been JIT-compiled via the MLIR execution adapter.
///
/// Wraps a `SolveModel` together with the compiled `eval_derivative`
/// shared library.  Currently supports pure explicit ODE models (no algebraics).
pub struct CompiledOdeModel {
    compiled: CompiledMlirResidual,
    parameters: Vec<f64>,
    state_count: usize,
    pub initial_y: Vec<f64>,
    pub visible_names: Vec<String>,
}

impl CompiledOdeModel {
    /// Evaluate `xdot = f(y, p, t)` using the MLIR-compiled function.
    pub fn eval_state_derivatives(&self, t: f64, y: &[f64]) -> Result<Vec<f64>, MlirError> {
        let mut out = vec![0.0; self.state_count];
        self.compiled.call(y, &self.parameters, t, &mut out)?;
        Ok(out)
    }

    pub fn state_count(&self) -> usize {
        self.state_count
    }
}

/// Compile the `derivative_rhs` of `model` via the MLIR toolchain with default options.
///
/// Requires `mlir-opt-18`, `mlir-translate-18`, `llc-18`, `clang-18` on `$PATH`.
pub fn build_ode_model(
    model: &SolveModel,
    model_name: &str,
) -> Result<CompiledOdeModel, MlirError> {
    build_ode_model_with_opts(model, model_name, &MlirBackendOptions::default())
}

/// Compile the `derivative_rhs` of `model` with explicit backend options.
pub fn build_ode_model_with_opts(
    model: &SolveModel,
    model_name: &str,
    opts: &MlirBackendOptions,
) -> Result<CompiledOdeModel, MlirError> {
    let compiled =
        compile_derivative_rhs_with_opts(&model.problem, &model.artifacts, model_name, opts)?;
    Ok(CompiledOdeModel {
        compiled,
        parameters: model.parameters.clone(),
        state_count: model.state_scalar_count(),
        initial_y: model.initial_y.clone(),
        visible_names: model.visible_names.clone(),
    })
}
