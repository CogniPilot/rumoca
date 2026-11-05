//! Mechanical native consumption of an issued full-model explicit-ODE graph.
use rumoca_eval_solve::model_jacobian::ModelStateJacobian;
use rumoca_ir_solve::SolveModel;

use crate::{CompileError, CompiledCheckedF64Call};

/// One full state Jacobian entry compiled from the model's canonical source.
/// The source remains borrowed; the backend selects no derivative formulas.
pub struct CompiledModelStateJacobian<'source> {
    product: ModelStateJacobian<'source>,
    code: CompiledCheckedF64Call,
}

impl<'source> CompiledModelStateJacobian<'source> {
    /// Construct and compile once. Any unsupported source/native owner refuses;
    /// there is no interpreted fallback on construction or call failure.
    pub fn prepare(model: &'source SolveModel) -> Result<Self, CompileError> {
        let product = ModelStateJacobian::construct(model)
            .map_err(|error| CompileError::Input(error.to_string()))?;
        Self::compile(product)
    }

    /// Compile only the already-issued source graph. Keeping this boundary
    /// separate lets callers measure symbolic construction and native
    /// preparation independently without replacing either input product.
    pub fn compile(product: ModelStateJacobian<'source>) -> Result<Self, CompileError> {
        let code = CompiledCheckedF64Call::compile(
            product.table(),
            product.call_site(),
            &[product.states(), product.parameters(), 1],
        )?;
        Ok(Self { product, code })
    }

    pub fn states(&self) -> usize {
        self.product.states()
    }
    pub fn parameters(&self) -> usize {
        self.product.parameters()
    }
    pub fn state_labels(&self) -> &[String] {
        self.product.state_labels()
    }
    pub fn source_product(&self) -> &ModelStateJacobian<'source> {
        &self.product
    }

    /// Dense row-major output, in the canonical solver-state order on BOTH axes.
    /// Borrows inputs; explicit IR guards precede a success-only staged commit.
    pub fn eval(
        &mut self,
        state: &[f64],
        parameters: &[f64],
        time: f64,
        out: &mut [f64],
    ) -> Result<(), CompileError> {
        let n = self.product.states();
        if state.len() != n || parameters.len() != self.product.parameters() || out.len() != n * n {
            return Err(CompileError::Input(
                "full-model Jacobian input/output extent mismatch".into(),
            ));
        }
        self.code
            .call(&[state, parameters, std::slice::from_ref(&time)], out)
    }
}
