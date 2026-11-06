//! Retain the exact source borrow and native owner behind the solver interface.
use std::cell::RefCell;
use std::rc::Rc;

use rumoca_eval_solve::model_jacobian::{ModelStateJacobian, ModelStateJacobianError};
use rumoca_exec_cranelift::CompiledModelStateJacobian;
use rumoca_ir_solve::SolveExecutionSource;
use rumoca_solver::{
    CompiledSolveModelStateJacobian, ModelStateJacobianPreparation, RuntimeSolveError,
};
#[cfg(test)]
mod tests;

self_cell::self_cell! {
    struct RetainedNativeJacobian {
        owner: SolveExecutionSource,
        #[not_covariant]
        dependent: CompiledModelStateJacobian,
    }
}

enum PreparationError {
    Source(ModelStateJacobianError),
    Native(rumoca_exec_cranelift::CompileError),
}

struct NativeJacobian(RefCell<RetainedNativeJacobian>);

impl CompiledSolveModelStateJacobian for NativeJacobian {
    fn call(
        &self,
        state: &[f64],
        parameters: &[f64],
        time: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        #[cfg(test)]
        tests::record_call();
        self.0
            .borrow_mut()
            .with_dependent_mut(|_, code| code.eval(state, parameters, time, out))
            .map_err(super::compiled_execution_error)
    }
}

pub(super) fn prepare(
    source: SolveExecutionSource,
) -> Result<ModelStateJacobianPreparation, String> {
    #[cfg(test)]
    tests::record_preparation();
    let target = selected_profile()?;
    let result = RetainedNativeJacobian::try_new(source, |source| {
        let graph = ModelStateJacobian::construct_for_target(source.model(), &target)
            .map_err(PreparationError::Source)?;
        CompiledModelStateJacobian::compile(graph).map_err(PreparationError::Native)
    });
    match result {
        Ok(cell) => Ok(ModelStateJacobianPreparation::Compiled(Rc::new(
            NativeJacobian(RefCell::new(cell)),
        ))),
        Err(PreparationError::Source(ModelStateJacobianError::Unsupported { reason, .. })) => {
            Ok(ModelStateJacobianPreparation::Unsupported {
                reason: reason.into(),
            })
        }
        Err(PreparationError::Source(error)) => Err(error.to_string()),
        Err(PreparationError::Native(error)) => Err(error.to_string()),
    }
}

/// The host selects its declared target before construction specializes the
/// final product. The native emitter receives the issued product only.
fn selected_profile() -> Result<rumoca_ir_solve::SolveTargetProfile, String> {
    profile_from_target(include_str!("../../targets/cranelift/target.toml"))
}

fn profile_from_target(source: &str) -> Result<rumoca_ir_solve::SolveTargetProfile, String> {
    #[derive(serde::Deserialize)]
    #[serde(deny_unknown_fields)]
    struct Target {
        version: u32,
        ir: String,
        name: String,
        specialization: rumoca_ir_solve::SolveTargetProfile,
    }
    let target: Target = toml::from_str(source)
        .map_err(|error| format!("invalid in-process target specialization: {error}"))?;
    if target.version != 1 || target.ir != "solve" || target.name != target.specialization.name() {
        return Err("invalid in-process target specialization identity".into());
    }
    Ok(target.specialization)
}
