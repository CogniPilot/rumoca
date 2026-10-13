//! Closed preparation owners for one immutable runtime source.

use super::*;

#[derive(Clone)]
pub(super) enum RuntimeExecutionInventory {
    Scalar(Box<ScalarExecutionInventory>),
    #[cfg(feature = "native-wasm")]
    Native(rumoca_exec_wasm::RetainedNativeWasmHost),
}

/// Borrowed scalar evaluator issued only from the scalar inventory.
/// Its private fields keep infallible numerical helpers behind admission.
#[derive(Clone, Copy)]
pub struct ScalarExecutionView<'runtime> {
    pub(super) runtime: &'runtime SolveRuntime,
    pub(super) scalar: &'runtime ScalarExecutionInventory,
}

impl std::ops::Deref for ScalarExecutionView<'_> {
    type Target = SolveRuntime;

    fn deref(&self) -> &Self::Target {
        self.runtime
    }
}

impl SolveRuntime {
    /// Admit the existing scalar evaluator before any scalar-only reader.
    pub fn scalar_execution(&self) -> Result<ScalarExecutionView<'_>, RuntimeSolveError> {
        match &self.execution {
            RuntimeExecutionInventory::Scalar(scalar) => Ok(ScalarExecutionView {
                runtime: self,
                scalar,
            }),
            #[cfg(feature = "native-wasm")]
            RuntimeExecutionInventory::Native(_) => Err(RuntimeSolveError::UnsupportedModel {
                reason: "prepared native execution has no scalar evaluation inventory".into(),
            }),
        }
    }

    pub(super) fn scalar_backend(&self) -> Option<&dyn SolveExecutionBackend> {
        match &self.execution {
            RuntimeExecutionInventory::Scalar(scalar) => scalar.execution_backend.as_deref(),
            #[cfg(feature = "native-wasm")]
            RuntimeExecutionInventory::Native(_) => None,
        }
    }

    pub(super) fn scalar_evaluation(&self) -> Result<ScalarExecutionView<'_>, EvalSolveError> {
        self.scalar_execution()
            .map_err(|error| EvalSolveError::Scalarization {
                message: error.to_string(),
                span: None,
            })
    }

    /// Correlate the requested instance coordinate with both exact retained owners.
    /// Source identity alone does not distinguish scheduleless instance anchors.
    #[cfg(feature = "native-wasm")]
    pub fn new_with_native_owner(
        model: SolveRuntimeModel,
        host: rumoca_exec_wasm::RetainedNativeWasmHost,
        start_time: f64,
    ) -> Result<Self, RuntimeSolveError> {
        let root =
            model
                .retained_assertion_root()
                .ok_or_else(|| RuntimeSolveError::UnsupportedModel {
                    reason: "native execution requires the original correlated component".into(),
                })?;
        let prepared = host.product().inputs().root();
        if !root.shares_root_owner(prepared) || !std::ptr::eq(root.metadata(), prepared.metadata())
        {
            return Err(RuntimeSolveError::UnsupportedModel {
                reason: "prepared native entry belongs to another component source".into(),
            });
        }
        let requested = Some(start_time.to_bits());
        if !start_time.is_finite()
            || root.resolved_start_time().map(f64::to_bits) != requested
            || prepared.resolved_start_time().map(f64::to_bits) != requested
        {
            return Err(RuntimeSolveError::UnsupportedModel {
                reason: "native execution startTime differs from its original resolved anchors"
                    .into(),
            });
        }
        if let Some(authority) = model.artifacts.continuous.structural.checked_assignment() {
            authority.check_source(&model.problem).map_err(|error| {
                RuntimeSolveError::UnsupportedModel {
                    reason: error.to_string(),
                }
            })?;
        }
        let delay = DelayRuntime::new(&model.problem.events.delays)?;
        let roots = total_root_condition_count(&model, delay.event_root_count())?;
        Ok(Self::with_inventory(
            model,
            RuntimeExecutionInventory::Native(host),
            delay,
            roots,
        ))
    }

    #[cfg(feature = "native-wasm")]
    pub(crate) fn native_host(&self) -> Option<&rumoca_exec_wasm::RetainedNativeWasmHost> {
        match &self.execution {
            RuntimeExecutionInventory::Native(host) => Some(host),
            RuntimeExecutionInventory::Scalar(_) => None,
        }
    }

    pub(super) fn with_inventory(
        model: SolveRuntimeModel,
        execution: RuntimeExecutionInventory,
        delay_runtime: DelayRuntime,
        root_condition_count: usize,
    ) -> Self {
        Self {
            state_count: model.state_scalar_count(),
            solver_count: model.solver_scalar_count(),
            runtime_state: solve_eval::SimulationRuntimeState::new(),
            delay_runtime,
            root_condition_count,
            warning_log: RefCell::new(warnings::WarningLog::default()),
            clock_activation_cache: RefCell::new(ClockActivationCache::default()),
            model,
            execution,
        }
    }
}

#[cfg(test)]
mod tests;
