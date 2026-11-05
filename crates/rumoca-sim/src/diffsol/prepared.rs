use super::{PreparedSimulation, SimError, check_prepared_component, simulate_prepared};

impl PreparedSimulation {
    /// The execution engine admitted for this exact prepared component.
    /// Missing historical metadata remains absent rather than inferred.
    pub fn execution_receipt(&self) -> Option<rumoca_solver::SimExecutionReceipt> {
        self.execution.clone()
    }

    pub fn backend(&self) -> rumoca_solver::SimBackend {
        rumoca_solver::SimBackend::Diffsol
    }

    pub fn run(&self) -> Result<rumoca_solver::SimResult, SimError> {
        simulate_prepared(self)
    }

    pub fn check_initialization(&self) -> Result<(), SimError> {
        check_prepared_component(self)
    }
}
