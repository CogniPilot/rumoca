//! Portable model-context admission and exact input footprints.

#[cfg(test)]
mod tests;
use rumoca_ir_solve::VarLayout;

pub(super) fn execution_receipt(
    policy: rumoca_solver::SimExecutionPolicy,
    states: usize,
    external_tables: usize,
) -> rumoca_solver::SimExecutionReceipt {
    rumoca_solver::SimExecutionReceipt::admission(
        policy,
        states,
        external_tables,
        rumoca_solver::SimExecutionEngine::WasmProgram,
    )
}

pub(super) fn validate_inputs(
    layout: &VarLayout,
    y: usize,
    p: usize,
    external_tables: usize,
) -> Result<(), String> {
    InputExtent::of(layout).validate(y, p, external_tables)
}

/// The input lengths a product checks on each call. A product that owns its
/// module, and so shares no batch layout, keeps these counts, never a copy of
/// the layout.
#[derive(Clone, Copy)]
pub(super) struct InputExtent {
    y: usize,
    p: usize,
}

impl InputExtent {
    pub(super) fn of(layout: &VarLayout) -> Self {
        Self {
            y: layout.y_scalars(),
            p: layout.p_scalars(),
        }
    }

    pub(super) fn validate(self, y: usize, p: usize, external_tables: usize) -> Result<(), String> {
        if y != self.y || p != self.p {
            return Err("WASM input lengths differ from the issued layout".into());
        }
        if external_tables != 0 {
            return Err("WASM profile declines external tables".into());
        }
        Ok(())
    }
}
