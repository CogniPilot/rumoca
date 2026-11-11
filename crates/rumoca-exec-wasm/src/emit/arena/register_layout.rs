//! Projection of the checked Solve IR root register layout.
use rumoca_ir_solve::{LinearOp, Reg, RootRegisterLayout};

#[derive(Debug)]
pub(in crate::emit) struct RegisterLayout(RootRegisterLayout);

impl RegisterLayout {
    pub(in crate::emit) fn derive(program: &[LinearOp]) -> Result<Self, String> {
        #[cfg(feature = "diagnostic-root-scalar-locals")]
        let layout = RootRegisterLayout::for_target(
            program,
            rumoca_ir_solve::RootRegisterTarget::scalar_locals(
                crate::typed_call::cost::ENGINE_LOCAL_LIMIT,
            ),
        )?;
        #[cfg(not(feature = "diagnostic-root-scalar-locals"))]
        let layout = RootRegisterLayout::derive(program)?;
        Ok(Self(layout))
    }

    pub(in crate::emit) fn extent(&self) -> usize {
        self.0.extent()
    }
    pub(in crate::emit) fn virtual_extent(&self) -> usize {
        self.0.virtual_extent()
    }
    pub(in crate::emit) fn physical(&self, register: Reg) -> Result<Reg, String> {
        self.0.physical(register)
    }
    #[cfg(feature = "diagnostic-root-scalar-locals")]
    pub(in crate::emit) fn scalar_local_count(&self) -> u32 {
        self.0.scalar_local_count()
    }
    #[cfg(feature = "diagnostic-root-scalar-locals")]
    pub(in crate::emit) fn scalar_local_slot(&self, register: Reg) -> Option<u32> {
        self.0.scalar_local_slot(register)
    }
}

#[cfg(test)]
mod tests;

#[cfg(all(test, feature = "diagnostic-root-scalar-locals"))]
mod local_projection_tests;
