//! Exact native VR access through the common accepted-input transaction.

use super::{MeSessionError, MeSessionLoss, MeSimulationSession};
use crate::fmi_me::{MeError, SolveMeKernel};

trait NativeSessionValue: Copy + Default {
    fn check(kernel: &SolveMeKernel, refs: &[u32], values: &[Self]) -> Result<(), MeError>;
    fn read(kernel: &SolveMeKernel, refs: &[u32], values: &mut [Self]) -> Result<(), MeError>;
    fn write(
        kernel: &mut crate::fmi_me::kernel::AcceptedInputWrite<'_>,
        refs: &[u32],
        values: &[Self],
    ) -> Result<(), MeError>;
    fn same(self, other: Self) -> bool;
}

macro_rules! native_value {
    ($scalar:ty, $check:ident, $set:ident, $get:ident, $same:expr) => {
        impl NativeSessionValue for $scalar {
            fn check(kernel: &SolveMeKernel, refs: &[u32], values: &[Self]) -> Result<(), MeError> {
                kernel.$check(refs, values)
            }
            fn read(
                kernel: &SolveMeKernel,
                refs: &[u32],
                values: &mut [Self],
            ) -> Result<(), MeError> {
                kernel.$get(refs, values)
            }
            fn write(
                kernel: &mut crate::fmi_me::kernel::AcceptedInputWrite<'_>,
                refs: &[u32],
                values: &[Self],
            ) -> Result<(), MeError> {
                kernel.$set(refs, values)
            }
            fn same(self, other: Self) -> bool {
                ($same)(self, other)
            }
        }
        impl MeSimulationSession<'_, '_> {
            pub fn $set(&mut self, refs: &[u32], values: &[$scalar]) -> Result<(), MeSessionError> {
                self.set_native_inputs(refs, values)
            }
            pub fn $get(&self, refs: &[u32], values: &mut [$scalar]) -> Result<(), MeSessionError> {
                self.require_live()?;
                Ok(self.host.kernel.borrow()?.$get(refs, values)?)
            }
        }
    };
}

native_value!(
    f64,
    check_native_float64_inputs,
    set_native_float64,
    get_native_float64,
    |before: f64, after: f64| before.to_bits() == after.to_bits()
);
native_value!(
    i64,
    check_native_integer_inputs,
    set_native_integer,
    get_native_integer,
    |before: i64, after: i64| before == after
);
native_value!(
    bool,
    check_boolean_inputs,
    set_boolean,
    get_boolean,
    |before: bool, after: bool| before == after
);

impl MeSimulationSession<'_, '_> {
    fn set_native_inputs<T: NativeSessionValue>(
        &mut self,
        refs: &[u32],
        values: &[T],
    ) -> Result<(), MeSessionError> {
        self.require_live()?;
        let unchanged = {
            let kernel = self.host.kernel.borrow()?;
            // Causality must be checked even when the proposed bits already match.
            T::check(&kernel, refs, values)?;
            let mut current = vec![T::default(); values.len()];
            T::read(&kernel, refs, &mut current)?;
            current
                .iter()
                .zip(values)
                .all(|(before, after)| before.same(*after))
        };
        if unchanged {
            return Ok(());
        }
        self.write_input_transaction(|kernel| T::write(kernel, refs, values))?;
        // The named-input map is reporting data, never native input authority.
        // It cannot retain stale scalar-convenience values after an exact VR write.
        self.host.inputs.clear();
        let correlated = self.correlate_input_write();
        self.host
            .guard_mutation(MeSessionLoss::InputApplication, correlated)
    }
}
