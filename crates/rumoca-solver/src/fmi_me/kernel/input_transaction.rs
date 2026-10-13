//! Borrowed write scope issued only inside the same kernel's rollback owner.

use super::*;

pub(in crate::fmi_me) struct AcceptedInputWrite<'kernel> {
    kernel: &'kernel mut SolveMeKernel,
}

impl SolveMeKernel {
    pub(in crate::fmi_me) fn with_input_rollback(
        &mut self,
        write: impl FnOnce(&mut AcceptedInputWrite<'_>) -> Result<(), MeError>,
    ) -> Result<(), (MeError, Result<(), MeError>)> {
        let checkpoint = self.fmu_state().map_err(|error| (error, Ok(())))?;
        let attempted = write(&mut AcceptedInputWrite { kernel: self });
        match attempted {
            Ok(()) => Ok(()),
            Err(error) => {
                #[cfg(test)]
                if FAIL_RESTORE.with(|fault| fault.replace(false)) {
                    return Err((error, Err(contract("injected input restoration failure"))));
                }
                Err((error, self.reset_to_fmu_state(&checkpoint)))
            }
        }
    }
}

impl AcceptedInputWrite<'_> {
    pub(in crate::fmi_me) fn position(
        &mut self,
        time: MeTime,
        states: &[f64],
        entry: MeEventEntry,
    ) -> Result<(), MeError> {
        self.kernel.set_time(time)?;
        self.kernel.set_continuous_states(states)?;
        self.kernel.enter_event_mode(entry)
    }

    pub(in crate::fmi_me) fn set_float64(
        &mut self,
        references: &[MeValueRef],
        values: &[f64],
    ) -> Result<(), MeError> {
        self.kernel.set_float64(references, values)
    }

    #[cfg(feature = "native-wasm")]
    pub(super) fn kernel(&mut self) -> &mut SolveMeKernel {
        self.kernel
    }

    #[cfg(feature = "native-wasm")]
    pub(in crate::fmi_me) fn set_native_integer(
        &mut self,
        references: &[u32],
        values: &[i64],
    ) -> Result<(), MeError> {
        self.kernel.set_native_integer(references, values)
    }

    #[cfg(feature = "native-wasm")]
    pub(in crate::fmi_me) fn set_boolean(
        &mut self,
        references: &[u32],
        values: &[bool],
    ) -> Result<(), MeError> {
        self.kernel.set_boolean(references, values)
    }
}

#[cfg(test)]
thread_local! {
    static SNAPSHOTS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
    static FAIL_RESTORE: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
}

#[cfg(test)]
pub(super) fn count_snapshot() {
    SNAPSHOTS.with(|count| count.set(count.get() + 1));
}

#[cfg(all(test, feature = "native-wasm"))]
pub(in crate::fmi_me) fn snapshot_count() -> usize {
    SNAPSHOTS.with(std::cell::Cell::get)
}

#[cfg(test)]
pub(in crate::fmi_me) fn fail_input_restore() {
    FAIL_RESTORE.with(|fault| fault.set(true));
}
