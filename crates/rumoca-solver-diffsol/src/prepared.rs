use std::cell::RefCell;

use rumoca_solver::{SimBackend, SimOptions, SimResult, fmi_me::session::MeRetainedComponent};

use crate::{SimError, run_prepared_simulation};

/// One checked model plus one retained FMI component reused across runs.
///
/// The component itself owns compiled evaluators; each run obtains the sole
/// mutable lease, restores the component's pristine snapshot, and constructs a
/// fresh numerical plugin. The `RefCell` preserves the historical `run(&self)`
/// API while turning an accidental overlapping lease into a typed error rather
/// than an aliasing path.
pub struct PreparedSimulation {
    pub(crate) opts: SimOptions,
    pub(crate) retained: RefCell<MeRetainedComponent>,
}

impl PreparedSimulation {
    pub(crate) fn new(opts: SimOptions, retained: MeRetainedComponent) -> Self {
        Self {
            opts,
            retained: RefCell::new(retained),
        }
    }

    pub fn backend(&self) -> SimBackend {
        SimBackend::Diffsol
    }

    pub fn run(&self) -> Result<SimResult, SimError> {
        run_prepared_simulation(self)
    }

    pub fn check_initialization(&self) -> Result<(), SimError> {
        crate::check_prepared_component(self)
    }
}
