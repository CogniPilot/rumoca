//! Explicit Runge-Kutta implementations of Rumoca's Model Exchange integrator
//! interface: the adaptive Dormand-Prince 5(4) pair and the fixed-step methods
//! of order 1, 2 and 4.
//!
//! FMI lifecycle, event handling, trace recording, and session orchestration
//! are owned by the shared simulation host; this crate owns only the numerical
//! methods and their continuous extensions.

mod dense_output;
mod fixed_step;
mod me_integrator;

pub use fixed_step::{
    FixedStepConfig, FixedStepOptionsKey, FixedStepScheme, fixed_step_integrator,
};
pub use me_integrator::{model_exchange_integrator, model_exchange_integrator_with_observation};

/// The explicit adapter currently has no backend-specific knobs. Supplying
/// this concrete value still permits identity/type validation of a request.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct Rk45Options;
