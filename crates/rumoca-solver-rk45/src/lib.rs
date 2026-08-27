//! Dormand-Prince 5(4) implementation of Rumoca's Model Exchange integrator interface.
//!
//! FMI lifecycle, event handling, trace recording, and session orchestration
//! are owned by the shared simulation host; this crate owns only the numerical
//! method and its continuous extension.

mod dense_output;
mod me_integrator;

pub use me_integrator::model_exchange_integrator;
