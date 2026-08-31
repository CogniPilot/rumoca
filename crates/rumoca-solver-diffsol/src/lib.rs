//! Diffsol BDF implementation of Rumoca's Model Exchange integrator interface.
//!
//! FMI lifecycle, event handling, trace recording, preparation, and session
//! orchestration are owned by the shared simulation host; this crate owns only
//! Diffsol's numerical method state.

mod me_integrator;

use diffsol::{FaerSparseLU, FaerSparseMat, MatrixCommon};

type Matrix = FaerSparseMat<f64>;
type Vector = <Matrix as MatrixCommon>::V;
type Scalar = <Matrix as MatrixCommon>::T;
pub(crate) type LinearSolver = FaerSparseLU<f64>;

pub use me_integrator::model_exchange_integrator;
