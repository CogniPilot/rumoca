//! Diffsol BDF implementation of Rumoca's Model Exchange integrator interface.
//!
//! FMI lifecycle, event handling, trace recording, preparation, and session
//! orchestration are owned by the shared simulation host; this crate owns only
//! Diffsol's numerical method state.

// Diffsol problem closures are single-threaded here but require cloneable shared
// handles that live with the leaked solver problem.
// SPEC_0021: Exception - DiffSL owns a thread-confined context behind its shared handle.
#![allow(clippy::arc_with_non_send_sync)]

mod me_integrator;

use diffsol::{FaerSparseLU, FaerSparseMat, MatrixCommon};

type Matrix = FaerSparseMat<f64>;
type Vector = <Matrix as MatrixCommon>::V;
type Scalar = <Matrix as MatrixCommon>::T;
pub(crate) type LinearSolver = FaerSparseLU<f64>;

pub use me_integrator::model_exchange_integrator;
