//! High-level simulation facade for Rumoca.
//!
//! Re-exports the primitives crate `rumoca-sim-core` plus, when the
//! corresponding features are enabled, the diffsol/rk45 solver entry points
//! and the I/O `runner` module that drives lockstep simulations.

pub use rumoca_sim_core::*;

#[cfg(feature = "solver-diffsol")]
pub use rumoca_solver_diffsol::*;

#[cfg(feature = "solver-rk45")]
pub mod rk45 {
    pub use rumoca_solver_rk45::*;
}

#[cfg(feature = "runner")]
pub mod runner;

#[cfg(feature = "report")]
pub mod report;

#[cfg(all(feature = "viz", not(target_arch = "wasm32")))]
pub mod viz_web {
    pub use rumoca_viz_web::*;
}
