//! Fixed-step linearly implicit Euler for stiff mechanical models.
//!
//! An opt-in alternative to the adaptive BDF session for models whose stiffness
//! comes from contact and damping, such as legged robots. Each step solves
//! `(I - h J) dx = h f(x)` with a Jacobian `J` that is reused for several steps
//! and refreshed after every reset.

mod linear;
mod me_integrator;
mod options;
#[cfg(feature = "sparse-linalg")]
mod sparse;

pub use me_integrator::{
    IterationMatrixSolver, VelImplicitConfig, last_iteration_matrix_solver,
    model_exchange_integrator,
};
pub use options::VelImplicitOptionsKey;
