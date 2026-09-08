use crate::RuntimeSolveError;

#[derive(Clone, Default)]
pub(super) struct StateDerivativeScratch {
    /// Full solver vector reconstructed from the state slots, reused across
    /// derivative and Jacobian evaluations to avoid per-call allocation.
    pub(super) solver_y: Vec<f64>,
    /// State-space probe direction expanded to a full solver-length seed, with
    /// the algebraic slots completed by the projection forward-sensitivity, for
    /// the AD Jacobian-vector product.
    pub(super) seed_buf: Vec<f64>,
    /// Scratch unit seed used to read a single residual row's diagonal
    /// sensitivity `∂g_row/∂y_target`; kept all-zero between uses.
    pub(super) unit_seed: Vec<f64>,
}

/// Tolerances for the algebraic projection's fixed-point settle (shared by the
/// value refresh and the seed/forward-sensitivity refresh).
#[derive(Debug, Clone, Copy)]
pub struct AlgebraicSettle {
    pub tol: f64,
    pub max_iters: usize,
}

/// Shared linearization context for the reconstruct-then-JVP entry points: the
/// evaluation time, the parameter vector, and the algebraic-settle tolerance
/// used to project algebraics from the state before linearizing. Bundling these
/// keeps the sensitivity entry points within the argument-count budget and threads
/// the same context through every layer without repetition.
#[derive(Debug, Clone, Copy)]
pub struct AlgebraicLinearization<'a> {
    pub t: f64,
    pub params: &'a [f64],
    pub settle: AlgebraicSettle,
}

/// Diagonal magnitude below which a seed residual row is treated as singular for
/// its paired target slot, matching the value refresh's residual-slope check.
pub(super) fn validate_derivative_output_len(
    out: &[f64],
    state_count: usize,
) -> Result<(), RuntimeSolveError> {
    if out.len() == state_count {
        return Ok(());
    }
    Err(RuntimeSolveError::solve_ir(format!(
        "state derivative output length {} does not match state count {}",
        out.len(),
        state_count
    )))
}
