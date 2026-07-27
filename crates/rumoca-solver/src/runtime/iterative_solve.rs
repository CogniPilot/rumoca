//! Matrix-free iterative linear solve for the steady-state adjoint.
//!
//! Restarted GMRES for a general (nonsymmetric) operator `A`, accessed only
//! through matrix-vector products `v ↦ A·v`. The steady adjoint uses it with the
//! reverse VJP as the `A = (∂f/∂y)ᵀ` operator, so the dense operator is never
//! formed — the win at grid scale, where `(∂f/∂y)ᵀ` is sparse and GMRES converges
//! in far fewer iterations than `n`.
//!
//! GMRES is exact in at most `n` iterations in exact arithmetic, so with the
//! restart length `>= n` the small steady systems solve to machine precision in
//! one cycle; the restart bounds the Krylov basis memory for large `n`.

/// Failure modes of a matrix-free solve. Reported (never silently swallowed) so
/// the caller fails loudly rather than returning a wrong adjoint. Generic over
/// the operator's own error `E`, so an operator (e.g. a reverse-VJP) failure
/// propagates with its real message rather than being flattened.
#[derive(Debug)]
pub(crate) enum GmresError<E> {
    /// The matrix-free operator `apply` failed.
    Operator(E),
    /// The operator/right-hand side produced a non-finite value.
    NonFinite,
    /// Did not reach `tol` within the iteration budget; carries the final
    /// relative residual.
    DidNotConverge { relative_residual: f64 },
}

impl<E: std::fmt::Display> std::fmt::Display for GmresError<E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Operator(error) => write!(f, "{error}"),
            Self::NonFinite => write!(f, "matrix-free solve produced a non-finite value"),
            Self::DidNotConverge { relative_residual } => write!(
                f,
                "matrix-free GMRES did not converge: relative residual {relative_residual:.3e}"
            ),
        }
    }
}

/// Tuning for [`gmres`].
#[derive(Debug, Clone, Copy)]
pub(crate) struct GmresConfig {
    /// Convergence threshold on the residual relative to `‖b‖`.
    pub tol: f64,
    /// Krylov subspace size per restart cycle (capped to `n`).
    pub restart: usize,
    /// Maximum number of restart cycles.
    pub max_restarts: usize,
}

impl Default for GmresConfig {
    fn default() -> Self {
        // `restart` is the Krylov subspace size before a restart; it is clamped to
        // `n`, so small steady systems run *full* GMRES (exact, one cycle). A large
        // restart matters for grid-scale adjoints: restarted GMRES(m) with a small
        // `m` stagnates on the stiff DAE-residual transpose (the 12x12 airfoil
        // steady adjoint stalls at ~3.5e-2 residual with m=50 but converges with
        // m≈400). `tol` 1e-10 on the relative residual is ample for gradients
        // (validation tolerances are ~1e-6). For substantially larger grids that
        // still stagnate, the next step is a preconditioner, not a bigger subspace.
        Self {
            tol: 1.0e-10,
            restart: 400,
            max_restarts: 8,
        }
    }
}

/// Solve `A x = b` for `x` with restarted GMRES, where `apply(v, out)` computes
/// `out = A·v`. `A` is `n × n` with `n = b.len()`. Returns the solution, or an
/// error carrying the final residual if it does not converge.
pub(crate) fn gmres<F, E>(
    mut apply: F,
    b: &[f64],
    config: GmresConfig,
) -> Result<Vec<f64>, GmresError<E>>
where
    F: FnMut(&[f64], &mut [f64]) -> Result<(), E>,
{
    let n = b.len();
    let mut x = vec![0.0_f64; n];
    if n == 0 {
        return Ok(x);
    }
    let b_norm = norm(b);
    if b_norm == 0.0 {
        return Ok(x); // x = 0 solves A x = 0.
    }
    let restart = config.restart.clamp(1, n);
    let mut residual = vec![0.0_f64; n];
    let mut work = vec![0.0_f64; n];

    for _cycle in 0..config.max_restarts {
        // r = b - A x.
        apply(&x, &mut work).map_err(GmresError::Operator)?;
        for i in 0..n {
            residual[i] = b[i] - work[i];
        }
        let beta = norm(&residual);
        check_finite(beta)?;
        if beta <= config.tol * b_norm {
            return Ok(x);
        }

        // Arnoldi basis and the Hessenberg least-squares state.
        let mut basis: Vec<Vec<f64>> = Vec::with_capacity(restart + 1);
        basis.push(scaled(&residual, 1.0 / beta));
        // Hessenberg matrix H[(restart+1) x restart], stored column-major-ish as
        // h[col] = column of length col+2 (rows 0..=col+1).
        let mut hessenberg: Vec<Vec<f64>> = Vec::with_capacity(restart);
        let mut cos = vec![0.0_f64; restart];
        let mut sin = vec![0.0_f64; restart];
        // g: the rotated residual vector; ‖g[k+1..]‖ is the current residual.
        let mut g = vec![0.0_f64; restart + 1];
        g[0] = beta;

        let mut converged_at = None;
        for j in 0..restart {
            apply(&basis[j], &mut work).map_err(GmresError::Operator)?;
            let mut column = vec![0.0_f64; j + 2];
            // Modified Gram-Schmidt against the existing basis.
            for (i, basis_i) in basis.iter().enumerate().take(j + 1) {
                let h_ij = dot(&work, basis_i);
                column[i] = h_ij;
                axpy(&mut work, -h_ij, basis_i);
            }
            let h_next = norm(&work);
            check_finite(h_next)?;
            column[j + 1] = h_next;

            // Apply previous Givens rotations to the new column.
            for i in 0..j {
                let temp = cos[i] * column[i] + sin[i] * column[i + 1];
                column[i + 1] = -sin[i] * column[i] + cos[i] * column[i + 1];
                column[i] = temp;
            }
            // New rotation to eliminate the sub-diagonal entry column[j+1].
            let (c, s) = givens(column[j], column[j + 1]);
            cos[j] = c;
            sin[j] = s;
            column[j] = c * column[j] + s * column[j + 1];
            column[j + 1] = 0.0;
            // Rotate g.
            let g_j = g[j];
            g[j] = c * g_j;
            g[j + 1] = -s * g_j;
            hessenberg.push(column);

            let relative = g[j + 1].abs() / b_norm;
            if relative <= config.tol {
                converged_at = Some(j);
                break;
            }
            // Happy breakdown: the next basis vector is (numerically) zero.
            if h_next <= f64::EPSILON * beta {
                converged_at = Some(j);
                break;
            }
            basis.push(scaled(&work, 1.0 / h_next));
        }

        // Solve the upper-triangular Hessenberg system and apply the Krylov step.
        let k = converged_at.map_or(restart, |j| j + 1);
        apply_hessenberg_solution(&hessenberg, &g, &basis, k, &mut x)?;

        if converged_at.is_some() {
            // Confirm against the true residual (rotation residual can drift).
            apply(&x, &mut work).map_err(GmresError::Operator)?;
            for i in 0..n {
                residual[i] = b[i] - work[i];
            }
            let r = norm(&residual);
            check_finite(r)?;
            if r <= config.tol * b_norm {
                return Ok(x);
            }
        }
    }

    apply(&x, &mut work).map_err(GmresError::Operator)?;
    for i in 0..n {
        residual[i] = b[i] - work[i];
    }
    let relative_residual = norm(&residual) / b_norm;
    if relative_residual <= config.tol.max(1.0e-8) {
        return Ok(x);
    }
    Err(GmresError::DidNotConverge { relative_residual })
}

/// Back-substitute the upper-triangular Hessenberg system `R y = g[..k]` for the
/// step coefficients and apply the Krylov correction `x += Σ yᵢ · basisᵢ`.
fn apply_hessenberg_solution<E>(
    hessenberg: &[Vec<f64>],
    g: &[f64],
    basis: &[Vec<f64>],
    k: usize,
    x: &mut [f64],
) -> Result<(), GmresError<E>> {
    let mut y = vec![0.0_f64; k];
    for i in (0..k).rev() {
        let mut acc = g[i];
        for (col, &y_col) in y.iter().enumerate().take(k).skip(i + 1) {
            acc -= hessenberg[col][i] * y_col;
        }
        let diag = hessenberg[i][i];
        if diag.abs() <= f64::EPSILON {
            return Err(GmresError::NonFinite);
        }
        y[i] = acc / diag;
    }
    for (i, &yi) in y.iter().enumerate() {
        axpy(x, yi, &basis[i]);
    }
    Ok(())
}

/// `target += factor * source` (axpy).
fn axpy(target: &mut [f64], factor: f64, source: &[f64]) {
    for (t, s) in target.iter_mut().zip(source) {
        *t += factor * s;
    }
}

fn norm(v: &[f64]) -> f64 {
    dot(v, v).sqrt()
}

fn dot(a: &[f64], b: &[f64]) -> f64 {
    a.iter().zip(b).map(|(x, y)| x * y).sum()
}

fn scaled(v: &[f64], factor: f64) -> Vec<f64> {
    v.iter().map(|x| x * factor).collect()
}

/// Givens rotation `(c, s)` zeroing `b` in `[a; b]`.
fn givens(a: f64, b: f64) -> (f64, f64) {
    if b == 0.0 {
        (1.0, 0.0)
    } else {
        let r = a.hypot(b);
        (a / r, b / r)
    }
}

fn check_finite<E>(value: f64) -> Result<(), GmresError<E>> {
    if value.is_finite() {
        Ok(())
    } else {
        Err(GmresError::NonFinite)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Apply a dense matrix (row-major) as the matrix-free operator. The operator
    // cannot fail, so its error type is `Infallible`.
    fn dense_apply(
        matrix: &[Vec<f64>],
    ) -> impl Fn(&[f64], &mut [f64]) -> Result<(), std::convert::Infallible> + '_ {
        move |v: &[f64], out: &mut [f64]| {
            for (row, matrix_row) in matrix.iter().enumerate() {
                out[row] = matrix_row.iter().zip(v).map(|(a, b)| a * b).sum();
            }
            Ok(())
        }
    }

    #[test]
    fn gmres_solves_nonsymmetric_system() {
        // A nonsymmetric, well-conditioned 3x3 system.
        let a = vec![
            vec![4.0, 1.0, 0.0],
            vec![-1.0, 3.0, 1.0],
            vec![0.0, 2.0, 5.0],
        ];
        let x_true = [1.0_f64, -2.0, 0.5];
        let mut b = vec![0.0; 3];
        dense_apply(&a)(&x_true, &mut b).unwrap();

        let x = gmres(dense_apply(&a), &b, GmresConfig::default()).expect("converge");
        for (got, want) in x.iter().zip(&x_true) {
            assert!((got - want).abs() < 1.0e-9, "got {got}, want {want}");
        }
    }

    #[test]
    fn gmres_zero_rhs_is_zero() {
        let a = vec![vec![2.0, 0.0], vec![0.0, 3.0]];
        let x = gmres(dense_apply(&a), &[0.0, 0.0], GmresConfig::default()).expect("zero");
        assert_eq!(x, vec![0.0, 0.0]);
    }
}
