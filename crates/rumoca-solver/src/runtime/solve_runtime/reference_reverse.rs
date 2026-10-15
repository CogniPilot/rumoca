//! Explicit reference execution for the separate reverse-sensitivity product.

use super::*;

/// Reverse sensitivity interpreted from one immutable reference runtime.
/// Construction is unavailable to compiled simulation runtimes: no compiled
/// reverse producer is currently admitted. The borrowed owner cannot be rebound.
///
/// ```compile_fail
/// use rumoca_solver::{ReferenceReverseExecution, SolveRuntime};
/// fn forge(runtime: &SolveRuntime) {
///     let _ = ReferenceReverseExecution { view: runtime.scalar_execution().unwrap() };
/// }
/// ```
pub struct ReferenceReverseExecution<'runtime> {
    view: ScalarExecutionView<'runtime>,
}

impl SolveRuntime {
    /// Prepare reverse sensitivity independently of the simulation product.
    /// Explicit reference runtimes retain the interpreter. A compiled runtime
    /// rejects this request before evaluation because native reverse lowering
    /// is not available; it never silently changes execution engines.
    pub fn prepare_reverse_sensitivity(
        &self,
    ) -> Result<ReferenceReverseExecution<'_>, RuntimeSolveError> {
        self.scalar_execution()?.prepare_reference_reverse()
    }
}

impl<'runtime> ScalarExecutionView<'runtime> {
    pub(super) fn prepare_reference_reverse(
        self,
    ) -> Result<ReferenceReverseExecution<'runtime>, RuntimeSolveError> {
        if !matches!(self.scalar.expressions, PreparedExpressions::Reference) {
            return Err(RuntimeSolveError::CompiledExecution {
                context: "reverse sensitivity preparation",
                message: "compiled reverse-sensitivity lowering is not available; explicitly prepare a reference runtime for reverse execution".into(),
                // This is a missing target product, not a fault at a source operation.
                source_spans: Box::new([]),
            });
        }
        Ok(ReferenceReverseExecution { view: self })
    }
}

impl ReferenceReverseExecution<'_> {
    /// Reverse-mode VJP of the state-derivative function: given the output
    /// cotangent `output_cotangents` (`λ`, one per state derivative), compute
    /// `(∂der/∂[solver_y|p])ᵀ · λ` in a single reverse sweep over the primal
    /// derivative program (Track A scalar reverse core). `out` (length
    /// `solver_count + p_scalars`) receives the cotangent over `[solver_y | p]`:
    /// the leading `solver_count` entries are `∂(λ·der)/∂solver_y`, the rest
    /// `∂(λ·der)/∂p`. The transpose property is the dot-product identity
    /// `λᵀ(J v) = (Jᵀλ)ᵀ v` against the forward JVP.
    ///
    /// This is the *bare* derivative VJP: it reverses only the primal derivative
    /// program and does **not** chain through the algebraic projection. It is the
    /// exact transpose of the forward JVP only when there are no solver algebraics
    /// (a pure ODE, `solver_count == state_count`). For a model with algebraics it
    /// would return a partial — not total — gradient, so this method refuses such
    /// models with an error rather than silently producing a wrong result; the
    /// algebraic-projection adjoint chain is Track B.
    pub fn reverse_state_derivative_vjp(
        &self,
        lin: AlgebraicLinearization<'_>,
        state: &[f64],
        output_cotangents: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        let AlgebraicLinearization { t, params, settle } = lin;
        if self.view.solver_count != self.view.state_count {
            return Err(RuntimeSolveError::solve_ir(format!(
                "reverse-mode VJP does not yet support models with solver algebraics \
                 ({} algebraic(s) beyond {} state(s)): the algebraic-projection adjoint is Track B. \
                 Reverse a pure-ODE model, or use the forward sensitivity for now",
                self.view.solver_count - self.view.state_count,
                self.view.state_count
            )));
        }
        let p_scalars = self.view.model.problem.layout.p_scalars();
        let expected = self.view.solver_count + p_scalars;
        if out.len() != expected {
            return Err(RuntimeSolveError::solve_ir(format!(
                "reverse VJP output has {} entries, expected solver_count + p_scalars = {expected}",
                out.len()
            )));
        }
        let derivative = self
            .view
            .scalar
            .derivative_scalar
            .reference()
            .ok_or_else(|| {
                RuntimeSolveError::solve_ir(
                    "reference derivative execution capability is unavailable",
                )
            })?;
        let mut scratch = self.view.scalar.derivative_scratch.borrow_mut();
        let solver_y = &mut scratch.solver_y;
        // Reconstruct the linearization point (states + derivative-dependency
        // algebraics), matching the forward JVP's primal point.
        self.view.populate_solver_y_from_state(solver_y, state)?;
        self.view.refresh_derivative_dependencies(
            t,
            solver_y,
            params,
            settle.tol,
            settle.max_iters,
        )?;
        out.fill(0.0);
        let (cot_y, cot_p) = out.split_at_mut(self.view.solver_count);
        let mut reverse_scratch = self.view.scalar.reverse_scratch.borrow_mut();
        derivative.reverse_vjp(
            &rumoca_eval_solve::reverse::ReverseInputs {
                y: solver_y,
                p: params,
                t,
                context: self.view.row_eval_context(),
            },
            output_cotangents,
            &mut rumoca_eval_solve::reverse::ReverseCotangents {
                y: cot_y,
                p: cot_p,
                seed: &mut [],
            },
            &mut reverse_scratch,
        )?;
        Ok(())
    }

    /// Reverse-mode VJP of the implicit residual program `g(solver_y, p)`: given a
    /// cotangent `output_cotangents` (one per residual row), compute
    /// `(∂g/∂[solver_y|p])ᵀ · μ` in a single reverse sweep. `out` (length
    /// `solver_count + p_scalars`) receives `(∂g/∂solver_y)ᵀ μ` in the leading
    /// `solver_count` entries and `(∂g/∂p)ᵀ μ` after.
    ///
    /// This is the transposed constraint-Jacobian operator (`∂g/∂yᵀ`) — the
    /// building block for the algebraic-projection adjoint (Track B): the steady
    /// adjoint of a model with solver algebraics solves `(∂g/∂z)ᵀ μ = ∂f/∂zᵀ λ`
    /// and corrects the gradient by `−∂g/∂[x|p]ᵀ μ`, all matrix-free via this VJP.
    /// Unlike [`Self::reverse_state_derivative_vjp`] the caller supplies the
    /// solver-y linearization point directly (already settled); no algebraic
    /// re-projection happens here.
    pub fn reverse_implicit_residual_vjp(
        &self,
        t: f64,
        solver_y: &[f64],
        params: &[f64],
        output_cotangents: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        let p_scalars = self.view.model.problem.layout.p_scalars();
        let expected = self.view.solver_count + p_scalars;
        if out.len() != expected {
            return Err(RuntimeSolveError::solve_ir(format!(
                "implicit residual VJP output has {} entries, expected solver_count + p_scalars = \
                 {expected}",
                out.len()
            )));
        }
        out.fill(0.0);
        let (cot_y, cot_p) = out.split_at_mut(self.view.solver_count);
        // A self-contained scratch keeps this composable with the derivative VJP
        // (the algebraic adjoint drives both); reuse can come once it is hot.
        let mut scratch = rumoca_eval_solve::reverse::ReverseScratch::default();
        self.view
            .scalar
            .implicit_scalar_rhs
            .reference()
            .ok_or_else(|| {
                RuntimeSolveError::solve_ir("reference execution capability is unavailable")
            })?
            .reverse_vjp(
                &rumoca_eval_solve::reverse::ReverseInputs {
                    y: solver_y,
                    p: params,
                    t,
                    context: self.view.row_eval_context(),
                },
                output_cotangents,
                &mut rumoca_eval_solve::reverse::ReverseCotangents {
                    y: cot_y,
                    p: cot_p,
                    seed: &mut [],
                },
                &mut scratch,
            )?;
        Ok(())
    }

    /// Implicit-residual rows that are the algebraic constraints `g` of the steady
    /// residual `R = [der; g]`, in a stable order (the algebraic-projection plan's
    /// block rows). The remaining implicit rows are state passthroughs and carry
    /// no steady-residual content. Taken from `algebraic_projection_plan` — not
    /// `implicit_row_targets`, whose target for a genuine implicit constraint
    /// (e.g. `z*z = b*x`) is `None`, so it cannot identify these rows.
    fn algebraic_constraint_rows(&self) -> Vec<usize> {
        self.view
            .model
            .problem
            .continuous
            .algebraic_projection_plan
            .blocks
            .scalar_blocks()
            .iter()
            .flat_map(|block| block.rows.iter().copied())
            .collect()
    }

    /// Apply the transpose of the **full steady-residual Jacobian** to `lambda`:
    /// `out = (∂R/∂[solver_y|p])ᵀ · λ`, where `R = [der(states); g(algebraics)]` is
    /// the steady residual (state ODEs at rest plus the algebraic constraints).
    ///
    /// Matrix-free, assembled from the two reverse VJPs at a *settled* solver-y
    /// point: the state (`der`) rows via the derivative program reversed with
    /// `λ[0..state_count]`, the algebraic (`g`) rows via the implicit-residual
    /// program reversed with `λ` scattered onto the constraint rows. The second
    /// VJP accumulates onto the first (both write `+=` into `out`), so `out` holds
    /// their sum. `out` length is `solver_count + p_scalars`.
    ///
    /// This is the operator the steady **adjoint** drives with GMRES; it lifts the
    /// pure-ODE restriction (handles solver algebraics and algebraic/output
    /// objectives uniformly). The caller supplies an algebraic-consistent
    /// `solver_y` (settled so `g = 0`).
    pub fn apply_steady_residual_transpose(
        &self,
        t: f64,
        solver_y: &[f64],
        params: &[f64],
        lambda: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        let p_scalars = self.view.model.problem.layout.p_scalars();
        let expected = self.view.solver_count + p_scalars;
        if out.len() != expected {
            return Err(RuntimeSolveError::solve_ir(format!(
                "steady-residual transpose output has {} entries, expected solver_count + \
                 p_scalars = {expected}",
                out.len()
            )));
        }
        if lambda.len() != self.view.solver_count {
            return Err(RuntimeSolveError::solve_ir(format!(
                "steady-residual transpose cotangent has {} entries, expected solver_count = {}",
                lambda.len(),
                self.view.solver_count
            )));
        }
        let derivative = self
            .view
            .scalar
            .derivative_scalar
            .reference()
            .ok_or_else(|| {
                RuntimeSolveError::solve_ir(
                    "reference derivative execution capability is unavailable",
                )
            })?;
        out.fill(0.0);

        // State (der) rows: ∂der/∂[y|p]ᵀ · λ[0..state_count].
        let state_lambda = &lambda[..self.view.state_count];
        self.accumulate_block_vjp(
            &mut |inputs, cot, scratch| derivative.reverse_vjp(inputs, state_lambda, cot, scratch),
            (t, solver_y, params),
            out,
        )?;

        // Algebraic (g) rows: scatter the algebraic multipliers onto the implicit
        // residual's constraint rows and accumulate ∂g/∂[y|p]ᵀ · μ onto `out`.
        //
        // `scatter_algebraic_multipliers` maps the k-th algebraic constraint to
        // `λ[state_count + k]`. That index arithmetic is only correct if solver-y is
        // laid out `[states (state_count) | algebraics]` AND there is exactly one
        // algebraic constraint per non-state solver-y slot. Assert that coupling here
        // rather than trust it: a future solver-y reordering would otherwise silently
        // read the wrong multipliers and return a wrong adjoint.
        let alg = self.algebraic_constraint_rows();
        let algebraic_count = self.view.solver_count - self.view.state_count;
        if alg.len() != algebraic_count {
            return Err(RuntimeSolveError::solve_ir(format!(
                "steady-residual transpose: {} algebraic constraint rows but {} non-state \
                 solver-y slots (solver_count {} − state_count {}); the adjoint multiplier \
                 mapping λ[state_count + k] assumes one constraint per non-state slot",
                alg.len(),
                algebraic_count,
                self.view.solver_count,
                self.view.state_count
            )));
        }
        if !alg.is_empty() {
            let mu = self.scatter_algebraic_multipliers(&alg, lambda);
            let reference = self
                .view
                .scalar
                .implicit_scalar_rhs
                .reference()
                .ok_or_else(|| {
                    RuntimeSolveError::solve_ir("reference execution capability is unavailable")
                })?;
            self.accumulate_block_vjp(
                &mut |inputs, cot, scratch| reference.reverse_vjp(inputs, &mu, cot, scratch),
                (t, solver_y, params),
                out,
            )?;
        }
        Ok(())
    }

    /// Reverse a scalar program block and **accumulate** `(∂block/∂[solver_y|p])ᵀ ·
    /// cotangents` into `out` (`out[..solver_count]` = solver-y part, rest = `p`
    /// part). `out` is not cleared, so successive calls sum their contributions.
    fn accumulate_block_vjp(
        &self,
        reverse: &mut dyn FnMut(
            &rumoca_eval_solve::reverse::ReverseInputs<'_>,
            &mut rumoca_eval_solve::reverse::ReverseCotangents<'_>,
            &mut rumoca_eval_solve::reverse::ReverseScratch,
        ) -> Result<(), RuntimeSolveError>,
        (t, solver_y, params): (f64, &[f64], &[f64]),
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        let mut scratch = rumoca_eval_solve::reverse::ReverseScratch::default();
        let (cot_y, cot_p) = out.split_at_mut(self.view.solver_count);
        reverse(
            &rumoca_eval_solve::reverse::ReverseInputs {
                y: solver_y,
                p: params,
                t,
                context: self.view.row_eval_context(),
            },
            &mut rumoca_eval_solve::reverse::ReverseCotangents {
                y: cot_y,
                p: cot_p,
                seed: &mut [],
            },
            &mut scratch,
        )
    }

    /// Build the implicit-residual row cotangent `μ` for the algebraic block: the
    /// k-th algebraic constraint row carries the multiplier `λ[state_count + k]`
    /// (the steady residual's algebraic-equation index); all other rows are zero.
    fn scatter_algebraic_multipliers(&self, alg_rows: &[usize], lambda: &[f64]) -> Vec<f64> {
        let mut mu = vec![0.0_f64; self.view.scalar.implicit_scalar_rhs.source().len()];
        for (k, &row) in alg_rows.iter().enumerate() {
            if let (Some(slot), Some(&value)) =
                (mu.get_mut(row), lambda.get(self.view.state_count + k))
            {
                *slot = value;
            }
        }
        mu
    }
}
