//! Forward-mode JVP and reverse-mode VJP / steady-adjoint methods for
//! `SolveRuntime`, split out of `runtime.rs` to keep it under the SPEC_0021
//! file-size limit. Child module of `runtime`, so the `impl SolveRuntime`
//! here retains access to the runtime's private fields and helper methods.
//!
//! # References
//!
//! Forward-mode JVP and reverse-mode VJP are the two sweep directions of
//! algorithmic differentiation: A. Griewank and A. Walther, "Evaluating
//! Derivatives", 2nd ed., SIAM 2008, doi:10.1137/1.9780898717761, chapters 3
//! and 4. The steady-state adjoint, in which the adjoint system is solved once
//! at the converged state rather than integrated backwards, is Y. Cao, S. Li,
//! L. Petzold and R. Serban, "Adjoint sensitivity analysis for
//! differential-algebraic equations: the adjoint DAE system and its numerical
//! solution", SIAM Journal on Scientific Computing 24(3):1076-1089, 2003,
//! doi:10.1137/S1064827501380630.

use super::*;

/// The Jacobian-vector seed of one forward sweep, with the scratch the sweep
/// expands it into.
///
/// The seed spans `[solver-y | parameter]` space: `copy_len` leading entries
/// come from the caller and the algebraic slots are filled by the projection
/// forward-sensitivity, so the vector the JVP finally reads is `buffer`, never
/// `values`. The expansion buffer belongs to [`StateDerivativeScratch`] and
/// is reused across sweeps.
struct JacobianSeed<'a> {
    values: &'a [f64],
    copy_len: usize,
    buffer: &'a mut Vec<f64>,
}

impl ScalarExecutionView<'_> {
    /// Exact state Jacobian-vector product `d(der)/d(state)·v` for the state-only
    /// BDF path, accounting for the algebraic projection.
    ///
    /// The state-only path integrates the reduced ODE `der = f(state, alg(state))`,
    /// where `alg(state)` is recovered each step by the algebraic projection. The
    /// total state Jacobian is therefore
    /// `∂f/∂state·v + ∂f/∂alg · (d(alg)/d(state)·v)`. We compute it in three steps:
    ///
    /// 1. reconstruct the linearization point `solver_y` (states + projected
    ///    algebraics) via the value refresh;
    /// 2. propagate the state seed `v` through the same projection
    ///    (`seed_refresh_derivative_dependencies`) to fill the algebraic
    ///    seeds `d(alg)/d(state)·v`;
    /// 3. apply the derivative JVP `derivative_jacobian_v` to the completed seed.
    ///
    /// The result is exact (true structural zeros stay exactly zero, so diffsol's
    /// NaN-sparsity probe recovers the correct pattern) and uses no finite
    /// differences. For pure ODEs step 2 is a no-op and this reduces to the plain
    /// derivative JVP.
    pub fn eval_state_jacobian_v_ad_into(
        &self,
        lin: AlgebraicLinearization<'_>,
        state: &[f64],
        seed: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        // State seed: copy only the state slice; the algebraic and parameter
        // seeds stay zero (the algebraic seeds are filled by the projection).
        self.eval_derivative_jacobian_v_with_seed(lin, state, seed, self.state_count, out)
    }

    pub fn eval_state_jacobian_v_ad_with_guess_into(
        &self,
        lin: AlgebraicLinearization<'_>,
        state: &[f64],
        seed: &[f64],
        solver_y_guess: &mut [f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.update_solver_y_guess_from_state(solver_y_guess, state)?;
        let mut scratch = self.scalar.derivative_scratch.borrow_mut();
        let StateDerivativeScratch { seed_buf, .. } = &mut *scratch;
        self.eval_derivative_jacobian_v_at_solver_y(
            lin,
            solver_y_guess,
            JacobianSeed {
                values: seed,
                copy_len: self.state_count,
                buffer: seed_buf,
            },
            out,
        )
    }

    /// Evaluate the state Jacobian-vector product at a solver vector that the
    /// caller already settled for this exact linearization coordinate.
    ///
    /// This is an internal FMI callback optimization. The ME component owns
    /// the bitwise coordinate cache and invalidates it on lifecycle mutation;
    /// callers that cannot supply that proof use
    /// [`Self::eval_state_jacobian_v_ad_with_guess_into`] instead.
    pub(crate) fn eval_state_jacobian_v_at_settled_solver_y_into(
        &self,
        lin: AlgebraicLinearization<'_>,
        solver_y: &[f64],
        seed: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.validate_refresh_inputs(solver_y, lin.params)?;
        let mut scratch = self.scalar.derivative_scratch.borrow_mut();
        let StateDerivativeScratch { seed_buf, .. } = &mut *scratch;
        self.eval_derivative_jacobian_v_from_settled_solver_y(
            lin,
            solver_y,
            JacobianSeed {
                values: seed,
                copy_len: self.state_count,
                buffer: seed_buf,
            },
            out,
        )
    }

    /// Like [`Self::eval_state_jacobian_v_ad_into`], but the input `seed` spans
    /// the full `[solver-y | parameter]` space and is copied in its entirety, so
    /// parameter tangents are honored. Seeding a unit vector in a parameter slot
    /// (offset `solver_count + p`) yields the column `∂der/∂p` — including the
    /// path through algebraics, since the projection JVP (`implicit_jacobian_v`)
    /// is also lowered with parameter seeds. Backs the parameter Jacobian. The
    /// seed's algebraic slots are overwritten by the projection
    /// forward-sensitivity, so callers should leave them zero.
    pub fn eval_full_jacobian_v_ad_into(
        &self,
        lin: AlgebraicLinearization<'_>,
        state: &[f64],
        seed: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.eval_derivative_jacobian_v_with_seed(lin, state, seed, seed.len(), out)
    }

    /// Forward-sensitivity right-hand side for one parameter: given the current
    /// state-sensitivity column `sens_column = ∂state/∂p` (length `state_count`)
    /// and the parameter's P-slot, computes
    /// `out = ∂f/∂state · sens_column + ∂f/∂p` — the time derivative of that
    /// sensitivity column. This is `eval_full_jacobian_v_ad_into` with the
    /// combined seed `[sens_column | eₚ]`; it is the building block for
    /// integrating forward parameter sensitivities (Track 0.2). The seed is
    /// allocated per call, which is fine for a foundation/validation primitive;
    /// a hot integrator should reuse a buffer.
    pub fn eval_forward_sensitivity_column_into(
        &self,
        lin: AlgebraicLinearization<'_>,
        state: &[f64],
        sens_column: &[f64],
        param_slot: usize,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        let p_scalars = self.model.problem.layout.p_scalars();
        let mut seed = vec![0.0; self.solver_count + p_scalars];
        let n = self.state_count.min(sens_column.len()).min(seed.len());
        seed[..n].copy_from_slice(&sens_column[..n]);
        let p_index = self.solver_count + param_slot;
        if p_index < seed.len() {
            seed[p_index] = 1.0;
        }
        self.eval_full_jacobian_v_ad_into(lin, state, &seed, out)
    }

    /// One right-hand side of the augmented variational system
    /// `[x' = F; S_j' = (∂F/∂x) S_j + ∂F/∂p_j]` at `(t, x, S)`, with `state`
    /// laid out as `layout` (`[x | S_1 .. S_m | ..]`) and `out` written over
    /// the same ranges.
    ///
    /// The algebraic projection of the primal point is settled once and serves
    /// the derivative and every parameter column; each column only seeds the
    /// projection's forward sensitivity and applies the derivative JVP. This is
    /// the batch form of [`Self::eval_forward_sensitivity_column_into`].
    pub fn eval_derivatives_and_sensitivities_into(
        &self,
        lin: AlgebraicLinearization<'_>,
        state: &[f64],
        layout: rumoca_ir_solve::SensitivityLayout,
        param_slots: &[usize],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        let AlgebraicLinearization { t, params, settle } = lin;
        let n = layout.states;
        let p_scalars = self.model.problem.layout.p_scalars();
        let mut scratch = self.scalar.derivative_scratch.borrow_mut();
        let StateDerivativeScratch { solver_y, seed_buf } = &mut *scratch;
        self.populate_solver_y_from_state(solver_y, &state[..n])?;
        self.eval_state_derivatives_at_solver_y(
            t,
            params,
            settle.tol,
            settle.max_iters,
            solver_y,
            &mut out[..n],
        )?;
        let mut seed = vec![0.0; self.solver_count + p_scalars];
        for (j, slot) in param_slots.iter().enumerate() {
            let range = layout.sensitivity(j);
            seed[..n].copy_from_slice(&state[range.clone()]);
            seed[self.solver_count + slot] = 1.0;
            let column = self.eval_derivative_jacobian_v_from_settled_solver_y(
                lin,
                solver_y,
                JacobianSeed {
                    values: &seed,
                    copy_len: seed.len(),
                    buffer: seed_buf,
                },
                &mut out[range],
            );
            seed[self.solver_count + slot] = 0.0;
            column?;
        }
        Ok(())
    }

    /// Project a state-sensitivity column `∂state/∂p` to the full solver-y
    /// sensitivity `∂(solver-y)/∂p` (states *and* algebraics) at the
    /// linearization point, via the algebraic projection's forward-sensitivity.
    /// `out` (length `solver_count`) receives the states unchanged and the
    /// algebraics filled with `∂(alg)/∂state·column + ∂(alg)/∂p`. Lets a steady
    /// objective be any solver-y variable (state or output/algebraic), Track 0.2.
    pub fn project_state_sensitivity_to_solver_y(
        &self,
        lin: AlgebraicLinearization<'_>,
        state: &[f64],
        state_sensitivity: &[f64],
        param_slot: usize,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.project_state_tangent_to_solver_y(lin, state, state_sensitivity, Some(param_slot), out)
    }

    /// [`Self::project_state_sensitivity_to_solver_y`] for a tangent that may
    /// carry no parameter seed: `param_slot = None` projects a pure state
    /// direction (the `C` rows of a linearization), `Some(slot)` adds the unit
    /// tangent of that parameter or input.
    pub fn project_state_tangent_to_solver_y(
        &self,
        lin: AlgebraicLinearization<'_>,
        state: &[f64],
        state_sensitivity: &[f64],
        param_slot: Option<usize>,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        let AlgebraicLinearization { t, params, settle } = lin;
        let mut scratch = self.scalar.derivative_scratch.borrow_mut();
        let StateDerivativeScratch { solver_y, seed_buf } = &mut *scratch;
        // Linearization point: settle *all* solver-y algebraics from the state
        // (the full plan, not just the derivative-dependency subset) so that leaf
        // algebraics — e.g. a pure output objective — are at their correct value.
        self.populate_solver_y_from_state(solver_y, state)?;
        self.refresh_algebraic_and_output_slots(t, solver_y, params, settle.tol, settle.max_iters)?;
        let p_scalars = self.model.problem.layout.p_scalars();
        let seed_len = (self.solver_count + p_scalars)
            .max(
                self.scalar
                    .implicit_jacobian_v
                    .source()
                    .requirements()
                    .seed_len,
            )
            .max(self.solver_count);
        seed_buf.clear();
        seed_buf.resize(seed_len, 0.0);
        let n = self
            .state_count
            .min(state_sensitivity.len())
            .min(seed_buf.len());
        seed_buf[..n].copy_from_slice(&state_sensitivity[..n]);
        if let Some(param_slot) = param_slot {
            let p_index = self.solver_count + param_slot;
            if p_index < seed_buf.len() {
                seed_buf[p_index] = 1.0;
            }
        }
        // Seed against the full algebraic plan so every solver-y algebraic (states
        // pass through unchanged) receives its `∂(alg)/∂state·v + ∂(alg)/∂p` seed.
        self.seed_refresh_with_plan(&self.scalar.algebraic_refresh, lin, solver_y, seed_buf)?;
        let copy = self.solver_count.min(out.len()).min(seed_buf.len());
        out[..copy].copy_from_slice(&seed_buf[..copy]);
        Ok(())
    }

    /// Index of a solver-y variable (state or algebraic) by qualified name, or
    /// `None` if it is not a solver variable (e.g. a parameter or unknown name).
    pub fn solver_variable_index(&self, name: &str) -> Option<usize> {
        self.model.problem.solver_index(name)
    }

    /// Prepare the requested reverse product before any source execution.
    pub fn reverse_state_derivative_vjp(
        &self,
        lin: AlgebraicLinearization<'_>,
        state: &[f64],
        output_cotangents: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.prepare_reference_reverse()?
            .reverse_state_derivative_vjp(lin, state, output_cotangents, out)
    }

    /// Prepare the requested reverse product before any source execution.
    pub fn reverse_implicit_residual_vjp(
        &self,
        t: f64,
        solver_y: &[f64],
        params: &[f64],
        output_cotangents: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.prepare_reference_reverse()?
            .reverse_implicit_residual_vjp(t, solver_y, params, output_cotangents, out)
    }

    /// Prepare the requested reverse product before any source execution.
    pub fn apply_steady_residual_transpose(
        &self,
        t: f64,
        solver_y: &[f64],
        params: &[f64],
        lambda: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.prepare_reference_reverse()?
            .apply_steady_residual_transpose(t, solver_y, params, lambda, out)
    }

    /// Shared core of the derivative JVP: reconstruct the linearization point,
    /// copy the leading `seed_copy_len` seed entries, fill the algebraic seeds
    /// via the projection forward-sensitivity, then apply `derivative_jacobian_v`.
    fn eval_derivative_jacobian_v_with_seed(
        &self,
        lin: AlgebraicLinearization<'_>,
        state: &[f64],
        seed: &[f64],
        seed_copy_len: usize,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        let mut scratch = self.scalar.derivative_scratch.borrow_mut();
        let StateDerivativeScratch { solver_y, seed_buf } = &mut *scratch;
        self.populate_solver_y_from_state(solver_y, state)?;
        self.eval_derivative_jacobian_v_at_solver_y(
            lin,
            solver_y,
            JacobianSeed {
                values: seed,
                copy_len: seed_copy_len,
                buffer: seed_buf,
            },
            out,
        )
    }

    fn eval_derivative_jacobian_v_at_solver_y(
        &self,
        lin: AlgebraicLinearization<'_>,
        solver_y: &mut [f64],
        seed: JacobianSeed<'_>,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        let AlgebraicLinearization { t, params, settle } = lin;
        validate_derivative_output_len(out, self.state_count)?;
        // (1) Linearization point: project the algebraics from the caller's seed.
        self.refresh_derivative_dependencies(t, solver_y, params, settle.tol, settle.max_iters)?;
        self.eval_derivative_jacobian_v_from_settled_solver_y(lin, solver_y, seed, out)
    }

    fn eval_derivative_jacobian_v_from_settled_solver_y(
        &self,
        lin: AlgebraicLinearization<'_>,
        solver_y: &[f64],
        seed: JacobianSeed<'_>,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        let JacobianSeed {
            values: seed,
            copy_len: seed_copy_len,
            buffer: seed_buf,
        } = seed;
        let AlgebraicLinearization { t, params, .. } = lin;
        validate_derivative_output_len(out, self.state_count)?;
        // The JVP rows seed both solver-y and parameters (`SeedMode::SolverYAndP`),
        // so the seed vector spans `[solver-y | parameter]` space. We copy the
        // leading `seed_copy_len` entries from the caller (state-only for the
        // state Jacobian, the whole span for ∂der/∂p); the algebraic seeds are
        // then filled by the projection forward-sensitivity below.
        let seed_len = self
            .scalar
            .derivative_jacobian_v
            .source()
            .requirements()
            .seed_len
            .max(
                self.scalar
                    .implicit_jacobian_v
                    .source()
                    .requirements()
                    .seed_len,
            )
            .max(self.solver_count);
        seed_buf.clear();
        seed_buf.resize(seed_len, 0.0);
        let n = seed_copy_len.min(seed.len()).min(seed_buf.len());
        seed_buf[..n].copy_from_slice(&seed[..n]);
        // (2) Forward-propagate the seed through the algebraic projection.
        self.seed_refresh_derivative_dependencies(lin, solver_y, seed_buf)?;
        // (3) Total Jacobian-vector product via the derivative JVP.
        let context = RowEvalContext {
            seed: Some(seed_buf.as_slice()),
            ..self.row_eval_context()
        };
        self.scalar.derivative_jacobian_v.eval(
            solve_eval::JacobianEvalInputs {
                y: solver_y,
                p: params,
                t,
                seed: seed_buf,
            },
            context,
            out,
        )
    }

    /// Forward-sensitivity ("seed") refresh: with `solver_y` at the linearization
    /// point and the state seed already written into `seed[..state_count]`, fill
    /// the algebraic slots of `seed` with `d(alg)/d(state)·v` by propagating the
    /// seed through the same projection rows used for the value refresh. This
    /// mirrors [`Self::refresh_derivative_dependencies`] but linearized.
    fn seed_refresh_derivative_dependencies(
        &self,
        lin: AlgebraicLinearization<'_>,
        solver_y: &[f64],
        seed: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.seed_refresh_with_plan(&self.scalar.derivative_refresh, lin, solver_y, seed)
    }

    /// Plan-parameterized forward-sensitivity refresh shared by the
    /// derivative-dependency projection ([`Self::seed_refresh_derivative_dependencies`])
    /// and the full solver-y projection ([`Self::project_state_sensitivity_to_solver_y`],
    /// which uses the complete `algebraic_refresh` plan so that *leaf* algebraics —
    /// ones that feed no state derivative, e.g. a pure output objective — also get
    /// their seed filled).
    pub(super) fn seed_refresh_with_plan(
        &self,
        plan: &solve::RefreshPlan,
        lin: AlgebraicLinearization<'_>,
        solver_y: &[f64],
        seed: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.seed_refresh_blocks(
            (&plan.simultaneous_plan, &plan.simultaneous_block_indices),
            lin,
            solver_y,
            seed,
        )
    }

    /// [`Self::seed_refresh_with_plan`] over a projection plan whose blocks
    /// carry their issued indices, such as a subset of the refresh plan's.
    pub(super) fn seed_refresh_blocks(
        &self,
        (plan, block_indices): (&solve::AlgebraicProjectionPlan, &[usize]),
        lin: AlgebraicLinearization<'_>,
        solver_y: &[f64],
        seed: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        let projection_model = RefreshProjectionModel {
            runtime: *self,
            seed_linearizations: Some(RefCell::new(SeedProjectionCache::at_point(
                &self.scalar.seed_projection_cache,
                lin,
                solver_y,
                self.scalar
                    .continuous_structural
                    .algebraic_projection()
                    .len(),
            ))),
            #[cfg(test)]
            plan,
            block_indices,
            plan_validated: false,
            jacobian_v: ProjectionJacobian::SolverYAndParameters(&self.scalar.implicit_jacobian_v),
        };
        let result = project_algebraic_seed_with_plan(
            &projection_model,
            plan,
            solver_y,
            (
                crate::runtime::projection::AlgebraicProjectionArgs {
                    parameters: lin.params,
                    time: lin.t,
                    state_count: self.state_count,
                    tolerance: lin.settle.tol,
                },
                &|block, p| self.singular_active_mode(block, p),
            ),
            seed,
        );
        if result.is_err()
            && let Some(cache) = &projection_model.seed_linearizations
        {
            cache.borrow_mut().clear();
        }
        result
    }
}
