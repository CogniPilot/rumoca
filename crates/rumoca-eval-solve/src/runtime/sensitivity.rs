//! Forward-mode JVP and reverse-mode VJP / steady-adjoint methods for
//! `SolveRuntime`, split out of `runtime.rs` to keep it under the SPEC_0021
//! file-size limit. Child module of `runtime`, so the `impl SolveRuntime`
//! here retains access to the runtime's private fields and helper methods.

use super::*;

impl SolveRuntime {
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

    /// State Jacobian-vector product at an explicitly seeded algebraic branch.
    /// The supplied guess is settled in place, so callers can use a private
    /// trial copy without mutating the accepted branch shared by an integrator.
    pub fn eval_state_jacobian_v_ad_with_guess_into(
        &self,
        lin: AlgebraicLinearization<'_>,
        state: &[f64],
        seed: &[f64],
        guess: &mut Vec<f64>,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.eval_derivative_jacobian_v_with_seed_and_guess(
            lin,
            state,
            seed,
            self.state_count,
            Some(guess),
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
        let AlgebraicLinearization { t, params, settle } = lin;
        let mut scratch = self.derivative_scratch.borrow_mut();
        let StateDerivativeScratch {
            solver_y,
            seed_buf,
            unit_seed,
        } = &mut *scratch;
        // Linearization point: settle *all* solver-y algebraics from the state
        // (the full plan, not just the derivative-dependency subset) so that leaf
        // algebraics — e.g. a pure output objective — are at their correct value.
        self.populate_solver_y_from_state(solver_y, state)?;
        self.refresh_algebraic_and_output_slots(t, solver_y, params, settle.tol, settle.max_iters)?;
        let p_scalars = self.model.problem.layout.p_scalars();
        let seed_len = (self.solver_count + p_scalars)
            .max(self.implicit_jacobian_v.requirements().seed_len)
            .max(self.solver_count);
        seed_buf.clear();
        seed_buf.resize(seed_len, 0.0);
        let n = self
            .state_count
            .min(state_sensitivity.len())
            .min(seed_buf.len());
        seed_buf[..n].copy_from_slice(&state_sensitivity[..n]);
        let p_index = self.solver_count + param_slot;
        if p_index < seed_buf.len() {
            seed_buf[p_index] = 1.0;
        }
        unit_seed.clear();
        unit_seed.resize(seed_len, 0.0);
        // Seed against the full algebraic plan so every solver-y algebraic (states
        // pass through unchanged) receives its `∂(alg)/∂state·v + ∂(alg)/∂p` seed.
        self.seed_refresh_with_plan(&self.algebraic_refresh, lin, solver_y, seed_buf, unit_seed)?;
        let copy = self.solver_count.min(out.len()).min(seed_buf.len());
        out[..copy].copy_from_slice(&seed_buf[..copy]);
        Ok(())
    }

    /// Index of a solver-y variable (state or algebraic) by qualified name, or
    /// `None` if it is not a solver variable (e.g. a parameter or unknown name).
    pub fn solver_variable_index(&self, name: &str) -> Option<usize> {
        self.model
            .problem
            .solve_layout
            .solver_maps
            .names
            .iter()
            .position(|candidate| candidate == name)
    }

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
        if self.solver_count != self.state_count {
            return Err(RuntimeSolveError::solve_ir(format!(
                "reverse-mode VJP does not yet support models with solver algebraics \
                 ({} algebraic(s) beyond {} state(s)): the algebraic-projection adjoint is Track B. \
                 Reverse a pure-ODE model, or use the forward sensitivity for now",
                self.solver_count - self.state_count,
                self.state_count
            )));
        }
        let p_scalars = self.model.problem.layout.p_scalars();
        let expected = self.solver_count + p_scalars;
        if out.len() != expected {
            return Err(RuntimeSolveError::solve_ir(format!(
                "reverse VJP output has {} entries, expected solver_count + p_scalars = {expected}",
                out.len()
            )));
        }
        let mut scratch = self.derivative_scratch.borrow_mut();
        let solver_y = &mut scratch.solver_y;
        // Reconstruct the linearization point (states + derivative-dependency
        // algebraics), matching the forward JVP's primal point.
        self.populate_solver_y_from_state(solver_y, state)?;
        self.refresh_derivative_dependencies(t, solver_y, params, settle.tol, settle.max_iters)?;
        out.fill(0.0);
        let (cot_y, cot_p) = out.split_at_mut(self.solver_count);
        let mut reverse_scratch = self.reverse_scratch.borrow_mut();
        self.derivative_scalar
            .reverse_vjp(
                &crate::reverse::ReverseInputs {
                    y: solver_y,
                    p: params,
                    t,
                    context: self.row_eval_context(),
                },
                output_cotangents,
                &mut crate::reverse::ReverseCotangents {
                    y: cot_y,
                    p: cot_p,
                    seed: &mut [],
                },
                &mut reverse_scratch,
            )
            .map_err(RuntimeSolveError::from)?;
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
        let p_scalars = self.model.problem.layout.p_scalars();
        let expected = self.solver_count + p_scalars;
        if out.len() != expected {
            return Err(RuntimeSolveError::solve_ir(format!(
                "implicit residual VJP output has {} entries, expected solver_count + p_scalars = \
                 {expected}",
                out.len()
            )));
        }
        out.fill(0.0);
        let (cot_y, cot_p) = out.split_at_mut(self.solver_count);
        // A self-contained scratch keeps this composable with the derivative VJP
        // (the algebraic adjoint drives both); reuse can come once it is hot.
        let mut scratch = crate::reverse::ReverseScratch::default();
        self.implicit_scalar_rhs
            .reverse_vjp(
                &crate::reverse::ReverseInputs {
                    y: solver_y,
                    p: params,
                    t,
                    context: self.row_eval_context(),
                },
                output_cotangents,
                &mut crate::reverse::ReverseCotangents {
                    y: cot_y,
                    p: cot_p,
                    seed: &mut [],
                },
                &mut scratch,
            )
            .map_err(RuntimeSolveError::from)?;
        Ok(())
    }

    /// Implicit-residual rows that are the algebraic constraints `g` of the steady
    /// residual `R = [der; g]`, in a stable order (the algebraic-projection plan's
    /// block rows). The remaining implicit rows are state passthroughs and carry
    /// no steady-residual content. Taken from `algebraic_projection_plan` — not
    /// `implicit_row_targets`, whose target for a genuine implicit constraint
    /// (e.g. `z*z = b*x`) is `None`, so it cannot identify these rows.
    fn algebraic_constraint_rows(&self) -> Vec<usize> {
        self.model
            .problem
            .continuous
            .algebraic_projection_plan
            .blocks
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
        let p_scalars = self.model.problem.layout.p_scalars();
        let expected = self.solver_count + p_scalars;
        if out.len() != expected {
            return Err(RuntimeSolveError::solve_ir(format!(
                "steady-residual transpose output has {} entries, expected solver_count + \
                 p_scalars = {expected}",
                out.len()
            )));
        }
        if lambda.len() != self.solver_count {
            return Err(RuntimeSolveError::solve_ir(format!(
                "steady-residual transpose cotangent has {} entries, expected solver_count = {}",
                lambda.len(),
                self.solver_count
            )));
        }
        out.fill(0.0);

        // State (der) rows: ∂der/∂[y|p]ᵀ · λ[0..state_count].
        self.accumulate_block_vjp(
            &self.derivative_scalar,
            t,
            solver_y,
            params,
            &lambda[..self.state_count],
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
        let algebraic_count = self.solver_count - self.state_count;
        if alg.len() != algebraic_count {
            return Err(RuntimeSolveError::solve_ir(format!(
                "steady-residual transpose: {} algebraic constraint rows but {} non-state \
                 solver-y slots (solver_count {} − state_count {}); the adjoint multiplier \
                 mapping λ[state_count + k] assumes one constraint per non-state slot",
                alg.len(),
                algebraic_count,
                self.solver_count,
                self.state_count
            )));
        }
        if !alg.is_empty() {
            let mu = self.scatter_algebraic_multipliers(&alg, lambda);
            self.accumulate_block_vjp(&self.implicit_scalar_rhs, t, solver_y, params, &mu, out)?;
        }
        Ok(())
    }

    /// Reverse a scalar program block and **accumulate** `(∂block/∂[solver_y|p])ᵀ ·
    /// cotangents` into `out` (`out[..solver_count]` = solver-y part, rest = `p`
    /// part). `out` is not cleared, so successive calls sum their contributions.
    fn accumulate_block_vjp(
        &self,
        block: &PreparedScalarProgramBlock,
        t: f64,
        solver_y: &[f64],
        params: &[f64],
        cotangents: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        let mut scratch = crate::reverse::ReverseScratch::default();
        let (cot_y, cot_p) = out.split_at_mut(self.solver_count);
        block
            .reverse_vjp(
                &crate::reverse::ReverseInputs {
                    y: solver_y,
                    p: params,
                    t,
                    context: self.row_eval_context(),
                },
                cotangents,
                &mut crate::reverse::ReverseCotangents {
                    y: cot_y,
                    p: cot_p,
                    seed: &mut [],
                },
                &mut scratch,
            )
            .map_err(RuntimeSolveError::from)
    }

    /// Build the implicit-residual row cotangent `μ` for the algebraic block: the
    /// k-th algebraic constraint row carries the multiplier `λ[state_count + k]`
    /// (the steady residual's algebraic-equation index); all other rows are zero.
    fn scatter_algebraic_multipliers(&self, alg_rows: &[usize], lambda: &[f64]) -> Vec<f64> {
        let mut mu = vec![0.0_f64; self.implicit_scalar_rhs.len()];
        for (k, &row) in alg_rows.iter().enumerate() {
            if let (Some(slot), Some(&value)) = (mu.get_mut(row), lambda.get(self.state_count + k))
            {
                *slot = value;
            }
        }
        mu
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
        self.eval_derivative_jacobian_v_with_seed_and_guess(
            lin,
            state,
            seed,
            seed_copy_len,
            None,
            out,
        )
    }

    fn eval_derivative_jacobian_v_with_seed_and_guess(
        &self,
        lin: AlgebraicLinearization<'_>,
        state: &[f64],
        seed: &[f64],
        seed_copy_len: usize,
        mut guess: Option<&mut Vec<f64>>,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        let AlgebraicLinearization { t, params, settle } = lin;
        validate_derivative_output_len(out, self.state_count)?;
        let mut scratch = self.derivative_scratch.borrow_mut();
        let StateDerivativeScratch {
            solver_y,
            seed_buf,
            unit_seed,
        } = &mut *scratch;
        // (1) Linearization point: project the algebraics from the state.
        if let Some(guess) = &mut guess {
            if guess.len() != self.solver_count {
                copy_runtime_values_into(guess, &self.model.initial_y, "Jacobian solver guess")?;
                resize_runtime_values(guess, self.solver_count, 0.0, "Jacobian solver guess")?;
            }
            solver_y.clear();
            solver_y.extend_from_slice(guess);
        }
        if guess.is_some() {
            self.overwrite_state_slots_preserving_algebraics(solver_y, state)?;
        } else {
            self.populate_solver_y_from_state(solver_y, state)?;
        }
        self.refresh_derivative_dependencies(t, solver_y, params, settle.tol, settle.max_iters)?;
        if let Some(guess) = guess {
            guess.copy_from_slice(solver_y);
        }
        // The JVP rows seed both solver-y and parameters (`SeedMode::SolverYAndP`),
        // so the seed vector spans `[solver-y | parameter]` space. We copy the
        // leading `seed_copy_len` entries from the caller (state-only for the
        // state Jacobian, the whole span for ∂der/∂p); the algebraic seeds are
        // then filled by the projection forward-sensitivity below.
        let seed_len = self
            .derivative_jacobian_v
            .requirements()
            .seed_len
            .max(self.implicit_jacobian_v.requirements().seed_len)
            .max(self.solver_count)
            .max(seed.len());
        seed_buf.clear();
        seed_buf.resize(seed_len, 0.0);
        let n = seed_copy_len.min(seed.len()).min(seed_buf.len());
        seed_buf[..n].copy_from_slice(&seed[..n]);
        unit_seed.clear();
        unit_seed.resize(seed_len, 0.0);
        // (2) Forward-propagate the seed through the algebraic projection.
        self.seed_refresh_derivative_dependencies(lin, solver_y, seed_buf, unit_seed)?;
        // (3) Total Jacobian-vector product via the derivative JVP.
        let context = RowEvalContext {
            seed: Some(seed_buf.as_slice()),
            ..self.row_eval_context()
        };
        self.derivative_jacobian_v
            .eval_with_context(solver_y, params, t, context, out)
            .map_err(Into::into)
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
        unit_seed: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.seed_refresh_with_plan(&self.derivative_refresh, lin, solver_y, seed, unit_seed)
    }

    /// Plan-parameterized forward-sensitivity refresh shared by the
    /// derivative-dependency projection ([`Self::seed_refresh_derivative_dependencies`])
    /// and the full solver-y projection ([`Self::project_state_sensitivity_to_solver_y`],
    /// which uses the complete `algebraic_refresh` plan so that *leaf* algebraics —
    /// ones that feed no state derivative, e.g. a pure output objective — also get
    /// their seed filled).
    pub(super) fn seed_refresh_with_plan(
        &self,
        plan: &RefreshPlan,
        lin: AlgebraicLinearization<'_>,
        solver_y: &[f64],
        seed: &mut [f64],
        unit_seed: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        let AlgebraicLinearization { t, params, .. } = lin;
        if plan.rows.is_empty() {
            return Ok(());
        }
        if !plan.iterative {
            // Causal single pass: each target is solved after its dependencies.
            for row in &plan.rows {
                self.seed_refresh_row(t, solver_y, params, seed, unit_seed, row)?;
            }
            return Ok(());
        }
        // Algebraic loop: the linearized seed equations form
        //     J_alg,alg * seed_alg = -J_alg,nonalg * seed_nonalg
        // at the already-settled value point. Solve that system directly instead
        // of using a fixed-point sweep: value refresh convergence does not imply
        // the seed fixed point contracts, while the direct linear solve is the
        // implicit-function theorem applied to the whole coupled block.
        self.seed_refresh_coupled_plan(plan, t, solver_y, params, seed, unit_seed)
    }

    fn seed_refresh_coupled_plan(
        &self,
        plan: &RefreshPlan,
        t: f64,
        solver_y: &[f64],
        params: &[f64],
        seed: &mut [f64],
        unit_seed: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        let saved_targets = self.zero_seed_refresh_targets(plan, seed)?;
        let result = self.seed_refresh_coupled_solution(plan, t, solver_y, params, seed, unit_seed);
        match result {
            Ok(solution) => {
                if let Err(error) = self.write_seed_refresh_solution(plan, &solution, seed) {
                    restore_seed_refresh_targets(plan, seed, &saved_targets);
                    return Err(error);
                }
                Ok(())
            }
            Err(error) => {
                restore_seed_refresh_targets(plan, seed, &saved_targets);
                Err(error)
            }
        }
    }

    fn zero_seed_refresh_targets(
        &self,
        plan: &RefreshPlan,
        seed: &mut [f64],
    ) -> Result<Vec<f64>, RuntimeSolveError> {
        let mut saved = Vec::new();
        reserve_runtime_vec_capacity(&mut saved, plan.rows.len(), "seed target snapshot")?;
        for row in &plan.rows {
            saved.push(seed[row.target_index]);
            seed[row.target_index] = 0.0;
        }
        Ok(saved)
    }

    fn seed_refresh_coupled_solution(
        &self,
        plan: &RefreshPlan,
        t: f64,
        solver_y: &[f64],
        params: &[f64],
        seed: &[f64],
        unit_seed: &mut [f64],
    ) -> Result<crate::linear_solve::AugmentedMatrix, RuntimeSolveError> {
        let n = plan.rows.len();
        let mut augmented =
            crate::linear_solve::AugmentedMatrix::zeroed(n).map_err(RuntimeSolveError::from)?;
        unit_seed.fill(0.0);
        for (row_pos, row) in plan.rows.iter().enumerate() {
            let off_diagonal =
                self.eval_refresh_residual_jacobian_row(row, solver_y, params, t, seed)?;
            self.validate_seed_jacobian_value(t, row, off_diagonal, "right-hand side")?;
            for (col_pos, target_row) in plan.rows.iter().enumerate() {
                let target = target_row.target_index;
                unit_seed[target] = 1.0;
                let coefficient =
                    self.eval_refresh_residual_jacobian_row(row, solver_y, params, t, unit_seed);
                unit_seed[target] = 0.0;
                let coefficient = coefficient?;
                self.validate_seed_jacobian_value(t, row, coefficient, "matrix coefficient")?;
                augmented.set(row_pos, col_pos, coefficient);
            }
            augmented.set(row_pos, n, -off_diagonal);
        }
        if crate::linear_solve::gaussian_eliminate(&mut augmented).is_none() {
            return Err(self.seed_refresh_singular_error(t, plan));
        }
        Ok(augmented)
    }

    fn write_seed_refresh_solution(
        &self,
        plan: &RefreshPlan,
        solution: &crate::linear_solve::AugmentedMatrix,
        seed: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        let n = plan.rows.len();
        for (row_pos, row) in plan.rows.iter().enumerate() {
            let value = solution.get(row_pos, n);
            if !value.is_finite() {
                return Err(RuntimeSolveError::solve_ir(format!(
                    "algebraic forward-sensitivity refresh solved non-finite seed for {}",
                    self.solver_name(row.target_index)
                )));
            }
            seed[row.target_index] = value;
        }
        Ok(())
    }

    fn validate_seed_jacobian_value(
        &self,
        t: f64,
        row: &AlgebraicRefreshRow,
        value: f64,
        role: &'static str,
    ) -> Result<(), RuntimeSolveError> {
        if value.is_finite() {
            return Ok(());
        }
        Err(RuntimeSolveError::solve_ir(format!(
            "algebraic forward-sensitivity refresh produced non-finite {role} from row {} for {} at t={t}",
            row.row_idx,
            self.solver_name(row.target_index)
        )))
    }

    /// Solve one residual row `g(y)=0` (which defines algebraic slot `target`) for
    /// its seed via the implicit-function theorem:
    /// `seed[target] = -(∂g/∂others · seed) / (∂g/∂target)`, both directional
    /// derivatives read from the per-row implicit JVP.
    fn seed_refresh_row(
        &self,
        t: f64,
        solver_y: &[f64],
        params: &[f64],
        seed: &mut [f64],
        unit_seed: &mut [f64],
        row: &AlgebraicRefreshRow,
    ) -> Result<(), RuntimeSolveError> {
        let target = row.target_index;
        // Off-diagonal term: JVP of the row with the target's own seed held at 0.
        let saved = seed[target];
        seed[target] = 0.0;
        let off_diagonal = self.eval_refresh_residual_jacobian_row(row, solver_y, params, t, seed);
        seed[target] = saved;
        let off_diagonal = off_diagonal?;
        self.validate_seed_jacobian_value(t, row, off_diagonal, "right-hand side")?;
        if self.refresh_row_subtracted_target_index(row) == Some(target) {
            seed[target] = off_diagonal;
            return Ok(());
        }
        if let Some(diagonal) = self.assignment_row_diagonal(row, solver_y, params, t)? {
            self.write_seed_refresh_from_diagonal(t, row, seed, off_diagonal, diagonal)?;
            return Ok(());
        }
        // Diagonal term ∂g/∂target via a unit seed isolated to the target slot.
        unit_seed[target] = 1.0;
        let diagonal = self.eval_refresh_residual_jacobian_row(row, solver_y, params, t, unit_seed);
        unit_seed[target] = 0.0;
        let diagonal = diagonal?;
        self.validate_seed_jacobian_value(t, row, diagonal, "diagonal coefficient")?;
        self.write_seed_refresh_from_diagonal(t, row, seed, off_diagonal, diagonal)
    }

    fn assignment_row_diagonal(
        &self,
        row: &AlgebraicRefreshRow,
        solver_y: &[f64],
        params: &[f64],
        t: f64,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        self.implicit_scalar_rhs
            .target_assignment_diagonal_unchecked_with_context(
                row.row_idx,
                row.target_index,
                solver_y,
                params,
                t,
                self.row_eval_context(),
            )
            .map_err(Into::into)
    }

    fn write_seed_refresh_from_diagonal(
        &self,
        t: f64,
        row: &AlgebraicRefreshRow,
        seed: &mut [f64],
        off_diagonal: f64,
        diagonal: f64,
    ) -> Result<(), RuntimeSolveError> {
        self.validate_seed_jacobian_value(t, row, diagonal, "diagonal coefficient")?;
        if diagonal.abs() <= SEED_DIAGONAL_EPS {
            return Err(self.seed_refresh_row_singular_error(t, row, diagonal));
        }
        seed[row.target_index] = -off_diagonal / diagonal;
        Ok(())
    }

    /// Directional derivative of the same residual that value refresh solves.
    /// Shapeless target-value rows are residualized as `raw - target`, so their
    /// JVP must subtract the target seed just like [`Self::refresh_row_residual`]
    /// subtracts the target value.
    fn eval_refresh_residual_jacobian_row(
        &self,
        row: &AlgebraicRefreshRow,
        solver_y: &[f64],
        params: &[f64],
        t: f64,
        seed: &[f64],
    ) -> Result<f64, RuntimeSolveError> {
        let raw = self.eval_implicit_jacobian_row(row, solver_y, params, t, seed)?;
        let Some(target) = self.refresh_row_subtracted_target_index(row) else {
            return Ok(raw);
        };
        let Some(target_seed) = seed.get(target).copied() else {
            return Err(RuntimeSolveError::solve_ir(format!(
                "algebraic forward-sensitivity seed length {} does not include residual target {}",
                seed.len(),
                self.solver_name(target)
            )));
        };
        Ok(raw - target_seed)
    }

    /// Directional derivative `∂g_row/∂y · seed` of implicit residual row
    /// `row_idx`, evaluated at `solver_y` with the given seed.
    fn eval_implicit_jacobian_row(
        &self,
        row: &AlgebraicRefreshRow,
        solver_y: &[f64],
        params: &[f64],
        t: f64,
        seed: &[f64],
    ) -> Result<f64, RuntimeSolveError> {
        self.implicit_jacobian_v
            .eval_row_output_unchecked_with_context(
                row.row_idx,
                row.output_offset,
                solver_y,
                params,
                t,
                RowEvalContext {
                    seed: Some(seed),
                    ..self.row_eval_context()
                },
            )
            .map_err(|err| {
                RuntimeSolveError::solve_ir(format!(
                    "implicit Jacobian seed row {} output {} failed: {err}",
                    row.row_idx, row.output_offset
                ))
            })
    }

    fn seed_refresh_singular_error(&self, t: f64, plan: &RefreshPlan) -> RuntimeSolveError {
        RuntimeSolveError::solve_ir(format!(
            "algebraic forward-sensitivity linear solve is singular at t={t}; targets={}",
            self.seed_refresh_target_names(plan)
        ))
    }

    fn seed_refresh_row_singular_error(
        &self,
        t: f64,
        row: &AlgebraicRefreshRow,
        diagonal: f64,
    ) -> RuntimeSolveError {
        RuntimeSolveError::solve_ir(format!(
            "algebraic forward-sensitivity row {} is singular for {} at t={t}; diagonal={diagonal:.6e}",
            row.row_idx,
            self.solver_name(row.target_index)
        ))
    }

    fn seed_refresh_target_names(&self, plan: &RefreshPlan) -> String {
        let mut names = plan
            .rows
            .iter()
            .take(8)
            .map(|row| self.solver_name(row.target_index).to_string())
            .collect::<Vec<_>>();
        if plan.rows.len() > names.len() {
            names.push(format!("... +{}", plan.rows.len() - names.len()));
        }
        names.join(", ")
    }
}

fn restore_seed_refresh_targets(plan: &RefreshPlan, seed: &mut [f64], saved: &[f64]) {
    for (row, value) in plan.rows.iter().zip(saved) {
        seed[row.target_index] = *value;
    }
}
