//! The committed algebraic seed of a continuous-time refresh (SPEC_0044
//! ME-PROJ-005), the warm-start rule of an importer-driven instance
//! (Solve IR `RefreshSeedRule::CommittedAcceptedPoint`; an integrator-driven
//! run keeps its integrator's warm start).
//!
//! An importer-driven refresh starts its Newton solves from the committed
//! seed, never from the importer's last trial evaluation, so its result is a
//! function of (t, x, p, relation memory, committed seed). The seed is the
//! settled coordinate at the last event exit or, after a completed integrator
//! step, the derivative refresh at the accepted point from the previous seed.
//! That refresh runs once, on the first refresh after the step, and a
//! derivative query at the accepted point reads its result. Event Mode keeps
//! its own iteration's warm start. `fmi3/me_projection.jinja`
//! `committed_seed` renders the same rule.

use super::*;

/// The seed and the accepted point waiting to become the next seed.
#[derive(Clone, Debug, Default)]
pub(super) struct CommittedSeed {
    active: bool,
    commit: Vec<f64>,
    pending: Option<SeedPoint>,
    resolved: Option<ResolvedPoint>,
}

#[derive(Clone, Debug)]
struct SeedPoint {
    time: f64,
    states: Vec<f64>,
}

#[derive(Clone, Debug)]
struct ResolvedPoint {
    point: SeedPoint,
    derivative: Vec<f64>,
}

impl SeedPoint {
    fn matches(&self, time: f64, states: &[f64]) -> bool {
        self.time.to_bits() == time.to_bits() && state_values_match(&self.states, states)
    }
}

impl SolveMeKernel {
    /// Event exit: the settled coordinate becomes the continuous-time seed.
    pub(super) fn commit_seed(&mut self) {
        // The Solve IR rule decides where continuous refreshes start.
        let seeded = match self
            .runtime
            .model()
            .problem
            .continuous
            .refresh_owners
            .seed_rule(self.refresh_executor)
        {
            rumoca_ir_solve::RefreshSeedRule::CommittedAcceptedPoint => true,
            rumoca_ir_solve::RefreshSeedRule::IntegratorWarmStart => false,
        };
        let seed = self.committed_seed.get_mut();
        seed.active = seeded;
        seed.commit.clone_from(&self.solver_y_guess.borrow());
        seed.pending = None;
        seed.resolved = None;
    }

    pub(super) fn seed_active(&self) -> bool {
        self.committed_seed.borrow().active
    }

    /// Event Mode keeps its own iteration's warm start.
    pub(super) fn suspend_seed(&mut self) {
        self.committed_seed.get_mut().active = false;
    }

    /// A completed integrator step marks the accepted point whose derivative
    /// refresh becomes the next seed. Values cached under the previous seed
    /// are not answers under the next one.
    pub(super) fn mark_seed(&mut self) {
        let time = self.continuous_eval_time();
        let seed = self.committed_seed.get_mut();
        if !seed.active {
            return;
        }
        seed.pending = Some(SeedPoint {
            time,
            states: self.states.clone(),
        });
        seed.resolved = None;
        self.clear_runtime_caches();
    }

    /// Run the pending accepted-point refresh from the current seed.
    pub(super) fn resolve_seed(&self) -> Result<(), MeError> {
        let pending = {
            let seed = self.committed_seed.borrow();
            if !seed.active {
                return Ok(());
            }
            match &seed.pending {
                Some(point) => point.clone(),
                None => return Ok(()),
            }
        };
        let mut guess = self.committed_seed.borrow().commit.clone();
        let derivative = self.derivative_refresh(&mut guess, pending.time, &pending.states)?;
        self.clear_runtime_caches();
        let mut seed = self.committed_seed.borrow_mut();
        seed.commit = guess;
        seed.pending = None;
        seed.resolved = Some(ResolvedPoint {
            point: pending,
            derivative,
        });
        Ok(())
    }

    /// Load the seed into the warm start of a continuous-time refresh at
    /// `(time, states)`. Returns the derivative already settled there when
    /// the point is the resolved accepted point.
    pub(super) fn load_seed(&self, time: f64) -> Result<Option<Vec<f64>>, MeError> {
        self.resolve_seed()?;
        let seed = self.committed_seed.borrow();
        if !seed.active {
            return Ok(None);
        }
        self.solver_y_guess.borrow_mut().clone_from(&seed.commit);
        Ok(seed
            .resolved
            .as_ref()
            .filter(|resolved| resolved.point.matches(time, &self.states))
            .map(|resolved| resolved.derivative.clone()))
    }
}

impl SolveMeKernel {
    /// The derivative refresh at `(time, states)` from `guess`, and the
    /// derivative there.
    fn derivative_refresh(
        &self,
        guess: &mut [f64],
        time: f64,
        states: &[f64],
    ) -> Result<Vec<f64>, MeError> {
        let settle = self.numerics_settle();
        let mut derivative = vec![0.0; self.state_count];
        self.with_delay_params_unseeded(time, states, |params| {
            self.runtime.eval_state_derivatives_with_guess_into(
                time,
                states,
                params,
                guess,
                settle.tol,
                settle.max_iters,
                &mut derivative,
            )
        })?
        .map_err(MeError::from)?;
        Ok(derivative)
    }
}
