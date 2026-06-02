use rumoca_solver::SolveStopSchedule;

use super::Rk45Backend;

#[derive(Clone)]
pub(super) struct Rk45ResetSnapshot {
    state: Vec<f64>,
    params: Vec<f64>,
    next_step: f64,
}

impl Rk45Backend<'_> {
    pub(super) fn reset_snapshot(&self) -> Rk45ResetSnapshot {
        Rk45ResetSnapshot {
            state: self.state.clone(),
            params: self.params.clone(),
            next_step: self.next_step,
        }
    }

    pub(super) fn reset_to_snapshot(&mut self, snapshot: &Rk45ResetSnapshot, t_start: f64) {
        self.time = t_start;
        self.state.clone_from(&snapshot.state);
        self.params.clone_from(&snapshot.params);
        self.next_step = snapshot.next_step;
        self.stop_schedule = SolveStopSchedule::new(&self.model.model.problem, t_start, self.t_end);
        self.termination = None;
        self.pending_root_crossings.clear();
        self.pending_event_pre_y = None;
        self.pending_event_pre_p = None;
        self.boundary_event_pre_y = None;
        self.boundary_event_pre_p = None;
        self.post_event_eval_time = None;
        self.solver_y_guess.borrow_mut().clear();
        self.clear_runtime_caches();
    }
}
