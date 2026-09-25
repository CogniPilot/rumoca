//! Defaults a direct `rumoca sim` run takes from the model.

/// The end time of a direct run: `--t-end` if given, else the model's
/// `experiment(StopTime)` when it is finite and after the default start of 0,
/// else 1.0. `experiment(Tolerance)` and `experiment(Solver)` are honored the
/// same way.
pub(super) fn direct_sim_t_end(t_end: Option<f64>, stop_time: Option<f64>) -> f64 {
    t_end
        .or(stop_time.filter(|stop| stop.is_finite() && *stop > 0.0))
        .unwrap_or(1.0)
}
