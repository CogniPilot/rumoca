//! Defaults a direct `rumoca sim` or `rumoca sim bench` run takes from the model.

/// The `(t_start, t_end)` window of a direct run. It starts at the model's
/// `experiment(StartTime)` when finite, else 0. It ends at `--t-end` if given,
/// else at the model's `experiment(StopTime)` when finite and after the start,
/// else one time unit after the start. `experiment(Tolerance)` and
/// `experiment(Solver)` are honored the same way.
pub(crate) fn direct_sim_window(
    t_end: Option<f64>,
    start_time: Option<f64>,
    stop_time: Option<f64>,
) -> (f64, f64) {
    let t_start = start_time.filter(|start| start.is_finite()).unwrap_or(0.0);
    let t_end = t_end
        .or(stop_time.filter(|stop| stop.is_finite() && *stop > t_start))
        .unwrap_or(t_start + 1.0);
    (t_start, t_end)
}

/// The window of a direct `rumoca sim` run of `result`.
pub(super) fn sim_window(
    args: &super::SimCommandArgs,
    result: &crate::DaeCompilationResult,
) -> (f64, f64) {
    direct_sim_window(
        args.t_end,
        result.experiment_start_time,
        result.experiment_stop_time,
    )
}

#[cfg(test)]
mod tests {
    use super::direct_sim_window;

    #[test]
    fn the_window_follows_the_experiment_unless_overridden() {
        assert_eq!(direct_sim_window(None, None, None), (0.0, 1.0));
        assert_eq!(direct_sim_window(None, None, Some(2.5)), (0.0, 2.5));
        assert_eq!(direct_sim_window(None, Some(1.0), Some(3.0)), (1.0, 3.0));
        assert_eq!(
            direct_sim_window(Some(4.0), Some(1.0), Some(3.0)),
            (1.0, 4.0)
        );
        // A stop at or before the start is not a window; one unit follows it.
        assert_eq!(direct_sim_window(None, Some(2.0), Some(2.0)), (2.0, 3.0));
        assert_eq!(
            direct_sim_window(None, Some(f64::NAN), Some(0.0)),
            (0.0, 1.0)
        );
    }
}
