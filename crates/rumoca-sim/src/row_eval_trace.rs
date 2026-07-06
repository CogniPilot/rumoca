//! Solve-row evaluation tracing controls for simulation diagnostics.

pub fn reset() {
    rumoca_eval_solve::reset_solve_row_eval_trace();
}

pub fn snapshot(label: &str) {
    rumoca_eval_solve::trace_solve_row_eval_snapshot(label);
}
