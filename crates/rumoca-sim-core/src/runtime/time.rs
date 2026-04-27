pub fn stop_time_reached_with_tol(t: f64, t_end: f64) -> bool {
    let tol = 1e-12 * (1.0 + t_end.abs());
    t >= t_end - tol
}

pub fn time_match_with_tol(a: f64, b: f64) -> bool {
    let tol = 1e-12 * (1.0 + a.abs().max(b.abs()));
    (a - b).abs() <= tol
}

pub fn time_advanced_with_tol(previous_t: f64, current_t: f64) -> bool {
    let tol = 1e-12 * (1.0 + previous_t.abs().max(current_t.abs()));
    current_t > previous_t + tol
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn stop_time_reached_uses_tolerance() {
        assert!(stop_time_reached_with_tol(1.0, 1.0));
        assert!(stop_time_reached_with_tol(0.999_999_999_999_9, 1.0));
        assert!(!stop_time_reached_with_tol(0.99, 1.0));
    }

    #[test]
    fn time_match_uses_symmetric_tolerance() {
        assert!(time_match_with_tol(1.0, 1.0));
        assert!(time_match_with_tol(1.0, 1.0 + 5.0e-13));
        assert!(!time_match_with_tol(1.0, 1.0 + 1.0e-6));
    }

    #[test]
    fn time_advanced_requires_progress_beyond_tolerance() {
        assert!(!time_advanced_with_tol(1.0, 1.0));
        assert!(!time_advanced_with_tol(1.0, 1.0 + 1.0e-13));
        assert!(time_advanced_with_tol(1.0, 1.0 + 1.0e-4));
    }
}
