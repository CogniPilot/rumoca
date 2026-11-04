use super::*;

#[test]
fn oversized_matrix_errors_propagate_without_allocating_or_reading_registers() {
    for (size, reason) in [(usize::MAX, "shape"), (usize::MAX.isqrt(), "capacity")] {
        let error = solve_component(&[], &[], 0, 0, size, 0).unwrap_err();
        let EvalSolveError::Scalarization { message, span } = error else {
            panic!("matrix extent errors retain the typed scalarization failure");
        };
        assert!(message.contains(reason), "{message}");
        assert!(message.contains("overflows"), "{message}");
        assert_eq!(span, None);
    }
}
