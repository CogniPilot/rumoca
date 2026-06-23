use super::*;

#[test]
fn checked_usize_dims_to_i64_rejects_overflow_with_span() {
    let Some(dim) = usize::try_from(i64::MAX)
        .ok()
        .and_then(|value| value.checked_add(1))
    else {
        return;
    };
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_function_calls_tests_source_45.mo",
        ),
        2,
        10,
    );

    let err = checked_usize_dims_to_i64(&[dim], "function input actual shape", span)
        .expect_err("function-call dimensions must fit in Modelica integer range");

    assert_eq!(err.source_span(), Some(span));
    assert_eq!(
        err.reason(),
        format!(
            "invalid IR contract: function input actual shape dimension {dim} exceeds i64 range"
        )
    );
}
