use super::*;
use rumoca_core::SourceId;

fn source_span() -> Span {
    Span::from_offsets(SourceId::from_source_name("selected_indices.mo"), 3, 17)
}

fn assert_overflow(error: LowerError, span: Span) {
    assert_eq!(error.code(), "EL005");
    assert_eq!(error.source_span(), Some(span));
    assert!(matches!(error, LowerError::ContractViolation { reason, .. }
        if reason == "selected projection register or capture overflow"));
}

#[test]
fn selected_projection_refuses_register_exhaustion_before_emitting_a_value() {
    let span = source_span();
    let mut region = Region::new(2, (u32::MAX - 1) as usize, None, span).unwrap();
    let register = region.value([2.0, 3.0]).unwrap();
    assert_eq!(register, u32::MAX - 1);
    let programs = region.family.programs.clone();
    assert_eq!(
        programs,
        vec![
            vec![solve::LinearOp::Const {
                dst: register,
                value: 2.0
            }],
            vec![solve::LinearOp::Const {
                dst: register,
                value: 3.0
            }],
        ]
    );
    assert_overflow(region.value([5.0, 7.0]).unwrap_err(), span);
    assert_eq!(region.family.programs, programs);
    assert_eq!(region.next, u32::MAX);
}

#[test]
fn selected_projection_refuses_capture_offset_overflow_without_partial_payload() {
    let span = source_span();
    let mut region = Region::new(2, 0, Some(usize::MAX - 1), span).unwrap();
    region.value([2.0, 3.0]).unwrap();
    region.value([5.0, 7.0]).unwrap();
    let programs = region.family.programs.clone();
    let captures = region.family.captures.clone();
    assert_eq!(captures, vec![vec![2.0, 5.0], vec![3.0, 7.0]]);
    assert_overflow(region.value([11.0, 13.0]).unwrap_err(), span);
    assert_eq!(region.family.programs, programs);
    assert_eq!(region.family.captures, captures);
}
