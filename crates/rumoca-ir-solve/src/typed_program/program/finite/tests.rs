use super::*;
use crate::{FaultClass, FaultKind, SolveLiveness, SolveOutputDemand};
use rumoca_core::SourceId;

fn at() -> Span {
    Span::from_offsets(SourceId::from_source_name("FiniteGuard.mo"), 1, 2)
}

#[test]
fn compact_finite_guard_survives_unused_output_demand_and_wire_replay() {
    let profile = crate::SolvePureCallTable::default().arithmetic();
    let program = TypedProgram::construct(profile, |b| {
        let scalar = b.constant(SolveValue::real(profile, 1.0), at())?;
        let tensor = b.fill(scalar, vec![1_000_000], at())?;
        b.check_finite(
            tensor,
            SolveFiniteCheck::Primal {
                program: 2,
                operation: 3,
            },
            at(),
        )
    })
    .unwrap();
    assert_eq!(program.operations().len(), 3);
    assert_eq!(
        program.region_fault_class(),
        FaultClass::MayFault(FaultKind::RealDomain)
    );
    let live = SolveLiveness::of(&program, &SolveOutputDemand::all());
    assert!(!live.is_dead(2));
    let wire = serde_json::to_value(&program).unwrap();
    let replay: TypedProgram = serde_json::from_value(wire).unwrap();
    assert_eq!(replay, program);
    assert_eq!(
        replay.fault_class(2),
        FaultClass::MayFault(FaultKind::RealDomain)
    );
}

#[test]
fn finite_guard_refuses_boolean_without_emitting_a_check() {
    let profile = crate::SolvePureCallTable::default().arithmetic();
    TypedProgram::construct(profile, |b| {
        let value = b.constant(SolveValue::boolean(true), at())?;
        let before = b.operations.len();
        assert_eq!(
            b.check_finite(value, SolveFiniteCheck::Derivative, at()),
            Err(SolveProgramConstructionError::TypeMismatch { provenance: at() })
        );
        assert_eq!(b.operations.len(), before);
        Ok(())
    })
    .unwrap();
}
