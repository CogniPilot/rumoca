use super::*;
use crate::{SolveFiniteCheck, SolvePureCallTable, SolveValue};
use rumoca_core::SourceId;

fn at(offset: usize) -> Span {
    Span::from_offsets(
        SourceId::from_source_name("TargetDomain.mo"),
        offset,
        offset + 1,
    )
}

#[test]
fn target_domain_refusal_retains_operation_and_original_source() {
    let profile = SolvePureCallTable::default().arithmetic();
    let program = TypedProgram::construct(profile, |b| {
        let value = b.constant(SolveValue::real(profile, 1.0), at(1))?;
        b.check_finite(value, SolveFiniteCheck::Derivative, at(2))
    })
    .unwrap();
    for target in [SolveFunctionTarget::CFmi, SolveFunctionTarget::Wasm] {
        let error = target.admit_finite_domain(&program).unwrap_err();
        assert_eq!(error.operation(), 1);
        assert!(error.region_path().is_empty());
        assert_eq!(error.provenance(), at(2));
        assert_eq!(error.opcode(), "check_finite");
    }
}

#[test]
fn target_domain_refuses_an_unselected_nested_guard_with_its_region_path() {
    let profile = SolvePureCallTable::default().arithmetic();
    let program = TypedProgram::construct(profile, |b| {
        let condition = b.constant(SolveValue::boolean(true), at(1))?;
        b.conditional(
            condition,
            &[],
            Vec::new(),
            at(2),
            |_, _, _| Ok(()),
            |arm, _, _| {
                let value = arm.constant(SolveValue::real(profile, 1.0), at(3))?;
                arm.check_finite(value, SolveFiniteCheck::Derivative, at(4))
            },
        )?;
        Ok(())
    })
    .unwrap();
    for target in [SolveFunctionTarget::CFmi, SolveFunctionTarget::Wasm] {
        let error = target.admit_finite_domain(&program).unwrap_err();
        assert_eq!(error.operation(), 1);
        assert_eq!(error.region_path(), &[(1, 1)]);
        assert_eq!(error.provenance(), at(4));
    }
}

#[test]
fn target_domain_admission_borrows_the_exact_unguarded_ieee_program() {
    let profile = SolvePureCallTable::default().arithmetic();
    let program = TypedProgram::construct(profile, |b| {
        b.constant(SolveValue::real(profile, f64::INFINITY), at(1))?;
        Ok(())
    })
    .unwrap();
    let before = serde_json::to_value(&program).unwrap();
    for target in [SolveFunctionTarget::CFmi, SolveFunctionTarget::Wasm] {
        let admitted = target.admit_finite_domain(&program).unwrap();
        assert!(std::ptr::eq(admitted.program(), &program));
        assert_eq!(admitted.target(), target);
    }
    assert_eq!(serde_json::to_value(&program).unwrap(), before);
}
