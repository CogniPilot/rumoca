use super::*;
use crate::{SolveScalarType, SolveSlotAccess, SolveStorageClass, SolveValue, SolveValueType};
use rumoca_core::{SourceId, Span};

fn span() -> Span {
    Span::from_offsets(SourceId::from_source_name("ScalarRelation.mo"), 1, 2)
}

fn input<'program>(
    builder: &mut TypedProgramBuilder<'program>,
) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
    let ty = SolveValueType::scalar(SolveScalarType::real(builder.arithmetic));
    let slot = builder.declare_slot(
        ty,
        SolveStorageClass::Input,
        SolveSlotAccess::ReadOnly,
        span(),
    )?;
    builder.load(slot, span())
}

#[test]
fn two_directions_share_the_issued_original_primal_occurrence() {
    let profile = crate::SolvePureCallTable::default().arithmetic();
    let mut originals = None;
    let program = TypedProgram::construct(profile, |builder| {
        let x = input(builder)?;
        let y = input(builder)?;
        let dx = input(builder)?;
        let dy = input(builder)?;
        let unary = builder.scalar_unary_directional_source(SolveUnaryOperator::Sin, x, span())?;
        let binary = builder.scalar_binary_directional_source(
            SolveBinaryOperator::Multiply,
            [x, y],
            span(),
        )?;
        originals = Some((unary.primal().id, binary.primal().id));
        for (dx, dy) in [(dx, dy), (dy, dx)] {
            assert!(
                builder
                    .scalar_unary_directional_tangent(unary, Some(dx))?
                    .is_some()
            );
            assert!(
                builder
                    .scalar_binary_directional_tangent(binary, [Some(dx), Some(dy)])?
                    .is_some()
            );
        }
        Ok(())
    })
    .unwrap();
    let (unary, binary) = originals.unwrap();
    assert_eq!(program.operations().iter().filter(|operation| {
        matches!(operation.operation(), SolveOperation::Unary { operator: SolveUnaryOperator::Sin, destination, .. } if *destination == unary)
    }).count(), 1);
    assert_eq!(program.operations().iter().filter(|operation| {
        matches!(operation.operation(), SolveOperation::Binary { operator: SolveBinaryOperator::Multiply, destination, .. } if *destination == binary)
    }).count(), 1);
}

#[test]
fn scalar_source_admission_refuses_aggregates_before_emitting_an_occurrence() {
    let profile = crate::SolvePureCallTable::default().arithmetic();
    TypedProgram::construct(profile, |builder| {
        let x = input(builder)?;
        let aggregate = builder.fill(x, vec![1_000_000], span())?;
        let before = builder.operations.len();
        for error in [
            builder
                .scalar_unary_directional_source(SolveUnaryOperator::Sin, aggregate, span())
                .unwrap_err(),
            builder
                .scalar_binary_directional_source(
                    SolveBinaryOperator::Multiply,
                    [x, aggregate],
                    span(),
                )
                .unwrap_err(),
        ] {
            assert_eq!(
                error,
                SolveProgramConstructionError::InvalidTensorAlgebra { provenance: span() }
            );
        }
        assert_eq!(builder.operations.len(), before);
        assert!(
            builder
                .scalar_unary_directional_source(SolveUnaryOperator::Sin, x, span())
                .is_ok()
        );
        Ok(())
    })
    .unwrap();
}

#[test]
fn typed_directions_refuse_a_foreign_scalar_kind_at_the_original_span() {
    let profile = crate::SolvePureCallTable::default().arithmetic();
    TypedProgram::construct(profile, |builder| {
        let x = input(builder)?;
        let wrong = builder.constant(SolveValue::boolean(true), span())?;
        let unary =
            builder.scalar_unary_directional_source(SolveUnaryOperator::Negate, x, span())?;
        let binary =
            builder.scalar_binary_directional_source(SolveBinaryOperator::Add, [x, x], span())?;
        for error in [
            builder
                .scalar_unary_directional_tangent(unary, Some(wrong))
                .unwrap_err(),
            builder
                .scalar_binary_directional_tangent(binary, [None, Some(wrong)])
                .unwrap_err(),
        ] {
            assert_eq!(error.source_span(), Some(span()));
            assert!(matches!(
                error,
                SolveProgramConstructionError::TypeMismatch { .. }
            ));
        }
        Ok(())
    })
    .unwrap();
}

#[test]
fn discrete_relations_have_no_numeric_direction() {
    let profile = crate::SolvePureCallTable::default().arithmetic();
    TypedProgram::construct(profile, |builder| {
        let value = builder.constant(SolveValue::boolean(true), span())?;
        let unary =
            builder.scalar_unary_directional_source(SolveUnaryOperator::Not, value, span())?;
        let binary = builder.scalar_binary_directional_source(
            SolveBinaryOperator::And,
            [value, value],
            span(),
        )?;
        assert!(
            builder
                .scalar_unary_directional_tangent(unary, None)?
                .is_none()
        );
        assert!(
            builder
                .scalar_binary_directional_tangent(binary, [None, None])?
                .is_none()
        );
        Ok(())
    })
    .unwrap();
}
