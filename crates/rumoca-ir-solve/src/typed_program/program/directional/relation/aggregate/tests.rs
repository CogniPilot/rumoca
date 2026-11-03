use super::*;
use crate::{SolveScalarType, SolveSlotAccess, SolveStorageClass, SolveValueType};
use rumoca_core::{SourceId, Span};

fn json_nodes(value: &serde_json::Value) -> usize {
    match value {
        serde_json::Value::Array(values) => 1 + values.iter().map(json_nodes).sum::<usize>(),
        serde_json::Value::Object(values) => 1 + values.values().map(json_nodes).sum::<usize>(),
        _ => 1,
    }
}

fn span() -> Span {
    Span::from_offsets(SourceId::from_source_name("AggregateRelation.mo"), 1, 2)
}

fn input<'program>(
    builder: &mut TypedProgramBuilder<'program>,
    ty: SolveValueType,
) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
    let slot = builder.declare_slot(
        ty,
        SolveStorageClass::Input,
        SolveSlotAccess::ReadOnly,
        span(),
    )?;
    builder.load(slot, span())
}

#[test]
fn aggregate_source_and_tangent_graphs_are_independent_of_element_extent() {
    let arithmetic = crate::SolvePureCallTable::default().arithmetic();
    let mut sizes = Vec::new();
    for count in [4, 1_000_000] {
        let ty = SolveValueType::tensor(SolveScalarType::real(arithmetic), vec![count]).unwrap();
        let program = TypedProgram::construct(arithmetic, |b| {
            let x = input(b, ty.clone())?;
            let dx = input(b, ty.clone())?;
            let unary =
                b.aggregate_unary_directional_source(SolveUnaryOperator::Sqrt, x, span())?;
            let binary =
                b.aggregate_binary_directional_source(SolveBinaryOperator::Power, [x, x], span())?;
            assert_eq!(b.value_type_of(unary.primal(), span())?, &ty);
            assert_eq!(b.value_type_of(binary.primal(), span())?, &ty);
            assert!(
                b.aggregate_unary_directional_tangent(unary, Some(dx))?
                    .is_some()
            );
            assert!(
                b.aggregate_binary_directional_tangent(binary, [Some(dx), Some(dx)])?
                    .is_some()
            );
            Ok(())
        })
        .unwrap();
        sizes.push((
            program.operations().len(),
            program.register_types().len(),
            json_nodes(&serde_json::to_value(&program).unwrap()),
        ));
    }
    assert_eq!(sizes[0].0, sizes[1].0);
    assert_eq!(sizes[0].1, sizes[1].1);
    assert_eq!(sizes[0].2, sizes[1].2);
}

#[test]
fn aggregate_sources_refuse_scalar_inputs_and_wrong_direction_shape() {
    let arithmetic = crate::SolvePureCallTable::default().arithmetic();
    let scalar = SolveValueType::scalar(SolveScalarType::real(arithmetic));
    let aggregate = SolveValueType::tensor(SolveScalarType::real(arithmetic), vec![3]).unwrap();
    TypedProgram::construct(arithmetic, |b| {
        let x = input(b, scalar)?;
        let values = input(b, aggregate)?;
        for error in [
            b.aggregate_unary_directional_source(SolveUnaryOperator::Sin, x, span()).unwrap_err(),
            b.aggregate_binary_directional_source(SolveBinaryOperator::Multiply, [values, x], span()).unwrap_err(),
        ] { assert_eq!(error.source_span(), Some(span())); }
        let unary = b.aggregate_unary_directional_source(SolveUnaryOperator::Sin, values, span())?;
        let binary = b.aggregate_binary_directional_source(SolveBinaryOperator::Multiply, [values, values], span())?;
        for error in [
            b.aggregate_unary_directional_tangent(unary, Some(x)).unwrap_err(),
            b.aggregate_binary_directional_tangent(binary, [None, Some(x)]).unwrap_err(),
        ] { assert!(matches!(error, SolveProgramConstructionError::TypeMismatch { provenance } if provenance == span())); }
        Ok(())
    }).unwrap();
}

#[test]
fn canonical_absent_and_present_aggregate_direction_roles_remain_distinct() {
    let arithmetic = crate::SolvePureCallTable::default().arithmetic();
    let ty = SolveValueType::tensor(SolveScalarType::real(arithmetic), vec![4]).unwrap();
    TypedProgram::construct(arithmetic, |b| {
        let x = input(b, ty.clone())?;
        let zero = b.constant(crate::SolveValue::real(arithmetic, 0.), span())?;
        let zeros = b.fill(zero, vec![4], span())?;
        let unary = b.aggregate_unary_directional_source(SolveUnaryOperator::Abs, x, span())?;
        let binary =
            b.aggregate_binary_directional_source(SolveBinaryOperator::Divide, [x, x], span())?;
        assert!(
            b.aggregate_unary_directional_tangent(unary, None)?
                .is_none()
        );
        assert!(
            b.aggregate_binary_directional_tangent(binary, [None, None])?
                .is_none()
        );
        assert!(
            b.aggregate_unary_directional_tangent(unary, Some(zeros))?
                .is_some()
        );
        assert!(
            b.aggregate_binary_directional_tangent(binary, [Some(zeros), Some(zeros)])?
                .is_some()
        );
        Ok(())
    })
    .unwrap();
}

#[test]
fn discrete_aggregate_sources_have_no_numeric_direction() {
    let arithmetic = crate::SolvePureCallTable::default().arithmetic();
    let ty = SolveValueType::tensor(SolveScalarType::Boolean, vec![4]).unwrap();
    TypedProgram::construct(arithmetic, |b| {
        let x = input(b, ty)?;
        let unary = b.aggregate_unary_directional_source(SolveUnaryOperator::Not, x, span())?;
        let binary =
            b.aggregate_binary_directional_source(SolveBinaryOperator::And, [x, x], span())?;
        assert!(
            b.aggregate_unary_directional_tangent(unary, None)?
                .is_none()
        );
        assert!(
            b.aggregate_binary_directional_tangent(binary, [None, None])?
                .is_none()
        );
        assert!(
            b.aggregate_unary_directional_tangent(unary, Some(x))
                .is_err()
        );
        Ok(())
    })
    .unwrap();
}
