use super::*;
use rumoca_core::{SourceId, Span};

fn span() -> Span {
    Span::from_offsets(SourceId::from_source_name("SharedColumns.mo"), 1, 2)
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

fn column_program(extent: u32) -> TypedProgram {
    let arithmetic = crate::SolvePureCallTable::default().arithmetic();
    let ty = SolveValueType::tensor(SolveScalarType::real(arithmetic), vec![extent]).unwrap();
    TypedProgram::construct(arithmetic, |b| {
        let x = input(b, ty.clone())?;
        let dx = input(b, ty.clone())?;
        let dy = input(b, ty.clone())?;
        let source = b.aggregate_unary_directional_source(SolveUnaryOperator::Sin, x, span())?;
        let columns = b.aggregate_unary_directional_columns(source, &[Some(dx), None, Some(dy)])?;
        assert_eq!(columns.present(), [true, true, true]);
        let packed = columns
            .packed()
            .expect("present columns issue a packed value");
        assert_eq!(b.value_type_of(packed, span())?.dimensions(), [extent, 3]);
        Ok(())
    })
    .unwrap()
}

#[test]
fn aggregate_columns_share_one_coordinate_body_and_one_cosine_coefficient() {
    let program = column_program(4);
    let maps = program
        .operations()
        .iter()
        .filter_map(|op| match op.operation() {
            SolveOperation::Map { body, .. } => Some(body.body()),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(maps.len(), 1);
    assert_eq!(
        maps[0]
            .operations()
            .iter()
            .filter(|op| matches!(
                op.operation(),
                SolveOperation::Unary {
                    operator: SolveUnaryOperator::Cos,
                    ..
                }
            ))
            .count(),
        1
    );
    assert_eq!(
        program
            .operations()
            .iter()
            .filter(|op| matches!(
                op.operation(),
                SolveOperation::Unary {
                    operator: SolveUnaryOperator::Sin,
                    ..
                }
            ))
            .count(),
        1
    );
}

#[test]
fn aggregate_columns_graph_size_does_not_expand_tensor_coordinates() {
    let signature = |program: TypedProgram| {
        let mut ops = program.operations().len();
        let mut registers = program.register_types().len();
        for op in program.operations() {
            if let SolveOperation::Map { body, .. } = op.operation() {
                ops += body.body().operations().len();
                registers += body.body().register_types().len();
            }
        }
        (ops, registers)
    };
    assert_eq!(
        signature(column_program(4)),
        signature(column_program(1_000_000))
    );
}

#[test]
fn all_absent_columns_emit_no_map_but_present_zero_retains_its_column() {
    let arithmetic = crate::SolvePureCallTable::default().arithmetic();
    let ty = SolveValueType::tensor(SolveScalarType::real(arithmetic), vec![4]).unwrap();
    TypedProgram::construct(arithmetic, |b| {
        let x = input(b, ty)?;
        let zero = b.constant(SolveValue::real(arithmetic, -0.0), span())?;
        let zeros = b.fill(zero, vec![4], span())?;
        let source =
            b.aggregate_binary_directional_source(SolveBinaryOperator::Divide, [x, x], span())?;
        let before = b.operations.len();
        let absent = b.aggregate_binary_directional_columns(source, &[[None, None]; 2])?;
        assert!(absent.packed().is_none());
        assert_eq!(absent.present(), [false, false]);
        assert_eq!(b.operations.len(), before);
        let present =
            b.aggregate_binary_directional_columns(source, &[[Some(zeros), None], [None, None]])?;
        assert!(present.packed().is_some());
        assert_eq!(present.present(), [true, false]);
        Ok(())
    })
    .unwrap();
}

#[test]
fn invalid_column_shape_and_empty_width_refuse_before_emission() {
    let arithmetic = crate::SolvePureCallTable::default().arithmetic();
    let ty = SolveValueType::tensor(SolveScalarType::real(arithmetic), vec![4]).unwrap();
    TypedProgram::construct(arithmetic, |b| {
        let x = input(b, ty)?;
        let wrong = input(b, SolveValueType::scalar(SolveScalarType::real(arithmetic)))?;
        let unary = b.aggregate_unary_directional_source(SolveUnaryOperator::Abs, x, span())?;
        let binary =
            b.aggregate_binary_directional_source(SolveBinaryOperator::Power, [x, x], span())?;
        let before = b.operations.len();
        for error in [
            b.aggregate_unary_directional_columns(unary, &[])
                .unwrap_err(),
            b.aggregate_unary_directional_columns(unary, &[Some(wrong)])
                .unwrap_err(),
            b.aggregate_binary_directional_columns(binary, &[[None, Some(wrong)]])
                .unwrap_err(),
        ] {
            assert_eq!(error.source_span(), Some(span()));
        }
        assert_eq!(b.operations.len(), before);
        Ok(())
    })
    .unwrap();
}

#[test]
fn packed_extent_overflow_refuses_even_for_absent_columns() {
    let arithmetic = crate::SolvePureCallTable::default().arithmetic();
    let ty = SolveValueType::tensor(SolveScalarType::real(arithmetic), vec![u32::MAX]).unwrap();
    TypedProgram::construct(arithmetic, |b| {
        let x = input(b, ty)?;
        let source = b.aggregate_unary_directional_source(SolveUnaryOperator::Sin, x, span())?;
        let before = b.operations.len();
        let error = b
            .aggregate_unary_directional_columns(source, &[None, None])
            .unwrap_err();
        assert!(matches!(
            error,
            SolveProgramConstructionError::InvalidTensorAlgebra { .. }
        ));
        assert_eq!(b.operations.len(), before);
        Ok(())
    })
    .unwrap();
}

#[test]
fn canonical_materialized_zero_and_absent_result_roles_are_not_conflated() {
    let arithmetic = crate::SolvePureCallTable::default().arithmetic();
    let ty = SolveValueType::tensor(SolveScalarType::real(arithmetic), vec![4]).unwrap();
    TypedProgram::construct(arithmetic, |b| {
        let x = input(b, ty)?;
        let sine = b.aggregate_unary_directional_source(SolveUnaryOperator::Sin, x, span())?;
        let root = b.aggregate_unary_directional_source(SolveUnaryOperator::Sqrt, x, span())?;
        let sine_single = b.aggregate_unary_directional_tangent(sine, None)?;
        let root_single = b.aggregate_unary_directional_tangent(root, None)?;
        let sine_columns = b.aggregate_unary_directional_columns(sine, &[None])?;
        let root_columns = b.aggregate_unary_directional_columns(root, &[None])?;
        assert_eq!(sine_columns.present(), [sine_single.is_some()]);
        assert_eq!(root_columns.present(), [root_single.is_some()]);
        assert!(sine_columns.packed().is_some());
        assert!(root_columns.packed().is_none());
        Ok(())
    })
    .unwrap();
}
