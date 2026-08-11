use std::num::NonZeroU64;

use rumoca_core::{SourceId, Span, StructuredIndexBinder, StructuredIndexDomain};
use rumoca_ir_solve::{
    LinearOp, ProgramTensorViewAxis, ScalarProgramBlock, SolveArithmeticProfile,
    SolveBinaryOperator, SolveCompareOperator, SolveConversionOperator, SolveIntegerDomain,
    SolvePureCallIdentity, SolvePureCallOutput, SolvePureCallTable, SolveRealFormat,
    SolveReductionOperator, SolveRoundingMode, SolveScalarType, SolveUnaryOperator, SolveValue,
    SolveValueKind, SolveValueType,
};

use super::{
    TypedProgramEvalError, TypedValue, eval_pure_call, eval_pure_call_directional,
    eval_pure_call_with_invocation_counts,
};

fn span(start: usize) -> Span {
    Span::from_offsets(
        SourceId::from_source_name("typed_program_eval.mo"),
        start,
        start + 1,
    )
}

fn profile(format: SolveRealFormat) -> SolveArithmeticProfile {
    SolveArithmeticProfile::construct(
        format,
        SolveRoundingMode::NearestTiesToEven,
        SolveIntegerDomain::construct(i64::MIN, i64::MAX).unwrap(),
    )
}

fn identity(value: u64) -> SolvePureCallIdentity {
    SolvePureCallIdentity::issued(NonZeroU64::new(value).unwrap())
}

fn real_kind(format: SolveRealFormat, value: f64) -> SolveValueKind {
    match format {
        SolveRealFormat::Binary32 => SolveValueKind::Real32((value as f32).to_bits()),
        SolveRealFormat::Binary64 => SolveValueKind::Real64(value.to_bits()),
    }
}

#[test]
fn scalar_program_invokes_checked_typed_owner_from_compact_ranges() {
    let arithmetic = profile(SolveRealFormat::Binary64);
    let real = SolveValueType::scalar(SolveScalarType::real(arithmetic));
    let table = SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            identity(90),
            vec![real.clone()],
            vec![SolvePureCallOutput::result(real.clone())],
            span(90),
            |builder, inputs, outputs| {
                let input = builder.load(inputs[0], span(91))?;
                let one = builder.constant(SolveValue::real(arithmetic, 1.0), span(92))?;
                let result = builder.binary(SolveBinaryOperator::Add, input, one, span(93))?;
                builder.store(outputs[0], result, span(94))
            },
        )?;
        Ok(())
    })
    .unwrap();
    let site = table.owners()[0].call_site();
    let block = ScalarProgramBlock::with_program_spans(
        vec![vec![
            LinearOp::LoadY { dst: 0, index: 0 },
            LinearOp::PureCall {
                dst_start: 1,
                input_starts: Box::new([0]),
                site,
            },
            LinearOp::StoreOutput { src: 1 },
        ]],
        vec![span(95)],
    )
    .unwrap();
    let mut output = [0.0];

    crate::eval_scalar_program_block_with_context(
        &block,
        &[2.0],
        &[],
        0.0,
        crate::RowEvalContext {
            pure_calls: Some(&table),
            ..Default::default()
        },
        &mut output,
    )
    .unwrap();

    assert_eq!(output, [3.0]);
}

#[test]
fn nested_call_evaluates_one_compact_aggregate_and_assertion_tuple() {
    let arithmetic = profile(SolveRealFormat::Binary64);
    let vector = SolveValueType::tensor(SolveScalarType::real(arithmetic), vec![2]).unwrap();
    let table = SolvePureCallTable::construct(arithmetic, |table| {
        let inner = table.add_owner(
            identity(1),
            vec![vector.clone()],
            vec![
                SolvePureCallOutput::result(vector.clone()),
                SolvePureCallOutput::assertion_predicate(),
            ],
            span(0),
            |builder, inputs, outputs| {
                let input = builder.load(inputs[0], span(1))?;
                let sine = builder.unary(SolveUnaryOperator::Sin, input, span(2))?;
                let second = builder.project_element(sine, vec![1], span(3))?;
                let zero = builder.constant(SolveValue::real(arithmetic, 0.0), span(4))?;
                let positive =
                    builder.compare(SolveCompareOperator::Greater, second, zero, span(5))?;
                builder.store(outputs[0], sine, span(6))?;
                builder.store(outputs[1], positive, span(7))
            },
        )?;
        table.add_owner(
            identity(2),
            vec![vector.clone()],
            vec![
                SolvePureCallOutput::result(vector.clone()),
                SolvePureCallOutput::assertion_predicate(),
            ],
            span(8),
            |builder, inputs, outputs| {
                let input = builder.load(inputs[0], span(9))?;
                let result = builder.call(inner, &[input], span(10))?;
                builder.store(outputs[0], result[0], span(11))?;
                builder.store(outputs[1], result[1], span(12))
            },
        )?;
        Ok(())
    })
    .unwrap();
    let input = TypedValue::construct(
        vector,
        vec![
            real_kind(SolveRealFormat::Binary64, 0.0),
            real_kind(SolveRealFormat::Binary64, 1.0),
        ],
    )
    .unwrap();

    let output = eval_pure_call(&table, table.owners()[1].id(), &[input]).unwrap();

    assert_eq!(output.len(), 2);
    let [
        SolveValueKind::Real64(first),
        SolveValueKind::Real64(second),
    ] = output[0].elements()
    else {
        panic!("vector result must retain two binary64 elements");
    };
    assert_eq!(f64::from_bits(*first), 0.0);
    assert_eq!(f64::from_bits(*second), 1.0_f64.sin());
    assert_eq!(output[1].elements(), [SolveValueKind::Boolean(true)]);
}

#[test]
fn conditional_projections_share_one_issued_call_invocation() {
    let arithmetic = profile(SolveRealFormat::Binary64);
    let boolean = SolveValueType::scalar(SolveScalarType::Boolean);
    let real = SolveValueType::scalar(SolveScalarType::real(arithmetic));
    let table = SolvePureCallTable::construct(arithmetic, |table| {
        let child = table.add_owner(
            identity(80),
            vec![real.clone()],
            vec![SolvePureCallOutput::result(real.clone())],
            span(80),
            |builder, inputs, outputs| {
                let input = builder.load(inputs[0], span(81))?;
                let two = builder.constant(SolveValue::real(arithmetic, 2.0), span(82))?;
                let doubled =
                    builder.binary(SolveBinaryOperator::Multiply, input, two, span(83))?;
                builder.store(outputs[0], doubled, span(84))
            },
        )?;
        table.add_owner(
            identity(81),
            vec![boolean.clone(), real.clone()],
            vec![SolvePureCallOutput::result(real.clone())],
            span(85),
            |builder, inputs, outputs| {
                let condition = builder.load(inputs[0], span(86))?;
                let input = builder.load(inputs[1], span(87))?;
                let mut select_call = |at| {
                    builder.conditional(
                        condition,
                        &[input],
                        vec![real.clone()],
                        span(at),
                        |region, inputs, outputs| {
                            let input = region.load(inputs[0], span(at + 1))?;
                            let called = region.call(child, &[input], span(at + 2))?;
                            region.store(outputs[0], called[0], span(at + 3))
                        },
                        |region, inputs, outputs| {
                            let input = region.load(inputs[0], span(at + 4))?;
                            region.store(outputs[0], input, span(at + 5))
                        },
                    )
                };
                let first = select_call(88)?;
                let second = select_call(94)?;
                let sum =
                    builder.binary(SolveBinaryOperator::Add, first[0], second[0], span(100))?;
                builder.store(outputs[0], sum, span(101))
            },
        )?;
        Ok(())
    })
    .unwrap();
    let condition = |value| {
        TypedValue::construct(boolean.clone(), vec![SolveValueKind::Boolean(value)]).unwrap()
    };
    let value =
        TypedValue::construct(real, vec![real_kind(SolveRealFormat::Binary64, 3.0)]).unwrap();

    let (active, active_counts) = eval_pure_call_with_invocation_counts(
        &table,
        table.owners()[1].id(),
        &[condition(true), value.clone()],
    )
    .unwrap();
    let (inactive, inactive_counts) = eval_pure_call_with_invocation_counts(
        &table,
        table.owners()[1].id(),
        &[condition(false), value],
    )
    .unwrap();

    assert_eq!(
        active[0].elements(),
        [real_kind(SolveRealFormat::Binary64, 12.0)]
    );
    assert_eq!(
        inactive[0].elements(),
        [real_kind(SolveRealFormat::Binary64, 6.0)]
    );
    assert_eq!(active_counts[0], 1, "one exact child invocation is issued");
    assert_eq!(inactive_counts[0], 0, "inactive child calls remain lazy");
}

#[test]
fn runtime_tensor_selection_owns_one_based_conversion_and_fallback() {
    let arithmetic = profile(SolveRealFormat::Binary64);
    let matrix = SolveValueType::tensor(SolveScalarType::real(arithmetic), vec![2, 3]).unwrap();
    let integer = SolveValueType::scalar(SolveScalarType::integer(arithmetic));
    let real = SolveValueType::scalar(SolveScalarType::real(arithmetic));
    let table = SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            identity(3),
            vec![matrix.clone(), integer.clone(), integer.clone()],
            vec![SolvePureCallOutput::result(real)],
            span(20),
            |builder, inputs, outputs| {
                let matrix = builder.load(inputs[0], span(21))?;
                let row = builder.load(inputs[1], span(22))?;
                let column = builder.load(inputs[2], span(23))?;
                let fallback = builder.constant(SolveValue::real(arithmetic, -99.0), span(24))?;
                let selected =
                    builder.select_element(matrix, &[row, column], fallback, span(25))?;
                builder.store(outputs[0], selected, span(26))
            },
        )?;
        Ok(())
    })
    .unwrap();
    let matrix = TypedValue::construct(
        matrix,
        (1..=6)
            .map(|value| real_kind(SolveRealFormat::Binary64, f64::from(value)))
            .collect(),
    )
    .unwrap();
    let integer_value = |value| {
        TypedValue::construct(integer.clone(), vec![SolveValueKind::Integer(value)]).unwrap()
    };
    let owner = table.owners()[0].id();

    let selected = eval_pure_call(
        &table,
        owner,
        &[matrix.clone(), integer_value(2), integer_value(3)],
    )
    .unwrap();
    let fallback =
        eval_pure_call(&table, owner, &[matrix, integer_value(0), integer_value(1)]).unwrap();

    assert_eq!(
        selected[0].elements(),
        [real_kind(SolveRealFormat::Binary64, 6.0)]
    );
    assert_eq!(
        fallback[0].elements(),
        [real_kind(SolveRealFormat::Binary64, -99.0)]
    );
}

#[test]
fn compact_tensor_updates_preserve_one_operation_per_update() {
    let arithmetic = profile(SolveRealFormat::Binary64);
    let matrix = SolveValueType::tensor(SolveScalarType::real(arithmetic), vec![3, 3]).unwrap();
    let block = SolveValueType::tensor(SolveScalarType::real(arithmetic), vec![2, 2]).unwrap();
    let scalar = SolveValueType::scalar(SolveScalarType::real(arithmetic));
    let integer = SolveValueType::scalar(SolveScalarType::integer(arithmetic));
    let table = SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            identity(9),
            vec![
                matrix.clone(),
                block.clone(),
                scalar.clone(),
                integer.clone(),
                integer.clone(),
            ],
            vec![
                SolvePureCallOutput::result(matrix.clone()),
                SolvePureCallOutput::result(scalar.clone()),
            ],
            span(90),
            |builder, inputs, outputs| {
                let matrix = builder.load(inputs[0], span(91))?;
                let block = builder.load(inputs[1], span(92))?;
                let scalar = builder.load(inputs[2], span(93))?;
                let row = builder.load(inputs[3], span(94))?;
                let column = builder.load(inputs[4], span(95))?;
                let selected = builder.project_element_dynamic(matrix, &[row, column], span(96))?;
                let with_block = builder.update_slice(matrix, block, vec![1, 1], span(96))?;
                let updated =
                    builder.update_element(with_block, scalar, &[row, column], span(97))?;
                builder.store(outputs[0], updated, span(98))?;
                builder.store(outputs[1], selected, span(99))
            },
        )?;
        Ok(())
    })
    .unwrap();
    let table: SolvePureCallTable =
        serde_json::from_str(&serde_json::to_string(&table).unwrap()).unwrap();
    let real_value = |value| real_kind(SolveRealFormat::Binary64, value);
    let matrix = TypedValue::construct(
        matrix,
        (0..9).map(|value| real_value(f64::from(value))).collect(),
    )
    .unwrap();
    let block = TypedValue::construct(
        block,
        (10..14).map(|value| real_value(f64::from(value))).collect(),
    )
    .unwrap();
    let scalar = TypedValue::construct(scalar, vec![real_value(99.0)]).unwrap();
    let index =
        || TypedValue::construct(integer.clone(), vec![SolveValueKind::Integer(1)]).unwrap();

    let outputs = eval_pure_call(
        &table,
        table.owners()[0].id(),
        &[matrix, block, scalar, index(), index()],
    )
    .unwrap();

    assert_eq!(
        outputs[0].elements(),
        [99.0, 1.0, 2.0, 3.0, 10.0, 11.0, 6.0, 12.0, 13.0].map(real_value)
    );
    assert_eq!(outputs[1].elements(), [real_value(0.0)]);
    let body = table.owners()[0].body();
    assert_eq!(
        body.operations()
            .iter()
            .filter(|operation| matches!(
                operation.operation(),
                rumoca_ir_solve::SolveOperation::UpdateElement { .. }
                    | rumoca_ir_solve::SolveOperation::UpdateSlice { .. }
                    | rumoca_ir_solve::SolveOperation::ProjectElementDynamic { .. }
            ))
            .count(),
        3
    );
}

#[test]
fn compact_mixed_tensor_view_projects_and_updates_one_column() {
    let arithmetic = profile(SolveRealFormat::Binary64);
    let matrix = SolveValueType::tensor(SolveScalarType::real(arithmetic), vec![3, 2]).unwrap();
    let vector = SolveValueType::tensor(SolveScalarType::real(arithmetic), vec![3]).unwrap();
    let integer = SolveValueType::scalar(SolveScalarType::integer(arithmetic));
    let table = SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            identity(10),
            vec![matrix.clone(), vector.clone(), integer.clone()],
            vec![
                SolvePureCallOutput::result(vector.clone()),
                SolvePureCallOutput::result(matrix.clone()),
            ],
            span(100),
            |builder, inputs, outputs| {
                let matrix = builder.load(inputs[0], span(101))?;
                let vector = builder.load(inputs[1], span(102))?;
                let column = builder.load(inputs[2], span(103))?;
                let axes = [
                    ProgramTensorViewAxis::Span {
                        origin: 0,
                        extent: 3,
                    },
                    ProgramTensorViewAxis::Index(column),
                ];
                let projected = builder.project_view(matrix, &axes, span(104))?;
                let updated = builder.update_view(matrix, vector, &axes, span(105))?;
                builder.store(outputs[0], projected, span(106))?;
                builder.store(outputs[1], updated, span(107))
            },
        )?;
        Ok(())
    })
    .unwrap();
    let table: SolvePureCallTable =
        serde_json::from_str(&serde_json::to_string(&table).unwrap()).unwrap();
    let real_value = |value| real_kind(SolveRealFormat::Binary64, value);
    let matrix = TypedValue::construct(
        matrix,
        [1.0, 2.0, 3.0, 4.0, 5.0, 6.0].map(real_value).to_vec(),
    )
    .unwrap();
    let vector = TypedValue::construct(vector, [9.0, 8.0, 7.0].map(real_value).to_vec()).unwrap();
    let column = TypedValue::construct(integer, vec![SolveValueKind::Integer(2)]).unwrap();

    let outputs =
        eval_pure_call(&table, table.owners()[0].id(), &[matrix, vector, column]).unwrap();

    assert_eq!(outputs[0].elements(), [2.0, 4.0, 6.0].map(real_value));
    assert_eq!(
        outputs[1].elements(),
        [1.0, 9.0, 3.0, 8.0, 5.0, 7.0].map(real_value)
    );
}

#[test]
fn binary32_rounds_at_each_typed_operation() {
    let arithmetic = profile(SolveRealFormat::Binary32);
    let real = SolveValueType::scalar(SolveScalarType::real(arithmetic));
    let table = SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            identity(4),
            vec![real.clone(), real.clone()],
            vec![SolvePureCallOutput::result(real.clone())],
            span(30),
            |builder, inputs, outputs| {
                let lhs = builder.load(inputs[0], span(31))?;
                let rhs = builder.load(inputs[1], span(32))?;
                let sum = builder.binary(SolveBinaryOperator::Add, lhs, rhs, span(33))?;
                builder.store(outputs[0], sum, span(34))
            },
        )?;
        Ok(())
    })
    .unwrap();
    let value = |value| {
        TypedValue::construct(
            real.clone(),
            vec![real_kind(SolveRealFormat::Binary32, value)],
        )
        .unwrap()
    };

    let output = eval_pure_call(
        &table,
        table.owners()[0].id(),
        &[value(16_777_216.0), value(1.0)],
    )
    .unwrap();

    assert_eq!(
        output[0].elements(),
        [real_kind(SolveRealFormat::Binary32, 16_777_216.0)]
    );
}

#[test]
fn real_to_integer_conversion_checks_the_declared_domain() {
    let arithmetic = SolveArithmeticProfile::construct(
        SolveRealFormat::Binary64,
        SolveRoundingMode::NearestTiesToEven,
        SolveIntegerDomain::construct(-10, 10).unwrap(),
    );
    let real = SolveValueType::scalar(SolveScalarType::real(arithmetic));
    let integer = SolveValueType::scalar(SolveScalarType::integer(arithmetic));
    let table = SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            identity(5),
            vec![real.clone()],
            vec![SolvePureCallOutput::result(integer)],
            span(40),
            |builder, inputs, outputs| {
                let input = builder.load(inputs[0], span(41))?;
                let converted = builder.convert(
                    SolveConversionOperator::RealToIntegerTowardZero,
                    input,
                    span(42),
                )?;
                builder.store(outputs[0], converted, span(43))
            },
        )?;
        Ok(())
    })
    .unwrap();
    let input =
        TypedValue::construct(real, vec![real_kind(SolveRealFormat::Binary64, 11.0)]).unwrap();

    let error = eval_pure_call(&table, table.owners()[0].id(), &[input]).unwrap_err();

    assert_eq!(
        error,
        TypedProgramEvalError::InvalidIntegerConversion {
            provenance: span(42)
        }
    );
}

#[test]
fn structured_conditional_executes_only_its_selected_checked_region() {
    let arithmetic = SolveArithmeticProfile::construct(
        SolveRealFormat::Binary64,
        SolveRoundingMode::NearestTiesToEven,
        SolveIntegerDomain::construct(-10, 10).unwrap(),
    );
    let boolean = SolveValueType::scalar(SolveScalarType::Boolean);
    let real = SolveValueType::scalar(SolveScalarType::real(arithmetic));
    let integer = SolveValueType::scalar(SolveScalarType::integer(arithmetic));
    let table = SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            identity(6),
            vec![boolean.clone(), real.clone()],
            vec![SolvePureCallOutput::result(integer.clone())],
            span(50),
            |builder, inputs, outputs| {
                let condition = builder.load(inputs[0], span(51))?;
                let capture = builder.load(inputs[1], span(52))?;
                let selected = builder.conditional(
                    condition,
                    &[capture],
                    vec![integer.clone()],
                    span(53),
                    |region, inputs, outputs| {
                        let value = region.load(inputs[0], span(54))?;
                        let converted = region.convert(
                            SolveConversionOperator::RealToIntegerTowardZero,
                            value,
                            span(55),
                        )?;
                        region.store(outputs[0], converted, span(56))
                    },
                    |region, _inputs, outputs| {
                        let seven = region
                            .constant(SolveValue::integer(arithmetic, 7).unwrap(), span(57))?;
                        region.store(outputs[0], seven, span(58))
                    },
                )?;
                builder.store(outputs[0], selected[0], span(59))
            },
        )?;
        Ok(())
    })
    .unwrap();
    let json = serde_json::to_string(&table).unwrap();
    let table: SolvePureCallTable = serde_json::from_str(&json).unwrap();
    let owner = table.owners()[0].id();
    let value =
        TypedValue::construct(real, vec![real_kind(SolveRealFormat::Binary64, 11.0)]).unwrap();
    let condition = |value| {
        TypedValue::construct(boolean.clone(), vec![SolveValueKind::Boolean(value)]).unwrap()
    };

    let inactive = eval_pure_call(&table, owner, &[condition(false), value.clone()]).unwrap();
    let active = eval_pure_call(&table, owner, &[condition(true), value]).unwrap_err();

    assert_eq!(inactive[0].elements(), [SolveValueKind::Integer(7)]);
    assert_eq!(
        active,
        TypedProgramEvalError::InvalidIntegerConversion {
            provenance: span(55)
        }
    );
}

#[test]
fn compact_fold_executes_one_transition_owner_over_the_checked_domain() {
    let arithmetic = profile(SolveRealFormat::Binary64);
    let integer = SolveValueType::scalar(SolveScalarType::integer(arithmetic));
    let domain = StructuredIndexDomain {
        binders: vec![StructuredIndexBinder {
            id: 7,
            display_name: "i".into(),
            lower: 1,
            upper: 100,
            step: 1,
        }],
    };
    let table = SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            identity(7),
            Vec::new(),
            vec![SolvePureCallOutput::result(integer.clone())],
            span(60),
            |builder, _inputs, outputs| {
                let zero =
                    builder.constant(SolveValue::integer(arithmetic, 0).unwrap(), span(61))?;
                let sum = builder.fold(
                    domain.clone(),
                    &[zero],
                    &[],
                    span(62),
                    |transition, carried, _captures, binders, outputs| {
                        let sum = transition.load(carried[0], span(63))?;
                        let index = transition.load(binders[0], span(64))?;
                        let next =
                            transition.binary(SolveBinaryOperator::Add, sum, index, span(65))?;
                        transition.store(outputs[0], next, span(66))
                    },
                )?;
                builder.store(outputs[0], sum[0], span(67))
            },
        )?;
        Ok(())
    })
    .unwrap();
    let json = serde_json::to_string(&table).unwrap();
    let table: SolvePureCallTable = serde_json::from_str(&json).unwrap();

    let output = eval_pure_call(&table, table.owners()[0].id(), &[]).unwrap();

    assert_eq!(output[0].elements(), [SolveValueKind::Integer(5_050)]);
    assert_eq!(
        table.owners()[0]
            .body()
            .operations()
            .iter()
            .filter(|operation| matches!(
                operation.operation(),
                rumoca_ir_solve::SolveOperation::Fold { .. }
            ))
            .count(),
        1
    );
}

#[test]
fn directional_fold_differentiates_only_the_selected_structured_region() {
    let arithmetic = profile(SolveRealFormat::Binary64);
    let real = SolveValueType::scalar(SolveScalarType::real(arithmetic));
    let domain = StructuredIndexDomain {
        binders: vec![StructuredIndexBinder {
            id: 8,
            display_name: "i".into(),
            lower: 1,
            upper: 3,
            step: 1,
        }],
    };
    let table = SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            identity(107),
            vec![real.clone()],
            vec![SolvePureCallOutput::result(real.clone())],
            span(610),
            |builder, inputs, outputs| {
                let initial = builder.load(inputs[0], span(611))?;
                let result = builder.fold(
                    domain,
                    &[initial],
                    &[],
                    span(612),
                    |transition, carried, _captures, binders, outputs| {
                        let carried = transition.load(carried[0], span(613))?;
                        let binder = transition.load(binders[0], span(614))?;
                        let three = transition
                            .constant(SolveValue::integer(arithmetic, 3).unwrap(), span(615))?;
                        let before_last = transition.compare(
                            SolveCompareOperator::Less,
                            binder,
                            three,
                            span(616),
                        )?;
                        let selected = transition.conditional(
                            before_last,
                            &[carried],
                            vec![real.clone()],
                            span(617),
                            |region, inputs, outputs| {
                                let value = region.load(inputs[0], span(618))?;
                                let two = region
                                    .constant(SolveValue::real(arithmetic, 2.0), span(619))?;
                                let value = region.binary(
                                    SolveBinaryOperator::Multiply,
                                    value,
                                    two,
                                    span(620),
                                )?;
                                region.store(outputs[0], value, span(621))
                            },
                            |region, inputs, outputs| {
                                let value = region.load(inputs[0], span(622))?;
                                let three = region
                                    .constant(SolveValue::real(arithmetic, 3.0), span(623))?;
                                let value = region.binary(
                                    SolveBinaryOperator::Multiply,
                                    value,
                                    three,
                                    span(624),
                                )?;
                                region.store(outputs[0], value, span(625))
                            },
                        )?;
                        transition.store(outputs[0], selected[0], span(626))
                    },
                )?;
                builder.store(outputs[0], result[0], span(627))
            },
        )?;
        Ok(())
    })
    .unwrap();
    let value = |number| {
        TypedValue::construct(
            real.clone(),
            vec![real_kind(SolveRealFormat::Binary64, number)],
        )
        .unwrap()
    };

    let outputs =
        eval_pure_call_directional(&table, table.owners()[0].id(), &[value(5.0), value(1.0)])
            .unwrap();

    assert_eq!(
        outputs[0].elements(),
        [real_kind(SolveRealFormat::Binary64, 60.0)]
    );
    assert_eq!(
        outputs[1].elements(),
        [real_kind(SolveRealFormat::Binary64, 12.0)]
    );
    let directional = table.owners()[0].directional().unwrap();
    assert!(
        directional
            .body()
            .operations()
            .iter()
            .any(|operation| matches!(
                operation.operation(),
                rumoca_ir_solve::SolveOperation::Fold { .. }
            ))
    );
}

#[test]
fn directional_scalar_guards_match_checked_ad_at_singular_values() {
    let arithmetic = profile(SolveRealFormat::Binary64);
    let real = SolveValueType::scalar(SolveScalarType::real(arithmetic));
    let value = |number| {
        TypedValue::construct(
            real.clone(),
            vec![real_kind(SolveRealFormat::Binary64, number)],
        )
        .unwrap()
    };
    let unary_cases = [
        (SolveUnaryOperator::Asin, 1.0, 0.0, 0.0),
        (SolveUnaryOperator::Acos, 1.0, 0.0, 0.0),
        (SolveUnaryOperator::Log, 0.0, 1.0, 0.0),
        (SolveUnaryOperator::Log10, 0.0, 1.0, 0.0),
        (SolveUnaryOperator::Sqrt, 0.0, 1.0, 0.0),
    ];
    for (case, (operator, primal, tangent, expected_tangent)) in unary_cases.into_iter().enumerate()
    {
        let table = SolvePureCallTable::construct(arithmetic, |table| {
            table.add_owner(
                identity(200 + case as u64),
                vec![real.clone()],
                vec![SolvePureCallOutput::result(real.clone())],
                span(800 + case * 10),
                |builder, inputs, outputs| {
                    let input = builder.load(inputs[0], span(801 + case * 10))?;
                    let result = builder.unary(operator, input, span(802 + case * 10))?;
                    builder.store(outputs[0], result, span(803 + case * 10))
                },
            )?;
            Ok(())
        })
        .unwrap();
        let outputs = eval_pure_call_directional(
            &table,
            table.owners()[0].id(),
            &[value(primal), value(tangent)],
        )
        .unwrap();
        assert_eq!(
            outputs[1].elements(),
            [real_kind(SolveRealFormat::Binary64, expected_tangent)]
        );
    }

    let binary_cases = [
        (SolveBinaryOperator::Divide, 0.0, 1.0, 0.0, 1.0, 0.0, 0.0),
        (SolveBinaryOperator::Power, 0.0, 2.0, 1.0, 0.0, 0.0, 2.0),
        (SolveBinaryOperator::Power, -2.0, 0.0, 2.0, 1.0, 4.0, 0.0),
    ];
    for (case, (operator, lhs, lhs_tangent, rhs, rhs_tangent, expected, expected_tangent)) in
        binary_cases.into_iter().enumerate()
    {
        let table = SolvePureCallTable::construct(arithmetic, |table| {
            table.add_owner(
                identity(300 + case as u64),
                vec![real.clone(), real.clone()],
                vec![SolvePureCallOutput::result(real.clone())],
                span(900 + case * 10),
                |builder, inputs, outputs| {
                    let lhs = builder.load(inputs[0], span(901 + case * 10))?;
                    let rhs = builder.load(inputs[1], span(902 + case * 10))?;
                    let result = builder.binary(operator, lhs, rhs, span(903 + case * 10))?;
                    builder.store(outputs[0], result, span(904 + case * 10))
                },
            )?;
            Ok(())
        })
        .unwrap();
        let outputs = eval_pure_call_directional(
            &table,
            table.owners()[0].id(),
            &[
                value(lhs),
                value(lhs_tangent),
                value(rhs),
                value(rhs_tangent),
            ],
        )
        .unwrap();
        assert_eq!(
            outputs[0].elements(),
            [real_kind(SolveRealFormat::Binary64, expected)]
        );
        assert_eq!(
            outputs[1].elements(),
            [real_kind(SolveRealFormat::Binary64, expected_tangent)]
        );
    }
}

#[test]
fn compact_tensor_algebra_evaluates_scale_transpose_product_and_reduction() {
    let arithmetic = profile(SolveRealFormat::Binary64);
    let matrix = SolveValueType::tensor(SolveScalarType::real(arithmetic), vec![2, 3]).unwrap();
    let vector = SolveValueType::tensor(SolveScalarType::real(arithmetic), vec![3]).unwrap();
    let result = SolveValueType::tensor(SolveScalarType::real(arithmetic), vec![2]).unwrap();
    let scalar = SolveValueType::scalar(SolveScalarType::real(arithmetic));
    let table = SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            identity(8),
            vec![matrix.clone(), vector.clone(), scalar.clone()],
            vec![
                SolvePureCallOutput::result(result.clone()),
                SolvePureCallOutput::result(scalar.clone()),
                SolvePureCallOutput::result(
                    SolveValueType::tensor(SolveScalarType::real(arithmetic), vec![3, 2]).unwrap(),
                ),
                SolvePureCallOutput::result(
                    SolveValueType::tensor(SolveScalarType::real(arithmetic), vec![3, 3]).unwrap(),
                ),
                SolvePureCallOutput::result(
                    SolveValueType::tensor(SolveScalarType::real(arithmetic), vec![3, 3]).unwrap(),
                ),
            ],
            span(70),
            |builder, inputs, outputs| {
                let matrix = builder.load(inputs[0], span(71))?;
                let vector = builder.load(inputs[1], span(72))?;
                let scalar = builder.load(inputs[2], span(73))?;
                let scaled = builder.scale(matrix, scalar, span(74))?;
                let transposed = builder.transpose(scaled, span(75))?;
                let product = builder.matrix_multiply(scaled, vector, span(76))?;
                let sum = builder.reduce(SolveReductionOperator::Sum, product, span(77))?;
                let identity = builder.identity(SolveScalarType::real(arithmetic), 3, span(78))?;
                let diagonal = builder.diagonal(vector, span(79))?;
                builder.store(outputs[0], product, span(79))?;
                builder.store(outputs[1], sum, span(80))?;
                builder.store(outputs[2], transposed, span(81))?;
                builder.store(outputs[3], identity, span(82))?;
                builder.store(outputs[4], diagonal, span(83))
            },
        )?;
        Ok(())
    })
    .unwrap();
    let real_value = |value| real_kind(SolveRealFormat::Binary64, value);
    let matrix =
        TypedValue::construct(matrix, (1..=6).map(|v| real_value(f64::from(v))).collect()).unwrap();
    let vector = TypedValue::construct(vector, vec![real_value(1.0); 3]).unwrap();
    let scalar = TypedValue::construct(scalar, vec![real_value(2.0)]).unwrap();

    let outputs =
        eval_pure_call(&table, table.owners()[0].id(), &[matrix, vector, scalar]).unwrap();

    assert_eq!(outputs[0].elements(), [real_value(12.0), real_value(30.0)]);
    assert_eq!(outputs[1].elements(), [real_value(42.0)]);
    assert_eq!(
        outputs[2].elements(),
        [
            real_value(2.0),
            real_value(8.0),
            real_value(4.0),
            real_value(10.0),
            real_value(6.0),
            real_value(12.0),
        ]
    );
    assert_eq!(
        outputs[3].elements(),
        [
            real_value(1.0),
            real_value(0.0),
            real_value(0.0),
            real_value(0.0),
            real_value(1.0),
            real_value(0.0),
            real_value(0.0),
            real_value(0.0),
            real_value(1.0),
        ]
    );
    assert_eq!(
        outputs[4].elements(),
        [1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0].map(real_value)
    );
}

#[test]
fn compact_tensor_directional_owner_preserves_aggregate_operations() {
    let arithmetic = profile(SolveRealFormat::Binary64);
    let matrix = SolveValueType::tensor(SolveScalarType::real(arithmetic), vec![2, 3]).unwrap();
    let vector = SolveValueType::tensor(SolveScalarType::real(arithmetic), vec![3]).unwrap();
    let result = SolveValueType::tensor(SolveScalarType::real(arithmetic), vec![2]).unwrap();
    let scalar = SolveValueType::scalar(SolveScalarType::real(arithmetic));
    let table = SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            identity(108),
            vec![matrix.clone(), vector.clone(), scalar.clone()],
            vec![
                SolvePureCallOutput::result(result.clone()),
                SolvePureCallOutput::result(scalar.clone()),
            ],
            span(700),
            |builder, inputs, outputs| {
                let matrix = builder.load(inputs[0], span(701))?;
                let vector = builder.load(inputs[1], span(702))?;
                let scalar = builder.load(inputs[2], span(703))?;
                let scaled = builder.scale(matrix, scalar, span(704))?;
                let product = builder.matrix_multiply(scaled, vector, span(705))?;
                let sum = builder.reduce(SolveReductionOperator::Sum, product, span(706))?;
                builder.store(outputs[0], product, span(707))?;
                builder.store(outputs[1], sum, span(708))
            },
        )?;
        Ok(())
    })
    .unwrap();
    let owner = &table.owners()[0];
    let directional = owner.directional().expect("tensor owner is differentiable");
    assert!(
        directional
            .body()
            .operations()
            .iter()
            .any(|operation| matches!(
                operation.operation(),
                rumoca_ir_solve::SolveOperation::MatrixMultiply { .. }
            ))
    );
    let real_value = |value| real_kind(SolveRealFormat::Binary64, value);
    let typed = |value_type: SolveValueType, values: &[f64]| {
        TypedValue::construct(value_type, values.iter().copied().map(real_value).collect()).unwrap()
    };
    let arguments = [
        typed(matrix.clone(), &[1.0, 2.0, 3.0, 4.0, 5.0, 6.0]),
        typed(matrix, &[0.0; 6]),
        typed(vector.clone(), &[1.0; 3]),
        typed(vector, &[0.0; 3]),
        typed(scalar.clone(), &[2.0]),
        typed(scalar, &[1.0]),
    ];

    let outputs = eval_pure_call_directional(&table, owner.id(), &arguments).unwrap();

    assert_eq!(outputs[0].elements(), [12.0, 30.0].map(real_value));
    assert_eq!(outputs[1].elements(), [6.0, 15.0].map(real_value));
    assert_eq!(outputs[2].elements(), [42.0].map(real_value));
    assert_eq!(outputs[3].elements(), [21.0].map(real_value));
}

#[test]
fn compact_cross_product_evaluates_without_coordinate_operations() {
    let arithmetic = profile(SolveRealFormat::Binary64);
    let vector = SolveValueType::tensor(SolveScalarType::real(arithmetic), vec![3]).unwrap();
    let table = SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            identity(9),
            vec![vector.clone(), vector.clone()],
            vec![SolvePureCallOutput::result(vector.clone())],
            span(84),
            |builder, inputs, outputs| {
                let lhs = builder.load(inputs[0], span(85))?;
                let rhs = builder.load(inputs[1], span(86))?;
                let cross = builder.cross(lhs, rhs, span(87))?;
                builder.store(outputs[0], cross, span(88))
            },
        )?;
        Ok(())
    })
    .unwrap();
    let real_value = |value| real_kind(SolveRealFormat::Binary64, value);
    let lhs =
        TypedValue::construct(vector.clone(), [1.0, 2.0, 3.0].map(real_value).to_vec()).unwrap();
    let rhs = TypedValue::construct(vector, [4.0, 5.0, 6.0].map(real_value).to_vec()).unwrap();

    let outputs = eval_pure_call(&table, table.owners()[0].id(), &[lhs, rhs]).unwrap();

    assert_eq!(outputs[0].elements(), [-3.0, 6.0, -3.0].map(real_value));
    assert!(
        table.owners()[0]
            .body()
            .operations()
            .iter()
            .any(|operation| matches!(
                operation.operation(),
                rumoca_ir_solve::SolveOperation::Cross { .. }
            ))
    );
}

#[test]
fn compact_map_evaluates_one_checked_body_over_its_domain() {
    let arithmetic = profile(SolveRealFormat::Binary64);
    let scalar = SolveValueType::scalar(SolveScalarType::real(arithmetic));
    let vector = SolveValueType::tensor(SolveScalarType::real(arithmetic), vec![3]).unwrap();
    let domain = StructuredIndexDomain {
        binders: vec![StructuredIndexBinder {
            id: 0,
            display_name: "i".to_owned(),
            lower: 1,
            upper: 3,
            step: 1,
        }],
    };
    let table = SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            identity(11),
            vec![scalar.clone()],
            vec![SolvePureCallOutput::result(vector.clone())],
            span(110),
            |builder, inputs, outputs| {
                let scale = builder.load(inputs[0], span(111))?;
                let mapped = builder.map(
                    domain,
                    &[scale],
                    scalar.clone(),
                    span(112),
                    |builder, captures, binders, output| {
                        let scale = builder.load(captures[0], span(113))?;
                        let binder = builder.load(binders[0], span(114))?;
                        let binder = builder.convert(
                            SolveConversionOperator::IntegerToReal,
                            binder,
                            span(115),
                        )?;
                        let value = builder.binary(
                            SolveBinaryOperator::Multiply,
                            scale,
                            binder,
                            span(116),
                        )?;
                        builder.store(output, value, span(117))
                    },
                )?;
                builder.store(outputs[0], mapped, span(118))
            },
        )?;
        Ok(())
    })
    .unwrap();
    let real_value = |value| real_kind(SolveRealFormat::Binary64, value);
    let scale = TypedValue::construct(scalar, vec![real_value(2.0)]).unwrap();

    let outputs = eval_pure_call(&table, table.owners()[0].id(), &[scale]).unwrap();

    assert_eq!(outputs[0].elements(), [2.0, 4.0, 6.0].map(real_value));
}

#[test]
fn scalar_broadcast_power_evaluates_at_the_tensor_boundary() {
    let arithmetic = profile(SolveRealFormat::Binary64);
    let scalar = SolveValueType::scalar(SolveScalarType::real(arithmetic));
    let vector = SolveValueType::tensor(SolveScalarType::real(arithmetic), vec![3]).unwrap();
    let table = SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            identity(12),
            vec![vector.clone(), scalar.clone()],
            vec![SolvePureCallOutput::result(vector.clone())],
            span(120),
            |builder, inputs, outputs| {
                let vector = builder.load(inputs[0], span(121))?;
                let exponent = builder.load(inputs[1], span(122))?;
                let powered = builder.broadcast_binary(
                    SolveBinaryOperator::Power,
                    vector,
                    exponent,
                    false,
                    span(123),
                )?;
                builder.store(outputs[0], powered, span(124))
            },
        )?;
        Ok(())
    })
    .unwrap();
    let real_value = |value| real_kind(SolveRealFormat::Binary64, value);
    let vector = TypedValue::construct(vector, [2.0, 3.0, 4.0].map(real_value).to_vec()).unwrap();
    let exponent = TypedValue::construct(scalar, vec![real_value(2.0)]).unwrap();

    let outputs = eval_pure_call(&table, table.owners()[0].id(), &[vector, exponent]).unwrap();

    assert_eq!(outputs[0].elements(), [4.0, 9.0, 16.0].map(real_value));
}
