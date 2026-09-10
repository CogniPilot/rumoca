use super::*;

#[test]
fn mapped_tensor_directional_values_follow_domain_order() {
    let arithmetic = profile(SolveRealFormat::Binary64);
    let real = SolveValueType::scalar(SolveScalarType::real(arithmetic));
    let pair = SolveValueType::tensor(real.element_type(), vec![2]).unwrap();
    let result = SolveValueType::tensor(real.element_type(), vec![3, 2]).unwrap();
    let table = SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            identity(501),
            vec![real.clone()],
            vec![SolvePureCallOutput::result(result)],
            span(1000),
            |builder, inputs, outputs| {
                let input = builder.load(inputs[0], span(1001))?;
                let mapped = builder.map(
                    StructuredIndexDomain {
                        binders: vec![StructuredIndexBinder {
                            id: 0,
                            display_name: "i".into(),
                            lower: 5,
                            upper: 1,
                            step: -2,
                        }],
                    },
                    &[input],
                    pair,
                    span(1002),
                    mapped_pair,
                )?;
                builder.store(outputs[0], mapped, span(1003))
            },
        )?;
        Ok(())
    })
    .unwrap();
    let owner = &table.owners()[0];
    assert!(
        owner.directional().is_some(),
        "Map must have a checked directional owner"
    );
    let value = |number| {
        TypedValue::construct(
            real.clone(),
            vec![real_kind(SolveRealFormat::Binary64, number)],
        )
        .unwrap()
    };
    let outputs =
        eval_pure_call_directional(&table, owner.id(), &[value(3.0), value(2.0)]).unwrap();
    let kinds = |values: [f64; 6]| values.map(|v| real_kind(SolveRealFormat::Binary64, v));
    assert_eq!(
        outputs[0].elements(),
        kinds([15.0, 9.0, 9.0, 9.0, 3.0, 9.0])
    );
    assert_eq!(
        outputs[1].elements(),
        kinds([10.0, 12.0, 6.0, 12.0, 2.0, 12.0])
    );
}

fn mapped_pair<'program>(
    builder: &mut TypedProgramBuilder<'program>,
    captures: &[ProgramSlot<'program>],
    binders: &[ProgramSlot<'program>],
    output: ProgramSlot<'program>,
) -> Result<(), SolveProgramConstructionError> {
    let x = builder.load(captures[0], span(1010))?;
    let i = builder.load(binders[0], span(1011))?;
    let i = builder.convert(SolveConversionOperator::IntegerToReal, i, span(1012))?;
    let product = builder.binary(SolveBinaryOperator::Multiply, x, i, span(1013))?;
    let square = builder.binary(SolveBinaryOperator::Multiply, x, x, span(1014))?;
    let pair = builder.construct_aggregate(&[product, square], vec![2], span(1015))?;
    builder.store(output, pair, span(1016))
}

#[test]
fn mapped_predicates_keep_primal_control_without_tangent_outputs() {
    let arithmetic = profile(SolveRealFormat::Binary64);
    let real = SolveValueType::scalar(SolveScalarType::real(arithmetic));
    let booleans = SolveValueType::tensor(SolveScalarType::Boolean, vec![3]).unwrap();
    let table = SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            identity(502),
            vec![real.clone()],
            vec![SolvePureCallOutput::result(booleans)],
            span(1100),
            |builder, inputs, outputs| {
                let input = builder.load(inputs[0], span(1101))?;
                let mapped = builder.map(
                    StructuredIndexDomain {
                        binders: vec![StructuredIndexBinder {
                            id: 0,
                            display_name: "i".into(),
                            lower: 1,
                            upper: 3,
                            step: 1,
                        }],
                    },
                    &[input],
                    SolveValueType::scalar(SolveScalarType::Boolean),
                    span(1102),
                    mapped_predicate,
                )?;
                builder.store(outputs[0], mapped, span(1103))
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
        eval_pure_call_directional(&table, table.owners()[0].id(), &[value(2.5), value(100.0)])
            .unwrap();
    assert_eq!(outputs.len(), 1);
    assert_eq!(
        outputs[0].elements(),
        [true, true, false].map(SolveValueKind::Boolean)
    );
}

fn mapped_predicate<'program>(
    builder: &mut TypedProgramBuilder<'program>,
    captures: &[ProgramSlot<'program>],
    binders: &[ProgramSlot<'program>],
    output: ProgramSlot<'program>,
) -> Result<(), SolveProgramConstructionError> {
    let x = builder.load(captures[0], span(1110))?;
    let i = builder.load(binders[0], span(1111))?;
    let i = builder.convert(SolveConversionOperator::IntegerToReal, i, span(1112))?;
    let predicate = builder.compare(SolveCompareOperator::Less, i, x, span(1113))?;
    builder.store(output, predicate, span(1114))
}
