use super::*;

#[test]
fn optional_variable_attributes_preserve_exact_identity_and_absence() {
    let source = TestSource::new("Real x; Real y;");
    let at = source.source("Real x", 0);
    let dae = Dae::construct(source.map, |dae| {
        let real = dae
            .types(|types| types.intern(TypeId::new(0), ValueType::scalar(ScalarType::Real), at))?;
        let values = dae.expressions(|expressions| {
            let mut values = [None; 5];
            for (value, literal) in values.iter_mut().zip([2.0, 3.0, 1.0, 5.0, 4.0]) {
                *value = Some(expressions.at(at).literal(DaeLiteral::Real(literal))?);
            }
            Ok(values)
        })?;
        dae.variables(|variables| {
            variables.algebraic(
                VarName::new("x"),
                rumoca_core::InstanceId::new(1),
                real,
                at,
                attributes(values),
            )?;
            variables.algebraic(
                VarName::new("y"),
                rumoca_core::InstanceId::new(2),
                real,
                at,
                attributes([None; 5]),
            )?;
            Ok(())
        })
    })
    .expect("distinct checked attribute expressions construct");

    assert_exact_attributes(&dae);
    let json = serde_json::to_string(&dae).expect("checked DAE serializes");
    let decoded: Dae = serde_json::from_str(&json).expect("wire replays constructors");
    assert_exact_attributes(&decoded);
}

fn assert_exact_attributes(dae: &Dae) {
    dae.inspect(|view| {
        for (variable_index, expected) in [
            (0, [Some(0), Some(1), Some(2), Some(3), Some(4)]),
            (1, [None; 5]),
        ] {
            let variable = view
                .variable(
                    view.variable_id(variable_index)
                        .expect("fixture variable exists"),
                )
                .expect("checked variable view exists");
            let actual = [
                variable.binding(),
                variable.start(),
                variable.minimum(),
                variable.maximum(),
                variable.nominal(),
            ];
            assert_eq!(actual.map(|value| value.map(ExprId::index)), expected);
        }
    });
}

fn attributes(values: [Option<ExprId<'_>>; 5]) -> VariableAttributes<'_> {
    let [binding, start, min, max, nominal] = values;
    VariableAttributes {
        component_ref: None,
        binding,
        start,
        fixed: Some(rumoca_core::Fixity::Free),
        min,
        max,
        nominal,
        unit: None,
        state_select: rumoca_core::StateSelect::Default,
        description: None,
        causality: VariableCausality::Local,
        is_tunable: false,
        is_held: false,
        origin: VariableOrigin::Source,
    }
}

#[test]
fn optional_string_attributes_preserve_absence() {
    let source = TestSource::new("String(1.0)");
    let at = source.source("String(1.0)", 0);
    let declaration = rumoca_core::DefId(73);
    let dae = Dae::construct(source.map, |dae| {
        dae.register_predefined_string(declaration)?;
        dae.expressions(|expressions| {
            let value = expressions.at(at).literal(DaeLiteral::Real(1.0))?;
            expressions.at(at).string_conversion(
                declaration,
                value,
                StringConversionFormatInput::Options {
                    minimum_length: None,
                    left_justified: None,
                    significant_digits: None,
                },
            )?;
            Ok(())
        })
    })
    .expect("omitted String options remain explicit absences");

    dae.inspect(|view| {
        let id = view.expression_id(1).expect("String conversion exists");
        let expression = view.expression(id).expect("checked expression exists");
        assert!(matches!(
            expression.operation(),
            ExpressionOperation::StringConversion {
                format: StringConversionFormatView::Options {
                    minimum_length: None,
                    left_justified: None,
                    significant_digits: None,
                },
                ..
            }
        ));
    });
}
