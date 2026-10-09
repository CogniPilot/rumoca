//! Source-issued starts survive singleton alias classes without scalar writes.

use super::*;

fn fixed_array(count: usize) -> dae::Dae {
    let text = format!(
        "model Starts Real x[{count}](each start=-0.0, each fixed=true); equation der(x) = -x; end Starts;"
    );
    let source = TestSource::new(&text);
    let at = source.at(0, text.len());
    dae::Dae::construct(source.map, |model| {
        let ty = model.types(|types| {
            types.derived(
                dae::ValueType::array(dae::ScalarType::Real, [u32::try_from(count).unwrap()]),
                at,
            )
        })?;
        let start = model.expressions(|e| e.at(at).literal(dae::DaeLiteral::Real(-0.0)))?;
        let state = model.variables(|v| {
            v.state(
                VarName::new("x"),
                ty,
                at,
                dae::VariableAttributes {
                    start: Some(start),
                    fixed: Some(vec![true]),
                    ..Default::default()
                },
            )
        })?;
        let residual = model.expressions(|e| {
            let derivative = e
                .at(at)
                .coordinate(dae::CoordinateInput::Derivative(state))?;
            let state = e.at(at).coordinate(dae::CoordinateInput::State(state))?;
            let rhs = e.at(at).unary(dae::UnaryOperator::Negate, state)?;
            e.at(at)
                .binary(dae::BinaryOperator::Subtract, derivative, rhs)
        })?;
        model.continuous(|c| c.value_equation(at, residual))
    })
    .unwrap()
}

#[test]
fn fixed_array_start_without_alias_transfer_keeps_one_source_run() {
    for count in [12usize, 120] {
        let model = fixed_array(count);
        let lowered = crate::lower_solve_model(&model, &Default::default(), |_| {}).unwrap();
        let values = &lowered.model().initial_y;
        assert_eq!(values.len(), count);
        assert_eq!(
            values.run_count(),
            1,
            "singleton alias classes must not scalarize source starts"
        );
        assert!(!values.has_dense_view());
        for index in 0..count {
            assert_eq!(values.value(index).unwrap().to_bits(), (-0.0f64).to_bits());
        }
    }
}

#[test]
fn checked_model_wire_refuses_nonfinite_owner_bits_and_legacy_schema() {
    #[derive(serde::Deserialize)]
    struct Replay(
        #[serde(deserialize_with = "crate::deserialize_solve_model")] rumoca_ir_solve::SolveModel,
    );
    let dae = fixed_array(12);
    let mut model = crate::lower_solve_model(&dae, &Default::default(), |_| {})
        .unwrap()
        .into_model();
    let original = serde_json::to_value(crate::solve_model_wire(&model).unwrap()).unwrap();
    let replay: Replay = serde_json::from_value(original.clone()).unwrap();
    assert_eq!(replay.0.initial_y, model.initial_y);
    assert!(!replay.0.initial_y.has_dense_view());
    for value in [
        f64::INFINITY,
        f64::NEG_INFINITY,
        f64::from_bits(0x7ff8_0000_0000_1234),
    ] {
        let mut forged = original.clone();
        forged["initial_y"]["runs"][0]["bits"] = value.to_bits().into();
        assert!(serde_json::from_value::<Replay>(forged).is_err());
        model.initial_y.set(0, value).unwrap();
        assert!(crate::solve_model_wire(&model).is_err());
        assert!(!model.initial_y.has_dense_view());
    }
    let mut old = original;
    old["schema_version"] = 1.into();
    assert!(serde_json::from_value::<Replay>(old).is_err());
}
