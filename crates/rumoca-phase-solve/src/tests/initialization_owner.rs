//! Source-issued starts survive singleton alias classes without scalar writes.

use super::*;
use rumoca_ir_solve as solve;

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

/// The same source broadcast changes its extent, not its declaration metadata.
#[test]
fn fmi_broadcast_metadata_wire_retains_declaration_sized_ownership() {
    let mut sizes = Vec::new();
    for count in [64usize, 512] {
        let dae = fixed_array(count);
        let lowered = crate::lower_solve_model(&dae, &Default::default(), |_| {}).unwrap();
        let component = crate::fmi::finish_fmi_component(lowered).unwrap();
        let root = component.runtime_view().retain_assertion_root();
        let wire = serde_json::to_value(
            rumoca_eval_solve::fmi_wire::retained_fmi_component_wire(&root).unwrap(),
        )
        .unwrap();
        let declarations = wire["variables"].as_array().unwrap();
        assert_eq!(declarations.len(), 1);
        assert_eq!(declarations[0]["name"], "x");
        assert_eq!(declarations[0]["dimensions"], serde_json::json!([count]));
        sizes.push(serde_json::to_vec(&wire["variables"]).unwrap().len());
    }
    assert!(
        sizes[1] <= sizes[0] + 128,
        "one broadcast declaration expanded FMI metadata wire: {sizes:?}"
    );
}

#[test]
fn fmi_current_wire_replays_exact_broadcast_bits_and_rejects_foreign_extents() {
    #[derive(serde::Deserialize)]
    struct Replay(
        #[serde(deserialize_with = "rumoca_eval_solve::fmi_wire::deserialize_fmi_component")]
        rumoca_ir_solve::fmi::FmiComponent,
    );
    let dae = fixed_array(64);
    let lowered = crate::lower_solve_model(&dae, &Default::default(), |_| {}).unwrap();
    let component = crate::fmi::finish_fmi_component(lowered).unwrap();
    let root = component.runtime_view().retain_assertion_root();
    let wire = serde_json::to_value(
        rumoca_eval_solve::fmi_wire::retained_fmi_component_wire(&root).unwrap(),
    )
    .unwrap();
    assert!(wire["variables"][0].get("scalar_names").is_none());
    let replay: Replay = serde_json::from_value(wire.clone()).unwrap();
    let variable = &replay.0.variables()[0];
    assert_eq!(variable.scalar_name(63).as_deref(), Some("x[64]"));
    assert_eq!(variable.scalar_name(64), None);
    let start = variable.start().unwrap();
    assert_eq!(start.len(), 64);
    assert_eq!(start.run_count(), 1);
    assert!(!start.has_dense_view());
    assert_eq!(start.value(63).unwrap().to_bits(), (-0.0f64).to_bits());
    assert_eq!(
        variable.numeric_attribute_count(rumoca_ir_solve::fmi::FmiNumericAttribute::StartFmi3),
        Some(1)
    );
    assert_eq!(wire["variables"][0]["start"]["kind"], "real");
    assert_eq!(wire["variables"][0]["start"]["values"]["count"], 64);
    assert_eq!(
        wire["variables"][0]["start"]["values"]["runs"]
            .as_array()
            .unwrap()
            .len(),
        1
    );
    let mut foreign = wire.clone();
    foreign["variables"][0]["start"]["values"]["count"] = 1.into();
    foreign["variables"][0]["start"]["values"]["runs"][0]["count"] = 1.into();
    assert!(serde_json::from_value::<Replay>(foreign).is_err());
    for value in [f64::INFINITY, f64::NEG_INFINITY, f64::NAN] {
        let mut nonfinite = wire.clone();
        nonfinite["variables"][0]["start"]["values"]["runs"][0]["bits"] = value.to_bits().into();
        assert!(serde_json::from_value::<Replay>(nonfinite).is_err());
    }
    let mut old = wire;
    old["schema_version"] = 4.into();
    assert!(serde_json::from_value::<Replay>(old).is_err());
}

#[test]
fn checked_model_wire_refuses_nonfinite_owner_bits_and_legacy_schema() {
    #[derive(serde::Deserialize)]
    struct Replay(
        #[serde(deserialize_with = "rumoca_eval_solve::model_wire::deserialize_solve_model")]
        rumoca_ir_solve::SolveModel,
    );
    let dae = fixed_array(12);
    let mut model = crate::lower_solve_model(&dae, &Default::default(), |_| {})
        .unwrap()
        .into_model();
    let original =
        serde_json::to_value(rumoca_eval_solve::model_wire::solve_model_wire(&model).unwrap())
            .unwrap();
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
        assert!(rumoca_eval_solve::model_wire::solve_model_wire(&model).is_err());
        assert!(!model.initial_y.has_dense_view());
    }
    let mut old = original;
    old["schema_version"] = 1.into();
    assert!(serde_json::from_value::<Replay>(old).is_err());
}

fn bound_parameter_source(constant: bool, mixed: bool) -> dae::Dae {
    let role = if constant { "constant" } else { "parameter" };
    let suffix = if mixed {
        " parameter Real q=2*dependent+base;"
    } else {
        ""
    };
    let text = format!("{role} Real base=3; parameter Real dependent=base;{suffix}");
    let source = TestSource::new(&text);
    let at = source.at(0, text.len());
    dae::Dae::construct(source.map, |model| {
        let real = model
            .types(|types| types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at))?;
        let three = model.expressions(|e| e.at(at).literal(dae::DaeLiteral::Real(3.0)))?;
        let base = model.variables(|variables| {
            let attributes = dae::VariableAttributes {
                binding: Some(three),
                is_tunable: !constant,
                ..Default::default()
            };
            if constant {
                variables.constant(VarName::new("base"), real, at, attributes)
            } else {
                variables.parameter(VarName::new("base"), real, at, attributes)
            }
        })?;
        let binding =
            model.expressions(|e| e.at(at).coordinate(dae::CoordinateInput::Parameter(base)))?;
        let dependent = model.variables(|variables| {
            variables.parameter(
                VarName::new("dependent"),
                real,
                at,
                dae::VariableAttributes {
                    binding: Some(binding),
                    is_tunable: true,
                    ..Default::default()
                },
            )
        })?;
        if mixed {
            let mixed_binding = model.expressions(|e| {
                let two = e.at(at).literal(dae::DaeLiteral::Real(2.0))?;
                let read = e
                    .at(at)
                    .coordinate(dae::CoordinateInput::Parameter(dependent))?;
                let product = e.at(at).binary(dae::BinaryOperator::Multiply, two, read)?;
                let constant = e.at(at).coordinate(dae::CoordinateInput::Parameter(base))?;
                e.at(at).binary(dae::BinaryOperator::Add, product, constant)
            })?;
            model.variables(|variables| {
                variables.parameter(
                    VarName::new("q"),
                    real,
                    at,
                    dae::VariableAttributes {
                        binding: Some(mixed_binding),
                        ..Default::default()
                    },
                )
            })?;
        }
        Ok(())
    })
    .unwrap()
}

#[test]
fn a_constant_coordinate_does_not_issue_a_parameter_initialization_update() {
    let model = bound_parameter_source(true, false);
    let problem = lower_solve_problem(&model).unwrap();
    assert!(problem.initialization.update_rhs().is_empty());
    assert!(problem.initialization.update_targets().is_empty());
    assert!(!matches!(
        problem
            .continuous
            .refresh_owners
            .native_assignment_refusal(),
        Some(solve::NativeScheduleRefusal::Evaluation(
            solve::NativeEvaluationRefusal::InitializationEquations
        ))
    ));
}

#[test]
fn a_parameter_coordinate_retains_its_dependent_update_and_override_value() {
    let model = bound_parameter_source(false, false);
    for value in [3.0f64, 7.0, 3.0] {
        let overrides = if value == 3.0 {
            Default::default()
        } else {
            std::collections::HashMap::from([("base".to_owned(), value)])
        };
        let lowered = crate::lower_solve_model(&model, &overrides, |_| {}).unwrap();
        let solve = lowered.model();
        assert_eq!(solve.problem.initialization.update_targets().len(), 1);
        assert_eq!(solve.problem.initialization.update_rhs().output_count(), 1);
        let ScalarSlot::P { index, .. } = solve.problem.layout.binding("dependent").unwrap() else {
            panic!("dependent parameter has P storage");
        };
        assert_eq!(
            solve.parameters.value(index).unwrap().to_bits(),
            value.to_bits()
        );
    }
}

fn parameter_value(model: &solve::SolveModel, name: &str) -> (ScalarSlot, f64) {
    let slot = model
        .problem
        .layout
        .binding(name)
        .expect("original declaration keeps storage");
    let ScalarSlot::P { index, .. } = slot else {
        panic!("parameter keeps its original P slot");
    };
    (slot, model.parameters.value(index).unwrap())
}

#[test]
fn a_constant_bound_parameter_remains_settable_without_an_update() {
    let source = bound_parameter_source(true, false);
    let mut original_slot = None;
    for value in [3.0f64, 7.0, 3.0] {
        let overrides = if value == 3.0 {
            Default::default()
        } else {
            std::collections::HashMap::from([("dependent".to_owned(), value)])
        };
        let lowered = crate::lower_solve_model(&source, &overrides, |_| {}).unwrap();
        let model = lowered.model();
        let (slot, actual) = parameter_value(model, "dependent");
        assert_eq!(*original_slot.get_or_insert(slot), slot);
        assert_eq!(actual.to_bits(), value.to_bits());
        assert!(model.problem.initialization.update_targets().is_empty());
        assert!(model.problem.initialization.update_rhs().is_empty());
        assert_eq!(parameter_value(model, "base").1.to_bits(), 3.0f64.to_bits());
    }
}

#[test]
fn mixed_transitive_parameter_dependencies_preserve_only_the_real_update() {
    let source = bound_parameter_source(true, true);
    let mut original_slots = None;
    for (value, expected) in [(3.0f64, 9.0f64), (7.0, 17.0), (3.0, 9.0)] {
        let overrides = if value == 3.0 {
            Default::default()
        } else {
            std::collections::HashMap::from([("dependent".to_owned(), value)])
        };
        let lowered = crate::lower_solve_model(&source, &overrides, |_| {}).unwrap();
        let model = lowered.model();
        let (p_slot, p_value) = parameter_value(model, "dependent");
        let (q_slot, q_value) = parameter_value(model, "q");
        assert_eq!(
            *original_slots.get_or_insert((p_slot, q_slot)),
            (p_slot, q_slot)
        );
        assert_eq!(p_value.to_bits(), value.to_bits());
        assert_eq!(q_value.to_bits(), expected.to_bits());
        assert_eq!(model.problem.initialization.update_targets(), [q_slot]);
        assert_eq!(model.problem.initialization.update_rhs().output_count(), 1);
        assert!(
            !model
                .problem
                .initialization
                .update_targets()
                .contains(&p_slot)
        );
        assert_eq!(parameter_value(model, "base").1.to_bits(), 3.0f64.to_bits());
    }
}

#[test]
fn a_non_tunable_mixed_parameter_still_refuses_an_override() {
    let source = bound_parameter_source(true, true);
    let overrides = std::collections::HashMap::from([("q".to_owned(), 17.0)]);
    let error = match crate::lower_solve_model(&source, &overrides, |_| {}) {
        Ok(_) => panic!("non-tunable q must not accept an override"),
        Err(error) => error,
    };
    let crate::SolveModelLoweringError::InvalidOverride { message } = error else {
        panic!("non-tunable override must retain its typed refusal");
    };
    assert_eq!(
        message,
        "`q` is not a tunable parameter; change structural values by recompiling"
    );
}

/// An element's Y slot is the variable's base slot plus its row-major
/// ordinal, the same slot the rendered element name resolves to.
#[test]
fn array_element_slots_follow_the_base_slot_and_match_the_named_binding() {
    let dae = fixed_array(7);
    let lowered = crate::lower_solve_model(&dae, &Default::default(), |_| {}).unwrap();
    let problem = &lowered.model().problem;
    dae.inspect(|view| {
        let (_, variable) = view.variables().next().unwrap();
        for scalar in 0..variable.scalar_count() {
            let named = match problem
                .layout
                .binding(&variable.scalar_name(scalar).unwrap())
            {
                Some(solve::ScalarSlot::Y { index, .. }) => Some(index),
                _ => None,
            };
            assert!(named.is_some());
            assert_eq!(
                crate::model_values::y_slot_index(problem, variable, scalar),
                named
            );
        }
        assert_eq!(
            crate::model_values::y_slot_index(problem, variable, variable.scalar_count()),
            None
        );
    });
}
