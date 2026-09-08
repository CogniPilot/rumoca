//! The FMI projection of a lowered model.
//!
//! These cases fix what the maximum-step-duration local of SPEC_0044 §8 is
//! derived from: the checked Solve delay partition, and nothing else. This
//! lowering supplies only ordinary DAE facts, so a model that calls `delay`
//! publishes exactly one such local and a model that does not publishes none,
//! and neither gains a Modelica declaration or a storage run.

use super::*;

use rumoca_ir_solve::SolveVariableValueKind;
use rumoca_ir_solve::fmi::{
    FmiCausality, FmiInitial, FmiValueBacking, FmiVariability, MAX_STEP_DURATION_DESCRIPTION,
    MAX_STEP_DURATION_NAME, MAX_STEP_DURATION_UNIT,
};

fn component(model: &dae::Dae) -> rumoca_ir_solve::fmi::FmiComponent {
    let lowered = crate::lower_solve_model(model, &std::collections::HashMap::new(), |_| {})
        .expect("the checked DAE constructs one complete Solve root");
    crate::fmi::finish_fmi_component(lowered)
        .expect("the completed Solve root projects to one correlated FMI component")
}

/// A fixed-start `Real x` with `der(x) = delay(x, 0.5);`.
fn delayed_state_model() -> dae::Dae {
    let source = TestSource::new("Real x; der(x) = delay(x, 0.5);");
    let declaration = source.at(0, 6);
    let owner = source.at(8, 31);
    let derivative_at = source.at(8, 14);
    let delay_at = source.at(17, 30);
    let source_at = source.at(23, 24);
    let timing_at = source.at(26, 29);
    dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                declaration,
            )
        })?;
        let state_attributes = real_state_attributes(model, declaration, 0.0, true)?;
        let state = model.variables(|variables| {
            variables.state(
                VarName::new("x"),
                rumoca_core::InstanceId::new(1),
                real,
                declaration,
                state_attributes,
            )
        })?;
        let (delayed, delay_time) = model.expressions(|expressions| {
            Ok((
                expressions
                    .at(source_at)
                    .coordinate(dae::CoordinateInput::State(state))?,
                expressions
                    .at(timing_at)
                    .literal(dae::DaeLiteral::Real(0.5))?,
            ))
        })?;
        let timing =
            model.temporal(|temporal| temporal.positive_parameter(delay_time, 0.5, timing_at))?;
        let delay = model
            .expressions(|expressions| expressions.at(delay_at).delay(delayed, timing, delay_at))?;
        let residual = model.expressions(|expressions| {
            let derivative = expressions
                .at(derivative_at)
                .coordinate(dae::CoordinateInput::Derivative(state))?;
            expressions.at(owner).binary(
                dae::BinaryOperator::Subtract,
                derivative,
                delay.expression(),
            )
        })?;
        model.continuous(|continuous| continuous.value_equation(owner, residual))
    })
    .expect("delayed state fixture is a checked DAE")
}

/// `Real x(start=0.0, fixed=false); initial equation x=0.0; der(x)=-x;`.
fn undelayed_state_model() -> dae::Dae {
    let source =
        TestSource::new("Real x(start=0.0, fixed=false); initial equation x = 0.0; der(x) = -x;");
    let declaration = source.at(0, 30);
    let initialization_owner = source.at(49, 56);
    let owner = source.at(58, 69);
    let derivative_at = source.at(58, 64);
    let value_at = source.at(67, 69);
    dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                declaration,
            )
        })?;
        let state_attributes = real_state_attributes(model, declaration, 0.0, false)?;
        let state = model.variables(|variables| {
            variables.state(
                VarName::new("x"),
                rumoca_core::InstanceId::new(2),
                real,
                declaration,
                state_attributes,
            )
        })?;
        let initialization_residual = model.expressions(|expressions| {
            let state = expressions
                .at(initialization_owner)
                .coordinate(dae::CoordinateInput::State(state))?;
            let zero = expressions
                .at(initialization_owner)
                .literal(dae::DaeLiteral::Real(0.0))?;
            expressions
                .at(initialization_owner)
                .binary(dae::BinaryOperator::Subtract, state, zero)
        })?;
        model.initialization(|initialization| {
            initialization.value_equation(initialization_owner, initialization_residual)
        })?;
        let residual = model.expressions(|expressions| {
            let derivative = expressions
                .at(derivative_at)
                .coordinate(dae::CoordinateInput::Derivative(state))?;
            let value = expressions
                .at(value_at)
                .coordinate(dae::CoordinateInput::State(state))?;
            let negated = expressions
                .at(value_at)
                .unary(dae::UnaryOperator::Negate, value)?;
            expressions
                .at(owner)
                .binary(dae::BinaryOperator::Subtract, derivative, negated)
        })?;
        model.continuous(|continuous| continuous.value_equation(owner, residual))
    })
    .expect("undelayed state fixture is a checked DAE")
}

#[test]
fn a_delayed_model_publishes_one_calculated_maximum_step_duration_local() {
    let model = delayed_state_model();
    let component = component(&model);

    let bound = component
        .max_step_duration()
        .expect("a delay-bearing component bounds the importer's step");
    assert_eq!(bound.name(), MAX_STEP_DURATION_NAME);
    assert_eq!(bound.value_kind(), SolveVariableValueKind::Real);
    assert_eq!(bound.causality(), FmiCausality::Local);
    assert_eq!(bound.variability(), FmiVariability::Continuous);
    assert_eq!(bound.initial(), Some(FmiInitial::Calculated));
    assert_eq!(bound.unit(), Some(MAX_STEP_DURATION_UNIT));
    assert_eq!(bound.description(), Some(MAX_STEP_DURATION_DESCRIPTION));
    assert_eq!(bound.backing(), &FmiValueBacking::MaxStepDuration);
    assert_eq!(bound.start(), None);
    assert_eq!(bound.scalar_names(), [MAX_STEP_DURATION_NAME]);
}

#[test]
fn the_delayed_inventory_is_one_entry_per_storage_run_plus_the_local() {
    let model = delayed_state_model();
    let component = component(&model);
    let runs = component.storage_variables().count();

    assert_eq!(component.variables().len(), runs + 1);
    assert_eq!(component.storage_variables().count(), runs);
}

#[test]
fn the_local_follows_the_run_derived_variables_and_precedes_the_derivatives() {
    let model = delayed_state_model();
    let component = component(&model);

    let declared = u32::try_from(component.variables().len()).expect("fixture inventory is small");
    assert_eq!(
        component
            .max_step_duration()
            .expect("a delay-bearing component bounds the importer's step")
            .value_reference_fmi3(),
        declared
    );
    assert_eq!(
        component.derivative_value_reference_base_fmi3(),
        declared + 1
    );
}

#[test]
fn the_local_is_in_the_inventory_but_is_not_a_modelica_variable() {
    let model = delayed_state_model();
    let component = component(&model);

    assert!(
        component
            .variables()
            .iter()
            .any(|variable| variable.name() == MAX_STEP_DURATION_NAME)
    );
    assert!(
        !component
            .storage_variables()
            .any(|variable| variable.name() == MAX_STEP_DURATION_NAME)
    );
    model.inspect(|view| {
        assert!(
            !view
                .variables()
                .any(|(_, variable)| variable.name().as_str() == MAX_STEP_DURATION_NAME)
        );
    });
}

#[test]
fn an_undelayed_model_publishes_no_maximum_step_duration_local() {
    let model = undelayed_state_model();
    let component = component(&model);
    let runs = component.storage_variables().count();

    assert!(component.max_step_duration().is_none());
    assert_eq!(component.variables().len(), runs);
    assert_eq!(
        component.derivative_value_reference_base_fmi3(),
        u32::try_from(runs).expect("fixture inventory is small") + 1
    );
}

#[test]
fn experiment_override_is_one_value_in_runtime_storage_catalog_and_fmi() {
    let model = undelayed_state_model();
    let overrides = std::collections::HashMap::from([("x".to_string(), 42.5)]);
    let lowered = crate::lower_solve_model(&model, &overrides, |_| {})
        .expect("the exact state scalar override lowers atomically");
    let solve = lowered.model();
    let catalog = solve
        .variable_catalog()
        .entries()
        .iter()
        .find(|entry| entry.name() == "x")
        .expect("the state declaration has one catalog entry");
    assert_eq!(solve.initial_state_values(), [42.5]);
    assert_eq!(catalog.start(), Some([42.5].as_slice()));
    assert_eq!(catalog.fixed(), rumoca_core::Fixity::Free);

    let component = crate::fmi::finish_fmi_component(lowered)
        .expect("the completed Solve root alone constructs FMI");
    let fmi = component
        .variables()
        .iter()
        .find(|variable| variable.name() == "x")
        .expect("the FMI projection derives the same state entry");
    assert_eq!(fmi.start(), Some([42.5].as_slice()));
    assert_eq!(fmi.initial(), Some(FmiInitial::Approx));
}

#[test]
fn runtime_override_must_name_one_exact_scalar_and_be_finite() {
    let model = undelayed_state_model();
    for overrides in [
        std::collections::HashMap::from([("missing".to_string(), 1.0)]),
        std::collections::HashMap::from([("x[1]".to_string(), 1.0)]),
        std::collections::HashMap::from([("x".to_string(), f64::NAN)]),
    ] {
        assert!(matches!(
            crate::lower_solve_model(&model, &overrides, |_| {}),
            Err(crate::SolveModelLoweringError::InvalidOverride { .. })
        ));
    }
}

#[test]
fn integer_runtime_override_must_be_integral() {
    let source = TestSource::new("input Integer count;");
    let declaration = source.at(0, 20);
    let model = dae::Dae::construct(source.map, |model| {
        let integer = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Integer),
                declaration,
            )
        })?;
        model.variables(|variables| {
            variables.discrete_value(
                VarName::new("count"),
                rumoca_core::InstanceId::new(3),
                integer,
                declaration,
                dae::VariableAttributes {
                    causality: dae::VariableCausality::Input,
                    ..dae::VariableAttributes::default()
                },
            )
        })?;
        Ok(())
    })
    .expect("Integer input fixture is a checked DAE");

    let overrides = std::collections::HashMap::from([("count".to_string(), 1.5)]);
    let error = crate::lower_solve_model(&model, &overrides, |_| {})
        .err()
        .expect("a fractional Integer override cannot enter Solve construction");
    assert!(matches!(
        error,
        crate::SolveModelLoweringError::InvalidOverride { .. }
    ));
}
