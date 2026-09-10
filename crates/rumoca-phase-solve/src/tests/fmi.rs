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

use crate::fmi::lower_to_fmi_component;

fn component(model: &dae::Dae) -> rumoca_ir_solve::fmi::FmiComponent {
    lower_to_fmi_component(model, &std::collections::HashMap::new())
        .expect("the checked DAE projects to one correlated FMI component")
}

/// `Real x; der(x) = delay(x, 0.5);`
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
        let start = model.expressions(|expressions| {
            expressions
                .at(declaration)
                .literal(dae::DaeLiteral::Real(0.0))
        })?;
        let state = model.variables(|variables| {
            variables.state(
                VarName::new("x"),
                real,
                declaration,
                dae::VariableAttributes {
                    start: Some(start),
                    ..dae::VariableAttributes::default()
                },
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

/// `Real x; der(x) = -x;`
fn undelayed_state_model() -> dae::Dae {
    let source = TestSource::new("Real x; der(x) = -x;");
    let declaration = source.at(0, 6);
    let owner = source.at(8, 19);
    let derivative_at = source.at(8, 14);
    let value_at = source.at(17, 19);
    dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                declaration,
            )
        })?;
        let start = model.expressions(|expressions| {
            expressions
                .at(declaration)
                .literal(dae::DaeLiteral::Real(0.0))
        })?;
        let state = model.variables(|variables| {
            variables.state(
                VarName::new("x"),
                real,
                declaration,
                dae::VariableAttributes {
                    start: Some(start),
                    ..dae::VariableAttributes::default()
                },
            )
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
