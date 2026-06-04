use rumoca_core::{BuiltinFunction, Expression, Literal, OpBinary, Reference, Span, VarName};
use rumoca_ir_dae as dae;
use rumoca_phase_galec::{GalecProfile, check_galec_admissible};

fn real_literal(value: f64) -> Expression {
    Expression::Literal {
        value: Literal::Real(value),
        span: Span::DUMMY,
    }
}

fn bool_literal(value: bool) -> Expression {
    Expression::Literal {
        value: Literal::Boolean(value),
        span: Span::DUMMY,
    }
}

fn var_ref(name: &str) -> Expression {
    Expression::VarRef {
        name: Reference::new(name),
        subscripts: Vec::new(),
        span: Span::DUMMY,
    }
}

fn binary(op: OpBinary, lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: Span::DUMMY,
    }
}

fn clocked_dae() -> dae::Dae {
    let mut dae = dae::Dae::new();
    dae.clocks.schedules.push(dae::ClockSchedule {
        period_seconds: 0.01,
        phase_seconds: 0.0,
        source_span: Span::DUMMY,
    });
    dae.variables.constants.insert(
        VarName::new("samplePeriod"),
        dae::Variable {
            name: VarName::new("samplePeriod"),
            start: Some(real_literal(0.01)),
            fixed: Some(true),
            unit: Some("s".to_string()),
            ..Default::default()
        },
    );
    dae
}

fn real_var(name: &str) -> dae::Variable {
    dae::Variable {
        name: VarName::new(name),
        start: Some(real_literal(0.0)),
        ..Default::default()
    }
}

fn violation_messages(error: rumoca_phase_galec::GalecAdmissibilityError) -> Vec<String> {
    error
        .report
        .violations
        .into_iter()
        .map(|violation| violation.message)
        .collect()
}

#[test]
fn admits_minimal_explicit_discrete_dae_model() {
    let mut dae = clocked_dae();
    dae.variables
        .outputs
        .insert(VarName::new("y"), real_var("y"));
    dae.discrete.real_updates.push(dae::Equation::explicit(
        VarName::new("y"),
        real_literal(1.0),
        Span::DUMMY,
        "y update",
    ));

    check_galec_admissible(&dae, GalecProfile::Efmi10)
        .expect("explicit clocked discrete DAE should be GALEC-admissible");
}

#[test]
fn rejects_missing_fixed_clock() {
    let dae = dae::Dae::new();

    let err = check_galec_admissible(&dae, GalecProfile::Efmi10)
        .expect_err("GALEC Algorithm Code requires a fixed sample clock");

    let messages = violation_messages(err);
    assert!(
        messages
            .iter()
            .any(|message| message.contains("fixed sample clock"))
    );
}

#[test]
fn rejects_clock_without_constant_period_variable() {
    let mut dae = dae::Dae::new();
    dae.clocks.schedules.push(dae::ClockSchedule {
        period_seconds: 0.01,
        phase_seconds: 0.0,
        source_span: Span::DUMMY,
    });

    let err = check_galec_admissible(&dae, GalecProfile::Efmi10)
        .expect_err("fixed clocks must be backed by constant variables");

    let messages = violation_messages(err);
    assert!(
        messages
            .iter()
            .any(|message| message.contains("constant variable"))
    );
}

#[test]
fn rejects_continuous_states_for_initial_discrete_profile() {
    let mut dae = clocked_dae();
    dae.variables
        .states
        .insert(VarName::new("x"), real_var("x"));

    let err = check_galec_admissible(&dae, GalecProfile::Efmi10)
        .expect_err("continuous states should be rejected for now");

    let messages = violation_messages(err);
    assert!(
        messages
            .iter()
            .any(|message| message.contains("continuous DAE states"))
    );
}

#[test]
fn rejects_continuous_equations_for_initial_discrete_profile() {
    let mut dae = clocked_dae();
    dae.variables
        .outputs
        .insert(VarName::new("y"), real_var("y"));
    dae.continuous.equations.push(dae::Equation::explicit(
        VarName::new("y"),
        real_literal(1.0),
        Span::DUMMY,
        "continuous y",
    ));

    let err = check_galec_admissible(&dae, GalecProfile::Efmi10)
        .expect_err("continuous equations should be rejected for now");

    let messages = violation_messages(err);
    assert!(
        messages
            .iter()
            .any(|message| message.contains("continuous DAE equations"))
    );
}

#[test]
fn rejects_event_partitions() {
    let mut dae = clocked_dae();
    dae.events.event_actions.push(dae::DaeEventAction {
        condition: bool_literal(true),
        kind: dae::DaeEventActionKind::Terminate {
            message: "stop".to_string(),
        },
        span: Span::DUMMY,
        origin: "test".to_string(),
    });

    let err = check_galec_admissible(&dae, GalecProfile::Efmi10)
        .expect_err("event partitions should be rejected");

    let messages = violation_messages(err);
    assert!(
        messages
            .iter()
            .any(|message| message.contains("event partitions"))
    );
}

#[test]
fn rejects_condition_partitions() {
    let mut dae = clocked_dae();
    dae.conditions
        .relations
        .push(binary(OpBinary::Gt, var_ref("u"), real_literal(0.0)));

    let err = check_galec_admissible(&dae, GalecProfile::Efmi10)
        .expect_err("condition partitions should be rejected");

    let messages = violation_messages(err);
    assert!(
        messages
            .iter()
            .any(|message| message.contains("condition partitions"))
    );
}

#[test]
fn rejects_residual_discrete_equations() {
    let mut dae = clocked_dae();
    dae.discrete.real_updates.push(dae::Equation::residual(
        real_literal(1.0),
        Span::DUMMY,
        "implicit residual",
    ));

    let err = check_galec_admissible(&dae, GalecProfile::Efmi10)
        .expect_err("residual DAE equations should be rejected");

    let messages = violation_messages(err);
    assert!(
        messages
            .iter()
            .any(|message| message.contains("not explicitly solved"))
    );
}

#[test]
fn rejects_assignment_to_input() {
    let mut dae = clocked_dae();
    dae.variables
        .inputs
        .insert(VarName::new("u"), real_var("u"));
    dae.discrete.real_updates.push(dae::Equation::explicit(
        VarName::new("u"),
        real_literal(1.0),
        Span::DUMMY,
        "bad input assignment",
    ));

    let err = check_galec_admissible(&dae, GalecProfile::Efmi10)
        .expect_err("input assignments should be rejected");

    let messages = violation_messages(err);
    assert!(
        messages
            .iter()
            .any(|message| message.contains("protected GALEC target `u`"))
    );
}

#[test]
fn rejects_duplicate_step_assignments() {
    let mut dae = clocked_dae();
    dae.variables
        .outputs
        .insert(VarName::new("y"), real_var("y"));
    dae.discrete.real_updates.push(dae::Equation::explicit(
        VarName::new("y"),
        real_literal(1.0),
        Span::DUMMY,
        "first y",
    ));
    dae.discrete.valued_updates.push(dae::Equation::explicit(
        VarName::new("y"),
        bool_literal(true),
        Span::DUMMY,
        "second y",
    ));

    let err = check_galec_admissible(&dae, GalecProfile::Efmi10)
        .expect_err("duplicate assignments should be rejected");

    let messages = violation_messages(err);
    assert!(
        messages
            .iter()
            .any(|message| message.contains("assigns `y` more than once"))
    );
}

#[test]
fn rejects_non_positive_dimensions() {
    let mut dae = clocked_dae();
    let mut variable = real_var("table");
    variable.dims = vec![0, 3];
    dae.variables
        .parameters
        .insert(VarName::new("table"), variable);

    let err = check_galec_admissible(&dae, GalecProfile::Efmi10)
        .expect_err("zero-sized dimensions should be rejected for GALEC");

    let messages = violation_messages(err);
    assert!(
        messages
            .iter()
            .any(|message| message.contains("non-positive GALEC array dimension"))
    );
}

#[test]
fn rejects_missing_variable_start() {
    let mut dae = clocked_dae();
    dae.variables.outputs.insert(
        VarName::new("y"),
        dae::Variable {
            name: VarName::new("y"),
            ..Default::default()
        },
    );

    let err = check_galec_admissible(&dae, GalecProfile::Efmi10)
        .expect_err("manifest variables need start values");

    let messages = violation_messages(err);
    assert!(
        messages
            .iter()
            .any(|message| message.contains("has no start value"))
    );
}

#[test]
fn rejects_source_temporal_builtin() {
    let mut dae = clocked_dae();
    dae.variables
        .outputs
        .insert(VarName::new("y"), real_var("y"));
    dae.discrete.real_updates.push(dae::Equation::explicit(
        VarName::new("y"),
        Expression::BuiltinCall {
            function: BuiltinFunction::Pre,
            args: vec![var_ref("y")],
            span: Span::DUMMY,
        },
        Span::DUMMY,
        "pre survives",
    ));

    let err = check_galec_admissible(&dae, GalecProfile::Efmi10)
        .expect_err("source temporal builtins should be rejected");

    let messages = violation_messages(err);
    assert!(
        messages
            .iter()
            .any(|message| message.contains("source temporal builtin"))
    );
}

#[test]
fn rejects_invalid_range_metadata() {
    let mut dae = clocked_dae();
    dae.variables.outputs.insert(
        VarName::new("y"),
        dae::Variable {
            name: VarName::new("y"),
            start: Some(real_literal(2.0)),
            min: Some(real_literal(-1.0)),
            max: Some(real_literal(1.0)),
            nominal: Some(real_literal(0.0)),
            ..Default::default()
        },
    );

    let err = check_galec_admissible(&dae, GalecProfile::Efmi10)
        .expect_err("invalid eFMI range metadata should be rejected");

    let messages = violation_messages(err);
    assert!(
        messages
            .iter()
            .any(|message| message.contains("outside min/max"))
    );
    assert!(messages.iter().any(|message| message.contains("not > 0")));
}

#[test]
fn rejects_dependent_parameter_without_recalibrate_assignment() {
    let mut dae = clocked_dae();
    dae.variables.parameters.insert(
        VarName::new("gain"),
        dae::Variable {
            name: VarName::new("gain"),
            start: Some(real_literal(1.0)),
            causality: dae::VariableCausality::CalculatedParameter,
            ..Default::default()
        },
    );

    let err = check_galec_admissible(&dae, GalecProfile::Efmi10)
        .expect_err("dependent parameters require recalibrate assignments");

    let messages = violation_messages(err);
    assert!(
        messages
            .iter()
            .any(|message| message.contains("requires an explicit Recalibrate assignment"))
    );
}

#[test]
fn admits_dependent_parameter_with_parameter_only_recalibrate_assignment() {
    let mut dae = clocked_dae();
    dae.variables
        .parameters
        .insert(VarName::new("k"), real_var("k"));
    dae.variables.parameters.insert(
        VarName::new("gain"),
        dae::Variable {
            name: VarName::new("gain"),
            start: Some(real_literal(1.0)),
            causality: dae::VariableCausality::CalculatedParameter,
            ..Default::default()
        },
    );
    dae.initialization.equations.push(dae::Equation::explicit(
        VarName::new("gain"),
        binary(OpBinary::Mul, var_ref("k"), real_literal(2.0)),
        Span::DUMMY,
        "gain recalibrate",
    ));

    check_galec_admissible(&dae, GalecProfile::Efmi10)
        .expect("dependent parameter assignment using parameters should be admissible");
}
