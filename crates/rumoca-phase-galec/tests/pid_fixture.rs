use rumoca_core::{
    ComponentReference, Expression, Literal, OpBinary, OpUnary, Reference, Span, Statement,
    StatementBlock, VarName,
};
use rumoca_ir_dae as dae;
use rumoca_phase_galec::{
    GalecExpr, GalecModel, GalecProfile, GalecStmt, GalecType, GalecVariableRole,
    check_galec_admissible, lower_statement_to_galec, lower_to_galec, render_galec,
};

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

fn unary(op: OpUnary, rhs: Expression) -> Expression {
    Expression::Unary {
        op,
        rhs: Box::new(rhs),
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

fn add(lhs: Expression, rhs: Expression) -> Expression {
    binary(OpBinary::Add, lhs, rhs)
}

fn sub(lhs: Expression, rhs: Expression) -> Expression {
    binary(OpBinary::Sub, lhs, rhs)
}

fn mul(lhs: Expression, rhs: Expression) -> Expression {
    binary(OpBinary::Mul, lhs, rhs)
}

fn div(lhs: Expression, rhs: Expression) -> Expression {
    binary(OpBinary::Div, lhs, rhs)
}

fn gt(lhs: Expression, rhs: Expression) -> Expression {
    binary(OpBinary::Gt, lhs, rhs)
}

fn lt(lhs: Expression, rhs: Expression) -> Expression {
    binary(OpBinary::Lt, lhs, rhs)
}

fn if_expr(branches: Vec<(Expression, Expression)>, else_branch: Expression) -> Expression {
    Expression::If {
        branches,
        else_branch: Box::new(else_branch),
        span: Span::DUMMY,
    }
}

fn assignment_stmt(name: &str, value: Expression) -> Statement {
    Statement::Assignment {
        comp: ComponentReference::from_flat_segments(name, Span::DUMMY, None),
        value,
        span: Span::DUMMY,
    }
}

fn named_real(
    name: &str,
    start: f64,
    min: Option<f64>,
    max: Option<f64>,
    causality: dae::VariableCausality,
) -> dae::Variable {
    dae::Variable {
        name: VarName::new(name),
        start: Some(real_literal(start)),
        min: min.map(real_literal),
        max: max.map(real_literal),
        causality,
        ..Default::default()
    }
}

fn local_real(name: &str) -> dae::Variable {
    dae::Variable {
        name: VarName::new(name),
        ..Default::default()
    }
}

fn insert_real_update(dae: &mut dae::Dae, lhs: &str, rhs: Expression, origin: &str) {
    dae.discrete.real_updates.push(dae::Equation::explicit(
        VarName::new(lhs),
        rhs,
        Span::DUMMY,
        origin,
    ));
}

fn insert_valued_update(dae: &mut dae::Dae, lhs: &str, rhs: Expression, origin: &str) {
    dae.discrete.valued_updates.push(dae::Equation::explicit(
        VarName::new(lhs),
        rhs,
        Span::DUMMY,
        origin,
    ));
}

/// Builds the currently representable subset of the eFMI 1.0 PID controller
/// example in section 3.2.7 of the standard.
///
/// The full example also uses GALEC `if` control flow for first-tick handling
/// and output limiting. Those constructs are intentionally left out here until
/// the GALEC IR grows statement/expression variants for guarded control flow.
fn efmi_pid_subset_dae() -> dae::Dae {
    let mut dae = dae::Dae::new();
    dae.clocks.schedules.push(dae::ClockSchedule {
        period_seconds: 0.001,
        phase_seconds: 0.0,
        source_span: Span::DUMMY,
    });

    dae.variables.constants.insert(
        VarName::new("samplePeriod"),
        dae::Variable {
            name: VarName::new("samplePeriod"),
            start: Some(real_literal(0.001)),
            fixed: Some(true),
            unit: Some("s".to_string()),
            ..Default::default()
        },
    );

    dae.variables.inputs.insert(
        VarName::new("wLoadRef"),
        named_real(
            "wLoadRef",
            0.0,
            Some(-1.0e5),
            Some(1.0e5),
            dae::VariableCausality::Input,
        ),
    );
    dae.variables.inputs.insert(
        VarName::new("wMotor"),
        named_real(
            "wMotor",
            0.0,
            Some(-1.0e5),
            Some(1.0e5),
            dae::VariableCausality::Input,
        ),
    );
    dae.variables.outputs.insert(
        VarName::new("vMotor"),
        named_real(
            "vMotor",
            0.0,
            Some(-1.0e7),
            Some(1.0e7),
            dae::VariableCausality::Output,
        ),
    );

    for (name, start, min, max) in [
        ("'limiter.uMax'", 400.0, 1.0, 1.0e5),
        ("gearRatio", 105.0, 10.0, 500.0),
        ("Ti", 0.1, 1.0e-7, 100.0),
        ("Td", 0.1, 1.0e-7, 100.0),
        ("kd", 0.1, 0.0, 1000.0),
        ("k", 10.0, 0.0, 1000.0),
        ("stepSize", 0.001, 1.0e-10, 0.01),
    ] {
        dae.variables.parameters.insert(
            VarName::new(name),
            named_real(
                name,
                start,
                Some(min),
                Some(max),
                dae::VariableCausality::Parameter,
            ),
        );
    }

    dae.variables.parameters.insert(
        VarName::new("'limiter.uMin'"),
        named_real(
            "'limiter.uMin'",
            -400.0,
            Some(-1.0e5),
            Some(-1.0),
            dae::VariableCausality::CalculatedParameter,
        ),
    );

    for name in ["'PID.I.x'", "'PID.D.x'", "'previous(feedback.y)'"] {
        dae.variables.discrete_reals.insert(
            VarName::new(name),
            named_real(name, 0.0, None, None, dae::VariableCausality::Local),
        );
    }
    dae.variables.discrete_valued.insert(
        VarName::new("firstTick"),
        dae::Variable {
            name: VarName::new("firstTick"),
            start: Some(bool_literal(true)),
            ..Default::default()
        },
    );

    for name in [
        "'gain.y'",
        "'feedback.y'",
        "'derivative(PID.I.x)'",
        "'derivative(PID.D.x)'",
        "'PID.D.y'",
        "'PID.y'",
    ] {
        dae.variables
            .algebraics
            .insert(VarName::new(name), local_real(name));
    }

    dae.initialization.equations.push(dae::Equation::explicit(
        VarName::new("'limiter.uMin'"),
        unary(OpUnary::Minus, var_ref("'limiter.uMax'")),
        Span::DUMMY,
        "PID dependent limiter lower bound",
    ));
    dae.initialization.equations.push(dae::Equation::explicit(
        VarName::new("'PID.I.x'"),
        real_literal(0.0),
        Span::DUMMY,
        "PID integral state init",
    ));
    dae.initialization.equations.push(dae::Equation::explicit(
        VarName::new("'PID.D.x'"),
        real_literal(0.0),
        Span::DUMMY,
        "PID derivative state init",
    ));
    dae.initialization.equations.push(dae::Equation::explicit(
        VarName::new("'previous(feedback.y)'"),
        real_literal(0.0),
        Span::DUMMY,
        "PID previous feedback init",
    ));
    dae.initialization.equations.push(dae::Equation::explicit(
        VarName::new("firstTick"),
        bool_literal(true),
        Span::DUMMY,
        "PID first tick init",
    ));
    dae.initialization.equations.push(dae::Equation::explicit(
        VarName::new("vMotor"),
        real_literal(0.0),
        Span::DUMMY,
        "PID output init",
    ));

    insert_real_update(
        &mut dae,
        "'derivative(PID.I.x)'",
        div(var_ref("'previous(feedback.y)'"), var_ref("Ti")),
        "PID integral derivative",
    );
    insert_real_update(
        &mut dae,
        "'derivative(PID.D.x)'",
        if_expr(
            vec![(var_ref("firstTick"), real_literal(0.0))],
            div(
                sub(var_ref("'previous(feedback.y)'"), var_ref("'PID.D.x'")),
                var_ref("Td"),
            ),
        ),
        "PID derivative derivative with first-tick latch",
    );
    insert_real_update(
        &mut dae,
        "'PID.I.x'",
        add(
            var_ref("'PID.I.x'"),
            mul(var_ref("stepSize"), var_ref("'derivative(PID.I.x)'")),
        ),
        "PID integral Euler update",
    );
    insert_real_update(
        &mut dae,
        "'PID.D.x'",
        add(
            var_ref("'PID.D.x'"),
            mul(var_ref("stepSize"), var_ref("'derivative(PID.D.x)'")),
        ),
        "PID derivative Euler update",
    );
    insert_real_update(
        &mut dae,
        "'gain.y'",
        mul(var_ref("gearRatio"), var_ref("wLoadRef")),
        "PID gain",
    );
    insert_real_update(
        &mut dae,
        "'feedback.y'",
        sub(var_ref("'gain.y'"), var_ref("wMotor")),
        "PID feedback",
    );
    insert_real_update(
        &mut dae,
        "'PID.D.y'",
        div(
            mul(
                var_ref("kd"),
                sub(var_ref("'feedback.y'"), var_ref("'PID.D.x'")),
            ),
            var_ref("Td"),
        ),
        "PID derivative output",
    );
    insert_real_update(
        &mut dae,
        "'PID.y'",
        mul(
            var_ref("k"),
            add(
                add(var_ref("'PID.D.y'"), var_ref("'PID.I.x'")),
                var_ref("'feedback.y'"),
            ),
        ),
        "PID sum",
    );
    insert_real_update(
        &mut dae,
        "vMotor",
        if_expr(
            vec![
                (
                    gt(var_ref("'PID.y'"), var_ref("'limiter.uMax'")),
                    var_ref("'limiter.uMax'"),
                ),
                (
                    lt(var_ref("'PID.y'"), var_ref("'limiter.uMin'")),
                    var_ref("'limiter.uMin'"),
                ),
            ],
            var_ref("'PID.y'"),
        ),
        "PID output limiter",
    );
    insert_real_update(
        &mut dae,
        "'previous(feedback.y)'",
        var_ref("'feedback.y'"),
        "PID previous feedback update",
    );
    insert_valued_update(
        &mut dae,
        "firstTick",
        bool_literal(false),
        "PID first tick latch without branch",
    );

    dae
}

fn normal_derivative_d_x_source_expr() -> Expression {
    div(
        sub(var_ref("'previous(feedback.y)'"), var_ref("'PID.D.x'")),
        var_ref("Td"),
    )
}

fn first_tick_derivative_source_statement() -> Statement {
    Statement::If {
        cond_blocks: vec![StatementBlock {
            cond: var_ref("firstTick"),
            stmts: vec![assignment_stmt("'derivative(PID.D.x)'", real_literal(0.0))],
        }],
        else_block: Some(vec![assignment_stmt(
            "'derivative(PID.D.x)'",
            normal_derivative_d_x_source_expr(),
        )]),
        span: Span::DUMMY,
    }
}

fn rewrite_first_tick_derivative_as_statement_if(galec: &mut GalecModel) {
    galec.step.statements[1] = lower_statement_to_galec(&first_tick_derivative_source_statement())
        .expect("firstTick statement should lower to GALEC IR");
}

fn lower_efmi_pid_subset_to_galec() -> GalecModel {
    let dae = efmi_pid_subset_dae();
    let admissible = check_galec_admissible(&dae, GalecProfile::Efmi10)
        .expect("representable PID subset should be GALEC-admissible");
    let mut galec = lower_to_galec(admissible, "PID_Controller")
        .expect("representable PID subset should lower to GALEC IR");
    rewrite_first_tick_derivative_as_statement_if(&mut galec);
    galec
}

#[test]
fn lowers_efmi_pid_controller_representable_subset() {
    let galec = lower_efmi_pid_subset_to_galec();

    assert_eq!(galec.name, "PID_Controller");
    assert_eq!(galec.sample_time.period_seconds, 0.001);
    assert_eq!(galec.sample_time.variable_name, "samplePeriod");

    assert_eq!(
        galec
            .interface
            .inputs
            .iter()
            .map(|variable| variable.name.as_str())
            .collect::<Vec<_>>(),
        vec!["wLoadRef", "wMotor"]
    );
    assert_eq!(galec.interface.outputs[0].name, "vMotor");

    let limiter_min = galec
        .interface
        .parameters
        .iter()
        .find(|variable| variable.name == "'limiter.uMin'")
        .expect("dependent limiter lower bound should be in the parameter interface");
    assert_eq!(limiter_min.role, GalecVariableRole::DependentParameter);

    let first_tick = galec
        .interface
        .states
        .iter()
        .find(|variable| variable.name == "firstTick")
        .expect("firstTick should lower as a GALEC state");
    assert_eq!(first_tick.ty, GalecType::Boolean);

    assert!(galec.recalibrate.statements.contains(&GalecStmt::Assign {
        lhs: "'limiter.uMin'".to_string(),
        rhs: GalecExpr::Neg(Box::new(GalecExpr::Variable("'limiter.uMax'".to_string()))),
    }));
    assert_eq!(galec.step.statements.len(), 11);
    assert_eq!(
        galec.step.statements[0],
        GalecStmt::Assign {
            lhs: "'derivative(PID.I.x)'".to_string(),
            rhs: GalecExpr::Div(
                Box::new(GalecExpr::Variable("'previous(feedback.y)'".to_string())),
                Box::new(GalecExpr::Variable("Ti".to_string())),
            ),
        }
    );
    assert_eq!(
        galec.step.statements[1],
        GalecStmt::If {
            branches: vec![(
                GalecExpr::Variable("firstTick".to_string()),
                vec![GalecStmt::Assign {
                    lhs: "'derivative(PID.D.x)'".to_string(),
                    rhs: GalecExpr::RealLiteral(0.0),
                }],
            )],
            else_branch: vec![GalecStmt::Assign {
                lhs: "'derivative(PID.D.x)'".to_string(),
                rhs: GalecExpr::Div(
                    Box::new(GalecExpr::Sub(
                        Box::new(GalecExpr::Variable("'previous(feedback.y)'".to_string())),
                        Box::new(GalecExpr::Variable("'PID.D.x'".to_string())),
                    )),
                    Box::new(GalecExpr::Variable("Td".to_string())),
                ),
            }],
        }
    );
    assert_eq!(
        galec.step.statements[8],
        GalecStmt::Assign {
            lhs: "vMotor".to_string(),
            rhs: GalecExpr::If {
                branches: vec![
                    (
                        GalecExpr::Gt(
                            Box::new(GalecExpr::Variable("'PID.y'".to_string())),
                            Box::new(GalecExpr::Variable("'limiter.uMax'".to_string())),
                        ),
                        GalecExpr::Variable("'limiter.uMax'".to_string()),
                    ),
                    (
                        GalecExpr::Lt(
                            Box::new(GalecExpr::Variable("'PID.y'".to_string())),
                            Box::new(GalecExpr::Variable("'limiter.uMin'".to_string())),
                        ),
                        GalecExpr::Variable("'limiter.uMin'".to_string()),
                    ),
                ],
                else_expr: Box::new(GalecExpr::Variable("'PID.y'".to_string())),
            },
        },
    );
}

#[test]
fn renders_efmi_pid_controller_subset_to_galec_text() {
    let galec = lower_efmi_pid_subset_to_galec();
    let rendered = render_galec(&galec).expect("representable PID subset should render as GALEC");

    assert!(rendered.contains("block PID_Controller"));
    assert!(rendered.contains("input Real wLoadRef(min = -100000.0, max = 100000.0);"));
    assert!(rendered.contains("output Real vMotor(min = -10000000.0, max = 10000000.0);"));
    assert!(rendered.contains("parameter Real 'limiter.uMax'(min = 1.0, max = 100000.0);"));
    assert!(rendered.contains("protected\n    parameter Real 'limiter.uMin'"));
    assert!(rendered.contains("Boolean firstTick;"));
    assert!(rendered.contains("method Startup"));
    assert!(rendered.contains("method Recalibrate"));
    assert!(rendered.contains("method DoStep"));
    assert!(rendered.contains("Real 'gain.y';"));
    assert!(rendered.contains("self.'limiter.uMin' := (-self.'limiter.uMax');"));
    assert!(rendered.contains("'derivative(PID.I.x)' := (self.'previous(feedback.y)' / self.Ti);"));
    assert!(rendered.contains("if self.firstTick then"));
    assert!(rendered.contains("'derivative(PID.D.x)' := 0.0;"));
    assert!(rendered.contains("else"));
    assert!(rendered.contains(
        "'derivative(PID.D.x)' := ((self.'previous(feedback.y)' - self.'PID.D.x') / self.Td);"
    ));
    assert!(rendered.contains("end if;"));
    assert!(rendered.contains(
        "self.vMotor := (if ('PID.y' > self.'limiter.uMax') then self.'limiter.uMax' elseif ('PID.y' < self.'limiter.uMin') then self.'limiter.uMin' else 'PID.y');"
    ));
    assert!(rendered.contains("end PID_Controller;"));
}
