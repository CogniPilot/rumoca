use rumoca_core::{
    Expression, Literal, OpBinary, Reference, Span, Statement, StatementBlock, VarName,
    component_reference_from_flat_name,
};
use rumoca_ir_dae as dae;
use rumoca_phase_galec::{
    GalecDeclRole, GalecExpr, GalecMethodKind, GalecProfile, GalecStmt, GalecType,
    GalecVariableRole, check_galec_admissible, check_galec_statements_admissible,
    lower_statements_to_galec_block, lower_to_galec, template_context,
};

fn real_literal(value: f64) -> Expression {
    Expression::Literal {
        value: Literal::Real(value),
        span: Span::DUMMY,
    }
}

fn int_literal(value: i64) -> Expression {
    Expression::Literal {
        value: Literal::Integer(value),
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

fn assignment(name: &str, value: Expression) -> Statement {
    Statement::Assignment {
        comp: component_reference_from_flat_name(&VarName::new(name), Span::DUMMY)
            .expect("test component reference should be valid"),
        value,
        span: Span::DUMMY,
    }
}

fn clocked_dae() -> dae::Dae {
    let mut dae = dae::Dae::new();
    dae.clocks.schedules.push(dae::ClockSchedule {
        period_seconds: 0.02,
        phase_seconds: 0.0,
        source_span: Span::DUMMY,
    });
    dae.variables.constants.insert(
        VarName::new("samplePeriod"),
        dae::Variable {
            name: VarName::new("samplePeriod"),
            start: Some(real_literal(0.02)),
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

#[test]
fn lower_classifies_discrete_dae_variables_into_galec_roles() {
    let mut dae = clocked_dae();
    dae.variables
        .inputs
        .insert(VarName::new("u"), real_var("u"));
    dae.variables
        .outputs
        .insert(VarName::new("y"), real_var("y"));
    dae.variables
        .parameters
        .insert(VarName::new("k"), real_var("k"));
    dae.variables
        .discrete_reals
        .insert(VarName::new("x"), real_var("x"));
    dae.variables
        .algebraics
        .insert(VarName::new("tmp"), real_var("tmp"));

    let admissible = check_galec_admissible(&dae, GalecProfile::Efmi10)
        .expect("simple discrete DAE should be GALEC-admissible");
    let galec = lower_to_galec(admissible, "Gain")
        .expect("admissible discrete DAE should lower to GALEC IR");

    assert_eq!(galec.name, "Gain");
    assert_eq!(galec.sample_time.period_seconds, 0.02);
    assert_eq!(galec.sample_time.variable_name, "samplePeriod");
    assert_eq!(galec.interface.inputs[0].role, GalecVariableRole::Input);
    assert_eq!(galec.interface.outputs[0].role, GalecVariableRole::Output);
    let sample_period = galec
        .interface
        .parameters
        .iter()
        .find(|variable| variable.name == "samplePeriod")
        .expect("sample period constant should lower as a GALEC parameter");
    let k = galec
        .interface
        .parameters
        .iter()
        .find(|variable| variable.name == "k")
        .expect("tunable parameter should lower as a GALEC parameter");
    assert_eq!(sample_period.role, GalecVariableRole::Constant);
    assert_eq!(k.role, GalecVariableRole::TunableParameter);
    assert_eq!(galec.interface.states[0].role, GalecVariableRole::State);
    assert_eq!(galec.declarations[0].variable.name, "tmp");
    assert_eq!(galec.declarations[0].role, GalecDeclRole::Local);
}

#[test]
fn lower_converts_explicit_discrete_dae_update_to_step_assignment() {
    let mut dae = clocked_dae();
    dae.variables
        .inputs
        .insert(VarName::new("u"), real_var("u"));
    dae.variables
        .outputs
        .insert(VarName::new("y"), real_var("y"));
    dae.variables
        .parameters
        .insert(VarName::new("k"), real_var("k"));
    dae.discrete.real_updates.push(dae::Equation::explicit(
        VarName::new("y"),
        binary(OpBinary::Mul, var_ref("k"), var_ref("u")),
        Span::DUMMY,
        "gain update",
    ));

    let admissible = check_galec_admissible(&dae, GalecProfile::Efmi10)
        .expect("simple gain DAE should be GALEC-admissible");
    let galec =
        lower_to_galec(admissible, "Gain").expect("simple gain DAE should lower to GALEC IR");

    assert_eq!(galec.step.statements.len(), 1);
    assert_eq!(
        galec.step.statements[0],
        GalecStmt::Assign {
            lhs: "y".to_string(),
            rhs: GalecExpr::Mul(
                Box::new(GalecExpr::Variable("k".to_string())),
                Box::new(GalecExpr::Variable("u".to_string())),
            ),
        }
    );
}

#[test]
fn lowers_core_statement_if_to_galec_statement_if() {
    let statements = vec![Statement::If {
        cond_blocks: vec![StatementBlock {
            cond: var_ref("firstTick"),
            stmts: vec![
                assignment("x", real_literal(0.0)),
                assignment("y", real_literal(1.0)),
            ],
        }],
        else_block: Some(vec![
            assignment("x", real_literal(2.0)),
            assignment("y", real_literal(3.0)),
        ]),
        span: Span::DUMMY,
    }];

    check_galec_statements_admissible(&statements, GalecProfile::Efmi10)
        .expect("statement-level if should be GALEC-admissible");

    let block = lower_statements_to_galec_block(&statements)
        .expect("statement-level if should lower to GALEC statements");

    assert_eq!(
        block.statements[0],
        GalecStmt::If {
            branches: vec![(
                GalecExpr::Variable("firstTick".to_string()),
                vec![
                    GalecStmt::Assign {
                        lhs: "x".to_string(),
                        rhs: GalecExpr::RealLiteral(0.0),
                    },
                    GalecStmt::Assign {
                        lhs: "y".to_string(),
                        rhs: GalecExpr::RealLiteral(1.0),
                    },
                ],
            )],
            else_branch: vec![
                GalecStmt::Assign {
                    lhs: "x".to_string(),
                    rhs: GalecExpr::RealLiteral(2.0),
                },
                GalecStmt::Assign {
                    lhs: "y".to_string(),
                    rhs: GalecExpr::RealLiteral(3.0),
                },
            ],
        }
    );
}

#[test]
fn lower_converts_initial_equations_to_startup_block() {
    let mut dae = clocked_dae();
    dae.variables
        .discrete_reals
        .insert(VarName::new("x"), real_var("x"));
    dae.initialization.equations.push(dae::Equation::explicit(
        VarName::new("x"),
        real_literal(3.0),
        Span::DUMMY,
        "initial x",
    ));

    let admissible = check_galec_admissible(&dae, GalecProfile::Efmi10)
        .expect("initial explicit DAE should be GALEC-admissible");
    let galec =
        lower_to_galec(admissible, "Init").expect("initial explicit DAE should lower to GALEC IR");

    assert_eq!(
        galec.init.statements[0],
        GalecStmt::Assign {
            lhs: "x".to_string(),
            rhs: GalecExpr::RealLiteral(3.0),
        }
    );
    assert!(galec.recalibrate.statements.is_empty());
}

#[test]
fn lower_dependent_parameter_assignment_to_recalibrate_block() {
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

    let admissible = check_galec_admissible(&dae, GalecProfile::Efmi10)
        .expect("dependent parameter DAE should be GALEC-admissible");
    let galec = lower_to_galec(admissible, "Recal")
        .expect("dependent parameter DAE should lower to GALEC IR");

    assert_eq!(galec.recalibrate.statements.len(), 1);
    assert_eq!(
        galec.recalibrate.statements[0],
        GalecStmt::Assign {
            lhs: "gain".to_string(),
            rhs: GalecExpr::Mul(
                Box::new(GalecExpr::Variable("k".to_string())),
                Box::new(GalecExpr::RealLiteral(2.0)),
            ),
        }
    );
}

#[test]
fn lower_uses_boolean_and_integer_discrete_value_types() {
    let mut dae = clocked_dae();
    dae.variables.discrete_valued.insert(
        VarName::new("enabled"),
        dae::Variable {
            name: VarName::new("enabled"),
            start: Some(bool_literal(false)),
            causality: dae::VariableCausality::Input,
            ..Default::default()
        },
    );
    dae.variables.discrete_valued.insert(
        VarName::new("mode"),
        dae::Variable {
            name: VarName::new("mode"),
            start: Some(int_literal(0)),
            ..Default::default()
        },
    );

    let admissible = check_galec_admissible(&dae, GalecProfile::Efmi10)
        .expect("typed discrete-valued variables should be GALEC-admissible");
    let galec = lower_to_galec(admissible, "Typed")
        .expect("typed discrete-valued variables should lower to GALEC IR");

    assert_eq!(galec.interface.inputs[0].name, "enabled");
    assert_eq!(galec.interface.inputs[0].ty, GalecType::Boolean);
    assert_eq!(galec.interface.states[0].name, "mode");
    assert_eq!(galec.interface.states[0].ty, GalecType::Integer);
}

#[test]
fn lower_preserves_variable_manifest_metadata() {
    let mut dae = clocked_dae();
    dae.variables.outputs.insert(
        VarName::new("y"),
        dae::Variable {
            name: VarName::new("y"),
            start: Some(real_literal(0.0)),
            min: Some(real_literal(-1.0)),
            max: Some(real_literal(1.0)),
            nominal: Some(real_literal(0.5)),
            unit: Some("m".to_string()),
            description: Some("position".to_string()),
            ..Default::default()
        },
    );

    let admissible = check_galec_admissible(&dae, GalecProfile::Efmi10)
        .expect("metadata literals should be GALEC-admissible");
    let galec =
        lower_to_galec(admissible, "Metadata").expect("metadata literals should lower to GALEC IR");

    let y = &galec.interface.outputs[0];
    assert_eq!(y.description.as_deref(), Some("position"));
    assert_eq!(y.unit.as_deref(), Some("m"));
    assert_eq!(y.start, Some(GalecExpr::RealLiteral(0.0)));
    assert_eq!(y.min, Some(GalecExpr::RealLiteral(-1.0)));
    assert_eq!(y.max, Some(GalecExpr::RealLiteral(1.0)));
    assert_eq!(y.nominal, Some(GalecExpr::RealLiteral(0.5)));
}

#[test]
fn exposes_serializable_template_context() {
    let mut dae = clocked_dae();
    dae.variables
        .outputs
        .insert(VarName::new("y"), real_var("y"));

    let admissible = check_galec_admissible(&dae, GalecProfile::Efmi10)
        .expect("template fixture should be GALEC-admissible");
    let galec =
        lower_to_galec(admissible, "Template").expect("template fixture should lower to GALEC IR");
    let context = template_context(&galec);

    assert_eq!(context.package.model.name, "Template");
    assert_eq!(context.package.manifest.clock.variable_name, "samplePeriod");
    assert_eq!(context.package.manifest.methods.len(), 3);
    assert_eq!(
        context.package.manifest.methods[0].kind,
        GalecMethodKind::Startup
    );
    assert_eq!(
        context.package.manifest.methods[1].kind,
        GalecMethodKind::Recalibrate
    );
    assert_eq!(
        context.package.manifest.methods[2].kind,
        GalecMethodKind::DoStep
    );
    assert!(
        context
            .package
            .manifest
            .variables
            .iter()
            .any(|variable| variable.name == "samplePeriod")
    );
}
