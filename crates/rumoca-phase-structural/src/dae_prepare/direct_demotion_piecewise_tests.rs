use super::*;

fn test_variable(name: &str) -> Variable {
    let mut variable = Variable::new(VarName::new(name), test_span());
    variable.source_span = test_span();
    variable
}

fn test_span() -> Span {
    Span::from_offsets(
        rumoca_core::SourceId::from_source_name("direct_demotion_piecewise_tests.mo"),
        1,
        2,
    )
}

fn eq(rhs: Expression) -> Equation {
    Equation::residual(rhs, Span::DUMMY, "test")
}

fn var(name: &str) -> Expression {
    Expression::VarRef {
        name: Reference::new(name),
        subscripts: vec![],
        span: Span::DUMMY,
    }
}

fn real(value: f64) -> Expression {
    Expression::Literal {
        value: Literal::Real(value),
        span: Span::DUMMY,
    }
}

fn der(name: &str) -> Expression {
    Expression::BuiltinCall {
        function: BuiltinFunction::Der,
        args: vec![var(name)],
        span: Span::DUMMY,
    }
}

fn add(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op: OpBinary::Add,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: Span::DUMMY,
    }
}

fn sub(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op: OpBinary::Sub,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: Span::DUMMY,
    }
}

fn mul(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op: OpBinary::Mul,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: Span::DUMMY,
    }
}

fn lt(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op: OpBinary::Lt,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: Span::DUMMY,
    }
}

#[test]
fn demote_direct_assigned_states_extracts_piecewise_trajectory_definition() {
    let mut dae = Dae::new();
    for name in ["s", "sd"] {
        dae.variables
            .states
            .insert(VarName::new(name), test_variable(name));
    }
    dae.variables
        .algebraics
        .insert(VarName::new("sdd"), test_variable("sdd"));
    for name in ["q", "qd"] {
        dae.variables
            .outputs
            .insert(VarName::new(name), test_variable(name));
    }

    dae.continuous.equations.push(eq(Expression::If {
        branches: vec![(lt(var("time"), real(1.0)), sub(var("s"), real(0.0)))],
        else_branch: Box::new(sub(
            Expression::If {
                branches: vec![(
                    lt(var("time"), real(2.0)),
                    sub(var("s"), mul(real(0.5), var("time"))),
                )],
                else_branch: Box::new(sub(var("s"), var("time"))),
                span: Span::DUMMY,
            },
            real(0.0),
        )),
        span: Span::DUMMY,
    }));
    dae.continuous.equations.push(eq(sub(var("sd"), der("s"))));
    dae.continuous
        .equations
        .push(eq(sub(var("sdd"), der("sd"))));
    dae.continuous
        .equations
        .push(eq(sub(var("q"), add(real(10.0), mul(real(2.0), var("s"))))));
    dae.continuous
        .equations
        .push(eq(sub(var("qd"), mul(real(2.0), var("sd")))));

    let demoted =
        demote_direct_assigned_states(&mut dae).expect("piecewise direct trajectory should demote");

    assert_eq!(demoted, 2);
    assert!(dae.variables.states.is_empty());
    for name in ["s", "sd", "sdd"] {
        assert!(dae.variables.algebraics.contains_key(&VarName::new(name)));
    }
    assert!(
        dae.continuous
            .equations
            .iter()
            .all(
                |eq| !rumoca_ir_dae::expr_contains_der_of(&eq.rhs, &VarName::new("s"))
                    && !rumoca_ir_dae::expr_contains_der_of(&eq.rhs, &VarName::new("sd"))
            ),
        "demotion should replace the derivative chain with symbolic trajectory derivatives"
    );
}
