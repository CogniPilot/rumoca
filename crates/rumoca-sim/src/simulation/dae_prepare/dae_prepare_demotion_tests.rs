use super::*;
use rumoca_core::Span;

fn var(name: &str) -> Expression {
    Expression::VarRef {
        name: VarName::new(name),
        subscripts: vec![],
    }
}

fn int(v: i64) -> Expression {
    Expression::Literal(Literal::Integer(v))
}

fn sub(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op: OpBinary::Sub(Default::default()),
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

fn mul(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op: OpBinary::Mul(Default::default()),
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

fn lt(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op: OpBinary::Lt(Default::default()),
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

fn der(name: &str) -> Expression {
    Expression::BuiltinCall {
        function: BuiltinFunction::Der,
        args: vec![var(name)],
    }
}

fn eq(rhs: Expression) -> Equation {
    Equation {
        lhs: None,
        rhs,
        span: Span::DUMMY,
        origin: "equation from ".to_string(),
        scalar_count: 1,
    }
}

#[test]
fn test_demote_direct_assigned_states_keeps_state_defined_by_non_state_alias() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.states
        .insert(VarName::new("v"), Variable::new(VarName::new("v")));
    dae.algebraics
        .insert(VarName::new("d"), Variable::new(VarName::new("d")));
    dae.parameters
        .insert(VarName::new("r"), Variable::new(VarName::new("r")));
    dae.parameters
        .insert(VarName::new("g"), Variable::new(VarName::new("g")));
    dae.parameters
        .insert(VarName::new("k"), Variable::new(VarName::new("k")));
    dae.parameters
        .insert(VarName::new("c"), Variable::new(VarName::new("c")));

    // der(x) = v
    dae.f_x.push(eq(sub(der("x"), var("v"))));
    // d = x - r
    dae.f_x.push(eq(sub(var("d"), sub(var("x"), var("r")))));
    // if d < 0 then der(v) = -g - k*d - c*v else der(v) = -g
    let cond = lt(var("d"), int(0));
    let then_rhs = sub(
        der("v"),
        sub(
            sub(
                Expression::Unary {
                    op: OpUnary::Minus(Default::default()),
                    rhs: Box::new(var("g")),
                },
                mul(var("k"), var("d")),
            ),
            mul(var("c"), var("v")),
        ),
    );
    let else_rhs = sub(
        der("v"),
        Expression::Unary {
            op: OpUnary::Minus(Default::default()),
            rhs: Box::new(var("g")),
        },
    );
    dae.f_x.push(eq(Expression::If {
        branches: vec![(cond, then_rhs)],
        else_branch: Box::new(else_rhs),
    }));

    let demoted = demote_direct_assigned_states(&mut dae);
    assert_eq!(
        demoted, 0,
        "state demotion must not treat algebraic alias constraints as trajectory assignment"
    );
    assert!(dae.states.contains_key(&VarName::new("x")));
    assert!(dae.states.contains_key(&VarName::new("v")));
}

#[test]
fn test_demote_direct_assigned_states_keeps_state_with_other_state_in_alias_closure() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.states
        .insert(VarName::new("y"), Variable::new(VarName::new("y")));
    dae.algebraics
        .insert(VarName::new("p"), Variable::new(VarName::new("p")));
    dae.algebraics
        .insert(VarName::new("n"), Variable::new(VarName::new("n")));
    dae.algebraics
        .insert(VarName::new("z"), Variable::new(VarName::new("z")));

    // MLS Appendix B / SPEC_0003: variables that appear differentiated remain
    // states. A direct-assignment candidate is not a dummy trajectory when its
    // non-state alias closure depends on another state.
    dae.f_x.push(eq(sub(der("x"), var("z"))));
    dae.f_x.push(eq(sub(der("y"), int(1))));
    dae.f_x.push(eq(sub(var("x"), sub(var("p"), var("n")))));
    dae.f_x.push(eq(sub(var("p"), var("y"))));
    dae.f_x.push(eq(sub(var("n"), int(0))));
    dae.f_x.push(eq(sub(var("z"), int(1))));

    let demoted = demote_direct_assigned_states(&mut dae);
    assert_eq!(
        demoted, 0,
        "state demotion must reject alias closures that resolve through another state"
    );
    assert!(dae.states.contains_key(&VarName::new("x")));
    assert!(dae.states.contains_key(&VarName::new("y")));
}
