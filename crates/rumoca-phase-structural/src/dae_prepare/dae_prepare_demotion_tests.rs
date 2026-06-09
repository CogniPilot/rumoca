use super::*;
use indexmap::IndexSet;
use rumoca_core::Span;

fn var(name: &str) -> Expression {
    Expression::VarRef {
        name: rumoca_core::Reference::new(name),
        subscripts: vec![],
        span: rumoca_core::Span::DUMMY,
    }
}

fn var_idx(name: &str, idx: i64) -> Expression {
    Expression::VarRef {
        name: rumoca_core::Reference::new(name),
        subscripts: vec![Subscript::generated_index(idx, rumoca_core::Span::DUMMY)],
        span: rumoca_core::Span::DUMMY,
    }
}

fn var_sub(name: &str, subscript: Expression) -> Expression {
    Expression::VarRef {
        name: rumoca_core::Reference::new(name),
        subscripts: vec![Subscript::generated_expr(Box::new(subscript))],
        span: rumoca_core::Span::DUMMY,
    }
}

fn int(v: i64) -> Expression {
    Expression::Literal {
        value: Literal::Integer(v),
        span: rumoca_core::Span::DUMMY,
    }
}

fn real(v: f64) -> Expression {
    Expression::Literal {
        value: Literal::Real(v),
        span: rumoca_core::Span::DUMMY,
    }
}

fn sub(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op: OpBinary::Sub,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: rumoca_core::Span::DUMMY,
    }
}

fn neg(rhs: Expression) -> Expression {
    Expression::Unary {
        op: OpUnary::Minus,
        rhs: Box::new(rhs),
        span: rumoca_core::Span::DUMMY,
    }
}

fn mul(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op: OpBinary::Mul,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: rumoca_core::Span::DUMMY,
    }
}

fn div(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op: OpBinary::Div,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: rumoca_core::Span::DUMMY,
    }
}

fn lt(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op: OpBinary::Lt,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: rumoca_core::Span::DUMMY,
    }
}

fn gt(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op: OpBinary::Gt,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: rumoca_core::Span::DUMMY,
    }
}

fn no_event(expr: Expression) -> Expression {
    Expression::BuiltinCall {
        function: BuiltinFunction::NoEvent,
        args: vec![expr],
        span: rumoca_core::Span::DUMMY,
    }
}

fn array(elements: Vec<Expression>) -> Expression {
    Expression::Array {
        elements,
        is_matrix: false,
        span: Span::DUMMY,
    }
}

fn call(name: &str, args: Vec<Expression>) -> Expression {
    Expression::FunctionCall {
        name: rumoca_core::Reference::new(name),
        args,
        is_constructor: false,
        span: Span::DUMMY,
    }
}

fn der(name: &str) -> Expression {
    Expression::BuiltinCall {
        function: BuiltinFunction::Der,
        args: vec![var(name)],
        span: rumoca_core::Span::DUMMY,
    }
}

fn der_idx(name: &str, idx: i64) -> Expression {
    Expression::BuiltinCall {
        function: BuiltinFunction::Der,
        args: vec![var_idx(name, idx)],
        span: rumoca_core::Span::DUMMY,
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
fn test_assignable_derivative_rows_keep_rows_with_non_state_rhs_aliases() {
    let mut dae = Dae::new();
    dae.variables
        .states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.variables
        .algebraics
        .insert(VarName::new("dx"), Variable::new(VarName::new("dx")));

    dae.continuous.equations.push(eq(sub(var("dx"), der("x"))));

    let demoted = demote_states_without_assignable_derivative_rows(&mut dae);
    assert_eq!(demoted, 0);
    assert!(dae.variables.states.contains_key(&VarName::new("x")));
}

#[test]
fn test_demote_states_without_derivative_refs_uses_exact_state_path() {
    let mut dae = Dae::new();
    let mut is = Variable::new(VarName::new("imc.is"));
    is.dims = vec![3];
    dae.variables.states.insert(VarName::new("imc.is"), is);
    let mut idq = Variable::new(VarName::new("imc.idq_rs"));
    idq.dims = vec![2];
    dae.variables.states.insert(VarName::new("imc.idq_rs"), idq);
    dae.variables.algebraics.insert(
        VarName::new("imc.plug_sp.pin[1].i"),
        Variable::new(VarName::new("imc.plug_sp.pin[1].i")),
    );

    dae.continuous
        .equations
        .push(eq(sub(var_idx("imc.is", 1), var("imc.plug_sp.pin[1].i"))));
    dae.continuous
        .equations
        .push(eq(sub(der_idx("imc.idq_rs", 1), var_idx("imc.is", 1))));

    let demoted = demote_states_without_derivative_refs(&mut dae);

    assert_eq!(demoted, 1);
    assert!(!dae.variables.states.contains_key(&VarName::new("imc.is")));
    assert!(
        dae.variables
            .algebraics
            .contains_key(&VarName::new("imc.is"))
    );
    assert!(
        dae.variables
            .states
            .contains_key(&VarName::new("imc.idq_rs"))
    );
}

#[test]
fn test_demote_states_ignores_derivatives_in_static_inactive_if_branch() {
    let mut dae = Dae::new();
    dae.variables.states.insert(
        VarName::new("pump.T"),
        Variable::new(VarName::new("pump.T")),
    );
    dae.variables
        .algebraics
        .insert(VarName::new("q"), Variable::new(VarName::new("q")));
    let mut mass = Variable::new(VarName::new("pump.m"));
    mass.start = Some(int(0));
    dae.variables
        .parameters
        .insert(VarName::new("pump.m"), mass);

    dae.continuous.equations.push(eq(Expression::If {
        branches: vec![(
            no_event(gt(var("pump.m"), real(f64::MIN_POSITIVE))),
            sub(var("q"), mul(var("pump.m"), der("pump.T"))),
        )],
        else_branch: Box::new(sub(var("q"), int(0))),
        span: Span::DUMMY,
    }));

    let demoted = demote_states_without_derivative_refs(&mut dae);

    assert_eq!(demoted, 1);
    assert!(!dae.variables.states.contains_key(&VarName::new("pump.T")));
    assert!(
        dae.variables
            .algebraics
            .contains_key(&VarName::new("pump.T"))
    );
}

#[test]
fn test_demote_states_keeps_derivatives_in_static_active_if_branch() {
    let mut dae = Dae::new();
    dae.variables.states.insert(
        VarName::new("pump.T"),
        Variable::new(VarName::new("pump.T")),
    );
    dae.variables
        .algebraics
        .insert(VarName::new("q"), Variable::new(VarName::new("q")));
    let mut mass = Variable::new(VarName::new("pump.m"));
    mass.start = Some(int(2));
    dae.variables
        .parameters
        .insert(VarName::new("pump.m"), mass);

    dae.continuous.equations.push(eq(Expression::If {
        branches: vec![(
            no_event(gt(var("pump.m"), real(f64::MIN_POSITIVE))),
            sub(var("q"), mul(var("pump.m"), der("pump.T"))),
        )],
        else_branch: Box::new(sub(var("q"), int(0))),
        span: Span::DUMMY,
    }));

    let demoted = demote_states_without_derivative_refs(&mut dae);

    assert_eq!(demoted, 0);
    assert!(dae.variables.states.contains_key(&VarName::new("pump.T")));
}

#[test]
fn test_assignable_derivative_rows_reject_non_state_derivatives() {
    let mut dae = Dae::new();
    dae.variables
        .states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.variables
        .algebraics
        .insert(VarName::new("a"), Variable::new(VarName::new("a")));

    dae.continuous.equations.push(eq(sub(der("x"), der("a"))));

    let demoted = demote_states_without_assignable_derivative_rows(&mut dae);

    assert_eq!(demoted, 1);
    assert!(!dae.variables.states.contains_key(&VarName::new("x")));
    assert!(dae.variables.algebraics.contains_key(&VarName::new("x")));
}

#[test]
fn test_demote_direct_assigned_states_keeps_state_defined_by_non_state_alias() {
    let mut dae = Dae::new();
    dae.variables
        .states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.variables
        .states
        .insert(VarName::new("v"), Variable::new(VarName::new("v")));
    dae.variables
        .algebraics
        .insert(VarName::new("d"), Variable::new(VarName::new("d")));
    dae.variables
        .parameters
        .insert(VarName::new("r"), Variable::new(VarName::new("r")));
    dae.variables
        .parameters
        .insert(VarName::new("g"), Variable::new(VarName::new("g")));
    dae.variables
        .parameters
        .insert(VarName::new("k"), Variable::new(VarName::new("k")));
    dae.variables
        .parameters
        .insert(VarName::new("c"), Variable::new(VarName::new("c")));

    // der(x) = v
    dae.continuous.equations.push(eq(sub(der("x"), var("v"))));
    // d = x - r
    dae.continuous
        .equations
        .push(eq(sub(var("d"), sub(var("x"), var("r")))));
    // if d < 0 then der(v) = -g - k*d - c*v else der(v) = -g
    let cond = lt(var("d"), int(0));
    let then_rhs = sub(
        der("v"),
        sub(
            sub(
                Expression::Unary {
                    op: OpUnary::Minus,
                    rhs: Box::new(var("g")),
                    span: rumoca_core::Span::DUMMY,
                },
                mul(var("k"), var("d")),
            ),
            mul(var("c"), var("v")),
        ),
    );
    let else_rhs = sub(
        der("v"),
        Expression::Unary {
            op: OpUnary::Minus,
            rhs: Box::new(var("g")),
            span: rumoca_core::Span::DUMMY,
        },
    );
    dae.continuous.equations.push(eq(Expression::If {
        branches: vec![(cond, then_rhs)],
        else_branch: Box::new(else_rhs),
        span: rumoca_core::Span::DUMMY,
    }));

    let demoted = demote_direct_assigned_states(&mut dae);
    assert_eq!(
        demoted, 0,
        "state demotion must not treat algebraic alias constraints as trajectory assignment"
    );
    assert!(dae.variables.states.contains_key(&VarName::new("x")));
    assert!(dae.variables.states.contains_key(&VarName::new("v")));
}

#[test]
fn test_constrained_dummy_reduction_keeps_state_with_direct_output_alias() {
    let mut dae = Dae::new();
    dae.variables
        .states
        .insert(VarName::new("delta"), Variable::new(VarName::new("delta")));
    dae.variables.outputs.insert(
        VarName::new("front_wheel_yaw"),
        Variable::new(VarName::new("front_wheel_yaw")),
    );
    dae.variables
        .parameters
        .insert(VarName::new("u"), Variable::new(VarName::new("u")));

    dae.continuous
        .equations
        .push(eq(sub(der("delta"), var("u"))));
    dae.continuous
        .equations
        .push(eq(sub(var("front_wheel_yaw"), var("delta"))));

    let demoted = reduce_constrained_dummy_derivatives(&mut dae);
    assert_eq!(
        demoted, 0,
        "a plain output alias must not make the source state a dummy derivative"
    );
    assert!(dae.variables.states.contains_key(&VarName::new("delta")));
    assert!(
        !dae.variables
            .algebraics
            .contains_key(&VarName::new("delta"))
    );
}

#[test]
fn test_demote_direct_assigned_states_keeps_state_with_other_state_in_alias_closure() {
    let mut dae = Dae::new();
    dae.variables
        .states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.variables
        .states
        .insert(VarName::new("y"), Variable::new(VarName::new("y")));
    dae.variables
        .algebraics
        .insert(VarName::new("p"), Variable::new(VarName::new("p")));
    dae.variables
        .algebraics
        .insert(VarName::new("n"), Variable::new(VarName::new("n")));
    dae.variables
        .algebraics
        .insert(VarName::new("z"), Variable::new(VarName::new("z")));

    // MLS Appendix B / SPEC_0003: variables that appear differentiated remain
    // states. A direct-assignment candidate is not a dummy trajectory when its
    // non-state alias closure depends on another state.
    dae.continuous.equations.push(eq(sub(der("x"), var("z"))));
    dae.continuous.equations.push(eq(sub(der("y"), int(1))));
    dae.continuous
        .equations
        .push(eq(sub(var("x"), sub(var("p"), var("n")))));
    dae.continuous.equations.push(eq(sub(var("p"), var("y"))));
    dae.continuous.equations.push(eq(sub(var("n"), int(0))));
    dae.continuous.equations.push(eq(sub(var("z"), int(1))));

    let demoted = demote_direct_assigned_states(&mut dae);
    assert_eq!(
        demoted, 0,
        "state demotion must reject alias closures that resolve through another state"
    );
    assert!(dae.variables.states.contains_key(&VarName::new("x")));
    assert!(dae.variables.states.contains_key(&VarName::new("y")));
}

#[test]
fn test_reduce_constrained_dummy_derivatives_allows_alias_closure() {
    let mut dae = Dae::new();
    for name in ["mass.s", "mass.v", "gap.s"] {
        dae.variables
            .states
            .insert(VarName::new(name), Variable::new(VarName::new(name)));
    }
    for name in ["left.s", "right.s", "gap.v"] {
        dae.variables
            .algebraics
            .insert(VarName::new(name), Variable::new(VarName::new(name)));
    }
    dae.variables.parameters.insert(
        VarName::new("fixed.s0"),
        Variable::new(VarName::new("fixed.s0")),
    );

    dae.continuous
        .equations
        .push(eq(sub(var("left.s"), var("mass.s"))));
    dae.continuous
        .equations
        .push(eq(sub(var("right.s"), var("fixed.s0"))));
    dae.continuous
        .equations
        .push(eq(sub(var("gap.s"), sub(var("right.s"), var("left.s")))));
    dae.continuous
        .equations
        .push(eq(sub(var("gap.v"), der("gap.s"))));
    dae.continuous
        .equations
        .push(eq(sub(der("mass.s"), var("mass.v"))));
    dae.continuous
        .equations
        .push(eq(sub(der("mass.v"), int(0))));

    let demoted = reduce_constrained_dummy_derivatives(&mut dae);

    assert_eq!(demoted, 1);
    assert!(!dae.variables.states.contains_key(&VarName::new("gap.s")));
    assert!(
        dae.variables
            .algebraics
            .contains_key(&VarName::new("gap.s"))
    );
    assert!(
        !dae.continuous
            .equations
            .iter()
            .any(|eq| expr_contains_der_of(&eq.rhs, &VarName::new("gap.s"))),
        "demotion must substitute derivative uses of the constrained dummy state"
    );
}

#[test]
fn test_demote_direct_assigned_states_moves_demotions_in_name_order() {
    let mut dae = Dae::new();
    dae.variables
        .algebraics
        .insert(VarName::new("base"), Variable::new(VarName::new("base")));
    dae.variables
        .states
        .insert(VarName::new("z"), Variable::new(VarName::new("z")));
    dae.variables
        .states
        .insert(VarName::new("a"), Variable::new(VarName::new("a")));
    dae.variables
        .parameters
        .insert(VarName::new("p"), Variable::new(VarName::new("p")));

    dae.continuous
        .equations
        .push(eq(sub(var("z"), var("time"))));
    dae.continuous.equations.push(eq(sub(var("a"), var("p"))));

    let demoted = demote_direct_assigned_states(&mut dae);

    assert_eq!(demoted, 2);
    let algebraic_names: Vec<&str> = dae
        .variables
        .algebraics
        .keys()
        .map(|name| name.as_str())
        .collect();
    assert_eq!(algebraic_names, vec!["base", "a", "z"]);
}

#[test]
fn test_demote_alias_states_without_der_moves_demotions_in_name_order() {
    let mut dae = Dae::new();
    dae.variables
        .algebraics
        .insert(VarName::new("base"), Variable::new(VarName::new("base")));
    dae.variables
        .algebraics
        .insert(VarName::new("b"), Variable::new(VarName::new("b")));
    dae.variables
        .states
        .insert(VarName::new("z"), Variable::new(VarName::new("z")));
    dae.variables
        .states
        .insert(VarName::new("a"), Variable::new(VarName::new("a")));

    dae.continuous.equations.push(eq(sub(var("z"), var("b"))));
    dae.continuous.equations.push(eq(sub(var("a"), var("b"))));

    let demoted =
        demote_alias_states_without_der(&mut dae).expect("state alias demotion should succeed");

    assert_eq!(demoted, 2);
    let algebraic_names: Vec<&str> = dae
        .variables
        .algebraics
        .keys()
        .map(|name| name.as_str())
        .collect();
    assert_eq!(algebraic_names, vec!["base", "b", "a", "z"]);
}

#[test]
fn test_demote_exact_alias_component_states_demotes_duplicate_alias_state() {
    let mut dae = Dae::new();
    let mut x = Variable::new(VarName::new("x"));
    x.fixed = Some(true);
    x.start = Some(int(0));
    dae.variables.states.insert(VarName::new("x"), x);
    dae.variables
        .states
        .insert(VarName::new("y"), Variable::new(VarName::new("y")));
    dae.variables
        .algebraics
        .insert(VarName::new("a"), Variable::new(VarName::new("a")));
    dae.variables
        .algebraics
        .insert(VarName::new("w"), Variable::new(VarName::new("w")));
    dae.variables
        .algebraics
        .insert(VarName::new("v"), Variable::new(VarName::new("v")));

    // MLS simple equality equations / generated connection equations:
    // x = a and y = a place x and y in one exact alias component, so only one
    // continuous trajectory is needed.
    dae.continuous.equations.push(eq(sub(var("x"), var("a"))));
    dae.continuous.equations.push(eq(sub(var("y"), var("a"))));
    dae.continuous.equations.push(eq(sub(var("w"), der("x"))));
    dae.continuous.equations.push(eq(sub(var("v"), der("y"))));

    let demoted = demote_exact_alias_component_states(&mut dae);
    assert_eq!(demoted, 1);
    assert!(dae.variables.states.contains_key(&VarName::new("x")));
    assert!(!dae.variables.states.contains_key(&VarName::new("y")));
    assert!(dae.variables.algebraics.contains_key(&VarName::new("y")));
    assert!(
        dae.continuous
            .equations
            .iter()
            .all(|eq| !expr_contains_der_of(&eq.rhs, &VarName::new("y")))
    );
    assert!(
        dae.continuous
            .equations
            .iter()
            .any(|eq| eq.rhs == sub(var("v"), der("x")))
    );
}

#[test]
fn test_eliminate_derivative_aliases_keeps_state_alias_row_after_alias_state_demotion() {
    let mut dae = Dae::new();
    let mut x = Variable::new(VarName::new("x"));
    x.fixed = Some(true);
    dae.variables.states.insert(VarName::new("x"), x);
    dae.variables
        .states
        .insert(VarName::new("y"), Variable::new(VarName::new("y")));
    dae.variables
        .states
        .insert(VarName::new("w"), Variable::new(VarName::new("w")));
    dae.variables
        .algebraics
        .insert(VarName::new("a"), Variable::new(VarName::new("a")));
    dae.variables
        .algebraics
        .insert(VarName::new("v"), Variable::new(VarName::new("v")));

    dae.continuous.equations.push(eq(sub(var("x"), var("a"))));
    dae.continuous.equations.push(eq(sub(var("y"), var("a"))));
    dae.continuous.equations.push(eq(sub(var("w"), der("x"))));
    dae.continuous.equations.push(eq(sub(var("v"), der("y"))));

    let demoted = demote_exact_alias_component_states(&mut dae);
    assert_eq!(demoted, 1);

    eliminate_derivative_aliases(&mut dae);

    assert!(dae.variables.states.contains_key(&VarName::new("w")));
    assert!(dae.variables.algebraics.contains_key(&VarName::new("y")));
    assert!(!dae.variables.algebraics.contains_key(&VarName::new("v")));
    assert!(
        dae.continuous
            .equations
            .iter()
            .any(|eq| eq.rhs == sub(var("w"), der("x")))
    );
}

#[test]
fn test_eliminate_derivative_aliases_keeps_output_derivative_alias_row() {
    let mut dae = Dae::new();
    dae.variables
        .states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.variables.algebraics.insert(
        VarName::new("dx_alias"),
        Variable::new(VarName::new("dx_alias")),
    );
    dae.variables
        .outputs
        .insert(VarName::new("y_out"), Variable::new(VarName::new("y_out")));

    dae.continuous.equations.push(eq(sub(var("x"), int(0))));
    dae.continuous
        .equations
        .push(eq(sub(var("dx_alias"), der("x"))));
    dae.continuous
        .equations
        .push(eq(sub(var("y_out"), der("x"))));

    eliminate_derivative_aliases(&mut dae);

    assert!(
        !dae.variables
            .algebraics
            .contains_key(&VarName::new("dx_alias"))
    );
    assert!(dae.variables.outputs.contains_key(&VarName::new("y_out")));
    assert!(
        dae.continuous
            .equations
            .iter()
            .any(|eq| eq.rhs == sub(var("y_out"), der("x")))
    );
    assert!(
        !dae.continuous
            .equations
            .iter()
            .any(|eq| eq.rhs == sub(var("dx_alias"), der("x")))
    );
}

#[test]
fn test_eliminate_derivative_aliases_removes_multiple_alias_rows() {
    let mut dae = Dae::new();
    dae.variables
        .states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.variables
        .algebraics
        .insert(VarName::new("dx_a"), Variable::new(VarName::new("dx_a")));
    dae.variables
        .algebraics
        .insert(VarName::new("dx_b"), Variable::new(VarName::new("dx_b")));
    dae.variables
        .outputs
        .insert(VarName::new("y_out"), Variable::new(VarName::new("y_out")));

    dae.continuous.equations.push(eq(sub(var("x"), int(0))));
    dae.continuous
        .equations
        .push(eq(sub(var("dx_a"), der("x"))));
    dae.continuous
        .equations
        .push(eq(sub(var("dx_b"), der("x"))));
    dae.continuous
        .equations
        .push(eq(sub(var("y_out"), der("x"))));

    eliminate_derivative_aliases(&mut dae);

    assert!(!dae.variables.algebraics.contains_key(&VarName::new("dx_a")));
    assert!(!dae.variables.algebraics.contains_key(&VarName::new("dx_b")));
    assert!(dae.continuous.equations.iter().all(|eq| !expr_contains_var(
        &eq.rhs,
        &VarName::new("dx_a")
    ) && !expr_contains_var(
        &eq.rhs,
        &VarName::new("dx_b")
    )));
    assert!(
        dae.continuous
            .equations
            .iter()
            .any(|eq| eq.rhs == sub(var("y_out"), der("x")))
    );
}

#[test]
fn test_eliminate_derivative_aliases_rewrites_sampled_runtime_surfaces() {
    let mut dae = Dae::new();
    dae.variables
        .states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.variables
        .states
        .insert(VarName::new("dx"), Variable::new(VarName::new("dx")));
    dae.variables.algebraics.insert(
        VarName::new("sample1.u"),
        Variable::new(VarName::new("sample1.u")),
    );
    dae.variables.discrete_reals.insert(
        VarName::new("sample1.y"),
        Variable::new(VarName::new("sample1.y")),
    );
    dae.variables.discrete_reals.insert(
        VarName::new("sample1.clock"),
        Variable::new(VarName::new("sample1.clock")),
    );

    dae.continuous.equations.push(eq(sub(var("x"), int(0))));
    dae.continuous.equations.push(eq(sub(var("dx"), der("x"))));
    dae.continuous
        .equations
        .push(eq(sub(var("sample1.u"), der("x"))));
    dae.discrete.real_updates.push(Equation {
        lhs: None,
        rhs: sub(
            var("sample1.y"),
            Expression::BuiltinCall {
                function: BuiltinFunction::Sample,
                args: vec![var("sample1.u"), var("sample1.clock")],
                span: rumoca_core::Span::DUMMY,
            },
        ),
        span: Span::DUMMY,
        origin: "sample1.y = sample(sample1.u, sample1.clock)".to_string(),
        scalar_count: 1,
    });

    eliminate_derivative_aliases(&mut dae);

    assert!(
        !dae.variables
            .algebraics
            .contains_key(&VarName::new("sample1.u"))
    );
    assert!(
        dae.discrete
            .real_updates
            .iter()
            .all(|eq| !expr_contains_var(&eq.rhs, &VarName::new("sample1.u"))),
        "runtime partitions must not retain dangling refs to eliminated derivative aliases"
    );
    assert!(
        dae.discrete
            .real_updates
            .iter()
            .any(|eq| expr_contains_der_of(&eq.rhs, &VarName::new("x"))),
        "sampled runtime surfaces should rewrite to the canonical derivative source"
    );
}

#[test]
fn test_eliminate_derivative_aliases_rewrites_runtime_surfaces() {
    let mut dae = Dae::new();
    dae.variables
        .states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.variables.algebraics.insert(
        VarName::new("dx_alias"),
        Variable::new(VarName::new("dx_alias")),
    );
    dae.variables
        .outputs
        .insert(VarName::new("y_out"), Variable::new(VarName::new("y_out")));
    dae.variables.discrete_reals.insert(
        VarName::new("sample1.u"),
        Variable::new(VarName::new("sample1.u")),
    );
    dae.variables.discrete_reals.insert(
        VarName::new("sample1.y"),
        Variable::new(VarName::new("sample1.y")),
    );
    dae.variables.discrete_valued.insert(
        VarName::new("trigger"),
        Variable::new(VarName::new("trigger")),
    );

    dae.continuous
        .equations
        .push(eq(sub(var("dx_alias"), der("x"))));
    dae.continuous
        .equations
        .push(eq(sub(var("y_out"), der("x"))));
    dae.discrete
        .real_updates
        .push(eq(sub(var("sample1.u"), var("dx_alias"))));
    dae.discrete.valued_updates.push(eq(sub(
        var("sample1.y"),
        Expression::BuiltinCall {
            function: BuiltinFunction::Sample,
            args: vec![var("dx_alias")],
            span: rumoca_core::Span::DUMMY,
        },
    )));
    dae.conditions
        .equations
        .push(eq(sub(var("trigger"), var("dx_alias"))));
    dae.conditions.relations.push(sub(var("dx_alias"), int(0)));
    dae.events
        .synthetic_root_conditions
        .push(sub(var("dx_alias"), int(0)));
    dae.clocks.constructor_exprs.push(var("dx_alias"));

    eliminate_derivative_aliases(&mut dae);

    let alias = VarName::new("dx_alias");
    assert!(
        dae.discrete
            .real_updates
            .iter()
            .all(|eq| !expr_contains_var(&eq.rhs, &alias))
    );
    assert!(
        dae.discrete
            .valued_updates
            .iter()
            .all(|eq| !expr_contains_var(&eq.rhs, &alias))
    );
    assert!(
        dae.conditions
            .equations
            .iter()
            .all(|eq| !expr_contains_var(&eq.rhs, &alias))
    );
    assert!(
        dae.conditions
            .relations
            .iter()
            .all(|expr| !expr_contains_var(expr, &alias))
    );
    assert!(
        dae.events
            .synthetic_root_conditions
            .iter()
            .all(|expr| !expr_contains_var(expr, &alias))
    );
    assert!(
        dae.clocks
            .constructor_exprs
            .iter()
            .all(|expr| !expr_contains_var(expr, &alias))
    );
}

#[test]
fn test_demote_direct_assigned_states_skips_unsliced_array_state_alias() {
    let mut dae = Dae::new();
    let mut omega = Variable::new(VarName::new("omega"));
    omega.dims = vec![3];
    dae.variables.states.insert(VarName::new("omega"), omega);

    let mut attitude_omega = Variable::new(VarName::new("attitude.omega"));
    attitude_omega.dims = vec![3];
    dae.variables
        .algebraics
        .insert(VarName::new("attitude.omega"), attitude_omega);
    let mut tau = Variable::new(VarName::new("tau"));
    tau.dims = vec![3];
    dae.variables.algebraics.insert(VarName::new("tau"), tau);

    // MLS §10.1 / SPEC_0019: `omega = attitude.omega` is an array equation.
    // Direct state demotion must not treat the unsliced array alias as a scalar
    // trajectory assignment while the retained derivative rows are indexed.
    dae.continuous
        .equations
        .push(eq(sub(var("attitude.omega"), var("omega"))));
    for idx in 1..=3 {
        dae.continuous
            .equations
            .push(eq(sub(der_idx("omega", idx), var_idx("tau", idx))));
    }

    let demoted = demote_direct_assigned_states(&mut dae);
    assert_eq!(demoted, 0);
    assert!(dae.variables.states.contains_key(&VarName::new("omega")));
    for idx in 1..=3 {
        assert!(
            dae.continuous
                .equations
                .iter()
                .any(|eq| expr_contains_der_of(&eq.rhs, &VarName::new(format!("omega[{idx}]")))),
            "indexed derivative row omega[{idx}] should be retained"
        );
    }
}

#[test]
fn test_index_reduction_differentiates_vector_function_constraint_with_structured_subscripts() {
    let mut dae = Dae::new();
    let mut q = Variable::new(VarName::new("Q"));
    q.dims = vec![4];
    dae.variables.states.insert(VarName::new("Q"), q);

    let mut function = rumoca_core::Function::new("orientationConstraint", Span::DUMMY);
    function
        .inputs
        .push(rumoca_core::FunctionParam::new("Q", "Orientation"));
    let mut output = rumoca_core::FunctionParam::new("residue", "Real");
    output.dims = vec![1];
    function.outputs.push(output);
    function.body.push(rumoca_core::Statement::Assignment {
        comp: rumoca_core::ComponentReference {
            local: false,
            span: Span::DUMMY,
            parts: vec![rumoca_core::ComponentRefPart {
                ident: "residue".to_string(),
                span: Span::DUMMY,
                subs: Vec::new(),
            }],
            def_id: None,
        },
        value: array(vec![sub(mul(var("Q"), var("Q")), int(1))]),
        span: Span::DUMMY,
    });
    dae.symbols
        .functions
        .insert(VarName::new("orientationConstraint"), function);

    dae.continuous.equations.push(eq(sub(
        array(vec![int(0)]),
        call("orientationConstraint", vec![var("Q")]),
    )));

    assert!(expr_contains_var(
        &dae.continuous.equations[0].rhs,
        &VarName::new("Q")
    ));
    assert!(
        symbolic_time_derivative(
            &dae.continuous.equations[0].rhs,
            &dae,
            &build_relaxed_derivative_map(&dae)
        )
        .is_some()
    );
    let changed = index_reduce_missing_state_derivatives_once(&mut dae)
        .expect("index reduction should succeed");

    assert_eq!(changed, 1);
    assert!(
        dae.continuous.equations.iter().any(|eq| {
            (1..=4).all(|idx| expr_contains_der_of(&eq.rhs, &VarName::new(format!("Q[{idx}]"))))
        }),
        "differentiated vector constraint should reference every structured state component"
    );
    assert!(
        dae.continuous
            .equations
            .iter()
            .all(|eq| !format!("{:?}", eq.rhs).contains("Q[")),
        "IR-DAE should preserve structured subscripts instead of embedded scalar names"
    );
}

#[test]
fn test_symbolic_function_derivative_stops_at_recursive_function_call() {
    let mut dae = Dae::new();
    dae.variables
        .states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));

    let mut function = rumoca_core::Function::new("recursiveDerivative", Span::DUMMY);
    function
        .inputs
        .push(rumoca_core::FunctionParam::new("u", "Real"));
    function
        .outputs
        .push(rumoca_core::FunctionParam::new("y", "Real"));
    function.body.push(rumoca_core::Statement::Assignment {
        comp: rumoca_core::ComponentReference {
            local: false,
            span: Span::DUMMY,
            parts: vec![rumoca_core::ComponentRefPart {
                ident: "y".to_string(),
                span: Span::DUMMY,
                subs: Vec::new(),
            }],
            def_id: None,
        },
        value: call("recursiveDerivative", vec![var("u")]),
        span: Span::DUMMY,
    });
    dae.symbols
        .functions
        .insert(VarName::new("recursiveDerivative"), function);

    let derivative = symbolic_time_derivative(
        &call("recursiveDerivative", vec![var("x")]),
        &dae,
        &build_relaxed_derivative_map(&dae),
    );
    assert!(derivative.is_none());
}

#[test]
fn test_index_reduction_accepts_coupled_vector_constraint_rows() {
    let mut dae = Dae::new();
    let mut x = Variable::new(VarName::new("x"));
    x.dims = vec![3];
    dae.variables.states.insert(VarName::new("x"), x);
    let mut y = Variable::new(VarName::new("y"));
    y.dims = vec![3];
    dae.variables.states.insert(VarName::new("y"), y);
    let mut distance = Variable::new(VarName::new("distance"));
    distance.dims = vec![3];
    dae.variables
        .algebraics
        .insert(VarName::new("distance"), distance);
    let mut vy = Variable::new(VarName::new("vy"));
    vy.dims = vec![3];
    dae.variables.algebraics.insert(VarName::new("vy"), vy);

    dae.continuous
        .equations
        .push(eq(sub(var("distance"), sub(var("y"), var("x")))));
    dae.continuous
        .equations
        .push(eq(sub(var_idx("distance", 3), int(0))));
    dae.continuous
        .equations
        .push(eq(sub(var_idx("vy", 1), der_idx("y", 1))));
    dae.continuous
        .equations
        .push(eq(sub(var_idx("vy", 2), der_idx("y", 2))));
    dae.continuous
        .equations
        .push(eq(sub(var_idx("vy", 3), der_idx("y", 3))));

    let changed = index_reduce_missing_state_derivatives_once(&mut dae)
        .expect("index reduction should succeed");

    assert_eq!(changed, 1);
    let differentiated = &dae.continuous.equations[1].rhs;
    assert!(
        expr_contains_der_of(differentiated, &VarName::new("x[3]")),
        "indexed differentiated row should preserve the missing state derivative: {differentiated:?}"
    );
    assert!(
        !expr_contains_der_of(differentiated, &VarName::new("distance")),
        "indexed algebraic derivative should be projected from the defining array equation: {differentiated:?}"
    );
}

#[test]
fn test_exact_alias_component_rewrites_derivative_of_non_state_alias_to_canonical_state() {
    let mut dae = Dae::new();
    dae.variables.states.insert(
        VarName::new("load.phi"),
        Variable::new(VarName::new("load.phi")),
    );
    dae.variables.algebraics.insert(
        VarName::new("load.flange_b.phi"),
        Variable::new(VarName::new("load.flange_b.phi")),
    );
    dae.variables.algebraics.insert(
        VarName::new("speed.flange.phi"),
        Variable::new(VarName::new("speed.flange.phi")),
    );
    dae.variables.outputs.insert(
        VarName::new("speed.w"),
        Variable::new(VarName::new("speed.w")),
    );

    // MLS §8 simple equality equations define one exact alias component:
    // load.phi = load.flange_b.phi = speed.flange.phi. Any derivative taken
    // through a non-state alias in that component must track the canonical
    // state trajectory before later derivative-alias cleanup runs.
    dae.continuous
        .equations
        .push(eq(sub(var("load.phi"), var("load.flange_b.phi"))));
    dae.continuous
        .equations
        .push(eq(sub(var("speed.flange.phi"), var("load.flange_b.phi"))));
    dae.continuous
        .equations
        .push(eq(sub(var("speed.w"), der("speed.flange.phi"))));

    let demoted = demote_exact_alias_component_states(&mut dae);
    assert_eq!(demoted, 0, "single-state alias component should not demote");
    assert!(
        dae.continuous
            .equations
            .iter()
            .any(|eq| eq.rhs == sub(var("speed.w"), der("load.phi"))),
        "derivative users of exact non-state aliases should be rewritten to the canonical state"
    );
    assert!(
        !dae.continuous
            .equations
            .iter()
            .any(|eq| eq.rhs == sub(var("speed.w"), der("speed.flange.phi"))),
        "non-state derivative alias should not survive after exact alias rewrite"
    );
}

#[test]
fn test_exact_alias_component_propagates_start_to_canonical_state() {
    let mut dae = Dae::new();
    dae.variables
        .states
        .insert(VarName::new("c.v"), Variable::new(VarName::new("c.v")));
    let mut alias = Variable::new(VarName::new("v"));
    alias.start = Some(int(0));
    dae.variables.algebraics.insert(VarName::new("v"), alias);

    // MLS §8 simple equalities define one exact alias component. If the chosen
    // canonical state lacks start/fixed metadata, it must inherit compatible
    // metadata from exact alias peers so initialization still observes the
    // declared start value on the shared trajectory.
    dae.continuous.equations.push(eq(sub(var("v"), var("c.v"))));

    let demoted = demote_exact_alias_component_states(&mut dae);
    assert_eq!(demoted, 0, "single-state alias component should not demote");
    assert_eq!(
        dae.variables
            .states
            .get(&VarName::new("c.v"))
            .and_then(|var| var.start.clone()),
        Some(int(0))
    );
}

#[test]
fn test_constrained_dummy_state_names_reports_direct_state_constraint() {
    let mut dae = Dae::new();
    for name in ["x1", "x2", "x3"] {
        dae.variables
            .states
            .insert(VarName::new(name), Variable::new(VarName::new(name)));
    }

    dae.continuous
        .equations
        .push(eq(sub(var("x1"), sub(neg(var("x2")), var("x3")))));
    dae.continuous.equations.push(eq(sub(der("x1"), int(1))));
    dae.continuous.equations.push(eq(sub(der("x2"), int(1))));
    dae.continuous.equations.push(eq(sub(der("x3"), int(1))));

    let dummy_states = constrained_dummy_state_names(&dae);

    assert_eq!(
        dummy_states,
        IndexSet::from(["x1".to_string()]),
        "direct algebraic state constraints should identify the dependent state"
    );
}

#[test]
fn test_constrained_dummy_state_names_follows_linear_alias_constraints() {
    let mut dae = Dae::new();
    for name in ["x1", "x2", "x3"] {
        dae.variables
            .states
            .insert(VarName::new(name), Variable::new(VarName::new(name)));
    }
    for name in ["i[1]", "i[2]", "i[3]"] {
        dae.variables
            .algebraics
            .insert(VarName::new(name), Variable::new(VarName::new(name)));
    }

    // Connector/current aliases expose the actual state relation only after
    // eliminating the non-state current variables:
    //   x1 + i[1] = 0
    //   x2 + i[2] = 0
    //   x3 + i[3] = 0
    //   i[1] + i[2] + i[3] = 0
    dae.continuous
        .equations
        .push(eq(add_expr(var("x1"), var_idx("i", 1))));
    dae.continuous
        .equations
        .push(eq(add_expr(var("x2"), var_idx("i", 2))));
    dae.continuous
        .equations
        .push(eq(add_expr(var("x3"), var_idx("i", 3))));
    dae.continuous.equations.push(eq(add_expr(
        add_expr(var_idx("i", 1), var_idx("i", 2)),
        var_idx("i", 3),
    )));
    dae.continuous.equations.push(eq(sub(der("x1"), int(1))));
    dae.continuous.equations.push(eq(sub(der("x2"), int(1))));
    dae.continuous.equations.push(eq(sub(der("x3"), int(1))));

    let dummy_states = constrained_dummy_state_names(&dae);

    assert_eq!(
        dummy_states,
        IndexSet::from(["x1".to_string()]),
        "linear alias constraints should identify one dependent state without model-specific names"
    );
}

#[test]
fn test_constrained_dummy_state_names_follows_constant_scaled_alias() {
    let mut dae = Dae::new();
    let mut preferred = Variable::new(VarName::new("accelerate.v"));
    preferred.state_select = rumoca_core::StateSelect::Prefer;
    dae.variables
        .states
        .insert(VarName::new("accelerate.v"), preferred);
    dae.variables.states.insert(
        VarName::new("mass.v"),
        Variable::new(VarName::new("mass.v")),
    );

    dae.continuous.equations.push(eq(sub(
        var("mass.v"),
        div(
            mul(neg(var("accelerate.v")), real(-1.0)),
            mul(real(-1.0), real(-1.0)),
        ),
    )));
    dae.continuous
        .equations
        .push(eq(sub(der("accelerate.v"), int(1))));
    dae.continuous
        .equations
        .push(eq(sub(der("mass.v"), int(1))));

    let dummy_states = constrained_dummy_state_names(&dae);

    assert_eq!(
        dummy_states,
        IndexSet::from(["mass.v".to_string()]),
        "constant scale wrappers around an exact alias should not hide duplicate states"
    );
}

#[test]
fn test_constrained_dummy_derivative_reduction_reaches_fixed_point() {
    let mut dae = Dae::new();
    for name in ["accelerate.s", "accelerate.v"] {
        let mut state = Variable::new(VarName::new(name));
        state.state_select = rumoca_core::StateSelect::Prefer;
        dae.variables.states.insert(VarName::new(name), state);
    }
    for name in ["mass.s", "mass.v"] {
        dae.variables
            .states
            .insert(VarName::new(name), Variable::new(VarName::new(name)));
    }
    dae.variables.parameters.insert(
        VarName::new("mass.L"),
        Variable::new(VarName::new("mass.L")),
    );

    dae.continuous
        .equations
        .push(eq(sub(der("accelerate.s"), var("accelerate.v"))));
    dae.continuous
        .equations
        .push(eq(sub(der("accelerate.v"), int(1))));
    dae.continuous
        .equations
        .push(eq(sub(der("mass.s"), var("mass.v"))));
    dae.continuous
        .equations
        .push(eq(sub(der("mass.v"), int(1))));
    dae.continuous.equations.push(eq(sub(
        var("accelerate.s"),
        sub(var("mass.s"), div(var("mass.L"), int(2))),
    )));
    dae.continuous.equations.push(eq(sub(
        var("mass.v"),
        div(
            mul(neg(var("accelerate.v")), real(-1.0)),
            mul(real(-1.0), real(-1.0)),
        ),
    )));

    let demoted = reduce_constrained_dummy_derivatives(&mut dae);

    assert_eq!(
        demoted, 2,
        "position and velocity alias states should both be reduced before BLT"
    );
    assert!(!dae.variables.states.contains_key(&VarName::new("mass.s")));
    assert!(!dae.variables.states.contains_key(&VarName::new("mass.v")));
    assert!(
        dae.variables
            .algebraics
            .contains_key(&VarName::new("mass.s"))
    );
    assert!(
        dae.variables
            .algebraics
            .contains_key(&VarName::new("mass.v"))
    );
}

#[test]
fn test_constrained_dummy_state_names_maps_singleton_array_component_state() {
    let mut dae = Dae::new();
    let mut x = Variable::new(VarName::new("x"));
    x.dims = vec![1];
    dae.variables.states.insert(VarName::new("x"), x);
    dae.variables
        .states
        .insert(VarName::new("y"), Variable::new(VarName::new("y")));

    dae.continuous
        .equations
        .push(eq(sub(var_idx("x", 1), var("y"))));
    dae.continuous
        .equations
        .push(eq(sub(der_idx("x", 1), int(1))));
    dae.continuous.equations.push(eq(sub(der("y"), int(1))));

    let dummy_states = constrained_dummy_state_names(&dae);

    assert_eq!(
        dummy_states.iter().next().map(String::as_str),
        Some("x"),
        "singleton state array constraints should map the indexed component back to the selected parent state"
    );
}

#[test]
fn test_constrained_dummy_derivative_reduction_rewrites_indexed_singleton_derivative() {
    let mut dae = Dae::new();
    let mut x = Variable::new(VarName::new("x"));
    x.dims = vec![1];
    dae.variables.states.insert(VarName::new("x"), x);
    dae.variables
        .states
        .insert(VarName::new("y"), Variable::new(VarName::new("y")));

    dae.continuous
        .equations
        .push(eq(sub(var_idx("x", 1), var("y"))));
    dae.continuous
        .equations
        .push(eq(sub(der_idx("x", 1), int(1))));
    dae.continuous.equations.push(eq(sub(der("y"), int(1))));

    let demoted = reduce_constrained_dummy_derivatives(&mut dae);

    assert_eq!(demoted, 1);
    assert!(!dae.variables.states.contains_key(&VarName::new("x")));
    assert!(dae.variables.algebraics.contains_key(&VarName::new("x")));
    assert!(
        dae.continuous
            .equations
            .iter()
            .any(|eq| eq.rhs == sub(der("y"), int(1))),
        "der(x[1]) should be rewritten to der(y) after singleton dummy-state demotion"
    );
}

#[test]
fn test_constrained_dummy_state_names_follows_singleton_array_alias_chain() {
    let mut dae = Dae::new();
    let mut filtered = Variable::new(VarName::new("criticalDamping.x"));
    filtered.dims = vec![1];
    dae.variables
        .states
        .insert(VarName::new("criticalDamping.x"), filtered);
    dae.variables.states.insert(
        VarName::new("firstOrder1.y"),
        Variable::new(VarName::new("firstOrder1.y")),
    );
    for name in [
        "criticalDamping.y",
        "inverseBlockConstraints.u1",
        "inverseBlockConstraints.u2",
    ] {
        dae.variables
            .algebraics
            .insert(VarName::new(name), Variable::new(VarName::new(name)));
    }

    dae.continuous.equations.push(eq(sub(
        var("criticalDamping.y"),
        var_sub("criticalDamping.x", var("criticalDamping.n")),
    )));
    dae.continuous.equations.push(eq(sub(
        var("criticalDamping.y"),
        var("inverseBlockConstraints.u1"),
    )));
    dae.continuous.equations.push(eq(sub(
        var("inverseBlockConstraints.u1"),
        var("inverseBlockConstraints.u2"),
    )));
    dae.continuous.equations.push(eq(sub(
        var("firstOrder1.y"),
        var("inverseBlockConstraints.u2"),
    )));
    dae.continuous.equations.push(eq(sub(
        der_idx("criticalDamping.x", 1),
        var("criticalDamping.u"),
    )));
    dae.continuous
        .equations
        .push(eq(sub(der("firstOrder1.y"), int(1))));

    let dummy_states = constrained_dummy_state_names(&dae);

    assert_eq!(
        dummy_states.iter().next().map(String::as_str),
        Some("criticalDamping.x"),
        "linear alias chains should expose singleton array state constraints"
    );
}

#[test]
fn test_constrained_dummy_state_names_keeps_always_state() {
    let mut dae = Dae::new();
    let mut x1 = Variable::new(VarName::new("x1"));
    x1.state_select = rumoca_core::StateSelect::Always;
    dae.variables.states.insert(VarName::new("x1"), x1);
    dae.variables
        .states
        .insert(VarName::new("x2"), Variable::new(VarName::new("x2")));

    dae.continuous.equations.push(eq(sub(var("x1"), var("x2"))));
    dae.continuous.equations.push(eq(sub(der("x1"), int(1))));
    dae.continuous.equations.push(eq(sub(der("x2"), int(1))));

    let dummy_states = constrained_dummy_state_names(&dae);

    assert!(
        dummy_states.is_empty(),
        "StateSelect.always must not be downgraded by metadata state selection"
    );
}
