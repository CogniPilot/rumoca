use super::*;
use indexmap::IndexSet;
use rumoca_core::Span;

fn test_span() -> Span {
    Span::from_offsets(
        rumoca_core::SourceId::from_source_name("dae_prepare_demotion_test.mo"),
        1,
        2,
    )
}

fn test_variable(name: &str) -> Variable {
    let mut variable = Variable::new(
        VarName::new(name),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
    variable.source_span = test_span();
    variable
}

fn var(name: &str) -> Expression {
    var_with_span(name, Span::DUMMY)
}

fn var_with_span(name: &str, span: Span) -> Expression {
    Expression::VarRef {
        name: rumoca_core::Reference::new(name),
        subscripts: vec![],
        span,
    }
}

fn var_idx(name: &str, idx: i64) -> Expression {
    var_idx_with_span(name, idx, rumoca_core::Span::DUMMY)
}

fn var_idx_with_span(name: &str, idx: i64, span: Span) -> Expression {
    Expression::VarRef {
        name: rumoca_core::Reference::new(name),
        subscripts: vec![Subscript::generated_index(idx, span)],
        span,
    }
}

fn var_sub(name: &str, subscript: Expression) -> Expression {
    Expression::VarRef {
        name: rumoca_core::Reference::new(name),
        subscripts: vec![Subscript::generated_expr(
            Box::new(subscript),
            rumoca_core::Span::DUMMY,
        )],
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

fn add(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op: OpBinary::Add,
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

fn builtin(function: BuiltinFunction, arg: Expression) -> Expression {
    Expression::BuiltinCall {
        function,
        args: vec![arg],
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
    call_with_span(name, args, Span::DUMMY)
}

fn call_with_span(name: &str, args: Vec<Expression>, span: Span) -> Expression {
    Expression::FunctionCall {
        name: rumoca_core::Reference::new(name),
        args,
        is_constructor: false,
        span,
    }
}

fn der(name: &str) -> Expression {
    Expression::BuiltinCall {
        function: BuiltinFunction::Der,
        args: vec![var_with_span(name, test_span())],
        span: test_span(),
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

fn spanned_eq(rhs: Expression, origin: &str, span: Span) -> Equation {
    Equation {
        lhs: None,
        rhs,
        span,
        origin: origin.to_string(),
        scalar_count: 1,
    }
}

#[test]
fn compound_derivative_expansion_skips_plain_state_derivatives() {
    let mut dae = Dae::new();
    let mut x = test_variable("x");
    x.dims = vec![2];
    dae.variables.states.insert(VarName::new("x"), x);
    dae.continuous
        .equations
        .push(eq(sub(der_idx("x", 1), int(1))));
    dae.continuous
        .equations
        .push(eq(sub(der_idx("x", 2), int(2))));

    assert!(!needs_compound_derivative_expansion(&dae));

    let before: Vec<Expression> = dae
        .continuous
        .equations
        .iter()
        .map(|eq| eq.rhs.clone())
        .collect();
    expand_compound_derivatives(&mut dae);
    let after: Vec<Expression> = dae
        .continuous
        .equations
        .iter()
        .map(|eq| eq.rhs.clone())
        .collect();

    assert_eq!(after, before);
}

#[test]
fn compound_derivative_expansion_keeps_algebraic_derivative_path() {
    let mut dae = Dae::new();
    dae.variables
        .states
        .insert(VarName::new("x"), test_variable("x"));
    dae.variables
        .algebraics
        .insert(VarName::new("y"), test_variable("y"));
    dae.continuous.equations.push(eq(sub(var("y"), var("x"))));
    dae.continuous.equations.push(eq(sub(der("x"), int(1))));
    dae.continuous.equations.push(eq(sub(der("y"), int(0))));

    assert!(needs_compound_derivative_expansion(&dae));

    expand_compound_derivatives(&mut dae);

    assert!(
        dae.continuous
            .equations
            .iter()
            .all(|eq| !expr_contains_der_of(&eq.rhs, &VarName::new("y"))),
        "der(y) should still expand through y = x"
    );
}

#[test]
fn test_split_linear_target_zero_remainder_uses_context_span() {
    let span = Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_structural_dae_prepare_demotion_source_12.mo",
        ),
        20,
        31,
    );
    let (_, remainder) = split_linear_target(&var_with_span("x", span), &VarName::new("x"), span)
        .expect("direct target should split");

    assert_eq!(remainder.span(), Some(span));
}

#[test]
fn test_assignable_derivative_rows_keep_rows_with_non_state_rhs_aliases() {
    let mut dae = Dae::new();
    dae.variables
        .states
        .insert(VarName::new("x"), test_variable("x"));
    dae.variables
        .algebraics
        .insert(VarName::new("dx"), test_variable("dx"));

    dae.continuous.equations.push(eq(sub(var("dx"), der("x"))));

    let demoted = demote_states_without_assignable_derivative_rows(&mut dae);
    assert_eq!(demoted, 0);
    assert!(dae.variables.states.contains_key(&VarName::new("x")));
}

#[test]
fn test_demote_states_without_derivative_refs_uses_exact_state_path() {
    let mut dae = Dae::new();
    let mut is = test_variable("imc.is");
    is.dims = vec![3];
    dae.variables.states.insert(VarName::new("imc.is"), is);
    let mut idq = test_variable("imc.idq_rs");
    idq.dims = vec![2];
    dae.variables.states.insert(VarName::new("imc.idq_rs"), idq);
    dae.variables.algebraics.insert(
        VarName::new("imc.plug_sp.pin[1].i"),
        test_variable("imc.plug_sp.pin[1].i"),
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
    dae.variables
        .states
        .insert(VarName::new("pump.T"), test_variable("pump.T"));
    dae.variables
        .algebraics
        .insert(VarName::new("q"), test_variable("q"));
    let mut mass = test_variable("pump.m");
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
    dae.variables
        .states
        .insert(VarName::new("pump.T"), test_variable("pump.T"));
    dae.variables
        .algebraics
        .insert(VarName::new("q"), test_variable("q"));
    let mut mass = test_variable("pump.m");
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
        .insert(VarName::new("x"), test_variable("x"));
    dae.variables
        .algebraics
        .insert(VarName::new("a"), test_variable("a"));

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
        .insert(VarName::new("x"), test_variable("x"));
    dae.variables
        .states
        .insert(VarName::new("v"), test_variable("v"));
    dae.variables
        .algebraics
        .insert(VarName::new("d"), test_variable("d"));
    dae.variables
        .parameters
        .insert(VarName::new("r"), test_variable("r"));
    dae.variables
        .parameters
        .insert(VarName::new("g"), test_variable("g"));
    dae.variables
        .parameters
        .insert(VarName::new("k"), test_variable("k"));
    dae.variables
        .parameters
        .insert(VarName::new("c"), test_variable("c"));

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

    let demoted = demote_direct_assigned_states(&mut dae).expect("direct demotion should succeed");
    assert_eq!(
        demoted, 0,
        "state demotion must not treat algebraic alias constraints as trajectory assignment"
    );
    assert!(dae.variables.states.contains_key(&VarName::new("x")));
    assert!(dae.variables.states.contains_key(&VarName::new("v")));
}

#[test]
fn test_demote_direct_assigned_states_allows_fixed_connection_alias() {
    let mut dae = Dae::new();
    let connection_span = Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_structural_dae_prepare_demotion_source_2.mo",
        ),
        10,
        42,
    );
    let derivative_span = Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_structural_dae_prepare_demotion_source_2.mo",
        ),
        43,
        58,
    );
    dae.variables
        .states
        .insert(VarName::new("support.phi"), test_variable("support.phi"));
    dae.variables
        .algebraics
        .insert(VarName::new("support.w"), test_variable("support.w"));
    dae.variables
        .parameters
        .insert(VarName::new("fixed.phi0"), test_variable("fixed.phi0"));

    dae.continuous.equations.push(spanned_eq(
        sub(
            var_with_span("fixed.phi0", connection_span),
            var_with_span("support.phi", connection_span),
        ),
        "connection equation: fixed.flange.phi = support.phi",
        connection_span,
    ));
    dae.continuous.equations.push(spanned_eq(
        sub(
            var_with_span("support.w", derivative_span),
            der("support.phi"),
        ),
        "equation from support.w = der(support.phi)",
        derivative_span,
    ));

    let demoted = demote_direct_assigned_states(&mut dae).expect("direct demotion should succeed");

    assert_eq!(demoted, 1);
    assert!(
        !dae.variables
            .states
            .contains_key(&VarName::new("support.phi"))
    );
    assert!(
        dae.variables
            .algebraics
            .contains_key(&VarName::new("support.phi"))
    );
    assert!(
        !dae.continuous
            .equations
            .iter()
            .any(|eq| expr_contains_der_of(&eq.rhs, &VarName::new("support.phi"))),
        "demotion must replace derivative uses of a fixed connection state"
    );
}

#[test]
fn test_demote_direct_assigned_states_rejects_state_dependent_connection_alias() {
    let mut dae = Dae::new();
    let connection_span = Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_structural_dae_prepare_demotion_source_3.mo",
        ),
        8,
        36,
    );
    dae.variables
        .states
        .insert(VarName::new("x"), test_variable("x"));
    dae.variables
        .states
        .insert(VarName::new("y"), test_variable("y"));
    dae.variables
        .algebraics
        .insert(VarName::new("p"), test_variable("p"));
    dae.variables
        .algebraics
        .insert(VarName::new("z"), test_variable("z"));

    dae.continuous.equations.push(eq(sub(der("x"), var("z"))));
    dae.continuous.equations.push(eq(sub(der("y"), int(1))));
    dae.continuous.equations.push(spanned_eq(
        sub(
            var_with_span("x", connection_span),
            var_with_span("p", connection_span),
        ),
        "connection equation: x = p",
        connection_span,
    ));
    dae.continuous.equations.push(eq(sub(var("p"), var("y"))));
    dae.continuous.equations.push(eq(sub(var("z"), int(1))));

    let demoted = demote_direct_assigned_states(&mut dae).expect("direct demotion should succeed");

    assert_eq!(
        demoted, 0,
        "connection aliases through another state must not demote the source state"
    );
    assert!(dae.variables.states.contains_key(&VarName::new("x")));
    assert!(dae.variables.states.contains_key(&VarName::new("y")));
}

#[test]
fn test_demote_direct_assigned_states_allows_fixed_state_with_extra_value_ref() {
    let mut dae = Dae::new();
    let fixed_span = Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_structural_dae_prepare_demotion_source_4.mo",
        ),
        12,
        18,
    );
    dae.variables
        .states
        .insert(VarName::new("w"), test_variable("w"));
    dae.variables
        .algebraics
        .insert(VarName::new("z"), test_variable("z"));
    dae.variables
        .parameters
        .insert(VarName::new("p"), test_variable("p"));

    dae.continuous.equations.push(spanned_eq(
        sub(
            var_with_span("w", fixed_span),
            var_with_span("p", fixed_span),
        ),
        "equation from fixed speed",
        fixed_span,
    ));
    dae.continuous.equations.push(eq(sub(var("z"), var("w"))));

    let demoted = demote_direct_assigned_states(&mut dae).expect("direct demotion should succeed");

    assert_eq!(demoted, 1);
    assert!(!dae.variables.states.contains_key(&VarName::new("w")));
    assert!(dae.variables.algebraics.contains_key(&VarName::new("w")));
}

#[test]
fn test_constrained_dummy_reduction_keeps_state_with_direct_output_alias() {
    let mut dae = Dae::new();
    dae.variables
        .states
        .insert(VarName::new("delta"), test_variable("delta"));
    dae.variables.outputs.insert(
        VarName::new("front_wheel_yaw"),
        test_variable("front_wheel_yaw"),
    );
    dae.variables
        .parameters
        .insert(VarName::new("u"), test_variable("u"));

    dae.continuous
        .equations
        .push(eq(sub(der("delta"), var("u"))));
    dae.continuous
        .equations
        .push(eq(sub(var("front_wheel_yaw"), var("delta"))));

    let demoted = reduce_constrained_dummy_derivatives(&mut dae)
        .expect("constrained dummy reduction should succeed");
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
fn test_constrained_dummy_state_names_skip_self_integrating_output_transform() {
    let mut dae = Dae::new();
    for name in ["occB", "manHours"] {
        dae.variables
            .states
            .insert(VarName::new(name), test_variable(name));
    }
    dae.variables
        .outputs
        .insert(VarName::new("occA"), test_variable("occA"));

    dae.continuous
        .equations
        .push(eq(sub(der("occB"), sub(real(0.5), var("occB")))));
    dae.continuous
        .equations
        .push(eq(sub(var("occA"), sub(real(1.0), var("occB")))));
    dae.continuous
        .equations
        .push(eq(sub(der("manHours"), var("occA"))));

    let dummy_states = constrained_dummy_state_names(&dae);
    assert!(
        dummy_states.is_empty(),
        "a self-integrating state transformed through an output must not be classified as dummy"
    );

    let demoted = reduce_constrained_dummy_derivatives(&mut dae)
        .expect("constrained dummy reduction should succeed");
    assert_eq!(
        demoted, 0,
        "a self-integrating output transform must not be demoted later"
    );
    assert!(dae.variables.states.contains_key(&VarName::new("occB")));
}

#[test]
fn test_demote_direct_assigned_states_keeps_state_with_other_state_in_alias_closure() {
    let mut dae = Dae::new();
    dae.variables
        .states
        .insert(VarName::new("x"), test_variable("x"));
    dae.variables
        .states
        .insert(VarName::new("y"), test_variable("y"));
    dae.variables
        .algebraics
        .insert(VarName::new("p"), test_variable("p"));
    dae.variables
        .algebraics
        .insert(VarName::new("n"), test_variable("n"));
    dae.variables
        .algebraics
        .insert(VarName::new("z"), test_variable("z"));

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

    let demoted = demote_direct_assigned_states(&mut dae).expect("direct demotion should succeed");
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
            .insert(VarName::new(name), test_variable(name));
    }
    for name in ["left.s", "right.s", "gap.v"] {
        dae.variables
            .algebraics
            .insert(VarName::new(name), test_variable(name));
    }
    dae.variables
        .parameters
        .insert(VarName::new("fixed.s0"), test_variable("fixed.s0"));

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

    let demoted = reduce_constrained_dummy_derivatives(&mut dae)
        .expect("constrained dummy reduction should succeed");

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
        .insert(VarName::new("base"), test_variable("base"));
    dae.variables
        .states
        .insert(VarName::new("z"), test_variable("z"));
    dae.variables
        .states
        .insert(VarName::new("a"), test_variable("a"));
    dae.variables
        .parameters
        .insert(VarName::new("p"), test_variable("p"));

    dae.continuous
        .equations
        .push(eq(sub(var("z"), var("time"))));
    dae.continuous.equations.push(eq(sub(var("a"), var("p"))));

    let demoted = demote_direct_assigned_states(&mut dae).expect("direct demotion should succeed");

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
        .insert(VarName::new("base"), test_variable("base"));
    dae.variables
        .algebraics
        .insert(VarName::new("b"), test_variable("b"));
    dae.variables
        .states
        .insert(VarName::new("z"), test_variable("z"));
    dae.variables
        .states
        .insert(VarName::new("a"), test_variable("a"));

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
    let mut x = test_variable("x");
    x.fixed = Some(true);
    x.start = Some(int(0));
    dae.variables.states.insert(VarName::new("x"), x);
    dae.variables
        .states
        .insert(VarName::new("y"), test_variable("y"));
    dae.variables
        .algebraics
        .insert(VarName::new("a"), test_variable("a"));
    dae.variables
        .algebraics
        .insert(VarName::new("w"), test_variable("w"));
    dae.variables
        .algebraics
        .insert(VarName::new("v"), test_variable("v"));

    // MLS simple equality equations / generated connection equations:
    // x = a and y = a place x and y in one exact alias component, so only one
    // continuous trajectory is needed.
    dae.continuous.equations.push(eq(sub(var("x"), var("a"))));
    dae.continuous.equations.push(eq(sub(var("y"), var("a"))));
    dae.continuous.equations.push(eq(sub(var("w"), der("x"))));
    dae.continuous.equations.push(eq(sub(var("v"), der("y"))));

    let demoted =
        demote_exact_alias_component_states(&mut dae).expect("exact alias demotion should succeed");
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
    let mut x = test_variable("x");
    x.fixed = Some(true);
    dae.variables.states.insert(VarName::new("x"), x);
    dae.variables
        .states
        .insert(VarName::new("y"), test_variable("y"));
    dae.variables
        .states
        .insert(VarName::new("w"), test_variable("w"));
    dae.variables
        .algebraics
        .insert(VarName::new("a"), test_variable("a"));
    dae.variables
        .algebraics
        .insert(VarName::new("v"), test_variable("v"));

    dae.continuous.equations.push(eq(sub(var("x"), var("a"))));
    dae.continuous.equations.push(eq(sub(var("y"), var("a"))));
    dae.continuous.equations.push(eq(sub(var("w"), der("x"))));
    dae.continuous.equations.push(eq(sub(var("v"), der("y"))));

    let demoted =
        demote_exact_alias_component_states(&mut dae).expect("exact alias demotion should succeed");
    assert_eq!(demoted, 1);

    eliminate_derivative_aliases(&mut dae).expect("derivative alias elimination should succeed");

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
        .insert(VarName::new("x"), test_variable("x"));
    dae.variables
        .algebraics
        .insert(VarName::new("dx_alias"), test_variable("dx_alias"));
    dae.variables
        .outputs
        .insert(VarName::new("y_out"), test_variable("y_out"));

    dae.continuous.equations.push(eq(sub(var("x"), int(0))));
    dae.continuous
        .equations
        .push(eq(sub(var("dx_alias"), der("x"))));
    dae.continuous
        .equations
        .push(eq(sub(var("y_out"), der("x"))));

    eliminate_derivative_aliases(&mut dae).expect("derivative alias elimination should succeed");

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
        .insert(VarName::new("x"), test_variable("x"));
    dae.variables
        .algebraics
        .insert(VarName::new("dx_a"), test_variable("dx_a"));
    dae.variables
        .algebraics
        .insert(VarName::new("dx_b"), test_variable("dx_b"));
    dae.variables
        .outputs
        .insert(VarName::new("y_out"), test_variable("y_out"));

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

    eliminate_derivative_aliases(&mut dae).expect("derivative alias elimination should succeed");

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
        .insert(VarName::new("x"), test_variable("x"));
    dae.variables
        .states
        .insert(VarName::new("dx"), test_variable("dx"));
    dae.variables
        .algebraics
        .insert(VarName::new("sample1.u"), test_variable("sample1.u"));
    dae.variables
        .discrete_reals
        .insert(VarName::new("sample1.y"), test_variable("sample1.y"));
    dae.variables.discrete_reals.insert(
        VarName::new("sample1.clock"),
        test_variable("sample1.clock"),
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

    eliminate_derivative_aliases(&mut dae).expect("derivative alias elimination should succeed");

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
        .insert(VarName::new("x"), test_variable("x"));
    dae.variables
        .algebraics
        .insert(VarName::new("dx_alias"), test_variable("dx_alias"));
    dae.variables
        .outputs
        .insert(VarName::new("y_out"), test_variable("y_out"));
    dae.variables
        .discrete_reals
        .insert(VarName::new("sample1.u"), test_variable("sample1.u"));
    dae.variables
        .discrete_reals
        .insert(VarName::new("sample1.y"), test_variable("sample1.y"));
    dae.variables
        .discrete_valued
        .insert(VarName::new("trigger"), test_variable("trigger"));

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

    eliminate_derivative_aliases(&mut dae).expect("derivative alias elimination should succeed");

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
    let mut omega = test_variable("omega");
    omega.dims = vec![3];
    dae.variables.states.insert(VarName::new("omega"), omega);

    let mut attitude_omega = test_variable("attitude.omega");
    attitude_omega.dims = vec![3];
    dae.variables
        .algebraics
        .insert(VarName::new("attitude.omega"), attitude_omega);
    let mut tau = test_variable("tau");
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

    let demoted = demote_direct_assigned_states(&mut dae).expect("direct demotion should succeed");
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
fn test_demote_direct_assigned_array_state_projects_indexed_derivative_reads() {
    let mut dae = Dae::new();
    let mut psi = test_variable("psi");
    psi.dims = vec![2];
    dae.variables.states.insert(VarName::new("psi"), psi);
    let mut v = test_variable("v");
    v.dims = vec![2];
    dae.variables.algebraics.insert(VarName::new("v"), v);

    dae.continuous
        .equations
        .push(eq(sub(var("psi"), array(vec![var("time"), var("time")]))));
    dae.continuous
        .equations
        .push(eq(sub(var_idx("v", 1), der_idx("psi", 1))));
    dae.continuous
        .equations
        .push(eq(sub(var_idx("v", 2), der_idx("psi", 2))));

    let demoted = demote_direct_assigned_states(&mut dae).expect("direct demotion should succeed");

    assert_eq!(demoted, 1);
    assert!(!dae.variables.states.contains_key(&VarName::new("psi")));
    assert!(dae.variables.algebraics.contains_key(&VarName::new("psi")));
    assert!(
        dae.continuous
            .equations
            .iter()
            .any(|eq| eq.rhs == sub(var_idx("v", 1), real(1.0))),
        "indexed der(psi[1]) should be projected to the first derivative component"
    );
    assert!(
        dae.continuous
            .equations
            .iter()
            .any(|eq| eq.rhs == sub(var_idx("v", 2), real(1.0))),
        "indexed der(psi[2]) should be projected to the second derivative component"
    );
}

#[test]
fn test_seeded_relaxed_derivative_map_resolves_algebraic_alias_derivative() {
    let mut dae = Dae::new();
    dae.variables
        .states
        .insert(VarName::new("q"), test_variable("q"));
    dae.variables
        .algebraics
        .insert(VarName::new("i"), test_variable("i"));
    dae.variables
        .algebraics
        .insert(VarName::new("u"), test_variable("u"));

    dae.continuous.equations.push(eq(sub(der("q"), var("u"))));
    dae.continuous.equations.push(eq(sub(var("i"), var("q"))));

    let der_map = build_relaxed_derivative_map_for_exprs(&dae, &[var("i")])
        .expect("seeded relaxed derivative map should build");

    assert_eq!(der_map.get("i"), Some(&var("u")));
}

#[test]
fn test_index_reduction_differentiates_vector_function_constraint_with_structured_subscripts() {
    let constraint_span = Span::from_offsets(
        rumoca_core::SourceId::from_source_name("orientation_constraint.mo"),
        40,
        64,
    );
    let mut dae = Dae::new();
    let mut q = test_variable("Q");
    q.dims = vec![4];
    dae.variables.states.insert(VarName::new("Q"), q);

    let mut function = rumoca_core::Function::new("orientationConstraint", test_span());
    function.inputs.push(rumoca_core::FunctionParam::new(
        "Q",
        "Orientation",
        test_span(),
    ));
    let mut output = rumoca_core::FunctionParam::new("residue", "Real", test_span());
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
        value: array(vec![sub(
            mul(
                var_with_span("Q", constraint_span),
                var_with_span("Q", constraint_span),
            ),
            int(1),
        )]),
        span: constraint_span,
    });
    dae.symbols
        .functions
        .insert(VarName::new("orientationConstraint"), function);

    dae.continuous.equations.push(eq(sub(
        array(vec![int(0)]),
        call_with_span("orientationConstraint", vec![var("Q")], constraint_span),
    )));

    assert!(expr_contains_var(
        &dae.continuous.equations[0].rhs,
        &VarName::new("Q")
    ));
    assert!(
        symbolic_time_derivative(
            &dae.continuous.equations[0].rhs,
            &dae,
            &build_relaxed_derivative_map(&dae).expect("relaxed derivative map should build")
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
        .insert(VarName::new("x"), test_variable("x"));

    let mut function = rumoca_core::Function::new("recursiveDerivative", test_span());
    function
        .inputs
        .push(rumoca_core::FunctionParam::new("u", "Real", test_span()));
    function
        .outputs
        .push(rumoca_core::FunctionParam::new("y", "Real", test_span()));
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
        &build_relaxed_derivative_map(&dae).expect("relaxed derivative map should build"),
    );
    assert!(derivative.is_none());
}

#[test]
fn test_expand_derivative_preserves_initial_condition_span() {
    let initial_span = Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_structural_dae_prepare_demotion_source_1.mo",
        ),
        10,
        17,
    );
    let if_span = Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_structural_dae_prepare_demotion_source_1.mo",
        ),
        4,
        30,
    );
    let expr = Expression::If {
        branches: vec![(
            Expression::BuiltinCall {
                function: BuiltinFunction::Initial,
                args: vec![],
                span: initial_span,
            },
            der("x"),
        )],
        else_branch: Box::new(real(0.0)),
        span: if_span,
    };
    let mut dae = Dae::new();
    dae.variables
        .states
        .insert(VarName::new("x"), test_variable("x"));

    let expanded = expand_der_in_expr_full(
        &expr,
        &dae,
        &build_relaxed_derivative_map(&dae).expect("relaxed derivative map should build"),
        &std::collections::HashSet::from(["x".to_string()]),
    );

    let Expression::If { branches, span, .. } = expanded else {
        panic!("expected derivative expansion to preserve if expression");
    };
    assert_eq!(span, if_span);
    let Expression::BuiltinCall {
        function: BuiltinFunction::Initial,
        span,
        ..
    } = &branches[0].0
    else {
        panic!("expected if condition to remain initial()");
    };
    assert_eq!(*span, initial_span);
}

#[test]
fn test_index_reduction_accepts_coupled_vector_constraint_rows() {
    let indexed_constraint_span = Span::from_offsets(
        rumoca_core::SourceId::from_source_name("vector_constraint.mo"),
        120,
        131,
    );
    let mut dae = Dae::new();
    let mut x = test_variable("x");
    x.dims = vec![3];
    dae.variables.states.insert(VarName::new("x"), x);
    let mut y = test_variable("y");
    y.dims = vec![3];
    dae.variables.states.insert(VarName::new("y"), y);
    let mut distance = test_variable("distance");
    distance.dims = vec![3];
    dae.variables
        .algebraics
        .insert(VarName::new("distance"), distance);
    let mut vy = test_variable("vy");
    vy.dims = vec![3];
    dae.variables.algebraics.insert(VarName::new("vy"), vy);

    dae.continuous
        .equations
        .push(eq(sub(var("distance"), sub(var("y"), var("x")))));
    dae.continuous.equations.push(eq(sub(
        var_idx_with_span("distance", 3, indexed_constraint_span),
        int(0),
    )));
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
fn test_symbolic_time_derivative_handles_rotation_matrix_transpose() {
    let mut dae = Dae::new();
    dae.variables
        .states
        .insert(VarName::new("theta"), test_variable("theta"));
    dae.variables.states.insert(VarName::new("psi"), {
        let mut psi = test_variable("psi");
        psi.dims = vec![2];
        psi
    });
    dae.variables
        .algebraics
        .insert(VarName::new("omega"), test_variable("omega"));

    let rotation = array(vec![
        array(vec![
            builtin(BuiltinFunction::Cos, var("theta")),
            neg(builtin(BuiltinFunction::Sin, var("theta"))),
        ]),
        array(vec![
            builtin(BuiltinFunction::Sin, var("theta")),
            builtin(BuiltinFunction::Cos, var("theta")),
        ]),
    ]);
    let expr = mul(builtin(BuiltinFunction::Transpose, rotation), var("psi"));
    let der_map = HashMap::from([
        ("theta".to_string(), var("omega")),
        ("psi".to_string(), array(vec![var("v1"), var("v2")])),
    ]);

    let derivative = symbolic_time_derivative(&expr, &dae, &der_map)
        .expect("rotation-frame vector derivative should be symbolic");

    assert!(
        !expr_contains_der_of_non_state(
            &derivative,
            &HashSet::from(["theta".to_string(), "psi".to_string()])
        ),
        "derivative should not leave der(non-state) calls: {derivative:?}"
    );
    assert!(
        !expr_contains_der_of(&derivative, &VarName::new("psi")),
        "indexed state derivatives should project through the derivative map: {derivative:?}"
    );
    assert!(
        format!("{derivative:?}").contains("Transpose"),
        "transpose derivative should stay structured: {derivative:?}"
    );
}

#[test]
fn test_exact_alias_component_rewrites_derivative_of_non_state_alias_to_canonical_state() {
    let mut dae = Dae::new();
    dae.variables
        .states
        .insert(VarName::new("load.phi"), test_variable("load.phi"));
    dae.variables.algebraics.insert(
        VarName::new("load.flange_b.phi"),
        test_variable("load.flange_b.phi"),
    );
    dae.variables.algebraics.insert(
        VarName::new("speed.flange.phi"),
        test_variable("speed.flange.phi"),
    );
    dae.variables
        .outputs
        .insert(VarName::new("speed.w"), test_variable("speed.w"));

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

    let demoted =
        demote_exact_alias_component_states(&mut dae).expect("exact alias demotion should succeed");
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
        .insert(VarName::new("c.v"), test_variable("c.v"));
    let mut alias = test_variable("v");
    alias.start = Some(int(0));
    dae.variables.algebraics.insert(VarName::new("v"), alias);

    // MLS §8 simple equalities define one exact alias component. If the chosen
    // canonical state lacks start/fixed metadata, it must inherit compatible
    // metadata from exact alias peers so initialization still observes the
    // declared start value on the shared trajectory.
    dae.continuous.equations.push(eq(sub(var("v"), var("c.v"))));

    let demoted =
        demote_exact_alias_component_states(&mut dae).expect("exact alias demotion should succeed");
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
            .insert(VarName::new(name), test_variable(name));
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
            .insert(VarName::new(name), test_variable(name));
    }
    for name in ["i[1]", "i[2]", "i[3]"] {
        dae.variables
            .algebraics
            .insert(VarName::new(name), test_variable(name));
    }

    // Connector/current aliases expose the actual state relation only after
    // eliminating the non-state current variables:
    //   x1 + i[1] = 0
    //   x2 + i[2] = 0
    //   x3 + i[3] = 0
    //   i[1] + i[2] + i[3] = 0
    dae.continuous
        .equations
        .push(eq(add(var("x1"), var_idx("i", 1))));
    dae.continuous
        .equations
        .push(eq(add(var("x2"), var_idx("i", 2))));
    dae.continuous
        .equations
        .push(eq(add(var("x3"), var_idx("i", 3))));
    dae.continuous.equations.push(eq(add(
        add(var_idx("i", 1), var_idx("i", 2)),
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
    let mut preferred = test_variable("accelerate.v");
    preferred.state_select = rumoca_core::StateSelect::Prefer;
    dae.variables
        .states
        .insert(VarName::new("accelerate.v"), preferred);
    dae.variables
        .states
        .insert(VarName::new("mass.v"), test_variable("mass.v"));

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
        let mut state = test_variable(name);
        state.state_select = rumoca_core::StateSelect::Prefer;
        dae.variables.states.insert(VarName::new(name), state);
    }
    for name in ["mass.s", "mass.v"] {
        dae.variables
            .states
            .insert(VarName::new(name), test_variable(name));
    }
    dae.variables
        .parameters
        .insert(VarName::new("mass.L"), test_variable("mass.L"));

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

    let demoted = reduce_constrained_dummy_derivatives(&mut dae)
        .expect("constrained dummy reduction should succeed");

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
    let mut x = test_variable("x");
    x.dims = vec![1];
    dae.variables.states.insert(VarName::new("x"), x);
    dae.variables
        .states
        .insert(VarName::new("y"), test_variable("y"));

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
    let mut x = test_variable("x");
    x.dims = vec![1];
    dae.variables.states.insert(VarName::new("x"), x);
    dae.variables
        .states
        .insert(VarName::new("y"), test_variable("y"));

    dae.continuous
        .equations
        .push(eq(sub(var_idx("x", 1), var("y"))));
    dae.continuous
        .equations
        .push(eq(sub(der_idx("x", 1), int(1))));
    dae.continuous.equations.push(eq(sub(der("y"), int(1))));

    let demoted = reduce_constrained_dummy_derivatives(&mut dae)
        .expect("constrained dummy reduction should succeed");

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
    let mut filtered = test_variable("criticalDamping.x");
    filtered.dims = vec![1];
    dae.variables
        .states
        .insert(VarName::new("criticalDamping.x"), filtered);
    dae.variables.states.insert(
        VarName::new("firstOrder1.y"),
        test_variable("firstOrder1.y"),
    );
    for name in [
        "criticalDamping.y",
        "inverseBlockConstraints.u1",
        "inverseBlockConstraints.u2",
    ] {
        dae.variables
            .algebraics
            .insert(VarName::new(name), test_variable(name));
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
    let mut x1 = test_variable("x1");
    x1.state_select = rumoca_core::StateSelect::Always;
    dae.variables.states.insert(VarName::new("x1"), x1);
    dae.variables
        .states
        .insert(VarName::new("x2"), test_variable("x2"));

    dae.continuous.equations.push(eq(sub(var("x1"), var("x2"))));
    dae.continuous.equations.push(eq(sub(der("x1"), int(1))));
    dae.continuous.equations.push(eq(sub(der("x2"), int(1))));

    let dummy_states = constrained_dummy_state_names(&dae);

    assert!(
        dummy_states.is_empty(),
        "StateSelect.always must not be downgraded by metadata state selection"
    );
}
