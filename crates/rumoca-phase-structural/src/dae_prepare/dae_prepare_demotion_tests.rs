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

#[test]
fn scalar_projection_does_not_define_its_aggregate_owner() {
    let mut dae = Dae::new();
    let mut vector = test_variable("vector");
    vector.dims = vec![3];
    dae.variables
        .algebraics
        .insert(VarName::new("vector"), vector);
    dae.variables
        .outputs
        .insert(VarName::new("scalar"), test_variable("scalar"));
    dae.continuous
        .equations
        .push(eq(sub(var("scalar"), var_idx("vector", 2))));

    let definitions = collect_residual_defining_expr_index(&dae);

    assert!(
        definitions.get("vector").is_none(),
        "a scalar projection must not be indexed as a definition of its aggregate owner"
    );
    assert_eq!(
        definitions
            .get("scalar")
            .and_then(|candidates| candidates.first())
            .map(|candidate| &candidate.expr),
        Some(&var_idx("vector", 2))
    );
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

fn resolved_call_with_span(
    name: &str,
    args: Vec<Expression>,
    span: Span,
    instance_id: u32,
) -> Expression {
    let component_ref = rumoca_core::component_reference_from_flat_name(&VarName::new(name), span)
        .expect("structured function reference");
    let base_part_count = component_ref.parts.len();
    Expression::FunctionCall {
        name: rumoca_core::Reference::from_component_reference(component_ref)
            .with_resolved_function(rumoca_core::ResolvedFunctionReference {
                instance_id: rumoca_core::FunctionInstanceId::new(instance_id),
                base_part_count,
            }),
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

fn cross(lhs: Expression, rhs: Expression) -> Expression {
    Expression::BuiltinCall {
        function: BuiltinFunction::Cross,
        args: vec![lhs, rhs],
        span: Span::DUMMY,
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
fn compound_derivative_expansion_rewrites_initial_algebraic_derivatives() {
    let mut dae = Dae::new();
    dae.variables
        .states
        .insert(VarName::new("x"), test_variable("x"));
    dae.variables
        .algebraics
        .insert(VarName::new("y"), test_variable("y"));
    dae.continuous.equations.push(eq(sub(var("y"), var("x"))));
    dae.continuous.equations.push(eq(sub(der("x"), int(1))));
    dae.initialization.equations.push(eq(sub(der("y"), int(0))));

    assert!(needs_compound_derivative_expansion(&dae));

    expand_compound_derivatives(&mut dae);

    assert!(
        dae.initialization
            .equations
            .iter()
            .all(|eq| !expr_contains_der_of(&eq.rhs, &VarName::new("y"))),
        "initial der(y) should expand through the continuous definition y = x"
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
fn test_demote_direct_assigned_state_with_state_free_alias_and_multiple_consumers() {
    let mut dae = Dae::new();
    dae.variables
        .states
        .insert(VarName::new("x"), test_variable("x"));
    for name in ["trajectory", "first", "second", "rate"] {
        dae.variables
            .algebraics
            .insert(VarName::new(name), test_variable(name));
    }

    dae.continuous
        .equations
        .push(eq(sub(var("trajectory"), var("time"))));
    dae.continuous
        .equations
        .push(eq(sub(var("x"), var("trajectory"))));
    dae.continuous
        .equations
        .push(eq(sub(var("first"), var("x"))));
    dae.continuous
        .equations
        .push(eq(sub(var("second"), var("x"))));
    dae.continuous
        .equations
        .push(eq(sub(var("rate"), der("x"))));

    let first_consumer = dae.continuous.equations[2].clone();
    let second_consumer = dae.continuous.equations[3].clone();

    let demoted = demote_direct_assigned_states(&mut dae).expect("direct demotion should succeed");

    assert_eq!(demoted, 1);
    assert!(!dae.variables.states.contains_key(&VarName::new("x")));
    assert!(dae.variables.algebraics.contains_key(&VarName::new("x")));
    assert_eq!(dae.continuous.equations[2].rhs, first_consumer.rhs);
    assert_eq!(dae.continuous.equations[3].rhs, second_consumer.rhs);
    assert!(!expr_contains_der_of(
        &dae.continuous.equations[4].rhs,
        &VarName::new("x")
    ));
}

#[test]
fn test_demote_direct_assigned_states_preserves_always_state() {
    let mut dae = Dae::new();
    let mut x = test_variable("x");
    x.state_select = rumoca_core::StateSelect::Always;
    dae.variables.states.insert(VarName::new("x"), x);

    dae.continuous
        .equations
        .push(eq(sub(var("x"), var("time"))));

    let demoted = demote_direct_assigned_states(&mut dae).expect("direct demotion should succeed");

    assert_eq!(demoted, 0);
    assert!(dae.variables.states.contains_key(&VarName::new("x")));
}

#[test]
fn test_derivative_resolution_retries_unclosed_alias_cache_entry() {
    let mut dae = Dae::new();
    for name in ["bad", "target"] {
        dae.variables
            .algebraics
            .insert(VarName::new(name), test_variable(name));
    }
    dae.variables
        .parameters
        .insert(VarName::new("p"), test_variable("p"));

    // Resolving `bad` through the first row enters the bad -> target -> bad
    // cycle before the second row proves that bad is constant.
    dae.continuous
        .equations
        .push(eq(sub(var("bad"), var("target"))));
    dae.continuous.equations.push(eq(sub(var("bad"), var("p"))));

    let derivative_map = build_relaxed_derivative_map_for_exprs(&dae, &[var("bad"), var("target")])
        .expect("derivative alias closure should succeed");
    let target_derivative = derivative_map
        .get("target")
        .expect("the target alias should be retried after the cycle closes");

    assert_eq!(
        crate::static_eval::eval_static_number(target_derivative, &HashMap::new()),
        Some(0.0)
    );
}

#[test]
fn test_derivative_resolution_blocks_unanchored_alias_cycle() {
    let mut dae = Dae::new();
    for name in ["a", "b"] {
        dae.variables
            .algebraics
            .insert(VarName::new(name), test_variable(name));
    }
    dae.continuous.equations.push(eq(sub(var("a"), var("b"))));

    let derivative_map = build_relaxed_derivative_map_for_exprs(&dae, &[var("a")])
        .expect("an unanchored alias cycle should terminate as unresolved");

    assert!(expr_contains_der_of(
        derivative_map.get("a").expect("a should remain symbolic"),
        &VarName::new("a")
    ));
    assert!(expr_contains_der_of(
        derivative_map.get("b").expect("b should remain symbolic"),
        &VarName::new("b")
    ));
}

#[test]
fn test_derivative_resolution_preserves_selected_state_terminal() {
    let mut dae = Dae::new();
    dae.variables
        .states
        .insert(VarName::new("x"), test_variable("x"));
    dae.variables
        .parameters
        .insert(VarName::new("p"), test_variable("p"));
    dae.continuous.equations.push(eq(sub(var("x"), var("p"))));

    let derivative_map = build_relaxed_derivative_map_for_exprs(&dae, &[var("x")])
        .expect("the selected state should remain a derivative terminal");

    assert!(expr_contains_der_of(
        derivative_map.get("x").expect("x should have a terminal"),
        &VarName::new("x")
    ));
}

#[test]
fn test_derivative_resolution_closes_alias_cycle_from_state_anchor() {
    let mut dae = Dae::new();
    dae.variables
        .states
        .insert(VarName::new("x"), test_variable("x"));
    for name in ["a", "b"] {
        dae.variables
            .algebraics
            .insert(VarName::new(name), test_variable(name));
    }
    dae.continuous.equations.push(eq(sub(var("a"), var("b"))));
    dae.continuous.equations.push(eq(sub(var("b"), var("x"))));

    let derivative_map = build_relaxed_derivative_map_for_exprs(&dae, &[var("a")])
        .expect("the state anchor should close the alias component");

    for name in ["a", "b"] {
        let derivative = derivative_map
            .get(name)
            .expect("the anchored aliases should resolve");
        assert!(expr_contains_der_of(derivative, &VarName::new("x")));
        assert!(!expr_contains_der_of(derivative, &VarName::new(name)));
    }
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

    let dummy_states = constrained_dummy_state_names(&dae).expect("analysis should succeed");
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

    let function_def_id = rumoca_core::DefId::new(4_101);
    let mut function = rumoca_core::Function::new("orientationConstraint", test_span());
    function.def_id = Some(function_def_id);
    function.instance_id = Some(rumoca_core::FunctionInstanceId::new(4_101));
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
        resolved_call_with_span(
            "orientationConstraint",
            vec![var("Q")],
            constraint_span,
            4_101,
        ),
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

fn assert_derivative_references_all_q_components(derivative: &Expression) {
    assert!(
        (1..=4).all(|index| {
            expr_contains_der_of(derivative, &VarName::new(format!("Q[{index}]")))
        }),
        "the projected derivative must retain every state dependency"
    );
}

#[test]
fn test_symbolic_derivative_resolves_projected_single_function_output() {
    let constraint_span = Span::from_offsets(
        rumoca_core::SourceId::from_source_name("projected_orientation_constraint.mo"),
        10,
        30,
    );
    let mut dae = Dae::new();
    let mut q = test_variable("Q");
    q.dims = vec![4];
    dae.variables.states.insert(VarName::new("Q"), q);

    let function_def_id = rumoca_core::DefId::new(4_102);
    let mut function = rumoca_core::Function::new("orientationConstraint", test_span());
    function.def_id = Some(function_def_id);
    function.instance_id = Some(rumoca_core::FunctionInstanceId::new(4_102));
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

    let projected_call = Expression::FunctionCall {
        name: rumoca_core::Reference::from_component_reference(rumoca_core::ComponentReference {
            local: false,
            span: constraint_span,
            parts: vec![
                rumoca_core::ComponentRefPart {
                    ident: "orientationConstraint".to_string(),
                    span: constraint_span,
                    subs: Vec::new(),
                },
                rumoca_core::ComponentRefPart {
                    ident: "residue".to_string(),
                    span: constraint_span,
                    subs: Vec::new(),
                },
            ],
            def_id: Some(function_def_id),
        })
        .with_resolved_function(rumoca_core::ResolvedFunctionReference {
            instance_id: rumoca_core::FunctionInstanceId::new(4_102),
            base_part_count: 1,
        }),
        args: vec![var("Q")],
        is_constructor: false,
        span: constraint_span,
    };
    let derivative = symbolic_time_derivative(
        &projected_call,
        &dae,
        &build_relaxed_derivative_map(&dae).expect("relaxed derivative map should build"),
    )
    .expect("projected single-output function call should be differentiable");

    assert_derivative_references_all_q_components(&derivative);

    let indexed_call = Expression::Index {
        base: Box::new(resolved_call_with_span(
            "orientationConstraint",
            vec![var("Q")],
            constraint_span,
            4_102,
        )),
        subscripts: vec![Subscript::generated_index(1, constraint_span)],
        span: constraint_span,
    };
    let indexed_derivative = symbolic_time_derivative(
        &indexed_call,
        &dae,
        &build_relaxed_derivative_map(&dae).expect("relaxed derivative map should build"),
    )
    .expect("a static projection of a function result should be differentiable");
    assert_derivative_references_all_q_components(&indexed_derivative);
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

mod vector_constraint_tests;
