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

#[test]
fn test_demote_exact_alias_component_states_demotes_duplicate_alias_state() {
    let mut dae = Dae::new();
    let mut x = Variable::new(VarName::new("x"));
    x.fixed = Some(true);
    x.start = Some(int(0));
    dae.states.insert(VarName::new("x"), x);
    dae.states
        .insert(VarName::new("y"), Variable::new(VarName::new("y")));
    dae.algebraics
        .insert(VarName::new("a"), Variable::new(VarName::new("a")));
    dae.algebraics
        .insert(VarName::new("w"), Variable::new(VarName::new("w")));
    dae.algebraics
        .insert(VarName::new("v"), Variable::new(VarName::new("v")));

    // MLS simple equality equations / generated connection equations:
    // x = a and y = a place x and y in one exact alias component, so only one
    // continuous trajectory is needed.
    dae.f_x.push(eq(sub(var("x"), var("a"))));
    dae.f_x.push(eq(sub(var("y"), var("a"))));
    dae.f_x.push(eq(sub(var("w"), der("x"))));
    dae.f_x.push(eq(sub(var("v"), der("y"))));

    let demoted = demote_exact_alias_component_states(&mut dae);
    assert_eq!(demoted, 1);
    assert!(dae.states.contains_key(&VarName::new("x")));
    assert!(!dae.states.contains_key(&VarName::new("y")));
    assert!(dae.algebraics.contains_key(&VarName::new("y")));
    assert!(
        dae.f_x
            .iter()
            .all(|eq| !expr_contains_der_of(&eq.rhs, &VarName::new("y")))
    );
    assert!(dae.f_x.iter().any(|eq| eq.rhs == sub(var("v"), der("x"))));
}

#[test]
fn test_eliminate_derivative_aliases_keeps_state_alias_row_after_alias_state_demotion() {
    let mut dae = Dae::new();
    let mut x = Variable::new(VarName::new("x"));
    x.fixed = Some(true);
    dae.states.insert(VarName::new("x"), x);
    dae.states
        .insert(VarName::new("y"), Variable::new(VarName::new("y")));
    dae.states
        .insert(VarName::new("w"), Variable::new(VarName::new("w")));
    dae.algebraics
        .insert(VarName::new("a"), Variable::new(VarName::new("a")));
    dae.algebraics
        .insert(VarName::new("v"), Variable::new(VarName::new("v")));

    dae.f_x.push(eq(sub(var("x"), var("a"))));
    dae.f_x.push(eq(sub(var("y"), var("a"))));
    dae.f_x.push(eq(sub(var("w"), der("x"))));
    dae.f_x.push(eq(sub(var("v"), der("y"))));

    let demoted = demote_exact_alias_component_states(&mut dae);
    assert_eq!(demoted, 1);

    eliminate_derivative_aliases(&mut dae);

    assert!(dae.states.contains_key(&VarName::new("w")));
    assert!(dae.algebraics.contains_key(&VarName::new("y")));
    assert!(!dae.algebraics.contains_key(&VarName::new("v")));
    assert!(dae.f_x.iter().any(|eq| eq.rhs == sub(var("w"), der("x"))));
}

#[test]
fn test_eliminate_derivative_aliases_keeps_output_derivative_alias_row() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.algebraics.insert(
        VarName::new("dx_alias"),
        Variable::new(VarName::new("dx_alias")),
    );
    dae.outputs
        .insert(VarName::new("y_out"), Variable::new(VarName::new("y_out")));

    dae.f_x.push(eq(sub(var("x"), int(0))));
    dae.f_x.push(eq(sub(var("dx_alias"), der("x"))));
    dae.f_x.push(eq(sub(var("y_out"), der("x"))));

    eliminate_derivative_aliases(&mut dae);

    assert!(!dae.algebraics.contains_key(&VarName::new("dx_alias")));
    assert!(dae.outputs.contains_key(&VarName::new("y_out")));
    assert!(
        dae.f_x
            .iter()
            .any(|eq| eq.rhs == sub(var("y_out"), der("x")))
    );
    assert!(
        !dae.f_x
            .iter()
            .any(|eq| eq.rhs == sub(var("dx_alias"), der("x")))
    );
}

#[test]
fn test_eliminate_derivative_aliases_rewrites_sampled_runtime_surfaces() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.states
        .insert(VarName::new("dx"), Variable::new(VarName::new("dx")));
    dae.algebraics.insert(
        VarName::new("sample1.u"),
        Variable::new(VarName::new("sample1.u")),
    );
    dae.discrete_reals.insert(
        VarName::new("sample1.y"),
        Variable::new(VarName::new("sample1.y")),
    );
    dae.discrete_reals.insert(
        VarName::new("sample1.clock"),
        Variable::new(VarName::new("sample1.clock")),
    );

    dae.f_x.push(eq(sub(var("x"), int(0))));
    dae.f_x.push(eq(sub(var("dx"), der("x"))));
    dae.f_x.push(eq(sub(var("sample1.u"), der("x"))));
    dae.f_z.push(Equation {
        lhs: None,
        rhs: sub(
            var("sample1.y"),
            Expression::BuiltinCall {
                function: BuiltinFunction::Sample,
                args: vec![var("sample1.u"), var("sample1.clock")],
            },
        ),
        span: Span::DUMMY,
        origin: "sample1.y = sample(sample1.u, sample1.clock)".to_string(),
        scalar_count: 1,
    });

    eliminate_derivative_aliases(&mut dae);

    assert!(!dae.algebraics.contains_key(&VarName::new("sample1.u")));
    assert!(
        dae.f_z
            .iter()
            .all(|eq| !expr_contains_var(&eq.rhs, &VarName::new("sample1.u"))),
        "runtime partitions must not retain dangling refs to eliminated derivative aliases"
    );
    assert!(
        dae.f_z
            .iter()
            .any(|eq| expr_contains_der_of(&eq.rhs, &VarName::new("x"))),
        "sampled runtime surfaces should rewrite to the canonical derivative source"
    );
}

#[test]
fn test_eliminate_derivative_aliases_rewrites_runtime_surfaces() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.algebraics.insert(
        VarName::new("dx_alias"),
        Variable::new(VarName::new("dx_alias")),
    );
    dae.outputs
        .insert(VarName::new("y_out"), Variable::new(VarName::new("y_out")));
    dae.discrete_reals.insert(
        VarName::new("sample1.u"),
        Variable::new(VarName::new("sample1.u")),
    );
    dae.discrete_reals.insert(
        VarName::new("sample1.y"),
        Variable::new(VarName::new("sample1.y")),
    );
    dae.discrete_valued.insert(
        VarName::new("trigger"),
        Variable::new(VarName::new("trigger")),
    );

    dae.f_x.push(eq(sub(var("dx_alias"), der("x"))));
    dae.f_x.push(eq(sub(var("y_out"), der("x"))));
    dae.f_z.push(eq(sub(var("sample1.u"), var("dx_alias"))));
    dae.f_m.push(eq(sub(
        var("sample1.y"),
        Expression::BuiltinCall {
            function: BuiltinFunction::Sample,
            args: vec![var("dx_alias")],
        },
    )));
    dae.f_c.push(eq(sub(var("trigger"), var("dx_alias"))));
    dae.relation.push(sub(var("dx_alias"), int(0)));
    dae.synthetic_root_conditions
        .push(sub(var("dx_alias"), int(0)));
    dae.clock_constructor_exprs.push(var("dx_alias"));

    eliminate_derivative_aliases(&mut dae);

    let alias = VarName::new("dx_alias");
    assert!(dae.f_z.iter().all(|eq| !expr_contains_var(&eq.rhs, &alias)));
    assert!(dae.f_m.iter().all(|eq| !expr_contains_var(&eq.rhs, &alias)));
    assert!(dae.f_c.iter().all(|eq| !expr_contains_var(&eq.rhs, &alias)));
    assert!(
        dae.relation
            .iter()
            .all(|expr| !expr_contains_var(expr, &alias))
    );
    assert!(
        dae.synthetic_root_conditions
            .iter()
            .all(|expr| !expr_contains_var(expr, &alias))
    );
    assert!(
        dae.clock_constructor_exprs
            .iter()
            .all(|expr| !expr_contains_var(expr, &alias))
    );
}

#[test]
fn test_exact_alias_component_rewrites_derivative_of_non_state_alias_to_canonical_state() {
    let mut dae = Dae::new();
    dae.states.insert(
        VarName::new("load.phi"),
        Variable::new(VarName::new("load.phi")),
    );
    dae.algebraics.insert(
        VarName::new("load.flange_b.phi"),
        Variable::new(VarName::new("load.flange_b.phi")),
    );
    dae.algebraics.insert(
        VarName::new("speed.flange.phi"),
        Variable::new(VarName::new("speed.flange.phi")),
    );
    dae.outputs.insert(
        VarName::new("speed.w"),
        Variable::new(VarName::new("speed.w")),
    );

    // MLS §8 simple equality equations define one exact alias component:
    // load.phi = load.flange_b.phi = speed.flange.phi. Any derivative taken
    // through a non-state alias in that component must track the canonical
    // state trajectory before later derivative-alias cleanup runs.
    dae.f_x
        .push(eq(sub(var("load.phi"), var("load.flange_b.phi"))));
    dae.f_x
        .push(eq(sub(var("speed.flange.phi"), var("load.flange_b.phi"))));
    dae.f_x
        .push(eq(sub(var("speed.w"), der("speed.flange.phi"))));

    let demoted = demote_exact_alias_component_states(&mut dae);
    assert_eq!(demoted, 0, "single-state alias component should not demote");
    assert!(
        dae.f_x
            .iter()
            .any(|eq| eq.rhs == sub(var("speed.w"), der("load.phi"))),
        "derivative users of exact non-state aliases should be rewritten to the canonical state"
    );
    assert!(
        !dae.f_x
            .iter()
            .any(|eq| eq.rhs == sub(var("speed.w"), der("speed.flange.phi"))),
        "non-state derivative alias should not survive after exact alias rewrite"
    );
}

#[test]
fn test_exact_alias_component_propagates_start_to_canonical_state() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("c.v"), Variable::new(VarName::new("c.v")));
    let mut alias = Variable::new(VarName::new("v"));
    alias.start = Some(int(0));
    dae.algebraics.insert(VarName::new("v"), alias);

    // MLS §8 simple equalities define one exact alias component. If the chosen
    // canonical state lacks start/fixed metadata, it must inherit compatible
    // metadata from exact alias peers so initialization still observes the
    // declared start value on the shared trajectory.
    dae.f_x.push(eq(sub(var("v"), var("c.v"))));

    let demoted = demote_exact_alias_component_states(&mut dae);
    assert_eq!(demoted, 0, "single-state alias component should not demote");
    assert_eq!(
        dae.states
            .get(&VarName::new("c.v"))
            .and_then(|var| var.start.clone()),
        Some(int(0))
    );
}
