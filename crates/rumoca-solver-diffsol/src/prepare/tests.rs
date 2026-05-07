use super::*;
use rumoca_sim_core::SimOptions;
use rumoca_sim_core::core::Span;

fn var(name: &str) -> dae::Expression {
    dae::Expression::VarRef {
        name: VarName::new(name),
        subscripts: vec![],
    }
}

fn expr_contains_var_idx(expr: &dae::Expression, target: &str, idx: i64) -> bool {
    match expr {
        dae::Expression::VarRef { name, subscripts } => {
            name.as_str() == target
                && matches!(subscripts.as_slice(), [dae::Subscript::Index(i)] if *i == idx)
        }
        dae::Expression::Binary { lhs, rhs, .. } => {
            expr_contains_var_idx(lhs, target, idx) || expr_contains_var_idx(rhs, target, idx)
        }
        dae::Expression::Unary { rhs, .. } => expr_contains_var_idx(rhs, target, idx),
        dae::Expression::BuiltinCall { args, .. } | dae::Expression::FunctionCall { args, .. } => {
            args.iter()
                .any(|arg| expr_contains_var_idx(arg, target, idx))
        }
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().any(|(condition, value)| {
                expr_contains_var_idx(condition, target, idx)
                    || expr_contains_var_idx(value, target, idx)
            }) || expr_contains_var_idx(else_branch, target, idx)
        }
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => elements
            .iter()
            .any(|element| expr_contains_var_idx(element, target, idx)),
        dae::Expression::Index { base, .. } | dae::Expression::FieldAccess { base, .. } => {
            expr_contains_var_idx(base, target, idx)
        }
        _ => false,
    }
}

fn int(v: i64) -> dae::Expression {
    dae::Expression::Literal(dae::Literal::Integer(v))
}

fn sub(lhs: dae::Expression, rhs: dae::Expression) -> dae::Expression {
    dae::Expression::Binary {
        op: rumoca_sim_core::ir_core::OpBinary::Sub(Default::default()),
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

fn add(lhs: dae::Expression, rhs: dae::Expression) -> dae::Expression {
    dae::Expression::Binary {
        op: rumoca_sim_core::ir_core::OpBinary::Add(Default::default()),
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

fn lt(lhs: dae::Expression, rhs: dae::Expression) -> dae::Expression {
    dae::Expression::Binary {
        op: rumoca_sim_core::ir_core::OpBinary::Lt(Default::default()),
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

fn eq(rhs: dae::Expression) -> dae::Equation {
    dae::Equation {
        lhs: None,
        rhs,
        span: Span::DUMMY,
        origin: "equation from ".to_string(),
        scalar_count: 1,
    }
}

fn der(name: &str) -> dae::Expression {
    dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Der,
        args: vec![var(name)],
    }
}

#[test]
fn test_prepare_demotes_state_when_runtime_alias_normalization_removes_derivative_row() {
    let mut dae = Dae::new();
    dae.states.insert(
        VarName::new("damper.phi_rel"),
        dae::Variable::new(VarName::new("damper.phi_rel")),
    );
    dae.states.insert(
        VarName::new("damper.w_rel"),
        dae::Variable::new(VarName::new("damper.w_rel")),
    );
    dae.algebraics.insert(
        VarName::new("damper.a_rel"),
        dae::Variable::new(VarName::new("damper.a_rel")),
    );

    // MLS Appendix B: damper.w_rel is a valid state only while the DAE has
    // a retained equation containing der(damper.w_rel). Runtime alias
    // normalization removes the unused acceleration alias row, matching
    // Modelica.Mechanics.Rotational.Components.Damper from PR #151.
    dae.f_x
        .push(eq(sub(var("damper.w_rel"), der("damper.phi_rel"))));
    dae.f_x
        .push(eq(sub(var("damper.a_rel"), der("damper.w_rel"))));

    let budget = TimeoutBudget::new(None);
    let (prepared, _, _) =
        prepare_dae_core(&dae, false, &budget, true).expect("prepare should not fail");

    assert!(
        prepared
            .states
            .contains_key(&VarName::new("damper.phi_rel"))
    );
    assert!(
        !prepared.states.contains_key(&VarName::new("damper.w_rel")),
        "state with no retained derivative row must be demoted before reorder"
    );
    assert!(
        prepared
            .algebraics
            .contains_key(&VarName::new("damper.w_rel"))
    );
}

#[test]
fn test_normalize_runtime_aliases_rewrites_event_surfaces_to_core_states() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), dae::Variable::new(VarName::new("x")));
    dae.states
        .insert(VarName::new("v"), dae::Variable::new(VarName::new("v")));
    dae.algebraics
        .insert(VarName::new("d"), dae::Variable::new(VarName::new("d")));
    dae.parameters
        .insert(VarName::new("r"), dae::Variable::new(VarName::new("r")));

    // der(x) = v
    dae.f_x.push(eq(sub(
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Der,
            args: vec![var("x")],
        },
        var("v"),
    )));
    // d = x - r
    dae.f_x.push(eq(sub(var("d"), sub(var("x"), var("r")))));
    // der(v) = if d < 0 then -1 else -2 (shape only; values not important)
    dae.f_x.push(eq(sub(
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Der,
            args: vec![var("v")],
        },
        dae::Expression::If {
            branches: vec![(
                lt(var("d"), int(0)),
                dae::Expression::Unary {
                    op: rumoca_sim_core::ir_core::OpUnary::Minus(Default::default()),
                    rhs: Box::new(int(1)),
                },
            )],
            else_branch: Box::new(dae::Expression::Unary {
                op: rumoca_sim_core::ir_core::OpUnary::Minus(Default::default()),
                rhs: Box::new(int(2)),
            }),
        },
    )));

    // Canonical condition roots reference alias before normalization.
    let cond = lt(var("d"), int(0));
    dae.relation.push(cond.clone());
    dae.f_c.push(eq(cond));

    let normalized = normalize_runtime_aliases_collect(&mut dae).0;
    assert_eq!(normalized, 1, "expected one alias variable normalized");
    assert!(
        !dae.algebraics.contains_key(&VarName::new("d")),
        "alias variable should be removed from algebraics after rewrite"
    );
    assert_eq!(
        dae.f_x.len(),
        2,
        "alias defining equation should be removed after substitution"
    );

    assert!(
        !dae.relation
            .iter()
            .any(|expr| expr_contains_var_ref(expr, &VarName::new("d"))),
        "relation expressions should not reference eliminated alias"
    );
    assert!(
        dae.relation
            .iter()
            .any(|expr| expr_contains_var_ref(expr, &VarName::new("x"))),
        "relation expressions should be rewritten to core-state variables"
    );
    assert!(
        !dae.f_c
            .iter()
            .any(|eq| expr_contains_var_ref(&eq.rhs, &VarName::new("d"))),
        "f_c expressions should not reference eliminated alias"
    );
}

#[test]
fn test_normalize_runtime_aliases_rewrites_scalarized_array_alias_refs() {
    let mut dae = Dae::new();
    let mut omega = dae::Variable::new(VarName::new("omega"));
    omega.dims = vec![3];
    dae.states.insert(VarName::new("omega"), omega);
    dae.states
        .insert(VarName::new("phi"), dae::Variable::new(VarName::new("phi")));

    let mut attitude_omega = dae::Variable::new(VarName::new("attitude.omega"));
    attitude_omega.dims = vec![3];
    dae.algebraics
        .insert(VarName::new("attitude.omega"), attitude_omega);

    dae.f_x.push(dae::Equation::residual_array(
        sub(var("attitude.omega"), var("omega")),
        Span::DUMMY,
        "attitude alias",
        3,
    ));
    dae.f_x.push(eq(sub(der("phi"), var("attitude.omega[2]"))));

    let normalized = normalize_runtime_aliases_collect(&mut dae).0;

    assert_eq!(normalized, 1, "expected vector alias to normalize");
    assert!(
        !dae.algebraics.contains_key(&VarName::new("attitude.omega")),
        "alias array should be removed"
    );
    assert_eq!(dae.f_x.len(), 1, "alias row should be removed");
    assert!(
        expr_contains_var_idx(&dae.f_x[0].rhs, "omega", 2),
        "scalarized alias reference should become omega[2]"
    );
    assert!(
        !expr_contains_var_ref(&dae.f_x[0].rhs, &VarName::new("attitude.omega")),
        "remaining equation should not reference eliminated alias"
    );
}

#[test]
fn test_normalize_runtime_aliases_preserves_continuous_only_outputs() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), dae::Variable::new(VarName::new("x")));
    dae.outputs
        .insert(VarName::new("y"), dae::Variable::new(VarName::new("y")));

    dae.f_x.push(eq(sub(der("x"), add(var("y"), int(1)))));
    dae.f_x.push(eq(sub(var("y"), var("x"))));

    let normalized = normalize_runtime_aliases_collect(&mut dae).0;
    assert_eq!(
        normalized, 1,
        "expected continuous output aliases to normalize"
    );
    assert!(
        dae.outputs.contains_key(&VarName::new("y")),
        "visible outputs should stay declared after alias normalization"
    );
    assert_eq!(
        dae.f_x.len(),
        1,
        "alias rows should be removed after rewrite"
    );
    assert!(
        dae.f_x
            .iter()
            .any(|eq| expr_contains_var_ref(&eq.rhs, &VarName::new("x"))),
        "remaining equations should reference the structural source"
    );
}

#[test]
fn test_normalize_runtime_aliases_updates_elim_for_continuous_only_outputs() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), dae::Variable::new(VarName::new("x")));
    dae.outputs
        .insert(VarName::new("y"), dae::Variable::new(VarName::new("y")));
    dae.f_x.push(eq(sub(der("x"), add(var("y"), int(1)))));
    dae.f_x.push(eq(sub(var("y"), var("x"))));

    let mut elim = eliminate::EliminationResult::default();
    normalize_runtime_aliases_and_update_elim(&mut dae, &mut elim, false);

    assert!(
        elim.substitutions.iter().any(|sub| {
            sub.var_name == VarName::new("y")
                && expr_contains_var_ref(&sub.expr, &VarName::new("x"))
        }),
        "normalized visible outputs must remain reconstructible after prepare"
    );
}

#[test]
fn test_normalize_runtime_aliases_keeps_output_used_by_runtime_surfaces() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), dae::Variable::new(VarName::new("x")));
    dae.outputs
        .insert(VarName::new("y"), dae::Variable::new(VarName::new("y")));

    dae.f_x.push(eq(sub(var("y"), var("x"))));
    let root = lt(var("y"), int(0));
    dae.relation.push(root.clone());
    dae.f_c.push(eq(root));

    let normalized = normalize_runtime_aliases_collect(&mut dae).0;
    assert_eq!(normalized, 0, "root-facing output alias must stay intact");
    assert!(dae.outputs.contains_key(&VarName::new("y")));
    assert_eq!(dae.f_x.len(), 1);
}

#[test]
fn test_runtime_alias_substitution_keeps_defining_row_for_branch_local_helper() {
    let mut dae = Dae::new();
    dae.algebraics
        .insert(VarName::new("u"), dae::Variable::new(VarName::new("u")));
    dae.algebraics
        .insert(VarName::new("x"), dae::Variable::new(VarName::new("x")));
    dae.algebraics
        .insert(VarName::new("y"), dae::Variable::new(VarName::new("y")));

    dae.f_x.push(dae::Equation::residual(
        sub(var("u"), var("x")),
        Span::DUMMY,
        "binding equation for u",
    ));
    dae.f_x.push(dae::Equation::residual(
        sub(
            var("y"),
            dae::Expression::BuiltinCall {
                function: dae::BuiltinFunction::Homotopy,
                args: vec![var("x"), var("u")],
            },
        ),
        Span::DUMMY,
        "equation from y",
    ));

    let substitutions = build_runtime_alias_substitutions(&dae);
    let u_sub = substitutions
        .iter()
        .find(|sub| sub.var_name == VarName::new("u"))
        .expect("u substitution");
    assert!(u_sub.preserve_defining_equation);

    apply_runtime_alias_substitutions(&mut dae, std::slice::from_ref(u_sub));

    assert!(
        expr_contains_var_ref(&dae.f_x[0].rhs, &VarName::new("u")),
        "branch-local helper must keep its defining row"
    );
    assert!(
        !expr_contains_var_ref(&dae.f_x[1].rhs, &VarName::new("u")),
        "other equations should still rewrite to the structural source"
    );
}

#[test]
fn test_runtime_alias_substitution_does_not_keep_plain_surviving_alias_rows() {
    let mut dae = Dae::new();
    dae.algebraics
        .insert(VarName::new("u"), dae::Variable::new(VarName::new("u")));
    dae.algebraics
        .insert(VarName::new("x"), dae::Variable::new(VarName::new("x")));
    dae.algebraics
        .insert(VarName::new("y"), dae::Variable::new(VarName::new("y")));

    dae.f_x.push(dae::Equation::residual(
        sub(var("u"), var("x")),
        Span::DUMMY,
        "binding equation for u",
    ));
    dae.f_x.push(dae::Equation::residual(
        sub(var("y"), add(var("x"), var("u"))),
        Span::DUMMY,
        "equation from y",
    ));

    let substitutions = build_runtime_alias_substitutions(&dae);
    let u_sub = substitutions
        .iter()
        .find(|sub| sub.var_name == VarName::new("u"))
        .expect("u substitution");
    assert!(!u_sub.preserve_defining_equation);

    apply_runtime_alias_substitutions(&mut dae, std::slice::from_ref(u_sub));

    assert!(
        !expr_contains_var_ref(&dae.f_x[0].rhs, &VarName::new("u")),
        "plain surviving aliases should still collapse"
    );
}

#[test]
fn test_substitute_standalone_state_derivatives_rewrites_non_ode_rows() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), dae::Variable::new(VarName::new("x")));
    dae.algebraics
        .insert(VarName::new("z"), dae::Variable::new(VarName::new("z")));
    dae.algebraics
        .insert(VarName::new("w"), dae::Variable::new(VarName::new("w")));

    dae.f_x.push(eq(sub(
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Der,
            args: vec![var("x")],
        },
        var("z"),
    )));
    dae.f_x.push(eq(add(
        var("w"),
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Der,
            args: vec![var("x")],
        },
    )));
    dae.f_x.push(eq(sub(var("z"), int(1))));

    problem::reorder_equations_for_solver(&mut dae).expect("reorder should succeed");
    normalize_ode_equation_signs(&mut dae);

    let rewritten = substitute_standalone_state_derivatives_in_non_ode_rows(&mut dae);
    assert_eq!(rewritten, 1, "expected one non-ODE row to be rewritten");
    assert!(
        !rumoca_sim_core::simulation::dae_prepare::expr_contains_der_of(
            &dae.f_x[1].rhs,
            &VarName::new("x")
        ),
        "non-ODE rows must not keep der(x) after substitution"
    );
    assert!(
        expr_contains_var_ref(&dae.f_x[1].rhs, &VarName::new("z")),
        "rewritten non-ODE row should reference the selected derivative expression"
    );
}

#[test]
fn test_exact_alias_state_demotion_runs_before_trivial_elimination() {
    let mut dae = Dae::new();
    let mut x = dae::Variable::new(VarName::new("x"));
    x.fixed = Some(true);
    x.start = Some(int(0));
    dae.states.insert(VarName::new("x"), x);
    dae.states
        .insert(VarName::new("y"), dae::Variable::new(VarName::new("y")));
    dae.algebraics
        .insert(VarName::new("a"), dae::Variable::new(VarName::new("a")));
    dae.algebraics
        .insert(VarName::new("vx"), dae::Variable::new(VarName::new("vx")));
    dae.algebraics
        .insert(VarName::new("vy"), dae::Variable::new(VarName::new("vy")));

    // MLS §8 simple equalities define one exact alias component here:
    // x = a and y = a. The state-selection pass must see that component
    // before trivial elimination removes one of the alias edges.
    dae.f_x.push(eq(sub(var("x"), var("a"))));
    dae.f_x.push(eq(sub(var("y"), var("a"))));
    dae.f_x.push(eq(sub(var("vx"), der("x"))));
    dae.f_x.push(eq(sub(var("vy"), der("y"))));
    dae.f_x.push(eq(sub(var("a"), int(0))));

    let demoted = demote_exact_alias_component_states(&mut dae);
    assert_eq!(
        demoted, 1,
        "exact alias demotion should fire before elimination"
    );

    let _elim = run_trivial_elimination_phase(&mut dae, false, false);

    assert!(
        dae.states.contains_key(&VarName::new("x")),
        "canonical fixed/start state should be retained"
    );
    assert!(
        !dae.states.contains_key(&VarName::new("y")),
        "duplicate exact-alias state should be demoted before trivial elimination"
    );
    assert!(
        dae.f_x.iter().all(|eq| {
            !rumoca_sim_core::simulation::dae_prepare::expr_contains_der_of(
                &eq.rhs,
                &VarName::new("y"),
            )
        }),
        "later prepare passes must not keep der(demoted_state) alive after exact alias demotion"
    );
}

#[test]
fn test_alias_state_demotion_only_handles_remaining_state_non_state_aliases() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), dae::Variable::new(VarName::new("x")));
    dae.states
        .insert(VarName::new("y"), dae::Variable::new(VarName::new("y")));
    dae.algebraics
        .insert(VarName::new("a"), dae::Variable::new(VarName::new("a")));
    dae.algebraics
        .insert(VarName::new("vx"), dae::Variable::new(VarName::new("vx")));

    // Phase 0 should collapse the exact alias component x = a = y first.
    dae.f_x.push(eq(sub(var("x"), var("a"))));
    dae.f_x.push(eq(sub(var("y"), var("a"))));
    dae.f_x.push(eq(sub(var("vx"), der("x"))));

    let exact_demoted = demote_exact_alias_component_states(&mut dae);
    assert_eq!(exact_demoted, 1, "exact alias phase should pick one state");
    assert!(dae.states.contains_key(&VarName::new("x")));
    assert!(!dae.states.contains_key(&VarName::new("y")));

    // Phase 1e should not try to demote the remaining state-to-state case
    // again; only the surviving state-to-non-state alias remains, and x has
    // a standalone derivative row so it must stay a state.
    let alias_demoted = demote_alias_states_without_der(&mut dae);
    assert_eq!(alias_demoted, 0);
    assert!(dae.states.contains_key(&VarName::new("x")));
}

#[test]
fn test_alias_state_demotion_demotes_state_non_state_no_der_case() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), dae::Variable::new(VarName::new("x")));
    dae.algebraics
        .insert(VarName::new("a"), dae::Variable::new(VarName::new("a")));

    dae.f_x.push(eq(sub(var("x"), var("a"))));
    dae.f_x.push(eq(sub(var("a"), int(0))));

    let alias_demoted = demote_alias_states_without_der(&mut dae);
    assert_eq!(alias_demoted, 1);
    assert!(!dae.states.contains_key(&VarName::new("x")));
    assert!(dae.algebraics.contains_key(&VarName::new("x")));
}

#[test]
fn test_template_codegen_prep_normalizes_runtime_aliases_for_event_surfaces() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), dae::Variable::new(VarName::new("x")));
    dae.states
        .insert(VarName::new("v"), dae::Variable::new(VarName::new("v")));
    dae.algebraics
        .insert(VarName::new("d"), dae::Variable::new(VarName::new("d")));
    dae.parameters
        .insert(VarName::new("r"), dae::Variable::new(VarName::new("r")));

    // der(x) = v
    dae.f_x.push(eq(sub(
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Der,
            args: vec![var("x")],
        },
        var("v"),
    )));
    // d = x - r
    dae.f_x.push(eq(sub(var("d"), sub(var("x"), var("r")))));
    // der(v) = if d < 0 then -1 else -2 (shape only; values not important)
    dae.f_x.push(eq(sub(
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Der,
            args: vec![var("v")],
        },
        dae::Expression::If {
            branches: vec![(
                lt(var("d"), int(0)),
                dae::Expression::Unary {
                    op: rumoca_sim_core::ir_core::OpUnary::Minus(Default::default()),
                    rhs: Box::new(int(1)),
                },
            )],
            else_branch: Box::new(dae::Expression::Unary {
                op: rumoca_sim_core::ir_core::OpUnary::Minus(Default::default()),
                rhs: Box::new(int(2)),
            }),
        },
    )));

    let cond = lt(var("d"), int(0));
    dae.relation.push(cond.clone());
    dae.f_c.push(eq(cond));

    let prepared =
        crate::build_simulation(&dae, &SimOptions::default()).expect("runtime prep succeeds");

    assert!(
        !prepared.dae.algebraics.contains_key(&VarName::new("d")),
        "runtime-prepared DAE should normalize runtime alias variable `d` out of algebraics"
    );
    assert!(
        !prepared
            .dae
            .relation
            .iter()
            .any(|expr| expr_contains_var_ref(expr, &VarName::new("d"))),
        "runtime-prepared relation roots should not reference eliminated alias `d`"
    );
    assert!(
        !prepared
            .dae
            .f_c
            .iter()
            .any(|eq| expr_contains_var_ref(&eq.rhs, &VarName::new("d"))),
        "runtime-prepared condition equations should not reference eliminated alias `d`"
    );
}
