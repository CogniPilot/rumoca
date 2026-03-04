use super::*;
use crate::test_support::{binop, eq_from, lit, var};
use rumoca_core::Span;
use rumoca_ir_dae as dae;
mod blt_linear;
mod core;
mod jacobian;
mod runtime_projection_seed;
mod runtime_state_chain;

use blt_linear::apply_substitutions_to_env;
use jacobian::assert_jacobians_close;

#[test]
fn test_count_empty_dae() {
    let dae = dae::Dae::new();
    assert_eq!(count_states(&dae), 0);
    assert_eq!(count_algebraics(&dae), 0);
    assert_eq!(count_parameters(&dae), 0);
}

#[test]
fn test_default_params_empty() {
    let dae = dae::Dae::new();
    let params = default_params(&dae);
    assert!(params.is_empty());
}

#[test]
fn test_default_params_with_budget_rejects_non_finite_values() {
    let mut dae = dae::Dae::new();
    let mut p = dae::Variable::new(dae::VarName::new("p"));
    p.start = Some(dae::Expression::FunctionCall {
        name: dae::VarName::new("missingFunction"),
        args: vec![],
        is_constructor: false,
    });
    dae.parameters.insert(dae::VarName::new("p"), p);

    let budget = crate::TimeoutBudget::new(None);
    let err = default_params_with_budget(&dae, &budget)
        .expect_err("non-finite parameter evaluation should fail in budgeted path");
    assert!(
        matches!(err, crate::SimError::SolverError(ref msg) if msg.contains("non-finite parameter")),
        "unexpected error: {err:?}"
    );
}

#[test]
fn test_default_params_with_budget_allows_infinite_parameter_defaults() {
    let mut dae = dae::Dae::new();
    let mut p = dae::Variable::new(dae::VarName::new("p"));
    p.start = Some(lit(f64::NEG_INFINITY));
    dae.parameters.insert(dae::VarName::new("p"), p);

    let budget = crate::TimeoutBudget::new(None);
    let params = default_params_with_budget(&dae, &budget)
        .expect("infinite parameter defaults are valid Modelica sentinels");
    assert_eq!(params.len(), 1);
    assert!(params[0].is_infinite() && params[0].is_sign_negative());
}

#[test]
fn test_default_params_with_budget_allows_transient_forward_ref_nan_in_pass1() {
    let mut dae = dae::Dae::new();

    let mut p0 = dae::Variable::new(dae::VarName::new("p0"));
    // Before forward refs are populated, this is 0/0 -> NaN in pass 1.
    p0.start = Some(binop(
        dae::OpBinary::Div(Default::default()),
        binop(dae::OpBinary::Mul(Default::default()), var("p1"), var("p2")),
        binop(dae::OpBinary::Sub(Default::default()), var("p1"), var("p2")),
    ));
    dae.parameters.insert(dae::VarName::new("p0"), p0);

    let mut p1 = dae::Variable::new(dae::VarName::new("p1"));
    p1.start = Some(lit(1.0));
    dae.parameters.insert(dae::VarName::new("p1"), p1);

    let mut p2 = dae::Variable::new(dae::VarName::new("p2"));
    p2.start = Some(lit(2.0));
    dae.parameters.insert(dae::VarName::new("p2"), p2);

    let budget = crate::TimeoutBudget::new(None);
    let params = default_params_with_budget(&dae, &budget)
        .expect("pass-2 forward-reference re-evaluation should resolve transient NaN");
    assert_eq!(params.len(), 3);
    assert!((params[0] + 2.0).abs() < 1e-12);
    assert!((params[1] - 1.0).abs() < 1e-12);
    assert!((params[2] - 2.0).abs() < 1e-12);
}

#[test]
fn test_default_params_with_budget_resolves_multi_level_forward_refs_to_avoid_nan() {
    let mut dae = dae::Dae::new();

    // Lsigma = (1 - ratio) * L
    let mut l_sigma = dae::Variable::new(dae::VarName::new("Lsigma"));
    l_sigma.start = Some(binop(
        dae::OpBinary::Mul(Default::default()),
        binop(
            dae::OpBinary::Sub(Default::default()),
            lit(1.0),
            var("ratio"),
        ),
        var("L"),
    ));
    dae.parameters.insert(dae::VarName::new("Lsigma"), l_sigma);

    // L = 1 / f
    let mut l = dae::Variable::new(dae::VarName::new("L"));
    l.start = Some(binop(
        dae::OpBinary::Div(Default::default()),
        lit(1.0),
        var("f"),
    ));
    dae.parameters.insert(dae::VarName::new("L"), l);

    // f = fs
    let mut f = dae::Variable::new(dae::VarName::new("f"));
    f.start = Some(var("fs"));
    dae.parameters.insert(dae::VarName::new("f"), f);

    // fs declared after f to force multi-level forward-reference settling.
    let mut fs = dae::Variable::new(dae::VarName::new("fs"));
    fs.start = Some(lit(50.0));
    dae.parameters.insert(dae::VarName::new("fs"), fs);

    let mut ratio = dae::Variable::new(dae::VarName::new("ratio"));
    ratio.start = Some(lit(1.0));
    dae.parameters.insert(dae::VarName::new("ratio"), ratio);

    let budget = crate::TimeoutBudget::new(None);
    let params = default_params_with_budget(&dae, &budget)
        .expect("multi-level forward references should converge to finite values");
    assert!(
        params[0].is_finite(),
        "Lsigma should be finite, got {}",
        params[0]
    );
    assert!(
        params[1].is_finite(),
        "L should be finite, got {}",
        params[1]
    );
    assert!((params[0] - 0.0).abs() < 1e-12);
    assert!((params[2] - 50.0).abs() < 1e-12);
}

#[test]
fn test_default_params_constant_array_index_uses_selected_entry() {
    let mut dae = dae::Dae::new();

    let mut conversion_table = dae::Variable::new(dae::VarName::new("conversionTable"));
    conversion_table.dims = vec![3];
    conversion_table.start = Some(dae::Expression::Array {
        elements: vec![lit(31536000.0), lit(3600.0), lit(1000.0)],
        is_matrix: false,
    });
    dae.constants
        .insert(dae::VarName::new("conversionTable"), conversion_table);

    let mut resolution = dae::Variable::new(dae::VarName::new("resolution"));
    resolution.start = Some(lit(3.0));
    dae.parameters
        .insert(dae::VarName::new("resolution"), resolution);

    let mut resolution_factor = dae::Variable::new(dae::VarName::new("resolutionFactor"));
    resolution_factor.start = Some(dae::Expression::Index {
        base: Box::new(var("conversionTable")),
        subscripts: vec![dae::Subscript::Expr(Box::new(
            dae::Expression::BuiltinCall {
                function: dae::BuiltinFunction::Integer,
                args: vec![var("resolution")],
            },
        ))],
    });
    dae.parameters
        .insert(dae::VarName::new("resolutionFactor"), resolution_factor);

    let budget = crate::TimeoutBudget::new(None);
    let params = default_params_with_budget(&dae, &budget)
        .expect("constant array indexing should resolve in parameter starts");

    let mut pidx = 0usize;
    let mut resolution_value = None;
    let mut resolution_factor_value = None;
    for (name, var) in &dae.parameters {
        if var.size() > 1 {
            pidx += var.size();
            continue;
        }
        let value = params[pidx];
        if name.as_str() == "resolution" {
            resolution_value = Some(value);
        } else if name.as_str() == "resolutionFactor" {
            resolution_factor_value = Some(value);
        }
        pidx += 1;
    }

    let resolution_value = resolution_value.expect("resolution parameter missing");
    let resolution_factor_value =
        resolution_factor_value.expect("resolutionFactor parameter missing");

    assert!((resolution_value - 3.0).abs() < 1e-12);
    assert!(
        (resolution_factor_value - 1000.0).abs() < 1e-12,
        "expected indexed conversion table entry 1000.0, got {resolution_factor_value}"
    );
}

#[test]
fn test_initialize_state_vector_respects_state_then_algebraic_order() {
    let mut dae = dae::Dae::new();
    dae.states.insert(
        dae::VarName::new("x"),
        dae::Variable::new(dae::VarName::new("x")),
    );
    dae.algebraics.insert(
        dae::VarName::new("z"),
        dae::Variable::new(dae::VarName::new("z")),
    );
    let mut y = vec![1.0, 2.0];
    initialize_state_vector(&dae, &mut y);
    assert_eq!(y, vec![0.0, 0.0]);
}

#[test]
fn test_expr_contains_der_of_varref_direct() {
    // der(x) — VarRef directly inside BuiltinCall
    let expr = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Der,
        args: vec![dae::Expression::VarRef {
            name: dae::VarName::new("x"),
            subscripts: vec![],
        }],
    };
    assert!(expr_contains_der_of(&expr, &dae::VarName::new("x")));
    assert!(!expr_contains_der_of(&expr, &dae::VarName::new("y")));
}

#[test]
fn test_expr_contains_der_of_varref_with_subscripts() {
    // der(x[1]) as VarRef { name: "x", subscripts: [Index(1)] }
    let expr = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Der,
        args: vec![dae::Expression::VarRef {
            name: dae::VarName::new("x"),
            subscripts: vec![dae::Subscript::Index(1)],
        }],
    };
    assert!(expr_contains_der_of(&expr, &dae::VarName::new("x")));
}

#[test]
fn test_expr_contains_der_of_varref_with_subscripts_does_not_cross_match_other_index() {
    let expr = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Der,
        args: vec![dae::Expression::VarRef {
            name: dae::VarName::new("x"),
            subscripts: vec![dae::Subscript::Index(1)],
        }],
    };
    assert!(!expr_contains_der_of(&expr, &dae::VarName::new("x[2]")));
}

#[test]
fn test_expr_contains_der_of_index_wrapping_varref() {
    // der(x[1]) as Index { base: VarRef { name: "x" }, subscripts: [Index(1)] }
    let expr = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Der,
        args: vec![dae::Expression::Index {
            base: Box::new(dae::Expression::VarRef {
                name: dae::VarName::new("x"),
                subscripts: vec![],
            }),
            subscripts: vec![dae::Subscript::Index(1)],
        }],
    };
    assert!(expr_contains_der_of(&expr, &dae::VarName::new("x")));
    assert!(!expr_contains_der_of(&expr, &dae::VarName::new("y")));
}

#[test]
fn test_component_base_name_simple() {
    assert_eq!(dae::component_base_name("x").as_deref(), Some("x"));
    assert_eq!(dae::component_base_name("x[1]").as_deref(), Some("x"));
    assert_eq!(dae::component_base_name("x[1][2]").as_deref(), Some("x"));
}

#[test]
fn test_component_base_name_mid_path() {
    assert_eq!(
        dae::component_base_name("support[1].phi").as_deref(),
        Some("support.phi")
    );
    assert_eq!(
        dae::component_base_name("a[1].b[2].c").as_deref(),
        Some("a.b.c")
    );
    assert_eq!(
        dae::component_base_name("foo.bar[3].baz").as_deref(),
        Some("foo.bar.baz")
    );
}

#[test]
fn test_expr_refers_to_var_mid_path_subscript() {
    // der(support[1].phi) should match state "support.phi"
    let expr = dae::Expression::VarRef {
        name: dae::VarName::new("support[1].phi"),
        subscripts: vec![],
    };
    assert!(expr_refers_to_var(&expr, &dae::VarName::new("support.phi")));
    assert!(!expr_refers_to_var(&expr, &dae::VarName::new("other.phi")));
}

#[test]
fn test_expr_refers_to_var_trailing_subscript() {
    // "x[1]" should match "x"
    let expr = dae::Expression::VarRef {
        name: dae::VarName::new("x[1]"),
        subscripts: vec![],
    };
    assert!(expr_refers_to_var(&expr, &dae::VarName::new("x")));
}

#[test]
fn test_expr_refers_to_var_indexed_target_requires_exact_index() {
    let expr = dae::Expression::VarRef {
        name: dae::VarName::new("x[1]"),
        subscripts: vec![],
    };
    assert!(!expr_refers_to_var(&expr, &dae::VarName::new("x[2]")));
}

#[test]
fn test_expr_refers_to_var_indexed_component_requires_exact_index() {
    let expr = dae::Expression::VarRef {
        name: dae::VarName::new("support[1].phi.im"),
        subscripts: vec![],
    };
    assert!(!expr_refers_to_var(
        &expr,
        &dae::VarName::new("support[2].phi.im")
    ));
}

#[test]
fn test_extract_direct_assignment_with_indexed_target() {
    let rhs = dae::Expression::Binary {
        op: dae::OpBinary::Sub(Default::default()),
        lhs: Box::new(dae::Expression::VarRef {
            name: dae::VarName::new("aux"),
            subscripts: vec![dae::Subscript::Index(1)],
        }),
        rhs: Box::new(lit(2.5)),
    };
    let (target, solution) =
        extract_direct_assignment(&rhs).expect("expected direct assignment extraction");
    assert_eq!(target, "aux[1]");
    assert!((eval_expr::<f64>(solution, &VarEnv::new()) - 2.5).abs() < 1e-12);
}

#[test]
fn test_apply_initial_section_assignments_propagates_aliases_into_pre_equations() {
    rumoca_eval_runtime::eval::clear_pre_values();

    let mut dae = dae::Dae::new();
    dae.discrete_valued.insert(
        dae::VarName::new("active"),
        dae::Variable::new(dae::VarName::new("active")),
    );
    dae.discrete_valued.insert(
        dae::VarName::new("localActive"),
        dae::Variable::new(dae::VarName::new("localActive")),
    );
    dae.discrete_valued.insert(
        dae::VarName::new("newActive"),
        dae::Variable::new(dae::VarName::new("newActive")),
    );
    dae.algebraics.insert(
        dae::VarName::new("z"),
        dae::Variable::new(dae::VarName::new("z")),
    );
    dae.f_x.push(eq_from(binop(
        dae::OpBinary::Sub(Default::default()),
        var("z"),
        lit(0.0),
    )));
    dae.f_m.push(eq_from(binop(
        dae::OpBinary::Sub(Default::default()),
        var("active"),
        var("localActive"),
    )));
    dae.initial_equations.push(dae::Equation::explicit(
        dae::VarName::new("active"),
        lit(1.0),
        Span::DUMMY,
        "initial active",
    ));
    let pre_new_active = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Pre,
        args: vec![var("newActive")],
    };
    let pre_local_active = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Pre,
        args: vec![var("localActive")],
    };
    dae.initial_equations.push(eq_from(binop(
        dae::OpBinary::Sub(Default::default()),
        pre_new_active,
        pre_local_active,
    )));

    let p = default_params(&dae);
    let mut y = vec![0.0; dae.f_x.len()];
    initialize_state_vector(&dae, &mut y);
    apply_initial_section_assignments(&dae, &mut y, &p, 0.0);

    let pre_local = rumoca_eval_runtime::eval::get_pre_value("localActive")
        .expect("pre(localActive) should be seeded from initial alias closure");
    let pre_new = rumoca_eval_runtime::eval::get_pre_value("newActive")
        .expect("pre(newActive) should follow explicit initial pre equation");
    assert!(
        (pre_local - 1.0).abs() < 1e-12,
        "expected pre(localActive)=1 from active=1 + alias equation, got {pre_local}"
    );
    assert!(
        (pre_new - 1.0).abs() < 1e-12,
        "expected pre(newActive)=1 from pre(newActive)=pre(localActive), got {pre_new}"
    );
}

#[test]
fn test_seed_direct_assignment_handles_size1_indexed_lhs() {
    let mut dae = dae::Dae::new();
    dae.algebraics.insert(
        dae::VarName::new("aux"),
        dae::Variable::new(dae::VarName::new("aux")),
    );
    let mut p_var = dae::Variable::new(dae::VarName::new("p"));
    p_var.start = Some(lit(1.25));
    dae.parameters.insert(dae::VarName::new("p"), p_var);
    dae.f_x.push(eq_from(dae::Expression::Binary {
        op: dae::OpBinary::Sub(Default::default()),
        lhs: Box::new(dae::Expression::VarRef {
            name: dae::VarName::new("aux"),
            subscripts: vec![dae::Subscript::Index(1)],
        }),
        rhs: Box::new(var("p")),
    }));

    let n_x = count_states(&dae);
    let p = default_params(&dae);
    let mut y = vec![0.0; dae.f_x.len()];
    initialize_state_vector(&dae, &mut y);

    let updates = seed_direct_assignment_initial_values(&dae, &mut y, &p, n_x, false, 0.0);
    assert!(updates > 0);
    assert!((y[0] - 1.25).abs() < 1e-12);
}

#[test]
fn test_seed_direct_assignment_updates_all_array_slots_from_base_target() {
    let mut dae = dae::Dae::new();
    let mut aw = dae::Variable::new(dae::VarName::new("aw"));
    aw.dims = vec![3];
    dae.algebraics.insert(dae::VarName::new("aw"), aw);

    let rhs = dae::Expression::Binary {
        op: dae::OpBinary::Sub(Default::default()),
        lhs: Box::new(var("aw")),
        rhs: Box::new(dae::Expression::Array {
            elements: vec![lit(1.0), lit(2.0), lit(3.0)],
            is_matrix: false,
        }),
    };
    dae.f_x.push(eq_from(rhs));

    let n_x = count_states(&dae);
    let p = default_params(&dae);
    let n_unknowns = dae.algebraics.values().map(|var| var.size()).sum::<usize>();
    let mut y = vec![0.0; n_unknowns];
    initialize_state_vector(&dae, &mut y);

    let updates = seed_direct_assignment_initial_values(&dae, &mut y, &p, n_x, false, 0.0);
    assert!(
        updates >= 3,
        "expected array assignment seeding updates for all slots"
    );
    assert!((y[0] - 1.0).abs() < 1e-12);
    assert!((y[1] - 2.0).abs() < 1e-12);
    assert!((y[2] - 3.0).abs() < 1e-12);
}

#[test]
fn test_seed_direct_assignment_ignores_orphaned_variable_pin_equations() {
    let mut dae = dae::Dae::new();
    dae.algebraics.insert(
        dae::VarName::new("x"),
        dae::Variable::new(dae::VarName::new("x")),
    );
    let mut p_var = dae::Variable::new(dae::VarName::new("p"));
    p_var.start = Some(lit(2.0));
    dae.parameters.insert(dae::VarName::new("p"), p_var);

    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: dae::Expression::Binary {
            op: dae::OpBinary::Sub(Default::default()),
            lhs: Box::new(var("x")),
            rhs: Box::new(var("p")),
        },
        span: rumoca_core::Span::DUMMY,
        origin: "equation from model".to_string(),
        scalar_count: 1,
    });
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: dae::Expression::Binary {
            op: dae::OpBinary::Sub(Default::default()),
            lhs: Box::new(var("x")),
            rhs: Box::new(lit(0.0)),
        },
        span: rumoca_core::Span::DUMMY,
        origin: "orphaned_variable_pin".to_string(),
        scalar_count: 1,
    });

    let n_x = count_states(&dae);
    let p = default_params(&dae);
    let mut y = vec![0.0; dae.f_x.len()];
    initialize_state_vector(&dae, &mut y);

    let updates = seed_direct_assignment_initial_values(&dae, &mut y, &p, n_x, false, 0.0);
    assert!(updates > 0);
    assert!(
        (y[0] - 2.0).abs() < 1e-12,
        "seeding should use physical direct assignment, not orphaned-variable pin"
    );
}

#[test]
fn test_runtime_projection_not_required_for_unique_acyclic_direct_assignments() {
    let mut dae = dae::Dae::new();
    dae.algebraics.insert(
        dae::VarName::new("x"),
        dae::Variable::new(dae::VarName::new("x")),
    );
    dae.algebraics.insert(
        dae::VarName::new("y"),
        dae::Variable::new(dae::VarName::new("y")),
    );
    dae.f_x.push(eq_from(dae::Expression::Binary {
        op: dae::OpBinary::Sub(Default::default()),
        lhs: Box::new(var("x")),
        rhs: Box::new(lit(1.0)),
    }));
    dae.f_x.push(eq_from(dae::Expression::Binary {
        op: dae::OpBinary::Sub(Default::default()),
        lhs: Box::new(var("y")),
        rhs: Box::new(dae::Expression::Binary {
            op: dae::OpBinary::Add(Default::default()),
            lhs: Box::new(var("x")),
            rhs: Box::new(lit(2.0)),
        }),
    }));

    assert!(
        !runtime_projection_required(&dae, 0),
        "unique acyclic direct assignments should use fast direct seeding"
    );
}

#[test]
fn test_seed_runtime_direct_assignments_resolves_acyclic_unknown_chain() {
    let mut dae = dae::Dae::new();
    dae.algebraics.insert(
        dae::VarName::new("x"),
        dae::Variable::new(dae::VarName::new("x")),
    );
    dae.algebraics.insert(
        dae::VarName::new("y"),
        dae::Variable::new(dae::VarName::new("y")),
    );
    dae.f_x.push(eq_from(dae::Expression::Binary {
        op: dae::OpBinary::Sub(Default::default()),
        lhs: Box::new(var("x")),
        rhs: Box::new(lit(1.0)),
    }));
    dae.f_x.push(eq_from(dae::Expression::Binary {
        op: dae::OpBinary::Sub(Default::default()),
        lhs: Box::new(var("y")),
        rhs: Box::new(dae::Expression::Binary {
            op: dae::OpBinary::Add(Default::default()),
            lhs: Box::new(var("x")),
            rhs: Box::new(lit(2.0)),
        }),
    }));

    let mut y = vec![0.0; dae.f_x.len()];
    let params = default_params(&dae);
    let updates = seed_runtime_direct_assignment_values(&dae, &mut y, &params, 0, 0.0);
    let names = solver_vector_names(&dae, y.len());
    let idx_for = |needle: &str| {
        names
            .iter()
            .position(|name| name == needle)
            .unwrap_or_else(|| panic!("missing solver variable '{needle}' in {:?}", names))
    };
    let x_idx = idx_for("x");
    let y_idx = idx_for("y");
    assert!(
        updates > 0,
        "runtime direct-assignment seeding should update acyclic chain variables"
    );
    assert!(
        (y[x_idx] - 1.0).abs() < 1.0e-12,
        "expected x=1 from x:=1 (names={names:?}, y={y:?})"
    );
    assert!(
        (y[y_idx] - 3.0).abs() < 1.0e-12,
        "expected y=x+2 to resolve in the same seeding pass chain (names={names:?}, y={y:?})"
    );
}

#[test]
fn test_runtime_projection_required_for_duplicate_direct_assignment_targets() {
    let mut dae = dae::Dae::new();
    dae.algebraics.insert(
        dae::VarName::new("x"),
        dae::Variable::new(dae::VarName::new("x")),
    );
    dae.algebraics.insert(
        dae::VarName::new("y"),
        dae::Variable::new(dae::VarName::new("y")),
    );
    dae.f_x.push(eq_from(dae::Expression::Binary {
        op: dae::OpBinary::Sub(Default::default()),
        lhs: Box::new(var("x")),
        rhs: Box::new(lit(1.0)),
    }));
    dae.f_x.push(eq_from(dae::Expression::Binary {
        op: dae::OpBinary::Sub(Default::default()),
        lhs: Box::new(var("x")),
        rhs: Box::new(var("y")),
    }));

    assert!(
        runtime_projection_required(&dae, 0),
        "multiple equations assigning the same target require Newton projection"
    );
}

#[test]
fn test_runtime_projection_required_for_direct_assignment_cycles() {
    let mut dae = dae::Dae::new();
    dae.algebraics.insert(
        dae::VarName::new("x"),
        dae::Variable::new(dae::VarName::new("x")),
    );
    dae.algebraics.insert(
        dae::VarName::new("y"),
        dae::Variable::new(dae::VarName::new("y")),
    );
    dae.f_x.push(eq_from(dae::Expression::Binary {
        op: dae::OpBinary::Sub(Default::default()),
        lhs: Box::new(var("x")),
        rhs: Box::new(var("y")),
    }));
    dae.f_x.push(eq_from(dae::Expression::Binary {
        op: dae::OpBinary::Sub(Default::default()),
        lhs: Box::new(var("y")),
        rhs: Box::new(var("x")),
    }));

    assert!(
        runtime_projection_required(&dae, 0),
        "cyclic direct assignments require Newton projection"
    );
}

#[test]
fn test_runtime_projection_required_for_runtime_discrete_builtins() {
    let mut dae = dae::Dae::new();
    dae.algebraics.insert(
        dae::VarName::new("x"),
        dae::Variable::new(dae::VarName::new("x")),
    );
    dae.f_x.push(eq_from(dae::Expression::Binary {
        op: dae::OpBinary::Sub(Default::default()),
        lhs: Box::new(var("x")),
        rhs: Box::new(dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Pre,
            args: vec![var("x")],
        }),
    }));

    assert!(
        runtime_projection_required(&dae, 0),
        "assignments that depend on runtime discrete/event builtins must use runtime projection"
    );
}

// =========================================================================
// AD Jacobian vs Finite-Difference comparison tests
// =========================================================================

/// Helper: compute the full n×n Jacobian matrix using AD (one column per
/// basis vector).
fn jacobian_ad(dae: &dae::Dae, y: &[f64], p: &[f64], t: f64, n_x: usize) -> Vec<Vec<f64>> {
    let n = y.len();
    let mut jac = vec![vec![0.0; n]; n];
    for col in 0..n {
        let mut v = vec![0.0; n];
        v[col] = 1.0;
        let mut jv = vec![0.0; n];
        eval_jacobian_vector_ad(dae, y, p, t, &v, &mut jv, n_x);
        for row in 0..n {
            jac[row][col] = jv[row];
        }
    }
    jac
}

/// Helper: compute the full n×n Jacobian matrix using central finite
/// differences: J[i][j] ≈ (f(y+h*e_j) - f(y-h*e_j)) / (2h).
fn jacobian_fd(dae: &dae::Dae, y: &[f64], p: &[f64], t: f64, n_x: usize) -> Vec<Vec<f64>> {
    let n = y.len();
    let h = 1e-7;
    let mut jac = vec![vec![0.0; n]; n];
    for col in 0..n {
        let mut y_plus = y.to_vec();
        let mut y_minus = y.to_vec();
        y_plus[col] += h;
        y_minus[col] -= h;
        let mut f_plus = vec![0.0; n];
        let mut f_minus = vec![0.0; n];
        eval_rhs_equations(dae, &y_plus, p, t, &mut f_plus, n_x);
        eval_rhs_equations(dae, &y_minus, p, t, &mut f_minus, n_x);
        for row in 0..n {
            jac[row][col] = (f_plus[row] - f_minus[row]) / (2.0 * h);
        }
    }
    jac
}

#[test]
fn test_solve_initial_algebraic_accepts_consistent_singular_initial_point() {
    // Structurally singular IC Jacobian: two unknowns (a, b) but both equations
    // constrain only `a`. The default starts y=0 are already consistent.
    let mut dae = dae::Dae::new();
    dae.algebraics.insert(
        dae::VarName::new("a"),
        dae::Variable::new(dae::VarName::new("a")),
    );
    dae.algebraics.insert(
        dae::VarName::new("b"),
        dae::Variable::new(dae::VarName::new("b")),
    );
    dae.f_x.push(eq_from(binop(
        dae::OpBinary::Sub(Default::default()),
        var("a"),
        lit(0.0),
    )));
    dae.f_x.push(eq_from(binop(
        dae::OpBinary::Sub(Default::default()),
        var("a"),
        lit(0.0),
    )));

    let timeout = crate::TimeoutBudget::new(None);
    let ok = solve_initial_algebraic(&mut dae, 0, 1e-9, &timeout)
        .expect("IC solve should not error on consistent singular point");
    assert!(
        ok,
        "IC solve should accept an already-consistent initial point without Newton singular failure"
    );
}

#[test]
fn test_solve_initial_algebraic_writes_seeded_solution_on_singular_jacobian() {
    let mut dae = dae::Dae::new();
    dae.algebraics.insert(
        dae::VarName::new("x"),
        dae::Variable::new(dae::VarName::new("x")),
    );
    dae.algebraics.insert(
        dae::VarName::new("aux"),
        dae::Variable::new(dae::VarName::new("aux")),
    );

    let mut p_var = dae::Variable::new(dae::VarName::new("p"));
    p_var.start = Some(lit(2.5));
    dae.parameters.insert(dae::VarName::new("p"), p_var);

    // Direct assignment gives a meaningful IC seed for aux.
    dae.f_x.push(eq_from(binop(
        dae::OpBinary::Sub(Default::default()),
        var("aux"),
        var("p"),
    )));
    // Constant residual row keeps Newton singular and non-convergent.
    dae.f_x.push(eq_from(lit(1.0)));

    let timeout = crate::TimeoutBudget::new(None);
    let ok = solve_initial_algebraic(&mut dae, 0, 1e-9, &timeout)
        .expect("IC solve should gracefully handle singular Jacobian");
    assert!(!ok, "singular system should report non-converged IC solve");

    let aux_start = dae
        .algebraics
        .get(&dae::VarName::new("aux"))
        .and_then(|v| v.start.as_ref())
        .expect("aux start should be written from seeded IC estimate");
    let aux_val = eval_expr::<f64>(aux_start, &VarEnv::new());
    assert!(
        (aux_val - 2.5).abs() < 1e-12,
        "seeded aux start should be retained when Newton fails singular"
    );
}

#[test]
fn test_solve_initial_algebraic_errors_when_residual_stays_non_finite_after_perturbation() {
    let mut dae = dae::Dae::new();
    dae.algebraics.insert(
        dae::VarName::new("z"),
        dae::Variable::new(dae::VarName::new("z")),
    );
    let denom = binop(dae::OpBinary::Sub(Default::default()), var("z"), var("z"));
    let inv = binop(dae::OpBinary::Div(Default::default()), lit(1.0), denom);
    dae.f_x.push(eq_from(binop(
        dae::OpBinary::Sub(Default::default()),
        var("z"),
        inv,
    )));

    let timeout = crate::TimeoutBudget::new(None);
    let err = solve_initial_algebraic(&mut dae, 0, 1e-9, &timeout)
        .expect_err("non-finite IC residual should fail fast");
    match err {
        crate::SimError::SolverError(msg) => {
            assert!(
                msg.contains("initial-condition residual is non-finite"),
                "unexpected error message: {msg}"
            );
        }
        other => panic!("unexpected error: {other}"),
    }
}

#[test]
fn test_project_runtime_keeps_direct_assigned_state_free() {
    let mut dae = dae::Dae::new();
    dae.states.insert(
        dae::VarName::new("x"),
        dae::Variable::new(dae::VarName::new("x")),
    );
    dae.algebraics.insert(
        dae::VarName::new("z"),
        dae::Variable::new(dae::VarName::new("z")),
    );

    dae.f_x.push(eq_from(binop(
        dae::OpBinary::Sub(Default::default()),
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Der,
            args: vec![var("x")],
        },
        var("z"),
    )));
    dae.f_x.push(eq_from(binop(
        dae::OpBinary::Sub(Default::default()),
        var("x"),
        var("time"),
    )));

    let timeout = crate::TimeoutBudget::new(None);
    let projected =
        project_algebraics_with_fixed_states_at_time(&dae, &[0.0, 0.0], 1, 2.0, 1e-9, &timeout)
            .expect("runtime projection should not error")
            .expect("runtime projection should converge");

    assert!((projected[0] - 2.0).abs() < 1e-9);
    assert!(projected[1].abs() < 1e-9);
}

#[test]
fn test_project_runtime_converges_on_rank_deficient_consistent_system() {
    let mut dae = dae::Dae::new();
    dae.states.insert(
        dae::VarName::new("x"),
        dae::Variable::new(dae::VarName::new("x")),
    );
    dae.algebraics.insert(
        dae::VarName::new("a"),
        dae::Variable::new(dae::VarName::new("a")),
    );
    dae.algebraics.insert(
        dae::VarName::new("b"),
        dae::Variable::new(dae::VarName::new("b")),
    );

    // State row (fixed during runtime projection).
    dae.f_x.push(eq_from(binop(
        dae::OpBinary::Sub(Default::default()),
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Der,
            args: vec![var("x")],
        },
        lit(0.0),
    )));
    // Rank-deficient algebraic rows: both constrain only `a`.
    dae.f_x.push(eq_from(binop(
        dae::OpBinary::Sub(Default::default()),
        var("a"),
        lit(1.0),
    )));
    dae.f_x.push(eq_from(binop(
        dae::OpBinary::Sub(Default::default()),
        var("a"),
        lit(1.0),
    )));

    let timeout = crate::TimeoutBudget::new(None);
    let projected = project_algebraics_with_fixed_states_at_time(
        &dae,
        &[0.0, 0.0, 0.0],
        1,
        0.0,
        1e-9,
        &timeout,
    )
    .expect("runtime projection should not error")
    .expect("rank-deficient but consistent runtime projection should converge");

    assert!((projected[1] - 1.0).abs() < 1e-9);
    assert!(projected[2].is_finite());
}

fn assert_matrix_close(lhs: &nalgebra::DMatrix<f64>, rhs: &nalgebra::DMatrix<f64>, tol: f64) {
    assert_eq!(lhs.nrows(), rhs.nrows());
    assert_eq!(lhs.ncols(), rhs.ncols());
    for i in 0..lhs.nrows() {
        for j in 0..lhs.ncols() {
            let a = lhs[(i, j)];
            let b = rhs[(i, j)];
            assert!(
                (a - b).abs() <= tol,
                "matrix mismatch at ({i}, {j}): {a} vs {b}"
            );
        }
    }
}

fn build_coloring_test_dae() -> dae::Dae {
    let mut dae = dae::Dae::new();
    dae.states.insert(
        dae::VarName::new("x1"),
        dae::Variable::new(dae::VarName::new("x1")),
    );
    dae.states.insert(
        dae::VarName::new("x2"),
        dae::Variable::new(dae::VarName::new("x2")),
    );
    dae.algebraics.insert(
        dae::VarName::new("z1"),
        dae::Variable::new(dae::VarName::new("z1")),
    );
    dae.algebraics.insert(
        dae::VarName::new("z2"),
        dae::Variable::new(dae::VarName::new("z2")),
    );

    // ODE rows first
    dae.f_x.push(eq_from(binop(
        dae::OpBinary::Sub(Default::default()),
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Der,
            args: vec![var("x1")],
        },
        var("z1"),
    )));
    dae.f_x.push(eq_from(binop(
        dae::OpBinary::Sub(Default::default()),
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Der,
            args: vec![var("x2")],
        },
        var("z2"),
    )));

    // Algebraic rows
    dae.f_x.push(eq_from(binop(
        dae::OpBinary::Sub(Default::default()),
        var("z1"),
        binop(dae::OpBinary::Mul(Default::default()), var("x1"), var("x1")),
    )));
    dae.f_x.push(eq_from(binop(
        dae::OpBinary::Sub(Default::default()),
        var("z2"),
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Sin,
            args: vec![var("x2")],
        },
    )));

    dae
}

fn init_jac_ctx<'a>(
    dae: &'a dae::Dae,
    y: &'a [f64],
    p: &'a [f64],
    t_eval: f64,
    n_x: usize,
    use_initial: bool,
) -> InitJacobianEvalContext<'a> {
    InitJacobianEvalContext {
        dae,
        y,
        p,
        t_eval,
        n_x,
        use_initial,
    }
}

#[test]
fn test_build_init_jacobian_colored_matches_dense() {
    let dae = build_coloring_test_dae();
    let y = vec![0.25, -0.4, 0.6, -0.7];
    let p = default_params(&dae);
    let fixed = vec![false, false];
    let timeout = crate::TimeoutBudget::new(None);
    let ctx = init_jac_ctx(&dae, &y, &p, 0.0, 2, false);

    let dense =
        build_init_jacobian_dense(&ctx, &fixed, &timeout).expect("dense Jacobian should build");
    let colored = build_init_jacobian_colored(&ctx, &fixed, &timeout)
        .expect("colored Jacobian build should not error")
        .expect("colored Jacobian should not fallback for this test case");

    assert_matrix_close(&dense, &colored, 1e-12);
}

#[test]
fn test_build_init_jacobian_colored_respects_fixed_state_locking() {
    let dae = build_coloring_test_dae();
    let y = vec![0.25, -0.4, 0.6, -0.7];
    let p = default_params(&dae);
    let fixed = vec![true, false];
    let timeout = crate::TimeoutBudget::new(None);
    let ctx = init_jac_ctx(&dae, &y, &p, 0.0, 2, false);

    let dense =
        build_init_jacobian_dense(&ctx, &fixed, &timeout).expect("dense Jacobian should build");
    let colored = build_init_jacobian_colored(&ctx, &fixed, &timeout)
        .expect("colored Jacobian build should not error")
        .expect("colored Jacobian should not fallback for this test case");

    assert_matrix_close(&dense, &colored, 1e-12);
    assert_eq!(colored[(0, 0)], 1.0);
    for j in 1..colored.ncols() {
        assert_eq!(colored[(0, j)], 0.0);
        assert_eq!(colored[(j, 0)], 0.0);
    }
}

fn build_time_switch_jacobian_dae() -> dae::Dae {
    let mut dae = dae::Dae::new();
    dae.states.insert(
        dae::VarName::new("x"),
        dae::Variable::new(dae::VarName::new("x")),
    );
    dae.algebraics.insert(
        dae::VarName::new("z"),
        dae::Variable::new(dae::VarName::new("z")),
    );

    dae.f_x.push(eq_from(binop(
        dae::OpBinary::Sub(Default::default()),
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Der,
            args: vec![var("x")],
        },
        var("z"),
    )));
    dae.f_x.push(eq_from(binop(
        dae::OpBinary::Sub(Default::default()),
        var("z"),
        dae::Expression::If {
            branches: vec![(
                binop(dae::OpBinary::Lt(Default::default()), var("time"), lit(1.0)),
                var("x"),
            )],
            else_branch: Box::new(binop(
                dae::OpBinary::Mul(Default::default()),
                lit(2.0),
                var("x"),
            )),
        },
    )));

    dae
}

#[test]
fn test_build_init_jacobian_respects_time_dependent_if_branches() {
    let dae = build_time_switch_jacobian_dae();
    let y = vec![0.25, 0.5];
    let p = default_params(&dae);
    let fixed = vec![false];
    let timeout = crate::TimeoutBudget::new(None);
    let ctx_before = init_jac_ctx(&dae, &y, &p, 0.5, 1, false);
    let ctx_after = init_jac_ctx(&dae, &y, &p, 2.0, 1, false);

    let jac_before = build_init_jacobian_dense(&ctx_before, &fixed, &timeout)
        .expect("dense Jacobian before event should build");
    let jac_after = build_init_jacobian_dense(&ctx_after, &fixed, &timeout)
        .expect("dense Jacobian after event should build");
    assert!((jac_before[(1, 0)] + 1.0).abs() < 1e-12);
    assert!((jac_after[(1, 0)] + 2.0).abs() < 1e-12);

    let colored_before = build_init_jacobian_colored(&ctx_before, &fixed, &timeout)
        .expect("colored Jacobian before event should not error")
        .expect("colored Jacobian before event should build");
    let colored_after = build_init_jacobian_colored(&ctx_after, &fixed, &timeout)
        .expect("colored Jacobian after event should not error")
        .expect("colored Jacobian after event should build");
    assert_matrix_close(&jac_before, &colored_before, 1e-12);
    assert_matrix_close(&jac_after, &colored_after, 1e-12);
}

#[test]
fn test_eval_jacobian_vector_seeds_size1_array_aliases() {
    let mut dae = dae::Dae::new();
    dae.states.insert(
        dae::VarName::new("x"),
        dae::Variable::new(dae::VarName::new("x")),
    );
    let mut y_arr = dae::Variable::new(dae::VarName::new("y"));
    y_arr.dims = vec![1];
    dae.outputs.insert(dae::VarName::new("y"), y_arr);

    dae.f_x.push(eq_from(binop(
        dae::OpBinary::Sub(Default::default()),
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Der,
            args: vec![var("x")],
        },
        lit(0.0),
    )));
    dae.f_x.push(eq_from(binop(
        dae::OpBinary::Sub(Default::default()),
        dae::Expression::VarRef {
            name: dae::VarName::new("y[1]"),
            subscripts: vec![],
        },
        var("x"),
    )));

    let y = vec![0.0, 0.0];
    let p = default_params(&dae);
    let mut out = vec![0.0; 2];
    let v = vec![0.0, 1.0];
    eval_jacobian_vector_ad(&dae, &y, &p, 0.0, &v, &mut out, 1);

    assert!(
        (out[1] - 1.0).abs() < 1e-12,
        "expected dy[1] derivative to propagate through y[1] alias"
    );
}

// =========================================================================
// BLT elimination numerical equivalence tests
//
// Verify that the reduced DAE (after eliminate_trivial) is numerically
// equivalent to the original DAE at concrete test points.
//
// Approach: evaluate the original residual with eliminated variables
// computed from substitutions, then evaluate the reduced residual.
// The remaining equations' residuals must match.
// =========================================================================
