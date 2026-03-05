use rumoca_ir_dae as dae;
use rumoca_sim_core::simulation::dae_prepare::{expr_contains_der_of, expr_refers_to_var};

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
