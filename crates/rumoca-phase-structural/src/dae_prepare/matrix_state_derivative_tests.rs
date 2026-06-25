use super::*;
use std::collections::{HashMap, HashSet};

fn test_span() -> Span {
    Span::from_offsets(
        rumoca_core::SourceId::from_source_name("matrix_state_derivative_test.mo"),
        1,
        2,
    )
}

fn test_variable(name: &str) -> Variable {
    let mut variable = Variable::new(VarName::new(name), test_span());
    variable.source_span = test_span();
    variable
}

fn var(name: &str) -> Expression {
    Expression::VarRef {
        name: rumoca_core::Reference::new(name),
        subscripts: vec![],
        span: Span::DUMMY,
    }
}

fn var_idx2(name: &str, row: i64, col: i64) -> Expression {
    Expression::VarRef {
        name: rumoca_core::Reference::new(name),
        subscripts: vec![
            Subscript::generated_index(row, Span::DUMMY),
            Subscript::generated_index(col, Span::DUMMY),
        ],
        span: Span::DUMMY,
    }
}

fn var_idx1(name: &str, index: i64) -> Expression {
    Expression::VarRef {
        name: rumoca_core::Reference::new(name),
        subscripts: vec![Subscript::generated_index(index, Span::DUMMY)],
        span: Span::DUMMY,
    }
}

fn real(v: f64) -> Expression {
    Expression::Literal {
        value: Literal::Real(v),
        span: Span::DUMMY,
    }
}

fn array(elements: Vec<Expression>) -> Expression {
    Expression::Array {
        elements,
        is_matrix: false,
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

fn der(expr: Expression) -> Expression {
    Expression::BuiltinCall {
        function: BuiltinFunction::Der,
        args: vec![expr],
        span: test_span(),
    }
}

fn der_idx2(name: &str, row: i64, col: i64) -> Expression {
    der(var_idx2(name, row, col))
}

fn der_idx1(name: &str, index: i64) -> Expression {
    der(var_idx1(name, index))
}

fn eq(rhs: Expression) -> Equation {
    Equation {
        lhs: None,
        rhs,
        span: Span::DUMMY,
        origin: "matrix state derivative regression".to_string(),
        scalar_count: 1,
    }
}

fn matrix_derivative_product_dae() -> Dae {
    let mut dae = Dae::new();
    let mut r = test_variable("R");
    r.dims = vec![3, 3];
    dae.variables.states.insert(VarName::new("R"), r);
    let mut skew = test_variable("skew");
    skew.dims = vec![3, 3];
    dae.variables.algebraics.insert(VarName::new("skew"), skew);

    dae.continuous.equations.push(eq(sub(
        var("skew"),
        array(vec![
            array(vec![real(0.0), real(-1.0), real(0.0)]),
            array(vec![real(1.0), real(0.0), real(0.0)]),
            array(vec![real(0.0), real(0.0), real(0.0)]),
        ]),
    )));
    dae.continuous
        .equations
        .push(eq(sub(der(var("R")), mul(var("R"), var("skew")))));
    dae
}

#[test]
fn test_exact_alias_component_rewrites_indexed_state_derivative_to_canonical_state() {
    let mut dae = Dae::new();
    let mut position = test_variable("position[1]");
    position.state_select = rumoca_core::StateSelect::Always;
    dae.variables
        .states
        .insert(VarName::new("position[1]"), position);
    dae.variables
        .states
        .insert(VarName::new("p[1]"), test_variable("p[1]"));
    dae.variables
        .algebraics
        .insert(VarName::new("v[1]"), test_variable("v[1]"));

    dae.continuous
        .equations
        .push(eq(sub(var_idx1("position", 1), var_idx1("p", 1))));
    dae.continuous
        .equations
        .push(eq(sub(der_idx1("p", 1), var_idx1("v", 1))));

    let demoted =
        demote_exact_alias_component_states(&mut dae).expect("exact alias demotion should run");

    assert_eq!(demoted, 1);
    assert!(
        dae.variables
            .states
            .contains_key(&VarName::new("position[1]"))
    );
    assert!(!dae.variables.states.contains_key(&VarName::new("p[1]")));
    assert!(
        dae.continuous
            .equations
            .iter()
            .any(|eq| expr_contains_der_of(&eq.rhs, &VarName::new("position[1]"))),
        "indexed derivative alias should be rewritten to the canonical scalarized state"
    );
    assert!(
        !dae.continuous
            .equations
            .iter()
            .any(|eq| expr_contains_der_of(&eq.rhs, &VarName::new("p[1]"))),
        "demoted scalarized state derivative should not survive exact alias rewrite"
    );
}

#[test]
fn test_assignable_derivative_rows_keep_matrix_state_component_rows() {
    let mut dae = Dae::new();
    let mut r = test_variable("R");
    r.dims = vec![3, 3];
    dae.variables.states.insert(VarName::new("R"), r);
    let mut rhs = test_variable("rhs");
    rhs.dims = vec![3, 3];
    dae.variables.algebraics.insert(VarName::new("rhs"), rhs);

    for row in 1..=3 {
        for col in 1..=3 {
            dae.continuous
                .equations
                .push(eq(sub(der_idx2("R", row, col), var_idx2("rhs", row, col))));
        }
    }

    let demoted = demote_states_without_assignable_derivative_rows(&mut dae);
    assert_eq!(demoted, 0);
    assert!(dae.variables.states.contains_key(&VarName::new("R")));
}

#[test]
fn test_direct_demotion_keeps_matrix_state_component_ode_rows() {
    let mut dae = Dae::new();
    let mut r = test_variable("R");
    r.dims = vec![3, 3];
    dae.variables.states.insert(VarName::new("R"), r);
    let mut skew = test_variable("skew");
    skew.dims = vec![3, 3];
    dae.variables.algebraics.insert(VarName::new("skew"), skew);

    for row in 1..=3 {
        for col in 1..=3 {
            dae.continuous.equations.push(eq(sub(
                der_idx2("R", row, col),
                mul(var_idx2("R", row, 1), var_idx2("skew", 1, col)),
            )));
        }
    }

    let demoted = demote_direct_assigned_states(&mut dae).expect("direct demotion should succeed");
    assert_eq!(demoted, 0);
    assert!(dae.variables.states.contains_key(&VarName::new("R")));
}

#[test]
fn test_direct_demotion_keeps_array_state_components_defined_by_output_aliases() {
    let mut dae = Dae::new();
    let mut p = test_variable("p");
    p.dims = vec![3];
    dae.variables.states.insert(VarName::new("p"), p);
    let mut position = test_variable("position");
    position.dims = vec![3];
    dae.variables
        .outputs
        .insert(VarName::new("position"), position);
    let mut v = test_variable("v");
    v.dims = vec![3];
    dae.variables.algebraics.insert(VarName::new("v"), v);

    for index in 1..=3 {
        dae.continuous
            .equations
            .push(eq(sub(var_idx1("position", index), var_idx1("p", index))));
        dae.continuous
            .equations
            .push(eq(sub(der_idx1("p", index), var_idx1("v", index))));
    }

    let demoted = demote_direct_assigned_states(&mut dae).expect("direct demotion should run");

    assert_eq!(demoted, 0);
    assert!(dae.variables.states.contains_key(&VarName::new("p")));
    for index in 1..=3 {
        assert!(
            dae.continuous
                .equations
                .iter()
                .any(|eq| expr_contains_der_of(&eq.rhs, &VarName::new(format!("p[{index}]")))),
            "state component p[{index}] should keep its derivative row"
        );
    }
}

#[test]
fn test_direct_demotion_keeps_scalarized_matrix_state_component_ode_rows() {
    let mut dae = matrix_derivative_product_dae();

    crate::scalarize::scalarize_equations(&mut dae).expect("scalarization should succeed");
    let demoted = demote_direct_assigned_states(&mut dae).expect("direct demotion should succeed");
    assert_eq!(demoted, 0);
    assert!(dae.variables.states.contains_key(&VarName::new("R")));
}

#[test]
fn test_eliminate_trivial_accepts_scalarized_matrix_state_component_ode_rows() {
    let mut dae = matrix_derivative_product_dae();

    crate::scalarize::scalarize_equations(&mut dae).expect("scalarization should succeed");
    demote_direct_assigned_states(&mut dae).expect("direct demotion should succeed");
    let result = crate::eliminate::eliminate_trivial(&mut dae).expect("elimination should run");

    assert!(
        result.blt_error.is_none(),
        "matrix ODE rows should stay structurally matchable: {:?}",
        result.blt_error
    );
    demote_states_without_retained_derivative_rows(&mut dae)
        .expect("post-elimination retained derivative demotion should run");
    assert!(dae.variables.states.contains_key(&VarName::new("R")));
}

#[test]
fn test_prepare_retains_scalarized_matrix_state_component_ode_rows() {
    let mut dae = matrix_derivative_product_dae();

    crate::scalarize::scalarize_equations(&mut dae).expect("scalarization should succeed");
    assert_eq!(demote_direct_assigned_states(&mut dae).unwrap(), 0);
    assert_eq!(reduce_constrained_dummy_derivatives(&mut dae).unwrap(), 0);
    assert_eq!(index_reduce_missing_state_derivatives(&mut dae).unwrap(), 0);
    assert_eq!(
        demote_states_without_assignable_derivative_rows(&mut dae),
        0
    );
    eliminate_derivative_aliases(&mut dae).expect("derivative alias elimination should run");
    demote_states_without_retained_derivative_rows(&mut dae)
        .expect("retained derivative demotion should run");

    assert!(dae.variables.states.contains_key(&VarName::new("R")));
}

#[test]
fn test_expand_derivative_preserves_indexed_state_component_derivative() {
    let mut dae = Dae::new();
    let mut r = test_variable("R");
    r.dims = vec![3, 3];
    dae.variables.states.insert(VarName::new("R"), r);

    let expanded = expand_der_in_expr_full(
        &der_idx2("R", 1, 2),
        &dae,
        &HashMap::from([("R".to_string(), array(vec![real(0.0)]))]),
        &HashSet::from(["R".to_string()]),
    );

    let Expression::BuiltinCall { function, args, .. } = expanded else {
        panic!("expected indexed state derivative to remain a der() call");
    };
    assert_eq!(function, BuiltinFunction::Der);
    assert_eq!(args.len(), 1);
    let Expression::VarRef {
        name, subscripts, ..
    } = &args[0]
    else {
        panic!("expected indexed state derivative argument");
    };
    assert_eq!(name.as_str(), "R");
    assert_eq!(subscripts.len(), 2);
}
