use super::*;
use rumoca_core::Span;
use rumoca_ir_flat as flat;
use std::collections::HashSet;

fn scalar_var(name: &str) -> flat::Variable {
    flat::Variable {
        name: VarName::new(name),
        is_primitive: true,
        ..Default::default()
    }
}

fn input_var(name: &str) -> flat::Variable {
    flat::Variable {
        name: VarName::new(name),
        causality: rumoca_ir_core::Causality::Input(rumoca_ir_core::Token::default()),
        is_primitive: true,
        ..Default::default()
    }
}

fn var_ref(name: &str) -> Expression {
    Expression::VarRef {
        name: VarName::new(name),
        subscripts: vec![],
    }
}

fn int(value: i64) -> Expression {
    Expression::Literal(Literal::Integer(value))
}

fn gt(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op: rumoca_ir_core::OpBinary::Gt(rumoca_ir_core::Token::default()),
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

fn if_expr(condition: Expression, when_true: Expression, when_false: Expression) -> Expression {
    Expression::If {
        branches: vec![(condition, when_true)],
        else_branch: Box::new(when_false),
    }
}

fn residual_eq(lhs: Expression, rhs: Expression, component: &str) -> flat::Equation {
    flat::Equation {
        residual: Expression::Binary {
            op: rumoca_ir_core::OpBinary::Sub(rumoca_ir_core::Token::default()),
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        },
        span: Span::DUMMY,
        origin: flat::EquationOrigin::ComponentEquation {
            component: component.to_string(),
        },
        scalar_count: 1,
    }
}

fn to_dae_unbalanced_ok(model: &Model) -> Dae {
    to_dae_with_options(
        model,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("ToDAE conversion should succeed")
}

fn relation_debug_set(dae_model: &Dae) -> HashSet<String> {
    dae_model
        .relation
        .iter()
        .map(|expr| format!("{expr:?}"))
        .collect()
}

#[test]
fn test_todae_populates_relation_and_fc_from_if_conditions() {
    let mut flat = Model::new();
    flat.add_variable(VarName::new("x"), scalar_var("x"));
    flat.add_variable(VarName::new("u"), input_var("u"));

    let condition = gt(var_ref("u"), int(0));
    flat.add_equation(residual_eq(
        var_ref("x"),
        if_expr(condition.clone(), int(1), int(0)),
        "if_condition",
    ));

    let dae_model = to_dae_unbalanced_ok(&flat);

    assert_eq!(dae_model.relation.len(), 1);
    assert_eq!(dae_model.f_c.len(), 1);
    assert_eq!(
        format!("{:?}", dae_model.relation[0]),
        format!("{condition:?}")
    );
    assert_eq!(
        format!("{:?}", dae_model.f_c[0].lhs),
        format!("{:?}", Some(VarName::new("c[1]")))
    );
    assert_eq!(
        format!("{:?}", dae_model.f_c[0].rhs),
        format!("{condition:?}")
    );
}

#[test]
fn test_todae_suppresses_noevent_if_conditions_from_relation() {
    let mut flat = Model::new();
    flat.add_variable(VarName::new("x"), scalar_var("x"));
    flat.add_variable(VarName::new("u"), input_var("u"));

    let condition = Expression::BuiltinCall {
        function: BuiltinFunction::NoEvent,
        args: vec![gt(var_ref("u"), int(0))],
    };
    flat.add_equation(residual_eq(
        var_ref("x"),
        if_expr(condition, int(1), int(0)),
        "if_noevent",
    ));

    let dae_model = to_dae_unbalanced_ok(&flat);

    assert!(
        dae_model.relation.is_empty(),
        "noEvent-wrapped conditions must not be emitted to relation"
    );
    assert!(
        dae_model.f_c.is_empty(),
        "noEvent-wrapped conditions must not be emitted to f_c"
    );
}

#[test]
fn test_todae_includes_when_condition_and_nested_conditional_when_conditions() {
    let mut flat = Model::new();
    flat.add_variable(VarName::new("u"), input_var("u"));
    flat.add_variable(VarName::new("u2"), input_var("u2"));
    flat.add_variable(VarName::new("z"), scalar_var("z"));

    let when_condition = gt(var_ref("u"), int(0));
    let nested_condition = gt(var_ref("u2"), int(0));

    let branch_assign =
        flat::WhenEquation::assign(VarName::new("z"), int(1), Span::DUMMY, "branch assign");
    let else_assign =
        flat::WhenEquation::assign(VarName::new("z"), int(2), Span::DUMMY, "else assign");
    let nested_conditional = flat::WhenEquation::conditional(
        vec![(nested_condition.clone(), vec![branch_assign])],
        vec![else_assign],
        Span::DUMMY,
        "nested when conditional",
    );

    let mut when_clause = flat::WhenClause::new(when_condition.clone(), Span::DUMMY);
    when_clause.add_equation(nested_conditional);
    flat.when_clauses.push(when_clause);

    let dae_model = to_dae_unbalanced_ok(&flat);
    let relation_set = relation_debug_set(&dae_model);

    assert_eq!(dae_model.relation.len(), 2);
    assert_eq!(dae_model.f_c.len(), 2);
    assert!(relation_set.contains(&format!("{when_condition:?}")));
    assert!(relation_set.contains(&format!("{nested_condition:?}")));
}

#[test]
fn test_todae_deduplicates_duplicate_if_conditions() {
    let mut flat = Model::new();
    flat.add_variable(VarName::new("x"), scalar_var("x"));
    flat.add_variable(VarName::new("y"), scalar_var("y"));
    flat.add_variable(VarName::new("u"), input_var("u"));

    let condition = gt(var_ref("u"), int(0));
    flat.add_equation(residual_eq(
        var_ref("x"),
        if_expr(condition.clone(), int(1), int(0)),
        "dup_1",
    ));
    flat.add_equation(residual_eq(
        var_ref("y"),
        if_expr(condition, int(2), int(0)),
        "dup_2",
    ));

    let dae_model = to_dae_unbalanced_ok(&flat);

    assert_eq!(
        dae_model.relation.len(),
        1,
        "duplicate condition expressions should be canonicalized once"
    );
    assert_eq!(dae_model.f_c.len(), 1);
}
