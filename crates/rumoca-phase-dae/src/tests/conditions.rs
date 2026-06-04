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
        causality: rumoca_core::Causality::Input(rumoca_core::Token::default()),
        is_primitive: true,
        ..Default::default()
    }
}

fn parameter_var(name: &str) -> flat::Variable {
    flat::Variable {
        name: VarName::new(name),
        variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
        is_primitive: true,
        ..Default::default()
    }
}

fn var_ref(name: &str) -> Expression {
    Expression::VarRef {
        name: VarName::new(name).into(),
        subscripts: vec![],
        span: rumoca_core::Span::DUMMY,
    }
}

fn int(value: i64) -> Expression {
    Expression::Literal {
        value: Literal::Integer(value),
        span: rumoca_core::Span::DUMMY,
    }
}

fn gt(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op: rumoca_core::OpBinary::Gt,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: rumoca_core::Span::DUMMY,
    }
}

fn ge(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op: rumoca_core::OpBinary::Ge,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: rumoca_core::Span::DUMMY,
    }
}

fn gt_with_token_number(lhs: Expression, rhs: Expression, _token_number: u32) -> Expression {
    Expression::Binary {
        op: rumoca_core::OpBinary::Gt,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: rumoca_core::Span::DUMMY,
    }
}

fn if_expr(condition: Expression, when_true: Expression, when_false: Expression) -> Expression {
    Expression::If {
        branches: vec![(condition, when_true)],
        else_branch: Box::new(when_false),
        span: rumoca_core::Span::DUMMY,
    }
}

fn builtin_call(function: BuiltinFunction, args: Vec<Expression>) -> Expression {
    Expression::BuiltinCall {
        function,
        args,
        span: rumoca_core::Span::DUMMY,
    }
}

fn residual_eq(lhs: Expression, rhs: Expression, component: &str) -> flat::Equation {
    flat::Equation {
        residual: Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span: rumoca_core::Span::DUMMY,
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
            preserve_overridable_param_starts: false,
        },
    )
    .expect("ToDAE conversion should succeed")
}

fn relation_debug_set(dae_model: &Dae) -> HashSet<String> {
    dae_model
        .conditions
        .relations
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

    assert_eq!(dae_model.conditions.relations.len(), 1);
    assert_eq!(dae_model.conditions.equations.len(), 1);
    assert_eq!(
        format!("{:?}", dae_model.conditions.relations[0]),
        format!("{condition:?}")
    );
    assert_eq!(
        format!("{:?}", dae_model.conditions.equations[0].lhs),
        format!("{:?}", Some(VarName::new("c[1]")))
    );
    assert_eq!(
        format!("{:?}", dae_model.conditions.equations[0].rhs),
        format!("{condition:?}")
    );
    assert!(
        dae_model
            .variables
            .discrete_valued
            .contains_key(&VarName::new("c")),
        "canonical condition variables must be available to solver layouts"
    );
    assert!(
        matches!(
            &dae_model.continuous.equations[0].rhs,
            rumoca_core::Expression::Binary { rhs, .. }
                if matches!(
                    rhs.as_ref(),
                    rumoca_core::Expression::If { branches, .. }
                        if matches!(
                            &branches[0].0,
                            rumoca_core::Expression::VarRef { name, subscripts, .. }
                                if name.as_str() == "c"
                                    && matches!(
                                        subscripts.as_slice(),
                                        [rumoca_core::Subscript::Index { value: 1, .. }]
                                    )
                        )
                )
        ),
        "MLS Appendix B event relations in continuous equations should use c[i]"
    );
}

#[test]
fn test_todae_keeps_parameter_only_if_conditions_out_of_event_memory() {
    let mut flat = Model::new();
    flat.add_variable(VarName::new("x"), scalar_var("x"));
    flat.add_variable(VarName::new("mode"), parameter_var("mode"));
    flat.enum_literal_ordinals.extend([
        ("LimiterHomotopy.Linear".to_string(), 1),
        ("LimiterHomotopy.LowerLimit".to_string(), 2),
    ]);

    let condition = Expression::Binary {
        op: rumoca_core::OpBinary::Eq,
        lhs: Box::new(var_ref("mode")),
        rhs: Box::new(var_ref("LimiterHomotopy.LowerLimit")),
        span: rumoca_core::Span::DUMMY,
    };
    flat.add_equation(residual_eq(
        var_ref("x"),
        if_expr(condition.clone(), int(1), int(0)),
        "parameter_only_if_condition",
    ));

    let dae_model = to_dae_unbalanced_ok(&flat);

    assert!(
        dae_model.conditions.relations.is_empty(),
        "parameter-only relations cannot generate runtime events"
    );
    assert!(dae_model.conditions.equations.is_empty());
    assert!(
        !dae_model
            .variables
            .discrete_valued
            .contains_key(&VarName::new("c")),
        "time-invariant if conditions should not allocate Appendix B condition memory"
    );
    assert!(
        matches!(
            &dae_model.continuous.equations[0].rhs,
            rumoca_core::Expression::Binary { rhs, .. }
                if matches!(
                    rhs.as_ref(),
                    rumoca_core::Expression::If { branches, .. }
                        if rumoca_core::expressions_semantically_equal(&branches[0].0, &condition)
                )
        ),
        "the parameter condition should remain directly evaluable by initialization lowering"
    );
}

#[test]
fn test_todae_condition_memory_does_not_shadow_user_c() {
    let mut flat = Model::new();
    flat.add_variable(VarName::new("c"), scalar_var("c"));
    flat.add_variable(VarName::new("u"), input_var("u"));

    let condition = gt(var_ref("u"), int(0));
    flat.add_equation(residual_eq(
        var_ref("c"),
        if_expr(condition, int(1), int(0)),
        "if_condition_with_user_c",
    ));

    let dae_model = to_dae_unbalanced_ok(&flat);

    assert!(
        dae_model
            .variables
            .algebraics
            .contains_key(&VarName::new("c"))
    );
    assert!(
        dae_model
            .variables
            .discrete_valued
            .contains_key(&VarName::new("__rumoca_c")),
        "compiler-generated condition memory must avoid user variable names"
    );
    assert_eq!(
        format!("{:?}", dae_model.conditions.equations[0].lhs),
        format!("{:?}", Some(VarName::new("__rumoca_c[1]")))
    );
}

#[test]
fn test_todae_condition_memory_does_not_shadow_user_c_component() {
    let mut flat = Model::new();
    flat.add_variable(VarName::new("c.x"), scalar_var("c.x"));
    flat.add_variable(VarName::new("u"), input_var("u"));

    let condition = gt(var_ref("u"), int(0));
    flat.add_equation(residual_eq(
        var_ref("c.x"),
        if_expr(condition, int(1), int(0)),
        "if_condition_with_user_c_component",
    ));

    let dae_model = to_dae_unbalanced_ok(&flat);

    assert!(
        dae_model
            .variables
            .algebraics
            .contains_key(&VarName::new("c.x"))
    );
    assert!(
        dae_model
            .variables
            .discrete_valued
            .contains_key(&VarName::new("__rumoca_c")),
        "compiler-generated condition memory must avoid user component prefixes"
    );
    assert_eq!(
        format!("{:?}", dae_model.conditions.equations[0].lhs),
        format!("{:?}", Some(VarName::new("__rumoca_c[1]")))
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
        span: rumoca_core::Span::DUMMY,
    };
    flat.add_equation(residual_eq(
        var_ref("x"),
        if_expr(condition, int(1), int(0)),
        "if_noevent",
    ));

    let dae_model = to_dae_unbalanced_ok(&flat);

    assert!(
        dae_model.conditions.relations.is_empty(),
        "noEvent-wrapped conditions must not be emitted to relation"
    );
    assert!(
        dae_model.conditions.equations.is_empty(),
        "noEvent-wrapped conditions must not be emitted to f_c"
    );
    assert!(
        !dae_model
            .variables
            .discrete_valued
            .contains_key(&VarName::new("c")),
        "noEvent conditions must not create canonical condition variables"
    );
}

#[test]
fn test_todae_keeps_smooth_if_conditions_event_generating() {
    let mut flat = Model::new();
    flat.add_variable(VarName::new("x"), scalar_var("x"));
    flat.add_variable(VarName::new("u"), input_var("u"));

    let condition = gt(var_ref("u"), int(0));
    let smooth_if = builtin_call(
        BuiltinFunction::Smooth,
        vec![int(0), if_expr(condition.clone(), int(1), int(0))],
    );
    flat.add_equation(residual_eq(var_ref("x"), smooth_if, "if_smooth"));

    let dae_model = to_dae_unbalanced_ok(&flat);

    assert_eq!(
        dae_model.conditions.relations.len(),
        1,
        "smooth is not noEvent; relations inside smooth may still generate events"
    );
    assert_eq!(
        format!("{:?}", dae_model.conditions.relations[0]),
        format!("{condition:?}")
    );
    assert!(
        matches!(
            &dae_model.continuous.equations[0].rhs,
            rumoca_core::Expression::Binary { rhs, .. }
                if matches!(
                    rhs.as_ref(),
                    rumoca_core::Expression::BuiltinCall { args, .. }
                        if matches!(
                            &args[1],
                            rumoca_core::Expression::If { branches, .. }
                                if matches!(
                                    &branches[0].0,
                                    rumoca_core::Expression::VarRef { name, subscripts, .. }
                                        if name.as_str() == "c"
                                            && matches!(
                                                subscripts.as_slice(),
                                                [rumoca_core::Subscript::Index { value: 1, .. }]
                                            )
                                )
                        )
                )
        ),
        "relations inside smooth should be rewritten through Appendix B condition memory"
    );
}

#[test]
fn test_todae_suppresses_noevent_inside_smooth_conditions() {
    let mut flat = Model::new();
    flat.add_variable(VarName::new("x"), scalar_var("x"));
    flat.add_variable(VarName::new("u"), input_var("u"));

    let noevent_if = builtin_call(
        BuiltinFunction::NoEvent,
        vec![if_expr(gt(var_ref("u"), int(0)), int(1), int(0))],
    );
    let smooth_noevent_if = builtin_call(BuiltinFunction::Smooth, vec![int(0), noevent_if]);
    flat.add_equation(residual_eq(
        var_ref("x"),
        smooth_noevent_if,
        "if_smooth_noevent",
    ));

    let dae_model = to_dae_unbalanced_ok(&flat);

    assert!(
        dae_model.conditions.relations.is_empty(),
        "noEvent inside smooth must still suppress event-generating relations"
    );
    assert!(dae_model.conditions.equations.is_empty());
}

#[test]
fn test_todae_populates_relation_from_discrete_boolean_relation_assignment() {
    let mut flat = Model::new();
    flat.add_variable(VarName::new("u"), input_var("u"));
    flat.add_variable(VarName::new("threshold"), scalar_var("threshold"));
    flat.add_variable(
        VarName::new("hit"),
        flat::Variable {
            name: VarName::new("hit"),
            is_primitive: true,
            is_discrete_type: true,
            ..Default::default()
        },
    );

    let condition = ge(var_ref("u"), var_ref("threshold"));
    flat.add_equation(residual_eq(
        var_ref("hit"),
        condition.clone(),
        "boolean_relation_assignment",
    ));

    let dae_model = to_dae_unbalanced_ok(&flat);

    assert_eq!(
        dae_model.conditions.relations.len(),
        1,
        "Boolean relation assignments such as GreaterEqualThreshold.y must produce an event root"
    );
    assert_eq!(
        format!("{:?}", dae_model.conditions.relations[0]),
        format!("{condition:?}")
    );
    assert!(
        dae_model
            .conditions
            .equations
            .iter()
            .any(|eq| eq.lhs.as_ref() == Some(&VarName::new("c[1]"))),
        "the relation must have Appendix B condition memory"
    );
}

#[test]
fn test_todae_includes_when_condition_and_nested_conditional_when_conditions() {
    let mut flat = Model::new();
    flat.add_variable(VarName::new("u"), input_var("u"));
    flat.add_variable(VarName::new("u2"), input_var("u2"));
    let mut z = scalar_var("z");
    z.variability = rumoca_core::Variability::Discrete(rumoca_core::Token::default());
    flat.add_variable(VarName::new("z"), z);

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

    assert_eq!(dae_model.conditions.relations.len(), 2);
    assert_eq!(dae_model.conditions.equations.len(), 2);
    assert!(relation_set.contains(&format!("{when_condition:?}")));
    assert!(relation_set.contains(&format!("{nested_condition:?}")));
}

#[test]
fn test_todae_reuses_condition_variable_for_repeated_if_condition() {
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
        dae_model.conditions.relations.len(),
        1,
        "repeated condition expressions should share one canonical relation"
    );
    assert_eq!(dae_model.conditions.equations.len(), 1);
}

#[test]
fn test_todae_interns_conditions_with_different_source_tokens() {
    let mut flat = Model::new();
    flat.add_variable(VarName::new("x"), scalar_var("x"));
    flat.add_variable(VarName::new("y"), scalar_var("y"));
    flat.add_variable(VarName::new("u"), input_var("u"));

    flat.add_equation(residual_eq(
        var_ref("x"),
        if_expr(
            gt_with_token_number(var_ref("u"), int(0), 1),
            int(1),
            int(0),
        ),
        "dup_source_1",
    ));
    flat.add_equation(residual_eq(
        var_ref("y"),
        if_expr(
            gt_with_token_number(var_ref("u"), int(0), 2),
            int(2),
            int(0),
        ),
        "dup_source_2",
    ));

    let dae_model = to_dae_unbalanced_ok(&flat);

    assert_eq!(
        dae_model.conditions.relations.len(),
        1,
        "MLS Appendix B relation identity is semantic; source-token metadata must not duplicate roots"
    );
    assert_eq!(dae_model.conditions.equations.len(), 1);
    assert!(dae_model.continuous.equations.iter().all(|eq| {
        matches!(
            &eq.rhs,
            rumoca_core::Expression::Binary { rhs, .. }
                if matches!(
                    rhs.as_ref(),
                    rumoca_core::Expression::If { branches, .. }
                        if matches!(
                            &branches[0].0,
                            rumoca_core::Expression::VarRef { name, subscripts, .. }
                                if name.as_str() == "c"
                                    && matches!(
                                        subscripts.as_slice(),
                                        [rumoca_core::Subscript::Index { value: 1, .. }]
                                    )
                        )
                )
        )
    }));
}
