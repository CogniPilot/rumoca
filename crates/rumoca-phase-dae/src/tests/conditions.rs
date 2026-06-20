use super::*;
use rumoca_core::Span;
use rumoca_ir_flat as flat;

fn test_span(start: usize, end: usize) -> Span {
    Span::from_offsets(rumoca_core::SourceId(91), start, end)
}

fn scalar_var(name: &str) -> flat::Variable {
    crate::test_support::with_component_ref(flat::Variable {
        name: VarName::new(name),
        is_primitive: true,
        ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name(file!()),
            1,
            2,
        ))
    })
}

fn input_var(name: &str) -> flat::Variable {
    crate::test_support::with_component_ref(flat::Variable {
        name: VarName::new(name),
        causality: rumoca_core::Causality::Input(rumoca_core::Token::default()),
        is_primitive: true,
        ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name(file!()),
            1,
            2,
        ))
    })
}

fn parameter_var(name: &str) -> flat::Variable {
    crate::test_support::with_component_ref(flat::Variable {
        name: VarName::new(name),
        variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
        is_primitive: true,
        ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name(file!()),
            1,
            2,
        ))
    })
}

fn var_ref(name: &str) -> Expression {
    Expression::VarRef {
        name: VarName::new(name).into(),
        subscripts: vec![],
        span: test_span(1, 2),
    }
}

fn int(value: i64) -> Expression {
    Expression::Literal {
        value: Literal::Integer(value),
        span: test_span(3, 4),
    }
}

fn gt(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op: rumoca_core::OpBinary::Gt,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: test_span(5, 6),
    }
}

fn and(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op: rumoca_core::OpBinary::And,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: test_span(7, 8),
    }
}

fn ge(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op: rumoca_core::OpBinary::Ge,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: test_span(9, 10),
    }
}

fn gt_with_token_number(lhs: Expression, rhs: Expression, _token_number: u32) -> Expression {
    Expression::Binary {
        op: rumoca_core::OpBinary::Gt,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: test_span(11, 12),
    }
}

fn if_expr(condition: Expression, when_true: Expression, when_false: Expression) -> Expression {
    Expression::If {
        branches: vec![(condition, when_true)],
        else_branch: Box::new(when_false),
        span: test_span(13, 14),
    }
}

fn not(expr: Expression) -> Expression {
    Expression::Unary {
        op: rumoca_core::OpUnary::Not,
        rhs: Box::new(expr),
        span: test_span(15, 16),
    }
}

fn builtin_call(function: BuiltinFunction, args: Vec<Expression>) -> Expression {
    Expression::BuiltinCall {
        function,
        args,
        span: test_span(17, 18),
    }
}

fn residual_eq(lhs: Expression, rhs: Expression, component: &str) -> flat::Equation {
    flat::Equation {
        residual: Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span: test_span(19, 20),
        },
        span: test_span(19, 20),
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

fn relations_contain(dae_model: &Dae, expected: &Expression) -> bool {
    dae_model
        .conditions
        .relations
        .iter()
        .any(|relation| rumoca_core::expressions_semantically_equal(relation, expected))
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
    assert!(
        rumoca_core::expressions_semantically_equal(&dae_model.conditions.relations[0], &condition),
        "relation should match the source condition"
    );
    let lhs = dae_model.conditions.equations[0]
        .lhs
        .as_ref()
        .expect("condition equation should have a generated lhs");
    assert_eq!(lhs.as_str(), "c[1]");
    assert!(lhs.is_generated());
    assert!(lhs.component_ref().is_some());
    assert!(
        rumoca_core::expressions_semantically_equal(
            &dae_model.conditions.equations[0].rhs,
            &condition
        ),
        "condition equation rhs should match the source condition"
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
        span: test_span(21, 22),
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
    // Enum literal references lower to their ordinals during conversion, so
    // the directly evaluable runtime form is `mode == 2`.
    let lowered_condition = Expression::Binary {
        op: rumoca_core::OpBinary::Eq,
        lhs: Box::new(var_ref("mode")),
        rhs: Box::new(int(2)),
        span: test_span(23, 24),
    };
    assert!(
        matches!(
            &dae_model.continuous.equations[0].rhs,
            rumoca_core::Expression::Binary { rhs, .. }
                if matches!(
                    rhs.as_ref(),
                    rumoca_core::Expression::If { branches, .. }
                        if rumoca_core::expressions_semantically_equal(
                            &branches[0].0,
                            &lowered_condition,
                        )
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
    let lhs = dae_model.conditions.equations[0]
        .lhs
        .as_ref()
        .expect("condition equation should have a generated lhs");
    assert_eq!(lhs.as_str(), "__rumoca_c[1]");
    assert!(lhs.is_generated());
    assert!(lhs.component_ref().is_some());
}

#[test]
fn test_generated_condition_memory_preserves_relation_span() {
    let mut flat = Model::new();
    flat.add_variable(VarName::new("x"), scalar_var("x"));
    flat.add_variable(VarName::new("u"), input_var("u"));

    let relation_span = test_span(30, 42);
    let condition = Expression::Binary {
        op: rumoca_core::OpBinary::Gt,
        lhs: Box::new(var_ref("u")),
        rhs: Box::new(int(0)),
        span: relation_span,
    };
    let mut equation = residual_eq(
        var_ref("x"),
        if_expr(condition, int(1), int(0)),
        "if_condition_with_relation_span",
    );
    equation.span = relation_span;
    flat.add_equation(equation);

    let dae_model = to_dae_unbalanced_ok(&flat);
    let condition_var = &dae_model.variables.discrete_valued[&VarName::new("c")];
    let pre_condition_var = &dae_model.variables.parameters[&VarName::new("__pre__.c")];

    assert_eq!(condition_var.source_span, relation_span);
    assert_eq!(condition_var.start_attribute_span(), Some(relation_span));
    assert_eq!(
        condition_var.start.as_ref().and_then(Expression::span),
        Some(relation_span)
    );
    assert_eq!(
        condition_var
            .component_ref
            .as_ref()
            .map(|component_ref| component_ref.span),
        Some(relation_span)
    );
    if let Some(Expression::Array { elements, .. }) = &condition_var.start {
        assert!(
            elements
                .iter()
                .all(|element| element.span() == Some(relation_span)),
            "generated condition start elements must use the relation owner span"
        );
    } else {
        panic!("generated condition memory start should be a Boolean array");
    }

    assert_eq!(pre_condition_var.source_span, relation_span);
    assert_eq!(
        pre_condition_var.start_attribute_span(),
        Some(relation_span)
    );
    assert_eq!(
        pre_condition_var
            .component_ref
            .as_ref()
            .map(|component_ref| component_ref.span),
        Some(relation_span)
    );
    assert_eq!(
        pre_condition_var
            .component_ref
            .as_ref()
            .and_then(|component_ref| component_ref.parts.first())
            .map(|part| part.span),
        Some(relation_span)
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
    let lhs = dae_model.conditions.equations[0]
        .lhs
        .as_ref()
        .expect("condition equation should have a generated lhs");
    assert_eq!(lhs.as_str(), "__rumoca_c[1]");
    assert!(lhs.is_generated());
    assert!(lhs.component_ref().is_some());
}

#[test]
fn test_todae_suppresses_noevent_if_conditions_from_relation() {
    let mut flat = Model::new();
    flat.add_variable(VarName::new("x"), scalar_var("x"));
    flat.add_variable(VarName::new("u"), input_var("u"));

    let condition = Expression::BuiltinCall {
        function: BuiltinFunction::NoEvent,
        args: vec![gt(var_ref("u"), int(0))],
        span: test_span(25, 26),
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
fn test_todae_suppresses_initial_only_branch_relations_from_runtime_roots() {
    let mut flat = Model::new();
    flat.add_variable(VarName::new("x"), scalar_var("x"));
    flat.add_variable(VarName::new("u"), input_var("u"));

    let initial_only_relation = gt(var_ref("u"), int(0));
    let runtime_relation = gt(var_ref("u"), int(1));
    flat.add_equation(residual_eq(
        var_ref("x"),
        if_expr(
            builtin_call(BuiltinFunction::Initial, vec![]),
            if_expr(initial_only_relation.clone(), int(1), int(0)),
            if_expr(runtime_relation.clone(), int(2), int(3)),
        ),
        "initial_guarded_relation",
    ));

    let dae_model = to_dae_unbalanced_ok(&flat);

    assert!(
        !relations_contain(&dae_model, &initial_only_relation),
        "relations reachable only while initial() is true must not become runtime roots"
    );
    assert!(
        relations_contain(&dae_model, &runtime_relation),
        "relations reachable after initialization must remain runtime roots"
    );
}

#[test]
fn test_todae_suppresses_initial_only_condition_relations_from_runtime_roots() {
    let mut flat = Model::new();
    flat.add_variable(VarName::new("x"), scalar_var("x"));
    flat.add_variable(VarName::new("u"), input_var("u"));

    let initial_only_relation = gt(var_ref("u"), int(0));
    flat.add_equation(residual_eq(
        var_ref("x"),
        if_expr(
            and(
                builtin_call(BuiltinFunction::Initial, vec![]),
                initial_only_relation.clone(),
            ),
            int(1),
            int(0),
        ),
        "initial_condition_relation",
    ));

    let dae_model = to_dae_unbalanced_ok(&flat);

    assert!(
        !relations_contain(&dae_model, &initial_only_relation),
        "relations inside conditions that are runtime-false due to initial() must not become roots"
    );
}

#[test]
fn test_todae_suppresses_not_initial_else_relations_from_runtime_roots() {
    let mut flat = Model::new();
    flat.add_variable(VarName::new("x"), scalar_var("x"));
    flat.add_variable(VarName::new("u"), input_var("u"));

    let runtime_relation = gt(var_ref("u"), int(1));
    let initial_only_relation = gt(var_ref("u"), int(0));
    flat.add_equation(residual_eq(
        var_ref("x"),
        if_expr(
            not(builtin_call(BuiltinFunction::Initial, vec![])),
            if_expr(runtime_relation.clone(), int(2), int(3)),
            if_expr(initial_only_relation.clone(), int(1), int(0)),
        ),
        "not_initial_guarded_relation",
    ));

    let dae_model = to_dae_unbalanced_ok(&flat);

    assert!(
        relations_contain(&dae_model, &runtime_relation),
        "not initial() branch relations are active during runtime"
    );
    assert!(
        !relations_contain(&dae_model, &initial_only_relation),
        "else relations guarded by not initial() must not become runtime roots"
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
    assert!(
        rumoca_core::expressions_semantically_equal(&dae_model.conditions.relations[0], &condition),
        "relation should match the source condition"
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
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("hit"),
            is_primitive: true,
            is_discrete_type: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
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
    assert!(
        rumoca_core::expressions_semantically_equal(&dae_model.conditions.relations[0], &condition),
        "relation should match the source condition"
    );
    assert!(
        dae_model
            .conditions
            .equations
            .iter()
            .any(|eq| eq.lhs.as_ref().is_some_and(|lhs| lhs.as_str() == "c[1]")),
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

    let branch_assign = flat::WhenEquation::assign(
        VarName::new("z"),
        int(1),
        test_span(27, 28),
        "branch assign",
    );
    let else_assign =
        flat::WhenEquation::assign(VarName::new("z"), int(2), test_span(29, 30), "else assign");
    let nested_conditional = flat::WhenEquation::conditional(
        vec![(nested_condition.clone(), vec![branch_assign])],
        vec![else_assign],
        test_span(31, 32),
        "nested when conditional",
    );

    let mut when_clause = flat::WhenClause::new(when_condition.clone(), test_span(33, 34));
    when_clause.add_equation(nested_conditional);
    flat.when_clauses.push(when_clause);

    let dae_model = to_dae_unbalanced_ok(&flat);

    assert_eq!(dae_model.conditions.relations.len(), 2);
    assert_eq!(dae_model.conditions.equations.len(), 2);
    assert!(relations_contain(&dae_model, &when_condition));
    assert!(relations_contain(&dae_model, &nested_condition));
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
