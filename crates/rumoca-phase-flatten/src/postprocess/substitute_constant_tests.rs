use super::*;
use rumoca_core::Span;

fn test_span() -> Span {
    Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_flatten_substitute_constant_source_91.mo"),
        10,
        22,
    )
}

fn simple_assignment(value: rumoca_core::Expression) -> rumoca_core::Statement {
    rumoca_core::Statement::Assignment {
        comp: rumoca_core::ComponentReference {
            local: false,
            span: rumoca_core::Span::DUMMY,
            parts: vec![rumoca_core::ComponentRefPart {
                ident: "y".to_string(),
                span: rumoca_core::Span::DUMMY,
                subs: vec![],
            }],
            def_id: None,
        },
        value,
        span: rumoca_core::Span::DUMMY,
    }
}

fn var_ref(name: &str) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::new(name),
        subscripts: vec![],
        span: rumoca_core::Span::DUMMY,
    }
}

fn named_arg(name: &str, value: rumoca_core::Expression) -> rumoca_core::Expression {
    rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new(format!(
            "{}{name}",
            rumoca_core::NAMED_FUNCTION_ARG_PREFIX
        )),
        args: vec![value],
        is_constructor: true,
        span: rumoca_core::Span::DUMMY,
    }
}

fn spanned_var_ref(name: &str) -> rumoca_core::Expression {
    let var_name = rumoca_core::VarName::new(name);
    let component_ref = rumoca_core::component_reference_from_flat_name(&var_name, test_span())
        .expect("test reference should parse as a component reference");
    rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::from_component_reference(component_ref),
        subscripts: vec![],
        span: test_span(),
    }
}

fn int_literal(value: i64) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Integer(value),
        span: rumoca_core::Span::DUMMY,
    }
}

fn string_literal(value: &str) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::String(value.to_string()),
        span: rumoca_core::Span::DUMMY,
    }
}

fn reference_x_fill_expr() -> rumoca_core::Expression {
    rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Fill,
        args: vec![
            rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Div,
                lhs: Box::new(int_literal(1)),
                rhs: Box::new(var_ref("nS")),
                span: rumoca_core::Span::DUMMY,
            },
            var_ref("nS"),
        ],
        span: rumoca_core::Span::DUMMY,
    }
}

fn add_primitive_variable(model: &mut flat::Model, name: &str) {
    model.add_variable(
        rumoca_core::VarName::new(name),
        flat::Variable {
            name: rumoca_core::VarName::new(name),
            is_primitive: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );
}

#[test]
fn substitutes_qualified_real_class_constant_inside_builtin_array_binding() {
    let mut model = flat::Model::new();
    let name = rumoca_core::VarName::new("pid.D.T");
    model.add_variable(
        name.clone(),
        flat::Variable {
            name: name.clone(),
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            binding: Some(rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Max,
                args: vec![rumoca_core::Expression::Array {
                    elements: vec![
                        var_ref("pid.Nd"),
                        rumoca_core::Expression::Binary {
                            op: rumoca_core::OpBinary::Mul,
                            lhs: Box::new(int_literal(100)),
                            rhs: Box::new(spanned_var_ref("Modelica.Constants.eps")),
                            span: test_span(),
                        },
                    ],
                    is_matrix: false,
                    span: test_span(),
                }],
                span: test_span(),
            }),
            binding_from_modification: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    let mut ctx = Context::new();
    ctx.class_constant_keys
        .insert("Modelica.Constants.eps".to_string());
    ctx.real_parameter_values
        .insert("Modelica.Constants.eps".to_string(), f64::EPSILON);

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    assert!(
        !expr_contains_var_ref(
            model.variables[&name].binding.as_ref().unwrap(),
            "Modelica.Constants.eps"
        ),
        "Modelica.Constants.eps should be folded inside runtime parameter modifier bindings: {:?}",
        model.variables[&name].binding
    );
}

#[test]
fn substitute_known_constants_preserves_named_arg_marker_for_record_constructor_value() {
    let mut model = flat::Model::new();
    model.add_equation(flat::Equation::new(
        rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::new("Pkg.f"),
            args: vec![
                named_arg("x", var_ref("live_x")),
                named_arg("per", var_ref("pCur1")),
            ],
            is_constructor: false,
            span: rumoca_core::Span::DUMMY,
        },
        rumoca_core::Span::DUMMY,
        flat::EquationOrigin::ComponentEquation {
            component: "test".to_string(),
        },
    ));
    let mut ctx = Context::new();
    ctx.constant_values.insert(
        "pCur1".to_string(),
        rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::new("Buildings.Fluid.Movers.Data.Generic"),
            args: vec![var_ref("pCur1.V_flow"), var_ref("pCur1.dp")],
            is_constructor: true,
            span: rumoca_core::Span::DUMMY,
        },
    );

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    let rumoca_core::Expression::FunctionCall { args, .. } = &model.equations[0].residual else {
        panic!("expected function call");
    };
    assert_eq!(args.len(), 2);
    let rumoca_core::Expression::FunctionCall {
        name,
        args: per_args,
        is_constructor: true,
        ..
    } = &args[1]
    else {
        panic!("expected named per argument marker");
    };
    assert_eq!(name.as_str(), "__rumoca_named_arg__.per");
    assert!(matches!(
        per_args.as_slice(),
        [rumoca_core::Expression::FunctionCall { name, is_constructor: true, .. }]
            if name.as_str() == "Buildings.Fluid.Movers.Data.Generic"
    ));
}

#[test]
fn substitute_known_constants_preserves_path_like_string_literal() {
    let mut ctx = Context::new();
    ctx.string_parameter_values.insert(
        "zone.spawnExe".to_string(),
        "spawn-0.4.3-7048a72798".to_string(),
    );

    let substituted = substitute_known_constants_expr(
        var_ref("zone.spawnExe"),
        &ctx,
        &rustc_hash::FxHashSet::default(),
        &std::collections::HashSet::new(),
        "",
    )
    .unwrap();

    assert_eq!(substituted, string_literal("spawn-0.4.3-7048a72798"));
}

#[test]
fn substitute_known_constants_recovers_path_like_string_variable_start() {
    let mut model = flat::Model::new();
    let name = rumoca_core::VarName::new("zone.spawnExe");
    model.add_variable(
        name.clone(),
        flat::Variable {
            name: name.clone(),
            type_id: rumoca_core::TypeId(3),
            start: Some(rumoca_core::Expression::FieldAccess {
                base: Box::new(rumoca_core::Expression::FieldAccess {
                    base: Box::new(rumoca_core::Expression::FieldAccess {
                        base: Box::new(var_ref("zone")),
                        field: "spawn-0".to_string(),
                        span: test_span(),
                    }),
                    field: "4".to_string(),
                    span: test_span(),
                }),
                field: "3-7048a72798".to_string(),
                span: test_span(),
            }),
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    model
        .variable_type_names
        .insert(name.clone(), "String".to_string());

    substitute_known_constants_in_flat(&mut model, &Context::new()).unwrap();

    assert_eq!(
        model.variables.get(&name).and_then(|var| var.start.clone()),
        Some(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::String("spawn-0.4.3-7048a72798".to_string()),
            span: test_span(),
        })
    );
}

#[test]
fn collapse_index_refs_collapses_indexed_field_access_to_known_var() {
    let mut model = flat::Model::new();
    model.add_variable(
        rumoca_core::VarName::new("port_a[1].Q_flow"),
        flat::Variable {
            name: rumoca_core::VarName::new("port_a[1].Q_flow"),
            is_primitive: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    model.add_equation(flat::Equation::new(
        rumoca_core::Expression::FieldAccess {
            base: Box::new(rumoca_core::Expression::Index {
                base: Box::new(rumoca_core::Expression::VarRef {
                    name: rumoca_core::Reference::new("port_a"),
                    subscripts: vec![],
                    span: rumoca_core::Span::DUMMY,
                }),
                subscripts: vec![rumoca_core::Subscript::generated_index(
                    1,
                    rumoca_core::Span::DUMMY,
                )],
                span: rumoca_core::Span::DUMMY,
            }),
            field: "Q_flow".to_string(),
            span: rumoca_core::Span::DUMMY,
        },
        rumoca_core::Span::DUMMY,
        flat::EquationOrigin::ComponentEquation {
            component: "test".to_string(),
        },
    ));

    collapse_index_refs_to_known_varrefs(&mut model);

    assert!(matches!(
        &model.equations[0].residual,
        rumoca_core::Expression::VarRef { name, subscripts, .. }
            if name.as_str() == "port_a[1].Q_flow" && subscripts.is_empty()
    ));
}

#[test]
fn collapse_index_refs_collapses_repeated_record_field_tail_to_known_var() {
    let mut model = flat::Model::new();
    model.add_variable(
        rumoca_core::VarName::new("pipe.flowModel.states[1].phase"),
        flat::Variable {
            name: rumoca_core::VarName::new("pipe.flowModel.states[1].phase"),
            is_primitive: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    let states = rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::new("pipe.flowModel.states"),
        subscripts: vec![rumoca_core::Subscript::generated_index(
            1,
            rumoca_core::Span::DUMMY,
        )],
        span: rumoca_core::Span::DUMMY,
    };
    let phase = rumoca_core::Expression::FieldAccess {
        base: Box::new(states),
        field: "phase".to_string(),
        span: rumoca_core::Span::DUMMY,
    };
    let phase_phase = rumoca_core::Expression::FieldAccess {
        base: Box::new(phase),
        field: "phase".to_string(),
        span: rumoca_core::Span::DUMMY,
    };
    model.add_equation(flat::Equation::new(
        rumoca_core::Expression::FieldAccess {
            base: Box::new(phase_phase),
            field: "phase".to_string(),
            span: rumoca_core::Span::DUMMY,
        },
        rumoca_core::Span::DUMMY,
        flat::EquationOrigin::ComponentEquation {
            component: "test".to_string(),
        },
    ));

    collapse_index_refs_to_known_varrefs(&mut model);
    collapse_index_refs_to_known_varrefs(&mut model);

    assert!(matches!(
        &model.equations[0].residual,
        rumoca_core::Expression::VarRef { name, subscripts, .. }
            if name.as_str() == "pipe.flowModel.states[1].phase" && subscripts.is_empty()
    ));
}

#[test]
fn collapse_index_refs_collapses_rendered_repeated_record_field_tail_to_known_var() {
    let mut model = flat::Model::new();
    model.add_variable(
        rumoca_core::VarName::new("pipe.flowModel.states[1].phase"),
        flat::Variable {
            name: rumoca_core::VarName::new("pipe.flowModel.states[1].phase"),
            is_primitive: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    model.add_equation(flat::Equation::new(
        rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("pipe.flowModel.states[1].phase.phase.phase"),
            subscripts: Vec::new(),
            span: rumoca_core::Span::DUMMY,
        },
        rumoca_core::Span::DUMMY,
        flat::EquationOrigin::ComponentEquation {
            component: "test".to_string(),
        },
    ));

    collapse_index_refs_to_known_varrefs(&mut model);

    assert!(matches!(
        &model.equations[0].residual,
        rumoca_core::Expression::VarRef { name, subscripts, .. }
            if name.as_str() == "pipe.flowModel.states[1].phase" && subscripts.is_empty()
    ));
}

#[test]
fn collapse_index_refs_collapses_repeated_record_field_tail_to_array_field_var() {
    let mut model = flat::Model::new();
    model.add_variable(
        rumoca_core::VarName::new("pipe.flowModel.states.phase[1]"),
        flat::Variable {
            name: rumoca_core::VarName::new("pipe.flowModel.states.phase[1]"),
            is_primitive: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    model.add_equation(flat::Equation::new(
        rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("pipe.flowModel.states[1].phase.phase"),
            subscripts: Vec::new(),
            span: rumoca_core::Span::DUMMY,
        },
        rumoca_core::Span::DUMMY,
        flat::EquationOrigin::ComponentEquation {
            component: "test".to_string(),
        },
    ));

    collapse_index_refs_to_known_varrefs(&mut model);

    assert!(matches!(
        &model.equations[0].residual,
        rumoca_core::Expression::VarRef { name, subscripts, .. }
            if name.as_str() == "pipe.flowModel.states.phase[1]" && subscripts.is_empty()
    ));
}

#[test]
fn collapse_index_refs_collapses_overexpanded_record_sibling_field_to_array_field_var() {
    let mut model = flat::Model::new();
    model.add_variable(
        rumoca_core::VarName::new("pipe.flowModel.states.h[1]"),
        flat::Variable {
            name: rumoca_core::VarName::new("pipe.flowModel.states.h[1]"),
            is_primitive: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    model.add_equation(flat::Equation::new(
        rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("pipe.flowModel.states[1].phase.h"),
            subscripts: Vec::new(),
            span: rumoca_core::Span::DUMMY,
        },
        rumoca_core::Span::DUMMY,
        flat::EquationOrigin::ComponentEquation {
            component: "test".to_string(),
        },
    ));

    collapse_index_refs_to_known_varrefs(&mut model);

    assert!(matches!(
        &model.equations[0].residual,
        rumoca_core::Expression::VarRef { name, subscripts, .. }
            if name.as_str() == "pipe.flowModel.states.h[1]" && subscripts.is_empty()
    ));
}

#[test]
fn collapse_index_refs_collapses_repeated_record_field_tail_to_array_base_ref() {
    let mut model = flat::Model::new();
    let leaf = "pipe.flowModel.states.phase[1]";
    model.add_variable(
        rumoca_core::VarName::new(leaf),
        flat::Variable {
            name: rumoca_core::VarName::new(leaf),
            component_ref: Some(rumoca_core::ComponentReference::from_flat_segments(
                leaf,
                test_span(),
                None,
            )),
            is_primitive: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    model.add_equation(flat::Equation::new(
        rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("pipe.flowModel.states.phase.phase"),
            subscripts: Vec::new(),
            span: rumoca_core::Span::DUMMY,
        },
        rumoca_core::Span::DUMMY,
        flat::EquationOrigin::ComponentEquation {
            component: "test".to_string(),
        },
    ));

    collapse_index_refs_to_known_varrefs(&mut model);

    let rumoca_core::Expression::VarRef {
        name, subscripts, ..
    } = &model.equations[0].residual
    else {
        panic!("expected collapsed VarRef");
    };
    assert_eq!(name.as_str(), "pipe.flowModel.states.phase");
    assert!(name.has_structure());
    assert!(subscripts.is_empty());
}

#[test]
fn collapse_index_refs_collapses_indexed_var_ref_to_known_scalar_var() {
    let mut model = flat::Model::new();
    add_primitive_variable(&mut model, "arr[1]");
    model.add_equation(flat::Equation::new(
        rumoca_core::Expression::Index {
            base: Box::new(rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::new("arr"),
                subscripts: vec![],
                span: rumoca_core::Span::DUMMY,
            }),
            subscripts: vec![rumoca_core::Subscript::generated_index(
                1,
                rumoca_core::Span::DUMMY,
            )],
            span: rumoca_core::Span::DUMMY,
        },
        rumoca_core::Span::DUMMY,
        flat::EquationOrigin::ComponentEquation {
            component: "test".to_string(),
        },
    ));

    collapse_index_refs_to_known_varrefs(&mut model);

    assert!(matches!(
        &model.equations[0].residual,
        rumoca_core::Expression::VarRef { name, subscripts, .. }
            if name.as_str() == "arr[1]" && subscripts.is_empty()
    ));
}

#[test]
fn recover_indexed_lhs_dimensions_does_not_expand_known_symbolic_dimension() {
    let mut model = flat::Model::new();
    let y_name = rumoca_core::VarName::new("aD_Converter.y");
    model.add_variable(
        y_name.clone(),
        flat::Variable {
            name: y_name.clone(),
            dims: vec![7],
            is_primitive: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    model.add_equation(flat::Equation::new(
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::new("aD_Converter.y"),
                subscripts: vec![rumoca_core::Subscript::generated_index(
                    8,
                    rumoca_core::Span::DUMMY,
                )],
                span: rumoca_core::Span::DUMMY,
            }),
            rhs: Box::new(int_literal(0)),
            span: rumoca_core::Span::DUMMY,
        },
        rumoca_core::Span::DUMMY,
        flat::EquationOrigin::ComponentEquation {
            component: "aD_Converter".to_string(),
        },
    ));

    recover_indexed_lhs_dimensions(&mut model);

    assert_eq!(
        model.variables.get(&y_name).expect("y variable").dims,
        vec![7]
    );
}

#[test]
fn substitutes_known_constants_inside_function_defaults_and_body() {
    let mut model = flat::Model::new();
    let mut function = rumoca_core::Function::new("Pkg.f", Span::DUMMY);
    function.add_input(
        rumoca_core::FunctionParam::new("u", "Real", test_span()).with_default(
            rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::new("Pkg.Constants.k"),
                subscripts: vec![],
                span: rumoca_core::Span::DUMMY,
            },
        ),
    );
    function
        .body
        .push(simple_assignment(rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("Pkg.Constants.k"),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        }));
    model.add_function(function);

    let mut ctx = Context::new();
    ctx.constant_values.insert(
        "Pkg.Constants.k".to_string(),
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(42.0),
            span: rumoca_core::Span::DUMMY,
        },
    );

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    let function = model
        .functions
        .get(&rumoca_core::VarName::new("Pkg.f"))
        .expect("function should exist");
    assert!(matches!(
        function.inputs[0].default,
        Some(rumoca_core::Expression::Literal { value: rumoca_core::Literal::Real(v), span: rumoca_core::Span::DUMMY }) if (v - 42.0).abs() < f64::EPSILON
    ));
    match &function.body[0] {
        rumoca_core::Statement::Assignment { value, .. } => assert!(matches!(
            value,
            rumoca_core::Expression::Literal { value: rumoca_core::Literal::Real(v), .. } if (*v - 42.0).abs() < f64::EPSILON
        )),
        other => panic!("expected assignment statement, got {other:?}"),
    }
}

#[test]
fn substitutes_scoped_relative_constant_alias_field() {
    let mut ctx = Context::new();
    ctx.constant_values.insert(
        "medium.steam".to_string(),
        var_ref("Utilities.Water95_Utilities.Constants"),
    );
    ctx.real_parameter_values.insert(
        "medium.Utilities.Water95_Utilities.Constants.R_s".to_string(),
        461.526,
    );

    let substituted = substitute_known_constants_expr(
        var_ref("steam.R_s"),
        &ctx,
        &rustc_hash::FxHashSet::default(),
        &HashSet::new(),
        "medium",
    )
    .unwrap();

    assert!(matches!(
        substituted,
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(v),
            ..
        } if (v - 461.526).abs() < f64::EPSILON
    ));
}

#[test]
fn substitutes_assert_condition_with_component_origin_scope() {
    let mut model = flat::Model::new();
    model.assert_equations.push(flat::AssertEquation::new(
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Gt,
            lhs: Box::new(var_ref("TD")),
            rhs: Box::new(int_literal(0)),
            span: test_span(),
        },
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::String("F or TD has to be positive".to_string()),
            span: test_span(),
        },
        None,
        test_span(),
        flat::EquationOrigin::ComponentEquation {
            component: "line".to_string(),
        },
    ));

    let mut ctx = Context::new();
    ctx.real_parameter_values.insert("TD".to_string(), 0.0);
    ctx.real_parameter_values
        .insert("line.TD".to_string(), 0.001);

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    let condition = &model.assert_equations[0].condition;
    let condition_uses_scoped_td = match condition {
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Boolean(true),
            ..
        } => true,
        rumoca_core::Expression::Binary { lhs, .. } => matches!(
            lhs.as_ref(),
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(value),
                ..
            } if (*value - 0.001).abs() < f64::EPSILON
        ),
        _ => false,
    };
    assert!(
        condition_uses_scoped_td,
        "unexpected condition: {condition:?}"
    );
}

#[test]
fn substituted_assert_condition_prefers_evaluated_scalar_over_stale_default() {
    let mut model = flat::Model::new();
    let condition = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Or,
        lhs: Box::new(rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Gt,
            lhs: Box::new(var_ref("F")),
            rhs: Box::new(int_literal(0)),
            span: test_span(),
        }),
        rhs: Box::new(rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Gt,
            lhs: Box::new(var_ref("TD")),
            rhs: Box::new(int_literal(0)),
            span: test_span(),
        }),
        span: test_span(),
    };
    model.assert_equations.push(flat::AssertEquation::new(
        condition,
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::String("F or TD has to be positive".to_string()),
            span: test_span(),
        },
        None,
        test_span(),
        flat::EquationOrigin::ComponentEquation {
            component: "line".to_string(),
        },
    ));

    let mut ctx = Context::new();
    ctx.constant_values
        .insert("line.F".to_string(), int_literal(0));
    ctx.constant_values
        .insert("line.TD".to_string(), int_literal(0));
    ctx.real_parameter_values.insert("line.F".to_string(), 0.0);
    ctx.real_parameter_values
        .insert("line.TD".to_string(), 0.001);

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    let rumoca_core::Expression::Binary { rhs, .. } = &model.assert_equations[0].condition else {
        panic!(
            "unexpected condition: {:?}",
            model.assert_equations[0].condition
        );
    };
    let rumoca_core::Expression::Binary { lhs, .. } = rhs.as_ref() else {
        panic!("unexpected TD comparison: {rhs:?}");
    };
    assert!(matches!(
        lhs.as_ref(),
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(value),
            ..
        } if (*value - 0.001).abs() < f64::EPSILON
    ));
}

#[test]
fn substitutes_function_scope_constants_inside_defaults_and_body() {
    let mut model = flat::Model::new();
    let mut function = rumoca_core::Function::new("Pkg.f", Span::DUMMY);
    function.add_input(
        rumoca_core::FunctionParam::new("u", "Real", test_span()).with_default(
            rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::new("reference_X"),
                subscripts: vec![],
                span: rumoca_core::Span::DUMMY,
            },
        ),
    );
    function
        .body
        .push(simple_assignment(rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("reference_X"),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        }));
    model.add_function(function);

    let reference_x = rumoca_core::Expression::Array {
        elements: vec![rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(1.0),
            span: rumoca_core::Span::DUMMY,
        }],
        is_matrix: false,
        span: rumoca_core::Span::DUMMY,
    };
    let mut ctx = Context::new();
    ctx.constant_values
        .insert("Pkg.reference_X".to_string(), reference_x.clone());

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    let function = model
        .functions
        .get(&rumoca_core::VarName::new("Pkg.f"))
        .expect("function should exist");
    assert_eq!(function.inputs[0].default, Some(reference_x.clone()));
    match &function.body[0] {
        rumoca_core::Statement::Assignment { value, .. } => {
            assert_eq!(value, &reference_x);
        }
        other => panic!("expected assignment statement, got {other:?}"),
    }
}

#[test]
fn substitutes_record_array_field_projection_from_flat_var_ref() {
    let mut model = flat::Model::new();
    let mut function = rumoca_core::Function::new("Pkg.f", Span::DUMMY);
    function.add_input(
        rumoca_core::FunctionParam::new("u", "Real", test_span())
            .with_default(var_ref("ConcreteMedium.data.MM")),
    );
    model.add_function(function);

    let record = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new("DataRecord"),
        args: vec![rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::new("__rumoca_named_arg__.MM"),
            args: vec![rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(28.0),
                span: rumoca_core::Span::DUMMY,
            }],
            is_constructor: true,
            span: rumoca_core::Span::DUMMY,
        }],
        is_constructor: true,
        span: rumoca_core::Span::DUMMY,
    };
    let mut ctx = Context::new();
    ctx.constant_values.insert(
        "ConcreteMedium.data".to_string(),
        rumoca_core::Expression::Array {
            elements: vec![record],
            is_matrix: false,
            span: rumoca_core::Span::DUMMY,
        },
    );
    ctx.constant_values.insert(
        "ConcreteMedium.data_alias".to_string(),
        var_ref("PartialMedium.data"),
    );

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    let function = model
        .functions
        .get(&rumoca_core::VarName::new("Pkg.f"))
        .expect("function should exist");
    let Some(rumoca_core::Expression::Array { elements, .. }) = &function.inputs[0].default else {
        panic!("expected projected array default");
    };
    assert!(matches!(
        elements.as_slice(),
        [rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(value),
            ..
        }] if (*value - 28.0).abs() < f64::EPSILON
    ));
}

#[test]
fn does_not_substitute_function_local_names() {
    let mut model = flat::Model::new();
    let mut function = rumoca_core::Function::new("Pkg.g", Span::DUMMY);
    function.add_input(rumoca_core::FunctionParam::new("k", "Real", test_span()));
    function
        .body
        .push(simple_assignment(rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("k"),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        }));
    model.add_function(function);

    let mut ctx = Context::new();
    ctx.constant_values.insert(
        "k".to_string(),
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(7.0),
            span: rumoca_core::Span::DUMMY,
        },
    );

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    let function = model
        .functions
        .get(&rumoca_core::VarName::new("Pkg.g"))
        .expect("function should exist");
    match &function.body[0] {
        rumoca_core::Statement::Assignment { value, .. } => assert!(matches!(
            value,
            rumoca_core::Expression::VarRef { name, .. } if name.as_str() == "k"
        )),
        other => panic!("expected assignment statement, got {other:?}"),
    }
}

#[test]
fn does_not_substitute_indexed_function_local_names() {
    let mut model = flat::Model::new();
    let mut function = rumoca_core::Function::new("Pkg.g_indexed", Span::DUMMY);
    function.add_input(
        rumoca_core::FunctionParam::new("table", "Real", test_span()).with_dims(vec![7, 2]),
    );
    function
        .body
        .push(simple_assignment(rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("table"),
            subscripts: vec![
                rumoca_core::Subscript::generated_expr(
                    Box::new(rumoca_core::Expression::VarRef {
                        name: rumoca_core::Reference::new("next"),
                        subscripts: vec![],
                        span: rumoca_core::Span::DUMMY,
                    }),
                    rumoca_core::Span::DUMMY,
                ),
                rumoca_core::Subscript::generated_index(1, rumoca_core::Span::DUMMY),
            ],
            span: rumoca_core::Span::DUMMY,
        }));
    model.add_function(function);

    let mut ctx = Context::new();
    ctx.constant_values.insert(
        "table".to_string(),
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Fill,
            args: vec![
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(0.0),
                    span: rumoca_core::Span::DUMMY,
                },
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(0),
                    span: rumoca_core::Span::DUMMY,
                },
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(2),
                    span: rumoca_core::Span::DUMMY,
                },
            ],
            span: rumoca_core::Span::DUMMY,
        },
    );

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    let function = model
        .functions
        .get(&rumoca_core::VarName::new("Pkg.g_indexed"))
        .expect("function should exist");
    match &function.body[0] {
        rumoca_core::Statement::Assignment { value, .. } => match value {
            rumoca_core::Expression::VarRef {
                name, subscripts, ..
            } => {
                assert_eq!(name.as_str(), "table");
                assert_eq!(subscripts.len(), 2);
            }
            other => panic!("expected table varref, got {other:?}"),
        },
        other => panic!("expected assignment statement, got {other:?}"),
    }
}

#[test]
fn substitutes_inline_multi_indexed_constant_varref_names() {
    let mut model = flat::Model::new();
    let mut function = rumoca_core::Function::new("Pkg.h", Span::DUMMY);
    function
        .body
        .push(simple_assignment(rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("Modelica.Blocks.Sources.IntegerTable.table[1,1]"),
            subscripts: vec![],
            span: test_span(),
        }));
    model.add_function(function);

    let mut ctx = Context::new();
    ctx.constant_values.insert(
        "Modelica.Blocks.Sources.IntegerTable.table".to_string(),
        rumoca_core::Expression::Array {
            elements: vec![
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(0),
                    span: rumoca_core::Span::DUMMY,
                },
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(1),
                    span: rumoca_core::Span::DUMMY,
                },
            ],
            is_matrix: false,
            span: rumoca_core::Span::DUMMY,
        },
    );

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    let function = model
        .functions
        .get(&rumoca_core::VarName::new("Pkg.h"))
        .expect("function should exist");
    match &function.body[0] {
        rumoca_core::Statement::Assignment { value, .. } => match value {
            rumoca_core::Expression::Index {
                base, subscripts, ..
            } => {
                assert!(matches!(
                    base.as_ref(),
                    rumoca_core::Expression::Array { elements, is_matrix, .. }
                        if !*is_matrix && elements.len() == 2
                ));
                assert_eq!(subscripts.len(), 2);
                assert!(matches!(
                    &subscripts[0],
                    rumoca_core::Subscript::Expr { expr, .. }
                        if matches!(expr.as_ref(), rumoca_core::Expression::Literal { value: rumoca_core::Literal::Integer(1), .. })
                ));
                assert!(matches!(
                    &subscripts[1],
                    rumoca_core::Subscript::Expr { expr, .. }
                        if matches!(expr.as_ref(), rumoca_core::Expression::Literal { value: rumoca_core::Literal::Integer(1), .. })
                ));
            }
            other => panic!("expected indexed expression, got {other:?}"),
        },
        other => panic!("expected assignment statement, got {other:?}"),
    }
}

#[test]
fn rejects_unspanned_inline_indexed_constant_varref_names() {
    let mut model = flat::Model::new();
    let mut function = rumoca_core::Function::new("Pkg.unspanned_inline", Span::DUMMY);
    function
        .body
        .push(simple_assignment(rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("Pkg.table[1]"),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        }));
    model.add_function(function);

    let mut ctx = Context::new();
    ctx.constant_values.insert(
        "Pkg.table".to_string(),
        rumoca_core::Expression::Array {
            elements: vec![int_literal(1)],
            is_matrix: false,
            span: rumoca_core::Span::DUMMY,
        },
    );

    match substitute_known_constants_in_flat(&mut model, &ctx) {
        Err(FlattenError::MissingSourceContext { reason }) => assert!(
            reason.contains("flatten inline indexed constant"),
            "unexpected reason: {reason}"
        ),
        other => panic!("expected missing-source-context error, got {other:?}"),
    }
}

#[test]
fn inline_indexed_name_uses_structured_scalar_name_parser() {
    assert_eq!(
        split_inline_indexed_name("table[1, 2]"),
        Some(("table", vec![1, 2]))
    );
    assert_eq!(
        split_inline_indexed_name("pkg.table[index.with.dot].value[3]"),
        Some(("pkg.table[index.with.dot].value", vec![3]))
    );
    assert!(split_inline_indexed_name("table").is_none());
    assert!(split_inline_indexed_name("table[1").is_none());
    assert!(split_inline_indexed_name("[1]").is_none());
    assert!(split_inline_indexed_name("table[index.with.dot]").is_none());
}

#[test]
fn does_not_substitute_inline_indexed_varref_when_base_is_local() {
    let mut model = flat::Model::new();
    let mut function = rumoca_core::Function::new("Pkg.inline_local", Span::DUMMY);
    function.add_input(
        rumoca_core::FunctionParam::new("table", "Real", test_span()).with_dims(vec![7, 2]),
    );
    function
        .body
        .push(simple_assignment(rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("table[1,1]"),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        }));
    model.add_function(function);

    let mut ctx = Context::new();
    ctx.constant_values.insert(
        "table".to_string(),
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Fill,
            args: vec![
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(0.0),
                    span: rumoca_core::Span::DUMMY,
                },
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(0),
                    span: rumoca_core::Span::DUMMY,
                },
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(2),
                    span: rumoca_core::Span::DUMMY,
                },
            ],
            span: rumoca_core::Span::DUMMY,
        },
    );

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    let function = model
        .functions
        .get(&rumoca_core::VarName::new("Pkg.inline_local"))
        .expect("function should exist");
    match &function.body[0] {
        rumoca_core::Statement::Assignment { value, .. } => assert!(matches!(
            value,
            rumoca_core::Expression::VarRef { name, subscripts, .. }
                if name.as_str() == "table[1,1]" && subscripts.is_empty()
        )),
        other => panic!("expected assignment statement, got {other:?}"),
    }
}

#[test]
fn substitutes_variable_attribute_constants_in_variable_scope() {
    let mut model = flat::Model::new();
    add_primitive_variable(&mut model, "tank.medium.X");
    model
        .variables
        .get_mut(&rumoca_core::VarName::new("tank.medium.X"))
        .expect("variable should exist")
        .start = Some(var_ref("reference_X"));

    let mut ctx = Context::new();
    ctx.constant_values.insert(
        "tank.medium.reference_X".to_string(),
        reference_x_fill_expr(),
    );
    ctx.parameter_values.insert("tank.medium.nS".to_string(), 1);

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    let start = model
        .variables
        .get(&rumoca_core::VarName::new("tank.medium.X"))
        .expect("variable should exist")
        .start
        .as_ref()
        .expect("start attribute should remain");
    assert!(!expr_contains_var_ref(start, "reference_X"));
    assert!(!expr_contains_var_ref(start, "nS"));
}

#[test]
fn substitutes_component_equation_constants_in_origin_scope() {
    let mut model = flat::Model::new();
    add_primitive_variable(&mut model, "tank.medium.X");
    model.add_equation(flat::Equation::new(
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(var_ref("tank.medium.X")),
            rhs: Box::new(var_ref("reference_X")),
            span: rumoca_core::Span::DUMMY,
        },
        rumoca_core::Span::DUMMY,
        flat::EquationOrigin::ComponentEquation {
            component: "tank.medium".to_string(),
        },
    ));

    let mut ctx = Context::new();
    ctx.constant_values.insert(
        "tank.medium.reference_X".to_string(),
        reference_x_fill_expr(),
    );
    ctx.parameter_values.insert("tank.medium.nS".to_string(), 1);

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    let residual = &model.equations[0].residual;
    assert!(!expr_contains_var_ref(residual, "reference_X"));
    assert!(!expr_contains_var_ref(residual, "nS"));
}

#[test]
fn substitutes_fully_qualified_constant_alias_in_declaration_scope() {
    let mut model = flat::Model::new();
    add_primitive_variable(&mut model, "tank.X_start");
    model
        .variables
        .get_mut(&rumoca_core::VarName::new("tank.X_start"))
        .expect("variable should exist")
        .start = Some(var_ref("Pkg.Medium.X_default"));

    let mut ctx = Context::new();
    ctx.constant_values
        .insert("Pkg.Medium.X_default".to_string(), var_ref("reference_X"));
    ctx.constant_values.insert(
        "Pkg.Medium.reference_X".to_string(),
        reference_x_fill_expr(),
    );
    ctx.parameter_values.insert("Pkg.Medium.nS".to_string(), 1);

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    let start = model
        .variables
        .get(&rumoca_core::VarName::new("tank.X_start"))
        .expect("variable should exist")
        .start
        .as_ref()
        .expect("start attribute should remain");
    assert!(!expr_contains_var_ref(start, "Pkg.Medium.X_default"));
    assert!(!expr_contains_var_ref(start, "reference_X"));
    assert!(!expr_contains_var_ref(start, "nS"));
}

#[test]
fn scoped_relative_alias_keeps_live_flat_variable_before_constant_expansion() {
    let mut model = flat::Model::new();
    add_primitive_variable(&mut model, "jointRRP.e_ia");
    add_primitive_variable(&mut model, "jointRRP.jointUSP.e2_ia");
    add_primitive_variable(&mut model, "jointRRP.jointUSP.rod1.e2_ia");
    model
        .variables
        .get_mut(&rumoca_core::VarName::new("jointRRP.e_ia"))
        .expect("variable should exist")
        .binding = Some(var_ref("jointUSP.e2_ia"));

    let mut ctx = Context::new();
    ctx.constant_values
        .insert("jointRRP.jointUSP.e2_ia".to_string(), var_ref("rod1.e2_ia"));
    ctx.constant_values.insert(
        "jointRRP.rod1.e2_ia".to_string(),
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(1.0),
            span: rumoca_core::Span::DUMMY,
        },
    );

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    let binding = model
        .variables
        .get(&rumoca_core::VarName::new("jointRRP.e_ia"))
        .expect("variable should exist")
        .binding
        .as_ref()
        .expect("binding should remain");
    assert!(matches!(
        binding,
        rumoca_core::Expression::VarRef { name, .. }
            if name.as_str() == "jointRRP.jointUSP.e2_ia"
    ));
}

#[test]
fn does_not_substitute_array_shaped_scalar_parameter_ref() {
    let mut model = flat::Model::new();
    model.equations.push(flat::Equation::new(
        spanned_var_ref("CriticalDamping.c0"),
        Span::DUMMY,
        flat::EquationOrigin::ComponentEquation {
            component: "CriticalDamping".to_string(),
        },
    ));

    let mut ctx = Context::new();
    ctx.array_dimensions
        .insert("CriticalDamping.c0".to_string(), vec![0]);
    ctx.real_parameter_values
        .insert("CriticalDamping.c0".to_string(), 0.0);

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    assert!(matches!(
        model.equations[0].residual,
        rumoca_core::Expression::VarRef { ref name, ref subscripts, .. }
            if name.as_str() == "CriticalDamping.c0" && subscripts.is_empty()
    ));
}

#[test]
fn does_not_substitute_scoped_zero_length_array_parameter_ref() {
    let mut model = flat::Model::new();
    model.equations.push(flat::Equation::new(
        var_ref("c0"),
        Span::DUMMY,
        flat::EquationOrigin::ComponentEquation {
            component: "CriticalDamping".to_string(),
        },
    ));

    let mut ctx = Context::new();
    ctx.array_dimensions
        .insert("CriticalDamping.c0".to_string(), vec![0]);
    ctx.real_parameter_values
        .insert("CriticalDamping.c0".to_string(), 0.0);

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    assert!(matches!(
        model.equations[0].residual,
        rumoca_core::Expression::VarRef { ref name, ref subscripts, .. }
            if name.as_str() == "c0" && subscripts.is_empty()
    ));
}

#[test]
fn does_not_substitute_array_shaped_scalar_constant_expr() {
    let mut model = flat::Model::new();
    model.equations.push(flat::Equation::new(
        var_ref("c0"),
        Span::DUMMY,
        flat::EquationOrigin::ComponentEquation {
            component: "CriticalDamping".to_string(),
        },
    ));

    let mut ctx = Context::new();
    ctx.array_dimensions
        .insert("CriticalDamping.c0".to_string(), vec![0]);
    ctx.constant_values.insert(
        "CriticalDamping.c0".to_string(),
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(0.0),
            span: rumoca_core::Span::DUMMY,
        },
    );

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    assert!(matches!(
        model.equations[0].residual,
        rumoca_core::Expression::VarRef { ref name, ref subscripts, .. }
            if name.as_str() == "c0" && subscripts.is_empty()
    ));
}

#[test]
fn materializes_referenced_zero_sized_array_declaration() {
    let mut model = flat::Model::new();
    model.equations.push(flat::Equation::new(
        spanned_var_ref("CriticalDamping.c0"),
        Span::DUMMY,
        flat::EquationOrigin::ComponentEquation {
            component: "CriticalDamping".to_string(),
        },
    ));

    let mut ctx = Context::new();
    ctx.array_dimensions
        .insert("CriticalDamping.c0".to_string(), vec![0]);

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    let var = model
        .variables
        .get(&rumoca_core::VarName::new("CriticalDamping.c0"))
        .expect("zero-sized referenced array should have a Flat declaration");
    assert_eq!(var.dims, vec![0]);
    assert!(var.is_primitive);
}

#[test]
fn materializes_unspanned_zero_sized_array_reference_from_dimension_provenance() {
    let mut model = flat::Model::new();
    model.equations.push(flat::Equation::new(
        var_ref("Modelica.Media.Water.WaterIF97_base.C_default"),
        Span::DUMMY,
        flat::EquationOrigin::ComponentEquation {
            component: "PumpingSystem".to_string(),
        },
    ));

    let mut ctx = Context::new();
    let name = "Modelica.Media.Water.WaterIF97_base.C_default";
    ctx.array_dimensions.insert(name.to_string(), vec![0]);
    ctx.array_dimension_spans
        .insert(name.to_string(), test_span());

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    let var = model
        .variables
        .get(&rumoca_core::VarName::new(name))
        .expect("zero-sized referenced array should have a Flat declaration");
    assert_eq!(var.dims, vec![0]);
    assert_eq!(var.source_span, test_span());
    assert!(
        var.component_ref.is_some(),
        "materialized zero-sized array variable should carry structured metadata"
    );
}

#[test]
fn substitutes_field_access_on_zero_arg_constructor_constants() {
    let mut model = flat::Model::new();
    let mut function = rumoca_core::Function::new("Pkg.k", Span::DUMMY);
    function
        .body
        .push(simple_assignment(rumoca_core::Expression::FieldAccess {
            base: Box::new(rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new(
                    "Modelica.Electrical.Batteries.ParameterRecords.ExampleData",
                ),
                args: vec![],
                is_constructor: true,
                span: rumoca_core::Span::DUMMY,
            }),
            field: "useLinearSOCDependency".to_string(),
            span: rumoca_core::Span::DUMMY,
        }));
    model.add_function(function);

    let mut ctx = Context::new();
    ctx.boolean_parameter_values.insert(
        "Modelica.Electrical.Batteries.ParameterRecords.ExampleData.useLinearSOCDependency"
            .to_string(),
        false,
    );

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    let function = model
        .functions
        .get(&rumoca_core::VarName::new("Pkg.k"))
        .expect("function should exist");
    match &function.body[0] {
        rumoca_core::Statement::Assignment { value, .. } => assert!(matches!(
            value,
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Boolean(false),
                span: rumoca_core::Span::DUMMY
            }
        )),
        other => panic!("expected assignment statement, got {other:?}"),
    }
}

#[test]
fn does_not_resolve_function_local_record_root_through_constant_alias() {
    let mut model = flat::Model::new();
    let mut function = rumoca_core::Function::new("Pkg.f", Span::DUMMY);
    function.add_output(rumoca_core::FunctionParam::new(
        "g",
        "Common.GibbsDerivs",
        test_span(),
    ));
    function.body.push(simple_assignment(var_ref("g.tau")));
    model.add_function(function);

    let mut ctx = Context::new();
    ctx.constant_values
        .insert("g".to_string(), var_ref("Modelica.Constants.g_n"));

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    let function = model
        .functions
        .get(&rumoca_core::VarName::new("Pkg.f"))
        .expect("function should exist");
    match &function.body[0] {
        rumoca_core::Statement::Assignment { value, .. } => assert!(matches!(
            value,
            rumoca_core::Expression::VarRef { name, .. } if name.as_str() == "g.tau"
        )),
        other => panic!("expected assignment statement, got {other:?}"),
    }
}

fn expr_contains_var_ref(expr: &rumoca_core::Expression, needle: &str) -> bool {
    match expr {
        rumoca_core::Expression::VarRef { name, .. } => name.as_str() == needle,
        rumoca_core::Expression::Binary { lhs, rhs, .. } => {
            expr_contains_var_ref(lhs, needle) || expr_contains_var_ref(rhs, needle)
        }
        rumoca_core::Expression::Unary { rhs, .. } => expr_contains_var_ref(rhs, needle),
        rumoca_core::Expression::BuiltinCall { args, .. }
        | rumoca_core::Expression::FunctionCall { args, .. }
        | rumoca_core::Expression::Array { elements: args, .. }
        | rumoca_core::Expression::Tuple { elements: args, .. } => {
            args.iter().any(|arg| expr_contains_var_ref(arg, needle))
        }
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            branches.iter().any(|(condition, value)| {
                expr_contains_var_ref(condition, needle) || expr_contains_var_ref(value, needle)
            }) || expr_contains_var_ref(else_branch, needle)
        }
        rumoca_core::Expression::Range {
            start, step, end, ..
        } => {
            expr_contains_var_ref(start, needle)
                || step
                    .as_ref()
                    .is_some_and(|step| expr_contains_var_ref(step, needle))
                || expr_contains_var_ref(end, needle)
        }
        rumoca_core::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => {
            expr_contains_var_ref(expr, needle)
                || indices
                    .iter()
                    .any(|index| expr_contains_var_ref(&index.range, needle))
                || filter
                    .as_ref()
                    .is_some_and(|filter| expr_contains_var_ref(filter, needle))
        }
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            expr_contains_var_ref(base, needle)
                || subscripts.iter().any(|subscript| match subscript {
                    rumoca_core::Subscript::Expr { expr, .. } => {
                        expr_contains_var_ref(expr, needle)
                    }
                    rumoca_core::Subscript::Index { .. } | rumoca_core::Subscript::Colon { .. } => {
                        false
                    }
                })
        }
        rumoca_core::Expression::FieldAccess { base, .. } => expr_contains_var_ref(base, needle),
        rumoca_core::Expression::Literal { .. } | rumoca_core::Expression::Empty { .. } => false,
    }
}
