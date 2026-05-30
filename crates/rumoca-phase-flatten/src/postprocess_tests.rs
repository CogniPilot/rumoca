use super::*;
use rumoca_core::Span;

fn simple_assignment(value: flat::Expression) -> flat::Statement {
    flat::Statement::Assignment {
        comp: flat::ComponentReference {
            local: false,
            parts: vec![flat::ComponentRefPart {
                ident: "y".to_string(),
                subs: vec![],
            }],
            def_id: None,
        },
        value,
    }
}

#[test]
fn canonicalize_varrefs_rewrites_alias_paths_to_existing_flat_vars() {
    let mut model = flat::Model::new();
    model.add_variable(
        flat::VarName::new("pipe.port_a.p"),
        flat::Variable {
            name: flat::VarName::new("pipe.port_a.p"),
            is_primitive: true,
            ..Default::default()
        },
    );
    model.equations.push(flat::Equation::new(
        flat::Expression::VarRef {
            name: flat::VarName::new("pipe.flowModel.port_a.p"),
            subscripts: vec![],
        },
        Span::DUMMY,
        flat::EquationOrigin::ComponentEquation {
            component: "pipe".to_string(),
        },
    ));

    let mut ctx = Context::new();
    ctx.record_aliases.insert(
        rumoca_core::ComponentPath::from_flat_path("pipe.flowModel"),
        rumoca_core::ComponentPath::from_flat_path("pipe"),
    );

    canonicalize_varrefs_via_record_aliases(&mut model, &ctx);

    let flat::Expression::VarRef { name, .. } = &model.equations[0].residual else {
        panic!("expected varref");
    };
    assert_eq!(name.as_str(), "pipe.port_a.p");
}

#[test]
fn substitutes_known_constants_inside_function_defaults_and_body() {
    let mut model = flat::Model::new();
    let mut function = flat::Function::new("Pkg.f", Span::DUMMY);
    function.add_input(flat::FunctionParam::new("u", "Real").with_default(
        flat::Expression::VarRef {
            name: flat::VarName::new("Pkg.Constants.k"),
            subscripts: vec![],
        },
    ));
    function
        .body
        .push(simple_assignment(flat::Expression::VarRef {
            name: flat::VarName::new("Pkg.Constants.k"),
            subscripts: vec![],
        }));
    model.add_function(function);

    let mut ctx = Context::new();
    ctx.constant_values.insert(
        "Pkg.Constants.k".to_string(),
        flat::Expression::Literal(flat::Literal::Real(42.0)),
    );

    substitute_known_constants_in_flat(&mut model, &ctx);

    let function = model
        .functions
        .get(&flat::VarName::new("Pkg.f"))
        .expect("function should exist");
    assert!(matches!(
        function.inputs[0].default,
        Some(flat::Expression::Literal(flat::Literal::Real(v))) if (v - 42.0).abs() < f64::EPSILON
    ));
    match &function.body[0] {
        flat::Statement::Assignment { value, .. } => assert!(matches!(
            value,
            flat::Expression::Literal(flat::Literal::Real(v)) if (*v - 42.0).abs() < f64::EPSILON
        )),
        other => panic!("expected assignment statement, got {other:?}"),
    }
}

#[test]
fn does_not_substitute_function_local_names() {
    let mut model = flat::Model::new();
    let mut function = flat::Function::new("Pkg.g", Span::DUMMY);
    function.add_input(flat::FunctionParam::new("k", "Real"));
    function
        .body
        .push(simple_assignment(flat::Expression::VarRef {
            name: flat::VarName::new("k"),
            subscripts: vec![],
        }));
    model.add_function(function);

    let mut ctx = Context::new();
    ctx.constant_values.insert(
        "k".to_string(),
        flat::Expression::Literal(flat::Literal::Real(7.0)),
    );

    substitute_known_constants_in_flat(&mut model, &ctx);

    let function = model
        .functions
        .get(&flat::VarName::new("Pkg.g"))
        .expect("function should exist");
    match &function.body[0] {
        flat::Statement::Assignment { value, .. } => assert!(matches!(
            value,
            flat::Expression::VarRef { name, .. } if name.as_str() == "k"
        )),
        other => panic!("expected assignment statement, got {other:?}"),
    }
}

#[test]
fn does_not_substitute_indexed_function_local_names() {
    let mut model = flat::Model::new();
    let mut function = flat::Function::new("Pkg.g_indexed", Span::DUMMY);
    function.add_input(flat::FunctionParam::new("table", "Real").with_dims(vec![7, 2]));
    function
        .body
        .push(simple_assignment(flat::Expression::VarRef {
            name: flat::VarName::new("table"),
            subscripts: vec![
                flat::Subscript::Expr(Box::new(flat::Expression::VarRef {
                    name: flat::VarName::new("next"),
                    subscripts: vec![],
                })),
                flat::Subscript::Index(1),
            ],
        }));
    model.add_function(function);

    let mut ctx = Context::new();
    ctx.constant_values.insert(
        "table".to_string(),
        flat::Expression::BuiltinCall {
            function: flat::BuiltinFunction::Fill,
            args: vec![
                flat::Expression::Literal(flat::Literal::Real(0.0)),
                flat::Expression::Literal(flat::Literal::Integer(0)),
                flat::Expression::Literal(flat::Literal::Integer(2)),
            ],
        },
    );

    substitute_known_constants_in_flat(&mut model, &ctx);

    let function = model
        .functions
        .get(&flat::VarName::new("Pkg.g_indexed"))
        .expect("function should exist");
    match &function.body[0] {
        flat::Statement::Assignment { value, .. } => match value {
            flat::Expression::VarRef { name, subscripts } => {
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
    let mut function = flat::Function::new("Pkg.h", Span::DUMMY);
    function
        .body
        .push(simple_assignment(flat::Expression::VarRef {
            name: flat::VarName::new("Modelica.Blocks.Sources.IntegerTable.table[1,1]"),
            subscripts: vec![],
        }));
    model.add_function(function);

    let mut ctx = Context::new();
    ctx.constant_values.insert(
        "Modelica.Blocks.Sources.IntegerTable.table".to_string(),
        flat::Expression::Array {
            elements: vec![
                flat::Expression::Literal(flat::Literal::Integer(0)),
                flat::Expression::Literal(flat::Literal::Integer(1)),
            ],
            is_matrix: false,
        },
    );

    substitute_known_constants_in_flat(&mut model, &ctx);

    let function = model
        .functions
        .get(&flat::VarName::new("Pkg.h"))
        .expect("function should exist");
    match &function.body[0] {
        flat::Statement::Assignment { value, .. } => match value {
            flat::Expression::Index { base, subscripts } => {
                assert!(matches!(
                    base.as_ref(),
                    flat::Expression::Array { elements, is_matrix }
                        if !*is_matrix && elements.len() == 2
                ));
                assert_eq!(subscripts.len(), 2);
                assert!(matches!(
                    &subscripts[0],
                    flat::Subscript::Expr(expr)
                        if matches!(expr.as_ref(), flat::Expression::Literal(flat::Literal::Integer(1)))
                ));
                assert!(matches!(
                    &subscripts[1],
                    flat::Subscript::Expr(expr)
                        if matches!(expr.as_ref(), flat::Expression::Literal(flat::Literal::Integer(1)))
                ));
            }
            other => panic!("expected indexed expression, got {other:?}"),
        },
        other => panic!("expected assignment statement, got {other:?}"),
    }
}

#[test]
fn does_not_substitute_inline_indexed_varref_when_base_is_local() {
    let mut model = flat::Model::new();
    let mut function = flat::Function::new("Pkg.inline_local", Span::DUMMY);
    function.add_input(flat::FunctionParam::new("table", "Real").with_dims(vec![7, 2]));
    function
        .body
        .push(simple_assignment(flat::Expression::VarRef {
            name: flat::VarName::new("table[1,1]"),
            subscripts: vec![],
        }));
    model.add_function(function);

    let mut ctx = Context::new();
    ctx.constant_values.insert(
        "table".to_string(),
        flat::Expression::BuiltinCall {
            function: flat::BuiltinFunction::Fill,
            args: vec![
                flat::Expression::Literal(flat::Literal::Real(0.0)),
                flat::Expression::Literal(flat::Literal::Integer(0)),
                flat::Expression::Literal(flat::Literal::Integer(2)),
            ],
        },
    );

    substitute_known_constants_in_flat(&mut model, &ctx);

    let function = model
        .functions
        .get(&flat::VarName::new("Pkg.inline_local"))
        .expect("function should exist");
    match &function.body[0] {
        flat::Statement::Assignment { value, .. } => assert!(matches!(
            value,
            flat::Expression::VarRef { name, subscripts }
                if name.as_str() == "table[1,1]" && subscripts.is_empty()
        )),
        other => panic!("expected assignment statement, got {other:?}"),
    }
}

#[test]
fn substitutes_unqualified_symbol_from_unique_scoped_constant_suffix() {
    let mut model = flat::Model::new();
    model.equations.push(flat::Equation::new(
        flat::Expression::VarRef {
            name: flat::VarName::new("reference_X"),
            subscripts: vec![],
        },
        Span::DUMMY,
        flat::EquationOrigin::ComponentEquation {
            component: "tank.medium".to_string(),
        },
    ));

    let mut ctx = Context::new();
    ctx.constant_values.insert(
        "tank.medium.reference_X".to_string(),
        flat::Expression::Array {
            elements: vec![flat::Expression::Literal(flat::Literal::Real(1.0))],
            is_matrix: false,
        },
    );

    substitute_known_constants_in_flat(&mut model, &ctx);
    assert!(matches!(
        model.equations[0].residual,
        flat::Expression::Array { ref elements, is_matrix }
            if !is_matrix
                && elements.len() == 1
                && matches!(elements[0], flat::Expression::Literal(flat::Literal::Real(v)) if (v - 1.0).abs() < f64::EPSILON)
    ));
}

#[test]
fn does_not_substitute_unqualified_symbol_when_scoped_suffix_is_ambiguous() {
    let mut model = flat::Model::new();
    model.equations.push(flat::Equation::new(
        flat::Expression::VarRef {
            name: flat::VarName::new("reference_X"),
            subscripts: vec![],
        },
        Span::DUMMY,
        flat::EquationOrigin::ComponentEquation {
            component: "tank.medium".to_string(),
        },
    ));

    let mut ctx = Context::new();
    ctx.constant_values.insert(
        "a.medium.reference_X".to_string(),
        flat::Expression::Array {
            elements: vec![flat::Expression::Literal(flat::Literal::Real(1.0))],
            is_matrix: false,
        },
    );
    ctx.constant_values.insert(
        "b.medium.reference_X".to_string(),
        flat::Expression::Array {
            elements: vec![flat::Expression::Literal(flat::Literal::Real(2.0))],
            is_matrix: false,
        },
    );

    substitute_known_constants_in_flat(&mut model, &ctx);
    assert!(matches!(
        model.equations[0].residual,
        flat::Expression::VarRef { ref name, ref subscripts }
            if name.as_str() == "reference_X" && subscripts.is_empty()
    ));
}

#[test]
fn substitutes_case_variant_symbol_when_unique_context_key_exists() {
    let mut model = flat::Model::new();
    model.equations.push(flat::Equation::new(
        flat::Expression::VarRef {
            name: flat::VarName::new("tank.medium.cv_const"),
            subscripts: vec![],
        },
        Span::DUMMY,
        flat::EquationOrigin::ComponentEquation {
            component: "tank.medium".to_string(),
        },
    ));

    let mut ctx = Context::new();
    ctx.real_parameter_values
        .insert("tank.Medium.cv_const".to_string(), 4184.0);

    substitute_known_constants_in_flat(&mut model, &ctx);
    assert!(matches!(
        model.equations[0].residual,
        flat::Expression::Literal(flat::Literal::Real(v)) if (v - 4184.0).abs() < f64::EPSILON
    ));
}

#[test]
fn does_not_substitute_case_variant_symbol_when_ambiguous() {
    let mut model = flat::Model::new();
    model.equations.push(flat::Equation::new(
        flat::Expression::VarRef {
            name: flat::VarName::new("tank.medium.cv_const"),
            subscripts: vec![],
        },
        Span::DUMMY,
        flat::EquationOrigin::ComponentEquation {
            component: "tank.medium".to_string(),
        },
    ));

    let mut ctx = Context::new();
    ctx.real_parameter_values
        .insert("tank.Medium.cv_const".to_string(), 4184.0);
    ctx.real_parameter_values
        .insert("tank.medium.Cv_const".to_string(), 1234.0);

    substitute_known_constants_in_flat(&mut model, &ctx);
    assert!(matches!(
        model.equations[0].residual,
        flat::Expression::VarRef { ref name, ref subscripts }
            if name.as_str() == "tank.medium.cv_const" && subscripts.is_empty()
    ));
}

#[test]
fn substitutes_field_access_on_zero_arg_constructor_constants() {
    let mut model = flat::Model::new();
    let mut function = flat::Function::new("Pkg.k", Span::DUMMY);
    function
        .body
        .push(simple_assignment(flat::Expression::FieldAccess {
            base: Box::new(flat::Expression::FunctionCall {
                name: flat::VarName::new(
                    "Modelica.Electrical.Batteries.ParameterRecords.ExampleData",
                ),
                args: vec![],
                is_constructor: true,
            }),
            field: "useLinearSOCDependency".to_string(),
        }));
    model.add_function(function);

    let mut ctx = Context::new();
    ctx.boolean_parameter_values.insert(
        "Modelica.Electrical.Batteries.ParameterRecords.ExampleData.useLinearSOCDependency"
            .to_string(),
        false,
    );

    substitute_known_constants_in_flat(&mut model, &ctx);

    let function = model
        .functions
        .get(&flat::VarName::new("Pkg.k"))
        .expect("function should exist");
    match &function.body[0] {
        flat::Statement::Assignment { value, .. } => assert!(matches!(
            value,
            flat::Expression::Literal(flat::Literal::Boolean(false))
        )),
        other => panic!("expected assignment statement, got {other:?}"),
    }
}
