use super::*;
use rumoca_core::Span;

fn test_span() -> Span {
    Span::from_offsets(
        rumoca_core::SourceId::from_source_name("functions_tests.mo"),
        1,
        2,
    )
}

#[test]
fn test_collect_no_function_calls() {
    let flat = flat::Model::new();
    let calls = collect_function_calls(&flat);
    assert!(calls.is_empty());
}

#[test]
fn test_collect_function_call_in_equation() {
    let mut flat = flat::Model::new();

    let func_call = flat::Expression::FunctionCall {
        name: flat::VarName::new("MyPackage.myFunc"),
        args: vec![flat::Expression::VarRef {
            name: flat::VarName::new("x"),
            subscripts: vec![],
        }],
        is_constructor: false,
    };
    let residual = flat::Expression::Binary {
        op: rumoca_ir_flat::OpBinary::Sub(rumoca_ir_flat::Token::default()),
        lhs: Box::new(func_call),
        rhs: Box::new(flat::Expression::VarRef {
            name: flat::VarName::new("y"),
            subscripts: vec![],
        }),
    };
    flat.add_equation(flat::Equation::new(
        residual,
        Span::DUMMY,
        rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "test".to_string(),
        },
    ));

    let calls = collect_function_calls(&flat);
    assert!(calls.contains("MyPackage.myFunc"));
    assert_eq!(calls.len(), 1);
}

#[test]
fn test_collect_nested_function_calls() {
    let mut flat = flat::Model::new();

    let inner_call = flat::Expression::FunctionCall {
        name: flat::VarName::new("inner"),
        args: vec![flat::Expression::VarRef {
            name: flat::VarName::new("x"),
            subscripts: vec![],
        }],
        is_constructor: false,
    };
    let outer_call = flat::Expression::FunctionCall {
        name: flat::VarName::new("outer"),
        args: vec![inner_call],
        is_constructor: false,
    };
    let residual = flat::Expression::Binary {
        op: rumoca_ir_flat::OpBinary::Sub(rumoca_ir_flat::Token::default()),
        lhs: Box::new(outer_call),
        rhs: Box::new(flat::Expression::VarRef {
            name: flat::VarName::new("y"),
            subscripts: vec![],
        }),
    };
    flat.add_equation(flat::Equation::new(
        residual,
        Span::DUMMY,
        rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "test".to_string(),
        },
    ));

    let calls = collect_function_calls(&flat);
    assert!(calls.contains("inner"));
    assert!(calls.contains("outer"));
    assert_eq!(calls.len(), 2);
}

#[test]
fn test_convert_component_to_param_prefers_binding_over_start_default() {
    let component = ast::Component {
        type_name: ast::Name::from_string("Real"),
        has_explicit_binding: true,
        start: ast::Expression::Terminal {
            terminal_type: rumoca_ir_ast::TerminalType::UnsignedInteger,
            token: rumoca_ir_core::Token {
                text: "0".into(),
                ..Default::default()
            },
        },
        binding: Some(ast::Expression::Terminal {
            terminal_type: rumoca_ir_ast::TerminalType::UnsignedInteger,
            token: rumoca_ir_core::Token {
                text: "3".into(),
                ..Default::default()
            },
        }),
        ..Default::default()
    };

    let def_map = indexmap::IndexMap::new();
    let param = convert_component_to_param(
        "m",
        &component,
        &def_map,
        &qualify::ImportMap::default(),
        &HashSet::new(),
    );
    assert!(matches!(
        param.default,
        Some(flat::Expression::Literal(flat::Literal::Integer(3)))
    ));
}

#[test]
fn test_extract_derivative_annotation_simple() {
    use rumoca_ir_ast::{ComponentRefPart, ComponentReference, Token};
    use std::sync::Arc;

    let annotations = vec![ast::Expression::NamedArgument {
        name: Token {
            text: Arc::from("derivative"),
            ..Default::default()
        },
        value: Arc::new(ast::Expression::ComponentReference(ComponentReference {
            local: false,
            def_id: None,
            parts: vec![ComponentRefPart {
                ident: Token {
                    text: Arc::from("myFunc_der"),
                    ..Default::default()
                },
                subs: None,
            }],
        })),
    }];

    let derivs = extract_derivative_annotations(&annotations);
    assert_eq!(derivs.len(), 1);
    assert_eq!(derivs[0].derivative_function, "myFunc_der");
    assert_eq!(derivs[0].order, 1);
    assert!(derivs[0].zero_derivative.is_empty());
    assert!(derivs[0].no_derivative.is_empty());
}

#[test]
fn test_extract_derivative_annotation_with_modification() {
    use rumoca_ir_ast::{ComponentRefPart, ComponentReference, Token};
    use std::sync::Arc;

    let annotations = vec![ast::Expression::Modification {
        target: ComponentReference {
            local: false,
            def_id: None,
            parts: vec![ComponentRefPart {
                ident: Token {
                    text: Arc::from("derivative"),
                    ..Default::default()
                },
                subs: Some(vec![ast::Subscript::Expression(
                    ast::Expression::NamedArgument {
                        name: Token {
                            text: Arc::from("order"),
                            ..Default::default()
                        },
                        value: Arc::new(ast::Expression::Terminal {
                            terminal_type: rumoca_ir_ast::TerminalType::UnsignedInteger,
                            token: Token {
                                text: Arc::from("2"),
                                ..Default::default()
                            },
                        }),
                    },
                )]),
            }],
        },
        value: Arc::new(ast::Expression::ComponentReference(ComponentReference {
            local: false,
            def_id: None,
            parts: vec![ComponentRefPart {
                ident: Token {
                    text: Arc::from("myFunc_der2"),
                    ..Default::default()
                },
                subs: None,
            }],
        })),
    }];

    let derivs = extract_derivative_annotations(&annotations);
    assert_eq!(derivs.len(), 1);
    assert_eq!(derivs[0].derivative_function, "myFunc_der2");
    assert_eq!(derivs[0].order, 2);
}

#[test]
fn test_extract_derivative_annotation_with_zero_derivative() {
    use rumoca_ir_ast::{ComponentRefPart, ComponentReference, Token};
    use std::sync::Arc;

    let annotations = vec![ast::Expression::Modification {
        target: ComponentReference {
            local: false,
            def_id: None,
            parts: vec![ComponentRefPart {
                ident: Token {
                    text: Arc::from("derivative"),
                    ..Default::default()
                },
                subs: Some(vec![ast::Subscript::Expression(
                    ast::Expression::NamedArgument {
                        name: Token {
                            text: Arc::from("zeroDerivative"),
                            ..Default::default()
                        },
                        value: Arc::new(ast::Expression::ComponentReference(ComponentReference {
                            local: false,
                            def_id: None,
                            parts: vec![ComponentRefPart {
                                ident: Token {
                                    text: Arc::from("k"),
                                    ..Default::default()
                                },
                                subs: None,
                            }],
                        })),
                    },
                )]),
            }],
        },
        value: Arc::new(ast::Expression::ComponentReference(ComponentReference {
            local: false,
            def_id: None,
            parts: vec![ComponentRefPart {
                ident: Token {
                    text: Arc::from("myFunc_der"),
                    ..Default::default()
                },
                subs: None,
            }],
        })),
    }];

    let derivs = extract_derivative_annotations(&annotations);
    assert_eq!(derivs.len(), 1);
    assert_eq!(derivs[0].derivative_function, "myFunc_der");
    assert_eq!(derivs[0].order, 1);
    assert_eq!(derivs[0].zero_derivative, vec!["k"]);
}

#[test]
fn test_specialize_static_function_param_from_explicit_call_arg() {
    let mut flat = flat::Model::new();

    let mut apply = flat::Function::new("Pkg.applyOp", Span::DUMMY);
    apply
        .inputs
        .push(flat::FunctionParam::new("f", "Pkg.partialScalarFunction", test_span()));
    apply.inputs.push(flat::FunctionParam::new("x", "Real", test_span()));
    apply.outputs.push(flat::FunctionParam::new("y", "Real", test_span()));
    apply.body.push(flat::Statement::Assignment {
        comp: flat::ComponentReference {
            local: false,
            parts: vec![flat::ComponentRefPart {
                ident: "y".to_string(),
                subs: vec![],
            }],
            def_id: None,
        },
        value: flat::Expression::FunctionCall {
            name: flat::VarName::new("Pkg.applyOp.f"),
            args: vec![flat::Expression::VarRef {
                name: flat::VarName::new("x"),
                subscripts: vec![],
            }],
            is_constructor: false,
        },
    });
    flat.add_function(apply);
    flat.add_function(flat::Function::new(
        "Pkg.partialScalarFunction",
        Span::DUMMY,
    ));
    flat.add_function(flat::Function::new("Pkg.square", Span::DUMMY));

    flat.add_equation(flat::Equation::new(
        flat::Expression::FunctionCall {
            name: flat::VarName::new("Pkg.applyOp"),
            args: vec![
                flat::Expression::VarRef {
                    name: flat::VarName::new("Pkg.square"),
                    subscripts: vec![],
                },
                flat::Expression::Literal(flat::Literal::Real(2.0)),
            ],
            is_constructor: false,
        },
        Span::DUMMY,
        flat::EquationOrigin::ComponentEquation {
            component: "probe".to_string(),
        },
    ));

    specialize_static_function_params(&mut flat);

    let flat::Expression::FunctionCall { name, .. } = &flat.equations[0].residual else {
        panic!("expected function call equation");
    };
    assert_ne!(name.as_str(), "Pkg.applyOp");

    let specialized = flat
        .functions
        .get(name)
        .expect("specialized function must exist");
    let flat::Statement::Assignment { value, .. } = &specialized.body[0] else {
        panic!("expected assignment in specialized body");
    };
    let flat::Expression::FunctionCall { name, .. } = value else {
        panic!("expected specialized call in body");
    };
    assert_eq!(name.as_str(), "Pkg.square");
}

#[test]
fn test_specialize_static_function_param_from_default_arg() {
    let mut flat = flat::Model::new();

    let mut apply = flat::Function::new("Pkg.applyDefault", Span::DUMMY);
    apply.inputs.push(flat::FunctionParam::new("x", "Real", test_span()));
    apply.inputs.push(
        flat::FunctionParam::new("f", "Pkg.partialScalarFunction", test_span()).with_default(
            flat::Expression::VarRef {
                name: flat::VarName::new("Pkg.square"),
                subscripts: vec![],
            },
        ),
    );
    apply.outputs.push(flat::FunctionParam::new("y", "Real", test_span()));
    apply.body.push(flat::Statement::Assignment {
        comp: flat::ComponentReference {
            local: false,
            parts: vec![flat::ComponentRefPart {
                ident: "y".to_string(),
                subs: vec![],
            }],
            def_id: None,
        },
        value: flat::Expression::FunctionCall {
            name: flat::VarName::new("f"),
            args: vec![flat::Expression::VarRef {
                name: flat::VarName::new("x"),
                subscripts: vec![],
            }],
            is_constructor: false,
        },
    });
    flat.add_function(apply);
    flat.add_function(flat::Function::new(
        "Pkg.partialScalarFunction",
        Span::DUMMY,
    ));
    flat.add_function(flat::Function::new("Pkg.square", Span::DUMMY));

    flat.add_equation(flat::Equation::new(
        flat::Expression::FunctionCall {
            name: flat::VarName::new("Pkg.applyDefault"),
            args: vec![flat::Expression::Literal(flat::Literal::Real(4.0))],
            is_constructor: false,
        },
        Span::DUMMY,
        flat::EquationOrigin::ComponentEquation {
            component: "probe".to_string(),
        },
    ));

    specialize_static_function_params(&mut flat);

    let flat::Expression::FunctionCall { name, .. } = &flat.equations[0].residual else {
        panic!("expected function call equation");
    };
    assert_ne!(name.as_str(), "Pkg.applyDefault");

    let specialized = flat
        .functions
        .get(name)
        .expect("specialized function must exist");
    let flat::Statement::Assignment { value, .. } = &specialized.body[0] else {
        panic!("expected assignment in specialized body");
    };
    let flat::Expression::FunctionCall { name, .. } = value else {
        panic!("expected specialized call in body");
    };
    assert_eq!(name.as_str(), "Pkg.square");
}
