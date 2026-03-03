//! Tests for flat expression conversion and analysis.

use super::*;
use rumoca_core::DefId;
use rumoca_ir_ast as ast;
use std::sync::Arc;

fn make_var(name: &str) -> ast::Expression {
    ast::Expression::ComponentReference(ast::ComponentReference {
        local: false,
        parts: vec![ast::ComponentRefPart {
            ident: ast::Token {
                text: Arc::from(name),
                ..Default::default()
            },
            subs: None,
        }],
        def_id: None,
    })
}

fn make_int(value: i64) -> ast::Expression {
    ast::Expression::Terminal {
        terminal_type: ast::TerminalType::UnsignedInteger,
        token: ast::Token {
            text: Arc::from(value.to_string()),
            ..Default::default()
        },
    }
}

fn make_named_arg(name: &str, value: ast::Expression) -> ast::Expression {
    ast::Expression::NamedArgument {
        name: ast::Token {
            text: Arc::from(name),
            ..Default::default()
        },
        value: Arc::new(value),
    }
}

fn make_der(var_name: &str) -> ast::Expression {
    ast::Expression::FunctionCall {
        comp: ast::ComponentReference {
            local: false,
            parts: vec![ast::ComponentRefPart {
                ident: ast::Token {
                    text: Arc::from("der"),
                    ..Default::default()
                },
                subs: None,
            }],
            def_id: None,
        },
        args: vec![make_var(var_name)],
    }
}

fn make_for_index(name: &str, start: i64, end: i64) -> ast::ForIndex {
    ast::ForIndex {
        ident: ast::Token {
            text: Arc::from(name),
            ..Default::default()
        },
        range: ast::Expression::Range {
            start: Arc::new(make_int(start)),
            step: None,
            end: Arc::new(make_int(end)),
        },
    }
}

fn make_component_ref_part(name: &str) -> ast::ComponentRefPart {
    ast::ComponentRefPart {
        ident: ast::Token {
            text: Arc::from(name),
            ..Default::default()
        },
        subs: None,
    }
}

fn make_subscripted_ref_expr(name: &str, subscript_value: i64) -> ast::Expression {
    ast::Expression::ComponentReference(ast::ComponentReference {
        local: false,
        parts: vec![ast::ComponentRefPart {
            ident: ast::Token {
                text: Arc::from(name),
                ..Default::default()
            },
            subs: Some(vec![ast::Subscript::Expression(make_int(subscript_value))]),
        }],
        def_id: None,
    })
}

#[test]
fn test_flat_expression_from_variable() {
    let expr = make_var("x");
    let flat = Expression::from_ast(&expr);

    match flat {
        Expression::VarRef { name, subscripts } => {
            assert_eq!(name.as_str(), "x");
            assert!(subscripts.is_empty());
        }
        _ => panic!("Expected VarRef"),
    }
}

#[test]
fn test_flat_expression_from_integer() {
    let expr = make_int(42);
    let flat = Expression::from_ast(&expr);

    match flat {
        Expression::Literal(Literal::Integer(v)) => {
            assert_eq!(v, 42);
        }
        _ => panic!("Expected Integer literal"),
    }
}

#[test]
fn test_flat_expression_from_der() {
    let expr = make_der("x");
    let flat = Expression::from_ast(&expr);

    match flat {
        Expression::BuiltinCall { function, args } => {
            assert_eq!(function, BuiltinFunction::Der);
            assert_eq!(args.len(), 1);
            match &args[0] {
                Expression::VarRef { name, .. } => {
                    assert_eq!(name.as_str(), "x");
                }
                _ => panic!("Expected VarRef in der() argument"),
            }
        }
        _ => panic!("Expected BuiltinCall"),
    }
}

#[test]
fn test_class_modification_uses_def_map_for_constructor_name() {
    let constructor_def_id = DefId::new(42);
    let mut def_map = IndexMap::new();
    def_map.insert(
        constructor_def_id,
        "Modelica.Clocked.RealSignals.Sampler.Utilities.Internal.UniformNoise".to_string(),
    );

    let expr = ast::Expression::ClassModification {
        target: ast::ComponentReference {
            local: false,
            parts: vec![make_component_ref_part("noise")],
            def_id: Some(constructor_def_id),
        },
        modifications: vec![make_int(1)],
    };
    let flat = Expression::from_ast_with_def_map(&expr, Some(&def_map));

    match flat {
        Expression::FunctionCall {
            name,
            args,
            is_constructor,
        } => {
            assert_eq!(
                name.as_str(),
                "Modelica.Clocked.RealSignals.Sampler.Utilities.Internal.UniformNoise"
            );
            assert!(is_constructor);
            assert_eq!(args.len(), 1);
        }
        _ => panic!("Expected constructor FunctionCall"),
    }
}

#[test]
fn test_class_modification_falls_back_to_textual_constructor_name_without_def_id() {
    let expr = ast::Expression::ClassModification {
        target: ast::ComponentReference {
            local: false,
            parts: vec![make_component_ref_part("noise")],
            def_id: None,
        },
        modifications: vec![make_int(1)],
    };
    let flat = Expression::from_ast_with_def_map(&expr, None);

    match flat {
        Expression::FunctionCall {
            name,
            is_constructor,
            ..
        } => {
            assert_eq!(name.as_str(), "noise");
            assert!(is_constructor);
        }
        _ => panic!("Expected constructor FunctionCall"),
    }
}

#[test]
fn test_function_call_named_arguments_preserved_as_internal_named_args() {
    let expr = ast::Expression::FunctionCall {
        comp: ast::ComponentReference {
            local: false,
            parts: vec![
                make_component_ref_part("Modelica"),
                make_component_ref_part("Electrical"),
                make_component_ref_part("Machines"),
                make_component_ref_part("Losses"),
                make_component_ref_part("CoreParameters"),
            ],
            def_id: None,
        },
        args: vec![
            make_named_arg("PRef", make_int(410)),
            make_named_arg("VRef", make_int(388)),
        ],
    };

    let flat = Expression::from_ast(&expr);
    let Expression::FunctionCall {
        args,
        is_constructor,
        ..
    } = flat
    else {
        panic!("Expected function call");
    };
    assert!(!is_constructor);
    assert_eq!(args.len(), 2);

    let Expression::FunctionCall {
        name: first_name,
        args: first_args,
        is_constructor: first_is_constructor,
    } = &args[0]
    else {
        panic!("Expected wrapped named argument");
    };
    assert!(first_is_constructor);
    assert_eq!(first_name.as_str(), "__rumoca_named_arg__.PRef");
    assert!(matches!(
        first_args.first(),
        Some(Expression::Literal(Literal::Integer(410)))
    ));

    let Expression::FunctionCall {
        name: second_name,
        args: second_args,
        is_constructor: second_is_constructor,
    } = &args[1]
    else {
        panic!("Expected wrapped named argument");
    };
    assert!(second_is_constructor);
    assert_eq!(second_name.as_str(), "__rumoca_named_arg__.VRef");
    assert!(matches!(
        second_args.first(),
        Some(Expression::Literal(Literal::Integer(388)))
    ));
}

#[test]
fn test_flat_expression_binary() {
    let expr = ast::Expression::Binary {
        op: ast::OpBinary::Add(ast::Token::default()),
        lhs: Arc::new(make_var("x")),
        rhs: Arc::new(make_int(1)),
    };
    let flat = Expression::from_ast(&expr);

    match flat {
        Expression::Binary { lhs, rhs, .. } => {
            assert!(matches!(*lhs, Expression::VarRef { .. }));
            assert!(matches!(*rhs, Expression::Literal(Literal::Integer(1))));
        }
        _ => panic!("Expected Binary"),
    }
}

#[test]
fn test_flat_expression_preserves_array_comprehension_structure() {
    let expr = ast::Expression::ArrayComprehension {
        expr: Arc::new(make_var("i")),
        indices: vec![make_for_index("i", 1, 3)],
        filter: None,
    };
    let flat = Expression::from_ast(&expr);

    match flat {
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            assert!(filter.is_none());
            assert_eq!(indices.len(), 1);
            assert_eq!(indices[0].name, "i");
            match expr.as_ref() {
                Expression::VarRef { name, .. } => assert_eq!(name.as_str(), "i"),
                _ => panic!("Expected comprehension body VarRef"),
            }
        }
        _ => panic!("Expected ArrayComprehension"),
    }
}

#[test]
fn test_flat_expression_contains_der() {
    let expr_with_der = ast::Expression::Binary {
        op: ast::OpBinary::Sub(ast::Token::default()),
        lhs: Arc::new(make_der("x")),
        rhs: Arc::new(make_var("y")),
    };
    let flat = Expression::from_ast(&expr_with_der);
    assert!(flat.contains_der());

    let expr_without_der = ast::Expression::Binary {
        op: ast::OpBinary::Add(ast::Token::default()),
        lhs: Arc::new(make_var("x")),
        rhs: Arc::new(make_int(1)),
    };
    let flat = Expression::from_ast(&expr_without_der);
    assert!(!flat.contains_der());
}

#[test]
fn test_flat_expression_collect_state_variables() {
    // der(x) + der(y) - z
    let expr = ast::Expression::Binary {
        op: ast::OpBinary::Sub(ast::Token::default()),
        lhs: Arc::new(ast::Expression::Binary {
            op: ast::OpBinary::Add(ast::Token::default()),
            lhs: Arc::new(make_der("x")),
            rhs: Arc::new(make_der("y")),
        }),
        rhs: Arc::new(make_var("z")),
    };
    let flat = Expression::from_ast(&expr);

    let mut states = std::collections::HashSet::new();
    flat.collect_state_variables(&mut states);

    assert_eq!(states.len(), 2);
    assert!(states.contains(&VarName::new("x")));
    assert!(states.contains(&VarName::new("y")));
    assert!(!states.contains(&VarName::new("z")));
}

#[test]
fn test_builtin_function_from_name() {
    assert_eq!(
        BuiltinFunction::from_name("der"),
        Some(BuiltinFunction::Der)
    );
    assert_eq!(
        BuiltinFunction::from_name("sin"),
        Some(BuiltinFunction::Sin)
    );
    assert_eq!(
        BuiltinFunction::from_name("cos"),
        Some(BuiltinFunction::Cos)
    );
    assert_eq!(
        BuiltinFunction::from_name("pre"),
        Some(BuiltinFunction::Pre)
    );
    assert_eq!(
        BuiltinFunction::from_name("linspace"),
        Some(BuiltinFunction::Linspace)
    );
    assert_eq!(BuiltinFunction::from_name("unknown"), None);
}

fn make_parameter_var(name: &str, fixed: Option<bool>, has_binding: bool) -> flat::Variable {
    flat::Variable {
        name: VarName::new(name),
        variability: ast::Variability::Parameter(ast::Token::default()),
        fixed,
        binding: has_binding.then_some(Expression::Literal(Literal::Real(1.0))),
        ..Default::default()
    }
}

#[test]
fn test_unbound_fixed_parameters_detects_missing_bindings() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("p_missing"),
        make_parameter_var("p_missing", None, false),
    );
    flat.add_variable(
        VarName::new("p_bound"),
        make_parameter_var("p_bound", None, true),
    );
    flat.add_variable(
        VarName::new("p_not_fixed"),
        make_parameter_var("p_not_fixed", Some(false), false),
    );

    assert!(flat.has_unbound_fixed_parameters());
    assert_eq!(
        flat.unbound_fixed_parameters(),
        vec![VarName::new("p_missing")]
    );
}

#[test]
fn test_extract_algorithm_outputs_drops_assignment_subscripts() {
    let stmts = vec![Statement::Assignment {
        comp: ComponentReference {
            local: false,
            parts: vec![ComponentRefPart {
                ident: "x".to_string(),
                subs: vec![Subscript::Index(1)],
            }],
            def_id: None,
        },
        value: Expression::Literal(Literal::Real(1.0)),
    }];

    let outputs = extract_algorithm_outputs(&stmts);
    assert_eq!(outputs, vec![VarName::new("x")]);
}

#[test]
fn test_extract_algorithm_outputs_keeps_function_call_targets() {
    let stmts = vec![Statement::FunctionCall {
        comp: ComponentReference {
            local: false,
            parts: vec![ComponentRefPart {
                ident: "f".to_string(),
                subs: Vec::new(),
            }],
            def_id: None,
        },
        args: Vec::new(),
        outputs: vec![Expression::VarRef {
            name: VarName::new("y"),
            subscripts: vec![Subscript::Index(2)],
        }],
    }];

    let outputs = extract_algorithm_outputs(&stmts);
    assert_eq!(outputs, vec![VarName::new("y")]);
}

#[test]
fn test_component_ref_from_ast_uses_def_map_when_parts_empty() {
    let def_id = ast::DefId::new(42);
    let ast_ref = ast::ComponentReference {
        local: false,
        parts: Vec::new(),
        def_id: Some(def_id),
    };

    let mut def_map = indexmap::IndexMap::new();
    def_map.insert(def_id, "Model.y".to_string());

    let flat = ComponentReference::from_ast_with_def_map(&ast_ref, Some(&def_map));
    assert_eq!(flat.to_var_name().as_str(), "Model.y");
}

#[test]
fn test_component_ref_from_ast_preserves_non_empty_parts_over_def_map() {
    let def_id = ast::DefId::new(7);
    let ast_ref = ast::ComponentReference {
        local: false,
        parts: vec![
            make_component_ref_part("inst"),
            make_component_ref_part("y"),
        ],
        def_id: Some(def_id),
    };

    let mut def_map = indexmap::IndexMap::new();
    def_map.insert(def_id, "Declaration.y".to_string());

    let flat = ComponentReference::from_ast_with_def_map(&ast_ref, Some(&def_map));
    assert_eq!(flat.to_var_name().as_str(), "inst.y");
}

#[test]
fn test_flat_expression_component_ref_canonicalizes_enum_literal_with_def_map() {
    let def_id = ast::DefId::new(314);
    let expr = ast::Expression::ComponentReference(ast::ComponentReference {
        local: false,
        parts: vec![make_component_ref_part("L"), make_component_ref_part("'1'")],
        def_id: Some(def_id),
    });

    let mut def_map = indexmap::IndexMap::new();
    def_map.insert(
        def_id,
        "Modelica.Electrical.Digital.Interfaces.Logic.'1'".to_string(),
    );

    let flat = Expression::from_ast_with_def_map(&expr, Some(&def_map));
    let Expression::VarRef { name, .. } = flat else {
        panic!("expected enum literal var ref");
    };
    assert_eq!(
        name.as_str(),
        "Modelica.Electrical.Digital.Interfaces.Logic.'1'"
    );
}

#[test]
fn test_flat_expression_component_ref_preserves_non_enum_textual_path() {
    let def_id = ast::DefId::new(2718);
    let expr = ast::Expression::ComponentReference(ast::ComponentReference {
        local: false,
        parts: vec![
            make_component_ref_part("inst"),
            make_component_ref_part("y"),
        ],
        def_id: Some(def_id),
    });

    let mut def_map = indexmap::IndexMap::new();
    def_map.insert(def_id, "Declaration.y".to_string());

    let flat = Expression::from_ast_with_def_map(&expr, Some(&def_map));
    let Expression::VarRef { name, .. } = flat else {
        panic!("expected var ref");
    };
    assert_eq!(name.as_str(), "inst.y");
}

#[test]
fn test_flat_expression_component_ref_encodes_final_segment_subscripts_in_name() {
    let expr = ast::Expression::ComponentReference(ast::ComponentReference {
        local: false,
        parts: vec![ast::ComponentRefPart {
            ident: ast::Token {
                text: Arc::from("A"),
                ..Default::default()
            },
            subs: Some(vec![ast::Subscript::Expression(make_int(2))]),
        }],
        def_id: None,
    });

    let flat = Expression::from_ast(&expr);
    let Expression::VarRef { name, subscripts } = flat else {
        panic!("expected var ref");
    };
    assert_eq!(name.as_str(), "A[2]");
    assert!(subscripts.is_empty());
}

#[test]
fn test_flat_expression_component_ref_preserves_nested_subscripts_in_subscript_expressions() {
    let expr = ast::Expression::ComponentReference(ast::ComponentReference {
        local: false,
        parts: vec![ast::ComponentRefPart {
            ident: ast::Token {
                text: Arc::from("T"),
                ..Default::default()
            },
            subs: Some(vec![
                ast::Subscript::Expression(make_subscripted_ref_expr("aux", 1)),
                ast::Subscript::Expression(make_subscripted_ref_expr("x", 2)),
            ]),
        }],
        def_id: None,
    });

    let flat = Expression::from_ast(&expr);
    let Expression::Index { base, subscripts } = flat else {
        panic!("expected Index expression");
    };
    let Expression::VarRef {
        name,
        subscripts: base_subs,
    } = base.as_ref()
    else {
        panic!("expected Index base var ref");
    };
    assert_eq!(name.as_str(), "T");
    assert!(base_subs.is_empty());
    assert_eq!(subscripts.len(), 2);
    assert!(matches!(subscripts[0], Subscript::Expr(_)));
    assert!(matches!(subscripts[1], Subscript::Expr(_)));
}
