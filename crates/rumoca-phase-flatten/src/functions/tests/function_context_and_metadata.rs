use super::*;

#[test]
fn test_function_context_inherits_base_lexical_imports() {
    let (tree, derived_function) = function_context_inheritance_tree();
    let class_index = ast::ClassDefIndex::from_tree(&tree);
    let mut member_cache = qualify::MemberDefIdCache::default();
    let context =
        collect_function_context(&tree, &class_index, &derived_function, &mut member_cache);

    assert_eq!(
        context.imports.get("pi").map(String::as_str),
        Some("Modelica.Constants.pi")
    );
    assert!(context.components.contains_key("crossArea"));
}

fn function_context_inheritance_tree() -> (ast::ClassTree, ast::ClassDef) {
    let base_def = rumoca_core::DefId::new(1);
    let derived_def = rumoca_core::DefId::new(2);
    let root_package_def = rumoca_core::DefId::new(3);
    let base_package_def = rumoca_core::DefId::new(4);
    let derived_package_def = rumoca_core::DefId::new(5);

    let base_function = base_pressure_loss_function(base_def);
    let derived_function = ast::ClassDef {
        def_id: Some(derived_def),
        name: rumoca_core::Token {
            text: "pressureLoss".into(),
            ..Default::default()
        },
        class_type: rumoca_core::ClassType::Function,
        extends: vec![ast::Extend {
            base_name: ast::Name::from_string("P.Base.pressureLoss"),
            base_def_id: Some(base_def),
            ..Default::default()
        }],
        ..Default::default()
    };
    let base_package = ast::ClassDef {
        def_id: Some(base_package_def),
        name: rumoca_core::Token {
            text: "Base".into(),
            ..Default::default()
        },
        class_type: rumoca_core::ClassType::Package,
        imports: vec![ast::Import::Qualified {
            path: ast::Name::from_string("Modelica.Constants.pi"),
            location: rumoca_core::Location::default(),
            global_scope: false,
        }],
        classes: ast::AstIndexMap::from_iter([("pressureLoss".to_string(), base_function)]),
        ..Default::default()
    };
    let derived_package = ast::ClassDef {
        def_id: Some(derived_package_def),
        name: rumoca_core::Token {
            text: "Derived".into(),
            ..Default::default()
        },
        class_type: rumoca_core::ClassType::Package,
        classes: ast::AstIndexMap::from_iter([(
            "pressureLoss".to_string(),
            derived_function.clone(),
        )]),
        ..Default::default()
    };
    let root_package = ast::ClassDef {
        def_id: Some(root_package_def),
        name: rumoca_core::Token {
            text: "P".into(),
            ..Default::default()
        },
        class_type: rumoca_core::ClassType::Package,
        classes: ast::AstIndexMap::from_iter([
            ("Base".to_string(), base_package),
            ("Derived".to_string(), derived_package),
        ]),
        ..Default::default()
    };

    let mut tree = ast::ClassTree::default();
    tree.definitions
        .classes
        .insert("P".to_string(), root_package);
    register_function_context_inheritance_names(
        &mut tree,
        [
            (root_package_def, "P"),
            (base_package_def, "P.Base"),
            (derived_package_def, "P.Derived"),
            (base_def, "P.Base.pressureLoss"),
            (derived_def, "P.Derived.pressureLoss"),
        ],
    );
    (tree, derived_function)
}

fn base_pressure_loss_function(base_def: rumoca_core::DefId) -> ast::ClassDef {
    ast::ClassDef {
        def_id: Some(base_def),
        name: rumoca_core::Token {
            text: "pressureLoss".into(),
            ..Default::default()
        },
        class_type: rumoca_core::ClassType::Function,
        components: ast::AstIndexMap::from_iter([("crossArea".to_string(), pi_component())]),
        ..Default::default()
    }
}

fn pi_component() -> ast::Component {
    ast::Component {
        name: "crossArea".to_string(),
        type_name: ast::Name::from_string("Real"),
        binding: Some(ast::Expression::ComponentReference(ast_comp_ref(
            &["pi"],
            rumoca_core::DefId::new(4300),
        ))),
        ..ast::Component::empty_with_span(test_span())
    }
}

fn register_function_context_inheritance_names(
    tree: &mut ast::ClassTree,
    names: [(rumoca_core::DefId, &str); 5],
) {
    for (def_id, name) in names {
        tree.name_map.insert(name.to_string(), def_id);
        tree.def_map.insert(def_id, name.to_string());
    }
}

#[test]
fn external_function_metadata_preserves_library_and_include_annotations() {
    let source = r##"
pure function Linked
  input Real u;
  output Real y;
external "C" y = linked_call(u)
  annotation(
    Library = {"Linked", "Support"},
    Include = "#include \"linked.h\"");
end Linked;
"##;
    let resolved = resolve_test_source(source, "external_annotations.mo");
    let external = resolved
        .inner()
        .definitions
        .classes
        .get("Linked")
        .and_then(|class| class.external.as_ref())
        .expect("external declaration");
    let metadata = convert_external_function(
        external,
        ast_lower::PredefinedIntrinsicIds::from_tree(resolved.inner()),
    )
    .expect("external annotations should lower without loss");

    let [library, include] = metadata.annotations.as_slice() else {
        panic!("expected Library and Include metadata");
    };
    assert_eq!(library.name, ["Library"]);
    let rumoca_core::Expression::Array { elements, .. } = &library.value else {
        panic!("Library must retain its array expression");
    };
    assert!(matches!(
        elements.as_slice(),
        [
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::String(first),
                ..
            },
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::String(second),
                ..
            }
        ] if first == "Linked" && second == "Support"
    ));

    assert_eq!(include.name, ["Include"]);
    assert!(matches!(
        &include.value,
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::String(value),
            ..
        } if value == "#include \"linked.h\""
    ));
}

#[test]
fn external_function_metadata_rejects_annotation_syntax_it_cannot_preserve() {
    let source = r#"
pure function Linked
  input Real u;
  output Real y;
external "C" y = linked_call(u)
  annotation(__Vendor(options = 1));
end Linked;
"#;
    let resolved = resolve_test_source(source, "external_annotations.mo");
    let external = resolved
        .inner()
        .definitions
        .classes
        .get("Linked")
        .and_then(|class| class.external.as_ref())
        .expect("external declaration");
    let error = convert_external_function(
        external,
        ast_lower::PredefinedIntrinsicIds::from_tree(resolved.inner()),
    )
    .expect_err("unrepresentable annotation syntax must fail instead of being dropped");

    assert!(
        error
            .to_string()
            .contains("unsupported external-function annotation")
    );
}

#[test]
fn external_function_metadata_preserves_ordered_argument_expressions() {
    let source = r#"
pure function Linked
  input Real u;
  input Real v;
  output Real y;
external "C" y = linked_call(u, size({u, v}, 1), 2.0 * v);
end Linked;
"#;
    let resolved = resolve_test_source(source, "external_arguments.mo");
    let external = resolved
        .inner()
        .definitions
        .classes
        .get("Linked")
        .and_then(|class| class.external.as_ref())
        .expect("external declaration");
    let lowered = convert_external_function(
        external,
        ast_lower::PredefinedIntrinsicIds::from_tree(resolved.inner()),
    )
    .expect("all external ABI arguments must lower without loss");

    assert_eq!(lowered.args.len(), 3);
    assert!(matches!(
        lowered.args[0],
        rumoca_core::Expression::VarRef { .. }
    ));
    assert!(matches!(
        lowered.args[1],
        rumoca_core::Expression::BuiltinCall { .. }
    ));
    assert!(matches!(
        lowered.args[2],
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Mul,
            ..
        }
    ));
    assert!(
        lowered
            .args
            .iter()
            .all(|argument| argument.span().is_some_and(|span| !span.is_dummy())),
        "every ABI argument must retain its exact source provenance"
    );
}

#[test]
fn test_extract_derivative_annotation_simple() {
    use rumoca_core::Token;
    use std::sync::Arc;

    // Test: annotation(derivative = myFunc_der)
    let derivative_function_def_id = rumoca_core::DefId::new(4400);
    let annotations = vec![ast::Expression::NamedArgument {
        name: Token {
            text: Arc::from("derivative"),
            ..Default::default()
        },
        value: Arc::new(ast::Expression::ComponentReference(ast_comp_ref(
            &["myFunc_der"],
            derivative_function_def_id,
        ))),
        span: test_span(),
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
    use rumoca_core::Token;
    use std::sync::Arc;

    // Test: annotation(derivative(order=2) = myFunc_der2)
    // This is represented as a Modification with target having subscripts
    let derivative_annotation_def_id = rumoca_core::DefId::new(4401);
    let derivative_function_def_id = rumoca_core::DefId::new(4402);
    let annotations = vec![ast::Expression::Modification {
        target: ast_comp_ref_with_subscripts(
            &["derivative"],
            derivative_annotation_def_id,
            vec![ast::Subscript::Expression(ast::Expression::NamedArgument {
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
                    span: test_span(),
                }),
                span: test_span(),
            })],
        ),
        value: Arc::new(ast::Expression::ComponentReference(ast_comp_ref(
            &["myFunc_der2"],
            derivative_function_def_id,
        ))),
        span: test_span(),
    }];

    let derivs = extract_derivative_annotations(&annotations);
    assert_eq!(derivs.len(), 1);
    assert_eq!(derivs[0].derivative_function, "myFunc_der2");
    assert_eq!(derivs[0].order, 2);
}

#[test]
fn test_extract_derivative_annotation_with_zero_derivative() {
    use rumoca_core::Token;
    use std::sync::Arc;

    // Test: annotation(derivative(zeroDerivative=k) = myFunc_der)
    let derivative_annotation_def_id = rumoca_core::DefId::new(4403);
    let derivative_function_def_id = rumoca_core::DefId::new(4404);
    let zero_derivative_param_def_id = rumoca_core::DefId::new(4405);
    let annotations = vec![ast::Expression::Modification {
        target: ast_comp_ref_with_subscripts(
            &["derivative"],
            derivative_annotation_def_id,
            vec![ast::Subscript::Expression(ast::Expression::NamedArgument {
                name: Token {
                    text: Arc::from("zeroDerivative"),
                    ..Default::default()
                },
                value: Arc::new(ast::Expression::ComponentReference(ast_comp_ref(
                    &["k"],
                    zero_derivative_param_def_id,
                ))),
                span: test_span(),
            })],
        ),
        value: Arc::new(ast::Expression::ComponentReference(ast_comp_ref(
            &["myFunc_der"],
            derivative_function_def_id,
        ))),
        span: test_span(),
    }];

    let derivs = extract_derivative_annotations(&annotations);
    assert_eq!(derivs.len(), 1);
    assert_eq!(derivs[0].derivative_function, "myFunc_der");
    assert_eq!(derivs[0].order, 1);
    assert_eq!(derivs[0].zero_derivative, vec!["k"]);
}
