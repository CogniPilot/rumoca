//! Diagnostics raised when a call occurrence has no exact callable
//! selection.

use super::*;

#[test]
fn modifier_rewrite_rejects_missing_selection_with_call_site_provenance() {
    let implementation = DefId::new(160);
    let mut standard = class("Standard", ClassType::Function);
    standard.def_id = Some(implementation);
    let mut tree = ClassTree::new();
    tree.definitions
        .classes
        .insert("Standard".to_string(), standard);
    tree.def_map.insert(implementation, "Standard".to_string());
    tree.name_map.insert("Standard".to_string(), implementation);
    let class_index = rumoca_ir_ast::ClassDefIndex::from_tree(&tree);
    let mut overrides = OverrideFunctionMap::default();
    overrides.insert(
        "gravity".to_string(),
        override_target("Standard", implementation, ClassType::Function),
    );
    let ctx = FunctionOverrideRewriteContext::new(&tree, &class_index, &[], &overrides);
    let mut expression = Expression::FunctionCall {
        name: rumoca_core::Reference::new("gravity"),
        args: Vec::new(),
        is_constructor: false,
        span: test_span(),
    };

    let error = rewrite_function_overrides_in_expression_with_ctx(&mut expression, &ctx)
        .expect_err("unstructured function selection must fail before Flat");
    assert!(matches!(
        error,
        FlattenError::MissingFunctionSelectionIdentity { span, .. } if span == test_span()
    ));
}

#[test]
fn generated_unstructured_builtin_does_not_require_callable_selection() {
    let mut overrides = OverrideFunctionMap::default();
    overrides.insert(
        "sin".to_string(),
        override_target("Unrelated.sin", DefId::new(190), ClassType::Function),
    );
    let tree = ClassTree::new();
    let class_index = rumoca_ir_ast::ClassDefIndex::from_tree(&tree);
    let ctx = FunctionOverrideRewriteContext::new(&tree, &class_index, &[], &overrides);
    let mut expression = Expression::FunctionCall {
        name: rumoca_core::Reference::generated("sin"),
        args: vec![Expression::Literal {
            value: rumoca_core::Literal::Real(0.0),
            span: test_span(),
        }],
        is_constructor: false,
        span: test_span(),
    };

    rewrite_function_overrides_in_expression_with_ctx(&mut expression, &ctx)
        .expect("generated builtin is outside source callable selection");

    let Expression::FunctionCall { name, .. } = expression else {
        panic!("expected generated builtin call");
    };
    assert!(name.is_generated());
    assert_eq!(name.as_str(), "sin");
}

#[test]
fn exact_package_function_selection_rejects_ambiguous_inherited_exposures() {
    let left_package_def = DefId::new(1);
    let left_function_def = DefId::new(2);
    let right_package_def = DefId::new(3);
    let right_function_def = DefId::new(4);
    let derived_package_def = DefId::new(5);

    let mut left_function = class("f", ClassType::Function);
    left_function.def_id = Some(left_function_def);
    let mut left_package = class("Left", ClassType::Package);
    left_package.def_id = Some(left_package_def);
    left_package.classes.insert("f".to_string(), left_function);

    let mut right_function = class("f", ClassType::Function);
    right_function.def_id = Some(right_function_def);
    let mut right_package = class("Right", ClassType::Package);
    right_package.def_id = Some(right_package_def);
    right_package
        .classes
        .insert("f".to_string(), right_function);

    let mut derived_package = class("Derived", ClassType::Package);
    derived_package.def_id = Some(derived_package_def);
    derived_package.extends.extend([
        Extend {
            base_name: Name {
                def_id: Some(left_package_def),
                ..Name::from_string("Left")
            },
            base_def_id: Some(left_package_def),
            ..Extend::default()
        },
        Extend {
            base_name: Name {
                def_id: Some(right_package_def),
                ..Name::from_string("Right")
            },
            base_def_id: Some(right_package_def),
            ..Extend::default()
        },
    ]);

    let mut tree = ClassTree::new();
    tree.definitions
        .classes
        .insert("Left".to_string(), left_package);
    tree.definitions
        .classes
        .insert("Right".to_string(), right_package);
    tree.definitions
        .classes
        .insert("Derived".to_string(), derived_package);
    let class_index = rumoca_ir_ast::ClassDefIndex::from_tree(&tree);

    let error = exact_package_function_exposure(
        &class_index,
        derived_package_def,
        "f",
        &mut FxHashSet::default(),
    )
    .expect_err("distinct inherited function DefIds must remain ambiguous");
    assert_eq!(
        error,
        "selected package inherits multiple exact function exposures"
    );
}

#[test]
fn exact_package_function_selection_rejects_unresolved_extends_edge() {
    let package_def = DefId::new(1);
    let mut package = class("Derived", ClassType::Package);
    package.def_id = Some(package_def);
    package.extends.push(Extend {
        base_name: Name::from_string("Missing"),
        ..Extend::default()
    });
    let mut tree = ClassTree::new();
    tree.definitions
        .classes
        .insert("Derived".to_string(), package);
    let class_index = rumoca_ir_ast::ClassDefIndex::from_tree(&tree);

    let error =
        exact_package_function_exposure(&class_index, package_def, "f", &mut FxHashSet::default())
            .expect_err("selection must not recover an unresolved extends edge by spelling");
    assert_eq!(error, "selected package base has no resolved DefId");
}
