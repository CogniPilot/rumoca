//! Inherited function bodies rewrite self-package calls to the
//! exposed package alias.

use super::*;

#[test]
fn inherited_function_body_rewrites_self_package_calls_to_exposed_package() {
    let (tree, mut function) = inherited_function_alias_rewrite_fixture();
    let class_index = rumoca_ir_ast::ClassDefIndex::from_tree(&tree);

    rewrite_function_extends_aliases_in_function(&mut function, &tree, &class_index)
        .expect("function alias rewrite");

    let rumoca_core::Statement::Assignment { value, .. } = &function.body[0] else {
        panic!("expected assignment");
    };
    let Expression::FunctionCall { name, args, .. } = value else {
        panic!("expected outer function call");
    };
    assert_eq!(name.as_str(), "ConcreteMedium.specificEnthalpy");
    let Expression::FunctionCall { name, .. } = &args[0] else {
        panic!("expected nested function call");
    };
    assert_eq!(name.as_str(), "ConcreteMedium.setState_pTX");
}

fn inherited_function_alias_rewrite_fixture() -> (ClassTree, rumoca_core::Function) {
    let ids = InheritedAliasIds::new();
    (
        inherited_alias_tree(ids),
        inherited_alias_function_body(ids),
    )
}

#[derive(Clone, Copy)]
struct InheritedAliasIds {
    partial_pkg: DefId,
    base_pkg: DefId,
    concrete_pkg: DefId,
    partial_specific: DefId,
    partial_set_state: DefId,
    base_specific: DefId,
    base_set_state: DefId,
    output_h: DefId,
    input_p: DefId,
}

impl InheritedAliasIds {
    fn new() -> Self {
        Self {
            partial_pkg: DefId::new(1),
            base_pkg: DefId::new(2),
            concrete_pkg: DefId::new(3),
            partial_specific: DefId::new(4),
            partial_set_state: DefId::new(5),
            base_specific: DefId::new(6),
            base_set_state: DefId::new(7),
            output_h: DefId::new(8),
            input_p: DefId::new(9),
        }
    }
}

fn inherited_alias_tree(ids: InheritedAliasIds) -> ClassTree {
    let mut partial_specific = class("specificEnthalpy", ClassType::Function);
    partial_specific.def_id = Some(ids.partial_specific);
    let mut partial_set_state = class("setState_pTX", ClassType::Function);
    partial_set_state.def_id = Some(ids.partial_set_state);
    let mut partial_pkg = class("PartialMedium", ClassType::Package);
    partial_pkg.def_id = Some(ids.partial_pkg);
    partial_pkg
        .classes
        .insert("specificEnthalpy".to_string(), partial_specific);
    partial_pkg
        .classes
        .insert("setState_pTX".to_string(), partial_set_state);

    let mut base_specific = class("specificEnthalpy", ClassType::Function);
    base_specific.def_id = Some(ids.base_specific);
    let mut base_set_state = class("setState_pTX", ClassType::Function);
    base_set_state.def_id = Some(ids.base_set_state);
    let mut base_pkg = class("BaseMedium", ClassType::Package);
    base_pkg.def_id = Some(ids.base_pkg);
    base_pkg.extends.push(Extend {
        base_name: Name {
            def_id: Some(ids.partial_pkg),
            ..Name::from_string("PartialMedium")
        },
        base_def_id: Some(ids.partial_pkg),
        ..Extend::default()
    });
    base_pkg
        .classes
        .insert("specificEnthalpy".to_string(), base_specific);
    base_pkg
        .classes
        .insert("setState_pTX".to_string(), base_set_state);

    let mut concrete_pkg = class("ConcreteMedium", ClassType::Package);
    concrete_pkg.def_id = Some(ids.concrete_pkg);
    concrete_pkg.extends.push(Extend {
        base_name: Name {
            def_id: Some(ids.base_pkg),
            ..Name::from_string("BaseMedium")
        },
        base_def_id: Some(ids.base_pkg),
        ..Extend::default()
    });

    let mut tree = ClassTree::new();
    tree.definitions
        .classes
        .insert("PartialMedium".to_string(), partial_pkg);
    tree.definitions
        .classes
        .insert("BaseMedium".to_string(), base_pkg);
    tree.definitions
        .classes
        .insert("ConcreteMedium".to_string(), concrete_pkg);
    register_inherited_alias_rewrite_names(
        &mut tree,
        [
            (ids.partial_pkg, "PartialMedium", false),
            (ids.base_pkg, "BaseMedium", false),
            (ids.concrete_pkg, "ConcreteMedium", false),
            (ids.partial_specific, "PartialMedium.specificEnthalpy", true),
            (ids.partial_set_state, "PartialMedium.setState_pTX", true),
            (ids.base_specific, "BaseMedium.specificEnthalpy", true),
            (ids.base_set_state, "BaseMedium.setState_pTX", true),
        ],
    );
    tree.name_map.insert(
        "ConcreteMedium.specificEnthalpy".to_string(),
        ids.base_specific,
    );
    tree.name_map.insert(
        "ConcreteMedium.setState_pTX".to_string(),
        ids.base_set_state,
    );
    tree
}

fn inherited_alias_function_body(ids: InheritedAliasIds) -> rumoca_core::Function {
    let partial_specific_ref = core_comp_ref(&[
        ("PartialMedium", ids.partial_pkg),
        ("specificEnthalpy", ids.partial_specific),
    ]);
    let partial_set_state_ref = core_comp_ref(&[
        ("PartialMedium", ids.partial_pkg),
        ("setState_pTX", ids.partial_set_state),
    ]);
    let mut function =
        rumoca_core::Function::new("ConcreteMedium.specificEnthalpy_pTX", test_span());
    function.body.push(rumoca_core::Statement::Assignment {
        comp: core_comp_ref(&[("h", ids.output_h)]),
        value: Expression::FunctionCall {
            name: rumoca_core::Reference::with_component_reference(
                "PartialMedium.specificEnthalpy",
                partial_specific_ref,
            ),
            args: vec![Expression::FunctionCall {
                name: rumoca_core::Reference::with_component_reference(
                    "PartialMedium.setState_pTX",
                    partial_set_state_ref,
                ),
                args: vec![core_var(&[("p", ids.input_p)])],
                is_constructor: false,
                span: test_span(),
            }],
            is_constructor: false,
            span: test_span(),
        },
        span: test_span(),
    });

    function
}

fn register_inherited_alias_rewrite_names(tree: &mut ClassTree, names: [(DefId, &str, bool); 7]) {
    for (def_id, name, add_name_map) in names {
        tree.def_map.insert(def_id, name.to_string());
        if add_name_map {
            tree.name_map.insert(name.to_string(), def_id);
        }
    }
}
