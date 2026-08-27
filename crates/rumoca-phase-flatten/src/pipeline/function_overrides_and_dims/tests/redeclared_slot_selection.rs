//! Replaceable-function redeclare selection by exact slot identity
//! (MLS §7.3): a visible redeclare retargets exactly the calls whose exposed
//! declaration is the slot it fills, and a redeclare without resolved slot
//! identity refuses instead of silently selecting the declared default.

use super::*;

struct RedeclareFixtureIds {
    double_def: DefId,
    triple_def: DefId,
    consumer_def: DefId,
    slot_def: DefId,
}

/// `package Pkg` containing concrete functions `Double`/`Triple` and
/// `block Consumer` declaring `replaceable function F = Double`.
fn redeclare_fixture_tree() -> (ClassTree, RedeclareFixtureIds) {
    let pkg_def = DefId::new(1);
    let double_def = DefId::new(2);
    let triple_def = DefId::new(3);
    let consumer_def = DefId::new(4);
    let slot_def = DefId::new(5);

    let mut double_fn = class("Double", ClassType::Function);
    double_fn.def_id = Some(double_def);
    double_fn.algorithms.push(Vec::new());
    let mut triple_fn = class("Triple", ClassType::Function);
    triple_fn.def_id = Some(triple_def);
    triple_fn.algorithms.push(Vec::new());

    let mut slot = class("F", ClassType::Function);
    slot.def_id = Some(slot_def);
    slot.is_replaceable = true;
    slot.extends.push(Extend {
        base_name: Name::from_string("Double"),
        base_def_id: Some(double_def),
        ..Extend::default()
    });
    let mut consumer = class("Consumer", ClassType::Block);
    consumer.def_id = Some(consumer_def);
    consumer.classes.insert("F".to_string(), slot);

    let mut pkg = class("Pkg", ClassType::Package);
    pkg.def_id = Some(pkg_def);
    pkg.classes.insert("Double".to_string(), double_fn);
    pkg.classes.insert("Triple".to_string(), triple_fn);
    pkg.classes.insert("Consumer".to_string(), consumer);

    let mut tree = ClassTree::new();
    tree.definitions.classes.insert("Pkg".to_string(), pkg);
    for (def_id, name) in [
        (pkg_def, "Pkg"),
        (double_def, "Pkg.Double"),
        (triple_def, "Pkg.Triple"),
        (consumer_def, "Pkg.Consumer"),
        (slot_def, "Pkg.Consumer.F"),
    ] {
        tree.def_map.insert(def_id, name.to_string());
        tree.name_map.insert(name.to_string(), def_id);
    }
    (
        tree,
        RedeclareFixtureIds {
            double_def,
            triple_def,
            consumer_def,
            slot_def,
        },
    )
}

fn redeclare_override(
    name: &str,
    def_id: DefId,
    function_slot: FunctionSlot,
) -> (String, OverrideTarget) {
    (
        "F".to_string(),
        OverrideTarget {
            alias: "F".to_string(),
            name: name.to_string(),
            def_id,
            class_type: ClassType::Function,
            active: true,
            modifier_args: Vec::new(),
            function_slot,
        },
    )
}

fn slot_call() -> (Expression, DefId) {
    let (_, ids) = redeclare_fixture_tree();
    (
        Expression::FunctionCall {
            name: rumoca_core::Reference::with_component_reference(
                "F",
                core_comp_ref(&[("F", ids.slot_def)]),
            ),
            args: Vec::new(),
            is_constructor: false,
            span: test_span(),
        },
        ids.slot_def,
    )
}

#[test]
fn exact_slot_redeclare_retargets_call_to_redeclared_implementation() {
    let (tree, ids) = redeclare_fixture_tree();
    let class_index = rumoca_ir_ast::ClassDefIndex::from_tree(&tree);
    let override_packages = Vec::new();
    let mut override_functions = OverrideFunctionMap::default();
    let (alias, target) = redeclare_override(
        "Pkg.Triple",
        ids.triple_def,
        FunctionSlot::Exact(ids.slot_def),
    );
    override_functions.insert(alias, target);
    let ctx = FunctionOverrideRewriteContext::new(
        &tree,
        &class_index,
        &override_packages,
        &override_functions,
    );

    let (mut expr, _) = slot_call();
    rewrite_function_overrides_in_expression_with_ctx(&mut expr, &ctx)
        .expect("redeclared slot call should rewrite");

    let Expression::FunctionCall { name, .. } = expr else {
        panic!("expected function call");
    };
    assert_eq!(name.as_str(), "Pkg.Triple");
    assert_eq!(name.target_def_id(), Some(ids.triple_def));
    let component_ref = name
        .component_ref()
        .expect("rewritten call remains structured");
    assert_eq!(
        component_ref
            .parts()
            .iter()
            .map(|part| part.ident.as_str())
            .collect::<Vec<_>>(),
        vec!["Pkg", "Triple"],
    );
}

#[test]
fn redeclare_of_a_different_slot_keeps_the_declared_default() {
    let (tree, ids) = redeclare_fixture_tree();
    let class_index = rumoca_ir_ast::ClassDefIndex::from_tree(&tree);
    let override_packages = Vec::new();
    let mut override_functions = OverrideFunctionMap::default();
    // The redeclare fills some other slot; this call's exposure must keep
    // selecting its own declared default implementation.
    let (alias, target) = redeclare_override(
        "Pkg.Triple",
        ids.triple_def,
        FunctionSlot::Exact(ids.consumer_def),
    );
    override_functions.insert(alias, target);
    let ctx = FunctionOverrideRewriteContext::new(
        &tree,
        &class_index,
        &override_packages,
        &override_functions,
    );

    let (mut expr, _) = slot_call();
    rewrite_function_overrides_in_expression_with_ctx(&mut expr, &ctx)
        .expect("unrelated redeclare must not fail the call");

    let Expression::FunctionCall { name, .. } = expr else {
        panic!("expected function call");
    };
    assert_eq!(name.as_str(), "F");
    assert_eq!(name.target_def_id(), Some(ids.double_def));
}

#[test]
fn unresolved_redeclare_slot_refuses_instead_of_defaulting() {
    let (tree, ids) = redeclare_fixture_tree();
    let class_index = rumoca_ir_ast::ClassDefIndex::from_tree(&tree);
    let override_packages = Vec::new();
    let mut override_functions = OverrideFunctionMap::default();
    let (alias, target) =
        redeclare_override("Pkg.Triple", ids.triple_def, FunctionSlot::Unresolved);
    override_functions.insert(alias, target);
    let ctx = FunctionOverrideRewriteContext::new(
        &tree,
        &class_index,
        &override_packages,
        &override_functions,
    );

    let (mut expr, _) = slot_call();
    let error = rewrite_function_overrides_in_expression_with_ctx(&mut expr, &ctx)
        .expect_err("an unresolved redeclare slot must refuse, never silently default");
    assert!(
        matches!(error, FlattenError::UnhonoredFunctionRedeclare { .. }),
        "expected UnhonoredFunctionRedeclare, got {error:?}"
    );
}

#[test]
fn element_redeclare_collection_records_exact_slot_identity() {
    let (mut tree, ids) = redeclare_fixture_tree();
    let uses_triple_def = DefId::new(6);
    let redeclared_def = DefId::new(7);

    let mut redeclared = class("F", ClassType::Function);
    redeclared.def_id = Some(redeclared_def);
    redeclared.is_redeclare = true;
    redeclared.redeclare_target_def_id = Some(ids.slot_def);
    redeclared.extends.push(Extend {
        base_name: Name::from_string("Triple"),
        base_def_id: Some(ids.triple_def),
        ..Extend::default()
    });
    let mut uses_triple = class("UsesTriple", ClassType::Block);
    uses_triple.def_id = Some(uses_triple_def);
    uses_triple.extends.push(Extend {
        base_name: Name::from_string("Consumer"),
        base_def_id: Some(ids.consumer_def),
        ..Extend::default()
    });
    uses_triple.classes.insert("F".to_string(), redeclared);
    tree.def_map
        .insert(uses_triple_def, "Pkg.UsesTriple".to_string());
    tree.def_map
        .insert(redeclared_def, "Pkg.UsesTriple.F".to_string());
    let pkg = tree
        .definitions
        .classes
        .get_mut("Pkg")
        .expect("fixture package");
    pkg.classes.insert("UsesTriple".to_string(), uses_triple);

    let class_index = rumoca_ir_ast::ClassDefIndex::from_tree(&tree);
    let uses_triple = class_index
        .get(uses_triple_def)
        .expect("fixture derived block");
    let mut overrides = rustc_hash::FxHashMap::default();
    let mut visited = FxHashSet::default();
    collect_component_constructor_aliases_for_class(
        &tree,
        &class_index,
        uses_triple,
        "Pkg.UsesTriple",
        true,
        &mut visited,
        &mut overrides,
    );

    let target = overrides
        .get("F")
        .expect("element redeclare must contribute an override");
    assert_eq!(target.def_id, redeclared_def);
    assert_eq!(target.class_type, ClassType::Function);
    assert!(target.active);
    assert_eq!(target.function_slot, FunctionSlot::Exact(ids.slot_def));
}

#[test]
fn extends_modification_redeclare_collection_records_exact_slot_identity() {
    let (mut tree, ids) = redeclare_fixture_tree();
    let uses_triple_def = DefId::new(6);

    let mut uses_triple = class("UsesTriple", ClassType::Block);
    uses_triple.def_id = Some(uses_triple_def);
    uses_triple.extends.push(Extend {
        base_name: Name::from_string("Consumer"),
        base_def_id: Some(ids.consumer_def),
        modifications: vec![rumoca_ir_ast::ExtendModification {
            expr: rumoca_ir_ast::Expression::Modification {
                target: comp_ref(&["F"]),
                value: std::sync::Arc::new(resolved_ast_var(&[("Triple", ids.triple_def)])),
                span: test_span(),
            },
            each: false,
            final_: false,
            redeclare: true,
        }],
        ..Extend::default()
    });
    tree.def_map
        .insert(uses_triple_def, "Pkg.UsesTriple".to_string());
    let pkg = tree
        .definitions
        .classes
        .get_mut("Pkg")
        .expect("fixture package");
    pkg.classes.insert("UsesTriple".to_string(), uses_triple);

    let class_index = rumoca_ir_ast::ClassDefIndex::from_tree(&tree);
    let uses_triple = class_index
        .get(uses_triple_def)
        .expect("fixture derived block");
    let mut overrides = rustc_hash::FxHashMap::default();
    let mut visited = FxHashSet::default();
    collect_component_constructor_aliases_for_class(
        &tree,
        &class_index,
        uses_triple,
        "Pkg.UsesTriple",
        true,
        &mut visited,
        &mut overrides,
    );

    let target = overrides
        .get("F")
        .expect("extends-modification redeclare must contribute an override");
    assert_eq!(target.def_id, ids.triple_def);
    assert_eq!(target.class_type, ClassType::Function);
    assert!(target.active);
    assert_eq!(target.function_slot, FunctionSlot::Exact(ids.slot_def));
}
