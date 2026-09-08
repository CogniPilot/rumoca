use super::*;
use rumoca_phase_parse::parse_to_ast;
use rumoca_phase_resolve::resolve;

fn resolved_tree(source: &str) -> ast::ClassTree {
    let file_name = "<inner_prescan_test>";
    let stored = parse_to_ast(source, file_name).expect("parse should succeed");
    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map.add(file_name, source);
    resolve(ast::ParsedTree::new(tree))
        .expect("resolve should succeed")
        .inner()
        .clone()
}

#[test]
fn synthetic_inner_retry_preserves_the_exact_instantiation_error() {
    let tree = resolved_tree("model Root Real value; end Root;");
    let mut malformed = tree
        .get_class_by_qualified_name("Root")
        .expect("resolved Root")
        .clone();
    let value = malformed
        .components
        .get_mut("value")
        .expect("resolved value component");
    value.type_name = ast::Name::from_string("InjectedMissingType");
    value.type_def_id = None;

    let class_index = ast::ClassDefIndex::from_tree(&tree);
    let error = match retry_with_synthetic_inners(
        &tree,
        &class_index,
        &malformed,
        &[],
        InstantiateOptions::default(),
    ) {
        Err(SyntheticInnerError::Error(error)) => error,
        Err(SyntheticInnerError::StillMissing { missing_inners, .. }) => panic!(
            "injected instantiation fault was reclassified as missing inners: {missing_inners:?}"
        ),
        Ok(_) => panic!("injected instantiation fault was accepted"),
    };
    assert!(
        matches!(
            error.as_ref(),
            InstantiateError::MissingResolvedIdentity { name, .. }
                if name == "selected type of component `value`"
        ),
        "the selected-type issuer must reject the forged unresolved component before lookup: {error:?}"
    );
}

fn prescan_context(source: &str, model_name: &str) -> InstantiateContext {
    let tree = resolved_tree(source);
    let model = tree
        .get_class_by_qualified_name(model_name)
        .expect("test model should exist");
    let mut ctx = InstantiateContext::new();
    ctx.index_source_scopes(&tree);
    let template = get_or_compute_template(&tree, model, &mut ctx.template_cache)
        .expect("template construction should succeed");
    let class_index = ast::ClassDefIndex::from_tree(&tree);
    preregister_class_inners(
        &tree,
        &class_index,
        &template.effective_components,
        &mut ctx,
    )
    .expect("inner prescan should succeed");
    ctx
}

fn component_paths(overlay: &ast::InstanceOverlay) -> Vec<String> {
    let mut paths = overlay
        .components
        .values()
        .map(|data| data.qualified_name.to_flat_string())
        .collect::<Vec<_>>();
    paths.sort();
    paths
}

fn connection_paths(overlay: &ast::InstanceOverlay) -> Vec<(String, String)> {
    let mut paths = overlay
        .classes
        .values()
        .flat_map(|class| rumoca_eval_ast::connection::scalar_connection_view(&class.connections))
        .map(|connection| {
            let connection = connection.expect("test connection must have a scalar view");
            (
                connection.a().to_flat_string(),
                connection.b().to_flat_string(),
            )
        })
        .collect::<Vec<_>>();
    paths.sort();
    paths
}

#[test]
fn modifiers_refold_bool_int_and_cross_type_dependents() {
    let source = r"
        model Settings
            parameter Boolean p = true;
            parameter Boolean q = p;
            parameter Integer n = 1;
            parameter Integer m = n + 1;
            parameter Real scale = m;
        end Settings;
        model Root
            inner Settings settings(p = false, n = 4);
        end Root;
    ";
    let ctx = prescan_context(source, "Root");

    assert_eq!(ctx.known_bool_params.get("settings.p"), Some(&false));
    assert_eq!(ctx.known_bool_params.get("settings.q"), Some(&false));
    assert_eq!(ctx.known_int_params.get("settings.n"), Some(&4));
    assert_eq!(ctx.known_int_params.get("settings.m"), Some(&5));
    assert_eq!(ctx.known_real_params.get("settings.scale"), Some(&5.0));
}

#[test]
fn dependency_folding_is_declaration_order_independent() {
    let source = r"
        model Settings
            parameter Real scale = m;
            parameter Integer m = n + 1;
            parameter Integer n = 1;
            parameter Boolean q = p;
            parameter Boolean p = true;
        end Settings;
        model Root
            inner Settings settings(p = false, n = 4);
        end Root;
    ";
    let ctx = prescan_context(source, "Root");

    assert_eq!(ctx.known_bool_params.get("settings.q"), Some(&false));
    assert_eq!(ctx.known_int_params.get("settings.m"), Some(&5));
    assert_eq!(ctx.known_real_params.get("settings.scale"), Some(&5.0));
}

#[test]
fn blocked_and_unknown_values_invalidate_only_their_dependents() {
    let source = r"
        model Settings
            parameter Boolean fixedOff(fixed = false) = true;
            parameter Boolean fixedDependent = fixedOff;
            parameter Integer evaluateOff = 1 annotation(Evaluate = false);
            parameter Integer evaluateDependent = evaluateOff + 1;
            parameter Real crossTypeDependent = evaluateDependent;
            parameter Boolean overridden = true;
            parameter Boolean overriddenDependent = overridden;
            parameter Boolean occurrenceFixed = true;
            parameter Boolean occurrenceFixedDependent = occurrenceFixed;
            parameter Boolean safeBoolean = true;
            parameter Integer safeInteger = 7;
            parameter Real safeReal = 2.5;
        end Settings;
        model Root
            parameter Boolean unsettled;
            parameter Boolean occurrenceFixedPolicy = false;
            inner Settings settings(
                overridden = unsettled,
                occurrenceFixed(fixed = occurrenceFixedPolicy) = true);
        end Root;
    ";
    let ctx = prescan_context(source, "Root");

    for absent in [
        "settings.fixedOff",
        "settings.fixedDependent",
        "settings.overridden",
        "settings.overriddenDependent",
        "settings.occurrenceFixed",
        "settings.occurrenceFixedDependent",
    ] {
        assert!(!ctx.known_bool_params.contains_key(absent), "{absent}");
    }
    for absent in ["settings.evaluateOff", "settings.evaluateDependent"] {
        assert!(!ctx.known_int_params.contains_key(absent), "{absent}");
    }
    assert!(
        !ctx.known_real_params
            .contains_key("settings.crossTypeDependent")
    );
    assert_eq!(
        ctx.known_bool_params.get("settings.safeBoolean"),
        Some(&true)
    );
    assert_eq!(ctx.known_int_params.get("settings.safeInteger"), Some(&7));
    assert_eq!(ctx.known_real_params.get("settings.safeReal"), Some(&2.5));
}

#[test]
fn canonical_boolean_identity_accepts_aliases_without_leaf_name_guessing() {
    let source = r"
        type DirectFlag = Boolean;
        type MultiHopFlag = DirectFlag;
        type FlagArray = Boolean[2];
        package AliasTypes
            type QualifiedFlag = Boolean;
        end AliasTypes;
        package MisleadingTypes
            type Boolean = Real;
        end MisleadingTypes;
        model Settings
            parameter Boolean predefined = true;
            parameter DirectFlag direct = true;
            parameter MultiHopFlag multihop = true;
            parameter AliasTypes.QualifiedFlag qualified = true;
            parameter MisleadingTypes.Boolean misleading = 1.0;
            parameter FlagArray arrayAlias = {true, false};
        end Settings;
        model Root
            inner Settings settings;
        end Root;
    ";
    let ctx = prescan_context(source, "Root");

    for present in ["predefined", "direct", "multihop", "qualified"] {
        assert_eq!(
            ctx.known_bool_params.get(&format!("settings.{present}")),
            Some(&true),
            "canonical Boolean alias `{present}`"
        );
    }
    assert!(!ctx.known_bool_params.contains_key("settings.misleading"));
    assert!(!ctx.known_bool_params.contains_key("settings.arrayAlias"));
    assert_eq!(ctx.known_real_params.get("settings.misleading"), Some(&1.0));
}

#[test]
fn incompatible_outer_identity_leaves_overlay_and_context_catalogs_unchanged() {
    let source = r"
        model T end T;
        model Root
            inner T shared;
        end Root;
    ";
    let tree = resolved_tree(source);
    let inner = tree
        .get_class_by_qualified_name("Root")
        .and_then(|class| class.components.get("shared"))
        .expect("resolved inner declaration")
        .clone();
    let mut ctx = InstantiateContext::new();
    let mut overlay = ast::InstanceOverlay::new();
    handle_inner_outer(
        &tree,
        &inner,
        &mut ctx,
        &mut overlay,
        &ast::QualifiedName::from_ident("shared"),
        "T",
    )
    .expect("resolved inner registers");

    let mut malformed_outer = inner.clone();
    malformed_outer.inner = false;
    malformed_outer.outer = true;
    malformed_outer.type_name = ast::Name::from_string("MissingType");
    malformed_outer.type_def_id = Some(rumoca_core::DefId::new(99_999));
    let overlay_prefix_snapshot = overlay.outer_prefix_to_inner.clone();
    let overlay_bridge_snapshot = overlay.inner_outer_to_parent_inner.clone();
    let event_snapshot = ctx.inner_outer_events;
    let missing_snapshot = ctx.missing_inners.len();
    let registered_inner = ctx
        .find_inner("shared")
        .expect("original inner remains registered")
        .qualified_name
        .clone();

    let error = handle_inner_outer(
        &tree,
        &malformed_outer,
        &mut ctx,
        &mut overlay,
        &ast::QualifiedName::from_dotted("child.shared"),
        "MissingType",
    )
    .expect_err("missing resolved type target must fail before mutation");
    assert!(matches!(*error, InstantiateError::ModelNotFound(_)));
    assert_eq!(overlay.outer_prefix_to_inner, overlay_prefix_snapshot);
    assert_eq!(overlay.inner_outer_to_parent_inner, overlay_bridge_snapshot);
    assert_eq!(ctx.inner_outer_events, event_snapshot);
    assert_eq!(ctx.missing_inners.len(), missing_snapshot);
    assert_eq!(
        ctx.find_inner("shared")
            .expect("original inner remains registered")
            .qualified_name,
        registered_inner
    );
}

#[test]
fn declaration_modifier_uses_its_import_scope() {
    let source = r"
        package Values
            constant Integer count = 4;
        end Values;
        model Settings
            parameter Integer n = 1;
            parameter Integer m = n + 1;
        end Settings;
        model Root
            import Count = Values.count;
            inner Settings settings(n = Count);
        end Root;
    ";
    let ctx = prescan_context(source, "Root");

    assert_eq!(ctx.known_int_params.get("settings.n"), Some(&4));
    assert_eq!(ctx.known_int_params.get("settings.m"), Some(&5));
}

#[test]
fn applied_modifier_uses_ancestor_instance_source_scope() {
    let source = r"
        model Leaf
            Real x;
        end Leaf;
        model Settings
            parameter Real baseScale = 1.0;
            parameter Real scale = 2.0 * baseScale;
        end Settings;
        model Consumer
            outer Settings settings;
            Leaf enabled if settings.scale > 9.0;
        end Consumer;
        model Host
            Consumer consumer;
            inner Settings settings;
        end Host;
        model Root
            parameter Real rootScale = 10.0;
            Host host(settings(baseScale = rootScale / 2.0));
        end Root;
    ";
    let tree = resolved_tree(source);
    let overlay = match crate::instantiate_model_with_outcome(&tree, "Root") {
        crate::InstantiationOutcome::Success(overlay) => overlay,
        crate::InstantiationOutcome::NeedsInner { missing_inners, .. } => {
            panic!("Root unexpectedly needs inner declarations: {missing_inners:?}")
        }
        crate::InstantiationOutcome::Error(error) => panic!("Root failed to instantiate: {error}"),
    };

    assert!(
        component_paths(&overlay)
            .iter()
            .any(|path| path == "host.consumer.enabled.x")
    );
}

#[test]
fn outer_integer_condition_is_independent_of_inner_declaration_order() {
    let source = r"
        model Leaf
            Real x;
        end Leaf;
        model Settings
            parameter Integer n = 1;
        end Settings;
        model Consumer
            outer Settings settings;
            Leaf enabled if settings.n > 4;
            Leaf promoted if settings.n > 4.5;
        end Consumer;
        model Root
            Consumer consumer;
            inner Settings settings(n = 5);
        end Root;
        model RootDisabled
            Consumer consumer;
            inner Settings settings(n = 4);
        end RootDisabled;
    ";
    let tree = resolved_tree(source);
    let overlay = match crate::instantiate_model_with_outcome(&tree, "Root") {
        crate::InstantiationOutcome::Success(overlay) => overlay,
        crate::InstantiationOutcome::NeedsInner { missing_inners, .. } => {
            panic!("Root unexpectedly needs inner declarations: {missing_inners:?}")
        }
        crate::InstantiationOutcome::Error(error) => panic!("Root failed to instantiate: {error}"),
    };

    assert!(
        component_paths(&overlay)
            .iter()
            .any(|path| path == "consumer.enabled.x"),
        "the outer Integer condition must retain the enabled component"
    );
    assert!(
        component_paths(&overlay)
            .iter()
            .any(|path| path == "consumer.promoted.x"),
        "an outer Integer must promote in a mixed numeric comparison"
    );

    let disabled = match crate::instantiate_model_with_outcome(&tree, "RootDisabled") {
        crate::InstantiationOutcome::Success(overlay) => overlay,
        crate::InstantiationOutcome::NeedsInner { missing_inners, .. } => {
            panic!("RootDisabled unexpectedly needs inner declarations: {missing_inners:?}")
        }
        crate::InstantiationOutcome::Error(error) => {
            panic!("RootDisabled failed to instantiate: {error}")
        }
    };
    assert!(
        !component_paths(&disabled)
            .iter()
            .any(|path| path.starts_with("consumer.enabled")),
        "the outer Integer condition must remove the disabled component"
    );
    assert!(
        !component_paths(&disabled)
            .iter()
            .any(|path| path.starts_with("consumer.promoted")),
        "the promoted outer Integer comparison must remove the disabled component"
    );
}

#[test]
fn outer_consumers_are_invariant_to_inner_declaration_order() {
    let source = r"
        connector Pin
            Real v;
            flow Real i;
        end Pin;
        model Leaf
            Real x;
        end Leaf;
        model Settings
            parameter Boolean p = true;
            parameter Boolean q = p;
            parameter Integer n = 1;
            parameter Integer m = n + 1;
            parameter Real scale = m;
        end Settings;
        model Consumer
            outer Settings settings;
            Leaf disabled if settings.q;
            Leaf enabled if not settings.q and settings.scale > 4.5;
            Pin a[5];
            Pin b[5];
        equation
            for i in 1:settings.m loop
                connect(a[i], b[i]);
            end for;
        end Consumer;
        model Before
            Consumer consumer;
            inner Settings settings(p = false, n = 4);
        end Before;
        model After
            inner Settings settings(p = false, n = 4);
            Consumer consumer;
        end After;
    ";
    let tree = resolved_tree(source);
    let before = match crate::instantiate_model_with_outcome(&tree, "Before") {
        crate::InstantiationOutcome::Success(overlay) => overlay,
        crate::InstantiationOutcome::NeedsInner { missing_inners, .. } => {
            panic!("Before unexpectedly needs inner declarations: {missing_inners:?}")
        }
        crate::InstantiationOutcome::Error(error) => {
            panic!("Before failed to instantiate: {error}")
        }
    };
    let after = match crate::instantiate_model_with_outcome(&tree, "After") {
        crate::InstantiationOutcome::Success(overlay) => overlay,
        crate::InstantiationOutcome::NeedsInner { missing_inners, .. } => {
            panic!("After unexpectedly needs inner declarations: {missing_inners:?}")
        }
        crate::InstantiationOutcome::Error(error) => panic!("After failed to instantiate: {error}"),
    };

    assert_eq!(component_paths(&before), component_paths(&after));
    assert_eq!(connection_paths(&before), connection_paths(&after));
    let paths = component_paths(&before);
    assert!(paths.iter().any(|path| path == "consumer.enabled.x"));
    assert!(
        !paths
            .iter()
            .any(|path| path.starts_with("consumer.disabled"))
    );
    assert_eq!(connection_paths(&before).len(), 5);
}
