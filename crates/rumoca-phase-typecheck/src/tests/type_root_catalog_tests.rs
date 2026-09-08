use super::*;
use std::path::Path;

fn seed_overlay_catalogs(overlay: &mut InstanceOverlay) {
    let sentinel_type = TypeId::new(u32::MAX - 1);
    let sentinel_declaration = DefId::new(u32::MAX - 1);
    overlay.type_roots.insert(sentinel_type, sentinel_type);
    overlay.enumeration_type_roots.insert(sentinel_type);
    overlay
        .type_ids_by_def_id
        .insert(sentinel_declaration, sentinel_type);
    overlay.effective_types.insert(
        sentinel_type,
        rumoca_core::EffectiveType::new(sentinel_type, sentinel_type, [])
            .expect("sentinel effective type is complete"),
    );
    overlay.enumeration_types.insert(sentinel_type);
}

#[derive(Debug, PartialEq)]
struct OverlayPublicationSnapshot {
    component_types_and_dims: Vec<(InstanceId, TypeId, Vec<i64>)>,
    type_roots: Vec<(TypeId, TypeId)>,
    declaration_types: Vec<(DefId, TypeId)>,
    enumeration_roots: Vec<TypeId>,
    effective_types: Vec<(TypeId, rumoca_core::EffectiveType)>,
    enumeration_types: Vec<TypeId>,
    overconstrained_counts: (usize, usize),
    overconstrained_state: String,
}

fn overlay_publication_snapshot(overlay: &InstanceOverlay) -> OverlayPublicationSnapshot {
    OverlayPublicationSnapshot {
        component_types_and_dims: overlay
            .components
            .iter()
            .map(|(instance, data)| (*instance, data.type_id, data.dims.clone()))
            .collect(),
        type_roots: overlay
            .type_roots
            .iter()
            .map(|(type_id, root)| (*type_id, *root))
            .collect(),
        declaration_types: overlay
            .type_ids_by_def_id
            .iter()
            .map(|(declaration, type_id)| (*declaration, *type_id))
            .collect(),
        enumeration_roots: overlay.enumeration_type_roots.iter().copied().collect(),
        effective_types: overlay
            .effective_types
            .iter()
            .map(|(type_id, effective)| (*type_id, effective.clone()))
            .collect(),
        enumeration_types: overlay.enumeration_types.iter().copied().collect(),
        overconstrained_counts: overlay.overconstrained_construction_counts(),
        overconstrained_state: format!("{:?}", overlay.finalized_overconstrained().err()),
    }
}

fn replace_alias_targets_with_cycle(tree: &mut ClassTree) {
    let first = tree
        .get_def_id_by_name("First")
        .expect("cycle fixture has First");
    let second = tree
        .get_def_id_by_name("Second")
        .expect("cycle fixture has Second");
    tree.definitions
        .classes
        .get_mut("First")
        .and_then(|class| class.extends.first_mut())
        .expect("First has one alias edge")
        .base_def_id = Some(second);
    tree.definitions
        .classes
        .get_mut("Second")
        .and_then(|class| class.extends.first_mut())
        .expect("Second has one alias edge")
        .base_def_id = Some(first);
}

fn tree_with_published_type_context(mut tree: ClassTree) -> ClassTree {
    let (type_table, _) = TypeChecker::new()
        .build_type_context(&tree)
        .expect("fixture declarations issue one exact complete type context");
    tree.type_table = type_table;
    tree
}

fn canonical_root_contract_diagnostic(
    diagnostics: &rumoca_core::Diagnostics,
) -> &rumoca_core::Diagnostic {
    diagnostics
        .iter()
        .find(|diagnostic| {
            diagnostic.code.as_deref() == Some("ET014") && diagnostic.message.contains("canonical")
        })
        .unwrap_or_else(|| panic!("missing canonical-root contract error: {diagnostics:?}"))
}

#[test]
fn alias_cycle_mutation_refuses_standalone_and_instanced_roots_identically() {
    let source = r#"
        type First = Real;
        type Second = Integer;
        model Test
            First value;
        end Test;
    "#;
    let resolved = resolve(parse(source)).expect("acyclic fixture resolves");
    let tree = resolved.inner().clone();
    let mut overlay = match rumoca_phase_instantiate::instantiate_model_with_outcome(&tree, "Test")
    {
        rumoca_phase_instantiate::InstantiationOutcome::Success(overlay) => overlay,
        rumoca_phase_instantiate::InstantiationOutcome::NeedsInner { missing_inners, .. } => {
            panic!("fixture unexpectedly needs inner declarations: {missing_inners:?}")
        }
        rumoca_phase_instantiate::InstantiationOutcome::Error(error) => {
            panic!("fixture instantiation failed: {error}")
        }
    };

    let mut standalone_tree = tree.clone();
    replace_alias_targets_with_cycle(&mut standalone_tree);
    let standalone_before = format!("{standalone_tree:?}");
    let (intact_type_table, _) = TypeChecker::new()
        .initialize_instanced_context(&tree)
        .expect("the intact aliases issue exact identities");
    let first_type = intact_type_table
        .lookup("First")
        .expect("the intact plan issues First");
    let second_type = intact_type_table
        .lookup("Second")
        .expect("the intact plan issues Second");
    let standalone = TypeChecker::new().check(&mut standalone_tree);
    assert_eq!(
        format!("{standalone_tree:?}"),
        standalone_before,
        "standalone canonical-root refusal cannot publish the detached TypeTable or mutate the tree",
    );

    let mut instanced_tree = tree;
    replace_alias_targets_with_cycle(&mut instanced_tree);
    seed_overlay_catalogs(&mut overlay);
    let overlay_before = overlay_publication_snapshot(&overlay);
    let complete_overlay_before = format!("{overlay:?}");
    let instanced =
        TypeChecker::new().check_instanced_test_projection(&instanced_tree, &mut overlay, "Test");

    let standalone_diagnostic = canonical_root_contract_diagnostic(&standalone);
    let instanced_diagnostic = canonical_root_contract_diagnostic(&instanced);
    assert_eq!(standalone_diagnostic.message, instanced_diagnostic.message);
    assert_eq!(
        standalone_diagnostic.labels.len(),
        instanced_diagnostic.labels.len(),
    );
    for (standalone_label, instanced_label) in standalone_diagnostic
        .labels
        .iter()
        .zip(&instanced_diagnostic.labels)
    {
        assert_eq!(standalone_label.span, instanced_label.span);
        assert_eq!(standalone_label.message, instanced_label.message);
        assert_eq!(standalone_label.primary, instanced_label.primary);
    }
    assert!(standalone_diagnostic.message.contains(&format!(
        "edge {second_type:?} -> {first_type:?} closes an alias cycle"
    )));
    let edge_start = source
        .find("Integer")
        .expect("fixture contains second alias base");
    let edge_span = standalone_diagnostic
        .labels
        .first()
        .expect("source-backed alias cycle has one primary edge label")
        .span;
    assert_eq!(edge_span.start.0, edge_start);
    assert_eq!(edge_span.end.0, edge_start + "Integer".len());
    assert_eq!(
        overlay_publication_snapshot(&overlay),
        overlay_before,
        "an impossible catalog cannot mutate any overlay publication state",
    );
    assert_eq!(
        format!("{overlay:?}"),
        complete_overlay_before,
        "canonical refusal leaves the complete overlay structurally bit-exact unchanged",
    );
}

#[test]
fn preexisting_alias_name_binding_refuses_before_type_table_publication() {
    let source = r#"
        type First = Real;
        model Test
            First value;
        end Test;
    "#;
    let tree = resolve(parse(source))
        .expect("the unmodified alias graph resolves")
        .inner()
        .clone();
    let mut tree = tree_with_published_type_context(tree);
    let before = format!("{tree:?}");

    let diagnostics = TypeChecker::new().check(&mut tree);
    let diagnostic = canonical_root_contract_diagnostic(&diagnostics);
    assert!(
        diagnostic
            .message
            .contains("payload not issued by this ClassTree declaration inventory"),
        "a replayed declaration table cannot become a second Typecheck issuer: {diagnostic:?}",
    );
    assert_eq!(format!("{tree:?}"), before);
}

#[test]
fn preexisting_class_and_enum_names_cannot_be_reused() {
    let cases = [
        (r#"record Payload Real value; end Payload;"#, "Payload"),
        (r#"type Choice = enumeration(first, second);"#, "Choice"),
    ];

    for (source, name) in cases {
        let tree = resolve(parse(source))
            .expect("the source declaration resolves")
            .inner()
            .clone();
        let mut tree = tree_with_published_type_context(tree);
        let before = format!("{tree:?}");

        let diagnostics = TypeChecker::new().check(&mut tree);
        let diagnostic = canonical_root_contract_diagnostic(&diagnostics);
        assert!(
            diagnostic
                .message
                .contains("payload not issued by this ClassTree declaration inventory"),
            "{name} replay cannot become a second type issuer: {diagnostic:?}",
        );
        assert_eq!(format!("{tree:?}"), before);
    }
}

#[test]
fn malformed_alias_edges_are_et014_at_the_exact_edge_and_publish_nothing() {
    let source = r#"
        type Exact = Real;
        model Test
            Exact value;
        end Test;
    "#;
    let original = resolve(parse(source))
        .expect("the mutation seed resolves")
        .inner()
        .clone();

    let mut missing_identity = original.clone();
    let edge = missing_identity
        .definitions
        .classes
        .get_mut("Exact")
        .and_then(|class| class.extends.first_mut())
        .expect("Exact owns one base edge");
    edge.base_def_id = None;
    let missing_identity_before = format!("{missing_identity:?}");
    let diagnostics = TypeChecker::new().check(&mut missing_identity);
    let diagnostic = canonical_root_contract_diagnostic(&diagnostics);
    assert!(diagnostic.message.contains("has no declaration identity"));
    let real_start = source.find("Real").expect("fixture contains the base edge");
    let label = diagnostic
        .labels
        .first()
        .expect("source-backed missing identity has one label");
    assert_eq!(label.span.start.0, real_start);
    assert_eq!(label.span.end.0, real_start + "Real".len());
    assert_eq!(format!("{missing_identity:?}"), missing_identity_before);

    let mut missing_target = original;
    missing_target
        .definitions
        .classes
        .get_mut("Exact")
        .and_then(|class| class.extends.first_mut())
        .expect("Exact owns one base edge")
        .base_def_id = Some(DefId::new(u32::MAX - 1));
    let missing_target_before = format!("{missing_target:?}");
    let diagnostics = TypeChecker::new().check(&mut missing_target);
    let diagnostic = canonical_root_contract_diagnostic(&diagnostics);
    assert!(diagnostic.message.contains("has no issued type payload"));
    let label = diagnostic
        .labels
        .first()
        .expect("source-backed missing target has one label");
    assert_eq!(label.span.start.0, real_start);
    assert_eq!(label.span.end.0, real_start + "Real".len());
    assert_eq!(format!("{missing_target:?}"), missing_target_before);

    let mut missing_edge = resolve(parse(source))
        .expect("the missing-edge seed resolves")
        .inner()
        .clone();
    missing_edge
        .definitions
        .classes
        .get_mut("Exact")
        .expect("Exact exists")
        .extends
        .clear();
    let missing_edge_before = format!("{missing_edge:?}");
    let diagnostics = TypeChecker::new().check(&mut missing_edge);
    let diagnostic = canonical_root_contract_diagnostic(&diagnostics);
    assert!(
        diagnostic
            .message
            .contains("exactly one base edge, found 0")
    );
    assert_eq!(format!("{missing_edge:?}"), missing_edge_before);
}

#[test]
fn duplicate_declaration_claim_refuses_before_any_type_table_publication() {
    let source = r#"
        record First
            Real value;
        end First;
        record Second
            Integer value;
        end Second;
    "#;
    let mut tree = resolve(parse(source))
        .expect("the distinct declaration seed resolves")
        .inner()
        .clone();
    let first = tree
        .get_def_id_by_name("First")
        .expect("First has an exact declaration identity");
    *tree
        .name_map
        .get_mut("Second")
        .expect("Second has an exact name claim") = first;
    let before = format!("{tree:?}");

    let diagnostics = TypeChecker::new().check(&mut tree);
    let diagnostic = canonical_root_contract_diagnostic(&diagnostics);
    assert!(
        diagnostic.message.contains("both claim"),
        "duplicate DefId claims have one typed ET014 refusal: {diagnostic:?}",
    );
    assert_eq!(format!("{tree:?}"), before);
}

#[test]
fn every_predefined_claim_mutation_is_et014_and_publishes_nothing() {
    let seed = resolve(parse("model Claimed end Claimed;"))
        .expect("the predefined-claim seed resolves")
        .inner()
        .clone();
    let predefined = seed
        .type_table
        .predefined_entries()
        .map(|(name, type_id)| {
            let def_id = seed
                .scope_tree
                .predefined_member(&ComponentPath::from_flat_path(name))
                .unwrap_or_else(|| panic!("{name} has an exact Resolve identity"));
            (name, type_id, def_id)
        })
        .collect::<Vec<_>>();

    for (name, type_id, def_id) in predefined {
        let mut deleted = seed.clone();
        deleted
            .def_map
            .shift_remove(&def_id)
            .unwrap_or_else(|| panic!("{name} has an exact DefId-name claim"));
        assert_predefined_mutation_refused(&mut deleted, name, type_id, "deleted");

        let mut substituted = seed.clone();
        substituted
            .scope_tree
            .get_mut(ScopeId::GLOBAL)
            .expect("Resolve retains the global scope")
            .members
            .insert(
                ComponentPath::from_flat_path(name),
                DefId::new(u32::MAX - 1),
            );
        assert_predefined_mutation_refused(&mut substituted, name, type_id, "substituted");

        let mut duplicated = seed.clone();
        duplicated
            .definitions
            .classes
            .get_mut("Claimed")
            .expect("fixture has one source class")
            .def_id = Some(def_id);
        assert_predefined_mutation_refused(&mut duplicated, name, type_id, "duplicated");
    }
}

fn assert_predefined_mutation_refused(
    tree: &mut ClassTree,
    name: &str,
    type_id: TypeId,
    mutation: &str,
) {
    assert_eq!(
        tree.type_table.lookup(name),
        Some(type_id),
        "the adversary leaves predefined payload `{name}` unchanged"
    );
    let before = format!("{tree:?}");
    let diagnostics = TypeChecker::new().check(tree);
    let diagnostic = canonical_root_contract_diagnostic(&diagnostics);
    assert!(
        diagnostic.message.contains(name) || diagnostic.message.contains("both claim"),
        "{mutation} `{name}` claim must have one typed ET014 explanation: {diagnostic:?}"
    );
    assert_eq!(
        format!("{tree:?}"),
        before,
        "{mutation} `{name}` claim cannot publish the detached type candidate"
    );
}

#[test]
fn one_checked_batch_preserves_exact_class_enum_and_alias_payloads() {
    let source = r#"
        record Payload
            Real value;
        end Payload;
        type Choice = enumeration(first, second);
        type Alias = Payload;
        model Test
            Payload payload;
            Choice choice;
            Alias aliasValue;
        end Test;
    "#;
    let resolved = resolve(parse(source)).expect("the exact declaration graph resolves");
    let payload_def = resolved
        .get_def_id_by_name("Payload")
        .expect("Payload has a declaration identity");
    let typed = typecheck(resolved)
        .expect("class, enumeration, and alias construct in one exact checked batch");
    let payload = typed
        .type_table
        .lookup("Payload")
        .expect("Payload receives one identity");
    let choice = typed
        .type_table
        .lookup("Choice")
        .expect("Choice receives one identity");
    let alias = typed
        .type_table
        .lookup("Alias")
        .expect("Alias receives one identity");
    assert!(matches!(
        typed.type_table.get(payload),
        Some(Type::Class(class)) if class.def_id == payload_def && class.name == "Payload"
    ));
    assert!(matches!(
        typed.type_table.get(choice),
        Some(Type::Enumeration(enumeration))
            if enumeration.name == "Choice"
                && enumeration.literals == ["first", "second"]
    ));
    assert!(matches!(
        typed.type_table.get(alias),
        Some(Type::Alias(alias_payload))
            if alias_payload.name == "Alias" && alias_payload.aliased == payload
    ));
}

#[test]
fn warning_only_typecheck_publishes_the_complete_type_product() {
    let source = r#"
        model Test
            parameter Integer n = integer(-1e40);
        end Test;
    "#;
    let mut tree = resolve(parse(source))
        .expect("the warning-only fixture resolves")
        .inner()
        .clone();
    assert!(tree.type_table.lookup("Test").is_none());

    let diagnostics = TypeChecker::new().check(&mut tree);
    assert!(!diagnostics.has_errors(), "warnings remain successful");
    assert!(
        diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code.as_deref() == Some("WT006")),
        "the fixture must exercise warning-success publication: {diagnostics:?}"
    );
    assert!(tree.type_table.lookup("Test").is_some());
}

#[test]
fn genuine_unknown_overlay_type_remains_et001() {
    let source = "model Test Real value; end Test;";
    let resolved = resolve(parse(source)).expect("the overlay seed resolves");
    let mut overlay =
        match rumoca_phase_instantiate::instantiate_model_with_outcome(resolved.inner(), "Test") {
            rumoca_phase_instantiate::InstantiationOutcome::Success(overlay) => overlay,
            rumoca_phase_instantiate::InstantiationOutcome::NeedsInner {
                missing_inners, ..
            } => {
                panic!("fixture unexpectedly needs inner declarations: {missing_inners:?}")
            }
            rumoca_phase_instantiate::InstantiationOutcome::Error(error) => {
                panic!("fixture instantiation failed: {error}")
            }
        };
    let value = overlay
        .components
        .values_mut()
        .find(|data| data.qualified_name.to_flat_string() == "value")
        .expect("fixture has one value occurrence");
    value.type_name = "Missing".to_string();
    value.type_def_id = None;
    value.type_id = TypeId::UNKNOWN;

    let diagnostics = typecheck_instanced_tree(&resolved, overlay, "Test")
        .expect_err("genuine unresolved overlay data must refuse");
    assert!(
        diagnostics.iter().any(|diagnostic| {
            diagnostic.code.as_deref() == Some("ET001") && diagnostic.message.contains("Missing")
        }),
        "UNKNOWN overlay type data remains ET001: {diagnostics:?}",
    );
    assert!(
        diagnostics
            .iter()
            .all(|diagnostic| diagnostic.code.as_deref() != Some("ET014")),
        "ET014 is reserved for malformed canonical construction: {diagnostics:?}",
    );
}

#[test]
fn late_et002_refusal_preserves_standalone_tree_bit_exact() {
    let source = r#"
        model Test
            Real invalid(start = true);
        end Test;
    "#;
    let mut tree = resolve(parse(source))
        .expect("the canonical type inventory resolves")
        .inner()
        .clone();
    let before = format!("{tree:?}");

    let diagnostics = TypeChecker::new().check(&mut tree);
    assert!(
        diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code.as_deref() == Some("ET002")),
        "the late modifier mismatch must remain ET002: {diagnostics:?}",
    );
    assert_eq!(
        format!("{tree:?}"),
        before,
        "late semantic refusal cannot publish the detached typed candidate",
    );
}

#[test]
fn late_et002_refusal_preserves_instanced_overlay_bit_exact() {
    let source = r#"
        model Test
            Real invalid(start = true);
        end Test;
    "#;
    let resolved = resolve(parse(source)).expect("the canonical type inventory resolves");
    let overlay =
        match rumoca_phase_instantiate::instantiate_model_with_outcome(resolved.inner(), "Test") {
            rumoca_phase_instantiate::InstantiationOutcome::Success(overlay) => overlay,
            rumoca_phase_instantiate::InstantiationOutcome::NeedsInner {
                missing_inners, ..
            } => panic!("fixture unexpectedly needs inner declarations: {missing_inners:?}"),
            rumoca_phase_instantiate::InstantiationOutcome::Error(error) => {
                panic!("fixture instantiation failed: {error}")
            }
        };
    // Non-publication on failure is enforced by construction: the mint
    // consumes the overlay by value, so a refusing run leaves no candidate
    // overlay for any caller to observe, partially annotated or otherwise.
    let diagnostics = typecheck_instanced_tree(&resolved, overlay, "Test")
        .expect_err("the late modifier mismatch must refuse");
    assert!(
        diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code.as_deref() == Some("ET002")),
        "the late modifier mismatch must remain ET002: {diagnostics:?}",
    );
}

#[test]
fn foreign_non_unknown_type_ids_are_et014_before_publication() {
    let source = "model Test Real value; end Test;";
    let resolved = resolve(parse(source)).expect("the canonical type inventory resolves");
    let mut standalone = resolved.inner().clone();
    standalone
        .definitions
        .classes
        .get_mut("Test")
        .and_then(|class| class.components.get_mut("value"))
        .expect("fixture has a standalone value declaration")
        .type_id = Some(TypeId::new(u32::MAX - 1));
    let standalone_before = format!("{standalone:?}");
    let standalone_diagnostics = TypeChecker::new().check(&mut standalone);
    assert!(
        standalone_diagnostics.iter().any(|diagnostic| {
            diagnostic.code.as_deref() == Some("ET014")
                && diagnostic
                    .message
                    .contains("non-issued external type identity")
        }),
        "foreign standalone identity must be a typed construction refusal: {standalone_diagnostics:?}",
    );
    assert_eq!(format!("{standalone:?}"), standalone_before);

    let mut overlay =
        match rumoca_phase_instantiate::instantiate_model_with_outcome(resolved.inner(), "Test") {
            rumoca_phase_instantiate::InstantiationOutcome::Success(overlay) => overlay,
            rumoca_phase_instantiate::InstantiationOutcome::NeedsInner {
                missing_inners, ..
            } => panic!("fixture unexpectedly needs inner declarations: {missing_inners:?}"),
            rumoca_phase_instantiate::InstantiationOutcome::Error(error) => {
                panic!("fixture instantiation failed: {error}")
            }
        };
    overlay
        .components
        .values_mut()
        .find(|data| data.qualified_name.to_flat_string() == "value")
        .expect("fixture has an overlay value occurrence")
        .type_id = TypeId::new(u32::MAX - 1);
    // The refusing mint consumes the overlay by value, so no caller can
    // observe a partially published candidate after this error.
    let overlay_diagnostics = typecheck_instanced_tree(&resolved, overlay, "Test")
        .expect_err("foreign overlay identity must refuse");
    assert!(
        overlay_diagnostics.iter().any(|diagnostic| {
            diagnostic.code.as_deref() == Some("ET014")
                && diagnostic
                    .message
                    .contains("non-issued external type identity")
        }),
        "foreign overlay identity must be a typed construction refusal: {overlay_diagnostics:?}",
    );
}

#[test]
fn type_table_issuer_bypasses_are_tombstoned() {
    let ast_types = std::fs::read_to_string(
        Path::new(env!("CARGO_MANIFEST_DIR")).join("../rumoca-ir-ast/src/types.rs"),
    )
    .expect("AST type construction source is readable");
    let typecheck_root =
        std::fs::read_to_string(Path::new(env!("CARGO_MANIFEST_DIR")).join("src/lib.rs"))
            .expect("Typecheck root source is readable");
    let type_table_declaration = ast_types
        .find("pub struct TypeTable")
        .expect("TypeTable declaration remains present");
    let derive_prefix =
        &ast_types[type_table_declaration.saturating_sub(160)..type_table_declaration];
    assert!(!derive_prefix.contains("Default"));
    for removed in [
        "pub fn add_type",
        "pub fn get_mut",
        "self.types.len() as u32",
    ] {
        assert!(!ast_types.contains(removed), "found bypass `{removed}`");
    }
    for removed in [
        "type_table.add_type",
        "register_enumeration_type",
        "register_class_type",
        "TypeId::new(idx as u32)",
    ] {
        assert!(
            !typecheck_root.contains(removed),
            "found alternate Typecheck issuer `{removed}`"
        );
    }
}

#[test]
fn missing_used_function_alias_base_fails_in_resolve() {
    let source = r#"
        type Broken = MissingLibrary.Value;

        function consumeBroken
            input Broken value;
            output Real result;
        algorithm
            result := 1.0;
        end consumeBroken;

        model Test
            Real result;
        equation
            result = consumeBroken(1.0);
        end Test;
    "#;
    let diagnostics = resolve(parse(source))
        .expect_err("a missing alias base must fail before an instanced overlay can exist");
    assert!(
        diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code.as_deref() == Some("ER003")
                && diagnostic.message.contains("MissingLibrary.Value")),
        "Resolve owns the exact missing alias-base refusal: {diagnostics:?}",
    );
}

#[test]
fn missing_exact_used_function_signature_refuses_atomic_catalog_install() {
    let source = r#"
        package P
            function select
                input Real value;
                output Real result;
            algorithm
                result := value;
            end select;
        end P;
        package Q
            function select
                input Real value;
                output Real result;
            algorithm
                result := value;
            end select;
        end Q;
        model Test
            Real result;
        equation
            result = P.select(1.0);
        end Test;
    "#;
    let resolved = resolve(parse(source)).expect("qualified calls resolve exact callable targets");
    let mut instanced = {
        let tree = resolved.inner().clone();
        let overlay = match rumoca_phase_instantiate::instantiate_model_with_outcome(&tree, "Test")
        {
            rumoca_phase_instantiate::InstantiationOutcome::Success(overlay) => overlay,
            rumoca_phase_instantiate::InstantiationOutcome::NeedsInner {
                missing_inners, ..
            } => panic!("fixture unexpectedly needs inner declarations: {missing_inners:?}"),
            rumoca_phase_instantiate::InstantiationOutcome::Error(error) => {
                panic!("fixture instantiation failed: {error}")
            }
        };
        rumoca_ir_ast::InstancedTree::new(tree, overlay)
    };
    let selected = instanced
        .tree
        .get_def_id_by_name("P.select")
        .expect("selected function declaration exists");
    let collision = instanced
        .tree
        .get_def_id_by_name("Q.select")
        .expect("same-spelling collision exists");
    let mut checker = TypeChecker::new();
    let (type_table, _type_root_catalog) = checker
        .initialize_instanced_context(&instanced.tree)
        .expect("the nominal type graph is complete");
    checker.function_signatures.remove(&collision);
    checker
        .populate_overlay_type_roots(&instanced.tree, &mut instanced.overlay, &type_table)
        .expect("an unused same-spelling function is not the selected callable");

    checker.function_signatures.remove(&selected);
    seed_overlay_catalogs(&mut instanced.overlay);
    let roots_before = instanced.overlay.type_roots.clone();
    let enumerations_before = instanced.overlay.enumeration_type_roots.clone();
    let declarations_before = instanced.overlay.type_ids_by_def_id.clone();
    let error = checker
        .populate_overlay_type_roots(&instanced.tree, &mut instanced.overlay, &type_table)
        .expect_err("the selected callable cannot lose its checked signature");
    assert!(matches!(
        error.as_ref(),
        TypeCheckError::PhaseDiagnostic { code, message, .. }
            if code == "ET012" && message.contains(&format!("{selected:?}"))
    ));
    assert_eq!(instanced.overlay.type_roots, roots_before);
    assert_eq!(
        instanced.overlay.enumeration_type_roots,
        enumerations_before
    );
    assert_eq!(instanced.overlay.type_ids_by_def_id, declarations_before);
}

#[test]
fn absent_required_record_declaration_refuses_atomic_catalog_install() {
    let source = r#"
        record Payload
            Real value;
        end Payload;
        function consume
            input Payload value;
            output Real result;
        algorithm
            result := value.value;
        end consume;
        model Test
            Real result;
        equation
            result = consume(Payload(1.0));
        end Test;
    "#;
    let resolved = resolve(parse(source)).expect("record call resolves");
    let mut instanced = {
        let tree = resolved.inner().clone();
        let overlay = match rumoca_phase_instantiate::instantiate_model_with_outcome(&tree, "Test")
        {
            rumoca_phase_instantiate::InstantiationOutcome::Success(overlay) => overlay,
            rumoca_phase_instantiate::InstantiationOutcome::NeedsInner {
                missing_inners, ..
            } => panic!("fixture unexpectedly needs inner declarations: {missing_inners:?}"),
            rumoca_phase_instantiate::InstantiationOutcome::Error(error) => {
                panic!("fixture instantiation failed: {error}")
            }
        };
        rumoca_ir_ast::InstancedTree::new(tree, overlay)
    };
    let record = instanced
        .tree
        .get_def_id_by_name("Payload")
        .expect("record declaration exists");
    let mut checker = TypeChecker::new();
    let (type_table, _type_root_catalog) = checker
        .initialize_instanced_context(&instanced.tree)
        .expect("the intact record graph produces a nominal table");
    instanced.tree.definitions.classes.shift_remove("Payload");
    seed_overlay_catalogs(&mut instanced.overlay);
    let roots_before = instanced.overlay.type_roots.clone();
    let enumerations_before = instanced.overlay.enumeration_type_roots.clone();
    let declarations_before = instanced.overlay.type_ids_by_def_id.clone();
    let error = checker
        .populate_overlay_type_roots(&instanced.tree, &mut instanced.overlay, &type_table)
        .expect_err("a required record class cannot disappear from the checked graph");
    assert!(
        matches!(
            error.as_ref(),
            TypeCheckError::CanonicalTypeRootInvariant {
                reason,
                span: None,
                ..
            } if reason.contains("no exact target")
        ),
        "unexpected refusal after removing {record:?}: {error:?}",
    );
    let diagnostic = error.to_diagnostic();
    assert_eq!(diagnostic.code.as_deref(), Some("ET014"));
    assert!(
        diagnostic.labels.is_empty(),
        "source-free graph corruption uses the dedicated global invariant diagnostic",
    );
    assert_eq!(instanced.overlay.type_roots, roots_before);
    assert_eq!(
        instanced.overlay.enumeration_type_roots,
        enumerations_before
    );
    assert_eq!(instanced.overlay.type_ids_by_def_id, declarations_before);
}

#[test]
fn missing_resolved_callable_target_refuses_atomic_catalog_install() {
    let source = r#"
        function selected
            input Real value;
            output Real result;
        algorithm
            result := value;
        end selected;
        model Test
            Real result;
        equation
            result = selected(1.0);
        end Test;
    "#;
    let resolved = resolve(parse(source)).expect("call resolves before the identity mutation");
    let mut instanced = {
        let tree = resolved.inner().clone();
        let overlay = match rumoca_phase_instantiate::instantiate_model_with_outcome(&tree, "Test")
        {
            rumoca_phase_instantiate::InstantiationOutcome::Success(overlay) => overlay,
            rumoca_phase_instantiate::InstantiationOutcome::NeedsInner {
                missing_inners, ..
            } => panic!("fixture unexpectedly needs inner declarations: {missing_inners:?}"),
            rumoca_phase_instantiate::InstantiationOutcome::Error(error) => {
                panic!("fixture instantiation failed: {error}")
            }
        };
        rumoca_ir_ast::InstancedTree::new(tree, overlay)
    };
    let call = instanced
        .overlay
        .classes
        .values_mut()
        .flat_map(|class| &mut class.equations)
        .find_map(|equation| match &mut equation.equation {
            rumoca_ir_ast::Equation::Simple {
                rhs: Expression::FunctionCall { comp, .. },
                ..
            } => Some(comp),
            _ => None,
        })
        .expect("mutation seed has one direct value call");
    call.set_target_def_id(None);

    let mut checker = TypeChecker::new();
    let (type_table, _type_root_catalog) = checker
        .initialize_instanced_context(&instanced.tree)
        .expect("the nominal type graph is complete");
    seed_overlay_catalogs(&mut instanced.overlay);
    let roots_before = instanced.overlay.type_roots.clone();
    let enumerations_before = instanced.overlay.enumeration_type_roots.clone();
    let declarations_before = instanced.overlay.type_ids_by_def_id.clone();
    let error = checker
        .populate_overlay_type_roots(&instanced.tree, &mut instanced.overlay, &type_table)
        .expect_err("a call without exact callable identity cannot issue Flat type roots");
    assert!(matches!(
        error.as_ref(),
        TypeCheckError::PhaseDiagnostic { code, message, .. }
            if code == "ET012" && message.contains("no exact resolved callable")
    ));
    assert_eq!(instanced.overlay.type_roots, roots_before);
    assert_eq!(
        instanced.overlay.enumeration_type_roots,
        enumerations_before
    );
    assert_eq!(instanced.overlay.type_ids_by_def_id, declarations_before);
}

#[test]
fn transitive_function_dimension_call_requires_checked_signature_atomically() {
    let source = r#"
        function dimension
            output Integer value;
        algorithm
            value := 1;
        end dimension;
        function consume
            input Real values[dimension()];
            output Real result;
        algorithm
            result := values[1];
        end consume;
        model Test
            Real result;
        equation
            result = consume({1.0});
        end Test;
    "#;
    let resolved = resolve(parse(source)).expect("function ABI dimension call resolves");
    let mut instanced = {
        let tree = resolved.inner().clone();
        let overlay = match rumoca_phase_instantiate::instantiate_model_with_outcome(&tree, "Test")
        {
            rumoca_phase_instantiate::InstantiationOutcome::Success(overlay) => overlay,
            rumoca_phase_instantiate::InstantiationOutcome::NeedsInner {
                missing_inners, ..
            } => panic!("fixture unexpectedly needs inner declarations: {missing_inners:?}"),
            rumoca_phase_instantiate::InstantiationOutcome::Error(error) => {
                panic!("fixture instantiation failed: {error}")
            }
        };
        rumoca_ir_ast::InstancedTree::new(tree, overlay)
    };
    let dimension = instanced
        .tree
        .get_def_id_by_name("dimension")
        .expect("dimension function declaration exists");
    let mut checker = TypeChecker::new();
    let (type_table, _type_root_catalog) = checker
        .initialize_instanced_context(&instanced.tree)
        .expect("the nominal type graph is complete");
    checker.function_signatures.remove(&dimension);
    seed_overlay_catalogs(&mut instanced.overlay);
    let roots_before = instanced.overlay.type_roots.clone();
    let enumerations_before = instanced.overlay.enumeration_type_roots.clone();
    let declarations_before = instanced.overlay.type_ids_by_def_id.clone();
    let error = checker
        .populate_overlay_type_roots(&instanced.tree, &mut instanced.overlay, &type_table)
        .expect_err("a transitive call in function ABI shape cannot lose its signature");
    assert!(matches!(
        error.as_ref(),
        TypeCheckError::PhaseDiagnostic { code, message, .. }
            if code == "ET012" && message.contains(&format!("{dimension:?}"))
    ));
    assert_eq!(instanced.overlay.type_roots, roots_before);
    assert_eq!(
        instanced.overlay.enumeration_type_roots,
        enumerations_before
    );
    assert_eq!(instanced.overlay.type_ids_by_def_id, declarations_before);
}

#[test]
fn unresolved_required_record_base_identity_refuses_atomic_catalog_install() {
    let source = r#"
        record Base
            Real inherited;
        end Base;
        record Derived
            extends Base;
            Boolean own;
        end Derived;
        function consume
            input Derived value;
            output Boolean result;
        algorithm
            result := value.own;
        end consume;
        model Test
            Real result;
        equation
            result = if consume(Derived(1.0, true)) then 1.0 else 0.0;
        end Test;
    "#;
    let resolved = resolve(parse(source)).expect("record inheritance resolves before mutation");
    let mut instanced = {
        let tree = resolved.inner().clone();
        let overlay = match rumoca_phase_instantiate::instantiate_model_with_outcome(&tree, "Test")
        {
            rumoca_phase_instantiate::InstantiationOutcome::Success(overlay) => overlay,
            rumoca_phase_instantiate::InstantiationOutcome::NeedsInner {
                missing_inners, ..
            } => panic!("fixture unexpectedly needs inner declarations: {missing_inners:?}"),
            rumoca_phase_instantiate::InstantiationOutcome::Error(error) => {
                panic!("fixture instantiation failed: {error}")
            }
        };
        rumoca_ir_ast::InstancedTree::new(tree, overlay)
    };
    let mut checker = TypeChecker::new();
    let (type_table, _type_root_catalog) = checker
        .initialize_instanced_context(&instanced.tree)
        .expect("the intact record graph produces a nominal table");
    let derived = instanced
        .tree
        .get_def_id_by_name("Derived")
        .expect("derived record declaration exists");
    instanced
        .tree
        .definitions
        .classes
        .get_mut("Derived")
        .and_then(|derived| derived.extends.first_mut())
        .expect("mutation seed has one record base")
        .base_def_id = None;
    seed_overlay_catalogs(&mut instanced.overlay);
    let roots_before = instanced.overlay.type_roots.clone();
    let enumerations_before = instanced.overlay.enumeration_type_roots.clone();
    let declarations_before = instanced.overlay.type_ids_by_def_id.clone();
    let error = checker
        .populate_overlay_type_roots(&instanced.tree, &mut instanced.overlay, &type_table)
        .expect_err("a required record base cannot lose exact declaration identity");
    assert!(
        matches!(
            error.as_ref(),
            TypeCheckError::MissingSourceContext { reason }
                if reason.contains("UnresolvedBaseIdentity")
                    && reason.contains(&format!("{derived:?}"))
        ),
        "unexpected refusal: {error:?}",
    );
    assert_eq!(instanced.overlay.type_roots, roots_before);
    assert_eq!(
        instanced.overlay.enumeration_type_roots,
        enumerations_before
    );
    assert_eq!(instanced.overlay.type_ids_by_def_id, declarations_before);
}

#[test]
fn non_record_required_base_refuses_atomic_catalog_install() {
    let source = r#"
        record Base
            Real inherited;
        end Base;
        record Derived
            extends Base;
            Boolean own;
        end Derived;
        function consume
            input Derived value;
            output Boolean result;
        algorithm
            result := value.own;
        end consume;
        model Test
            Derived value;
            Boolean result;
        equation
            result = consume(value);
        end Test;
    "#;
    let resolved = resolve(parse(source)).expect("record inheritance resolves before mutation");
    let mut instanced = {
        let tree = resolved.inner().clone();
        let overlay = match rumoca_phase_instantiate::instantiate_model_with_outcome(&tree, "Test")
        {
            rumoca_phase_instantiate::InstantiationOutcome::Success(overlay) => overlay,
            rumoca_phase_instantiate::InstantiationOutcome::NeedsInner {
                missing_inners, ..
            } => panic!("fixture unexpectedly needs inner declarations: {missing_inners:?}"),
            rumoca_phase_instantiate::InstantiationOutcome::Error(error) => {
                panic!("fixture instantiation failed: {error}")
            }
        };
        rumoca_ir_ast::InstancedTree::new(tree, overlay)
    };
    let mut checker = TypeChecker::new();
    let (type_table, _type_root_catalog) = checker
        .initialize_instanced_context(&instanced.tree)
        .expect("the intact record graph produces a nominal table");
    instanced
        .tree
        .definitions
        .classes
        .get_mut("Base")
        .expect("mutation seed retains its exact base declaration")
        .class_type = rumoca_core::ClassType::Model;

    seed_overlay_catalogs(&mut instanced.overlay);
    let roots_before = instanced.overlay.type_roots.clone();
    let enumerations_before = instanced.overlay.enumeration_type_roots.clone();
    let declarations_before = instanced.overlay.type_ids_by_def_id.clone();
    let error = checker
        .populate_overlay_type_roots(&instanced.tree, &mut instanced.overlay, &type_table)
        .expect_err("record inheritance cannot silently cross a non-record declaration");
    assert!(matches!(
        error.as_ref(),
        TypeCheckError::PhaseDiagnostic { code, message, .. }
            if code == "ET000" && message.contains("not a record class")
    ));
    assert_eq!(instanced.overlay.type_roots, roots_before);
    assert_eq!(
        instanced.overlay.enumeration_type_roots,
        enumerations_before
    );
    assert_eq!(instanced.overlay.type_ids_by_def_id, declarations_before);
}

#[test]
fn missing_record_field_alias_base_fails_in_resolve() {
    let source = r#"
        package P
            type Broken = MissingLibrary.Value;
            record Payload
                Broken value;
            end Payload;
            type Alias = Payload;
            function consume
                input Alias value;
                output Real result;
            algorithm
                result := 1.0;
            end consume;
        end P;
        package Q
            record Payload
                Real value;
            end Payload;
        end Q;
        model Test
            P.Alias value;
            Real result;
        equation
            result = P.consume(value);
        end Test;
    "#;
    let diagnostics = resolve(parse(source))
        .expect_err("a missing field alias base must fail before type-root construction");
    assert!(
        diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code.as_deref() == Some("ER003")
                && diagnostic.message.contains("MissingLibrary.Value")),
        "Resolve owns the exact nested alias-base refusal: {diagnostics:?}",
    );
}

#[test]
fn missing_exact_alias_record_refuses_despite_same_spelling_collision() {
    let source = r#"
        package P
            record Payload
                Real value;
            end Payload;
            type Alias = Payload;
            function consume
                input Alias value;
                output Real result;
            algorithm
                result := value.value;
            end consume;
        end P;
        package Q
            record Payload
                Real value;
            end Payload;
        end Q;
        model Test
            P.Alias value;
            Real result;
        equation
            result = P.consume(value);
        end Test;
    "#;
    let resolved = resolve(parse(source)).expect("the exact alias record graph resolves");
    let mut instanced = {
        let tree = resolved.inner().clone();
        let overlay = match rumoca_phase_instantiate::instantiate_model_with_outcome(&tree, "Test")
        {
            rumoca_phase_instantiate::InstantiationOutcome::Success(overlay) => overlay,
            rumoca_phase_instantiate::InstantiationOutcome::NeedsInner {
                missing_inners, ..
            } => panic!("fixture unexpectedly needs inner declarations: {missing_inners:?}"),
            rumoca_phase_instantiate::InstantiationOutcome::Error(error) => {
                panic!("fixture instantiation failed: {error}")
            }
        };
        rumoca_ir_ast::InstancedTree::new(tree, overlay)
    };
    let selected = instanced
        .tree
        .get_def_id_by_name("P.Payload")
        .expect("selected underlying record exists");
    let collision = instanced
        .tree
        .get_def_id_by_name("Q.Payload")
        .expect("same-spelling record collision exists");
    assert_ne!(selected, collision);
    let mut checker = TypeChecker::new();
    let (type_table, _type_root_catalog) = checker
        .initialize_instanced_context(&instanced.tree)
        .expect("the intact exact record graph produces a nominal table");
    instanced
        .tree
        .definitions
        .classes
        .get_mut("P")
        .and_then(|package| package.classes.shift_remove("Payload"))
        .expect("mutation removes only the selected underlying record");
    assert!(
        instanced.tree.get_class_by_def_id(collision).is_some(),
        "same-spelling collision remains available"
    );

    seed_overlay_catalogs(&mut instanced.overlay);
    let roots_before = instanced.overlay.type_roots.clone();
    let enumerations_before = instanced.overlay.enumeration_type_roots.clone();
    let declarations_before = instanced.overlay.type_ids_by_def_id.clone();
    let error = checker
        .populate_overlay_type_roots(&instanced.tree, &mut instanced.overlay, &type_table)
        .expect_err("a same-spelling record cannot replace the selected alias target");
    assert!(
        matches!(
            error.as_ref(),
            TypeCheckError::MissingSourceContext { reason }
                if reason.contains("MissingBase")
                    && reason.contains(&format!("{selected:?}"))
        ),
        "unexpected refusal: {error:?}",
    );
    assert_eq!(instanced.overlay.type_roots, roots_before);
    assert_eq!(
        instanced.overlay.enumeration_type_roots,
        enumerations_before
    );
    assert_eq!(instanced.overlay.type_ids_by_def_id, declarations_before);
}

#[test]
fn presentation_annotation_calls_are_not_runtime_signature_reachability() {
    let source = r#"
        function decoration
            output Real result;
        algorithm
            result := 1.0;
        end decoration;
        function helper
            input Real value;
            output Real result;
        algorithm
            result := decoration();
        end helper;
        model Test
            Real result;
        equation
            result = helper(value = 1.0);
        end Test;
    "#;
    let resolved = resolve(parse(source)).expect("runtime and named-argument calls resolve");
    let mut tree = resolved.inner().clone();
    let decoration = tree
        .get_def_id_by_name("decoration")
        .expect("annotation-only function declaration exists");
    let helper_declaration = tree
        .get_def_id_by_name("helper")
        .expect("runtime helper declaration exists");
    let helper = tree
        .definitions
        .classes
        .get_mut("helper")
        .expect("transitively used helper exists");
    let annotation = match &helper.algorithms[0][0] {
        rumoca_ir_ast::Statement::Assignment { value, .. } => value.clone(),
        other => panic!("expected one assignment call, got {other:?}"),
    };
    helper.algorithms[0][0] = rumoca_ir_ast::Statement::Empty;
    helper
        .components
        .get_mut("result")
        .expect("helper output exists")
        .annotation
        .push(annotation);
    let annotation_call = helper.components["result"]
        .annotation
        .first()
        .expect("resolved call moved into presentation metadata");
    assert!(matches!(
        annotation_call,
        Expression::FunctionCall { comp, .. } if comp.target_def_id() == Some(decoration)
    ));
    assert!(
        collect_class_function_declarations(helper)
            .expect("semantic helper body retains exact call identities")
            .iter()
            .all(|(declaration, _)| *declaration != decoration),
        "the presentation call is absent from runtime class reachability"
    );

    let mut overlay = match rumoca_phase_instantiate::instantiate_model_with_outcome(&tree, "Test")
    {
        rumoca_phase_instantiate::InstantiationOutcome::Success(overlay) => overlay,
        rumoca_phase_instantiate::InstantiationOutcome::NeedsInner { missing_inners, .. } => {
            panic!("fixture unexpectedly needs inner declarations: {missing_inners:?}")
        }
        rumoca_phase_instantiate::InstantiationOutcome::Error(error) => {
            panic!("fixture instantiation failed: {error}")
        }
    };
    let used = collect_overlay_function_declarations(&overlay)
        .expect("instantiated runtime calls retain exact identities");
    assert!(
        used.iter()
            .any(|(declaration, _)| *declaration == helper_declaration),
        "named-argument call retains the exact helper target"
    );
    assert!(
        used.iter()
            .all(|(declaration, _)| *declaration != decoration),
        "the authoritative overlay contains no runtime decoration call"
    );

    let mut checker = TypeChecker::new();
    let (type_table, _type_root_catalog) = checker
        .initialize_instanced_context(&tree)
        .expect("the nominal type graph is complete");
    checker.function_signatures.remove(&decoration);
    checker
        .populate_overlay_type_roots(&tree, &mut overlay, &type_table)
        .expect("presentation metadata and named-argument syntax do not invent runtime calls");
}

#[test]
fn connection_graph_operator_identities_are_predefined_call_targets() {
    let resolved = resolve(parse("model Test end Test;")).expect("minimal fixture resolves");
    let mut declarations = Vec::new();
    for role in rumoca_core::ConnectionGraphOperatorRole::ALL {
        let path = ComponentPath::from_parts(role.predefined_path());
        let declaration = resolved
            .scope_tree
            .predefined_member(&path)
            .unwrap_or_else(|| panic!("missing predefined {path}"));
        assert_eq!(
            resolved.semantic_catalogs().connections().declaration(role),
            declaration,
            "Resolve must issue the exact predefined identity for {path}"
        );
        assert!(
            !declarations.contains(&declaration),
            "connection-graph roles must have five distinct declaration identities"
        );
        declarations.push(declaration);
    }
    let tree = resolved.inner().clone();
    let semantic_catalogs = semantic_catalog_projection_for_test(&tree)
        .expect("resolved fixture has exact semantic identities");
    for role in rumoca_core::ConnectionGraphOperatorRole::ALL {
        let path = ComponentPath::from_parts(role.predefined_path());
        let declaration = tree
            .scope_tree
            .predefined_member(&path)
            .unwrap_or_else(|| panic!("missing predefined {path}"));
        assert!(
            is_predefined_function_declaration(&tree, &semantic_catalogs, declaration,),
            "the exact predefined {path} identity must not require a user class body"
        );
    }
}
