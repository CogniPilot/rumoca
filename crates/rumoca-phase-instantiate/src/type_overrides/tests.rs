//! Unit tests for redeclaration and virtual-class selection.

use super::component_type_selection::apply_type_override;
use super::override_collection::build_type_override_map;
use super::override_map::TypeOverrideMap;
use super::redeclare_values::resolve_cref_def_id;
use miette::Diagnostic;
use rumoca_core::DefId;
use rumoca_ir_ast as ast;
use std::sync::Arc;

fn make_token(text: &str) -> rumoca_core::Token {
    rumoca_core::Token {
        text: Arc::from(text),
        location: rumoca_core::Location::default(),
        token_number: 0,
        token_type: 0,
    }
}

fn make_name(text: &str) -> ast::Name {
    ast::Name::from_string(text)
}

fn make_comp_ref(names: &[&str]) -> ast::ComponentReference {
    ast::ComponentReference {
        local: false,
        parts: names
            .iter()
            .map(|name| ast::ComponentRefPart {
                ident: make_token(name),
                subs: None,
                def_id: None,
            })
            .collect(),
        span: test_span(),
        qualified_display_name: None,
    }
}

fn test_span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("type_overrides_test.mo"),
        1,
        2,
    )
}

const COMPONENT_REDECLARE_SOURCE: &str = r"
package Constraint
  constant Real k = 10.0;
end Constraint;

package Good
  extends Constraint(k = 20.0);
end Good;

package Bad
  constant Real k = 30.0;
end Bad;

model Inner
  replaceable package Medium = Constraint
    constrainedby Constraint;
  Real y = Medium.k;
end Inner;

model FinalInner
  final package Medium = Constraint;
  Real y = Medium.k;
end FinalInner;

model NonReplaceableInner
  package Medium = Constraint;
  Real y = Medium.k;
end NonReplaceableInner;

model ComponentGood
  Inner i(redeclare package Medium = Good);
end ComponentGood;

model ComponentReplaceableGood
  Inner i(replaceable package Medium = Good);
end ComponentReplaceableGood;

model ComponentExplicit
  Inner i(redeclare package Medium = Good(k = 25.0));
end ComponentExplicit;

model ComponentBad
  Inner i(redeclare package Medium = Bad);
end ComponentBad;

model ExtendsBad
  extends Inner(redeclare package Medium = Bad);
end ExtendsBad;

model ComponentFinal
  FinalInner i(redeclare package Medium = Good);
end ComponentFinal;

model ComponentNonReplaceable
  NonReplaceableInner i(redeclare package Medium = Good);
end ComponentNonReplaceable;

model ComponentWithoutRedeclare
  Inner i(Medium = Good);
end ComponentWithoutRedeclare;

model ComponentClassModification
  Inner i(Medium(k = 15.0));
end ComponentClassModification;
";

fn resolved_component_redeclare_tree() -> ast::ClassTree {
    let file_name = "<component_redeclare_test>";
    let stored = rumoca_phase_parse::parse_to_ast(COMPONENT_REDECLARE_SOURCE, file_name)
        .expect("component redeclare fixture should parse");
    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map.add(file_name, COMPONENT_REDECLARE_SOURCE);
    rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree))
        .expect("component redeclare fixture should resolve")
        .inner()
        .clone()
}

fn resolved_tree(source: &str) -> ast::ClassTree {
    let file_name = "<dynamic_type_identity_test>";
    let stored = rumoca_phase_parse::parse_to_ast(source, file_name)
        .expect("dynamic type identity fixture should parse");
    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map.add(file_name, source);
    rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree))
        .expect("dynamic type identity fixture should resolve")
        .inner()
        .clone()
}

#[test]
fn override_collection_traverses_more_than_thirty_two_exact_bases() {
    let mut source = String::from("model C0\n  replaceable package Marker end Marker;\nend C0;\n");
    for index in 1..=40 {
        source.push_str(&format!(
            "model C{index}\n  extends C{};\nend C{index};\n",
            index - 1,
        ));
    }
    let tree = resolved_tree(&source);
    let root = tree
        .get_class_by_qualified_name("C40")
        .expect("deep derived class");
    let marker_def_id = tree
        .get_class_by_qualified_name("C0.Marker")
        .and_then(|class| class.def_id)
        .expect("deep inherited nested alias identity");
    let mut overrides = TypeOverrideMap::new();

    super::override_collection::collect_nested_overrides_in_extends_chain(
        &tree,
        root,
        None,
        &mut overrides,
    )
    .expect("exact traversal has no arbitrary depth cutoff");
    assert_eq!(
        overrides.target_for_alias_def_id(marker_def_id),
        Some(marker_def_id),
    );
}

#[test]
fn override_collection_treats_exact_predefined_edge_as_terminal() {
    let tree = resolved_tree("type Voltage = Real;");
    let root = tree
        .get_class_by_qualified_name("Voltage")
        .expect("type alias identity");
    let mut overrides = TypeOverrideMap::new();

    super::override_collection::collect_nested_overrides_in_extends_chain(
        &tree,
        root,
        None,
        &mut overrides,
    )
    .expect("exact predefined edge is a valid terminal");

    assert!(overrides.is_empty());
}

#[test]
fn override_collection_rejects_missing_nested_and_rhs_identities() {
    let mut missing_nested = crate::test_support::ResolvedFixture::parse(
        "missing_nested_identity.mo",
        "model Root\n  replaceable package Marker end Marker;\nend Root;",
    );
    missing_nested.remove_class_def_id("Root.Marker");
    let root = missing_nested
        .tree()
        .get_class_by_qualified_name("Root")
        .expect("missing-nested-id root");
    let nested_error = build_type_override_map(missing_nested.tree(), root, None)
        .expect_err("nested override without DefId must fail");
    assert!(matches!(
        *nested_error,
        crate::InstantiateError::RedeclareError { .. }
    ));

    let mut missing_rhs = resolved_component_redeclare_tree();
    let ext_mod =
        &mut missing_rhs.definitions.classes["ExtendsBad"].extends[0].modifications[0].expr;
    let ast::Expression::Modification {
        value: Some(value), ..
    } = ext_mod
    else {
        panic!("expected resolved extends redeclare modification");
    };
    let ast::Expression::ClassModification { target, .. } = Arc::make_mut(value) else {
        panic!("expected resolved extends redeclare RHS");
    };
    target.set_root_def_id(None);
    target.set_target_def_id(None);
    let derived = missing_rhs
        .get_class_by_qualified_name("ExtendsBad")
        .expect("missing-RHS-id class");
    let rhs_error = build_type_override_map(&missing_rhs, derived, None)
        .expect_err("recognized redeclare RHS without DefId must fail");
    assert!(matches!(
        *rhs_error,
        crate::InstantiateError::MissingResolvedIdentity { .. }
    ));
}

#[test]
fn override_collection_rejects_missing_exact_lhs_identity() {
    let mut tree = resolved_component_redeclare_tree();
    let ext_mod = &mut tree.definitions.classes["ExtendsBad"].extends[0].modifications[0].expr;
    let ast::Expression::Modification { target, .. } = ext_mod else {
        panic!("expected resolved extends redeclare modification");
    };
    target.set_root_def_id(None);
    target.set_target_def_id(None);
    let derived = tree
        .get_class_by_qualified_name("ExtendsBad")
        .expect("derived class");

    let error = build_type_override_map(&tree, derived, None)
        .expect_err("redeclare LHS without exact identity must fail");
    assert!(
        error
            .to_string()
            .contains("LHS has no exact resolved DefId")
    );
}

#[test]
fn override_collection_uses_exact_lhs_among_same_spelled_inherited_slots() {
    let mut tree = resolved_component_redeclare_tree();
    let other_id = DefId::new(89_100);
    let other_medium_id = DefId::new(89_101);
    let mut other = ast::ClassDef {
        name: make_token("Other"),
        def_id: Some(other_id),
        ..Default::default()
    };
    other.classes.insert(
        "Medium".to_string(),
        ast::ClassDef {
            name: make_token("Medium"),
            def_id: Some(other_medium_id),
            is_replaceable: true,
            ..Default::default()
        },
    );
    tree.name_map.insert("Other".to_string(), other_id);
    tree.def_map.insert(other_id, "Other".to_string());
    tree.def_map
        .insert(other_medium_id, "Other.Medium".to_string());
    tree.definitions.classes.insert("Other".to_string(), other);
    tree.definitions.classes["ExtendsBad"]
        .extends
        .push(ast::Extend {
            base_name: ast::Name {
                name: vec![make_token("Other")],
                def_id: Some(other_id),
            },
            base_def_id: Some(other_id),
            ..Default::default()
        });
    let selected_alias_id = tree
        .get_class_by_qualified_name("Inner.Medium")
        .and_then(|class| class.def_id)
        .expect("original inherited Medium slot");
    let expected_target_id = tree
        .get_class_by_qualified_name("Bad")
        .and_then(|class| class.def_id)
        .expect("replacement identity");
    let derived = tree
        .get_class_by_qualified_name("ExtendsBad")
        .expect("derived class");

    let overrides = build_type_override_map(&tree, derived, None)
        .expect("exact LHS selects one of the same-spelled inherited slots");
    assert_eq!(
        overrides.target_for_alias_def_id(selected_alias_id),
        Some(expected_target_id)
    );
    assert_ne!(selected_alias_id, other_medium_id);

    let mut mismatched = tree.clone();
    let ast::Expression::Modification { target, .. } =
        &mut mismatched.definitions.classes["ExtendsBad"].extends[0].modifications[0].expr
    else {
        panic!("expected resolved extends redeclare modification");
    };
    target.set_root_def_id(Some(other_medium_id));
    target.set_target_def_id(Some(other_medium_id));
    let derived = mismatched
        .get_class_by_qualified_name("ExtendsBad")
        .expect("derived class");
    let error = build_type_override_map(&mismatched, derived, None)
        .expect_err("an exact slot from a different extends edge must be refused");
    assert!(matches!(
        *error,
        crate::InstantiateError::RedeclareError { .. }
    ));
}

#[test]
fn override_collection_rejects_missing_exact_alias_slot() {
    let mut fixture = crate::test_support::ResolvedFixture::parse(
        "missing_exact_alias_slot.mo",
        COMPONENT_REDECLARE_SOURCE,
    );
    fixture.tree_mut().definitions.classes["Inner"]
        .classes
        .shift_remove("Medium");
    let derived = fixture
        .tree()
        .get_class_by_qualified_name("ExtendsBad")
        .expect("missing-alias-slot class");
    let error = build_type_override_map(fixture.tree(), derived, None)
        .expect_err("redeclare without exact inherited slot must fail");
    assert!(matches!(
        *error,
        crate::InstantiateError::RedeclareError { .. }
    ));
}

#[test]
fn direct_redeclare_is_not_discarded_as_forwarding_at_extraction_boundary() {
    let tree = resolved_component_redeclare_tree();
    let component = tree
        .get_class_by_qualified_name("ComponentGood")
        .and_then(|class| class.components.get("i"))
        .expect("component redeclare occurrence");
    let alias_def_id = tree
        .get_class_by_qualified_name("Inner.Medium")
        .and_then(|class| class.def_id)
        .expect("replaceable package alias identity");
    let source = component
        .source_modifications
        .first()
        .expect("source redeclare modifier");
    let resolved = component
        .modifications
        .get("Medium")
        .expect("resolved redeclare modifier");
    assert!(!super::redeclare_modifiers::is_forwarding_component_redeclare(source, "Medium"));
    assert!(!super::redeclare_modifiers::is_forwarding_component_redeclare(resolved, "Medium"));

    assert!(
        super::checked_source_forwarding_witness(super::SourceForwardingEvidence {
            tree: &tree,
            type_overrides: &TypeOverrideMap::new(),
            is_redeclare: component.source_modification_redeclare_flags[0],
            source,
            resolved,
            target_name: "Medium",
            alias_def_id,
        })
        .expect("non-forwarding evidence is well formed")
        .is_none(),
    );
    let target_class = tree
        .get_class_by_qualified_name("Inner")
        .expect("component target class");
    let expected_target = tree
        .get_class_by_qualified_name("Good")
        .and_then(|class| class.def_id)
        .expect("direct redeclare target identity");
    let extracted = super::component_class_overrides::extract_component_class_overrides(
        &tree,
        component,
        Some(target_class),
        None,
        &TypeOverrideMap::new(),
    )
    .expect("direct redeclare extraction");
    assert_eq!(
        extracted
            .get(&alias_def_id)
            .map(|entry| entry.target_def_id),
        Some(expected_target),
    );
    let mut unmarked = component.clone();
    unmarked.source_modification_redeclare_flags[0] = false;
    let unmarked_error = super::component_class_overrides::extract_component_class_overrides(
        &tree,
        &unmarked,
        Some(target_class),
        None,
        &TypeOverrideMap::new(),
    )
    .expect_err("an explicit class replacement without redeclare must fail");
    assert!(
        unmarked_error
            .to_string()
            .contains("requires the `redeclare` keyword")
    );

    let mut mismatched_catalog = component.clone();
    mismatched_catalog
        .source_modification_redeclare_flags
        .clear();
    let catalog_error = super::component_class_overrides::extract_component_class_overrides(
        &tree,
        &mismatched_catalog,
        Some(target_class),
        None,
        &TypeOverrideMap::new(),
    )
    .expect_err("source modifier and redeclare-flag catalogs must remain aligned");
    assert!(
        catalog_error
            .to_string()
            .contains("lost their redeclare metadata")
    );
}

#[test]
fn forwarding_witness_refuses_same_spelling_with_different_lhs_identity() {
    let active_slot_id = DefId::new(89_000);
    let other_slot_id = DefId::new(89_001);
    let rhs_alias_id = DefId::new(89_002);
    let reference = |def_id| ast::ComponentReference {
        local: false,
        parts: vec![ast::ComponentRefPart {
            ident: make_token("Medium"),
            subs: None,
            def_id: Some(def_id),
        }],
        span: test_span(),
        qualified_display_name: None,
    };
    let source = ast::Expression::ClassModification {
        target: reference(other_slot_id),
        modifications: Vec::new(),
        each_flags: Vec::new(),
        final_flags: Vec::new(),
        redeclare_flags: Vec::new(),
        span: test_span(),
    };
    let resolved = ast::Expression::ClassModification {
        target: reference(rhs_alias_id),
        modifications: Vec::new(),
        each_flags: Vec::new(),
        final_flags: Vec::new(),
        redeclare_flags: Vec::new(),
        span: test_span(),
    };

    let tree = ast::ClassTree::default();
    let type_overrides = TypeOverrideMap::new();
    let error = super::checked_source_forwarding_witness(super::SourceForwardingEvidence {
        tree: &tree,
        type_overrides: &type_overrides,
        is_redeclare: true,
        source: &source,
        resolved: &resolved,
        target_name: "Medium",
        alias_def_id: active_slot_id,
    })
    .expect_err("source spelling cannot substitute for exact LHS identity");
    assert!(matches!(
        error,
        super::SourceForwardingEvidenceError::MismatchedResolvedLhsIdentity {
            expected,
            found,
        } if expected == active_slot_id && found == other_slot_id
    ));
}

#[test]
fn explicit_mapped_rhs_alias_cannot_be_reclassified_as_self_forwarding() {
    let tree = resolved_tree(
        r"
record R
  Real x;
  replaceable function equalityConstraint
    input R a;
    input R b;
    output Real residue[1];
  end equalityConstraint;
end R;
function AlternateConstraint
  extends R.equalityConstraint;
end AlternateConstraint;
model M
  replaceable function Choice = AlternateConstraint
    constrainedby R.equalityConstraint;
  R selected(redeclare function equalityConstraint = Choice);
end M;
",
    );
    let owner = tree
        .get_class_by_qualified_name("M")
        .expect("component owner class");
    let component = owner
        .components
        .get("selected")
        .expect("explicit mapped-RHS redeclare occurrence");
    let record = tree
        .get_class_by_qualified_name("R")
        .expect("record target class");
    let alias_def_id = tree
        .get_class_by_qualified_name("R.equalityConstraint")
        .and_then(|class| class.def_id)
        .expect("equalityConstraint slot identity");
    let rhs_alias_def_id = tree
        .get_class_by_qualified_name("M.Choice")
        .and_then(|class| class.def_id)
        .expect("mapped RHS alias identity");
    let type_overrides =
        build_type_override_map(&tree, owner, None).expect("mapped RHS override catalog");
    assert!(
        type_overrides
            .target_for_alias_def_id(rhs_alias_def_id)
            .is_some(),
        "mutation requires an exact active DefId mapping for the explicit RHS alias",
    );
    let source = component
        .source_modifications
        .first()
        .expect("source function redeclare");
    let resolved = component
        .modifications
        .get("equalityConstraint")
        .expect("resolved function redeclare");
    assert!(
        !super::redeclare_modifiers::is_forwarding_component_redeclare(
            source,
            "equalityConstraint",
        ),
        "source-ordered evidence must preserve the explicit RHS spelling",
    );
    assert!(
        !super::redeclare_modifiers::is_forwarding_component_redeclare(
            resolved,
            "equalityConstraint",
        )
    );
    assert!(
        super::checked_source_forwarding_witness(super::SourceForwardingEvidence {
            tree: &tree,
            type_overrides: &type_overrides,
            is_redeclare: component.source_modification_redeclare_flags[0],
            source,
            resolved,
            target_name: "equalityConstraint",
            alias_def_id,
        })
        .expect("explicit mapped RHS evidence is well formed")
        .is_none(),
    );
    let extracted = super::component_class_overrides::extract_component_class_overrides(
        &tree,
        component,
        Some(record),
        None,
        &type_overrides,
    )
    .expect("explicit mapped RHS remains a direct extracted override");
    assert_eq!(
        extracted
            .get(&alias_def_id)
            .map(|entry| entry.target_def_id),
        Some(rhs_alias_def_id),
    );
}

#[test]
fn equality_direct_redeclare_extractor_retains_selected_function_identity() {
    let tree = resolved_tree(
        r"
record R
  Real x;
  replaceable function equalityConstraint
    input R a;
    input R b;
    output Real residue[2];
  end equalityConstraint;
end R;
function AlternateConstraint
  extends R.equalityConstraint;
end AlternateConstraint;
model M
  R ordinary;
  R alternate(redeclare function equalityConstraint = AlternateConstraint);
end M;
",
    );
    let component = tree
        .get_class_by_qualified_name("M")
        .and_then(|class| class.components.get("alternate"))
        .expect("equalityConstraint redeclare occurrence");
    let record = tree
        .get_class_by_qualified_name("R")
        .expect("record target class");
    let slot_def_id = tree
        .get_class_by_qualified_name("R.equalityConstraint")
        .and_then(|class| class.def_id)
        .expect("equalityConstraint slot identity");
    let selected_def_id = tree
        .get_class_by_qualified_name("AlternateConstraint")
        .and_then(|class| class.def_id)
        .expect("selected equalityConstraint function identity");
    let source = component
        .source_modifications
        .first()
        .expect("source equalityConstraint redeclare");
    let resolved = component
        .modifications
        .get("equalityConstraint")
        .expect("resolved equalityConstraint redeclare");
    assert!(
        !super::redeclare_modifiers::is_forwarding_component_redeclare(
            source,
            "equalityConstraint",
        )
    );
    assert!(
        !super::redeclare_modifiers::is_forwarding_component_redeclare(
            resolved,
            "equalityConstraint",
        )
    );
    let extracted = super::component_class_overrides::extract_component_class_overrides(
        &tree,
        component,
        Some(record),
        None,
        &TypeOverrideMap::new(),
    )
    .expect("direct equalityConstraint extraction");
    assert_eq!(
        extracted.get(&slot_def_id).map(|entry| entry.target_def_id),
        Some(selected_def_id),
    );
    let owner = tree
        .get_class_by_qualified_name("M")
        .expect("component owner class");
    let type_overrides =
        build_type_override_map(&tree, owner, None).expect("owner type override catalog");
    let (propagated, has_forwarding, _, _) =
        crate::nested_scope::resolve_component_nested_type_overrides(
            &tree,
            component,
            Some(record),
            &ast::ModificationEnvironment::new(),
            &type_overrides,
        )
        .expect("direct equalityConstraint override propagation")
        .into_parts();
    assert!(!has_forwarding);
    assert_eq!(
        propagated
            .get(&slot_def_id)
            .map(|entry| entry.target_def_id),
        Some(selected_def_id),
    );
}

const SELF_REDECLARE_FORWARDING_SOURCE: &str = r"
package P
  partial package PartialMedium end PartialMedium;
  package MediumB
    extends PartialMedium;
  end MediumB;
  package IncompatibleMedium end IncompatibleMedium;
  model Holder
    replaceable package Medium = PartialMedium constrainedby PartialMedium;
  end Holder;
  model Layer
    replaceable package Medium = PartialMedium constrainedby PartialMedium;
    Holder holder(redeclare package Medium = Medium);
  end Layer;
end P;
";

struct SelfRedeclareForwardingFixture {
    tree: ast::ClassTree,
    component: ast::Component,
    alias_def_id: DefId,
    forwarding_alias_def_id: DefId,
    expected_target: DefId,
    incompatible_target: DefId,
    type_overrides: TypeOverrideMap,
}

fn self_redeclare_forwarding_fixture() -> SelfRedeclareForwardingFixture {
    let tree = resolved_tree(SELF_REDECLARE_FORWARDING_SOURCE);
    let component = tree
        .get_class_by_qualified_name("P.Layer")
        .and_then(|class| class.components.get("holder"))
        .expect("forwarding redeclare occurrence")
        .clone();
    let alias_def_id = tree
        .get_class_by_qualified_name("P.Holder.Medium")
        .and_then(|class| class.def_id)
        .expect("replaceable package alias identity");
    let layer = tree
        .get_class_by_qualified_name("P.Layer")
        .expect("forwarding owner class");
    let mut type_overrides =
        build_type_override_map(&tree, layer, None).expect("forwarding override catalog");
    let resolved = component
        .modifications
        .get("Medium")
        .expect("resolved forwarding redeclare");
    let forwarding_alias_def_id =
        super::redeclare_values::resolve_redeclare_value_def_id(&tree, resolved, None)
            .expect("forwarding identity resolution succeeds")
            .expect("resolved enclosing forwarding alias identity");
    let expected_target = tree
        .get_class_by_qualified_name("P.MediumB")
        .and_then(|class| class.def_id)
        .expect("compatible effective forwarding target");
    let incompatible_target = tree
        .get_class_by_qualified_name("P.IncompatibleMedium")
        .and_then(|class| class.def_id)
        .expect("incompatible forwarding target identity");
    type_overrides.insert_alias(forwarding_alias_def_id, expected_target);

    SelfRedeclareForwardingFixture {
        tree,
        component,
        alias_def_id,
        forwarding_alias_def_id,
        expected_target,
        incompatible_target,
        type_overrides,
    }
}

fn assert_checked_self_redeclare_forwarding_witness(
    fixture: &SelfRedeclareForwardingFixture,
    source: &ast::Expression,
    resolved: &ast::Expression,
) {
    let witness = super::checked_source_forwarding_witness(super::SourceForwardingEvidence {
        tree: &fixture.tree,
        type_overrides: &fixture.type_overrides,
        is_redeclare: fixture.component.source_modification_redeclare_flags[0],
        source,
        resolved,
        target_name: "Medium",
        alias_def_id: fixture.alias_def_id,
    })
    .expect("forwarding evidence is well formed")
    .expect("checked forwarding witness");
    assert_eq!(witness.lhs_slot_def_id(), fixture.alias_def_id);
    assert_eq!(witness.rhs_alias_def_id(), fixture.forwarding_alias_def_id);
    assert_eq!(witness.effective_target_def_id(), fixture.expected_target);
}

#[test]
fn self_redeclare_is_deferred_as_forwarding_at_extraction_boundary() {
    let fixture = self_redeclare_forwarding_fixture();
    let component = &fixture.component;
    let source = component
        .source_modifications
        .first()
        .expect("identity-bearing source forwarding redeclare");
    let resolved = component
        .modifications
        .get("Medium")
        .expect("resolved forwarding redeclare");

    assert_checked_self_redeclare_forwarding_witness(&fixture, source, resolved);
    let holder = fixture
        .tree
        .get_class_by_qualified_name("P.Holder")
        .expect("forwarding component target class");
    let extracted = super::component_class_overrides::extract_component_class_overrides(
        &fixture.tree,
        component,
        Some(holder),
        None,
        &fixture.type_overrides,
    )
    .expect("forwarding extraction");
    assert!(extracted.is_empty());

    // The RHS identity was issued by Resolve in the modifier's declaring
    // scope. An active modifier with the same presentation name must not be a
    // second lookup authority: following it here turns legal `Medium = Medium`
    // forwarding into a fabricated self-cycle.
    let mut same_name_env = ast::ModificationEnvironment::new();
    same_name_env.add(
        ast::QualifiedName::from_ident("Medium"),
        ast::ModificationValue::simple(resolved.clone()),
    );
    let extracted_with_same_name_env =
        super::component_class_overrides::extract_component_class_overrides(
            &fixture.tree,
            component,
            Some(holder),
            Some(&same_name_env),
            &fixture.type_overrides,
        )
        .expect("Resolve-issued RHS identity is independent of same-name modifier state");
    assert!(extracted_with_same_name_env.is_empty());

    let mut missing_normalized = component.clone();
    missing_normalized.modifications.shift_remove("Medium");
    let missing_normalized_error =
        super::component_class_overrides::extract_component_class_overrides(
            &fixture.tree,
            &missing_normalized,
            Some(holder),
            None,
            &fixture.type_overrides,
        )
        .expect_err("a source redeclare cannot substitute for missing normalized semantics");
    assert!(
        missing_normalized_error
            .to_string()
            .contains("no normalized semantic modifier")
    );

    let mut incompatible_overrides = fixture.type_overrides.clone();
    incompatible_overrides
        .insert_alias(fixture.forwarding_alias_def_id, fixture.incompatible_target);
    let incompatible_error = match crate::nested_scope::resolve_component_nested_type_overrides(
        &fixture.tree,
        component,
        Some(holder),
        &ast::ModificationEnvironment::new(),
        &incompatible_overrides,
    ) {
        Err(error) => error,
        Ok(_) => panic!("incompatible exact forwarding target must be rejected"),
    };
    assert!(matches!(
        *incompatible_error,
        crate::InstantiateError::RedeclareConstraintViolation { .. }
    ));

    let (propagated, has_forwarding, _, _) =
        crate::nested_scope::resolve_component_nested_type_overrides(
            &fixture.tree,
            component,
            Some(holder),
            &ast::ModificationEnvironment::new(),
            &fixture.type_overrides,
        )
        .expect("forwarding propagation")
        .into_parts();
    assert!(has_forwarding);
    assert_eq!(
        propagated
            .get(&fixture.alias_def_id)
            .map(|entry| entry.target_def_id),
        Some(fixture.expected_target),
    );
}

#[test]
fn qualified_type_exposure_keeps_each_package_redeclare_instance_local() {
    let source = r"
partial package PartialRotation
  replaceable record Orientation
    Real marker[0];
  end Orientation;
end PartialRotation;

package Quaternion
  extends PartialRotation;
  redeclare record extends Orientation
    Real q[4];
  end Orientation;
end Quaternion;

package Mrp
  extends PartialRotation;
  redeclare record extends Orientation
    Real r[3];
  end Orientation;
end Mrp;

package Generic
  replaceable package Rotation = Quaternion constrainedby PartialRotation;
  record Element
    Rotation.Orientation rotation;
  end Element;
end Generic;

package WithQuaternion
  extends Generic(redeclare package Rotation = Quaternion);
end WithQuaternion;

package WithMrp
  extends Generic(redeclare package Rotation = Mrp);
end WithMrp;

model Root
  WithQuaternion.Element quaternion;
  WithMrp.Element mrp;
end Root;
";
    let tree = resolved_tree(source);
    let overlay = match crate::instantiate_model_with_outcome(&tree, "Root") {
        crate::InstantiationOutcome::Success(overlay) => overlay,
        crate::InstantiationOutcome::NeedsInner { missing_inners, .. } => {
            panic!("fixture unexpectedly needs inner declarations: {missing_inners:?}")
        }
        crate::InstantiationOutcome::Error(error) => panic!("fixture failed: {error}"),
    };
    let paths = overlay
        .components
        .values()
        .map(|component| component.qualified_name.to_flat_string())
        .collect::<std::collections::HashSet<_>>();

    assert!(paths.contains("quaternion.rotation.q"));
    assert!(!paths.contains("quaternion.rotation.r"));
    assert!(paths.contains("mrp.rotation.r"));
    assert!(!paths.contains("mrp.rotation.q"));
}

fn instantiate_component_redeclare_error(model: &str) -> Box<crate::InstantiateError> {
    match crate::instantiate_model_with_outcome(&resolved_component_redeclare_tree(), model) {
        crate::InstantiationOutcome::Error(error) => error,
        crate::InstantiationOutcome::NeedsInner { missing_inners, .. } => panic!(
            "component redeclare fixture unexpectedly needs inner declarations: {missing_inners:?}"
        ),
        crate::InstantiationOutcome::Success(_) => {
            panic!("component redeclare fixture should fail")
        }
    }
}

fn diagnostic_code(error: &crate::InstantiateError) -> Option<String> {
    error.code().map(|code| code.to_string())
}

#[test]
fn component_redeclare_is_source_marked_and_selects_the_resolved_target() {
    let tree = resolved_component_redeclare_tree();
    let component_declaration = tree
        .get_class_by_qualified_name("ComponentGood")
        .and_then(|class| class.components.get("i"))
        .expect("component declaration i");
    assert_eq!(
        component_declaration.source_modification_redeclare_flags,
        vec![true]
    );
    let overlay = match crate::instantiate_model_with_outcome(&tree, "ComponentGood") {
        crate::InstantiationOutcome::Success(overlay) => overlay,
        crate::InstantiationOutcome::NeedsInner { missing_inners, .. } => panic!(
            "component redeclare fixture unexpectedly needs inner declarations: {missing_inners:?}"
        ),
        crate::InstantiationOutcome::Error(error) => {
            panic!("valid component redeclare failed: {error}")
        }
    };
    let component = overlay
        .components
        .values()
        .find(|component| component.qualified_name.to_flat_string() == "i")
        .expect("component instance i");
    let class_override = component
        .class_overrides
        .values()
        .find(|class_override| class_override.alias == "Medium")
        .expect("source-marked Medium redeclare");

    assert_eq!(
        tree.def_map.get(&class_override.target_def_id),
        Some(&"Good".to_string())
    );
    assert!(class_override.modifier_args.is_empty());
}

#[test]
fn replaceable_component_modifier_is_a_source_marked_redeclare() {
    let tree = resolved_component_redeclare_tree();
    let component_declaration = tree
        .get_class_by_qualified_name("ComponentReplaceableGood")
        .and_then(|class| class.components.get("i"))
        .expect("component declaration i");
    assert_eq!(
        component_declaration.source_modification_redeclare_flags,
        vec![true]
    );
    let overlay = match crate::instantiate_model_with_outcome(&tree, "ComponentReplaceableGood") {
        crate::InstantiationOutcome::Success(overlay) => overlay,
        crate::InstantiationOutcome::NeedsInner { missing_inners, .. } => panic!(
            "replaceable modifier fixture unexpectedly needs inner declarations: {missing_inners:?}"
        ),
        crate::InstantiationOutcome::Error(error) => {
            panic!("replaceable modifier failed: {error}")
        }
    };
    let class_override = overlay
        .components
        .values()
        .find(|component| component.qualified_name.to_flat_string() == "i")
        .and_then(|component| {
            component
                .class_overrides
                .values()
                .find(|class_override| class_override.alias == "Medium")
        })
        .expect("source-marked Medium redeclare");

    assert_eq!(
        tree.def_map.get(&class_override.target_def_id),
        Some(&"Good".to_string())
    );
}

#[test]
fn component_redeclare_rejects_constraining_type_violation() {
    let component_error = instantiate_component_redeclare_error("ComponentBad");
    let extends_error = instantiate_component_redeclare_error("ExtendsBad");
    assert_eq!(
        diagnostic_code(&component_error),
        Some("rumoca::instantiate::EI027".to_string())
    );
    assert_eq!(
        diagnostic_code(&extends_error),
        Some("rumoca::instantiate::EI027".to_string())
    );
}

#[test]
fn component_redeclare_preserves_explicit_replacement_modifiers() {
    let tree = resolved_component_redeclare_tree();
    let overlay = match crate::instantiate_model_with_outcome(&tree, "ComponentExplicit") {
        crate::InstantiationOutcome::Success(overlay) => overlay,
        crate::InstantiationOutcome::NeedsInner { missing_inners, .. } => panic!(
            "modified redeclare fixture unexpectedly needs inner declarations: {missing_inners:?}"
        ),
        crate::InstantiationOutcome::Error(error) => {
            panic!("valid modified component redeclare failed: {error}")
        }
    };
    let class_override = overlay
        .components
        .values()
        .find(|component| component.qualified_name.to_flat_string() == "i")
        .and_then(|component| {
            component
                .class_overrides
                .values()
                .find(|class_override| class_override.alias == "Medium")
        })
        .expect("source-marked Medium redeclare");

    assert_eq!(class_override.modifier_args.len(), 1);
    let ast::Expression::Modification { target, .. } = &class_override.modifier_args[0] else {
        panic!("explicit replacement modifier should remain a modification");
    };
    assert_eq!(target.to_string(), "k");
    assert_eq!(target.root_def_id(), target.target_def_id());
    assert_eq!(
        target
            .target_def_id()
            .and_then(|def_id| tree.def_map.get(&def_id)),
        Some(&"Constraint.k".to_string()),
        "instantiation must resolve the modification against the selected package hierarchy"
    );
}

#[test]
fn dotted_type_under_redeclared_package_materializes_exact_instance_identity() {
    let source = r"
package P
  type VoltageA = Real;
  type VoltageB = Real;
  partial package PartialPhaseSystem
    replaceable type Voltage = VoltageA constrainedby Real;
  end PartialPhaseSystem;
  package TwoConductor
    extends PartialPhaseSystem(redeclare type Voltage = VoltageB);
  end TwoConductor;
  connector Terminal
    replaceable package PhaseSystem = PartialPhaseSystem
      constrainedby PartialPhaseSystem;
    PhaseSystem.Voltage v;
  end Terminal;
  model Test
    Terminal term(redeclare package PhaseSystem = TwoConductor);
  end Test;
end P;
";
    let tree = resolved_tree(source);
    let overlay = match crate::instantiate_model_with_outcome(&tree, "P.Test") {
        crate::InstantiationOutcome::Success(overlay) => overlay,
        crate::InstantiationOutcome::NeedsInner { missing_inners, .. } => {
            panic!("package fixture unexpectedly needs inner declarations: {missing_inners:?}")
        }
        crate::InstantiationOutcome::Error(error) => panic!("package fixture failed: {error}"),
    };
    let voltage = overlay
        .components
        .values()
        .find(|component| component.qualified_name.to_flat_string() == "term.v")
        .expect("terminal voltage occurrence");
    let expected_type = tree
        .get_class_by_qualified_name("P.VoltageB")
        .and_then(|class| class.def_id)
        .expect("selected voltage declaration identity");
    let source_alias = tree
        .get_class_by_qualified_name("P.Terminal.PhaseSystem")
        .and_then(|class| class.def_id)
        .expect("replaceable package declaration identity");

    assert_eq!(voltage.type_name, "PhaseSystem.Voltage");
    assert_eq!(voltage.type_reference_root_def_id, Some(source_alias));
    assert_eq!(voltage.type_def_id, Some(expected_type));
}

#[test]
fn forwarded_package_redeclare_resolves_all_instance_section_references() {
    let source = r"
package P
  partial package PartialMedium
    constant Real k = 2;
  end PartialMedium;
  package MediumB
    extends PartialMedium;
  end MediumB;
  model Holder
    replaceable package Medium = PartialMedium
      constrainedby PartialMedium;
    Real x;
  equation
    x = Medium.k;
  initial equation
    x = Medium.k;
  algorithm
    if Medium.k > 0 then
      x := Medium.k;
    end if;
  end Holder;
  model Layer
    replaceable package Medium = PartialMedium
      constrainedby PartialMedium;
    Holder holder(redeclare package Medium = Medium);
  end Layer;
  model Test
    Layer layer(redeclare package Medium = MediumB);
  end Test;
end P;
";
    let tree = resolved_tree(source);
    let expected = tree
        .get_class_by_qualified_name("P.PartialMedium")
        .and_then(|class| class.components.get("k"))
        .and_then(|component| component.def_id)
        .expect("package constant declaration identity");
    let holder_source = tree
        .get_class_by_qualified_name("P.Holder")
        .expect("Holder definition");
    let ast::Equation::Simple { rhs, .. } = &holder_source.equations[0] else {
        panic!("expected simple source equation");
    };
    let unresolved_source = ast::collect_component_refs(rhs)
        .into_iter()
        .find(|reference| reference.to_string() == "Medium.k")
        .expect("source equation refers through replaceable package");
    assert_eq!(
        unresolved_source.target_def_id(),
        None,
        "Resolve must defer the instance-dependent package member"
    );

    let overlay = match crate::instantiate_model_with_outcome(&tree, "P.Test") {
        crate::InstantiationOutcome::Success(overlay) => overlay,
        crate::InstantiationOutcome::NeedsInner { missing_inners, .. } => panic!(
            "forwarding redeclare fixture unexpectedly needs inner declarations: {missing_inners:?}"
        ),
        crate::InstantiationOutcome::Error(error) => {
            panic!("forwarding redeclare failed: {error}")
        }
    };
    let holder = overlay
        .classes
        .values()
        .find(|class| class.qualified_name.to_flat_string() == "layer.holder")
        .expect("nested Holder class occurrence");

    let ast::Equation::Simple { rhs, .. } = &holder.equations[0].equation else {
        panic!("expected instantiated runtime equation");
    };
    assert_reference_target(rhs, "Medium.k", expected);
    let ast::Equation::Simple { rhs, .. } = &holder.initial_equations[0].equation else {
        panic!("expected instantiated initial equation");
    };
    assert_reference_target(rhs, "Medium.k", expected);

    let ast::Statement::If { cond_blocks, .. } = &holder.algorithms[0][0].statement else {
        panic!("expected instantiated if statement");
    };
    assert_reference_target(&cond_blocks[0].cond, "Medium.k", expected);
    let ast::Statement::Assignment { value, .. } = &cond_blocks[0].stmts[0] else {
        panic!("expected nested instantiated assignment");
    };
    assert_reference_target(value, "Medium.k", expected);
}

mod nested_redeclare_cases;
use nested_redeclare_cases::assert_reference_target;

/// Each component edge advances through the exact occurrence catalog before
/// the selected middle component proves its deferred member.
#[test]
fn component_boundary_selection_uses_the_complete_occurrence_chain() {
    let tree = resolved_tree(COMPONENT_BOUNDARY_SOURCE);
    let binding = component_boundary_binding(&tree);
    let root_component_def_id = tree
        .get_class_by_qualified_name("M")
        .and_then(|class| class.components.get("h"))
        .and_then(|component| component.def_id)
        .expect("M.h must carry its declaration identity");
    let root_selected_class_def_id = tree
        .get_class_by_qualified_name("H")
        .and_then(|class| class.def_id)
        .expect("H must carry its class identity");
    let boundary_def_id = tree
        .get_class_by_qualified_name("H")
        .and_then(|class| class.components.get("w"))
        .and_then(|component| component.def_id)
        .expect("H.w must carry its declaration identity");
    let selected_class_def_id = tree
        .get_class_by_qualified_name("W")
        .and_then(|class| class.def_id)
        .expect("W must carry its class identity");
    let member_def_id = tree
        .get_class_by_qualified_name("W")
        .and_then(|class| class.components.get("v"))
        .and_then(|component| component.def_id)
        .expect("W.v must carry its declaration identity");
    let source_plan = super::deferred_references::SelectedComponentTypes::one_structured_for_test(
        root_component_def_id,
        root_selected_class_def_id,
    );
    let child_plan = super::deferred_references::SelectedComponentTypes::one_structured_for_test(
        boundary_def_id,
        selected_class_def_id,
    );
    let root_occurrence = ast::QualifiedName::new();
    let child_occurrence = root_occurrence.child("h");
    let selected_occurrence = child_occurrence.child("w");
    let mut catalog = super::deferred_references::SelectedComponentTypeCatalog::new();
    catalog
        .issue(child_occurrence, Arc::new(child_plan))
        .expect("H occurrence plan");
    catalog
        .issue(
            selected_occurrence,
            Arc::new(super::deferred_references::SelectedComponentTypes::empty_for_test()),
        )
        .expect("W occurrence plan");
    let resolved = super::deferred_references::resolve_dynamic_expression_targets_at_occurrence(
        &tree,
        &TypeOverrideMap::new(),
        &catalog,
        &root_occurrence,
        &source_plan,
        binding,
    )
    .expect("the complete occurrence chain proves the deferred member");
    let ast::Expression::ComponentReference(reference) = &resolved else {
        panic!("binding must stay a component reference");
    };
    assert_eq!(
        reference.parts.get(2).and_then(|part| part.def_id),
        Some(member_def_id),
        "the deferred member must be proved from the boundary's selection"
    );
}

const COMPONENT_BOUNDARY_SOURCE: &str = r"
model W
  Real v;
end W;

model H
  replaceable W w;
end H;

model M
  H h;
  Real y = h.w.v;
end M;
";

fn component_boundary_binding(tree: &ast::ClassTree) -> ast::Expression {
    let class = tree.get_class_by_qualified_name("M").expect("M must exist");
    let component = class.components.get("y").expect("M.y must exist");
    let binding = component
        .binding
        .as_ref()
        .expect("M.y must carry its binding")
        .clone();
    let ast::Expression::ComponentReference(reference) = &binding else {
        panic!("M.y binding must be a component reference");
    };
    assert_eq!(
        (
            reference
                .parts
                .first()
                .and_then(|part| part.def_id)
                .is_some(),
            reference
                .parts
                .get(1)
                .and_then(|part| part.def_id)
                .is_some(),
            reference.parts.get(2).and_then(|part| part.def_id),
        ),
        (true, true, None),
        "Resolve must record the contiguous prefix and defer the member"
    );
    binding
}
