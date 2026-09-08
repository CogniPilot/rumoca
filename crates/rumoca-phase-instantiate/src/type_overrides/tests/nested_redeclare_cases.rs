use super::*;

#[test]
fn enclosing_selected_component_reproves_nested_modifier_bindings() {
    let source = r"
record DriveData
  parameter Real JL = 2;
end DriveData;
model LoadInertia
  parameter Real J = 1;
end LoadInertia;
partial model PartialDrive
  replaceable parameter DriveData driveData constrainedby DriveData;
  LoadInertia loadInertia(J = driveData.JL);
end PartialDrive;
model Test
  extends PartialDrive;
end Test;
";
    let tree = resolved_tree(source);
    let expected = tree
        .get_class_by_qualified_name("DriveData")
        .and_then(|class| class.components.get("JL"))
        .and_then(|component| component.def_id)
        .expect("DriveData.JL declaration identity");
    let unresolved = tree
        .get_class_by_qualified_name("PartialDrive")
        .and_then(|class| class.components.get("loadInertia"))
        .and_then(|component| component.modifications.get("J"))
        .and_then(|value| {
            ast::collect_component_refs(value)
                .into_iter()
                .find(|reference| reference.to_string() == "driveData.JL")
        })
        .expect("nested modifier source reference");
    assert_eq!(
        unresolved.target_def_id(),
        None,
        "Resolve must defer the member of a replaceable component occurrence"
    );

    let overlay = match crate::instantiate_model_with_outcome(&tree, "Test") {
        crate::InstantiationOutcome::Success(overlay) => overlay,
        crate::InstantiationOutcome::NeedsInner { missing_inners, .. } => {
            panic!("drive fixture unexpectedly needs inner declarations: {missing_inners:?}")
        }
        crate::InstantiationOutcome::Error(error) => panic!("drive fixture failed: {error}"),
    };
    let inertia = overlay
        .components
        .values()
        .find(|component| component.qualified_name.to_flat_string() == "loadInertia.J")
        .expect("nested J occurrence");
    if let Some(reference) = inertia.binding.as_ref().and_then(|binding| {
        ast::collect_component_refs(binding)
            .into_iter()
            .find(|reference| reference.to_string() == "driveData.JL")
    }) {
        assert_eq!(reference.target_def_id(), Some(expected));
    }
    assert_reference_target(
        inertia
            .binding_source
            .as_ref()
            .expect("symbolic J binding source"),
        "driveData.JL",
        expected,
    );
    assert_eq!(
        inertia.binding_source_scope,
        Some(ast::QualifiedName::new()),
        "the nested binding must retain the root writer occurrence"
    );
}

#[test]
fn selected_media_components_reprove_multihop_record_members() {
    let source = r"
record StateBase
  Real p;
  Real T;
  Real X;
  Real d;
end StateBase;
record StateConcrete
  extends StateBase;
end StateConcrete;
model BaseProperties
  replaceable StateBase state constrainedby StateBase;
end BaseProperties;
model ConcreteProperties
  extends BaseProperties(redeclare StateConcrete state);
  Real localPressure = state.p;
end ConcreteProperties;
model Test
  replaceable ConcreteProperties medium constrainedby BaseProperties;
  Real pressure = medium.state.p;
  Real temperature = medium.state.T;
  Real composition = medium.state.X;
  Real density = medium.state.d;
end Test;
";
    let tree = resolved_tree(source);
    let state = tree
        .get_class_by_qualified_name("StateBase")
        .expect("state record");
    let expected = ["p", "T", "X", "d"].map(|name| {
        state
            .components
            .get(name)
            .and_then(|component| component.def_id)
            .unwrap_or_else(|| panic!("StateBase.{name} identity"))
    });

    let overlay = match crate::instantiate_model_with_outcome(&tree, "Test") {
        crate::InstantiationOutcome::Success(overlay) => overlay,
        crate::InstantiationOutcome::NeedsInner { missing_inners, .. } => {
            panic!("media fixture unexpectedly needs inner declarations: {missing_inners:?}")
        }
        crate::InstantiationOutcome::Error(error) => panic!("media fixture failed: {error}"),
    };
    for (component_name, reference_name, expected) in [
        ("pressure", "medium.state.p", expected[0]),
        ("temperature", "medium.state.T", expected[1]),
        ("composition", "medium.state.X", expected[2]),
        ("density", "medium.state.d", expected[3]),
        ("medium.localPressure", "state.p", expected[0]),
    ] {
        let component = overlay
            .components
            .values()
            .find(|component| component.qualified_name.to_flat_string() == component_name)
            .unwrap_or_else(|| panic!("component occurrence {component_name}"));
        assert_reference_target(
            component.binding.as_ref().expect("declaration binding"),
            reference_name,
            expected,
        );
    }
}

fn selected_sibling_record_projection_tree() -> ast::ClassTree {
    resolved_tree(
        r"
package P
partial package PartialMedium
  record State
    Real p;
  end State;
  model BaseProperties
    State state;
  end BaseProperties;
end PartialMedium;
package MediumB
  extends PartialMedium;
end MediumB;
model HeatTransfer
  replaceable package Medium = PartialMedium constrainedby PartialMedium;
  parameter Integer n = 1;
  parameter Medium.State states[n];
end HeatTransfer;
model Layer
  replaceable package Medium = PartialMedium constrainedby PartialMedium;
  Medium.BaseProperties medium;
  HeatTransfer heatTransfer(
    redeclare package Medium = Medium,
    final n = 1,
    final states = {medium.state});
end Layer;
model Test
  Layer layer(redeclare package Medium = MediumB);
end Test;
end P;
",
    )
}

#[test]
fn selected_sibling_component_type_proves_record_binding_projection_before_expansion() {
    let tree = selected_sibling_record_projection_tree();
    let layer = tree
        .get_class_by_qualified_name("P.Layer")
        .expect("resolved layer");
    let expected_medium = layer
        .components
        .get("medium")
        .and_then(|component| component.def_id)
        .expect("resolved sibling medium occurrence");
    let expected_state = tree
        .get_class_by_qualified_name("P.PartialMedium.BaseProperties")
        .and_then(|class| class.components.get("state"))
        .and_then(|component| component.def_id)
        .expect("resolved state declaration");
    let expected_p = tree
        .get_class_by_qualified_name("P.PartialMedium.State")
        .and_then(|class| class.components.get("p"))
        .and_then(|component| component.def_id)
        .expect("resolved state field declaration");
    let source_binding = layer
        .components
        .get("heatTransfer")
        .and_then(|component| component.modifications.get("states"))
        .expect("source-backed states modifier");
    let medium_state = ast::collect_component_refs(source_binding)
        .into_iter()
        .find(|reference| reference.to_string() == "medium.state")
        .expect("medium.state binding reference");
    assert_eq!(
        medium_state
            .parts
            .iter()
            .map(|part| part.def_id)
            .collect::<Vec<_>>(),
        vec![Some(expected_medium), None],
        "Resolve must retain the exact sibling occurrence and defer only the selected-type member",
    );

    let overlay = match crate::instantiate_model_with_outcome(&tree, "P.Test") {
        crate::InstantiationOutcome::Success(overlay) => overlay,
        crate::InstantiationOutcome::NeedsInner { missing_inners, .. } => {
            panic!(
                "record projection fixture unexpectedly needs inner declarations: {missing_inners:?}"
            )
        }
        crate::InstantiationOutcome::Error(error) => {
            panic!("selected sibling type must prove record projection: {error}")
        }
    };
    let projected_state = overlay
        .components
        .values()
        .find(|component| {
            component.qualified_name.to_flat_string() == "layer.heatTransfer.states[1].p"
        })
        .expect("projected record field occurrence");
    let binding = projected_state
        .binding
        .as_ref()
        .expect("projected record field binding");
    let ast::Expression::FieldAccess {
        base,
        field,
        field_def_id,
        ..
    } = binding
    else {
        panic!("record field binding must be an exact field projection: {binding:?}");
    };
    let ast::Expression::ComponentReference(projected) = base.as_ref() else {
        panic!("record field projection must retain the resolved sibling base: {base:?}");
    };
    assert_eq!(field, "p");
    assert_eq!(*field_def_id, Some(expected_p));
    assert_eq!(
        projected
            .parts
            .iter()
            .map(|part| part.def_id)
            .collect::<Vec<_>>(),
        vec![Some(expected_medium), Some(expected_state)],
        "Instantiate must publish the exact resolver-issued sibling and selected-state identities",
    );
}

#[test]
fn nested_component_redeclare_proves_multihop_member_before_publication() {
    let source = r"
record BaseData
  parameter Real inherited = 1;
end BaseData;
record SelectedData
  extends BaseData;
  parameter Real selected = 2;
end SelectedData;
model Holder
  replaceable parameter BaseData data constrainedby BaseData;
end Holder;
model Test
  Holder holder(redeclare parameter SelectedData data);
  Real y = holder.data.selected;
end Test;
";
    let tree = resolved_tree(source);
    let selected_member = tree
        .get_class_by_qualified_name("SelectedData")
        .and_then(|class| class.components.get("selected"))
        .and_then(|component| component.def_id)
        .expect("SelectedData.selected identity");
    let source_binding = tree
        .get_class_by_qualified_name("Test")
        .and_then(|class| class.components.get("y"))
        .and_then(|component| component.binding.as_ref())
        .expect("source-backed y binding");
    let source_reference = ast::collect_component_refs(source_binding)
        .into_iter()
        .find(|reference| reference.to_string() == "holder.data.selected")
        .expect("source-backed multihop reference");
    assert!(
        source_reference.parts[0].def_id.is_some()
            && source_reference.parts[1].def_id.is_some()
            && source_reference.parts[2].def_id.is_none(),
        "Resolve must issue both exact component slots and defer only the selected-type member: {source_reference:?}",
    );
    let overlay = match crate::instantiate_model_with_outcome(&tree, "Test") {
        crate::InstantiationOutcome::Success(overlay) => overlay,
        crate::InstantiationOutcome::NeedsInner { missing_inners, .. } => {
            panic!("nested selection unexpectedly needs inner declarations: {missing_inners:?}")
        }
        crate::InstantiationOutcome::Error(error) => {
            panic!("nested selected component must prove the multihop member: {error}")
        }
    };
    let y = overlay
        .components
        .values()
        .find(|component| component.qualified_name.to_flat_string() == "y")
        .expect("y occurrence");
    assert_reference_target(
        y.binding.as_ref().expect("y binding"),
        "holder.data.selected",
        selected_member,
    );
}

const MULTIHOP_SECTION_AND_DIMENSION_SOURCE: &str = r"
record ConstraintData
end ConstraintData;
record DeclaredData
  extends ConstraintData;
  parameter Integer n = 1;
  parameter Real selected = -1;
end DeclaredData;
record ChosenData
  extends ConstraintData;
  parameter Integer n = 3;
  parameter Real selected = 7;
end ChosenData;
model Holder
  replaceable parameter DeclaredData data constrainedby ConstraintData;
end Holder;
model Test
  Holder holder(redeclare parameter ChosenData data);
  Real samples[holder.data.n];
  Real equationValue;
  Real algorithmValue;
equation
  equationValue = holder.data.selected;
algorithm
  algorithmValue := holder.data.selected;
end Test;
";

#[test]
fn multihop_occurrence_routes_equations_algorithms_and_dimensions() {
    let tree = resolved_tree(MULTIHOP_SECTION_AND_DIMENSION_SOURCE);
    let declared = tree
        .get_class_by_qualified_name("DeclaredData")
        .expect("declared default data type");
    let chosen = tree
        .get_class_by_qualified_name("ChosenData")
        .expect("chosen occurrence data type");
    let member_id = |class: &ast::ClassDef, name: &str| {
        class
            .components
            .get(name)
            .and_then(|component| component.def_id)
            .unwrap_or_else(|| panic!("selected member `{name}` identity"))
    };
    let declared_n = member_id(declared, "n");
    let declared_selected = member_id(declared, "selected");
    let chosen_n = member_id(chosen, "n");
    let chosen_selected = member_id(chosen, "selected");
    assert_ne!(declared_n, chosen_n, "dimension names are not identity");
    assert_ne!(
        declared_selected, chosen_selected,
        "equation member names are not identity"
    );

    let source_test = tree
        .get_class_by_qualified_name("Test")
        .expect("resolved Test definition");
    let ast::Equation::Simple { rhs, .. } = &source_test.equations[0] else {
        panic!("source equation");
    };
    let equation_reference = ast::collect_component_refs(rhs)
        .into_iter()
        .find(|reference| reference.to_string() == "holder.data.selected")
        .expect("source equation multihop reference");
    assert_eq!(
        equation_reference.target_def_id(),
        None,
        "Resolve must defer a member that also exists on the declared default type"
    );
    let ast::Statement::Assignment { value, .. } = &source_test.algorithms[0][0] else {
        panic!("source algorithm assignment");
    };
    let algorithm_reference = ast::collect_component_refs(value)
        .into_iter()
        .find(|reference| reference.to_string() == "holder.data.selected")
        .expect("source algorithm multihop reference");
    assert_eq!(algorithm_reference.target_def_id(), None);
    let ast::Subscript::Expression(source_dimension) = &source_test
        .components
        .get("samples")
        .expect("source samples declaration")
        .shape_expr[0]
    else {
        panic!("source symbolic dimension");
    };
    let dimension_reference = ast::collect_component_refs(source_dimension)
        .into_iter()
        .find(|reference| reference.to_string() == "holder.data.n")
        .expect("source dimension multihop reference");
    assert_eq!(dimension_reference.target_def_id(), None);

    let overlay = match crate::instantiate_model_with_outcome(&tree, "Test") {
        crate::InstantiationOutcome::Success(overlay) => overlay,
        crate::InstantiationOutcome::NeedsInner { missing_inners, .. } => panic!(
            "multihop surface fixture unexpectedly needs inner declarations: {missing_inners:?}"
        ),
        crate::InstantiationOutcome::Error(error) => {
            panic!("multihop surfaces must use their exact occurrence catalog: {error}")
        }
    };
    let test_class_id = source_test.def_id.expect("Test class identity");
    let test_occurrence = overlay
        .classes
        .values()
        .find(|class| class.class_def_id == Some(test_class_id))
        .expect("Test class occurrence");
    let ast::Equation::Simple { rhs, .. } = &test_occurrence.equations[0].equation else {
        panic!("instantiated equation");
    };
    assert_reference_target(rhs, "holder.data.selected", chosen_selected);
    let ast::Statement::Assignment { value, .. } = &test_occurrence.algorithms[0][0].statement
    else {
        panic!("instantiated algorithm assignment");
    };
    assert_reference_target(value, "holder.data.selected", chosen_selected);
    let samples = overlay
        .components
        .values()
        .find(|component| component.qualified_name.to_flat_string() == "samples")
        .expect("samples occurrence");
    let ast::Subscript::Expression(dimension) = &samples.dims_expr[0] else {
        panic!("instantiated symbolic dimension");
    };
    assert_reference_target(dimension, "holder.data.n", chosen_n);
}

#[test]
fn missing_intermediate_occurrence_plan_refuses_multihop_resolution() {
    use super::super::deferred_references::{
        SelectedComponentTypeCatalog, SelectedComponentTypes,
        resolve_dynamic_expression_targets_at_occurrence,
    };

    let tree = resolved_tree(MULTIHOP_SECTION_AND_DIMENSION_SOURCE);
    let test = tree
        .get_class_by_qualified_name("Test")
        .expect("resolved Test definition");
    let holder_class_id = tree
        .get_class_by_qualified_name("Holder")
        .and_then(|class| class.def_id)
        .expect("Holder class identity");
    let chosen_class_id = tree
        .get_class_by_qualified_name("ChosenData")
        .and_then(|class| class.def_id)
        .expect("ChosenData class identity");
    let holder_component_id = test
        .components
        .get("holder")
        .and_then(|component| component.def_id)
        .expect("Test.holder identity");
    let data_component_id = tree
        .get_class_by_qualified_name("Holder")
        .and_then(|class| class.components.get("data"))
        .and_then(|component| component.def_id)
        .expect("Holder.data identity");
    let source_plan =
        SelectedComponentTypes::one_structured_for_test(holder_component_id, holder_class_id);
    let holder_plan =
        SelectedComponentTypes::one_structured_for_test(data_component_id, chosen_class_id);
    let root_occurrence = ast::QualifiedName::new();
    let holder_occurrence = root_occurrence.child("holder");
    let data_occurrence = holder_occurrence.child("data");
    let mut catalog = SelectedComponentTypeCatalog::new();
    catalog
        .issue(holder_occurrence.clone(), std::sync::Arc::new(holder_plan))
        .expect("intermediate Holder occurrence plan");
    catalog
        .issue(
            data_occurrence,
            std::sync::Arc::new(SelectedComponentTypes::empty_for_test()),
        )
        .expect("selected data occurrence plan");
    assert!(
        catalog.remove_for_test(&holder_occurrence).is_some(),
        "mutation must delete the exact intermediate plan"
    );
    let ast::Equation::Simple { rhs, .. } = &test.equations[0] else {
        panic!("source equation");
    };
    let result = resolve_dynamic_expression_targets_at_occurrence(
        &tree,
        &TypeOverrideMap::new(),
        &catalog,
        &root_occurrence,
        &source_plan,
        rhs.clone(),
    );
    let Err(error) = result else {
        panic!("a deleted intermediate occurrence plan must refuse resolution");
    };
    let crate::InstantiateError::MissingSourceContext { reason } = error.as_ref() else {
        panic!("missing child plan must be a typed source-context refusal: {error:?}");
    };
    assert!(
        reason.contains("selected component occurrence `holder`")
            && reason.contains("no exact child component-type plan"),
        "refusal must identify the deleted structured occurrence: {reason}"
    );
}

#[test]
fn sibling_occurrences_select_distinct_same_spelled_component_types() {
    let source = r"
record BaseData
  parameter Real inherited = 1;
end BaseData;
package Left
  record SelectedData
    extends BaseData;
    parameter Real left = 2;
  end SelectedData;
end Left;
package Right
  record SelectedData
    extends BaseData;
    parameter Real right = 3;
  end SelectedData;
end Right;
model Holder
  replaceable parameter BaseData data constrainedby BaseData;
end Holder;
model Test
  Holder second(redeclare parameter Right.SelectedData data);
  Holder first(redeclare parameter Left.SelectedData data);
  Real firstValue = first.data.left;
  Real secondValue = second.data.right;
end Test;
";
    let tree = resolved_tree(source);
    let left_type = tree
        .get_class_by_qualified_name("Left.SelectedData")
        .and_then(|class| class.def_id)
        .expect("left selected type identity");
    let right_type = tree
        .get_class_by_qualified_name("Right.SelectedData")
        .and_then(|class| class.def_id)
        .expect("right selected type identity");
    let left_member = tree
        .get_class_by_qualified_name("Left.SelectedData")
        .and_then(|class| class.components.get("left"))
        .and_then(|component| component.def_id)
        .expect("left selected member identity");
    let right_member = tree
        .get_class_by_qualified_name("Right.SelectedData")
        .and_then(|class| class.components.get("right"))
        .and_then(|component| component.def_id)
        .expect("right selected member identity");

    let overlay = match crate::instantiate_model_with_outcome(&tree, "Test") {
        crate::InstantiationOutcome::Success(overlay) => overlay,
        crate::InstantiationOutcome::NeedsInner { missing_inners, .. } => {
            panic!("sibling selections unexpectedly need inner declarations: {missing_inners:?}")
        }
        crate::InstantiationOutcome::Error(error) => {
            panic!("sibling occurrence selections must remain distinct: {error}")
        }
    };
    for (occurrence, expected_type) in [("first.data", left_type), ("second.data", right_type)] {
        let data = overlay
            .components
            .values()
            .find(|component| component.qualified_name.to_flat_string() == occurrence)
            .unwrap_or_else(|| panic!("selected component occurrence `{occurrence}`"));
        assert_eq!(data.type_def_id, Some(expected_type));
    }
    for (component_name, reference_name, expected_member) in [
        ("firstValue", "first.data.left", left_member),
        ("secondValue", "second.data.right", right_member),
    ] {
        let component = overlay
            .components
            .values()
            .find(|component| component.qualified_name.to_flat_string() == component_name)
            .unwrap_or_else(|| panic!("result component `{component_name}`"));
        assert_reference_target(
            component.binding.as_ref().expect("result binding"),
            reference_name,
            expected_member,
        );
    }
}

#[test]
fn inherited_component_slot_consumes_one_occurrence_selection() {
    let source = r"
record BaseData
  parameter Real inherited = 1;
end BaseData;
record SelectedData
  extends BaseData;
  parameter Real selected = 2;
end SelectedData;
model BaseHolder
  replaceable parameter BaseData data constrainedby BaseData;
end BaseHolder;
model Holder
  extends BaseHolder;
  Real inheritedValue = data.inherited;
end Holder;
model Test
  Holder holder(redeclare parameter SelectedData data);
  Real selectedValue = holder.data.selected;
end Test;
";
    let tree = resolved_tree(source);
    let selected_type = tree
        .get_class_by_qualified_name("SelectedData")
        .and_then(|class| class.def_id)
        .expect("selected inherited-slot type identity");
    let selected_member = tree
        .get_class_by_qualified_name("SelectedData")
        .and_then(|class| class.components.get("selected"))
        .and_then(|component| component.def_id)
        .expect("selected inherited-slot member identity");
    let overlay = match crate::instantiate_model_with_outcome(&tree, "Test") {
        crate::InstantiationOutcome::Success(overlay) => overlay,
        crate::InstantiationOutcome::NeedsInner { missing_inners, .. } => {
            panic!("inherited slot unexpectedly needs inner declarations: {missing_inners:?}")
        }
        crate::InstantiationOutcome::Error(error) => {
            panic!("inherited component slot must select exactly once: {error}")
        }
    };
    let data = overlay
        .components
        .values()
        .find(|component| component.qualified_name.to_flat_string() == "holder.data")
        .expect("inherited selected component occurrence");
    assert_eq!(data.type_def_id, Some(selected_type));
    let selected_value = overlay
        .components
        .values()
        .find(|component| component.qualified_name.to_flat_string() == "selectedValue")
        .expect("selected-value occurrence");
    assert_reference_target(
        selected_value
            .binding
            .as_ref()
            .expect("selected-value binding"),
        "holder.data.selected",
        selected_member,
    );
}

#[test]
fn duplicate_component_selection_claim_refuses_without_publication() {
    let mut fixture = crate::test_support::ResolvedFixture::parse(
        "duplicate_component_selection.mo",
        r"
record BaseData end BaseData;
record SelectedData
  extends BaseData;
end SelectedData;
model Holder
  replaceable parameter BaseData data constrainedby BaseData;
end Holder;
model Test
  Holder holder(redeclare parameter SelectedData data);
end Test;
",
    );
    let holder = fixture
        .tree_mut()
        .definitions
        .classes
        .get_mut("Test")
        .and_then(|class| class.components.get_mut("holder"))
        .expect("holder redeclare occurrence");
    let duplicate = holder
        .source_modifications
        .first()
        .expect("source component redeclare")
        .clone();
    holder.source_modifications.push(duplicate);
    holder.source_modification_each_flags.push(false);
    holder.source_modification_final_flags.push(false);
    holder.source_modification_redeclare_flags.push(true);

    let error = match crate::instantiate_model_with_outcome(fixture.tree(), "Test") {
        crate::InstantiationOutcome::Error(error) => error,
        crate::InstantiationOutcome::NeedsInner { missing_inners, .. } => {
            panic!("duplicate claim unexpectedly needs inner declarations: {missing_inners:?}")
        }
        crate::InstantiationOutcome::Success(overlay) => panic!(
            "duplicate component selection must not publish an InstanceOverlay; published {} components",
            overlay.components.len()
        ),
    };
    assert!(error.to_string().contains("redeclared more than once"));
}

#[test]
fn deleted_normalized_component_selection_refuses_without_publication() {
    let mut fixture = crate::test_support::ResolvedFixture::parse(
        "deleted_component_selection.mo",
        r"
record BaseData end BaseData;
record SelectedData
  extends BaseData;
end SelectedData;
model Holder
  replaceable parameter BaseData data constrainedby BaseData;
end Holder;
model Test
  Holder holder(redeclare parameter SelectedData data);
end Test;
",
    );
    fixture
        .tree_mut()
        .definitions
        .classes
        .get_mut("Test")
        .and_then(|class| class.components.get_mut("holder"))
        .expect("holder redeclare occurrence")
        .modifications
        .shift_remove("data")
        .expect("normalized component selection");

    let error = match crate::instantiate_model_with_outcome(fixture.tree(), "Test") {
        crate::InstantiationOutcome::Error(error) => error,
        crate::InstantiationOutcome::NeedsInner { missing_inners, .. } => {
            panic!("deleted claim unexpectedly needs inner declarations: {missing_inners:?}")
        }
        crate::InstantiationOutcome::Success(overlay) => panic!(
            "deleted normalized selection must not publish an InstanceOverlay; published {} components",
            overlay.components.len()
        ),
    };
    assert!(
        error
            .to_string()
            .contains("no normalized semantic modifier")
    );
}

fn selected_missing_member_fixture() -> crate::test_support::ResolvedFixture {
    crate::test_support::ResolvedFixture::parse(
        "selected_missing_member.mo",
        r"
partial package PartialMedium
  record Data
    Real JL;
  end Data;
end PartialMedium;
package MediumB
  extends PartialMedium;
end MediumB;
package Other
  record Data
    Real JL;
  end Data;
end Other;
model Holder
  replaceable package Medium = PartialMedium constrainedby PartialMedium;
  Medium.Data data;
  Real copied = data.JL;
  Other.Data unrelated;
end Holder;
model Test
  Holder holder(redeclare package Medium = MediumB);
end Test;
",
    )
}

#[test]
fn selected_class_missing_member_refuses_without_unrelated_name_fallback_or_publication() {
    let mut fixture = selected_missing_member_fixture();
    let unrelated_member = fixture
        .tree()
        .get_class_by_qualified_name("Other.Data")
        .and_then(|class| class.components.get("JL"))
        .and_then(|component| component.def_id)
        .expect("unrelated same-spelled member identity");
    let holder = fixture
        .tree()
        .get_class_by_qualified_name("Holder")
        .expect("resolved holder");
    let data_id = holder
        .components
        .get("data")
        .and_then(|component| component.def_id)
        .expect("resolved selected component slot");
    let source_reference = holder
        .components
        .get("copied")
        .and_then(|component| component.binding.as_ref())
        .and_then(|binding| {
            ast::collect_component_refs(binding)
                .into_iter()
                .find(|reference| reference.to_string() == "data.JL")
        })
        .expect("source-backed selected-member reference");
    assert_eq!(
        source_reference
            .parts
            .iter()
            .map(|part| part.def_id)
            .collect::<Vec<_>>(),
        vec![Some(data_id), None],
        "Resolve must preserve the selected component slot and defer only its member",
    );
    let baseline = match crate::instantiate_model_with_outcome(fixture.tree(), "Test") {
        crate::InstantiationOutcome::Success(overlay) => overlay,
        crate::InstantiationOutcome::NeedsInner { missing_inners, .. } => {
            panic!("valid selected-member fixture unexpectedly needs inner: {missing_inners:?}")
        }
        crate::InstantiationOutcome::Error(error) => {
            panic!("valid selected-member fixture must instantiate: {error}")
        }
    };
    assert!(
        baseline
            .components
            .values()
            .any(|component| { component.qualified_name.to_flat_string() == "holder.copied" })
    );
    let removed_member = fixture
        .tree_mut()
        .definitions
        .classes
        .get_mut("PartialMedium")
        .expect("resolved selected package base")
        .classes
        .get_mut("Data")
        .expect("resolved selected data class")
        .components
        .shift_remove("JL")
        .expect("resolved selected member");
    assert_ne!(removed_member.def_id, Some(unrelated_member));

    let error = match crate::instantiate_model_with_outcome(fixture.tree(), "Test") {
        crate::InstantiationOutcome::Error(error) => error,
        crate::InstantiationOutcome::NeedsInner { missing_inners, .. } => {
            panic!("selected-member mutation unexpectedly needs inner: {missing_inners:?}")
        }
        crate::InstantiationOutcome::Success(overlay) => panic!(
            "missing selected member must not publish an InstanceOverlay; published {} components",
            overlay.components.len()
        ),
    };
    let crate::InstantiateError::RedeclareError { name, msg, .. } = error.as_ref() else {
        panic!("missing selected member must be a typed redeclare refusal: {error:?}");
    };
    assert_eq!(name, "JL");
    assert_eq!(msg, "selected redeclare class has no such member");
}

#[test]
fn selected_component_members_are_reproved_on_attributes_and_dimensions() {
    let source = r"
record Data
  parameter Integer n = 2;
  parameter Real lo = 0;
  parameter Real hi = 10;
  parameter Real nom = 1;
end Data;
model Test
  replaceable parameter Data data constrainedby Data;
  Real x[data.n](start = data.lo, min = data.lo, max = data.hi,
    nominal = data.nom);
end Test;
";
    let tree = resolved_tree(source);
    let data = tree
        .get_class_by_qualified_name("Data")
        .expect("Data record");
    let member_id = |name: &str| {
        data.components
            .get(name)
            .and_then(|component| component.def_id)
            .unwrap_or_else(|| panic!("Data.{name} identity"))
    };
    let overlay = match crate::instantiate_model_with_outcome(&tree, "Test") {
        crate::InstantiationOutcome::Success(overlay) => overlay,
        crate::InstantiationOutcome::NeedsInner { missing_inners, .. } => {
            panic!("surface fixture unexpectedly needs inner declarations: {missing_inners:?}")
        }
        crate::InstantiationOutcome::Error(error) => panic!("surface fixture failed: {error}"),
    };
    let x = overlay
        .components
        .values()
        .find(|component| component.qualified_name.to_flat_string() == "x")
        .expect("x occurrence");

    let ast::Subscript::Expression(dimension) = &x.dims_expr[0] else {
        panic!("symbolic dimension expression");
    };
    assert_reference_target(dimension, "data.n", member_id("n"));
    for (name, expression, expected) in [
        ("data.lo", x.start.as_ref(), member_id("lo")),
        ("data.lo", x.min.as_ref(), member_id("lo")),
        ("data.hi", x.max.as_ref(), member_id("hi")),
        ("data.nom", x.nominal.as_ref(), member_id("nom")),
    ] {
        assert_reference_target(expression.expect("numeric attribute"), name, expected);
    }
}

pub(super) fn assert_reference_target(expression: &ast::Expression, name: &str, expected: DefId) {
    let reference = ast::collect_component_refs(expression)
        .into_iter()
        .find(|reference| reference.to_string() == name)
        .unwrap_or_else(|| panic!("missing reference `{name}`"));
    assert_eq!(reference.target_def_id(), Some(expected));
}

#[test]
fn component_redeclare_rejects_final_and_nonreplaceable_targets() {
    let final_error = instantiate_component_redeclare_error("ComponentFinal");
    assert_eq!(
        diagnostic_code(&final_error),
        Some("rumoca::instantiate::EI028".to_string())
    );

    let nonreplaceable_error = instantiate_component_redeclare_error("ComponentNonReplaceable");
    assert_eq!(
        diagnostic_code(&nonreplaceable_error),
        Some("rumoca::instantiate::EI014".to_string())
    );
}

#[test]
fn class_replacement_without_redeclare_is_not_inferred_from_expression_shape() {
    let tree = resolved_component_redeclare_tree();
    let component_declaration = tree
        .get_class_by_qualified_name("ComponentWithoutRedeclare")
        .and_then(|class| class.components.get("i"))
        .expect("component declaration i");
    assert_eq!(
        component_declaration.source_modification_redeclare_flags,
        vec![false]
    );
    let error = match crate::instantiate_model_with_outcome(&tree, "ComponentWithoutRedeclare") {
        crate::InstantiationOutcome::Error(error) => error,
        crate::InstantiationOutcome::NeedsInner { missing_inners, .. } => panic!(
            "class replacement fixture unexpectedly needs inner declarations: {missing_inners:?}"
        ),
        crate::InstantiationOutcome::Success(_) => {
            panic!("unmarked class replacement should fail")
        }
    };
    assert_eq!(
        diagnostic_code(&error),
        Some("rumoca::instantiate::EI007".to_string())
    );
    assert!(
        error
            .to_string()
            .contains("requires the `redeclare` keyword")
    );
}

#[test]
fn ordinary_class_modification_without_redeclare_does_not_select_a_new_target() {
    let tree = resolved_component_redeclare_tree();
    let component_declaration = tree
        .get_class_by_qualified_name("ComponentClassModification")
        .and_then(|class| class.components.get("i"))
        .expect("component declaration i");
    assert_eq!(
        component_declaration.source_modification_redeclare_flags,
        vec![false]
    );
    let overlay = match crate::instantiate_model_with_outcome(&tree, "ComponentClassModification") {
        crate::InstantiationOutcome::Success(overlay) => overlay,
        crate::InstantiationOutcome::NeedsInner { missing_inners, .. } => panic!(
            "class modification fixture unexpectedly needs inner declarations: {missing_inners:?}"
        ),
        crate::InstantiationOutcome::Error(error) => {
            panic!("ordinary class modification failed: {error}")
        }
    };
    let component = overlay
        .components
        .values()
        .find(|component| component.qualified_name.to_flat_string() == "i")
        .expect("component instance i");
    assert!(
        component
            .class_overrides
            .values()
            .all(|class_override| class_override.alias != "Medium")
    );
}

#[test]
fn test_redeclared_nested_type_remaps_inherited_type_def_id() {
    let tree = resolved_tree(
        r"
partial package BaseMedium
  replaceable record ThermodynamicState
    Real x;
  end ThermodynamicState;
  model BaseProperties
    ThermodynamicState state;
  end BaseProperties;
end BaseMedium;
package DerivedMedium
  extends BaseMedium;
  redeclare record ThermodynamicState
    Real x;
    Real selected;
  end ThermodynamicState;
end DerivedMedium;
model Holder
  replaceable package Medium = BaseMedium constrainedby BaseMedium;
  Medium.BaseProperties properties;
end Holder;
model SelectedUse
  Holder holder(redeclare package Medium = DerivedMedium);
end SelectedUse;
",
    );
    let derived_state_id = tree
        .get_class_by_qualified_name("DerivedMedium.ThermodynamicState")
        .and_then(|class| class.def_id)
        .expect("selected state identity");
    let declared_state_id = tree
        .get_class_by_qualified_name("BaseMedium.ThermodynamicState")
        .and_then(|class| class.def_id)
        .expect("declared state identity");
    let overlay = match crate::instantiate_model_with_outcome(&tree, "SelectedUse") {
        crate::InstantiationOutcome::Success(overlay) => overlay,
        crate::InstantiationOutcome::NeedsInner { missing_inners, .. } => {
            panic!("specialization fixture unexpectedly needs inner: {missing_inners:?}")
        }
        crate::InstantiationOutcome::Error(error) => {
            panic!("production specialization owner rejected fixture: {error}")
        }
    };
    let state = overlay
        .components
        .values()
        .find(|component| component.qualified_name.to_flat_string() == "holder.properties.state")
        .expect("specialized state occurrence");
    assert_ne!(declared_state_id, derived_state_id);
    assert_eq!(
        state.type_def_id,
        Some(derived_state_id),
        "the production occurrence-selection owner must specialize the inherited member type",
    );
    assert!(overlay.components.values().any(|component| {
        component.qualified_name.to_flat_string() == "holder.properties.state.selected"
    }));
}

#[test]
fn test_resolve_cref_def_id_requires_exact_multi_part_target() {
    // Reproduces MSL-style redeclare values such as:
    // `redeclare package Medium = Modelica.Media.Water.StandardWater`.
    // Every resolved semantic segment carries its own declaration identity.
    let modelica_id = DefId::new(1);
    let media_id = DefId::new(2);
    let water_id = DefId::new(3);
    let standard_water_id = DefId::new(4);

    let cref = ast::ComponentReference {
        local: false,
        parts: [
            ("Modelica", modelica_id),
            ("Media", media_id),
            ("Water", water_id),
            ("StandardWater", standard_water_id),
        ]
        .iter()
        .map(|(part, def_id)| ast::ComponentRefPart {
            ident: make_token(part),
            subs: None,
            def_id: Some(*def_id),
        })
        .collect(),
        span: rumoca_core::Span::DUMMY,
        qualified_display_name: None,
    };

    assert_eq!(
        resolve_cref_def_id(&cref),
        Some(standard_water_id),
        "multi-part class references must resolve to the full path target, not the first segment"
    );
    let mut unresolved_tail = cref.clone();
    unresolved_tail.set_target_def_id(None);
    assert_eq!(
        resolve_cref_def_id(&unresolved_tail),
        None,
        "a multi-part class reference with no final identity must not degrade to its root"
    );
    let direct_target = ast::ComponentReference {
        local: false,
        parts: vec![ast::ComponentRefPart {
            ident: make_token("StandardWater"),
            subs: None,
            def_id: Some(standard_water_id),
        }],
        span: rumoca_core::Span::DUMMY,
        qualified_display_name: None,
    };
    assert_eq!(
        resolve_cref_def_id(&direct_target),
        Some(standard_water_id),
        "a direct one-segment class reference must preserve its exact target"
    );
}

#[test]
fn type_override_does_not_recover_missing_identity_from_rendered_names() {
    // Remove only the deferred final type target. The exact package-alias root
    // and its selected package remain proved, so spelling is the only evidence
    // deliberately unavailable to the override owner.
    let mut fixture = crate::test_support::ResolvedFixture::parse(
        "missing_deferred_type_target.mo",
        r"
partial package PartialMedium
  model BaseProperties end BaseProperties;
end PartialMedium;
package ConcreteMedium
  extends PartialMedium;
end ConcreteMedium;
model Holder
  replaceable package Medium = PartialMedium constrainedby PartialMedium;
  Medium.BaseProperties state;
end Holder;
",
    );
    let alias_id = fixture
        .tree()
        .get_class_by_qualified_name("Holder.Medium")
        .and_then(|class| class.def_id)
        .expect("resolved package alias root");
    let selected_package_id = fixture
        .tree()
        .get_class_by_qualified_name("ConcreteMedium")
        .and_then(|class| class.def_id)
        .expect("resolved selected package");
    let comp = fixture
        .tree()
        .get_class_by_qualified_name("Holder")
        .and_then(|class| class.components.get("state"))
        .expect("resolved dotted component")
        .clone();
    assert_eq!(comp.type_def_id, None);
    assert_eq!(comp.type_name.def_id, Some(alias_id));
    fixture.remove_class_def_id("PartialMedium.BaseProperties");
    let mut overrides = TypeOverrideMap::new();
    overrides.insert_alias(alias_id, selected_package_id);
    let overrides_snapshot = overrides.clone();

    let error = apply_type_override(fixture.tree(), &comp, &overrides)
        .expect_err("a missing deferred target identity must fail closed");
    assert!(matches!(
        *error,
        crate::InstantiateError::MissingResolvedIdentity { .. }
    ));
    assert_eq!(comp.type_name.def_id, Some(alias_id));
    assert_eq!(overrides, overrides_snapshot);
}

#[test]
fn test_apply_type_override_uses_dotted_member_not_partial_name_def_id() {
    let tree = resolved_tree(
        r"
partial package PartialMedium
  model BaseProperties end BaseProperties;
end PartialMedium;
package ConcreteMedium
  extends PartialMedium;
end ConcreteMedium;
model Holder
  replaceable package Medium = PartialMedium constrainedby PartialMedium;
  Medium.BaseProperties medium;
end Holder;
",
    );
    let holder = tree
        .get_class_by_qualified_name("Holder")
        .expect("resolved holder");
    let medium_alias_id = holder
        .classes
        .get("Medium")
        .and_then(|class| class.def_id)
        .expect("resolved package alias");
    let concrete_medium_id = tree
        .get_class_by_qualified_name("ConcreteMedium")
        .and_then(|class| class.def_id)
        .expect("resolved concrete package");
    let base_properties_id = tree
        .get_class_by_qualified_name("PartialMedium.BaseProperties")
        .and_then(|class| class.def_id)
        .expect("resolved inherited member");
    let comp = holder.components.get("medium").expect("dotted component");
    let mut type_overrides = TypeOverrideMap::new();
    type_overrides.insert_alias(medium_alias_id, concrete_medium_id);

    let overridden =
        apply_type_override(&tree, comp, &type_overrides).expect("override should validate");

    assert_eq!(
        overridden.type_def_id,
        Some(base_properties_id),
        "dotted type names with partial first-segment DefIds must resolve to the concrete member"
    );
}

#[test]
fn exact_type_override_can_select_predefined_target_identity() {
    let tree = resolved_tree(
        r"
model Holder
  replaceable type T = Real;
  T x;
end Holder;
",
    );
    let holder = tree
        .get_class_by_qualified_name("Holder")
        .expect("resolved holder");
    let alias_id = holder
        .classes
        .get("T")
        .and_then(|class| class.def_id)
        .expect("resolved type alias");
    let real_id = tree
        .scope_tree
        .predefined_member(&rumoca_core::ComponentPath::from_flat_path("Real"))
        .expect("canonical predefined Real identity");
    let component = holder
        .components
        .get("x")
        .expect("resolved alias component");
    let mut overrides = TypeOverrideMap::new();
    overrides.insert_alias(alias_id, real_id);

    let overridden = apply_type_override(&tree, component, &overrides)
        .expect("exact predefined target is a valid resolved type selection");

    assert_eq!(overridden.type_def_id, Some(real_id));
}

#[test]
fn test_selected_package_specializes_types_in_inherited_member_models() {
    let partial_medium_id = DefId::new(30);
    let partial_state_id = DefId::new(31);
    let base_properties_id = DefId::new(32);
    let concrete_medium_id = DefId::new(33);
    let concrete_state_id = DefId::new(34);

    let partial_state = ast::ClassDef {
        name: make_token("ThermodynamicState"),
        class_type: rumoca_core::ClassType::Record,
        def_id: Some(partial_state_id),
        ..Default::default()
    };
    let base_properties = ast::ClassDef {
        name: make_token("BaseProperties"),
        class_type: rumoca_core::ClassType::Model,
        def_id: Some(base_properties_id),
        ..Default::default()
    };
    let mut partial_medium = ast::ClassDef {
        name: make_token("PartialMedium"),
        class_type: rumoca_core::ClassType::Package,
        def_id: Some(partial_medium_id),
        ..Default::default()
    };
    partial_medium
        .classes
        .insert("ThermodynamicState".to_string(), partial_state);
    partial_medium
        .classes
        .insert("BaseProperties".to_string(), base_properties);

    let concrete_state = ast::ClassDef {
        name: make_token("ThermodynamicState"),
        class_type: rumoca_core::ClassType::Record,
        def_id: Some(concrete_state_id),
        is_redeclare: true,
        redeclare_target_def_id: Some(partial_state_id),
        ..Default::default()
    };
    let mut concrete_medium = ast::ClassDef {
        name: make_token("ConcreteMedium"),
        class_type: rumoca_core::ClassType::Package,
        def_id: Some(concrete_medium_id),
        extends: vec![ast::Extend {
            base_name: make_name("PartialMedium"),
            base_def_id: Some(partial_medium_id),
            ..Default::default()
        }],
        ..Default::default()
    };
    concrete_medium
        .classes
        .insert("ThermodynamicState".to_string(), concrete_state);

    let mut tree = ast::ClassTree::default();
    tree.definitions
        .classes
        .insert("PartialMedium".to_string(), partial_medium);
    tree.definitions
        .classes
        .insert("ConcreteMedium".to_string(), concrete_medium);
    for (name, def_id) in [
        ("PartialMedium", partial_medium_id),
        ("PartialMedium.ThermodynamicState", partial_state_id),
        ("PartialMedium.BaseProperties", base_properties_id),
        ("ConcreteMedium", concrete_medium_id),
        ("ConcreteMedium.ThermodynamicState", concrete_state_id),
    ] {
        tree.name_map.insert(name.to_string(), def_id);
        tree.def_map.insert(def_id, name.to_string());
    }

    let inherited_state_component = ast::Component {
        name: "state".to_string(),
        type_name: make_name("ThermodynamicState"),
        type_def_id: Some(partial_state_id),
        ..ast::Component::empty_with_span(test_span())
    };
    let mut type_overrides = TypeOverrideMap::new();
    type_overrides
        .specialize_inherited_nested_types(&tree, concrete_medium_id)
        .expect("selected package hierarchy is resolved");

    let overridden = apply_type_override(&tree, &inherited_state_component, &type_overrides)
        .expect("selected package should specialize inherited member types");
    assert_eq!(
        overridden.type_def_id,
        Some(concrete_state_id),
        "an inherited BaseProperties model must use the selected medium's state type"
    );
}

#[test]
fn test_resolved_type_identity_rejects_unrelated_same_named_override() {
    let tree = resolved_tree(
        r"
package P record SpiceConstants end SpiceConstants; end P;
package Q record SpiceConstants end SpiceConstants; end Q;
model Holder
  P.SpiceConstants constants;
end Holder;
",
    );
    let internal_constants_id = tree
        .get_class_by_qualified_name("P.SpiceConstants")
        .and_then(|class| class.def_id)
        .expect("internal constants identity");
    let unrelated_constants_id = tree
        .get_class_by_qualified_name("Q.SpiceConstants")
        .and_then(|class| class.def_id)
        .expect("unrelated constants identity");
    let component = tree
        .get_class_by_qualified_name("Holder")
        .and_then(|class| class.components.get("constants"))
        .expect("resolved constants component");
    let mut type_overrides = TypeOverrideMap::new();
    type_overrides.insert_alias(internal_constants_id, unrelated_constants_id);

    let overridden = apply_type_override(&tree, component, &type_overrides)
        .expect("unrelated type collision should be ignored");
    assert_eq!(
        overridden.type_def_id,
        Some(internal_constants_id),
        "resolve's exact type identity must survive unrelated same-named outer types"
    );
}

#[test]
fn nested_type_specialization_is_atomic_on_missing_member_identity() {
    let package_id = DefId::new(4_000);
    let good_id = DefId::new(4_001);
    let mut package = ast::ClassDef {
        name: make_token("Medium"),
        def_id: Some(package_id),
        class_type: rumoca_core::ClassType::Package,
        ..Default::default()
    };
    package.classes.insert(
        "Good".to_string(),
        ast::ClassDef {
            name: make_token("Good"),
            def_id: Some(good_id),
            ..Default::default()
        },
    );
    package.classes.insert(
        "Broken".to_string(),
        ast::ClassDef {
            name: make_token("Broken"),
            def_id: None,
            ..Default::default()
        },
    );
    let mut tree = ast::ClassTree::default();
    tree.name_map.insert("Medium".to_string(), package_id);
    tree.def_map.insert(package_id, "Medium".to_string());
    tree.def_map.insert(good_id, "Medium.Good".to_string());
    tree.definitions
        .classes
        .insert("Medium".to_string(), package);

    let mut type_overrides = TypeOverrideMap::new();
    let sentinel_alias = DefId::new(4_100);
    let sentinel_target = DefId::new(4_101);
    type_overrides.insert_alias(sentinel_alias, sentinel_target);
    let before = type_overrides.clone();

    assert!(
        type_overrides
            .specialize_inherited_nested_types(&tree, package_id)
            .is_err()
    );
    assert_eq!(type_overrides, before);
}

#[test]
fn class_override_materialization_refuses_missing_alias_without_mutating_catalog() {
    let missing_alias = DefId::new(4_200);
    let target = DefId::new(4_201);
    let valid_alias = DefId::new(4_202);
    let mut tree = ast::ClassTree::default();
    for (name, def_id) in [("ValidAlias", valid_alias), ("Target", target)] {
        tree.definitions.classes.insert(
            name.to_string(),
            ast::ClassDef {
                name: make_token(name),
                def_id: Some(def_id),
                ..Default::default()
            },
        );
    }
    let mut overrides = TypeOverrideMap::new();
    overrides.insert_alias(valid_alias, target);
    overrides.insert_alias(missing_alias, target);
    let before = overrides.clone();

    let error = overrides
        .class_overrides(&tree, test_span())
        .expect_err("an override alias absent from the resolved catalog must be refused");

    assert!(error.to_string().contains("virtual-class alias"));
    assert_eq!(
        overrides, before,
        "failed publication must be observationally atomic"
    );
}

#[test]
fn class_override_materialization_refuses_missing_target_without_mutating_catalog() {
    let missing_target = DefId::new(4_211);
    let tree = resolved_tree(
        r"
model ValidAlias end ValidAlias;
model Alias end Alias;
model ValidTarget end ValidTarget;
",
    );
    let id = |name: &str| {
        tree.get_class_by_qualified_name(name)
            .and_then(|class| class.def_id)
            .unwrap_or_else(|| panic!("resolved fixture class {name}"))
    };
    let alias = id("Alias");
    let valid_target = id("ValidTarget");
    let valid_alias = id("ValidAlias");
    let mut overrides = TypeOverrideMap::new();
    overrides.insert_alias(valid_alias, valid_target);
    overrides.insert_alias(alias, missing_target);
    let before = overrides.clone();

    let error = overrides
        .class_overrides(&tree, test_span())
        .expect_err("an override target absent from the resolved catalog must be refused");

    assert!(matches!(
        *error,
        crate::InstantiateError::MissingResolvedIdentity { .. }
    ));
    assert_eq!(
        overrides, before,
        "failed publication must be observationally atomic"
    );
}

#[test]
fn exact_override_reference_does_not_fall_back_to_same_spelled_alias() {
    let tree = resolved_tree(
        r"
package P package Medium end Medium; end P;
package Q package Medium end Medium; end Q;
package ConcreteMedium end ConcreteMedium;
",
    );
    let id = |name: &str| {
        tree.get_class_by_qualified_name(name)
            .and_then(|class| class.def_id)
            .unwrap_or_else(|| panic!("resolved fixture class {name}"))
    };
    let mapped_alias = id("P.Medium");
    let other_alias = id("Q.Medium");
    let target = id("ConcreteMedium");
    let mut overrides = TypeOverrideMap::new();
    overrides.insert_alias(mapped_alias, target);
    let mut other_reference = make_comp_ref(&["Medium"]);
    other_reference.set_root_def_id(Some(other_alias));
    other_reference.set_target_def_id(Some(other_alias));

    assert_eq!(
        overrides
            .target_for_reference(&tree, &other_reference)
            .expect("the other exact identity is resolved"),
        None,
        "same spelling must not select another declaration's override slot"
    );

    let unresolved_reference = make_comp_ref(&["Medium"]);
    assert!(
        overrides
            .target_for_reference(&tree, &unresolved_reference)
            .is_err(),
        "identity-less references must be refused instead of using path fallback"
    );
}
