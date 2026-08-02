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
        .into_inner()
}

fn resolved_tree(source: &str) -> ast::ClassTree {
    let file_name = "<dynamic_type_identity_test>";
    let stored = rumoca_phase_parse::parse_to_ast(source, file_name)
        .expect("dynamic type identity fixture should parse");
    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map.add(file_name, source);
    rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree))
        .expect("dynamic type identity fixture should resolve")
        .into_inner()
}

fn instantiate_component_redeclare_error(model: &str) -> Box<crate::InstantiateError> {
    crate::instantiate_model(&resolved_component_redeclare_tree(), model)
        .expect_err("component redeclare fixture should fail")
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
    let overlay = crate::instantiate_model(&tree, "ComponentGood")
        .expect("valid component redeclare should instantiate");
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
    let overlay = crate::instantiate_model(&tree, "ComponentReplaceableGood")
        .expect("replaceable modifier should act as a redeclare");
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
    let overlay = crate::instantiate_model(&tree, "ComponentExplicit")
        .expect("valid modified component redeclare should instantiate");
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
    let overlay =
        crate::instantiate_model(&tree, "P.Test").expect("concrete package should instantiate");
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

    let overlay =
        crate::instantiate_model(&tree, "P.Test").expect("forwarding redeclare instantiates");
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

    let overlay = crate::instantiate_model(&tree, "Test").expect("drive fixture instantiates");
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

    let overlay = crate::instantiate_model(&tree, "Test").expect("media fixture instantiates");
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
    let overlay = crate::instantiate_model(&tree, "Test").expect("surface fixture instantiates");
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

#[test]
fn selected_component_missing_member_fails_post_materialization_without_name_fallback() {
    let source = r"
record DefaultData
  Real JL;
end DefaultData;
record SelectedData
  Real other;
end SelectedData;
record Unrelated
  Real JL;
end Unrelated;
model Base
  replaceable parameter DefaultData data constrainedby DefaultData;
  Real copied = data.JL;
  Unrelated unrelated;
end Base;
";
    let tree = resolved_tree(source);
    let selected_type = tree
        .get_class_by_qualified_name("SelectedData")
        .and_then(|class| class.def_id)
        .expect("SelectedData identity");
    let mut overlay = crate::instantiate_model(&tree, "Base").expect("base fixture instantiates");
    overlay
        .components
        .values_mut()
        .find(|component| component.qualified_name.to_flat_string() == "data")
        .expect("selected data occurrence")
        .type_def_id = Some(selected_type);
    let copied = overlay
        .components
        .values_mut()
        .find(|component| component.qualified_name.to_flat_string() == "copied")
        .expect("copied occurrence");
    let ast::Expression::ComponentReference(reference) =
        copied.binding.as_mut().expect("copied declaration binding")
    else {
        panic!("copied binding must remain a component reference");
    };
    reference.set_target_def_id(None);

    let error = super::post_materialization::resolve_post_materialization_component_targets(
        &tree,
        &mut overlay,
    )
    .expect_err("a selected type without JL must fail post-materialization");
    assert_eq!(
        diagnostic_code(&error),
        Some("rumoca::instantiate::EI007".to_string())
    );
    assert!(
        error
            .to_string()
            .contains("selected redeclare class has no such member"),
        "the selected class must be checked directly: {error}"
    );
}

fn assert_reference_target(expression: &ast::Expression, name: &str, expected: DefId) {
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
    let error = crate::instantiate_model(&tree, "ComponentWithoutRedeclare")
        .expect_err("unmarked class replacement should fail");
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
    let overlay = crate::instantiate_model(&tree, "ComponentClassModification")
        .expect("ordinary nested class modification should instantiate");
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

fn nested_type_override_fixture() -> (ast::ClassTree, DefId, DefId) {
    let base_package_id = DefId::new(1);
    let base_state_id = DefId::new(2);
    let derived_package_id = DefId::new(3);
    let derived_state_id = DefId::new(4);
    let base_properties_id = DefId::new(5);

    let base_state = ast::ClassDef {
        name: make_token("ThermodynamicState"),
        def_id: Some(base_state_id),
        class_type: rumoca_core::ClassType::Record,
        is_replaceable: true,
        ..Default::default()
    };
    let mut base_package = ast::ClassDef {
        name: make_token("BaseMedium"),
        def_id: Some(base_package_id),
        class_type: rumoca_core::ClassType::Package,
        ..Default::default()
    };
    base_package
        .classes
        .insert("ThermodynamicState".to_string(), base_state);

    let derived_state = ast::ClassDef {
        name: make_token("ThermodynamicState"),
        def_id: Some(derived_state_id),
        class_type: rumoca_core::ClassType::Record,
        is_replaceable: true,
        is_redeclare: true,
        redeclare_target_def_id: Some(base_state_id),
        ..Default::default()
    };
    let base_properties = ast::ClassDef {
        name: make_token("BaseProperties"),
        def_id: Some(base_properties_id),
        class_type: rumoca_core::ClassType::Model,
        ..Default::default()
    };
    let mut derived_package = ast::ClassDef {
        name: make_token("DerivedMedium"),
        def_id: Some(derived_package_id),
        class_type: rumoca_core::ClassType::Package,
        extends: vec![ast::Extend {
            base_name: make_name("BaseMedium"),
            base_def_id: Some(base_package_id),
            ..Default::default()
        }],
        ..Default::default()
    };
    derived_package
        .classes
        .insert("ThermodynamicState".to_string(), derived_state);
    derived_package
        .classes
        .insert("BaseProperties".to_string(), base_properties);

    let mut tree = ast::ClassTree::default();
    // Scope structure mirrors what resolve registration produces: the
    // enclosing-class walk traverses the scope tree, not rendered names.
    let derived_scope = tree
        .scope_tree
        .create_scope(rumoca_core::ScopeId::GLOBAL, ast::ScopeKind::Class);
    let base_properties_scope = tree
        .scope_tree
        .create_scope(derived_scope, ast::ScopeKind::Class);
    tree.scope_to_class
        .insert(derived_scope, derived_package_id);
    tree.scope_to_class
        .insert(base_properties_scope, base_properties_id);
    if let Some(base_properties) = derived_package.classes.get_mut("BaseProperties") {
        base_properties.scope_id = Some(base_properties_scope);
    }
    derived_package.scope_id = Some(derived_scope);
    tree.definitions
        .classes
        .insert("BaseMedium".to_string(), base_package);
    tree.definitions
        .classes
        .insert("DerivedMedium".to_string(), derived_package);
    for (name, def_id) in [
        ("BaseMedium", base_package_id),
        ("BaseMedium.ThermodynamicState", base_state_id),
        ("DerivedMedium", derived_package_id),
        ("DerivedMedium.ThermodynamicState", derived_state_id),
        ("DerivedMedium.BaseProperties", base_properties_id),
    ] {
        tree.name_map.insert(name.to_string(), def_id);
        tree.def_map.insert(def_id, name.to_string());
    }
    (tree, base_state_id, derived_state_id)
}

#[test]
fn test_redeclared_nested_type_remaps_inherited_type_def_id() {
    let (tree, base_state_id, derived_state_id) = nested_type_override_fixture();
    let base_properties = tree
        .get_class_by_qualified_name("DerivedMedium.BaseProperties")
        .expect("base properties class");
    let overrides = build_type_override_map(&tree, base_properties, None);
    let comp = ast::Component {
        name: "state".to_string(),
        type_name: make_name("ThermodynamicState"),
        type_def_id: Some(base_state_id),
        ..ast::Component::empty_with_span(test_span())
    };

    let overridden =
        apply_type_override(&tree, &comp, &overrides).expect("override should validate");

    assert_eq!(
        overridden.type_def_id,
        Some(derived_state_id),
        "inherited references resolved to the base nested DefId must use the active redeclared nested type"
    );
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
    // An unresolved source type is invalid phase input. Even when a
    // same-spelled class path happens to exist, applying overrides must not
    // invent the missing declaration identity.
    let medium_b_id = DefId::new(10);
    let medium_alias_id = DefId::new(11);
    let base_properties_id = DefId::new(12);

    let base_properties = ast::ClassDef {
        name: make_token("BaseProperties"),
        class_type: rumoca_core::ClassType::Model,
        def_id: Some(base_properties_id),
        ..Default::default()
    };

    let mut medium_b = ast::ClassDef {
        name: make_token("MediumB"),
        class_type: rumoca_core::ClassType::Package,
        def_id: Some(medium_b_id),
        ..Default::default()
    };
    medium_b
        .classes
        .insert("BaseProperties".to_string(), base_properties);

    let medium_alias = ast::ClassDef {
        name: make_token("MediumAlias"),
        class_type: rumoca_core::ClassType::Package,
        def_id: Some(medium_alias_id),
        extends: vec![ast::Extend {
            base_name: make_name("MediumB"),
            base_def_id: Some(medium_b_id),
            ..Default::default()
        }],
        ..Default::default()
    };

    let mut tree = ast::ClassTree::default();
    tree.definitions
        .classes
        .insert("MediumB".to_string(), medium_b);
    tree.definitions
        .classes
        .insert("MediumAlias".to_string(), medium_alias);
    for (name, def_id) in [
        ("MediumB", medium_b_id),
        ("MediumB.BaseProperties", base_properties_id),
        ("MediumAlias", medium_alias_id),
    ] {
        tree.name_map.insert(name.to_string(), def_id);
        tree.def_map.insert(def_id, name.to_string());
    }

    let comp = ast::Component {
        name: "state".to_string(),
        type_name: make_name("Medium.BaseProperties"),
        type_def_id: None,
        ..ast::Component::empty_with_span(test_span())
    };

    let overridden =
        apply_type_override(&tree, &comp, &TypeOverrideMap::new()).expect("override operation");

    assert_eq!(
        overridden.type_def_id, None,
        "instantiation must not recover a missing exact identity from rendered class names"
    );
}

#[test]
fn test_apply_type_override_uses_dotted_member_not_partial_name_def_id() {
    let medium_alias_id = DefId::new(20);
    let concrete_medium_id = DefId::new(21);
    let base_properties_id = DefId::new(22);

    let base_properties = ast::ClassDef {
        name: make_token("BaseProperties"),
        class_type: rumoca_core::ClassType::Model,
        def_id: Some(base_properties_id),
        ..Default::default()
    };
    let mut concrete_medium = ast::ClassDef {
        name: make_token("ConcreteMedium"),
        class_type: rumoca_core::ClassType::Package,
        def_id: Some(concrete_medium_id),
        ..Default::default()
    };
    concrete_medium
        .classes
        .insert("BaseProperties".to_string(), base_properties);

    let mut tree = ast::ClassTree::default();
    tree.definitions
        .classes
        .insert("ConcreteMedium".to_string(), concrete_medium);
    tree.name_map
        .insert("ConcreteMedium".to_string(), concrete_medium_id);
    tree.name_map.insert(
        "ConcreteMedium.BaseProperties".to_string(),
        base_properties_id,
    );
    tree.def_map
        .insert(concrete_medium_id, "ConcreteMedium".to_string());
    tree.def_map.insert(
        base_properties_id,
        "ConcreteMedium.BaseProperties".to_string(),
    );

    let mut type_name = make_name("Medium.BaseProperties");
    type_name.def_id = Some(medium_alias_id);
    let comp = ast::Component {
        name: "medium".to_string(),
        type_name,
        type_def_id: None,
        ..ast::Component::empty_with_span(test_span())
    };
    let mut type_overrides = TypeOverrideMap::new();
    type_overrides.insert_alias(
        ast::QualifiedName::from_ident("Medium"),
        Some(medium_alias_id),
        concrete_medium_id,
    );

    let overridden =
        apply_type_override(&tree, &comp, &type_overrides).expect("override should validate");

    assert_eq!(
        overridden.type_def_id,
        Some(base_properties_id),
        "dotted type names with partial first-segment DefIds must resolve to the concrete member"
    );
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
    type_overrides.insert_alias(
        ast::QualifiedName::from_ident("Medium"),
        None,
        concrete_medium_id,
    );
    type_overrides.specialize_inherited_nested_types(&tree, concrete_medium_id);

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
    let internal_constants_id = DefId::new(40);
    let unrelated_constants_id = DefId::new(41);
    let internal_constants = ast::ClassDef {
        name: make_token("SpiceConstants"),
        class_type: rumoca_core::ClassType::Record,
        def_id: Some(internal_constants_id),
        ..Default::default()
    };
    let unrelated_constants = ast::ClassDef {
        name: make_token("SpiceConstants"),
        class_type: rumoca_core::ClassType::Record,
        def_id: Some(unrelated_constants_id),
        ..Default::default()
    };
    let mut tree = ast::ClassTree::default();
    tree.definitions
        .classes
        .insert("InternalConstants".to_string(), internal_constants);
    tree.definitions
        .classes
        .insert("UnrelatedConstants".to_string(), unrelated_constants);
    for (name, def_id) in [
        ("Root.Internal.SpiceConstants", internal_constants_id),
        ("Root.Examples.Test.SpiceConstants", unrelated_constants_id),
    ] {
        tree.name_map.insert(name.to_string(), def_id);
        tree.def_map.insert(def_id, name.to_string());
    }

    let component = ast::Component {
        name: "constants".to_string(),
        type_name: make_name("SpiceConstants"),
        type_def_id: Some(internal_constants_id),
        ..ast::Component::empty_with_span(test_span())
    };
    let mut type_overrides = TypeOverrideMap::new();
    type_overrides.insert_alias(
        ast::QualifiedName::from_ident("SpiceConstants"),
        Some(internal_constants_id),
        unrelated_constants_id,
    );

    let overridden = apply_type_override(&tree, &component, &type_overrides)
        .expect("unrelated type collision should be ignored");
    assert_eq!(
        overridden.type_def_id,
        Some(internal_constants_id),
        "resolve's exact type identity must survive unrelated same-named outer types"
    );
}
