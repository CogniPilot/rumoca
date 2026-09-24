use super::*;

const FLUID_CONSTANTS: &str = r"
package Types
  package Basic
    record FluidConstants
      Real molarMass;
    end FluidConstants;
  end Basic;
  package TwoPhase
    record FluidConstants
      extends Basic.FluidConstants;
      Real criticalPressure;
    end FluidConstants;
  end TwoPhase;
end Types;

partial package PartialMedium
  replaceable record FluidConstants = Types.Basic.FluidConstants;
end PartialMedium;

partial package PartialPure
  extends PartialMedium;
end PartialPure;

partial package PartialTwoPhase
  extends PartialPure(redeclare replaceable record FluidConstants = Types.TwoPhase.FluidConstants);
  constant FluidConstants[1] fluidConstants;
end PartialTwoPhase;

package Water
  extends PartialTwoPhase;
end Water;

model Test
  replaceable package Medium = Water;
  Real limit = Medium.fluidConstants[1].criticalPressure;
end Test;
";

const SUCCESSIVE_FLUID_CONSTANTS: &str = r"
package Types
  record BasicConstants
    Real base;
  end BasicConstants;
  record FirstConstants
    extends BasicConstants;
    Real first;
  end FirstConstants;
  record SecondConstants
    extends BasicConstants;
    Real second;
  end SecondConstants;
end Types;

partial package PartialMedium
  replaceable record FluidConstants = Types.BasicConstants;
end PartialMedium;

partial package MiddleMedium
  extends PartialMedium(redeclare replaceable record FluidConstants = Types.FirstConstants);
end MiddleMedium;

package Water
  extends MiddleMedium(redeclare replaceable record FluidConstants = Types.SecondConstants);
  constant FluidConstants[1] fluidConstants;
end Water;

model Test
  replaceable package Medium = Water;
  Real selected = Medium.fluidConstants[1].second;
end Test;
";

const SIBLING_FLUID_CONSTANTS: &str = r"
package Types
  record BasicConstants
    Real base;
  end BasicConstants;
  record FirstConstants
    extends BasicConstants;
    Real first;
  end FirstConstants;
  record SecondConstants
    extends BasicConstants;
    Real second;
  end SecondConstants;
end Types;

partial package BaseMedium
  replaceable record R = Types.BasicConstants;
end BaseMedium;

partial package Left
  extends BaseMedium(redeclare replaceable record R = Types.FirstConstants);
end Left;

partial package Right
  extends BaseMedium(redeclare replaceable record R = Types.SecondConstants);
end Right;

package DiamondLR
  extends Left;
  extends Right;
end DiamondLR;

package DiamondRL
  extends Right;
  extends Left;
end DiamondRL;

model TestLR
  replaceable package Medium = DiamondLR;
  Medium.R value;
  Real selected = value.first;
end TestLR;

model TestRL
  replaceable package Medium = DiamondRL;
  Medium.R value;
  Real selected = value.first;
end TestRL;
";

fn resolved_fixture(source: &str) -> ast::ClassTree {
    let filename = "fluid_constants.mo";
    let parsed = rumoca_phase_parse::parse_to_ast(source, filename).unwrap();
    let mut tree = ast::ClassTree::from_parsed(parsed);
    tree.source_map.add(filename, source);
    rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree))
        .expect("package record fixture resolves")
        .into_inner()
}

#[test]
fn selected_package_record_member_uses_inherited_redeclared_record() {
    let tree = resolved_fixture(FLUID_CONSTANTS);
    let expected = tree
        .get_class_by_qualified_name("Types.TwoPhase.FluidConstants")
        .unwrap()
        .components["criticalPressure"]
        .def_id
        .unwrap();
    let owner = tree.get_class_by_qualified_name("Water").unwrap();
    let constants = crate::get_effective_components(&tree, owner).unwrap();
    assert_eq!(
        constants["fluidConstants"].type_def_id,
        tree.get_class_by_qualified_name("PartialMedium.FluidConstants")
            .unwrap()
            .def_id,
        "resolved inherited component retains the virtual declaration slot"
    );
    let partial_two_phase = tree.get_class_by_qualified_name("PartialTwoPhase").unwrap();
    let partial_medium_slot = tree
        .get_class_by_qualified_name("PartialMedium.FluidConstants")
        .unwrap()
        .def_id;
    let [extend] = partial_two_phase.extends.as_slice() else {
        panic!("fixture has one redeclaring extends clause");
    };
    let [modification] = extend.modifications.as_slice() else {
        panic!("fixture has one record redeclare");
    };
    let ast::Expression::Modification { target, value, .. } = &modification.expr else {
        panic!("fixture redeclare has the expected modification shape");
    };
    assert_eq!(target.target_def_id(), partial_medium_slot);
    let ast::Expression::ClassModification {
        target: selected, ..
    } = value.as_ref()
    else {
        panic!("fixture redeclare RHS has the expected class shape");
    };
    assert_eq!(selected.target_def_id(), Some(expected_record(&tree)));
    let overlay = crate::instantiate_model(&tree, "Test")
        .expect("selected package must use its redeclared record type");
    let limit = overlay
        .components
        .values()
        .find(|component| component.qualified_name.to_flat_string() == "limit")
        .unwrap();
    let Some(ast::Expression::ComponentReference(reference)) = &limit.binding else {
        panic!("limit must retain its record member reference");
    };
    assert_eq!(reference.target_def_id(), Some(expected));
}

fn expected_record(tree: &ast::ClassTree) -> rumoca_core::DefId {
    tree.get_class_by_qualified_name("Types.TwoPhase.FluidConstants")
        .and_then(|class| class.def_id)
        .unwrap()
}

#[test]
fn selected_package_cannot_borrow_members_from_an_unselected_record() {
    let source = FLUID_CONSTANTS.replace(
        "extends PartialTwoPhase;",
        "extends PartialPure; constant FluidConstants[1] fluidConstants;",
    );
    let tree = resolved_fixture(&source);
    let error = crate::instantiate_model(&tree, "Test")
        .expect_err("the selected basic record has no criticalPressure member");
    assert!(matches!(
        *error,
        InstantiateError::RedeclareError { ref name, .. } if name == "criticalPressure"
    ));
}

#[test]
fn nearest_extends_redeclare_wins_for_the_same_original_record_slot() {
    let tree = resolved_fixture(SUCCESSIVE_FLUID_CONSTANTS);
    let base_slot = tree
        .get_class_by_qualified_name("PartialMedium.FluidConstants")
        .and_then(|class| class.def_id)
        .expect("original replaceable record slot");
    let first = tree
        .get_class_by_qualified_name("Types.FirstConstants")
        .and_then(|class| class.def_id)
        .expect("first redeclare target");
    let second = tree
        .get_class_by_qualified_name("Types.SecondConstants")
        .and_then(|class| class.def_id)
        .expect("nearest redeclare target");
    let middle = tree
        .get_class_by_qualified_name("MiddleMedium")
        .expect("middle package");
    let water = tree
        .get_class_by_qualified_name("Water")
        .expect("water package");
    let ast::Expression::Modification {
        target: middle_slot,
        value: middle_value,
        ..
    } = &middle.extends[0].modifications[0].expr
    else {
        panic!("middle redeclare shape");
    };
    let ast::Expression::ClassModification {
        target: middle_target,
        ..
    } = middle_value.as_ref()
    else {
        panic!("middle RHS shape");
    };
    let ast::Expression::Modification {
        target: water_slot,
        value: water_value,
        ..
    } = &water.extends[0].modifications[0].expr
    else {
        panic!("nearest redeclare shape");
    };
    let ast::Expression::ClassModification {
        target: water_target,
        ..
    } = water_value.as_ref()
    else {
        panic!("nearest RHS shape");
    };
    assert_eq!(middle_slot.target_def_id(), Some(base_slot));
    assert_eq!(water_slot.target_def_id(), Some(base_slot));
    assert_eq!(middle_target.target_def_id(), Some(first));
    assert_eq!(water_target.target_def_id(), Some(second));

    let overlay =
        crate::instantiate_model(&tree, "Test").expect("nearest redeclare should instantiate");
    let selected = overlay
        .components
        .values()
        .find(|component| component.qualified_name.to_flat_string() == "selected")
        .expect("selected binding");
    let Some(ast::Expression::ComponentReference(reference)) = &selected.binding else {
        panic!("selected must retain its record member reference");
    };
    let second_field = tree
        .get_class_by_qualified_name("Types.SecondConstants")
        .and_then(|class| class.components.get("second"))
        .and_then(|component| component.def_id)
        .expect("nearest field identity");
    assert_eq!(reference.target_def_id(), Some(second_field));
}

#[test]
fn sibling_redeclares_of_one_slot_are_order_independent_conflicts() {
    let left_first = resolved_fixture(SIBLING_FLUID_CONSTANTS);
    let right_first = resolved_fixture(SIBLING_FLUID_CONSTANTS);
    let left_error = crate::instantiate_model(&left_first, "TestLR")
        .expect_err("different same-depth RHS identities must be rejected");
    let right_error = crate::instantiate_model(&right_first, "TestRL")
        .expect_err("reversing same-depth bases must remain rejected");
    assert!(matches!(
        *left_error,
        InstantiateError::RedeclareError { ref name, ref msg, .. }
            if name == "Medium.R" && msg == "selected package has conflicting nested redeclarations"
    ));
    assert!(matches!(
        *right_error,
        InstantiateError::RedeclareError { ref name, ref msg, .. }
            if name == "Medium.R" && msg == "selected package has conflicting nested redeclarations"
    ));
}

#[test]
fn paired_package_selections_keep_same_slot_targets_scoped() {
    let source = r"
package Types
  record Basic
    Real base;
  end Basic;
  record First
    extends Basic;
    Real first;
  end First;
  record Second
    extends Basic;
    Real second;
  end Second;
end Types;

partial package Base
  replaceable record R = Types.Basic;
end Base;

package A
  extends Base(redeclare replaceable record R = Types.First);
end A;

package B
  extends Base(redeclare replaceable record R = Types.Second);
end B;

model Test
  replaceable package AMedium = A;
  replaceable package BMedium = B;
  AMedium.R first;
  BMedium.R second;
  Real firstValue = first.first;
  Real secondValue = second.second;
end Test;
";
    let tree = resolved_fixture(source);
    let first_id = tree
        .get_class_by_qualified_name("Types.First")
        .and_then(|class| class.def_id)
        .expect("first selected record");
    let second_id = tree
        .get_class_by_qualified_name("Types.Second")
        .and_then(|class| class.def_id)
        .expect("second selected record");
    let overlay = crate::instantiate_model(&tree, "Test")
        .expect("two selected packages must retain independent member identities");
    let first_instance = overlay
        .components
        .values()
        .find(|component| component.qualified_name.to_flat_string() == "first")
        .expect("first instance");
    let second_instance = overlay
        .components
        .values()
        .find(|component| component.qualified_name.to_flat_string() == "second")
        .expect("second instance");
    assert_eq!(first_instance.type_def_id, Some(first_id));
    assert_eq!(second_instance.type_def_id, Some(second_id));
}

#[test]
fn dotted_selected_member_component_can_resolve_without_source_type_def_id() {
    let source = r"
partial package PartialMedium
  model BaseProperties
    Real p;
  end BaseProperties;
end PartialMedium;

package Water
  extends PartialMedium;
end Water;

model Tank
  replaceable package Medium = Water constrainedby PartialMedium;
  Medium.BaseProperties medium;
  Real p = medium.p;
end Tank;

model Test
  Tank tank(redeclare package Medium = Water);
  Real outerP = tank.medium.p;
end Test;
";
    let tree = resolved_fixture(source);
    let owner = tree.get_class_by_qualified_name("Tank").unwrap();
    let components = crate::get_effective_components(&tree, owner).unwrap();
    let medium = components.get("medium").unwrap();
    assert_eq!(medium.type_def_id, None);
    let expected_member = tree
        .get_class_by_qualified_name("PartialMedium.BaseProperties")
        .and_then(|class| class.components.get("p"))
        .and_then(|component| component.def_id)
        .expect("resolved BaseProperties.p declaration identity");
    let overlay =
        crate::instantiate_model(&tree, "Test").expect("dotted selected member should instantiate");
    let outer_p = overlay
        .components
        .values()
        .find(|component| component.qualified_name.to_flat_string() == "outerP")
        .expect("outerP component");
    let Some(ast::Expression::ComponentReference(reference)) = &outer_p.binding else {
        panic!("outerP must retain its selected member reference");
    };
    assert_eq!(reference.target_def_id(), Some(expected_member));
}

#[test]
fn dotted_selected_member_does_not_borrow_from_default_package() {
    let source = r"
package Types
  model DefaultProperties
    Real p;
  end DefaultProperties;
  model EmptyProperties
    Real q;
  end EmptyProperties;
end Types;

partial package MediumBase
  replaceable model BaseProperties = Types.DefaultProperties;
end MediumBase;

package NoPressure
  extends MediumBase(redeclare model BaseProperties = Types.EmptyProperties);
end NoPressure;

model Tank
  replaceable package Medium = MediumBase constrainedby MediumBase;
  Medium.BaseProperties medium;
  Real localP = medium.p;
end Tank;

model Test
  Tank tank(redeclare package Medium = NoPressure);
end Test;
";
    let tree = resolved_fixture(source);
    let error = crate::instantiate_model(&tree, "Test")
        .expect_err("selected package must not borrow p from the default record");
    assert!(matches!(
        *error,
        InstantiateError::RedeclareError { ref name, .. } if name == "p"
    ));
}

#[test]
fn forwarded_selected_package_reaches_redeclared_record_member() {
    let source = r"
package Types
  record Basic
    Real basic;
  end Basic;
  record Other
    extends Basic;
    Real special;
  end Other;
end Types;

partial package MediumBase
  replaceable record R = Types.Basic;
  constant R[1] values;
end MediumBase;

package MediumVariant
  extends MediumBase(redeclare record R = Types.Other);
end MediumVariant;

model Inner
  replaceable package Medium = MediumBase constrainedby MediumBase;
  Real x = Medium.values[1].special;
end Inner;

partial model Outer
  replaceable package Medium = MediumBase constrainedby MediumBase;
  Inner inst(redeclare package Medium = Medium);
end Outer;

model Test
  extends Outer(redeclare package Medium = MediumVariant);
end Test;
";
    let tree = resolved_fixture(source);
    let expected = tree
        .get_class_by_qualified_name("Types.Other")
        .and_then(|class| class.components.get("special"))
        .and_then(|component| component.def_id)
        .expect("redeclared record member identity");
    let overlay = crate::instantiate_model(&tree, "Test")
        .expect("forwarded package selection must reach the selected record");
    let x = overlay
        .components
        .values()
        .find(|component| component.qualified_name.to_flat_string() == "inst.x")
        .expect("forwarded record binding");
    let Some(ast::Expression::ComponentReference(reference)) = &x.binding else {
        panic!("inst.x must retain its selected member reference");
    };
    assert_eq!(reference.target_def_id(), Some(expected));
}

#[test]
fn forwarded_alias_package_reaches_redeclared_record_member() {
    let source = r"
package Types
  record Basic
    Real basic;
  end Basic;
  record Other
    extends Basic;
    Real special;
  end Other;
end Types;

partial package MediumBase
  replaceable record R = Types.Basic;
  constant R[1] values;
end MediumBase;

package MediumVariant
  extends MediumBase(redeclare record R = Types.Other);
end MediumVariant;

model Inner
  replaceable package Medium = MediumBase constrainedby MediumBase;
  Real x = Medium.values[1].special;
end Inner;

partial model Outer
  replaceable package M = MediumBase constrainedby MediumBase;
  Inner inst(redeclare package Medium = M);
end Outer;

model Test
  extends Outer(redeclare package M = MediumVariant);
end Test;
";
    let tree = resolved_fixture(source);
    let expected = tree
        .get_class_by_qualified_name("Types.Other")
        .and_then(|class| class.components.get("special"))
        .and_then(|component| component.def_id)
        .expect("forwarded alias member identity");
    let overlay = crate::instantiate_model(&tree, "Test")
        .expect("forwarded alias package must reach the selected record");
    let x = overlay
        .components
        .values()
        .find(|component| component.qualified_name.to_flat_string() == "inst.x")
        .expect("forwarded alias record binding");
    let Some(ast::Expression::ComponentReference(reference)) = &x.binding else {
        panic!("inst.x must retain its forwarded alias member reference");
    };
    assert_eq!(reference.target_def_id(), Some(expected));
}

#[test]
fn forwarded_alias_rechecks_nested_constraint_for_selected_actual() {
    let source = r"
partial package Broad
end Broad;

package Narrow
  extends Broad;
  constant Real required;
end Narrow;

package Actual
  extends Broad;
end Actual;

model Inner
  replaceable package Medium = Narrow constrainedby Narrow;
end Inner;

partial model Outer
  replaceable package M = Narrow constrainedby Broad;
  Inner inst(redeclare package Medium = M);
end Outer;

model Test
  extends Outer(redeclare package M = Actual);
end Test;
";
    let tree = resolved_fixture(source);
    let error = crate::instantiate_model(&tree, "Test")
        .expect_err("selected actual must satisfy the nested package constraint");
    assert!(matches!(
        *error,
        InstantiateError::RedeclareConstraintViolation { ref name, ref new_type, ref constraint, .. }
            if name == "Medium" && new_type == "Actual" && constraint == "Narrow"
    ));
}
