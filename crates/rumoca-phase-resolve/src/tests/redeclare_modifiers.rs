//! Redeclaration and modifier resolution: inherited slot identity, modifier
//! value scoping, and replaceable package redeclare forwarding.

use super::*;

#[test]
fn class_redeclarations_record_exact_inherited_slot_identity() {
    let source = r#"
        package Base
            replaceable record State
                Real x;
            end State;
        end Base;
        package Middle
            extends Base;
            redeclare replaceable record State
                Real x;
                Real y;
            end State;
        end Middle;
        package Concrete
            extends Middle;
            redeclare record extends State
                Real z;
            end State;
        end Concrete;
    "#;

    let resolved = resolve_test_source(source).expect("resolution should succeed");
    let tree = resolved.inner();
    let base = tree
        .get_class_by_qualified_name("Base.State")
        .and_then(|class| class.def_id)
        .expect("base slot DefId");
    let middle_class = tree
        .get_class_by_qualified_name("Middle.State")
        .expect("middle redeclaration");
    let middle = middle_class.def_id.expect("middle DefId");
    let concrete_class = tree
        .get_class_by_qualified_name("Concrete.State")
        .expect("concrete redeclaration");

    assert!(middle_class.is_redeclare);
    assert!(concrete_class.is_redeclare);
    assert_eq!(middle_class.redeclare_target_def_id, Some(base));
    assert_eq!(concrete_class.redeclare_target_def_id, Some(middle));
}
#[test]
fn test_redeclare_package_modifier_resolves_rhs_in_modifier_scope() {
    let source = r#"
package Interfaces
  partial package PartialMedium
  end PartialMedium;
end Interfaces;

package TableBased
  extends Interfaces.PartialMedium;
end TableBased;

model B
  replaceable package Medium = Interfaces.PartialMedium;
end B;

model C
  package Medium = TableBased;
  B b(redeclare package Medium = Medium);
end C;
"#;
    let tree = resolve_tree_source(source).into_inner();
    let model = tree.definitions.classes.get("C").expect("C should exist");
    let component = model.components.get("b").expect("b should exist");
    let modification = component
        .modifications
        .get("Medium")
        .expect("Medium redeclare should be preserved");
    let rumoca_ir_ast::Expression::ClassModification { target, .. } = modification else {
        panic!("expected redeclare package value to be a class modification");
    };
    let def_id = target
        .def_id
        .expect("redeclare package RHS should resolve to enclosing Medium");
    let resolved = tree
        .def_map
        .get(&def_id)
        .expect("resolved Medium def_id should exist in def_map");

    assert_eq!(resolved, "C.Medium");
    assert_eq!(target.to_string(), "Medium");
}

#[test]
fn component_modifier_values_resolve_in_the_declaring_class_scope() {
    let source = r#"
model Device
  parameter Integer m;
end Device;

partial model Template
  parameter Integer m = 3;
  Device device(final m = m);
end Template;
"#;

    let tree = resolve_tree_source(source).into_inner();
    let template = tree
        .definitions
        .classes
        .get("Template")
        .expect("Template should exist");
    let parameter_def_id = template
        .components
        .get("m")
        .and_then(|component| component.def_id)
        .expect("m should have a DefId");
    let modifier = template
        .components
        .get("device")
        .and_then(|component| component.modifications.get("m"))
        .expect("device.m modifier");

    assert_eq!(find_comp_ref_def_id(modifier), Some(parameter_def_id));
}

#[test]
fn test_replaceable_medium_member_calls_resolve_through_forwarded_redeclare() {
    let source = r#"
package Interfaces
  partial package PartialMedium
replaceable function setState_pTX
  input Real p;
  input Real T;
  output Real state;
algorithm
  state := p + T;
end setState_pTX;
replaceable function density
  input Real state;
  output Real d;
algorithm
  d := state;
end density;
  end PartialMedium;
end Interfaces;

package TableBased
  extends Interfaces.PartialMedium;
end TableBased;

model Boundary
  replaceable package Medium = Interfaces.PartialMedium;
  Real d;
equation
  d = Medium.density(Medium.setState_pTX(1.0, 2.0));
end Boundary;

model Network
  replaceable package Medium = TableBased constrainedby Interfaces.PartialMedium;
  Boundary source(redeclare package Medium = Medium);
end Network;
"#;
    let _ = resolve_tree_source(source);
}

#[test]
fn test_extends_redeclared_replaceable_medium_member_calls_resolve() {
    let source = r#"
package Interfaces
  partial package PartialMedium
replaceable function setState_pTX
  input Real p;
  input Real T;
  output Real state;
algorithm
  state := p + T;
end setState_pTX;
replaceable function density
  input Real state;
  output Real d;
algorithm
  d := state;
end density;
  end PartialMedium;
  partial package PartialTwoPhaseMedium
extends PartialMedium;
replaceable function saturationPressure
  input Real T;
  output Real p;
algorithm
  p := T;
end saturationPressure;
  end PartialTwoPhaseMedium;
end Interfaces;

package TableBased
  extends Interfaces.PartialTwoPhaseMedium;
end TableBased;

partial model Base
  replaceable package Medium = Interfaces.PartialMedium;
end Base;

model Derived
  extends Base(
    redeclare replaceable package Medium = TableBased
      constrainedby Interfaces.PartialTwoPhaseMedium);
  Real p = Medium.saturationPressure(1.0);
end Derived;
"#;
    let tree = resolve_tree_source(source).into_inner();
    let model = tree
        .definitions
        .classes
        .get("Derived")
        .expect("Derived should exist");
    let component = model.components.get("p").expect("p should exist");
    let binding = component.binding.as_ref().expect("p should have a binding");
    let rumoca_ir_ast::Expression::FunctionCall { comp, .. } = binding else {
        panic!("expected Medium.saturationPressure call");
    };
    let def_id = comp
        .def_id
        .expect("deferred medium call should be anchored to replaceable package root");
    let resolved = tree
        .def_map
        .get(&def_id)
        .expect("resolved Medium def_id should exist in def_map");
    assert_eq!(resolved, "Base.Medium");
}

#[test]
fn redeclare_without_extends_targets_the_container_chain_slot() {
    let source = r#"
        package Lib
            package Bottom
                replaceable record State
                    Real x;
                end State;
            end Bottom;
            package Middle
                extends Bottom;
            end Middle;
            package Top
                extends Middle;
                redeclare record State
                    Real x;
                    Real y;
                end State;
            end Top;
        end Lib;
    "#;

    let resolved = resolve_test_source(source).expect("resolution should succeed");
    let tree = resolved.inner();
    let bottom_state = tree
        .get_class_by_qualified_name("Lib.Bottom.State")
        .and_then(|class| class.def_id)
        .expect("inherited slot DefId");
    let top_state = tree
        .get_class_by_qualified_name("Lib.Top.State")
        .expect("redeclaring class");

    assert!(top_state.is_redeclare);
    assert_eq!(
        top_state.redeclare_target_def_id,
        Some(bottom_state),
        "MLS §5.3.2/§7.3: a redeclare with no extends clause replaces the element the \
         *enclosing* class inherits, found through that container's base chain"
    );
}

/// The container a redeclared class replaces an element in is the owner of its
/// parent scope (SPEC_0002), addressed by `DefId` (SPEC_0001).
///
/// The fixture deliberately gives the nested class a rendered qualified name
/// whose leading segments name a *different*, unrelated class. Structure and
/// display text therefore disagree, so an implementation that recovered the
/// container by chopping the last segment off the rendered name would answer
/// with the unrelated class's inherited element and fail this test.
#[test]
fn redeclare_container_identity_comes_from_the_scope_tree_not_rendered_text() {
    let mut resolver = Resolver::new();
    let global = resolver.scope_tree.global();

    let (base, _) = declare_test_class(&mut resolver, None, "Base", global);
    let base_state = resolver.alloc_def_id(Some("Base"), "State");
    let (container, container_scope) = declare_test_class(&mut resolver, None, "Outer", global);
    resolver.class_to_bases.insert(container, vec![base]);

    let (unrelated_base, _) = declare_test_class(&mut resolver, None, "Unrelated", global);
    let unrelated_state = resolver.alloc_def_id(Some("Unrelated"), "State");
    let (unrelated, _) = declare_test_class(&mut resolver, None, "Decoy", global);
    resolver
        .class_to_bases
        .insert(unrelated, vec![unrelated_base]);

    // Declared inside `Outer`'s scope, but rendered as `Decoy.State`.
    let (nested, _) = declare_test_class(&mut resolver, Some("Decoy"), "State", container_scope);

    assert_ne!(
        base_state, unrelated_state,
        "fixture must distinguish the structural answer from the textual one"
    );
    assert_eq!(
        resolver.lookup_inherited_member("Decoy", "State"),
        Some(unrelated_state),
        "fixture check: the textually named container inherits a different `State`"
    );
    assert_eq!(
        resolver.enclosing_class_def_id(nested),
        Some(container),
        "the container is the class owning the parent scope, not a name prefix"
    );
    assert_eq!(
        resolver
            .enclosing_class_def_id(nested)
            .and_then(|owner| resolver.lookup_inherited_member_of(owner, "State")),
        Some(base_state),
        "the redeclared slot must come from the structural container's base chain"
    );
}

/// Register a class the way the registration phase does: one DefId plus its own
/// scope, linked to the enclosing scope in both directions.
fn declare_test_class(
    resolver: &mut Resolver,
    enclosing: Option<&str>,
    leaf: &str,
    enclosing_scope: ScopeId,
) -> (DefId, ScopeId) {
    let def_id = resolver.alloc_def_id(enclosing, leaf);
    let scope = resolver
        .scope_tree
        .create_scope(enclosing_scope, rumoca_ir_ast::ScopeKind::Class);
    resolver.scope_to_class_def.insert(scope, def_id);
    resolver.class_def_scopes.insert(def_id, scope);
    (def_id, scope)
}
