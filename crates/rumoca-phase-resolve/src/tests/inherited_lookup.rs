//! Name lookup through inheritance: inherited members versus enclosing and
//! global candidates, plus multiple-inheritance candidate arbitration.

use super::*;

#[test]
fn test_simple_inherited_type_name_resolves_before_global_short_name_fallback() {
    let source = r#"
package Other
  model Temperature
  end Temperature;
end Other;

package Base
  type Temperature = Real;
end Base;

package Derived
  extends Base;

  record State
Temperature T;
  end State;
end Derived;
"#;
    let tree = resolve_test_source(source).expect("resolution should succeed");
    let state = tree
        .definitions
        .classes
        .get("Derived")
        .and_then(|derived| derived.classes.get("State"))
        .expect("Derived.State should exist");
    let temp = state
        .components
        .get("T")
        .expect("State.T should exist")
        .type_def_id
        .and_then(|def_id| tree.def_map.get(&def_id));

    assert_eq!(
        temp.map(String::as_str),
        Some("Base.Temperature"),
        "record field type must resolve through the enclosing package's inherited members, \
         not by global short-name fallback"
    );
}

#[test]
fn sibling_type_resolution_is_not_captured_by_an_unrelated_nested_type() {
    let source = r#"
package Root
  package Examples
    model Test
      record Shared
      end Shared;
      Root.Internal.Device device;
    end Test;
  end Examples;
  package Internal
    model Device
      constant Shared constants;
    end Device;
    record Shared
    end Shared;
  end Internal;
end Root;
"#;
    let tree = resolve_test_source(source).expect("resolution should succeed");
    let component = tree
        .get_class_by_qualified_name("Root.Internal.Device")
        .and_then(|device| device.components.get("constants"))
        .expect("device constants component");
    let resolved_name = component
        .type_def_id
        .and_then(|def_id| tree.def_map.get(&def_id));

    assert_eq!(
        resolved_name.map(String::as_str),
        Some("Root.Internal.Shared"),
        "lexical sibling types must remain authoritative across unrelated nested declarations"
    );
}

#[test]
fn test_inherited_component_resolves_before_enclosing_member() {
    let source = r#"
model Base
  Real x;
end Base;
package P
  constant Real x = 1;
  model Derived
    extends Base;
    Real y = x;
  end Derived;
end P;
"#;
    let tree = resolve_test_source(source).expect("resolution should succeed");
    let base_x = tree
        .definitions
        .classes
        .get("Base")
        .and_then(|base| base.components.get("x"))
        .and_then(|component| component.def_id)
        .expect("Base.x DefId");
    let binding = tree
        .definitions
        .classes
        .get("P")
        .and_then(|package| package.classes.get("Derived"))
        .and_then(|derived| derived.components.get("y"))
        .and_then(|component| component.binding.as_ref())
        .expect("Derived.y binding");

    assert_eq!(
        find_comp_ref_def_id(binding),
        Some(base_x),
        "the inherited Base.x member must win before the enclosing P.x member"
    );
}

#[test]
fn test_conflicting_inherited_component_does_not_bind_enclosing_member() {
    let source = r#"
model BaseA
  Real x;
end BaseA;
model BaseB
  Integer x;
end BaseB;
package P
  constant Real x = 1;
  model Derived
    extends BaseA;
    extends BaseB;
    Real y = x;
  end Derived;
end P;
"#;
    let diagnostics = resolve_test_source(source)
        .expect_err("an ambiguous inherited member must be rejected during resolution");
    assert!(
        diagnostics.iter().any(|diagnostic| {
            diagnostic.code.as_deref() == Some("ER002")
                && diagnostic
                    .message
                    .contains("unresolved component reference: 'x'")
        }),
        "an ambiguous inherited x must not bind arbitrarily or fall through to P.x: \
         {diagnostics:?}"
    );
}

#[test]
fn extends_modified_duplicate_inherited_component_still_resolves() {
    // Shape of `Modelica.Fluid.Fittings.SimpleGenericOrifice`: `m_flow` is
    // inherited from two bases whose source declarations differ, and the
    // extends-clause modification on the first base is what makes the two
    // elements identical. MLS §5.6.1.4 checks duplicate-element identity on
    // *instantiated* (modified) elements, so resolve may not declare the name
    // unresolvable here; MLS §5.3.1 lookup finds the inherited declaration.
    let source = r#"
model TransportBase
  Real m_flow(min = 0) "Mass flow rate in design flow direction";
end TransportBase;
model LumpedFlowBase
  Real m_flow(min = 0, nominal = 2) "Mass flow rates between states";
end LumpedFlowBase;
model Orifice
  extends TransportBase(m_flow(nominal = 2));
  extends LumpedFlowBase;
  Real dp = m_flow;
end Orifice;
"#;
    let tree = resolve_test_source(source)
        .expect("an extends-modified duplicate inherited element must still resolve")
        .into_inner();
    let transport_m_flow = tree
        .definitions
        .classes
        .get("TransportBase")
        .and_then(|base| base.components.get("m_flow"))
        .and_then(|component| component.def_id)
        .expect("TransportBase.m_flow DefId");
    let binding = tree
        .definitions
        .classes
        .get("Orifice")
        .and_then(|orifice| orifice.components.get("dp"))
        .and_then(|component| component.binding.as_ref())
        .expect("Orifice.dp binding");

    assert_eq!(
        find_comp_ref_def_id(binding),
        Some(transport_m_flow),
        "MLS §5.6.1.4 keeps the first duplicate element, so the reference binds \
         to the first base's declaration"
    );
}

#[test]
fn equivalent_multiple_inheritance_components_share_one_effective_identity() {
    let source = r#"
model BaseA
  parameter Integer m = 3;
end BaseA;
model BaseB
  parameter Integer m = 3;
end BaseB;
model Derived
  extends BaseA;
  extends BaseB;
  Real x[m];
end Derived;
"#;
    let tree = resolve_tree_source(source).into_inner();
    let derived = tree
        .definitions
        .classes
        .get("Derived")
        .expect("Derived should exist");
    let shape = derived
        .components
        .get("x")
        .and_then(|component| component.shape_expr.first())
        .expect("x should have a dimension");
    let ast::Subscript::Expression(ast::Expression::ComponentReference(reference)) = shape else {
        panic!("expected m component-reference dimension");
    };

    assert_eq!(
        reference.def_id,
        tree.name_map.get("BaseA.m").copied(),
        "equivalent inherited declarations should use the first deterministic identity"
    );
}
#[test]
fn duplicate_inherited_parameter_differing_only_by_description_still_resolves() {
    // Shape of `Modelica.Fluid.Pipes.DynamicPipe`: `nParallel` is inherited
    // from `PartialStraightPipe` and from `PartialTwoPortFlow`, whose
    // declarations agree in type, prefixes, binding and modifiers and differ
    // only in their description string. A description string carries no
    // semantics, so MLS §5.6.1.4 treats the two as one element and keeps the
    // first; MLS §5.3.1 lookup of `nParallel` must therefore find it rather
    // than report an ambiguity.
    let source = r#"
partial model StraightPipeBase
  parameter Real nParallel(min = 1) = 1 "Number of identical parallel pipes";
end StraightPipeBase;
partial model TwoPortFlowBase
  parameter Real nParallel(min = 1) = 1 "Number of identical parallel flow devices";
end TwoPortFlowBase;
model DynamicPipe
  extends StraightPipeBase;
  extends TwoPortFlowBase;
  Real volume = nParallel;
end DynamicPipe;
"#;
    let tree = resolve_test_source(source)
        .expect("a duplicate inherited parameter differing only by description must resolve")
        .into_inner();
    let first_base_n_parallel = tree
        .definitions
        .classes
        .get("StraightPipeBase")
        .and_then(|base| base.components.get("nParallel"))
        .and_then(|component| component.def_id)
        .expect("StraightPipeBase.nParallel DefId");
    let binding = tree
        .definitions
        .classes
        .get("DynamicPipe")
        .and_then(|pipe| pipe.components.get("volume"))
        .and_then(|component| component.binding.as_ref())
        .expect("DynamicPipe.volume binding");

    assert_eq!(
        find_comp_ref_def_id(binding),
        Some(first_base_n_parallel),
        "MLS §5.6.1.4 keeps the first of two identical duplicate elements"
    );
}

fn resolver_with_inheritance(
    class_names: &[(DefId, &str)],
    inheritance: &[(DefId, &[DefId])],
) -> Resolver {
    let mut resolver = Resolver::new();
    for &(def_id, name) in class_names {
        resolver.def_names.insert(def_id, name.to_string());
        resolver.name_to_def.insert(name.to_string(), def_id);
    }
    for &(class_id, base_ids) in inheritance {
        resolver.class_to_bases.insert(class_id, base_ids.to_vec());
    }
    resolver
}

#[test]
fn inherited_member_lookup_rejects_distinct_multiple_inheritance_candidates() {
    let derived = DefId::new(100);
    let base_a = DefId::new(101);
    let base_b = DefId::new(102);
    let state_a = DefId::new(103);
    let state_b = DefId::new(104);
    let resolver = resolver_with_inheritance(
        &[
            (derived, "Derived"),
            (base_a, "BaseA"),
            (base_b, "BaseB"),
            (state_a, "BaseA.State"),
            (state_b, "BaseB.State"),
        ],
        &[(derived, &[base_a, base_b])],
    );

    assert_eq!(
        resolver.lookup_inherited_member("Derived", "State"),
        None,
        "multiple inheritance must not choose an arbitrary redeclare target"
    );
}

#[test]
fn inherited_member_lookup_deduplicates_diamond_candidate_identity() {
    let derived = DefId::new(100);
    let left = DefId::new(101);
    let right = DefId::new(102);
    let root = DefId::new(103);
    let state = DefId::new(104);
    let resolver = resolver_with_inheritance(
        &[
            (derived, "Derived"),
            (left, "Left"),
            (right, "Right"),
            (root, "Root"),
            (state, "Root.State"),
        ],
        &[(derived, &[left, right]), (left, &[root]), (right, &[root])],
    );

    assert_eq!(
        resolver.lookup_inherited_member("Derived", "State"),
        Some(state),
        "the same declaration reached through both sides of a diamond is unambiguous"
    );
}
