//! Deferred receiver admission by recorded identity.
//!
//! A receiver-qualified call across a replaceable edge leaves Resolve as a
//! deferred reference carrying the receiver's resolved root `DefId`. The
//! resolver records that identity at the lookup that proves the receiver;
//! deriving the first segment again from its spelling would reject inherited
//! receivers and conflate same-spelled receivers at different nesting depths.

use super::*;

fn class_def_id(tree: &ResolvedTree, qualified_name: &str) -> DefId {
    tree.inner()
        .get_class_by_qualified_name(qualified_name)
        .and_then(|class| class.def_id)
        .unwrap_or_else(|| panic!("class `{qualified_name}` must resolve with a DefId"))
}

fn binding_call_root(tree: &ResolvedTree, class_name: &str, comp_name: &str) -> Option<DefId> {
    let class = tree
        .inner()
        .get_class_by_qualified_name(class_name)
        .unwrap_or_else(|| panic!("class `{class_name}` must exist"));
    let comp = class
        .components
        .get(comp_name)
        .unwrap_or_else(|| panic!("component `{comp_name}` must exist in `{class_name}`"));
    let binding = comp
        .binding
        .as_ref()
        .unwrap_or_else(|| panic!("component `{comp_name}` must carry its binding"));
    call_root_def_id(binding)
}

fn call_root_def_id(expr: &rumoca_ir_ast::Expression) -> Option<DefId> {
    match expr {
        ast::Expression::FunctionCall { comp, .. } => comp.root_def_id(),
        ast::Expression::Binary { lhs, rhs, .. } => {
            call_root_def_id(lhs).or_else(|| call_root_def_id(rhs))
        }
        ast::Expression::Unary { rhs, .. } => call_root_def_id(rhs),
        _ => None,
    }
}

/// An inherited replaceable-package receiver is admitted by its recorded
/// identity even when the lexical scope holds an unrelated same-spelled
/// package. A spelling re-lookup of the first segment rejects this shape
/// with a false `ER002` unresolved function call.
#[test]
fn inherited_replaceable_receiver_call_is_admitted_by_identity() {
    let source = r#"
package P
  package GoodMedium
    function density
      output Real y;
    algorithm
      y := 2;
    end density;
  end GoodMedium;
  model Base
    replaceable package Medium = GoodMedium constrainedby GoodMedium;
  end Base;
  package Medium
  end Medium;
  model Use
    extends Base;
    Real d = Medium.density();
  end Use;
end P;
"#;
    let result = resolve_parsed_tree_source(source);
    let tree = match result {
        Ok(tree) => tree,
        Err(diags) => panic!("inherited replaceable receiver must resolve, got: {diags:?}"),
    };

    let recorded_root = binding_call_root(&tree, "P.Use", "d")
        .expect("deferred call must retain its receiver identity");
    let inherited_slot = class_def_id(&tree, "P.Base.Medium");
    let outer_same_spelled = class_def_id(&tree, "P.Medium");
    assert_eq!(
        recorded_root, inherited_slot,
        "call receiver must be the inherited replaceable slot"
    );
    assert_ne!(
        recorded_root, outer_same_spelled,
        "call receiver must not be captured by the same-spelled outer package"
    );
}

/// Two same-spelled `Medium` aliases at different nesting depths select
/// different packages: each call's recorded receiver is the alias declared in
/// its own class, never the same-spelled alias at another depth.
#[test]
fn same_spelled_receivers_at_different_depths_select_different_packages() {
    let source = r#"
package P
  package MedA
    function f
      output Real y;
    algorithm
      y := 1;
    end f;
  end MedA;
  package MedB
    function f
      output Real y;
    algorithm
      y := 2;
    end f;
  end MedB;
  model Housing
    replaceable package Medium = MedA constrainedby MedA;
    Real od = Medium.f();
    model Inner
      replaceable package Medium = MedB constrainedby MedB;
      Real id = Medium.f();
    end Inner;
  end Housing;
end P;
"#;
    let result = resolve_parsed_tree_source(source);
    let tree = match result {
        Ok(tree) => tree,
        Err(diags) => panic!("same-spelled receivers must resolve, got: {diags:?}"),
    };

    let outer_slot = class_def_id(&tree, "P.Housing.Medium");
    let inner_slot = class_def_id(&tree, "P.Housing.Inner.Medium");
    assert_ne!(outer_slot, inner_slot, "the two slots are distinct");

    let outer_root = binding_call_root(&tree, "P.Housing", "od")
        .expect("outer call must retain its receiver identity");
    let inner_root = binding_call_root(&tree, "P.Housing.Inner", "id")
        .expect("inner call must retain its receiver identity");
    assert_eq!(outer_root, outer_slot, "outer call selects the outer alias");
    assert_eq!(inner_root, inner_slot, "inner call selects the inner alias");
    assert_ne!(
        outer_root, inner_root,
        "same spelling must never merge distinct receiver identities"
    );
}
/// The pump-monitoring shape from the pinned `Modelica.Fluid.Machines`:
/// the receiver is inherited and redeclared by the extends clause, and the
/// calls go through the inherited alias.
#[test]
fn inherited_and_extends_redeclared_receiver_resolves() {
    let source = r#"
package P
  package PartialMedium
    type Density = Real;
    function density
      input Real state;
      output Real d;
    algorithm
      d := state;
    end density;
  end PartialMedium;
  package TwoPhaseMedium
    extends PartialMedium;
    function saturationPressure
      input Real state;
      output Real p;
    algorithm
      p := state;
    end saturationPressure;
  end TwoPhaseMedium;
  model MonitoringBase
    replaceable package Medium = PartialMedium;
    input Real state_in;
  end MonitoringBase;
  model MonitoringNPSH
    extends MonitoringBase(redeclare replaceable package Medium = TwoPhaseMedium);
    Medium.Density rho_in = Medium.density(state_in);
  end MonitoringNPSH;
end P;
"#;
    let result = resolve_parsed_tree_source(source);
    if let Err(diags) = &result {
        panic!("inherited redeclared receiver must resolve, got: {diags:?}");
    }
}

/// The compressible-liquids shape from the pinned `Modelica.Media`: a static
/// package chain whose instance-dependent edge sits past the first segment
/// (`Lib.Water.StandardWater.density(...)` with `StandardWater` replaceable).
/// The deferral must be admitted from the recorded segment identities; any
/// first-segment gate, spelling-keyed or root-kind-keyed, rejects this shape
/// with a false `ER002`.
#[test]
fn static_package_chain_with_dynamic_middle_segment_is_deferred() {
    let source = r#"
package Lib
  package Water
    package WaterModel
      function density
        input Real s;
        output Real d;
      algorithm
        d := s;
      end density;
    end WaterModel;
    replaceable package StandardWater = WaterModel;
  end Water;
end Lib;
model Use
  Real d = Lib.Water.StandardWater.density(1.0);
end Use;
"#;
    let result = resolve_parsed_tree_source(source);
    let tree = match result {
        Ok(tree) => tree,
        Err(diags) => panic!("dynamic-middle-segment chain must resolve, got: {diags:?}"),
    };

    let class = tree
        .inner()
        .get_class_by_qualified_name("Use")
        .expect("Use must exist");
    let comp = class.components.get("d").expect("d must exist");
    let binding = comp.binding.as_ref().expect("d must carry its binding");
    let ast::Expression::FunctionCall { comp: call, .. } = binding else {
        panic!("binding must be the deferred call");
    };
    let standard_water = class_def_id(&tree, "Lib.Water.StandardWater");
    assert_eq!(
        call.parts.get(2).and_then(|part| part.def_id),
        Some(standard_water),
        "the recorded prefix must reach the replaceable edge by identity"
    );
    assert_eq!(
        call.parts.get(3).and_then(|part| part.def_id),
        None,
        "the member past the replaceable edge stays deferred to instantiation"
    );
}

/// A leading dot selects the global scope even when an enclosing package owns
/// a same-spelled decoy. Lexical lookup would bind `P.A` and then either select
/// the wrong function or reject the valid global call as a missing static tail.
#[test]
fn leading_dot_call_selects_global_receiver_identity() {
    let source = r#"
package A
  function f
    output Real y;
  algorithm
    y := 1;
  end f;
end A;
package P
  package A
  end A;
  model Use
    Real y = .A.f();
  end Use;
end P;
"#;
    let tree = resolve_parsed_tree_source(source)
        .unwrap_or_else(|diagnostics| panic!("global receiver must resolve: {diagnostics:?}"));

    let recorded_root =
        binding_call_root(&tree, "P.Use", "y").expect("global call must retain its root identity");
    let global_a = class_def_id(&tree, "A");
    let lexical_decoy = class_def_id(&tree, "P.A");
    assert_eq!(recorded_root, global_a, "leading dot must select global A");
    assert_ne!(
        recorded_root, lexical_decoy,
        "leading dot must bypass the enclosing same-spelled package"
    );
}
