//! Function call resolution: unresolved-call handling, target DefId
//! canonicalization, and source-scope preservation of the call path.

use super::*;

#[test]
fn test_unresolved_function_call_is_error() {
    let source = r#"
model Test
Real y;
equation
y = unknownFunc(1.0);
end Test;
"#;
    let result = resolve_parsed_tree_source(source);
    assert!(result.is_err(), "resolution should fail");

    let diags = result.expect_err("expected resolve diagnostics");
    assert!(
        diags
            .iter()
            .any(|d| d.message.contains("unresolved function call")
                && d.code.as_deref() == Some("ER002"))
    );
}

#[test]
fn failed_resolution_exposes_only_the_planning_artifact() {
    let source = r#"
model Test
  Real y = unknownFunc(1.0);
end Test;
"#;
    let failure: ResolveFailure = match resolve_with_diagnostics(parsed_tree_from_source(source)) {
        Ok(_) => panic!("undefined call must not produce ResolveSuccess"),
        Err(failure) => failure,
    };

    assert!(
        failure.diagnostics().has_errors(),
        "planning artifact must retain its exact failure diagnostics"
    );
    assert!(
        failure.tree().definitions.classes.contains_key("Test"),
        "planning remains available without receiving a ResolvedTree proof"
    );
}

#[test]
fn known_package_missing_function_is_rejected_at_the_call_target() {
    let source = r#"
package Known
  function present
    input Real u;
    output Real y;
  algorithm
    y := u;
  end present;
end Known;

model Test
  Real y;
equation
  y = Known.missing(1.0);
end Test;
"#;
    let diagnostics =
        resolve_parsed_tree_source(source).expect_err("missing package member must fail resolve");
    let diagnostic = diagnostics
        .iter()
        .find(|diagnostic| diagnostic.code.as_deref() == Some("ER002"))
        .expect("missing function must produce ER002");
    let span = diagnostic
        .labels
        .first()
        .expect("undefined call must carry source provenance")
        .span;

    assert_eq!(
        span.source,
        rumoca_core::SourceId::from_source_name("test.mo")
    );
    assert_eq!(&source[span.start.0..span.end.0], "Known.missing");
}

#[test]
fn known_package_function_carries_the_member_identity() {
    let source = r#"
package Known
  function present
    input Real u;
    output Real y;
  algorithm
    y := u;
  end present;
end Known;

model Test
  Real y = Known.present(1.0);
end Test;
"#;
    let tree = resolve_tree_source(source).into_inner();
    let binding = tree.definitions.classes["Test"].components["y"]
        .binding
        .as_ref()
        .expect("source has a binding");
    let target = extract_call_target(binding).expect("binding calls Known.present");
    let root_def_id = target
        .root_def_id()
        .expect("successful qualified call must retain its root identity");
    let target_def_id = target
        .target_def_id()
        .expect("successful call must carry its exact callable identity");

    assert_eq!(tree.def_map[&root_def_id], "Known");
    assert_eq!(tree.def_map[&target_def_id], "Known.present");
    assert_ne!(root_def_id, target_def_id);
}

#[test]
fn test_function_call_preserves_source_scope_and_resolves_target_def_id() {
    let source = r#"
package Interfaces
  partial package PartialMedium
replaceable function f
  input Real u;
  output Real y;
algorithm
  y := u;
end f;
  end PartialMedium;
end Interfaces;

package TableBased
  extends Interfaces.PartialMedium;
  redeclare function f
input Real u;
output Real y;
  algorithm
y := u + 1;
  end f;
end TableBased;

model UsesMediumAlias
  package Medium = TableBased;
  Real y;
equation
  y = Medium.f(1.0);
end UsesMediumAlias;
"#;
    let tree = resolve_tree_source(source).into_inner();
    let model = tree
        .definitions
        .classes
        .get("UsesMediumAlias")
        .expect("UsesMediumAlias should exist");
    let rumoca_ir_ast::Equation::Simple { rhs, .. } = &model.equations[0] else {
        panic!("expected simple equation");
    };
    let rumoca_ir_ast::Expression::FunctionCall { comp, .. } = rhs else {
        panic!("expected function call on rhs");
    };
    let root_def_id = comp
        .root_def_id()
        .expect("function call should retain root def_id");
    let def_id = comp
        .target_def_id()
        .expect("function call should have exact target_def_id");
    let resolved = tree
        .def_map
        .get(&def_id)
        .expect("resolved function def_id should exist in def_map");
    assert_eq!(
        resolved, "TableBased.f",
        "function call should resolve to canonical qualified function"
    );
    assert_eq!(tree.def_map[&root_def_id], "UsesMediumAlias.Medium");
    assert_eq!(
        comp.to_string(),
        "Medium.f",
        "function call path should preserve source component-reference scope"
    );
}
#[test]
fn test_inherited_medium_alias_function_call_preserves_source_scope() {
    let source = r#"
package Interfaces
  partial package PartialMedium
replaceable function density_pTX
  input Real p;
  input Real T;
  output Real d;
algorithm
  d := p + T;
end density_pTX;
  end PartialMedium;
end Interfaces;

package TableBased
  extends Interfaces.PartialMedium;
end TableBased;

model Base
  package Medium = TableBased;
end Base;

model Derived
  extends Base;
  Real d;
equation
  d = Medium.density_pTX(1.0, 2.0);
end Derived;
"#;
    let tree = resolve_tree_source(source).into_inner();
    let model = tree
        .definitions
        .classes
        .get("Derived")
        .expect("Derived should exist");
    let rumoca_ir_ast::Equation::Simple { rhs, .. } = &model.equations[0] else {
        panic!("expected simple equation");
    };
    let rumoca_ir_ast::Expression::FunctionCall { comp, .. } = rhs else {
        panic!("expected function call on rhs");
    };
    let def_id = comp
        .target_def_id()
        .expect("inherited Medium call should have exact target_def_id");
    let resolved = tree
        .def_map
        .get(&def_id)
        .expect("resolved function def_id should exist in def_map");
    assert_eq!(
        resolved, "Interfaces.PartialMedium.density_pTX",
        "inherited alias function should resolve to concrete target"
    );
    assert_eq!(
        comp.to_string(),
        "Medium.density_pTX",
        "function call path should preserve source component-reference scope"
    );
}

#[test]
fn test_component_binding_function_call_preserves_source_scope() {
    let source = r#"
package Interfaces
  partial package PartialMedium
replaceable function f
  input Real u;
  output Real y;
algorithm
  y := u;
end f;
  end PartialMedium;
end Interfaces;

package TableBased
  extends Interfaces.PartialMedium;
  redeclare function f
input Real u;
output Real y;
  algorithm
y := u + 2;
  end f;
end TableBased;

model UsesTableBasedState
  package Medium = TableBased;
  Real state = Medium.f(1.0);
end UsesTableBasedState;
"#;
    let tree = resolve_tree_source(source).into_inner();
    let model = tree
        .definitions
        .classes
        .get("UsesTableBasedState")
        .expect("UsesTableBasedState should exist");
    let state = model
        .components
        .get("state")
        .expect("state component should exist");

    let binding = state
        .binding
        .as_ref()
        .expect("state component should preserve explicit binding");
    let target = extract_call_target(binding).expect("binding should contain function call");
    let def_id = target
        .target_def_id()
        .expect("binding function call should have exact target_def_id");
    let resolved = tree
        .def_map
        .get(&def_id)
        .expect("resolved function def_id should exist in def_map");
    assert_eq!(resolved, "TableBased.f");
    assert_eq!(target.to_string(), "Medium.f");
}

fn extract_call_target(
    expr: &rumoca_ir_ast::Expression,
) -> Option<&rumoca_ir_ast::ComponentReference> {
    match expr {
        rumoca_ir_ast::Expression::FunctionCall { comp, .. } => Some(comp),
        rumoca_ir_ast::Expression::ClassModification { target, .. } => Some(target),
        _ => None,
    }
}

#[test]
fn test_binding_call_with_redeclared_record_alias_preserves_source_scope() {
    let source = r#"
package Common
  record BaseProps_Tpoly
Real T;
Real p;
  end BaseProps_Tpoly;
end Common;

package Interfaces
  partial package PartialMedium
replaceable record ThermodynamicState
  Real x;
end ThermodynamicState;

replaceable function setState_pTX
  input Real p;
  input Real T;
  output ThermodynamicState state;
  external "C";
end setState_pTX;
  end PartialMedium;
end Interfaces;

package TableBased
  extends Interfaces.PartialMedium(
redeclare record ThermodynamicState = Common.BaseProps_Tpoly
  );

  redeclare function setState_pTX
input Real p;
input Real T;
output ThermodynamicState state;
external "C";
  end setState_pTX;
end TableBased;

model UsesTableBasedState
  package Medium = TableBased;
  Medium.ThermodynamicState state = Medium.setState_pTX(1, 2);
end UsesTableBasedState;
"#;
    let tree = resolve_tree_source(source).into_inner();
    let model = tree
        .definitions
        .classes
        .get("UsesTableBasedState")
        .expect("UsesTableBasedState should exist");
    let state = model
        .components
        .get("state")
        .expect("state component should exist");
    let binding = state
        .binding
        .as_ref()
        .expect("state component should preserve explicit binding");
    let target = extract_call_target(binding).expect("binding should contain function call");
    let def_id = target
        .target_def_id()
        .expect("binding function call should have exact target_def_id");
    let resolved = tree
        .def_map
        .get(&def_id)
        .expect("resolved function def_id should exist in def_map");
    assert_eq!(resolved, "TableBased.setState_pTX");
    assert_eq!(target.to_string(), "Medium.setState_pTX");
}
