//! `import` clause resolution: renamed and short package aliases, import
//! lookup origin rules, and unresolved/invalid import diagnostics.

use super::*;

#[test]
fn test_record_parameter_type_resolves_through_renamed_package_import() {
    let source = r#"
package Modelica
  package Units
    package SI
      operator record ComplexVoltage
        Real re;
        Real im;
      end ComplexVoltage;
    end SI;
  end Units;
end Modelica;

package QuasiStatic
  import SI = Modelica.Units.SI;

  function activePower
    input SI.ComplexVoltage voltage[:];
    output Real power;
  algorithm
    power := voltage[1].re;
  end activePower;
end QuasiStatic;
"#;
    let tree = resolve_test_source(source).expect("resolution should succeed");
    let function = tree
        .definitions
        .classes
        .get("QuasiStatic")
        .and_then(|package| package.classes.get("activePower"))
        .expect("QuasiStatic.activePower should exist");
    let type_name = function
        .components
        .get("voltage")
        .expect("voltage input should exist")
        .type_def_id
        .and_then(|def_id| tree.def_map.get(&def_id));

    assert_eq!(
        type_name.map(String::as_str),
        Some("Modelica.Units.SI.ComplexVoltage"),
        "renamed package imports must preserve the record declaration identity"
    );
}
#[test]
fn test_short_package_alias_member_lookup_resolves_inherited_member() {
    let source = r#"
package PhaseSystems
  package ThreePhase_dq0
function j
  input Real x;
  output Real y;
algorithm
  y := x;
end j;
  end ThreePhase_dq0;
end PhaseSystems;

package AC3ph
  package Ports
model PortBase
  package PS = PhaseSystems.ThreePhase_dq0;
  function j = PS.j;
  Real y;
equation
  y = j(1.0);
end PortBase;
  end Ports;
end AC3ph;
"#;

    resolve_test_source(source)
        .expect("short package alias member access like `PS.j` should resolve");
}
#[test]
fn test_unresolved_import_is_emitted_before_unresolved_type_reference() {
    let source = r#"
model Ball
import Modelica.Blocks.Continuous.PID;
PID pid();
end Ball;
"#;
    let result = resolve_parsed_tree_source(source);
    assert!(result.is_err(), "resolution should fail");

    let diags = result.expect_err("expected resolve diagnostics");
    let messages: Vec<_> = diags.iter().map(|d| d.message.as_str()).collect();

    let import_pos = messages
        .iter()
        .position(|msg| msg.contains("unresolved import") && msg.contains("PID"));
    let type_pos = messages
        .iter()
        .position(|msg| msg.contains("unresolved type reference") && msg.contains("PID"));

    assert!(
        import_pos.is_some(),
        "expected unresolved import diagnostic, got: {messages:?}"
    );
    assert!(
        type_pos.is_some(),
        "expected unresolved type reference diagnostic, got: {messages:?}"
    );
    assert!(
        import_pos.expect("import diagnostic index")
            < type_pos.expect("unresolved type diagnostic index"),
        "expected import diagnostic before unresolved type reference, got: {messages:?}"
    );
}

#[test]
fn test_unresolved_diagnostics_include_source_labels() {
    let source = r#"
model Ball
import Modelica.Blocks.Continuous.PID;
PID pid();
equation
der(x) = x;
end Ball;
"#;
    let result = resolve_parsed_tree_source(source);
    assert!(result.is_err(), "resolution should fail");

    let diags = result.expect_err("expected resolve diagnostics");
    let import = diags
        .iter()
        .find(|d| d.message.contains("unresolved import"))
        .expect("missing unresolved import diagnostic");
    let unresolved_type = diags
        .iter()
        .find(|d| d.message.contains("unresolved type reference"))
        .expect("missing unresolved type reference diagnostic");

    assert!(
        !import.labels.is_empty(),
        "unresolved import should include a source label"
    );
    assert!(
        !unresolved_type.labels.is_empty(),
        "unresolved type reference should include a source label"
    );
}

#[test]
fn test_unresolved_selective_import_member_is_error() {
    let source = r#"
package P
  model A
  end A;
end P;

model M
  import P.{A, B};
end M;
"#;
    let result = resolve_parsed_tree_source(source);
    assert!(result.is_err(), "resolution should fail");

    let diags = result.expect_err("expected resolve diagnostics");
    let import = diags
        .iter()
        .find(|d| d.message.contains("unresolved import member") && d.message.contains("B"))
        .expect("missing unresolved selective import member diagnostic");

    assert_eq!(import.code.as_deref(), Some("ER002"));
    assert!(
        !import.labels.is_empty(),
        "unresolved selective import member should include source label"
    );
}

#[test]
fn test_import_first_segment_resolves_from_top_level() {
    let source = r#"
package Library
  type Count = Integer;
end Library;
package Outer
  package Library
  end Library;
  model M
    import Count = Library.Count;
    Count n;
  end M;
end Outer;
"#;

    resolve_test_source(source)
        .expect("the nested Library must not shadow the top-level import path");
}

#[test]
fn test_import_cannot_start_from_enclosing_package_member() {
    let source = r#"
package Outer
  package LocalLibrary
    type Count = Integer;
  end LocalLibrary;
  model M
    import Count = LocalLibrary.Count;
    Count n;
  end M;
end Outer;
"#;

    let diagnostics = resolve_test_source(source)
        .expect_err("import paths must start at top level, not in an enclosing package");
    assert!(
        diagnostics.iter().any(|diag| {
            diag.code.as_deref() == Some("ER002")
                && diag.message.contains("unresolved import")
                && diag.message.contains("LocalLibrary.Count")
        }),
        "expected unresolved import diagnostic, got: {diagnostics:?}"
    );
}

#[test]
fn test_import_from_non_package_is_rejected() {
    let source = r#"
model Outer
  model Inner
  end Inner;
end Outer;

model Test
  import Outer.Inner;
  Inner x;
end Test;
"#;
    let result = resolve_parsed_tree_source(source);
    assert!(result.is_err(), "resolution should fail");

    let diags = result.expect_err("expected resolve diagnostics");
    assert!(diags.iter().any(|d| {
        d.code.as_deref() == Some("ER002")
            && d.message.contains("invalid import target")
            && d.message.contains("Outer.Inner")
    }));
}

#[test]
fn test_single_segment_class_import_is_allowed() {
    let source = r#"
operator record Complex
  encapsulated operator function '0'
import Complex;
output Complex result;
  algorithm
result := Complex(0);
  end '0';
end Complex;
"#;
    let result = resolve_parsed_tree_source(source);
    assert!(
        result.is_ok(),
        "single-segment class import must be allowed for operator records"
    );
}

#[test]
fn test_import_cannot_traverse_non_package_member() {
    let source = r#"
package P
  model A
constant Real x = 1;
  end A;
end P;

model Test
  import P.A.x;
  Real y;
equation
  y = x;
end Test;
"#;
    let result = resolve_parsed_tree_source(source);
    assert!(result.is_err(), "resolution should fail");

    let diags = result.expect_err("expected resolve diagnostics");
    assert!(diags.iter().any(|d| {
        d.code.as_deref() == Some("ER002")
            && d.message.contains("invalid import target")
            && d.message.contains("P.A.x")
    }));
}
