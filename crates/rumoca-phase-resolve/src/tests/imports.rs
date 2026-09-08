//! `import` clause resolution: renamed and short package aliases, import
//! lookup origin rules, and unresolved/invalid import diagnostics.

use super::*;

fn component_type_path<'a>(tree: &'a ast::ClassTree, class: &str, component: &str) -> &'a str {
    let definition = tree
        .definitions
        .classes
        .get(class)
        .unwrap_or_else(|| panic!("missing class {class}"));
    let type_id = definition
        .components
        .get(component)
        .unwrap_or_else(|| panic!("missing component {class}.{component}"))
        .type_def_id
        .unwrap_or_else(|| panic!("missing type identity for {class}.{component}"));
    tree.def_map
        .get(&type_id)
        .map(String::as_str)
        .unwrap_or_else(|| panic!("missing definition path for {class}.{component}"))
}

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
fn inherited_package_import_can_supply_an_extends_target() {
    let source = r#"
package BaseLibrary
  package Types
    model Parent
      Real x;
    end Parent;
  end Types;
end BaseLibrary;

package Library
  extends BaseLibrary;
end Library;

model Child
  import Library.Types.Parent;
  extends Parent;
end Child;
"#;

    let tree = resolve_test_source(source)
        .expect("imports must traverse the effective inherited package view before extends lookup");
    let child = tree
        .definitions
        .classes
        .get("Child")
        .expect("Child must be registered");
    let parent = child.extends[0]
        .base_def_id
        .and_then(|def_id| tree.def_map.get(&def_id));
    assert_eq!(parent.map(String::as_str), Some("BaseLibrary.Types.Parent"));
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
fn inherited_declaration_precedes_qualified_and_wildcard_imports() {
    let source = r#"
package Imported
  model X end X;
end Imported;
package Wild
  model X end X;
end Wild;
model Base
  model X end X;
end Base;
model Use
  extends Base;
  import Imported.X;
  import Wild.*;
  X value;
end Use;
"#;
    let tree = resolve_test_source(source).expect("inherited X must resolve before imports");
    assert_eq!(component_type_path(&tree, "Use", "value"), "Base.X");
}

#[test]
fn single_definition_import_precedes_wildcard_in_both_clause_orders() {
    for imports in [
        "import Wild.*; import Named.X;",
        "import Named.X; import Wild.*;",
    ] {
        let source = format!(
            r#"
package Named
  model X end X;
end Named;
package Wild
  model X end X;
end Wild;
model Use
  {imports}
  X value;
end Use;
"#
        );
        let tree = resolve_test_source(&source)
            .unwrap_or_else(|diagnostics| panic!("named import must win: {diagnostics:?}"));
        assert_eq!(component_type_path(&tree, "Use", "value"), "Named.X");
    }
}

#[test]
fn selective_import_precedes_wildcard_in_both_clause_orders() {
    for imports in [
        "import Wild.*; import Named.{X};",
        "import Named.{X}; import Wild.*;",
    ] {
        let source = format!(
            r#"
package Named
  model X end X;
end Named;
package Wild
  model X end X;
end Wild;
model Use
  {imports}
  X value;
end Use;
"#
        );
        let tree = resolve_test_source(&source)
            .unwrap_or_else(|diagnostics| panic!("selective import must win: {diagnostics:?}"));
        assert_eq!(component_type_path(&tree, "Use", "value"), "Named.X");
    }
}

#[test]
fn import_precedes_enclosing_scope_member() {
    let source = r#"
package Named
  model X end X;
end Named;
package Outer
  model X end X;
  model Use
    import Named.X;
    X value;
  end Use;
end Outer;
"#;
    let tree = resolve_test_source(source).expect("import must win before the parent walk");
    let use_class = tree.definitions.classes["Outer"]
        .classes
        .get("Use")
        .expect("missing Outer.Use");
    let type_id = use_class.components["value"]
        .type_def_id
        .expect("missing imported type identity");
    assert_eq!(tree.def_map[&type_id], "Named.X");
}

#[test]
fn wildcard_ambiguity_is_diagnosed_at_use_and_unused_overlap_is_accepted() {
    let declarations = r#"
package A constant Real x = 1; end A;
package B constant Real x = 2; end B;
"#;
    let used = format!(
        r#"{declarations}
model Use
  import A.*;
  import B.*;
  Real y = x;
end Use;
"#
    );
    let diagnostics = resolve_test_source(&used).expect_err("used overlap must be ambiguous");
    assert_eq!(
        diagnostics
            .iter()
            .filter(|diagnostic| diagnostic.code.as_deref() == Some("ER112"))
            .count(),
        1,
        "the lookup consumer must emit exactly one ER112: {diagnostics:?}"
    );

    let unused = format!(
        r#"{declarations}
model Use
  import A.*;
  import B.*;
  Real y = 0;
end Use;
"#
    );
    resolve_test_source(&unused).expect("unused wildcard overlap is legal");
}

#[test]
fn wildcard_overlap_does_not_capture_instance_owned_modifier_targets() {
    let source = r#"
package A constant Real tol = 1e-3; end A;
package B constant Real tol = 1e-6; end B;
model Sub Real tol = 1; end Sub;
model Base Sub sub; end Base;
model Use
  import A.*;
  import B.*;
  extends Base(sub(tol = 3));
end Use;
"#;

    resolve_test_source(source)
        .expect("the nested modifier target names Sub.tol; lexical wildcard overlap is irrelevant");
}

#[test]
fn wildcard_ambiguity_is_diagnosed_by_every_lookup_use_kind() {
    let cases = [
        r#"
package A constant Real x = 1; end A;
package B constant Real x = 2; end B;
model Use
  import A.*; import B.*;
  Real y;
algorithm
  y := x;
end Use;
"#,
        r#"
package A model X end X; end A;
package B model X end X; end B;
model Use
  import A.*; import B.*;
  X value;
end Use;
"#,
        r#"
package A constant Integer n = 1; end A;
package B constant Integer n = 2; end B;
model Use
  import A.*; import B.*;
  Real value[n];
end Use;
"#,
        r#"
package A
  function f input Real x; output Real y; algorithm y := x; end f;
end A;
package B
  function f input Real x; output Real y; algorithm y := x; end f;
end B;
model Use
  import A.*; import B.*;
  Real value = f(1);
end Use;
"#,
        r#"
package A model X end X; end A;
package B model X end X; end B;
model Use
  import A.*; import B.*;
  extends X;
end Use;
"#,
    ];

    for source in cases {
        let diagnostics = resolve_test_source(source)
            .expect_err("every ambiguous wildcard use must fail at Resolve");
        assert_eq!(
            diagnostics
                .iter()
                .filter(|diagnostic| diagnostic.code.as_deref() == Some("ER112"))
                .count(),
            1,
            "expected exactly one producer-owned ER112: {diagnostics:?}"
        );
    }
}

#[test]
fn higher_precedence_match_suppresses_wildcard_ambiguity() {
    let source = r#"
package A model X end X; end A;
package B model X end X; end B;
model Base
  model X Real selected; end X;
end Base;
model InheritedWins
  extends Base;
  import A.*;
  import B.*;
  X value;
equation
  value.selected = 1;
end InheritedWins;
package Named
  model X Real selected; end X;
end Named;
model NamedWins
  import A.*;
  import B.*;
  import Named.X;
  X value;
equation
  value.selected = 1;
end NamedWins;
"#;
    resolve_test_source(source)
        .expect("inherited and named matches must stop before wildcard ambiguity");
}

#[test]
fn wildcard_import_preserves_ambiguity_exported_by_its_package() {
    let source = r#"
package A constant Real x = 1; end A;
package B constant Real x = 2; end B;
package P
  extends A;
  extends B;
end P;
package Outer
  constant Real x = 3;
  model Use
    import P.*;
    Real y = x;
  end Use;
end Outer;
"#;
    let diagnostics =
        resolve_test_source(source).expect_err("P.x ambiguity must not disappear into Outer.x");
    assert!(diagnostics.iter().any(|diagnostic| {
        diagnostic.code.as_deref() == Some("ER002")
            && diagnostic.message.contains("ambiguous inherited reference")
    }));
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
