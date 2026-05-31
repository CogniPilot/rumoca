use super::*;

#[test]
fn unknown_nested_builtin_modifier_is_reported_via_lsp_compile_diagnostics() {
    let source = r#"
model Plane
  Real x, y, theta;
equation
  der(x) = cos(theta);
  der(y) = sin(theta);
  der(theta) = 1;
end Plane;

model Sim
  Plane p1(x.star88t = 1.0), p2(y.start = 10.0);
end Sim;
"#;
    let mut session = Session::default();
    let diagnostics = compute_diagnostics(source, "input.mo", Some(&mut session));
    assert!(
        diagnostics.iter().any(|d| {
            d.code == Some(NumberOrString::String("ET001".to_string()))
                && d.message.contains("unknown modifier `x.star88t`")
        }),
        "expected unknown nested builtin modifier diagnostic, got: {:?}",
        diagnostics
    );
}

#[test]
fn unknown_inherited_block_member_is_reported_before_balance_diagnostics() {
    let lib = r#"
package Modelica
  package Blocks
    package Interfaces
      partial block SISO
        input Real u;
        output Real y;
      end SISO;
    end Interfaces;

    package Continuous
      block PID
        extends Modelica.Blocks.Interfaces.SISO;
        parameter Real k = 1;
      equation
        y = k * u;
      end PID;
    end Continuous;
  end Blocks;
end Modelica;
"#;
    let source = r#"
model Ball
  import Modelica.Blocks.Continuous.PID;
  Real x(start=10);
  Real v(start=1);
  parameter Real g = 9.81;
  PID pid(k=10);
equation
  pid.u2 = 3 - x;
  der(x) = v;
  der(v) = -g + pid.y;
end Ball;
"#;
    let mut session = Session::default();
    let parsed =
        parse_source_to_ast_with_errors(lib, "Modelica/package.mo").expect("parse source root");
    let inserted = session.replace_parsed_source_set(
        "external::Modelica",
        SourceRootKind::DurableExternal,
        vec![("Modelica/package.mo".to_string(), parsed)],
        None,
    );
    assert_eq!(inserted, 1, "source root should be inserted");

    let diagnostics = compute_diagnostics(source, "input.mo", Some(&mut session));
    let unknown_member = diagnostics
        .iter()
        .find(|d| {
            d.code == Some(NumberOrString::String("ET001".to_string()))
                && d.message.contains("unknown member `u2`")
                && d.message.contains("pid.u2")
        })
        .unwrap_or_else(|| {
            panic!(
                "expected unknown inherited block member diagnostic, got: {:?}",
                diagnostics
            )
        });
    assert_eq!(unknown_member.range.start.line, 8);
    assert_eq!(unknown_member.range.start.character, 6);
    assert_eq!(unknown_member.range.end.line, 8);
    assert_eq!(unknown_member.range.end.character, 8);
    assert_eq!(unknown_member.data, Some(json!({ "precise_range": true })));
    assert!(
        !diagnostics
            .iter()
            .any(|d| d.message.contains("unbalanced model")),
        "member error should block later balance diagnostics, got: {:?}",
        diagnostics
    );
}

#[test]
fn global_resolution_failures_are_not_duplicated_per_class_target() {
    let source = r#"
model A
  Real x;
equation
  x = 1;
end A;

model B
  Real y;
equation
  y = 2;
end B;
"#;
    let mut session = Session::default();
    session
        .add_document(
            "other.mo",
            r#"
model A
  Real z;
equation
  z = 3;
end A;
"#,
        )
        .expect("preload parses");
    let diagnostics = compute_diagnostics(source, "input.mo", Some(&mut session));
    let dup_class_diags = diagnostics
        .iter()
        .filter(|d| d.message.contains("Duplicate class 'A'"))
        .count();
    assert_eq!(
        dup_class_diags, 1,
        "expected one global merge diagnostic, got: {:?}",
        diagnostics
    );
}

#[test]
fn unresolved_source_root_diagnostics_do_not_panic_or_leak_into_active_file() {
    let source = "model M\n  Real x;\nequation\n  der(x) = -x;\nend M;\n";
    let mut session = Session::default();

    let mut broken_source_root = String::from("model BrokenLib\n  Real x;\nequation\n");
    for _ in 0..256 {
        broken_source_root.push_str("  // filler\n");
    }
    broken_source_root.push_str("  x = unknownLibFn(1.0);\nend BrokenLib;\n");

    session
        .add_document("lib_with_error.mo", &broken_source_root)
        .expect("source-root preload should parse");

    let diagnostics = compute_diagnostics(source, "input.mo", Some(&mut session));
    assert!(
        diagnostics
            .iter()
            .all(|diag| !diag.message.contains("unknownLibFn")),
        "source-root-only unresolved diagnostics should be filtered out for active file: {:?}",
        diagnostics
    );
}

#[test]
fn unknown_builtin_modifier_is_reported_with_preloaded_source_root_session() {
    let source = r#"
model M
  Real x(startd = 1.0);
equation
  der(x) = -x;
end M;
"#;
    let mut session = Session::default();
    session
        .add_document(
            "Lib.mo",
            r#"
package Lib
  model Helper
Real y;
  equation
y = 1.0;
  end Helper;
end Lib;
"#,
        )
        .expect("source-root preload should parse");

    let diagnostics = compute_diagnostics(source, "input.mo", Some(&mut session));
    assert!(
        diagnostics.iter().any(|d| {
            d.code == Some(NumberOrString::String("ET001".to_string()))
                && d.message.contains("unknown modifier `startd`")
        }),
        "expected unknown-modifier diagnostic with preloaded source roots, got: {:?}",
        diagnostics
    );
}

#[test]
fn unknown_builtin_modifier_startdt_is_reported_with_preloaded_source_root_session() {
    let source = r#"
model M
  Real x(startdt=1.0);
equation
  der(x) = -x;
end M;
"#;
    let mut session = Session::default();
    session
        .add_document(
            "Lib.mo",
            r#"
package Lib
  model Helper
Real y;
  equation
y = 1.0;
  end Helper;
end Lib;
"#,
        )
        .expect("source-root preload should parse");

    let diagnostics = compute_diagnostics(source, "input.mo", Some(&mut session));
    assert!(
        diagnostics.iter().any(|d| {
            d.code == Some(NumberOrString::String("ET001".to_string()))
                && d.message.contains("unknown modifier `startdt`")
        }),
        "expected unknown-modifier diagnostic with preloaded source roots, got: {:?}",
        diagnostics
    );
}

#[test]
fn unknown_builtin_modifier_highlights_modifier_identifier() {
    let source = "model M\n  Real x(sltart=0);\nequation\n  der(x) = -x;\nend M;\n";
    let mut session = Session::default();
    let diagnostics = compute_diagnostics(source, "input.mo", Some(&mut session));
    let unknown_modifier = diagnostics
        .iter()
        .find(|d| d.message.contains("unknown modifier `sltart`"))
        .unwrap_or_else(|| {
            panic!(
                "expected unknown-modifier diagnostic, got: {:?}",
                diagnostics
            )
        });
    assert_eq!(unknown_modifier.range.start.line, 1);
    assert_eq!(unknown_modifier.range.start.character, 9);
    assert_eq!(unknown_modifier.range.end.line, 1);
    assert_eq!(unknown_modifier.range.end.character, 15);
    assert_eq!(
        unknown_modifier.data,
        Some(json!({ "precise_range": true }))
    );
}

#[test]
fn builtin_modifier_type_mismatch_is_reported_via_lsp_compile_diagnostics() {
    let source = r#"
model M
  Boolean df = true;
  Real v(start = df);
equation
  der(v) = -v;
end M;
"#;
    let mut session = Session::default();
    let diagnostics = compute_diagnostics(source, "input.mo", Some(&mut session));
    assert!(
        diagnostics.iter().any(|d| {
            d.code == Some(NumberOrString::String("ET002".to_string()))
                && d.message.contains("modifier `start`")
                && d.message.contains("expects `Real`, found `Boolean`")
        }),
        "expected modifier type mismatch diagnostic, got: {:?}",
        diagnostics
    );
}
