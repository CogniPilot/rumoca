//! Component-modifier diagnostics on the class tree: unknown builtin,
//! nested, and class-member modifier names, and modifier value types.

use super::*;

#[test]
fn test_unknown_builtin_modifier_reports_error() {
    let source = r#"
        model Test
            Real x(startd = 1.0);
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let result = typecheck(resolved);
    assert!(result.is_err(), "typecheck should reject unknown modifiers");

    let diags = result.expect_err("expected diagnostics");
    assert!(
        diags.iter().any(|d| d.code.as_deref() == Some("ET001")
            && d.message.contains("unknown modifier `startd`")),
        "expected unknown modifier diagnostic, got: {:?}",
        diags
    );
}

#[test]
fn test_unknown_builtin_modifier_startdt_reports_error() {
    let source = r#"
        model Test
            Real x(startdt = 1.0);
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let result = typecheck(resolved);
    assert!(result.is_err(), "typecheck should reject unknown modifiers");

    let diags = result.expect_err("expected diagnostics");
    assert!(
        diags.iter().any(|d| d.code.as_deref() == Some("ET001")
            && d.message.contains("unknown modifier `startdt`")),
        "expected unknown modifier diagnostic, got: {:?}",
        diags
    );
}

#[test]
fn test_unknown_builtin_modifier_startdt_without_spaces_reports_error() {
    let source = r#"
        model Test
            Real x(startdt=1.0);
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let result = typecheck(resolved);
    assert!(result.is_err(), "typecheck should reject unknown modifiers");

    let diags = result.expect_err("expected diagnostics");
    assert!(
        diags.iter().any(|d| d.code.as_deref() == Some("ET001")
            && d.message.contains("unknown modifier `startdt`")),
        "expected unknown modifier diagnostic, got: {:?}",
        diags
    );
}

#[test]
fn test_unknown_class_component_modifier_reports_error() {
    let source = r#"
        model PID
            parameter Real kp = 1.0;
        end PID;

        model Test
            PID pid(kps = 10.0);
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let result = typecheck(resolved);
    assert!(
        result.is_err(),
        "typecheck should reject unknown class modifiers"
    );

    let diags = result.expect_err("expected diagnostics");
    assert!(
        diags
            .iter()
            .any(|d| d.code.as_deref() == Some("ET001")
                && d.message.contains("unknown modifier `kps`")),
        "expected unknown class modifier diagnostic, got: {:?}",
        diags
    );
}

#[test]
fn test_unknown_class_component_start_modifier_reports_error() {
    let source = r#"
        model Main
            Test t1(start=1), t2(start=2);
        end Main;

        model Test
            Real x;
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let result = typecheck(resolved);
    assert!(
        result.is_err(),
        "typecheck should reject unknown class modifiers"
    );

    let diags = result.expect_err("expected diagnostics");
    assert!(
        diags.iter().any(|d| d.code.as_deref() == Some("ET001")
            && d.message.contains("unknown modifier `start`")),
        "expected unknown class start modifier diagnostic, got: {:?}",
        diags
    );
}

#[test]
fn test_unknown_nested_builtin_modifier_reports_error() {
    let source = r#"
        model Plane
            Real x;
            Real y;
            Real theta;
        end Plane;

        model Test
            Plane p1(x.star88t = 1.0);
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let result = typecheck(resolved);
    assert!(
        result.is_err(),
        "typecheck should reject unknown nested builtin modifiers"
    );

    let diags = result.expect_err("expected diagnostics");
    assert!(
        diags.iter().any(|d| d.code.as_deref() == Some("ET001")
            && d.message.contains("unknown modifier `x.star88t`")),
        "expected unknown nested builtin modifier diagnostic, got: {:?}",
        diags
    );
}

#[test]
fn test_inherited_class_component_modifier_is_allowed() {
    let source = r#"
        model Base
            parameter Real kp = 1.0;
        end Base;

        model PID
            extends Base;
        end PID;

        model Test
            PID pid(kp = 10.0);
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let result = typecheck(resolved);
    assert!(
        result.is_ok(),
        "typecheck should allow inherited class member modifiers"
    );
}

#[test]
fn test_builtin_start_modifier_type_mismatch_reports_error() {
    let source = r#"
        model Test
            Boolean df = true;
            Real v(start = df);
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let result = typecheck(resolved);
    assert!(
        result.is_err(),
        "typecheck should reject incompatible builtin modifier types"
    );

    let diags = result.expect_err("expected diagnostics");
    assert!(
        diags.iter().any(|d| d.code.as_deref() == Some("ET002")
            && d.message.contains("modifier `start`")
            && d.message.contains("expects `Real`, found `Boolean`")),
        "expected modifier type mismatch diagnostic, got: {:?}",
        diags
    );
}

#[test]
fn test_builtin_fixed_modifier_type_mismatch_reports_error() {
    let source = r#"
        model Test
            Real v(fixed = 1);
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let result = typecheck(resolved);
    assert!(
        result.is_err(),
        "typecheck should reject incompatible builtin modifier types"
    );

    let diags = result.expect_err("expected diagnostics");
    assert!(
        diags.iter().any(|d| d.code.as_deref() == Some("ET002")
            && d.message.contains("modifier `fixed`")
            && d.message.contains("expects `Boolean`")),
        "expected modifier type mismatch diagnostic, got: {:?}",
        diags
    );
}
