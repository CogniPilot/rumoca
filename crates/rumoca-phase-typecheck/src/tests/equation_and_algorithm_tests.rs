//! Equation and algorithm-section checking: assignment compatibility,
//! operator/member diagnostics, and user-defined equation compatibility.

use super::*;

#[test]
fn test_equation_typecheck() {
    let source = r#"
        model Test
            Real x;
            Real y;
        equation
            x = y + 1;
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let result = typecheck(resolved);
    assert!(result.is_ok());
}

#[test]
fn test_builtin_numeric_assignment_conversion_is_directional() {
    let accepted = typecheck_diagnostics(
        r#"
        model Accepted
            Real r;
            Integer i;
        algorithm
            r := i;
        end Accepted;
        "#,
    );
    assert!(
        accepted
            .iter()
            .all(|diagnostic| diagnostic.code.as_deref() != Some("ET002")),
        "Integer-to-Real assignment should be accepted: {accepted:?}"
    );

    let rejected = typecheck_diagnostics(
        r#"
        model Rejected
            Real r;
            Integer i;
        algorithm
            i := r;
        end Rejected;
        "#,
    );
    assert!(
        rejected
            .iter()
            .any(|diagnostic| diagnostic.code.as_deref() == Some("ET002")),
        "Real-to-Integer assignment must be rejected: {rejected:?}"
    );
}

#[test]
fn test_clock_is_not_assignment_compatible_with_real() {
    let diagnostics = typecheck_diagnostics(
        r#"
        model Test
            Clock c;
            Real r;
        equation
            c = r;
        end Test;
        "#,
    );

    assert!(
        diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code.as_deref() == Some("ET002")),
        "Clock and Real must remain distinct builtin types: {diagnostics:?}"
    );
}

#[test]
fn test_algorithm_typecheck() {
    let source = r#"
        model Test
            Real x;
            Real y;
        algorithm
            x := y + 1;
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let result = typecheck(resolved);
    assert!(result.is_ok());
}

#[test]
fn test_typecheck_rejects_unknown_operator_record_member_reference() {
    // MLS §5.3/§5.6: each dotted component-reference segment must resolve
    // against the declared component type during flattening.
    let source = r#"
        operator record SE2
            Real x;
            Real y;
            Real theta;
        end SE2;

        model Test2
            SE2 pose;
        equation
            der(pose.x) = 1;
            der(pose.y) = 0;
            der(pose.z) = 2;
        end Test2;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let err = typecheck(resolved).expect_err("unknown record member should fail typecheck");
    assert!(
        err.iter().any(|d| d.code.as_deref() == Some("ET001")
            && d.message.contains("unknown member `z`")
            && d.message.contains("pose.z")),
        "expected unknown-member diagnostic, got: {:?}",
        err
    );
}

#[test]
fn test_user_defined_equation_compatibility() {
    let source = r#"
        type Mode = enumeration(Off, On);
        record Payload
            Real x;
        end Payload;

        model Test
            Mode m1;
            Mode m2;
            Payload p1;
            Payload p2;
        equation
            m1 = m2;
            p1 = p2;
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let result = typecheck(resolved);
    assert!(
        result.is_ok(),
        "same enum/record types should be compatible"
    );
}

#[test]
fn test_equation_shape_mismatch_detection() {
    let source = r#"
        model Test
            Real lhs[2];
            Real rhs[3];
        equation
            lhs = rhs;
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let result = typecheck(resolved);
    assert!(
        result.is_err(),
        "same root type with different array dimensions should mismatch"
    );

    let diags = result.expect_err("expected diagnostics");
    assert!(
        diags.iter().any(|d| d.code.as_deref() == Some("ET002")
            && d.message.contains("array dimension mismatch")),
        "expected ET002 shape mismatch diagnostic, got: {:?}",
        diags
    );
}

#[test]
fn test_user_defined_equation_mismatch_detection() {
    let source = r#"
        type ModeA = enumeration(Off, On);
        type ModeB = enumeration(Off, On);
        record PayloadA
            Real x;
        end PayloadA;
        record PayloadB
            Real x;
        end PayloadB;

        model Test
            ModeA m1;
            ModeB m2;
            PayloadA p1;
            PayloadB p2;
        equation
            m1 = m2;
            p1 = p2;
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let result = typecheck(resolved);
    assert!(
        result.is_err(),
        "different enum/record types should mismatch"
    );

    let diags = result.expect_err("expected diagnostics");
    let et002_count = diags
        .iter()
        .filter(|d| d.code.as_deref() == Some("ET002"))
        .count();
    assert!(
        et002_count >= 2,
        "expected ET002 diagnostics for enum and record equation mismatch"
    );
}
