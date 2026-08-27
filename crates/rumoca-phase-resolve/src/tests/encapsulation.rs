//! `encapsulated` class visibility: enclosing names are hidden while
//! predefined types and explicit imports stay reachable.

use super::*;

#[test]
fn test_nested_non_encapsulated_class_sees_enclosing_name() {
    let source = r#"
package P
constant Real c = 1;
model M
    Real x = c;
end M;
end P;
"#;

    resolve_test_source(source).expect("ordinary nested lookup should resolve enclosing c");
}

#[test]
fn test_encapsulated_class_cannot_see_enclosing_name() {
    let source = r#"
package P
constant Real c = 1;
encapsulated model M
    Real x = c;
end M;
end P;
"#;

    let diagnostics = resolve_test_source(source).expect_err("encapsulated M must not resolve P.c");
    assert!(
        diagnostics.iter().any(|diag| {
            diag.message.contains("unresolved component reference: 'c'")
                && diag.code.as_deref() == Some("ER002")
        }),
        "expected unresolved component diagnostic for c, got: {diagnostics:?}"
    );
    assert!(
        !diagnostics
            .iter()
            .any(|diag| { diag.message.contains("unresolved type reference: 'Real'") }),
        "predefined Real type should remain visible from encapsulated scope"
    );
}

#[test]
fn test_encapsulated_class_resolves_predefined_type() {
    let source = r#"
package P
encapsulated model M
    Real x = 1;
end M;
end P;
"#;

    resolve_test_source(source).expect("encapsulated scope should resolve predefined Real");
}

#[test]
fn test_encapsulated_class_cannot_see_unimported_top_level_class() {
    let source = r#"
package UserLibrary
  type Count = Integer;
end UserLibrary;
package P
  encapsulated model M
    UserLibrary.Count n;
  end M;
end P;
"#;

    let diagnostics = resolve_test_source(source)
        .expect_err("encapsulated M must not see an unimported top-level UserLibrary");
    assert!(
        diagnostics.iter().any(|diag| {
            diag.code.as_deref() == Some("ER002")
                && diag
                    .message
                    .contains("unresolved type reference: 'UserLibrary.Count'")
        }),
        "expected unresolved type diagnostic, got: {diagnostics:?}"
    );
}

#[test]
fn test_encapsulated_class_can_import_top_level_class() {
    let source = r#"
package UserLibrary
  type Count = Integer;
end UserLibrary;
package P
  encapsulated model M
    import Count = UserLibrary.Count;
    Count n;
  end M;
end P;
"#;

    resolve_test_source(source)
        .expect("an explicit import must expose a top-level class inside an encapsulated scope");
}
