//! Builtin and predefined type resolution, plus the numeric-coercion
//! warnings raised while folding constant expressions.

use super::*;

fn assert_predefined_enum_binding_identity(
    resolved: &rumoca_phase_resolve::ResolvedTree,
    component_name: &str,
    enum_name: &str,
) {
    let component = &resolved.definitions.classes["Test"].components[component_name];
    let Some(rumoca_ir_ast::Expression::ComponentReference(reference)) = component.binding.as_ref()
    else {
        panic!("predefined enum fixture must have a component-reference binding");
    };
    let predefined = resolved
        .scope_tree
        .predefined_member(&rumoca_core::ComponentPath::from_flat_path(enum_name))
        .expect("predefined enum has an exact identity");
    assert_eq!(reference.root_def_id(), Some(predefined));
    let literal = reference
        .target_def_id()
        .expect("predefined enum literal has an exact identity");
    assert_ne!(literal, predefined);
    assert!(
        resolved.def_map[&literal].starts_with(&format!("{enum_name}.")),
        "literal identity must name a declaration owned by its enum"
    );
    let declared_literals = rumoca_core::PREDEFINED_ENUM_LITERALS
        .iter()
        .find_map(|(name, literals)| (*name == enum_name).then_some(*literals))
        .expect("fixture names a predefined enum");
    let literal_ids = declared_literals
        .iter()
        .map(|literal_name| {
            resolved
                .scope_tree
                .predefined_member(&rumoca_core::ComponentPath::from_parts([
                    enum_name,
                    *literal_name,
                ]))
                .expect("every predefined literal has an identity")
        })
        .collect::<std::collections::HashSet<_>>();
    assert_eq!(
        literal_ids.len(),
        declared_literals.len(),
        "sibling literals must have distinct declaration identities"
    );
}

#[test]
fn test_empty_typecheck() {
    let tree = ClassTree::new();
    let parsed = ParsedTree::new(tree);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let result = typecheck(resolved);
    assert!(result.is_ok());
}

#[test]
fn legitimate_integer_conversion_emits_no_warning() {
    let diagnostics = typecheck_diagnostics(
        r#"
        model Test
          parameter Integer n = integer(4.0);
          Real x[n];
        end Test;
        "#,
    );

    assert!(
        diagnostics
            .iter()
            .all(|diag| diag.code.as_deref() != Some("ET006")
                && diag.code.as_deref() != Some("ET007")),
        "expected no integer-coercion warnings, got: {diagnostics:?}"
    );
}

#[test]
fn out_of_range_integer_coercion_emits_warning() {
    let diagnostics = typecheck_diagnostics(
        r#"
        model Test
          parameter Integer n = integer(-1e40);
        end Test;
        "#,
    );

    assert!(
        diagnostics
            .iter()
            .any(|diag| diag.code.as_deref() == Some("ET006")),
        "expected ET006 warning, got: {diagnostics:?}"
    );
}

#[test]
fn integer_fold_overflow_emits_warning() {
    let diagnostics = typecheck_diagnostics(
        r#"
        model Test
          parameter Integer n = integer(9e18) + integer(9e18);
        end Test;
        "#,
    );

    assert!(
        diagnostics
            .iter()
            .any(|diag| diag.code.as_deref() == Some("ET007")),
        "expected ET007 warning, got: {diagnostics:?}"
    );
}

#[test]
fn model_qualified_nested_package_constants_evaluate_dimensions() {
    let diagnostics = typecheck_diagnostics(
        r#"
        package Root
          package Media
            package Examples
              model UsesMedium
                replaceable package Medium = Air;
                Real x[Medium.nX];
              end UsesMedium;

              package Air
                constant Integer nX = 2;
              end Air;
            end Examples;
          end Media;
        end Root;
        "#,
    );

    assert!(
        diagnostics
            .iter()
            .all(|diag| diag.code.as_deref() != Some("ET001")),
        "model-qualified nested package dimension should be evaluable, got: {diagnostics:?}"
    );
}

#[test]
fn test_builtin_type_resolution() {
    // Parse a simple model with Real, Integer, Boolean, String types
    let source = r#"
        model Test
            Real x;
            Integer i;
            Boolean b;
            String s;
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let typed = typecheck(resolved).expect("typecheck should succeed");

    // Check that types were resolved
    let tree = typed.into_inner();
    let test_class = tree
        .definitions
        .classes
        .get("Test")
        .expect("Test class should exist");

    let x = test_class.components.get("x").expect("x should exist");
    assert!(x.type_id.is_some());
    assert_ne!(x.type_id.unwrap(), TypeId::UNKNOWN);

    let i = test_class.components.get("i").expect("i should exist");
    assert!(i.type_id.is_some());
    assert_ne!(i.type_id.unwrap(), TypeId::UNKNOWN);

    let b = test_class.components.get("b").expect("b should exist");
    assert!(b.type_id.is_some());
    assert_ne!(b.type_id.unwrap(), TypeId::UNKNOWN);

    let s = test_class.components.get("s").expect("s should exist");
    assert!(s.type_id.is_some());
    assert_ne!(s.type_id.unwrap(), TypeId::UNKNOWN);
}

#[test]
fn test_builtin_clock_type_resolution() {
    let source = r#"
        model Test
            Clock c;
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let typed = typecheck(resolved).expect("typecheck should succeed");

    let tree = typed.into_inner();
    let test_class = tree
        .definitions
        .classes
        .get("Test")
        .expect("Test class should exist");
    let c = test_class.components.get("c").expect("c should exist");
    assert!(c.type_id.is_some());
    assert_ne!(c.type_id.unwrap(), TypeId::UNKNOWN);
}

#[test]
fn test_integer_builtin_accepts_integer_argument() {
    let source = r#"
        model Test
            parameter Integer n = 3;
            parameter Integer m = integer(n);
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    typecheck(resolved).expect("integer(Integer) should typecheck");
}

#[test]
fn test_predefined_stateselect_type_resolution() {
    let source = r#"
        model Test
            StateSelect sel = StateSelect.default;
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    assert_predefined_enum_binding_identity(&resolved, "sel", "StateSelect");
    let typed = typecheck(resolved).expect("typecheck should succeed");

    let tree = typed.into_inner();
    let test_class = tree
        .definitions
        .classes
        .get("Test")
        .expect("Test class should exist");
    let sel = test_class.components.get("sel").expect("sel should exist");
    assert!(sel.type_id.is_some());
    assert_ne!(sel.type_id.unwrap(), TypeId::UNKNOWN);
}

#[test]
fn test_predefined_assertion_level_type_resolution() {
    let source = r#"
        model Test
            AssertionLevel level = AssertionLevel.warning;
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    assert_predefined_enum_binding_identity(&resolved, "level", "AssertionLevel");
    let typed = typecheck(resolved).expect("typecheck should succeed");

    let tree = typed.into_inner();
    let test_class = tree
        .definitions
        .classes
        .get("Test")
        .expect("Test class should exist");
    let level = test_class
        .components
        .get("level")
        .expect("level should exist");
    assert!(level.type_id.is_some());
    assert_ne!(level.type_id.unwrap(), TypeId::UNKNOWN);
}
