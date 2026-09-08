//! Component-modifier diagnostics on the class tree: unknown builtin,
//! nested, and class-member modifier names, and modifier value types.

use super::*;

fn typecheck_instantiated_source(
    source: &str,
    model_name: &str,
) -> Result<(), rumoca_core::Diagnostics> {
    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let overlay = match rumoca_phase_instantiate::instantiate_model_with_outcome(
        resolved.inner(),
        model_name,
    ) {
        rumoca_phase_instantiate::InstantiationOutcome::Success(overlay) => overlay,
        rumoca_phase_instantiate::InstantiationOutcome::NeedsInner { missing_inners, .. } => {
            panic!("modifier fixture unexpectedly needs inner components: {missing_inners:?}")
        }
        rumoca_phase_instantiate::InstantiationOutcome::Error(error) => {
            panic!("modifier fixture must reach its typecheck owner: {error}")
        }
    };
    typecheck_instanced_tree(&resolved, overlay, model_name).map(|_typed| ())
}

fn assert_unknown_modifier(source: &str, model_name: &str, target: &str) {
    let diagnostics = typecheck_instantiated_source(source, model_name)
        .expect_err("an unknown modifier target must fail in phase-typecheck");
    assert!(
        diagnostics.iter().any(|diagnostic| {
            diagnostic.code.as_deref() == Some("ET001")
                && diagnostic
                    .message
                    .contains(&format!("unknown modifier `{target}`"))
        }),
        "expected ET001 for modifier target `{target}`, got: {diagnostics:?}"
    );
}

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

#[test]
fn misspelled_nested_start_in_extends_is_not_silently_discarded() {
    assert_unknown_modifier(
        r#"
        model Base
            Real x(start = 0.0);
        end Base;
        model Test
            extends Base(x(strat = 0.5));
        end Test;
        "#,
        "Test",
        "strat",
    );
}

#[test]
fn nested_component_modification_requires_each_member_to_exist() {
    assert_unknown_modifier(
        r#"
        model Limiter
            Real u;
        end Limiter;
        model Controller
            Limiter limiter;
        end Controller;
        model Test
            Controller controller(limiter(typo = 1.0));
        end Test;
        "#,
        "Test",
        "typo",
    );
}

#[test]
fn direct_extends_modification_requires_a_member() {
    assert_unknown_modifier(
        r#"
        model Base
            Real x;
        end Base;
        model Test
            extends Base(bogus = 1.0);
        end Test;
        "#,
        "Test",
        "bogus",
    );
}

#[test]
fn alias_extends_modification_requires_a_predefined_attribute() {
    assert_unknown_modifier(
        r#"
        type BadReal = Real(bogusAttr = 1.0);
        model Test
            BadReal x;
        end Test;
        "#,
        "Test",
        "bogusAttr",
    );
}

#[test]
fn short_class_modification_requires_a_member() {
    assert_unknown_modifier(
        r#"
        model Component
            Real x;
        end Component;
        model Test = Component(nothere(start = 1.0));
        "#,
        "Test",
        "nothere",
    );
}

#[test]
fn valid_nested_and_real_attribute_modifiers_reach_typecheck() {
    let source = r#"
        model Limiter
            Real u;
        end Limiter;
        model Controller
            Limiter limiter;
        end Controller;
        model Test
            Controller controller(limiter(u(
                quantity = "signal",
                unit = "m",
                displayUnit = "cm",
                min = 0.0,
                max = 2.0,
                start = 1.0,
                fixed = true,
                nominal = 1.0,
                unbounded = false,
                stateSelect = StateSelect.default)));
        end Test;
    "#;
    typecheck_instantiated_source(source, "Test")
        .expect("valid nested modifiers and every Real attribute must be accepted");
}

#[test]
fn modifier_path_descends_through_a_replaceable_member() {
    let source = r#"
        model Plant
            Real state;
        end Plant;
        model Holder
            replaceable model SelectedPlant = Plant;
            SelectedPlant plant;
        end Holder;
        model Test
            Holder holder(plant(state(start = 2.0)));
        end Test;
    "#;
    typecheck_instantiated_source(source, "Test")
        .expect("a modifier may descend through the replaceable member's declared interface");
}

#[test]
fn enumeration_component_accepts_predefined_start_attribute() {
    let source = r#"
        type Mode = enumeration(off, on);
        model Holder
            Mode mode;
        end Holder;
        model Test
            Holder holder(mode.start = Mode.on);
        end Test;
    "#;
    typecheck_instantiated_source(source, "Test")
        .expect("enumeration-valued components expose predefined attributes");
}

#[test]
fn ambiguous_inherited_modifier_target_is_a_typed_error() {
    let source = r#"
        model Left
            Real shared;
        end Left;
        model Right
            Integer shared;
        end Right;
        model Both
            extends Left;
            extends Right;
        end Both;
        model Test
            Both value(shared = 1.0);
        end Test;
    "#;
    let resolved = resolve(parse(source)).expect("resolve must preserve modifier ambiguity");
    let diagnostics = typecheck(resolved)
        .expect_err("the exact modifier-member owner must reject inherited ambiguity");
    assert!(
        diagnostics.iter().any(|diagnostic| {
            diagnostic.code.as_deref() == Some("ET001")
                && diagnostic.message.contains("ambiguous across inherited")
        }),
        "expected an ambiguity-preserving ET001, got: {diagnostics:?}"
    );
}

#[test]
fn unrepresented_bare_modifier_target_is_a_typed_error() {
    let source = r#"
        model Test
            Real x(start);
        end Test;
    "#;
    let resolved = resolve(parse(source)).expect("resolve should preserve the source carrier");
    let diagnostics =
        typecheck(resolved).expect_err("a modifier target without semantic identity must fail");
    assert!(
        diagnostics.iter().any(|diagnostic| {
            diagnostic.code.as_deref() == Some("ET000")
                && diagnostic.message.contains("not represented")
        }),
        "expected ET000 for an unrepresented bare modifier, got: {diagnostics:?}"
    );
}

#[test]
fn modifier_target_without_tokens_or_span_is_not_silently_skipped() {
    let source = r#"
        model Test
            Real x(strat = 1.0);
        end Test;
    "#;
    let mut tree = resolve(parse(source))
        .expect("resolve should preserve the target carrier")
        .inner()
        .clone();
    let modification = tree
        .definitions
        .classes
        .get_mut("Test")
        .and_then(|class| class.components.get_mut("x"))
        .and_then(|component| component.source_modifications.first_mut())
        .expect("fixture must contain one source modifier");
    let Expression::Modification { target, .. } = modification else {
        panic!("fixture modifier must retain its semantic target carrier")
    };
    target.parts.clear();
    target.span = rumoca_core::Span::DUMMY;

    let diagnostics = TypeChecker::new().check(&mut tree);
    assert!(
        diagnostics.iter().any(|diagnostic| {
            diagnostic.code.as_deref() == Some("ET000")
                && diagnostic
                    .message
                    .contains("no source span or token location")
        }),
        "expected ET000 instead of an empty-target early return, got: {diagnostics:?}"
    );
}

#[test]
fn unavailable_modifier_source_map_emits_one_provenance_error() {
    let source = r#"
        model Test
            Real x(strat = 1.0);
        end Test;
    "#;
    let mut tree = resolve(parse(source))
        .expect("resolve should preserve the target carrier")
        .inner()
        .clone();
    tree.source_map = rumoca_core::SourceMap::new();

    let diagnostics = TypeChecker::new().check(&mut tree);
    let provenance_errors = diagnostics
        .iter()
        .filter(|diagnostic| {
            diagnostic.code.as_deref() == Some("ET000")
                && diagnostic.message.contains("component modifier target")
        })
        .count();
    assert_eq!(
        provenance_errors, 1,
        "missing modifier provenance must be diagnosed exactly once: {diagnostics:?}"
    );
    assert!(
        diagnostics
            .iter()
            .all(|diagnostic| diagnostic.code.as_deref() != Some("ET001")),
        "a target error without source provenance must not emit an unanchored ET001"
    );
}

#[test]
fn modifier_values_remain_strict_resolve_references() {
    let source = r#"
        model Test
            Real x(start = missingValue);
        end Test;
    "#;
    let diagnostics =
        resolve(parse(source)).expect_err("modifier values remain ordinary strict references");
    assert!(
        diagnostics.iter().any(|diagnostic| {
            diagnostic.code.as_deref() == Some("ER002")
                && diagnostic.message.contains("missingValue")
        }),
        "expected ER002 for the modifier value, got: {diagnostics:?}"
    );
}
