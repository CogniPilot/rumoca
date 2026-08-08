//! MLS §12.9.7 ExternalObject lifecycle grammar and signature checks.

use super::*;

fn lifecycle_diagnostic<'a>(
    diagnostics: &'a Diagnostics,
    code: &str,
    message: &str,
) -> &'a rumoca_core::Diagnostic {
    diagnostics
        .iter()
        .find(|diagnostic| {
            diagnostic.code.as_deref() == Some(code) && diagnostic.message.contains(message)
        })
        .unwrap_or_else(|| panic!("expected {code} containing `{message}`, got: {diagnostics:?}"))
}

fn lifecycle_diagnostic_slice<'a>(
    diagnostics: &'a [rumoca_core::Diagnostic],
    code: &str,
    message: &str,
) -> &'a rumoca_core::Diagnostic {
    diagnostics
        .iter()
        .find(|diagnostic| {
            diagnostic.code.as_deref() == Some(code) && diagnostic.message.contains(message)
        })
        .unwrap_or_else(|| panic!("expected {code} containing `{message}`, got: {diagnostics:?}"))
}

fn assert_primary_source(source: &str, diagnostic: &rumoca_core::Diagnostic, expected: &str) {
    let primary = diagnostic
        .labels
        .iter()
        .find(|label| label.primary)
        .expect("ExternalObject diagnostic must retain exact declaration provenance");
    assert_eq!(
        primary.span.source,
        rumoca_core::SourceId::from_source_name("test.mo")
    );
    assert_eq!(&source[primary.span.start.0..primary.span.end.0], expected);
}

#[test]
fn func_036_external_object_requires_constructor() {
    let source = r#"
class MissingConstructor
  extends ExternalObject;
  function destructor
    input MissingConstructor object;
    external "C" release(object);
  end destructor;
end MissingConstructor;
"#;

    let diagnostics =
        resolve_test_source(source).expect_err("missing constructor must fail in Resolve");
    let diagnostic = lifecycle_diagnostic(&diagnostics, "ER132", "missing constructor");
    assert_primary_source(source, diagnostic, "MissingConstructor");
}

#[test]
fn func_036_external_object_requires_destructor() {
    let source = r#"
class MissingDestructor
  extends ExternalObject;
  function constructor
    output MissingDestructor object;
    external "C" object = create();
  end constructor;
end MissingDestructor;
"#;

    let diagnostics =
        resolve_test_source(source).expect_err("missing destructor must fail in Resolve");
    let diagnostic = lifecycle_diagnostic(&diagnostics, "ER132", "missing destructor");
    assert_primary_source(source, diagnostic, "MissingDestructor");
}

#[test]
fn func_036_external_object_lifecycle_children_must_be_functions() {
    let source = r#"
class WrongConstructorRestriction
  extends ExternalObject;
  model constructor
  end constructor;
  function destructor
    input WrongConstructorRestriction object;
    external "C" release(object);
  end destructor;
end WrongConstructorRestriction;
"#;

    let diagnostics = resolve_test_source(source)
        .expect_err("non-function constructor declaration must fail in Resolve");
    let diagnostic = lifecycle_diagnostic(&diagnostics, "ER132", "constructor must be a function");
    assert_primary_source(source, diagnostic, "constructor");
}

#[test]
fn func_036_external_object_owner_uses_class_restriction() {
    let source = r#"
model WrongRestriction
  extends ExternalObject;
  function constructor
    output WrongRestriction object;
    external "C" object = create();
  end constructor;
  function destructor
    input WrongRestriction object;
    external "C" release(object);
  end destructor;
end WrongRestriction;
"#;

    let diagnostics =
        resolve_test_source(source).expect_err("ExternalObject owner must be a class");
    let diagnostic = lifecycle_diagnostic(
        &diagnostics,
        "ER132",
        "must use the specialized class restriction",
    );
    assert_primary_source(source, diagnostic, "model");
}

#[test]
fn func_036_external_object_lifecycle_functions_are_not_replaceable() {
    let source = r#"
class ReplaceableConstructor
  extends ExternalObject;
  replaceable function constructor
    output ReplaceableConstructor object;
    external "C" object = create();
  end constructor;
  function destructor
    input ReplaceableConstructor object;
    external "C" release(object);
  end destructor;
end ReplaceableConstructor;
"#;

    let diagnostics = resolve_test_source(source)
        .expect_err("replaceable lifecycle declaration must fail in Resolve");
    let diagnostic = lifecycle_diagnostic(&diagnostics, "ER132", "must not be replaceable");
    assert_primary_source(source, diagnostic, "constructor");
}

#[test]
fn func_036_external_object_lifecycle_functions_are_not_partial() {
    let source = r#"
class PartialConstructor
  extends ExternalObject;
  partial function constructor
    output PartialConstructor object;
  end constructor;
  function destructor
    input PartialConstructor object;
    external "C" release(object);
  end destructor;
end PartialConstructor;
"#;

    let diagnostics =
        resolve_test_source(source).expect_err("partial lifecycle declaration must fail");
    let diagnostic = lifecycle_diagnostic(&diagnostics, "ER132", "must not be partial");
    assert_primary_source(source, diagnostic, "constructor");
}

#[test]
fn func_036_external_object_rejects_other_elements() {
    let source = r#"
class ExtraElement
  extends ExternalObject;
  Real state;
  function constructor
    output ExtraElement object;
    external "C" object = create();
  end constructor;
  function destructor
    input ExtraElement object;
    external "C" release(object);
  end destructor;
end ExtraElement;
"#;

    let diagnostics =
        resolve_test_source(source).expect_err("other owner elements must fail in Resolve");
    let diagnostic =
        lifecycle_diagnostic(&diagnostics, "ER132", "cannot contain component 'state'");
    assert_primary_source(source, diagnostic, "state");
}

#[test]
fn func_036_external_object_rejects_other_nested_classes() {
    let source = r#"
class ExtraNestedClass
  extends ExternalObject;
  function constructor
    output ExtraNestedClass object;
    external "C" object = create();
  end constructor;
  function destructor
    input ExtraNestedClass object;
    external "C" release(object);
  end destructor;
  function helper
    input Real value;
    output Real result;
  algorithm
    result := value;
  end helper;
end ExtraNestedClass;
"#;

    let diagnostics =
        resolve_test_source(source).expect_err("other nested classes must fail in Resolve");
    let diagnostic = lifecycle_diagnostic(&diagnostics, "ER132", "cannot contain class 'helper'");
    assert_primary_source(source, diagnostic, "helper");
}

#[test]
fn malformed_extra_child_without_identity_is_a_typed_phase_error() {
    let source = r#"
class MissingChildIdentity
  extends ExternalObject;
  function constructor
    output MissingChildIdentity object;
    external "C" object = create();
  end constructor;
  function destructor
    input MissingChildIdentity object;
    external "C" release(object);
  end destructor;
  function helper
    input Real value;
    output Real result;
  algorithm
    result := value;
  end helper;
end MissingChildIdentity;
"#;

    let failure = match resolve_with_diagnostics(parsed_tree_from_source(source)) {
        Err(failure) => failure,
        Ok(_) => panic!("the source has a forbidden extra lifecycle child"),
    };
    let (mut tree, _) = failure.into_parts();
    tree.definitions.classes["MissingChildIdentity"].classes["helper"].def_id = None;

    let diagnostics = semantic_checks::check_resolved_semantics(&tree);
    let diagnostic =
        lifecycle_diagnostic_slice(&diagnostics, "ER132", "declaration identity is missing");
    assert_primary_source(source, diagnostic, "helper");
}

#[test]
fn func_036_external_object_must_extend_builtin_directly() {
    let source = r#"
class BaseHandle
  extends ExternalObject;
  function constructor
    output BaseHandle object;
    external "C" object = create();
  end constructor;
  function destructor
    input BaseHandle object;
    external "C" release(object);
  end destructor;
end BaseHandle;

class IndirectHandle
  extends BaseHandle;
end IndirectHandle;
"#;

    let diagnostics =
        resolve_test_source(source).expect_err("indirect ExternalObject extension must fail");
    let diagnostic = lifecycle_diagnostic(&diagnostics, "ER132", "must directly extend");
    assert_primary_source(source, diagnostic, "extends BaseHandle");
}

#[test]
fn func_036_external_object_rejects_transitive_indirect_extension() {
    let source = r#"
class BaseHandle
  extends ExternalObject;
  function constructor
    output BaseHandle object;
    external "C" object = create();
  end constructor;
  function destructor
    input BaseHandle object;
    external "C" release(object);
  end destructor;
end BaseHandle;

class DerivedHandle
  extends BaseHandle;
end DerivedHandle;

class DerivedAgain
  extends DerivedHandle;
end DerivedAgain;
"#;

    let diagnostics =
        resolve_test_source(source).expect_err("transitive ExternalObject extension must fail");
    let diagnostic = diagnostics
        .iter()
        .find(|diagnostic| {
            diagnostic.code.as_deref() == Some("ER132")
                && diagnostic.labels.iter().any(|label| {
                    label.primary
                        && &source[label.span.start.0..label.span.end.0] == "extends DerivedHandle"
                })
        })
        .expect("transitive derived owner must retain its exact extends provenance");
    assert!(diagnostic.message.contains("must directly extend"));
}

#[test]
fn func_036_external_object_rejects_short_class_definition() {
    let source = r#"
class ShortHandle = ExternalObject;
"#;

    let diagnostics =
        resolve_test_source(source).expect_err("ExternalObject short-class definition must fail");
    let diagnostic = lifecycle_diagnostic(
        &diagnostics,
        "ER132",
        "must not use a short-class definition",
    );
    assert_primary_source(source, diagnostic, "ShortHandle");
}

#[test]
fn func_037_external_object_constructor_has_one_owner_typed_output() {
    let source = r#"
class WrongConstructorOutput
  extends ExternalObject;
  function constructor
    output Real object;
    external "C" object = create();
  end constructor;
  function destructor
    input WrongConstructorOutput object;
    external "C" release(object);
  end destructor;
end WrongConstructorOutput;
"#;

    let diagnostics = resolve_test_source(source)
        .expect_err("constructor output type mismatch must fail in Resolve");
    let diagnostic = lifecycle_diagnostic(&diagnostics, "ER133", "must have type");
    assert_primary_source(source, diagnostic, "Real");
}

#[test]
fn func_037_external_object_destructor_has_one_owner_input_and_no_output() {
    let source = r#"
class WrongDestructorSignature
  extends ExternalObject;
  function constructor
    output WrongDestructorSignature object;
    external "C" object = create();
  end constructor;
  function destructor
    input Real object;
    output Integer status;
    external "C" status = release(object);
  end destructor;
end WrongDestructorSignature;
"#;

    let diagnostics = resolve_test_source(source)
        .expect_err("destructor signature mismatch must fail in Resolve");
    let input_diagnostic = lifecycle_diagnostic(&diagnostics, "ER133", "must have type");
    assert_primary_source(source, input_diagnostic, "Real");
    let output_diagnostic = lifecycle_diagnostic(&diagnostics, "ER133", "must not declare outputs");
    assert_primary_source(source, output_diagnostic, "status");
}

#[test]
fn func_037_external_object_lifecycle_values_are_scalar_in_source_and_semantics() {
    let source = r#"
class ArrayConstructorOutput
  extends ExternalObject;
  function constructor
    output ArrayConstructorOutput object[1];
    external "C" object = create();
  end constructor;
  function destructor
    input ArrayConstructorOutput object;
    external "C" release(object);
  end destructor;
end ArrayConstructorOutput;
"#;

    let diagnostics =
        resolve_test_source(source).expect_err("array lifecycle value must fail in Resolve");
    let diagnostic = lifecycle_diagnostic(&diagnostics, "ER133", "must be scalar");
    assert_primary_source(source, diagnostic, "1");
}

#[test]
fn func_037_external_object_rejects_non_scalar_semantic_shape() {
    let source = r#"
class SemanticArrayOutput
  extends ExternalObject;
  function constructor
    output SemanticArrayOutput object;
    external "C" object = create();
  end constructor;
  function destructor
    input SemanticArrayOutput object;
    external "C" release(object);
  end destructor;
end SemanticArrayOutput;
"#;

    let mut tree = resolve_tree_source(source).into_inner();
    tree.definitions.classes["SemanticArrayOutput"].classes["constructor"].components["object"]
        .shape = vec![1];
    let diagnostics = semantic_checks::check_resolved_semantics(&tree);
    let diagnostic = lifecycle_diagnostic_slice(&diagnostics, "ER133", "non-scalar semantic shape");
    assert_primary_source(source, diagnostic, "object");
}

#[test]
fn unresolved_lifecycle_type_is_not_misdiagnosed_as_signature_mismatch() {
    let source = r#"
class UnresolvedOutput
  extends ExternalObject;
  function constructor
    output MissingType object;
    external "C" object = create();
  end constructor;
  function destructor
    input UnresolvedOutput object;
    external "C" release(object);
  end destructor;
end UnresolvedOutput;
"#;

    let diagnostics =
        resolve_test_source(source).expect_err("unresolved output type must fail in Resolve");
    assert!(
        diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code.as_deref() == Some("ER002")),
        "unresolved type must remain owned by ER002: {diagnostics:?}"
    );
    assert!(
        diagnostics
            .iter()
            .all(|diagnostic| diagnostic.code.as_deref() != Some("ER133")),
        "partial type identity must not create a false lifecycle mismatch: {diagnostics:?}"
    );
}

#[test]
fn func_038_external_object_lifecycle_functions_cannot_be_called_explicitly() {
    let source = r#"
class Handle
  extends ExternalObject;
  function constructor
    output Handle object;
    external "C" object = create();
  end constructor;
  function destructor
    input Handle object;
    external "C" release(object);
  end destructor;
end Handle;

model ExplicitLifecycleCall
  Handle object;
algorithm
  Handle.destructor(object);
end ExplicitLifecycleCall;
"#;

    let diagnostics =
        resolve_test_source(source).expect_err("explicit lifecycle call must fail in Resolve");
    let diagnostic = lifecycle_diagnostic(&diagnostics, "ER134", "cannot be called explicitly");
    assert_primary_source(source, diagnostic, "Handle.destructor");
}

#[test]
fn external_object_with_checked_lifecycle_resolves() {
    let source = r#"
class Handle
  extends ExternalObject;
  function constructor
    input Integer seed;
    output Handle object;
    external "C" object = create(seed);
  end constructor;
  function destructor
    input Handle object;
    external "C" release(object);
  end destructor;
end Handle;
"#;

    resolve_test_source(source).expect("well-formed ExternalObject lifecycle must resolve");
}
