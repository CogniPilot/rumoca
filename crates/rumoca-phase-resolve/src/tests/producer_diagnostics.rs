//! Lookup failures are emitted by the resolver operation that owns the lookup.
//!
//! These tests exercise the public ParsedTree -> ResolvedTree boundary. They
//! deliberately do not inspect a raw failed ClassTree through a second visitor.

use super::*;

fn rejects_with(source: &str, code: &str, message: &str) {
    let diagnostics = resolve_parsed_tree_source(source)
        .expect_err("an unresolved semantic reference must not mint ResolvedTree");
    assert!(
        diagnostics.iter().any(|diagnostic| {
            diagnostic.code.as_deref() == Some(code) && diagnostic.message.contains(message)
        }),
        "expected {code} containing {message:?}, got: {diagnostics:?}"
    );
}

#[test]
fn component_reference_producers_cover_nondefault_expression_surfaces() {
    let fixtures = [
        (
            r#"
class EOTest
  extends ExternalObject;
  function constructor
    input Boolean verbdose = true;
    output EOTest env;
    external "C" env = init(verbose);
  end constructor;
end EOTest;
"#,
            "verbose",
        ),
        (
            r#"
class EOTest
  extends ExternalObject;
  function constructor
    input Boolean verbose = true;
    output EOTest env;
    external "C" wrong = init(verbose);
  end constructor;
end EOTest;
"#,
            "wrong",
        ),
        (
            r#"
model M
equation
  assert(true, "ok", lvl);
end M;
"#,
            "lvl",
        ),
        (
            r#"
model M
  Real x;
algorithm
  (x, y) := sin(1.0);
end M;
"#,
            "y",
        ),
        (
            r#"
model Base
  parameter Real k = 0;
end Base;
model Derived
  extends Base(k = missing);
end Derived;
"#,
            "missing",
        ),
        (
            r#"
model M
  Real a[2];
algorithm
  a[i] := 1;
end M;
"#,
            "i",
        ),
    ];

    for (source, missing) in fixtures {
        rejects_with(
            source,
            "ER002",
            &format!("unresolved component reference: '{missing}'"),
        );
    }
}

#[test]
fn function_call_producers_preserve_call_classification() {
    for (source, missing) in [
        (
            r#"
model M
equation
  unknown(1.0);
end M;
"#,
            "unknown",
        ),
        (
            r#"
model M
algorithm
  unknown(1.0);
end M;
"#,
            "unknown",
        ),
        (
            r#"
model M
  Real y = MissingPkg.f(1.0);
end M;
"#,
            "MissingPkg.f",
        ),
        (
            r#"
model Decoy
  Real MissingPkg;
end Decoy;
model M
  Real y = MissingPkg.f(1.0);
end M;
"#,
            "MissingPkg.f",
        ),
    ] {
        rejects_with(
            source,
            "ER002",
            &format!("unresolved function call: '{missing}'"),
        );
    }
}

#[test]
fn presentation_annotation_names_do_not_become_semantic_lookup_failures() {
    let source = r#"
model M
  Real x annotation(Dialog(group = missingAnnotationRef));
equation
  x = 1;
end M;
"#;
    resolve_parsed_tree_source(source)
        .expect("presentation annotation names are not Modelica semantic references");
}

#[test]
fn type_reference_producers_cover_declarations_and_constraints() {
    let fixtures = [
        (
            "model M UnknownType x; end M;",
            "unresolved type reference: 'UnknownType'",
        ),
        (
            r#"
package RealMedium
end RealMedium;
model UsesMedium
  replaceable package Medium = RealMedium constrainedby MissingMedium;
end UsesMedium;
"#,
            "unresolved type reference: 'MissingMedium'",
        ),
        (
            r#"
model M
  replaceable Real x constrainedby Missing;
equation
  x = 0;
end M;
"#,
            "unresolved type reference: 'Missing'",
        ),
    ];

    for (source, message) in fixtures {
        rejects_with(source, "ER002", message);
    }
}

#[test]
fn extends_and_static_tail_fail_once_at_their_lookup_producer() {
    rejects_with(
        "model M extends UnknownBase; end M;",
        "ER003",
        "base class not found",
    );

    let source = r#"
record R
  Real a;
end R;
model M
  R r;
  Real y = r.b;
end M;
"#;
    let diagnostics = resolve_parsed_tree_source(source)
        .expect_err("a statically missing member must not mint ResolvedTree");
    let matching = diagnostics
        .iter()
        .filter(|diagnostic| {
            diagnostic.code.as_deref() == Some("ER002")
                && diagnostic
                    .message
                    .contains("unresolved component reference: 'r.b'")
        })
        .count();
    assert_eq!(matching, 1, "the lookup producer must emit exactly once");
}

#[test]
fn builtin_attribute_modifiers_on_short_class_aliases_stay_silent() {
    // MLS §4.8/§7.2: `Real(final quantity = ..., final unit = ...)` is
    // unconditionally legal. A short class alias desugars to an
    // alias-extends whose modifier targets name builtin attributes of the
    // predefined base type; those attributes have no lexical declaration in
    // the resolve scope, so their lookup belongs to Typecheck after receiver
    // selection. Routing them
    // through the Component use turned every SI unit alias in the MSL into an
    // ER002 storm on `quantity`/`unit`/`displayUnit`.
    let source = r#"
type Angle = Real(final quantity = "Angle", final unit = "rad", displayUnit = "deg");

model M
  Angle x;
end M;
"#;
    let result = resolve_parsed_tree_source(source);
    assert!(
        result.is_ok(),
        "builtin-attribute modifier targets on a short class alias must not \
         fail resolve: {:?}",
        result.err()
    );
}

#[test]
fn short_class_alias_modifier_values_still_resolve_strictly() {
    // The tolerance above covers only the modifier *target*. The value is an
    // ordinary expression in the enclosing scope and must still fail loudly
    // when it names nothing.
    rejects_with(
        r#"
type Bad = Real(min = missingLower);

model M
  Bad x;
end M;
"#,
        "ER002",
        "unresolved component reference: 'missingLower'",
    );
}

#[test]
fn hierarchical_class_modification_targets_are_not_function_calls() {
    // `c(limiter(u(start = 0)))` modifies members of the modified instance's
    // type. Neither `limiter` nor `u` is a function, and neither is a lexical
    // declaration in the modifying scope; Typecheck owns their member lookup
    // after receiver selection. Routing the class-modification target through
    // the FunctionCall use produced "unresolved function call:
    // 'limiter'/'u'" on every hierarchical modifier in the MSL (e.g.
    // PID_Controller).
    let source = r#"
model Sub
  Real u;
end Sub;

model Comp
  Sub limiter;
end Comp;

model M
  Comp c(limiter(u(start = 0)));
end M;
"#;
    let result = resolve_parsed_tree_source(source);
    assert!(
        result.is_ok(),
        "hierarchical class-modification targets must not fail resolve as \
         function calls: {:?}",
        result.err()
    );
}

#[test]
fn hierarchical_class_modification_values_still_resolve_strictly() {
    // Silencing the nested targets must not swallow their values: the bound
    // expression is resolved in the modifying scope and still fails loudly.
    rejects_with(
        r#"
model Sub
  Real u;
end Sub;

model Comp
  Sub limiter;
end Comp;

model M
  Comp c(limiter(u(start = missingStart)));
end M;
"#,
        "ER002",
        "unresolved component reference: 'missingStart'",
    );
}

#[test]
fn inherited_ambiguity_on_an_extends_modifier_target_is_not_deferred() {
    rejects_with(
        r#"
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
  extends Both(shared = 1);
end Test;
"#,
        "ER002",
        "ambiguous inherited reference: 'shared'",
    );
}

#[test]
fn compatible_inherited_extends_modifier_target_remains_resolvable() {
    let source = r#"
model Left
  Real shared;
end Left;
model Right
  Real shared;
end Right;
model Both
  extends Left;
  extends Right;
end Both;
model Test
  extends Both(shared = 1.0);
end Test;
"#;
    let result = resolve_parsed_tree_source(source);
    assert!(
        result.is_ok(),
        "semantically compatible inherited declarations must remain one resolvable target: {:?}",
        result.err()
    );
}
