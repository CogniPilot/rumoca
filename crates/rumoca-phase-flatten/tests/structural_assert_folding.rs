//! Structural folding of initial-algorithm assertions (MLS §8.3.7, §18.3).
//!
//! An assertion whose condition is decided from translation-frozen values
//! alone — constants, `annotation(Evaluate = true)` parameters, `final`
//! parameters — folds away when proven true, becomes the EF030 translation
//! diagnostic when proven false at error level, and stays untouched in every
//! other case: warning level, undecidable conditions, and — the load-bearing
//! adversary — an ordinary tunable parameter whose concrete default the
//! broader structural context happens to know. Membership in the fold's
//! evaluation context is itself the structural proof; nothing here consults
//! a default that a simulation run could still override.

use rumoca_ir_ast as ast;

const SOURCE_NAME: &str = "<structural_assert_folding>";
const SOURCE: &str = r#"
package P
  function isPow2ish
    input Integer i;
    output Boolean r;
  protected
    Integer p;
  algorithm
    p := 1;
    while p < i loop
      p := p * 2;
    end while;
    r := p == i;
  end isPow2ish;

  model FoldsTrue
    parameter Integer m = 4 annotation(Evaluate=true);
    Real x(start = 0, fixed = true);
  initial algorithm
    assert(isPow2ish(m), "m must be a power of two");
  equation
    der(x) = -x;
  end FoldsTrue;

  model FinalFolds
    final parameter Integer m = 4;
    Real x(start = 0, fixed = true);
  initial algorithm
    assert(isPow2ish(m), "m must be a power of two");
  equation
    der(x) = -x;
  end FinalFolds;

  model FailsFalse
    parameter Integer m = 3 annotation(Evaluate=true);
    Real x(start = 0, fixed = true);
  initial algorithm
    assert(isPow2ish(m), "m must be a power of two");
  equation
    der(x) = -x;
  end FailsFalse;

  model WarningKept
    parameter Integer m = 3 annotation(Evaluate=true);
    Real x(start = 0, fixed = true);
  initial algorithm
    assert(isPow2ish(m), "m should be a power of two", AssertionLevel.warning);
  equation
    der(x) = -x;
  end WarningKept;

  model TunableKept
    parameter Integer m(fixed=false) = 3;
    Real x(start = 0, fixed = true);
  initial algorithm
    assert(isPow2ish(m), "runtime owns this check");
  equation
    der(x) = -x;
  end TunableKept;

  model RuntimeMessageKept
    parameter Integer m = 3 annotation(Evaluate=true);
    Real x(start = 0, fixed = true);
  initial algorithm
    assert(isPow2ish(m), "x was " + String(x));
  equation
    der(x) = -x;
  end RuntimeMessageKept;

  model ExplicitErrorFails
    parameter Integer m = 3 annotation(Evaluate=true);
    Real x(start = 0, fixed = true);
  initial algorithm
    assert(isPow2ish(m), "m must be a power of two", AssertionLevel.error);
  equation
    der(x) = -x;
  end ExplicitErrorFails;

  type MyAssertionLevel = enumeration(warning, error);

  model SpoofedLevelKept
    parameter Integer m = 3 annotation(Evaluate=true);
    Real x(start = 0, fixed = true);
  initial algorithm
    assert(isPow2ish(m), "spoofed level", MyAssertionLevel.error);
  equation
    der(x) = -x;
  end SpoofedLevelKept;
end P;
"#;

/// The exact declaration identities the production fixture resolves, so
/// every pin below tests `DefId` equality rather than a rendered name.
struct Identities {
    pow2: rumoca_core::DefId,
    warning_literal: rumoca_core::DefId,
}

fn flatten_model(
    model_name: &str,
) -> (
    Result<rumoca_ir_flat::Model, rumoca_phase_flatten::FlattenError>,
    Identities,
) {
    let stored = rumoca_phase_parse::parse_to_ast(SOURCE, SOURCE_NAME).expect("source parses");
    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map.add(SOURCE_NAME, SOURCE);
    let resolved =
        rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree)).expect("source resolves");
    let pow2 = resolved
        .inner()
        .get_class_by_qualified_name("P.isPow2ish")
        .expect("isPow2ish resolves")
        .def_id
        .expect("isPow2ish has declaration identity");
    let overlay = match rumoca_phase_instantiate::instantiate_model_with_outcome(
        resolved.inner(),
        model_name,
    ) {
        rumoca_phase_instantiate::InstantiationOutcome::Success(overlay) => overlay,
        rumoca_phase_instantiate::InstantiationOutcome::NeedsInner { missing_inners, .. } => {
            panic!("fixture unexpectedly needs inner declarations: {missing_inners:?}")
        }
        rumoca_phase_instantiate::InstantiationOutcome::Error(error) => {
            panic!("fixture instantiation failed: {error}")
        }
    };
    let warning_literal = resolved
        .inner()
        .scope_tree
        .predefined_member(&rumoca_core::ComponentPath::from_parts([
            "AssertionLevel",
            "warning",
        ]))
        .expect("the predefined AssertionLevel.warning identity exists");
    let typed = rumoca_phase_typecheck::typecheck_instanced_tree(&resolved, overlay, model_name)
        .expect("model typechecks");
    let flat =
        rumoca_phase_flatten::flatten_typed(typed, rumoca_phase_flatten::FlattenOptions::default());
    (
        flat,
        Identities {
            pow2,
            warning_literal,
        },
    )
}

fn initial_asserts(flat: &rumoca_ir_flat::Model) -> Vec<&rumoca_core::Statement> {
    flat.initial_algorithms
        .iter()
        .flat_map(|algorithm| algorithm.statements.iter())
        .filter(|statement| matches!(statement, rumoca_core::Statement::Assert { .. }))
        .collect()
}

fn functions_with_identity(flat: &rumoca_ir_flat::Model, def_id: rumoca_core::DefId) -> usize {
    flat.functions
        .values()
        .filter(|function| function.def_id == Some(def_id))
        .count()
}

#[test]
fn proven_true_assert_over_an_evaluate_parameter_folds_away() {
    let (flat, ids) = flatten_model("P.FoldsTrue");
    let flat = flat.expect("a proven-true assertion flattens");
    assert_eq!(initial_asserts(&flat).len(), 0);
    assert_eq!(
        functions_with_identity(&flat, ids.pow2),
        0,
        "the folded assertion was the sole call, so its function is pruned"
    );
}

#[test]
fn proven_true_assert_over_a_final_parameter_folds_away() {
    let (flat, ids) = flatten_model("P.FinalFolds");
    let flat = flat.expect("a proven-true assertion flattens");
    assert_eq!(initial_asserts(&flat).len(), 0);
    assert_eq!(functions_with_identity(&flat, ids.pow2), 0);
}

#[test]
fn proven_false_error_assert_is_a_translation_diagnostic() {
    let (result, _) = flatten_model("P.FailsFalse");
    let error = result.expect_err("a structurally false error-level assertion fails translation");
    let rumoca_phase_flatten::FlattenError::StructuralAssertionFailed { message, span } = &error
    else {
        panic!("the exact typed rejection is EF030, found {error:?}");
    };
    assert_eq!(message, "m must be a power of two");
    let model_start = SOURCE
        .find("model FailsFalse")
        .expect("fixture model exists");
    let assert_start = model_start
        + SOURCE[model_start..]
            .find("assert(")
            .expect("fixture assertion exists");
    assert_eq!(
        span.start.0, assert_start,
        "the diagnostic anchors at the source assert statement"
    );
    // The parser anchors a statement span at its keyword; the diagnostic
    // carries that statement span unchanged.
    assert_eq!(&SOURCE[span.start.0..span.end.0], "assert");
}

#[test]
fn proven_false_warning_assert_stays_for_the_runtime_owner() {
    let (flat, ids) = flatten_model("P.WarningKept");
    let flat = flat.expect("warning-level assertions never fail translation");
    let asserts = initial_asserts(&flat);
    assert_eq!(asserts.len(), 1);
    let rumoca_core::Statement::Assert { message, level, .. } = asserts[0] else {
        unreachable!("initial_asserts returns only Assert statements");
    };
    assert!(
        matches!(
            message.as_ref(),
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::String(text),
                ..
            } if text == "m should be a power of two"
        ),
        "the retained message expression is preserved, found {message:?}"
    );
    let level = level
        .as_ref()
        .expect("the explicit AssertionLevel.warning expression is preserved");
    let rumoca_core::Expression::VarRef { name, .. } = level.as_ref() else {
        panic!("the retained level is the structured enum reference, found {level:?}");
    };
    let parts = name
        .component_ref()
        .expect("the level reference keeps its resolved structure")
        .parts();
    assert_eq!(parts.len(), 2);
    assert_eq!(parts[0].ident, "AssertionLevel");
    assert_eq!(parts[1].ident, "warning");
    assert_eq!(
        parts[1].def_id, ids.warning_literal,
        "the level literal keeps the predefined declaration identity"
    );
    assert_eq!(
        functions_with_identity(&flat, ids.pow2),
        1,
        "a retained assertion keeps its called function"
    );
}

#[test]
fn explicit_error_level_assert_is_a_translation_diagnostic() {
    // The explicit AssertionLevel.error literal classifies by its
    // predefined declaration identity, exactly like the omitted default.
    let (result, _) = flatten_model("P.ExplicitErrorFails");
    let error = result.expect_err("an explicit error-level structural failure fails translation");
    assert!(matches!(
        &error,
        rumoca_phase_flatten::FlattenError::StructuralAssertionFailed { message, .. }
            if message == "m must be a power of two"
    ));
}

#[test]
fn spoofed_assertion_level_enum_keeps_the_assert() {
    // The [76] adversary: a user enumeration whose rendered type name ends
    // in "AssertionLevel" and whose literal spells "error" must never
    // classify as the predefined error level — identity, not spelling,
    // decides — so the statement and its function stay with the runtime.
    let (flat, ids) = flatten_model("P.SpoofedLevelKept");
    let flat = flat.expect("a spoofed level never fails translation");
    assert_eq!(initial_asserts(&flat).len(), 1);
    assert_eq!(functions_with_identity(&flat, ids.pow2), 1);
}

#[test]
fn tunable_parameter_assert_stays_despite_a_known_default() {
    // The [54]/[63] adversary: `m(fixed=false) = 3` gives the broader
    // structural context a concrete default while initialization, not
    // translation, owns the parameter's value. The fold context never
    // carries it, so the statement and its function must both remain.
    let (flat, ids) = flatten_model("P.TunableKept");
    let flat = flat.expect("an undecided assertion flattens");
    assert_eq!(initial_asserts(&flat).len(), 1);
    assert_eq!(functions_with_identity(&flat, ids.pow2), 1);
}

#[test]
fn runtime_dependent_message_keeps_a_false_assert() {
    // The condition is structurally false, but the message reads the
    // continuous variable x: the message is not structurally evaluable, so
    // the runtime keeps the statement, its exact message structure, and the
    // function. The message pin proves the pass neither replaced nor
    // altered a runtime-owned message: the original concatenation, the
    // String conversion, and the exact x declaration identity survive.
    let (flat, ids) = flatten_model("P.RuntimeMessageKept");
    let flat = flat.expect("an assertion with a runtime message never fails translation");
    let asserts = initial_asserts(&flat);
    assert_eq!(asserts.len(), 1);
    let rumoca_core::Statement::Assert { message, .. } = asserts[0] else {
        unreachable!("initial_asserts returns only Assert statements");
    };
    let rumoca_core::Expression::Binary { op, lhs, rhs, .. } = message.as_ref() else {
        panic!("the retained message keeps its concatenation, found {message:?}");
    };
    assert_eq!(*op, rumoca_core::OpBinary::Add);
    assert!(
        matches!(
            lhs.as_ref(),
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::String(text),
                ..
            } if text == "x was "
        ),
        "the literal half of the message is unchanged, found {lhs:?}"
    );
    let rumoca_core::Expression::StringConversion { value, .. } = rhs.as_ref() else {
        panic!("the runtime half keeps its String conversion, found {rhs:?}");
    };
    let rumoca_core::Expression::VarRef { name, .. } = value.as_ref() else {
        panic!("the conversion argument is the continuous x, found {value:?}");
    };
    let x_declaration = flat
        .variables
        .get(&rumoca_core::VarName::new("x"))
        .expect("the flat model keeps x")
        .component_ref
        .as_ref()
        .expect("the flat x keeps its resolved reference")
        .target_def_id();
    assert_eq!(
        name.component_ref()
            .expect("the message reference keeps its resolved structure")
            .target_def_id(),
        x_declaration,
        "the message still names the exact x declaration"
    );
    assert_eq!(functions_with_identity(&flat, ids.pow2), 1);
}
