//! Full-pipeline coverage for the import view a function body lowers against.
//!
//! Every fixture runs parse, resolve, instantiate, typecheck, and flatten, so
//! each assertion witnesses the import decision that actually reaches the
//! Flat function: the lookup authority decides each name for the scope that
//! textually declares the body (MLS §5.3.1, §13.2), a named import beats a
//! wildcard regardless of clause order, imports are never inherited across
//! `extends`, the encapsulation barrier stops the lexical climb, and an
//! ambiguous name is a typed refusal rather than a silent binding.

use rumoca_ir_ast as ast;

const SOURCE_NAME: &str = "<function_import_authority>";

fn flatten_pipeline(source: &str, model: &str) -> Result<rumoca_ir_flat::Model, String> {
    let stored =
        rumoca_phase_parse::parse_to_ast(source, SOURCE_NAME).map_err(|e| e.to_string())?;
    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map.add(SOURCE_NAME, source);
    let resolved = rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree))
        .map_err(|e| format!("resolve: {e:?}"))?;
    let overlay =
        match rumoca_phase_instantiate::instantiate_model_with_outcome(resolved.inner(), model) {
            rumoca_phase_instantiate::InstantiationOutcome::Success(overlay) => overlay,
            rumoca_phase_instantiate::InstantiationOutcome::NeedsInner {
                missing_inners, ..
            } => {
                return Err(format!(
                    "instantiate: unexpectedly needs inner declarations: {missing_inners:?}"
                ));
            }
            rumoca_phase_instantiate::InstantiationOutcome::Error(error) => {
                return Err(format!("instantiate: {error}"));
            }
        };
    let typed = rumoca_phase_typecheck::typecheck_instanced_tree(&resolved, overlay, model)
        .map_err(|e| format!("typecheck: {e:?}"))?;
    rumoca_phase_flatten::flatten_typed(typed, rumoca_phase_flatten::FlattenOptions::default())
        .map_err(|e| format!("flatten: {e}"))
}

/// The one collected function body, rendered for exact-name assertions.
fn sole_function_body(model: &rumoca_ir_flat::Model, function_name: &str) -> String {
    let function = model
        .functions
        .values()
        .find(|function| function.name.as_str().ends_with(function_name))
        .unwrap_or_else(|| {
            panic!(
                "function {function_name} must be collected, got {:?}",
                model.functions.keys().collect::<Vec<_>>()
            )
        });
    format!("{:?}", function.body)
}

/// A function body reaches, through `import Wild.*`, a member the imported
/// package only inherits (MLS §13.2: the wildcard covers the package's export
/// view, inherited declarations included).
#[test]
fn function_body_wildcard_import_reaches_inherited_member() {
    let source = r#"
package Base
  constant Real X = 1.0;
end Base;

package Wild
  extends Base;
end Wild;

function f
  import Wild.*;
  input Real u;
  output Real y;
algorithm
  y := u + X;
end f;

model M
  Real z;
equation
  z = f(1.0);
end M;
"#;
    let model = flatten_pipeline(source, "M").expect("inherited wildcard member must flatten");
    let body = sole_function_body(&model, "f");
    assert!(
        body.contains("Real(1.0)"),
        "the function body must bind X through the wildcard's export view and \
         carry Base.X = 1.0, got {body}"
    );
}

/// A named import wins over a wildcard for the same short name (MLS §5.3.1),
/// with the named clause written first.
#[test]
fn function_named_import_beats_wildcard_named_first() {
    let model = flatten_pipeline(NAMED_BEFORE_WILDCARD, "M")
        .expect("named-import-over-wildcard fixture must flatten");
    let body = sole_function_body(&model, "f");
    assert!(
        body.contains("Real(1.0)") && !body.contains("Real(2.0)"),
        "the named import A.X = 1.0 must win over the wildcard's B.X = 2.0, got {body}"
    );
}

/// The same decision with the wildcard clause written first: clause order is
/// erased, the named import still wins (MLS §5.3.1).
#[test]
fn function_named_import_beats_wildcard_wildcard_first() {
    let model =
        flatten_pipeline(WILDCARD_BEFORE_NAMED, "M").expect("wildcard-first fixture must flatten");
    let body = sole_function_body(&model, "f");
    assert!(
        body.contains("Real(1.0)") && !body.contains("Real(2.0)"),
        "the named import A.X = 1.0 must win regardless of clause order, got {body}"
    );
}

const NAMED_BEFORE_WILDCARD: &str = r#"
package A
  constant Real X = 1.0;
end A;

package B
  constant Real X = 2.0;
end B;

function f
  import A.X;
  import B.*;
  output Real y;
algorithm
  y := X;
end f;

model M
  Real z;
equation
  z = f();
end M;
"#;

const WILDCARD_BEFORE_NAMED: &str = r#"
package A
  constant Real X = 1.0;
end A;

package B
  constant Real X = 2.0;
end B;

function f
  import B.*;
  import A.X;
  output Real y;
algorithm
  y := X;
end f;

model M
  Real z;
equation
  z = f();
end M;
"#;

/// An inherited function body still binds through the import of the scope
/// that textually declares it: the base's statements resolve in the base's
/// scope (MLS §7.1), not in the derived function's.
#[test]
fn inherited_function_body_binds_its_origin_scopes_import() {
    let source = r#"
package P
  constant Real X = 1.0;
end P;

function base
  import P.X;
  input Real u;
  output Real y;
algorithm
  y := u + X;
end base;

function derived
  extends base;
end derived;

model M
  Real z;
equation
  z = derived(1.0);
end M;
"#;
    let model = flatten_pipeline(source, "M")
        .expect("an inherited body must bind through its origin scope's import");
    let body = sole_function_body(&model, "derived");
    assert!(
        body.contains("Real(1.0)"),
        "the inherited body must bind the base scope's P.X = 1.0, got {body}"
    );
}

/// The negative twin (MLS §13.2): a derived function's own body must not see
/// the base function's import; the unimported short name is refused, never
/// silently bound through the extends chain.
#[test]
fn function_import_is_not_inherited_across_extends() {
    let source = r#"
package P
  constant Real X = 1.0;
end P;

partial function base
  import P.X;
  input Real u;
  output Real y;
end base;

function derived
  extends base;
algorithm
  y := u + X;
end derived;

model M
  Real z;
equation
  z = derived(1.0);
end M;
"#;
    let error = flatten_pipeline(source, "M")
        .expect_err("a derived function must not inherit the base function's import");
    assert!(
        error.starts_with("resolve:") && error.contains("unresolved component reference: 'X'"),
        "the unimported name must already fail name resolution, got: {error}"
    );
}

/// The encapsulation barrier (MLS §5.3.1): an encapsulated function's body
/// cannot reach an enclosing scope's import, while a sibling function without
/// the barrier can.
#[test]
fn encapsulated_function_body_does_not_reach_enclosing_import() {
    let error = flatten_pipeline(ENCAPSULATED_BARRIER, "MBarrier")
        .expect_err("an encapsulated function must not see the enclosing scope's import");
    assert!(
        error.starts_with("resolve:") && error.contains("unresolved component reference: 'X'"),
        "the barrier must already stop name resolution, got: {error}"
    );
}

/// The positive twin: without `encapsulated`, the same body binds the
/// enclosing scope's import.
#[test]
fn unencapsulated_function_body_reaches_enclosing_import() {
    let model = flatten_pipeline(ENCLOSING_IMPORT_VISIBLE, "MOpen")
        .expect("a non-encapsulated function must see the enclosing scope's import");
    let body = sole_function_body(&model, "g");
    assert!(
        body.contains("Real(1.0)"),
        "the enclosing scope's import Lib.X = 1.0 must bind in the body, got {body}"
    );
}

const ENCAPSULATED_BARRIER: &str = r#"
package Lib
  constant Real X = 1.0;
end Lib;

package Q
  import Lib.X;
  encapsulated function f
    output Real y;
  algorithm
    y := X;
  end f;
end Q;

model MBarrier
  Real z;
equation
  z = Q.f();
end MBarrier;
"#;

const ENCLOSING_IMPORT_VISIBLE: &str = r#"
package Lib
  constant Real X = 1.0;
end Lib;

package Q
  import Lib.X;
  function g
    output Real y;
  algorithm
    y := X;
  end g;
end Q;

model MOpen
  Real z;
equation
  z = Q.g();
end MOpen;
"#;

/// A short name supplied by two wildcard imports inside a function body is a
/// typed refusal at its use site (MLS §5.3.1), never a silent
/// last-clause-wins binding.
#[test]
fn ambiguous_wildcard_name_in_function_body_is_refused() {
    let source = r#"
package P1
  constant Real X = 1.0;
end P1;

package P2
  constant Real X = 2.0;
end P2;

function f
  import P1.*;
  import P2.*;
  output Real y;
algorithm
  y := X;
end f;

model M
  Real z;
equation
  z = f();
end M;
"#;
    let error = flatten_pipeline(source, "M")
        .expect_err("a doubly wildcard-supplied name must be refused, not bound");
    assert!(
        error.to_lowercase().contains("ambiguous"),
        "the refusal must name the ambiguity, got: {error}"
    );
}
