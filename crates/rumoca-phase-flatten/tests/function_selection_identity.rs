//! Flat resolves every call to exactly one exposed function declaration plus
//! exactly one selected implementation.  A short-class function alias inherits
//! its implementation, so the alias chain must terminate in a single body.
//!
//! When it does not — two `extends` clauses each contributing a distinct
//! implementation — there is no exact selection to record, and guessing one
//! would silently pick an arbitrary body.  Flatten therefore refuses the call
//! with `EF025`, naming the callable and the fact it could not establish.

use miette::Diagnostic;
use rumoca_core::{ExpressionVisitor, PhaseError};
use rumoca_ir_ast as ast;
use rumoca_ir_flat as flat;
use rumoca_phase_flatten::FlattenError;

/// Both bases carry an algorithm section, so `ambiguous` inherits two rival
/// implementations and exposes neither exactly.
const AMBIGUOUS_ALIAS: &str = r#"
function baseA
  input Real u;
  output Real y;
algorithm
  y := u;
end baseA;

function baseB
  input Real u;
  output Real y;
algorithm
  y := 2 * u;
end baseB;

function ambiguous
  extends baseA;
  extends baseB;
end ambiguous;

model UsesAmbiguous
  Real x;
equation
  x = ambiguous(1.0);
end UsesAmbiguous;
"#;

/// The same alias shape with a single base: selection is exact, so flattening
/// must still succeed.  This keeps `EF025` a guard on ambiguity rather than a
/// blanket rejection of inherited function bodies.
const UNIQUE_ALIAS: &str = r#"
function baseA
  input Real u;
  output Real y;
algorithm
  y := u;
end baseA;

function aliasA
  extends baseA;
end aliasA;

model UsesUniqueAlias
  Real x;
equation
  x = aliasA(1.0);
end UsesUniqueAlias;
"#;

const SOURCE_ALIAS_REDECLARES: &str = r#"
package P
  partial package PartialMedium
    function f
      input Real x;
      output Real y;
    algorithm
      y := x;
    end f;
  end PartialMedium;

  package MediumA
    extends PartialMedium;
    function f
      input Real x;
      output Real y;
    algorithm
      y := 2*x;
    end f;
  end MediumA;

  package MediumB
    extends PartialMedium;
    function f
      input Real x;
      output Real y;
    algorithm
      y := 3*x;
    end f;
  end MediumB;

  model Holder
    replaceable package Medium = PartialMedium constrainedby PartialMedium;
    Real y;
  equation
    y = Medium.f(1);
  end Holder;

  model Siblings
    Holder first(redeclare package Medium = MediumA);
    Holder second(redeclare package Medium = MediumB);
  end Siblings;
end P;
"#;

const AMBIGUOUS_FILE: &str = "<function_selection_identity_ambiguous>";
const UNIQUE_FILE: &str = "<function_selection_identity_unique>";

fn required_def_id(tree: &ast::ClassTree, name: &str) -> rumoca_core::DefId {
    tree.get_def_id_by_name(name)
        .unwrap_or_else(|| panic!("missing DefId for {name}"))
}

fn source_alias_function_calls(overlay: &ast::InstanceOverlay) -> Vec<ast::ComponentReference> {
    overlay
        .classes
        .values()
        .flat_map(|class| class.equations.iter())
        .filter_map(|equation| {
            let ast::Equation::Simple { lhs, rhs } = &equation.equation else {
                return None;
            };
            Some([lhs, rhs])
        })
        .flatten()
        .flat_map(ast::collect_component_refs)
        .filter(|reference| reference.to_string() == "Medium.f")
        .collect()
}

fn source_alias_override(
    component: &ast::InstanceData,
    alias_def_id: rumoca_core::DefId,
) -> &ast::ClassOverride {
    component
        .class_overrides
        .values()
        .find(|class_override| class_override.alias_def_id == alias_def_id)
        .expect("exact package slot selection")
}

/// One source carried through parse, resolve, instantiate and typecheck, so
/// flattening runs on the same tree the compiler builds rather than on a
/// hand-assembled one.
struct Fixture {
    tree: ast::ClassTree,
    overlay: ast::InstanceOverlay,
    model_name: String,
}

impl Fixture {
    fn prepare(source: &str, file_name: &str, model_name: &str) -> Self {
        let stored = rumoca_phase_parse::parse_to_ast(source, file_name).expect("source parses");
        let mut tree = ast::ClassTree::from_parsed(stored);
        tree.source_map.add(file_name, source);
        let resolved =
            rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree)).expect("source resolves");
        let ast::InstancedTree { tree, mut overlay } =
            rumoca_phase_instantiate::instantiate(resolved, model_name)
                .expect("model instantiates");
        rumoca_phase_typecheck::typecheck_instanced(&tree, &mut overlay, model_name)
            .expect("model typechecks");
        Self {
            tree,
            overlay,
            model_name: model_name.to_string(),
        }
    }

    fn flatten(&self) -> Result<flat::Model, FlattenError> {
        rumoca_phase_flatten::flatten_ref(&self.tree, &self.overlay, &self.model_name)
    }
}

/// Gathers the call references a flat model still carries, so the positive
/// control can follow a call to the implementation it selected.
#[derive(Default)]
struct CallReferences {
    references: Vec<rumoca_core::Reference>,
}

impl ExpressionVisitor for CallReferences {
    fn visit_function_call(
        &mut self,
        name: &rumoca_core::Reference,
        args: &[rumoca_core::Expression],
        is_constructor: bool,
    ) {
        self.references.push(name.clone());
        self.walk_function_call(name, args, is_constructor);
    }
}

#[test]
fn ambiguous_alias_fails_function_selection_identity_at_the_call_site() {
    let fixture = Fixture::prepare(AMBIGUOUS_ALIAS, AMBIGUOUS_FILE, "UsesAmbiguous");
    let error = fixture
        .flatten()
        .expect_err("an alias inheriting two implementations has no exact selection");

    let FlattenError::MissingFunctionSelectionIdentity {
        function,
        reason,
        span,
    } = &error
    else {
        panic!("expected a missing-selection-identity failure, got {error:?}");
    };
    assert_eq!(function, "ambiguous");
    assert_eq!(
        reason,
        "exposed function has no unique exact extends implementation"
    );
    assert_eq!(
        error.to_string(),
        "missing exact function-selection identity for `ambiguous`: \
exposed function has no unique exact extends implementation"
    );

    assert_eq!(
        error.code().map(|code| code.to_string()).as_deref(),
        Some("rumoca::flatten::EF025"),
        "EF025 is the shipped identity of this diagnostic"
    );
    assert!(
        error
            .help()
            .map(|help| help.to_string())
            .is_some_and(|help| help.contains("selected implementation")),
        "the help text must name the missing exposure/implementation pair"
    );

    assert_eq!(
        span.source,
        rumoca_core::source_id_for_name(AMBIGUOUS_FILE),
        "the failure must be attributed to the source that declared the call"
    );
    assert_eq!(
        &AMBIGUOUS_ALIAS[span.start.0..span.end.0],
        "ambiguous(1.0)",
        "the span must point at the call whose selection could not be established"
    );

    let diagnostic = error.to_diagnostic();
    assert_eq!(diagnostic.code.as_deref(), Some("EF025"));
    assert_eq!(
        diagnostic.severity,
        rumoca_core::DiagnosticSeverity::Error,
        "an `E` range code must be reported at error severity (SPEC_0008)"
    );
    assert_eq!(
        diagnostic
            .labels
            .iter()
            .map(|label| label.span)
            .collect::<Vec<_>>(),
        vec![*span],
        "the diagnostic must carry the call-site span as its only label"
    );
    assert!(
        diagnostic
            .notes
            .iter()
            .any(|note| note.contains("selected implementation")),
        "the rendered diagnostic must keep the help text as a note"
    );
}

#[test]
fn unique_alias_keeps_its_inherited_function_selection_identity() {
    let fixture = Fixture::prepare(UNIQUE_ALIAS, UNIQUE_FILE, "UsesUniqueAlias");
    let exposed_alias = required_def_id(&fixture.tree, "aliasA");
    let model = fixture
        .flatten()
        .expect("a single-base alias has an exact selection");

    let mut calls = CallReferences::default();
    for equation in &model.equations {
        calls.visit_expression(&equation.residual);
    }
    let [call] = calls.references.as_slice() else {
        panic!(
            "the alias call must survive flattening exactly once; got {:?}",
            calls.references
        );
    };
    assert_eq!(
        call.as_str(),
        "aliasA",
        "the call keeps the exposed alias as its display spelling"
    );
    assert_eq!(
        call.target_def_id(),
        Some(exposed_alias),
        "the call occurrence keeps the exposed alias identity while its collected body uses the inherited implementation"
    );

    let selected = call
        .resolved_function()
        .expect("an exactly selected call records its function instance");
    let implementation = model
        .functions
        .values()
        .find(|function| function.instance_id == Some(selected.instance_id))
        .unwrap_or_else(|| {
            let collected = model
                .functions
                .values()
                .map(|function| (function.name.as_str(), function.instance_id))
                .collect::<Vec<_>>();
            panic!(
                "the recorded instance {:?} must name a collected flat function; collected \
                 {collected:?}",
                selected.instance_id
            )
        });
    assert!(
        !implementation.body.is_empty(),
        "selection must reach the inherited algorithm body, not the empty alias shell"
    );
}

#[test]
fn parsed_source_aliases_keep_sibling_package_selections_exact() {
    let fixture = Fixture::prepare(
        SOURCE_ALIAS_REDECLARES,
        "<function_selection_identity_source_aliases>",
        "P.Siblings",
    );
    let source_alias = required_def_id(&fixture.tree, "P.Holder.Medium");
    let medium_a = required_def_id(&fixture.tree, "P.MediumA");
    let medium_b = required_def_id(&fixture.tree, "P.MediumB");
    let function_a = required_def_id(&fixture.tree, "P.MediumA.f");
    let function_b = required_def_id(&fixture.tree, "P.MediumB.f");
    assert_ne!(source_alias, medium_a);
    assert_ne!(source_alias, medium_b);
    assert_ne!(medium_a, medium_b);
    assert_ne!(function_a, function_b);

    let first = fixture
        .overlay
        .components
        .values()
        .find(|component| component.qualified_name.to_flat_string() == "first")
        .expect("first sibling instance");
    let second = fixture
        .overlay
        .components
        .values()
        .find(|component| component.qualified_name.to_flat_string() == "second")
        .expect("second sibling instance");
    let first_override = source_alias_override(first, source_alias);
    let second_override = source_alias_override(second, source_alias);
    assert_eq!(first_override.alias_def_id, source_alias);
    assert_eq!(second_override.alias_def_id, source_alias);
    assert_eq!(first_override.target_def_id, medium_a);
    assert_eq!(second_override.target_def_id, medium_b);

    let source_calls = source_alias_function_calls(&fixture.overlay);
    assert_eq!(
        source_calls.len(),
        2,
        "both sibling calls retain source spelling"
    );
    assert!(source_calls.iter().all(|reference| {
        reference.root_def_id() == Some(source_alias)
            && reference.target_def_id() != Some(source_alias)
    }));
    assert_eq!(
        source_calls
            .iter()
            .filter_map(|reference| reference.target_def_id())
            .collect::<Vec<_>>(),
        vec![function_a, function_b],
        "instantiation selects each sibling's concrete function while preserving the source alias prefix"
    );

    let model = fixture
        .flatten()
        .expect("sibling source aliases select exact function implementations");
    let mut flat_calls = CallReferences::default();
    for equation in &model.equations {
        flat_calls.visit_expression(&equation.residual);
    }
    let selected = flat_calls
        .references
        .iter()
        .filter_map(|reference| reference.target_def_id())
        .collect::<Vec<_>>();
    assert!(
        selected.contains(&function_a),
        "first selected implementation survives"
    );
    assert!(
        selected.contains(&function_b),
        "second selected implementation survives"
    );
}
