//! Flat resolves every call to exactly one exposed function declaration plus
//! exactly one selected implementation.  A short-class function alias inherits
//! its implementation, so the alias chain must terminate in a single body.
//!
//! When it does not, two `extends` clauses each contributing an algorithm
//! section, the function has two bodies, which MLS 3.7 §12.2 forbids (\"A
//! function can have at most one algorithm section\"). Flatten refuses the
//! function with `EF035` rather than pick an arbitrary body.

use miette::Diagnostic;
use rumoca_core::ExpressionVisitor;
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

const AMBIGUOUS_FILE: &str = "<function_selection_identity_ambiguous>";
const UNIQUE_FILE: &str = "<function_selection_identity_unique>";

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
fn ambiguous_alias_is_refused_as_a_function_with_two_bodies() {
    let fixture = Fixture::prepare(AMBIGUOUS_ALIAS, AMBIGUOUS_FILE, "UsesAmbiguous");
    let error = fixture
        .flatten()
        .expect_err("a function inheriting two algorithm sections has two bodies");
    let FlattenError::MultipleFunctionBodies { sections, .. } = &error else {
        panic!("expected a multiple-function-bodies failure, got {error:?}");
    };
    assert_eq!(*sections, 2);
    assert_eq!(
        error.code().map(|code| code.to_string()).as_deref(),
        Some("rumoca::flatten::EF035")
    );
}

#[test]
fn unique_alias_keeps_its_inherited_function_selection_identity() {
    let fixture = Fixture::prepare(UNIQUE_ALIAS, UNIQUE_FILE, "UsesUniqueAlias");
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
