//! Qualification derives flat identity from resolved structure, never from a
//! hard-coded catalogue of top-level library names.
//!
//! Flat component-reference identity comes from the resolved `DefId` recorded on
//! each reference part (SPEC_0001 §Semantic Identity Keys: rendered names and
//! component-reference display text "are not valid identity fields"; compiler
//! semantic identity "is resolved-id based"). Two consequences are pinned here:
//!
//! - A top-level package whose spelling is unknown to any built-in list still
//!   reaches flatten and materializes with correct flat spelling. No allowlist
//!   of library names is consulted, so none can ever need extending.
//!
//! - A local component whose spelling collides with a well-known library name
//!   (`Buildings`) is still instance-prefixed from its resolved identity. The
//!   flat name follows the instance path, never the bare source spelling that a
//!   name-matching qualification shortcut would have preserved.

use rumoca_core::ExpressionVisitor;
use rumoca_ir_ast as ast;
use rumoca_ir_flat as flat;

fn flatten_model(source: &str, source_name: &str, model: &str) -> flat::Model {
    let stored = rumoca_phase_parse::parse_to_ast(source, source_name).expect("source parses");
    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map.add(source_name, source);
    let resolved =
        rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree)).expect("source resolves");
    let overlay =
        match rumoca_phase_instantiate::instantiate_model_with_outcome(resolved.inner(), model) {
            rumoca_phase_instantiate::InstantiationOutcome::Success(overlay) => overlay,
            rumoca_phase_instantiate::InstantiationOutcome::NeedsInner {
                missing_inners, ..
            } => {
                panic!("fixture unexpectedly needs inner declarations: {missing_inners:?}")
            }
            rumoca_phase_instantiate::InstantiationOutcome::Error(error) => {
                panic!("fixture instantiation failed: {error}")
            }
        };
    let typed = rumoca_phase_typecheck::typecheck_instanced_tree(&resolved, overlay, model)
        .expect("instanced model typechecks");
    rumoca_phase_flatten::flatten_typed(typed, rumoca_phase_flatten::FlattenOptions::default())
        .expect("typed model flattens")
}

#[derive(Default)]
struct ReferenceNameCollector {
    references: Vec<String>,
    literals: Vec<f64>,
}

impl ExpressionVisitor for ReferenceNameCollector {
    fn visit_expression(&mut self, expression: &rumoca_core::Expression) {
        if let rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(value),
            ..
        } = expression
        {
            self.literals.push(*value);
        }
        self.walk_expression(expression);
    }

    fn visit_var_ref(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
    ) {
        self.references.push(name.as_str().to_string());
        self.walk_var_ref(name, subscripts);
    }
}

fn collect_reference_names(model: &flat::Model) -> ReferenceNameCollector {
    let mut collector = ReferenceNameCollector::default();
    for equation in &model.equations {
        collector.visit_expression(&equation.residual);
    }
    collector
}

fn variable_names(model: &flat::Model) -> Vec<String> {
    model
        .variables
        .iter()
        .map(|(name, _)| name.as_str().to_string())
        .collect()
}

/// W1: an unlisted top-level library flattens correctly with no allowlist.
///
/// `AcmeLib` is not, and must never need to be, a member of any built-in list
/// of "known package" spellings. Its package constant materializes into the
/// flat equation exactly as a listed library's would, proving qualification is
/// list-independent and that "add the name to the array" is not a valid repair.
#[test]
fn unlisted_top_level_library_materializes_without_an_allowlist() {
    const SOURCE_NAME: &str = "<qualify_no_library_allowlist::w1>";
    const SOURCE: &str = r#"
package AcmeLib
    package Constants
        final constant Real k = 0.375;
    end Constants;

    model Top
        Real y;
    equation
        y = AcmeLib.Constants.k;
    end Top;
end AcmeLib;
"#;

    let model = flatten_model(SOURCE, SOURCE_NAME, "AcmeLib.Top");

    let names = variable_names(&model);
    assert!(
        names.iter().any(|name| name == "y"),
        "flat model must expose the top-level variable `y`, got {names:?}"
    );

    let collector = collect_reference_names(&model);
    assert!(
        !collector
            .references
            .iter()
            .any(|name| name == "AcmeLib.Constants.k"),
        "the unlisted package constant must be materialized, not left as a \
         reference, got {:?}",
        collector.references
    );
    assert!(
        collector.literals.contains(&0.375),
        "the unlisted package constant must materialize to its value, got {:?}",
        collector.literals
    );
}

/// W3: a package constant referenced from a nested component occurrence is
/// materialized before the occurrence prefix can become semantic identity.
///
/// Unlike W2, this is not a mutation discriminator for the deleted branch. It
/// exercises the branch's former non-empty-prefix path and pins the reason the
/// deletion is safe: exact package-constant identity removes the reference
/// before its presentation spelling can escape into Flat IR.
#[test]
fn nested_instance_package_constant_is_materialized_before_flat_publication() {
    const SOURCE_NAME: &str = "<qualify_no_library_allowlist::w3>";
    const SOURCE: &str = r#"
package AcmeLib
    package Constants
        final constant Real k = 0.625;
    end Constants;

    model Inner
        Real y;
    equation
        y = AcmeLib.Constants.k;
    end Inner;

    model Top
        Inner nested;
    end Top;
end AcmeLib;
"#;

    let model = flatten_model(SOURCE, SOURCE_NAME, "AcmeLib.Top");

    let names = variable_names(&model);
    assert!(
        names.iter().any(|name| name == "nested.y"),
        "nested occurrence must publish its instance-prefixed variable, got {names:?}"
    );

    let collector = collect_reference_names(&model);
    assert!(
        !collector
            .references
            .iter()
            .any(|name| name.contains("AcmeLib.Constants.k")),
        "the nested package constant must be materialized before any prefixed display spelling escapes, got {:?}",
        collector.references
    );
    assert!(
        collector.literals.contains(&0.625),
        "the nested package constant must materialize to its exact value, got {:?}",
        collector.literals
    );
}

/// W2: a local component that shadows a well-known library name is still
/// instance-prefixed from its resolved identity.
///
/// `Buildings` is a bare `Real` component of `C`, not a package. When `C` is a
/// component `c` of the flattened model, every use of `Buildings` must resolve
/// to the instance-prefixed flat name `c.Buildings`. A qualification shortcut
/// that treated the source spelling `Buildings` as already fully qualified
/// would leave the bare name and collapse the two instances' identities; the
/// resolved-`DefId` identity forbids that.
#[test]
fn library_shadowing_local_is_instance_prefixed_not_left_bare() {
    const SOURCE_NAME: &str = "<qualify_no_library_allowlist::w2>";
    const SOURCE: &str = r#"
model C
    Real Buildings;
    Real y;
equation
    Buildings = 1;
    y = Buildings + 1;
end C;

model Top
    C c;
end Top;
"#;

    let model = flatten_model(SOURCE, SOURCE_NAME, "Top");

    let names = variable_names(&model);
    assert!(
        names.iter().any(|name| name == "c.Buildings"),
        "the shadowing local must flatten to the instance-prefixed name \
         `c.Buildings`, got {names:?}"
    );
    assert!(
        !names.iter().any(|name| name == "Buildings"),
        "the shadowing local must never keep its bare source spelling \
         `Buildings`, got {names:?}"
    );

    let collector = collect_reference_names(&model);
    assert!(
        collector
            .references
            .iter()
            .any(|name| name == "c.Buildings"),
        "equation references to the shadowing local must be instance-prefixed \
         `c.Buildings`, got {:?}",
        collector.references
    );
    assert!(
        !collector.references.iter().any(|name| name == "Buildings"),
        "no equation reference may keep the bare spelling `Buildings`, got {:?}",
        collector.references
    );
}
