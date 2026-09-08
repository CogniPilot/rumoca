//! End-to-end identity coverage for MLS import precedence.
//!
//! Resolve selects a single-definition import ahead of a wildcard import.
//! Instantiate and Flatten must project that exact selection rather than
//! rebuilding a short-name map in source-clause order.

use rumoca_core::ExpressionVisitor;
use rumoca_ir_ast as ast;

const SOURCE_NAME: &str = "<import_precedence_identity>";

#[derive(Default)]
struct IntegerLiteralCollector {
    values: Vec<i64>,
}

impl ExpressionVisitor for IntegerLiteralCollector {
    fn visit_expression(&mut self, expression: &rumoca_core::Expression) {
        if let rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(value),
            ..
        } = expression
        {
            self.values.push(*value);
        }
        self.walk_expression(expression);
    }
}

fn flattened_model(imports: &str) -> rumoca_ir_flat::Model {
    let source = format!(
        r#"
package Named
  constant Integer X = 1;
end Named;

package Wild
  constant Integer X = 2;
end Wild;

model M
{imports}
  Integer y;
equation
  y = X;
end M;
"#
    );
    let stored = rumoca_phase_parse::parse_to_ast(&source, SOURCE_NAME).expect("source parses");
    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map.add(SOURCE_NAME, &source);
    let resolved =
        rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree)).expect("import fixture resolves");
    let overlay =
        match rumoca_phase_instantiate::instantiate_model_with_outcome(resolved.inner(), "M") {
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
    let typed = rumoca_phase_typecheck::typecheck_instanced_tree(&resolved, overlay, "M")
        .expect("import fixture typechecks");
    rumoca_phase_flatten::flatten_typed(typed, rumoca_phase_flatten::FlattenOptions::default())
        .expect("import fixture flattens")
}

fn instantiated_array_projection(imports: &str) -> (ast::InstanceOverlay, rumoca_core::DefId) {
    let source = format!(
        r#"
package Named
  constant Integer X = 1;
end Named;

package Wild
  constant Integer X = 2;
end Wild;

model M
{imports}
  Real a[X];
end M;
"#
    );
    let stored = rumoca_phase_parse::parse_to_ast(&source, SOURCE_NAME).expect("source parses");
    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map.add(SOURCE_NAME, &source);
    let resolved = rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree))
        .expect("array import fixture resolves");
    let selected = resolved
        .inner()
        .get_def_id_by_name("Named.X")
        .expect("Resolve registered Named.X");
    let overlay =
        match rumoca_phase_instantiate::instantiate_model_with_outcome(resolved.inner(), "M") {
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
    (overlay, selected)
}

fn assert_named_import_wins(imports: &str) {
    let model = flattened_model(imports);
    let mut collector = IntegerLiteralCollector::default();
    for equation in &model.equations {
        collector.visit_expression(&equation.residual);
    }
    let values = collector.values;
    assert!(
        values.contains(&1),
        "the selected Named.X declaration must reach Flat, got {values:?}"
    );
    assert!(
        !values.contains(&2),
        "the lower-precedence Wild.X declaration must not reach Flat, got {values:?}"
    );
    let (overlay, selected) = instantiated_array_projection(imports);
    let array = overlay
        .components
        .values()
        .find(|instance| {
            let name = instance.qualified_name.to_flat_string();
            name == "a" || name.ends_with(".a")
        })
        .expect("fixture contains instantiated component `a`");
    let [ast::Subscript::Expression(ast::Expression::ComponentReference(reference))] =
        array.dims_expr.as_slice()
    else {
        panic!(
            "fixture retains one symbolic reference dimension, got {:?}",
            array.dims_expr
        );
    };
    assert_eq!(
        reference.to_string(),
        "Named.X",
        "array dimension spelling must project Resolve's selected import"
    );
    assert_eq!(
        reference.target_def_id(),
        Some(selected),
        "array dimension identity must remain Resolve's selected Named.X declaration"
    );
}

#[test]
fn single_definition_import_beats_later_wildcard_projection() {
    assert_named_import_wins("  import Named.X;\n  import Wild.*;");
}

#[test]
fn single_definition_import_beats_earlier_wildcard_projection() {
    assert_named_import_wins("  import Wild.*;\n  import Named.X;");
}
