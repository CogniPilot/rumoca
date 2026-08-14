//! Exact-identity regression coverage for transitive package constants.
//!
//! A retained Resolve dependency closure may keep only the package/class
//! owners needed by the selected model. Flat constant materialization must
//! therefore follow the resolved root identity and structured member path,
//! rather than depending on a rendered-name search over unrelated classes.

use rumoca_core::ExpressionVisitor;
use rumoca_ir_ast as ast;

const SOURCE_NAME: &str = "<transitive_package_constants>";
const SOURCE: &str = r#"
package Services
    package Machine
        final constant Real eps = 0.125;
    end Machine;
end Services;

package Library
    package Constants
        final constant Real eps = Services.Machine.eps;
    end Constants;

    model Top
        Real y;
        Real z;
    equation
        y = Library.Constants.eps;
        z = Other.Constants.eps;
    end Top;
end Library;

package Other
    package Constants
        final constant Real eps = 0.5;
    end Constants;
end Other;
"#;

#[derive(Default)]
struct ConstantUseCollector {
    literals: Vec<(f64, rumoca_core::Span)>,
    references: Vec<String>,
}

impl ExpressionVisitor for ConstantUseCollector {
    fn visit_expression(&mut self, expression: &rumoca_core::Expression) {
        if let rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(value),
            span,
        } = expression
        {
            self.literals.push((*value, *span));
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

#[test]
fn same_leaf_package_constants_materialize_by_exact_target_at_each_use_site() {
    let stored = rumoca_phase_parse::parse_to_ast(SOURCE, SOURCE_NAME).expect("source parses");
    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map.add(SOURCE_NAME, SOURCE);
    let resolved =
        rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree)).expect("source resolves");
    let instanced =
        rumoca_phase_instantiate::instantiate(resolved, "Library.Top").expect("model instantiates");
    let model =
        rumoca_phase_flatten::flatten_ref(instanced.inner(), instanced.overlay(), "Library.Top")
            .expect("model flattens");

    let mut collector = ConstantUseCollector::default();
    for equation in &model.equations {
        collector.visit_expression(&equation.residual);
    }

    assert!(
        !collector.references.iter().any(|name| {
            name == "Library.Constants.eps"
                || name == "Services.Machine.eps"
                || name == "Other.Constants.eps"
        }),
        "package constants must be materialized, got {:?}",
        collector.references
    );
    let use_start = SOURCE
        .rfind("Library.Constants.eps")
        .expect("fixture contains the equation occurrence");
    let expected_span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(SOURCE_NAME),
        use_start,
        use_start + "Library.Constants.eps".len(),
    );
    assert!(
        collector
            .literals
            .iter()
            .any(|(value, span)| *value == 0.125 && *span == expected_span),
        "the materialized value must retain the exact use-site span, got {:?}",
        collector.literals
    );

    let other_start = SOURCE
        .rfind("Other.Constants.eps")
        .expect("fixture contains the second equation occurrence");
    let other_span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(SOURCE_NAME),
        other_start,
        other_start + "Other.Constants.eps".len(),
    );
    assert!(
        collector
            .literals
            .iter()
            .any(|(value, span)| *value == 0.5 && *span == other_span),
        "same-named leaves in another package cannot cross-bind, got {:?}",
        collector.literals
    );
}
