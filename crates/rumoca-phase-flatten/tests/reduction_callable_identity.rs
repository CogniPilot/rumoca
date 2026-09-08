//! Reduction rewrites require the exact predefined callable identity.
//!
//! `sum` and `product` are ordinary identifiers for lookup purposes: a user
//! declaration with either spelling shadows the predefined reduction. These
//! fixtures run the production parse-to-flatten path so the rewrite consumes
//! Resolve's identity instead of inferring semantics from source text.

use rumoca_core::Expression;
use rumoca_ir_ast as ast;

fn flatten_source(source: &str, model_name: &str) -> rumoca_ir_flat::Model {
    let file_name = "<reduction_callable_identity>";
    let stored = rumoca_phase_parse::parse_to_ast(source, file_name).expect("source parses");
    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map.add(file_name, source);
    let resolved = rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree))
        .expect("source resolves by exact declaration identity");
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
    let typed = rumoca_phase_typecheck::typecheck_instanced_tree(&resolved, overlay, model_name)
        .expect("model typechecks");
    let options = rumoca_phase_flatten::FlattenOptions {
        simplify_variable_names: false,
        materialize_structured_families: false,
    };
    rumoca_phase_flatten::flatten_typed(typed, options).expect("model flattens")
}

fn model_contains(model: &rumoca_ir_flat::Model, predicate: impl Fn(&Expression) -> bool) -> bool {
    model
        .equations
        .iter()
        .any(|equation| equation.residual.contains_subexpression(&predicate))
}

#[test]
fn user_sum_shadow_is_never_rewritten_as_the_predefined_reduction() {
    let source = r"
package ShadowReduction
  function sum
    input Real v[:];
    output Real y;
  algorithm
    y := 999.0;
  end sum;

  model Top
    Real v[2];
    Real y;
  equation
    v = {1.0, 2.0};
    y = sum(v);
  end Top;
end ShadowReduction;
";

    let model = flatten_source(source, "ShadowReduction.Top");
    assert!(
        model_contains(&model, |expression| matches!(
            expression,
            Expression::FunctionCall { name, .. }
                if name.as_str().ends_with("sum") && name.target_def_id().is_some()
        )),
        "the exact user-function call must survive flattening"
    );
    assert!(
        !model_contains(&model, |expression| matches!(
            expression,
            Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Sum,
                ..
            }
        )),
        "same-spelling user code must never become a builtin call"
    );
}

#[test]
fn predefined_sum_still_expands_a_known_nonempty_array() {
    let source = r"
model BuiltinReduction
  Real v[2];
  Real y;
equation
  v = {1.0, 2.0};
  y = sum(v);
end BuiltinReduction;
";

    let model = flatten_source(source, "BuiltinReduction");
    assert!(
        model_contains(&model, |expression| matches!(
            expression,
            Expression::Binary {
                op: rumoca_core::OpBinary::Add,
                ..
            }
        )),
        "the exact predefined reduction must still lower to addition"
    );
    assert!(
        !model_contains(&model, |expression| matches!(
            expression,
            Expression::FunctionCall { name, .. } if name.as_str().ends_with("sum")
        )),
        "the predefined operator must not become a user call"
    );
}

#[test]
fn leading_dot_predefined_reductions_use_the_same_resolved_identity() {
    let source = r"
model GlobalReduction
  Real v[2];
  Real summed;
  Real multiplied;
equation
  v = {2.0, 3.0};
  summed = .sum(v);
  multiplied = .product(v);
end GlobalReduction;
";

    let model = flatten_source(source, "GlobalReduction");
    assert!(
        model_contains(&model, |expression| matches!(
            expression,
            Expression::Binary {
                op: rumoca_core::OpBinary::Add,
                ..
            }
        )),
        "leading-dot sum must lower by its predefined DefId"
    );
    assert!(
        model_contains(&model, |expression| matches!(
            expression,
            Expression::Binary {
                op: rumoca_core::OpBinary::Mul,
                ..
            }
        )),
        "leading-dot product must lower by its predefined DefId"
    );
    assert!(
        !model_contains(&model, |expression| matches!(
            expression,
            Expression::FunctionCall { name, .. }
                if name.as_str().ends_with("sum") || name.as_str().ends_with("product")
        )),
        "predefined global reductions must not survive as unresolved user calls"
    );
}
