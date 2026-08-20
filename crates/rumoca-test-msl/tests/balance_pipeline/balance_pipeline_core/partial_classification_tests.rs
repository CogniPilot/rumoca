use super::*;
use std::sync::Arc;

fn class(name: &str, partial: bool) -> rumoca_ir_ast::ClassDef {
    rumoca_ir_ast::ClassDef {
        name: rumoca_compile::compile::core::Token {
            text: Arc::from(name),
            ..rumoca_compile::compile::core::Token::default()
        },
        partial,
        ..rumoca_ir_ast::ClassDef::default()
    }
}

fn classification_tree() -> rumoca_ir_ast::ClassTree {
    let mut root = class("Root", false);
    root.classes
        .insert("Complete".to_string(), class("Complete", false));
    root.classes
        .insert("Partial".to_string(), class("Partial", true));
    let mut tree = rumoca_ir_ast::ClassTree::new();
    tree.definitions.classes.insert("Root".to_string(), root);
    tree
}

#[test]
fn source_partial_classification_uses_exact_qualified_declarations() {
    let classification = source_partial_classification(
        &classification_tree(),
        &["Root.Partial".to_string(), "Root.Complete".to_string()],
    )
    .expect("both selected classes exist");

    assert_eq!(
        classification.into_iter().collect::<Vec<_>>(),
        vec![
            ("Root.Complete".to_string(), false),
            ("Root.Partial".to_string(), true),
        ]
    );
}

#[test]
fn source_partial_classification_fails_closed_for_missing_class() {
    let error =
        source_partial_classification(&classification_tree(), &["Root.Missing".to_string()])
            .expect_err("a selected target without a declaration cannot be classified");

    assert!(error.contains("Root.Missing"), "{error}");
}

#[test]
fn failed_partial_result_remains_visible_to_summary() {
    let classification = BTreeMap::from([
        ("Root.Complete".to_string(), false),
        ("Root.Partial".to_string(), true),
    ]);
    let mut complete = phase_error_result("Root.Complete".to_string(), "Success", None, None);
    complete.is_partial = Some(false);
    complete.is_balanced = Some(true);
    complete.balance = Some(0);
    complete.initial_balance_ok = Some(true);
    let failed_partial = phase_error_result(
        "Root.Partial".to_string(),
        "ToDae",
        Some("typed refusal".to_string()),
        Some("ED019".to_string()),
    );
    let mut results = vec![complete, failed_partial];

    apply_source_partial_classification(&mut results, &classification)
        .expect("source classification should cover every result");
    let counters = summarize_msl_results(&results);
    let compiled_partial_models = results
        .iter()
        .filter(|result| result.phase_reached == "Success" && result.is_partial == Some(true))
        .count();
    let prior_balance_denominator = counters.compiled_models - compiled_partial_models;
    let source_static_balance_denominator = counters.balanced_models + counters.unbalanced_models;

    assert_eq!(results[1].is_partial, Some(true));
    assert_eq!(
        counters.partial_model_names,
        BTreeSet::from(["Root.Partial".to_string()])
    );
    assert_eq!(counters.compiled_models, 1);
    assert_eq!(prior_balance_denominator, 1);
    assert_eq!(source_static_balance_denominator, prior_balance_denominator);
}

#[test]
fn compiled_partial_disagreement_is_rejected() {
    let classification = BTreeMap::from([("Root.Partial".to_string(), true)]);
    let mut result = phase_error_result("Root.Partial".to_string(), "Success", None, None);
    result.is_partial = Some(false);

    let error = apply_source_partial_classification(&mut [result], &classification)
        .expect_err("compiled and source classifications must agree");

    assert!(error.contains("source=true, compiled=false"), "{error}");
}

#[test]
fn classification_rejects_duplicate_and_missing_results() {
    let classification = BTreeMap::from([
        ("Root.Complete".to_string(), false),
        ("Root.Partial".to_string(), true),
    ]);
    let complete = phase_error_result("Root.Complete".to_string(), "ToDae", None, None);
    let duplicate = complete.clone();
    let duplicate_error =
        apply_source_partial_classification(&mut [complete.clone(), duplicate], &classification)
            .expect_err("duplicate results cannot establish a cohort roster");
    assert!(duplicate_error.contains("duplicate"), "{duplicate_error}");

    let missing_error = apply_source_partial_classification(&mut [complete], &classification)
        .expect_err("every selected declaration must produce a result");
    assert!(missing_error.contains("Root.Partial"), "{missing_error}");
}
