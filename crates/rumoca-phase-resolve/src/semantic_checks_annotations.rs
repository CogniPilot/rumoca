//! Resolve-time annotation semantic checks.
//!
//! These checks cover annotation semantics that depend on resolved declaration
//! context, such as `Evaluate` only being legal on parameter/constant components.

use super::*;

pub(super) const ER070_EVALUATE_SCOPE: &str = "ER070";

pub(super) fn check_annotation_restrictions(class: &ClassDef, diags: &mut Vec<Diagnostic>) {
    check_non_component_evaluate_annotations(
        &class.annotation,
        "class",
        class.name.text.as_ref(),
        diags,
    );

    for ext in &class.extends {
        check_non_component_evaluate_annotations(
            &ext.annotation,
            "extends clause",
            &ext.base_name.to_string(),
            diags,
        );
    }

    for comp in class.components.values() {
        check_component_evaluate_annotations(comp, diags);
    }
}

fn check_non_component_evaluate_annotations(
    annotations: &[Expression],
    owner_kind: &str,
    owner_name: &str,
    diags: &mut Vec<Diagnostic>,
) {
    for expr in annotations {
        if !is_evaluate_annotation(expr) {
            continue;
        }
        let label = label_from_expression(
            expr,
            "check_annotation_restrictions/non_component_evaluate",
            format!("Evaluate is not allowed on {} '{}'", owner_kind, owner_name),
        )
        .expect("annotation expression must carry a span");
        diags.push(semantic_error(
            ER070_EVALUATE_SCOPE,
            "annotation Evaluate is only allowed on parameter or constant components (MLS §18.6)",
            label,
        ));
    }
}

fn check_component_evaluate_annotations(comp: &ast::Component, diags: &mut Vec<Diagnostic>) {
    if matches!(
        comp.variability,
        Variability::Parameter(_) | Variability::Constant(_)
    ) {
        return;
    }

    for expr in &comp.annotation {
        if !is_evaluate_annotation(expr) {
            continue;
        }
        let label = label_from_expression(
            expr,
            "check_annotation_restrictions/component_evaluate",
            format!("Evaluate is not allowed on component '{}'", comp.name),
        )
        .expect("annotation expression must carry a span");
        diags.push(semantic_error(
            ER070_EVALUATE_SCOPE,
            format!(
                "annotation Evaluate is only allowed on parameter or constant components; '{}' is not parameter or constant (MLS §18.6)",
                comp.name
            ),
            label,
        ));
    }
}

fn is_evaluate_annotation(expr: &Expression) -> bool {
    match expr {
        Expression::NamedArgument { name, .. } => name.text.as_ref() == "Evaluate",
        Expression::Modification { target, .. } => target
            .parts
            .first()
            .is_some_and(|part| part.ident.text.as_ref() == "Evaluate"),
        _ => false,
    }
}
