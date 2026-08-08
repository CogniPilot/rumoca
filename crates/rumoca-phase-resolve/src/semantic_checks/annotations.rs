//! Resolve-time annotation semantic checks.
//!
//! These checks cover annotation semantics that depend on resolved declaration
//! context, such as `Evaluate` only being legal on parameter/constant components.

use super::*;

pub(super) const ER070_EVALUATE_SCOPE: &str = "ER070";
pub(super) const WR006_EVALUATE_WITHOUT_EFFECT: &str = "WR006";

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

    let in_function = class.class_type == ClassType::Function;
    for comp in class.components.values() {
        check_component_evaluate_annotations(comp, in_function, diags);
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

/// ANN-008 (MLS §18.6): `Evaluate` is a parameter/constant annotation.
///
/// Inside a `function` the MLS sentence that applies is "the annotation
/// Evaluate only has effect for a component declared with the prefix
/// parameter": a function has no parameter-variability locals at all, so the
/// annotation is defined to be without effect rather than illegal there. MSL
/// 4.1.0 relies on exactly that reading in
/// `Modelica.Electrical.Machines.SpacePhasors.Functions.ToSpacePhasor`, whose
/// protected `Integer m = size(x, 1)` carries `annotation(Evaluate=true)`.
/// Function-local declarations therefore warn (WR006) and drop the annotation;
/// every other class kind keeps the hard ER070 rejection, because there the
/// modeler can express the intent with a `parameter`/`constant` prefix.
fn check_component_evaluate_annotations(
    comp: &ast::Component,
    in_function: bool,
    diags: &mut Vec<Diagnostic>,
) {
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
        if in_function {
            let label = label_from_expression(
                expr,
                "check_annotation_restrictions/function_local_evaluate",
                format!("Evaluate has no effect on function local '{}'", comp.name),
            )
            .expect("annotation expression must carry a span");
            diags.push(Diagnostic::warning(
                WR006_EVALUATE_WITHOUT_EFFECT,
                format!(
                    "annotation Evaluate has no effect on function local '{}': only components declared with the parameter prefix are evaluated (MLS §18.6)",
                    comp.name
                ),
                label,
            ));
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
