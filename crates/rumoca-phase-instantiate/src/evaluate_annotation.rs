use rumoca_ir_ast as ast;

/// Check if a component has annotation(Evaluate=true).
///
/// MLS §18.3: The Evaluate annotation indicates that a parameter should be
/// evaluated at compile time. This is used for structural parameters that
/// affect equation structure (e.g., if-equation branch selection).
///
/// Returns true if:
/// - The component has `annotation(Evaluate=true)`, or
/// - The component is declared `final` (implies compile-time evaluation)
pub(crate) fn has_evaluate_annotation(comp: &ast::Component) -> bool {
    if comp.is_final {
        return true;
    }

    comp.annotation.iter().any(is_evaluate_true_annotation)
}

fn is_evaluate_true_annotation(anno_expr: &ast::Expression) -> bool {
    let (name_text, value) = match anno_expr {
        ast::Expression::NamedArgument { name, value, .. } => (name.text.as_ref(), value.as_ref()),
        ast::Expression::Modification { target, value, .. } => {
            let Some(first_part) = target.parts.first() else {
                return false;
            };
            (first_part.ident.text.as_ref(), value.as_ref())
        }
        _ => return false,
    };

    if name_text != "Evaluate" {
        return false;
    }
    matches!(
        value,
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::Bool,
            token,
            ..
        } if token.text.as_ref() == "true"
    )
}
