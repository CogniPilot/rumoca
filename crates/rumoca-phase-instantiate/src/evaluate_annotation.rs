use rumoca_ir_ast as ast;

/// The component's own `annotation(Evaluate = ...)` literal, if it writes one.
///
/// MLS §18.6: `Evaluate = true` asks for the parameter's value to be used
/// during symbolic processing; `Evaluate = false` forbids it. An explicit
/// `false` therefore outranks every default that would otherwise evaluate the
/// parameter, `final` and an enclosing `Evaluate = true` included.
pub(crate) fn evaluate_annotation(comp: &ast::Component) -> Option<bool> {
    comp.annotation.iter().find_map(evaluate_literal)
}

fn evaluate_literal(anno_expr: &ast::Expression) -> Option<bool> {
    let (name_text, value) = match anno_expr {
        ast::Expression::NamedArgument { name, value, .. } => (name.text.as_ref(), value.as_ref()),
        ast::Expression::Modification { target, value, .. } => {
            (target.parts.first()?.ident.text.as_ref(), value.as_ref())
        }
        _ => return None,
    };
    if name_text != "Evaluate" {
        return None;
    }
    match value {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::Bool,
            token,
            ..
        } => match token.text.as_ref() {
            "true" => Some(true),
            "false" => Some(false),
            _ => None,
        },
        _ => None,
    }
}
