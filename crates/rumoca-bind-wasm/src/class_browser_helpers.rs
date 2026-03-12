use rumoca_session::parsing::{
    ClassType, ComponentReference, Expression, OpBinary, TerminalType, Token,
};

pub(crate) fn class_type_label(class_type: &ClassType) -> String {
    class_type.as_str().to_string()
}

pub(crate) fn token_list_to_text(tokens: &[Token]) -> Option<String> {
    let text = tokens
        .iter()
        .map(|tok| tok.text.as_ref())
        .collect::<Vec<_>>()
        .join("");
    let trimmed = text.trim();
    if trimmed.is_empty() {
        None
    } else {
        Some(trimmed.to_string())
    }
}

pub(crate) fn component_reference_to_path(comp: &ComponentReference) -> String {
    comp.parts
        .iter()
        .map(|part| part.ident.text.as_ref())
        .collect::<Vec<_>>()
        .join(".")
}

pub(crate) fn expression_path(expr: &Expression) -> Option<String> {
    match expr {
        Expression::ComponentReference(comp) => Some(component_reference_to_path(comp)),
        Expression::FieldAccess { base, field } => {
            expression_path(base).map(|base_path| format!("{base_path}.{field}"))
        }
        Expression::Parenthesized { inner } => expression_path(inner),
        _ => None,
    }
}

pub(crate) fn extract_string_literal(expr: &Expression) -> Option<String> {
    match expr {
        Expression::Terminal {
            terminal_type: TerminalType::String,
            token,
        } => Some(token.text.to_string()),
        Expression::Parenthesized { inner } => extract_string_literal(inner),
        Expression::Binary {
            op: OpBinary::Add(_) | OpBinary::AddElem(_),
            lhs,
            rhs,
        } => {
            let lhs = extract_string_literal(lhs)?;
            let rhs = extract_string_literal(rhs)?;
            Some(format!("{lhs}{rhs}"))
        }
        _ => None,
    }
}

pub(crate) fn join_path(context: Option<&str>, tail: &str) -> String {
    match context {
        Some(prefix) if !prefix.is_empty() => format!("{prefix}.{tail}"),
        _ => tail.to_string(),
    }
}
