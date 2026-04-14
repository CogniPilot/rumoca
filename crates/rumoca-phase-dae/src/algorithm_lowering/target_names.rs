use super::*;

fn algorithm_target_subscript_name(subscript: &Subscript) -> Option<String> {
    match subscript {
        Subscript::Index(index) => Some(index.to_string()),
        Subscript::Colon => Some(":".to_string()),
        Subscript::Expr(expr) => match expr.as_ref() {
            Expression::Literal(Literal::Integer(value)) => Some(value.to_string()),
            _ => None,
        },
    }
}

pub(super) fn algorithm_assignment_target_name(comp: &ComponentReference) -> Option<VarName> {
    let rendered_parts = comp
        .parts
        .iter()
        .map(|part| {
            if part.subs.is_empty() {
                return Some(part.ident.clone());
            }
            let subscripts = part
                .subs
                .iter()
                .map(algorithm_target_subscript_name)
                .collect::<Option<Vec<_>>>()?;
            Some(format!("{}[{}]", part.ident, subscripts.join(",")))
        })
        .collect::<Option<Vec<_>>>()?;
    Some(VarName::new(rendered_parts.join(".")))
}

pub(super) fn algorithm_assignment_base_with_subscripts(
    comp: &ComponentReference,
) -> Option<(VarName, &[Subscript])> {
    let last = comp.parts.last()?;
    if last.subs.is_empty() {
        return None;
    }
    if comp.parts[..comp.parts.len().saturating_sub(1)]
        .iter()
        .any(|part| !part.subs.is_empty())
    {
        return None;
    }
    let mut parts = comp
        .parts
        .iter()
        .map(|part| part.ident.clone())
        .collect::<Vec<_>>();
    let last_ident = parts.pop()?;
    parts.push(last_ident);
    Some((VarName::new(parts.join(".")), &last.subs))
}

pub(super) fn varref_with_subscripts(name: &VarName, subscripts: &[Subscript]) -> VarName {
    if subscripts.is_empty() {
        return name.clone();
    }

    fn render_subscript(subscript: &Subscript) -> String {
        match subscript {
            Subscript::Index(index) => index.to_string(),
            Subscript::Colon => ":".to_string(),
            Subscript::Expr(expr) => format!("{expr:?}"),
        }
    }

    let rendered = subscripts
        .iter()
        .map(render_subscript)
        .collect::<Vec<_>>()
        .join(",");
    VarName::new(format!("{}[{rendered}]", name.as_str()))
}

pub(super) fn algorithm_output_target_name(output: &Expression) -> Option<VarName> {
    match output {
        Expression::VarRef { name, subscripts } => Some(varref_with_subscripts(name, subscripts)),
        Expression::FieldAccess { base, field } => {
            if let Expression::VarRef { name, subscripts } = base.as_ref() {
                let base = varref_with_subscripts(name, subscripts);
                Some(VarName::new(format!("{}.{}", base.as_str(), field)))
            } else {
                None
            }
        }
        _ => None,
    }
}
