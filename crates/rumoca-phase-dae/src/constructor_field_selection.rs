pub(crate) fn positional_constructor_arg_for_field<'a>(
    args: &'a [rumoca_core::Expression],
    field: &str,
) -> Option<&'a rumoca_core::Expression> {
    args.iter()
        .filter(|arg| !is_named_arg_marker(arg))
        .find(|arg| expression_leaf_name(arg) == Some(field))
}

fn is_named_arg_marker(arg: &rumoca_core::Expression) -> bool {
    matches!(
        arg,
        rumoca_core::Expression::FunctionCall {
            name,
            is_constructor: true,
            ..
        } if name
            .as_str()
            .starts_with(rumoca_core::NAMED_FUNCTION_ARG_PREFIX)
    )
}

fn expression_leaf_name(expr: &rumoca_core::Expression) -> Option<&str> {
    match expr {
        rumoca_core::Expression::VarRef { name, .. } => name
            .component_ref()
            .and_then(|component_ref| component_ref.parts.last())
            .map(|part| part.ident.as_str())
            .or_else(|| name.as_str().rsplit('.').next()),
        rumoca_core::Expression::Index { base, .. } => expression_leaf_name(base),
        rumoca_core::Expression::FieldAccess { field, .. } => Some(field.as_str()),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn component_ref_expr(parts: &[&str]) -> rumoca_core::Expression {
        rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::with_component_reference(
                parts.join("."),
                rumoca_core::ComponentReference {
                    local: false,
                    span: rumoca_core::Span::DUMMY,
                    parts: parts
                        .iter()
                        .map(|part| rumoca_core::ComponentRefPart {
                            ident: (*part).to_string(),
                            span: rumoca_core::Span::DUMMY,
                            subs: Vec::new(),
                        })
                        .collect(),
                    def_id: None,
                },
            ),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        }
    }

    #[test]
    fn selects_positional_constructor_arg_by_structured_leaf_name() {
        let args = vec![
            component_ref_expr(&["pCur1", "V_flow"]),
            component_ref_expr(&["pCur1", "dp"]),
        ];

        let Some(rumoca_core::Expression::VarRef { name, .. }) =
            positional_constructor_arg_for_field(&args, "V_flow")
        else {
            panic!("expected V_flow field proxy");
        };

        assert_eq!(name.as_str(), "pCur1.V_flow");
    }
}
