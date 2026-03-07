use super::*;

pub(super) fn function_component_ref_from_ast(
    comp: &ast::ComponentReference,
    def_map: Option<&IndexMap<DefId, String>>,
) -> ComponentReference {
    if let Some(def_id) = comp.def_id
        && let Some(path) = def_map.and_then(|map| map.get(&def_id))
    {
        return ComponentReference {
            local: comp.local,
            parts: path
                .split('.')
                .map(|segment| ComponentRefPart {
                    ident: segment.to_string(),
                    subs: Vec::new(),
                })
                .collect(),
            def_id: Some(def_id),
        };
    }

    ComponentReference::from_ast_with_def_map(comp, None)
}

/// Convert a function call to either a builtin call or user function call.
#[cfg(test)]
pub(super) fn convert_function_call(
    comp: &ast::ComponentReference,
    args: &[ast::Expression],
) -> Expression {
    convert_function_call_with_def_map(comp, args, None)
}

/// Convert a function call, using def_map to resolve the fully qualified name if available.
#[cfg(test)]
pub(super) fn convert_function_call_with_def_map(
    comp: &ast::ComponentReference,
    args: &[ast::Expression],
    def_map: Option<&IndexMap<DefId, String>>,
) -> Expression {
    // Check if this is a builtin function (single-part name)
    if comp.parts.len() == 1 {
        let func_name = &comp.parts[0].ident.text;
        if let Some(builtin) = BuiltinFunction::from_name(func_name) {
            return Expression::BuiltinCall {
                function: builtin,
                args: args
                    .iter()
                    .map(|a| Expression::from_ast_with_def_map(a, def_map))
                    .collect(),
            };
        }
    }

    // For user-defined functions, use the resolved DefId when available.
    // Resolve phase canonicalizes function references to their concrete target.
    let textual_name = comp
        .parts
        .iter()
        .map(|p| p.ident.text.clone())
        .collect::<Vec<_>>()
        .join(".");

    let func_name = comp
        .def_id
        .and_then(|def_id| def_map.and_then(|map| map.get(&def_id).cloned()))
        .unwrap_or(textual_name);

    Expression::FunctionCall {
        name: VarName::new(func_name),
        args: args
            .iter()
            .map(|a| convert_call_arg_with_def_map(a, def_map))
            .collect(),
        is_constructor: false,
    }
}

/// Convert a terminal token to a literal.
#[cfg(test)]
pub(super) fn convert_terminal(
    terminal_type: &ast::TerminalType,
    token: &rumoca_ir_core::Token,
) -> Literal {
    match terminal_type {
        ast::TerminalType::UnsignedReal => Literal::Real(token.text.parse().unwrap_or(0.0)),
        ast::TerminalType::UnsignedInteger => Literal::Integer(token.text.parse().unwrap_or(0)),
        ast::TerminalType::Bool => Literal::Boolean(token.text.eq_ignore_ascii_case("true")),
        ast::TerminalType::String => Literal::String(strip_quotes(&token.text)),
        ast::TerminalType::End | ast::TerminalType::Empty => Literal::Integer(0),
    }
}

#[cfg(test)]
pub(super) fn convert_comprehension_indices(
    indices: &[ast::ForIndex],
    def_map: Option<&IndexMap<DefId, String>>,
) -> Vec<ComprehensionIndex> {
    indices
        .iter()
        .map(|index| ComprehensionIndex {
            name: index.ident.text.to_string(),
            range: Expression::from_ast_with_def_map(&index.range, def_map),
        })
        .collect()
}

#[cfg(test)]
pub(super) fn convert_expr_vec_with_def_map(
    exprs: &[ast::Expression],
    def_map: Option<&IndexMap<DefId, String>>,
) -> Vec<Expression> {
    exprs
        .iter()
        .map(|expr| Expression::from_ast_with_def_map(expr, def_map))
        .collect()
}

#[cfg(test)]
pub(super) fn convert_if_with_def_map(
    branches: &[(ast::Expression, ast::Expression)],
    else_branch: &ast::Expression,
    def_map: Option<&IndexMap<DefId, String>>,
) -> Expression {
    Expression::If {
        branches: branches
            .iter()
            .map(|(cond, then_expr)| {
                (
                    Expression::from_ast_with_def_map(cond, def_map),
                    Expression::from_ast_with_def_map(then_expr, def_map),
                )
            })
            .collect(),
        else_branch: Box::new(Expression::from_ast_with_def_map(else_branch, def_map)),
    }
}

#[cfg(test)]
pub(super) fn convert_array_comprehension_with_def_map(
    expr: &ast::Expression,
    indices: &[ast::ForIndex],
    filter: &Option<std::sync::Arc<ast::Expression>>,
    def_map: Option<&IndexMap<DefId, String>>,
) -> Expression {
    Expression::ArrayComprehension {
        expr: Box::new(Expression::from_ast_with_def_map(expr, def_map)),
        indices: convert_comprehension_indices(indices, def_map),
        filter: filter
            .as_ref()
            .map(|cond| Box::new(Expression::from_ast_with_def_map(cond, def_map))),
    }
}

#[cfg(test)]
const NAMED_CONSTRUCTOR_ARG_PREFIX: &str = "__rumoca_named_arg__.";

#[cfg(test)]
fn wrap_named_constructor_arg(name: &str, value: Expression) -> Expression {
    Expression::FunctionCall {
        name: VarName::new(format!("{NAMED_CONSTRUCTOR_ARG_PREFIX}{name}")),
        args: vec![value],
        // Mark as constructor-like so downstream function preflight does not
        // treat this internal marker as a normal function call.
        is_constructor: true,
    }
}

#[cfg(test)]
fn convert_constructor_arg_with_def_map(
    expr: &ast::Expression,
    def_map: Option<&IndexMap<DefId, String>>,
) -> Expression {
    convert_call_arg_with_def_map(expr, def_map)
}

#[cfg(test)]
fn convert_call_arg_with_def_map(
    expr: &ast::Expression,
    def_map: Option<&IndexMap<DefId, String>>,
) -> Expression {
    match expr {
        ast::Expression::NamedArgument { name, value } => wrap_named_constructor_arg(
            &name.text,
            Expression::from_ast_with_def_map(value, def_map),
        ),
        ast::Expression::Modification { target, value } => {
            let arg_name = target
                .parts
                .iter()
                .map(|p| p.ident.text.to_string())
                .collect::<Vec<_>>()
                .join(".");
            wrap_named_constructor_arg(&arg_name, Expression::from_ast_with_def_map(value, def_map))
        }
        _ => Expression::from_ast_with_def_map(expr, def_map),
    }
}

#[cfg(test)]
pub(super) fn convert_constructor_arg(expr: &ast::Expression) -> Expression {
    convert_constructor_arg_with_def_map(expr, None)
}

#[cfg(test)]
pub(super) fn convert_class_modification_with_def_map(
    target: &ast::ComponentReference,
    modifications: &[ast::Expression],
    def_map: Option<&IndexMap<DefId, String>>,
) -> Expression {
    let textual_name = target
        .parts
        .iter()
        .map(|p| p.ident.text.clone())
        .collect::<Vec<_>>()
        .join(".");
    let constructor_name = target
        .def_id
        .and_then(|def_id| def_map.and_then(|map| map.get(&def_id).cloned()))
        .unwrap_or(textual_name);
    Expression::FunctionCall {
        name: VarName::new(constructor_name),
        args: modifications
            .iter()
            .map(|expr| convert_constructor_arg_with_def_map(expr, def_map))
            .collect(),
        is_constructor: true,
    }
}
