use super::*;
use rumoca_core::{ComponentRefPart, ComponentReference, Reference, split_path_with_indices};

/// Build a flattened component reference from AST for test conversion.
pub(super) fn component_reference_from_ast_with_def_map(
    comp: &ast::ComponentReference,
    def_map: Option<&IndexMap<DefId, String>>,
) -> ComponentReference {
    if comp.parts.is_empty()
        && let Some(def_id) = comp.def_id
        && let Some(path) = def_map.and_then(|map| map.get(&def_id))
    {
        return ComponentReference {
            local: comp.local,
            span: comp.span,
            parts: split_path_with_indices(path)
                .into_iter()
                .map(|segment| ComponentRefPart {
                    ident: segment.to_string(),
                    span: comp.span,
                    subs: Vec::new(),
                })
                .collect(),
            def_id: Some(def_id),
        };
    }

    ComponentReference {
        local: comp.local,
        span: comp.span,
        parts: comp
            .parts
            .iter()
            .map(|part| ComponentRefPart {
                ident: part.ident.text.to_string(),
                span: comp.span,
                subs: part
                    .subs
                    .as_ref()
                    .map(|subs| subs.iter().map(subscript_from_ast).collect())
                    .unwrap_or_default(),
            })
            .collect(),
        def_id: comp.def_id,
    }
}

pub(super) fn expression_from_ast(expr: &ast::Expression) -> Expression {
    expression_from_ast_with_def_map(expr, None)
}

pub(super) fn expression_from_ast_with_def_map(
    expr: &ast::Expression,
    def_map: Option<&IndexMap<DefId, String>>,
) -> Expression {
    let span = expr.span();
    match expr {
        ast::Expression::Empty { .. } => Expression::Empty { span },
        ast::Expression::Binary { op, lhs, rhs, .. } => Expression::Binary {
            op: op.clone(),
            lhs: Box::new(expression_from_ast_with_def_map(lhs, def_map)),
            rhs: Box::new(expression_from_ast_with_def_map(rhs, def_map)),
            span,
        },
        ast::Expression::Unary { op, rhs, .. } => Expression::Unary {
            op: op.clone(),
            rhs: Box::new(expression_from_ast_with_def_map(rhs, def_map)),
            span,
        },
        ast::Expression::ComponentReference(cr) => {
            from_component_ref_with_def_map_impl(cr, def_map)
        }
        ast::Expression::FunctionCall { comp, args, .. } => {
            convert_function_call_with_def_map(comp, args, span, def_map)
        }
        ast::Expression::Terminal {
            terminal_type,
            token,
            ..
        } => Expression::Literal {
            value: convert_terminal(terminal_type, token),
            span,
        },
        ast::Expression::If {
            branches,
            else_branch,
            ..
        } => convert_if_with_def_map(branches, else_branch, span, def_map),
        ast::Expression::Array {
            elements,
            is_matrix,
            ..
        } => Expression::Array {
            elements: convert_expr_vec_with_def_map(elements, def_map),
            is_matrix: *is_matrix,
            span,
        },
        ast::Expression::Tuple { elements, .. } => Expression::Tuple {
            elements: convert_expr_vec_with_def_map(elements, def_map),
            span,
        },
        ast::Expression::Range {
            start, step, end, ..
        } => Expression::Range {
            start: Box::new(expression_from_ast_with_def_map(start, def_map)),
            step: step
                .as_ref()
                .map(|s| Box::new(expression_from_ast_with_def_map(s, def_map))),
            end: Box::new(expression_from_ast_with_def_map(end, def_map)),
            span,
        },
        ast::Expression::Parenthesized { inner, .. } => {
            expression_from_ast_with_def_map(inner, def_map)
        }
        ast::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => convert_array_comprehension_with_def_map(expr, indices, filter, span, def_map),
        ast::Expression::ClassModification {
            target,
            modifications,
            ..
        } => convert_class_modification_with_def_map(target, modifications, span, def_map),
        ast::Expression::NamedArgument { value, .. } => {
            expression_from_ast_with_def_map(value, def_map)
        }
        ast::Expression::Modification { value, .. } => {
            expression_from_ast_with_def_map(value, def_map)
        }
        ast::Expression::ArrayIndex {
            base, subscripts, ..
        } => Expression::Index {
            base: Box::new(expression_from_ast_with_def_map(base, def_map)),
            subscripts: subscripts.iter().map(subscript_from_ast).collect(),
            span,
        },
        ast::Expression::FieldAccess { base, field, .. } => Expression::FieldAccess {
            base: Box::new(expression_from_ast_with_def_map(base, def_map)),
            field: field.clone(),
            span,
        },
    }
}

/// Convert a component reference to a VarRef.
pub(super) fn expression_from_component_ref(cr: &ast::ComponentReference) -> Expression {
    Expression::VarRef {
        name: Reference::from_component_reference(component_reference_from_ast_with_def_map(
            cr, None,
        )),
        subscripts: Vec::new(),
        span: cr.span,
    }
}

/// Convert a function call, using def_map to resolve the fully qualified name if available.
#[cfg(test)]
pub(super) fn convert_function_call_with_def_map(
    comp: &ast::ComponentReference,
    args: &[ast::Expression],
    span: rumoca_core::Span,
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
                    .map(|a| expression_from_ast_with_def_map(a, def_map))
                    .collect(),
                span,
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
        name: Reference::from(func_name),
        args: args
            .iter()
            .map(|a| convert_call_arg_with_def_map(a, def_map))
            .collect(),
        is_constructor: false,
        span,
    }
}

/// Convert a terminal token to a literal.
#[cfg(test)]
pub(super) fn convert_terminal(
    terminal_type: &ast::TerminalType,
    token: &rumoca_core::Token,
) -> Literal {
    match terminal_type {
        ast::TerminalType::UnsignedReal => Literal::Real(token.text.parse().unwrap_or(0.0)),
        ast::TerminalType::UnsignedInteger => Literal::Integer(token.text.parse().unwrap_or(0)),
        ast::TerminalType::Bool => Literal::Boolean(token.text.eq_ignore_ascii_case("true")),
        ast::TerminalType::String => Literal::String(strip_quotes(&token.text)),
        ast::TerminalType::End | ast::TerminalType::Empty => Literal::Integer(0),
    }
}

fn strip_quotes(text: &str) -> String {
    if text.starts_with('"') && text.ends_with('"') && text.len() >= 2 {
        text[1..text.len() - 1].to_string()
    } else {
        text.to_string()
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
            range: expression_from_ast_with_def_map(&index.range, def_map),
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
        .map(|expr| expression_from_ast_with_def_map(expr, def_map))
        .collect()
}

#[cfg(test)]
pub(super) fn convert_if_with_def_map(
    branches: &[(ast::Expression, ast::Expression)],
    else_branch: &ast::Expression,
    span: rumoca_core::Span,
    def_map: Option<&IndexMap<DefId, String>>,
) -> Expression {
    Expression::If {
        branches: branches
            .iter()
            .map(|(cond, then_expr)| {
                (
                    expression_from_ast_with_def_map(cond, def_map),
                    expression_from_ast_with_def_map(then_expr, def_map),
                )
            })
            .collect(),
        else_branch: Box::new(expression_from_ast_with_def_map(else_branch, def_map)),
        span,
    }
}

#[cfg(test)]
pub(super) fn convert_array_comprehension_with_def_map(
    expr: &ast::Expression,
    indices: &[ast::ForIndex],
    filter: &Option<std::sync::Arc<ast::Expression>>,
    span: rumoca_core::Span,
    def_map: Option<&IndexMap<DefId, String>>,
) -> Expression {
    Expression::ArrayComprehension {
        expr: Box::new(expression_from_ast_with_def_map(expr, def_map)),
        indices: convert_comprehension_indices(indices, def_map),
        filter: filter
            .as_ref()
            .map(|cond| Box::new(expression_from_ast_with_def_map(cond, def_map))),
        span,
    }
}

#[cfg(test)]
const NAMED_CONSTRUCTOR_ARG_PREFIX: &str = "__rumoca_named_arg__.";

#[cfg(test)]
fn wrap_named_constructor_arg(name: &str, value: Expression) -> Expression {
    let span = value.span().unwrap_or(rumoca_core::Span::DUMMY);
    Expression::FunctionCall {
        name: Reference::from(format!("{NAMED_CONSTRUCTOR_ARG_PREFIX}{name}")),
        args: vec![value],
        // Mark as constructor-like so downstream function preflight does not
        // treat this internal marker as a normal function call.
        is_constructor: true,
        span,
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
        ast::Expression::NamedArgument { name, value, .. } => {
            wrap_named_constructor_arg(&name.text, expression_from_ast_with_def_map(value, def_map))
        }
        ast::Expression::Modification { target, value, .. } => {
            let arg_name = target
                .parts
                .iter()
                .map(|p| p.ident.text.to_string())
                .collect::<Vec<_>>()
                .join(".");
            wrap_named_constructor_arg(&arg_name, expression_from_ast_with_def_map(value, def_map))
        }
        _ => expression_from_ast_with_def_map(expr, def_map),
    }
}

#[cfg(test)]
pub(super) fn convert_class_modification_with_def_map(
    target: &ast::ComponentReference,
    modifications: &[ast::Expression],
    span: rumoca_core::Span,
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
        name: Reference::from(constructor_name),
        args: modifications
            .iter()
            .map(|expr| convert_constructor_arg_with_def_map(expr, def_map))
            .collect(),
        is_constructor: true,
        span,
    }
}
