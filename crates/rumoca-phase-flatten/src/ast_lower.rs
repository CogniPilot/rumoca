use indexmap::IndexMap;
use rumoca_core::DefId;
use rumoca_ir_ast as ast;
use rumoca_ir_flat as flat;

pub(crate) fn expression_from_ast(expr: &ast::Expression) -> flat::Expression {
    expression_from_ast_with_def_map(expr, None)
}

pub(crate) fn expression_from_ast_with_def_map(
    expr: &ast::Expression,
    def_map: Option<&IndexMap<DefId, String>>,
) -> flat::Expression {
    match expr {
        ast::Expression::Empty => flat::Expression::Empty,

        ast::Expression::Binary { op, lhs, rhs } => flat::Expression::Binary {
            op: flat::op_binary_from_ast(op),
            lhs: Box::new(expression_from_ast_with_def_map(lhs, def_map)),
            rhs: Box::new(expression_from_ast_with_def_map(rhs, def_map)),
        },

        ast::Expression::Unary { op, rhs } => flat::Expression::Unary {
            op: flat::op_unary_from_ast(op),
            rhs: Box::new(expression_from_ast_with_def_map(rhs, def_map)),
        },

        ast::Expression::ComponentReference(cr) => {
            expression_from_component_ref_with_def_map(cr, def_map)
        }

        ast::Expression::FunctionCall { comp, args } => {
            convert_function_call_with_def_map(comp, args, def_map)
        }

        ast::Expression::Terminal {
            terminal_type,
            token,
        } => flat::Expression::Literal(convert_terminal(terminal_type, token)),

        ast::Expression::If {
            branches,
            else_branch,
        } => convert_if_with_def_map(branches, else_branch, def_map),

        ast::Expression::Array {
            elements,
            is_matrix,
        } => flat::Expression::Array {
            elements: convert_expr_vec_with_def_map(elements, def_map),
            is_matrix: *is_matrix,
        },

        ast::Expression::Tuple { elements } => flat::Expression::Tuple {
            elements: convert_expr_vec_with_def_map(elements, def_map),
        },

        ast::Expression::Range { start, step, end } => flat::Expression::Range {
            start: Box::new(expression_from_ast_with_def_map(start, def_map)),
            step: step
                .as_ref()
                .map(|s| Box::new(expression_from_ast_with_def_map(s, def_map))),
            end: Box::new(expression_from_ast_with_def_map(end, def_map)),
        },

        ast::Expression::Parenthesized { inner } => {
            expression_from_ast_with_def_map(inner, def_map)
        }

        ast::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => convert_array_comprehension_with_def_map(expr, indices, filter, def_map),

        ast::Expression::ClassModification {
            target,
            modifications,
        } => convert_class_modification_with_def_map(target, modifications, def_map),

        ast::Expression::NamedArgument { value, .. } => {
            expression_from_ast_with_def_map(value, def_map)
        }

        ast::Expression::Modification { value, .. } => {
            expression_from_ast_with_def_map(value, def_map)
        }

        ast::Expression::ArrayIndex { base, subscripts } => {
            let base_flat = Box::new(expression_from_ast_with_def_map(base, def_map));
            let flat_subs = subscripts.iter().map(subscript_from_ast).collect();
            flat::Expression::Index {
                base: base_flat,
                subscripts: flat_subs,
            }
        }

        ast::Expression::FieldAccess { base, field } => flat::Expression::FieldAccess {
            base: Box::new(expression_from_ast_with_def_map(base, def_map)),
            field: field.clone(),
        },
    }
}

pub(crate) fn statement_from_ast_with_def_map(
    stmt: &ast::Statement,
    def_map: Option<&IndexMap<DefId, String>>,
) -> flat::Statement {
    match stmt {
        ast::Statement::Empty => flat::Statement::Empty,
        ast::Statement::Assignment { comp, value } => flat::Statement::Assignment {
            comp: component_reference_from_ast_with_def_map(comp, def_map),
            value: expression_from_ast_with_def_map(value, def_map),
        },
        ast::Statement::Return { .. } => flat::Statement::Return,
        ast::Statement::Break { .. } => flat::Statement::Break,
        ast::Statement::For { indices, equations } => flat::Statement::For {
            indices: indices
                .iter()
                .map(|index| for_index_from_ast_with_def_map(index, def_map))
                .collect(),
            equations: equations
                .iter()
                .map(|inner| statement_from_ast_with_def_map(inner, def_map))
                .collect(),
        },
        ast::Statement::While(block) => {
            flat::Statement::While(statement_block_from_ast_with_def_map(block, def_map))
        }
        ast::Statement::If {
            cond_blocks,
            else_block,
        } => flat::Statement::If {
            cond_blocks: cond_blocks
                .iter()
                .map(|block| statement_block_from_ast_with_def_map(block, def_map))
                .collect(),
            else_block: else_block.as_ref().map(|stmts| {
                stmts
                    .iter()
                    .map(|inner| statement_from_ast_with_def_map(inner, def_map))
                    .collect()
            }),
        },
        ast::Statement::When(blocks) => flat::Statement::When(
            blocks
                .iter()
                .map(|block| statement_block_from_ast_with_def_map(block, def_map))
                .collect(),
        ),
        ast::Statement::FunctionCall {
            comp,
            args,
            outputs,
        } => flat::Statement::FunctionCall {
            comp: function_component_ref_from_ast(comp, def_map),
            args: args
                .iter()
                .map(|arg| expression_from_ast_with_def_map(arg, def_map))
                .collect(),
            outputs: outputs
                .iter()
                .map(|output| expression_from_ast_with_def_map(output, def_map))
                .collect(),
        },
        ast::Statement::Reinit { variable, value } => flat::Statement::Reinit {
            variable: component_reference_from_ast_with_def_map(variable, def_map),
            value: expression_from_ast_with_def_map(value, def_map),
        },
        ast::Statement::Assert {
            condition,
            message,
            level,
        } => flat::Statement::Assert {
            condition: expression_from_ast_with_def_map(condition, def_map),
            message: expression_from_ast_with_def_map(message, def_map),
            level: level
                .as_ref()
                .map(|expr| expression_from_ast_with_def_map(expr, def_map)),
        },
    }
}

fn for_index_from_ast_with_def_map(
    index: &ast::ForIndex,
    def_map: Option<&IndexMap<DefId, String>>,
) -> flat::ForIndex {
    flat::ForIndex {
        ident: index.ident.text.to_string(),
        range: expression_from_ast_with_def_map(&index.range, def_map),
    }
}

fn statement_block_from_ast_with_def_map(
    block: &ast::StatementBlock,
    def_map: Option<&IndexMap<DefId, String>>,
) -> flat::StatementBlock {
    flat::StatementBlock {
        cond: expression_from_ast_with_def_map(&block.cond, def_map),
        stmts: block
            .stmts
            .iter()
            .map(|stmt| statement_from_ast_with_def_map(stmt, def_map))
            .collect(),
    }
}

fn component_reference_from_ast_with_def_map(
    comp: &ast::ComponentReference,
    def_map: Option<&IndexMap<DefId, String>>,
) -> flat::ComponentReference {
    if comp.parts.is_empty()
        && let Some(def_id) = comp.def_id
        && let Some(path) = def_map.and_then(|map| map.get(&def_id))
    {
        return flat::ComponentReference {
            local: comp.local,
            parts: path
                .split('.')
                .map(|segment| flat::ComponentRefPart {
                    ident: segment.to_string(),
                    subs: Vec::new(),
                })
                .collect(),
            def_id: Some(def_id),
        };
    }

    flat::ComponentReference {
        local: comp.local,
        parts: comp
            .parts
            .iter()
            .map(|part| flat::ComponentRefPart {
                ident: part.ident.text.to_string(),
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

fn function_component_ref_from_ast(
    comp: &ast::ComponentReference,
    def_map: Option<&IndexMap<DefId, String>>,
) -> flat::ComponentReference {
    if let Some(def_id) = comp.def_id
        && let Some(path) = def_map.and_then(|map| map.get(&def_id))
    {
        return flat::ComponentReference {
            local: comp.local,
            parts: path
                .split('.')
                .map(|segment| flat::ComponentRefPart {
                    ident: segment.to_string(),
                    subs: Vec::new(),
                })
                .collect(),
            def_id: Some(def_id),
        };
    }

    component_reference_from_ast_with_def_map(comp, None)
}

fn subscript_from_ast(sub: &ast::Subscript) -> flat::Subscript {
    match sub {
        ast::Subscript::Expression(expr) => {
            if let Some(val) = try_constant_integer(expr) {
                return flat::Subscript::Index(val);
            }
            flat::Subscript::Expr(Box::new(expression_from_ast(expr)))
        }
        ast::Subscript::Range { .. } | ast::Subscript::Empty => flat::Subscript::Colon,
    }
}

fn expression_from_component_ref_with_def_map(
    cr: &ast::ComponentReference,
    def_map: Option<&IndexMap<DefId, String>>,
) -> flat::Expression {
    if cr.parts.is_empty()
        && let Some(def_id) = cr.def_id
        && let Some(path) = def_map.and_then(|map| map.get(&def_id))
    {
        return flat::Expression::VarRef {
            name: flat::VarName::new(path.clone()),
            subscripts: vec![],
        };
    }

    if cr.parts.iter().all(|part| part.subs.is_none())
        && let Some(def_id) = cr.def_id
        && let Some(path) = def_map.and_then(|map| map.get(&def_id))
        && is_enum_literal_ref(cr, path)
    {
        return flat::Expression::VarRef {
            name: flat::VarName::new(path.clone()),
            subscripts: vec![],
        };
    }

    if let Some(index_expr) = component_ref_with_dynamic_final_subscripts(cr) {
        return index_expr;
    }

    expression_from_component_ref(cr)
}

fn expression_from_component_ref(cr: &ast::ComponentReference) -> flat::Expression {
    let name_parts: Vec<String> = cr
        .parts
        .iter()
        .map(|p| {
            let base = p.ident.text.to_string();
            match &p.subs {
                Some(subs) if !subs.is_empty() => {
                    let sub_strs: Vec<String> = subs.iter().map(subscript_to_string).collect();
                    format!("{base}[{}]", sub_strs.join(","))
                }
                _ => base,
            }
        })
        .collect();
    let name = flat::VarName::new(name_parts.join("."));

    flat::Expression::VarRef {
        name,
        subscripts: vec![],
    }
}

fn component_ref_with_dynamic_final_subscripts(
    cr: &ast::ComponentReference,
) -> Option<flat::Expression> {
    let last = cr.parts.last()?;
    let last_subs_ast = last.subs.as_ref()?;
    if last_subs_ast.is_empty() {
        return None;
    }

    let last_subs: Vec<flat::Subscript> = last_subs_ast.iter().map(subscript_from_ast).collect();
    let has_dynamic = last_subs
        .iter()
        .any(|sub| !matches!(sub, flat::Subscript::Index(_)));
    if !has_dynamic {
        return None;
    }

    let mut name_parts: Vec<String> = Vec::with_capacity(cr.parts.len());
    for (idx, part) in cr.parts.iter().enumerate() {
        let base = part.ident.text.to_string();
        if idx + 1 == cr.parts.len() {
            name_parts.push(base);
            continue;
        }

        let seg = match &part.subs {
            Some(subs) if !subs.is_empty() => {
                let sub_text: Vec<String> = subs.iter().map(subscript_to_string).collect();
                format!("{base}[{}]", sub_text.join(","))
            }
            _ => base,
        };
        name_parts.push(seg);
    }

    Some(flat::Expression::Index {
        base: Box::new(flat::Expression::VarRef {
            name: flat::VarName::new(name_parts.join(".")),
            subscripts: vec![],
        }),
        subscripts: last_subs,
    })
}

fn is_enum_literal_ref(cr: &ast::ComponentReference, canonical_path: &str) -> bool {
    let Some(last_part) = cr.parts.last() else {
        return false;
    };

    let textual_literal = last_part.ident.text.as_ref();
    if !is_quoted_identifier(textual_literal) {
        return false;
    }

    canonical_path
        .rsplit('.')
        .next()
        .is_some_and(is_quoted_identifier)
}

fn is_quoted_identifier(name: &str) -> bool {
    name.starts_with('\'') && name.ends_with('\'') && name.len() >= 2
}

fn convert_function_call_with_def_map(
    comp: &ast::ComponentReference,
    args: &[ast::Expression],
    def_map: Option<&IndexMap<DefId, String>>,
) -> flat::Expression {
    if comp.parts.len() == 1 {
        let func_name = &comp.parts[0].ident.text;
        if let Some(builtin) = flat::BuiltinFunction::from_name(func_name) {
            return flat::Expression::BuiltinCall {
                function: builtin,
                args: args
                    .iter()
                    .map(|a| expression_from_ast_with_def_map(a, def_map))
                    .collect(),
            };
        }
    }

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

    flat::Expression::FunctionCall {
        name: flat::VarName::new(func_name),
        args: args
            .iter()
            .map(|a| convert_call_arg_with_def_map(a, def_map))
            .collect(),
        is_constructor: false,
    }
}

fn convert_terminal(
    terminal_type: &ast::TerminalType,
    token: &rumoca_ir_core::Token,
) -> flat::Literal {
    match terminal_type {
        ast::TerminalType::UnsignedReal => flat::Literal::Real(token.text.parse().unwrap_or(0.0)),
        ast::TerminalType::UnsignedInteger => {
            flat::Literal::Integer(token.text.parse().unwrap_or(0))
        }
        ast::TerminalType::Bool => flat::Literal::Boolean(token.text.eq_ignore_ascii_case("true")),
        ast::TerminalType::String => flat::Literal::String(strip_quotes(&token.text)),
        ast::TerminalType::End | ast::TerminalType::Empty => flat::Literal::Integer(0),
    }
}

fn convert_comprehension_indices(
    indices: &[ast::ForIndex],
    def_map: Option<&IndexMap<DefId, String>>,
) -> Vec<flat::ComprehensionIndex> {
    indices
        .iter()
        .map(|index| flat::ComprehensionIndex {
            name: index.ident.text.to_string(),
            range: expression_from_ast_with_def_map(&index.range, def_map),
        })
        .collect()
}

fn convert_expr_vec_with_def_map(
    exprs: &[ast::Expression],
    def_map: Option<&IndexMap<DefId, String>>,
) -> Vec<flat::Expression> {
    exprs
        .iter()
        .map(|expr| expression_from_ast_with_def_map(expr, def_map))
        .collect()
}

fn convert_if_with_def_map(
    branches: &[(ast::Expression, ast::Expression)],
    else_branch: &ast::Expression,
    def_map: Option<&IndexMap<DefId, String>>,
) -> flat::Expression {
    flat::Expression::If {
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
    }
}

fn convert_array_comprehension_with_def_map(
    expr: &ast::Expression,
    indices: &[ast::ForIndex],
    filter: &Option<std::sync::Arc<ast::Expression>>,
    def_map: Option<&IndexMap<DefId, String>>,
) -> flat::Expression {
    flat::Expression::ArrayComprehension {
        expr: Box::new(expression_from_ast_with_def_map(expr, def_map)),
        indices: convert_comprehension_indices(indices, def_map),
        filter: filter
            .as_ref()
            .map(|cond| Box::new(expression_from_ast_with_def_map(cond, def_map))),
    }
}

const NAMED_CONSTRUCTOR_ARG_PREFIX: &str = "__rumoca_named_arg__.";

fn wrap_named_constructor_arg(name: &str, value: flat::Expression) -> flat::Expression {
    flat::Expression::FunctionCall {
        name: flat::VarName::new(format!("{NAMED_CONSTRUCTOR_ARG_PREFIX}{name}")),
        args: vec![value],
        is_constructor: true,
    }
}

fn convert_constructor_arg_with_def_map(
    expr: &ast::Expression,
    def_map: Option<&IndexMap<DefId, String>>,
) -> flat::Expression {
    convert_call_arg_with_def_map(expr, def_map)
}

fn convert_call_arg_with_def_map(
    expr: &ast::Expression,
    def_map: Option<&IndexMap<DefId, String>>,
) -> flat::Expression {
    match expr {
        ast::Expression::NamedArgument { name, value } => {
            wrap_named_constructor_arg(&name.text, expression_from_ast_with_def_map(value, def_map))
        }
        ast::Expression::Modification { target, value } => {
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

fn convert_class_modification_with_def_map(
    target: &ast::ComponentReference,
    modifications: &[ast::Expression],
    def_map: Option<&IndexMap<DefId, String>>,
) -> flat::Expression {
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
    flat::Expression::FunctionCall {
        name: flat::VarName::new(constructor_name),
        args: modifications
            .iter()
            .map(|expr| convert_constructor_arg_with_def_map(expr, def_map))
            .collect(),
        is_constructor: true,
    }
}

fn subscript_to_string(sub: &ast::Subscript) -> String {
    match sub {
        ast::Subscript::Expression(expr) => {
            if let Some(val) = try_constant_integer(expr) {
                return val.to_string();
            }
            expression_to_string(expr)
        }
        ast::Subscript::Range { .. } | ast::Subscript::Empty => ":".to_string(),
    }
}

fn expression_to_string(expr: &ast::Expression) -> String {
    match expr {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedInteger,
            token,
        } => token.text.to_string(),
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedReal,
            token,
        } => token.text.to_string(),
        ast::Expression::ComponentReference(cr) => {
            let mut result = String::new();
            for (i, p) in cr.parts.iter().enumerate() {
                if i > 0 {
                    result.push('.');
                }
                result.push_str(&p.ident.text);
                if let Some(subs) = &p.subs
                    && !subs.is_empty()
                {
                    let sub_strs: Vec<String> = subs.iter().map(subscript_to_string).collect();
                    result.push('[');
                    result.push_str(&sub_strs.join(","));
                    result.push(']');
                }
            }
            result
        }
        ast::Expression::ArrayIndex { base, subscripts } => {
            let base_str = expression_to_string(base);
            let sub_strs: Vec<String> = subscripts.iter().map(subscript_to_string).collect();
            format!("{base_str}[{}]", sub_strs.join(","))
        }
        ast::Expression::FieldAccess { base, field } => {
            format!("{}.{}", expression_to_string(base), field)
        }
        ast::Expression::Binary { op, lhs, rhs } => {
            format!(
                "({} {} {})",
                expression_to_string(lhs),
                op,
                expression_to_string(rhs)
            )
        }
        ast::Expression::Unary { op, rhs } => {
            format!("({}{})", op, expression_to_string(rhs))
        }
        ast::Expression::FunctionCall { comp, args } => {
            let func_name: String = comp
                .parts
                .iter()
                .map(|p| p.ident.text.as_ref())
                .collect::<Vec<_>>()
                .join(".");
            let arg_strs: Vec<String> = args.iter().map(expression_to_string).collect();
            format!("{func_name}({})", arg_strs.join(", "))
        }
        ast::Expression::Range { start, step, end } => match step {
            Some(s) => format!(
                "{}:{}:{}",
                expression_to_string(start),
                expression_to_string(s),
                expression_to_string(end)
            ),
            None => format!(
                "{}:{}",
                expression_to_string(start),
                expression_to_string(end)
            ),
        },
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::End,
            ..
        } => "end".to_string(),
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::Bool,
            token,
        } => token.text.to_string(),
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::String,
            token,
        } => format!("\"{}\"", token.text),
        _ => "?".to_string(),
    }
}

fn try_constant_integer(expr: &ast::Expression) -> Option<i64> {
    match expr {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedInteger,
            token,
        } => token.text.parse().ok(),
        _ => None,
    }
}

fn strip_quotes(text: &str) -> String {
    if text.starts_with('"') && text.ends_with('"') && text.len() >= 2 {
        text[1..text.len() - 1].to_string()
    } else {
        text.to_string()
    }
}
