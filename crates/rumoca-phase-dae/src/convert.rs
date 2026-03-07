use indexmap::IndexMap;
use rumoca_ir_dae as dae;
use rumoca_ir_flat as flat;

pub(crate) fn flat_to_dae_var_name(name: &flat::VarName) -> dae::VarName {
    dae::VarName::new(name.as_str())
}

pub(crate) fn dae_to_flat_var_name(name: &dae::VarName) -> flat::VarName {
    flat::VarName::new(name.as_str())
}

pub(crate) fn flat_to_dae_literal(literal: &flat::Literal) -> dae::Literal {
    match literal {
        flat::Literal::Real(v) => dae::Literal::Real(*v),
        flat::Literal::Integer(v) => dae::Literal::Integer(*v),
        flat::Literal::Boolean(v) => dae::Literal::Boolean(*v),
        flat::Literal::String(v) => dae::Literal::String(v.clone()),
    }
}

pub(crate) fn dae_to_flat_literal(literal: &dae::Literal) -> flat::Literal {
    match literal {
        dae::Literal::Real(v) => flat::Literal::Real(*v),
        dae::Literal::Integer(v) => flat::Literal::Integer(*v),
        dae::Literal::Boolean(v) => flat::Literal::Boolean(*v),
        dae::Literal::String(v) => flat::Literal::String(v.clone()),
    }
}

pub(crate) fn flat_to_dae_builtin(function: flat::BuiltinFunction) -> dae::BuiltinFunction {
    dae::BuiltinFunction::from_name(function.name()).expect("flat/dae builtin sets must match")
}

pub(crate) fn dae_to_flat_builtin(function: dae::BuiltinFunction) -> flat::BuiltinFunction {
    flat::BuiltinFunction::from_name(function.name()).expect("flat/dae builtin sets must match")
}

pub(crate) fn flat_to_dae_subscript(subscript: &flat::Subscript) -> dae::Subscript {
    match subscript {
        flat::Subscript::Index(index) => dae::Subscript::Index(*index),
        flat::Subscript::Colon => dae::Subscript::Colon,
        flat::Subscript::Expr(expr) => dae::Subscript::Expr(Box::new(flat_to_dae_expression(expr))),
    }
}

pub(crate) fn dae_to_flat_subscript(subscript: &dae::Subscript) -> flat::Subscript {
    match subscript {
        dae::Subscript::Index(index) => flat::Subscript::Index(*index),
        dae::Subscript::Colon => flat::Subscript::Colon,
        dae::Subscript::Expr(expr) => flat::Subscript::Expr(Box::new(dae_to_flat_expression(expr))),
    }
}

pub(crate) fn flat_to_dae_expression(expr: &flat::Expression) -> dae::Expression {
    match expr {
        flat::Expression::Binary { op, lhs, rhs } => dae::Expression::Binary {
            op: op.clone(),
            lhs: Box::new(flat_to_dae_expression(lhs)),
            rhs: Box::new(flat_to_dae_expression(rhs)),
        },
        flat::Expression::Unary { op, rhs } => dae::Expression::Unary {
            op: op.clone(),
            rhs: Box::new(flat_to_dae_expression(rhs)),
        },
        flat::Expression::VarRef { name, subscripts } => dae::Expression::VarRef {
            name: flat_to_dae_var_name(name),
            subscripts: subscripts.iter().map(flat_to_dae_subscript).collect(),
        },
        flat::Expression::BuiltinCall { function, args } => dae::Expression::BuiltinCall {
            function: flat_to_dae_builtin(*function),
            args: args.iter().map(flat_to_dae_expression).collect(),
        },
        flat::Expression::FunctionCall {
            name,
            args,
            is_constructor,
        } => dae::Expression::FunctionCall {
            name: flat_to_dae_var_name(name),
            args: args.iter().map(flat_to_dae_expression).collect(),
            is_constructor: *is_constructor,
        },
        flat::Expression::Literal(literal) => {
            dae::Expression::Literal(flat_to_dae_literal(literal))
        }
        flat::Expression::If {
            branches,
            else_branch,
        } => dae::Expression::If {
            branches: branches
                .iter()
                .map(|(condition, value)| {
                    (
                        flat_to_dae_expression(condition),
                        flat_to_dae_expression(value),
                    )
                })
                .collect(),
            else_branch: Box::new(flat_to_dae_expression(else_branch)),
        },
        flat::Expression::Array {
            elements,
            is_matrix,
        } => dae::Expression::Array {
            elements: elements.iter().map(flat_to_dae_expression).collect(),
            is_matrix: *is_matrix,
        },
        flat::Expression::Tuple { elements } => dae::Expression::Tuple {
            elements: elements.iter().map(flat_to_dae_expression).collect(),
        },
        flat::Expression::Range { start, step, end } => dae::Expression::Range {
            start: Box::new(flat_to_dae_expression(start)),
            step: step
                .as_ref()
                .map(|step_expr| Box::new(flat_to_dae_expression(step_expr))),
            end: Box::new(flat_to_dae_expression(end)),
        },
        flat::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => dae::Expression::ArrayComprehension {
            expr: Box::new(flat_to_dae_expression(expr)),
            indices: indices
                .iter()
                .map(|index| dae::ComprehensionIndex {
                    name: index.name.clone(),
                    range: flat_to_dae_expression(&index.range),
                })
                .collect(),
            filter: filter
                .as_ref()
                .map(|value| Box::new(flat_to_dae_expression(value))),
        },
        flat::Expression::Index { base, subscripts } => dae::Expression::Index {
            base: Box::new(flat_to_dae_expression(base)),
            subscripts: subscripts.iter().map(flat_to_dae_subscript).collect(),
        },
        flat::Expression::FieldAccess { base, field } => dae::Expression::FieldAccess {
            base: Box::new(flat_to_dae_expression(base)),
            field: field.clone(),
        },
        flat::Expression::Empty => dae::Expression::Empty,
    }
}

pub(crate) fn dae_to_flat_expression(expr: &dae::Expression) -> flat::Expression {
    match expr {
        dae::Expression::Binary { op, lhs, rhs } => flat::Expression::Binary {
            op: op.clone(),
            lhs: Box::new(dae_to_flat_expression(lhs)),
            rhs: Box::new(dae_to_flat_expression(rhs)),
        },
        dae::Expression::Unary { op, rhs } => flat::Expression::Unary {
            op: op.clone(),
            rhs: Box::new(dae_to_flat_expression(rhs)),
        },
        dae::Expression::VarRef { name, subscripts } => flat::Expression::VarRef {
            name: dae_to_flat_var_name(name),
            subscripts: subscripts.iter().map(dae_to_flat_subscript).collect(),
        },
        dae::Expression::BuiltinCall { function, args } => flat::Expression::BuiltinCall {
            function: dae_to_flat_builtin(*function),
            args: args.iter().map(dae_to_flat_expression).collect(),
        },
        dae::Expression::FunctionCall {
            name,
            args,
            is_constructor,
        } => flat::Expression::FunctionCall {
            name: dae_to_flat_var_name(name),
            args: args.iter().map(dae_to_flat_expression).collect(),
            is_constructor: *is_constructor,
        },
        dae::Expression::Literal(literal) => {
            flat::Expression::Literal(dae_to_flat_literal(literal))
        }
        dae::Expression::If {
            branches,
            else_branch,
        } => flat::Expression::If {
            branches: branches
                .iter()
                .map(|(condition, value)| {
                    (
                        dae_to_flat_expression(condition),
                        dae_to_flat_expression(value),
                    )
                })
                .collect(),
            else_branch: Box::new(dae_to_flat_expression(else_branch)),
        },
        dae::Expression::Array {
            elements,
            is_matrix,
        } => flat::Expression::Array {
            elements: elements.iter().map(dae_to_flat_expression).collect(),
            is_matrix: *is_matrix,
        },
        dae::Expression::Tuple { elements } => flat::Expression::Tuple {
            elements: elements.iter().map(dae_to_flat_expression).collect(),
        },
        dae::Expression::Range { start, step, end } => flat::Expression::Range {
            start: Box::new(dae_to_flat_expression(start)),
            step: step
                .as_ref()
                .map(|step_expr| Box::new(dae_to_flat_expression(step_expr))),
            end: Box::new(dae_to_flat_expression(end)),
        },
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => flat::Expression::ArrayComprehension {
            expr: Box::new(dae_to_flat_expression(expr)),
            indices: indices
                .iter()
                .map(|index| flat::ComprehensionIndex {
                    name: index.name.clone(),
                    range: dae_to_flat_expression(&index.range),
                })
                .collect(),
            filter: filter
                .as_ref()
                .map(|value| Box::new(dae_to_flat_expression(value))),
        },
        dae::Expression::Index { base, subscripts } => flat::Expression::Index {
            base: Box::new(dae_to_flat_expression(base)),
            subscripts: subscripts.iter().map(dae_to_flat_subscript).collect(),
        },
        dae::Expression::FieldAccess { base, field } => flat::Expression::FieldAccess {
            base: Box::new(dae_to_flat_expression(base)),
            field: field.clone(),
        },
        dae::Expression::Empty => flat::Expression::Empty,
    }
}

pub(crate) fn flat_to_dae_component_ref_part(
    part: &flat::ComponentRefPart,
) -> dae::ComponentRefPart {
    dae::ComponentRefPart {
        ident: part.ident.clone(),
        subs: part.subs.iter().map(flat_to_dae_subscript).collect(),
    }
}

pub(crate) fn flat_to_dae_component_reference(
    comp: &flat::ComponentReference,
) -> dae::ComponentReference {
    dae::ComponentReference {
        local: comp.local,
        parts: comp
            .parts
            .iter()
            .map(flat_to_dae_component_ref_part)
            .collect(),
        def_id: comp.def_id,
    }
}

pub(crate) fn flat_to_dae_for_index(index: &flat::ForIndex) -> dae::ForIndex {
    dae::ForIndex {
        ident: index.ident.clone(),
        range: flat_to_dae_expression(&index.range),
    }
}

pub(crate) fn flat_to_dae_statement_block(block: &flat::StatementBlock) -> dae::StatementBlock {
    dae::StatementBlock {
        cond: flat_to_dae_expression(&block.cond),
        stmts: block.stmts.iter().map(flat_to_dae_statement).collect(),
    }
}

pub(crate) fn flat_to_dae_statement(statement: &flat::Statement) -> dae::Statement {
    match statement {
        flat::Statement::Empty => dae::Statement::Empty,
        flat::Statement::Assignment { comp, value } => dae::Statement::Assignment {
            comp: flat_to_dae_component_reference(comp),
            value: flat_to_dae_expression(value),
        },
        flat::Statement::Return => dae::Statement::Return,
        flat::Statement::Break => dae::Statement::Break,
        flat::Statement::For { indices, equations } => dae::Statement::For {
            indices: indices.iter().map(flat_to_dae_for_index).collect(),
            equations: equations.iter().map(flat_to_dae_statement).collect(),
        },
        flat::Statement::While(block) => dae::Statement::While(flat_to_dae_statement_block(block)),
        flat::Statement::If {
            cond_blocks,
            else_block,
        } => dae::Statement::If {
            cond_blocks: cond_blocks
                .iter()
                .map(flat_to_dae_statement_block)
                .collect(),
            else_block: else_block
                .as_ref()
                .map(|statements| statements.iter().map(flat_to_dae_statement).collect()),
        },
        flat::Statement::When(blocks) => {
            dae::Statement::When(blocks.iter().map(flat_to_dae_statement_block).collect())
        }
        flat::Statement::FunctionCall {
            comp,
            args,
            outputs,
        } => dae::Statement::FunctionCall {
            comp: flat_to_dae_component_reference(comp),
            args: args.iter().map(flat_to_dae_expression).collect(),
            outputs: outputs.iter().map(flat_to_dae_expression).collect(),
        },
        flat::Statement::Reinit { variable, value } => dae::Statement::Reinit {
            variable: flat_to_dae_component_reference(variable),
            value: flat_to_dae_expression(value),
        },
        flat::Statement::Assert {
            condition,
            message,
            level,
        } => dae::Statement::Assert {
            condition: flat_to_dae_expression(condition),
            message: flat_to_dae_expression(message),
            level: level.as_ref().map(flat_to_dae_expression),
        },
    }
}

pub(crate) fn flat_to_dae_external_function(
    value: &flat::ExternalFunction,
) -> dae::ExternalFunction {
    dae::ExternalFunction {
        language: value.language.clone(),
        function_name: value.function_name.clone(),
        output_name: value.output_name.clone(),
        arg_names: value.arg_names.clone(),
    }
}

pub(crate) fn flat_to_dae_derivative_annotation(
    value: &flat::DerivativeAnnotation,
) -> dae::DerivativeAnnotation {
    dae::DerivativeAnnotation {
        derivative_function: value.derivative_function.clone(),
        order: value.order,
        zero_derivative: value.zero_derivative.clone(),
        no_derivative: value.no_derivative.clone(),
    }
}

pub(crate) fn flat_to_dae_function_param(value: &flat::FunctionParam) -> dae::FunctionParam {
    dae::FunctionParam {
        name: value.name.clone(),
        type_name: value.type_name.clone(),
        dims: value.dims.clone(),
        default: value.default.as_ref().map(flat_to_dae_expression),
        description: value.description.clone(),
    }
}

pub(crate) fn flat_to_dae_function(value: &flat::Function) -> dae::Function {
    dae::Function {
        name: flat_to_dae_var_name(&value.name),
        inputs: value
            .inputs
            .iter()
            .map(flat_to_dae_function_param)
            .collect(),
        outputs: value
            .outputs
            .iter()
            .map(flat_to_dae_function_param)
            .collect(),
        locals: value
            .locals
            .iter()
            .map(flat_to_dae_function_param)
            .collect(),
        body: value.body.iter().map(flat_to_dae_statement).collect(),
        pure: value.pure,
        external: value.external.as_ref().map(flat_to_dae_external_function),
        derivatives: value
            .derivatives
            .iter()
            .map(flat_to_dae_derivative_annotation)
            .collect(),
        span: value.span,
    }
}

pub(crate) fn flat_to_dae_function_map(
    functions: &IndexMap<flat::VarName, flat::Function>,
) -> IndexMap<dae::VarName, dae::Function> {
    let mut out = IndexMap::with_capacity(functions.len());
    for (name, function) in functions {
        out.insert(flat_to_dae_var_name(name), flat_to_dae_function(function));
    }
    out
}
