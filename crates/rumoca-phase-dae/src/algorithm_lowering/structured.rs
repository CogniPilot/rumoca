use rumoca_core::{ComponentReference, Expression, Span, Statement, StatementBlock};
use rumoca_ir_flat::Model;

use super::{lower_assignment_statement, resolve_multi_output_selection_names};
use crate::{ToDaeError, flat_to_dae_expression_with_refs};

pub(super) fn structured_dae_algorithm(
    algorithm: &rumoca_ir_flat::Algorithm,
    flat: &Model,
) -> Result<rumoca_ir_dae::Algorithm, ToDaeError> {
    Ok(rumoca_ir_dae::Algorithm::new(
        structured_dae_statements(&algorithm.statements, flat)?,
        algorithm.span,
        &algorithm.origin,
    ))
}

fn structured_dae_statements(
    statements: &[Statement],
    flat: &Model,
) -> Result<Vec<Statement>, ToDaeError> {
    statements
        .iter()
        .try_fold(Vec::new(), |mut normalized, statement| {
            normalized.extend(structured_dae_statement(statement, flat)?);
            Ok(normalized)
        })
}

fn structured_dae_statement(
    statement: &Statement,
    flat: &Model,
) -> Result<Vec<Statement>, ToDaeError> {
    let expr = |value: &Expression| flat_to_dae_expression_with_refs(value, flat);
    let normalized = match statement {
        Statement::Empty { span } => Statement::Empty { span: *span },
        Statement::Assignment { comp, value, span } => {
            return structured_dae_assignment(comp, value, *span, flat);
        }
        Statement::Return { span } => Statement::Return { span: *span },
        Statement::Break { span } => Statement::Break { span: *span },
        Statement::For {
            indices,
            equations,
            span,
        } => Statement::For {
            indices: indices
                .iter()
                .map(|index| {
                    Ok(rumoca_core::ForIndex {
                        ident: index.ident.clone(),
                        range: expr(&index.range)?,
                    })
                })
                .collect::<Result<Vec<_>, ToDaeError>>()?,
            equations: structured_dae_statements(equations, flat)?,
            span: *span,
        },
        Statement::While { block, span } => Statement::While {
            block: structured_dae_statement_block(block, flat)?,
            span: *span,
        },
        Statement::If {
            cond_blocks,
            else_block,
            span,
        } => Statement::If {
            cond_blocks: cond_blocks
                .iter()
                .map(|block| structured_dae_statement_block(block, flat))
                .collect::<Result<Vec<_>, _>>()?,
            else_block: else_block
                .as_ref()
                .map(|statements| structured_dae_statements(statements, flat))
                .transpose()?,
            span: *span,
        },
        Statement::When { blocks, span } => Statement::When {
            blocks: blocks
                .iter()
                .map(|block| structured_dae_statement_block(block, flat))
                .collect::<Result<Vec<_>, _>>()?,
            span: *span,
        },
        Statement::FunctionCall {
            comp,
            args,
            outputs,
            span,
        } => return structured_dae_function_call(comp, args, outputs, *span, flat),
        Statement::Reinit {
            variable,
            value,
            span,
        } => Statement::Reinit {
            variable: variable.clone(),
            value: expr(value)?,
            span: *span,
        },
        Statement::Assert {
            condition,
            message,
            level,
            span,
        } => Statement::Assert {
            condition: expr(condition)?,
            message: Box::new(expr(message)?),
            level: level.as_deref().map(expr).transpose()?.map(Box::new),
            span: *span,
        },
    };
    Ok(vec![normalized])
}

fn structured_dae_assignment(
    comp: &ComponentReference,
    value: &Expression,
    span: Span,
    flat: &Model,
) -> Result<Vec<Statement>, ToDaeError> {
    lower_assignment_statement(flat, comp, value, span)
        .map_err(ToDaeError::internal)?
        .into_iter()
        .map(|(target, value, assignment_span, _)| {
            let target_reference = crate::convert::structured_target_reference_with_flat_metadata(
                &target,
                assignment_span,
                flat,
            )?;
            let comp = target_reference.component_ref().cloned().ok_or_else(|| {
                ToDaeError::internal(format!(
                    "structured algorithm target `{target}` lost its component reference"
                ))
            })?;
            Ok(Statement::Assignment {
                comp,
                value: flat_to_dae_expression_with_refs(&value, flat)?,
                span: assignment_span,
            })
        })
        .collect()
}

fn structured_dae_function_call(
    comp: &ComponentReference,
    args: &[Expression],
    outputs: &[ComponentReference],
    span: Span,
    flat: &Model,
) -> Result<Vec<Statement>, ToDaeError> {
    let normalized_args = args
        .iter()
        .map(|arg| flat_to_dae_expression_with_refs(arg, flat))
        .collect::<Result<Vec<_>, _>>()?;
    if outputs.is_empty() {
        return Ok(vec![Statement::FunctionCall {
            comp: comp.clone(),
            args: normalized_args,
            outputs: Vec::new(),
            span,
        }]);
    }
    let function_name = comp.to_var_name();
    let selection_names = if outputs.len() == 1 {
        vec![function_name]
    } else {
        resolve_multi_output_selection_names(flat, &function_name, outputs.len())
            .map_err(ToDaeError::internal)?
    };
    Ok(outputs
        .iter()
        .zip(selection_names)
        .map(|(output, selection_name)| Statement::Assignment {
            comp: output.clone(),
            value: Expression::FunctionCall {
                name: selection_name.into(),
                args: normalized_args.clone(),
                is_constructor: false,
                span,
            },
            span,
        })
        .collect())
}

fn structured_dae_statement_block(
    block: &StatementBlock,
    flat: &Model,
) -> Result<StatementBlock, ToDaeError> {
    Ok(StatementBlock {
        cond: flat_to_dae_expression_with_refs(&block.cond, flat)?,
        stmts: structured_dae_statements(&block.stmts, flat)?,
    })
}
