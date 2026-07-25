//! Structured DAE algorithm → GALEC statement lowering.

use rumoca_core::{BuiltinFunction, Expression, Statement};
use rumoca_ir_dae::Algorithm;
use rumoca_ir_galec::ast::{self as gast, Condition, IfBranch, IfStatement, RefPart, Reference};

use crate::classify::Classification;
use crate::diagnostic::GalecTargetError;
use crate::lower::expr::ExprLowerer;
use crate::lower::methods;

pub(crate) fn lower_model_algorithms(
    algorithms: &[Algorithm],
    classification: &Classification<'_>,
    lowerer: &mut ExprLowerer<'_>,
) -> Result<Vec<gast::Spanned<gast::Statement>>, GalecTargetError> {
    let mut lowered = Vec::new();
    for algorithm in algorithms {
        for statement in &algorithm.statements {
            lower_runtime_statement(statement, classification, lowerer, &mut lowered)?;
        }
    }
    Ok(lowered)
}

fn lower_runtime_statement(
    statement: &Statement,
    classification: &Classification<'_>,
    lowerer: &mut ExprLowerer<'_>,
    lowered: &mut Vec<gast::Spanned<gast::Statement>>,
) -> Result<(), GalecTargetError> {
    match statement {
        Statement::When { blocks, .. } => {
            for block in blocks {
                if contains_sample(&block.cond) {
                    lower_statements(&block.stmts, classification, lowerer, lowered)?;
                }
            }
            Ok(())
        }
        other => {
            if let Some(statement) = lower_statement(other, classification, lowerer)? {
                lowered.push(statement);
            }
            Ok(())
        }
    }
}

fn lower_statements(
    statements: &[Statement],
    classification: &Classification<'_>,
    lowerer: &mut ExprLowerer<'_>,
    lowered: &mut Vec<gast::Spanned<gast::Statement>>,
) -> Result<(), GalecTargetError> {
    for statement in statements {
        if let Some(statement) = lower_statement(statement, classification, lowerer)? {
            lowered.push(statement);
        }
    }
    Ok(())
}

fn lower_statement(
    statement: &Statement,
    classification: &Classification<'_>,
    lowerer: &mut ExprLowerer<'_>,
) -> Result<Option<gast::Spanned<gast::Statement>>, GalecTargetError> {
    match statement {
        Statement::Empty { .. } => Ok(None),
        Statement::Assignment { comp, value, span } => {
            let target_name = comp.to_var_name();
            let (classified, subscripts) =
                super::resolve_target(classification, target_name.as_str())?;
            let value = methods::coerce_to(
                lowerer.lower(value)?,
                classified.scalar_type,
                target_name.as_str(),
            )?;
            Ok(Some(gast::Spanned::new(
                gast::Statement::Assignment {
                    target: Reference::State(vec![RefPart {
                        name: classified.galec_name.clone(),
                        subscripts,
                        span: rumoca_core::Span::DUMMY,
                    }]),
                    value,
                },
                *span,
            )))
        }
        Statement::If {
            cond_blocks,
            else_block,
            span,
        } => {
            let mut branches = Vec::with_capacity(cond_blocks.len());
            for block in cond_blocks {
                let condition = lowerer.lower_as_boolean(&block.cond, "if-statement condition")?;
                let mut body = Vec::new();
                lower_statements(&block.stmts, classification, lowerer, &mut body)?;
                let branch_span = body
                    .first()
                    .map_or(rumoca_core::Span::DUMMY, |statement| statement.span);
                branches.push(IfBranch {
                    condition: Condition::Expression(condition),
                    body,
                    span: branch_span,
                });
            }
            let else_body = else_block
                .as_ref()
                .map(|statements| {
                    let mut body = Vec::new();
                    lower_statements(statements, classification, lowerer, &mut body)?;
                    Ok::<_, GalecTargetError>(body)
                })
                .transpose()?;
            Ok(Some(gast::Spanned::new(
                gast::Statement::If(IfStatement {
                    branches,
                    else_body,
                }),
                *span,
            )))
        }
        other => Err(GalecTargetError::UnsupportedFeature {
            feature: "structured-algorithm-statement".to_owned(),
            detail: format!("structured algorithm statement `{}`", statement_kind(other)),
            span: other.source_span(),
        }),
    }
}

fn contains_sample(expression: &Expression) -> bool {
    match expression {
        Expression::BuiltinCall {
            function: BuiltinFunction::Sample,
            ..
        } => true,
        Expression::Unary { rhs, .. } => contains_sample(rhs),
        Expression::Binary { lhs, rhs, .. } => contains_sample(lhs) || contains_sample(rhs),
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            branches
                .iter()
                .any(|(condition, value)| contains_sample(condition) || contains_sample(value))
                || contains_sample(else_branch)
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements, .. } => {
            elements.iter().any(contains_sample)
        }
        Expression::BuiltinCall { args, .. } | Expression::FunctionCall { args, .. } => {
            args.iter().any(contains_sample)
        }
        _ => false,
    }
}

fn statement_kind(statement: &Statement) -> &'static str {
    match statement {
        Statement::Empty { .. } => "empty",
        Statement::Assignment { .. } => "assignment",
        Statement::Return { .. } => "return",
        Statement::Break { .. } => "break",
        Statement::For { .. } => "for",
        Statement::While { .. } => "while",
        Statement::If { .. } => "if",
        Statement::When { .. } => "when",
        Statement::FunctionCall { .. } => "function call",
        Statement::Reinit { .. } => "reinit",
        Statement::Assert { .. } => "assert",
    }
}
