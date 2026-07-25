//! Structured DAE algorithm → GALEC statement lowering.

use rumoca_core::{
    BuiltinFunction, Expression, ExpressionRewriter, Literal, Span, Statement, VarName,
};
use rumoca_ir_dae::Algorithm;
use rumoca_ir_galec::ast::{self as gast, Condition, IfBranch, IfStatement, RefPart, Reference};

use crate::classify::Classification;
use crate::diagnostic::GalecTargetError;
use crate::lower::expr::ExprLowerer;
use crate::lower::methods;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct DerivedUpdateKey {
    target: VarName,
    span: Span,
}

impl DerivedUpdateKey {
    pub(crate) fn matches(&self, equation: &rumoca_ir_dae::Equation) -> bool {
        equation
            .lhs
            .as_ref()
            .is_some_and(|lhs| lhs.var_name() == &self.target && equation.span == self.span)
    }
}

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

pub(crate) fn derived_update_keys(algorithms: &[Algorithm]) -> Vec<DerivedUpdateKey> {
    let mut keys = Vec::new();
    for algorithm in algorithms {
        collect_statement_keys(&algorithm.statements, algorithm.span, &mut keys);
    }
    keys
}

fn collect_statement_keys(
    statements: &[Statement],
    fallback_span: Span,
    keys: &mut Vec<DerivedUpdateKey>,
) {
    for statement in statements {
        match statement {
            Statement::Assignment { comp, span, .. } => {
                let span = if span.is_dummy() {
                    fallback_span
                } else {
                    *span
                };
                keys.push(DerivedUpdateKey {
                    target: comp.to_var_name(),
                    span,
                });
            }
            Statement::When { blocks, span } => {
                let span = if span.is_dummy() {
                    fallback_span
                } else {
                    *span
                };
                for block in blocks {
                    collect_when_target_keys(&block.stmts, span, keys);
                }
            }
            Statement::If {
                cond_blocks,
                else_block,
                ..
            } => {
                for block in cond_blocks {
                    collect_statement_keys(&block.stmts, fallback_span, keys);
                }
                if let Some(statements) = else_block {
                    collect_statement_keys(statements, fallback_span, keys);
                }
            }
            _ => {}
        }
    }
}

fn collect_when_target_keys(
    statements: &[Statement],
    when_span: Span,
    keys: &mut Vec<DerivedUpdateKey>,
) {
    for statement in statements {
        match statement {
            Statement::Assignment { comp, .. } => keys.push(DerivedUpdateKey {
                target: comp.to_var_name(),
                span: when_span,
            }),
            Statement::If {
                cond_blocks,
                else_block,
                ..
            } => {
                for block in cond_blocks {
                    collect_when_target_keys(&block.stmts, when_span, keys);
                }
                if let Some(statements) = else_block {
                    collect_when_target_keys(statements, when_span, keys);
                }
            }
            _ => {}
        }
    }
}

fn lower_runtime_statement(
    statement: &Statement,
    classification: &Classification<'_>,
    lowerer: &mut ExprLowerer<'_>,
    lowered: &mut Vec<gast::Spanned<gast::Statement>>,
) -> Result<(), GalecTargetError> {
    match statement {
        Statement::When { blocks, span } => {
            lower_sampled_when(blocks, *span, classification, lowerer, lowered)
        }
        other => {
            if let Some(statement) = lower_statement(other, classification, lowerer)? {
                lowered.push(statement);
            }
            Ok(())
        }
    }
}

fn lower_sampled_when(
    blocks: &[rumoca_core::StatementBlock],
    span: Span,
    classification: &Classification<'_>,
    lowerer: &mut ExprLowerer<'_>,
    lowered: &mut Vec<gast::Spanned<gast::Statement>>,
) -> Result<(), GalecTargetError> {
    let mut branches = Vec::new();
    for block in blocks {
        if !contains_sample(&block.cond) {
            continue;
        }
        let residual = SampleTickRewriter.rewrite_expression(&block.cond);
        let condition = lowerer.lower_as_boolean(&residual, "when-statement condition")?;
        let mut body = Vec::new();
        lower_statements(&block.stmts, classification, lowerer, &mut body)?;
        branches.push(IfBranch {
            condition: Condition::Expression(condition),
            body,
            span,
        });
    }
    if branches.len() == 1 && is_true_condition(&branches[0].condition) {
        lowered.append(&mut branches.remove(0).body);
    } else if !branches.is_empty() {
        lowered.push(gast::Spanned::new(
            gast::Statement::If(IfStatement {
                branches,
                else_body: None,
            }),
            span,
        ));
    }
    Ok(())
}

fn is_true_condition(condition: &Condition) -> bool {
    matches!(
        condition,
        Condition::Expression(gast::Expression::Bool(true))
    )
}

struct SampleTickRewriter;

impl ExpressionRewriter for SampleTickRewriter {
    fn rewrite_expression(&mut self, expression: &Expression) -> Expression {
        match expression {
            Expression::BuiltinCall {
                function: BuiltinFunction::Sample,
                span,
                ..
            } => Expression::Literal {
                value: Literal::Boolean(true),
                span: *span,
            },
            _ => self.walk_expression(expression),
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
