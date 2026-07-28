//! Structural equality of the DAE bodies behind a structured family's rows.
//!
//! A tensor node lifts ONE kernel over an index domain, so every row of the
//! family must have the same expression SHAPE -- same operators, same operand
//! structure, same literal kinds -- differing only in the scalarized indices
//! the domain binders supply. This module reduces a DAE right-hand side to
//! that shape and compares it across rows; a mismatch is the
//! `MismatchedDaeBodyShape` decline, a property of the model rather than a
//! gap in the compiler.

use rumoca_ir_dae as dae;

use super::{
    StructuredProgram, stencil_contract_violation, stencil_vec_with_capacity,
    structured_domain_corner_ordinals,
};
use crate::lower::LowerError;

pub(super) fn structured_dae_body_shapes_match(
    rows: &[StructuredProgram],
    row_indices: &[usize],
    dae_equations: &[dae::Equation],
) -> Result<bool, LowerError> {
    let mut first = None;
    for row_index in row_indices {
        let Some(equation_index) = rows.get(*row_index).and_then(|row| row.dae_equation_index)
        else {
            return Ok(false);
        };
        let Some(equation) = dae_equations.get(equation_index) else {
            return Ok(false);
        };
        let signature = expression_body_shape(&equation.rhs, equation.span)?;
        let Some(first_signature) = &first else {
            first = Some(signature);
            continue;
        };
        if signature != *first_signature {
            return Ok(false);
        }
    }
    if first.is_none() {
        return Ok(false);
    }
    Ok(true)
}

#[derive(Debug, Clone, PartialEq)]
enum ExpressionBodyShape {
    Binary {
        op: rumoca_core::OpBinary,
        lhs: Box<ExpressionBodyShape>,
        rhs: Box<ExpressionBodyShape>,
    },
    Unary {
        op: rumoca_core::OpUnary,
        rhs: Box<ExpressionBodyShape>,
    },
    VarRef {
        name: String,
        subscripts: Vec<SubscriptBodyShape>,
    },
    BuiltinCall {
        function: String,
        args: Vec<ExpressionBodyShape>,
    },
    FunctionCall {
        name: String,
        args: Vec<ExpressionBodyShape>,
        is_constructor: bool,
    },
    Literal(LiteralBodyShape),
    If {
        branches: Vec<(ExpressionBodyShape, ExpressionBodyShape)>,
        else_branch: Box<ExpressionBodyShape>,
    },
    Array {
        elements: Vec<ExpressionBodyShape>,
        is_matrix: bool,
    },
    Tuple(Vec<ExpressionBodyShape>),
    Range {
        start: Box<ExpressionBodyShape>,
        step: Option<Box<ExpressionBodyShape>>,
        end: Box<ExpressionBodyShape>,
    },
    ArrayComprehension,
    Index {
        base: Box<ExpressionBodyShape>,
        subscripts: Vec<SubscriptBodyShape>,
    },
    FieldAccess {
        base: Box<ExpressionBodyShape>,
        field: String,
    },
    Empty,
}

#[derive(Debug, Clone, PartialEq)]
enum SubscriptBodyShape {
    ScalarizedIndex,
    Colon,
    Expr(ExpressionBodyShape),
}

#[derive(Debug, Clone, PartialEq)]
enum LiteralBodyShape {
    Real,
    Integer,
    Boolean,
    String,
}

fn expression_body_shape(
    expr: &rumoca_core::Expression,
    context_span: rumoca_core::Span,
) -> Result<ExpressionBodyShape, LowerError> {
    use rumoca_core::Expression;
    let span = expr.span().unwrap_or(context_span);
    match expr {
        Expression::Binary { op, lhs, rhs, .. } => Ok(ExpressionBodyShape::Binary {
            op: op.clone(),
            lhs: Box::new(expression_body_shape(lhs, span)?),
            rhs: Box::new(expression_body_shape(rhs, span)?),
        }),
        Expression::Unary { op, rhs, .. } => Ok(ExpressionBodyShape::Unary {
            op: op.clone(),
            rhs: Box::new(expression_body_shape(rhs, span)?),
        }),
        Expression::VarRef {
            name, subscripts, ..
        } => Ok(ExpressionBodyShape::VarRef {
            name: name.to_string(),
            subscripts: subscript_body_shapes(subscripts, span)?,
        }),
        Expression::BuiltinCall { function, args, .. } => Ok(ExpressionBodyShape::BuiltinCall {
            function: function.name().to_string(),
            args: expression_body_shapes(args, "builtin-call body-shape argument count", span)?,
        }),
        Expression::FunctionCall {
            name,
            args,
            is_constructor,
            ..
        } => Ok(ExpressionBodyShape::FunctionCall {
            name: name.to_string(),
            args: expression_body_shapes(args, "function-call body-shape argument count", span)?,
            is_constructor: *is_constructor,
        }),
        Expression::Literal { value, .. } => {
            Ok(ExpressionBodyShape::Literal(literal_body_shape(value)))
        }
        Expression::If {
            branches,
            else_branch,
            ..
        } => Ok(ExpressionBodyShape::If {
            branches: branch_body_shapes(branches, span)?,
            else_branch: Box::new(expression_body_shape(else_branch, span)?),
        }),
        Expression::Array {
            elements,
            is_matrix,
            ..
        } => Ok(ExpressionBodyShape::Array {
            elements: expression_body_shapes(elements, "array body-shape element count", span)?,
            is_matrix: *is_matrix,
        }),
        Expression::Tuple { elements, .. } => Ok(ExpressionBodyShape::Tuple(
            expression_body_shapes(elements, "tuple body-shape element count", span)?,
        )),
        Expression::Range {
            start, step, end, ..
        } => Ok(ExpressionBodyShape::Range {
            start: Box::new(expression_body_shape(start, span)?),
            step: step
                .as_ref()
                .map(|step| expression_body_shape(step, span).map(Box::new))
                .transpose()?,
            end: Box::new(expression_body_shape(end, span)?),
        }),
        Expression::ArrayComprehension { .. } => Ok(ExpressionBodyShape::ArrayComprehension),
        Expression::Index {
            base, subscripts, ..
        } => Ok(ExpressionBodyShape::Index {
            base: Box::new(expression_body_shape(base, span)?),
            subscripts: subscript_body_shapes(subscripts, span)?,
        }),
        Expression::FieldAccess { base, field, .. } => Ok(ExpressionBodyShape::FieldAccess {
            base: Box::new(expression_body_shape(base, span)?),
            field: field.clone(),
        }),
        Expression::Empty { .. } => Ok(ExpressionBodyShape::Empty),
    }
}

fn literal_body_shape(value: &rumoca_core::Literal) -> LiteralBodyShape {
    match value {
        rumoca_core::Literal::Real(_) => LiteralBodyShape::Real,
        rumoca_core::Literal::Integer(_) => LiteralBodyShape::Integer,
        rumoca_core::Literal::Boolean(_) => LiteralBodyShape::Boolean,
        rumoca_core::Literal::String(_) => LiteralBodyShape::String,
    }
}

fn expression_body_shapes(
    expressions: &[rumoca_core::Expression],
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<Vec<ExpressionBodyShape>, LowerError> {
    let mut shapes = stencil_vec_with_capacity(expressions.len(), context, span)?;
    for expression in expressions {
        shapes.push(expression_body_shape(expression, span)?);
    }
    Ok(shapes)
}

fn branch_body_shapes(
    branches: &[(rumoca_core::Expression, rumoca_core::Expression)],
    span: rumoca_core::Span,
) -> Result<Vec<(ExpressionBodyShape, ExpressionBodyShape)>, LowerError> {
    let mut shapes = stencil_vec_with_capacity(branches.len(), "if body-shape branch count", span)?;
    for (condition, branch) in branches {
        shapes.push((
            expression_body_shape(condition, span)?,
            expression_body_shape(branch, span)?,
        ));
    }
    Ok(shapes)
}

fn subscript_body_shapes(
    subscripts: &[rumoca_core::Subscript],
    span: rumoca_core::Span,
) -> Result<Vec<SubscriptBodyShape>, LowerError> {
    let mut shapes =
        stencil_vec_with_capacity(subscripts.len(), "body-shape subscript count", span)?;
    for subscript in subscripts {
        shapes.push(match subscript {
            rumoca_core::Subscript::Index { .. } => SubscriptBodyShape::ScalarizedIndex,
            rumoca_core::Subscript::Colon { .. } => SubscriptBodyShape::Colon,
            rumoca_core::Subscript::Expr { expr, .. } => {
                SubscriptBodyShape::Expr(expression_body_shape(expr, span)?)
            }
        });
    }
    Ok(shapes)
}

/// Like [`structured_dae_body_shapes_match`] but reads only the family's corner
/// rows (base + one neighbor per binder). For a regular family every cell shares
/// one body, so the corners are representative -- this avoids reading the interior
/// rows' DAE bodies. Singleton binders need no neighbor because they do not vary.
pub(super) fn corner_dae_body_shapes_match(
    rows: &[StructuredProgram],
    row_indices: &[usize],
    dae_equations: &[dae::Equation],
    domain: &rumoca_core::StructuredIndexDomain,
    span: rumoca_core::Span,
) -> Result<bool, LowerError> {
    let point_count = domain.scalar_count().map_err(|err| {
        stencil_contract_violation(format!("structured index domain is invalid: {err}"), span)
    })?;
    if row_indices.len() != point_count {
        return Ok(false);
    }
    let corner_positions = structured_domain_corner_ordinals(domain, span)?;
    let mut corner_rows =
        stencil_vec_with_capacity(corner_positions.len(), "corner body-shape row count", span)?;
    for &position in &corner_positions {
        corner_rows.push(row_indices[position]);
    }
    structured_dae_body_shapes_match(rows, &corner_rows, dae_equations)
}
