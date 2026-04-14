use crate::compiled::layout::VarLayout;
use crate::compiled::linear_op::{LinearOp, UnaryOp};
use indexmap::IndexMap;
use rumoca_ir_dae as dae;

use super::{LowerBuilder, LowerError, Scope};

pub fn lower_expression_rows_from_expressions(
    expressions: &[dae::Expression],
    layout: &VarLayout,
    functions: &IndexMap<dae::VarName, dae::Function>,
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    lower_expression_rows_from_expressions_with_mode(expressions, layout, functions, None, false)
}

pub fn lower_initial_expression_rows_from_expressions(
    expressions: &[dae::Expression],
    layout: &VarLayout,
    functions: &IndexMap<dae::VarName, dae::Function>,
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    lower_expression_rows_from_expressions_with_mode(expressions, layout, functions, None, true)
}

pub fn lower_expression_rows_from_expressions_with_runtime_metadata(
    expressions: &[dae::Expression],
    layout: &VarLayout,
    functions: &IndexMap<dae::VarName, dae::Function>,
    clock_intervals: &IndexMap<String, f64>,
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    lower_expression_rows_from_expressions_with_mode(
        expressions,
        layout,
        functions,
        Some(clock_intervals),
        false,
    )
}

pub fn lower_initial_expression_rows_from_expressions_with_runtime_metadata(
    expressions: &[dae::Expression],
    layout: &VarLayout,
    functions: &IndexMap<dae::VarName, dae::Function>,
    clock_intervals: &IndexMap<String, f64>,
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    lower_expression_rows_from_expressions_with_mode(
        expressions,
        layout,
        functions,
        Some(clock_intervals),
        true,
    )
}

fn lower_expression_rows_from_expressions_with_mode(
    expressions: &[dae::Expression],
    layout: &VarLayout,
    functions: &IndexMap<dae::VarName, dae::Function>,
    clock_intervals: Option<&IndexMap<String, f64>>,
    is_initial_mode: bool,
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    let mut rows = Vec::with_capacity(expressions.len());
    for expression in expressions {
        rows.push(lower_expression_row(
            expression,
            layout,
            functions,
            clock_intervals,
            is_initial_mode,
        )?);
    }
    Ok(rows)
}

pub(super) fn lower_expression_rows_with_mode<'a>(
    equations: impl IntoIterator<Item = &'a dae::Equation>,
    layout: &VarLayout,
    functions: &IndexMap<dae::VarName, dae::Function>,
    clock_intervals: &IndexMap<String, f64>,
    is_initial_mode: bool,
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    let equations: Vec<&dae::Equation> = equations.into_iter().collect();
    let mut rows = Vec::with_capacity(equations.len());
    for equation in equations {
        rows.push(lower_expression_row(
            &equation.rhs,
            layout,
            functions,
            Some(clock_intervals),
            is_initial_mode,
        )?);
    }
    Ok(rows)
}

pub(super) fn lower_residual_rows_with_mode(
    dae_model: &dae::Dae,
    layout: &VarLayout,
    is_initial_mode: bool,
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    let n_x: usize = dae_model.states.values().map(|v| v.size()).sum();
    let mut rows = Vec::with_capacity(dae_model.f_x.len());
    for (row_idx, eq) in dae_model.f_x.iter().enumerate() {
        if eq.scalar_count != 1 {
            return Err(LowerError::Unsupported {
                reason: format!(
                    "array residual row unsupported in PR2 (origin={} scalar_count={})",
                    eq.origin, eq.scalar_count
                ),
            });
        }

        let mut builder = LowerBuilder::new_with_runtime_metadata(
            layout,
            &dae_model.functions,
            &dae_model.clock_intervals,
            is_initial_mode,
        );
        let scope = Scope::new();
        let row = builder.lower_expr(&eq.rhs, &scope, 0)?;
        let signed = if row_idx < n_x {
            builder.emit_unary(UnaryOp::Neg, row)
        } else {
            row
        };
        builder.ops.push(LinearOp::StoreOutput { src: signed });
        rows.push(builder.ops);
    }
    Ok(rows)
}

fn lower_expression_row(
    expression: &dae::Expression,
    layout: &VarLayout,
    functions: &IndexMap<dae::VarName, dae::Function>,
    clock_intervals: Option<&IndexMap<String, f64>>,
    is_initial_mode: bool,
) -> Result<Vec<LinearOp>, LowerError> {
    let mut builder = match clock_intervals {
        Some(clock_intervals) => LowerBuilder::new_with_runtime_metadata(
            layout,
            functions,
            clock_intervals,
            is_initial_mode,
        ),
        None => LowerBuilder::new_with_mode(layout, functions, is_initial_mode),
    };
    let scope = Scope::new();
    let value = builder.lower_expr(expression, &scope, 0)?;
    builder.ops.push(LinearOp::StoreOutput { src: value });
    Ok(builder.ops)
}
