use rumoca_core::Expression;
use rumoca_core::ExpressionVisitor;
use rumoca_ir_dae as dae;
use rumoca_ir_solve::{LinearOp, ScalarSlot, VarLayout};

use super::{LowerError, derivative_rhs::state_derivative_equation_flags, expression_rows};

pub fn lower_initial_residual(
    dae_model: &dae::Dae,
    layout: &VarLayout,
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    let initial_equations = initial_residual_equations(dae_model, layout)?;
    expression_rows::lower_residual_rows_from_equations_with_mode(
        dae_model,
        layout,
        initial_equations,
        0,
        true,
    )
}

pub fn initial_residual_equations<'a>(
    dae_model: &'a dae::Dae,
    layout: &VarLayout,
) -> Result<Vec<(usize, &'a dae::Equation)>, LowerError> {
    let state_derivative_rows = state_derivative_equation_flags(dae_model)?;
    Ok(dae_model
        .continuous
        .equations
        .iter()
        .enumerate()
        .filter(|(row_idx, equation)| {
            !state_derivative_rows
                .get(*row_idx)
                .copied()
                .unwrap_or(false)
                || derivative_row_constrains_initial_unknown(layout, equation)
        })
        .map(|(_, eq)| eq)
        .chain(
            dae_model
                .initialization
                .equations
                .iter()
                .filter(|eq| initial_equation_constrains_solver_unknown(layout, eq)),
        )
        .enumerate()
        .collect())
}

fn derivative_row_constrains_initial_unknown(layout: &VarLayout, equation: &dae::Equation) -> bool {
    if equation
        .lhs
        .as_ref()
        .is_some_and(|lhs| is_initialization_unknown(layout.binding(lhs.as_str())))
    {
        return true;
    }
    let Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs,
        rhs,
        ..
    } = &equation.rhs
    else {
        return false;
    };
    expression_is_direct_initial_unknown(layout, lhs)
        || expression_is_direct_initial_unknown(layout, rhs)
}

fn expression_is_direct_initial_unknown(layout: &VarLayout, expr: &Expression) -> bool {
    if expr.contains_der() {
        return false;
    }
    let Expression::VarRef {
        name, subscripts, ..
    } = expr
    else {
        return false;
    };
    if !subscripts.is_empty() {
        return false;
    }
    is_initialization_unknown(layout.binding(name.as_str()))
}

fn initial_equation_constrains_solver_unknown(
    layout: &VarLayout,
    equation: &dae::Equation,
) -> bool {
    equation
        .lhs
        .as_ref()
        .is_some_and(|lhs| is_initialization_unknown(layout.binding(lhs.as_str())))
        || expression_references_solver_unknown(layout, &equation.rhs)
}

fn is_initialization_unknown(slot: Option<ScalarSlot>) -> bool {
    matches!(slot, Some(ScalarSlot::Y { .. } | ScalarSlot::P { .. }))
}

fn expression_references_solver_unknown(layout: &VarLayout, expr: &Expression) -> bool {
    let mut checker = SolverUnknownReferenceChecker {
        layout,
        found: false,
    };
    checker.visit_expression(expr);
    checker.found
}

struct SolverUnknownReferenceChecker<'a> {
    layout: &'a VarLayout,
    found: bool,
}

impl ExpressionVisitor for SolverUnknownReferenceChecker<'_> {
    fn visit_expression(&mut self, expr: &Expression) {
        if !self.found {
            self.walk_expression(expr);
        }
    }

    fn visit_var_ref(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
    ) {
        if is_initialization_unknown(self.layout.binding(name.as_str())) {
            self.found = true;
            return;
        }
        for subscript in subscripts {
            self.visit_subscript(subscript);
        }
    }
}
