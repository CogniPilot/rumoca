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
    lower_initial_residual_from_equations(dae_model, layout, initial_equations)
}

pub(crate) fn lower_initial_residual_from_equations<'a>(
    dae_model: &dae::Dae,
    layout: &VarLayout,
    initial_equations: impl IntoIterator<Item = (usize, &'a dae::Equation)>,
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    expression_rows::lower_residual_rows_from_equations_with_mode(
        dae_model,
        layout,
        initial_equations,
        0,
        true,
    )
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum InitialResidualSource {
    Continuous,
    Initialization,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct InitialResidualEquation<'a> {
    pub equation: &'a dae::Equation,
    pub source: InitialResidualSource,
}

pub fn initial_residual_equations<'a>(
    dae_model: &'a dae::Dae,
    layout: &VarLayout,
) -> Result<Vec<(usize, &'a dae::Equation)>, LowerError> {
    Ok(initial_residual_equations_with_sources(dae_model, layout)?
        .into_iter()
        .map(|row| row.equation)
        .enumerate()
        .collect())
}

pub(crate) fn initial_residual_equations_with_sources<'a>(
    dae_model: &'a dae::Dae,
    layout: &VarLayout,
) -> Result<Vec<InitialResidualEquation<'a>>, LowerError> {
    let state_derivative_rows = state_derivative_equation_flags(dae_model);
    validate_state_derivative_flags(dae_model, &state_derivative_rows)?;
    Ok(dae_model
        .continuous
        .equations
        .iter()
        .enumerate()
        .filter(|(row_idx, equation)| {
            !state_derivative_rows[*row_idx]
                || derivative_row_constrains_initial_unknown(layout, equation)
        })
        .map(|(_, eq)| eq)
        .map(|equation| InitialResidualEquation {
            equation,
            source: InitialResidualSource::Continuous,
        })
        .chain(
            dae_model
                .initialization
                .equations
                .iter()
                .filter(|eq| initial_equation_constrains_solver_unknown(layout, eq))
                .map(|equation| InitialResidualEquation {
                    equation,
                    source: InitialResidualSource::Initialization,
                }),
        )
        .collect())
}

fn validate_state_derivative_flags(
    dae_model: &dae::Dae,
    state_derivative_rows: &[bool],
) -> Result<(), LowerError> {
    let equation_count = dae_model.continuous.equations.len();
    if state_derivative_rows.len() == equation_count {
        return Ok(());
    }
    let span = match dae_model
        .continuous
        .equations
        .get(state_derivative_rows.len())
    {
        Some(equation) => equation.span,
        None => match dae_model.continuous.equations.last() {
            Some(equation) => equation.span,
            // With no continuous equation there is no source construct to label.
            None => rumoca_core::Span::DUMMY,
        },
    };
    Err(LowerError::ContractViolation {
        reason: format!(
            "state derivative flag count {} does not match continuous equation count {equation_count}",
            state_derivative_rows.len()
        ),
        span,
    })
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
