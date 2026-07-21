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
    // Each continuous-derived equation keeps its ORIGINAL `continuous.equations`
    // index as the tuple's `usize`. That index is the structured-family
    // provenance: the array-native residual lowering matches it against the
    // continuous structured families (`structured_equation_slot`), so a
    // re-enumerated 0..N index would break stencil/Map detection and force the
    // whole initialization residual back to per-cell scalar programs. Row lowering
    // itself only uses the index for per-row temp namespacing (uniqueness) and
    // `row_idx >= state_scalar_count` comparisons (`state_scalar_count` is 0 in
    // initial mode, so the value beyond "is it < 0" is irrelevant there).
    let continuous = dae_model
        .continuous
        .equations
        .iter()
        .enumerate()
        .filter(|(row_idx, _)| {
            !state_derivative_rows
                .get(*row_idx)
                .copied()
                .unwrap_or(false)
        });
    // Initialization-specific equations are not part of the continuous system, so
    // they take indices past the continuous range: no structured family matches
    // (clean scalar fallback) while the index stays unique for temp namespacing.
    let continuous_len = dae_model.continuous.equations.len();
    let initial = dae_model
        .initialization
        .equations
        .iter()
        .filter(|eq| initial_equation_constrains_solver_unknown(layout, eq))
        .enumerate()
        .map(move |(offset, eq)| (continuous_len + offset, eq));
    Ok(continuous.chain(initial).collect())
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
