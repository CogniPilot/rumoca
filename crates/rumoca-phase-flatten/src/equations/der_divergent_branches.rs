//! Translation-time resolution of if-equations whose branches disagree about
//! which variables they differentiate (MLS §8.3.4).
//!
//! An if-equation with matching branch equation counts is normally lowered into
//! one residual carrying a conditional expression, which keeps the condition
//! live at simulation time. That is only sound while every branch describes the
//! same DAE. When one branch contains `der(x)` and another does not, the merged
//! residual still mentions `der(x)`, so `x` is selected as a state even in the
//! configuration whose branch never assigns it — and nothing then defines the
//! derivative.
//!
//! MSL's `Thermal.FluidHeatFlow.BaseClasses.TwoPort` is written exactly that
//! way:
//!
//! ```modelica
//! if m > Modelica.Constants.small then
//!   flowPort_a.H_flow + flowPort_b.H_flow + Q_flow = m*medium.cv*der(T);
//! else
//!   flowPort_a.H_flow + flowPort_b.H_flow + Q_flow = 0;
//! end if;
//! ```
//!
//! Components declared with `m = 0` (every pump and valve in
//! `Thermal.FluidHeatFlow.Examples`) take the algebraic branch, so keeping the
//! conditional made `der(T)` evaluate as `.../0`.

use std::collections::BTreeSet;
use std::ops::ControlFlow;

use rumoca_ir_ast as ast;

use crate::Context;
use crate::boolean_eval::try_eval_boolean_with_ctx_inner;

/// True when the branches do not agree on which variables they differentiate.
pub(super) fn branches_differ_in_der_targets(
    cond_blocks: &[ast::EquationBlock],
    else_block: &Option<Vec<ast::Equation>>,
) -> bool {
    let mut branches = cond_blocks
        .iter()
        .map(|block| der_targets_in_equations(&block.eqs))
        .collect::<Vec<_>>();
    branches.push(der_targets_in_equations(
        else_block.as_deref().unwrap_or(&[]),
    ));
    let Some((first, rest)) = branches.split_first() else {
        return false;
    };
    rest.iter().any(|targets| targets != first)
}

/// Resolve the condition using every known parameter value, not only the
/// structural ones `try_select_constant_branch` admits.
///
/// Returns `None` when no condition can be decided, which leaves the caller's
/// existing conditional-expression lowering in place.
pub(super) fn try_select_parameter_branch(
    cond_blocks: &[ast::EquationBlock],
    else_block: &Option<Vec<ast::Equation>>,
    ctx: &Context,
    prefix: &ast::QualifiedName,
) -> Option<Vec<ast::Equation>> {
    for block in cond_blocks {
        match try_eval_boolean_with_ctx_inner(&block.cond, Some(ctx), prefix) {
            Some(true) => return Some(block.eqs.clone()),
            Some(false) => continue,
            None => return None,
        }
    }
    Some(match else_block {
        Some(equations) => equations.clone(),
        None => Vec::new(),
    })
}

/// Collect the rendered `der(...)` arguments reachable from `equations`.
///
/// Nested `for`/`if`/`when` bodies are included: a `der()` anywhere inside a
/// branch makes its target a state candidate for that branch.
fn der_targets_in_equations(equations: &[ast::Equation]) -> BTreeSet<String> {
    struct DerTargets(BTreeSet<String>);

    impl ast::Visitor for DerTargets {
        fn visit_expr_function_call(
            &mut self,
            comp: &ast::ComponentReference,
            args: &[ast::Expression],
        ) -> ControlFlow<()> {
            if comp.parts.len() == 1
                && comp.parts[0].ident.text.as_ref() == "der"
                && let Some(target) = args.first()
            {
                self.0.insert(target.to_string());
            }
            self.visit_each(args, Self::visit_expression)
        }
    }

    let mut targets = DerTargets(BTreeSet::new());
    for equation in equations {
        let _ = ast::Visitor::visit_equation(&mut targets, equation);
    }
    targets.0
}
