//! Annotated call inlining (SPEC_0040 STRUCT-T10(b)).
//!
//! A model-level call of a pure Modelica function whose body is one
//! straight-line assignment to its single result is replaced by that
//! assignment's expression with the call arguments substituted, in one
//! checked reconstruction. `Inline=true` and `LateInline=true` (MLS §18.3)
//! inline before index reduction, and never a callee with a derivative
//! annotation, so structural differentiation still reaches the supplied
//! derivative (MLS §12.7.1). `InlineAfterIndexReduction=true` inlines only
//! after formal-derivative construction. Calls of at most parameter variability
//! are settled once and stay calls, as does every refused call. A function
//! from which a call-graph cycle is reachable can recurse and is never
//! inlined.
//!
//! A body is admitted only when substitution keeps evaluation meaning: it may
//! not contain a relation, conditional, or event-generating operator, because
//! a function body generates no events (MLS §8.5, §12.2) while the same
//! expression written in an equation would. A body with any other statement,
//! assertions included, is not a single assignment and is refused, so no
//! assertion is ever dropped.

use rumoca_eval_dae::FunctionCallContext;
use rumoca_ir_dae as dae;

use crate::StructuralError;

/// When a call's inline request is honored.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum InlineStage {
    /// On the source system: `Inline`/`LateInline` callees without a
    /// derivative annotation.
    BeforeIndexReduction,
    /// After formal-derivative construction: every requested callee,
    /// `InlineAfterIndexReduction` included.
    AfterIndexReduction,
}

/// Inline the admitted annotated calls of `model` before index reduction.
///
/// Returns `None` when no call is admitted, leaving the source untouched.
pub fn inline_annotated_calls(model: &dae::Dae) -> Result<Option<dae::Dae>, StructuralError> {
    let plan = model.inspect(|view| inline_plan(view, InlineStage::BeforeIndexReduction));
    if !plan.iter().any(|&inline| inline) {
        return Ok(None);
    }
    super::reconstruction::rebuild_inlined_calls(model, &plan, &[]).map(|(model, _)| Some(model))
}

/// Inline the admitted calls of a finalized candidate DAE after
/// formal-derivative construction, `InlineAfterIndexReduction` callees
/// included.
///
/// The rebuilt root keeps every declaration ordinal, so reduced-chart
/// coordinates survive unchanged; retained manifold expressions are replayed
/// onto the rebuilt arena, and pins and structural analysis are recomputed.
pub fn inline_formal_calls(
    prepared: super::PreparedDae<'_>,
) -> Result<super::PreparedDae<'_>, StructuralError> {
    let super::PreparedDae::Transformed {
        dae,
        manifold,
        manifold_redundant,
        charts,
        ..
    } = &prepared
    else {
        return Ok(prepared);
    };
    let plan = dae.inspect(|view| inline_plan(view, InlineStage::AfterIndexReduction));
    if !plan.iter().any(|&inline| inline) {
        return Ok(prepared);
    }
    let (model, manifold) = super::reconstruction::rebuild_inlined_calls(dae, &plan, manifold)?;
    let manifold = manifold
        .into_iter()
        .zip(manifold_redundant.iter().copied())
        .map(|(expression, redundant)| super::ManifoldEntry {
            expression,
            redundant,
        })
        .collect();
    let structural = super::structural_analysis(&model)?;
    super::transformed(model, manifold, structural, charts.clone())
}

/// One flag per source expression: whether that call node is inlined.
pub(super) fn inline_plan(view: dae::DaeView<'_>, stage: InlineStage) -> Vec<bool> {
    let recursive = recursive_functions(view);
    (0..view.expression_count())
        .map(|index| {
            view.expression_id(index)
                .is_some_and(|id| admits_call(view, id, stage, &recursive))
        })
        .collect()
}

/// Per function index, whether the function can recurse: a call-graph cycle
/// is reachable from it, so it calls itself directly or through other
/// functions. Recursive groups are constructed as their own function
/// instances, so a model-level callee can reach a cycle without lying on it.
/// Such a function is never inlined, whatever frame a call of it appears in.
fn recursive_functions(view: dae::DaeView<'_>) -> Vec<bool> {
    let edges = (0..view.expression_count())
        .filter_map(|index| view.expression(view.expression_id(index)?))
        .filter_map(|node| match (node.function_scope(), node.operation()) {
            (Some(caller), dae::ExpressionOperation::Call { function, .. }) => {
                Some((caller.index() as usize, function.index() as usize))
            }
            _ => None,
        })
        .collect::<Vec<_>>();
    let count = edges
        .iter()
        .map(|&(caller, callee)| caller.max(callee) + 1)
        .max()
        .unwrap_or(0);
    let mut calls = vec![Vec::new(); count];
    for (caller, callee) in edges {
        calls[caller].push(callee);
    }
    let mut recursive = vec![false; count];
    // Tarjan emits components callees first, so every callee outside a
    // component is settled before the component itself.
    for component in crate::tarjan::tarjan_scc(count, &calls) {
        let cyclic = component.len() > 1
            || component
                .first()
                .is_some_and(|&function| calls[function].contains(&function));
        let reaches = cyclic
            || component
                .iter()
                .any(|&function| calls[function].iter().any(|&callee| recursive[callee]));
        for function in component {
            recursive[function] = reaches;
        }
    }
    recursive
}

fn admits_call<'dae>(
    view: dae::DaeView<'dae>,
    call: dae::ExprId<'dae>,
    stage: InlineStage,
    recursive: &[bool],
) -> bool {
    let Some(node) = view.expression(call) else {
        return false;
    };
    let dae::ExpressionOperation::Call {
        owner, function, ..
    } = node.operation()
    else {
        return false;
    };
    // A call of at most parameter variability is settled once per
    // simulation; substituting its body there buys nothing.
    if owner != call
        || matches!(
            node.variability(),
            dae::ExpressionVariability::Constant | dae::ExpressionVariability::Parameter
        )
        || node.function_scope().is_some()
        || node.binder_domain().is_some()
        || node.call_derivative().is_some()
    {
        return false;
    }
    let Some(callee) = view.function(function) else {
        return false;
    };
    let requested = match stage {
        InlineStage::BeforeIndexReduction => {
            callee.inline() == rumoca_core::InlineAnnotation::Requested
                && callee.derivatives().len() == 0
        }
        InlineStage::AfterIndexReduction => matches!(
            callee.inline(),
            rumoca_core::InlineAnnotation::Requested
                | rumoca_core::InlineAnnotation::AfterIndexReduction
        ),
    };
    if !requested
        || callee.is_external()
        || callee.result_types().len() != 1
        || recursive
            .get(function.index() as usize)
            .copied()
            .unwrap_or(false)
    {
        return false;
    }
    let Some((result, context)) = FunctionCallContext::default().call_result(view, call) else {
        return false;
    };
    BodyAdmission { view, depth: 0 }.admits(result, &context)
}

/// The substitution preflight: exactly the operations
/// `ExpressionRebuilder::rebuild_instantiated` reconstructs, minus every
/// operation whose meaning depends on being inside a function body.
struct BodyAdmission<'dae> {
    view: dae::DaeView<'dae>,
    depth: usize,
}

const MAXIMUM_DEPTH: usize = 256;

impl<'dae> BodyAdmission<'dae> {
    fn admits(
        &mut self,
        expression: dae::ExprId<'dae>,
        context: &FunctionCallContext<'dae>,
    ) -> bool {
        if self.depth >= MAXIMUM_DEPTH {
            return false;
        }
        self.depth += 1;
        let admitted = self.admits_scoped(expression, context);
        self.depth -= 1;
        admitted
    }

    fn admits_scoped(
        &mut self,
        expression: dae::ExprId<'dae>,
        context: &FunctionCallContext<'dae>,
    ) -> bool {
        let context = context.scoped_to_expression(self.view, expression);
        if let Some(branch) = context.selected_branch(self.view, expression) {
            return self.admits(branch, &context);
        }
        let Some(node) = self.view.expression(expression) else {
            return false;
        };
        if let dae::ExpressionOperation::Call { arguments, .. } = node.operation() {
            if node.function_scope().is_none() && node.binder_domain().is_none() {
                return true;
            }
            return node.call_derivative().is_none()
                && arguments
                    .iter()
                    .all(|argument| self.admits(argument, &context));
        }
        if node.function_scope().is_none() && node.binder_domain().is_none() {
            return true;
        }
        match node.operation() {
            dae::ExpressionOperation::Literal(_) => true,
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::FunctionParameter(
                parameter,
            )) => context
                .parameter_argument(parameter)
                .is_some_and(|argument| self.admits(argument, &context)),
            dae::ExpressionOperation::Unary { operand, .. } => self.admits(operand, &context),
            dae::ExpressionOperation::Binary { operator, lhs, rhs } => {
                !is_relation(operator) && self.admits(lhs, &context) && self.admits(rhs, &context)
            }
            dae::ExpressionOperation::Array(operands)
            | dae::ExpressionOperation::Record(operands) => {
                !operands.is_empty()
                    && operands
                        .iter()
                        .all(|operand| self.admits(operand, &context))
            }
            dae::ExpressionOperation::Field { base, field } => {
                context.projected_field(self.view, base, field).is_some_and(
                    |(projected, projected_context)| self.admits(projected, &projected_context),
                )
            }
            dae::ExpressionOperation::Index { base, subscripts } => {
                self.admits(base, &context)
                    && subscripts.iter().all(|subscript| match subscript {
                        dae::SubscriptView::Whole { .. } => true,
                        dae::SubscriptView::Index { expression, .. }
                        | dae::SubscriptView::Slice { expression, .. } => {
                            self.admits(expression, &context)
                        }
                    })
            }
            dae::ExpressionOperation::Builtin { builtin, arguments } => {
                is_event_free(builtin)
                    && arguments
                        .iter()
                        .all(|argument| self.admits(argument, &context))
            }
            _ => false,
        }
    }
}

const fn is_relation(operator: dae::BinaryOperator) -> bool {
    matches!(
        operator,
        dae::BinaryOperator::Equal
            | dae::BinaryOperator::NotEqual
            | dae::BinaryOperator::Less
            | dae::BinaryOperator::LessEqual
            | dae::BinaryOperator::Greater
            | dae::BinaryOperator::GreaterEqual
    )
}

/// Builtins whose value is continuous in their arguments and that generate no
/// event outside a function body (MLS §3.7.2 lists the event-generating ones).
const fn is_event_free(builtin: dae::PureBuiltin) -> bool {
    matches!(
        builtin,
        dae::PureBuiltin::Sqrt
            | dae::PureBuiltin::Sin
            | dae::PureBuiltin::Cos
            | dae::PureBuiltin::Tan
            | dae::PureBuiltin::Asin
            | dae::PureBuiltin::Acos
            | dae::PureBuiltin::Atan
            | dae::PureBuiltin::Atan2
            | dae::PureBuiltin::Sinh
            | dae::PureBuiltin::Cosh
            | dae::PureBuiltin::Tanh
            | dae::PureBuiltin::Exp
            | dae::PureBuiltin::Log
            | dae::PureBuiltin::Log10
            | dae::PureBuiltin::Sum
            | dae::PureBuiltin::Product
            | dae::PureBuiltin::Zeros
            | dae::PureBuiltin::Ones
            | dae::PureBuiltin::Fill
            | dae::PureBuiltin::Cross
            | dae::PureBuiltin::PromotedCat1
            | dae::PureBuiltin::PromotedCat2
            | dae::PureBuiltin::Identity
            | dae::PureBuiltin::Vector
            | dae::PureBuiltin::Transpose
            | dae::PureBuiltin::Diagonal
            | dae::PureBuiltin::OuterProduct
            | dae::PureBuiltin::Skew
    )
}
