//! The phasor each visible angle or power-factor scalar is a function of
//! (SPEC_0040 SOLVE-C65).
//!
//! A visible scalar `v` is recorded when one continuous residual equation
//! states `v = E` and `E` reduces, by substitution alone, to `atan2(y, x)`
//! (an angle) or `cos(atan2(y, x))` (the cosine of one) of two visible Real
//! scalar coordinates `y` and `x`, each up to sign. The reduction follows a Modelica
//! function call into its result definition with the call's arguments bound to
//! its parameters, and selects a conditional branch only when the guard
//! reduces to a literal (`Modelica.Math.atan3(y, x, 0)` is `atan2(y, x)`
//! because its `y0 == 0` guard does). A conditional whose guard does not
//! reduce is admitted only when every branch reduces to an angle of the same
//! coordinates. Nothing else is read: no name, no run-time value, and no
//! parameter value, which a run may override; only literals fold.
//!
//! The fact holds at every point satisfying the equation, whatever the
//! equation is matched to, so it is independent of causalization. A scalar
//! two equations would record differently is left unrecorded.

mod literal;
mod proof;
mod scoped;
#[cfg(test)]
mod tests;

use literal::literal;
use proof::ProofQuery;
use rumoca_eval_dae::NumericSourceScalar;

use std::collections::HashMap;

use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

/// One visible scalar coordinate: declaration ordinal and scalar ordinal.
type ScalarCoordinate = (u32, usize);

/// The two coordinates an angle is `atan2` of, each up to sign.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct AngleOperands {
    im: ScalarCoordinate,
    re: ScalarCoordinate,
}

/// Visible scalars defined as a function of a phasor's angle.
pub(crate) struct PhasorSources {
    sources: HashMap<ScalarCoordinate, Option<solve::SolvePhasor>>,
}

impl PhasorSources {
    pub(crate) fn declaration_sources(
        &self,
        id: dae::VariableId<'_>,
    ) -> Vec<(usize, solve::SolvePhasor)> {
        let mut facts = self
            .sources
            .iter()
            .filter_map(|(&(declaration, ordinal), fact)| {
                (declaration == id.index())
                    .then(|| fact.clone().map(|fact| (ordinal, fact)))
                    .flatten()
            })
            .collect::<Vec<_>>();
        facts.sort_by_key(|fact| fact.0);
        facts
    }

    pub(crate) fn of(view: dae::DaeView<'_>) -> Self {
        let mut sources = HashMap::new();
        for (target, phasor) in view
            .continuous_owners()
            .filter_map(|owner| match owner {
                dae::ContinuousOwnerView::Residual { equation, .. } => {
                    crate::model_values::equation_sides(view, equation.residual())
                }
                dae::ContinuousOwnerView::Structured { .. } => None,
            })
            .flat_map(|(lhs, rhs)| [(lhs, rhs), (rhs, lhs)])
            .filter_map(|(target, value)| {
                let target = scalar_coordinate(view, target, None, &ProofQuery::default())?;
                Some((target, phasor_of(view, value, target)?))
            })
        {
            let recorded = sources
                .entry(target)
                .or_insert_with(|| Some(phasor.clone()));
            if recorded.as_ref() != Some(&phasor) {
                *recorded = None;
            }
        }
        Self { sources }
    }
}

/// The phasor `value` is a function of, when it reduces to an angle of two
/// visible scalars other than `target`, or to the cosine of one.
fn phasor_of<'dae>(
    view: dae::DaeView<'dae>,
    value: dae::ExprId<'dae>,
    target: ScalarCoordinate,
) -> Option<solve::SolvePhasor> {
    let query = ProofQuery::default();
    let (function, operands) = match view.expression(value)?.operation() {
        dae::ExpressionOperation::Builtin {
            builtin: dae::PureBuiltin::Cos,
            arguments,
        } if arguments.len() == 1 => (
            solve::SolvePhasorFunction::CosineOfAngle,
            angle_operands(view, arguments.get(0)?, None, &query)?,
        ),
        _ => (
            solve::SolvePhasorFunction::Angle,
            angle_operands(view, value, None, &query)?,
        ),
    };
    if operands.im == target || operands.re == target || operands.im == operands.re {
        return None;
    }
    Some(solve::SolvePhasor {
        function,
        re: scalar_name(view, operands.re)?,
        im: scalar_name(view, operands.im)?,
    })
}

fn scalar_name(view: dae::DaeView<'_>, (variable, scalar): ScalarCoordinate) -> Option<String> {
    view.variable(view.variable_id(variable as usize)?)?
        .scalar_name(scalar)
}

/// The arguments of one Modelica function call whose result is being read,
/// bound to that function's parameters; `parent` is the caller's frame, in
/// which the arguments are read.
struct CallFrame<'frame, 'dae> {
    call: dae::ExprId<'dae>,
    function: dae::FunctionId<'dae>,
    arguments: dae::ExpressionOperands<'dae>,
    parent: Option<&'frame CallFrame<'frame, 'dae>>,
}

impl<'dae> CallFrame<'_, 'dae> {
    fn calls(&self, function: dae::FunctionId<'dae>) -> bool {
        self.function == function || self.parent.is_some_and(|parent| parent.calls(function))
    }
}

/// The value one expression reads in `frame` when it forwards another: a
/// local or result definition, or a parameter bound to a call argument.
enum Forward<'frame, 'dae> {
    Expression(dae::ExprId<'dae>, Option<&'frame CallFrame<'frame, 'dae>>),
    Opaque,
}

fn forward<'frame, 'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    frame: Option<&'frame CallFrame<'frame, 'dae>>,
) -> Forward<'frame, 'dae> {
    let Some(node) = view.expression(expression) else {
        return Forward::Opaque;
    };
    match node.operation() {
        dae::ExpressionOperation::FunctionValue { definition, .. } => {
            Forward::Expression(definition.rhs(), frame)
        }
        dae::ExpressionOperation::Coordinate(dae::CoordinateView::FunctionParameter(parameter)) => {
            match frame {
                Some(frame) if frame.function == parameter.function() => {
                    match frame.arguments.get(parameter.ordinal() as usize) {
                        Some(argument) => Forward::Expression(argument, frame.parent),
                        None => Forward::Opaque,
                    }
                }
                _ => Forward::Opaque,
            }
        }
        _ => Forward::Opaque,
    }
}

/// The coordinates `expression` is the `atan2` angle of, up to the sign of
/// either coordinate and of the angle.
fn angle_operands<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    frame: Option<&CallFrame<'_, 'dae>>,
    query: &ProofQuery<'dae>,
) -> Option<AngleOperands> {
    query
        .angle_operands
        .get_or_compute(proof::expression_key(expression, frame), || {
            angle_operands_uncached(view, expression, frame, query)
        })
}

fn angle_operands_uncached<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    frame: Option<&CallFrame<'_, 'dae>>,
    query: &ProofQuery<'dae>,
) -> Option<AngleOperands> {
    if let Forward::Expression(forwarded, frame) = forward(view, expression, frame) {
        return angle_operands(view, forwarded, frame, query);
    }
    match view.expression(expression)?.operation() {
        dae::ExpressionOperation::Builtin {
            builtin: dae::PureBuiltin::Atan2,
            arguments,
        } if arguments.len() == 2 => Some(AngleOperands {
            im: scalar_coordinate(view, arguments.get(0)?, frame, query)?,
            re: scalar_coordinate(view, arguments.get(1)?, frame, query)?,
        }),
        dae::ExpressionOperation::Unary {
            operator: dae::UnaryOperator::Plus | dae::UnaryOperator::Negate,
            operand,
        } => angle_operands(view, operand, frame, query),
        dae::ExpressionOperation::Call {
            function,
            output,
            arguments,
            ..
        } => {
            if frame.is_some_and(|frame| frame.calls(function)) {
                return None;
            }
            let callee = view.function(function)?;
            if callee.is_external() {
                return None;
            }
            let result = callee.result_values().rhs(output as usize)?;
            let call = CallFrame {
                call: expression,
                function,
                arguments,
                parent: frame,
            };
            angle_operands(view, result, Some(&call), query)
        }
        dae::ExpressionOperation::FunctionConditionalOutput {
            conditional, value, ..
        } => scoped::reduce(
            view,
            conditional,
            value,
            frame,
            query,
            &query.angle_states,
            angle_operands,
        ),
        dae::ExpressionOperation::Conditional(operands) => {
            let mut angles = selected_branches(view, operands, frame, query)?
                .into_iter()
                .map(|branch| angle_operands(view, branch, frame, query));
            let first = angles.next()??;
            angles.all(|angle| angle == Some(first)).then_some(first)
        }
        _ => None,
    }
}

/// The branches of `if c1 then v1 elseif ... else f` that can be taken: a
/// guard reducing to literal `false` drops its branch, the first reducing to
/// literal `true` ends the list with its own, and a guard that does not reduce
/// keeps its branch and the ones after it.
fn selected_branches<'dae>(
    view: dae::DaeView<'dae>,
    operands: dae::ExpressionOperands<'dae>,
    frame: Option<&CallFrame<'_, 'dae>>,
    query: &ProofQuery<'dae>,
) -> Option<Vec<dae::ExprId<'dae>>> {
    let count = operands.len();
    if count.is_multiple_of(2) {
        return None;
    }
    let mut branches = Vec::new();
    for ordinal in (0..count - 1).step_by(2) {
        let branch = operands.get(ordinal + 1)?;
        match literal(view, operands.get(ordinal)?, frame, query) {
            Some(NumericSourceScalar::Boolean(true)) => {
                branches.push(branch);
                return Some(branches);
            }
            Some(NumericSourceScalar::Boolean(false)) => {}
            Some(_) => return None,
            None => branches.push(branch),
        }
    }
    branches.push(operands.get(count - 1)?);
    Some(branches)
}

/// A visible scalar coordinate `expression` reads, up to sign.
fn scalar_coordinate<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    frame: Option<&CallFrame<'_, 'dae>>,
    query: &ProofQuery<'dae>,
) -> Option<ScalarCoordinate> {
    query
        .scalar_coordinate
        .get_or_compute(proof::expression_key(expression, frame), || {
            scalar_coordinate_uncached(view, expression, frame, query)
        })
}

fn scalar_coordinate_uncached<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    frame: Option<&CallFrame<'_, 'dae>>,
    query: &ProofQuery<'dae>,
) -> Option<ScalarCoordinate> {
    if let Forward::Expression(forwarded, frame) = forward(view, expression, frame) {
        return scalar_coordinate(view, forwarded, frame, query);
    }
    match view.expression(expression)?.operation() {
        dae::ExpressionOperation::FunctionConditionalOutput {
            conditional, value, ..
        } => scoped::reduce(
            view,
            conditional,
            value,
            frame,
            query,
            &query.coordinate_states,
            scalar_coordinate,
        ),
        dae::ExpressionOperation::Unary {
            operator: dae::UnaryOperator::Plus | dae::UnaryOperator::Negate,
            operand,
        } => scalar_coordinate(view, operand, frame, query),
        dae::ExpressionOperation::Coordinate(coordinate) => {
            let variable = visible_value_variable(view, expression, coordinate)?;
            (view.variable(variable)?.scalar_count() == 1).then_some((variable.index(), 0))
        }
        dae::ExpressionOperation::Index { base, subscripts } => {
            let dae::ExpressionOperation::Coordinate(coordinate) =
                view.expression(base)?.operation()
            else {
                return None;
            };
            let variable = visible_value_variable(view, base, coordinate)?;
            let scalar = crate::model_values::single_index_scalar(
                view,
                subscripts,
                view.variable(variable)?.scalar_count(),
            )?;
            Some((variable.index(), scalar))
        }
        _ => None,
    }
}

/// The variable whose current value a coordinate read is, when that value is
/// a traced Real channel: a Real state, algebraic, input, or discrete Real.
fn visible_value_variable<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    coordinate: dae::CoordinateView<'dae>,
) -> Option<dae::VariableId<'dae>> {
    match coordinate {
        dae::CoordinateView::State(_)
        | dae::CoordinateView::Algebraic(_)
        | dae::CoordinateView::Input(_)
        | dae::CoordinateView::DiscreteReal(_) => {
            let variable = view.expression(expression)?.variable_coordinate()?;
            (view.variable(variable)?.value_type().scalar_type() == dae::ScalarType::Real)
                .then_some(variable)
        }
        _ => None,
    }
}
