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

use std::collections::HashMap;

use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

/// One visible scalar coordinate: declaration ordinal and scalar ordinal.
type ScalarCoordinate = (u32, usize);

/// The two coordinates an angle is `atan2` of, each up to sign.
#[derive(Clone, Copy, PartialEq, Eq)]
struct AngleOperands {
    im: ScalarCoordinate,
    re: ScalarCoordinate,
}

/// Visible scalars defined as a function of a phasor's angle.
pub(crate) struct PhasorSources {
    sources: HashMap<ScalarCoordinate, Option<solve::SolvePhasor>>,
}

impl PhasorSources {
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
                let target = scalar_coordinate(view, target, None)?;
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

    pub(crate) fn source(
        &self,
        id: dae::VariableId<'_>,
        scalar: usize,
    ) -> Option<solve::SolvePhasor> {
        self.sources.get(&(id.index(), scalar)).cloned().flatten()
    }
}

/// The phasor `value` is a function of, when it reduces to an angle of two
/// visible scalars other than `target`, or to the cosine of one.
fn phasor_of<'dae>(
    view: dae::DaeView<'dae>,
    value: dae::ExprId<'dae>,
    target: ScalarCoordinate,
) -> Option<solve::SolvePhasor> {
    let (function, operands) = match view.expression(value)?.operation() {
        dae::ExpressionOperation::Builtin {
            builtin: dae::PureBuiltin::Cos,
            arguments,
        } if arguments.len() == 1 => (
            solve::SolvePhasorFunction::CosineOfAngle,
            angle_operands(view, arguments.get(0)?, None)?,
        ),
        _ => (
            solve::SolvePhasorFunction::Angle,
            angle_operands(view, value, None)?,
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
) -> Option<AngleOperands> {
    if let Forward::Expression(forwarded, frame) = forward(view, expression, frame) {
        return angle_operands(view, forwarded, frame);
    }
    match view.expression(expression)?.operation() {
        dae::ExpressionOperation::Builtin {
            builtin: dae::PureBuiltin::Atan2,
            arguments,
        } if arguments.len() == 2 => Some(AngleOperands {
            im: scalar_coordinate(view, arguments.get(0)?, frame)?,
            re: scalar_coordinate(view, arguments.get(1)?, frame)?,
        }),
        dae::ExpressionOperation::Unary {
            operator: dae::UnaryOperator::Plus | dae::UnaryOperator::Negate,
            operand,
        } => angle_operands(view, operand, frame),
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
                function,
                arguments,
                parent: frame,
            };
            angle_operands(view, result, Some(&call))
        }
        dae::ExpressionOperation::Conditional(operands) => {
            let mut angles = selected_branches(view, operands, frame)?
                .into_iter()
                .map(|branch| angle_operands(view, branch, frame));
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
) -> Option<Vec<dae::ExprId<'dae>>> {
    let count = operands.len();
    if count.is_multiple_of(2) {
        return None;
    }
    let mut branches = Vec::new();
    for ordinal in (0..count - 1).step_by(2) {
        let branch = operands.get(ordinal + 1)?;
        match literal(view, operands.get(ordinal)?, frame) {
            Some(guard) if guard != 0.0 => {
                branches.push(branch);
                return Some(branches);
            }
            Some(_) => {}
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
) -> Option<ScalarCoordinate> {
    if let Forward::Expression(forwarded, frame) = forward(view, expression, frame) {
        return scalar_coordinate(view, forwarded, frame);
    }
    match view.expression(expression)?.operation() {
        dae::ExpressionOperation::Unary {
            operator: dae::UnaryOperator::Plus | dae::UnaryOperator::Negate,
            operand,
        } => scalar_coordinate(view, operand, frame),
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

/// The literal value `expression` reduces to by substitution, Booleans as 0
/// and 1; `None` for anything that reads a coordinate, a parameter included.
fn literal<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    frame: Option<&CallFrame<'_, 'dae>>,
) -> Option<f64> {
    if let Forward::Expression(forwarded, frame) = forward(view, expression, frame) {
        return literal(view, forwarded, frame);
    }
    let truth = |value: bool| Some(if value { 1.0 } else { 0.0 });
    match view.expression(expression)?.operation() {
        dae::ExpressionOperation::Literal(dae::DaeLiteral::Real(value)) => Some(*value),
        dae::ExpressionOperation::Literal(dae::DaeLiteral::Integer(value)) => Some(*value as f64),
        dae::ExpressionOperation::Literal(dae::DaeLiteral::Boolean(value)) => truth(*value),
        dae::ExpressionOperation::Unary { operator, operand } => {
            let value = literal(view, operand, frame)?;
            match operator {
                dae::UnaryOperator::Plus => Some(value),
                dae::UnaryOperator::Negate => Some(-value),
                dae::UnaryOperator::Not => truth(value == 0.0),
            }
        }
        dae::ExpressionOperation::Binary { operator, lhs, rhs } => {
            let (lhs, rhs) = (literal(view, lhs, frame)?, literal(view, rhs, frame)?);
            match operator {
                dae::BinaryOperator::Add => Some(lhs + rhs),
                dae::BinaryOperator::Subtract => Some(lhs - rhs),
                dae::BinaryOperator::Multiply => Some(lhs * rhs),
                dae::BinaryOperator::Divide => Some(lhs / rhs),
                dae::BinaryOperator::Equal => truth(lhs == rhs),
                dae::BinaryOperator::NotEqual => truth(lhs != rhs),
                dae::BinaryOperator::Less => truth(lhs < rhs),
                dae::BinaryOperator::LessEqual => truth(lhs <= rhs),
                dae::BinaryOperator::Greater => truth(lhs > rhs),
                dae::BinaryOperator::GreaterEqual => truth(lhs >= rhs),
                dae::BinaryOperator::And => truth(lhs != 0.0 && rhs != 0.0),
                dae::BinaryOperator::Or => truth(lhs != 0.0 || rhs != 0.0),
                _ => None,
            }
        }
        _ => None,
    }
}
