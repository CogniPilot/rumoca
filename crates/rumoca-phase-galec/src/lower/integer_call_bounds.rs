//! Conservative Integer intervals through the existing exact call-context owner.
//! Only literal/binder arithmetic and its assignments-only pure call projection
//! qualify. Unknown inputs/defaults, mutable fold values and exhausted work issue
//! no fact. Calls remain emitted; this does not substitute or evaluate product code.
use super::*;
use rumoca_eval_dae::FunctionCallContext;

type Bounds = (i64, i64);
type Point<'dae> = std::collections::BTreeMap<dae::DomainBinderId<'dae>, i64>;

pub(super) fn derive<'dae>(view: dae::DaeView<'dae>, call: dae::ExprId<'dae>) -> Option<Bounds> {
    let mut binders = Vec::new();
    collect_binders(view, call, &mut binders, &mut 4096, 0)?;
    let mut points = 1u32;
    for binder in &binders {
        points = points.checked_mul(binder_extent(view, *binder)?)?;
        if points > 65_536 {
            return None;
        }
    }
    let mut fuel = 1_000_000;
    let mut result: Option<Bounds> = None;
    for ordinal in 0..points {
        let coordinates = point(view, &binders, ordinal, &mut fuel)?;
        let current = bounds(
            view,
            call,
            &FunctionCallContext::default(),
            &coordinates,
            &mut fuel,
            0,
        )?;
        result = Some(match result {
            None => current,
            Some(old) => (old.0.min(current.0), old.1.max(current.1)),
        });
    }
    result
}

fn bounds<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    context: &FunctionCallContext<'dae>,
    coordinates: &Point<'dae>,
    fuel: &mut usize,
    depth: usize,
) -> Option<Bounds> {
    *fuel = fuel.checked_sub(1)?;
    if depth > 64 {
        return None;
    }
    let node = view.expression(expression)?;
    if node.value_type().scalar_type() != dae::ScalarType::Integer
        || !node.value_type().dimensions().is_empty()
    {
        return None;
    }
    let context = context.scoped_to_expression(view, expression);
    let next = |id, context: &FunctionCallContext<'dae>, fuel: &mut usize| {
        bounds(view, id, context, coordinates, fuel, depth + 1)
    };
    match node.operation() {
        dae::ExpressionOperation::Literal(dae::DaeLiteral::Integer(n)) => Some((*n, *n)),
        dae::ExpressionOperation::Coordinate(dae::CoordinateView::Binder(binder)) => {
            let value = *coordinates.get(&binder)?;
            Some((value, value))
        }
        dae::ExpressionOperation::Coordinate(dae::CoordinateView::FunctionParameter(parameter)) => {
            next(context.parameter_argument(parameter)?, &context, fuel)
        }
        dae::ExpressionOperation::FunctionValue { definition, .. } => {
            next(definition.rhs(), &context, fuel)
        }
        dae::ExpressionOperation::Unary { operator, operand } => {
            let (a, b) = next(operand, &context, fuel)?;
            match operator {
                dae::UnaryOperator::Plus => Some((a, b)),
                dae::UnaryOperator::Negate => Some((b.checked_neg()?, a.checked_neg()?)),
                _ => None,
            }
        }
        dae::ExpressionOperation::Binary { operator, lhs, rhs } => {
            let a = next(lhs, &context, fuel)?;
            let b = next(rhs, &context, fuel)?;
            match operator {
                dae::BinaryOperator::Add => Some((a.0.checked_add(b.0)?, a.1.checked_add(b.1)?)),
                dae::BinaryOperator::Subtract => {
                    Some((a.0.checked_sub(b.1)?, a.1.checked_sub(b.0)?))
                }
                dae::BinaryOperator::Multiply => hull([
                    a.0.checked_mul(b.0)?,
                    a.0.checked_mul(b.1)?,
                    a.1.checked_mul(b.0)?,
                    a.1.checked_mul(b.1)?,
                ]),
                _ => None,
            }
        }
        dae::ExpressionOperation::Builtin { builtin, arguments } => {
            let a = next(arguments.get(0)?, &context, fuel)?;
            let b = next(arguments.get(1)?, &context, fuel)?;
            if arguments.len() != 2 {
                return None;
            }
            match builtin {
                dae::PureBuiltin::Min => Some((a.0.min(b.0), a.1.min(b.1))),
                dae::PureBuiltin::Max => Some((a.0.max(b.0), a.1.max(b.1))),
                dae::PureBuiltin::Div if b.0 > 0 || b.1 < 0 => hull([
                    a.0.checked_div(b.0)?,
                    a.0.checked_div(b.1)?,
                    a.1.checked_div(b.0)?,
                    a.1.checked_div(b.1)?,
                ]),
                _ => None,
            }
        }
        dae::ExpressionOperation::Call { .. } => {
            let (result, nested) = context.straight_line_call_result(view, expression)?;
            next(result, &nested, fuel)
        }
        _ => None,
    }
}

fn hull(values: [i64; 4]) -> Option<Bounds> {
    Some((*values.iter().min()?, *values.iter().max()?))
}

fn collect_binders<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    binders: &mut Vec<dae::DomainBinderId<'dae>>,
    fuel: &mut usize,
    depth: usize,
) -> Option<()> {
    *fuel = fuel.checked_sub(1)?;
    if depth > 64 {
        return None;
    }
    let children = match view.expression(expression)?.operation() {
        dae::ExpressionOperation::Literal(_) => Vec::new(),
        dae::ExpressionOperation::Coordinate(dae::CoordinateView::Binder(binder)) => {
            if !binders.contains(&binder) {
                binders.push(binder);
            }
            Vec::new()
        }
        dae::ExpressionOperation::FunctionValue { definition, .. } => vec![definition.rhs()],
        dae::ExpressionOperation::Unary { operand, .. } => vec![operand],
        dae::ExpressionOperation::Binary { lhs, rhs, .. } => vec![lhs, rhs],
        dae::ExpressionOperation::Builtin { arguments, .. }
        | dae::ExpressionOperation::Call { arguments, .. } => arguments.iter().collect(),
        _ => return None,
    };
    for child in children {
        collect_binders(view, child, binders, fuel, depth + 1)?;
    }
    Some(())
}

fn binder_extent<'dae>(view: dae::DaeView<'dae>, binder: dae::DomainBinderId<'dae>) -> Option<u32> {
    let b = view
        .domain(binder.domain())?
        .structured()
        .binders
        .get(binder.ordinal() as usize)?;
    if b.step == 0 || (b.step > 0 && b.upper < b.lower) || (b.step < 0 && b.upper > b.lower) {
        return None;
    }
    u32::try_from((i128::from(b.upper) - i128::from(b.lower)).abs() / i128::from(b.step).abs() + 1)
        .ok()
}

fn point<'dae>(
    view: dae::DaeView<'dae>,
    binders: &[dae::DomainBinderId<'dae>],
    ordinal: u32,
    fuel: &mut usize,
) -> Option<Point<'dae>> {
    let mut remainder = ordinal;
    let mut result = Point::new();
    for &binder in binders.iter().rev() {
        *fuel = fuel.checked_sub(1)?;
        let extent = binder_extent(view, binder)?;
        let offset = remainder % extent;
        remainder /= extent;
        let b = view
            .domain(binder.domain())?
            .structured()
            .binders
            .get(binder.ordinal() as usize)?;
        let value =
            i64::try_from(i128::from(b.lower) + i128::from(b.step) * i128::from(offset)).ok()?;
        result.insert(binder, value);
    }
    Some(result)
}
