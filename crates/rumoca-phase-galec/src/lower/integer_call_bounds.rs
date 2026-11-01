//! Conservative Integer intervals through the existing exact call-context owner.
//! Only literal/binder arithmetic, parameters every call site bounds, and their
//! assignments-only pure call projection qualify. Unknown inputs/defaults,
//! mutable fold values and exhausted work issue no fact. Calls remain emitted;
//! this does not substitute or evaluate product code.
use super::*;
use rumoca_eval_dae::FunctionCallContext;
use std::collections::BTreeMap;

type Bounds = (i64, i64);
type Point<'dae> = BTreeMap<dae::DomainBinderId<'dae>, i64>;
/// Every call of each function, keyed by its index: the callee and the
/// arguments of each call.
type CallSites<'dae> = BTreeMap<u32, (dae::FunctionId<'dae>, Vec<dae::ExpressionOperands<'dae>>)>;

/// Proven ranges of scalar Integer function parameters, from their call sites.
///
/// A parameter's range is the hull of the ranges every one of its call sites
/// passes. A call is the only way a DAE expression names a function, so the
/// census over every call node sees every value an emitted body can receive.
/// That is what lets a body index by its own parameter: a packed position
/// `index(i, j, n)` is in range only for the `n` its callers actually pass.
///
/// A call site whose argument has no proven range leaves its parameter
/// without one. The census repeats until no new range appears, so a range
/// passes through a caller's own parameter; each pass uses only ranges the
/// previous one proved, so stopping early, at the round or work limit, keeps
/// every range already issued. A recursive call passing its own parameter
/// never proves one.
pub(super) struct ParameterRanges {
    ranges: BTreeMap<(u32, u32), Bounds>,
}

impl ParameterRanges {
    /// No proven ranges: what a lowering without a census knows.
    pub(super) const fn none() -> Self {
        Self {
            ranges: BTreeMap::new(),
        }
    }

    pub(super) fn derive<'dae>(view: dae::DaeView<'dae>) -> Self {
        let mut sites = CallSites::new();
        for index in 0..view.expression_count() {
            let Some(node) = view.expression_id(index).and_then(|id| view.expression(id)) else {
                continue;
            };
            if let dae::ExpressionOperation::Call {
                function,
                arguments,
                ..
            } = node.operation()
            {
                sites
                    .entry(function.index())
                    .or_insert_with(|| (function, Vec::new()))
                    .1
                    .push(arguments);
            }
        }
        let mut census = Self::none();
        let mut fuel = 4_000_000;
        for _ in 0..64 {
            let proven = census.round(view, &sites, &mut fuel);
            if proven == census.ranges {
                break;
            }
            census.ranges = proven;
            if fuel == 0 {
                break;
            }
        }
        census
    }

    /// Every parameter range provable from the ranges this census holds.
    fn round<'dae>(
        &self,
        view: dae::DaeView<'dae>,
        sites: &CallSites<'dae>,
        fuel: &mut usize,
    ) -> BTreeMap<(u32, u32), Bounds> {
        let mut proven = BTreeMap::new();
        for (&key, (function, calls)) in sites {
            let parameters = view.function(*function).map_or(0, |f| f.parameters().len());
            proven.extend((0..parameters).filter_map(|ordinal| {
                Some((
                    (key, ordinal as u32),
                    self.hull(view, calls, ordinal, fuel)?,
                ))
            }));
        }
        proven
    }

    /// The hull of what every call passes at `ordinal`, if each is proven.
    fn hull<'dae>(
        &self,
        view: dae::DaeView<'dae>,
        calls: &[dae::ExpressionOperands<'dae>],
        ordinal: usize,
        fuel: &mut usize,
    ) -> Option<Bounds> {
        calls
            .iter()
            .try_fold(None, |hull: Option<Bounds>, arguments| {
                let range = enumerate(view, arguments.get(ordinal)?, self, fuel)?;
                Some(Some(hull.map_or(range, |old| union(old, range))))
            })?
    }

    fn get(&self, parameter: dae::FunctionParameterId<'_>) -> Option<Bounds> {
        self.ranges
            .get(&(parameter.function().index(), parameter.ordinal()))
            .copied()
    }
}

pub(super) fn derive<'dae>(
    view: dae::DaeView<'dae>,
    call: dae::ExprId<'dae>,
    parameters: &ParameterRanges,
) -> Option<Bounds> {
    enumerate(view, call, parameters, &mut 1_000_000)
}

/// The hull of `expression` over every point of the binders it reads.
fn enumerate<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    parameters: &ParameterRanges,
    fuel: &mut usize,
) -> Option<Bounds> {
    let mut binders = Vec::new();
    collect_binders(view, expression, &mut binders, &mut 4096, 0)?;
    let mut points = 1u32;
    for binder in &binders {
        points = points.checked_mul(binder_extent(view, *binder)?)?;
        if points > 65_536 {
            return None;
        }
    }
    let mut result: Option<Bounds> = None;
    for ordinal in 0..points {
        let coordinates = point(view, &binders, ordinal, fuel)?;
        let current = bounds(
            view,
            expression,
            &FunctionCallContext::default(),
            &Evaluation {
                coordinates: &coordinates,
                parameters,
            },
            fuel,
            0,
        )?;
        result = Some(result.map_or(current, |old| union(old, current)));
    }
    result
}

fn union(a: Bounds, b: Bounds) -> Bounds {
    (a.0.min(b.0), a.1.max(b.1))
}

/// What an evaluation point fixes: the binders' values, and the ranges of
/// parameters no call frame binds.
struct Evaluation<'p, 'dae> {
    coordinates: &'p Point<'dae>,
    parameters: &'p ParameterRanges,
}

fn bounds<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    context: &FunctionCallContext<'dae>,
    at: &Evaluation<'_, 'dae>,
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
        bounds(view, id, context, at, fuel, depth + 1)
    };
    match node.operation() {
        dae::ExpressionOperation::Literal(dae::DaeLiteral::Integer(n)) => Some((*n, *n)),
        dae::ExpressionOperation::Coordinate(dae::CoordinateView::Binder(binder)) => {
            let value = *at.coordinates.get(&binder)?;
            Some((value, value))
        }
        dae::ExpressionOperation::Coordinate(dae::CoordinateView::FunctionParameter(parameter)) => {
            match context.parameter_argument(parameter) {
                Some(argument) => next(argument, &context, fuel),
                // No frame binds it: a parameter of the body being proven,
                // which takes only what its call sites pass.
                None => at.parameters.get(parameter),
            }
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
        dae::ExpressionOperation::Coordinate(dae::CoordinateView::FunctionParameter(_)) => {
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
