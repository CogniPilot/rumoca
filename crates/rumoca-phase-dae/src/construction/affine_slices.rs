//! Fixed-extent slice vectors from checked lexical Integer domains.

mod arithmetic;
#[cfg(test)]
mod tests;

use super::*;
use arithmetic::Affine;
pub(in crate::construction) use arithmetic::same_reference;

pub(super) struct SlicePlan<'a> {
    pub(super) extent: u32,
    start: &'a Expression,
    step: i64,
    span: Span,
}

/// Prove both original endpoints and their complete evaluation prefixes before
/// replacing a symbolic range by an index vector. Only lexical binders and
/// declaration constants are eligible; no runtime value is folded here.
pub(super) fn plan<'a>(
    expression: &'a Expression,
    shapes: &ShapeEnvironment,
) -> Option<SlicePlan<'a>> {
    let Expression::Range {
        start,
        step,
        end,
        span,
    } = expression
    else {
        return None;
    };
    let first = Affine::checked(start, shapes)?;
    let last = Affine::checked(end, shapes)?;
    if first.terms.is_empty() || !first.same_terms(&last) {
        return None;
    }
    let step = match step.as_deref() {
        None => 1,
        Some(expression) => {
            let value = Affine::checked(expression, shapes)?;
            if !value.terms.is_empty() {
                return None;
            }
            i64::try_from(value.constant).ok()?
        }
    };
    if step == 0 {
        return None;
    }
    let distance = last.constant.checked_sub(first.constant)?;
    let oriented = if step > 0 {
        distance
    } else {
        distance.checked_neg()?
    };
    let extent = if oriented < 0 {
        0
    } else {
        u32::try_from(
            oriented
                .checked_div(i128::from(step).abs())?
                .checked_add(1)?,
        )
        .ok()?
    };
    if extent > 0 {
        let offset = i64::from(extent - 1).checked_mul(step)?;
        first.bounds.0.checked_add(offset)?;
        first.bounds.1.checked_add(offset)?;
    }
    Some(SlicePlan {
        extent,
        start,
        step,
        span: *span,
    })
}

pub(super) fn lower<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: LoweringSymbols<'_, 'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    expression: &Expression,
) -> Result<Option<dae::ExprId<'dae>>, dae::DaeConstructionError> {
    if !matches!(expression, Expression::Range { .. }) || binders.is_empty() {
        return Ok(None);
    }
    // Reconstruct the shape witness from these exact checked lexical owners.
    // Arbitrary inferred runtime Integer intervals do not grant eligibility.
    let provenance = dae::DaeProvenance::source(expression_span(expression).expect("range span"))?;
    let mut shapes = symbols.shapes.clone();
    for (name, binder) in binders {
        let (lower, upper) =
            construction.domains(|domains| domains.binder_bounds(*binder, provenance))?;
        shapes.bind_slice_binder(name.clone(), lower, upper);
    }
    let Some(plan) = plan(expression, &shapes) else {
        return Ok(None);
    };
    let name = rumoca_core::affine_slice_binder_name(plan.span.start.0);
    if binders.contains_key(&VarName::new(&name)) {
        return Ok(None);
    }
    if plan.extent == 0 {
        let ty = construction.types(|types| {
            types.derived(
                dae::ValueType::array(dae::ScalarType::Integer, [0]),
                provenance,
            )
        })?;
        return construction
            .expressions(|expressions| expressions.at(provenance).empty_array(ty))
            .map(Some);
    }
    let domain = construction.domains(|domains| {
        domains.nested_in_scope(
            binders.values().copied(),
            StructuredIndexDomain {
                binders: vec![StructuredIndexBinder {
                    id: 0,
                    display_name: name.clone(),
                    lower: 1,
                    upper: i64::from(plan.extent),
                    step: 1,
                }],
            },
            provenance,
        )
    })?;
    let binder = construction.domains(|domains| domains.binder(domain, 0, provenance))?;
    let mut locals = binders.clone();
    locals.insert(VarName::new(&name), binder);
    let coordinate = plan.coordinate(&name);
    let body = lower_expression_scoped(construction, symbols, &locals, &coordinate, None)?;
    construction
        .expressions(|expressions| expressions.at(provenance).comprehension(domain, body))
        .map(Some)
}

pub(super) fn scoped_shapes(
    values: &ShapeEnvironment,
    domain: &StructuredIndexDomain,
    span: Span,
) -> Result<ShapeEnvironment, ToDaeError> {
    let extents = domain.extents().map_err(|error| {
        ToDaeError::unsupported_flat("structured equation domain", error.to_string(), span)
    })?;
    let mut values = values.clone();
    for (binder, extent) in domain.binders.iter().zip(extents) {
        let name = VarName::new(&binder.display_name);
        values.insert(name.clone(), Vec::new());
        if extent == 0 {
            continue;
        }
        let last = i128::from(binder.lower)
            + i128::try_from(extent - 1).expect("domain extent") * i128::from(binder.step);
        let last = i64::try_from(last).map_err(|_| {
            ToDaeError::unsupported_flat(
                "structured equation domain",
                "last binder value overflows Integer",
                span,
            )
        })?;
        values.bind_slice_binder(name, binder.lower, last);
    }
    Ok(values)
}

impl SlicePlan<'_> {
    fn coordinate(&self, binder: &str) -> Expression {
        let binary = |op, lhs, rhs| Expression::Binary {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span: self.span,
        };
        let literal = |value| Expression::Literal {
            value: Literal::Integer(value),
            span: self.span,
        };
        let index = Expression::VarRef {
            name: rumoca_core::Reference::generated(binder),
            subscripts: Vec::new(),
            span: self.span,
        };
        binary(
            OpBinary::Add,
            self.start.clone(),
            binary(
                OpBinary::Mul,
                binary(OpBinary::Sub, index, literal(1)),
                literal(self.step),
            ),
        )
    }
}
