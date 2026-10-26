//! Keep equation domains in sync with occurrence-proven array projections.
//!
//! MLS §§10.6.1/10.6.9: `components.v` includes both the component array's
//! dimensions and the dimensions of each `v`. Before occurrence resolution,
//! equation shape inference may know only the component array's extents.
//! The pass that materializes the projection must retain its complete shape
//! in both the equation cardinality and its row-major template domain.

use super::{KnownFlatVars, collapse_index_expr};
use crate::FlattenError;
use rumoca_core::{ComprehensionScalarView, Expression, InstanceId, OpBinary};
use rumoca_ir_flat as flat;
use rustc_hash::FxHashMap;

type Dimensions = FxHashMap<InstanceId, Vec<usize>>;

pub(super) fn variable_dimensions(flat: &flat::Model) -> Dimensions {
    flat.variables
        .values()
        .filter_map(|variable| {
            let dims = variable
                .dims
                .iter()
                .map(|&n| usize::try_from(n).ok())
                .collect::<Option<_>>()?;
            Some((variable.instance_id, dims))
        })
        .collect()
}

pub(super) fn collapse_equations(
    equations: &mut [flat::Equation],
    families: &mut Vec<flat::StructuredEquationFamily>,
    known: &KnownFlatVars,
    dimensions: &Dimensions,
) -> Result<(), FlattenError> {
    for (row, equation) in equations.iter_mut().enumerate() {
        if !collapse_index_expr(&mut equation.residual, known) {
            continue;
        }
        let owner = families.iter().position(|family| {
            family
                .materialized_rows()
                .is_some_and(|rows| rows.contains(&row))
        });
        if owner.is_some_and(|index| !is_array_equation(&families[index])) {
            continue;
        }
        let Some(dims) = expression_dimensions(&equation.residual, dimensions) else {
            continue;
        };
        if dims.is_empty() {
            continue;
        }
        let count = dims
            .iter()
            .try_fold(1usize, |n, &extent| n.checked_mul(extent));
        let dims = dims
            .into_iter()
            .map(i64::try_from)
            .collect::<Result<Vec<_>, _>>();
        let (Some(count), Ok(dims)) = (count, dims) else {
            return Err(FlattenError::unsupported_equation(
                "projected array equation dimensions exceed the structured-domain index range",
                equation.span,
            ));
        };
        equation.scalar_count = count;
        let family = crate::equations::array_family::structured_array_equation_family(
            row,
            equation,
            Some(&dims),
        )?;
        match (owner, family) {
            (Some(index), Some(family)) => families[index] = family,
            (Some(index), None) => {
                families.remove(index);
            }
            (None, Some(family)) => families.push(family),
            (None, None) => {}
        }
    }
    Ok(())
}

fn is_array_equation(family: &flat::StructuredEquationFamily) -> bool {
    family.equations_per_point == 1
        && family.template.as_ref().is_some_and(|template| {
            template.scalar_view == ComprehensionScalarView::RowMajorProjection
        })
}

/// Only exact shapes of resolved occurrences and rectangular constructors
/// establish a count. Calls, slices, records, and unknown shapes retain their
/// owning analysis; no operand's shape is guessed from its displayed name.
fn expression_dimensions(expression: &Expression, dimensions: &Dimensions) -> Option<Vec<usize>> {
    match expression {
        Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => dimensions.get(&name.instance_id()?).cloned(),
        Expression::Array { elements, kind, .. } => {
            let shapes = elements
                .iter()
                .map(|element| expression_dimensions(element, dimensions))
                .collect::<Option<Vec<_>>>()?;
            kind.checked_dimensions(&shapes)
        }
        Expression::Binary {
            op: OpBinary::Sub,
            lhs,
            rhs,
            ..
        } => {
            let lhs = expression_dimensions(lhs, dimensions)?;
            let rhs = expression_dimensions(rhs, dimensions)?;
            (lhs == rhs).then_some(lhs)
        }
        Expression::Unary { rhs, .. } => expression_dimensions(rhs, dimensions),
        Expression::Literal { .. } => Some(Vec::new()),
        _ => None,
    }
}
