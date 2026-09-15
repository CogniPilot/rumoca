//! Borrowed component selections from authored aggregate operations.

use super::*;

pub(super) fn array_scalar<'dae>(
    view: dae::DaeView<'dae>,
    elements: dae::ExpressionOperands<'dae>,
    scalar: usize,
) -> (dae::ExprId<'dae>, usize) {
    let first = elements.get(0).expect("checked array is nonempty");
    let width = view
        .expression(first)
        .unwrap()
        .value_type()
        .scalar_count()
        .unwrap();
    (
        elements
            .get(scalar / width)
            .expect("checked scalar selects an array element"),
        scalar % width,
    )
}

pub(super) fn concatenation_scalar<'dae>(
    view: dae::DaeView<'dae>,
    arguments: dae::ExpressionOperands<'dae>,
    axis: usize,
    result_dimensions: &[u32],
    scalar: usize,
) -> (dae::ExprId<'dae>, usize) {
    let mut coordinates = row_major_coordinates(result_dimensions, scalar)
        .expect("checked concatenation scalar belongs to its result shape");
    let selected = coordinates[axis];
    let mut offset = 0_u32;
    for argument in arguments.iter() {
        let dimensions = view.expression(argument).unwrap().value_type().dimensions();
        let extent = dimensions.get(axis).copied().unwrap_or(1);
        let end = offset
            .checked_add(extent)
            .expect("checked concatenation extent remains in the u32 domain");
        if selected < end {
            coordinates[axis] = selected - offset;
            let scalar = flatten_coordinates(dimensions, &coordinates[..dimensions.len()])
                .expect("checked promoted coordinate belongs to its operand shape");
            return (argument, scalar);
        }
        offset = end;
    }
    unreachable!("checked concatenation operands cover the result")
}
