//! Compact coordinate relations for aggregate assembly and checked static updates.
use super::*;
use rumoca_core::AffineForm;

type Accesses = Vec<(SolveRegisterId, Coordinates)>;

pub(super) fn derive(
    body: &TypedProgram,
    operation: &SolveOperation,
    integers: &map_access::IntegerValues,
    registers: &mut [Vec<SolveCallDependency>],
    provenance: Span,
) -> Result<bool, SolveProgramConstructionError> {
    let selected = match operation {
        SolveOperation::ConstructAggregate {
            destination,
            elements,
        } => assembly(body, *destination, elements).map(|accesses| (*destination, accesses)),
        SolveOperation::UpdateElement {
            destination,
            aggregate,
            value,
            indices,
        } => update(body, *aggregate, *value, indices, integers)
            .map(|accesses| (*destination, accesses)),
        _ => None,
    };
    let Some((destination, accesses)) = selected else {
        return Ok(false);
    };
    let mut dependencies = Dependencies::default();
    for (input, access) in accesses {
        for source in &registers[input.index()] {
            dependencies.insert(source.remap_may_depend(&access, provenance)?);
        }
    }
    registers[destination.index()] = dependencies.finish();
    Ok(true)
}

fn flat_form(dimensions: &[u32], start: usize, count: usize) -> Option<AffineForm> {
    let mut form = AffineForm::constant(0, count);
    let mut stride = 1i64;
    for (axis, &extent) in dimensions.iter().enumerate().rev() {
        *form.coeffs.get_mut(start + axis)? = stride;
        stride = stride.checked_mul(i64::from(extent))?;
    }
    Some(form)
}

fn assembly(
    body: &TypedProgram,
    destination: SolveRegisterId,
    elements: &[SolveRegisterId],
) -> Option<Accesses> {
    let output = &body.register_types()[destination.index()];
    let rank = output.dimensions().len();
    let mut offset = 0i64;
    let mut accesses = Vec::with_capacity(elements.len());
    for &input in elements {
        let input_type = &body.register_types()[input.index()];
        let count = rank.checked_add(input_type.dimensions().len())?;
        let mut relation = flat_form(output.dimensions(), 0, count)?;
        relation.constant = offset.checked_neg()?;
        relation = relation
            .checked_add(&flat_form(input_type.dimensions(), rank, count)?.checked_scale(-1)?)?;
        accesses.push((
            input,
            operations::whole_access(rank, input_type.dimensions()).restricted(relation, 0, 0),
        ));
        offset = offset.checked_add(i64::from(input_type.scalar_count()))?;
    }
    (offset == i64::from(output.scalar_count())).then_some(accesses)
}

fn update(
    body: &TypedProgram,
    aggregate: SolveRegisterId,
    value: SolveRegisterId,
    indices: &[SolveRegisterId],
    integers: &map_access::IntegerValues,
) -> Option<Accesses> {
    let value_type = &body.register_types()[aggregate.index()];
    let dimensions = value_type.dimensions();
    if dimensions.len() != indices.len() {
        return None;
    }
    let mut offset = 0i64;
    for (&index, &extent) in indices.iter().zip(dimensions) {
        // Typed evaluation converts Modelica's one-based Integer coordinates.
        let coordinate = integers.value(index)?.checked_sub(1)?;
        if coordinate < 0 || coordinate >= i64::from(extent) {
            return None;
        }
        offset = offset
            .checked_mul(i64::from(extent))?
            .checked_add(coordinate)?;
    }
    let rank = dimensions.len();
    let flat = flat_form(dimensions, 0, rank)?;
    let mut accesses = Vec::new();
    let total = i64::from(value_type.scalar_count());
    if offset > 0 {
        accesses.push((
            aggregate,
            Coordinates::identity(rank).restricted(flat.clone(), 0, offset - 1),
        ));
    }
    if offset < total - 1 {
        accesses.push((
            aggregate,
            Coordinates::identity(rank).restricted(flat.clone(), offset + 1, total - 1),
        ));
    }
    accesses.push((
        value,
        Coordinates::access(rank, &[], Vec::new()).restricted(flat, offset, offset),
    ));
    Some(accesses)
}
