//! Checks shared by typed program construction: interface slots, loop
//! domains, region bodies and outputs, operator typing, and index bounds.

use super::*;

pub(super) fn declare_interface_slots<'program>(
    builder: &mut TypedProgramBuilder<'program>,
    value_types: &[SolveValueType],
    storage: SolveStorageClass,
    access: SolveSlotAccess,
    provenance: Span,
) -> Result<Vec<ProgramSlot<'program>>, SolveProgramConstructionError> {
    value_types
        .iter()
        .cloned()
        .map(|value_type| builder.declare_slot(value_type, storage, access, provenance))
        .collect()
}

pub(super) fn require_fold_domain(
    domain: &StructuredIndexDomain,
    arithmetic: SolveArithmeticProfile,
    provenance: Span,
) -> Result<(), SolveProgramConstructionError> {
    domain
        .validate()
        .map_err(|_| SolveProgramConstructionError::InvalidFold { provenance })?;
    let integers = arithmetic.integer_domain();
    if domain.binders.is_empty()
        || domain
            .binders
            .iter()
            .any(|binder| !integers.contains(binder.lower) || !integers.contains(binder.upper))
    {
        return Err(SolveProgramConstructionError::InvalidFold { provenance });
    }
    Ok(())
}

pub(super) fn require_map_domain(
    domain: &StructuredIndexDomain,
    arithmetic: SolveArithmeticProfile,
    provenance: Span,
) -> Result<(), SolveProgramConstructionError> {
    domain
        .validate()
        .map_err(|_| SolveProgramConstructionError::InvalidMap { provenance })?;
    let integers = arithmetic.integer_domain();
    if domain.binders.is_empty()
        || domain
            .binders
            .iter()
            .any(|binder| !integers.contains(binder.lower) || !integers.contains(binder.upper))
    {
        return Err(SolveProgramConstructionError::InvalidMap { provenance });
    }
    Ok(())
}

pub(in crate::typed_program) fn construct_region(
    inputs: Vec<SolveValueType>,
    outputs: Vec<SolveValueType>,
    body: TypedProgram,
    provenance: Span,
) -> Result<SolveProgramRegion, SolveProgramConstructionError> {
    if provenance.is_dummy()
        || outputs.is_empty()
        || inputs
            .iter()
            .chain(&outputs)
            .any(|value_type| !value_type.belongs_to(body.arithmetic()))
    {
        return Err(SolveProgramConstructionError::InvalidRegion { provenance });
    }
    validate_region_body(&body, &inputs, &outputs, provenance)?;
    Ok(SolveProgramRegion {
        inputs: inputs.into_boxed_slice(),
        outputs: outputs.into_boxed_slice(),
        body,
        provenance,
    })
}

fn validate_region_body(
    body: &TypedProgram,
    inputs: &[SolveValueType],
    outputs: &[SolveValueType],
    provenance: Span,
) -> Result<(), SolveProgramConstructionError> {
    let interface_count = inputs
        .len()
        .checked_add(outputs.len())
        .ok_or(SolveProgramConstructionError::IdentityOverflow { provenance })?;
    if body.slots().len() < interface_count
        || !slots_match(
            &body.slots()[..inputs.len()],
            inputs,
            SolveStorageClass::Input,
        )
        || !slots_match(
            &body.slots()[inputs.len()..interface_count],
            outputs,
            SolveStorageClass::Output,
        )
        || body.slots()[interface_count..]
            .iter()
            .any(|slot| slot.storage() != SolveStorageClass::MethodLocal)
    {
        return Err(SolveProgramConstructionError::InvalidRegion { provenance });
    }
    validate_region_outputs(body, inputs.len(), outputs.len(), provenance)
}

fn slots_match(
    slots: &[SolveSlot],
    expected: &[SolveValueType],
    storage: SolveStorageClass,
) -> bool {
    slots
        .iter()
        .zip(expected)
        .all(|(slot, expected)| slot.storage() == storage && slot.value_type() == expected)
}

fn validate_region_outputs(
    body: &TypedProgram,
    output_start: usize,
    output_count: usize,
    provenance: Span,
) -> Result<(), SolveProgramConstructionError> {
    let mut stores = vec![0usize; output_count];
    for operation in body.operations() {
        match operation.operation() {
            SolveOperation::Load { slot, .. }
                if (output_start..output_start + output_count).contains(&slot.index()) =>
            {
                return Err(SolveProgramConstructionError::InvalidRegion { provenance });
            }
            SolveOperation::Store { slot, .. } => {
                if let Some(output) = slot
                    .index()
                    .checked_sub(output_start)
                    .filter(|output| *output < output_count)
                {
                    stores[output] += 1;
                }
            }
            _ => {}
        }
    }
    if stores.iter().any(|count| *count != 1) {
        return Err(SolveProgramConstructionError::InvalidRegion { provenance });
    }
    Ok(())
}

pub(super) fn checked_ordinal(
    value: usize,
    provenance: Span,
) -> Result<u32, SolveProgramConstructionError> {
    u32::try_from(value).map_err(|_| SolveProgramConstructionError::IdentityOverflow { provenance })
}

pub(super) fn require_provenance(provenance: Span) -> Result<(), SolveProgramConstructionError> {
    if provenance.is_dummy() {
        return Err(SolveProgramConstructionError::MissingProvenance);
    }
    Ok(())
}

pub(super) fn binary_operator_accepts(
    operator: SolveBinaryOperator,
    scalar: SolveScalarType,
) -> bool {
    match operator {
        SolveBinaryOperator::Add
        | SolveBinaryOperator::Subtract
        | SolveBinaryOperator::Multiply => scalar.is_numeric(),
        // MLS §10.3.4 orders Boolean operands with `false < true`.
        SolveBinaryOperator::Min | SolveBinaryOperator::Max => {
            scalar.is_numeric() || scalar == SolveScalarType::Boolean
        }
        SolveBinaryOperator::Divide | SolveBinaryOperator::Power | SolveBinaryOperator::Atan2 => {
            matches!(scalar, SolveScalarType::Real { .. })
        }
        SolveBinaryOperator::And | SolveBinaryOperator::Or => scalar == SolveScalarType::Boolean,
        SolveBinaryOperator::IntegerQuotient
        | SolveBinaryOperator::IntegerModulo
        | SolveBinaryOperator::IntegerRemainder => {
            matches!(scalar, SolveScalarType::Integer(_))
        }
    }
}

pub(super) fn indices_in_bounds(dimensions: &[u32], indices: &[u32]) -> bool {
    dimensions.len() == indices.len()
        && dimensions
            .iter()
            .zip(indices)
            .all(|(extent, index)| *index < *extent)
}

pub(super) fn slice_in_bounds(base: &[u32], origin: &[u32], dimensions: &[u32]) -> bool {
    base.len() == origin.len()
        && base.len() == dimensions.len()
        && base
            .iter()
            .zip(origin)
            .zip(dimensions)
            .all(|((base, origin), extent)| {
                origin.checked_add(*extent).is_some_and(|end| end <= *base)
            })
}
