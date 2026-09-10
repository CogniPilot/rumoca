//! Replay one map iteration into a compact fold carrying its primal and tangent.

use super::*;

impl<'primal, 'program> DirectionalBuilder<'primal, 'program> {
    pub(super) fn derive_map(
        &mut self,
        domain: &StructuredIndexDomain,
        captures: &[SolveRegisterId],
        destination: SolveRegisterId,
        body: &SolveProgramRegion,
        provenance: Span,
    ) -> Result<(), SolveProgramConstructionError> {
        let value_type = self.primal.register_types()[destination.index()].clone();
        let initial = empty_map_value(self.builder, &value_type, provenance)?;
        let initial = if is_real(&value_type) {
            vec![initial, initial]
        } else {
            vec![initial]
        };
        let captures = self.expanded_registers(captures, provenance)?;
        let available = self.available;
        let result = self.builder.fold(
            ordinal_domain(domain, provenance)?,
            &initial,
            &captures,
            provenance,
            |builder, carried, captures, binders, outputs| {
                let mut inputs = captures.to_vec();
                inputs.extend(source_binders(builder, domain, binders, provenance)?);
                let values = replay_iteration(builder, body, &inputs, available)?;
                store_iteration_results(
                    builder,
                    &body.outputs()[0],
                    carried,
                    &values,
                    binders,
                    outputs,
                    provenance,
                )
            },
        )?;
        self.bind_expanded(&[destination], &result, provenance)
    }
}

fn store_iteration_results<'program>(
    builder: &mut TypedProgramBuilder<'program>,
    body_type: &SolveValueType,
    carried: &[ProgramSlot<'program>],
    values: &[ProgramRegister<'program>],
    binders: &[ProgramSlot<'program>],
    outputs: &[ProgramSlot<'program>],
    provenance: Span,
) -> Result<(), SolveProgramConstructionError> {
    let coordinates = binders
        .iter()
        .map(|binder| builder.load(*binder, provenance))
        .collect::<Result<Vec<_>, _>>()?;
    let mut axes = coordinates
        .iter()
        .copied()
        .map(ProgramTensorViewAxis::Index)
        .collect::<Vec<_>>();
    axes.extend(
        body_type
            .dimensions()
            .iter()
            .map(|extent| ProgramTensorViewAxis::Span {
                origin: 0,
                extent: *extent,
            }),
    );
    for ((carried, value), output) in carried.iter().zip(values).zip(outputs) {
        let carried = builder.load(*carried, provenance)?;
        let updated = if body_type.dimensions().is_empty() {
            builder.update_element(carried, *value, &coordinates, provenance)?
        } else {
            builder.update_view(carried, *value, &axes, provenance)?
        };
        builder.store(*output, updated, provenance)?;
    }
    Ok(())
}

fn empty_map_value<'program>(
    builder: &mut TypedProgramBuilder<'program>,
    value_type: &SolveValueType,
    provenance: Span,
) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
    let value = match value_type.element_type() {
        SolveScalarType::Real { .. } => SolveValue::real(builder.arithmetic, 0.0),
        SolveScalarType::Integer(_) => SolveValue::integer(builder.arithmetic, 0)
            .map_err(|_| SolveProgramConstructionError::ProfileMismatch { provenance })?,
        SolveScalarType::Boolean => SolveValue::boolean(false),
    };
    let value = builder.constant(value, provenance)?;
    builder.fill(value, value_type.dimensions().to_vec(), provenance)
}

fn ordinal_domain(
    domain: &StructuredIndexDomain,
    provenance: Span,
) -> Result<StructuredIndexDomain, SolveProgramConstructionError> {
    let extents = domain
        .extents()
        .map_err(|_| SolveProgramConstructionError::InvalidMap { provenance })?;
    let mut ordinal = domain.clone();
    for (binder, extent) in ordinal.binders.iter_mut().zip(extents) {
        binder.lower = 1;
        binder.upper = i64::try_from(extent)
            .map_err(|_| SolveProgramConstructionError::InvalidMap { provenance })?;
        binder.step = 1;
    }
    Ok(ordinal)
}

fn source_binders<'program>(
    builder: &mut TypedProgramBuilder<'program>,
    domain: &StructuredIndexDomain,
    binders: &[ProgramSlot<'program>],
    provenance: Span,
) -> Result<Vec<ProgramSlot<'program>>, SolveProgramConstructionError> {
    domain
        .binders
        .iter()
        .zip(binders)
        .map(|(source, ordinal)| {
            let literal = |value| {
                SolveValue::integer(builder.arithmetic, value)
                    .map_err(|_| SolveProgramConstructionError::ProfileMismatch { provenance })
            };
            let one = literal(1)?;
            let start = literal(source.lower)?;
            let step = literal(source.step)?;
            let one = builder.constant(one, provenance)?;
            let start = builder.constant(start, provenance)?;
            let step = builder.constant(step, provenance)?;
            let ordinal = builder.load(*ordinal, provenance)?;
            let ordinal =
                builder.binary(SolveBinaryOperator::Subtract, ordinal, one, provenance)?;
            let offset =
                builder.binary(SolveBinaryOperator::Multiply, ordinal, step, provenance)?;
            let value = builder.binary(SolveBinaryOperator::Add, start, offset, provenance)?;
            let slot = builder.declare_slot(
                SolveValueType::scalar(SolveScalarType::integer(builder.arithmetic)),
                SolveStorageClass::MethodLocal,
                SolveSlotAccess::ReadWrite,
                provenance,
            )?;
            builder.store(slot, value, provenance)?;
            Ok(slot)
        })
        .collect()
}

fn replay_iteration<'program>(
    builder: &mut TypedProgramBuilder<'program>,
    region: &SolveProgramRegion,
    inputs: &[ProgramSlot<'program>],
    available: &[Option<SolvePureCallDirectionalInterface>],
) -> Result<Vec<ProgramRegister<'program>>, SolveProgramConstructionError> {
    let provenance = region.provenance();
    let slots = iteration_slots(builder, region, inputs)?;
    let result = slots[region.inputs().len()];
    let mut directional = DirectionalBuilder {
        primal: region.body(),
        builder,
        slots,
        registers: vec![None; region.body().register_types().len()],
        available,
    };
    directional.derive_all()?;
    let primal = builder.load(result.primal, provenance)?;
    let mut values = vec![primal];
    if let Some(tangent) = result.tangent {
        values.push(builder.load(tangent, provenance)?);
    }
    Ok(values)
}

fn iteration_slots<'program>(
    builder: &mut TypedProgramBuilder<'program>,
    region: &SolveProgramRegion,
    inputs: &[ProgramSlot<'program>],
) -> Result<Vec<Directional<ProgramSlot<'program>>>, SolveProgramConstructionError> {
    let provenance = region.provenance();
    let mut inputs = inputs.iter().copied();
    let mut slots = Vec::new();
    for (index, slot) in region.body().slots().iter().enumerate() {
        let real = is_real(slot.value_type()) && slot.storage() != SolveStorageClass::Constant;
        let (primal, tangent) = if index < region.inputs().len() {
            let mut next = || {
                inputs
                    .next()
                    .ok_or(SolveProgramConstructionError::InvalidMap { provenance })
            };
            (next()?, real.then(&mut next).transpose()?)
        } else {
            let mut declare = || {
                builder.declare_slot(
                    slot.value_type().clone(),
                    SolveStorageClass::MethodLocal,
                    slot.access(),
                    slot.provenance(),
                )
            };
            (declare()?, real.then(&mut declare).transpose()?)
        };
        slots.push(Directional { primal, tangent });
    }
    if inputs.next().is_some() {
        return Err(SolveProgramConstructionError::InvalidMap { provenance });
    }
    Ok(slots)
}
