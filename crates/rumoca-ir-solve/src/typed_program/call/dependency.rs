//! Constructor-derived dependencies on compact typed input coordinates.

mod accumulator;
pub(in crate::typed_program) mod affinity;
mod coordinates;
mod map_access;
#[cfg(test)]
mod operation_tests;
mod operations;
pub(in crate::typed_program) mod value_projection;

use super::{SolveOperation, SolveProgramConstructionError, SolvePureCallTableView, TypedProgram};
use crate::{SolvePureCallOwnerId, SolveRegisterId, SolveValueKind, SolveValueType};
use accumulator::Dependencies;
use coordinates::Coordinates;
use rumoca_core::Span;
use serde::{Deserialize, Serialize};

/// One input dependency of an issued pure-call output. Its coordinate relation
/// is derived from the checked body and matched against that owner on replay.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct SolveCallDependency {
    input: usize,
    /// None explicitly denotes dependence on the whole input leaf.
    coordinates: Option<Coordinates>,
}

/// Issued by the existing coordinate owner; never accepted from wire data.
#[derive(Clone)]
pub(crate) struct CheckedCallDependencyProjection {
    coordinates: Coordinates,
    output: SolveValueType,
    input: SolveValueType,
}

impl CheckedCallDependencyProjection {
    pub(crate) fn is_flat_identity(&self) -> bool {
        self.output.dimensions() == self.input.dimensions()
            && self.coordinates.is_identity(self.output.dimensions())
    }

    pub(crate) fn input_elements(&self, element: usize) -> Option<Vec<usize>> {
        self.coordinates
            .input_elements(self.output.dimensions(), element, self.input.dimensions())
    }
}

impl SolveCallDependency {
    pub(crate) fn checked_projection(
        &self,
        output: &SolveValueType,
        input: &SolveValueType,
    ) -> Option<CheckedCallDependencyProjection> {
        let coordinates = self.coordinates.as_ref()?;
        coordinates.complete_domain(output.dimensions(), input.dimensions())?;
        Some(CheckedCallDependencyProjection {
            coordinates: coordinates.clone(),
            output: output.clone(),
            input: input.clone(),
        })
    }
    #[must_use]
    pub const fn input_index(&self) -> usize {
        self.input
    }

    pub(crate) const fn is_whole_input(&self) -> bool {
        self.coordinates.is_none()
    }

    pub(crate) fn input_elements(
        &self,
        output: &SolveValueType,
        element: usize,
        input: &SolveValueType,
    ) -> Option<Vec<usize>> {
        self.coordinates
            .as_ref()?
            .input_elements(output.dimensions(), element, input.dimensions())
    }

    fn whole(input: usize) -> Self {
        Self {
            input,
            coordinates: None,
        }
    }

    fn remap(
        &self,
        access: &Coordinates,
        provenance: Span,
    ) -> Result<Self, SolveProgramConstructionError> {
        let coordinates = self
            .coordinates
            .as_ref()
            .map(|source| {
                source
                    .compose(access)
                    .ok_or(SolveProgramConstructionError::InvalidCallInterface { provenance })
            })
            .transpose()?;
        Ok(Self {
            input: self.input,
            coordinates,
        })
    }
}

pub(in crate::typed_program) fn derive(
    body: &TypedProgram,
    input_count: usize,
    output_count: usize,
    available: SolvePureCallTableView<'_>,
) -> Result<Box<[Box<[SolveCallDependency]>]>, SolveProgramConstructionError> {
    let mut slots = vec![Vec::new(); body.slots().len()];
    for (index, slot) in slots.iter_mut().take(input_count).enumerate() {
        slot.push(SolveCallDependency {
            input: index,
            coordinates: Some(Coordinates::identity(
                body.slots()[index].value_type().dimensions().len(),
            )),
        });
    }
    let mut registers = vec![Vec::new(); body.register_types().len()];
    let mut integers = map_access::IntegerValues::new(body.register_types().len());
    for (index, spanned) in body.operations().iter().enumerate() {
        let operation = spanned.operation();
        integers.track(operation);
        match operation {
            SolveOperation::Load { destination, slot } => {
                registers[destination.index()] = if body.slot_last_load_at(*slot, index) {
                    std::mem::take(&mut slots[slot.index()])
                } else {
                    slots[slot.index()].clone()
                };
            }
            SolveOperation::Store { slot, source } => {
                slots[slot.index()] = if body.register_moves_at(*source, index) {
                    std::mem::take(&mut registers[source.index()])
                } else {
                    registers[source.index()].clone()
                };
            }
            SolveOperation::Call {
                owner,
                arguments,
                destinations,
                ..
            } => {
                substitute_call(
                    *owner,
                    arguments,
                    destinations,
                    &mut registers,
                    available,
                    spanned.provenance(),
                )?;
            }
            SolveOperation::Map {
                domain,
                captures,
                destination,
                body: region,
            } => {
                let captured = captures
                    .iter()
                    .map(|capture| (registers[capture.index()].clone(), integers.value(*capture)))
                    .collect::<Vec<_>>();
                match map_access::derive(domain, &captured, region, spanned.provenance()) {
                    Some(dependencies) => registers[destination.index()] = dependencies,
                    None => {
                        operations::derive(body, operation, &mut registers, spanned.provenance())?;
                    }
                }
            }
            operation => operations::derive(body, operation, &mut registers, spanned.provenance())?,
        }
        retire_registers(body, operation, index, &mut registers);
    }
    Ok(slots
        .into_iter()
        .skip(input_count)
        .take(output_count)
        .map(Vec::into_boxed_slice)
        .collect())
}

/// Dependency summaries follow the checked program's value lifetimes. Retire
/// only after all operands were consumed, including repeated operands.
fn retire_registers(
    body: &TypedProgram,
    operation: &SolveOperation,
    index: usize,
    registers: &mut [Vec<SolveCallDependency>],
) {
    operation.visit_input_registers(|register| {
        if body.register_last_reads()[register.index()] == Some(index) {
            registers[register.index()] = Vec::new();
        }
    });
    operation.visit_output_registers(|register| {
        if body.register_last_reads()[register.index()].is_none() {
            registers[register.index()] = Vec::new();
        }
    });
}

/// Replace every coordinate relation by dependence on its whole input leaf.
pub(in crate::typed_program) fn widen(
    summaries: Box<[Box<[SolveCallDependency]>]>,
) -> Box<[Box<[SolveCallDependency]>]> {
    summaries
        .into_vec()
        .into_iter()
        .map(|output| {
            let mut whole = Dependencies::default();
            for dependency in output.iter() {
                whole.insert(SolveCallDependency::whole(dependency.input));
            }
            whole.finish().into_boxed_slice()
        })
        .collect()
}

fn substitute_call(
    owner: SolvePureCallOwnerId,
    arguments: &[SolveRegisterId],
    destinations: &[SolveRegisterId],
    registers: &mut [Vec<SolveCallDependency>],
    available: SolvePureCallTableView<'_>,
    provenance: Span,
) -> Result<(), SolveProgramConstructionError> {
    let error = || SolveProgramConstructionError::InvalidCallInterface { provenance };
    let summaries = available
        .get(owner.index() as usize)
        .map(|interface| interface.dependencies)
        .filter(|summaries| summaries.len() == destinations.len())
        .ok_or_else(error)?;
    for (destination, inputs) in destinations.iter().zip(summaries) {
        let mut dependencies = Dependencies::default();
        for input in inputs {
            let argument = arguments.get(input.input).ok_or_else(error)?;
            substitute_argument(
                &mut dependencies,
                &registers[argument.index()],
                input,
                provenance,
            )?;
        }
        registers[destination.index()] = dependencies.finish();
    }
    Ok(())
}

fn substitute_argument(
    dependencies: &mut Dependencies,
    argument: &[SolveCallDependency],
    input: &SolveCallDependency,
    provenance: Span,
) -> Result<(), SolveProgramConstructionError> {
    for source in argument {
        let dependency = match &input.coordinates {
            Some(access) => source.remap(access, provenance)?,
            None => SolveCallDependency::whole(source.input),
        };
        dependencies.insert(dependency);
    }
    Ok(())
}

fn finite_constant(value: SolveValueKind) -> bool {
    match value {
        SolveValueKind::Real32(bits) => f32::from_bits(bits).is_finite(),
        SolveValueKind::Real64(bits) => f64::from_bits(bits).is_finite(),
        SolveValueKind::Integer(_) | SolveValueKind::Boolean(_) => true,
    }
}
