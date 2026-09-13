//! Constructor-derived dependencies on compact typed input coordinates.

pub(in crate::typed_program) mod affinity;
mod coordinates;
mod operations;
pub(in crate::typed_program) mod value_projection;

use super::{SolveOperation, SolveProgramConstructionError, SolvePureCallTableView, TypedProgram};
use crate::{SolvePureCallOwnerId, SolveRegisterId, SolveValueKind, SolveValueType};
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

impl SolveCallDependency {
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
    for spanned in body.operations() {
        let operation = spanned.operation();
        match operation {
            SolveOperation::Load { destination, slot } => {
                registers[destination.index()] = slots[slot.index()].clone();
            }
            SolveOperation::Store { slot, source } => {
                slots[slot.index()] = registers[source.index()].clone();
            }
            SolveOperation::Call {
                owner,
                arguments,
                destinations,
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
            operation => operations::derive(body, operation, &mut registers, spanned.provenance())?,
        }
    }
    Ok(slots
        .into_iter()
        .skip(input_count)
        .take(output_count)
        .map(Vec::into_boxed_slice)
        .collect())
}

fn insert(target: &mut Vec<SolveCallDependency>, dependency: SolveCallDependency) {
    if target
        .iter()
        .any(|prior| prior.input == dependency.input && prior.is_whole_input())
    {
        return;
    }
    if dependency.is_whole_input() {
        target.retain(|prior| prior.input != dependency.input);
    }
    if !target.contains(&dependency) {
        target.push(dependency);
    }
    target.sort_by_key(SolveCallDependency::input_index);
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
        let mut dependencies = Vec::new();
        for input in inputs {
            let argument = arguments.get(input.input).ok_or_else(error)?;
            substitute_argument(
                &mut dependencies,
                &registers[argument.index()],
                input,
                provenance,
            )?;
        }
        registers[destination.index()] = dependencies;
    }
    Ok(())
}

fn substitute_argument(
    dependencies: &mut Vec<SolveCallDependency>,
    argument: &[SolveCallDependency],
    input: &SolveCallDependency,
    provenance: Span,
) -> Result<(), SolveProgramConstructionError> {
    for source in argument {
        let dependency = match &input.coordinates {
            Some(access) => source.remap(access, provenance)?,
            None => SolveCallDependency::whole(source.input),
        };
        insert(dependencies, dependency);
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
