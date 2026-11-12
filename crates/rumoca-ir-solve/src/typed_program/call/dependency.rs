//! Constructor-derived dependencies on compact typed input coordinates.

mod accumulator;
pub(in crate::typed_program) mod affinity;
mod assembly;
#[cfg(test)]
mod budget_tests;
mod coordinates;
mod map_access;
#[cfg(test)]
mod operation_tests;
mod operations;
mod regions;
mod substitution;
pub(in crate::typed_program) mod value_projection;

/// The followed work of derivations on this thread, for tests of the budgets.
#[cfg(test)]
pub(in crate::typed_program::call) mod followed_work {
    use super::regions;

    pub(in crate::typed_program::call) const PASS_BUDGET: usize = regions::FOLLOWED_PASS_BUDGET;
    pub(in crate::typed_program::call) const WORK_BUDGET: usize = regions::FOLLOWED_WORK_BUDGET;

    /// Forget every derivation so far.
    pub(in crate::typed_program::call) fn reset() {
        regions::FOLLOWED.with(|followed| {
            followed.set((regions::FollowedWork::NONE, regions::FollowedWork::NONE))
        });
    }

    /// A derivation starts: its followed work is counted from nothing.
    pub(super) fn start() {
        regions::FOLLOWED
            .with(|followed| followed.set((regions::FollowedWork::NONE, followed.get().1)));
    }

    /// The followed work of the derivation charged the most since [`reset`].
    pub(in crate::typed_program::call) fn most() -> regions::FollowedWork {
        regions::FOLLOWED.with(|followed| followed.get().1)
    }
}

use super::{SolveOperation, SolveProgramConstructionError, SolvePureCallTableView, TypedProgram};
use crate::{SolvePureCallOwnerId, SolveRegisterId, SolveValueKind, SolveValueType};
use accumulator::Dependencies;
use coordinates::Coordinates;
use rumoca_core::Span;
use serde::{Deserialize, Serialize};
use std::num::NonZeroUsize;
use std::ops::Range;

/// One dependency run of an issued pure-call output: the consecutive input
/// leaves `input..input + count`. Its coordinate relation is derived from the
/// checked body and matched against that owner on replay.
///
/// A whole-input run (`coordinates` is `None`) covers every scalar of each leaf
/// in the run. A coordinate relation holds, with the same relation, for each
/// leaf of its run. Runs of one summary are disjoint, ordered by first input,
/// and maximal; only the constructors below issue either form.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct SolveCallDependency {
    input: usize,
    count: NonZeroUsize,
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
    /// The consecutive input leaves this run covers.
    #[must_use]
    pub const fn inputs(&self) -> Range<usize> {
        self.input..self.input.saturating_add(self.count.get())
    }

    /// The first input leaf of this run.
    #[must_use]
    pub const fn first_input(&self) -> usize {
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

    /// Append whole-input leaf `leaf`, extending a run that ends at it.
    #[cfg(test)]
    fn push_whole_leaf(list: &mut Vec<Self>, leaf: usize) {
        match list.last_mut() {
            Some(last) if last.is_whole_input() && last.inputs().end == leaf => {
                *last = Self::whole_run(last.first_input(), leaf + 1);
            }
            _ => list.push(Self::whole(leaf)),
        }
    }

    #[cfg(test)]
    fn whole(input: usize) -> Self {
        Self::exact_inputs(input, NonZeroUsize::MIN, None)
    }

    /// One coordinate relation on exactly one leaf.
    fn exact(input: usize, coordinates: Coordinates) -> Self {
        Self::exact_inputs(input, NonZeroUsize::MIN, Some(coordinates))
    }

    /// One coordinate relation applied to each leaf of the non-empty range
    /// `start..end`.
    fn exact_run(start: usize, end: usize, coordinates: Coordinates) -> Self {
        let count = NonZeroUsize::MIN.saturating_add(end.saturating_sub(start).saturating_sub(1));
        Self::exact_inputs(start, count, Some(coordinates))
    }

    /// The whole-input run over the non-empty range `start..end`.
    fn whole_run(start: usize, end: usize) -> Self {
        let count = NonZeroUsize::MIN.saturating_add(end.saturating_sub(start).saturating_sub(1));
        Self::exact_inputs(start, count, None)
    }

    fn exact_inputs(input: usize, count: NonZeroUsize, coordinates: Option<Coordinates>) -> Self {
        Self {
            input,
            count,
            coordinates,
        }
    }

    /// Dependence on the whole of every leaf this run covers.
    fn widened(&self) -> Self {
        Self::exact_inputs(self.input, self.count, None)
    }

    /// [`Self::remap`] for may-depend summaries: a composition that would
    /// exceed [`coordinates::RELATION_TERM_BUDGET`] or
    /// [`coordinates::RELATION_COEFFICIENT_BUDGET`] is not built, and the
    /// dependency covers the whole input run instead.
    fn remap_may_depend(
        &self,
        access: &Coordinates,
        provenance: Span,
    ) -> Result<Self, SolveProgramConstructionError> {
        let coordinates = match &self.coordinates {
            Some(source) => may_depend_coordinates(source, access, provenance)?,
            None => None,
        };
        Ok(Self {
            input: self.input,
            count: self.count,
            coordinates,
        })
    }

    /// Whether composing `access` into this relation could exceed the term or
    /// coefficient budget; decided before any composition is allocated.
    fn composition_exceeds_budget(&self, access: &Coordinates) -> bool {
        self.coordinates
            .as_ref()
            .is_some_and(|coordinates| coordinates.composition_exceeds_budget(access))
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
            count: self.count,
            coordinates,
        })
    }
}

/// `source` composed with `access`, or `None` (the whole input) when the
/// composition could exceed the term or coefficient budget; that is decided
/// before any composition is allocated.
fn may_depend_coordinates(
    source: &Coordinates,
    access: &Coordinates,
    provenance: Span,
) -> Result<Option<Coordinates>, SolveProgramConstructionError> {
    if source.composition_exceeds_budget(access) {
        return Ok(None);
    }
    source
        .compose(access)
        .map(Some)
        .ok_or(SolveProgramConstructionError::InvalidCallInterface { provenance })
}

pub(in crate::typed_program) fn derive(
    body: &TypedProgram,
    input_count: usize,
    output_count: usize,
    available: SolvePureCallTableView<'_>,
) -> Result<Box<[Box<[SolveCallDependency]>]>, SolveProgramConstructionError> {
    // One budget for the whole derivation: every followed pass of every fold
    // in this body, however nested, draws on it.
    let work = std::cell::Cell::new(regions::FOLLOWED_WORK_BUDGET);
    #[cfg(test)]
    followed_work::start();
    Ok(derive_execution(body, input_count, output_count, available, &[], &work)?.summaries)
}

/// The summaries of one execution of a body and the Integer values it leaves
/// in its outputs.
struct Execution {
    summaries: Box<[Box<[SolveCallDependency]>]>,
    /// Per output: its value, when the known inputs fix it.
    integers: Vec<Option<i64>>,
}

/// [`derive`] for the executions whose leading inputs hold `known` Integer
/// values (`None` where an input is not fixed): one pass of a loop at its
/// binder values, a branch under known captures. Every summary it returns is
/// one `derive` would return for a body with those inputs replaced by
/// literals, so it is a summary of exactly those executions.
fn derive_execution(
    body: &TypedProgram,
    input_count: usize,
    output_count: usize,
    available: SolvePureCallTableView<'_>,
    known: &[Option<i64>],
    work: &std::cell::Cell<usize>,
) -> Result<Execution, SolveProgramConstructionError> {
    let mut slots = input_dependencies(body, input_count);
    let mut registers = vec![Vec::new(); body.register_types().len()];
    let mut assigned = vec![Vec::new(); body.assigned_state_types().len()];
    let mut integers = map_access::IntegerValues::new(
        body.register_types().len(),
        body.slots().len(),
        &known[..known.len().min(input_count)],
    );
    for (index, spanned) in body.operations().iter().enumerate() {
        let operation = spanned.operation();
        integers.track(operation);
        let context = regions::Context {
            body,
            available,
            provenance: spanned.provenance(),
            work,
        };
        if regions::derive(context, operation, &mut integers, &mut registers)?
            || assembly::derive(
                body,
                operation,
                &integers,
                &mut registers,
                spanned.provenance(),
            )?
        {
            retire_registers(body, operation, index, &mut registers);
            continue;
        }
        match operation {
            SolveOperation::AssignedState { operation } => {
                derive_assigned_dependencies(
                    body,
                    operation,
                    &mut registers,
                    &mut assigned,
                    spanned.provenance(),
                )?;
            }
            SolveOperation::AssignedConditional { program } => {
                derive_conditional_dependencies(program, &mut registers, &mut assigned);
            }
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
            SolveOperation::Fold {
                state_captures,
                destinations,
                ..
            } => {
                operations::derive(body, operation, &mut registers, spanned.provenance())?;
                derive_fold_state_dependencies(
                    destinations,
                    state_captures,
                    &mut registers,
                    &assigned,
                );
            }
            SolveOperation::Map { .. } => {
                derive_map(context, operation, &integers, &mut registers)?
            }
            operation => operations::derive(body, operation, &mut registers, spanned.provenance())?,
        }
        retire_registers(body, operation, index, &mut registers);
    }
    Ok(Execution {
        integers: (input_count..input_count + output_count)
            .map(|slot| integers.slot(slot))
            .collect(),
        summaries: output_summaries(body, input_count, output_count, slots),
    })
}

/// A map by its binders' access to its captures where that is exact, else
/// the whole-operand summary every other operation gets.
fn derive_map(
    context: regions::Context<'_>,
    operation: &SolveOperation,
    integers: &map_access::IntegerValues,
    registers: &mut [Vec<SolveCallDependency>],
) -> Result<(), SolveProgramConstructionError> {
    if let SolveOperation::Map {
        domain,
        captures,
        destination,
        body: region,
    } = operation
    {
        let captured = capture_dependencies(captures, registers, integers);
        if let Some(dependencies) =
            map_access::derive(domain, &captured, region, context.provenance)
        {
            registers[destination.index()] = dependencies;
            return Ok(());
        }
    }
    operations::derive(context.body, operation, registers, context.provenance)
}

fn capture_dependencies(
    captures: &[SolveRegisterId],
    registers: &[Vec<SolveCallDependency>],
    integers: &map_access::IntegerValues,
) -> Vec<(Vec<SolveCallDependency>, Option<i64>)> {
    captures
        .iter()
        .map(|capture| (registers[capture.index()].clone(), integers.value(*capture)))
        .collect()
}

fn output_summaries(
    body: &TypedProgram,
    input_count: usize,
    output_count: usize,
    slots: Vec<Vec<SolveCallDependency>>,
) -> Box<[Box<[SolveCallDependency]>]> {
    slots
        .into_iter()
        .skip(input_count)
        .take(output_count)
        .enumerate()
        .map(|(output, dependencies)| canonical_summary(body, input_count + output, dependencies))
        .collect()
}

fn input_dependencies(body: &TypedProgram, input_count: usize) -> Vec<Vec<SolveCallDependency>> {
    let mut slots = vec![Vec::new(); body.slots().len()];
    for (index, slot) in slots.iter_mut().take(input_count).enumerate() {
        slot.push(SolveCallDependency::exact(
            index,
            Coordinates::identity(body.slots()[index].value_type().dimensions().len()),
        ));
    }
    slots
}

fn canonical_summary(
    body: &TypedProgram,
    output: usize,
    dependencies: Vec<SolveCallDependency>,
) -> Box<[SolveCallDependency]> {
    let rank = body.slots()[output].value_type().dimensions().len();
    let mut canonical = Dependencies::default();
    for dependency in dependencies {
        let complete = dependency.coordinates.as_ref().is_some_and(|coordinates| {
            dependency.inputs().all(|leaf| {
                *coordinates
                    == operations::whole_access(rank, body.slots()[leaf].value_type().dimensions())
            })
        });
        canonical.insert(if complete {
            dependency.widened()
        } else {
            dependency
        });
    }
    canonical.finish().into_boxed_slice()
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
                whole.insert(dependency.widened());
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
    let interface = available.get(owner.index() as usize).ok_or_else(error)?;
    let summaries = interface.dependencies;
    if summaries.len() != destinations.len() {
        return Err(error());
    }
    let mut substitution = substitution::Substitution::default();
    for (output_index, (destination, inputs)) in destinations.iter().zip(summaries).enumerate() {
        let result = substitute_output(
            &mut substitution,
            inputs,
            interface.outputs[output_index].value_type(),
            interface.inputs,
            |leaf| arguments.get(leaf).map(|argument| argument.index()),
            registers,
            provenance,
        )?;
        registers[destination.index()] = result;
        substitution.assigned(destination.index());
    }
    Ok(())
}

/// One output of a summary over its arguments: each relation composed with
/// the relations of the argument it reads. `argument` names the entry of
/// `registers` that holds the dependencies of an input leaf. A call and one
/// followed pass of a loop are both this substitution.
fn substitute_output(
    substitution: &mut substitution::Substitution,
    summary: &[SolveCallDependency],
    output: &SolveValueType,
    inputs: &[SolveValueType],
    argument: impl Fn(usize) -> Option<usize>,
    registers: &[Vec<SolveCallDependency>],
    provenance: Span,
) -> Result<Vec<SolveCallDependency>, SolveProgramConstructionError> {
    let error = || SolveProgramConstructionError::InvalidCallInterface { provenance };
    if let Some(result) = substitution.finished(summary, output) {
        return Ok(result.clone());
    }
    let reads = summary
        .iter()
        .flat_map(|input| input.inputs().map(move |leaf| (input, leaf)))
        .map(|(input, leaf)| {
            let register = argument(leaf).ok_or_else(error)?;
            let input_type = inputs.get(leaf).ok_or_else(error)?;
            Ok((register, projected_argument(input, output, input_type)))
        })
        .collect::<Result<Vec<_>, _>>()?;
    let result = substitution.output(
        reads
            .iter()
            .map(|(register, projected)| substitution::Read {
                register: *register,
                access: projected.coordinates.as_ref(),
            }),
        registers,
        |source, access| may_depend_coordinates(source, access, provenance),
        error,
    )?;
    substitution.finish(summary, output, &result);
    Ok(result)
}

fn projected_argument(
    input: &SolveCallDependency,
    output_type: &SolveValueType,
    input_type: &SolveValueType,
) -> SolveCallDependency {
    let mut projected = input.clone();
    if projected.coordinates.is_none() {
        projected.coordinates = Some(operations::whole_access(
            output_type.dimensions().len(),
            input_type.dimensions(),
        ));
    }
    projected
}

fn finite_constant(value: SolveValueKind) -> bool {
    match value {
        SolveValueKind::Real32(bits) => f32::from_bits(bits).is_finite(),
        SolveValueKind::Real64(bits) => f64::from_bits(bits).is_finite(),
        SolveValueKind::Integer(_) | SolveValueKind::Boolean(_) => true,
    }
}

fn derive_assigned_dependencies(
    body: &TypedProgram,
    operation: &crate::SolveAssignedStateOperation,
    registers: &mut [Vec<SolveCallDependency>],
    states: &mut [Vec<SolveCallDependency>],
    provenance: Span,
) -> Result<(), SolveProgramConstructionError> {
    use crate::SolveAssignedStateOperation;
    match operation {
        SolveAssignedStateOperation::Present {
            destination,
            source,
        } => states[destination.index()] = registers[source.index()].clone(),
        SolveAssignedStateOperation::IsPresent {
            destination,
            source,
        } => {
            let access = operations::whole_access(
                0,
                body.assigned_state_types()[source.index()].dimensions(),
            );
            registers[destination.index()] = states[source.index()]
                .iter()
                .map(|dependency| dependency.remap(&access, provenance))
                .collect::<Result<_, _>>()?;
        }
        SolveAssignedStateOperation::ProjectChecked {
            destination,
            source,
            ..
        } => registers[destination.index()] = states[source.index()].clone(),
        SolveAssignedStateOperation::AllPresent {
            destination,
            sources,
        } => {
            let mut dependencies = Dependencies::default();
            for source in sources {
                for dependency in &states[source.index()] {
                    dependencies.insert(dependency.widened());
                }
            }
            registers[destination.index()] = dependencies.finish();
        }
        SolveAssignedStateOperation::Absent { .. }
        | SolveAssignedStateOperation::Input { .. }
        | SolveAssignedStateOperation::Output { .. } => {}
    }
    Ok(())
}

fn derive_conditional_dependencies(
    program: &crate::SolveAssignedConditional,
    registers: &mut [Vec<SolveCallDependency>],
    assigned: &mut [Vec<SolveCallDependency>],
) {
    let mut dependencies = Dependencies::default();
    for input in std::iter::once(program.condition()).chain(program.captures().iter().copied()) {
        for source in &registers[input.index()] {
            dependencies.insert(source.widened());
        }
    }
    for input in program.state_captures() {
        for source in &assigned[input.index()] {
            dependencies.insert(source.widened());
        }
    }
    let dependencies = dependencies.finish();
    for destination in program.destinations() {
        registers[destination.index()] = dependencies.clone();
    }
    for destination in program.state_destinations() {
        assigned[destination.index()] = dependencies.clone();
    }
}

fn derive_fold_state_dependencies(
    destinations: &[SolveRegisterId],
    state_captures: &[crate::SolveAssignedStateId],
    registers: &mut [Vec<SolveCallDependency>],
    assigned: &[Vec<SolveCallDependency>],
) {
    for destination in destinations {
        let mut dependencies = Dependencies::default();
        for source in &registers[destination.index()] {
            dependencies.insert(source.clone());
        }
        for state in state_captures {
            for source in &assigned[state.index()] {
                dependencies.insert(source.widened());
            }
        }
        registers[destination.index()] = dependencies.finish();
    }
}
