//! Immutable standalone execution source and construction-issued event bindings.
use super::*;
use crate::{ScalarProgramBlock, SolveModel};
use self_cell::self_cell;

/// A standalone execution source. This grants no FMI component capability.
#[derive(Clone, Debug)]
pub struct SolveExecutionSource(Arc<SolveModel>);

impl SolveExecutionSource {
    pub(super) fn from_shared(model: Arc<SolveModel>) -> Self {
        Self(model)
    }
    pub fn resolved_periodic_schedules_at(
        &self,
        start_time: f64,
    ) -> Result<Self, rumoca_core::ClockLatticeErrorKind> {
        Ok(match self.0.resolved_periodic_schedules_at(start_time)? {
            std::borrow::Cow::Borrowed(_) => self.clone(),
            std::borrow::Cow::Owned(model) => Self::new(model),
        })
    }
    pub fn new(model: SolveModel) -> Self {
        Self(Arc::new(model))
    }
    pub fn model(&self) -> &SolveModel {
        &self.0
    }
    pub fn event_program(
        &self,
        source: RetainedAssertionSource,
        index: usize,
    ) -> Option<RetainedEventProgram> {
        let owner = EventProgramOwner {
            source: self.clone(),
            block: source,
            index,
        };
        let cell = EventProgramCell::try_new(owner, |owner| {
            let block = event_block(owner.source.model(), owner.block);
            let program = block.programs().get(owner.index).ok_or(())?;
            let span = block.program_span(owner.index).ok_or(())?;
            AssertionProgramProof::derive(program).ok_or(())?;
            let mut invocations = Vec::new();
            collect_invocations(
                program,
                owner.source.model(),
                &mut Vec::new(),
                &mut invocations,
            )
            .ok_or(())?;
            Ok::<_, ()>(IssuedEventProgram {
                program,
                span,
                invocations,
            })
        })
        .ok()?;
        Some(RetainedEventProgram { cell })
    }
}

fn event_block(model: &SolveModel, source: RetainedAssertionSource) -> &ScalarProgramBlock {
    match source {
        RetainedAssertionSource::RootConditions => &model.problem.events.root_conditions,
        RetainedAssertionSource::ActionConditions => &model.problem.events.action_conditions,
    }
}

struct EventProgramOwner {
    source: SolveExecutionSource,
    block: RetainedAssertionSource,
    index: usize,
}
struct EventInvocationBinding<'source> {
    path: Vec<(usize, usize)>,
    checked: CheckedAssertionInvocation<'source>,
}
struct IssuedEventProgram<'source> {
    program: &'source [LinearOp],
    span: rumoca_core::Span,
    invocations: Vec<EventInvocationBinding<'source>>,
}
self_cell!(
    struct EventProgramCell {
        owner: EventProgramOwner,
        #[covariant]
        dependent: IssuedEventProgram,
    }
);

/// A program and its exact source/action projections, checked once before compilation.
/// No bare action, program, carrier bytes or diagnostic can reconstruct this owner.
pub struct RetainedEventProgram {
    cell: EventProgramCell,
}

impl RetainedEventProgram {
    pub fn source(&self) -> &SolveExecutionSource {
        &self.cell.borrow_owner().source
    }
    pub fn program(&self) -> &[LinearOp] {
        self.cell.borrow_dependent().program
    }
    pub fn span(&self) -> rumoca_core::Span {
        self.cell.borrow_dependent().span
    }
    pub fn table(&self) -> &SolvePureCallTable {
        &self.source().model().pure_calls
    }
    pub fn has_observations(&self) -> bool {
        !self.cell.borrow_dependent().invocations.is_empty()
    }
    pub fn invocation_at(
        &self,
        path: &[(usize, usize)],
        operation: usize,
    ) -> Option<RetainedEventInvocation<'_>> {
        self.cell
            .borrow_dependent()
            .invocations
            .iter()
            .position(|binding| binding.path == path && binding.checked.operation() == operation)
            .and_then(|index| self.invocation_by_index(index))
    }
    pub fn invocation_by_index(&self, index: usize) -> Option<RetainedEventInvocation<'_>> {
        Some(RetainedEventInvocation {
            binding: self.cell.borrow_dependent().invocations.get(index)?,
            index,
        })
    }
    pub fn invocations(&self) -> impl ExactSizeIterator<Item = RetainedEventInvocation<'_>> {
        self.cell
            .borrow_dependent()
            .invocations
            .iter()
            .enumerate()
            .map(|(index, binding)| RetainedEventInvocation { binding, index })
    }
}

pub struct RetainedEventInvocation<'program> {
    binding: &'program EventInvocationBinding<'program>,
    index: usize,
}
impl RetainedEventInvocation<'_> {
    pub fn index(&self) -> usize {
        self.index
    }
    pub fn region_path(&self) -> &[(usize, usize)] {
        &self.binding.path
    }
    pub fn operation(&self) -> usize {
        self.binding.checked.operation()
    }
    pub fn owner(&self) -> &SolvePureCallOwner {
        self.binding.checked.owner()
    }
    pub fn projections(
        &self,
    ) -> impl ExactSizeIterator<Item = retained::RetainedAssertionAction<'_>> {
        self.binding
            .checked
            .projections()
            .iter()
            .map(retained::RetainedAssertionAction::checked)
    }
    pub fn checked(&self) -> &CheckedAssertionInvocation<'_> {
        &self.binding.checked
    }
}

/// Region ordinals are structural coordinates of the exact original source.
fn child_regions(operation: &LinearOp) -> Vec<&[LinearOp]> {
    match operation {
        LinearOp::FunctionConditional { program, .. } => program
            .arms
            .iter()
            .flat_map(|arm| [arm.condition.as_slice(), arm.result.as_slice()])
            .chain(std::iter::once(program.fallback.as_slice()))
            .collect(),
        LinearOp::FunctionFold { program, .. }
        | LinearOp::GuardedFunctionFold { program, .. }
        | LinearOp::StoreOutputFunctionFold { program, .. } => program.regions().collect(),
        _ => Vec::new(),
    }
}
fn collect_invocations<'source>(
    program: &'source [LinearOp],
    model: &'source SolveModel,
    path: &mut Vec<(usize, usize)>,
    bindings: &mut Vec<EventInvocationBinding<'source>>,
) -> Option<()> {
    for (operation, op) in program.iter().enumerate() {
        if matches!(op, LinearOp::PureCallObservation { .. }) {
            // The root's register-flow proof covers each region in its real capture/fold context.
            let checked = CheckedAssertionProgram { program }.invocation(
                &model.pure_calls,
                operation,
                &model.problem.events.actions,
            )?;
            bindings.push(EventInvocationBinding {
                path: path.clone(),
                checked,
            });
        }
        for (region, child) in child_regions(op).into_iter().enumerate() {
            path.push((operation, region));
            collect_invocations(child, model, path, bindings)?;
            path.pop();
        }
    }
    Some(())
}
