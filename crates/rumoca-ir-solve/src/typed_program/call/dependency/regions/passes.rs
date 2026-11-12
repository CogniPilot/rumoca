//! A loop followed one pass at a time.
//!
//! One pass is one execution of the transition at known binder values, so
//! its summary names the elements that pass reads and writes where the
//! summary over all passes can only name whole arrays. Composing the passes
//! in domain order, each as a call of the transition with the carried tuple
//! of the pass before, gives the dependencies of the tuple after the last
//! pass: element `row` of a carry written at `row = f(i)` depends on what
//! pass `i` stored there and on nothing another pass read.
use super::*;

/// Most passes a loop is followed for. A longer loop keeps the summary over
/// all passes, which only adds dependencies.
pub(in crate::typed_program::call::dependency) const FOLLOWED_PASS_BUDGET: usize = 64;

/// Most operations the followed passes of one owner's derivation may walk,
/// over all its folds together. Each followed pass is charged the operations
/// one walk of its transition visits, nested regions included, so a nest of
/// short folds cannot multiply the work: a fold whose next pass costs more
/// than is left keeps the summary over all passes, which only adds
/// dependencies. What is left stays available to the folds after it, so the
/// passes followed in one derivation never cost more than the budget.
pub(in crate::typed_program::call::dependency) const FOLLOWED_WORK_BUDGET: usize = 65_536;

/// What the followed passes of one derivation came to, for tests of the
/// budgets.
#[cfg(test)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(in crate::typed_program::call) struct FollowedWork {
    /// Passes followed.
    pub passes: usize,
    /// Operations charged to the work budget for them.
    pub charged: usize,
}

#[cfg(test)]
impl FollowedWork {
    pub(in crate::typed_program::call) const NONE: Self = Self {
        passes: 0,
        charged: 0,
    };
}

// The followed work of the derivation running on this thread and of the
// derivation charged the most.
#[cfg(test)]
thread_local! {
    pub(in crate::typed_program::call) static FOLLOWED:
        std::cell::Cell<(FollowedWork, FollowedWork)> =
        const { std::cell::Cell::new((FollowedWork::NONE, FollowedWork::NONE)) };
}

/// The operations one walk of `program` visits, nested regions included.
fn walked_operations(program: &TypedProgram) -> usize {
    program
        .operations()
        .iter()
        .fold(0, |count: usize, spanned| {
            let nested = match spanned.operation() {
                SolveOperation::Conditional {
                    if_true, if_false, ..
                } => walked_operations(if_true.body())
                    .saturating_add(walked_operations(if_false.body())),
                SolveOperation::Fold {
                    transition,
                    continuation,
                    ..
                } => walked_operations(transition.body()).saturating_add(
                    continuation
                        .as_ref()
                        .map_or(0, |region| walked_operations(region.body())),
                ),
                SolveOperation::Map { body, .. } => walked_operations(body.body()),
                _ => 0,
            };
            count.saturating_add(1).saturating_add(nested)
        })
}

/// The carried tuple after the last pass.
pub(super) struct Followed {
    pub carried: Vec<Vec<SolveCallDependency>>,
    /// Per carried value: its Integer value, when the passes fix it.
    pub integers: Vec<Option<i64>>,
}

/// Follow every pass of a fold without an early exit, or `None` when the
/// loop has more passes than [`FOLLOWED_PASS_BUDGET`] or the owner's
/// [`FOLLOWED_WORK_BUDGET`] cannot pay for one of its passes.
pub(super) fn follow(
    context: Context<'_>,
    domain: &rumoca_core::StructuredIndexDomain,
    initial: &[SolveRegisterId],
    captures: &[SolveRegisterId],
    transition: &SolveProgramRegion,
    integers: &map_access::IntegerValues,
    registers: &[Vec<SolveCallDependency>],
) -> Result<Option<Followed>, SolveProgramConstructionError> {
    let error = || SolveProgramConstructionError::InvalidCallInterface {
        provenance: context.provenance,
    };
    let Ok(passes) = domain.scalar_count() else {
        return Ok(None);
    };
    if passes > FOLLOWED_PASS_BUDGET {
        return Ok(None);
    }
    let cost = walked_operations(transition.body()).max(1);
    let binders = initial.len() + captures.len();
    let inputs = transition.inputs();
    if inputs.len() != binders + domain.binders.len() || transition.outputs().len() != initial.len()
    {
        return Err(error());
    }
    // Arguments of a pass in input order: the carried tuple, the captures,
    // then the binders, which hold values and depend on nothing.
    let values = initial.iter().chain(captures);
    let mut arguments: Vec<_> = values
        .clone()
        .map(|value| registers[value.index()].clone())
        .collect();
    arguments.resize(inputs.len(), Vec::new());
    let mut known: Vec<_> = values.map(|value| integers.value(*value)).collect();
    known.resize(inputs.len(), None);
    for tuple in domain.index_tuple_iter().map_err(|_| error())? {
        for (binder, value) in known[binders..].iter_mut().zip(&tuple) {
            *binder = Some(*value);
        }
        // Charged before the pass is walked; a pass that costs more than is
        // left ends the following of this fold and charges nothing.
        let Some(remaining) = context.work.get().checked_sub(cost) else {
            return Ok(None);
        };
        context.work.set(remaining);
        #[cfg(test)]
        FOLLOWED.with(|followed| {
            let (mut current, most) = followed.get();
            current.passes += 1;
            current.charged += cost;
            let most = if current.charged > most.charged {
                current
            } else {
                most
            };
            followed.set((current, most));
        });
        let execution = super::super::derive_execution(
            transition.body(),
            inputs.len(),
            initial.len(),
            context.available,
            &known,
            context.work,
        )?;
        let mut substitution = Substitution::default();
        let next = execution
            .summaries
            .iter()
            .zip(transition.outputs())
            .map(|(summary, output)| {
                super::super::substitute_output(
                    &mut substitution,
                    summary,
                    output,
                    inputs,
                    Some,
                    &arguments,
                    context.provenance,
                )
            })
            .collect::<Result<Vec<_>, _>>()?;
        for (index, (carried, value)) in next.into_iter().zip(execution.integers).enumerate() {
            arguments[index] = carried;
            known[index] = value;
        }
    }
    arguments.truncate(initial.len());
    known.truncate(initial.len());
    Ok(Some(Followed {
        carried: arguments,
        integers: known,
    }))
}
