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

/// Most operations the followed passes of one loop may walk: the passes
/// times the operations one pass emits. Nested loops are bounded each by
/// their own passes.
pub(in crate::typed_program::call::dependency) const FOLLOWED_OPERATION_BUDGET: usize = 16_384;

/// The carried tuple after the last pass.
pub(super) struct Followed {
    pub carried: Vec<Vec<SolveCallDependency>>,
    /// Per carried value: its Integer value, when the passes fix it.
    pub integers: Vec<Option<i64>>,
}

/// Follow every pass of a fold without an early exit, or `None` when the
/// loop is outside the budgets.
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
    let weight = transition.body().emitted_weight().max(1);
    if passes > FOLLOWED_PASS_BUDGET || passes.saturating_mul(weight) > FOLLOWED_OPERATION_BUDGET {
        return Ok(None);
    }
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
        let execution = super::super::derive_execution(
            transition.body(),
            inputs.len(),
            initial.len(),
            context.available,
            &known,
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
