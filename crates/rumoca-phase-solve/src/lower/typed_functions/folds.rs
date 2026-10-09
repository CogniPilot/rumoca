//! Lowering of one checked DAE function loop into a compact typed fold.
//!
//! The transition is built in three separable acts, which is why they are three
//! items here: the DAE's fold record is checked into a [`FoldTransition`], the
//! enclosing scope's entry values become the carried tuple and its layout, and
//! the region body reconstructs one iteration from the captures, the carried
//! tuple, and the binders. Only the first act can reject a wire, only the
//! second can demand a value from the enclosing scope, and only the third runs
//! inside the region.

use std::ops::Range;

use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

use super::{
    AssertionSlot, EnvironmentLayout, ExpressionLowerer, LoweredValue, RegionContext,
    load_region_lowerer,
};

/// One fold's checked transition record.
///
/// Every field is read from the DAE's own fold entry and cross-checked against
/// the others before any register exists, so the region builder below never has
/// to re-validate arity or target agreement.
struct FoldTransition<'dae> {
    /// Reaching definition of each carried target in the enclosing scope.
    initial_definitions: Vec<dae::FunctionDefinitionView<'dae>>,
    /// Right-hand side each carried target ends an iteration at.
    update_expressions: Vec<dae::ExprId<'dae>>,
    /// Definition identity of each per-iteration entry parameter.
    parameter_definitions: Vec<dae::FunctionDefinitionId<'dae>>,
    /// Definition identity of each end-of-iteration update.
    update_definitions: Vec<dae::FunctionDefinitionId<'dae>>,
    /// A bounded `while` fold's predicate over the carried state.
    continuation: Option<dae::ExprId<'dae>>,
    domain: dae::DomainId<'dae>,
}

/// Carried tuple position, its declared type, and its span of flat leaves.
type CarriedLayout<'dae> = Vec<(u32, dae::ValueTypeId<'dae>, Range<usize>)>;

/// Everything one iteration of a fold needs that is fixed before it runs.
#[derive(Clone, Copy)]
struct FoldIteration<'a, 'dae> {
    fold: dae::FunctionFoldId<'dae>,
    transition: &'a FoldTransition<'dae>,
    environment: &'a EnvironmentLayout<'dae>,
    context: &'a RegionContext<'dae>,
    carried_layout: &'a CarriedLayout<'dae>,
    /// Assertion slots of calls the iterations reach, carried after the
    /// targets (see [`FoldAssertions`]).
    assertions: &'a FoldAssertions,
    source: Option<&'a super::assertions::LoopStatements<'dae>>,
    provenance: rumoca_core::Span,
}

/// Diagnostic slots a fold's iterations reach. The in-frame observer owns
/// their failures and captures; the normal carried tuple keeps inactive cells.
struct FoldAssertions {
    slots: Vec<usize>,
}

impl<'program, 'dae> ExpressionLowerer<'_, 'program, 'dae> {
    pub(super) fn function_fold(
        &mut self,
        fold: dae::FunctionFoldId<'dae>,
        provenance: rumoca_core::Span,
    ) -> Result<Vec<LoweredValue<'program, 'dae>>, solve::SolveProgramConstructionError> {
        if let Some(values) = self.fold_values.get(&fold) {
            return Ok(values.clone());
        }
        let transition = checked_transition(self.view, fold, provenance)?;
        let source = self.loop_statements.get(&fold).cloned();
        let mut roots = source.as_ref().map_or_else(
            || transition.update_expressions.clone(),
            |source| super::assertions::statement_roots(source.statements.clone()),
        );
        if let Some(source) = &source {
            for declaration in &self.direct_assertions[source.assertions.clone()] {
                roots.extend(declaration.message_values.iter().map(|&(value, _)| value));
            }
        }
        let (mut initial_flat, carried_layout) =
            self.carried_entry_values(&transition, provenance)?;
        let assertions =
            self.fold_assertions(&roots, source.as_ref(), &mut initial_flat, provenance)?;
        if initial_flat.is_empty() {
            self.fold_values.insert(fold, Vec::new());
            return Ok(Vec::new());
        }
        let domain = self
            .view
            .domain(transition.domain)
            .ok_or(solve::SolveProgramConstructionError::WireMismatch)?
            .structured()
            .clone();
        // The DAE issued this loop's body its own definition scope. Asking it
        // which definitions the iteration computes - rather than inferring the
        // boundary from definition ordinals - is what keeps the captured
        // environment exact under any future arena order.
        let scope = self
            .view
            .function_scope(fold.function(), Some(fold))
            .ok_or(solve::SolveProgramConstructionError::WireMismatch)?;
        let (captures, environment) = self.capture_environment_for_fold(
            roots.iter().copied().chain(transition.continuation),
            fold,
            scope,
        )?;
        let context = self.region_context();
        let iteration = FoldIteration {
            fold,
            transition: &transition,
            environment: &environment,
            context: &context,
            carried_layout: &carried_layout,
            assertions: &assertions,
            source: source.as_ref(),
            provenance,
        };
        let destinations = match transition.continuation {
            Some(continuation) => self.builder.fold_while(
                domain,
                &initial_flat,
                &captures,
                provenance,
                move |builder, carried, captures, outputs| {
                    iteration.lower_continuation(
                        builder,
                        continuation,
                        (carried, captures),
                        outputs,
                    )
                },
                move |builder, carried, captures, binders, outputs| {
                    iteration.lower(builder, carried, captures, binders, outputs)
                },
            )?,
            None => self.builder.fold(
                domain,
                &initial_flat,
                &captures,
                provenance,
                move |builder, carried, captures, binders, outputs| {
                    iteration.lower(builder, carried, captures, binders, outputs)
                },
            )?,
        };
        for slot in &assertions.slots {
            let value = self.inactive_slot(*slot, provenance)?;
            let published = self
                .predicate_values
                .get_mut(*slot)
                .ok_or(solve::SolveProgramConstructionError::InvalidCallOutput { provenance })?;
            if published.replace(value).is_some() {
                return Err(solve::SolveProgramConstructionError::InvalidCallOutput { provenance });
            }
        }
        let values = carried_layout
            .iter()
            .map(|(_, value_type, range)| LoweredValue {
                value_type: *value_type,
                leaves: destinations[range.clone()].to_vec(),
            })
            .collect::<Vec<_>>();
        self.fold_values.insert(fold, values.clone());
        Ok(values)
    }

    /// Carry the assertion slots of every call the loop body reaches, each
    /// entering the loop at its unselected value.
    fn fold_assertions(
        &mut self,
        roots: &[dae::ExprId<'dae>],
        source: Option<&super::assertions::LoopStatements<'dae>>,
        initial_flat: &mut Vec<solve::ProgramRegister<'program>>,
        provenance: rumoca_core::Span,
    ) -> Result<FoldAssertions, solve::SolveProgramConstructionError> {
        let mut slots = source.map_or_else(Vec::new, |source| source.assertions.clone().collect());
        if let Some(source) = source {
            slots.extend(
                self.assertion_slots
                    .iter()
                    .enumerate()
                    .filter_map(|(slot, kind)| {
                        message_slot_in_range(slot, kind, &source.assertions)
                    }),
            );
        }
        slots.extend(self.unissued_assertion_slots(roots.iter().copied()));
        for slot in &slots {
            let kind =
                self.assertion_slots.get(*slot).cloned().ok_or(
                    solve::SolveProgramConstructionError::InvalidCallInterface { provenance },
                )?;
            // A message value is carried with its own predicate, which every
            // call that publishes the value also publishes.
            if let AssertionSlot::MessageValue { predicate, .. } = kind
                && slot
                    .checked_sub(predicate)
                    .is_none_or(|predicate| !slots.contains(&predicate))
            {
                return Err(solve::SolveProgramConstructionError::InvalidCallInterface {
                    provenance,
                });
            }
            initial_flat.push(kind.unselected(self.builder, provenance)?);
        }
        Ok(FoldAssertions { slots })
    }

    /// Entry value of each carried target, flattened into the initial tuple.
    ///
    /// Typing the carried tuple from the entry value's own right-hand side
    /// would let a Real target enter the loop as an Integer. The DAE already
    /// types every `FunctionFoldParameter` and `FunctionFoldOutput` naming this
    /// target by that target's declaration, so the declaration is the only
    /// carried type the body and the loop's consumers can agree on. Resolving
    /// the entry definition through the enclosing scope's value table is what
    /// supplies it: that table holds every definition at its target's declared
    /// type, and the same resolution answers an entry value whose own statement
    /// has not been lowered yet.
    fn carried_entry_values(
        &mut self,
        transition: &FoldTransition<'dae>,
        provenance: rumoca_core::Span,
    ) -> Result<
        (Vec<solve::ProgramRegister<'program>>, CarriedLayout<'dae>),
        solve::SolveProgramConstructionError,
    > {
        let mut initial_flat = Vec::new();
        let mut carried_layout = Vec::with_capacity(transition.initial_definitions.len());
        for (ordinal, definition) in transition.initial_definitions.iter().copied().enumerate() {
            let value = self.function_definition_value(definition)?;
            let start = initial_flat.len();
            initial_flat.extend(value.leaves.iter().copied());
            let carried = u32::try_from(ordinal).map_err(|_| {
                solve::SolveProgramConstructionError::IdentityOverflow { provenance }
            })?;
            carried_layout.push((carried, value.value_type, start..initial_flat.len()));
        }
        Ok((initial_flat, carried_layout))
    }
}

fn message_slot_in_range(
    slot: usize,
    kind: &AssertionSlot,
    assertions: &std::ops::Range<usize>,
) -> Option<usize> {
    let AssertionSlot::MessageValue { predicate, .. } = kind else {
        return None;
    };
    assertions
        .contains(&slot.checked_sub(*predicate)?)
        .then_some(slot)
}

/// Check one DAE fold record into a transition. An assertion-only loop carries
/// its real assertion observations even when it has no ordinary target.
fn checked_transition<'dae>(
    view: dae::DaeView<'dae>,
    fold: dae::FunctionFoldId<'dae>,
    provenance: rumoca_core::Span,
) -> Result<FoldTransition<'dae>, solve::SolveProgramConstructionError> {
    let fold_view = view
        .function_fold(fold)
        .ok_or(solve::SolveProgramConstructionError::WireMismatch)?;
    // The fold's entry value of each carried target is the enclosing scope's
    // reaching definition of that target, so it is a definition - not a bare
    // right-hand side - and is produced by the one definition-value rule.
    let initial_definitions = fold_view.initial_values().iter().collect::<Vec<_>>();
    let update_expressions = fold_view.update_values().rhs_iter().collect::<Vec<_>>();
    let carried_targets = fold_view.targets().collect::<Vec<_>>();
    // Definition identities the fold issues for its own carried values: the
    // per-iteration entry parameter and the end-of-iteration update. The region
    // installs both itself, so neither is a capture.
    let parameter_definitions = fold_view
        .parameter_values()
        .iter()
        .map(|definition| definition.id())
        .collect::<Vec<_>>();
    let update_definitions = fold_view
        .update_values()
        .iter()
        .map(|definition| definition.id())
        .collect::<Vec<_>>();
    if initial_definitions.len() != update_expressions.len() {
        return Err(solve::SolveProgramConstructionError::InvalidFold { provenance });
    }
    if carried_targets.len() != initial_definitions.len()
        || parameter_definitions.len() != initial_definitions.len()
    {
        return Err(solve::SolveProgramConstructionError::InvalidFold { provenance });
    }
    for (ordinal, definition) in initial_definitions.iter().enumerate() {
        // The entry definition and the carried target the DAE recorded are one
        // fact stated twice; disagreement is a broken wire, not a conversion.
        if carried_targets.get(ordinal) != Some(&definition.target()) {
            return Err(solve::SolveProgramConstructionError::InvalidFold { provenance });
        }
    }
    Ok(FoldTransition {
        initial_definitions,
        update_expressions,
        parameter_definitions,
        update_definitions,
        domain: fold_view.domain(),
        continuation: fold_view.continuation(),
    })
}

impl<'dae> FoldIteration<'_, 'dae> {
    /// Build a bounded `while` fold's predicate region over the carried
    /// tuple and the captures.
    fn lower_continuation<'region>(
        self,
        builder: &mut solve::TypedProgramBuilder<'region>,
        continuation: dae::ExprId<'dae>,
        (carried, captures): (
            &[solve::ProgramSlot<'region>],
            &[solve::ProgramSlot<'region>],
        ),
        outputs: &[solve::ProgramSlot<'region>],
    ) -> Result<(), solve::SolveProgramConstructionError> {
        let mut lowerer = load_region_lowerer(
            builder,
            captures,
            self.environment,
            self.context,
            self.provenance,
        )?;
        self.seed_carried_values(&mut lowerer, carried)?;
        let value = lowerer.expression(continuation)?;
        match (value.leaves.as_slice(), outputs) {
            ([predicate], [output]) => lowerer.builder.store(*output, *predicate, self.provenance),
            _ => Err(solve::SolveProgramConstructionError::InvalidFold {
                provenance: self.provenance,
            }),
        }
    }

    /// Build one iteration of the transition region.
    fn lower<'region>(
        self,
        builder: &mut solve::TypedProgramBuilder<'region>,
        carried: &[solve::ProgramSlot<'region>],
        captures: &[solve::ProgramSlot<'region>],
        binders: &[solve::ProgramSlot<'region>],
        outputs: &[solve::ProgramSlot<'region>],
    ) -> Result<(), solve::SolveProgramConstructionError> {
        let mut lowerer = load_region_lowerer(
            builder,
            captures,
            self.environment,
            self.context,
            self.provenance,
        )?;
        self.seed_carried_values(&mut lowerer, carried)?;
        self.seed_binders(&mut lowerer, binders)?;
        if let Some(source) = self.source {
            lowerer.next_direct_assertion = source.assertions.start;
            lowerer.statements(source.statements.clone())?;
            if lowerer.next_direct_assertion != source.assertions.end {
                return Err(solve::SolveProgramConstructionError::InvalidCallOutput {
                    provenance: self.provenance,
                });
            }
        }
        let mut updated = self.lower_updates(&mut lowerer)?;
        updated.extend(self.accumulate_assertions(&mut lowerer, carried)?);
        if updated.len() != outputs.len() {
            return Err(solve::SolveProgramConstructionError::InvalidCallOutput {
                provenance: self.provenance,
            });
        }
        for (output, value) in outputs.iter().zip(updated) {
            lowerer.builder.store(*output, value, self.provenance)?;
        }
        Ok(())
    }

    /// The observer owns each iteration's diagnostics; normal carried outputs
    /// contain only the inactive assertion cells.
    fn accumulate_assertions<'program>(
        &self,
        lowerer: &mut ExpressionLowerer<'_, 'program, 'dae>,
        _carried: &[solve::ProgramSlot<'program>],
    ) -> Result<Vec<solve::ProgramRegister<'program>>, solve::SolveProgramConstructionError> {
        self.assertions
            .slots
            .iter()
            .map(|slot| lowerer.inactive_slot(*slot, self.provenance))
            .collect()
    }

    /// Install this iteration's entry value of every carried target.
    ///
    /// The value lands under both names the body can read it by: the fold's
    /// `FunctionFoldParameter` and the entry parameter's own SSA definition.
    /// The entry parameter is its own issued definition, so seeding it cannot
    /// shadow a redefinition made inside the body.
    fn seed_carried_values<'program>(
        &self,
        lowerer: &mut ExpressionLowerer<'_, 'program, 'dae>,
        carried: &[solve::ProgramSlot<'program>],
    ) -> Result<(), solve::SolveProgramConstructionError> {
        let mut loaded = Vec::with_capacity(carried.len());
        for slot in carried {
            loaded.push(lowerer.builder.load(*slot, self.provenance)?);
        }
        for (ordinal, value_type, range) in self.carried_layout {
            let value = LoweredValue {
                value_type: *value_type,
                leaves: loaded[range.clone()].to_vec(),
            };
            lowerer
                .fold_parameters
                .insert((self.fold, *ordinal), value.clone());
            let parameter = self
                .transition
                .parameter_definitions
                .get(*ordinal as usize)
                .ok_or(solve::SolveProgramConstructionError::InvalidFold {
                    provenance: self.provenance,
                })?;
            lowerer.function_values.insert(*parameter, value);
        }
        Ok(())
    }

    /// Install this iteration's index for every binder the loop domain owns.
    fn seed_binders<'program>(
        &self,
        lowerer: &mut ExpressionLowerer<'_, 'program, 'dae>,
        binders: &[solve::ProgramSlot<'program>],
    ) -> Result<(), solve::SolveProgramConstructionError> {
        let domain = self.transition.domain.index();
        for (ordinal, slot) in binders.iter().enumerate() {
            let register = lowerer.builder.load(*slot, self.provenance)?;
            let ordinal = u32::try_from(ordinal).map_err(|_| {
                solve::SolveProgramConstructionError::IdentityOverflow {
                    provenance: self.provenance,
                }
            })?;
            lowerer.binders.insert((domain, ordinal), register);
        }
        Ok(())
    }

    /// Lower each carried target's end-of-iteration value, in issued order.
    ///
    /// The DAE fold tuple is ordered by the function's sequential
    /// redefinitions. A later tuple member reads the completed value of every
    /// preceding target through that target's own update definition, while a
    /// `FunctionFoldParameter` continues to name the iteration's entry value.
    /// Publishing each update under its own definition preserves that issued
    /// order directly.
    fn lower_updates<'program>(
        &self,
        lowerer: &mut ExpressionLowerer<'_, 'program, 'dae>,
    ) -> Result<Vec<solve::ProgramRegister<'program>>, solve::SolveProgramConstructionError> {
        let mut updated = Vec::new();
        for (definition, (ordinal, value_type, _)) in self
            .transition
            .update_definitions
            .iter()
            .zip(self.carried_layout)
        {
            let definition = lowerer
                .view
                .function_definition(*definition)
                .ok_or(solve::SolveProgramConstructionError::WireMismatch)?;
            let value = lowerer.function_definition_value(definition)?;
            let value = lowerer.coerce_value(value, *value_type, self.provenance)?;
            if definition.id()
                != *self
                    .transition
                    .update_definitions
                    .get(*ordinal as usize)
                    .ok_or(solve::SolveProgramConstructionError::InvalidFold {
                        provenance: self.provenance,
                    })?
            {
                return Err(solve::SolveProgramConstructionError::InvalidFold {
                    provenance: self.provenance,
                });
            }
            updated.extend(value.leaves);
        }
        Ok(updated)
    }
}
