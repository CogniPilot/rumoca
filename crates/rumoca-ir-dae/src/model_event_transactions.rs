use std::marker::PhantomData;

use rustc_hash::FxHashSet;

use crate::model::{Storage, check_provenance, checked_u32, unknown};
use crate::{
    ClockId, ConditionId, DaeConstructionError, DaeProvenance, DiscreteRealId, DiscreteValueId,
    ExprId, ModelEventTransactionId, VariableRole,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize)]
#[serde(rename_all = "snake_case", tag = "kind", content = "variable")]
pub(crate) enum ModelEventTargetEntry {
    DiscreteReal(u32),
    DiscreteValue(u32),
}

#[derive(Debug, Clone, PartialEq, serde::Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct ModelEventDefinitionEntry {
    pub(crate) target: ModelEventTargetEntry,
    pub(crate) value: u32,
    pub(crate) provenance: DaeProvenance,
}

#[derive(Debug, Clone, PartialEq, serde::Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct ModelEventStepEntry {
    pub(crate) trigger: u32,
    pub(crate) guard: u32,
    pub(crate) clock: Option<u32>,
    pub(crate) definitions: Vec<ModelEventDefinitionEntry>,
    pub(crate) provenance: DaeProvenance,
}

#[derive(Debug, Clone, PartialEq, serde::Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct ModelEventTransactionEntry {
    pub(crate) targets: Vec<ModelEventTargetEntry>,
    pub(crate) steps: Vec<ModelEventStepEntry>,
    pub(crate) provenance: DaeProvenance,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ModelEventTarget<'dae> {
    DiscreteReal(DiscreteRealId<'dae>),
    DiscreteValue(DiscreteValueId<'dae>),
}

impl ModelEventTarget<'_> {
    pub const fn variable(self) -> u32 {
        match self {
            Self::DiscreteReal(variable) => variable.index(),
            Self::DiscreteValue(variable) => variable.index(),
        }
    }

    const fn erase(self) -> ModelEventTargetEntry {
        match self {
            Self::DiscreteReal(variable) => ModelEventTargetEntry::DiscreteReal(variable.index()),
            Self::DiscreteValue(variable) => ModelEventTargetEntry::DiscreteValue(variable.index()),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ModelEventDefinition<'dae> {
    target: ModelEventTarget<'dae>,
    value: ExprId<'dae>,
    provenance: DaeProvenance,
}

impl<'dae> ModelEventDefinition<'dae> {
    pub const fn new(
        target: ModelEventTarget<'dae>,
        value: ExprId<'dae>,
        provenance: DaeProvenance,
    ) -> Self {
        Self {
            target,
            value,
            provenance,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ModelEventStep<'dae> {
    trigger: ConditionId<'dae>,
    guard: ConditionId<'dae>,
    clock: Option<ClockId<'dae>>,
    definitions: Vec<ModelEventDefinition<'dae>>,
    provenance: DaeProvenance,
}

impl<'dae> ModelEventStep<'dae> {
    pub fn new(
        trigger: ConditionId<'dae>,
        guard: ConditionId<'dae>,
        clock: Option<ClockId<'dae>>,
        definitions: impl IntoIterator<Item = ModelEventDefinition<'dae>>,
        provenance: DaeProvenance,
    ) -> Self {
        Self {
            trigger,
            guard,
            clock,
            definitions: definitions.into_iter().collect(),
            provenance,
        }
    }
}

pub struct ModelEventTransactions<'storage, 'dae> {
    pub(crate) source_map: &'storage rumoca_core::SourceMap,
    pub(crate) storage: &'storage mut Storage,
    pub(crate) marker: PhantomData<&'dae mut &'dae ()>,
}

impl<'dae> ModelEventTransactions<'_, 'dae> {
    pub fn transaction(
        &mut self,
        targets: impl IntoIterator<Item = ModelEventTarget<'dae>>,
        steps: impl IntoIterator<Item = ModelEventStep<'dae>>,
        provenance: DaeProvenance,
    ) -> Result<ModelEventTransactionId<'dae>, DaeConstructionError> {
        check_provenance(self.source_map, provenance)?;
        let targets = targets.into_iter().collect::<Vec<_>>();
        let steps = steps.into_iter().collect::<Vec<_>>();
        if targets.is_empty() || steps.is_empty() {
            return Err(DaeConstructionError::EmptyModelEventTransaction {
                span: provenance.span(),
            });
        }
        self.validate_targets(&targets, provenance)?;
        let declared = targets
            .iter()
            .map(|target| target.variable())
            .collect::<FxHashSet<_>>();
        let mut covered = FxHashSet::default();
        let steps = steps
            .into_iter()
            .map(|step| self.validate_step(step, &declared, &mut covered))
            .collect::<Result<Vec<_>, _>>()?;
        if covered != declared {
            return Err(DaeConstructionError::IncompleteModelEventTransaction {
                span: provenance.span(),
            });
        }
        let id = ModelEventTransactionId::from_raw(checked_u32(
            self.storage.model_event_transactions.len(),
            "model-event transaction arena",
            provenance,
        )?);
        for target in &targets {
            self.storage
                .model_event_transaction_by_variable
                .insert(target.variable(), id.index());
        }
        self.storage
            .model_event_transactions
            .push(ModelEventTransactionEntry {
                targets: targets.into_iter().map(ModelEventTarget::erase).collect(),
                steps,
                provenance,
            });
        Ok(id)
    }

    fn validate_targets(
        &self,
        targets: &[ModelEventTarget<'dae>],
        provenance: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        let mut unique = FxHashSet::default();
        for &target in targets {
            self.validate_target_role(target, provenance)?;
            if self
                .storage
                .model_event_transaction_by_variable
                .contains_key(&target.variable())
            {
                return Err(DaeConstructionError::DuplicateDefinition {
                    kind: "model-event transaction target owner",
                    index: target.variable(),
                    span: provenance.span(),
                });
            }
            if !unique.insert(target.variable()) {
                return Err(DaeConstructionError::DuplicateDefinition {
                    kind: "model-event transaction target",
                    index: target.variable(),
                    span: provenance.span(),
                });
            }
        }
        Ok(())
    }

    fn validate_step(
        &self,
        step: ModelEventStep<'dae>,
        declared: &FxHashSet<u32>,
        covered: &mut FxHashSet<u32>,
    ) -> Result<ModelEventStepEntry, DaeConstructionError> {
        check_provenance(self.source_map, step.provenance)?;
        crate::discrete_values::expect_complete_condition(
            self.storage,
            step.trigger.index(),
            step.provenance,
        )?;
        crate::discrete_values::expect_complete_condition(
            self.storage,
            step.guard.index(),
            step.provenance,
        )?;
        if step.definitions.is_empty() {
            return Err(DaeConstructionError::EmptyModelEventTransaction {
                span: step.provenance.span(),
            });
        }
        let mut step_targets = FxHashSet::default();
        let definitions = step
            .definitions
            .into_iter()
            .map(|definition| {
                self.validate_definition(definition, step.clock, declared, &mut step_targets)
            })
            .collect::<Result<Vec<_>, _>>()?;
        covered.extend(step_targets);
        Ok(ModelEventStepEntry {
            trigger: step.trigger.index(),
            guard: step.guard.index(),
            clock: step.clock.map(ClockId::index),
            definitions,
            provenance: step.provenance,
        })
    }

    fn validate_definition(
        &self,
        definition: ModelEventDefinition<'dae>,
        clock: Option<ClockId<'dae>>,
        declared: &FxHashSet<u32>,
        step_targets: &mut FxHashSet<u32>,
    ) -> Result<ModelEventDefinitionEntry, DaeConstructionError> {
        check_provenance(self.source_map, definition.provenance)?;
        self.validate_target_role(definition.target, definition.provenance)?;
        let variable = definition.target.variable();
        if !declared.contains(&variable) {
            return Err(DaeConstructionError::UndeclaredModelEventTarget {
                target: variable,
                span: definition.provenance.span(),
            });
        }
        if !step_targets.insert(variable) {
            return Err(DaeConstructionError::DuplicateDefinition {
                kind: "model-event transaction step target",
                index: variable,
                span: definition.provenance.span(),
            });
        }
        self.storage
            .expect_closed_expression(definition.value, definition.provenance)?;
        let variable_type = self
            .storage
            .variables
            .get(variable as usize)
            .map(|entry| entry.value_type)
            .ok_or_else(|| unknown("variable", variable, definition.provenance))?;
        let value_type = *self
            .storage
            .expressions
            .value_types
            .get(definition.value.index() as usize)
            .ok_or_else(|| {
                unknown(
                    "expression",
                    definition.value.index(),
                    definition.provenance,
                )
            })?;
        self.storage.expect_value_type_compatible(
            variable_type,
            value_type,
            definition.provenance,
        )?;
        self.validate_clock(variable, clock, definition.provenance)?;
        Ok(ModelEventDefinitionEntry {
            target: definition.target.erase(),
            value: definition.value.index(),
            provenance: definition.provenance,
        })
    }

    fn validate_target_role(
        &self,
        target: ModelEventTarget<'dae>,
        provenance: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        let expected = match target {
            ModelEventTarget::DiscreteReal(_) => VariableRole::DiscreteReal,
            ModelEventTarget::DiscreteValue(_) => VariableRole::DiscreteValue,
        };
        let variable = target.variable();
        let entry = self
            .storage
            .variables
            .get(variable as usize)
            .ok_or_else(|| unknown("variable", variable, provenance))?;
        if entry.role == expected {
            Ok(())
        } else {
            Err(DaeConstructionError::InvalidVariableRole {
                name: entry.name.clone(),
                span: provenance.span(),
            })
        }
    }

    fn validate_clock(
        &self,
        variable: u32,
        clock: Option<ClockId<'dae>>,
        provenance: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        let established = self
            .storage
            .clock_ownership_by_variable
            .get(&variable)
            .and_then(|ownership| self.storage.clock_ownerships.get(*ownership as usize))
            .map(|ownership| ownership.clock);
        match (clock.map(ClockId::index), established) {
            (Some(expected), Some(found)) if expected == found => Ok(()),
            (None, None) => Ok(()),
            (Some(clock), _) => Err(DaeConstructionError::MissingClockOwnership {
                variable,
                clock,
                span: provenance.span(),
            }),
            (None, Some(clock)) => Err(DaeConstructionError::MissingClockOwnership {
                variable,
                clock,
                span: provenance.span(),
            }),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ModelEventDefinitionView<'dae> {
    entry: &'dae ModelEventDefinitionEntry,
}

impl<'dae> ModelEventDefinitionView<'dae> {
    pub const fn target(self) -> ModelEventTarget<'dae> {
        match self.entry.target {
            ModelEventTargetEntry::DiscreteReal(variable) => {
                ModelEventTarget::DiscreteReal(DiscreteRealId::from_raw(variable))
            }
            ModelEventTargetEntry::DiscreteValue(variable) => {
                ModelEventTarget::DiscreteValue(DiscreteValueId::from_raw(variable))
            }
        }
    }

    pub const fn value(self) -> ExprId<'dae> {
        ExprId::from_raw(self.entry.value)
    }

    pub const fn provenance(self) -> DaeProvenance {
        self.entry.provenance
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ModelEventStepView<'dae> {
    entry: &'dae ModelEventStepEntry,
}

impl<'dae> ModelEventStepView<'dae> {
    pub const fn trigger(self) -> ConditionId<'dae> {
        ConditionId::from_raw(self.entry.trigger)
    }

    pub const fn guard(self) -> ConditionId<'dae> {
        ConditionId::from_raw(self.entry.guard)
    }

    pub const fn clock(self) -> Option<ClockId<'dae>> {
        match self.entry.clock {
            Some(clock) => Some(ClockId::from_raw(clock)),
            None => None,
        }
    }

    pub fn definitions(self) -> impl ExactSizeIterator<Item = ModelEventDefinitionView<'dae>> {
        self.entry
            .definitions
            .iter()
            .map(|entry| ModelEventDefinitionView { entry })
    }

    pub const fn provenance(self) -> DaeProvenance {
        self.entry.provenance
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ModelEventTransactionView<'dae> {
    pub(crate) entry: &'dae ModelEventTransactionEntry,
}

impl<'dae> ModelEventTransactionView<'dae> {
    pub fn targets(self) -> impl ExactSizeIterator<Item = ModelEventTarget<'dae>> {
        self.entry
            .targets
            .iter()
            .copied()
            .map(|target| match target {
                ModelEventTargetEntry::DiscreteReal(variable) => {
                    ModelEventTarget::DiscreteReal(DiscreteRealId::from_raw(variable))
                }
                ModelEventTargetEntry::DiscreteValue(variable) => {
                    ModelEventTarget::DiscreteValue(DiscreteValueId::from_raw(variable))
                }
            })
    }

    pub fn steps(self) -> impl ExactSizeIterator<Item = ModelEventStepView<'dae>> {
        self.entry
            .steps
            .iter()
            .map(|entry| ModelEventStepView { entry })
    }

    pub const fn provenance(self) -> DaeProvenance {
        self.entry.provenance
    }
}
