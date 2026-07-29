use super::*;

#[derive(Clone, Copy)]
pub struct DaeView<'dae> {
    pub(super) dae: &'dae Dae,
    pub(super) marker: PhantomData<&'dae mut &'dae ()>,
}

impl<'dae> DaeView<'dae> {
    /// Returns one exact source span that can own a whole-model diagnostic.
    ///
    /// This is intentionally optional: an empty, source-free DAE has no
    /// provenance to invent. Consumers must emit an explicitly unspanned
    /// diagnostic for that case.
    pub fn responsible_span(self) -> Option<Span> {
        self.dae
            .storage
            .variables
            .first()
            .map(|variable| variable.declaration.span())
            .or_else(|| {
                self.dae
                    .storage
                    .expressions
                    .provenance
                    .first()
                    .map(|provenance| provenance.span())
            })
            .or_else(|| {
                self.dae
                    .storage
                    .value_type_provenance
                    .first()
                    .map(|provenance| provenance.span())
            })
            .or_else(|| {
                self.dae
                    .storage
                    .functions
                    .first()
                    .map(|function| function.declaration.span())
            })
            .or_else(|| {
                self.dae
                    .storage
                    .domains
                    .first()
                    .map(|domain| domain.provenance.span())
            })
    }

    pub fn expression_count(self) -> usize {
        self.dae.storage.expressions.nodes.len()
    }

    pub fn variable_count(self) -> usize {
        self.dae.storage.variables.len()
    }

    pub fn domain_count(self) -> usize {
        self.dae.storage.domains.len()
    }

    pub fn domain_id(self, index: usize) -> Option<DomainId<'dae>> {
        self.dae
            .storage
            .domains
            .get(index)
            .and_then(|_| u32::try_from(index).ok())
            .map(DomainId::from_raw)
    }

    pub fn domain(self, id: DomainId<'dae>) -> Option<DomainView<'dae>> {
        Some(DomainView {
            entry: self.dae.storage.domains.get(id.index() as usize)?,
            marker: PhantomData,
        })
    }

    pub fn value_type_count(self) -> usize {
        self.dae.storage.value_types.len()
    }

    pub fn value_type_id(self, index: usize) -> Option<ValueTypeId<'dae>> {
        self.dae
            .storage
            .value_types
            .get(index)
            .and_then(|_| u32::try_from(index).ok())
            .map(ValueTypeId::from_raw)
    }

    pub fn value_type(self, id: ValueTypeId<'dae>) -> Option<&'dae ValueType> {
        self.dae.storage.value_types.get(id.index() as usize)
    }

    pub fn record_field(
        self,
        id: ValueTypeId<'dae>,
        ordinal: usize,
    ) -> Option<(&'dae VarName, ValueTypeId<'dae>)> {
        let value_type = self.value_type(id)?;
        Some((
            value_type.record_field_name(ordinal)?,
            ValueTypeId::from_raw(value_type.record_field_type(ordinal)?),
        ))
    }

    pub fn value_type_provenance(self, id: ValueTypeId<'dae>) -> Option<DaeProvenance> {
        self.dae
            .storage
            .value_type_provenance
            .get(id.index() as usize)
            .copied()
    }

    pub fn effective_flat_type(self, id: ValueTypeId<'dae>) -> Option<TypeId> {
        self.dae
            .storage
            .flat_type_ids
            .get(id.index() as usize)
            .copied()
            .flatten()
    }

    pub fn function_count(self) -> usize {
        self.dae.storage.functions.len()
    }

    pub fn continuous_equation_count(self) -> usize {
        self.dae.storage.continuous_equations.len()
    }

    pub fn continuous_owner_count(self) -> usize {
        self.dae.storage.continuous_equation_owners.len()
    }

    pub fn continuous_family_count(self) -> usize {
        self.dae.storage.continuous_families.len()
    }

    pub fn initialization_family_count(self) -> usize {
        self.dae.storage.initialization_families.len()
    }

    pub fn initialization_equation_count(self) -> usize {
        self.dae.storage.initialization_equations.len()
    }

    pub fn initialization_owner_count(self) -> usize {
        self.dae.storage.initialization_equation_owners.len()
    }

    pub fn discrete_real_equation_count(self) -> usize {
        self.dae.storage.discrete_real_equations.len()
    }

    pub fn discrete_assignment_count(self) -> usize {
        self.dae.storage.discrete_assignments.len()
    }

    pub fn relation_count(self) -> usize {
        self.dae.storage.relations.len()
    }

    pub fn condition_count(self) -> usize {
        self.dae.storage.conditions.len()
    }

    pub fn root_count(self) -> usize {
        self.dae.storage.roots.len()
    }

    pub fn time_event_count(self) -> usize {
        self.dae.storage.time_events.len()
    }

    pub fn event_action_count(self) -> usize {
        self.dae.storage.event_actions.len()
    }

    pub fn clock_count(self) -> usize {
        self.dae.storage.clocks.len()
    }

    pub fn clock_ownership_count(self) -> usize {
        self.dae.storage.clock_ownerships.len()
    }

    pub fn previous_value_count(self) -> usize {
        self.dae.storage.previous_values.len()
    }

    pub fn terminal_count(self) -> usize {
        self.dae.storage.terminals.len()
    }

    pub fn delay_count(self) -> usize {
        self.dae.storage.delays.len()
    }

    pub fn expression_id(self, index: usize) -> Option<ExprId<'dae>> {
        self.dae
            .storage
            .expressions
            .nodes
            .get(index)
            .and_then(|_| u32::try_from(index).ok())
            .map(ExprId::from_raw)
    }

    pub fn function_id(self, index: usize) -> Option<FunctionId<'dae>> {
        self.dae
            .storage
            .functions
            .get(index)
            .and_then(|_| u32::try_from(index).ok())
            .map(FunctionId::from_raw)
    }

    pub fn function(self, id: FunctionId<'dae>) -> Option<FunctionView<'dae>> {
        Some(FunctionView {
            id,
            entry: self.dae.storage.functions.get(id.index() as usize)?,
        })
    }

    pub fn function_fold(self, id: FunctionFoldId<'dae>) -> Option<FunctionFoldView<'dae>> {
        let function = self
            .dae
            .storage
            .functions
            .get(id.function().index() as usize)?;
        let raw = *function.folds.get(id.ordinal() as usize)?;
        let entry = self.dae.storage.function_folds.get(raw as usize)?;
        (entry.function == id.function().index() && entry.ordinal == id.ordinal()).then_some(
            FunctionFoldView {
                id,
                entry,
                marker: PhantomData,
            },
        )
    }

    pub fn variable_id(self, index: usize) -> Option<VariableId<'dae>> {
        self.dae
            .storage
            .variables
            .get(index)
            .and_then(|_| u32::try_from(index).ok())
            .map(VariableId::from_raw)
    }

    pub fn variable_declaration(self, id: VariableId<'dae>) -> Option<DaeProvenance> {
        self.dae
            .storage
            .variables
            .get(id.index() as usize)
            .map(|variable| variable.declaration)
    }

    pub fn variable(self, id: VariableId<'dae>) -> Option<VariableView<'dae>> {
        let value_type = self
            .dae
            .storage
            .variables
            .get(id.index() as usize)?
            .value_type;
        Some(VariableView {
            id,
            entry: self.dae.storage.variables.get(id.index() as usize)?,
            value_type_id: ValueTypeId::from_raw(value_type),
            value_type: self.dae.storage.value_types.get(value_type as usize)?,
            marker: PhantomData,
        })
    }

    pub fn variables(
        self,
    ) -> impl ExactSizeIterator<Item = (VariableId<'dae>, VariableView<'dae>)> {
        (0..self.variable_count()).map(move |index| {
            let id = self
                .variable_id(index)
                .expect("finalized dense variable index has a u32 identity");
            let variable = self
                .variable(id)
                .expect("finalized dense variable identity resolves");
            (id, variable)
        })
    }

    pub fn expression(self, id: ExprId<'dae>) -> Option<ExpressionView<'dae>> {
        let index = id.index() as usize;
        Some(ExpressionView {
            dae: self.dae,
            node: self.dae.storage.expressions.nodes.get(index)?,
            provenance: *self.dae.storage.expressions.provenance.get(index)?,
            variability: *self.dae.storage.expressions.variability.get(index)?,
            binder_domain: *self.dae.storage.expressions.binder_domains.get(index)?,
            function_scope: *self.dae.storage.expressions.function_scopes.get(index)?,
            value_type_id: ValueTypeId::from_raw(
                *self.dae.storage.expressions.value_types.get(index)?,
            ),
            value_type: self
                .dae
                .storage
                .value_types
                .get(*self.dae.storage.expressions.value_types.get(index)? as usize)?,
        })
    }

    pub fn continuous_equation(self, index: usize) -> Option<ResidualEquationView<'dae>> {
        Some(residual_equation_view(
            self.dae.storage.continuous_equations.get(index)?,
        ))
    }

    pub fn continuous_owner(self, index: usize) -> Option<ContinuousOwnerView<'dae>> {
        Some(
            match *self.dae.storage.continuous_equation_owners.get(index)? {
                EquationOwnerEntry::Residual(raw) => ContinuousOwnerView::Residual {
                    id: ContinuousEquationId::from_raw(raw),
                    equation: self.continuous_equation(raw as usize)?,
                },
                EquationOwnerEntry::Structured(raw) => ContinuousOwnerView::Structured {
                    id: ContinuousFamilyId::from_raw(raw),
                    family: self.continuous_family(raw as usize)?,
                },
            },
        )
    }

    pub fn continuous_owners(self) -> impl ExactSizeIterator<Item = ContinuousOwnerView<'dae>> {
        (0..self.continuous_owner_count()).map(move |index| {
            self.continuous_owner(index)
                .expect("finalized continuous owner resolves")
        })
    }

    /// Resolve the semantic owner of one row in the derived scalar view.
    pub fn continuous_owner_for_scalar_row(
        self,
        scalar_row: usize,
    ) -> Option<ContinuousOwnerView<'dae>> {
        let mut first_row = 0usize;
        for owner in self.continuous_owners() {
            let row_count = match owner {
                ContinuousOwnerView::Residual { .. } => 1,
                ContinuousOwnerView::Structured { family, .. } => family.scalar_rows() as usize,
            };
            let end = first_row.checked_add(row_count)?;
            if scalar_row < end {
                return Some(owner);
            }
            first_row = end;
        }
        None
    }

    pub fn continuous_family(self, index: usize) -> Option<StructuredFamilyView<'dae>> {
        self.structured_family(self.dae.storage.continuous_families.get(index)?)
    }

    pub fn initialization_family(self, index: usize) -> Option<StructuredFamilyView<'dae>> {
        self.structured_family(self.dae.storage.initialization_families.get(index)?)
    }

    pub fn initialization_equation(self, index: usize) -> Option<ResidualEquationView<'dae>> {
        Some(residual_equation_view(
            self.dae.storage.initialization_equations.get(index)?,
        ))
    }

    pub fn initialization_owner(self, index: usize) -> Option<InitializationOwnerView<'dae>> {
        Some(
            match *self.dae.storage.initialization_equation_owners.get(index)? {
                EquationOwnerEntry::Residual(raw) => InitializationOwnerView::Residual {
                    id: InitializationEquationId::from_raw(raw),
                    equation: self.initialization_equation(raw as usize)?,
                },
                EquationOwnerEntry::Structured(raw) => InitializationOwnerView::Structured {
                    id: InitializationFamilyId::from_raw(raw),
                    family: self.initialization_family(raw as usize)?,
                },
            },
        )
    }

    pub fn initialization_owners(
        self,
    ) -> impl ExactSizeIterator<Item = InitializationOwnerView<'dae>> {
        (0..self.initialization_owner_count()).map(move |index| {
            self.initialization_owner(index)
                .expect("finalized initialization owner resolves")
        })
    }

    pub fn discrete_real_equation(self, index: usize) -> Option<ResidualEquationView<'dae>> {
        Some(residual_equation_view(
            self.dae.storage.discrete_real_equations.get(index)?,
        ))
    }

    pub fn discrete_assignment_id(self, index: usize) -> Option<DiscreteAssignmentId<'dae>> {
        self.dae
            .storage
            .discrete_assignments
            .get(index)
            .and_then(|_| u32::try_from(index).ok())
            .map(DiscreteAssignmentId::from_raw)
    }

    pub fn discrete_assignment(
        self,
        id: DiscreteAssignmentId<'dae>,
    ) -> Option<DiscreteAssignmentView<'dae>> {
        let entry = self
            .dae
            .storage
            .discrete_assignments
            .get(id.index() as usize)?;
        Some(DiscreteAssignmentView {
            target: DiscreteValueId::from_raw(entry.target),
            value: ExprId::from_raw(entry.value),
            provenance: entry.provenance,
        })
    }

    pub fn relation_id(self, index: usize) -> Option<RelationId<'dae>> {
        self.dae
            .storage
            .relations
            .get(index)
            .and_then(|_| u32::try_from(index).ok())
            .map(RelationId::from_raw)
    }

    pub fn relation(self, id: RelationId<'dae>) -> Option<RelationView<'dae>> {
        let entry = self.dae.storage.relations.get(id.index() as usize)?;
        Some(RelationView {
            expression: ExprId::from_raw(entry.expression),
            provenance: entry.provenance,
        })
    }

    pub fn condition_id(self, index: usize) -> Option<ConditionId<'dae>> {
        self.dae
            .storage
            .conditions
            .get(index)
            .and_then(|_| u32::try_from(index).ok())
            .map(ConditionId::from_raw)
    }

    pub fn condition(self, id: ConditionId<'dae>) -> Option<ConditionView<'dae>> {
        let entry = self.dae.storage.conditions.get(id.index() as usize)?;
        let operation = match entry
            .node
            .as_ref()
            .expect("final DAE cannot contain an undefined condition")
        {
            crate::conditions::ConditionNode::Relation(raw) => {
                ConditionOperation::Relation(RelationId::from_raw(*raw))
            }
            crate::conditions::ConditionNode::Discrete(raw) => {
                ConditionOperation::Discrete(ExprId::from_raw(*raw))
            }
            crate::conditions::ConditionNode::Clock(raw) => {
                ConditionOperation::Clock(ClockId::from_raw(*raw))
            }
            crate::conditions::ConditionNode::Not(raw) => {
                ConditionOperation::Not(ConditionId::from_raw(*raw))
            }
            crate::conditions::ConditionNode::And { lhs, rhs } => {
                ConditionOperation::And(ConditionId::from_raw(*lhs), ConditionId::from_raw(*rhs))
            }
            crate::conditions::ConditionNode::Or { lhs, rhs } => {
                ConditionOperation::Or(ConditionId::from_raw(*lhs), ConditionId::from_raw(*rhs))
            }
        };
        Some(ConditionView {
            operation,
            provenance: entry.provenance,
        })
    }

    pub fn root_id(self, index: usize) -> Option<RootId<'dae>> {
        self.dae
            .storage
            .roots
            .get(index)
            .and_then(|_| u32::try_from(index).ok())
            .map(RootId::from_raw)
    }

    pub fn root(self, id: RootId<'dae>) -> Option<RootView<'dae>> {
        let entry = self.dae.storage.roots.get(id.index() as usize)?;
        Some(RootView {
            relation: RelationId::from_raw(entry.relation),
            activation: ConditionId::from_raw(entry.activation),
            provenance: entry.provenance,
        })
    }

    pub fn time_event_id(self, index: usize) -> Option<TimeEventId<'dae>> {
        self.dae
            .storage
            .time_events
            .get(index)
            .and_then(|_| u32::try_from(index).ok())
            .map(TimeEventId::from_raw)
    }

    pub fn time_event(self, id: TimeEventId<'dae>) -> Option<TimeEventView<'dae>> {
        let entry = self.dae.storage.time_events.get(id.index() as usize)?;
        Some(TimeEventView {
            instant: &entry.instant,
            provenance: entry.provenance,
        })
    }

    pub fn event_action_id(self, index: usize) -> Option<EventActionId<'dae>> {
        self.dae
            .storage
            .event_actions
            .get(index)
            .and_then(|_| u32::try_from(index).ok())
            .map(EventActionId::from_raw)
    }

    pub fn event_action(self, id: EventActionId<'dae>) -> Option<EventActionView<'dae>> {
        let entry = self.dae.storage.event_actions.get(id.index() as usize)?;
        let operation = match entry.kind {
            EventActionKind::Assert { message, level } => EventActionOperation::Assert {
                message: ExprId::from_raw(message),
                level: level.map(ExprId::from_raw),
            },
            EventActionKind::Terminate { message } => EventActionOperation::Terminate {
                message: ExprId::from_raw(message),
            },
            EventActionKind::Reinitialize { state, value } => EventActionOperation::Reinitialize {
                state: StateId::from_raw(state),
                value: ExprId::from_raw(value),
            },
            EventActionKind::AssignDiscreteReal { target, value } => {
                EventActionOperation::AssignDiscreteReal {
                    target: DiscreteRealId::from_raw(target),
                    value: ExprId::from_raw(value),
                }
            }
            EventActionKind::AssignDiscreteValue { target, value } => {
                EventActionOperation::AssignDiscreteValue {
                    target: DiscreteValueId::from_raw(target),
                    value: ExprId::from_raw(value),
                }
            }
        };
        Some(EventActionView {
            trigger: ConditionId::from_raw(entry.trigger),
            guard: ConditionId::from_raw(entry.guard),
            operation,
            provenance: entry.provenance,
        })
    }

    pub fn clock_id(self, index: usize) -> Option<ClockId<'dae>> {
        self.dae
            .storage
            .clocks
            .get(index)
            .and_then(|_| u32::try_from(index).ok())
            .map(ClockId::from_raw)
    }

    pub fn clock(self, id: ClockId<'dae>) -> Option<ClockView<'dae>> {
        let entry = self.dae.storage.clocks.get(id.index() as usize)?;
        let operation = match &entry.kind {
            crate::clocks::ClockKind::Periodic(lattice) => ClockOperation::Periodic(lattice),
            crate::clocks::ClockKind::Triggered(condition) => {
                ClockOperation::Triggered(ConditionId::from_raw(*condition))
            }
        };
        Some(ClockView {
            operation,
            provenance: entry.provenance,
        })
    }

    pub fn clock_ownership_id(self, index: usize) -> Option<ClockOwnershipId<'dae>> {
        self.dae
            .storage
            .clock_ownerships
            .get(index)
            .and_then(|_| u32::try_from(index).ok())
            .map(ClockOwnershipId::from_raw)
    }

    pub fn clock_ownership(self, id: ClockOwnershipId<'dae>) -> Option<ClockOwnershipView<'dae>> {
        let entry = self.dae.storage.clock_ownerships.get(id.index() as usize)?;
        Some(ClockOwnershipView {
            variable: VariableId::from_raw(entry.variable),
            kind: match entry.role {
                ClockedVariableRole::DiscreteReal => ClockedVariableKind::DiscreteReal,
                ClockedVariableRole::DiscreteValue => ClockedVariableKind::DiscreteValue,
            },
            clock: ClockId::from_raw(entry.clock),
            provenance: entry.provenance,
        })
    }

    pub fn previous_id(self, index: usize) -> Option<PreviousId<'dae>> {
        self.dae
            .storage
            .previous_values
            .get(index)
            .and_then(|_| u32::try_from(index).ok())
            .map(PreviousId::from_raw)
    }

    pub fn previous(self, id: PreviousId<'dae>) -> Option<PreviousView<'dae>> {
        let entry = self.dae.storage.previous_values.get(id.index() as usize)?;
        Some(PreviousView {
            variable: VariableId::from_raw(entry.variable),
            clock: ClockId::from_raw(entry.clock),
            provenance: entry.provenance,
        })
    }

    pub fn terminal_id(self, index: usize) -> Option<TerminalId<'dae>> {
        self.dae
            .storage
            .terminals
            .get(index)
            .and_then(|_| u32::try_from(index).ok())
            .map(TerminalId::from_raw)
    }

    pub fn terminal(self, id: TerminalId<'dae>) -> Option<TerminalView> {
        let entry = self.dae.storage.terminals.get(id.index() as usize)?;
        Some(TerminalView {
            provenance: entry.provenance,
        })
    }

    pub fn delay_id(self, index: usize) -> Option<DelayId<'dae>> {
        self.dae
            .storage
            .delays
            .get(index)
            .and_then(|_| u32::try_from(index).ok())
            .map(DelayId::from_raw)
    }

    pub fn delay(self, id: DelayId<'dae>) -> Option<DelayView<'dae>> {
        let entry = self.dae.storage.delays.get(id.index() as usize)?;
        Some(DelayView {
            source: ExprId::from_raw(entry.source),
            delay_time: ExprId::from_raw(entry.delay_time),
            delay_time_evidence: entry
                .delay_time_evidence
                .as_ref()
                .map(positive_parameter_view),
            delay_max: entry.delay_max.as_ref().map(positive_parameter_view),
            value_type: self
                .dae
                .storage
                .value_types
                .get(entry.value_type as usize)?,
            variability: entry.variability,
            provenance: entry.provenance,
        })
    }

    pub fn subscript_provenance(self, index: usize) -> Option<DaeProvenance> {
        self.dae
            .storage
            .expressions
            .subscripts
            .get(index)
            .map(|subscript| subscript.provenance)
    }

    pub fn source_text(self, provenance: DaeProvenance) -> Option<&'dae str> {
        source_text(&self.dae.source_map, provenance)
    }

    fn structured_family(
        self,
        entry: &'dae StructuredFamilyEntry,
    ) -> Option<StructuredFamilyView<'dae>> {
        Some(StructuredFamilyView {
            domain: DomainId::from_raw(entry.domain),
            scalar_view: entry.scalar_view,
            bodies: ExpressionOperands {
                raw: self
                    .dae
                    .storage
                    .equation_family_bodies
                    .get(entry.bodies.indices())?,
                marker: PhantomData,
            },
            scalar_rows: entry.scalar_rows,
            provenance: entry.provenance,
        })
    }
}

#[derive(Clone, Copy)]
pub struct VariableView<'dae> {
    id: VariableId<'dae>,
    entry: &'dae VariableEntry,
    value_type_id: ValueTypeId<'dae>,
    value_type: &'dae ValueType,
    marker: PhantomData<&'dae mut &'dae ()>,
}

impl<'dae> VariableView<'dae> {
    pub const fn id(self) -> VariableId<'dae> {
        self.id
    }

    pub fn identity(self) -> VariableIdentity<'dae> {
        match self.entry.role {
            VariableRole::Parameter | VariableRole::Constant => {
                VariableIdentity::Parameter(ParameterId::from_raw(self.id.index()))
            }
            VariableRole::Input => VariableIdentity::Input(InputId::from_raw(self.id.index())),
            VariableRole::State => VariableIdentity::State(StateId::from_raw(self.id.index())),
            VariableRole::Algebraic | VariableRole::Output => {
                VariableIdentity::Algebraic(AlgebraicId::from_raw(self.id.index()))
            }
            VariableRole::DiscreteReal => {
                VariableIdentity::DiscreteReal(DiscreteRealId::from_raw(self.id.index()))
            }
            VariableRole::DiscreteValue => {
                VariableIdentity::DiscreteValue(DiscreteValueId::from_raw(self.id.index()))
            }
        }
    }

    pub fn name(self) -> &'dae VarName {
        &self.entry.name
    }

    pub const fn role(self) -> VariableRole {
        self.entry.role
    }

    pub const fn variability(self) -> ExpressionVariability {
        self.entry.variability
    }

    pub const fn value_type(self) -> &'dae ValueType {
        self.value_type
    }

    pub const fn value_type_id(self) -> ValueTypeId<'dae> {
        self.value_type_id
    }

    pub fn scalar_count(self) -> usize {
        self.value_type
            .scalar_count()
            .expect("final DAE value type has a checked scalar capacity")
    }

    pub fn scalar_name(self, flat_index: usize) -> Option<String> {
        let subscripts = self.value_type.scalar_subscripts(flat_index)?;
        if subscripts.is_empty() {
            return Some(self.entry.name.to_string());
        }
        let indices = subscripts
            .iter()
            .map(u32::to_string)
            .collect::<Vec<_>>()
            .join(",");
        Some(format!("{}[{indices}]", self.entry.name))
    }

    pub const fn declaration(self) -> DaeProvenance {
        self.entry.declaration
    }

    pub fn component_reference(self) -> Option<&'dae ComponentReference> {
        self.attributes().component_ref.as_ref()
    }

    pub fn binding(self) -> Option<ExprId<'dae>> {
        self.attributes().binding.map(ExprId::from_raw)
    }

    pub fn start(self) -> Option<ExprId<'dae>> {
        self.attributes().start.map(ExprId::from_raw)
    }

    pub fn fixed(self) -> Option<bool> {
        self.attributes().fixed
    }

    pub fn minimum(self) -> Option<ExprId<'dae>> {
        self.attributes().min.map(ExprId::from_raw)
    }

    pub fn maximum(self) -> Option<ExprId<'dae>> {
        self.attributes().max.map(ExprId::from_raw)
    }

    pub fn nominal(self) -> Option<ExprId<'dae>> {
        self.attributes().nominal.map(ExprId::from_raw)
    }

    pub fn unit(self) -> Option<&'dae str> {
        self.attributes().unit.as_deref()
    }

    pub fn state_select(self) -> StateSelect {
        self.attributes().state_select
    }

    pub fn description(self) -> Option<&'dae str> {
        self.attributes().description.as_deref()
    }

    pub fn causality(self) -> VariableCausality {
        self.attributes().causality
    }

    pub fn is_tunable(self) -> bool {
        self.attributes().is_tunable
    }

    pub fn is_held(self) -> bool {
        self.attributes().is_held
    }

    pub fn origin(self) -> VariableOrigin {
        self.attributes().origin
    }

    fn attributes(self) -> &'dae VariableAttributesWire {
        let Some(attributes) = &self.entry.attributes else {
            unreachable!("final DAE cannot contain an unfilled variable");
        };
        attributes
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VariableIdentity<'dae> {
    Parameter(ParameterId<'dae>),
    Input(InputId<'dae>),
    State(StateId<'dae>),
    Algebraic(AlgebraicId<'dae>),
    DiscreteReal(DiscreteRealId<'dae>),
    DiscreteValue(DiscreteValueId<'dae>),
}

#[derive(Clone, Copy)]
pub struct FunctionView<'dae> {
    id: FunctionId<'dae>,
    entry: &'dae FunctionEntry,
}

impl<'dae> FunctionView<'dae> {
    pub const fn id(self) -> FunctionId<'dae> {
        self.id
    }

    pub fn name(self) -> &'dae VarName {
        &self.entry.name
    }

    pub fn parameter_types(self) -> ValueTypeOperands<'dae> {
        ValueTypeOperands {
            raw: &self.entry.parameters,
            marker: PhantomData,
        }
    }

    pub fn parameters(self) -> impl ExactSizeIterator<Item = FunctionParameterView<'dae>> {
        self.entry
            .parameter_values
            .iter()
            .enumerate()
            .map(move |(ordinal, entry)| FunctionParameterView {
                id: FunctionParameterId::from_raw(
                    self.id.index(),
                    u32::try_from(ordinal).expect("function parameter index was checked"),
                ),
                entry,
            })
    }

    pub fn result_types(self) -> ValueTypeOperands<'dae> {
        ValueTypeOperands {
            raw: &self.entry.results,
            marker: PhantomData,
        }
    }

    pub fn result_values(self) -> ExpressionOperands<'dae> {
        let definition = self
            .entry
            .definition
            .as_ref()
            .expect("final DAE cannot contain an undefined function");
        ExpressionOperands {
            raw: &definition.results,
            marker: PhantomData,
        }
    }

    pub fn fold_count(self) -> usize {
        self.entry.folds.len()
    }

    pub fn fold_id(self, index: usize) -> Option<FunctionFoldId<'dae>> {
        self.entry
            .folds
            .get(index)
            .and_then(|_| u32::try_from(index).ok())
            .map(|ordinal| FunctionFoldId::from_raw(self.id.index(), ordinal))
    }

    pub fn values(self) -> impl ExactSizeIterator<Item = FunctionValueView<'dae>> {
        self.entry
            .values
            .iter()
            .enumerate()
            .map(move |(ordinal, entry)| FunctionValueView {
                id: FunctionValueId::from_raw(
                    self.id.index(),
                    u32::try_from(ordinal).expect("function value index was checked"),
                ),
                entry,
            })
    }

    pub fn statements(self) -> FunctionStatements<'dae> {
        let definition = self
            .entry
            .definition
            .as_ref()
            .expect("final DAE cannot contain an undefined function");
        FunctionStatements {
            function: self.id,
            statements: &definition.statements,
            next: 0,
        }
    }

    pub const fn declaration(self) -> DaeProvenance {
        self.entry.declaration
    }
}

#[derive(Clone, Copy)]
pub struct FunctionParameterView<'dae> {
    id: FunctionParameterId<'dae>,
    entry: &'dae FunctionParameterEntry,
}

impl<'dae> FunctionParameterView<'dae> {
    pub const fn id(self) -> FunctionParameterId<'dae> {
        self.id
    }

    pub fn name(self) -> &'dae VarName {
        &self.entry.name
    }

    pub const fn value_type(self) -> ValueTypeId<'dae> {
        ValueTypeId::from_raw(self.entry.value_type)
    }

    pub const fn declaration(self) -> DaeProvenance {
        self.entry.declaration
    }
}

#[derive(Clone, Copy)]
pub struct FunctionValueView<'dae> {
    id: FunctionValueId<'dae>,
    entry: &'dae FunctionValueEntry,
}

impl<'dae> FunctionValueView<'dae> {
    pub const fn id(self) -> FunctionValueId<'dae> {
        self.id
    }

    pub fn name(self) -> &'dae VarName {
        &self.entry.name
    }

    pub const fn value_type(self) -> ValueTypeId<'dae> {
        ValueTypeId::from_raw(self.entry.value_type)
    }

    pub const fn role(self) -> FunctionValueRole {
        self.entry.role
    }

    pub const fn declaration(self) -> DaeProvenance {
        self.entry.declaration
    }
}

#[derive(Clone)]
pub enum FunctionStatementView<'dae> {
    Assignment {
        target: FunctionValueId<'dae>,
        value: ExprId<'dae>,
        provenance: DaeProvenance,
    },
    For {
        fold: FunctionFoldId<'dae>,
        statements: FunctionStatements<'dae>,
        provenance: DaeProvenance,
    },
}

impl<'dae> FunctionStatementView<'dae> {
    fn from_wire(function: FunctionId<'dae>, statement: &'dae FunctionStatementWire) -> Self {
        match statement {
            FunctionStatementWire::Assignment {
                target,
                value,
                provenance,
            } => Self::Assignment {
                target: FunctionValueId::from_raw(function.index(), *target),
                value: ExprId::from_raw(*value),
                provenance: *provenance,
            },
            FunctionStatementWire::For {
                fold,
                statements,
                provenance,
            } => Self::For {
                fold: FunctionFoldId::from_raw(function.index(), *fold),
                statements: FunctionStatements {
                    function,
                    statements,
                    next: 0,
                },
                provenance: *provenance,
            },
        }
    }
}

#[derive(Clone)]
pub struct FunctionStatements<'dae> {
    function: FunctionId<'dae>,
    statements: &'dae [FunctionStatementWire],
    next: usize,
}

impl<'dae> Iterator for FunctionStatements<'dae> {
    type Item = FunctionStatementView<'dae>;

    fn next(&mut self) -> Option<Self::Item> {
        let statement = self.statements.get(self.next)?;
        self.next += 1;
        Some(FunctionStatementView::from_wire(self.function, statement))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = self.statements.len() - self.next;
        (remaining, Some(remaining))
    }
}

impl ExactSizeIterator for FunctionStatements<'_> {}

#[derive(Clone, Copy)]
pub struct FunctionFoldView<'dae> {
    id: FunctionFoldId<'dae>,
    entry: &'dae FunctionFoldEntry,
    marker: PhantomData<&'dae mut &'dae ()>,
}

impl<'dae> FunctionFoldView<'dae> {
    pub const fn id(self) -> FunctionFoldId<'dae> {
        self.id
    }

    pub const fn domain(self) -> DomainId<'dae> {
        DomainId::from_raw(self.entry.domain)
    }

    pub fn targets(self) -> impl ExactSizeIterator<Item = FunctionValueId<'dae>> {
        self.entry
            .targets
            .iter()
            .copied()
            .map(move |target| FunctionValueId::from_raw(self.entry.function, target))
    }

    pub fn initial_values(self) -> ExpressionOperands<'dae> {
        ExpressionOperands {
            raw: &self.entry.initial_values,
            marker: PhantomData,
        }
    }

    pub fn parameter_values(self) -> ExpressionOperands<'dae> {
        ExpressionOperands {
            raw: &self.entry.parameter_values,
            marker: PhantomData,
        }
    }

    pub fn update_values(self) -> ExpressionOperands<'dae> {
        ExpressionOperands {
            raw: &self.entry.update_values,
            marker: PhantomData,
        }
    }

    pub fn output_values(self) -> ExpressionOperands<'dae> {
        ExpressionOperands {
            raw: &self.entry.output_values,
            marker: PhantomData,
        }
    }

    pub const fn provenance(self) -> DaeProvenance {
        self.entry.provenance
    }
}

#[derive(Clone, Copy)]
pub struct ValueTypeOperands<'dae> {
    raw: &'dae [u32],
    marker: PhantomData<&'dae mut &'dae ()>,
}

impl<'dae> ValueTypeOperands<'dae> {
    pub const fn len(self) -> usize {
        self.raw.len()
    }

    pub const fn is_empty(self) -> bool {
        self.raw.is_empty()
    }

    pub fn get(self, index: usize) -> Option<ValueTypeId<'dae>> {
        self.raw.get(index).copied().map(ValueTypeId::from_raw)
    }

    pub fn iter(self) -> impl ExactSizeIterator<Item = ValueTypeId<'dae>> {
        self.raw.iter().copied().map(ValueTypeId::from_raw)
    }
}

#[derive(Clone, Copy)]
pub struct DomainView<'dae> {
    entry: &'dae DomainEntry,
    marker: PhantomData<&'dae mut &'dae ()>,
}

impl<'dae> DomainView<'dae> {
    pub fn structured(self) -> &'dae StructuredIndexDomain {
        &self.entry.domain
    }

    pub fn extents(self) -> &'dae [u32] {
        &self.entry.extents
    }

    pub const fn scalar_count(self) -> u32 {
        self.entry.scalar_count
    }

    pub const fn provenance(self) -> DaeProvenance {
        self.entry.provenance
    }
}

#[derive(Clone, Copy)]
pub struct ExpressionView<'dae> {
    dae: &'dae Dae,
    node: &'dae ExprNode,
    provenance: DaeProvenance,
    variability: ExpressionVariability,
    binder_domain: Option<u32>,
    function_scope: Option<u32>,
    value_type_id: ValueTypeId<'dae>,
    value_type: &'dae ValueType,
}

impl<'dae> ExpressionView<'dae> {
    pub const fn provenance(self) -> DaeProvenance {
        self.provenance
    }

    pub const fn value_type(self) -> &'dae ValueType {
        self.value_type
    }

    pub const fn value_type_id(self) -> ValueTypeId<'dae> {
        self.value_type_id
    }

    pub const fn variability(self) -> ExpressionVariability {
        self.variability
    }

    pub fn binder_domain(self) -> Option<DomainId<'dae>> {
        self.binder_domain.map(DomainId::from_raw)
    }

    pub fn function_scope(self) -> Option<FunctionId<'dae>> {
        self.function_scope.map(FunctionId::from_raw)
    }

    pub fn kind(self) -> ExpressionKind {
        match self.node {
            ExprNode::Literal(_) => ExpressionKind::Literal,
            ExprNode::Coordinate(_) => ExpressionKind::Coordinate,
            ExprNode::Unary { .. } => ExpressionKind::Unary,
            ExprNode::Binary { .. } => ExpressionKind::Binary,
            ExprNode::Conditional { .. } => ExpressionKind::Conditional,
            ExprNode::Array { .. } => ExpressionKind::Array,
            ExprNode::Record { .. } => ExpressionKind::Record,
            ExprNode::Field { .. } => ExpressionKind::Field,
            ExprNode::Range { .. } => ExpressionKind::Range,
            ExprNode::Comprehension { .. } => ExpressionKind::Comprehension,
            ExprNode::Index { .. } => ExpressionKind::Index,
            ExprNode::ArrayUpdate { .. } => ExpressionKind::ArrayUpdate,
            ExprNode::Builtin { .. } => ExpressionKind::Builtin,
            ExprNode::Call { .. } => ExpressionKind::Call,
            ExprNode::FunctionValue { .. } => ExpressionKind::FunctionValue,
            ExprNode::FunctionFoldParameter { .. } => ExpressionKind::FunctionFoldParameter,
            ExprNode::FunctionFoldOutput { .. } => ExpressionKind::FunctionFoldOutput,
        }
    }

    pub fn operation(self) -> ExpressionOperation<'dae> {
        match self.node {
            ExprNode::Literal(value) => ExpressionOperation::Literal(value),
            ExprNode::Coordinate(coordinate) => {
                ExpressionOperation::Coordinate(coordinate_view(*coordinate))
            }
            ExprNode::Unary { operator, operand } => ExpressionOperation::Unary {
                operator: *operator,
                operand: ExprId::from_raw(*operand),
            },
            ExprNode::Binary { operator, lhs, rhs } => ExpressionOperation::Binary {
                operator: *operator,
                lhs: ExprId::from_raw(*lhs),
                rhs: ExprId::from_raw(*rhs),
            },
            ExprNode::Conditional { operands } => {
                ExpressionOperation::Conditional(self.expression_operands(*operands))
            }
            ExprNode::Array { operands } => {
                ExpressionOperation::Array(self.expression_operands(*operands))
            }
            ExprNode::Record { operands } => {
                ExpressionOperation::Record(self.expression_operands(*operands))
            }
            ExprNode::Field { base, field } => ExpressionOperation::Field {
                base: ExprId::from_raw(*base),
                field: *field,
            },
            ExprNode::Range { start, step, stop } => ExpressionOperation::Range {
                start: *start,
                step: *step,
                stop: *stop,
            },
            ExprNode::Comprehension { domain, body } => ExpressionOperation::Comprehension {
                domain: DomainId::from_raw(*domain),
                body: ExprId::from_raw(*body),
            },
            ExprNode::Index { base, subscripts } => ExpressionOperation::Index {
                base: ExprId::from_raw(*base),
                subscripts: self.subscripts(*subscripts),
            },
            ExprNode::ArrayUpdate {
                base,
                value,
                subscripts,
            } => ExpressionOperation::ArrayUpdate {
                base: ExprId::from_raw(*base),
                value: ExprId::from_raw(*value),
                subscripts: self.subscripts(*subscripts),
            },
            ExprNode::Builtin { builtin, operands } => ExpressionOperation::Builtin {
                builtin: *builtin,
                arguments: self.expression_operands(*operands),
            },
            ExprNode::Call {
                function,
                output,
                operands,
            } => ExpressionOperation::Call {
                function: FunctionId::from_raw(*function),
                output: *output,
                arguments: self.expression_operands(*operands),
            },
            ExprNode::FunctionValue {
                function,
                value,
                definition,
            } => ExpressionOperation::FunctionValue {
                value: FunctionValueId::from_raw(*function, *value),
                definition: ExprId::from_raw(*definition),
            },
            ExprNode::FunctionFoldParameter {
                function,
                fold,
                carried,
            } => ExpressionOperation::FunctionFoldParameter {
                fold: FunctionFoldId::from_raw(*function, *fold),
                carried: *carried,
            },
            ExprNode::FunctionFoldOutput {
                function,
                fold,
                carried,
            } => ExpressionOperation::FunctionFoldOutput {
                fold: FunctionFoldId::from_raw(*function, *fold),
                carried: *carried,
            },
        }
    }

    pub fn variable_coordinate(self) -> Option<VariableId<'dae>> {
        match self.node {
            ExprNode::Coordinate(
                Coordinate::Parameter(variable)
                | Coordinate::Input(variable)
                | Coordinate::State(variable)
                | Coordinate::Derivative(variable)
                | Coordinate::Algebraic(variable)
                | Coordinate::DiscreteReal(variable)
                | Coordinate::DiscreteValue(variable)
                | Coordinate::PreDiscreteReal(variable)
                | Coordinate::PreDiscreteValue(variable),
            ) => Some(VariableId::from_raw(*variable)),
            _ => None,
        }
    }

    fn expression_operands(
        self,
        range: crate::expression::OperandRange,
    ) -> ExpressionOperands<'dae> {
        let Some(raw) = self.dae.storage.expressions.operands.get(range.indices()) else {
            unreachable!("final DAE cannot contain an invalid expression operand range");
        };
        ExpressionOperands {
            raw,
            marker: PhantomData,
        }
    }

    fn subscripts(self, range: crate::expression::OperandRange) -> SubscriptsView<'dae> {
        let Some(raw) = self.dae.storage.expressions.subscripts.get(range.indices()) else {
            unreachable!("final DAE cannot contain an invalid subscript range");
        };
        SubscriptsView {
            raw,
            marker: PhantomData,
        }
    }
}

#[derive(Clone, Copy)]
pub struct ExpressionOperands<'dae> {
    raw: &'dae [u32],
    marker: PhantomData<&'dae mut &'dae ()>,
}

impl<'dae> ExpressionOperands<'dae> {
    pub const fn len(self) -> usize {
        self.raw.len()
    }

    pub const fn is_empty(self) -> bool {
        self.raw.is_empty()
    }

    pub fn get(self, index: usize) -> Option<ExprId<'dae>> {
        self.raw.get(index).copied().map(ExprId::from_raw)
    }

    pub fn iter(self) -> impl ExactSizeIterator<Item = ExprId<'dae>> {
        self.raw.iter().copied().map(ExprId::from_raw)
    }
}

#[derive(Clone, Copy)]
pub struct SubscriptsView<'dae> {
    raw: &'dae [crate::expression::PackedSubscript],
    marker: PhantomData<&'dae mut &'dae ()>,
}

impl<'dae> SubscriptsView<'dae> {
    pub const fn len(self) -> usize {
        self.raw.len()
    }

    pub const fn is_empty(self) -> bool {
        self.raw.is_empty()
    }

    pub fn get(self, index: usize) -> Option<SubscriptView<'dae>> {
        let subscript = self.raw.get(index)?;
        Some(match subscript.kind {
            crate::expression::PackedSubscriptKind::Index(expression) => SubscriptView::Index {
                expression: ExprId::from_raw(expression),
                provenance: subscript.provenance,
            },
            crate::expression::PackedSubscriptKind::Whole => SubscriptView::Whole {
                provenance: subscript.provenance,
            },
            crate::expression::PackedSubscriptKind::Slice(expression) => SubscriptView::Slice {
                expression: ExprId::from_raw(expression),
                provenance: subscript.provenance,
            },
        })
    }

    pub fn iter(self) -> impl ExactSizeIterator<Item = SubscriptView<'dae>> {
        (0..self.len()).map(move |index| {
            self.get(index)
                .expect("finalized subscript ordinal resolves")
        })
    }
}

#[derive(Debug, Clone, Copy)]
pub enum SubscriptView<'dae> {
    Index {
        expression: ExprId<'dae>,
        provenance: DaeProvenance,
    },
    Whole {
        provenance: DaeProvenance,
    },
    Slice {
        expression: ExprId<'dae>,
        provenance: DaeProvenance,
    },
}

#[derive(Debug, Clone, Copy)]
pub enum CoordinateView<'dae> {
    Parameter(ParameterId<'dae>),
    Input(InputId<'dae>),
    State(StateId<'dae>),
    Derivative(StateId<'dae>),
    Algebraic(AlgebraicId<'dae>),
    DiscreteReal(DiscreteRealId<'dae>),
    DiscreteValue(DiscreteValueId<'dae>),
    PreDiscreteReal(DiscreteRealId<'dae>),
    PreDiscreteValue(DiscreteValueId<'dae>),
    Time,
    Condition(ConditionId<'dae>),
    Delay(crate::DelayId<'dae>),
    Previous(crate::PreviousId<'dae>),
    Terminal(crate::TerminalId<'dae>),
    Binder(DomainBinderId<'dae>),
    FunctionParameter(FunctionParameterId<'dae>),
}

#[derive(Clone, Copy)]
pub enum ExpressionOperation<'dae> {
    Literal(&'dae DaeLiteral),
    Coordinate(CoordinateView<'dae>),
    Unary {
        operator: crate::UnaryOperator,
        operand: ExprId<'dae>,
    },
    Binary {
        operator: BinaryOperator,
        lhs: ExprId<'dae>,
        rhs: ExprId<'dae>,
    },
    Conditional(ExpressionOperands<'dae>),
    Array(ExpressionOperands<'dae>),
    Record(ExpressionOperands<'dae>),
    Field {
        base: ExprId<'dae>,
        field: u32,
    },
    Range {
        start: i64,
        step: i64,
        stop: i64,
    },
    Comprehension {
        domain: DomainId<'dae>,
        body: ExprId<'dae>,
    },
    Index {
        base: ExprId<'dae>,
        subscripts: SubscriptsView<'dae>,
    },
    ArrayUpdate {
        base: ExprId<'dae>,
        value: ExprId<'dae>,
        subscripts: SubscriptsView<'dae>,
    },
    Builtin {
        builtin: crate::PureBuiltin,
        arguments: ExpressionOperands<'dae>,
    },
    Call {
        function: FunctionId<'dae>,
        output: u32,
        arguments: ExpressionOperands<'dae>,
    },
    FunctionValue {
        value: FunctionValueId<'dae>,
        definition: ExprId<'dae>,
    },
    FunctionFoldParameter {
        fold: FunctionFoldId<'dae>,
        carried: u32,
    },
    FunctionFoldOutput {
        fold: FunctionFoldId<'dae>,
        carried: u32,
    },
}

fn coordinate_view<'dae>(coordinate: Coordinate) -> CoordinateView<'dae> {
    match coordinate {
        Coordinate::Parameter(raw) => CoordinateView::Parameter(ParameterId::from_raw(raw)),
        Coordinate::Input(raw) => CoordinateView::Input(InputId::from_raw(raw)),
        Coordinate::State(raw) => CoordinateView::State(StateId::from_raw(raw)),
        Coordinate::Derivative(raw) => CoordinateView::Derivative(StateId::from_raw(raw)),
        Coordinate::Algebraic(raw) => CoordinateView::Algebraic(AlgebraicId::from_raw(raw)),
        Coordinate::DiscreteReal(raw) => {
            CoordinateView::DiscreteReal(DiscreteRealId::from_raw(raw))
        }
        Coordinate::DiscreteValue(raw) => {
            CoordinateView::DiscreteValue(DiscreteValueId::from_raw(raw))
        }
        Coordinate::PreDiscreteReal(raw) => {
            CoordinateView::PreDiscreteReal(DiscreteRealId::from_raw(raw))
        }
        Coordinate::PreDiscreteValue(raw) => {
            CoordinateView::PreDiscreteValue(DiscreteValueId::from_raw(raw))
        }
        Coordinate::Time => CoordinateView::Time,
        Coordinate::Condition(raw) => CoordinateView::Condition(ConditionId::from_raw(raw)),
        Coordinate::Delay(raw) => CoordinateView::Delay(crate::DelayId::from_raw(raw)),
        Coordinate::Previous(raw) => CoordinateView::Previous(crate::PreviousId::from_raw(raw)),
        Coordinate::Terminal(raw) => CoordinateView::Terminal(crate::TerminalId::from_raw(raw)),
        Coordinate::Binder { domain, ordinal } => {
            CoordinateView::Binder(DomainBinderId::from_raw(domain, ordinal))
        }
        Coordinate::FunctionParameter { function, ordinal } => {
            CoordinateView::FunctionParameter(FunctionParameterId::from_raw(function, ordinal))
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExpressionKind {
    Literal,
    Coordinate,
    Unary,
    Binary,
    Conditional,
    Array,
    Record,
    Field,
    Range,
    Comprehension,
    Index,
    ArrayUpdate,
    Builtin,
    Call,
    FunctionValue,
    FunctionFoldParameter,
    FunctionFoldOutput,
}

#[derive(Clone, Copy)]
pub struct ResidualEquationView<'dae> {
    residual: ExprId<'dae>,
    provenance: DaeProvenance,
}

#[derive(Clone, Copy)]
pub struct StructuredFamilyView<'dae> {
    domain: DomainId<'dae>,
    scalar_view: rumoca_core::ComprehensionScalarView,
    bodies: ExpressionOperands<'dae>,
    scalar_rows: u32,
    provenance: DaeProvenance,
}

#[derive(Clone, Copy)]
pub enum ContinuousOwnerView<'dae> {
    Residual {
        id: ContinuousEquationId<'dae>,
        equation: ResidualEquationView<'dae>,
    },
    Structured {
        id: ContinuousFamilyId<'dae>,
        family: StructuredFamilyView<'dae>,
    },
}

#[derive(Clone, Copy)]
pub enum InitializationOwnerView<'dae> {
    Residual {
        id: InitializationEquationId<'dae>,
        equation: ResidualEquationView<'dae>,
    },
    Structured {
        id: InitializationFamilyId<'dae>,
        family: StructuredFamilyView<'dae>,
    },
}

impl<'dae> StructuredFamilyView<'dae> {
    pub const fn domain(self) -> DomainId<'dae> {
        self.domain
    }

    pub const fn scalar_view(self) -> rumoca_core::ComprehensionScalarView {
        self.scalar_view
    }

    pub const fn bodies(self) -> ExpressionOperands<'dae> {
        self.bodies
    }

    pub const fn scalar_rows(self) -> u32 {
        self.scalar_rows
    }

    pub const fn provenance(self) -> DaeProvenance {
        self.provenance
    }
}

impl<'dae> ResidualEquationView<'dae> {
    pub const fn residual(self) -> ExprId<'dae> {
        self.residual
    }

    pub const fn provenance(self) -> DaeProvenance {
        self.provenance
    }
}

fn residual_equation_view(entry: &ResidualEquationEntry) -> ResidualEquationView<'_> {
    ResidualEquationView {
        residual: ExprId::from_raw(entry.residual),
        provenance: entry.provenance,
    }
}

fn positive_parameter_view(entry: &PositiveParameterEntry) -> PositiveParameterView<'_> {
    PositiveParameterView {
        expression: ExprId::from_raw(entry.expression),
        value: entry.value,
        provenance: entry.provenance,
    }
}
