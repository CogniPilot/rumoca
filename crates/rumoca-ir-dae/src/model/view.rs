pub(super) mod arena_walks;
mod quotient_owners;
pub use quotient_owners::{RuntimeQuotientOwnerKind, RuntimeQuotientOwnerView};

use rumoca_core::checked_extent_product;

use super::*;

#[derive(Clone, Copy)]
pub struct DaeView<'dae> {
    pub(super) dae: &'dae Dae,
    pub(super) marker: PhantomData<&'dae mut &'dae ()>,
}

/// Name-free variable facts exposed to a cross-stage refinement checker.
///
/// This projection cannot reveal display labels, scalar names, provenance,
/// runtime values, or arena identities. A checker that accepts only this type
/// therefore cannot accidentally turn presentation data into cross-stage
/// identity.
pub struct DaeVariableRefinementView {
    entries: Box<[DaeVariableRefinementEntry]>,
}

pub struct DaeVariableRefinementEntry {
    source_occurrence: rumoca_core::SourceOccurrenceId,
    role: VariableRole,
    fixed: rumoca_core::Fixity,
    variability: ExpressionVariability,
    is_tunable: bool,
    causality: VariableCausality,
    scalar_type: ScalarType,
    dimensions: Box<[u32]>,
    scalar_count: usize,
}

impl DaeVariableRefinementView {
    #[must_use]
    pub fn entries(&self) -> &[DaeVariableRefinementEntry] {
        std::ops::Deref::deref(&self.entries)
    }
}

impl DaeVariableRefinementEntry {
    #[must_use]
    pub const fn source_occurrence(&self) -> rumoca_core::SourceOccurrenceId {
        self.source_occurrence
    }

    #[must_use]
    pub const fn role(&self) -> VariableRole {
        self.role
    }

    #[must_use]
    pub const fn fixed(&self) -> rumoca_core::Fixity {
        self.fixed
    }

    #[must_use]
    pub const fn variability(&self) -> ExpressionVariability {
        self.variability
    }

    #[must_use]
    pub const fn is_tunable(&self) -> bool {
        self.is_tunable
    }

    #[must_use]
    pub const fn causality(&self) -> VariableCausality {
        self.causality
    }

    #[must_use]
    pub const fn scalar_type(&self) -> ScalarType {
        self.scalar_type
    }

    #[must_use]
    pub fn dimensions(&self) -> &[u32] {
        std::ops::Deref::deref(&self.dimensions)
    }

    #[must_use]
    pub const fn scalar_count(&self) -> usize {
        self.scalar_count
    }
}

/// Exact packed layout of one field projected from a record value.
///
/// Record arrays remain compact typed values in DAE-IR. Consumers that must
/// cross a scalar execution boundary use this layout to gather the selected
/// field from each record without inventing a second packing convention.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RecordFieldLayout {
    outer_count: usize,
    record_width: usize,
    field_offset: usize,
    field_width: usize,
}

impl RecordFieldLayout {
    pub const fn outer_count(self) -> usize {
        self.outer_count
    }

    pub const fn record_width(self) -> usize {
        self.record_width
    }

    pub const fn field_offset(self) -> usize {
        self.field_offset
    }

    pub const fn field_width(self) -> usize {
        self.field_width
    }
}

macro_rules! storage_count_accessors {
    ($($name:ident => $($field:ident).+),+ $(,)?) => {
        $(pub fn $name(self) -> usize {
            self.dae.storage.$($field).+.len()
        })+
    };
}

macro_rules! storage_id_accessors {
    ($($name:ident => ($id:ident, $($field:ident).+)),+ $(,)?) => {
        $(pub fn $name(self, index: usize) -> Option<$id<'dae>> {
            let raw = u32::try_from(index).ok()?;
            (index < self.dae.storage.$($field).+.len()).then(|| $id::from_raw(raw))
        })+
    };
}

macro_rules! view_getters {
    (
        $(const fn $const_name:ident -> $const_return:ty = |$const_view:ident| $const_body:expr;)*
        $(fn $name:ident -> $return:ty = |$view:ident| $body:expr;)*
    ) => {
        $(pub const fn $const_name(self) -> $const_return {
            let $const_view = self;
            $const_body
        })*

        $(pub fn $name(self) -> $return {
            let $view = self;
            $body
        })*
    };
}

macro_rules! raw_id_slice_view {
    ($view:ident => $id:ident) => {
        #[derive(Clone, Copy)]
        pub struct $view<'dae> {
            raw: &'dae [u32],
            marker: PhantomData<&'dae mut &'dae ()>,
        }

        impl<'dae> $view<'dae> {
            view_getters! {
                const fn len -> usize = |view| view.raw.len();
                const fn is_empty -> bool = |view| view.raw.is_empty();
            }

            pub fn get(self, index: usize) -> Option<$id<'dae>> {
                self.raw.get(index).copied().map($id::from_raw)
            }

            pub fn iter(self) -> impl ExactSizeIterator<Item = $id<'dae>> {
                self.raw.iter().copied().map($id::from_raw)
            }
        }
    };
}

impl<'dae> DaeView<'dae> {
    /// Restrict this inspection brand to the name-free facts admitted by the
    /// DAE-to-Solve refinement checker.
    #[must_use]
    pub fn variable_refinement(self) -> DaeVariableRefinementView {
        let entries = self
            .variables()
            .map(|(_, variable)| DaeVariableRefinementEntry {
                source_occurrence: variable.source_occurrence(),
                role: variable.role(),
                fixed: variable.fixed(),
                variability: variable.variability(),
                is_tunable: variable.is_tunable(),
                causality: variable.causality(),
                scalar_type: variable.value_type().scalar_type(),
                dimensions: variable.value_type().dimensions().into(),
                scalar_count: variable.scalar_count(),
            })
            .collect::<Vec<_>>()
            .into_boxed_slice();
        DaeVariableRefinementView { entries }
    }

    pub fn predefined_string_declaration(self) -> Option<rumoca_core::DefId> {
        self.dae.storage.predefined_string_declaration
    }

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

    storage_count_accessors! {
        expression_count => expressions.nodes,
        variable_count => variables,
        domain_count => domains,
        value_type_count => value_types,
        function_count => functions,
        continuous_equation_count => continuous_equations,
        continuous_owner_count => continuous_equation_owners,
        continuous_family_count => continuous_families,
        initialization_family_count => initialization_families,
        initialization_equation_count => initialization_equations,
        initialization_owner_count => initialization_equation_owners,
        initial_discrete_value_count => initial_discrete_values,
        discrete_real_equation_count => discrete_real_equations,
        discrete_value_owner_count => discrete_value_owners,
        model_event_transaction_count => model_event_transactions,
        relation_count => relations,
        condition_count => conditions,
        root_count => roots,
        structured_root_count => structured_roots,
        time_event_count => time_events,
        event_action_count => event_actions,
        clock_count => clocks,
        clock_ownership_count => clock_ownerships,
        previous_value_count => previous_values,
        terminal_count => terminals,
        delay_count => delays,
    }

    storage_id_accessors! {
        domain_id => (DomainId, domains),
        value_type_id => (ValueTypeId, value_types),
        expression_id => (ExprId, expressions.nodes),
        function_id => (FunctionId, functions),
        variable_id => (VariableId, variables),
        discrete_value_owner_id => (DiscreteValueOwnerId, discrete_value_owners),
        model_event_transaction_id => (ModelEventTransactionId, model_event_transactions),
        relation_id => (RelationId, relations),
        condition_id => (ConditionId, conditions),
        root_id => (RootId, roots),
        structured_root_id => (StructuredRootId, structured_roots),
        time_event_id => (TimeEventId, time_events),
        event_action_id => (EventActionId, event_actions),
        clock_id => (ClockId, clocks),
        clock_ownership_id => (ClockOwnershipId, clock_ownerships),
        previous_id => (PreviousId, previous_values),
        terminal_id => (TerminalId, terminals),
        delay_id => (DelayId, delays),
    }

    pub fn domain(self, id: DomainId<'dae>) -> Option<DomainView<'dae>> {
        Some(DomainView {
            entry: self.dae.storage.domains.get(id.index() as usize)?,
            marker: PhantomData,
        })
    }

    pub fn value_type(self, id: ValueTypeId<'dae>) -> Option<&'dae ValueType> {
        self.dae.storage.value_types.get(id.index() as usize)
    }

    /// Total lookup for a value-type identity issued by this exact branded DAE
    /// view: the identity is minted only over an occupied arena slot, so the
    /// brand carries the resolution proof.
    pub fn exact_value_type(self, id: ValueTypeId<'dae>) -> &'dae ValueType {
        &self.dae.storage.value_types[id.index() as usize]
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

    /// Return the one certified element-major packing layout for a record
    /// field, including compact outer record-array dimensions.
    pub fn record_field_layout(
        self,
        id: ValueTypeId<'dae>,
        ordinal: usize,
    ) -> Option<RecordFieldLayout> {
        let record = self.value_type(id)?;
        if !record.is_record() {
            return None;
        }
        let outer_count = checked_extent_product(record.dimensions())?;
        let field_offset = (0..ordinal).try_fold(0usize, |offset, field| {
            let (_, value_type) = self.record_field(id, field)?;
            offset.checked_add(self.packed_value_count(value_type)?)
        })?;
        let (_, selected) = self.record_field(id, ordinal)?;
        let field_width = self.packed_value_count(selected)?;
        let record_width = (0..record.record_field_count()).try_fold(0usize, |width, field| {
            let (_, value_type) = self.record_field(id, field)?;
            width.checked_add(self.packed_value_count(value_type)?)
        })?;
        outer_count.checked_mul(record_width)?;
        outer_count.checked_mul(field_width)?;
        Some(RecordFieldLayout {
            outer_count,
            record_width,
            field_offset,
            field_width,
        })
    }

    /// Number of scalar lanes one value of this type occupies once packed.
    ///
    /// This is `ValueType::scalar_count` for every non-record type. A record
    /// has no scalar count of its own, so consumers that must lay one out in a
    /// scalar execution buffer, such as a loop-carried tuple, use the packed
    /// width of its fields instead.
    pub fn packed_scalar_count(self, id: ValueTypeId<'dae>) -> Option<usize> {
        self.packed_value_count(id)
    }

    fn packed_value_count(self, id: ValueTypeId<'dae>) -> Option<usize> {
        let value_type = self.value_type(id)?;
        if !value_type.is_record() {
            return value_type.scalar_count();
        }
        let outer_count = checked_extent_product(value_type.dimensions())?;
        let record_width =
            (0..value_type.record_field_count()).try_fold(0usize, |width, ordinal| {
                let (_, field) = self.record_field(id, ordinal)?;
                width.checked_add(self.packed_value_count(field)?)
            })?;
        outer_count.checked_mul(record_width)
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

    pub fn function(self, id: FunctionId<'dae>) -> Option<FunctionView<'dae>> {
        Some(FunctionView {
            dae: self.dae,
            id,
            entry: self.dae.storage.functions.get(id.index() as usize)?,
        })
    }

    /// Every identity in the finalized function arena, minted by the same
    /// walk that observes its occupied slot.
    pub fn function_identities(self) -> impl ExactSizeIterator<Item = FunctionId<'dae>> {
        arena_walks::ArenaWalk::new(&self.dae.storage.functions)
            .map(|(raw, _)| FunctionId::from_raw(raw))
    }

    /// Total lookup for an identity issued by this exact branded DAE view.
    pub fn exact_function(self, id: FunctionId<'dae>) -> FunctionView<'dae> {
        FunctionView {
            dae: self.dae,
            id,
            entry: &self.dae.storage.functions[id.index() as usize],
        }
    }

    pub fn function_definition(
        self,
        id: FunctionDefinitionId<'dae>,
    ) -> Option<FunctionDefinitionView<'dae>> {
        let entry = self
            .dae
            .storage
            .functions
            .get(id.function().index() as usize)?
            .definitions
            .get(id.ordinal() as usize)?;
        Some(FunctionDefinitionView { id, entry })
    }

    /// Total lookup for an owner-local identity issued by this exact DAE.
    pub fn exact_function_definition(
        self,
        id: FunctionDefinitionId<'dae>,
    ) -> FunctionDefinitionView<'dae> {
        FunctionDefinitionView {
            id,
            entry: &self.dae.storage.functions[id.function().index() as usize].definitions
                [id.ordinal() as usize],
        }
    }

    /// Total lookup for a parameter identity issued by this exact branded DAE
    /// view: the identity is minted only over an occupied parameter slot, so
    /// the brand carries the resolution proof.
    pub fn exact_function_parameter(
        self,
        id: FunctionParameterId<'dae>,
    ) -> FunctionParameterView<'dae> {
        FunctionParameterView {
            id,
            entry: &self.dae.storage.functions[id.function().index() as usize].parameter_values
                [id.ordinal() as usize],
        }
    }

    /// Total lookup for a value identity issued by this exact branded DAE
    /// view: the identity is minted only over an occupied value slot, so the
    /// brand carries the resolution proof.
    pub fn exact_function_value(self, id: FunctionValueId<'dae>) -> FunctionValueView<'dae> {
        FunctionValueView {
            id,
            entry: &self.dae.storage.functions[id.function().index() as usize].values
                [id.ordinal() as usize],
        }
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
                dae: self.dae,
                id,
                entry,
                marker: PhantomData,
            },
        )
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

    /// Every identity in the finalized expression arena, minted by the same
    /// walk that observes its occupied slot.
    pub fn expression_identities(self) -> impl ExactSizeIterator<Item = ExprId<'dae>> {
        arena_walks::ArenaWalk::new(&self.dae.storage.expressions.nodes)
            .map(|(raw, _)| ExprId::from_raw(raw))
    }

    /// Total lookup for an expression identity issued by this exact DAE.
    pub fn exact_expression(self, id: ExprId<'dae>) -> ExpressionView<'dae> {
        let index = id.index() as usize;
        let value_type = self.dae.storage.expressions.value_types[index];
        ExpressionView {
            dae: self.dae,
            node: &self.dae.storage.expressions.nodes[index],
            provenance: self.dae.storage.expressions.provenance[index],
            variability: self.dae.storage.expressions.variability[index],
            binder_domain: self.dae.storage.expressions.binder_domains[index],
            function_scope: self.dae.storage.expressions.function_scopes[index],
            value_type_id: ValueTypeId::from_raw(value_type),
            value_type: &self.dae.storage.value_types[value_type as usize],
        }
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

    pub fn discrete_value_owner(
        self,
        id: DiscreteValueOwnerId<'dae>,
    ) -> Option<DiscreteValueOwnerView<'dae>> {
        let entry = self
            .dae
            .storage
            .discrete_value_owners
            .get(id.index() as usize)?;
        Some(DiscreteValueOwnerView {
            targets: crate::DiscreteValueTargets {
                raw: self
                    .dae
                    .storage
                    .discrete_value_targets
                    .get(entry.targets.indices())?,
                marker: PhantomData,
            },
            branches: crate::DiscreteValueBranches {
                entries: self
                    .dae
                    .storage
                    .discrete_value_branches
                    .get(entry.branches.indices())?,
                values: &self.dae.storage.discrete_value_branch_values,
                value_provenance: &self.dae.storage.discrete_value_branch_value_provenance,
                marker: PhantomData,
            },
            structure: entry
                .structure
                .map(|structure| crate::StructuredDiscreteValueView {
                    domain: DomainId::from_raw(structure.domain),
                    scalar_view: structure.scalar_view,
                    scalar_rows: structure.scalar_rows,
                }),
            provenance: entry.provenance,
        })
    }

    pub fn discrete_value_definition_count(self) -> usize {
        self.dae
            .storage
            .discrete_value_owners
            .iter()
            .map(|owner| owner.targets.len as usize)
            .sum()
    }

    pub fn periodic_clock(
        self,
        id: crate::PeriodicClockId<'dae>,
    ) -> &'dae rumoca_core::PeriodicClockSchedule {
        let entry = &self.dae.storage.clocks[id.index() as usize];
        let crate::clocks::ClockKind::Periodic(lattice) = &entry.kind else {
            unreachable!("PeriodicClockId is minted only for periodic clocks");
        };
        lattice
    }

    pub fn delay(self, id: DelayId<'dae>) -> Option<DelayView<'dae>> {
        let entry = self.dae.storage.delays.get(id.index() as usize)?;
        let source = ExprId::from_raw(entry.source);
        let source_view = self.expression(source)?;
        let operation = match &entry.kind {
            DelayKind::ParameterDelay { delay_time } => DelayOperation::ParameterDelay {
                delay_time: positive_parameter_view(delay_time),
            },
            DelayKind::BoundedDelay {
                delay_time,
                delay_max,
            } => DelayOperation::BoundedDelay {
                delay_time: ExprId::from_raw(*delay_time),
                delay_max: positive_parameter_view(delay_max),
            },
        };
        Some(DelayView {
            source,
            operation,
            value_type: source_view.value_type(),
            variability: source_view.variability(),
            provenance: entry.provenance,
        })
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

const fn optional_expression_id<'dae>(raw: Option<u32>) -> Option<ExprId<'dae>> {
    match raw {
        Some(raw) => Some(ExprId::from_raw(raw)),
        None => None,
    }
}

impl<'dae> VariableView<'dae> {
    view_getters! {
        const fn id -> VariableId<'dae> = |view| view.id;
        const fn role -> VariableRole = |view| view.entry.role;
        const fn variability -> ExpressionVariability = |view| view.entry.variability;
        const fn value_type -> &'dae ValueType = |view| view.value_type;
        const fn value_type_id -> ValueTypeId<'dae> = |view| view.value_type_id;
        const fn declaration -> DaeProvenance = |view| view.entry.declaration;
        const fn source_occurrence -> rumoca_core::SourceOccurrenceId =
            |view| view.entry.source_occurrence;
        fn name -> &'dae VarName = |view| &view.entry.name;
        fn component_reference -> Option<&'dae ComponentReference> =
            |view| view.attributes().component_ref.as_ref();
        fn binding -> Option<ExprId<'dae>> =
            |view| optional_expression_id(view.attributes().binding);
        fn start -> Option<ExprId<'dae>> =
            |view| optional_expression_id(view.attributes().start);
        fn fixed -> rumoca_core::Fixity = |view| view.attributes().fixed;
        fn minimum -> Option<ExprId<'dae>> =
            |view| optional_expression_id(view.attributes().min);
        fn maximum -> Option<ExprId<'dae>> =
            |view| optional_expression_id(view.attributes().max);
        fn nominal -> Option<ExprId<'dae>> =
            |view| optional_expression_id(view.attributes().nominal);
        fn unit -> Option<&'dae str> = |view| view.attributes().unit.as_deref();
        fn state_select -> StateSelect = |view| view.attributes().state_select;
        fn description -> Option<&'dae str> =
            |view| view.attributes().description.as_deref();
        fn causality -> VariableCausality = |view| view.attributes().causality;
        fn is_tunable -> bool = |view| view.attributes().is_tunable;
        fn is_held -> bool = |view| view.attributes().is_held;
        fn origin -> VariableOrigin = |view| view.attributes().origin;
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
    dae: &'dae Dae,
    id: FunctionId<'dae>,
    entry: &'dae FunctionEntry,
}

impl<'dae> FunctionView<'dae> {
    view_getters! {
        const fn id -> FunctionId<'dae> = |view| view.id;
        const fn declaration -> DaeProvenance = |view| view.entry.declaration;
        // `inline`: the MLS §18.3 `Inline`/`LateInline` request this
        // declaration wrote. A request, not a decision: a backend that can
        // substitute this body answers to it, and one that cannot ignores it,
        // because an annotation changes nothing about what the function means.
        const fn inline -> rumoca_core::InlineAnnotation = |view| view.entry.inline;
        fn name -> &'dae VarName = |view| &view.entry.name;
        fn parameter_types -> ValueTypeOperands<'dae> = |view| ValueTypeOperands {
            raw: &view.entry.parameters,
            marker: PhantomData,
        };
        fn result_types -> ValueTypeOperands<'dae> = |view| ValueTypeOperands {
            raw: &view.entry.results,
            marker: PhantomData,
        };
        fn definition_count -> usize = |view| view.entry.definitions.len();
        fn fold_count -> usize = |view| view.entry.folds.len();
    }

    pub fn parameters(self) -> impl ExactSizeIterator<Item = FunctionParameterView<'dae>> {
        arena_walks::ArenaWalk::new(&self.entry.parameter_values).map(move |(ordinal, entry)| {
            FunctionParameterView {
                id: FunctionParameterId::from_raw(self.id.index(), ordinal),
                entry,
            }
        })
    }

    /// Every reaching-definition identity owned by this finalized function.
    pub fn definition_identities(
        self,
    ) -> impl ExactSizeIterator<Item = FunctionDefinitionId<'dae>> {
        arena_walks::ArenaWalk::new(&self.entry.definitions)
            .map(move |(raw, _)| FunctionDefinitionId::from_raw(self.id.index(), raw))
    }

    /// Result definitions of a Modelica body.
    ///
    /// An MLS §12.9 external body defines its outputs through its foreign
    /// interface rather than through DAE definitions, so this view is empty
    /// there and consumers must reach [`FunctionView::external`] instead of
    /// treating a missing definition as a value.
    pub fn result_values(self) -> FunctionDefinitionValues<'dae> {
        let definition = self
            .entry
            .definition
            .as_ref()
            .expect("final DAE cannot contain an undefined function");
        FunctionDefinitionValues {
            dae: self.dae,
            function: self.id,
            raw: definition.modelica().map_or(&[][..], |body| &body.results),
        }
    }

    /// The checked MLS §12.9 external interface, when this function has one.
    pub fn external(self) -> Option<ExternalFunctionView<'dae>> {
        self.entry
            .definition
            .as_ref()
            .expect("final DAE cannot contain an undefined function")
            .external()
            .map(|entry| ExternalFunctionView {
                function: self.id,
                entry,
            })
    }

    /// Whether this function is defined by a foreign body rather than Modelica.
    pub fn is_external(self) -> bool {
        self.external().is_some()
    }

    pub fn definition_id(self, index: usize) -> Option<FunctionDefinitionId<'dae>> {
        let ordinal = u32::try_from(index).ok()?;
        (index < self.entry.definitions.len())
            .then(|| FunctionDefinitionId::from_raw(self.id.index(), ordinal))
    }

    pub fn fold_id(self, index: usize) -> Option<FunctionFoldId<'dae>> {
        self.entry
            .folds
            .get(index)
            .and_then(|_| u32::try_from(index).ok())
            .map(|ordinal| FunctionFoldId::from_raw(self.id.index(), ordinal))
    }

    pub fn values(self) -> impl ExactSizeIterator<Item = FunctionValueView<'dae>> {
        arena_walks::ArenaWalk::new(&self.entry.values).map(move |(ordinal, entry)| {
            FunctionValueView {
                id: FunctionValueId::from_raw(self.id.index(), ordinal),
                entry,
            }
        })
    }

    /// Ordered Modelica body statements; empty for an external interface.
    pub fn statements(self) -> FunctionStatements<'dae> {
        let definition = self
            .entry
            .definition
            .as_ref()
            .expect("final DAE cannot contain an undefined function");
        FunctionStatements {
            dae: self.dae,
            function: self.id,
            statements: definition
                .modelica()
                .map_or(&[][..], |body| &body.statements),
            next: 0,
        }
    }
}

/// Read-only view of one checked MLS §12.9 external function interface.
#[derive(Clone, Copy)]
pub struct ExternalFunctionView<'dae> {
    function: FunctionId<'dae>,
    entry: &'dae ExternalBodyEntry,
}

impl<'dae> ExternalFunctionView<'dae> {
    view_getters! {
        const fn purity -> FunctionPurity = |view| view.entry.purity;
        const fn language -> ExternalLanguage = |view| view.entry.language;
        fn symbol -> &'dae VarName = |view| &view.entry.symbol;
        fn linkage -> &'dae ExternalLinkage = |view| &view.entry.linkage;
        fn argument_count -> usize = |view| view.entry.arguments.len();
        fn provenance -> DaeProvenance = |view| view.entry.provenance;
    }

    /// Output bound by the MLS §12.9 `output = symbol(...)` return form.
    pub fn result(self) -> Option<FunctionValueId<'dae>> {
        self.entry
            .result
            .map(|value| FunctionValueId::from_raw(self.function.index(), value))
    }

    /// Ordered ABI arguments in declaration order.
    pub fn arguments(self) -> impl ExactSizeIterator<Item = ExternalArgumentView<'dae>> {
        let function = self.function;
        self.entry
            .arguments
            .iter()
            .map(move |argument| match argument {
                ExternalArgumentEntry::Input(expression) => {
                    ExternalArgumentView::Input(ExprId::from_raw(*expression))
                }
                ExternalArgumentEntry::Output(value) => ExternalArgumentView::Output(
                    FunctionValueId::from_raw(function.index(), *value),
                ),
            })
    }
}

/// One ordered ABI argument position of an external interface.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExternalArgumentView<'dae> {
    Input(ExprId<'dae>),
    Output(FunctionValueId<'dae>),
}

#[derive(Clone, Copy)]
pub struct FunctionParameterView<'dae> {
    id: FunctionParameterId<'dae>,
    entry: &'dae FunctionParameterEntry,
}

impl<'dae> FunctionParameterView<'dae> {
    view_getters! {
        const fn id -> FunctionParameterId<'dae> = |view| view.id;
        const fn value_type -> ValueTypeId<'dae> =
            |view| ValueTypeId::from_raw(view.entry.value_type);
        const fn declaration -> DaeProvenance = |view| view.entry.declaration;
        fn name -> &'dae VarName = |view| &view.entry.name;
    }
}

#[derive(Clone, Copy)]
pub struct FunctionValueView<'dae> {
    id: FunctionValueId<'dae>,
    entry: &'dae FunctionValueEntry,
}

impl<'dae> FunctionValueView<'dae> {
    view_getters! {
        const fn id -> FunctionValueId<'dae> = |view| view.id;
        const fn value_type -> ValueTypeId<'dae> =
            |view| ValueTypeId::from_raw(view.entry.value_type);
        const fn role -> FunctionValueRole = |view| view.entry.role;
        const fn declaration -> DaeProvenance = |view| view.entry.declaration;
        fn name -> &'dae VarName = |view| &view.entry.name;
    }
}

#[derive(Clone, Copy)]
pub struct FunctionDefinitionView<'dae> {
    id: FunctionDefinitionId<'dae>,
    entry: &'dae FunctionDefinitionEntry,
}

impl<'dae> FunctionDefinitionView<'dae> {
    view_getters! {
        const fn id -> FunctionDefinitionId<'dae> = |view| view.id;
        const fn target -> FunctionValueId<'dae> = |view| FunctionValueId::from_raw(
            view.id.function().index(),
            view.entry.target,
        );
        const fn rhs -> ExprId<'dae> = |view| ExprId::from_raw(view.entry.rhs);
        const fn provenance -> DaeProvenance = |view| view.entry.provenance;
    }
}

#[derive(Clone, Copy)]
pub struct FunctionDefinitionValues<'dae> {
    dae: &'dae Dae,
    function: FunctionId<'dae>,
    raw: &'dae [u32],
}

impl<'dae> FunctionDefinitionValues<'dae> {
    view_getters! {
        const fn len -> usize = |view| view.raw.len();
        const fn is_empty -> bool = |view| view.raw.is_empty();
    }

    pub fn get(self, index: usize) -> Option<FunctionDefinitionView<'dae>> {
        self.raw
            .get(index)
            .copied()
            .map(|raw| function_definition_view(self.dae, self.function, raw))
    }

    pub fn iter(self) -> impl ExactSizeIterator<Item = FunctionDefinitionView<'dae>> {
        self.raw
            .iter()
            .copied()
            .map(move |raw| function_definition_view(self.dae, self.function, raw))
    }

    pub fn rhs(self, index: usize) -> Option<ExprId<'dae>> {
        self.get(index).map(FunctionDefinitionView::rhs)
    }

    pub fn rhs_iter(self) -> impl ExactSizeIterator<Item = ExprId<'dae>> {
        self.iter().map(FunctionDefinitionView::rhs)
    }
}

fn function_definition_view<'dae>(
    dae: &'dae Dae,
    function: FunctionId<'dae>,
    ordinal: u32,
) -> FunctionDefinitionView<'dae> {
    let id = FunctionDefinitionId::from_raw(function.index(), ordinal);
    DaeView {
        dae,
        marker: PhantomData,
    }
    .function_definition(id)
    .expect("final DAE definition identity resolves")
}

#[derive(Clone)]
pub enum FunctionStatementView<'dae> {
    Assignment {
        definition: FunctionDefinitionView<'dae>,
    },
    AssignmentGroup {
        definitions: FunctionDefinitionValues<'dae>,
        conditional: Option<FunctionConditionalView<'dae>>,
    },
    Assertion {
        condition: ExprId<'dae>,
        message: ExprId<'dae>,
        provenance: DaeProvenance,
    },
    For {
        fold: FunctionFoldId<'dae>,
        statements: FunctionStatements<'dae>,
        provenance: DaeProvenance,
    },
}

/// Checked shared branch correlation for one atomic function assignment group.
#[derive(Clone, Copy)]
pub struct FunctionConditionalView<'dae> {
    raw: &'dae FunctionConditionalWire,
}

impl<'dae> FunctionConditionalView<'dae> {
    pub fn conditions(self) -> impl ExactSizeIterator<Item = ExprId<'dae>> + 'dae {
        self.raw.conditions.iter().copied().map(ExprId::from_raw)
    }

    pub fn branch_count(self) -> usize {
        self.raw.branches.len()
    }

    pub fn branch(
        self,
        ordinal: usize,
    ) -> Option<impl ExactSizeIterator<Item = ExprId<'dae>> + 'dae> {
        self.raw
            .branches
            .get(ordinal)
            .map(|values| values.iter().copied().map(ExprId::from_raw))
    }

    pub fn fallback(self) -> impl ExactSizeIterator<Item = ExprId<'dae>> + 'dae {
        self.raw.fallback.iter().copied().map(ExprId::from_raw)
    }
}

impl<'dae> FunctionStatementView<'dae> {
    fn from_wire(
        dae: &'dae Dae,
        function: FunctionId<'dae>,
        statement: &'dae FunctionStatementWire,
    ) -> Self {
        match statement {
            FunctionStatementWire::Assignment { definition } => Self::Assignment {
                definition: function_definition_view(dae, function, *definition),
            },
            FunctionStatementWire::AssignmentGroup {
                definitions,
                conditional,
            } => Self::AssignmentGroup {
                definitions: FunctionDefinitionValues {
                    dae,
                    function,
                    raw: definitions,
                },
                conditional: conditional
                    .as_ref()
                    .map(|raw| FunctionConditionalView { raw }),
            },
            FunctionStatementWire::Assertion {
                condition,
                message,
                provenance,
            } => Self::Assertion {
                condition: ExprId::from_raw(*condition),
                message: ExprId::from_raw(*message),
                provenance: *provenance,
            },
            FunctionStatementWire::For {
                fold,
                statements,
                provenance,
            } => Self::For {
                fold: FunctionFoldId::from_raw(function.index(), *fold),
                statements: FunctionStatements {
                    dae,
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
    dae: &'dae Dae,
    function: FunctionId<'dae>,
    statements: &'dae [FunctionStatementWire],
    next: usize,
}

impl<'dae> Iterator for FunctionStatements<'dae> {
    type Item = FunctionStatementView<'dae>;

    fn next(&mut self) -> Option<Self::Item> {
        let statement = self.statements.get(self.next)?;
        self.next += 1;
        Some(FunctionStatementView::from_wire(
            self.dae,
            self.function,
            statement,
        ))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = self.statements.len() - self.next;
        (remaining, Some(remaining))
    }
}

impl ExactSizeIterator for FunctionStatements<'_> {}

#[derive(Clone, Copy)]
pub struct FunctionFoldView<'dae> {
    pub(super) dae: &'dae Dae,
    pub(super) id: FunctionFoldId<'dae>,
    pub(super) entry: &'dae FunctionFoldEntry,
    pub(super) marker: PhantomData<&'dae mut &'dae ()>,
}

impl<'dae> FunctionFoldView<'dae> {
    view_getters! {
        const fn id -> FunctionFoldId<'dae> = |view| view.id;
        const fn domain -> DomainId<'dae> = |view| DomainId::from_raw(view.entry.domain);
        const fn provenance -> DaeProvenance = |view| view.entry.provenance;
        fn initial_values -> FunctionDefinitionValues<'dae> = |view| FunctionDefinitionValues {
            dae: view.dae,
            function: view.id.function(),
            raw: &view.entry.initial_definitions,
        };
        fn parameter_values -> FunctionDefinitionValues<'dae> = |view| FunctionDefinitionValues {
            dae: view.dae,
            function: view.id.function(),
            raw: &view.entry.parameter_definitions,
        };
        fn update_values -> FunctionDefinitionValues<'dae> = |view| FunctionDefinitionValues {
            dae: view.dae,
            function: view.id.function(),
            raw: &view.entry.update_definitions,
        };
        fn output_values -> FunctionDefinitionValues<'dae> = |view| FunctionDefinitionValues {
            dae: view.dae,
            function: view.id.function(),
            raw: &view.entry.output_definitions,
        };
    }

    pub fn targets(self) -> impl ExactSizeIterator<Item = FunctionValueId<'dae>> {
        self.entry
            .targets
            .iter()
            .copied()
            .map(move |target| FunctionValueId::from_raw(self.entry.function, target))
    }

    pub fn iteration_locals(self) -> impl ExactSizeIterator<Item = FunctionValueId<'dae>> {
        self.entry
            .iteration_locals
            .iter()
            .copied()
            .map(move |target| FunctionValueId::from_raw(self.entry.function, target))
    }
}

raw_id_slice_view!(ValueTypeOperands => ValueTypeId);

#[derive(Clone, Copy)]
pub struct DomainView<'dae> {
    entry: &'dae DomainEntry,
    marker: PhantomData<&'dae mut &'dae ()>,
}

impl<'dae> DomainView<'dae> {
    view_getters! {
        const fn scalar_count -> u32 = |view| view.entry.scalar_count;
        const fn provenance -> DaeProvenance = |view| view.entry.provenance;
        fn parent -> Option<DomainId<'dae>> =
            |view| view.entry.parent.map(DomainId::from_raw);
        fn structured -> &'dae StructuredIndexDomain = |view| &view.entry.domain;
        fn extents -> &'dae [u32] = |view| &view.entry.extents;
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
    view_getters! {
        const fn provenance -> DaeProvenance = |view| view.provenance;
        const fn value_type -> &'dae ValueType = |view| view.value_type;
        const fn value_type_id -> ValueTypeId<'dae> = |view| view.value_type_id;
        const fn variability -> ExpressionVariability = |view| view.variability;
        fn binder_domain -> Option<DomainId<'dae>> =
            |view| view.binder_domain.map(DomainId::from_raw);
        fn function_scope -> Option<FunctionId<'dae>> =
            |view| view.function_scope.map(FunctionId::from_raw);
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
            ExprNode::StringConversion { .. } => ExpressionKind::StringConversion,
            ExprNode::FunctionValue { .. } => ExpressionKind::FunctionValue,
            ExprNode::FunctionFoldParameter { .. } => ExpressionKind::FunctionFoldParameter,
            ExprNode::FunctionFoldOutput { .. } => ExpressionKind::FunctionFoldOutput,
            ExprNode::ClockTransfer { .. } => ExpressionKind::ClockTransfer,
        }
    }

    pub fn operation(self) -> ExpressionOperation<'dae> {
        match self.node {
            ExprNode::Literal(_)
            | ExprNode::Coordinate(_)
            | ExprNode::Unary { .. }
            | ExprNode::Binary { .. } => self.primitive_operation(),
            ExprNode::Conditional { .. }
            | ExprNode::Array { .. }
            | ExprNode::Record { .. }
            | ExprNode::Field { .. }
            | ExprNode::Range { .. }
            | ExprNode::Comprehension { .. } => self.aggregate_operation(),
            ExprNode::Index { .. }
            | ExprNode::ArrayUpdate { .. }
            | ExprNode::Builtin { .. }
            | ExprNode::Call { .. }
            | ExprNode::StringConversion { .. } => self.application_operation(),
            ExprNode::ClockTransfer { .. } => self.application_operation(),
            ExprNode::FunctionValue { .. }
            | ExprNode::FunctionFoldParameter { .. }
            | ExprNode::FunctionFoldOutput { .. } => self.function_operation(),
        }
    }

    fn primitive_operation(self) -> ExpressionOperation<'dae> {
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
            _ => unreachable!("expression operation family is selected from its checked node"),
        }
    }

    fn aggregate_operation(self) -> ExpressionOperation<'dae> {
        match self.node {
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
            ExprNode::Range {
                start,
                explicit_step,
                stop,
            } => ExpressionOperation::Range(RangeView {
                dae: self.dae,
                start: *start,
                explicit_step: *explicit_step,
                stop: *stop,
            }),
            ExprNode::Comprehension { domain, body } => ExpressionOperation::Comprehension {
                domain: DomainId::from_raw(*domain),
                body: ExprId::from_raw(*body),
            },
            _ => unreachable!("expression operation family is selected from its checked node"),
        }
    }

    fn application_operation(self) -> ExpressionOperation<'dae> {
        match self.node {
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
                owner,
                function,
                output,
                operands,
            } => ExpressionOperation::Call {
                owner: ExprId::from_raw(*owner),
                function: FunctionId::from_raw(*function),
                output: *output,
                arguments: self.expression_operands(*operands),
            },
            ExprNode::StringConversion {
                declaration,
                value,
                minimum_length,
                left_justified,
                significant_digits,
                format,
            } => ExpressionOperation::StringConversion {
                declaration: *declaration,
                value: ExprId::from_raw(*value),
                format: match format {
                    Some(format) => StringConversionFormatView::Format {
                        value: ExprId::from_raw(*format),
                    },
                    None => StringConversionFormatView::Options {
                        minimum_length: optional_expression_id(*minimum_length),
                        left_justified: optional_expression_id(*left_justified),
                        significant_digits: optional_expression_id(*significant_digits),
                    },
                },
            },
            ExprNode::ClockTransfer {
                kind,
                source,
                source_clock,
                target_clock,
            } => ExpressionOperation::ClockTransfer {
                kind: *kind,
                source: ExprId::from_raw(*source),
                source_clock: ClockId::from_raw(*source_clock),
                target_clock: ClockId::from_raw(*target_clock),
            },
            _ => unreachable!("expression operation family is selected from its checked node"),
        }
    }

    fn function_operation(self) -> ExpressionOperation<'dae> {
        match self.node {
            ExprNode::FunctionValue {
                function,
                value,
                definition_ordinal,
            } => ExpressionOperation::FunctionValue {
                value: FunctionValueId::from_raw(*function, *value),
                definition: function_definition_view(
                    self.dae,
                    FunctionId::from_raw(*function),
                    *definition_ordinal,
                ),
            },
            ExprNode::FunctionFoldParameter {
                function,
                fold,
                carried,
                definition_ordinal,
            } => ExpressionOperation::FunctionFoldParameter {
                fold: FunctionFoldId::from_raw(*function, *fold),
                carried: *carried,
                definition: function_definition_view(
                    self.dae,
                    FunctionId::from_raw(*function),
                    *definition_ordinal,
                ),
            },
            ExprNode::FunctionFoldOutput {
                function,
                fold,
                carried,
                definition_ordinal,
            } => ExpressionOperation::FunctionFoldOutput {
                fold: FunctionFoldId::from_raw(*function, *fold),
                carried: *carried,
                definition: function_definition_view(
                    self.dae,
                    FunctionId::from_raw(*function),
                    *definition_ordinal,
                ),
            },
            _ => unreachable!("expression operation family is selected from its checked node"),
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
                | Coordinate::PreDiscreteValue(variable)
                | Coordinate::PreState(variable)
                | Coordinate::PreAlgebraic(variable),
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

raw_id_slice_view!(ExpressionOperands => ExprId);

#[derive(Clone, Copy)]
pub struct SubscriptsView<'dae> {
    raw: &'dae [crate::expression::PackedSubscript],
    marker: PhantomData<&'dae mut &'dae ()>,
}

impl<'dae> SubscriptsView<'dae> {
    view_getters! {
        const fn len -> usize = |view| view.raw.len();
        const fn is_empty -> bool = |view| view.raw.is_empty();
    }

    pub fn get(self, index: usize) -> Option<SubscriptView<'dae>> {
        self.raw.get(index).map(subscript_view)
    }

    pub fn iter(self) -> impl ExactSizeIterator<Item = SubscriptView<'dae>> {
        self.raw.iter().map(subscript_view)
    }
}

fn subscript_view(subscript: &crate::expression::PackedSubscript) -> SubscriptView<'_> {
    match subscript.kind {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
    /// `pre()` of a continuous state: its left limit at event entry.
    PreState(StateId<'dae>),
    /// `pre()` of a continuous algebraic/output: its left limit at event entry.
    PreAlgebraic(AlgebraicId<'dae>),
    Time,
    ClockInterval(crate::PeriodicClockId<'dae>),
    Condition(ConditionId<'dae>),
    Delay(crate::DelayId<'dae>),
    Previous(crate::PreviousId<'dae>),
    Terminal(crate::TerminalId<'dae>),
    Binder(DomainBinderId<'dae>),
    FunctionParameter(FunctionParameterId<'dae>),
}

#[derive(Clone, Copy)]
pub struct RangeBoundView<'dae> {
    id: ExprId<'dae>,
    value: i64,
    provenance: DaeProvenance,
}

impl<'dae> RangeBoundView<'dae> {
    view_getters! {
        const fn expression -> ExprId<'dae> = |view| view.id;
        const fn value -> i64 = |view| view.value;
        const fn provenance -> DaeProvenance = |view| view.provenance;
    }
}

#[derive(Clone, Copy)]
pub struct RangeView<'dae> {
    dae: &'dae Dae,
    start: u32,
    explicit_step: Option<u32>,
    stop: u32,
}

impl<'dae> RangeView<'dae> {
    pub fn start(self) -> RangeBoundView<'dae> {
        self.bound(self.start)
    }

    pub fn explicit_step(self) -> Option<RangeBoundView<'dae>> {
        self.explicit_step.map(|step| self.bound(step))
    }

    pub fn stop(self) -> RangeBoundView<'dae> {
        self.bound(self.stop)
    }

    pub fn effective_step(self) -> i64 {
        self.explicit_step().map_or(1, |step| step.value())
    }

    fn bound(self, raw: u32) -> RangeBoundView<'dae> {
        let id = ExprId::from_raw(raw);
        let node = self
            .dae
            .storage
            .expressions
            .nodes
            .get(raw as usize)
            .expect("checked range bound resolves");
        let ExprNode::Literal(DaeLiteral::Integer(value)) = node else {
            unreachable!("checked range bounds are literal Integer expressions")
        };
        let provenance = self.dae.storage.expressions.provenance[raw as usize];
        RangeBoundView {
            id,
            value: *value,
            provenance,
        }
    }
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
    Range(RangeView<'dae>),
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
        owner: ExprId<'dae>,
        function: FunctionId<'dae>,
        output: u32,
        arguments: ExpressionOperands<'dae>,
    },
    StringConversion {
        declaration: rumoca_core::DefId,
        value: ExprId<'dae>,
        format: StringConversionFormatView<'dae>,
    },
    ClockTransfer {
        kind: crate::ClockTransferKind,
        source: ExprId<'dae>,
        source_clock: ClockId<'dae>,
        target_clock: ClockId<'dae>,
    },
    FunctionValue {
        value: FunctionValueId<'dae>,
        definition: FunctionDefinitionView<'dae>,
    },
    FunctionFoldParameter {
        fold: FunctionFoldId<'dae>,
        carried: u32,
        definition: FunctionDefinitionView<'dae>,
    },
    FunctionFoldOutput {
        fold: FunctionFoldId<'dae>,
        carried: u32,
        definition: FunctionDefinitionView<'dae>,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum StringConversionFormatView<'dae> {
    Options {
        minimum_length: Option<ExprId<'dae>>,
        left_justified: Option<ExprId<'dae>>,
        significant_digits: Option<ExprId<'dae>>,
    },
    Format {
        value: ExprId<'dae>,
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
        Coordinate::PreState(raw) => CoordinateView::PreState(StateId::from_raw(raw)),
        Coordinate::PreAlgebraic(raw) => CoordinateView::PreAlgebraic(AlgebraicId::from_raw(raw)),
        Coordinate::Time => CoordinateView::Time,
        Coordinate::ClockInterval(raw) => {
            CoordinateView::ClockInterval(crate::PeriodicClockId::from_raw(raw))
        }
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
    StringConversion,
    FunctionValue,
    FunctionFoldParameter,
    FunctionFoldOutput,
    ClockTransfer,
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
    view_getters! {
        const fn domain -> DomainId<'dae> = |view| view.domain;
        const fn scalar_view -> rumoca_core::ComprehensionScalarView =
            |view| view.scalar_view;
        const fn bodies -> ExpressionOperands<'dae> = |view| view.bodies;
        const fn scalar_rows -> u32 = |view| view.scalar_rows;
        const fn provenance -> DaeProvenance = |view| view.provenance;
    }
}

impl<'dae> ResidualEquationView<'dae> {
    view_getters! {
        const fn residual -> ExprId<'dae> = |view| view.residual;
        const fn provenance -> DaeProvenance = |view| view.provenance;
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
