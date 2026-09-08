use super::analysis::function_statement_products::FunctionLoweringPlan;
use super::function_shapes::FunctionCallShapeCertificate;
use super::*;

pub(super) fn advance_function_lowering_record_staging(
    plan: &FunctionLoweringPlan,
    available: &mut HashSet<FunctionRecordFieldIdentity>,
) {
    match plan {
        FunctionLoweringPlan::Assignment(assignment) => {
            advance_lowering_assignment_staging(assignment, available);
        }
        FunctionLoweringPlan::MultiOutputCall { outputs } => {
            for assignment in outputs.iter().flatten() {
                advance_lowering_assignment_staging(assignment, available);
            }
        }
        FunctionLoweringPlan::RecordMultiOutputAssembly(assembly) => {
            available.retain(|identity| identity.target != assembly.target_def_id);
        }
        FunctionLoweringPlan::RecordAssembly(assembly) => {
            available.retain(|identity| identity.target != assembly.target_def_id);
        }
        FunctionLoweringPlan::RecordFieldAssembly(assembly) => {
            let identity = FunctionRecordFieldIdentity {
                target: assembly.target_def_id,
                field: assembly.field.def_id,
            };
            if assembly.finalize_fields.is_some() {
                available.retain(|field| field.target != identity.target);
            } else {
                available.insert(identity);
            }
        }
        FunctionLoweringPlan::If {
            branches, fallback, ..
        } => {
            let mut paths = branches
                .iter()
                .map(|branch| lowering_record_staging_after(branch, available))
                .collect::<Vec<_>>();
            paths.push(match fallback {
                Some(sequence) => lowering_record_staging_after(sequence, available),
                None => available.clone(),
            });
            intersect_lowering_staging_paths(available, &paths);
        }
        FunctionLoweringPlan::ProvenBranch { statements, .. } => {
            advance_function_sequence_record_staging(statements, available);
        }
        FunctionLoweringPlan::For { statements, .. } => {
            let body = lowering_record_staging_after(statements, available);
            available.retain(|identity| body.contains(identity));
        }
        FunctionLoweringPlan::ProvenAssertion
        | FunctionLoweringPlan::RuntimeAssertion
        | FunctionLoweringPlan::GeneratedBooleanAssignment { .. }
        | FunctionLoweringPlan::ArrayAssembly(_) => {}
    }
}

fn advance_function_sequence_record_staging(
    sequence: &FunctionStatementSequence,
    available: &mut HashSet<FunctionRecordFieldIdentity>,
) {
    for product in sequence.products() {
        advance_function_lowering_record_staging(product.plan(), available);
    }
}

fn lowering_record_staging_after(
    sequence: &FunctionStatementSequence,
    incoming: &HashSet<FunctionRecordFieldIdentity>,
) -> HashSet<FunctionRecordFieldIdentity> {
    let mut available = incoming.clone();
    advance_function_sequence_record_staging(sequence, &mut available);
    available
}

fn advance_lowering_assignment_staging(
    assignment: &FunctionAssignmentPlan,
    available: &mut HashSet<FunctionRecordFieldIdentity>,
) {
    match assignment.record_field() {
        Some(identity) if assignment.is_whole() => {
            available.insert(identity);
        }
        Some(_) => {}
        None => {
            available.retain(|identity| identity.target != assignment.target_def_id());
        }
    }
}

fn intersect_lowering_staging_paths(
    available: &mut HashSet<FunctionRecordFieldIdentity>,
    paths: &[HashSet<FunctionRecordFieldIdentity>],
) {
    let Some(first) = paths.first() else {
        available.clear();
        return;
    };
    *available = first
        .iter()
        .copied()
        .filter(|identity| paths[1..].iter().all(|path| path.contains(identity)))
        .collect();
}

#[derive(Clone, Copy)]
pub(super) struct FunctionSymbols<'symbols, 'dae> {
    pub(super) coordinates: &'symbols HashMap<VarName, Coordinate<'dae>>,
    pub(super) function_values: &'symbols HashMap<rumoca_core::DefId, dae::FunctionValueId<'dae>>,
    pub(super) record_staging: &'symbols FunctionRecordStagingValues<'dae>,
    pub(super) record_staging_available: &'symbols FunctionRecordStagingAvailability,
    pub(super) functions: &'symbols FunctionRegistry<'symbols, 'dae>,
    pub(super) shapes: &'symbols ShapeEnvironment,
}

impl<'symbols, 'dae> FunctionSymbols<'symbols, 'dae> {
    pub(super) fn with_record_staging_available<'short>(
        &'short self,
        available: &'short FunctionRecordStagingAvailability,
    ) -> FunctionSymbols<'short, 'dae> {
        FunctionSymbols {
            coordinates: self.coordinates,
            function_values: self.function_values,
            record_staging: self.record_staging,
            record_staging_available: available,
            functions: self.functions,
            shapes: self.shapes,
        }
    }

    pub(super) fn record_staging_scope(self) -> FunctionRecordStagingScope<'symbols, 'dae> {
        FunctionRecordStagingScope::from_inventories(
            self.record_staging,
            self.record_staging_available,
        )
    }
}

#[derive(Clone, Copy)]
pub(super) enum FunctionRecordStagedValue<'dae> {
    Local(dae::FunctionValueId<'dae>),
    Expression(dae::ExprId<'dae>),
}

#[derive(Default)]
pub(super) struct FunctionRecordStagingValues<'dae> {
    by_identity: HashMap<FunctionRecordFieldIdentity, FunctionRecordStagedValue<'dae>>,
}

#[derive(Clone, Default)]
pub(super) struct FunctionRecordStagingAvailability {
    identities: HashSet<FunctionRecordFieldIdentity>,
}

impl FunctionRecordStagingAvailability {
    pub(super) fn advance(&mut self, plan: &FunctionStatementPlan) {
        advance_function_record_staging(plan, &mut self.identities);
    }

    pub(super) fn insert(&mut self, identity: FunctionRecordFieldIdentity) {
        self.identities.insert(identity);
    }
}

#[derive(Clone, Copy)]
pub(super) struct FunctionRecordStagingScope<'symbols, 'dae> {
    values: &'symbols FunctionRecordStagingValues<'dae>,
    available: &'symbols FunctionRecordStagingAvailability,
    overrides: Option<&'symbols HashMap<FunctionRecordFieldIdentity, dae::ExprId<'dae>>>,
}

impl<'symbols, 'dae> FunctionRecordStagingScope<'symbols, 'dae> {
    pub(super) fn from_inventories(
        values: &'symbols FunctionRecordStagingValues<'dae>,
        available: &'symbols FunctionRecordStagingAvailability,
    ) -> Self {
        Self {
            values,
            available,
            overrides: None,
        }
    }

    pub(super) fn get(
        self,
        identity: FunctionRecordFieldIdentity,
    ) -> Option<FunctionRecordStagedValue<'dae>> {
        if !self.available.identities.contains(&identity) {
            return None;
        }
        self.overrides
            .and_then(|values| values.get(&identity).copied())
            .map(FunctionRecordStagedValue::Expression)
            .or_else(|| self.values.get(identity))
    }

    pub(super) fn with_overrides(
        self,
        overrides: &'symbols HashMap<FunctionRecordFieldIdentity, dae::ExprId<'dae>>,
    ) -> Self {
        Self {
            overrides: Some(overrides),
            ..self
        }
    }
}

impl<'dae> FunctionRecordStagingValues<'dae> {
    pub(super) fn insert_local(
        &mut self,
        identity: FunctionRecordFieldIdentity,
        local: dae::FunctionValueId<'dae>,
        span: Span,
    ) -> Result<(), dae::DaeConstructionError> {
        self.insert(identity, FunctionRecordStagedValue::Local(local), span)
    }

    pub(super) fn insert_expression(
        &mut self,
        identity: FunctionRecordFieldIdentity,
        expression: dae::ExprId<'dae>,
        span: Span,
    ) -> Result<(), dae::DaeConstructionError> {
        self.insert(
            identity,
            FunctionRecordStagedValue::Expression(expression),
            span,
        )
    }

    pub(super) fn get(
        &self,
        identity: FunctionRecordFieldIdentity,
    ) -> Option<FunctionRecordStagedValue<'dae>> {
        self.by_identity.get(&identity).copied()
    }

    fn insert(
        &mut self,
        identity: FunctionRecordFieldIdentity,
        value: FunctionRecordStagedValue<'dae>,
        span: Span,
    ) -> Result<(), dae::DaeConstructionError> {
        match self.by_identity.entry(identity) {
            std::collections::hash_map::Entry::Vacant(entry) => {
                entry.insert(value);
                Ok(())
            }
            std::collections::hash_map::Entry::Occupied(_) => {
                Err(dae::DaeConstructionError::DuplicateKey {
                    kind: "function record staging field identity",
                    key: format!("{}:{}", identity.target.index(), identity.field.index()),
                    span,
                })
            }
        }
    }
}

pub(super) struct FunctionRegistry<'shape, 'dae> {
    pub(super) flat: &'shape flat::Model,
    pub(super) shapes: &'shape FunctionShapeAnalysis,
    pub(super) ids: &'shape HashMap<FunctionSpecializationKey, dae::FunctionId<'dae>>,
    pub(super) comprehension_plans: &'shape ComprehensionPlans,
    pub(super) record_array_fields: &'shape RecordArrayFieldPlans,
    pub(super) constants: &'shape EvalContext,
    pub(super) delay_plans: &'shape HashMap<Span, DelayPlan>,
    pub(super) derivatives: &'shape DerivativePlans,
    pub(super) history_operators: &'shape HistoryOperatorPlans,
    pub(super) coordinate_instances: &'shape HashMap<rumoca_core::InstanceId, Coordinate<'dae>>,
    pub(super) state_occurrences:
        &'shape HashMap<rumoca_core::SourceOccurrenceId, dae::StateId<'dae>>,
    /// MLS §8.5 event owners proven for the model equation expressions this
    /// registry lowers. Function bodies never occupy those spans, so the same
    /// registry serves both without leaking model events into functions.
    pub(super) expression_events: &'shape ExpressionEventPlans,
    pub(super) sample_alias_schedules: &'shape HashMap<VarName, PeriodicClockSchedule>,
    pub(super) clock_transfer_plans: &'shape ClockTransferPlans,
    pub(super) clocks: &'shape LoweredClocks<'dae>,
}

impl<'dae> FunctionRegistry<'_, 'dae> {
    pub(super) fn select(
        &self,
        name: &rumoca_core::Reference,
        arguments: &[Expression],
        values: &ShapeEnvironment,
        span: Span,
    ) -> Result<dae::FunctionId<'dae>, dae::DaeConstructionError> {
        Ok(self.select_with_key(name, arguments, values, span)?.1)
    }

    pub(super) fn select_with_key(
        &self,
        name: &rumoca_core::Reference,
        arguments: &[Expression],
        values: &ShapeEnvironment,
        span: Span,
    ) -> Result<(FunctionSpecializationKey, dae::FunctionId<'dae>), dae::DaeConstructionError> {
        let key = self
            .shapes
            .call_key(name, arguments, values, span)
            .map_err(
                |_| dae::DaeConstructionError::MissingFunctionCallCertificate {
                    function: name.var_name().clone(),
                    span,
                },
            )?;
        let id = self.ids.get(&key).copied().ok_or(
            dae::DaeConstructionError::MissingFunctionCallCertificate {
                function: name.var_name().clone(),
                span,
            },
        )?;
        Ok((key, id))
    }

    pub(super) fn select_with_call_certificate(
        &self,
        name: &rumoca_core::Reference,
        arguments: &[Expression],
        values: &ShapeEnvironment,
        span: Span,
    ) -> Result<(&FunctionCallShapeCertificate, dae::FunctionId<'dae>), dae::DaeConstructionError>
    {
        let call = self
            .shapes
            .call_certificate(name, arguments, values, span)
            .map_err(
                |_| dae::DaeConstructionError::MissingFunctionCallCertificate {
                    function: name.var_name().clone(),
                    span,
                },
            )?;
        let id = self.ids.get(&call.specialization.key).copied().ok_or(
            dae::DaeConstructionError::MissingFunctionCallCertificate {
                function: name.var_name().clone(),
                span,
            },
        )?;
        Ok((call, id))
    }
}

pub(super) fn construct_functions<'dae>(
    flat: &flat::Model,
    shapes: &FunctionShapeAnalysis,
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    registry: FunctionRegistryInput<'_, 'dae>,
    plans: &HashMap<FunctionSpecializationKey, FunctionPlan>,
) -> Result<HashMap<FunctionSpecializationKey, dae::FunctionId<'dae>>, dae::DaeConstructionError> {
    let mut ids = HashMap::with_capacity(shapes.certificates().len());
    for component in shapes.construction_components() {
        if component.recursive {
            construct_recursive_component(
                construction,
                coordinates,
                shapes,
                &mut ids,
                registry,
                plans,
                &component.members,
            )?;
        } else {
            let specialization = component.members[0];
            let signature = function_signature(construction, flat, shapes, specialization)?;
            let (function, ()) =
                construction.function(signature, |construction, reservation| {
                    define_function(
                        construction,
                        coordinates,
                        FunctionRegistry::new(registry, shapes, &ids),
                        reservation,
                        specialization,
                        plans,
                    )
                })?;
            ids.insert(shapes.certificates()[specialization].key.clone(), function);
        }
    }
    Ok(ids)
}

#[derive(Clone, Copy)]
pub(super) struct FunctionRegistryInput<'shape, 'dae> {
    pub(super) flat: &'shape flat::Model,
    pub(super) comprehension_plans: &'shape ComprehensionPlans,
    pub(super) record_array_fields: &'shape RecordArrayFieldPlans,
    pub(super) constants: &'shape EvalContext,
    pub(super) delay_plans: &'shape HashMap<Span, DelayPlan>,
    pub(super) derivatives: &'shape DerivativePlans,
    pub(super) history_operators: &'shape HistoryOperatorPlans,
    pub(super) coordinate_instances: &'shape HashMap<rumoca_core::InstanceId, Coordinate<'dae>>,
    pub(super) state_occurrences:
        &'shape HashMap<rumoca_core::SourceOccurrenceId, dae::StateId<'dae>>,
    pub(super) expression_events: &'shape ExpressionEventPlans,
    pub(super) sample_alias_schedules: &'shape HashMap<VarName, PeriodicClockSchedule>,
    pub(super) clock_transfer_plans: &'shape ClockTransferPlans,
    pub(super) clocks: &'shape LoweredClocks<'dae>,
}

impl<'shape, 'dae> FunctionRegistry<'shape, 'dae> {
    fn new(
        input: FunctionRegistryInput<'shape, 'dae>,
        shapes: &'shape FunctionShapeAnalysis,
        ids: &'shape HashMap<FunctionSpecializationKey, dae::FunctionId<'dae>>,
    ) -> Self {
        Self {
            flat: input.flat,
            shapes,
            ids,
            comprehension_plans: input.comprehension_plans,
            record_array_fields: input.record_array_fields,
            constants: input.constants,
            delay_plans: input.delay_plans,
            derivatives: input.derivatives,
            history_operators: input.history_operators,
            coordinate_instances: input.coordinate_instances,
            state_occurrences: input.state_occurrences,
            expression_events: input.expression_events,
            sample_alias_schedules: input.sample_alias_schedules,
            clock_transfer_plans: input.clock_transfer_plans,
            clocks: input.clocks,
        }
    }
}

fn construct_recursive_component<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    shapes: &FunctionShapeAnalysis,
    ids: &mut HashMap<FunctionSpecializationKey, dae::FunctionId<'dae>>,
    registry: FunctionRegistryInput<'_, 'dae>,
    plans: &HashMap<FunctionSpecializationKey, FunctionPlan>,
    specializations: &[usize],
) -> Result<(), dae::DaeConstructionError> {
    let mut signatures = specializations
        .iter()
        .map(|&specialization| {
            function_signature(construction, registry.flat, shapes, specialization)
        })
        .collect::<Result<Vec<_>, _>>()?
        .into_iter();
    let first = signatures
        .next()
        .ok_or(dae::DaeConstructionError::MissingProvenance {
            origin: dae::DaeProvenanceOrigin::Source,
            attempted_span: None,
        })?;
    construction.recursive_functions(first, signatures, |construction, reservations| {
        for (&specialization, reservation) in specializations.iter().zip(&reservations) {
            ids.insert(
                shapes.certificates()[specialization].key.clone(),
                reservation.function(),
            );
        }
        for (&specialization, reservation) in specializations.iter().zip(reservations) {
            define_function(
                construction,
                coordinates,
                FunctionRegistry::new(registry, shapes, ids),
                reservation,
                specialization,
                plans,
            )?;
        }
        Ok(())
    })?;
    Ok(())
}

fn function_signature<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    flat: &flat::Model,
    shapes: &FunctionShapeAnalysis,
    specialization: usize,
) -> Result<dae::FunctionSignature<'dae>, dae::DaeConstructionError> {
    let certificate = &shapes.certificates()[specialization];
    let function = &flat.functions[&certificate.key.function];
    let declaration = dae::DaeProvenance::source(function.span)?;
    let parameters = function
        .inputs
        .iter()
        .zip(&certificate.parameters)
        .map(|(parameter, shape)| {
            function_value_type(construction, flat, parameter, shape, &mut HashSet::new())
        })
        .collect::<Result<Vec<_>, _>>()?;
    let results = function
        .outputs
        .iter()
        .zip(&certificate.results)
        .map(|(result, shape)| {
            function_value_type(construction, flat, result, shape, &mut HashSet::new())
        })
        .collect::<Result<Vec<_>, _>>()?;
    Ok(
        dae::FunctionSignature::new(function.name.clone(), parameters, results, declaration)
            .with_inline(function.inline),
    )
}

pub(super) fn function_value_type<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    flat: &flat::Model,
    value: &rumoca_core::FunctionParam,
    dimensions: &ValueShape,
    active_records: &mut HashSet<rumoca_core::DefId>,
) -> Result<dae::ValueTypeId<'dae>, dae::DaeConstructionError> {
    let provenance = dae::DaeProvenance::source(value.span)?;
    if let Some(scalar) = effective_function_scalar_type(flat, value) {
        return construction.types(|types| {
            types.derived(
                dae::ValueType::array(scalar, dimensions.clone()),
                provenance,
            )
        });
    }
    let type_def_id = value
        .type_def_id
        .ok_or(dae::DaeConstructionError::InvalidExpressionForm { span: value.span })?;
    if !active_records.insert(type_def_id) {
        return Err(dae::DaeConstructionError::InvalidExpressionForm { span: value.span });
    }
    let constructor = rumoca_core::resolve_record_constructor(
        flat.functions.values(),
        &value.type_name,
        type_def_id,
    )
    .map_err(|_| dae::DaeConstructionError::InvalidExpressionForm { span: value.span })?;
    let mut fields = Vec::with_capacity(constructor.inputs.len());
    for field in &constructor.inputs {
        let shape = field
            .dimensions()
            .iter()
            .map(|extent| checked_source_array_extent(*extent, field.span))
            .collect::<Result<Vec<_>, _>>()?;
        let value_type = function_value_type(construction, flat, field, &shape, active_records)?;
        fields.push((VarName::new(&field.name), value_type));
    }
    active_records.remove(&type_def_id);
    let record_name = flat
        .record_types
        .get(&type_def_id)
        .map(|record| VarName::new(&record.name))
        .ok_or(dae::DaeConstructionError::InvalidExpressionForm { span: value.span })?;
    construction
        .types(|types| types.record_array(record_name, fields, dimensions.clone(), provenance))
}

fn define_function<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    global_coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: FunctionRegistry<'_, 'dae>,
    reservation: dae::FunctionReservation<'_, 'dae>,
    specialization: usize,
    plans: &HashMap<FunctionSpecializationKey, FunctionPlan>,
) -> Result<(), dae::DaeConstructionError> {
    let certificate = &functions.shapes.certificates()[specialization];
    let function = &functions.flat.functions[&certificate.key.function];
    let plan = exact_function_plan(plans, certificate, specialization, function.span)?;
    let mut coordinates =
        register_function_inputs(construction, global_coordinates, &reservation, function)?;
    let mut mutable_values = Vec::with_capacity(function.outputs.len() + function.locals.len());
    for (ordinal, output) in function.outputs.iter().enumerate() {
        let provenance = dae::DaeProvenance::source(output.span)?;
        let value = construction.functions(|functions| {
            functions.output(
                &reservation,
                VarName::new(&output.name),
                ordinal,
                provenance,
            )
        })?;
        coordinates.insert(VarName::new(&output.name), Coordinate::FunctionValue(value));
        mutable_values.push((value, output));
    }
    for local in &function.locals {
        let provenance = dae::DaeProvenance::source(local.span)?;
        let shape = exact_function_local_shape(certificate, local)?;
        let value_type = function_value_type(
            construction,
            functions.flat,
            local,
            shape,
            &mut HashSet::new(),
        )?;
        let value = construction.functions(|functions| {
            functions.local(
                &reservation,
                VarName::new(&local.name),
                value_type,
                provenance,
            )
        })?;
        coordinates.insert(VarName::new(&local.name), Coordinate::FunctionValue(value));
        mutable_values.push((value, local));
    }
    let function_values = exact_function_value_coordinates(&mutable_values)?;
    register_generated_boolean_values(construction, &reservation, plan, &mut coordinates)?;
    let record_staging =
        register_record_staging_fields(construction, &functions, &reservation, function, plan)?;
    if let FunctionPlan::External(external) = plan {
        return define_external_function(
            construction,
            &coordinates,
            &functions,
            &certificate.values,
            reservation,
            function,
            external,
        );
    }
    let provenance = dae::DaeProvenance::source(function.span)?;
    let mut body = construction.functions(|functions| functions.begin(reservation, provenance))?;
    for (value, declaration) in mutable_values {
        let Some(default) = &declaration.default else {
            continue;
        };
        let expression = lower_function_expression(
            construction,
            &coordinates,
            None,
            &functions,
            &certificate.values,
            &body,
            default,
        )?;
        let assignment = dae::DaeProvenance::source(declaration.span)?;
        construction
            .functions(|functions| functions.assign(&mut body, value, expression, assignment))?;
    }
    let mut plan_shapes = certificate.values.clone();
    for (name, _) in generated_boolean_values(plan) {
        plan_shapes.insert(name.clone(), Vec::new());
    }
    let record_staging_available = FunctionRecordStagingAvailability::default();
    let symbols = FunctionSymbols {
        coordinates: &coordinates,
        function_values: &function_values,
        record_staging: &record_staging,
        record_staging_available: &record_staging_available,
        functions: &functions,
        shapes: &plan_shapes,
    };
    body = lower_function_plan(construction, symbols, body, function, plan)?;
    construction.functions(|owner| owner.define(body, provenance))?;
    Ok(())
}

fn exact_function_plan<'plan>(
    plans: &'plan HashMap<FunctionSpecializationKey, FunctionPlan>,
    certificate: &FunctionShapeCertificate,
    specialization: usize,
    span: Span,
) -> Result<&'plan FunctionPlan, dae::DaeConstructionError> {
    if let Some(plan) = plans.get(&certificate.key) {
        return Ok(plan);
    }
    let index =
        u32::try_from(specialization).map_err(|_| dae::DaeConstructionError::CapacityExceeded {
            arena: "function specialization",
            attempted_index: specialization,
            span,
        })?;
    Err(dae::DaeConstructionError::IncompleteDefinition {
        kind: "function lowering plan",
        index,
        span,
    })
}

fn exact_function_local_shape<'shape>(
    certificate: &'shape FunctionShapeCertificate,
    local: &rumoca_core::FunctionParam,
) -> Result<&'shape ValueShape, dae::DaeConstructionError> {
    certificate
        .values
        .get(&VarName::new(&local.name))
        .ok_or(dae::DaeConstructionError::InvalidExpressionForm { span: local.span })
}

fn exact_function_value_coordinates<'dae>(
    values: &[(dae::FunctionValueId<'dae>, &rumoca_core::FunctionParam)],
) -> Result<HashMap<rumoca_core::DefId, dae::FunctionValueId<'dae>>, dae::DaeConstructionError> {
    let mut coordinates = HashMap::with_capacity(values.len());
    for (value, declaration) in values {
        let Some(identity) = declaration.def_id.filter(|identity| identity.index() != 0) else {
            continue;
        };
        if coordinates.insert(identity, *value).is_some() {
            return Err(dae::DaeConstructionError::DuplicateDefinition {
                kind: "function-value identity",
                index: identity.index(),
                span: declaration.span,
            });
        }
    }
    Ok(coordinates)
}

fn register_function_inputs<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    globals: &HashMap<VarName, Coordinate<'dae>>,
    reservation: &dae::FunctionReservation<'_, 'dae>,
    function: &rumoca_core::Function,
) -> Result<HashMap<VarName, Coordinate<'dae>>, dae::DaeConstructionError> {
    let mut coordinates = globals.clone();
    for (ordinal, parameter) in function.inputs.iter().enumerate() {
        let provenance = dae::DaeProvenance::source(parameter.span)?;
        let parameter_id = construction.functions(|owner| {
            owner.parameter(
                reservation,
                VarName::new(&parameter.name),
                ordinal,
                provenance,
            )
        })?;
        coordinates.insert(
            VarName::new(&parameter.name),
            Coordinate::FunctionParameter(parameter_id),
        );
    }
    Ok(coordinates)
}

fn register_generated_boolean_values<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    reservation: &dae::FunctionReservation<'_, 'dae>,
    plan: &FunctionPlan,
    coordinates: &mut HashMap<VarName, Coordinate<'dae>>,
) -> Result<(), dae::DaeConstructionError> {
    for (name, span) in generated_boolean_values(plan) {
        let provenance = dae::DaeProvenance::source(*span)?;
        let value_type = construction.types(|types| {
            types.derived(dae::ValueType::scalar(dae::ScalarType::Boolean), provenance)
        })?;
        let value = construction.functions(|functions| {
            functions.local(reservation, name.clone(), value_type, provenance)
        })?;
        coordinates.insert(name.clone(), Coordinate::FunctionValue(value));
    }
    Ok(())
}

fn generated_boolean_values(plan: &FunctionPlan) -> &[(VarName, Span)] {
    match plan {
        FunctionPlan::Statements {
            generated_booleans, ..
        } => generated_booleans,
        _ => &[],
    }
}

fn register_record_staging_fields<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    functions: &FunctionRegistry<'_, 'dae>,
    reservation: &dae::FunctionReservation<'_, 'dae>,
    function: &rumoca_core::Function,
    plan: &FunctionPlan,
) -> Result<FunctionRecordStagingValues<'dae>, dae::DaeConstructionError> {
    let mut staging = FunctionRecordStagingValues::default();
    for (target, target_def_id, field) in record_staging_fields(plan) {
        let declaration = function
            .outputs
            .iter()
            .chain(&function.locals)
            .find(|value| value.def_id == Some(target_def_id) && value.name == target.as_str())
            .ok_or(dae::DaeConstructionError::InvalidExpressionForm {
                span: function.span,
            })?;
        let type_def_id =
            declaration
                .type_def_id
                .ok_or(dae::DaeConstructionError::InvalidExpressionForm {
                    span: declaration.span,
                })?;
        let constructor = rumoca_core::resolve_record_constructor(
            functions.flat.functions.values(),
            &declaration.type_name,
            type_def_id,
        )
        .map_err(|_| dae::DaeConstructionError::InvalidExpressionForm {
            span: declaration.span,
        })?;
        let field_declaration = constructor
            .inputs
            .iter()
            .find(|candidate| {
                candidate.def_id == Some(field.def_id) && candidate.name == field.name.as_str()
            })
            .ok_or(dae::DaeConstructionError::InvalidExpressionForm {
                span: declaration.span,
            })?;
        let shape = field_declaration
            .dimensions()
            .iter()
            .map(|extent| checked_source_array_extent(*extent, field_declaration.span))
            .collect::<Result<Vec<_>, _>>()?;
        let value_type = function_value_type(
            construction,
            functions.flat,
            field_declaration,
            &shape,
            &mut HashSet::new(),
        )?;
        let staging_name = function_record_field_name(&target, &field.name);
        let provenance = dae::DaeProvenance::source(field_declaration.span)?;
        let value = construction.functions(|owner| {
            owner.local(reservation, staging_name.clone(), value_type, provenance)
        })?;
        staging.insert_local(
            FunctionRecordFieldIdentity {
                target: target_def_id,
                field: field.def_id,
            },
            value,
            field_declaration.span,
        )?;
    }
    Ok(staging)
}

fn record_staging_fields(
    plan: &FunctionPlan,
) -> Vec<(VarName, rumoca_core::DefId, ResolvedFunctionRecordField)> {
    let mut fields = Vec::new();
    match plan {
        FunctionPlan::Statements { statements, .. } => {
            collect_record_staging_fields(statements, &mut fields)
        }
        FunctionPlan::GuardedReturn { branches, tail, .. } => {
            for branch in branches {
                collect_record_staging_fields(branch, &mut fields);
            }
            collect_record_staging_fields(tail, &mut fields);
        }
        FunctionPlan::IntegerReduction { initial, .. } => {
            collect_record_staging_fields(initial, &mut fields)
        }
        FunctionPlan::External(_) => {}
    }
    fields.sort_by_key(|(_, target, field)| (target.index(), field.def_id.index()));
    fields.dedup_by_key(|(_, target, field)| (*target, field.def_id));
    fields
}

fn collect_record_staging_fields(
    sequence: &FunctionStatementSequence,
    fields: &mut Vec<(VarName, rumoca_core::DefId, ResolvedFunctionRecordField)>,
) {
    for product in sequence.products() {
        let plan = product.plan();
        match plan {
            FunctionStatementPlan::Assignment(assignment) => {
                append_resolved_record_staging_field(assignment, fields);
            }
            FunctionStatementPlan::MultiOutputCall { outputs } => {
                for assignment in outputs.iter().flatten() {
                    append_resolved_record_staging_field(assignment, fields);
                }
            }
            FunctionStatementPlan::RecordFieldAssembly(assembly) => {
                fields.push((
                    assembly.target.clone(),
                    assembly.target_def_id,
                    ResolvedFunctionRecordField {
                        name: assembly.field.name.clone(),
                        def_id: assembly.field.def_id,
                    },
                ));
            }
            FunctionStatementPlan::For { statements, .. }
            | FunctionStatementPlan::ProvenBranch { statements, .. } => {
                collect_record_staging_fields(statements, fields)
            }
            FunctionStatementPlan::If {
                branches, fallback, ..
            } => {
                for branch in branches {
                    collect_record_staging_fields(branch, fields);
                }
                if let Some(fallback) = fallback {
                    collect_record_staging_fields(fallback, fields);
                }
            }
            FunctionStatementPlan::ProvenAssertion
            | FunctionStatementPlan::RuntimeAssertion
            | FunctionStatementPlan::GeneratedBooleanAssignment { .. }
            | FunctionStatementPlan::RecordMultiOutputAssembly(_)
            | FunctionStatementPlan::ArrayAssembly(_)
            | FunctionStatementPlan::RecordAssembly(_) => {}
        }
    }
}

fn append_resolved_record_staging_field(
    assignment: &FunctionAssignmentPlan,
    fields: &mut Vec<(VarName, rumoca_core::DefId, ResolvedFunctionRecordField)>,
) {
    let Some((target, identity, field)) = assignment.resolved_record_field() else {
        return;
    };
    fields.push((
        target.clone(),
        identity.target,
        ResolvedFunctionRecordField {
            name: field.clone(),
            def_id: identity.field,
        },
    ));
}

fn lower_function_plan<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    body: dae::FunctionBody<'dae>,
    function: &rumoca_core::Function,
    plan: &FunctionPlan,
) -> Result<dae::FunctionBody<'dae>, dae::DaeConstructionError> {
    match plan {
        FunctionPlan::External(_) => Err(dae::DaeConstructionError::InvalidExpressionForm {
            span: function.span,
        }),
        FunctionPlan::Statements {
            statements,
            certified_output_seeds,
            ..
        } => {
            let body = lower_named_function_seeds(
                construction,
                symbols,
                body,
                certified_output_seeds,
                function.span,
            )?;
            let body = lower_function_sequence_seeds(
                construction,
                symbols,
                body,
                statements,
                function.span,
            )?;
            lower_function_statements(construction, symbols, body, statements)
        }
        FunctionPlan::GuardedReturn {
            conditions,
            branches,
            tail,
            targets,
            span,
        } => lower_guarded_function_return(
            construction,
            symbols,
            body,
            GuardedFunctionReturn {
                conditions,
                branches,
                tail,
                targets,
                span: *span,
            },
        ),
        FunctionPlan::IntegerReduction {
            initial,
            result,
            reduction,
        } => lower_integer_reduction(
            construction,
            symbols,
            body,
            function,
            initial,
            result,
            reduction,
        ),
    }
}
