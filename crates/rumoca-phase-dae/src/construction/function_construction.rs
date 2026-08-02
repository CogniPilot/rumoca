use super::function_shapes::FunctionCallShapeCertificate;
use super::*;

pub(super) struct FunctionRegistry<'shape, 'dae> {
    pub(super) flat: &'shape flat::Model,
    pub(super) shapes: &'shape FunctionShapeAnalysis,
    pub(super) ids: &'shape HashMap<FunctionSpecializationKey, dae::FunctionId<'dae>>,
    pub(super) comprehension_plans: &'shape HashMap<ComprehensionKey, ComprehensionPlan>,
    pub(super) record_array_fields: &'shape RecordArrayFieldPlans,
    pub(super) constants: &'shape EvalContext,
    pub(super) delay_plans: &'shape HashMap<Span, DelayPlan>,
    pub(super) coordinate_instances: &'shape HashMap<rumoca_core::InstanceId, Coordinate<'dae>>,
    /// MLS §8.5 event owners proven for the model equation expressions this
    /// registry lowers. Function bodies never occupy those spans, so the same
    /// registry serves both without leaking model events into functions.
    pub(super) expression_events: &'shape ExpressionEventPlans,
}

impl<'dae> FunctionRegistry<'_, 'dae> {
    pub(super) fn select(
        &self,
        name: &rumoca_core::Reference,
        arguments: &[Expression],
        values: &ShapeEnvironment,
        span: Span,
    ) -> dae::FunctionId<'dae> {
        self.select_with_key(name, arguments, values, span).1
    }

    pub(super) fn select_with_key(
        &self,
        name: &rumoca_core::Reference,
        arguments: &[Expression],
        values: &ShapeEnvironment,
        span: Span,
    ) -> (FunctionSpecializationKey, dae::FunctionId<'dae>) {
        let key = self
            .shapes
            .call_key(name, arguments, values, span)
            .expect("analysis supplies a concrete specialization for every accepted call");
        let id = self.ids[&key];
        (key, id)
    }

    pub(super) fn select_with_call_certificate(
        &self,
        name: &rumoca_core::Reference,
        arguments: &[Expression],
        values: &ShapeEnvironment,
        span: Span,
    ) -> (&FunctionCallShapeCertificate, dae::FunctionId<'dae>) {
        let call = self
            .shapes
            .call_certificate(name, arguments, values, span)
            .expect("analysis supplies a checked call-shape certificate");
        let id = self.ids[&call.specialization];
        (call, id)
    }

    pub(super) fn primitive_parameter_scalar(
        &self,
        key: &FunctionSpecializationKey,
        ordinal: usize,
    ) -> dae::ScalarType {
        let parameter = &self.flat.functions[&key.function].inputs[ordinal];
        effective_function_scalar_type(self.flat, parameter)
            .expect("record lowering leaves primitive function parameters")
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
    pub(super) comprehension_plans: &'shape HashMap<ComprehensionKey, ComprehensionPlan>,
    pub(super) record_array_fields: &'shape RecordArrayFieldPlans,
    pub(super) constants: &'shape EvalContext,
    pub(super) delay_plans: &'shape HashMap<Span, DelayPlan>,
    pub(super) coordinate_instances: &'shape HashMap<rumoca_core::InstanceId, Coordinate<'dae>>,
    pub(super) expression_events: &'shape ExpressionEventPlans,
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
            coordinate_instances: input.coordinate_instances,
            expression_events: input.expression_events,
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
        .expect("recursive components are constructor-proven nonempty");
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
    Ok(dae::FunctionSignature::new(
        function.name.clone(),
        parameters,
        results,
        declaration,
    ))
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
        .expect("function analysis requires resolved record type identity");
    assert!(
        active_records.insert(type_def_id),
        "function analysis rejects recursive value records"
    );
    let constructor = rumoca_core::resolve_record_constructor(
        flat.functions.values(),
        &value.type_name,
        type_def_id,
    )
    .expect("function analysis requires resolved record constructor metadata");
    let mut fields = Vec::with_capacity(constructor.inputs.len());
    for field in &constructor.inputs {
        let shape = field
            .dimensions()
            .iter()
            .map(|extent| u32::try_from(*extent).expect("function shape analysis proves extents"))
            .collect::<Vec<_>>();
        let value_type = function_value_type(construction, flat, field, &shape, active_records)?;
        fields.push((VarName::new(&field.name), value_type));
    }
    active_records.remove(&type_def_id);
    construction.types(|types| types.record(constructor.name.clone(), fields, provenance))
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
    let mut coordinates = global_coordinates.clone();
    for (ordinal, parameter) in function.inputs.iter().enumerate() {
        let provenance = dae::DaeProvenance::source(parameter.span)?;
        let parameter_id = construction.functions(|owner| {
            owner.parameter(
                &reservation,
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
        let shape = &certificate.values[&VarName::new(&local.name)];
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
    let plan = &plans[&certificate.key];
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
            &functions,
            &certificate.values,
            &body,
            default,
        )?;
        let assignment = dae::DaeProvenance::source(declaration.span)?;
        construction
            .functions(|functions| functions.assign(&mut body, value, expression, assignment))?;
    }
    let symbols = FunctionSymbols {
        coordinates: &coordinates,
        functions: &functions,
        shapes: &certificate.values,
    };
    body = lower_function_plan(construction, symbols, body, function, plan)?;
    construction.functions(|owner| owner.define(body, provenance))?;
    Ok(())
}

fn lower_function_plan<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    body: dae::FunctionBody<'dae>,
    function: &rumoca_core::Function,
    plan: &FunctionPlan,
) -> Result<dae::FunctionBody<'dae>, dae::DaeConstructionError> {
    match plan {
        FunctionPlan::External(_) => unreachable!("external bodies define through their interface"),
        FunctionPlan::Statements { statements } => {
            lower_function_statements(construction, symbols, body, &function.body, statements)
        }
        FunctionPlan::GuardedReturn {
            branches,
            tail,
            targets,
        } => lower_guarded_function_return(
            construction,
            symbols,
            body,
            function,
            branches,
            tail,
            targets,
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
