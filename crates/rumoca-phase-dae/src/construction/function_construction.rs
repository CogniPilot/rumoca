use super::*;

pub(super) struct ReservedFunction<'flat, 'dae> {
    flat: &'flat rumoca_core::Function,
    specialization: usize,
    reservation: dae::FunctionReservation<'dae>,
}

pub(super) struct FunctionRegistry<'shape, 'dae> {
    pub(super) flat: &'shape flat::Model,
    pub(super) shapes: &'shape FunctionShapeAnalysis,
    pub(super) ids: HashMap<FunctionSpecializationKey, dae::FunctionId<'dae>>,
    pub(super) comprehension_plans: &'shape HashMap<ComprehensionKey, ComprehensionPlan>,
    pub(super) record_array_fields: &'shape HashMap<Span, RecordArrayFieldPlan>,
    pub(super) constants: &'shape EvalContext,
    pub(super) reinit_state_pre: &'shape HashSet<Span>,
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

    pub(super) fn primitive_parameter_scalar(
        &self,
        key: &FunctionSpecializationKey,
        ordinal: usize,
    ) -> dae::ScalarType {
        let parameter = &self.flat.functions[&key.function].inputs[ordinal];
        primitive_scalar_type(&parameter.type_name)
            .expect("record lowering leaves primitive function parameters")
    }
}

pub(super) fn reserve_functions<'flat, 'dae>(
    flat: &'flat flat::Model,
    shapes: &FunctionShapeAnalysis,
    construction: &mut dae::DaeConstruction<'dae>,
) -> Result<
    (
        HashMap<FunctionSpecializationKey, dae::FunctionId<'dae>>,
        Vec<ReservedFunction<'flat, 'dae>>,
    ),
    dae::DaeConstructionError,
> {
    let mut functions = HashMap::with_capacity(shapes.certificates().len());
    let mut reserved = Vec::with_capacity(shapes.certificates().len());
    for (specialization, certificate) in shapes.certificates().iter().enumerate() {
        debug_assert!(
            !certificate.first_call.is_dummy(),
            "shape certificates retain their first call-site equality"
        );
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
        let (id, reservation) = construction.functions(|functions| {
            functions.reserve_recursive(function.name.clone(), parameters, results, declaration)
        })?;
        functions.insert(certificate.key.clone(), id);
        reserved.push(ReservedFunction {
            flat: function,
            specialization,
            reservation,
        });
    }
    Ok((functions, reserved))
}

fn function_value_type<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    flat: &flat::Model,
    value: &rumoca_core::FunctionParam,
    dimensions: &ValueShape,
    active_records: &mut HashSet<rumoca_core::DefId>,
) -> Result<dae::ValueTypeId<'dae>, dae::DaeConstructionError> {
    let provenance = dae::DaeProvenance::source(value.span)?;
    if let Some(scalar) = primitive_scalar_type(&value.type_name) {
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
            .dims
            .iter()
            .map(|extent| u32::try_from(*extent).expect("function shape analysis proves extents"))
            .collect::<Vec<_>>();
        let value_type = function_value_type(construction, flat, field, &shape, active_records)?;
        fields.push((VarName::new(&field.name), value_type));
    }
    active_records.remove(&type_def_id);
    construction.types(|types| types.record(constructor.name.clone(), fields, provenance))
}

pub(super) fn define_functions<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    flat: &flat::Model,
    global_coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    reserved: Vec<ReservedFunction<'_, 'dae>>,
    plans: &HashMap<FunctionSpecializationKey, FunctionPlan>,
) -> Result<(), dae::DaeConstructionError> {
    for reserved in reserved {
        let certificate = &functions.shapes.certificates()[reserved.specialization];
        let mut coordinates = global_coordinates.clone();
        for (ordinal, parameter) in reserved.flat.inputs.iter().enumerate() {
            let provenance = dae::DaeProvenance::source(parameter.span)?;
            let parameter_id = construction.functions(|owner| {
                owner.parameter(
                    &reserved.reservation,
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
        let mut mutable_values =
            Vec::with_capacity(reserved.flat.outputs.len() + reserved.flat.locals.len());
        for (ordinal, output) in reserved.flat.outputs.iter().enumerate() {
            let provenance = dae::DaeProvenance::source(output.span)?;
            let value = construction.functions(|functions| {
                functions.output(
                    &reserved.reservation,
                    VarName::new(&output.name),
                    ordinal,
                    provenance,
                )
            })?;
            coordinates.insert(VarName::new(&output.name), Coordinate::FunctionValue(value));
            mutable_values.push((value, output));
        }
        for local in &reserved.flat.locals {
            let provenance = dae::DaeProvenance::source(local.span)?;
            let shape = &certificate.values[&VarName::new(&local.name)];
            let value_type =
                function_value_type(construction, flat, local, shape, &mut HashSet::new())?;
            let value = construction.functions(|functions| {
                functions.local(
                    &reserved.reservation,
                    VarName::new(&local.name),
                    value_type,
                    provenance,
                )
            })?;
            coordinates.insert(VarName::new(&local.name), Coordinate::FunctionValue(value));
            mutable_values.push((value, local));
        }
        let provenance = dae::DaeProvenance::source(reserved.flat.span)?;
        let mut body = construction
            .functions(|functions| functions.begin(reserved.reservation, provenance))?;
        for (value, declaration) in mutable_values {
            let Some(default) = &declaration.default else {
                continue;
            };
            let expression = lower_function_expression(
                construction,
                &coordinates,
                functions,
                &certificate.values,
                &body,
                default,
            )?;
            let assignment = dae::DaeProvenance::source(declaration.span)?;
            construction.functions(|functions| {
                functions.assign(&mut body, value, expression, assignment)
            })?;
        }
        let plan = &plans[&certificate.key];
        let symbols = FunctionSymbols {
            coordinates: &coordinates,
            functions,
            shapes: &certificate.values,
        };
        lower_function_plan(construction, symbols, &mut body, reserved.flat, plan)?;
        construction.functions(|functions| functions.define(body, provenance))?;
    }
    Ok(())
}

fn lower_function_plan<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    body: &mut dae::FunctionBody<'dae>,
    function: &rumoca_core::Function,
    plan: &FunctionPlan,
) -> Result<(), dae::DaeConstructionError> {
    match plan {
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
