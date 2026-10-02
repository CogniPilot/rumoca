use super::function_shapes::FunctionCallShapeCertificate;
use super::*;

pub(super) struct FunctionRegistry<'shape, 'dae> {
    pub(super) flat: &'shape flat::Model,
    pub(super) shapes: &'shape FunctionShapeAnalysis,
    pub(super) ids: &'shape HashMap<FunctionSpecializationKey, dae::FunctionId<'dae>>,
    pub(super) comprehension_plans: &'shape ComprehensionPlans,
    pub(super) record_array_fields: &'shape RecordArrayFieldPlans,
    pub(super) constants: &'shape EvalContext,
    pub(super) delay_plans: &'shape HashMap<Span, DelayPlan>,
    pub(super) history_operators: &'shape HistoryOperatorPlans,
    pub(super) coordinate_instances: &'shape HashMap<rumoca_core::InstanceId, Coordinate<'dae>>,
    /// MLS §8.5 event owners proven for the model equation expressions this
    /// registry lowers. Function bodies never occupy those spans, so the same
    /// registry serves both without leaking model events into functions.
    pub(super) expression_events: &'shape ExpressionEventPlans,
    pub(super) sample_alias_schedules: &'shape HashMap<VarName, PeriodicClockSchedule>,
    pub(super) clocked_coordinate_owners: &'shape HashMap<InstanceId, ClockPlan>,
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
        let id = self.ids[&key];
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
        let id = self.ids[&call.specialization];
        Ok((call, id))
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
    construct_derivatives(construction, shapes, &ids)?;
    Ok(ids)
}

fn construct_derivatives<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    shapes: &FunctionShapeAnalysis,
    functions: &HashMap<FunctionSpecializationKey, dae::FunctionId<'dae>>,
) -> Result<(), dae::DaeConstructionError> {
    let mut links = Vec::new();
    for derivative in shapes.derivatives() {
        let source = functions[&shapes.certificates()[derivative.source].key];
        let target = functions[&shapes.certificates()[derivative.target].key];
        let at = dae::DaeProvenance::source(derivative.span)?;
        let link = construction.functions(|functions| match derivative.previous {
            Some(previous) => functions.next_derivative(
                source,
                links[previous],
                target,
                derivative.inputs.iter().copied(),
                derivative.priority,
                at,
            ),
            None => functions.first_derivative(
                source,
                target,
                derivative.inputs.iter().copied(),
                derivative.priority,
                at,
            ),
        })?;
        links.push(link);
    }
    Ok(())
}

#[derive(Clone, Copy)]
pub(super) struct FunctionRegistryInput<'shape, 'dae> {
    pub(super) flat: &'shape flat::Model,
    pub(super) comprehension_plans: &'shape ComprehensionPlans,
    pub(super) record_array_fields: &'shape RecordArrayFieldPlans,
    pub(super) constants: &'shape EvalContext,
    pub(super) delay_plans: &'shape HashMap<Span, DelayPlan>,
    pub(super) history_operators: &'shape HistoryOperatorPlans,
    pub(super) coordinate_instances: &'shape HashMap<rumoca_core::InstanceId, Coordinate<'dae>>,
    pub(super) expression_events: &'shape ExpressionEventPlans,
    pub(super) sample_alias_schedules: &'shape HashMap<VarName, PeriodicClockSchedule>,
    pub(super) clocked_coordinate_owners: &'shape HashMap<InstanceId, ClockPlan>,
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
            history_operators: input.history_operators,
            coordinate_instances: input.coordinate_instances,
            expression_events: input.expression_events,
            sample_alias_schedules: input.sample_alias_schedules,
            clocked_coordinate_owners: input.clocked_coordinate_owners,
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
    if let Some(type_def_id) = value.type_def_id
        && super::native_tables::native_table_family(flat, type_def_id).is_some()
    {
        // MLS §12.9.7: an opaque native table handle is modeled as an integer
        // table id (never a Real, so derivative specialization seeds no tangent
        // for it, matching the derivative classification in function_shapes).
        return construction.types(|types| {
            types.derived(
                dae::ValueType::array(dae::ScalarType::Integer, dimensions.clone()),
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
    let record_name = flat
        .record_types
        .get(&type_def_id)
        .map(|record| VarName::new(&record.name))
        .expect("Flat construction retains every reachable constructor layout");
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
    let plan = &plans[&certificate.key];
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
    register_generated_boolean_values(construction, &reservation, plan, &mut coordinates)?;
    register_record_staging_fields(
        construction,
        &functions,
        &reservation,
        function,
        plan,
        &mut coordinates,
    )?;
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
    assign_declaration_defaults(
        construction,
        (&coordinates, &functions, &certificate.values),
        &mut body,
        mutable_values,
    )?;
    if let FunctionPlan::NativeLinearSolve {
        matrix,
        solution,
        info,
    } = plan
    {
        lower_native_linear_solve(
            construction,
            &coordinates,
            &mut body,
            (matrix, solution, info),
            provenance,
        )?;
        construction.functions(|owner| owner.define(body, provenance))?;
        return Ok(());
    }
    let mut plan_shapes = certificate.values.clone();
    for (name, _) in generated_boolean_values(plan) {
        plan_shapes.insert(name.clone(), Vec::new());
    }
    let symbols = FunctionSymbols {
        coordinates: &coordinates,
        functions: &functions,
        shapes: &plan_shapes,
    };
    body = lower_function_plan(construction, symbols, body, function, plan)?;
    construction.functions(|owner| owner.define(body, provenance))?;
    Ok(())
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
    coordinates: &mut HashMap<VarName, Coordinate<'dae>>,
) -> Result<(), dae::DaeConstructionError> {
    for (target, field) in record_staging_fields(plan) {
        let declaration = function
            .outputs
            .iter()
            .chain(&function.locals)
            .find(|value| value.name == target.as_str())
            .expect("record staging target resolves its declaration");
        let constructor = rumoca_core::resolve_record_constructor(
            functions.flat.functions.values(),
            &declaration.type_name,
            declaration
                .type_def_id
                .expect("record staging target has exact type identity"),
        )
        .expect("record staging target has a constructor layout");
        let field_declaration = constructor
            .inputs
            .iter()
            .find(|candidate| candidate.name == field.as_str())
            .expect("record staging field belongs to the constructor");
        let shape = field_declaration
            .dimensions()
            .iter()
            .map(|extent| u32::try_from(*extent).expect("analysis proves field extents"))
            .collect::<Vec<_>>();
        let value_type = function_value_type(
            construction,
            functions.flat,
            field_declaration,
            &shape,
            &mut HashSet::new(),
        )?;
        let staging_name = function_record_field_name(&target, &field);
        let provenance = dae::DaeProvenance::source(field_declaration.span)?;
        let value = construction.functions(|owner| {
            owner.local(reservation, staging_name.clone(), value_type, provenance)
        })?;
        coordinates.insert(staging_name, Coordinate::FunctionValue(value));
    }
    Ok(())
}

fn record_staging_fields(plan: &FunctionPlan) -> Vec<(VarName, VarName)> {
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
        FunctionPlan::External(_) | FunctionPlan::NativeLinearSolve { .. } => {}
    }
    fields.sort();
    fields.dedup();
    fields
}

fn collect_record_staging_fields(
    plans: &[FunctionStatementPlan],
    fields: &mut Vec<(VarName, VarName)>,
) {
    for plan in plans {
        match plan {
            FunctionStatementPlan::RecordFieldAssembly(assembly) => {
                fields.push((assembly.target.clone(), assembly.field.name.clone()));
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
            _ => {}
        }
    }
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
        FunctionPlan::NativeLinearSolve { .. } => {
            unreachable!("a native linear solve defines its body directly")
        }
        FunctionPlan::Statements {
            source,
            statements,
            entry_seeds,
            definedness,
            ..
        } => {
            let body = lower_named_function_seeds(
                construction,
                symbols,
                body,
                entry_seeds,
                function.span,
            )?;
            let body = lower_function_sequence_seeds(
                construction,
                symbols,
                body,
                statements,
                function.span,
            )?;
            let mut top_level = TopLevelDefinedness {
                plan: definedness,
                predicates: DefinednessPredicates::default(),
                span: function.span,
            };
            let mut body = lower_function_statements(
                construction,
                symbols,
                body,
                source,
                statements,
                Some(&mut top_level),
            )?;
            top_level.predicates.assert_defined(
                construction,
                &mut body,
                &definedness.returned,
                function.span,
            )?;
            Ok(body)
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

/// Define the body of a LAPACK `dgesv` call with one right-hand side (see
/// `analysis::function_native_lapack`). `info` is the dgesv status: the
/// first step `k` at which elimination with partial pivoting meets an
/// exactly zero pivot column, or 0. When `info = 0` the solution output,
/// initialized to the right-hand side by its declaration equation, becomes
/// the linear solve of the matrix argument; otherwise it keeps the
/// right-hand side, as dgesv leaves `B` unchanged, and the caller decides
/// what a singular matrix means.
fn lower_native_linear_solve<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    body: &mut dae::FunctionBody<'dae>,
    (matrix, solution, info): (&VarName, &VarName, &VarName),
    provenance: dae::DaeProvenance,
) -> Result<(), dae::DaeConstructionError> {
    let read = |construction: &mut dae::DaeConstruction<'dae>,
                body: &dae::FunctionBody<'dae>,
                name: &VarName| {
        match coordinates[name] {
            Coordinate::FunctionValue(value) => {
                construction.functions(|functions| functions.read(body, value, provenance))
            }
            Coordinate::FunctionParameter(parameter) => construction.expressions(|expressions| {
                expressions.at(provenance).function_parameter(parameter)
            }),
            _ => unreachable!("analysis proves the linear-solve operands are function values"),
        }
    };
    let matrix = read(construction, body, matrix)?;
    let rhs = read(construction, body, solution)?;
    let status = construction.expressions(|expressions| {
        let mut builder = PivotBuilder {
            expressions,
            provenance,
        };
        let status = builder.zero_pivot_step(matrix)?;
        let solvable = builder.is_integer(status, 0)?;
        let identity = builder.identity(builder.extent(matrix)?)?;
        let regular = builder.conditional(solvable, matrix, identity)?;
        let solved = builder
            .expressions
            .at(provenance)
            .builtin(dae::PureBuiltin::LinearSolve, [regular, rhs])?;
        Ok::<_, dae::DaeConstructionError>((status, solved))
    });
    let (status, solved) = status?;
    let solution = function_value_coordinate(coordinates, solution);
    construction.functions(|functions| functions.assign(body, solution, solved, provenance))?;
    let info = function_value_coordinate(coordinates, info);
    construction.functions(|functions| functions.assign(body, info, status, provenance))
}

/// Straight-line elimination with partial pivoting over a matrix of
/// translation-time extent `n`, kept as `n` row vectors: step `k` takes the
/// first row of largest magnitude in column `k` (LAPACK `idamax`), swaps it
/// into row `k`, and eliminates column `k` below it. A zero largest
/// magnitude is a zero pivot: the step eliminates nothing (its multipliers
/// are zero) and the first such step is the dgesv `info`.
struct PivotBuilder<'scope, 'storage, 'dae> {
    expressions: &'scope mut dae::Expressions<'storage, 'dae>,
    provenance: dae::DaeProvenance,
}

impl<'dae> PivotBuilder<'_, '_, 'dae> {
    fn extent(&self, matrix: dae::ExprId<'dae>) -> Result<usize, dae::DaeConstructionError> {
        let ty = self.expressions.value_type(matrix, self.provenance)?;
        Ok(ty.dimensions()[0] as usize)
    }

    fn literal(
        &mut self,
        value: dae::DaeLiteral,
    ) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
        self.expressions.at(self.provenance).literal(value)
    }

    fn integer(&mut self, value: usize) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
        self.literal(dae::DaeLiteral::Integer(value as i64))
    }

    fn is_integer(
        &mut self,
        lhs: dae::ExprId<'dae>,
        value: i64,
    ) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
        let rhs = self.literal(dae::DaeLiteral::Integer(value))?;
        self.op(dae::BinaryOperator::Equal, lhs, rhs)
    }

    fn op(
        &mut self,
        operator: dae::BinaryOperator,
        lhs: dae::ExprId<'dae>,
        rhs: dae::ExprId<'dae>,
    ) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
        self.expressions
            .at(self.provenance)
            .binary(operator, lhs, rhs)
    }

    fn conditional(
        &mut self,
        condition: dae::ExprId<'dae>,
        then: dae::ExprId<'dae>,
        otherwise: dae::ExprId<'dae>,
    ) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
        self.expressions
            .at(self.provenance)
            .conditional([(condition, then)], otherwise)
    }

    /// `base[index]` for a translation-time 1-based `index`, with the
    /// remaining axes whole.
    fn at_index(
        &mut self,
        base: dae::ExprId<'dae>,
        index: usize,
    ) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
        let expression = self.integer(index)?;
        let provenance = self.provenance;
        let rank = self
            .expressions
            .value_type(base, provenance)?
            .dimensions()
            .len();
        let subscripts = std::iter::once(dae::Subscript::Value {
            expression,
            provenance,
        })
        .chain((1..rank).map(|_| dae::Subscript::Whole { provenance }));
        self.expressions.at(provenance).index(base, subscripts)
    }

    fn identity(&mut self, n: usize) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
        let rows = (1..=n)
            .map(|row| {
                let elements = (1..=n)
                    .map(|column| {
                        self.literal(dae::DaeLiteral::Real(f64::from(u8::from(row == column))))
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                self.expressions.at(self.provenance).array(elements)
            })
            .collect::<Result<Vec<_>, _>>()?;
        self.expressions.at(self.provenance).array(rows)
    }

    /// The first elimination step whose pivot column is exactly zero, or 0.
    fn zero_pivot_step(
        &mut self,
        matrix: dae::ExprId<'dae>,
    ) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
        let n = self.extent(matrix)?;
        let mut rows = (1..=n)
            .map(|row| self.at_index(matrix, row))
            .collect::<Result<Vec<_>, _>>()?;
        let mut status = self.integer(0)?;
        let zero = self.literal(dae::DaeLiteral::Real(0.0))?;
        for step in 0..n {
            let (magnitudes, largest) = self.column_magnitudes(&rows[step..], step + 1)?;
            let is_zero = self.op(dae::BinaryOperator::Equal, largest, zero)?;
            let unset = self.is_integer(status, 0)?;
            let first_zero = self.op(dae::BinaryOperator::And, unset, is_zero)?;
            let step_number = self.integer(step + 1)?;
            status = self.conditional(first_zero, step_number, status)?;
            if step + 1 < n {
                let taken = self.first_largest(&magnitudes, largest)?;
                self.eliminate(&mut rows[step..], step + 1, &taken, is_zero)?;
            }
        }
        Ok(status)
    }

    /// The magnitudes of column `column` over `rows` and their largest.
    fn column_magnitudes(
        &mut self,
        rows: &[dae::ExprId<'dae>],
        column: usize,
    ) -> Result<(Vec<dae::ExprId<'dae>>, dae::ExprId<'dae>), dae::DaeConstructionError> {
        let magnitudes = rows
            .iter()
            .map(|row| {
                let element = self.at_index(*row, column)?;
                self.expressions
                    .at(self.provenance)
                    .builtin(dae::PureBuiltin::Abs, [element])
            })
            .collect::<Result<Vec<_>, _>>()?;
        let mut largest = magnitudes[0];
        for magnitude in &magnitudes[1..] {
            largest = self
                .expressions
                .at(self.provenance)
                .builtin(dae::PureBuiltin::Max, [largest, *magnitude])?;
        }
        Ok((magnitudes, largest))
    }

    /// Whether each row is the first of largest magnitude (LAPACK `idamax`).
    fn first_largest(
        &mut self,
        magnitudes: &[dae::ExprId<'dae>],
        largest: dae::ExprId<'dae>,
    ) -> Result<Vec<dae::ExprId<'dae>>, dae::DaeConstructionError> {
        let mut taken = Vec::with_capacity(magnitudes.len());
        let mut earlier: Option<dae::ExprId<'dae>> = None;
        for magnitude in magnitudes {
            let select = self.op(dae::BinaryOperator::Equal, *magnitude, largest)?;
            let (first, seen) = match earlier {
                None => (select, select),
                Some(earlier) => {
                    let not_earlier = self
                        .expressions
                        .at(self.provenance)
                        .unary(dae::UnaryOperator::Not, earlier)?;
                    let first = self.op(dae::BinaryOperator::And, select, not_earlier)?;
                    (first, self.op(dae::BinaryOperator::Or, earlier, select)?)
                }
            };
            earlier = Some(seen);
            taken.push(first);
        }
        Ok(taken)
    }

    /// Swap the taken row into `rows[0]` and eliminate column `column` from
    /// the rows below it. A zero pivot column eliminates nothing: its
    /// leading entries are zero, so dividing them by 1 gives zero multipliers.
    fn eliminate(
        &mut self,
        rows: &mut [dae::ExprId<'dae>],
        column: usize,
        taken: &[dae::ExprId<'dae>],
        is_zero: dae::ExprId<'dae>,
    ) -> Result<(), dae::DaeConstructionError> {
        let pivot_row = self.expressions.at(self.provenance).conditional(
            taken[1..].iter().copied().zip(rows[1..].iter().copied()),
            rows[0],
        )?;
        let pivot = self.at_index(pivot_row, column)?;
        let one = self.literal(dae::DaeLiteral::Real(1.0))?;
        let denominator = self.conditional(is_zero, one, pivot)?;
        for offset in 1..rows.len() {
            // The old pivot row moves to the taken row's place.
            let swapped = self.conditional(taken[offset], rows[0], rows[offset])?;
            let leading = self.at_index(swapped, column)?;
            let factor = self.op(dae::BinaryOperator::Divide, leading, denominator)?;
            let scaled = self.op(dae::BinaryOperator::Multiply, factor, pivot_row)?;
            rows[offset] = self.op(dae::BinaryOperator::Subtract, swapped, scaled)?;
        }
        rows[0] = pivot_row;
        Ok(())
    }
}

/// MLS §12.4.4: the declaration equations of outputs and protected locals
/// define their values before the algorithm runs.
fn assign_declaration_defaults<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    (coordinates, functions, shapes): (
        &HashMap<VarName, Coordinate<'dae>>,
        &FunctionRegistry<'_, 'dae>,
        &ShapeEnvironment,
    ),
    body: &mut dae::FunctionBody<'dae>,
    mutable_values: Vec<(dae::FunctionValueId<'dae>, &rumoca_core::FunctionParam)>,
) -> Result<(), dae::DaeConstructionError> {
    for (value, declaration) in mutable_values {
        let Some(default) = &declaration.default else {
            continue;
        };
        let expression =
            lower_function_expression(construction, coordinates, functions, shapes, body, default)?;
        let assignment = dae::DaeProvenance::source(declaration.span)?;
        construction
            .functions(|functions| functions.assign(body, value, expression, assignment))?;
    }
    Ok(())
}
