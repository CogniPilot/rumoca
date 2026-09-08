//! Typed proof plans for whole-coordinate and whole-record receivers of
//! event-algorithm function calls.

use super::reference_identity::same_exact_reference;
use super::*;

pub(in crate::construction) struct ModelEventFunctionCallPlan {
    pub(in crate::construction) outputs: Vec<Option<ModelEventFunctionOutputPlan>>,
}

#[derive(Clone)]
pub(in crate::construction) enum ModelEventFunctionOutputPlan {
    Coordinate(VarName),
    Record(Vec<ModelEventRecordFieldPlan>),
}

#[derive(Clone)]
pub(in crate::construction) struct ModelEventRecordFieldPlan {
    pub(in crate::construction) target: VarName,
    pub(in crate::construction) projection: Box<[usize]>,
}

pub(super) struct ModelEventCallAnalysis<'flat, 'analysis> {
    flat: &'flat flat::Model,
    roles: &'analysis HashMap<VarName, PlannedRole>,
    shapes: &'analysis FunctionShapeAnalysis,
    records: ModelRecordIndex<'flat>,
}

impl<'flat, 'analysis> ModelEventCallAnalysis<'flat, 'analysis> {
    pub(super) fn new(
        flat: &'flat flat::Model,
        roles: &'analysis HashMap<VarName, PlannedRole>,
        shapes: &'analysis FunctionShapeAnalysis,
    ) -> Self {
        Self {
            flat,
            roles,
            shapes,
            records: ModelRecordIndex::new(flat),
        }
    }

    pub(super) fn analyze_call(
        &self,
        component: &rumoca_core::Reference,
        arguments: &[Expression],
        outputs: &[Option<rumoca_core::ComponentReference>],
        span: Span,
    ) -> Result<ModelEventFunctionCallPlan, ToDaeError> {
        analyze_event_function_call(self, component, arguments, outputs, span)
    }

    pub(super) fn analyze_assignment_call(
        &self,
        component: &rumoca_core::ComponentReference,
        value: &Expression,
        span: Span,
    ) -> Result<Option<ModelEventFunctionCallPlan>, ToDaeError> {
        let Expression::FunctionCall {
            name,
            args,
            is_constructor: false,
            call_kind: rumoca_core::FunctionCallKind::Invocation,
            ..
        } = value
        else {
            return Ok(None);
        };
        let target = rumoca_core::component_ref_to_base_reference(component)
            .var_name()
            .clone();
        if !self.flat.record_instances.contains_key(&target) {
            return Ok(None);
        }
        let output = Some(component.clone());
        self.analyze_call(name, args, std::slice::from_ref(&output), span)
            .map(Some)
    }
}

fn analyze_event_function_call(
    context: &ModelEventCallAnalysis<'_, '_>,
    component: &rumoca_core::Reference,
    arguments: &[Expression],
    outputs: &[Option<rumoca_core::ComponentReference>],
    span: Span,
) -> Result<ModelEventFunctionCallPlan, ToDaeError> {
    let call = context.shapes.call_certificate(
        component,
        arguments,
        context.shapes.model_values(),
        span,
    )?;
    let certificate = &call.specialization;
    if outputs.len() > certificate.results.len() {
        return Err(ToDaeError::unsupported_algorithm(
            "model",
            format!(
                "function-call assignment receives {} results from `{}`, which declares {}",
                outputs.len(),
                component.as_str(),
                certificate.results.len()
            ),
            span,
        ));
    }
    let function = &context.flat.functions[&certificate.key.function];
    if outputs.iter().flatten().count() > 1 && !function.body_is_pure() {
        return Err(ToDaeError::unsupported_algorithm(
            "model",
            format!(
                "MLS §11.2.1.1 evaluates `{}` once, but canonical DAE result projections may be evaluated independently; an impure function cannot preserve that contract",
                function.name
            ),
            span,
        ));
    }
    let mut output_plans = Vec::with_capacity(outputs.len());
    let mut results = function.outputs.iter();
    let mut shapes = certificate.results.iter();
    for output in outputs {
        let Some(result) = results.next() else {
            return Err(ToDaeError::unsupported_algorithm(
                "model",
                "function-call result certificate is shorter than its source receivers",
                span,
            ));
        };
        let Some(result_shape) = shapes.next() else {
            return Err(ToDaeError::unsupported_algorithm(
                "model",
                "function-call shape certificate is shorter than its source receivers",
                span,
            ));
        };
        match output {
            None => output_plans.push(None),
            Some(output) => {
                let shape = call
                    .prefix
                    .iter()
                    .copied()
                    .chain(result_shape.iter().copied())
                    .collect::<Vec<_>>();
                output_plans.push(Some(analyze_event_function_output(
                    context, output, result, &shape, span,
                )?));
            }
        }
    }
    Ok(ModelEventFunctionCallPlan {
        outputs: output_plans,
    })
}

fn analyze_event_function_output(
    context: &ModelEventCallAnalysis<'_, '_>,
    output: &rumoca_core::ComponentReference,
    result: &rumoca_core::FunctionParam,
    result_shape: &[u32],
    span: Span,
) -> Result<ModelEventFunctionOutputPlan, ToDaeError> {
    if output.parts().iter().any(|part| !part.subs.is_empty()) {
        return Err(ToDaeError::unsupported_algorithm(
            "model",
            "a function-call receiver must be one whole coordinate or record",
            output.span(),
        ));
    }
    let target = output.to_var_name();
    if let Some(variable) = context.flat.variables.get(&target) {
        validate_event_coordinate_result(
            context.flat,
            variable,
            result,
            result_shape,
            context.roles,
            span,
        )?;
        return Ok(ModelEventFunctionOutputPlan::Coordinate(target));
    }
    let record = context.flat.record_instances.get(&target).ok_or_else(|| {
        ToDaeError::unsupported_algorithm(
            "model",
            format!("function-call receiver `{target}` has no exact Flat coordinate or record"),
            output.span(),
        )
    })?;
    if !same_exact_reference(output, &record.component_ref) {
        return Err(ToDaeError::unsupported_algorithm(
            "model",
            format!("function-call receiver `{target}` disagrees with its resolved Flat identity"),
            output.span(),
        ));
    }
    let record_shape = concrete_shape(&record.dims, output.span())?;
    if result.type_class != Some(rumoca_core::ClassType::Record)
        || result.type_def_id != Some(record.type_def_id)
        || result_shape != record_shape
    {
        return Err(ToDaeError::unsupported_algorithm(
            "model",
            format!(
                "record receiver `{target}` does not match the function result's resolved type identity and shape"
            ),
            output.span(),
        ));
    }
    let fields = context.records.record_fields(record, output.span())?;
    for field in &fields {
        if !matches!(
            context.roles.get(&field.target),
            Some(PlannedRole::DiscreteReal | PlannedRole::DiscreteValue)
        ) {
            return Err(ToDaeError::unsupported_algorithm(
                "model",
                format!(
                    "record receiver leaf `{}` is not a discrete event coordinate",
                    field.target
                ),
                output.span(),
            ));
        }
    }
    Ok(ModelEventFunctionOutputPlan::Record(fields))
}

fn validate_event_coordinate_result(
    flat: &flat::Model,
    variable: &flat::Variable,
    result: &rumoca_core::FunctionParam,
    result_shape: &[u32],
    roles: &HashMap<VarName, PlannedRole>,
    span: Span,
) -> Result<(), ToDaeError> {
    let shape = concrete_shape(&variable.dims, variable.source_span)?;
    let is_discrete = matches!(
        roles.get(&variable.name),
        Some(PlannedRole::DiscreteReal | PlannedRole::DiscreteValue)
    );
    let same_type = effective_variable_scalar_type(flat, variable)
        == effective_function_scalar_type(flat, result);
    if is_discrete && same_type && shape == result_shape {
        return Ok(());
    }
    Err(ToDaeError::unsupported_algorithm(
        "model",
        format!(
            "function-call receiver `{}` is not an exact discrete result coordinate",
            variable.name
        ),
        span,
    ))
}

fn concrete_shape(dimensions: &[i64], span: Span) -> Result<Vec<u32>, ToDaeError> {
    dimensions
        .iter()
        .map(|extent| {
            u32::try_from(*extent).map_err(|_| {
                ToDaeError::unsupported_algorithm(
                    "model",
                    "function-call receiver has a non-concrete dimension",
                    span,
                )
            })
        })
        .collect()
}

struct ModelRecordIndex<'flat> {
    flat: &'flat flat::Model,
    children: HashMap<InstanceId, Vec<InstanceId>>,
    variables: HashMap<InstanceId, &'flat flat::Variable>,
    records: HashMap<InstanceId, &'flat flat::RecordInstance>,
}

impl<'flat> ModelRecordIndex<'flat> {
    fn new(flat: &'flat flat::Model) -> Self {
        let mut children: HashMap<InstanceId, Vec<InstanceId>> = HashMap::new();
        for (instance, relation) in &flat.instance_relations {
            if let Some(owner) = relation.owner {
                children.entry(owner).or_default().push(*instance);
            }
        }
        Self {
            flat,
            children,
            variables: flat
                .variables
                .values()
                .map(|variable| (variable.instance_id, variable))
                .collect(),
            records: flat
                .record_instances
                .values()
                .map(|record| (record.instance_id, record))
                .collect(),
        }
    }

    fn record_fields(
        &self,
        record: &flat::RecordInstance,
        span: Span,
    ) -> Result<Vec<ModelEventRecordFieldPlan>, ToDaeError> {
        if !record.dims.is_empty() {
            return Err(ToDaeError::unsupported_algorithm(
                "model",
                "a record-array call receiver requires a compact record-family owner",
                span,
            ));
        }
        let class = self.unique_class_child(record.instance_id, span)?;
        let mut fields = Vec::new();
        self.collect_record_fields(
            record,
            class,
            Vec::new(),
            span,
            &mut HashSet::new(),
            &mut fields,
        )?;
        Ok(fields)
    }

    fn collect_record_fields(
        &self,
        record: &flat::RecordInstance,
        class: InstanceId,
        projection: Vec<usize>,
        span: Span,
        active: &mut HashSet<rumoca_core::DefId>,
        fields: &mut Vec<ModelEventRecordFieldPlan>,
    ) -> Result<(), ToDaeError> {
        if !active.insert(record.type_def_id) {
            return Err(ToDaeError::unsupported_algorithm(
                "model",
                format!(
                    "record receiver `{}` has a recursive field layout",
                    record.type_name
                ),
                span,
            ));
        }
        let layout = self
            .flat
            .record_types
            .get(&record.type_def_id)
            .ok_or_else(|| {
                ToDaeError::unsupported_algorithm(
                    "model",
                    format!(
                        "record receiver `{}` has no retained Flat field layout",
                        record.type_name
                    ),
                    span,
                )
            })?;
        for (ordinal, field) in layout.fields.iter().enumerate() {
            let child = self.unique_declared_child(class, field.def_id, span)?;
            let mut child_projection = projection.clone();
            child_projection.push(ordinal);
            self.collect_record_field(child, field, child_projection, span, active, fields)?;
        }
        active.remove(&record.type_def_id);
        Ok(())
    }

    fn collect_record_field(
        &self,
        child: InstanceId,
        field: &flat::RecordField,
        projection: Vec<usize>,
        span: Span,
        active: &mut HashSet<rumoca_core::DefId>,
        fields: &mut Vec<ModelEventRecordFieldPlan>,
    ) -> Result<(), ToDaeError> {
        match self.flat.instance_relations[&child].kind {
            flat::InstanceKind::Materialized => {
                fields.push(self.materialized_field(child, field, projection, span)?);
                Ok(())
            }
            flat::InstanceKind::Aggregate => {
                self.collect_nested_record(child, field, projection, span, active, fields)
            }
            flat::InstanceKind::Class => Err(ToDaeError::unsupported_algorithm(
                "model",
                "a record field relation cannot be an unowned class occurrence",
                span,
            )),
        }
    }

    fn materialized_field(
        &self,
        child: InstanceId,
        field: &flat::RecordField,
        projection: Vec<usize>,
        span: Span,
    ) -> Result<ModelEventRecordFieldPlan, ToDaeError> {
        let variable = self.variables.get(&child).ok_or_else(|| {
            ToDaeError::unsupported_algorithm(
                "model",
                "a materialized record field has no exact Flat variable",
                span,
            )
        })?;
        if variable.dims != field.dims {
            return Err(ToDaeError::unsupported_algorithm(
                "model",
                format!(
                    "record field `{}` disagrees with its retained Flat dimensions",
                    field.name
                ),
                span,
            ));
        }
        Ok(ModelEventRecordFieldPlan {
            target: variable.name.clone(),
            projection: projection.into_boxed_slice(),
        })
    }

    fn collect_nested_record(
        &self,
        child: InstanceId,
        field: &flat::RecordField,
        projection: Vec<usize>,
        span: Span,
        active: &mut HashSet<rumoca_core::DefId>,
        fields: &mut Vec<ModelEventRecordFieldPlan>,
    ) -> Result<(), ToDaeError> {
        let nested = self.records.get(&child).ok_or_else(|| {
            ToDaeError::unsupported_algorithm(
                "model",
                "an aggregate record field has no exact Flat record instance",
                span,
            )
        })?;
        if !nested.dims.is_empty() || nested.dims != field.dims {
            return Err(ToDaeError::unsupported_algorithm(
                "model",
                "nested record-array receivers require a compact record-family owner",
                span,
            ));
        }
        let nested_class = self.unique_class_child(nested.instance_id, span)?;
        self.collect_record_fields(nested, nested_class, projection, span, active, fields)
    }

    fn unique_declared_child(
        &self,
        owner: InstanceId,
        declaration: rumoca_core::DefId,
        span: Span,
    ) -> Result<InstanceId, ToDaeError> {
        let matches = self
            .children
            .get(&owner)
            .into_iter()
            .flatten()
            .copied()
            .filter(|child| self.flat.instance_relations[child].declaration == Some(declaration))
            .collect::<Vec<_>>();
        let [child] = matches.as_slice() else {
            return Err(ToDaeError::unsupported_algorithm(
                "model",
                format!(
                    "record field declaration {} has {} concrete Flat occurrences",
                    declaration.index(),
                    matches.len()
                ),
                span,
            ));
        };
        Ok(*child)
    }

    fn unique_class_child(&self, owner: InstanceId, span: Span) -> Result<InstanceId, ToDaeError> {
        let matches = self
            .children
            .get(&owner)
            .into_iter()
            .flatten()
            .copied()
            .filter(|child| self.flat.instance_relations[child].kind == flat::InstanceKind::Class)
            .collect::<Vec<_>>();
        let [child] = matches.as_slice() else {
            return Err(ToDaeError::unsupported_algorithm(
                "model",
                format!(
                    "record component occurrence {} has {} exact Flat class bodies",
                    owner.index(),
                    matches.len()
                ),
                span,
            ));
        };
        Ok(*child)
    }
}
