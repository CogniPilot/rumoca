//! Typed proof plans for whole-coordinate and whole-record receivers of
//! event-algorithm function calls.

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

struct ModelEventCallContext<'flat> {
    flat: &'flat flat::Model,
    roles: &'flat HashMap<VarName, PlannedRole>,
    shapes: &'flat FunctionShapeAnalysis,
    records: ModelRecordIndex<'flat>,
}

pub(super) fn analyze_event_function_calls(
    flat: &flat::Model,
    statements: &[rumoca_core::Statement],
    roles: &HashMap<VarName, PlannedRole>,
    shapes: &FunctionShapeAnalysis,
    plans: &mut HashMap<Span, ModelEventFunctionCallPlan>,
) -> Result<(), ToDaeError> {
    let context = ModelEventCallContext {
        flat,
        roles,
        shapes,
        records: ModelRecordIndex::new(flat),
    };
    analyze_event_function_call_statements(&context, statements, plans)
}

fn analyze_event_function_call_statements(
    context: &ModelEventCallContext<'_>,
    statements: &[rumoca_core::Statement],
    plans: &mut HashMap<Span, ModelEventFunctionCallPlan>,
) -> Result<(), ToDaeError> {
    for statement in statements {
        match statement {
            rumoca_core::Statement::FunctionCall {
                comp,
                args,
                outputs,
                span,
            } => {
                let plan = analyze_event_function_call(context, comp, args, outputs, *span)?;
                if plans.insert(*span, plan).is_some() {
                    return Err(ToDaeError::unsupported_algorithm(
                        "model",
                        "event function calls require distinct source owners",
                        *span,
                    ));
                }
            }
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                ..
            } => {
                for block in cond_blocks {
                    analyze_event_function_call_statements(context, &block.stmts, plans)?;
                }
                if let Some(fallback) = else_block {
                    analyze_event_function_call_statements(context, fallback, plans)?;
                }
            }
            rumoca_core::Statement::When { blocks, .. } => {
                for block in blocks {
                    analyze_event_function_call_statements(context, &block.stmts, plans)?;
                }
            }
            rumoca_core::Statement::For { equations, .. } => {
                analyze_event_function_call_statements(context, equations, plans)?;
            }
            _ => {}
        }
    }
    Ok(())
}

fn analyze_event_function_call(
    context: &ModelEventCallContext<'_>,
    component: &rumoca_core::ComponentReference,
    arguments: &[Expression],
    outputs: &[Option<rumoca_core::ComponentReference>],
    span: Span,
) -> Result<ModelEventFunctionCallPlan, ToDaeError> {
    let function_reference = rumoca_core::Reference::from_component_reference(component.clone());
    let call = context.shapes.call_certificate(
        &function_reference,
        arguments,
        context.shapes.model_values(),
        span,
    )?;
    let certificate = context
        .shapes
        .certificate(&call.specialization)
        .expect("a call-shape certificate names one function certificate");
    if outputs.len() > certificate.results.len() {
        return Err(ToDaeError::unsupported_algorithm(
            "model",
            format!(
                "function-call assignment receives {} results from `{}`, which declares {}",
                outputs.len(),
                function_reference.as_str(),
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
    for (ordinal, output) in outputs.iter().enumerate() {
        let Some(output) = output else {
            output_plans.push(None);
            continue;
        };
        let result = &function.outputs[ordinal];
        let shape = call
            .prefix
            .iter()
            .copied()
            .chain(certificate.results[ordinal].iter().copied())
            .collect::<Vec<_>>();
        output_plans.push(Some(analyze_event_function_output(
            context, output, result, &shape, span,
        )?));
    }
    Ok(ModelEventFunctionCallPlan {
        outputs: output_plans,
    })
}

fn analyze_event_function_output(
    context: &ModelEventCallContext<'_>,
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
    if !same_unsubscripted_reference(output, &record.component_ref) {
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

fn same_unsubscripted_reference(
    left: &rumoca_core::ComponentReference,
    right: &rumoca_core::ComponentReference,
) -> bool {
    left.local() == right.local()
        && left.parts().len() == right.parts().len()
        && left.parts().iter().zip(right.parts()).all(|(left, right)| {
            left.subs.is_empty() && right.subs.is_empty() && left.def_id == right.def_id
        })
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
        let class = self.unique_child(
            record.instance_id,
            record.type_def_id,
            flat::InstanceKind::Class,
            span,
        )?;
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
        let nested_class = self.unique_child(
            nested.instance_id,
            nested.type_def_id,
            flat::InstanceKind::Class,
            span,
        )?;
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

    fn unique_child(
        &self,
        owner: InstanceId,
        declaration: rumoca_core::DefId,
        kind: flat::InstanceKind,
        span: Span,
    ) -> Result<InstanceId, ToDaeError> {
        let matches = self
            .children
            .get(&owner)
            .into_iter()
            .flatten()
            .copied()
            .filter(|child| {
                let relation = &self.flat.instance_relations[child];
                relation.declaration == Some(declaration) && relation.kind == kind
            })
            .collect::<Vec<_>>();
        let [child] = matches.as_slice() else {
            return Err(ToDaeError::unsupported_algorithm(
                "model",
                format!(
                    "record type declaration {} has {} exact Flat class occurrences",
                    declaration.index(),
                    matches.len()
                ),
                span,
            ));
        };
        Ok(*child)
    }
}
