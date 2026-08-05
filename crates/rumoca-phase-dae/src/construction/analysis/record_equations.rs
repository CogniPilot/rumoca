use super::*;
use rumoca_core::DefId;

pub(super) fn analyze_record_equations(
    flat: &flat::Model,
    equations: &[flat::Equation],
) -> Result<HashMap<usize, RecordEquationPlan>, ToDaeError> {
    let mut plans = HashMap::new();
    for (row, equation) in equations.iter().enumerate() {
        if let Some(plan) = analyze_record_equation(flat, equation)? {
            plans.insert(row, plan);
        }
    }
    Ok(plans)
}

fn analyze_record_equation(
    flat: &flat::Model,
    equation: &flat::Equation,
) -> Result<Option<RecordEquationPlan>, ToDaeError> {
    let Some((target, target_name, value)) = record_equation(flat, equation) else {
        return Ok(None);
    };
    if !target.dims.is_empty() {
        return Err(ToDaeError::unsupported_flat(
            "record equation",
            "arrays of records require a compact record-family owner",
            equation.span,
        ));
    }
    let target_leaves = record_leaves(flat, target, target_name, equation.span)?;
    let fields = match value {
        RecordEquationValue::Aggregate(call) => {
            validate_constructor_layout(flat, target, call, equation.span)?;
            target_leaves
                .into_iter()
                .map(|leaf| RecordEquationFieldPlan {
                    target: leaf.coordinate,
                    value: RecordEquationFieldValue::AggregateProjection(leaf.projection),
                })
                .collect()
        }
        RecordEquationValue::Record(source, source_name) => {
            if !source.dims.is_empty() || source.type_def_id != target.type_def_id {
                return Err(ToDaeError::unsupported_flat(
                    "record equation",
                    "record equality operands have distinct resolved type identities or shapes",
                    equation.span,
                ));
            }
            let source_leaves = record_leaves(flat, source, source_name, equation.span)?;
            if target_leaves.len() != source_leaves.len()
                || target_leaves
                    .iter()
                    .zip(&source_leaves)
                    .any(|(target, source)| {
                        target.projection != source.projection
                            || flat.variables[&target.coordinate].dims
                                != flat.variables[&source.coordinate].dims
                    })
            {
                return Err(ToDaeError::unsupported_flat(
                    "record equation",
                    "record equality operands have distinct resolved field layouts",
                    equation.span,
                ));
            }
            target_leaves
                .into_iter()
                .zip(source_leaves)
                .map(|(target, source)| RecordEquationFieldPlan {
                    target: target.coordinate,
                    value: RecordEquationFieldValue::Coordinate(source.coordinate),
                })
                .collect()
        }
    };
    Ok(Some(RecordEquationPlan { fields }))
}

fn validate_constructor_layout(
    flat: &flat::Model,
    record: &flat::RecordInstance,
    call: RecordCall<'_>,
    equation_span: Span,
) -> Result<(), ToDaeError> {
    let record_type = flat.record_types.get(&record.type_def_id).ok_or_else(|| {
        ToDaeError::unsupported_flat(
            "record equation",
            format!("`{}` has no resolved Flat field layout", record.type_name),
            record.source_span,
        )
    })?;
    let constructor = equation_constructor(flat, record, call, equation_span)?;
    let same_layout = constructor.inputs.len() == record_type.fields.len()
        && constructor
            .inputs
            .iter()
            .zip(&record_type.fields)
            .all(|(input, field)| {
                input.def_id == Some(field.def_id)
                    && input.name == field.name
                    && input.dimensions() == field.dims
            });
    if !same_layout {
        return Err(ToDaeError::unsupported_flat(
            "record equation",
            "Flat record field and constructor layouts disagree",
            record.source_span,
        ));
    }
    Ok(())
}

struct RecordLeaf {
    coordinate: VarName,
    projection: Box<[usize]>,
}

fn record_leaves(
    flat: &flat::Model,
    record: &flat::RecordInstance,
    name: &rumoca_core::Reference,
    span: Span,
) -> Result<Vec<RecordLeaf>, ToDaeError> {
    let layout = flat.record_types.get(&record.type_def_id).ok_or_else(|| {
        ToDaeError::unsupported_flat(
            "record equation",
            format!("`{}` has no resolved Flat field layout", record.type_name),
            record.source_span,
        )
    })?;
    let mut fields = Vec::new();
    for (ordinal, field) in layout.fields.iter().enumerate() {
        let child = VarName::new(format!("{}.{}", name.as_str(), field.name));
        collect_record_equation_leaves(
            flat,
            &child,
            vec![ordinal],
            span,
            &mut HashSet::new(),
            &mut fields,
        )?;
    }
    Ok(fields)
}

fn collect_record_equation_leaves(
    flat: &flat::Model,
    name: &VarName,
    projection: Vec<usize>,
    span: Span,
    active: &mut HashSet<DefId>,
    fields: &mut Vec<RecordLeaf>,
) -> Result<(), ToDaeError> {
    if flat.variables.contains_key(name) {
        fields.push(RecordLeaf {
            coordinate: name.clone(),
            projection: projection.into_boxed_slice(),
        });
        return Ok(());
    }
    let instance = flat.record_instances.get(name).ok_or_else(|| {
        ToDaeError::unsupported_flat(
            "record equation",
            format!("`{name}` has neither a leaf coordinate nor a nested record layout"),
            span,
        )
    })?;
    if !instance.dims.is_empty() {
        return Err(ToDaeError::unsupported_flat(
            "record equation",
            format!("nested record array `{name}` requires a compact record-family owner"),
            span,
        ));
    }
    if !active.insert(instance.type_def_id) {
        return Err(ToDaeError::unsupported_flat(
            "record equation",
            format!("nested record layout `{name}` is recursive"),
            span,
        ));
    }
    let layout = flat
        .record_types
        .get(&instance.type_def_id)
        .ok_or_else(|| {
            ToDaeError::unsupported_flat(
                "record equation",
                format!("nested record `{name}` has no resolved Flat field layout"),
                span,
            )
        })?;
    for (ordinal, field) in layout.fields.iter().enumerate() {
        let child = VarName::new(format!("{name}.{}", field.name));
        let mut child_projection = projection.clone();
        child_projection.push(ordinal);
        collect_record_equation_leaves(flat, &child, child_projection, span, active, fields)?;
    }
    active.remove(&instance.type_def_id);
    Ok(())
}

fn equation_constructor<'flat>(
    flat: &'flat flat::Model,
    record: &flat::RecordInstance,
    call: RecordCall<'_>,
    equation_span: Span,
) -> Result<&'flat rumoca_core::Function, ToDaeError> {
    let function = flat
        .functions
        .get(call.name.var_name())
        .ok_or_else(|| ToDaeError::unresolved_reference(call.name.as_str(), call.span))?;
    if call.is_constructor {
        if function.is_constructor {
            return Ok(function);
        }
        return Err(ToDaeError::unsupported_flat(
            "record equation",
            format!("`{}` is not constructor metadata", function.name),
            call.span,
        ));
    }
    let [result] = function.outputs.as_slice() else {
        return Err(ToDaeError::unsupported_flat(
            "record equation",
            format!(
                "`{}` must have exactly one record result for whole-record equality",
                function.name
            ),
            call.span,
        ));
    };
    if result.type_class != Some(rumoca_core::ClassType::Record)
        || result.type_def_id != Some(record.type_def_id)
    {
        return Err(ToDaeError::unsupported_flat(
            "record equation",
            "record equality operands have distinct resolved type identities",
            equation_span,
        ));
    }
    rumoca_core::resolve_record_constructor(
        flat.functions.values(),
        &result.type_name,
        record.type_def_id,
    )
    .map_err(|error| {
        ToDaeError::unsupported_flat(
            "record equation",
            format!("`{}` has no constructor layout: {error}", record.type_name),
            record.source_span,
        )
    })
}

struct RecordCall<'scope> {
    name: &'scope rumoca_core::Reference,
    span: Span,
    is_constructor: bool,
}

enum RecordEquationValue<'scope> {
    Aggregate(RecordCall<'scope>),
    Record(&'scope flat::RecordInstance, &'scope rumoca_core::Reference),
}

fn record_equation<'scope>(
    flat: &'scope flat::Model,
    equation: &'scope flat::Equation,
) -> Option<(
    &'scope flat::RecordInstance,
    &'scope rumoca_core::Reference,
    RecordEquationValue<'scope>,
)> {
    let Expression::Binary {
        op: OpBinary::Sub,
        lhs,
        rhs,
        ..
    } = &equation.residual
    else {
        return None;
    };
    let Expression::VarRef {
        name, subscripts, ..
    } = lhs.as_ref()
    else {
        return None;
    };
    if !subscripts.is_empty() {
        return None;
    }
    let record = flat.record_instances.get(name.var_name())?;
    let value = match rhs.as_ref() {
        Expression::FunctionCall {
            name,
            args: _,
            is_constructor,
            span,
        } => RecordEquationValue::Aggregate(RecordCall {
            name,
            span: *span,
            is_constructor: *is_constructor,
        }),
        Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => {
            RecordEquationValue::Record(flat.record_instances.get(name.var_name())?, name)
        }
        _ => return None,
    };
    Some((record, name, value))
}
