use super::*;

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
    let Some((record, call)) = record_equation(flat, equation) else {
        return Ok(None);
    };
    if !record.dims.is_empty() {
        return Err(ToDaeError::unsupported_flat(
            "record equation",
            "arrays of records require a compact record-family owner",
            equation.span,
        ));
    }
    let record_type = flat.record_types.get(&record.type_def_id).ok_or_else(|| {
        ToDaeError::unsupported_flat(
            "record equation",
            format!("`{}` has no resolved Flat field layout", record.type_name),
            record.source_span,
        )
    })?;
    let constructor = equation_constructor(flat, record, call, equation.span)?;
    if !record_layout_matches(record_type, constructor) {
        return Err(ToDaeError::unsupported_flat(
            "record equation",
            "Flat record field and constructor layouts disagree",
            record.source_span,
        ));
    }
    let fields = record_type
        .fields
        .iter()
        .enumerate()
        .map(|(ordinal, field)| {
            let coordinate = VarName::new(format!("{}.{}", record_name(equation), field.name));
            if !flat.variables.contains_key(&coordinate) {
                return Err(ToDaeError::unresolved_reference(
                    coordinate.as_str(),
                    equation.span,
                ));
            }
            Ok(RecordEquationFieldPlan {
                coordinate,
                ordinal,
            })
        })
        .collect::<Result<Vec<_>, _>>()?;
    Ok(Some(RecordEquationPlan { fields }))
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
        &record.type_name,
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

fn record_layout_matches(record: &flat::RecordType, constructor: &rumoca_core::Function) -> bool {
    record.fields.len() == constructor.inputs.len()
        && record
            .fields
            .iter()
            .zip(&constructor.inputs)
            .all(|(field, input)| {
                input.def_id == Some(field.def_id)
                    && field.name == input.name
                    && field.dims == input.dimensions()
            })
}

struct RecordCall<'scope> {
    name: &'scope rumoca_core::Reference,
    span: Span,
    is_constructor: bool,
}

fn record_equation<'scope>(
    flat: &'scope flat::Model,
    equation: &'scope flat::Equation,
) -> Option<(&'scope flat::RecordInstance, RecordCall<'scope>)> {
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
    let Expression::FunctionCall {
        name,
        args: _,
        is_constructor,
        span,
    } = rhs.as_ref()
    else {
        return None;
    };
    Some((
        record,
        RecordCall {
            name,
            span: *span,
            is_constructor: *is_constructor,
        },
    ))
}

fn record_name(equation: &flat::Equation) -> &str {
    let Expression::Binary { lhs, .. } = &equation.residual else {
        unreachable!("record equation certificate has a binary residual")
    };
    let Expression::VarRef { name, .. } = lhs.as_ref() else {
        unreachable!("record equation certificate has a variable left operand")
    };
    name.as_str()
}
