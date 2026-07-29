use super::*;

pub(super) fn analyze_record_equations(
    flat: &flat::Model,
    equations: &[flat::Equation],
) -> Result<HashMap<usize, RecordEquationPlan>, ToDaeError> {
    let mut plans = HashMap::new();
    for (row, equation) in equations.iter().enumerate() {
        let Some((record, call)) = record_equation(flat, equation) else {
            continue;
        };
        if !record.dims.is_empty() {
            return Err(ToDaeError::unsupported_flat(
                "record equation",
                "arrays of records require a compact record-family owner",
                equation.span,
            ));
        }
        let function = flat
            .functions
            .get(call.name.var_name())
            .ok_or_else(|| ToDaeError::unresolved_reference(call.name.as_str(), call.span))?;
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
        let constructor = rumoca_core::resolve_record_constructor(
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
        })?;
        if record_type.fields.len() != constructor.inputs.len()
            || !record_type
                .fields
                .iter()
                .zip(&constructor.inputs)
                .all(|(field, input)| field.name == input.name && field.dims == input.dims)
        {
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
        plans.insert(row, RecordEquationPlan { fields });
    }
    Ok(plans)
}

struct RecordCall<'scope> {
    name: &'scope rumoca_core::Reference,
    span: Span,
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
        is_constructor: false,
        span,
    } = rhs.as_ref()
    else {
        return None;
    };
    Some((record, RecordCall { name, span: *span }))
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
