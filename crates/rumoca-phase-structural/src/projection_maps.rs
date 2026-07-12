use std::collections::HashMap;

use rumoca_core::{ComponentRefPart, Span, Subscript};
use rumoca_ir_dae as dae;

use crate::StructuralError;

pub type ProjectionParts = Vec<ComponentRefPart>;
pub type FunctionOutputProjectionMap =
    HashMap<rumoca_core::FunctionInstanceId, HashMap<usize, ProjectionParts>>;
pub type RecordFieldProjectionMap =
    HashMap<rumoca_core::FunctionInstanceId, HashMap<String, HashMap<usize, ProjectionParts>>>;

fn extract_first_component_index(name: &rumoca_core::VarName) -> Option<usize> {
    for segment in name.segments() {
        if let Some(scalar) = rumoca_core::parse_scalar_name(segment) {
            let first_index = scalar.indices.first().copied()?;
            return usize::try_from(first_index).ok().filter(|index| *index > 0);
        }
        if rumoca_core::split_trailing_subscript_suffix(segment).is_some() {
            return None;
        }
    }
    None
}

/// Build projection map for scalarized component-array field names.
///
/// Example:
/// - `plug.pin[1].v` contributes base `plug.pin.v` with index `1`.
pub fn build_component_index_projection_map(
    dae: &dae::Dae,
) -> HashMap<String, HashMap<usize, String>> {
    let mut map: HashMap<String, HashMap<usize, String>> = HashMap::new();
    for (name, _) in dae
        .variables
        .states
        .iter()
        .chain(dae.variables.algebraics.iter())
        .chain(dae.variables.outputs.iter())
        .chain(dae.variables.parameters.iter())
        .chain(dae.variables.constants.iter())
        .chain(dae.variables.inputs.iter())
    {
        let raw = name.as_str();
        let Some(base) = dae::component_base_name(raw) else {
            continue;
        };
        if base == raw {
            continue;
        }
        let Some(idx) = extract_first_component_index(name) else {
            continue;
        };
        map.entry(base)
            .or_default()
            .entry(idx)
            .or_insert_with(|| raw.to_string());
    }
    map
}

pub fn output_scalar_count(dims: &[i64], span: Span) -> Result<usize, StructuralError> {
    if dims.is_empty() {
        return Ok(1);
    }
    let mut count = 1usize;
    for &dim in dims {
        let dim = usize::try_from(dim).map_err(|_| StructuralError::ContractViolation {
            reason: format!("array output dimensions must be non-negative, got {dims:?}"),
            span,
        })?;
        count = count
            .checked_mul(dim)
            .ok_or_else(|| StructuralError::ContractViolation {
                reason: format!("array output dimensions overflow scalar count: {dims:?}"),
                span,
            })?;
    }
    Ok(count)
}

pub fn output_is_complex_record(output: &rumoca_core::FunctionParam) -> bool {
    rumoca_core::qualified_type_name_matches(&output.type_name, "Complex")
}

fn push_projection_entry(
    by_index: &mut HashMap<usize, ProjectionParts>,
    scalar_idx: &mut usize,
    selector: ProjectionParts,
) {
    by_index.insert(*scalar_idx, selector);
    *scalar_idx += 1;
}

fn append_output_projection_entry(
    by_index: &mut HashMap<usize, ProjectionParts>,
    scalar_idx: &mut usize,
    output_name: &str,
    output_dims: &[i64],
    output_span: Span,
    element_idx: Option<usize>,
    is_complex: bool,
) -> Result<(), StructuralError> {
    let subs = element_idx
        .map(|index| projection_subscripts(output_dims, index - 1, output_span))
        .transpose()?
        .unwrap_or_default();
    let part = |ident: &str, subs: Vec<Subscript>| ComponentRefPart {
        ident: ident.to_string(),
        span: output_span,
        subs,
    };
    match (is_complex, element_idx) {
        (true, Some(_)) => {
            push_projection_entry(
                by_index,
                scalar_idx,
                vec![part(output_name, vec![]), part("re", subs.clone())],
            );
            push_projection_entry(
                by_index,
                scalar_idx,
                vec![part(output_name, vec![]), part("im", subs)],
            );
        }
        (true, None) => {
            push_projection_entry(
                by_index,
                scalar_idx,
                vec![part(output_name, vec![]), part("re", vec![])],
            );
            push_projection_entry(
                by_index,
                scalar_idx,
                vec![part(output_name, vec![]), part("im", vec![])],
            );
        }
        (false, Some(_)) => {
            push_projection_entry(by_index, scalar_idx, vec![part(output_name, subs)]);
        }
        (false, None) => {
            push_projection_entry(by_index, scalar_idx, vec![part(output_name, vec![])]);
        }
    }
    Ok(())
}

fn projection_subscripts(
    dims: &[i64],
    flat_index: usize,
    span: Span,
) -> Result<Vec<Subscript>, StructuralError> {
    dae::flat_index_to_subscripts(dims, flat_index)
        .ok_or_else(|| StructuralError::ContractViolation {
            reason: format!("invalid output projection index {flat_index} for dimensions {dims:?}"),
            span,
        })?
        .into_iter()
        .map(|index| {
            i64::try_from(index)
                .map(|value| Subscript::index(value, span))
                .map_err(|_| StructuralError::ContractViolation {
                    reason: format!("output projection index {index} exceeds Modelica Integer"),
                    span,
                })
        })
        .collect()
}

/// Build projection map for scalarizing multi-output function calls.
///
/// Maps 1-based scalar output index to a projected output selector:
/// - scalar output `x` -> `x`
/// - array output `seedOut[3]` -> `seedOut[1]`, `seedOut[2]`, `seedOut[3]`
pub fn build_function_output_projection_map(
    dae: &dae::Dae,
) -> Result<FunctionOutputProjectionMap, StructuralError> {
    let mut map = HashMap::new();
    for function in dae.symbols.functions.values() {
        let instance_id =
            function
                .instance_id
                .ok_or_else(|| StructuralError::ContractViolation {
                    reason: format!(
                        "function `{}` lacks flattened instance identity",
                        function.name
                    ),
                    span: function.span,
                })?;
        let mut by_index = HashMap::new();
        let mut scalar_idx = 1usize;
        for output in &function.outputs {
            let count = output_scalar_count(&output.dims, output.span)?;
            let is_complex = output_is_complex_record(output);
            if count <= 1 {
                append_output_projection_entry(
                    &mut by_index,
                    &mut scalar_idx,
                    output.name.as_str(),
                    &output.dims,
                    output.span,
                    None,
                    is_complex,
                )?;
                continue;
            }
            for element_idx in 1..=count {
                append_output_projection_entry(
                    &mut by_index,
                    &mut scalar_idx,
                    output.name.as_str(),
                    &output.dims,
                    output.span,
                    Some(element_idx),
                    is_complex,
                )?;
            }
        }
        if !by_index.is_empty() {
            map.insert(instance_id, by_index);
        }
    }
    Ok(map)
}

fn append_record_field_projection(
    dae: &dae::Dae,
    record_type: rumoca_core::DefId,
    selector_prefix: &[ComponentRefPart],
    field_prefix: &str,
    fields: &mut HashMap<String, HashMap<usize, ProjectionParts>>,
    active_types: &mut Vec<rumoca_core::DefId>,
) -> Result<(), StructuralError> {
    if active_types.contains(&record_type) {
        return Ok(());
    }
    let selector_span = selector_prefix
        .last()
        .map(|part| part.span)
        .ok_or_else(|| StructuralError::UnspannedContractViolation {
            reason: "record projection selector is empty".to_string(),
        })?;
    let Some(constructor) = dae
        .symbols
        .functions
        .values()
        .find(|function| function.is_constructor && function.def_id == Some(record_type))
    else {
        return Err(StructuralError::ContractViolation {
            reason: format!("record type {record_type} has no constructor metadata"),
            span: selector_span,
        });
    };
    active_types.push(record_type);
    for field in &constructor.inputs {
        let field_path = if field_prefix.is_empty() {
            field.name.clone()
        } else {
            format!("{field_prefix}.{}", field.name)
        };
        let mut selector = selector_prefix.to_vec();
        selector.push(ComponentRefPart {
            ident: field.name.clone(),
            span: field.span,
            subs: vec![],
        });
        if field.type_class == Some(rumoca_core::ClassType::Record) {
            let field_type_def_id =
                field
                    .type_def_id
                    .ok_or_else(|| StructuralError::ContractViolation {
                        reason: format!("record field `{field_path}` lacks resolved type identity"),
                        span: field.span,
                    })?;
            append_record_field_projection(
                dae,
                field_type_def_id,
                &selector,
                &field_path,
                fields,
                active_types,
            )?;
            continue;
        }
        let count = output_scalar_count(&field.dims, field.span)?;
        let by_index = fields.entry(field_path).or_default();
        if count <= 1 {
            by_index.insert(1, selector);
            continue;
        }
        for element_index in 1..=count {
            let mut indexed = selector.clone();
            let last = indexed
                .last_mut()
                .ok_or_else(|| StructuralError::ContractViolation {
                    reason: "record field selector is empty".to_string(),
                    span: field.span,
                })?;
            last.subs = projection_subscripts(&field.dims, element_index - 1, field.span)?;
            by_index.insert(element_index, indexed);
        }
    }
    active_types.pop();
    Ok(())
}

/// Map array fields of record-valued function results to scalar output paths.
///
/// For a function returning `Pose pose` with `Real position[2]`, this records
/// `position -> {1: pose.position[1], 2: pose.position[2]}`. Record schemas are
/// read from their retained constructor functions and may be nested.
pub fn build_record_field_projection_map(
    dae: &dae::Dae,
) -> Result<RecordFieldProjectionMap, StructuralError> {
    let mut map = HashMap::new();
    for function in dae.symbols.functions.values() {
        if function.is_constructor {
            continue;
        }
        let Some(output) = function.outputs.first() else {
            continue;
        };
        if output.type_class != Some(rumoca_core::ClassType::Record) {
            continue;
        }
        let instance_id =
            function
                .instance_id
                .ok_or_else(|| StructuralError::ContractViolation {
                    reason: format!(
                        "function `{}` lacks flattened instance identity",
                        function.name
                    ),
                    span: function.span,
                })?;
        let type_def_id = output
            .type_def_id
            .ok_or_else(|| StructuralError::ContractViolation {
                reason: format!(
                    "record output `{}.{}` lacks resolved type identity",
                    function.name, output.name
                ),
                span: output.span,
            })?;
        let mut fields = HashMap::new();
        append_record_field_projection(
            dae,
            type_def_id,
            &[ComponentRefPart {
                ident: output.name.clone(),
                span: output.span,
                subs: vec![],
            }],
            "",
            &mut fields,
            &mut Vec::new(),
        )?;
        if !fields.is_empty() {
            map.insert(instance_id, fields);
        }
    }
    Ok(map)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn first_component_index_uses_structured_scalar_name_segments() {
        assert_eq!(
            extract_first_component_index(&rumoca_core::VarName::new("plug.pin[1].v")),
            Some(1)
        );
        assert_eq!(
            extract_first_component_index(&rumoca_core::VarName::new("plug.pin[2, 3].v")),
            Some(2)
        );
        assert_eq!(
            extract_first_component_index(&rumoca_core::VarName::new(
                "plug[index.with.dot].pin[2].v"
            )),
            None
        );
        assert_eq!(
            extract_first_component_index(&rumoca_core::VarName::new("plug.selector.pin[2].v")),
            Some(2)
        );
        assert_eq!(
            extract_first_component_index(&rumoca_core::VarName::new("plug.pin[0].v")),
            None
        );
        assert_eq!(
            extract_first_component_index(&rumoca_core::VarName::new("plug.pin[-1].v")),
            None
        );
        assert_eq!(
            extract_first_component_index(&rumoca_core::VarName::new("plug.pin[1.v")),
            None
        );
    }

    #[test]
    fn output_scalar_count_accepts_zero_dimensions() {
        assert_eq!(output_scalar_count(&[], Span::DUMMY).unwrap(), 1);
        assert_eq!(output_scalar_count(&[0], Span::DUMMY).unwrap(), 0);
        assert_eq!(output_scalar_count(&[2, 0, 3], Span::DUMMY).unwrap(), 0);
    }

    #[test]
    fn output_scalar_count_rejects_negative_dimensions() {
        let err = output_scalar_count(&[2, -1], Span::DUMMY).unwrap_err();
        assert!(format!("{err:?}").contains("non-negative"));
    }
}
