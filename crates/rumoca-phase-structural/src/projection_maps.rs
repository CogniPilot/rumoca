use std::collections::HashMap;

use rumoca_core::Span;
use rumoca_ir_dae as dae;

use crate::StructuralError;

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
    by_index: &mut HashMap<usize, String>,
    scalar_idx: &mut usize,
    selector: String,
) {
    by_index.insert(*scalar_idx, selector);
    *scalar_idx += 1;
}

fn append_output_projection_entry(
    by_index: &mut HashMap<usize, String>,
    scalar_idx: &mut usize,
    output_name: &str,
    output_dims: &[i64],
    output_span: Span,
    element_idx: Option<usize>,
    is_complex: bool,
) -> Result<(), StructuralError> {
    let indexed_output = element_idx.map(|element_idx| {
        dae::scalar_name_text_for_flat_index(output_name, output_dims, element_idx - 1)
    });
    let index_suffix = match (indexed_output.as_deref(), element_idx) {
        (Some(name), Some(_)) => name.strip_prefix(output_name).ok_or_else(|| {
            StructuralError::ContractViolation {
                reason: format!(
                    "scalarized output selector `{name}` is not prefixed by output `{output_name}`"
                ),
                span: output_span,
            }
        })?,
        (None, Some(_)) => {
            return Err(StructuralError::ContractViolation {
                reason: format!(
                    "missing scalarized output selector for `{output_name}` at index {element_idx:?}"
                ),
                span: output_span,
            });
        }
        _ => "",
    };
    match (is_complex, element_idx) {
        (true, Some(_)) => {
            push_projection_entry(
                by_index,
                scalar_idx,
                format!("{output_name}.re{index_suffix}"),
            );
            push_projection_entry(
                by_index,
                scalar_idx,
                format!("{output_name}.im{index_suffix}"),
            );
        }
        (true, None) => {
            push_projection_entry(by_index, scalar_idx, format!("{output_name}.re"));
            push_projection_entry(by_index, scalar_idx, format!("{output_name}.im"));
        }
        (false, Some(_)) => {
            let selector = indexed_output.ok_or_else(|| StructuralError::ContractViolation {
                reason: format!(
                    "missing scalarized output selector for `{output_name}` at index {element_idx:?}"
                ),
                span: output_span,
            })?;
            push_projection_entry(by_index, scalar_idx, selector);
        }
        (false, None) => {
            push_projection_entry(by_index, scalar_idx, output_name.to_string());
        }
    }
    Ok(())
}

/// Build projection map for scalarizing multi-output function calls.
///
/// Maps 1-based scalar output index to a projected output selector:
/// - scalar output `x` -> `x`
/// - array output `seedOut[3]` -> `seedOut[1]`, `seedOut[2]`, `seedOut[3]`
pub fn build_function_output_projection_map(
    dae: &dae::Dae,
) -> Result<HashMap<String, HashMap<usize, String>>, StructuralError> {
    let mut map: HashMap<String, HashMap<usize, String>> = HashMap::new();
    for (function_name, function) in &dae.symbols.functions {
        let mut by_index: HashMap<usize, String> = HashMap::new();
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
            map.insert(function_name.as_str().to_string(), by_index);
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
