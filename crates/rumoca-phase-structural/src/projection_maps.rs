use std::collections::HashMap;

use rumoca_ir_dae as dae;

fn extract_first_component_index(name: &str) -> Option<usize> {
    for segment in rumoca_core::split_path_with_indices(name) {
        if !segment.contains('[') && !segment.contains(']') {
            continue;
        }
        let scalar = rumoca_core::parse_scalar_name(segment)?;
        let first_index = scalar.indices.first().copied()?;
        return usize::try_from(first_index).ok().filter(|index| *index > 0);
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
        let Some(idx) = extract_first_component_index(raw) else {
            continue;
        };
        map.entry(base)
            .or_default()
            .entry(idx)
            .or_insert_with(|| raw.to_string());
    }
    map
}

pub fn output_scalar_count(dims: &[i64]) -> usize {
    if dims.is_empty() {
        return 1;
    }
    dims.iter()
        .try_fold(1usize, |acc, &dim| {
            if dim <= 0 {
                None
            } else {
                acc.checked_mul(dim as usize)
            }
        })
        .unwrap_or(0)
}

pub fn output_is_complex_record(output: &rumoca_core::FunctionParam) -> bool {
    rumoca_core::top_level_last_segment(&output.type_name) == "Complex"
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
    element_idx: Option<usize>,
    is_complex: bool,
) {
    let indexed_output = element_idx.map(|element_idx| {
        dae::scalar_name_text_for_flat_index(output_name, output_dims, element_idx - 1)
    });
    let index_suffix = indexed_output
        .as_deref()
        .and_then(|name| name.strip_prefix(output_name))
        .unwrap_or_default();
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
            push_projection_entry(
                by_index,
                scalar_idx,
                indexed_output.unwrap_or_else(|| output_name.to_string()),
            );
        }
        (false, None) => {
            push_projection_entry(by_index, scalar_idx, output_name.to_string());
        }
    }
}

/// Build projection map for scalarizing multi-output function calls.
///
/// Maps 1-based scalar output index to a projected output selector:
/// - scalar output `x` -> `x`
/// - array output `seedOut[3]` -> `seedOut[1]`, `seedOut[2]`, `seedOut[3]`
pub fn build_function_output_projection_map(
    dae: &dae::Dae,
) -> HashMap<String, HashMap<usize, String>> {
    let mut map: HashMap<String, HashMap<usize, String>> = HashMap::new();
    for (function_name, function) in &dae.symbols.functions {
        let mut by_index: HashMap<usize, String> = HashMap::new();
        let mut scalar_idx = 1usize;
        for output in &function.outputs {
            let count = output_scalar_count(&output.dims);
            let is_complex = output_is_complex_record(output);
            if count <= 1 {
                append_output_projection_entry(
                    &mut by_index,
                    &mut scalar_idx,
                    output.name.as_str(),
                    &output.dims,
                    None,
                    is_complex,
                );
                continue;
            }
            for element_idx in 1..=count {
                append_output_projection_entry(
                    &mut by_index,
                    &mut scalar_idx,
                    output.name.as_str(),
                    &output.dims,
                    Some(element_idx),
                    is_complex,
                );
            }
        }
        if !by_index.is_empty() {
            map.insert(function_name.as_str().to_string(), by_index);
        }
    }
    map
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn first_component_index_uses_structured_scalar_name_segments() {
        assert_eq!(extract_first_component_index("plug.pin[1].v"), Some(1));
        assert_eq!(extract_first_component_index("plug.pin[2, 3].v"), Some(2));
        assert_eq!(
            extract_first_component_index("plug[index.with.dot].pin[2].v"),
            None
        );
        assert_eq!(
            extract_first_component_index("plug.selector.pin[2].v"),
            Some(2)
        );
        assert_eq!(extract_first_component_index("plug.pin[0].v"), None);
        assert_eq!(extract_first_component_index("plug.pin[-1].v"), None);
        assert_eq!(extract_first_component_index("plug.pin[1.v"), None);
    }
}
