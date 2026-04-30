use std::collections::HashMap;

use rumoca_ir_dae as dae;

fn extract_first_component_index(name: &str) -> Option<usize> {
    let open = name.find('[')?;
    let close = name[open + 1..].find(']')?;
    let inside = &name[open + 1..open + 1 + close];
    let token = inside.split(',').next()?.trim();
    let idx = token.parse::<usize>().ok()?;
    (idx > 0).then_some(idx)
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
        .states
        .iter()
        .chain(dae.algebraics.iter())
        .chain(dae.outputs.iter())
        .chain(dae.parameters.iter())
        .chain(dae.constants.iter())
        .chain(dae.inputs.iter())
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

pub fn output_is_complex_record(output: &dae::FunctionParam) -> bool {
    output
        .type_name
        .rsplit('.')
        .next()
        .is_some_and(|leaf| leaf == "Complex")
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
    element_idx: Option<usize>,
    is_complex: bool,
) {
    match (is_complex, element_idx) {
        (true, Some(element_idx)) => {
            push_projection_entry(
                by_index,
                scalar_idx,
                format!("{output_name}.re[{element_idx}]"),
            );
            push_projection_entry(
                by_index,
                scalar_idx,
                format!("{output_name}.im[{element_idx}]"),
            );
        }
        (true, None) => {
            push_projection_entry(by_index, scalar_idx, format!("{output_name}.re"));
            push_projection_entry(by_index, scalar_idx, format!("{output_name}.im"));
        }
        (false, Some(element_idx)) => {
            push_projection_entry(
                by_index,
                scalar_idx,
                format!("{output_name}[{element_idx}]"),
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
    for (function_name, function) in &dae.functions {
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
