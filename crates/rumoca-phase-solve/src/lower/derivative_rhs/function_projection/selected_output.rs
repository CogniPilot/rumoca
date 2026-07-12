use rumoca_ir_dae as dae;
use rumoca_phase_structural::projection_maps::build_function_output_projection_map;

use crate::lower::LowerError;
use crate::projection_suffix::{output_projection_suffix, resolve_function_reference};

pub(super) fn selected_function_output_call(
    expr: &rumoca_core::Expression,
    dae_model: &dae::Dae,
) -> Result<Option<(rumoca_core::Expression, usize)>, LowerError> {
    let rumoca_core::Expression::FunctionCall {
        name,
        args,
        is_constructor: false,
        span,
    } = expr
    else {
        return Ok(None);
    };
    let Some((function_name, _)) = resolve_function_reference(&dae_model.symbols.functions, name)
    else {
        return Ok(None);
    };
    if name.var_name() == function_name {
        return Ok(None);
    }
    let Some(projection) = output_projection_suffix(function_name, name) else {
        return Ok(None);
    };
    let projection_map = build_function_output_projection_map(dae_model).map_err(|err| {
        LowerError::contract_violation(
            format!("function output projection map failed: {err}"),
            *span,
        )
    })?;
    let mut selector = Vec::with_capacity(1 + projection.output_fields.len());
    selector.push((projection.output_name.as_str(), Vec::new()));
    selector.extend(
        projection
            .output_fields
            .iter()
            .map(|field| (field.as_str(), Vec::new())),
    );
    if let Some((_, indices)) = selector.last_mut() {
        *indices = projection.indices;
    }
    if let Some(by_index) = projection_map.get(function_name.as_str()) {
        for (scalar_index, candidate) in by_index {
            if !projection_parts_match(candidate, &selector) {
                continue;
            }
            let zero_based_index = scalar_index.checked_sub(1).ok_or_else(|| {
                LowerError::contract_violation(
                    format!(
                        "function output projection for `{}` used zero scalar index",
                        name.as_str()
                    ),
                    *span,
                )
            })?;
            let mut base_ref = name.component_ref().cloned().ok_or_else(|| {
                LowerError::contract_violation(
                    "projected function call lacks structured identity",
                    *span,
                )
            })?;
            base_ref.parts.truncate(function_name.segments().len());
            return Ok(Some((
                rumoca_core::Expression::FunctionCall {
                    name: rumoca_core::Reference::from_component_reference(base_ref),
                    args: args.clone(),
                    is_constructor: false,
                    span: *span,
                },
                zero_based_index,
            )));
        }
    }
    Ok(None)
}

fn projection_parts_match(
    candidate: &[rumoca_core::ComponentRefPart],
    selector: &[(&str, Vec<usize>)],
) -> bool {
    candidate.len() == selector.len()
        && candidate
            .iter()
            .zip(selector)
            .all(|(part, (name, indices))| {
                part.ident == *name
                    && part.subs.len() == indices.len()
                    && part.subs.iter().zip(indices).all(|(subscript, index)| {
                        matches!(subscript, rumoca_core::Subscript::Index { value, .. }
                        if usize::try_from(*value).ok() == Some(*index))
                    })
            })
}
