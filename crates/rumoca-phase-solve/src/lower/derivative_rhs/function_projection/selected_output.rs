use rumoca_ir_dae as dae;
use rumoca_phase_structural::projection_maps::build_function_output_projection_map;

use crate::lower::LowerError;

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
    if dae_model.symbols.functions.contains_key(name.var_name()) {
        return Ok(None);
    }
    let projection_map = build_function_output_projection_map(dae_model).map_err(|err| {
        LowerError::contract_violation(
            format!("function output projection map failed: {err}"),
            *span,
        )
    })?;
    let selected_name = name.as_str();
    for (function_name, _) in &dae_model.symbols.functions {
        let Some(by_index) = projection_map.get(function_name.as_str()) else {
            continue;
        };
        for (scalar_index, selector) in by_index {
            if selected_name != format!("{}.{}", function_name.as_str(), selector) {
                continue;
            }
            let zero_based_index = scalar_index.checked_sub(1).ok_or_else(|| {
                LowerError::contract_violation(
                    format!(
                        "function output projection for `{selected_name}` used zero scalar index"
                    ),
                    *span,
                )
            })?;
            return Ok(Some((
                rumoca_core::Expression::FunctionCall {
                    name: rumoca_core::Reference::from_var_name(function_name.clone()),
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
