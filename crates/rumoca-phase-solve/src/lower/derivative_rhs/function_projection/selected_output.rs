use crate::lower::LowerError;
use crate::projection_suffix::{
    OutputProjectionSuffix, output_projection_suffix, resolve_function_reference,
};
use rumoca_ir_dae as dae;

pub(super) fn selected_function_output_call(
    expr: &rumoca_core::Expression,
    dae_model: &dae::Dae,
) -> Result<Option<(rumoca_core::Expression, OutputProjectionSuffix)>, LowerError> {
    let rumoca_core::Expression::FunctionCall {
        name,
        args,
        is_constructor: false,
        span,
    } = expr
    else {
        return Ok(None);
    };
    let Some((_, function)) = resolve_function_reference(&dae_model.symbols.functions, name) else {
        return Ok(None);
    };
    let Some(resolved) = name.resolved_function() else {
        return Ok(None);
    };
    if name.component_ref().map(|reference| reference.parts.len()) == Some(resolved.base_part_count)
    {
        return Ok(None);
    }
    let Some(projection) = output_projection_suffix(function, name) else {
        return Ok(None);
    };
    let mut base_ref = name.component_ref().cloned().ok_or_else(|| {
        LowerError::contract_violation("projected function call lacks structured identity", *span)
    })?;
    base_ref.parts.truncate(resolved.base_part_count);
    Ok(Some((
        rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::from_component_reference(base_ref)
                .with_resolved_function(resolved),
            args: args.clone(),
            is_constructor: false,
            span: *span,
        },
        projection,
    )))
}
