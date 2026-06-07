use crate::FlattenError;
use crate::pipeline::qualify_expression;
use rumoca_ir_ast::{InstanceData, InstanceOverlay, QualifiedName};

/// Evaluate structural component bindings and parameter/constant start values.
///
/// Non-parameter variables' start values are initial conditions, not compile-time
/// constants, and must not be used for structural equation evaluation (MLS 8.6).
pub(crate) fn collect_component_binding_values(
    overlay: &InstanceOverlay,
    eval_ctx: &mut rumoca_eval_flat::constant::EvalContext,
) -> Result<(), FlattenError> {
    for instance_data in overlay.components.values() {
        if !component_binding_is_structural(instance_data) {
            continue;
        }
        let qualified_name = instance_data.qualified_name.to_flat_string();

        if eval_ctx.get(&qualified_name).is_some() {
            continue;
        }

        if let Some(binding) = &instance_data.binding {
            let flat_binding = qualify_expression(binding, &QualifiedName::new())?;
            if let Ok(value) = rumoca_eval_flat::constant::eval_expr(&flat_binding, eval_ctx) {
                eval_ctx.add_parameter(qualified_name.clone(), value);
                continue;
            }
        }

        if component_start_is_structural(instance_data)
            && let Some(start) = &instance_data.start
        {
            let flat_start = qualify_expression(start, &QualifiedName::new())?;
            if let Ok(value) = rumoca_eval_flat::constant::eval_expr(&flat_start, eval_ctx) {
                eval_ctx.add_parameter(qualified_name, value);
            }
        }
    }
    Ok(())
}

fn component_binding_is_structural(instance_data: &InstanceData) -> bool {
    component_start_is_structural(instance_data) || instance_data.is_discrete_type
}

fn component_start_is_structural(instance_data: &InstanceData) -> bool {
    matches!(
        instance_data.variability,
        rumoca_core::Variability::Parameter(_) | rumoca_core::Variability::Constant(_)
    )
}
