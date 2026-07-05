use crate::FlattenError;
use crate::pipeline::qualify_expression;
use rumoca_ir_ast::{InstanceData, InstanceOverlay, QualifiedName};

/// Evaluate structural component bindings.
///
/// Start values are initialization attributes, not declaration values, and must
/// not drive structural equation evaluation (MLS 4.4.1, 8.6).
pub(crate) fn collect_component_binding_values(
    overlay: &InstanceOverlay,
    eval_ctx: &mut rumoca_eval_flat::constant::EvalContext,
) -> Result<(), FlattenError> {
    for instance_data in overlay.components.values() {
        if !component_binding_is_structural(instance_data) {
            continue;
        }
        let qualified_name = instance_data.qualified_name.to_flat_string();

        if let Some(binding) = &instance_data.binding {
            let flat_binding = qualify_expression(binding, &QualifiedName::new())?;
            if let Ok(value) = rumoca_eval_flat::constant::eval_expr(&flat_binding, eval_ctx) {
                eval_ctx.add_parameter(qualified_name.clone(), value);
                continue;
            }
        }
    }
    Ok(())
}

fn component_binding_is_structural(instance_data: &InstanceData) -> bool {
    matches!(
        instance_data.variability,
        rumoca_core::Variability::Parameter(_) | rumoca_core::Variability::Constant(_)
    ) || instance_data.is_discrete_type
}
