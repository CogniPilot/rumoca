use super::Context;
use crate::FlattenError;
use crate::pipeline::qualify_expression;
use rumoca_ir_ast::{InstanceData, InstanceOverlay, QualifiedName};

/// Evaluate structurally eligible component bindings.
///
/// `start` values are initialization hints/equations, not declaration bindings,
/// and must never be substituted for an absent or unevaluable binding (MLS 8.6).
///
/// Each binding is lowered from the overlay here, outside the Flat model, so
/// its calls receive their exact occurrence from `call_canonicalizer` before
/// the evaluator sees them; a call the catalog cannot restate stays unresolved
/// and is refused, never matched by name.
pub(crate) fn collect_component_binding_values(
    ctx: &Context,
    overlay: &InstanceOverlay,
    eval_ctx: &mut rumoca_eval_flat::constant::EvalContext,
    call_canonicalizer: &mut crate::functions::StructuralFoldCallCanonicalizer<'_>,
) -> Result<(), FlattenError> {
    for instance_data in overlay.components.values() {
        eval_ctx.set_lookup_scope(
            instance_data
                .qualified_name
                .parent()
                .map(|scope| scope.to_component_path()),
        );
        let qualified_name = instance_data.qualified_name.to_flat_string();
        if !component_binding_is_structural(ctx, instance_data, &qualified_name) {
            continue;
        }

        if eval_ctx.get(&qualified_name).is_some() {
            continue;
        }

        if let Some(binding) = &instance_data.binding {
            let mut flat_binding = qualify_expression(binding, &QualifiedName::new())?;
            call_canonicalizer.canonicalize(&mut flat_binding)?;
            if let Some(value) = crate::constant_eval::evaluate_optional(
                &flat_binding,
                eval_ctx,
                "evaluating a structural component binding",
                binding.span(),
            )? {
                eval_ctx.add_parameter(qualified_name, value);
            }
        }
    }
    eval_ctx.set_lookup_scope(None);
    Ok(())
}

fn component_binding_is_structural(
    ctx: &Context,
    instance_data: &InstanceData,
    qualified_name: &str,
) -> bool {
    match instance_data.variability {
        rumoca_core::Variability::Constant(_) => true,
        rumoca_core::Variability::Parameter(_) => {
            instance_data.evaluate
                || (instance_data.fixed != Some(false)
                    && !ctx.non_structural_params.contains(qualified_name))
        }
        _ => instance_data.is_discrete_type,
    }
}
