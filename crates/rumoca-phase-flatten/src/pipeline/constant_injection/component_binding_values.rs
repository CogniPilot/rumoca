use rumoca_ir_flat::{Model, Variable};

/// Evaluate the already-qualified Flat bindings and parameter/constant starts.
///
/// MLS §7.2.4: normal variable lowering owns modifier scope and declaration
/// identity. A forwarded AST value can still refer to its original writer;
/// re-lowering it here would bypass the proved binding used by Flat.
///
/// Non-parameter variables' start values are initial conditions, not compile-time
/// constants, and must not be used for structural equation evaluation (MLS 8.6).
pub(crate) fn collect_component_binding_values(
    flat: &Model,
    eval_ctx: &mut rumoca_eval_flat::constant::EvalContext,
) {
    for variable in flat.variables.values() {
        if !component_binding_is_structural(variable) {
            continue;
        }
        let qualified_name = variable.name.to_string();

        if eval_ctx.get(&qualified_name).is_some() {
            continue;
        }

        if let Some(binding) = &variable.binding
            && let Ok(value) = rumoca_eval_flat::constant::eval_expr(binding, eval_ctx)
        {
            eval_ctx.add_parameter(qualified_name.clone(), value);
            continue;
        }

        if component_start_is_structural(variable)
            && let Some(start) = &variable.start
            && let Ok(value) = rumoca_eval_flat::constant::eval_expr(start, eval_ctx)
        {
            eval_ctx.add_parameter(qualified_name, value);
        }
    }
}

fn component_binding_is_structural(variable: &Variable) -> bool {
    component_start_is_structural(variable) || variable.is_discrete_type
}

fn component_start_is_structural(variable: &Variable) -> bool {
    matches!(
        variable.variability,
        rumoca_core::Variability::Parameter(_) | rumoca_core::Variability::Constant(_)
    )
}
