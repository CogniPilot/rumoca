use super::*;
use std::collections::HashSet;

pub(super) fn lower_root_conditions(
    dae_model: &dae::Dae,
    layout: &VarLayout,
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    let mut rows =
        Vec::with_capacity(dae_model.relation.len() + dae_model.synthetic_root_conditions.len());
    for condition in &dae_model.relation {
        if root_condition_is_inactive(dae_model, condition) {
            rows.push(lower_inactive_root_row(layout, &dae_model.functions));
        } else {
            rows.push(lower_root_condition_row(
                condition,
                layout,
                &dae_model.functions,
            )?);
        }
    }
    for condition in &dae_model.synthetic_root_conditions {
        if root_condition_is_inactive(dae_model, condition) {
            rows.push(lower_inactive_root_row(layout, &dae_model.functions));
        } else {
            rows.push(lower_root_condition_row(
                condition,
                layout,
                &dae_model.functions,
            )?);
        }
    }
    Ok(rows)
}

fn lower_root_condition_row(
    condition: &dae::Expression,
    layout: &VarLayout,
    functions: &IndexMap<dae::VarName, dae::Function>,
) -> Result<Vec<LinearOp>, LowerError> {
    let mut builder = LowerBuilder::new(layout, functions);
    let scope = Scope::new();
    let root_value = match condition {
        dae::Expression::Binary { op, lhs, rhs } => match op {
            rumoca_ir_core::OpBinary::Lt(_) | rumoca_ir_core::OpBinary::Le(_) => {
                let l = builder.lower_expr(lhs, &scope, 0)?;
                let r = builder.lower_expr(rhs, &scope, 0)?;
                builder.emit_binary(BinaryOp::Sub, l, r)
            }
            rumoca_ir_core::OpBinary::Gt(_) | rumoca_ir_core::OpBinary::Ge(_) => {
                let l = builder.lower_expr(lhs, &scope, 0)?;
                let r = builder.lower_expr(rhs, &scope, 0)?;
                builder.emit_binary(BinaryOp::Sub, r, l)
            }
            _ => lower_bool_condition_as_root(condition, &mut builder, &scope)?,
        },
        _ => lower_bool_condition_as_root(condition, &mut builder, &scope)?,
    };

    builder.ops.push(LinearOp::StoreOutput { src: root_value });
    Ok(builder.ops)
}

fn lower_bool_condition_as_root(
    condition: &dae::Expression,
    builder: &mut LowerBuilder<'_>,
    scope: &Scope,
) -> Result<Reg, LowerError> {
    let cond = builder.lower_expr(condition, scope, 0)?;
    let neg_one = builder.emit_const(-1.0);
    let pos_one = builder.emit_const(1.0);
    Ok(builder.emit_select(cond, neg_one, pos_one))
}

fn lower_inactive_root_row(
    layout: &VarLayout,
    functions: &IndexMap<dae::VarName, dae::Function>,
) -> Vec<LinearOp> {
    let mut builder = LowerBuilder::new(layout, functions);
    let positive = builder.emit_const(1.0);
    builder.ops.push(LinearOp::StoreOutput { src: positive });
    builder.ops
}

fn root_condition_is_inactive(dae_model: &dae::Dae, expr: &dae::Expression) -> bool {
    uses_runtime_discrete_condition(expr)
        || expression_uses_runtime_discrete_bindings(dae_model, expr)
        || !expression_uses_known_root_bindings(dae_model, expr)
}

fn expression_uses_known_root_bindings(dae_model: &dae::Dae, expr: &dae::Expression) -> bool {
    let mut refs: HashSet<dae::VarName> = HashSet::new();
    expr.collect_var_refs(&mut refs);
    refs.into_iter().all(|name| {
        if name.as_str() == "time" {
            return true;
        }
        if has_runtime_binding(dae_model, &name) {
            return true;
        }
        dae::component_base_name(name.as_str())
            .map(|base| has_runtime_binding(dae_model, &dae::VarName::new(base)))
            .unwrap_or(false)
    })
}

fn has_runtime_binding(dae_model: &dae::Dae, name: &dae::VarName) -> bool {
    dae_model.states.contains_key(name)
        || dae_model.algebraics.contains_key(name)
        || dae_model.outputs.contains_key(name)
        || dae_model.inputs.contains_key(name)
        || dae_model.parameters.contains_key(name)
        || dae_model.constants.contains_key(name)
        || dae_model.discrete_reals.contains_key(name)
        || dae_model.discrete_valued.contains_key(name)
        || dae_model.derivative_aliases.contains_key(name)
}

fn expression_uses_runtime_discrete_bindings(dae_model: &dae::Dae, expr: &dae::Expression) -> bool {
    let mut refs: HashSet<dae::VarName> = HashSet::new();
    expr.collect_var_refs(&mut refs);
    refs.into_iter().any(|name| {
        is_runtime_discrete_binding(dae_model, &name)
            || dae::component_base_name(name.as_str())
                .map(|base| is_runtime_discrete_binding(dae_model, &dae::VarName::new(base)))
                .unwrap_or(false)
    })
}

fn is_runtime_discrete_binding(dae_model: &dae::Dae, name: &dae::VarName) -> bool {
    dae_model.discrete_reals.contains_key(name) || dae_model.discrete_valued.contains_key(name)
}

fn uses_runtime_discrete_condition(expr: &dae::Expression) -> bool {
    match expr {
        dae::Expression::BuiltinCall { function, args } => {
            if matches!(
                function,
                dae::BuiltinFunction::Sample
                    | dae::BuiltinFunction::Pre
                    | dae::BuiltinFunction::Edge
                    | dae::BuiltinFunction::Change
                    | dae::BuiltinFunction::Reinit
                    | dae::BuiltinFunction::Initial
            ) {
                return true;
            }
            args.iter().any(uses_runtime_discrete_condition)
        }
        dae::Expression::FunctionCall { name, args, .. } => {
            let short = name.as_str().rsplit('.').next().unwrap_or(name.as_str());
            if matches!(
                short,
                "sample"
                    | "Sample"
                    | "pre"
                    | "Pre"
                    | "edge"
                    | "Edge"
                    | "change"
                    | "Change"
                    | "reinit"
                    | "Reinit"
                    | "initial"
                    | "Initial"
                    | "Clock"
                    | "subSample"
                    | "superSample"
                    | "shiftSample"
                    | "backSample"
                    | "firstTick"
                    | "previous"
                    | "hold"
            ) {
                return true;
            }
            args.iter().any(uses_runtime_discrete_condition)
        }
        dae::Expression::Binary { lhs, rhs, .. } => {
            uses_runtime_discrete_condition(lhs) || uses_runtime_discrete_condition(rhs)
        }
        dae::Expression::Unary { rhs, .. } => uses_runtime_discrete_condition(rhs),
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().any(|(cond, value)| {
                uses_runtime_discrete_condition(cond) || uses_runtime_discrete_condition(value)
            }) || uses_runtime_discrete_condition(else_branch)
        }
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => {
            elements.iter().any(uses_runtime_discrete_condition)
        }
        dae::Expression::Range { start, step, end } => {
            uses_runtime_discrete_condition(start)
                || step.as_deref().is_some_and(uses_runtime_discrete_condition)
                || uses_runtime_discrete_condition(end)
        }
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            uses_runtime_discrete_condition(expr)
                || indices
                    .iter()
                    .any(|index| uses_runtime_discrete_condition(&index.range))
                || filter
                    .as_deref()
                    .is_some_and(uses_runtime_discrete_condition)
        }
        dae::Expression::Index { base, subscripts } => {
            uses_runtime_discrete_condition(base)
                || subscripts.iter().any(|sub| match sub {
                    dae::Subscript::Expr(expr) => uses_runtime_discrete_condition(expr),
                    _ => false,
                })
        }
        dae::Expression::FieldAccess { base, .. } => uses_runtime_discrete_condition(base),
        dae::Expression::VarRef { .. } | dae::Expression::Literal(_) | dae::Expression::Empty => {
            false
        }
    }
}
