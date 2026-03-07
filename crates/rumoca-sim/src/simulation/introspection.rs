use std::collections::HashSet;

use rumoca_ir_dae as dae;

pub fn collect_unindexed_multiscalar_refs(
    expr: &dae::Expression,
    dae_model: &dae::Dae,
    out: &mut Vec<String>,
    seen: &mut HashSet<String>,
) {
    match expr {
        dae::Expression::VarRef { name, subscripts } => {
            if subscripts.is_empty() {
                let size = dae_model
                    .states
                    .get(name)
                    .or_else(|| dae_model.algebraics.get(name))
                    .or_else(|| dae_model.outputs.get(name))
                    .map(|v| v.size())
                    .unwrap_or(0);
                let key = name.as_str().to_string();
                if size > 1 && seen.insert(key.clone()) {
                    out.push(key);
                }
            }
        }
        dae::Expression::Binary { lhs, rhs, .. } => {
            collect_unindexed_multiscalar_refs(lhs, dae_model, out, seen);
            collect_unindexed_multiscalar_refs(rhs, dae_model, out, seen);
        }
        dae::Expression::Unary { rhs, .. } => {
            collect_unindexed_multiscalar_refs(rhs, dae_model, out, seen);
        }
        dae::Expression::BuiltinCall { args, .. } | dae::Expression::FunctionCall { args, .. } => {
            for arg in args {
                collect_unindexed_multiscalar_refs(arg, dae_model, out, seen);
            }
        }
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, value) in branches {
                collect_unindexed_multiscalar_refs(cond, dae_model, out, seen);
                collect_unindexed_multiscalar_refs(value, dae_model, out, seen);
            }
            collect_unindexed_multiscalar_refs(else_branch, dae_model, out, seen);
        }
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => {
            for element in elements {
                collect_unindexed_multiscalar_refs(element, dae_model, out, seen);
            }
        }
        dae::Expression::Range { start, step, end } => {
            collect_unindexed_multiscalar_refs(start, dae_model, out, seen);
            if let Some(step_expr) = step.as_deref() {
                collect_unindexed_multiscalar_refs(step_expr, dae_model, out, seen);
            }
            collect_unindexed_multiscalar_refs(end, dae_model, out, seen);
        }
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            for idx in indices {
                collect_unindexed_multiscalar_refs(&idx.range, dae_model, out, seen);
            }
            collect_unindexed_multiscalar_refs(expr, dae_model, out, seen);
            if let Some(filter_expr) = filter.as_deref() {
                collect_unindexed_multiscalar_refs(filter_expr, dae_model, out, seen);
            }
        }
        dae::Expression::Index { base, subscripts } => {
            collect_unindexed_multiscalar_refs(base, dae_model, out, seen);
            for sub in subscripts {
                if let dae::Subscript::Expr(expr) = sub {
                    collect_unindexed_multiscalar_refs(expr, dae_model, out, seen);
                }
            }
        }
        dae::Expression::FieldAccess { base, .. } => {
            collect_unindexed_multiscalar_refs(base, dae_model, out, seen);
        }
        dae::Expression::Literal(_) | dae::Expression::Empty => {}
    }
}

pub fn trace_flow_array_alias_watch(phase: &str, dae_model: &dae::Dae, trace: bool) {
    if !trace {
        return;
    }
    let mut hits = Vec::new();
    for (eq_idx, eq) in dae_model.f_x.iter().enumerate() {
        if !eq.origin.starts_with("flow sum equation:") {
            continue;
        }
        let mut refs = Vec::new();
        let mut seen = HashSet::new();
        collect_unindexed_multiscalar_refs(&eq.rhs, dae_model, &mut refs, &mut seen);
        if refs.is_empty() {
            continue;
        }
        let mut rhs = format!("{:?}", eq.rhs);
        const MAX_RHS: usize = 220;
        if rhs.len() > MAX_RHS {
            rhs.truncate(MAX_RHS);
            rhs.push_str("...");
        }
        hits.push(format!(
            "f_x[{eq_idx}] refs={} origin='{}' rhs={}",
            refs.join(", "),
            eq.origin,
            rhs
        ));
    }
    if hits.is_empty() {
        eprintln!("[sim-trace] flow-array-alias-watch phase={phase} suspicious=0");
        return;
    }
    eprintln!(
        "[sim-trace] flow-array-alias-watch phase={phase} suspicious={}",
        hits.len()
    );
    for sample in hits.into_iter().take(10) {
        eprintln!("[sim-trace]   {sample}");
    }
}
