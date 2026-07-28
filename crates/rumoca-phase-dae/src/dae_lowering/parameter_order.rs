use indexmap::{IndexMap, IndexSet};
use rumoca_core::ExpressionVisitor;
use rumoca_ir_dae as dae;

/// Topologically sort parameters so that start-value dependencies are ordered.
///
/// If parameter A's `start` expression references parameter B, then B must
/// appear before A in the parameter map. This ensures code generators that
/// evaluate start values sequentially produce correct numeric results.
///
/// Falls back to the original order if cycles are detected.
pub(crate) fn sort_parameters_by_start_dependency(dae: &mut dae::Dae) {
    let param_names: IndexSet<rumoca_core::VarName> =
        dae.variables.parameters.keys().cloned().collect();
    if param_names.len() <= 1 {
        return;
    }

    // Build adjacency: param → set of params its start expression depends on
    let mut deps: IndexMap<usize, IndexSet<usize>> = IndexMap::new();
    for (idx, (_, var)) in dae.variables.parameters.iter().enumerate() {
        let Some(ref start_expr) = var.start else {
            continue;
        };
        for ref_name in collect_expression_var_refs(start_expr) {
            let Some(dep_idx) = param_names.get_index_of(&ref_name) else {
                continue;
            };
            if dep_idx != idx {
                deps.entry(idx).or_default().insert(dep_idx);
            }
        }
    }

    if deps.is_empty() {
        return;
    }

    let n = param_names.len();
    let mut in_degree = vec![0usize; n];
    let mut forward: Vec<Vec<usize>> = vec![Vec::new(); n];
    for (&node, predecessors) in &deps {
        for &pred in predecessors {
            forward[pred].push(node);
            in_degree[node] += 1;
        }
    }

    let mut queue = std::collections::VecDeque::new();
    for (i, &degree) in in_degree.iter().enumerate() {
        if degree == 0 {
            queue.push_back(i);
        }
    }

    let mut sorted_indices = Vec::with_capacity(n);
    while let Some(node) = queue.pop_front() {
        sorted_indices.push(node);
        for &next in &forward[node] {
            in_degree[next] -= 1;
            if in_degree[next] == 0 {
                queue.push_back(next);
            }
        }
    }

    if sorted_indices.len() != n {
        return;
    }

    let old_params: Vec<(rumoca_core::VarName, dae::Variable)> =
        dae.variables.parameters.drain(..).collect();
    for &idx in &sorted_indices {
        let (name, var) = old_params[idx].clone();
        dae.variables.parameters.insert(name, var);
    }
}

fn collect_expression_var_refs(expr: &rumoca_core::Expression) -> Vec<rumoca_core::VarName> {
    let mut collector = VarRefListCollector { refs: Vec::new() };
    collector.visit_expression(expr);
    collector.refs
}

struct VarRefListCollector {
    refs: Vec<rumoca_core::VarName>,
}

impl ExpressionVisitor for VarRefListCollector {
    fn visit_var_ref(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
    ) {
        self.refs.push(name.var_name().clone());
        for subscript in subscripts {
            self.visit_subscript(subscript);
        }
    }
}
