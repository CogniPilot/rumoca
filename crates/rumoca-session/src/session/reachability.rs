use indexmap::{IndexMap, IndexSet};

/// Reachability planner for strict target compilation.
///
/// The planner traverses class dependency edges starting from the requested
/// target and then filters the closure down to compilable model names.
pub(crate) struct ReachabilityPlanner<'a> {
    graph: &'a IndexMap<String, IndexSet<String>>,
    compilable_models: IndexSet<String>,
}

impl<'a> ReachabilityPlanner<'a> {
    pub(crate) fn new(
        graph: &'a IndexMap<String, IndexSet<String>>,
        compilable_models: &[String],
    ) -> Self {
        Self {
            graph,
            compilable_models: compilable_models.iter().cloned().collect(),
        }
    }

    pub(crate) fn reachable_classes(&self, requested_model: &str) -> Vec<String> {
        let mut visited = IndexSet::new();
        let mut stack = vec![requested_model.to_string()];

        while let Some(current) = stack.pop() {
            if !visited.insert(current.clone()) {
                continue;
            }

            let mut deps: Vec<_> = self
                .graph
                .get(&current)
                .map(|set| set.iter().cloned().collect())
                .unwrap_or_default();
            // Stable traversal order for deterministic planning and tests.
            deps.sort_unstable_by(|a, b| b.cmp(a));
            stack.extend(deps.into_iter().filter(|dep| !visited.contains(dep)));
        }

        visited.into_iter().collect()
    }

    pub(crate) fn compile_targets(&self, requested_model: &str) -> Vec<String> {
        let reachable = self.reachable_classes(requested_model);
        let mut targets = IndexSet::new();
        targets.insert(requested_model.to_string());
        for name in reachable {
            if self.compilable_models.contains(&name) {
                targets.insert(name);
            }
        }
        targets.into_iter().collect()
    }
}
