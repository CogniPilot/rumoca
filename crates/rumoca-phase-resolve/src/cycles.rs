//! Phase 3: Cycle Detection - detect indirect inheritance cycles.
//!
//! This phase uses DFS-based cycle detection on the inheritance graph
//! to find cycles that weren't caught during resolution (e.g., A extends B, B extends A).

use crate::Resolver;
use rumoca_core::{DefId, Diagnostic, Label};
use rumoca_ir_ast as ast;
use std::collections::{HashMap, HashSet};

impl Resolver {
    /// Check for circular inheritance using the collected inheritance edges (MLS §7.1).
    ///
    /// This detects indirect cycles that aren't caught during resolution,
    /// such as: model A extends B; model B extends A;
    ///
    /// Uses DFS-based cycle detection on the inheritance graph.
    pub(crate) fn check_inheritance_cycles(&mut self, _def: &ast::StoredDefinition) {
        // Build adjacency list from inheritance edges
        // Map from class DefId to list of (base DefId, location)
        let mut graph: HashMap<DefId, Vec<(DefId, ast::Location)>> = HashMap::new();
        for (class_id, base_id, location) in &self.inheritance_edges {
            graph
                .entry(*class_id)
                .or_default()
                .push((*base_id, location.clone()));
        }

        // Track visited nodes and nodes in current path
        let mut visited: HashSet<DefId> = HashSet::new();
        let mut in_path: HashSet<DefId> = HashSet::new();
        let mut path: Vec<DefId> = Vec::new();

        // Check each class for cycles
        let all_classes: Vec<DefId> = graph.keys().copied().collect();
        for start in all_classes {
            if !visited.contains(&start) {
                self.detect_cycle_dfs(start, &graph, &mut visited, &mut in_path, &mut path);
            }
        }
    }

    /// DFS helper for cycle detection.
    fn detect_cycle_dfs(
        &mut self,
        node: DefId,
        graph: &HashMap<DefId, Vec<(DefId, ast::Location)>>,
        visited: &mut HashSet<DefId>,
        in_path: &mut HashSet<DefId>,
        path: &mut Vec<DefId>,
    ) {
        visited.insert(node);
        in_path.insert(node);
        path.push(node);

        if let Some(edges) = graph.get(&node) {
            self.process_cycle_edges(node, edges, graph, visited, in_path, path);
        }

        path.pop();
        in_path.remove(&node);
    }

    /// Process edges during cycle detection DFS.
    fn process_cycle_edges(
        &mut self,
        node: DefId,
        edges: &[(DefId, ast::Location)],
        graph: &HashMap<DefId, Vec<(DefId, ast::Location)>>,
        visited: &mut HashSet<DefId>,
        in_path: &mut HashSet<DefId>,
        path: &mut Vec<DefId>,
    ) {
        for (base_id, location) in edges {
            if in_path.contains(base_id) {
                self.emit_cycle_error(node, *base_id, location, path);
            } else if !visited.contains(base_id) {
                self.detect_cycle_dfs(*base_id, graph, visited, in_path, path);
            }
        }
    }

    /// Emit a circular inheritance error with cycle path information.
    fn emit_cycle_error(
        &mut self,
        node: DefId,
        base_id: DefId,
        location: &ast::Location,
        path: &[DefId],
    ) {
        let cycle_start_idx = path.iter().position(|&id| id == base_id).unwrap_or(0);
        let cycle_path: Vec<_> = path[cycle_start_idx..]
            .iter()
            .filter_map(|id| self.def_names.get(id))
            .cloned()
            .collect();

        let class_name = self
            .def_names
            .get(&node)
            .cloned()
            .unwrap_or_else(|| "unknown".to_string());
        let base_name = self
            .def_names
            .get(&base_id)
            .cloned()
            .unwrap_or_else(|| "unknown".to_string());

        let span = crate::location_to_span(location, &self.source_map);
        self.diagnostics.emit(
            Diagnostic::error(format!(
                "circular inheritance: `{}` extends `{}` creating cycle: {}",
                class_name,
                base_name,
                cycle_path.join(" -> ")
            ))
            .with_code("ER004")
            .with_label(Label::primary(span).with_message("extends clause creates cycle")),
        );
    }
}
