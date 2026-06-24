use super::*;
use rumoca_core::ComponentPath;

impl TypeChecker {
    /// Collect record-parameter aliases from overlay bindings (MLS §7.2.3).
    ///
    /// Example: for `chain(cellData = cellData2)`, collect
    /// `chain.cellData -> cellData2` so field references like
    /// `chain.cellData.nRC` can resolve through the modifier binding.
    pub(crate) fn collect_record_aliases(overlay: &InstanceOverlay) -> Vec<(String, String)> {
        let mut aliases: HashMap<ComponentPath, ComponentPath> = HashMap::new();
        let (known_paths, known_prefixes) = Self::collect_known_component_paths(overlay);
        for instance_data in overlay.components.values() {
            let Some(binding) = &instance_data.binding else {
                continue;
            };
            let Some(target_unqualified) = Self::extract_simple_path(binding) else {
                continue;
            };
            let source = instance_data.qualified_name.to_component_path();
            let target = Self::resolve_alias_target(
                &source,
                &target_unqualified,
                instance_data.binding_from_modification,
                &known_paths,
                &known_prefixes,
            );
            if source != target {
                aliases.insert(source, target);
            }
        }
        Self::collapse_alias_chains(&mut aliases);
        let mut out: Vec<(String, String)> = aliases
            .into_iter()
            .map(|(source, target)| (source.to_flat_string(), target.to_flat_string()))
            .collect();
        out.sort_by(|a, b| a.0.cmp(&b.0));
        out
    }

    /// Extract a simple dotted path from an expression if it is a plain reference.
    pub(crate) fn extract_simple_path(expr: &Expression) -> Option<ComponentPath> {
        match expr {
            Expression::ComponentReference(cr) => (!cr.parts.is_empty())
                .then(|| ComponentPath::from_parts(cr.parts.iter().map(ToString::to_string))),
            Expression::FieldAccess { base, field, .. } => {
                let base_path = Self::extract_simple_path(base)?;
                Some(base_path.join(&ComponentPath::from_flat_path(field)))
            }
            _ => None,
        }
    }

    /// Collect full component paths and all dotted prefixes from the overlay.
    fn collect_known_component_paths(
        overlay: &InstanceOverlay,
    ) -> (HashSet<ComponentPath>, HashSet<ComponentPath>) {
        let mut known_paths: HashSet<ComponentPath> = HashSet::new();
        let mut known_prefixes: HashSet<ComponentPath> = HashSet::new();
        for instance_data in overlay.components.values() {
            let path = instance_data.qualified_name.to_component_path();
            known_paths.insert(path.clone());
            for idx in 1..path.len() {
                known_prefixes.insert(path.prefix(idx).expect("prefix index is in range"));
            }
        }
        (known_paths, known_prefixes)
    }

    /// Resolve modifier/declaration alias targets into absolute instance scope.
    ///
    /// Applies lexical scope traversal and picks the first candidate that maps
    /// to an existing overlay path or a known parent prefix.
    fn resolve_alias_target(
        source: &ComponentPath,
        target_unqualified: &ComponentPath,
        from_modification: bool,
        known_paths: &HashSet<ComponentPath>,
        known_prefixes: &HashSet<ComponentPath>,
    ) -> ComponentPath {
        let mut scope = if from_modification {
            Self::grandparent_path(source)
        } else {
            Self::parent_path(source)
        };
        loop {
            let candidate = scope.join(target_unqualified);
            if Self::is_known_component_or_prefix(&candidate, known_paths, known_prefixes) {
                return candidate;
            }
            if scope.is_root() {
                break;
            }
            scope = scope.parent().unwrap_or_else(ComponentPath::root);
        }
        target_unqualified.clone()
    }

    fn is_known_component_or_prefix(
        candidate: &ComponentPath,
        known_paths: &HashSet<ComponentPath>,
        known_prefixes: &HashSet<ComponentPath>,
    ) -> bool {
        known_paths.contains(candidate) || known_prefixes.contains(candidate)
    }

    pub(crate) fn enclosing_scope_or_root(path: &str) -> &str {
        crate::path_utils::enclosing_scope_str(path).unwrap_or("")
    }

    fn parent_path(path: &ComponentPath) -> ComponentPath {
        path.parent().unwrap_or_else(ComponentPath::root)
    }

    fn grandparent_path(path: &ComponentPath) -> ComponentPath {
        Self::parent_path(path)
            .parent()
            .unwrap_or_else(ComponentPath::root)
    }

    /// Collapse `A->B->C` alias chains to direct `A->C` mappings.
    fn collapse_alias_chains(aliases: &mut HashMap<ComponentPath, ComponentPath>) {
        const MAX_PASSES: usize = 20;
        for _ in 0..MAX_PASSES {
            let mut changed = false;
            let keys: Vec<ComponentPath> = aliases.keys().cloned().collect();
            for key in keys {
                changed |= Self::collapse_alias_for_key(aliases, &key);
            }
            if !changed {
                break;
            }
        }
    }

    fn collapse_alias_for_key(
        aliases: &mut HashMap<ComponentPath, ComponentPath>,
        key: &ComponentPath,
    ) -> bool {
        let Some(target) = Self::resolve_alias_terminal(aliases, key) else {
            return false;
        };
        if aliases.get(key) == Some(&target) {
            return false;
        }
        aliases.insert(key.clone(), target);
        true
    }

    fn resolve_alias_terminal(
        aliases: &HashMap<ComponentPath, ComponentPath>,
        key: &ComponentPath,
    ) -> Option<ComponentPath> {
        let mut target = aliases.get(key)?.clone();
        let mut seen = std::collections::HashSet::new();
        while seen.insert(target.clone()) {
            let Some(next) = aliases.get(&target).cloned() else {
                break;
            };
            target = next;
        }
        Some(target)
    }

    /// Propagate evaluated scalar/dimension values through record aliases.
    ///
    /// If `A -> B`, then `A.field` should mirror `B.field` for compile-time
    /// evaluation of dimension expressions and range bounds.
    pub(crate) fn propagate_record_alias_values(
        &mut self,
        record_aliases: &[(String, String)],
    ) -> bool {
        Self::propagate_alias_map(record_aliases, &mut self.eval_ctx.integers)
            | Self::propagate_alias_map(record_aliases, &mut self.eval_ctx.reals)
            | Self::propagate_alias_map(record_aliases, &mut self.eval_ctx.booleans)
            | Self::propagate_alias_map(record_aliases, &mut self.eval_ctx.enums)
            | Self::propagate_alias_map(record_aliases, &mut self.eval_ctx.dimensions)
    }

    pub(crate) fn propagate_alias_map<T: Clone + PartialEq>(
        record_aliases: &[(String, String)],
        values: &mut rustc_hash::FxHashMap<String, T>,
    ) -> bool {
        if record_aliases.is_empty() || values.is_empty() {
            return false;
        }

        let mut updates: rustc_hash::FxHashMap<String, T> = rustc_hash::FxHashMap::default();
        for (alias_source, alias_target) in record_aliases {
            Self::queue_alias_root_update(alias_source, alias_target, values, &mut updates);
        }

        let mut alias_prefixes_by_head: rustc_hash::FxHashMap<String, Vec<(&str, String)>> =
            rustc_hash::FxHashMap::default();
        for (alias_source, alias_target) in record_aliases {
            let target_prefix = format!("{alias_target}.");
            alias_prefixes_by_head
                .entry(Self::alias_head(&target_prefix).to_string())
                .or_default()
                .push((alias_source.as_str(), target_prefix));
        }
        let value_keys = values.keys().cloned().collect::<Vec<_>>();
        for field_name in value_keys {
            let Some(alias_prefixes) = alias_prefixes_by_head.get(Self::alias_head(&field_name))
            else {
                continue;
            };
            Self::queue_alias_field_updates(alias_prefixes, &field_name, values, &mut updates);
        }

        let mut progress = false;
        for (name, value) in updates {
            if values.get(&name) != Some(&value) {
                values.insert(name, value);
                progress = true;
            }
        }
        progress
    }

    fn queue_alias_field_updates<T: Clone + PartialEq>(
        alias_prefixes: &[(&str, String)],
        field_name: &str,
        values: &rustc_hash::FxHashMap<String, T>,
        updates: &mut rustc_hash::FxHashMap<String, T>,
    ) {
        for (alias_source, target_prefix) in alias_prefixes {
            if !field_name.starts_with(target_prefix) {
                continue;
            }
            Self::queue_alias_field_update(
                alias_source,
                target_prefix,
                field_name,
                values,
                updates,
            );
        }
    }

    #[allow(dead_code)]
    pub(crate) fn alias_head(path: &str) -> &str {
        let mut bracket_depth = 0usize;
        for (index, byte) in path.bytes().enumerate() {
            match byte {
                b'[' => bracket_depth += 1,
                b']' => bracket_depth = bracket_depth.saturating_sub(1),
                b'.' if bracket_depth == 0 => return &path[..index],
                _ => {}
            }
        }
        path
    }
}
