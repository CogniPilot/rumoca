use super::*;

impl TypeChecker {
    /// Type check an instanced model using the overlay for modification context.
    ///
    /// This builds an evaluation context from the overlay's parameter values
    /// and evaluates dimensions for components that have unevaluated dimensions.
    pub fn check_instanced(
        &mut self,
        tree: &ClassTree,
        overlay: &mut InstanceOverlay,
        model_name: &str,
    ) {
        let Some(type_table) = self.initialize_instanced_context(tree) else {
            return;
        };
        self.populate_overlay_type_roots(tree, overlay, &type_table);
        self.resolve_overlay_component_types(overlay, &type_table);
        if !self.initialize_instanced_modifier_member_types(tree, overlay, model_name, &type_table)
        {
            return;
        }
        self.collect_overlay_eval_values(overlay);
        if !self.collect_instanced_eval_constants(tree, overlay, model_name) {
            return;
        }
        let record_aliases = Self::collect_record_aliases(overlay);

        // MLS §10.1: array dimensions must be evaluable at translation time.
        // Evaluate explicit and colon dimensions in one loop because they can
        // depend on each other through bindings and size(..) expressions.
        self.evaluate_all_dimensions_multi_pass(tree, overlay, &record_aliases);
        self.validate_dimensions(overlay);
        self.check_instanced_component_modifiers(tree, model_name, &type_table);
        self.check_instanced_equations(tree, overlay, model_name, &type_table);
        self.flush_eval_warnings();
    }

    fn initialize_instanced_context(&mut self, tree: &ClassTree) -> Option<TypeTable> {
        self.source_map = tree.source_map.clone();
        self.def_qualified_names = tree
            .def_map
            .iter()
            .map(|(def_id, name)| (*def_id, name.clone()))
            .collect();
        self.eval_ctx = rumoca_eval_ast::eval::TypeCheckEvalContext::new();
        let (type_table, type_ids_by_def_id) = match self.build_type_context(tree) {
            Ok(context) => context,
            Err(error) => {
                self.emit_typecheck_error(*error);
                return None;
            }
        };
        self.type_ids_by_def_id = type_ids_by_def_id;
        self.type_suffix_index = Self::build_type_suffix_index(&type_table);
        self.rebuild_type_roots(tree, &type_table);
        self.component_modifier_targets = modifier_targets::build_component_modifier_targets(tree);
        Some(type_table)
    }

    fn initialize_instanced_modifier_member_types(
        &mut self,
        tree: &ClassTree,
        overlay: &InstanceOverlay,
        model_name: &str,
        type_table: &TypeTable,
    ) -> bool {
        let root_def_ids =
            self.instanced_modifier_member_type_roots(tree, overlay, model_name, type_table);
        self.component_modifier_member_types =
            match modifier_targets::build_component_modifier_member_types_for_def_ids(
                tree,
                type_table,
                &self.type_ids_by_def_id,
                &self.type_suffix_index,
                &self.source_map,
                root_def_ids,
            ) {
                Ok(member_types) => member_types,
                Err(error) => {
                    self.emit_typecheck_error(*error);
                    return false;
                }
            };
        true
    }

    fn instanced_modifier_member_type_roots(
        &self,
        tree: &ClassTree,
        overlay: &InstanceOverlay,
        model_name: &str,
        type_table: &TypeTable,
    ) -> HashSet<DefId> {
        let mut roots = HashSet::new();
        if let Some(class) = tree.get_class_by_qualified_name(model_name)
            && let Some(def_id) = class.def_id
        {
            roots.insert(def_id);
            self.collect_class_component_type_roots(class, type_table, &mut roots);
        }
        for data in overlay.components.values() {
            if let Some(def_id) = data.type_def_id {
                roots.insert(def_id);
            }
            let root_type = self.resolve_type_root(type_table, data.type_id);
            if let Some(Type::Class(class_type)) = type_table.get(root_type) {
                roots.insert(class_type.def_id);
            }
        }
        roots
    }

    fn collect_class_component_type_roots(
        &self,
        class: &ClassDef,
        type_table: &TypeTable,
        roots: &mut HashSet<DefId>,
    ) {
        for component in class.components.values() {
            let type_name = component.type_name.to_string();
            let type_id = self.resolve_type_name(&type_name, component.type_def_id, type_table);
            let root_type = self.resolve_type_root(type_table, type_id);
            if let Some(Type::Class(class_type)) = type_table.get(root_type) {
                roots.insert(class_type.def_id);
            }
        }
        for nested in class.classes.values() {
            self.collect_class_component_type_roots(nested, type_table, roots);
        }
    }

    fn collect_overlay_eval_values(&mut self, overlay: &InstanceOverlay) {
        // Uses scope-aware evaluation so `nout = max(size(deltaq,1))` in component
        // `kinematicPTP` can resolve `deltaq` as `kinematicPTP.deltaq`.
        for instance_data in overlay.components.values() {
            let path = instance_data.qualified_name.to_component_path();
            let name = path.to_flat_string();
            let scope = path
                .parent()
                .map(|path| path.to_flat_string())
                .unwrap_or_default();

            if let Some(ref binding) = instance_data.binding
                && let Some(value) =
                    rumoca_eval_ast::eval::eval_integer_with_scope(binding, &self.eval_ctx, &scope)
            {
                self.eval_ctx.add_integer(&name, value);
            } else if let Some(ref start) = instance_data.start
                && let Some(value) =
                    rumoca_eval_ast::eval::eval_integer_with_scope(start, &self.eval_ctx, &scope)
            {
                self.eval_ctx.add_integer(&name, value);
            }

            if let Some(ref binding) = instance_data.binding
                && let Some(value) =
                    rumoca_eval_ast::eval::eval_boolean_with_scope(binding, &self.eval_ctx, &scope)
            {
                self.eval_ctx.booleans.insert(name.clone(), value);
            } else if let Some(ref start) = instance_data.start
                && let Some(value) =
                    rumoca_eval_ast::eval::eval_boolean_with_scope(start, &self.eval_ctx, &scope)
            {
                self.eval_ctx.booleans.insert(name.clone(), value);
            }

            if let Some(ref binding) = instance_data.binding
                && let Some(value) =
                    rumoca_eval_ast::eval::eval_real_with_scope(binding, &self.eval_ctx, &scope)
            {
                self.eval_ctx.reals.insert(name.clone(), value);
            } else if let Some(ref start) = instance_data.start
                && let Some(value) =
                    rumoca_eval_ast::eval::eval_real_with_scope(start, &self.eval_ctx, &scope)
            {
                self.eval_ctx.reals.insert(name.clone(), value);
            }

            if let Some(ref binding) = instance_data.binding
                && let Some(value) =
                    rumoca_eval_ast::eval::eval_enum_with_scope(binding, &self.eval_ctx, &scope)
            {
                self.eval_ctx.enums.insert(name.clone(), value);
            } else if let Some(ref start) = instance_data.start
                && let Some(value) =
                    rumoca_eval_ast::eval::eval_enum_with_scope(start, &self.eval_ctx, &scope)
            {
                self.eval_ctx.enums.insert(name.clone(), value);
            }

            if !instance_data.dims.is_empty() {
                self.eval_ctx.add_dimensions(
                    &name,
                    instance_data.dims.iter().map(|&d| d as usize).collect(),
                );
            }
        }
    }

    fn collect_instanced_eval_constants(
        &mut self,
        tree: &ClassTree,
        overlay: &InstanceOverlay,
        model_name: &str,
    ) -> bool {
        if let Err(error) = self.collect_enum_sizes(tree) {
            self.emit_typecheck_error(*error);
            return false;
        }
        Self::collect_import_constants(tree, &mut self.eval_ctx);
        Self::collect_model_extends_redeclare_constants(tree, model_name, &mut self.eval_ctx);
        Self::collect_nested_class_constants(tree, model_name, &mut self.eval_ctx);
        Self::collect_model_extends_redeclare_constants(tree, model_name, &mut self.eval_ctx);
        Self::collect_component_type_nested_constants(tree, overlay, &mut self.eval_ctx);
        Self::collect_enclosing_class_constants(tree, model_name, &mut self.eval_ctx);
        Self::collect_function_defs(tree, &mut self.eval_ctx);
        Self::collect_instance_class_override_constants(tree, overlay, &mut self.eval_ctx);
        true
    }

    fn check_instanced_component_modifiers(
        &mut self,
        tree: &ClassTree,
        model_name: &str,
        type_table: &TypeTable,
    ) {
        let model_class = tree
            .get_class_by_qualified_name(model_name)
            .or_else(|| tree.get_class_by_qualified_name(top_level_last_segment(model_name)));
        let Some(model_class) = model_class else {
            return;
        };
        self.check_instanced_component_modifiers_in_class(model_class, type_table);
    }

    fn check_instanced_component_modifiers_in_class(
        &mut self,
        class: &ClassDef,
        type_table: &TypeTable,
    ) {
        for (comp_name, comp) in &class.components {
            let type_name = comp.type_name.to_string();
            let type_id = self.resolve_type_name(&type_name, comp.type_def_id, type_table);
            self.validate_component_modifier_names(comp_name, comp, type_table, type_id);
        }
        for nested in class.classes.values() {
            self.check_instanced_component_modifiers_in_class(nested, type_table);
        }
    }

    /// Check equation compatibility for a specific instanced model.
    fn check_instanced_equations(
        &mut self,
        tree: &ClassTree,
        overlay: &InstanceOverlay,
        model_name: &str,
        type_table: &TypeTable,
    ) {
        let model_class = tree
            .get_class_by_qualified_name(model_name)
            .or_else(|| tree.get_class_by_qualified_name(top_level_last_segment(model_name)));
        let Some(model_class) = model_class else {
            return;
        };

        let prev_scope_types = std::mem::take(&mut self.current_component_types);
        let prev_scope_shapes = std::mem::take(&mut self.current_component_shapes);
        let (full_prefix, short_model) = Self::instanced_scope_prefixes(model_name);
        self.current_component_types =
            Self::build_instanced_component_type_scope(overlay, &full_prefix, &short_model);
        self.current_component_shapes =
            Self::build_instanced_component_shape_scope(overlay, &full_prefix, &short_model);

        self.check_component_modifier_types_in_class(model_class, type_table);
        walk_equations(self, &model_class.equations, type_table);
        walk_equations(self, &model_class.initial_equations, type_table);
        // Note: component *bindings* are not walked here. Binding
        // expressions are written in the declaring component's scope, so
        // validating them against this model-scope name map produces false
        // positives (nested `P[k]` vs a top-level scalar `P`). Binding
        // checks need per-instance scope maps first.

        self.current_component_types = prev_scope_types;
        self.current_component_shapes = prev_scope_shapes;
    }
}
