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
        self.populate_nominal_class_context(tree);
        self.function_signatures = function_signatures::build_function_signatures(tree);
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
        self.expand_component_type_root_closure(tree, type_table, &mut roots);
        roots
    }

    fn expand_component_type_root_closure(
        &self,
        tree: &ClassTree,
        type_table: &TypeTable,
        roots: &mut HashSet<DefId>,
    ) {
        let mut pending = roots.iter().copied().collect::<Vec<_>>();
        while let Some(def_id) = pending.pop() {
            let Some(class) = tree.get_class_by_def_id(def_id) else {
                continue;
            };
            for extend in &class.extends {
                queue_new_type_root(extend.base_def_id, roots, &mut pending);
            }
            for component in class.components.values() {
                let type_name = component.type_name.to_string();
                let type_id = self.resolve_type_name(&type_name, component.type_def_id, type_table);
                let root_type = self.resolve_type_root(type_table, type_id);
                let component_def_id = match type_table.get(root_type) {
                    Some(Type::Class(class_type)) => Some(class_type.def_id),
                    _ => None,
                };
                queue_new_type_root(component_def_id, roots, &mut pending);
            }
            for nested in class.classes.values() {
                queue_new_type_root(nested.def_id, roots, &mut pending);
            }
        }
    }

    fn collect_overlay_eval_values(&mut self, overlay: &InstanceOverlay) {
        // Uses scope-aware evaluation so `nout = max(size(deltaq,1))` in component
        // `kinematicPTP` can resolve `deltaq` as `kinematicPTP.deltaq`.
        for instance_data in overlay.components.values() {
            let path = instance_data.qualified_name.to_component_path();
            let name = path.to_flat_string();
            let binding_scope = Self::instance_binding_scope_name(instance_data);
            let start_scope = Self::instance_attribute_scope_name(instance_data, "start");

            if let Some(ref binding) = instance_data.binding
                && let Some(value) = rumoca_eval_ast::eval::eval_integer_with_scope(
                    binding,
                    &self.eval_ctx,
                    &binding_scope,
                )
            {
                self.eval_ctx.add_integer(&name, value);
            } else if let Some(ref start) = instance_data.start
                && let Some(value) = rumoca_eval_ast::eval::eval_integer_with_scope(
                    start,
                    &self.eval_ctx,
                    &start_scope,
                )
            {
                self.eval_ctx.add_integer(&name, value);
            }

            if let Some(ref binding) = instance_data.binding
                && let Some(value) = rumoca_eval_ast::eval::eval_boolean_with_scope(
                    binding,
                    &self.eval_ctx,
                    &binding_scope,
                )
            {
                self.eval_ctx.booleans.insert(name.clone(), value);
            } else if let Some(ref start) = instance_data.start
                && let Some(value) = rumoca_eval_ast::eval::eval_boolean_with_scope(
                    start,
                    &self.eval_ctx,
                    &start_scope,
                )
            {
                self.eval_ctx.booleans.insert(name.clone(), value);
            }

            if let Some(ref binding) = instance_data.binding
                && let Some(value) = rumoca_eval_ast::eval::eval_real_with_scope(
                    binding,
                    &self.eval_ctx,
                    &binding_scope,
                )
            {
                self.eval_ctx.reals.insert(name.clone(), value);
            } else if let Some(ref start) = instance_data.start
                && let Some(value) =
                    rumoca_eval_ast::eval::eval_real_with_scope(start, &self.eval_ctx, &start_scope)
            {
                self.eval_ctx.reals.insert(name.clone(), value);
            }

            if let Some(ref binding) = instance_data.binding
                && let Some(value) = rumoca_eval_ast::eval::eval_enum_with_scope(
                    binding,
                    &self.eval_ctx,
                    &binding_scope,
                )
            {
                self.eval_ctx.enums.insert(name.clone(), value);
            } else if let Some(ref start) = instance_data.start
                && let Some(value) =
                    rumoca_eval_ast::eval::eval_enum_with_scope(start, &self.eval_ctx, &start_scope)
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

    /// Check equation compatibility for a specific instanced model.
    fn check_instanced_equations(
        &mut self,
        tree: &ClassTree,
        overlay: &InstanceOverlay,
        model_name: &str,
        type_table: &TypeTable,
    ) {
        let model_class = tree.get_class_by_qualified_name(model_name).or_else(|| {
            tree.get_class_by_qualified_name(crate::path_utils::class_name_leaf(model_name))
        });
        let Some(model_class) = model_class else {
            return;
        };

        let previous_declarations = std::mem::take(&mut self.current_declaration_semantics);
        let previous_semantics = std::mem::take(&mut self.current_instance_semantics);
        let prev_instance_scope = self.current_instance_scope.take();
        let previous_class_instance_id = self.current_class_instance_id.take();
        let prev_instance_domain_shape = std::mem::take(&mut self.current_instance_domain_shape);
        self.current_instance_semantics = InstanceSemanticScope::from_overlay(overlay);

        if overlay.classes.is_empty() {
            self.check_declaration_only_overlay(tree, overlay, model_class, model_name, type_table);
        } else {
            let mut checked_declarations = HashSet::new();
            for class_data in overlay.classes.values() {
                self.check_instanced_class_instance(
                    tree,
                    overlay,
                    class_data,
                    type_table,
                    &mut checked_declarations,
                );
            }
        }
        self.check_instanced_bindings(tree, overlay, type_table);

        self.current_declaration_semantics = previous_declarations;
        self.current_instance_semantics = previous_semantics;
        self.current_instance_scope = prev_instance_scope;
        self.current_class_instance_id = previous_class_instance_id;
        self.current_instance_domain_shape = prev_instance_domain_shape;
    }

    fn check_instanced_class_instance(
        &mut self,
        tree: &ClassTree,
        overlay: &InstanceOverlay,
        class_data: &rumoca_ir_ast::ClassInstanceData,
        type_table: &TypeTable,
        checked_declarations: &mut HashSet<(DefId, ComponentPath)>,
    ) {
        let instance_scope = class_data.qualified_name.to_component_path();
        let previous_scope = self.current_instance_scope.replace(instance_scope.clone());
        let previous_class_instance_id = self
            .current_class_instance_id
            .replace(class_data.instance_id);
        let previous_domain = std::mem::take(&mut self.current_instance_domain_shape);
        let previous_call_type_overrides = std::mem::take(&mut self.current_call_type_overrides);
        self.current_instance_domain_shape = overlay
            .components
            .values()
            .find(|data| data.qualified_name == class_data.qualified_name)
            .map(|data| data.dims.iter().map(|dim| *dim as usize).collect())
            .unwrap_or_default();
        self.current_call_type_overrides = class_data
            .class_def_id
            .map(|class_def_id| {
                function_signatures::build_call_type_overrides(
                    tree,
                    class_def_id,
                    Some(&class_data.class_overrides),
                )
            })
            .unwrap_or_default();

        if let Some(class) = class_data
            .class_def_id
            .and_then(|class_def_id| tree.get_class_by_def_id(class_def_id))
        {
            self.check_instanced_class_declarations_and_bases(
                tree,
                class,
                &instance_scope,
                type_table,
                checked_declarations,
            );
        }
        self.check_instanced_class_body(class_data, type_table);

        self.current_instance_scope = previous_scope;
        self.current_class_instance_id = previous_class_instance_id;
        self.current_instance_domain_shape = previous_domain;
        self.current_call_type_overrides = previous_call_type_overrides;
    }

    fn check_instanced_class_body(
        &mut self,
        class_data: &rumoca_ir_ast::ClassInstanceData,
        type_table: &TypeTable,
    ) {
        for equation in &class_data.equations {
            walk_equation(self, &equation.equation, type_table);
        }
        for equation in &class_data.initial_equations {
            walk_equation(self, &equation.equation, type_table);
        }
        self.check_instanced_statement_sections(&class_data.algorithms, type_table);
        self.check_instanced_statement_sections(&class_data.initial_algorithms, type_table);
    }

    fn check_instanced_statement_sections(
        &mut self,
        sections: &[Vec<rumoca_ir_ast::InstanceStatement>],
        type_table: &TypeTable,
    ) {
        for statements in sections {
            for statement in statements {
                walk_statement(self, &statement.statement, type_table);
            }
        }
    }

    /// Some focused unit/API callers provide only component overlay rows and
    /// intentionally omit the instantiated class IR. Keep that reduced input
    /// useful, while production compilation always consumes the authoritative
    /// `overlay.classes` bodies above.
    fn check_declaration_only_overlay(
        &mut self,
        tree: &ClassTree,
        overlay: &InstanceOverlay,
        model_class: &ClassDef,
        model_name: &str,
        type_table: &TypeTable,
    ) {
        let mut checked_instances = HashSet::new();
        let model_scope = ComponentPath::from_flat_path(model_name);
        self.check_declaration_body_and_bases(
            tree,
            model_class,
            &model_scope,
            type_table,
            &mut checked_instances,
        );

        for data in overlay
            .components
            .values()
            .filter(|data| !data.is_primitive)
        {
            let root_type = self.resolve_type_root(type_table, data.type_id);
            let Some(Type::Class(class_type)) = type_table.get(root_type) else {
                continue;
            };
            let Some(class) = tree.get_class_by_def_id(class_type.def_id) else {
                continue;
            };
            self.check_declaration_body_and_bases(
                tree,
                class,
                &data.qualified_name.to_component_path(),
                type_table,
                &mut checked_instances,
            );
        }
    }

    fn check_declaration_body_and_bases(
        &mut self,
        tree: &ClassTree,
        class: &ClassDef,
        instance_scope: &ComponentPath,
        type_table: &TypeTable,
        checked_instances: &mut HashSet<(DefId, ComponentPath)>,
    ) {
        let Some(class_def_id) = class.def_id else {
            return;
        };
        if !checked_instances.insert((class_def_id, instance_scope.clone())) {
            return;
        }
        for extend in &class.extends {
            let Some(base_def_id) = extend.base_def_id else {
                continue;
            };
            if let Some(base) = tree.get_class_by_def_id(base_def_id) {
                self.check_declaration_body_and_bases(
                    tree,
                    base,
                    instance_scope,
                    type_table,
                    checked_instances,
                );
            }
        }

        let previous_scope = self.current_instance_scope.replace(instance_scope.clone());
        let previous_call_type_overrides = std::mem::take(&mut self.current_call_type_overrides);
        self.current_call_type_overrides =
            function_signatures::build_call_type_overrides(tree, class_def_id, None);
        self.check_instanced_class_declaration(class, type_table);
        walk_equations(self, &class.equations, type_table);
        walk_equations(self, &class.initial_equations, type_table);
        for statements in &class.algorithms {
            walk_statements(self, statements, type_table);
        }
        for statements in &class.initial_algorithms {
            walk_statements(self, statements, type_table);
        }
        self.current_instance_scope = previous_scope;
        self.current_call_type_overrides = previous_call_type_overrides;
    }

    fn check_instanced_class_declarations_and_bases(
        &mut self,
        tree: &ClassTree,
        class: &ClassDef,
        instance_scope: &ComponentPath,
        type_table: &TypeTable,
        checked_instances: &mut HashSet<(DefId, ComponentPath)>,
    ) {
        let Some(class_def_id) = class.def_id else {
            return;
        };
        if !checked_instances.insert((class_def_id, instance_scope.clone())) {
            return;
        }

        for extend in &class.extends {
            let Some(base_def_id) = extend.base_def_id else {
                continue;
            };
            if let Some(base) = tree.get_class_by_def_id(base_def_id) {
                self.check_instanced_class_declarations_and_bases(
                    tree,
                    base,
                    instance_scope,
                    type_table,
                    checked_instances,
                );
            }
        }

        self.check_instanced_class_declaration(class, type_table);
    }

    fn check_instanced_class_declaration(&mut self, class: &ClassDef, type_table: &TypeTable) {
        for component in class.components.values() {
            let Some(def_id) = component.def_id else {
                continue;
            };
            let type_id = self.resolve_type_name(
                &component.type_name.to_string(),
                component.type_def_id,
                type_table,
            );
            self.current_declaration_semantics.insert(
                def_id,
                ComponentSemantics::from_declaration_with_type(component, type_id),
            );
        }
        for (name, component) in &class.components {
            let type_id = self.resolve_type_name(
                &component.type_name.to_string(),
                component.type_def_id,
                type_table,
            );
            self.validate_component_modifier_names(name, component, type_table, type_id);
        }
        self.check_component_modifier_types_in_class(class, type_table);
        self.validate_variability_constraints(class);
    }

    fn check_instanced_bindings(
        &mut self,
        tree: &ClassTree,
        overlay: &InstanceOverlay,
        type_table: &TypeTable,
    ) {
        for data in overlay.components.values() {
            let Some(binding) = data.binding.as_ref() else {
                continue;
            };
            // A modification-derived binding carries two intentional forms:
            // `binding` is the resolved semantic value used by later IR stages,
            // while `binding_source` is the expression as written in its
            // lexical source scope. Type checking must keep expression and
            // scope paired; checking a resolved outer value in the inner
            // source scope can capture a same-named scalar component.
            let binding_to_check = if data.binding_from_modification {
                data.binding_source.as_ref().unwrap_or(binding)
            } else {
                binding
            };
            // MLS §7.2.4: a modification binding is evaluated in the lexical
            // scope where the modifier was written, not in the modified
            // component's scope. Instantiation records that source scope so
            // type lookup cannot accidentally capture a same-named nested
            // component (for example `stack(stackData=stackData)`).
            let binding_scope = if data.binding_from_modification {
                data.binding_source_scope
                    .as_ref()
                    .map(|scope| scope.to_component_path())
                    .or_else(|| data.qualified_name.to_component_path().parent())
            } else {
                data.qualified_name.to_component_path().parent()
            };
            let previous_scope = std::mem::replace(&mut self.current_instance_scope, binding_scope);
            let previous_call_type_overrides =
                std::mem::take(&mut self.current_call_type_overrides);
            self.current_call_type_overrides =
                call_type_overrides_for_instance_scope(tree, overlay, &self.current_instance_scope);
            walk_expression(self, binding_to_check, type_table);
            if let Some(found) = self.infer_expression_type(binding_to_check, type_table) {
                self.check_expected_expression_type(
                    data.type_id,
                    found,
                    binding_to_check
                        .get_location()
                        .or(Some(&data.source_location)),
                    "component binding type compatibility",
                    "component binding here",
                    type_table,
                );
            }
            self.current_instance_scope = previous_scope;
            self.current_call_type_overrides = previous_call_type_overrides;
        }
    }
}

fn call_type_overrides_for_instance_scope(
    tree: &ClassTree,
    overlay: &InstanceOverlay,
    scope: &Option<ComponentPath>,
) -> function_signatures::CallTypeOverrides {
    let Some(scope) = scope else {
        return function_signatures::CallTypeOverrides::default();
    };
    let mut candidate = Some(scope.clone());
    while let Some(path) = candidate {
        if let Some(class_data) = overlay
            .classes
            .values()
            .find(|data| data.qualified_name.to_component_path() == path)
            && let Some(class_def_id) = class_data.class_def_id
        {
            return function_signatures::build_call_type_overrides(
                tree,
                class_def_id,
                Some(&class_data.class_overrides),
            );
        }
        candidate = path.parent();
    }
    function_signatures::CallTypeOverrides::default()
}

fn queue_new_type_root(
    candidate: Option<DefId>,
    roots: &mut HashSet<DefId>,
    pending: &mut Vec<DefId>,
) {
    let Some(def_id) = candidate else {
        return;
    };
    if roots.insert(def_id) {
        pending.push(def_id);
    }
}
