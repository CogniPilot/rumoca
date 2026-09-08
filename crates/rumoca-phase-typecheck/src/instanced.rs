use super::*;

impl TypeChecker {
    /// Type check an instanced model using the overlay for modification context.
    ///
    /// This builds an evaluation context from the overlay's parameter values
    /// and evaluates dimensions for components that have unevaluated dimensions.
    ///
    /// The overlay arrives by value and becomes the checker's owned working
    /// state; on zero errors it is sealed inside the minted proof, and on
    /// failure it is dropped, so no caller ever observes a partially
    /// annotated overlay.
    pub(super) fn check_instanced(
        self,
        resolved: &ResolvedTree,
        mut overlay: InstanceOverlay,
        model_name: &str,
    ) -> Result<crate::TypedInstancedTree, Diagnostics> {
        let tree = resolved.inner();
        let diagnostics = self.check_instanced_detached(
            tree,
            &mut overlay,
            model_name,
            resolved
                .semantic_catalogs()
                .clone_for_typecheck_publication(),
        );
        if diagnostics.has_errors() {
            Err(diagnostics)
        } else {
            Ok(crate::TypedInstancedTree::mint(
                resolved.project_for_typecheck(),
                overlay,
                model_name.to_string(),
            ))
        }
    }

    /// Exercise the instanced checker from unit fixtures that intentionally
    /// build or mutate a raw class tree. This issuer does not exist in
    /// production builds and does not construct the Resolve-branded catalog.
    #[cfg(test)]
    pub(super) fn check_instanced_test_projection(
        mut self,
        tree: &ClassTree,
        overlay: &mut InstanceOverlay,
        model_name: &str,
    ) -> Diagnostics {
        let mut candidate = overlay.clone();
        if matches!(
            candidate.finalized_overconstrained(),
            Err(rumoca_ir_ast::EqualityConstraintOccurrenceError::OwnerCatalogNotFinalized)
        ) && let Err(error) = candidate.finalize_overconstrained_record_owners()
        {
            self.emit_typecheck_error(TypeCheckError::missing_source_context(format!(
                "cannot finalize test overconstrained owner catalogs: {error:?}",
            )));
            return self.diagnostics;
        }
        let semantic_catalogs = match crate::semantic_catalog_projection_for_test(tree) {
            Ok(catalogs) => catalogs,
            Err(error) => {
                self.emit_typecheck_error(TypeCheckError::missing_source_context(format!(
                    "cannot project test semantic catalogs: {error}",
                )));
                return self.diagnostics;
            }
        };
        let diagnostics =
            self.check_instanced_detached(tree, &mut candidate, model_name, semantic_catalogs);
        if !diagnostics.has_errors() {
            *overlay = candidate;
        }
        diagnostics
    }

    fn check_instanced_detached(
        mut self,
        tree: &ClassTree,
        overlay: &mut InstanceOverlay,
        model_name: &str,
        semantic_catalogs: rumoca_ir_ast::SemanticCatalogProjection,
    ) -> Diagnostics {
        self.check_instanced_with_semantic_projection(tree, overlay, model_name, semantic_catalogs);
        self.diagnostics
    }

    fn check_instanced_with_semantic_projection(
        &mut self,
        tree: &ClassTree,
        overlay: &mut InstanceOverlay,
        model_name: &str,
        semantic_catalogs: rumoca_ir_ast::SemanticCatalogProjection,
    ) {
        let Some((type_table, type_root_catalog)) = self.initialize_instanced_context(tree) else {
            return;
        };
        let used_functions = match collect_overlay_function_declarations(overlay) {
            Ok(used) => used,
            Err(error) => {
                self.emit_typecheck_error(*missing_checked_call_identity(error));
                self.flush_eval_warnings();
                return;
            }
        };
        if let Err(error) = self.populate_overlay_type_roots_from_catalog(
            tree,
            overlay,
            &type_table,
            used_functions,
            &semantic_catalogs,
            type_root_catalog,
        ) {
            self.emit_typecheck_error(*error);
            self.flush_eval_warnings();
            return;
        }
        self.resolve_overlay_component_types(tree, overlay, &type_table);
        self.initialize_instanced_modifier_member_types(tree);
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
        if !self.has_errors() && !self.finalize_effective_types(overlay, semantic_catalogs) {
            self.flush_eval_warnings();
            return;
        }
        self.flush_eval_warnings();
    }

    /// Mint one deterministic identity for each resolved nominal-type/shape pair.
    ///
    /// This is deliberately the final typecheck transition: equation checking
    /// still addresses the nominal `TypeTable`, while successful consumers see
    /// only concrete effective identities.
    pub(super) fn finalize_effective_types(
        &mut self,
        overlay: &mut InstanceOverlay,
        semantic_catalogs: rumoca_ir_ast::SemanticCatalogProjection,
    ) -> bool {
        match overlay.finalize_effective_type_publication(semantic_catalogs) {
            Ok(()) => true,
            Err(rumoca_ir_ast::EffectiveTypePublicationError::EqualityConstraint(reason)) => {
                self.emit_equality_constraint_effective_type_error(overlay, reason);
                false
            }
            Err(error) => {
                self.emit_effective_type_publication_error(overlay, error);
                false
            }
        }
    }

    fn emit_equality_constraint_effective_type_error(
        &mut self,
        overlay: &InstanceOverlay,
        reason: rumoca_ir_ast::EqualityConstraintOccurrenceError,
    ) {
        let data = reason
            .occurrence()
            .and_then(|occurrence| overlay.components.get(&occurrence))
            .or_else(|| overlay.components.values().next());
        let Some(data) = data else {
            self.emit_typecheck_error(TypeCheckError::missing_source_context(format!(
                "cannot construct the effective equalityConstraint catalog: {reason}",
            )));
            return;
        };
        let Some(span) = self.diagnostic_location_span(
            &data.source_location,
            "effective equalityConstraint exposure",
        ) else {
            return;
        };
        self.emit_typecheck_error(TypeCheckError::phase_diagnostic(
            "ET013",
            format!(
                "cannot construct the effective equalityConstraint exposure of `{}`: {reason}",
                data.qualified_name.to_flat_string(),
            ),
            "effective record occurrence declared here",
            span,
        ));
    }

    fn emit_effective_type_publication_error(
        &mut self,
        overlay: &InstanceOverlay,
        error: rumoca_ir_ast::EffectiveTypePublicationError,
    ) {
        if let Some(data) = error
            .occurrence()
            .and_then(|occurrence| overlay.components.get(&occurrence))
        {
            self.emit_effective_type_error(data, error.to_string());
        } else {
            self.emit_typecheck_error(TypeCheckError::missing_source_context(format!(
                "cannot publish the effective type catalog: {error}",
            )));
        }
    }

    fn emit_effective_type_error(
        &mut self,
        data: &rumoca_ir_ast::InstanceData,
        reason: impl Into<String>,
    ) {
        let Some(span) =
            self.diagnostic_location_span(&data.source_location, "effective type identity")
        else {
            return;
        };
        self.emit_typecheck_error(TypeCheckError::phase_diagnostic(
            "ET000",
            format!(
                "cannot construct the effective type of `{}`: {}",
                data.qualified_name.to_flat_string(),
                reason.into()
            ),
            "concrete component type declared here",
            span,
        ));
    }

    pub(super) fn initialize_instanced_context(
        &mut self,
        tree: &ClassTree,
    ) -> Option<(TypeTable, ResolvedTypeRootCatalog)> {
        self.source_map = tree.source_map.clone();
        self.def_qualified_names = tree
            .def_map
            .iter()
            .map(|(def_id, name)| (*def_id, name.clone()))
            .collect();
        self.populate_nominal_class_context(tree);
        self.populate_operator_record_capabilities(tree);
        self.function_signatures = function_signatures::build_function_signatures(tree);
        self.eval_ctx = rumoca_eval_ast::eval::TypeCheckEvalContext::for_resolved_identities();
        register_predefined_eval_functions(tree, &mut self.eval_ctx);
        let (type_table, type_ids_by_def_id) = match self.build_type_context(tree) {
            Ok(context) => context,
            Err(error) => {
                self.emit_typecheck_error(*error);
                return None;
            }
        };
        self.type_ids_by_def_id = type_ids_by_def_id;
        let type_root_catalog = match self.construct_type_root_catalog(tree, &type_table) {
            Ok(catalog) => catalog,
            Err(error) => {
                self.emit_typecheck_error(*error);
                return None;
            }
        };
        self.type_roots = type_root_catalog.roots.clone();
        Some((type_table, type_root_catalog))
    }

    fn initialize_instanced_modifier_member_types(&mut self, tree: &ClassTree) {
        self.class_members =
            modifier_targets::build_modifier_member_catalog(tree, &self.type_ids_by_def_id);
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
        Self::collect_component_type_enclosing_constants(tree, overlay, &mut self.eval_ctx);
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

        self.validate_reachable_modifier_targets(tree, model_class, type_table);
        for data in overlay.components.values() {
            let root = self.resolve_type_root(data.type_id);
            let Some(Type::Class(class_type)) = type_table.get(root) else {
                continue;
            };
            if let Some(class) = tree.get_class_by_def_id(class_type.def_id) {
                self.validate_reachable_modifier_targets(tree, class, type_table);
            }
        }

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
                class_data.instance_id,
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
            let root_type = self.resolve_type_root(data.type_id);
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
        self.check_instanced_class_declaration(class, None, type_table);
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
        class_instance_id: InstanceId,
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
                    class_instance_id,
                    instance_scope,
                    type_table,
                    checked_instances,
                );
            }
        }

        self.check_instanced_class_declaration(class, Some(class_instance_id), type_table);
    }

    fn check_instanced_class_declaration(
        &mut self,
        class: &ClassDef,
        class_instance_id: Option<InstanceId>,
        type_table: &TypeTable,
    ) {
        for component in class.components.values() {
            let Some(def_id) = component.def_id else {
                continue;
            };
            let Some(type_id) =
                self.instanced_declaration_type(component, class_instance_id, type_table)
            else {
                continue;
            };
            self.current_declaration_semantics.insert(
                def_id,
                ComponentSemantics::from_declaration_with_type(component, type_id),
            );
        }
        self.check_component_modifier_types_in_class(class, type_table);
        self.validate_variability_constraints(class);
    }

    fn instanced_declaration_type(
        &mut self,
        component: &Component,
        class_instance_id: Option<InstanceId>,
        type_table: &TypeTable,
    ) -> Option<TypeId> {
        if let (Some(class_instance_id), Some(def_id)) = (class_instance_id, component.def_id) {
            match self
                .current_instance_semantics
                .lookup_declaration(class_instance_id, def_id)
            {
                SemanticLookup::Found(semantics) => return Some(semantics.type_id),
                SemanticLookup::Ambiguous => {
                    self.emit_ambiguous_occurrence_type(component, def_id);
                    return None;
                }
                SemanticLookup::InvalidAstSubscript => {
                    unreachable!("declaration lookup has no subscript input")
                }
                SemanticLookup::Missing => {}
            }
            if component.type_def_id.is_none()
                && component.type_name.name.len() > 1
                && component.type_name.def_id.is_some()
            {
                return None;
            }
        }
        Some(self.resolve_type_name(
            &component.type_name.to_string(),
            component.type_def_id,
            type_table,
        ))
    }

    fn emit_ambiguous_occurrence_type(&mut self, component: &Component, def_id: DefId) {
        let Some(span) = self
            .diagnostic_location_span(&component.location, "ambiguous occurrence type identity")
        else {
            return;
        };
        self.emit_typecheck_error(TypeCheckError::phase_diagnostic(
            "ET000",
            format!(
                "component declaration {:?} has heterogeneous types in one instance",
                def_id
            ),
            "component declaration has conflicting occurrence types",
            span,
        ));
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
            let previous_class_instance_id =
                std::mem::replace(&mut self.current_class_instance_id, data.owner_class_id);
            let previous_call_type_overrides =
                std::mem::take(&mut self.current_call_type_overrides);
            self.current_call_type_overrides =
                call_type_overrides_for_instance_scope(tree, overlay, &self.current_instance_scope);
            walk_expression(self, binding_to_check, type_table);
            if let Some(found) = self
                .infer_expression_type(binding_to_check, type_table)
                .value_identity()
            {
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
            self.current_class_instance_id = previous_class_instance_id;
            self.current_call_type_overrides = previous_call_type_overrides;
        }
    }
}

pub(super) fn overlay_component_type_specializations(
    tree: &ClassTree,
    overlay: &InstanceOverlay,
) -> HashMap<ComponentPath, function_signatures::CallTypeOverrides> {
    overlay
        .classes
        .values()
        .filter(|class_data| !class_data.class_overrides.is_empty())
        .filter_map(|class_data| {
            let class_def_id = class_data.class_def_id?;
            let overrides = function_signatures::build_call_type_overrides(
                tree,
                class_def_id,
                Some(&class_data.class_overrides),
            );
            Some((class_data.qualified_name.to_component_path(), overrides))
        })
        .collect()
}

pub(super) fn specialized_instance_type_def_id(
    data: &rumoca_ir_ast::InstanceData,
    specializations: &HashMap<ComponentPath, function_signatures::CallTypeOverrides>,
) -> Option<DefId> {
    let declaration_type = data.type_def_id?;
    let alias = data.type_reference_root_def_id?;
    let mut scope = data.qualified_name.to_component_path().parent();
    while let Some(current) = scope {
        if let Some(overrides) = specializations.get(&current)
            && let Some(effective) =
                overrides.specialized_declared_type_by_alias_def_id(alias, declaration_type)
        {
            return Some(effective);
        }
        scope = current.parent();
    }
    None
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
