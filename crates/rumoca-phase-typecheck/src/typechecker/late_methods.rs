use super::*;

#[derive(Clone, Copy)]
pub(crate) enum BuiltinModifierExpectedType {
    Component(rumoca_ir_ast::BuiltinType),
    Boolean,
    String,
}

enum ModifierPathAdvance {
    Next(TypeId),
    Complete,
    Invalid,
}

impl TypeChecker {
    pub(crate) fn alias_field_key_range<'a>(
        sorted_keys: &'a [String],
        target_prefix: &str,
    ) -> &'a [String] {
        let start = sorted_keys.partition_point(|name| name.as_str() < target_prefix);
        let end_rel = sorted_keys[start..].partition_point(|name| name.starts_with(target_prefix));
        &sorted_keys[start..start + end_rel]
    }

    pub(crate) fn queue_alias_root_update<T: Clone + PartialEq>(
        alias_source: &str,
        alias_target: &str,
        values: &rustc_hash::FxHashMap<String, T>,
        updates: &mut rustc_hash::FxHashMap<String, T>,
    ) {
        let Some(value) = values.get(alias_target).cloned() else {
            return;
        };
        if values.get(alias_source) == Some(&value) {
            return;
        }
        updates.entry(alias_source.to_string()).or_insert(value);
    }

    pub(crate) fn queue_alias_field_update<T: Clone + PartialEq>(
        alias_source: &str,
        target_prefix: &str,
        target_field_name: &str,
        values: &rustc_hash::FxHashMap<String, T>,
        updates: &mut rustc_hash::FxHashMap<String, T>,
    ) {
        let Some(field_suffix) = target_field_name.strip_prefix(target_prefix) else {
            return;
        };
        let Some(value) = values.get(target_field_name) else {
            return;
        };
        let mut alias_name = String::with_capacity(alias_source.len() + 1 + field_suffix.len());
        alias_name.push_str(alias_source);
        alias_name.push('.');
        alias_name.push_str(field_suffix);
        if values.get(alias_name.as_str()) == Some(value) {
            return;
        }
        updates.entry(alias_name).or_insert_with(|| value.clone());
    }

    /// Single pass of explicit (non-colon) dimension evaluation.
    ///
    /// Returns true if any progress was made (dimensions were evaluated or updated).
    pub(crate) fn evaluate_explicit_dimensions_pass(
        &mut self,
        overlay: &mut InstanceOverlay,
    ) -> bool {
        let mut progress = false;
        let type_scope_hints = Self::build_type_scope_hints(overlay);
        for (_def_id, instance_data) in overlay.components.iter_mut() {
            // Skip components without explicit dims.
            if instance_data.dims_expr.is_empty() {
                continue;
            }

            // Skip colon dimensions (handled by infer_colon_dimensions_single_pass)
            let has_colon = instance_data
                .dims_expr
                .iter()
                .any(|s| matches!(s, rumoca_ir_ast::Subscript::Range { .. }));
            if has_colon {
                continue;
            }

            let name = instance_data.qualified_name.to_flat_string();

            // Get the parent scope for parameter lookup
            let scope = crate::path_utils::parent_scope(&name).unwrap_or("");

            // Try to evaluate each dimension expression using scope-aware lookup
            let evaluated: Option<Vec<i64>> = instance_data
                .dims_expr
                .iter()
                .map(|sub| self.eval_dimension_with_fallback(sub, scope, &type_scope_hints))
                .collect();

            if let Some(dims) = evaluated
                && dims != instance_data.dims
            {
                instance_data.dims = dims;
                self.eval_ctx.add_dimensions(
                    &name,
                    instance_data.dims.iter().map(|&d| d as usize).collect(),
                );
                progress = true;
            }
        }

        progress
    }

    /// Evaluate one dimension expression using instance scope, then type-scope fallback.
    pub(crate) fn eval_dimension_with_fallback(
        &self,
        sub: &rumoca_ir_ast::Subscript,
        instance_scope: &str,
        type_scope_hints: &HashMap<String, String>,
    ) -> Option<i64> {
        eval::eval_dimension_with_scope(sub, &self.eval_ctx, instance_scope)
            .or_else(|| {
                Self::eval_dimension_with_type_scope_fallback(
                    sub,
                    instance_scope,
                    type_scope_hints,
                    &self.eval_ctx,
                )
            })
            .map(|v| v as i64)
    }

    /// Build component-name → type-scope hints for dimension lookup fallback.
    ///
    /// For component `state1` of type `Medium.ThermodynamicState`, this records:
    /// `state1` → `Medium`. Nested fields like `state1.X` can then resolve `nX`
    /// through the type scope when instance-scope lookup is insufficient.
    pub(crate) fn build_type_scope_hints(overlay: &InstanceOverlay) -> HashMap<String, String> {
        let mut hints = HashMap::new();
        for (_def_id, instance_data) in &overlay.components {
            let Some(pos) = crate::path_utils::find_last_top_level_dot(&instance_data.type_name)
            else {
                continue;
            };
            let component_name = instance_data.qualified_name.to_flat_string();
            let type_scope = &instance_data.type_name[..pos];
            if !type_scope.is_empty() {
                hints.insert(component_name, type_scope.to_string());
            }
        }
        hints
    }

    /// Fallback dimension evaluation using enclosing component type scopes.
    ///
    /// Walks up `instance_scope` ancestors and, for the first component with a
    /// known type scope, tries evaluating the subscript in that type scope.
    pub(crate) fn eval_dimension_with_type_scope_fallback(
        sub: &rumoca_ir_ast::Subscript,
        instance_scope: &str,
        type_scope_hints: &HashMap<String, String>,
        ctx: &eval::TypeCheckEvalContext,
    ) -> Option<usize> {
        if instance_scope.is_empty() {
            return None;
        }
        let mut current = instance_scope;
        loop {
            if let Some(type_scope) = type_scope_hints.get(current)
                && let Some(v) = eval::eval_dimension_with_scope(sub, ctx, type_scope)
            {
                return Some(v);
            }
            if let Some(parent_scope) = crate::path_utils::parent_scope(current) {
                current = parent_scope;
            } else {
                break;
            }
        }
        None
    }

    /// Re-evaluate integer parameters that may depend on size() of arrays.
    ///
    /// Returns true if any integer value was added or corrected.
    pub(crate) fn reevaluate_integer_parameters(&mut self, overlay: &InstanceOverlay) -> bool {
        let mut progress = false;

        for (_def_id, instance_data) in &overlay.components {
            let name = instance_data.qualified_name.to_flat_string();
            let scope = crate::path_utils::parent_scope(&name).unwrap_or("");

            // Recompute from the most specific declaration source each pass.
            // MLS §10.1 dependency chains can reveal better values in later passes;
            // keep updating until reaching a fixed point instead of keeping stale
            // early values from fallback scope resolution.
            let computed = instance_data
                .binding
                .as_ref()
                .and_then(|binding| eval::eval_integer_with_scope(binding, &self.eval_ctx, scope));
            let computed = computed.or_else(|| {
                instance_data
                    .start
                    .as_ref()
                    .and_then(|start| eval::eval_integer_with_scope(start, &self.eval_ctx, scope))
            });
            if let Some(value) = computed
                && self.eval_ctx.get_integer(&name) != Some(value)
            {
                self.eval_ctx.add_integer(&name, value);
                progress = true;
            }
        }

        progress
    }

    /// Re-evaluate boolean and real parameters that may now be computable.
    ///
    /// Returns true if any new values were computed. Boolean values enable
    /// if-expression evaluation for dimension inference. Real values enable
    /// `integer(realParam)` evaluation.
    pub(crate) fn reevaluate_boolean_and_real_parameters(
        &mut self,
        overlay: &InstanceOverlay,
    ) -> bool {
        let mut progress = false;

        for (_def_id, instance_data) in &overlay.components {
            let name = instance_data.qualified_name.to_flat_string();
            let scope = crate::path_utils::parent_scope(&name).unwrap_or("");

            if !self.eval_ctx.booleans.contains_key(&name) {
                progress |= self.try_eval_boolean(instance_data, &name, scope);
            }

            if !self.eval_ctx.reals.contains_key(&name) {
                progress |= self.try_eval_real(instance_data, &name, scope);
            }
        }

        progress
    }

    /// Try to evaluate a boolean value from binding or start.
    pub(crate) fn try_eval_boolean(
        &mut self,
        data: &rumoca_ir_ast::InstanceData,
        name: &str,
        scope: &str,
    ) -> bool {
        let binding_val = data
            .binding
            .as_ref()
            .and_then(|b| eval::eval_boolean_with_scope(b, &self.eval_ctx, scope));
        let start_val = data
            .start
            .as_ref()
            .and_then(|s| eval::eval_boolean_with_scope(s, &self.eval_ctx, scope));
        if let Some(value) = binding_val.or(start_val) {
            self.eval_ctx.booleans.insert(name.to_string(), value);
            return true;
        }
        false
    }

    /// Try to evaluate a real value from binding or start.
    pub(crate) fn try_eval_real(
        &mut self,
        data: &rumoca_ir_ast::InstanceData,
        name: &str,
        scope: &str,
    ) -> bool {
        let binding_val = data
            .binding
            .as_ref()
            .and_then(|b| eval::eval_real_with_scope(b, &self.eval_ctx, scope));
        let start_val = data
            .start
            .as_ref()
            .and_then(|s| eval::eval_real_with_scope(s, &self.eval_ctx, scope));
        if let Some(value) = binding_val.or(start_val) {
            self.eval_ctx.reals.insert(name.to_string(), value);
            return true;
        }
        false
    }

    /// Single pass of colon dimension inference.
    ///
    /// Returns true if any progress was made (dimensions were inferred).
    pub(crate) fn infer_colon_dimensions_single_pass(
        &mut self,
        overlay: &mut InstanceOverlay,
    ) -> bool {
        let mut progress = false;

        for (_def_id, instance_data) in overlay.components.iter_mut() {
            // Skip if dimensions are already evaluated
            if !instance_data.dims.is_empty() {
                continue;
            }

            // Only handle colon dimensions
            let has_colon = instance_data
                .dims_expr
                .iter()
                .any(|s| matches!(s, rumoca_ir_ast::Subscript::Range { .. }));
            if !has_colon {
                continue;
            }

            if self.try_infer_instance_dims(instance_data) {
                progress = true;
            }
        }

        progress
    }

    /// Try to infer dimensions for a single instance with colon dimensions.
    ///
    /// Returns true if dimensions were successfully inferred.
    pub(crate) fn try_infer_instance_dims(
        &mut self,
        instance_data: &mut rumoca_ir_ast::InstanceData,
    ) -> bool {
        let name = instance_data.qualified_name.to_flat_string();

        // Compute scope: parent component path for resolving relative references
        // For "combiTimeTable.table", scope is "combiTimeTable" which is empty at top level
        // For nested components, we strip the last component to get the parent scope
        let scope = crate::path_utils::parent_scope(&name).unwrap_or("");

        // Try to infer from binding first
        if let Some(ref binding) = instance_data.binding
            && let Some(dims) =
                eval::infer_dimensions_from_binding_with_scope(binding, &self.eval_ctx, scope)
        {
            instance_data.dims = dims.iter().map(|&d| d as i64).collect();
            self.eval_ctx.add_dimensions(&name, dims);
            return true;
        }

        // Fallback: try to infer from start value of a record element binding
        if let Some(ref start) = instance_data.start
            && let Some(dims) =
                eval::infer_dimensions_from_binding_with_scope(start, &self.eval_ctx, scope)
        {
            instance_data.dims = dims.iter().map(|&d| d as i64).collect();
            self.eval_ctx.add_dimensions(&name, dims);
            return true;
        }

        false
    }

    /// Validate that all array dimensions have been evaluated (MLS §10.1).
    ///
    /// This checks all primitive components that have dimension expressions
    /// (both colon `:` and explicit like `[n+1]`) and ensures they've been
    /// resolved to concrete sizes. If not, emit an error.
    ///
    /// Per MLS §10.1 / §10.3:
    /// - Input variables with colon dimensions are allowed - their size comes from connections
    /// - Non-input variables must have evaluable dimensions at translation time
    pub(crate) fn validate_dimensions(&mut self, overlay: &InstanceOverlay) {
        use rumoca_ir_ast as ast;

        for (_def_id, instance_data) in &overlay.components {
            // Only check primitives with dimension expressions
            if !instance_data.is_primitive || instance_data.dims_expr.is_empty() {
                continue;
            }

            // Check if dimensions were successfully evaluated
            if !instance_data.dims.is_empty() {
                continue;
            }

            // Skip input variables with colon dims - their size comes from connections
            let has_colon_dim = instance_data
                .dims_expr
                .iter()
                .any(|s| matches!(s, ast::Subscript::Range { .. }));
            let is_input = matches!(instance_data.causality, ast::Causality::Input(_));
            if is_input && has_colon_dim {
                continue;
            }

            let var_name = instance_data.qualified_name.to_flat_string();

            let reason = match (has_colon_dim, instance_data.binding.is_none()) {
                (true, true) => {
                    "colon dimension without binding - provide an array literal or use explicit size"
                        .to_string()
                }
                (true, false) => {
                    "colon dimension could not be inferred from binding".to_string()
                }
                (false, _) => format!(
                    "dimension expression could not be evaluated: {:?}",
                    instance_data.dims_expr
                ),
            };

            // Emit as error per MLS §10.1
            self.diagnostics.emit(
                CommonDiagnostic::error(format!(
                    "unevaluable array dimensions for '{}': {}",
                    var_name, reason
                ))
                .with_code("ET004"),
            );
        }
    }

    /// Type check a StoredDefinition.
    pub(crate) fn check_stored_definition(
        &mut self,
        def: &mut StoredDefinition,
        type_table: &mut TypeTable,
    ) {
        for (_name, class) in def.classes.iter_mut() {
            self.check_class(class, type_table);
        }
    }

    /// Type check a ClassDef.
    pub(crate) fn check_class(&mut self, class: &mut ClassDef, type_table: &mut TypeTable) {
        // Collect constants from this class for dimension evaluation
        self.eval_ctx = eval::collect_constants(class, "");

        // Resolve component types and evaluate dimensions
        for (name, comp) in class.components.iter_mut() {
            self.check_component(name, comp, type_table);
        }

        // Expose resolved component types for equation compatibility checks in this class.
        let prev_scope_types = std::mem::take(&mut self.current_component_types);
        let mut scope_types = HashMap::new();
        let class_name = class.name.text.to_string();
        for (name, comp) in &class.components {
            let Some(type_id) = comp.type_id else {
                continue;
            };
            // Store both local names (`x`) and class-qualified names (`Test.x`) so
            // equation refs remain type-checkable after name qualification.
            scope_types.insert(name.clone(), type_id);
            scope_types.insert(format!("{class_name}.{name}"), type_id);
        }
        self.current_component_types = scope_types;

        // Validate known builtin modifier value types now that local component
        // types are available in scope (e.g., `Real x(start = y)`).
        self.check_component_modifier_types_in_class(class, type_table);

        // Validate variability constraints (MLS §4.5)
        self.validate_variability_constraints(class);

        // Validate causality constraints (MLS §4.6)
        self.validate_causality_constraints(class);

        // Mark structural parameters (MLS §18.3)
        self.mark_structural_parameters(class);

        // Type check equations
        for equation in &class.equations {
            self.check_equation(equation, type_table);
        }
        for equation in &class.initial_equations {
            self.check_equation(equation, type_table);
        }

        // Type check algorithms
        for statements in &class.algorithms {
            for statement in statements {
                self.check_statement(statement, type_table);
            }
        }
        for statements in &class.initial_algorithms {
            for statement in statements {
                self.check_statement(statement, type_table);
            }
        }

        // Recursively check nested classes
        for (_name, nested) in class.classes.iter_mut() {
            self.check_class(nested, type_table);
        }

        // Restore parent class scope.
        self.current_component_types = prev_scope_types;
    }

    /// Type check a component declaration.
    pub(crate) fn check_component(
        &mut self,
        name: &str,
        comp: &mut Component,
        type_table: &mut TypeTable,
    ) {
        let type_name = comp.type_name.to_string();
        let type_id = self.resolve_type_name(&type_name, comp.type_def_id, type_table);
        if type_id.is_unknown() {
            let span = self.source_map.location_to_span(
                &comp.location.file_name,
                comp.location.start as usize,
                comp.location.end as usize,
            );
            self.diagnostics.emit(
                CommonDiagnostic::error(format!(
                    "undefined type '{}' for component '{}'",
                    type_name, name
                ))
                .with_code("ET001")
                .with_label(Label::primary(span).with_message("type declaration here")),
            );
        }
        comp.type_id = Some(type_id);

        // Evaluate shape_expr → shape (MLS §10.1)
        self.evaluate_component_dimensions(name, comp);

        // Validate modifier names for builtin and class-typed components.
        self.validate_component_modifier_names(name, comp, type_table, type_id);

        // Type check the start expression if not empty
        if !matches!(comp.start, Expression::Empty) {
            self.check_expression(&comp.start, type_table);
        }

        // Type check modification expressions
        for (_name, mod_expr) in &comp.modifications {
            self.check_expression(mod_expr, type_table);
        }
    }

    pub(crate) fn validate_component_modifier_names(
        &mut self,
        comp_name: &str,
        comp: &Component,
        type_table: &TypeTable,
        type_id: TypeId,
    ) {
        let root_type_id = self.resolve_type_root(type_table, type_id);
        let Some(ty) = type_table.get(root_type_id) else {
            return;
        };

        if matches!(ty, Type::Builtin(_)) {
            self.validate_builtin_component_modifiers(comp_name, comp, type_table, type_id);
            return;
        }

        let Type::Class(class_ty) = ty else {
            return;
        };
        let Some(allowed_roots) = self.component_modifier_targets.get(&class_ty.def_id) else {
            return;
        };
        let allowed_roots = allowed_roots.clone();

        for (modifier_name, modifier_expr) in &comp.modifications {
            let modifier_root = Self::modifier_root_name(modifier_name);
            if modifier_root.is_empty() {
                continue;
            }
            if !allowed_roots.contains(modifier_root) {
                self.emit_unknown_component_modifier(
                    comp_name,
                    comp,
                    modifier_name,
                    modifier_expr,
                    modifier_root,
                );
                continue;
            }
            self.validate_class_modifier_path(
                comp_name,
                comp,
                modifier_name,
                modifier_expr,
                class_ty.def_id,
                type_table,
            );
        }
    }

    fn validate_class_modifier_path(
        &mut self,
        comp_name: &str,
        comp: &Component,
        modifier_name: &str,
        modifier_expr: &Expression,
        class_def_id: DefId,
        type_table: &TypeTable,
    ) {
        let segments = Self::modifier_segments(modifier_name);
        if segments.len() <= 1 {
            return;
        }

        let Some(mut current_type_id) = self
            .component_modifier_member_types
            .get(&class_def_id)
            .and_then(|members| members.get(&segments[0]).copied())
        else {
            return;
        };

        for (idx, segment) in segments.iter().enumerate().skip(1) {
            let is_last = idx == segments.len() - 1;
            match self.advance_modifier_path_segment(current_type_id, segment, is_last, type_table)
            {
                ModifierPathAdvance::Next(next_type_id) => current_type_id = next_type_id,
                ModifierPathAdvance::Complete => return,
                ModifierPathAdvance::Invalid => {
                    self.emit_unknown_component_modifier(
                        comp_name,
                        comp,
                        modifier_name,
                        modifier_expr,
                        segment,
                    );
                    return;
                }
            }
        }
    }

    fn advance_modifier_path_segment(
        &self,
        current_type_id: TypeId,
        segment: &str,
        is_last: bool,
        type_table: &TypeTable,
    ) -> ModifierPathAdvance {
        if current_type_id.is_unknown() {
            return ModifierPathAdvance::Invalid;
        }
        let current_root = self.resolve_type_root(type_table, current_type_id);
        let Some(current_type) = type_table.get(current_root) else {
            return ModifierPathAdvance::Invalid;
        };

        match current_type {
            Type::Builtin(_) => {
                if is_last && Self::is_allowed_builtin_modifier(segment) {
                    ModifierPathAdvance::Complete
                } else {
                    ModifierPathAdvance::Invalid
                }
            }
            Type::Class(class_type) => self
                .component_modifier_member_types
                .get(&class_type.def_id)
                .and_then(|members| members.get(segment).copied())
                .map_or(ModifierPathAdvance::Invalid, ModifierPathAdvance::Next),
            _ => ModifierPathAdvance::Invalid,
        }
    }

    fn emit_unknown_component_modifier(
        &mut self,
        comp_name: &str,
        comp: &Component,
        modifier_name: &str,
        modifier_expr: &Expression,
        span_name: &str,
    ) {
        let span = self
            .find_modifier_name_span(comp, span_name)
            .unwrap_or_else(|| {
                let location = modifier_expr.get_location().unwrap_or(&comp.location);
                self.source_map.location_to_span(
                    &location.file_name,
                    location.start as usize,
                    location.end as usize,
                )
            });
        self.diagnostics.emit(
            CommonDiagnostic::error(format!(
                "unknown modifier `{}` for component `{}` of type `{}`",
                modifier_name, comp_name, comp.type_name
            ))
            .with_code("ET001")
            .with_label(Label::primary(span).with_message("unknown modifier")),
        );
    }

    pub(crate) fn validate_builtin_component_modifiers(
        &mut self,
        comp_name: &str,
        comp: &Component,
        type_table: &TypeTable,
        type_id: TypeId,
    ) {
        let root_type_id = self.resolve_type_root(type_table, type_id);
        let Some(ty) = type_table.get(root_type_id) else {
            return;
        };
        if !matches!(ty, Type::Builtin(_)) {
            return;
        }

        for (modifier_name, modifier_expr) in &comp.modifications {
            if Self::is_allowed_builtin_modifier(modifier_name) {
                continue;
            }
            let span = self
                .find_modifier_name_span(comp, modifier_name)
                .unwrap_or_else(|| {
                    let location = modifier_expr.get_location().unwrap_or(&comp.location);
                    self.source_map.location_to_span(
                        &location.file_name,
                        location.start as usize,
                        location.end as usize,
                    )
                });
            self.diagnostics.emit(
                CommonDiagnostic::error(format!(
                    "unknown modifier `{}` for builtin component `{}` of type `{}`",
                    modifier_name, comp_name, comp.type_name
                ))
                .with_code("ET001")
                .with_label(Label::primary(span).with_message("unknown modifier")),
            );
        }
    }

    pub(crate) fn check_component_modifier_types_in_class(
        &mut self,
        class: &ClassDef,
        type_table: &TypeTable,
    ) {
        let class_name = class.name.text.to_string();
        for (comp_name, comp) in &class.components {
            let type_id = comp.type_id.unwrap_or_else(|| {
                self.resolve_type_name(&comp.type_name.to_string(), comp.type_def_id, type_table)
            });
            if type_id.is_unknown() {
                continue;
            }
            self.current_component_types
                .entry(comp_name.clone())
                .or_insert(type_id);
            if !class_name.is_empty() {
                self.current_component_types
                    .entry(format!("{class_name}.{comp_name}"))
                    .or_insert(type_id);
            }
        }

        for (comp_name, comp) in &class.components {
            let type_id = comp.type_id.unwrap_or_else(|| {
                self.resolve_type_name(&comp.type_name.to_string(), comp.type_def_id, type_table)
            });
            self.validate_builtin_component_modifier_types(comp_name, comp, type_table, type_id);
        }
    }

    pub(crate) fn validate_builtin_component_modifier_types(
        &mut self,
        comp_name: &str,
        comp: &Component,
        type_table: &TypeTable,
        type_id: TypeId,
    ) {
        let root_type_id = self.resolve_type_root(type_table, type_id);
        let Some(Type::Builtin(builtin_type)) = type_table.get(root_type_id) else {
            return;
        };

        if comp.start_is_modification && !matches!(comp.start, Expression::Empty) {
            let Some(expected_desc) = Self::builtin_modifier_expected_type(*builtin_type, "start")
            else {
                return;
            };
            self.validate_single_builtin_modifier_type(
                comp_name,
                &comp.type_name.to_string(),
                expected_desc,
                "start",
                &comp.start,
                type_table,
            );
        }

        for (modifier_name, modifier_expr) in &comp.modifications {
            if !Self::is_allowed_builtin_modifier(modifier_name) {
                continue;
            }
            let Some(expected_desc) =
                Self::builtin_modifier_expected_type(*builtin_type, modifier_name)
            else {
                continue;
            };
            self.validate_single_builtin_modifier_type(
                comp_name,
                &comp.type_name.to_string(),
                expected_desc,
                modifier_name,
                modifier_expr,
                type_table,
            );
        }
    }

    pub(crate) fn validate_single_builtin_modifier_type(
        &mut self,
        comp_name: &str,
        comp_type_name: &str,
        expected_desc: BuiltinModifierExpectedType,
        modifier_name: &str,
        modifier_expr: &Expression,
        type_table: &TypeTable,
    ) {
        let Some(found_type) = self.infer_expression_type(modifier_expr, type_table) else {
            return;
        };
        if found_type.is_unknown() {
            return;
        }
        let found_root = self.resolve_type_root(type_table, found_type);
        if found_root.is_unknown() || Self::is_unresolved_alias_root(type_table, found_root) {
            return;
        }
        if Self::modifier_value_type_matches(expected_desc, found_root, type_table) {
            return;
        }

        let span = modifier_expr
            .get_location()
            .map(|location| {
                self.source_map.location_to_span(
                    &location.file_name,
                    location.start as usize,
                    location.end as usize,
                )
            })
            .unwrap_or_else(|| Span::DUMMY);
        let expected = Self::modifier_expected_type_name(expected_desc);
        let found = Self::format_type_name(type_table, found_type);
        self.diagnostics.emit(
            CommonDiagnostic::error(format!(
                "modifier `{}` for builtin component `{}` of type `{}` expects `{}`, found `{}`",
                modifier_name, comp_name, comp_type_name, expected, found
            ))
            .with_code("ET002")
            .with_label(Label::primary(span).with_message("modifier value here")),
        );
    }

    pub(crate) fn builtin_modifier_expected_type(
        component_builtin_type: rumoca_ir_ast::BuiltinType,
        modifier_name: &str,
    ) -> Option<BuiltinModifierExpectedType> {
        match modifier_name {
            "fixed" => Some(BuiltinModifierExpectedType::Boolean),
            "unit" | "displayUnit" | "quantity" => Some(BuiltinModifierExpectedType::String),
            "start" | "min" | "max" | "nominal" => Some(BuiltinModifierExpectedType::Component(
                component_builtin_type,
            )),
            // TODO(MLS §4.9): enforce enum/record contracts for stateSelect,
            // uncertain, and distribution when those type identities are tracked.
            _ => None,
        }
    }

    pub(crate) fn modifier_expected_type_name(expected: BuiltinModifierExpectedType) -> String {
        match expected {
            BuiltinModifierExpectedType::Boolean => "Boolean".to_string(),
            BuiltinModifierExpectedType::String => "String".to_string(),
            BuiltinModifierExpectedType::Component(component_builtin_type) => {
                component_builtin_type.name().to_string()
            }
        }
    }

    pub(crate) fn modifier_value_type_matches(
        expected: BuiltinModifierExpectedType,
        found_root_type: TypeId,
        type_table: &TypeTable,
    ) -> bool {
        let Some(found_type) = type_table.get(found_root_type) else {
            return false;
        };
        match expected {
            BuiltinModifierExpectedType::Boolean => {
                matches!(
                    found_type,
                    Type::Builtin(rumoca_ir_ast::BuiltinType::Boolean)
                )
            }
            BuiltinModifierExpectedType::String => {
                matches!(
                    found_type,
                    Type::Builtin(rumoca_ir_ast::BuiltinType::String)
                )
            }
            BuiltinModifierExpectedType::Component(component_builtin) => {
                let matches_component = matches!(
                    (component_builtin, found_type),
                    (
                        rumoca_ir_ast::BuiltinType::Real,
                        Type::Builtin(rumoca_ir_ast::BuiltinType::Real)
                    ) | (
                        rumoca_ir_ast::BuiltinType::Integer,
                        Type::Builtin(rumoca_ir_ast::BuiltinType::Integer)
                    ) | (
                        rumoca_ir_ast::BuiltinType::Boolean,
                        Type::Builtin(rumoca_ir_ast::BuiltinType::Boolean)
                    ) | (
                        rumoca_ir_ast::BuiltinType::String,
                        Type::Builtin(rumoca_ir_ast::BuiltinType::String)
                    ) | (
                        rumoca_ir_ast::BuiltinType::Clock,
                        Type::Builtin(rumoca_ir_ast::BuiltinType::Clock)
                    )
                );
                if matches_component {
                    return true;
                }
                // MLS §6.7: Integer expressions are assignment-compatible with Real.
                matches!(
                    (component_builtin, found_type),
                    (
                        rumoca_ir_ast::BuiltinType::Real,
                        Type::Builtin(rumoca_ir_ast::BuiltinType::Integer)
                    )
                )
            }
        }
    }

    pub(crate) fn is_allowed_builtin_modifier(name: &str) -> bool {
        matches!(
            name,
            "quantity"
                | "unit"
                | "displayUnit"
                | "min"
                | "max"
                | "start"
                | "fixed"
                | "nominal"
                | "stateSelect"
                | "uncertain"
                | "distribution"
        )
    }

    fn modifier_root_name(modifier_name: &str) -> &str {
        let mut bracket_depth = 0usize;
        let mut segment_end = modifier_name.len();
        for (idx, ch) in modifier_name.char_indices() {
            match ch {
                '[' => bracket_depth += 1,
                ']' => bracket_depth = bracket_depth.saturating_sub(1),
                '.' if bracket_depth == 0 => {
                    segment_end = idx;
                    break;
                }
                _ => {}
            }
        }
        let segment = &modifier_name[..segment_end];
        segment
            .split_once('[')
            .map_or(segment, |(root, _rest)| root)
    }

    fn modifier_segments(modifier_name: &str) -> Vec<String> {
        let mut segments = Vec::new();
        let mut bracket_depth = 0usize;
        let mut start = 0usize;
        for (idx, ch) in modifier_name.char_indices() {
            match ch {
                '[' => bracket_depth += 1,
                ']' => bracket_depth = bracket_depth.saturating_sub(1),
                '.' if bracket_depth == 0 => {
                    segments.push(Self::normalize_modifier_segment(&modifier_name[start..idx]));
                    start = idx + 1;
                }
                _ => {}
            }
        }
        segments.push(Self::normalize_modifier_segment(&modifier_name[start..]));
        segments
            .into_iter()
            .filter(|segment| !segment.is_empty())
            .collect()
    }

    fn normalize_modifier_segment(segment: &str) -> String {
        let trimmed = segment.trim();
        let root = trimmed
            .split_once('[')
            .map_or(trimmed, |(prefix, _rest)| prefix);
        root.trim().to_string()
    }

    /// Find a precise span for a modifier name inside a component declaration.
    ///
    /// Falls back to expression/component spans if source text or byte ranges are unavailable.
    pub(crate) fn find_modifier_name_span(
        &self,
        comp: &Component,
        modifier_name: &str,
    ) -> Option<Span> {
        if modifier_name.is_empty() {
            return None;
        }
        let source_id = self.source_map.get_id(&comp.location.file_name)?;
        let (_name, source) = self.source_map.get_source(source_id)?;
        let start = comp.location.start as usize;
        let mut end = comp.location.end as usize;
        if start >= source.len() {
            return None;
        }
        end = end.min(source.len());
        if start >= end {
            return None;
        }
        let snippet = source.get(start..end)?;
        if let Some(rel) = Self::find_modifier_identifier(snippet, modifier_name) {
            let abs_start = start + rel;
            let abs_end = abs_start + modifier_name.len();
            return Some(self.source_map.location_to_span(
                &comp.location.file_name,
                abs_start,
                abs_end,
            ));
        }

        // Some component locations end before class modifications; scan declaration lines.
        let start_line = comp
            .location
            .start_line
            .max(comp.name_token.location.start_line) as usize;
        let end_line = comp
            .location
            .end_line
            .max(comp.location.start_line)
            .max(comp.name_token.location.start_line) as usize;
        for line in start_line..=end_line {
            let Some((line_start, line_end)) = Self::line_byte_range(source, line) else {
                continue;
            };
            let line_text = &source[line_start..line_end];
            if let Some(rel) = Self::find_modifier_identifier(line_text, modifier_name) {
                let abs_start = line_start + rel;
                let abs_end = abs_start + modifier_name.len();
                return Some(self.source_map.location_to_span(
                    &comp.location.file_name,
                    abs_start,
                    abs_end,
                ));
            }
        }
        None
    }

    pub(crate) fn line_byte_range(source: &str, line_number: usize) -> Option<(usize, usize)> {
        if line_number == 0 {
            return None;
        }
        let mut current_line = 1usize;
        let mut line_start = 0usize;
        for (idx, ch) in source.char_indices() {
            if current_line == line_number && ch == '\n' {
                return Some((line_start, idx));
            }
            if ch == '\n' {
                current_line += 1;
                line_start = idx + 1;
            }
        }
        (current_line == line_number).then_some((line_start, source.len()))
    }

    pub(crate) fn find_modifier_identifier(snippet: &str, modifier_name: &str) -> Option<usize> {
        let mut offset = 0usize;
        while let Some(found) = snippet[offset..].find(modifier_name) {
            let idx = offset + found;
            if !Self::has_identifier_boundaries(snippet, idx, modifier_name.len()) {
                offset = idx + modifier_name.len();
                continue;
            }
            let after = &snippet[idx + modifier_name.len()..];
            let after_trimmed = after.trim_start();
            if after_trimmed.starts_with('=')
                || after_trimmed.starts_with('(')
                || after_trimmed.starts_with(',')
                || after_trimmed.starts_with(')')
                || after_trimmed.starts_with(';')
            {
                return Some(idx);
            }
            offset = idx + modifier_name.len();
        }
        None
    }

    pub(crate) fn has_identifier_boundaries(text: &str, start: usize, len: usize) -> bool {
        let before = text[..start].chars().next_back();
        let after = text[start + len..].chars().next();
        !before.is_some_and(Self::is_identifier_char)
            && !after.is_some_and(Self::is_identifier_char)
    }

    pub(crate) fn is_identifier_char(ch: char) -> bool {
        ch.is_ascii_alphanumeric() || ch == '_'
    }

    /// Evaluate dimension expressions for a component.
    ///
    /// Per MLS §10.1, dimension expressions must be parameter expressions
    /// that can be evaluated at translation time. This populates `comp.shape`
    /// from `comp.shape_expr`.
    pub(crate) fn evaluate_component_dimensions(&mut self, name: &str, comp: &mut Component) {
        // Skip if already has evaluated dimensions or no shape_expr
        if !comp.shape.is_empty() || comp.shape_expr.is_empty() {
            return;
        }

        // Try colon dimension inference first
        if self.try_infer_colon_dimensions(name, comp) {
            return;
        }

        // Try to evaluate each dimension explicitly
        self.try_evaluate_explicit_dimensions(name, comp);
    }

    /// Try to infer dimensions from binding for colon (`:`) dimensions.
    pub(crate) fn try_infer_colon_dimensions(&mut self, name: &str, comp: &mut Component) -> bool {
        let has_colon = comp
            .shape_expr
            .iter()
            .any(|s| matches!(s, rumoca_ir_ast::Subscript::Range { .. }));
        if !has_colon {
            return false;
        }

        let Some(binding) = &comp.binding else {
            return false;
        };
        let Some(dims) = eval::infer_dimensions_from_binding(binding, &self.eval_ctx) else {
            return false;
        };

        comp.shape = dims;
        self.eval_ctx.add_dimensions(name, comp.shape.clone());
        true
    }

    /// Try to evaluate explicit dimension expressions.
    pub(crate) fn try_evaluate_explicit_dimensions(&mut self, name: &str, comp: &mut Component) {
        let evaluated: Option<Vec<usize>> = comp
            .shape_expr
            .iter()
            .map(|sub| eval::eval_dimension(sub, &self.eval_ctx))
            .collect();

        if let Some(dims) = evaluated
            && !dims.is_empty()
        {
            comp.shape = dims;
            self.eval_ctx.add_dimensions(name, comp.shape.clone());
        }
    }

    /// Mark structural parameters (MLS §18.3).
    ///
    /// Structural parameters are those that affect array sizes, for-loop ranges,
    /// or if-equation conditions. They must be evaluable at translation time.
    pub(crate) fn mark_structural_parameters(&mut self, class: &mut ClassDef) {
        // Collect all variable references from dimension expressions
        let mut structural_refs = std::collections::HashSet::new();
        for (_name, comp) in class.components.iter() {
            for sub in &comp.shape_expr {
                structural_refs.extend(eval::collect_subscript_refs(sub));
            }
        }

        // Collect from for-loop ranges and if-equation conditions
        super::api::collect_structural_refs_from_equations(&class.equations, &mut structural_refs);
        super::api::collect_structural_refs_from_equations(
            &class.initial_equations,
            &mut structural_refs,
        );

        // Mark referenced parameters as structural
        for (name, comp) in class.components.iter_mut() {
            let is_param = matches!(comp.variability, rumoca_ir_ast::Variability::Parameter(_));
            if structural_refs.contains(name) && is_param {
                comp.is_structural = true;
            }
        }
    }

    /// Validate variability constraints (MLS §4.5).
    ///
    /// Ensures that bindings and start values respect variability ordering:
    /// constant < parameter < discrete < continuous
    pub(crate) fn validate_variability_constraints(&mut self, class: &ClassDef) {
        for (name, comp) in &class.components {
            let comp_level = eval::VariabilityLevel::from_variability(&comp.variability);

            // Check binding expression
            if let Some(binding) = &comp.binding {
                self.check_binding_variability(name, binding, comp_level, class, &comp.location);
            }

            // Check start expression (if it's a modification, not binding)
            if comp.start_is_modification && !matches!(comp.start, Expression::Empty) {
                self.check_binding_variability(
                    name,
                    &comp.start,
                    comp_level,
                    class,
                    &comp.location,
                );
            }
        }
    }

    /// Check that a binding expression respects variability constraints.
    pub(crate) fn check_binding_variability(
        &mut self,
        comp_name: &str,
        expr: &Expression,
        comp_level: eval::VariabilityLevel,
        class: &ClassDef,
        location: &rumoca_ir_ast::Location,
    ) {
        let expr_level = eval::max_variability_in_expr(expr, class);

        if expr_level > comp_level {
            // Convert location to span for proper error reporting
            let span = self.source_map.location_to_span(
                &location.file_name,
                location.start as usize,
                location.end as usize,
            );

            self.diagnostics.emit(
                CommonDiagnostic::warning(format!(
                    "variability violation: {} has {} variability but binding references {} variables (MLS §4.5)",
                    comp_name,
                    comp_level.name(),
                    expr_level.name()
                ))
                .with_code("ET004")
                .with_label(Label::primary(span).with_message("binding here")),
            );
        }
    }

    /// Validate causality constraints (MLS §4.6).
    ///
    /// Checks that input/output causality is respected:
    /// - Input variables should not have explicit binding equations
    ///   (they receive values via connections)
    /// - Output variables should be defined (checked at model level in todae)
    pub(crate) fn validate_causality_constraints(&mut self, class: &ClassDef) {
        for (name, comp) in &class.components {
            // Check if input has explicit binding
            if matches!(comp.causality, rumoca_ir_ast::Causality::Input(_))
                && comp.binding.is_some()
            {
                let span = self.source_map.location_to_span(
                    &comp.location.file_name,
                    comp.location.start as usize,
                    comp.location.end as usize,
                );

                self.diagnostics.emit(
                    CommonDiagnostic::warning(format!(
                        "input '{}' has explicit binding which may be overwritten by connection (MLS §4.6)",
                        name
                    ))
                    .with_code("ET005")
                    .with_label(Label::primary(span).with_message("input with binding")),
                );
            }
        }
    }

    /// Type check an equation.
    pub(crate) fn check_equation(&mut self, equation: &Equation, type_table: &TypeTable) {
        match equation {
            Equation::Empty => {}
            Equation::Simple { lhs, rhs } => {
                self.check_expression(lhs, type_table);
                self.check_expression(rhs, type_table);
                self.check_equation_type_compatibility(lhs, rhs, type_table);
            }
            Equation::Connect { lhs: _, rhs: _ } => {
                // Connector compatibility is validated during connection processing
            }
            Equation::For {
                indices: _,
                equations,
            } => {
                self.check_equations(equations, type_table);
            }
            Equation::When(blocks) => {
                for block in blocks {
                    self.check_equation_block(block, type_table);
                }
            }
            Equation::If {
                cond_blocks,
                else_block,
            } => {
                for block in cond_blocks {
                    self.check_equation_block(block, type_table);
                }
                if let Some(equations) = else_block {
                    self.check_equations(equations, type_table);
                }
            }
            Equation::FunctionCall { comp: _, args } => {
                self.check_expressions(args, type_table);
            }
            Equation::Assert {
                condition,
                message,
                level,
            } => {
                self.check_expression(condition, type_table);
                self.check_expression(message, type_table);
                if let Some(lvl) = level {
                    self.check_expression(lvl, type_table);
                }
            }
        }
    }

    /// Type check a list of equations.
    pub(crate) fn check_equations(&mut self, equations: &[Equation], type_table: &TypeTable) {
        for eq in equations {
            self.check_equation(eq, type_table);
        }
    }

    /// Type check an equation block (condition + equations).
    pub(crate) fn check_equation_block(
        &mut self,
        block: &rumoca_ir_ast::EquationBlock,
        type_table: &TypeTable,
    ) {
        self.check_expression(&block.cond, type_table);
        self.check_equations(&block.eqs, type_table);
    }

    /// Type check a list of expressions.
    pub(crate) fn check_expressions(&mut self, exprs: &[Expression], type_table: &TypeTable) {
        for expr in exprs {
            self.check_expression(expr, type_table);
        }
    }

    /// Type check a statement.
    pub(crate) fn check_statement(&mut self, statement: &Statement, type_table: &TypeTable) {
        match statement {
            Statement::Empty => {}
            Statement::Assignment { comp: _, value } => {
                self.check_expression(value, type_table);
            }
            Statement::FunctionCall {
                comp: _,
                args,
                outputs,
            } => {
                self.check_expressions(args, type_table);
                self.check_expressions(outputs, type_table);
            }
            Statement::Return { .. } => {}
            Statement::Break { .. } => {}
            Statement::For {
                indices: _,
                equations,
            } => {
                self.check_statements(equations, type_table);
            }
            Statement::While(block) => {
                self.check_statement_block(block, type_table);
            }
            Statement::When(blocks) => {
                for block in blocks {
                    self.check_statement_block(block, type_table);
                }
            }
            Statement::If {
                cond_blocks,
                else_block,
            } => {
                for block in cond_blocks {
                    self.check_statement_block(block, type_table);
                }
                if let Some(statements) = else_block {
                    self.check_statements(statements, type_table);
                }
            }
            Statement::Reinit { variable: _, value } => {
                self.check_expression(value, type_table);
                // reinit variable must be a continuous-time Real state (MLS §8.3.6)
                // Validated during flatten/todae where der() usage is known
            }
            Statement::Assert {
                condition,
                message,
                level,
            } => {
                self.check_expression(condition, type_table);
                self.check_expression(message, type_table);
                if let Some(lvl) = level {
                    self.check_expression(lvl, type_table);
                }
            }
        }
    }

    /// Type check a list of statements.
    pub(crate) fn check_statements(&mut self, statements: &[Statement], type_table: &TypeTable) {
        for stmt in statements {
            self.check_statement(stmt, type_table);
        }
    }

    /// Type check a statement block (condition + statements).
    pub(crate) fn check_statement_block(
        &mut self,
        block: &rumoca_ir_ast::StatementBlock,
        type_table: &TypeTable,
    ) {
        self.check_expression(&block.cond, type_table);
        self.check_statements(&block.stmts, type_table);
    }

    /// Type check an expression.
    pub(crate) fn check_expression(&mut self, expr: &Expression, _type_table: &TypeTable) {
        match expr {
            Expression::Empty => {}
            Expression::Range { start, step, end } => {
                self.check_expression(start, _type_table);
                if let Some(s) = step {
                    self.check_expression(s, _type_table);
                }
                self.check_expression(end, _type_table);
            }
            Expression::Unary { op: _, rhs } => {
                self.check_expression(rhs, _type_table);
            }
            Expression::Binary { op: _, lhs, rhs } => {
                self.check_expression(lhs, _type_table);
                self.check_expression(rhs, _type_table);
            }
            Expression::Terminal { .. } => {
                // Terminals have inherent types (literals)
            }
            Expression::ComponentReference(_) => {
                // Type comes from the referenced component
            }
            Expression::FunctionCall { comp: _, args } => {
                for arg in args {
                    self.check_expression(arg, _type_table);
                }
            }
            Expression::ClassModification {
                target: _,
                modifications,
            } => {
                for mod_expr in modifications {
                    self.check_expression(mod_expr, _type_table);
                }
            }
            Expression::NamedArgument { name: _, value } => {
                self.check_expression(value, _type_table);
            }
            Expression::Modification { target: _, value } => {
                self.check_expression(value, _type_table);
            }
            Expression::Array { elements, .. } => {
                for elem in elements {
                    self.check_expression(elem, _type_table);
                }
            }
            Expression::Tuple { elements } => {
                for elem in elements {
                    self.check_expression(elem, _type_table);
                }
            }
            Expression::If {
                branches,
                else_branch,
            } => {
                for (cond, then_expr) in branches {
                    self.check_expression(cond, _type_table);
                    self.check_expression(then_expr, _type_table);
                }
                self.check_expression(else_branch, _type_table);
            }
            Expression::Parenthesized { inner } => {
                self.check_expression(inner, _type_table);
            }
            Expression::ArrayComprehension {
                expr,
                indices: _,
                filter,
            } => {
                self.check_expression(expr, _type_table);
                if let Some(f) = filter {
                    self.check_expression(f, _type_table);
                }
            }
            Expression::ArrayIndex { base, subscripts } => {
                self.check_expression(base, _type_table);
                self.check_subscripts(subscripts, _type_table);
            }
            Expression::FieldAccess { base, .. } => {
                self.check_expression(base, _type_table);
            }
        }
    }

    /// Type check subscript expressions.
    pub(crate) fn check_subscripts(
        &mut self,
        subscripts: &[rumoca_ir_ast::Subscript],
        type_table: &TypeTable,
    ) {
        for sub in subscripts {
            if let rumoca_ir_ast::Subscript::Expression(e) = sub {
                self.check_expression(e, type_table);
            }
        }
    }

    /// Check assignment compatibility for a simple equation.
    ///
    /// This currently targets scalar/component identity checks needed for
    /// user-defined type mismatch diagnostics in equations.
    pub(crate) fn check_equation_type_compatibility(
        &mut self,
        lhs: &Expression,
        rhs: &Expression,
        type_table: &TypeTable,
    ) {
        let lhs_ty = self.infer_expression_type(lhs, type_table);
        let rhs_ty = self.infer_expression_type(rhs, type_table);

        let (Some(lhs_ty), Some(rhs_ty)) = (lhs_ty, rhs_ty) else {
            return;
        };
        if lhs_ty.is_unknown() || rhs_ty.is_unknown() {
            return;
        }
        let lhs_root = self.resolve_type_root(type_table, lhs_ty);
        let rhs_root = self.resolve_type_root(type_table, rhs_ty);
        if lhs_root == rhs_root {
            return;
        }
        if Self::is_unresolved_alias_root(type_table, lhs_root)
            || Self::is_unresolved_alias_root(type_table, rhs_root)
        {
            return;
        }
        if !Self::is_user_defined_compatibility_type(type_table, lhs_root)
            || !Self::is_user_defined_compatibility_type(type_table, rhs_root)
        {
            return;
        }

        let span = lhs
            .get_location()
            .map(|loc| {
                self.source_map.location_to_span(
                    &loc.file_name,
                    loc.start as usize,
                    loc.end as usize,
                )
            })
            .unwrap_or(Span::DUMMY);
        let expected = Self::format_type_name(type_table, lhs_ty);
        let found = Self::format_type_name(type_table, rhs_ty);
        self.diagnostics.emit(
            CommonDiagnostic::error(format!(
                "type mismatch: expected `{expected}`, found `{found}`"
            ))
            .with_code("ET002")
            .with_label(Label::primary(span).with_message("equation assignment here")),
        );
    }

    pub(crate) fn infer_expression_type(
        &self,
        expr: &Expression,
        type_table: &TypeTable,
    ) -> Option<TypeId> {
        match expr {
            Expression::Terminal { terminal_type, .. } => match terminal_type {
                rumoca_ir_ast::TerminalType::UnsignedReal => Some(type_table.real()),
                rumoca_ir_ast::TerminalType::UnsignedInteger => Some(type_table.integer()),
                rumoca_ir_ast::TerminalType::Bool => Some(type_table.boolean()),
                rumoca_ir_ast::TerminalType::String => Some(type_table.string()),
                _ => None,
            },
            Expression::ComponentReference(cr) => self.infer_component_ref_type(cr, type_table),
            Expression::FunctionCall { comp, .. } => {
                self.infer_function_call_result_type(comp, type_table)
            }
            Expression::Parenthesized { inner } => self.infer_expression_type(inner, type_table),
            _ => None,
        }
    }

    pub(crate) fn infer_function_call_result_type(
        &self,
        comp: &rumoca_ir_ast::ComponentReference,
        type_table: &TypeTable,
    ) -> Option<TypeId> {
        // Record constructors use call syntax (`Payload(...)`) but semantically
        // evaluate to the record type. Resolve through the type table so
        // equation compatibility checks can reject mismatched record identities.
        let dotted_name = Self::component_ref_name(comp);
        let type_id = self.resolve_type_name(&dotted_name, comp.def_id, type_table);
        if type_id.is_unknown() {
            return None;
        }
        match type_table.get(type_id) {
            Some(Type::Class(class_ty)) if class_ty.kind == ClassKind::Record => Some(type_id),
            Some(Type::Alias(_)) | Some(Type::Enumeration(_)) => Some(type_id),
            _ => None,
        }
    }

    pub(crate) fn component_ref_name(cr: &rumoca_ir_ast::ComponentReference) -> String {
        cr.parts
            .iter()
            .map(|part| part.ident.text.as_ref())
            .collect::<Vec<_>>()
            .join(".")
    }

    pub(crate) fn infer_component_ref_type(
        &self,
        cr: &rumoca_ir_ast::ComponentReference,
        type_table: &TypeTable,
    ) -> Option<TypeId> {
        if cr.parts.len() == 1 {
            let name = cr.parts[0].ident.text.as_ref();
            return self
                .current_component_types
                .get(name)
                .copied()
                .and_then(|ty| Self::filter_non_value_component_type(type_table, ty));
        }

        // Qualified component refs can include leading class/package names.
        // Try longest dotted suffixes first (excluding single-name suffixes).
        let parts: Vec<&str> = cr.parts.iter().map(|p| p.ident.text.as_ref()).collect();
        if parts.len() > 1 {
            let qualified_match = (0..(parts.len() - 1)).find_map(|start| {
                let candidate = parts[start..].join(".");
                self.current_component_types
                    .get(&candidate)
                    .copied()
                    .and_then(|ty| Self::filter_non_value_component_type(type_table, ty))
            });
            if let Some(type_id) = qualified_match {
                return Some(type_id);
            }
        }

        // Enum literal: <EnumType>.<Literal> or <Pkg>.<EnumType>.<Literal>.
        for i in (1..parts.len()).rev() {
            let candidate = parts[..i].join(".");
            let Some(type_id) = type_table.lookup(&candidate) else {
                continue;
            };
            if matches!(type_table.get(type_id), Some(Type::Enumeration(_))) {
                return Some(type_id);
            }
        }
        None
    }

    pub(crate) fn filter_non_value_component_type(
        type_table: &TypeTable,
        ty: TypeId,
    ) -> Option<TypeId> {
        match type_table.get(ty) {
            Some(Type::Class(class_ty)) if class_ty.kind == ClassKind::Package => None,
            _ => Some(ty),
        }
    }

    pub(crate) fn is_user_defined_compatibility_type(type_table: &TypeTable, ty: TypeId) -> bool {
        match type_table.get(ty) {
            Some(Type::Enumeration(_)) => true,
            Some(Type::Class(class_ty)) => class_ty.kind == ClassKind::Record,
            _ => false,
        }
    }

    pub(crate) fn is_unresolved_alias_root(type_table: &TypeTable, ty: TypeId) -> bool {
        matches!(
            type_table.get(ty),
            Some(Type::Alias(alias)) if alias.aliased.is_unknown() || alias.aliased == ty
        )
    }

    pub(crate) fn resolve_alias_root(type_table: &TypeTable, mut ty: TypeId) -> TypeId {
        const MAX_DEPTH: usize = 16;
        for _ in 0..MAX_DEPTH {
            let Some(Type::Alias(alias)) = type_table.get(ty) else {
                return ty;
            };
            if alias.aliased.is_unknown() || alias.aliased == ty {
                return ty;
            }
            ty = alias.aliased;
        }
        ty
    }

    pub(crate) fn resolve_type_root(&self, type_table: &TypeTable, ty: TypeId) -> TypeId {
        self.type_roots
            .get(&ty)
            .copied()
            .unwrap_or_else(|| Self::resolve_alias_root(type_table, ty))
    }

    pub(crate) fn format_type_name(type_table: &TypeTable, type_id: TypeId) -> String {
        type_table
            .get(type_id)
            .and_then(|ty| ty.name().map(ToOwned::to_owned))
            .unwrap_or_else(|| format!("{type_id:?}"))
    }

    /// Resolve a type name to a TypeId.
    pub(crate) fn resolve_type_name(
        &self,
        name: &str,
        type_def_id: Option<DefId>,
        type_table: &TypeTable,
    ) -> TypeId {
        // Prefer DefId-based resolution for user-defined types.
        if let Some(type_id) = type_def_id
            .and_then(|def_id| self.resolve_type_from_def_anchor(def_id, name, type_table))
        {
            return type_id;
        }

        // Fall back to direct name lookup (builtins and fully-qualified names).
        if let Some(type_id) = type_table.lookup(name) {
            return type_id;
        }

        // Fall back to a unique dotted-suffix match.
        // This supports imported names like `SI.Reluctance` or `StateSelect`
        // when the type table stores canonical qualified names.
        if let Some(type_id) = self.type_suffix_index.get(name).copied().flatten() {
            return type_id;
        }

        // Last resort: unique short-name lookup.
        // Keep this as a compatibility fallback for mixed qualification styles.
        let short_name = crate::path_utils::top_level_last_segment(name);
        if let Some(type_id) = self.type_suffix_index.get(short_name).copied().flatten() {
            return type_id;
        }

        TypeId::UNKNOWN
    }

    pub(crate) fn resolve_type_from_def_anchor(
        &self,
        def_id: DefId,
        name: &str,
        type_table: &TypeTable,
    ) -> Option<TypeId> {
        if crate::path_utils::has_top_level_dot(name)
            && let Some(type_id) = self.resolve_dotted_type_from_anchor(def_id, name, type_table)
        {
            return Some(type_id);
        }
        // If we only have a first-segment anchor (e.g. `Medium`), keep
        // the anchor type instead of failing hard to UNKNOWN. Later checks
        // treat package/class anchors conservatively.
        self.type_ids_by_def_id.get(&def_id).copied()
    }

    pub(crate) fn resolve_dotted_type_from_anchor(
        &self,
        anchor_def_id: DefId,
        dotted_name: &str,
        type_table: &TypeTable,
    ) -> Option<TypeId> {
        let (_, tail) = dotted_name.split_once('.')?;
        let anchor_qname = self.def_qualified_names.get(&anchor_def_id)?;
        let candidate = format!("{anchor_qname}.{tail}");
        type_table.lookup(&candidate).or_else(|| {
            self.type_suffix_index
                .get(candidate.as_str())
                .copied()
                .flatten()
        })
    }

    /// Check if type checking produced any errors.
    pub fn has_errors(&self) -> bool {
        self.diagnostics.has_errors()
    }

    /// Get the collected diagnostics.
    pub fn diagnostics(&self) -> &Diagnostics {
        &self.diagnostics
    }

    /// Take the diagnostics (consuming them).
    pub fn take_diagnostics(self) -> Diagnostics {
        self.diagnostics
    }
}
