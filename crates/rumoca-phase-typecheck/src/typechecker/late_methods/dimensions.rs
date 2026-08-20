//! Array-dimension evaluation, inference, and validation for the late
//! typecheck pass (MLS §10.1): explicit and colon extents on instanced
//! components, parameter re-evaluation feeding those extents, and the
//! declared-dimension validity checks.

use super::*;

impl TypeChecker {
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

            let name_path = Self::instance_component_path(&instance_data.qualified_name);
            let name = name_path.to_flat_string();
            let scope = name_path.parent().unwrap_or_else(ComponentPath::root);

            // Try to evaluate each dimension expression using scope-aware lookup
            let evaluated: Option<Vec<i64>> = instance_data
                .dims_expr
                .iter()
                .map(|sub| self.eval_dimension_with_fallback(sub, &scope, &type_scope_hints))
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
        instance_scope: &ComponentPath,
        type_scope_hints: &HashMap<ComponentPath, Vec<String>>,
    ) -> Option<i64> {
        let instance_scope_name = instance_scope.to_flat_string();
        rumoca_eval_ast::eval::eval_dimension_with_scope(sub, &self.eval_ctx, &instance_scope_name)
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

    /// Build component-name -> ordered type-scope hints for dimension lookup fallback.
    ///
    /// For component `state1` of type `Medium.ThermodynamicState`, this records:
    /// `state1` -> [`Medium.ThermodynamicState`, `Medium`]. Nested fields like
    /// `state1.X` can then resolve dimension symbols through the type scopes
    /// when instance-scope lookup is insufficient.
    pub(crate) fn build_type_scope_hints(
        overlay: &InstanceOverlay,
    ) -> HashMap<ComponentPath, Vec<String>> {
        let mut hints = HashMap::new();
        let component_scopes: HashMap<ComponentPath, &rumoca_ir_ast::InstanceData> = overlay
            .components
            .values()
            .map(|data| (Self::instance_component_path(&data.qualified_name), data))
            .collect();
        for (_def_id, instance_data) in &overlay.components {
            if instance_data.type_name.is_empty() {
                continue;
            }
            let component_name = Self::instance_component_path(&instance_data.qualified_name);
            let mut scopes = Vec::with_capacity(2 + instance_data.class_overrides.len());
            scopes.push(instance_data.type_name.clone());
            if let Some(enclosing) = Self::parent_type_scope(&instance_data.type_name) {
                scopes.push(enclosing.to_string());
            }
            Self::push_enclosing_class_override_scopes(
                &component_name,
                &component_scopes,
                &mut scopes,
            );
            hints.insert(component_name, scopes);
        }
        hints
    }

    fn push_enclosing_class_override_scopes(
        component_name: &ComponentPath,
        component_scopes: &HashMap<ComponentPath, &rumoca_ir_ast::InstanceData>,
        scopes: &mut Vec<String>,
    ) {
        let mut current = component_name.parent();
        while let Some(scope_path) = current {
            if let Some(scope_data) = component_scopes.get(&scope_path) {
                Self::push_class_override_scopes(&scope_path, scope_data, scopes);
            }
            current = scope_path.parent();
        }
    }

    fn push_class_override_scopes(
        scope_path: &ComponentPath,
        scope_data: &rumoca_ir_ast::InstanceData,
        scopes: &mut Vec<String>,
    ) {
        if scope_data.class_overrides.is_empty() {
            return;
        }
        let scope_name = scope_path.to_flat_string();
        for class_override in scope_data.class_overrides.values() {
            let alias_scope = format!("{}.{}", scope_name, class_override.alias);
            if !scopes.iter().any(|scope| scope == &alias_scope) {
                scopes.push(alias_scope);
            }
        }
    }

    fn parent_type_scope(type_name: &str) -> Option<&str> {
        crate::path_utils::enclosing_scope_str(type_name).filter(|scope| !scope.is_empty())
    }

    /// Fallback dimension evaluation using enclosing component type scopes.
    ///
    /// Walks up `instance_scope` ancestors and, for the first component with a
    /// known type scope, tries evaluating the subscript in that type scope.
    pub(crate) fn eval_dimension_with_type_scope_fallback(
        sub: &rumoca_ir_ast::Subscript,
        instance_scope: &ComponentPath,
        type_scope_hints: &HashMap<ComponentPath, Vec<String>>,
        ctx: &rumoca_eval_ast::eval::TypeCheckEvalContext,
    ) -> Option<usize> {
        if instance_scope.is_root() {
            return None;
        }
        let mut current = instance_scope.clone();
        loop {
            if let Some(type_scopes) = type_scope_hints.get(&current)
                && let Some(value) = Self::eval_dimension_from_type_scopes(sub, ctx, type_scopes)
            {
                return Some(value);
            }
            if let Some(enclosing) = current.parent() {
                current = enclosing;
            } else {
                break;
            }
        }
        None
    }

    fn eval_dimension_from_type_scopes(
        sub: &rumoca_ir_ast::Subscript,
        ctx: &rumoca_eval_ast::eval::TypeCheckEvalContext,
        type_scopes: &[String],
    ) -> Option<usize> {
        type_scopes
            .iter()
            .find_map(|scope| rumoca_eval_ast::eval::eval_dimension_with_scope(sub, ctx, scope))
    }

    /// Re-evaluate integer parameters that may depend on size() of arrays.
    ///
    /// Returns true if any integer value was added or corrected.
    pub(crate) fn reevaluate_integer_parameters(&mut self, overlay: &InstanceOverlay) -> bool {
        let mut progress = false;

        for (_def_id, instance_data) in &overlay.components {
            let name_path = Self::instance_component_path(&instance_data.qualified_name);
            let name = name_path.to_flat_string();
            let binding_scope = Self::instance_binding_scope_name(instance_data);
            let start_scope = Self::instance_attribute_scope_name(instance_data, "start");

            // Recompute from the most specific declaration source each pass.
            // MLS §10.1 dependency chains can reveal better values in later passes;
            // keep updating until reaching a fixed point instead of keeping stale
            // early values from fallback scope resolution.
            let mut computed = None;
            if let Some(binding) = instance_data.binding.as_ref() {
                computed = rumoca_eval_ast::eval::eval_integer_with_scope(
                    binding,
                    &self.eval_ctx,
                    &binding_scope,
                );
            }
            if computed.is_none()
                && let Some(start) = instance_data.start.as_ref()
            {
                computed = rumoca_eval_ast::eval::eval_integer_with_scope(
                    start,
                    &self.eval_ctx,
                    &start_scope,
                );
            }
            if let Some(value) = computed
                && self.eval_ctx.get_integer(&name) != Some(value)
            {
                self.eval_ctx.add_integer(&name, value);
                progress = true;
            }
        }

        progress
    }

    /// Re-evaluate boolean, real, and enum parameters that may now be computable.
    ///
    /// Returns true if any new values were computed. Boolean and enum values
    /// enable if-expression evaluation for dimension inference. Real values
    /// enable `integer(realParam)` evaluation.
    pub(crate) fn reevaluate_boolean_real_and_enum_parameters(
        &mut self,
        overlay: &InstanceOverlay,
    ) -> bool {
        let mut progress = false;

        for (_def_id, instance_data) in &overlay.components {
            let name_path = Self::instance_component_path(&instance_data.qualified_name);
            let name = name_path.to_flat_string();

            if !self.eval_ctx.booleans.contains_key(&name) {
                progress |= self.try_eval_boolean(instance_data, &name);
            }

            if !self.eval_ctx.reals.contains_key(&name) {
                progress |= self.try_eval_real(instance_data, &name);
            }

            if !self.eval_ctx.enums.contains_key(&name) {
                progress |= self.try_eval_enum(instance_data, &name);
            }
        }

        progress
    }

    /// Try to evaluate a boolean value from binding or start.
    pub(crate) fn try_eval_boolean(
        &mut self,
        data: &rumoca_ir_ast::InstanceData,
        name: &str,
    ) -> bool {
        let binding_scope = Self::instance_binding_scope_name(data);
        let start_scope = Self::instance_attribute_scope_name(data, "start");
        let binding_val = data.binding.as_ref().and_then(|b| {
            rumoca_eval_ast::eval::eval_boolean_with_scope(b, &self.eval_ctx, &binding_scope)
        });
        let start_val = data.start.as_ref().and_then(|s| {
            rumoca_eval_ast::eval::eval_boolean_with_scope(s, &self.eval_ctx, &start_scope)
        });
        if let Some(value) = binding_val.or(start_val) {
            self.eval_ctx.booleans.insert(name.to_string(), value);
            return true;
        }
        false
    }

    /// Try to evaluate a real value from binding or start.
    pub(crate) fn try_eval_real(&mut self, data: &rumoca_ir_ast::InstanceData, name: &str) -> bool {
        let binding_scope = Self::instance_binding_scope_name(data);
        let start_scope = Self::instance_attribute_scope_name(data, "start");
        let binding_val = data.binding.as_ref().and_then(|b| {
            rumoca_eval_ast::eval::eval_real_with_scope(b, &self.eval_ctx, &binding_scope)
        });
        let start_val = data.start.as_ref().and_then(|s| {
            rumoca_eval_ast::eval::eval_real_with_scope(s, &self.eval_ctx, &start_scope)
        });
        if let Some(value) = binding_val.or(start_val) {
            self.eval_ctx.reals.insert(name.to_string(), value);
            return true;
        }
        false
    }

    /// Try to evaluate an enum value from binding or start.
    pub(crate) fn try_eval_enum(&mut self, data: &rumoca_ir_ast::InstanceData, name: &str) -> bool {
        let binding_scope = Self::instance_binding_scope_name(data);
        let start_scope = Self::instance_attribute_scope_name(data, "start");
        let binding_val = data.binding.as_ref().and_then(|b| {
            rumoca_eval_ast::eval::eval_enum_with_scope(b, &self.eval_ctx, &binding_scope)
        });
        let start_val = data.start.as_ref().and_then(|s| {
            rumoca_eval_ast::eval::eval_enum_with_scope(s, &self.eval_ctx, &start_scope)
        });
        if let Some(value) = binding_val.or(start_val) {
            self.eval_ctx.enums.insert(name.to_string(), value);
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
        let name_path = Self::instance_component_path(&instance_data.qualified_name);
        let name = name_path.to_flat_string();
        let binding_scope = Self::instance_binding_scope_name(instance_data);
        let start_scope = Self::instance_attribute_scope_name(instance_data, "start");

        // Try to infer from binding first
        if let Some(ref binding) = instance_data.binding
            && let Some(dims) = rumoca_eval_ast::eval::infer_dimensions_from_binding_with_scope(
                binding,
                &self.eval_ctx,
                &binding_scope,
            )
        {
            return Self::apply_inferred_instance_dims(
                instance_data,
                &mut self.eval_ctx,
                &name,
                dims,
            );
        }

        // Fallback: try to infer from start value of a record element binding
        if let Some(ref start) = instance_data.start
            && let Some(dims) = rumoca_eval_ast::eval::infer_dimensions_from_binding_with_scope(
                start,
                &self.eval_ctx,
                &start_scope,
            )
        {
            return Self::apply_inferred_instance_dims(
                instance_data,
                &mut self.eval_ctx,
                &name,
                dims,
            );
        }

        false
    }

    fn apply_inferred_instance_dims(
        instance_data: &mut rumoca_ir_ast::InstanceData,
        eval_ctx: &mut rumoca_eval_ast::eval::TypeCheckEvalContext,
        name: &str,
        dims: Vec<usize>,
    ) -> bool {
        let inferred: Vec<i64> = dims.iter().map(|&d| d as i64).collect();
        let dims_changed = instance_data.dims != inferred;
        let ctx_changed = eval_ctx.get_dimensions(name) != Some(&dims);
        if dims_changed {
            instance_data.dims = inferred;
        }
        if ctx_changed {
            eval_ctx.add_dimensions(name, dims);
        }
        dims_changed || ctx_changed
    }

    /// The first dimension expression that evaluates to a negative value,
    /// if any. Distinguishes "evaluated but invalid" from "unevaluable" for
    /// the MLS §10.1 dimension diagnostic.
    fn negative_dimension_value(&self, instance_data: &rumoca_ir_ast::InstanceData) -> Option<i64> {
        let scope = Self::instance_component_path(&instance_data.qualified_name)
            .parent()
            .unwrap_or_else(ComponentPath::root)
            .to_flat_string();
        instance_data.dims_expr.iter().find_map(|sub| {
            let rumoca_ir_ast::Subscript::Expression(expr) = sub else {
                return None;
            };
            rumoca_eval_ast::eval::eval_integer_with_scope(expr, &self.eval_ctx, &scope)
                .filter(|value| *value < 0)
        })
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
        use rumoca_core::Variability;
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
            let is_input = matches!(instance_data.causality, rumoca_core::Causality::Input(_));
            if is_input && has_colon_dim {
                continue;
            }

            // MLS §10.1 allows `[:]` dimensions to remain unspecified until a
            // binding (or enclosing configuration) determines concrete size.
            // For parameters/constants this often happens at instantiation time
            // through record constructor modifications.
            let is_structural_parameter_like = matches!(
                instance_data.variability,
                Variability::Parameter(_) | Variability::Constant(_)
            );
            if has_colon_dim && is_structural_parameter_like {
                continue;
            }

            let var_name = instance_data.qualified_name.to_flat_string();
            let base_reason = match (has_colon_dim, instance_data.binding.is_none()) {
                (true, true) => {
                    "colon dimension without binding - provide an array literal or use explicit size"
                        .to_string()
                }
                (true, false) => {
                    "colon dimension could not be inferred from binding".to_string()
                }
                // A dimension that evaluates to a negative value must be
                // diagnosed as invalid, not as "could not be evaluated" —
                // the wrong diagnosis sends the user hunting an evaluation
                // problem that doesn't exist.
                (false, _) => match self.negative_dimension_value(instance_data) {
                    Some(value) => format!(
                        "dimension expression evaluates to {value}, but array \
                         dimensions must be non-negative (MLS §10.1)"
                    ),
                    None => format!(
                        "dimension expression could not be evaluated: {:?}",
                        instance_data.dims_expr
                    ),
                },
            };
            let invalid_value =
                !has_colon_dim && self.negative_dimension_value(instance_data).is_some();
            let reason = base_reason;

            // Emit as error per MLS §10.1.
            let Some(span) =
                self.diagnostic_location_span(&instance_data.source_location, "array dimensions")
            else {
                continue;
            };
            let headline = if invalid_value {
                "invalid array dimensions"
            } else {
                "unevaluable array dimensions"
            };
            self.emit_typecheck_error(TypeCheckError::phase_diagnostic(
                "ET004",
                format!("{headline} for '{var_name}': {reason}"),
                "array dimension declaration",
                span,
            ));
        }
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
        let Some(dims) =
            rumoca_eval_ast::eval::infer_dimensions_from_binding(binding, &self.eval_ctx)
        else {
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
            .map(|sub| rumoca_eval_ast::eval::eval_dimension(sub, &self.eval_ctx))
            .collect();

        if let Some(dims) = evaluated
            && !dims.is_empty()
        {
            comp.shape = dims;
            self.eval_ctx.add_dimensions(name, comp.shape.clone());
        }
    }
}
