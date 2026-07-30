//! Symbolic component dimension recovery: resolving declared `dims_expr`
//! subscripts against evaluated parameters, enumeration ranges, and binding
//! shapes so flat variables carry concrete dimensions (MLS §10.1).

use super::*;

impl Context {
    pub(crate) fn recompute_symbolic_component_dimensions(
        &mut self,
        flat: &mut Model,
        overlay: &InstanceOverlay,
        tree: &ClassTree,
    ) -> Result<bool, FlattenError> {
        let mut changed = false;
        for instance_data in overlay.components.values() {
            if !instance_data.is_primitive || instance_data.dims_expr.is_empty() {
                continue;
            }
            let var_name = qualified_to_var_name(&instance_data.qualified_name);
            let Some(flat_var) = flat.variables.get(&var_name) else {
                continue;
            };
            let span = instance_source_span(instance_data, tree)?;
            let resolved_dims = self.resolve_component_dims_expr(
                var_name.as_str(),
                &instance_data.dims_expr,
                flat_var,
                tree,
                span,
            )?;
            let inherited_dims = unexpanded_structured_parent_dims(instance_data, overlay);
            let resolved_dims = normalize_inferred_dims_for_parent(&resolved_dims, &inherited_dims);
            let Some(flat_var) = flat.variables.get_mut(&var_name) else {
                continue;
            };
            if flat_var.dims != resolved_dims {
                flat_var.dims.clone_from(&resolved_dims);
                changed = true;
            }
            if self.array_dimensions.get(var_name.as_str()) != Some(&resolved_dims) {
                self.array_dimensions
                    .insert(var_name.to_string(), resolved_dims);
                changed = true;
            }
        }
        Ok(changed)
    }

    fn resolve_component_dims_expr(
        &self,
        var_name: &str,
        dims_expr: &[ast::Subscript],
        flat_var: &flat::Variable,
        tree: &ClassTree,
        span: rumoca_core::Span,
    ) -> Result<Vec<i64>, FlattenError> {
        let mut dims = Vec::with_capacity(dims_expr.len());
        for (index, subscript) in dims_expr.iter().enumerate() {
            let dim = match subscript {
                ast::Subscript::Expression(_) => {
                    self.eval_component_dim_subscript(var_name, subscript, tree, span)?
                }
                ast::Subscript::Range { .. } | ast::Subscript::Empty => {
                    self.resolve_colon_component_dimension(var_name, flat_var, index, tree, span)?
                }
            };
            dims.push(dim);
        }
        Ok(dims)
    }

    fn resolve_colon_component_dimension(
        &self,
        var_name: &str,
        flat_var: &flat::Variable,
        index: usize,
        tree: &ClassTree,
        span: rumoca_core::Span,
    ) -> Result<i64, FlattenError> {
        let inferred_dims = flat_var
            .binding
            .as_ref()
            .and_then(|binding| self.infer_binding_dimensions(var_name, binding, tree));
        let resolved_dims = best_dims(self.array_dimensions.get(var_name), inferred_dims.as_ref());

        if let Some(dim) = resolved_dims
            .as_ref()
            .and_then(|dims| dims.get(index).copied())
            .filter(|dim| *dim >= 0)
        {
            return Ok(dim);
        }

        if (flat_var.dims.len() > 1 || flat_var.dims.iter().any(|dim| *dim > 1))
            && let Some(dim) = flat_var.dims.get(index).copied().filter(|dim| *dim >= 0)
        {
            return Ok(dim);
        }

        let Some(dim) = resolved_dims.and_then(|dims| dims.get(index).copied()) else {
            return Err(FlattenError::unresolved_component_dimension(
                var_name,
                ":".to_string(),
                span,
            ));
        };
        if dim < 0 {
            return Err(FlattenError::unresolved_component_dimension(
                var_name,
                ":".to_string(),
                span,
            ));
        }
        Ok(dim)
    }

    fn infer_binding_dimensions(
        &self,
        var_name: &str,
        binding: &Expression,
        tree: &ClassTree,
    ) -> Option<Vec<i64>> {
        infer_enum_range_dimensions(binding, tree).or_else(|| {
            infer_array_dimensions_full_with_functions(
                binding,
                &ParamEvalContext::new(
                    &self.parameter_values,
                    &self.real_parameter_values,
                    &self.boolean_parameter_values,
                    &self.enum_parameter_values,
                    &self.array_dimensions,
                    &self.functions,
                    Some(var_name),
                ),
            )
        })
    }

    fn eval_component_dim_subscript(
        &self,
        var_name: &str,
        subscript: &ast::Subscript,
        tree: &ClassTree,
        span: rumoca_core::Span,
    ) -> Result<i64, FlattenError> {
        let ast::Subscript::Expression(expr) = subscript else {
            return Err(FlattenError::unresolved_component_dimension(
                var_name,
                subscript.to_string(),
                span,
            ));
        };
        if let Some(dim) = enum_type_dimension(expr, tree) {
            return Ok(dim);
        }
        let lowered = crate::ast_lower::expression_from_ast_with_intrinsics(
            expr,
            crate::ast_lower::PredefinedIntrinsicIds::from_tree(tree),
        )?;
        let eval_ctx = ParamEvalContext {
            known_ints: &self.parameter_values,
            known_reals: &self.real_parameter_values,
            known_bools: &self.boolean_parameter_values,
            known_enums: &self.enum_parameter_values,
            array_dims: &self.array_dimensions,
            functions: &self.functions,
            var_context: Some(var_name),
        };
        let Some(dim) = try_eval_integer_with_context(&lowered, &eval_ctx) else {
            return Err(FlattenError::unresolved_component_dimension(
                var_name,
                expr.to_string(),
                span,
            ));
        };
        if dim < 0 {
            return Err(FlattenError::unresolved_component_dimension(
                var_name,
                expr.to_string(),
                span,
            ));
        }
        Ok(dim)
    }
}

fn unexpanded_structured_parent_dims(
    instance: &ast::InstanceData,
    overlay: &ast::InstanceOverlay,
) -> Vec<i64> {
    let mut parents = overlay
        .components
        .values()
        .filter(|candidate| !candidate.is_primitive && !candidate.dims.is_empty())
        .filter(|candidate| {
            candidate.qualified_name.parts.len() < instance.qualified_name.parts.len()
                && instance
                    .qualified_name
                    .parts
                    .starts_with(&candidate.qualified_name.parts)
        })
        .collect::<Vec<_>>();
    parents.sort_by_key(|candidate| candidate.qualified_name.parts.len());
    parents
        .into_iter()
        .flat_map(|candidate| candidate.dims.iter().copied())
        .collect()
}

fn instance_source_span(
    instance_data: &rumoca_ir_ast::InstanceData,
    tree: &rumoca_ir_ast::ClassTree,
) -> Result<rumoca_core::Span, FlattenError> {
    let location = &instance_data.source_location;
    if !location.has_source() {
        return Err(FlattenError::missing_source_context(
            "symbolic component dimensions are missing a non-empty source location",
        ));
    }
    tree.source_map
        .try_span(
            location.source,
            location.start as usize,
            location.end as usize,
        )
        .ok_or_else(|| {
            let file_name = tree
                .source_map
                .name(location.source)
                .unwrap_or(crate::source_spans::UNKNOWN_SOURCE_DISPLAY_NAME);
            FlattenError::missing_source_context(format!(
                "source file `{file_name}` for symbolic component dimensions was not found"
            ))
        })
}
