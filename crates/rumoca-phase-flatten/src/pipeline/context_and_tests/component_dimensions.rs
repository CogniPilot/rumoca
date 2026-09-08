//! Discharge of deferred colon dimensions from binding shape (MLS §10.1).
//!
//! Typecheck resolves explicit-only declarations, while this bridge validates
//! explicit axes inside mixed `[explicit, :]` declarations and discharges the
//! remaining colon axes. Local declaration axes and structured-parent prefixes
//! stay distinct until the final construction.

use super::*;

#[derive(Debug, Clone, PartialEq, Eq)]
struct LocalAxes(Vec<i64>);

impl LocalAxes {
    fn from_local(values: Vec<i64>) -> Self {
        Self(values)
    }

    fn from_cached(
        values: &[i64],
        parent: &[i64],
        expected_local_rank: usize,
        var_name: &str,
    ) -> Result<Self, FlattenError> {
        if parent.is_empty() || values.len() == expected_local_rank {
            return Ok(Self(values.to_vec()));
        }
        if values.len() == parent.len() + expected_local_rank {
            if values[..parent.len()] != *parent {
                return Err(FlattenError::internal(format!(
                    "cached dimensions for `{var_name}` do not carry the issued parent prefix"
                )));
            }
            return Ok(Self(values[parent.len()..].to_vec()));
        }
        Ok(Self(values.to_vec()))
    }

    fn values(&self) -> &[i64] {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct PrefixedDims {
    parent: Vec<i64>,
    local: LocalAxes,
}

impl PrefixedDims {
    fn from_parts(parent: &[i64], local: LocalAxes) -> Self {
        Self {
            parent: parent.to_vec(),
            local,
        }
    }

    fn into_values(self) -> Vec<i64> {
        let mut values = Vec::with_capacity(self.parent.len() + self.local.0.len());
        values.extend(self.parent);
        values.extend(self.local.0);
        values
    }
}

impl Context {
    pub(crate) fn discharge_deferred_colon_dimensions(
        &mut self,
        flat: &mut Model,
        overlay: &InstanceOverlay,
        tree: &ClassTree,
    ) -> Result<bool, FlattenError> {
        let mut changed = false;
        for instance_data in overlay.components.values() {
            if !instance_data.is_primitive
                || !instance_data
                    .dims_expr
                    .iter()
                    .any(|subscript| matches!(subscript, ast::Subscript::Range { .. }))
            {
                continue;
            }
            let var_name = qualified_to_var_name(&instance_data.qualified_name);
            let Some(flat_var) =
                deferred_colon_flat_variable(flat, overlay, instance_data, &var_name)?
            else {
                continue;
            };
            let span = instance_source_span(instance_data, tree)?;
            let inherited_dims = unexpanded_structured_parent_dims(instance_data, overlay);
            let local_dims = self.resolve_component_local_axes(
                var_name.as_str(),
                instance_data,
                flat_var,
                &inherited_dims,
                tree,
                span,
            )?;
            let resolved_dims = PrefixedDims::from_parts(&inherited_dims, local_dims).into_values();
            let flat_var = flat
                .variables
                .get_mut(&var_name)
                .expect("the Flat variable was proved present above");
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

    fn resolve_component_local_axes(
        &self,
        var_name: &str,
        instance_data: &ast::InstanceData,
        flat_var: &flat::Variable,
        inherited_dims: &[i64],
        tree: &ClassTree,
        span: rumoca_core::Span,
    ) -> Result<LocalAxes, FlattenError> {
        let dims_expr = &instance_data.dims_expr;
        let inferred_dims = match flat_var.binding.as_ref() {
            Some(binding) => self.infer_binding_dimensions(var_name, binding, tree)?,
            None => None,
        };
        let expected_local_rank = inferred_dims
            .as_ref()
            .map_or_else(|| instance_data.dims.len().max(dims_expr.len()), Vec::len);
        let admitted_dims = self
            .array_dimensions
            .get(var_name)
            .map(|dims| LocalAxes::from_cached(dims, inherited_dims, expected_local_rank, var_name))
            .transpose()?;
        let inferred_dims = inferred_dims.map(LocalAxes::from_local);
        let candidate = reconcile_local_axes(var_name, admitted_dims, inferred_dims, span)?;

        let mut dims = Vec::with_capacity(dims_expr.len());
        for (index, subscript) in dims_expr.iter().enumerate() {
            let dim = match subscript {
                ast::Subscript::Expression(_) => {
                    let explicit =
                        self.eval_component_dim_subscript(var_name, subscript, tree, span)?;
                    refuse_conflicting_explicit_axis(
                        var_name,
                        index,
                        explicit,
                        candidate.as_ref(),
                        span,
                    )?;
                    explicit
                }
                ast::Subscript::Range { .. } => candidate
                    .as_ref()
                    .and_then(|axes| axes.values().get(index))
                    .copied()
                    .filter(|dimension| *dimension >= 0)
                    .ok_or_else(|| {
                        FlattenError::unresolved_component_dimension(
                            var_name,
                            ":".to_string(),
                            span,
                        )
                    })?,
                ast::Subscript::Empty => {
                    return Err(FlattenError::invalid_ast_subscript(
                        "empty recovery subscript cannot define a component dimension",
                        span,
                    ));
                }
            };
            dims.push(dim);
        }
        if let Some(candidate) = candidate
            && candidate.values().len() > dims_expr.len()
        {
            dims.extend_from_slice(&candidate.values()[dims_expr.len()..]);
        }
        Ok(LocalAxes(dims))
    }

    fn infer_binding_dimensions(
        &self,
        var_name: &str,
        binding: &Expression,
        tree: &ClassTree,
    ) -> Result<Option<Vec<i64>>, FlattenError> {
        if let Some(dims) = infer_enum_range_dimensions(binding, tree) {
            return Ok(Some(dims));
        }
        match infer_array_dimensions_full_with_functions(
            binding,
            &ParamEvalContext::new_resolved(
                &self.parameter_values,
                &self.real_parameter_values,
                &self.boolean_parameter_values,
                &self.array_dimensions,
                &self.functions,
                rumoca_eval_flat::phase_constant::ResolvedParamInventory::new(
                    &self.parameter_values_by_identity,
                    &self.array_dimensions_by_identity,
                    &self.resolved_enum_catalog,
                ),
                Some(var_name),
            ),
        ) {
            Ok(dims) => Ok(dims),
            Err(error) => Err(crate::constant_eval::map_evaluation_error(
                error,
                "inferring a component binding shape",
                binding.span(),
            )?),
        }
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
        let eval_ctx = ParamEvalContext::new_resolved(
            &self.parameter_values,
            &self.real_parameter_values,
            &self.boolean_parameter_values,
            &self.array_dimensions,
            &self.functions,
            rumoca_eval_flat::phase_constant::ResolvedParamInventory::new(
                &self.parameter_values_by_identity,
                &self.array_dimensions_by_identity,
                &self.resolved_enum_catalog,
            ),
            Some(var_name),
        );
        let evaluated = crate::constant_eval::map_optional_evaluation(
            rumoca_eval_flat::phase_constant::try_eval_integer_with_context(&lowered, &eval_ctx),
            "evaluating a required component dimension",
            lowered.span().or(Some(span)),
        )?;
        let Some(dim) = evaluated.filter(|dim| *dim >= 0) else {
            return Err(FlattenError::unresolved_component_dimension(
                var_name,
                expr.to_string(),
                span,
            ));
        };
        Ok(dim)
    }
}

fn reconcile_local_axes(
    var_name: &str,
    admitted: Option<LocalAxes>,
    inferred: Option<LocalAxes>,
    span: rumoca_core::Span,
) -> Result<Option<LocalAxes>, FlattenError> {
    let (admitted, inferred) = match (admitted, inferred) {
        (Some(admitted), Some(inferred)) => (admitted, inferred),
        (admitted, None) => return Ok(admitted),
        (None, inferred) => return Ok(inferred),
    };
    if admitted.values().len() == inferred.values().len()
        && admitted.values().iter().all(|extent| *extent >= 0)
        && inferred.values().iter().all(|extent| *extent >= 0)
    {
        if let Some((index, (&admitted_extent, &inferred_extent))) = admitted
            .values()
            .iter()
            .zip(inferred.values())
            .enumerate()
            .find(|(_, (admitted_extent, inferred_extent))| admitted_extent != inferred_extent)
        {
            return Err(conflicting_component_dimension(
                var_name,
                index,
                admitted_extent,
                inferred_extent,
                span,
            ));
        }
        return Ok(Some(admitted));
    }
    Ok(Some(
        if dims_are_better(inferred.values(), admitted.values()) {
            inferred
        } else {
            admitted
        },
    ))
}

fn conflicting_component_dimension(
    var_name: &str,
    zero_based_axis: usize,
    admitted: i64,
    inferred: i64,
    span: rumoca_core::Span,
) -> FlattenError {
    FlattenError::ConflictingComponentDimension {
        name: var_name.to_string(),
        axis: zero_based_axis + 1,
        admitted,
        inferred,
        span,
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

/// Resolve a deferred-colon component's Flat variable.
///
/// `Ok(None)` means the component belongs to a disabled branch and the caller
/// should skip it; `Err` means a component that should exist does not. Naming
/// the rule here keeps the disabled-component case ahead of the internal error,
/// which is the order the loop relied on when this was inline.
fn deferred_colon_flat_variable<'flat>(
    flat: &'flat flat::Model,
    overlay: &ast::InstanceOverlay,
    instance_data: &ast::InstanceData,
    var_name: &rumoca_core::VarName,
) -> Result<Option<&'flat flat::Variable>, FlattenError> {
    if let Some(variable) = flat.variables.get(var_name) {
        return Ok(Some(variable));
    }
    if crate::is_in_disabled_component(&instance_data.qualified_name, &overlay.disabled_components)
    {
        return Ok(None);
    }
    Err(FlattenError::internal(format!(
        "deferred colon component `{var_name}` has no Flat variable"
    )))
}

/// Refuse an explicit axis that contradicts an already-inferred one.
///
/// A negative inferred extent means "not yet known" and never conflicts; only a
/// non-negative inferred extent that differs from the written one is a conflict.
/// Extracted so the rule is named rather than nested three deep inside the
/// subscript match.
fn refuse_conflicting_explicit_axis(
    var_name: &str,
    index: usize,
    explicit: i64,
    candidate: Option<&LocalAxes>,
    span: rumoca_core::Span,
) -> Result<(), FlattenError> {
    if let Some(inferred) = candidate
        .and_then(|axes| axes.values().get(index))
        .copied()
        .filter(|inferred| *inferred >= 0 && *inferred != explicit)
    {
        return Err(conflicting_component_dimension(
            var_name, index, explicit, inferred, span,
        ));
    }
    Ok(())
}
