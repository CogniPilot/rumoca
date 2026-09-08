//! Array dimension inference and propagation for the flatten context: seeding
//! from declared dims and array literals, inferring from builtin calls, and
//! propagating through component-reference bindings (MLS §10.1, §10.4).

use super::param_binding::{ParamBinding, is_array_literal_binding};
use super::*;

impl Context {
    /// Initialize array dimensions from declared dims (MLS §10.1).
    ///
    /// Bindings are read against the identified callable catalog: a
    /// user-function call names its shape through the callee's declared
    /// output, and the call carries its exact occurrence from the early
    /// canonicalization pass, so the catalog can answer without a name guess.
    pub(super) fn init_array_dimensions(&mut self, flat: &Model) -> Result<(), FlattenError> {
        for (name, var) in &flat.variables {
            if var.dims.is_empty() {
                continue;
            }
            let dims_to_use = map_dimension_evaluation(
                try_infer_better_dims_with_functions(var, &self.functions),
                var.binding.as_ref(),
                "inferring declared array dimensions",
            )?;
            self.array_dimensions.insert(name.to_string(), dims_to_use);
        }
        Ok(())
    }

    /// Infer dimensions from array literal bindings (MLS §10.1).
    pub(super) fn infer_dims_from_literals(&mut self, flat: &Model) -> Result<(), FlattenError> {
        for (name, var) in &flat.variables {
            if self
                .array_dimensions
                .contains_key(name.to_string().as_str())
            {
                continue;
            }
            if let Some(binding) = &var.binding
                && let Some(inferred_dims) = map_dimension_evaluation(
                    infer_array_dimensions_checked_with_functions(binding, &self.functions),
                    Some(binding),
                    "inferring array literal dimensions",
                )?
            {
                #[cfg(feature = "tracing")]
                tracing::debug!(var = %name, dims = ?inferred_dims, "inferred array dimensions from binding");
                self.array_dimensions
                    .insert(name.to_string(), inferred_dims);
            }
        }
        Ok(())
    }

    /// Try to infer array dimensions using known integer parameters (MLS §10.4).
    ///
    /// This allows evaluating `zeros(n)`, `ones(m)`, `fill(v, n1, n2)` when
    /// the dimension arguments are now-known parameter values.
    ///
    /// Also handles Range expressions like `2:size(table, 2)` when the array
    /// dimensions of the referenced arrays are known.
    ///
    /// Also handles conditional expressions like `table = if cond then A else B`
    /// by evaluating conditions using known boolean and enum parameters.
    pub(super) fn eval_array_dimensions(
        &mut self,
        var_bindings: &[ParamBinding<'_>],
    ) -> Result<bool, FlattenError> {
        let mut new_dims = false;
        for ParamBinding {
            name,
            binding,
            binding_from_modification,
            ..
        } in var_bindings
        {
            new_dims |= self.try_infer_array_dims(name, binding, *binding_from_modification)?;
        }
        Ok(new_dims)
    }

    /// Try to infer array dimensions for a single binding.
    fn try_infer_array_dims(
        &mut self,
        name: &str,
        binding: &Expression,
        binding_from_modification: bool,
    ) -> Result<bool, FlattenError> {
        // Skip when the variable is inside an expanded array component element.
        // During array expansion, sub-component modifications (e.g., `L=fill(L1sigma,m)`)
        // are NOT indexed for each element. So `inductor[1].L` gets the same unindexed
        // binding as the parent `inductor.L`, which infers to the parent's array dims.
        // Detect this by checking if any path segment (not the last) has embedded subscripts.
        if has_embedded_array_subscript_in_parent(name)
            && !(binding_from_modification && is_array_literal_binding(binding))
        {
            return Ok(false);
        }

        let inferred = map_dimension_evaluation(
            infer_array_dimensions_full_with_functions(
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
                    Some(name),
                ),
            ),
            Some(binding),
            "inferring parameter-dependent array dimensions",
        )?;
        let inferred_dims = match inferred {
            Some(dims) => dims,
            None => return Ok(false),
        };

        // Check if we should update (MLS §10.1)
        let should_update = self
            .array_dimensions
            .get(name)
            .is_none_or(|existing| dims_are_better(&inferred_dims, existing));

        if should_update {
            #[cfg(feature = "tracing")]
            tracing::debug!(var = %name, dims = ?inferred_dims, "inferred array dimensions from builtin");
            self.array_dimensions
                .insert(name.to_string(), inferred_dims);
            Ok(true)
        } else {
            Ok(false)
        }
    }

    /// Propagate array dimensions for VarRef bindings (MLS §10.1, §7.2.3).
    ///
    /// When a parameter has a VarRef binding (e.g., `table = cellData.OCV_SOC_internal`),
    /// we need to propagate dimensions from the target variable. This handles:
    /// - Direct VarRef lookups
    /// - VarRef targets that need alias resolution
    /// - Better dimension propagation (more complete dims replace incomplete ones)
    pub(super) fn propagate_varref_dimensions(
        &mut self,
        var_bindings: &[ParamBinding<'_>],
    ) -> bool {
        var_bindings
            .iter()
            .filter_map(|ParamBinding { name, binding, .. }| {
                self.try_propagate_varref_dims(name, binding)
            })
            .count()
            > 0
    }

    /// Try to propagate dimensions from a VarRef binding.
    fn try_propagate_varref_dims(&mut self, name: &str, binding: &Expression) -> Option<()> {
        let target_name = match binding {
            Expression::VarRef {
                name: target,
                subscripts,
                ..
            } if subscripts.is_empty() => target.to_string(),
            _ => return None,
        };

        // Skip when the name passes through an expanded array component element.
        if has_embedded_array_subscript_in_parent(name) {
            return None;
        }

        let direct_dims = self.array_dimensions.get(&target_name);
        let resolved_name = self.resolve_alias(&target_name);
        let alias_dims = (resolved_name != target_name)
            .then(|| self.array_dimensions.get(&resolved_name))
            .flatten();

        // Use better (more complete) dimensions
        let target_dims = best_dims(direct_dims, alias_dims)?;

        // Update if we don't have dims or new dims are better
        let should_update = self
            .array_dimensions
            .get(name)
            .is_none_or(|existing| dims_are_better(&target_dims, existing));

        if should_update {
            self.array_dimensions.insert(name.to_string(), target_dims);
            Some(())
        } else {
            None
        }
    }

    /// Get array dimensions for a variable.
    ///
    /// Returns the dimensions vector if the variable has array dimensions,
    /// or None for scalar variables.
    pub(crate) fn get_array_dimensions(&self, name: &str) -> Option<&Vec<i64>> {
        self.array_dimensions.get(name)
    }

    /// Look up array dimensions, resolving through aliases if needed.
    pub(crate) fn get_array_dims(&self, name: &str) -> Option<Vec<i64>> {
        // Try direct lookup first
        if let Some(dims) = self.array_dimensions.get(name) {
            return Some(dims.clone());
        }
        // Try alias resolution
        let resolved = self.resolve_alias(name);
        if resolved != name {
            return self.array_dimensions.get(&resolved).cloned();
        }
        None
    }
}

fn map_dimension_evaluation<T>(
    result: Result<T, rumoca_eval_flat::constant::EvalError>,
    owner: Option<&Expression>,
    operation: &'static str,
) -> Result<T, FlattenError> {
    match result {
        Ok(value) => Ok(value),
        Err(error) => Err(crate::constant_eval::map_evaluation_error(
            error,
            operation,
            owner.and_then(Expression::span),
        )?),
    }
}
