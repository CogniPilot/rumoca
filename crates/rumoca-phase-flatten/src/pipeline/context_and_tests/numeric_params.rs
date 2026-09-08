//! Integer, boolean, and real parameter evaluation passes plus the scalar
//! lookups they feed (MLS §4.5, §8.6).

use super::param_binding::ParamBinding;
use super::*;

impl Context {
    /// Try to evaluate integer parameters in one pass.
    ///
    /// Uses full context including enums to handle conditional bindings like:
    /// `parameter Integer nr = if filterType == LowPass then order else 0`
    ///
    /// Also passes variable context for modification binding resolution (MLS §7.2):
    /// When a binding like `G1(n=n)` has unqualified refs, they're resolved
    /// relative to the parent scope.
    pub(super) fn eval_integer_param_bindings(
        &mut self,
        params: &[ParamBinding<'_>],
    ) -> Result<bool, FlattenError> {
        let mut progress = false;
        // Collect new values to avoid cloning HashMaps for borrow splitting.
        // Build one evaluator per pass (not per parameter). `ParamEvaluator`
        // owns the same constant evaluator used by the former fallback, plus
        // scoped lookup and enumeration identity, so a second full context
        // would only duplicate every parameter and function.
        let param_context = ParamEvalContext::new_resolved(
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
            None,
        );
        let mut param_evaluator = ParamEvaluator::new(&param_context)
            .map_err(|error| FlattenError::internal(error.to_string()))?;

        let mut new_vals = Vec::new();
        for ParamBinding {
            name,
            binding,
            primitive,
            binding_from_modification,
            ..
        } in params
        {
            if !primitive.may_evaluate_as_integer() || self.binding_has_array_shape(name) {
                continue;
            }
            if let Some(val) = self.try_eval_modifier_scoped_integer_alias(
                name,
                binding,
                *binding_from_modification,
            ) {
                new_vals.push(((*name).to_string(), val));
                continue;
            }
            if let Some(val) = map_param_evaluation(
                param_evaluator.eval_integer(binding, Some(name)),
                binding,
                "evaluating an Integer parameter binding",
            )? {
                new_vals.push(((*name).to_string(), val));
            }
        }

        for (name, val) in new_vals {
            if self.parameter_values.get(&name).copied() != Some(val) {
                self.parameter_values.insert(name.clone(), val);
                progress = true;
            }
        }
        Ok(progress)
    }

    fn try_eval_modifier_scoped_integer_alias(
        &self,
        name: &str,
        binding: &Expression,
        binding_from_modification: bool,
    ) -> Option<i64> {
        if !binding_from_modification {
            return None;
        }
        let target = unqualified_varref_name(binding)?;
        let source_scope = modifier_source_scope(name)?;
        rumoca_core::EvalLookup::lookup_integer(self, target, source_scope.as_str())
    }

    /// Try to evaluate boolean parameters in one pass.
    pub(super) fn eval_boolean_params(
        &mut self,
        params: &[ParamBinding<'_>],
    ) -> Result<bool, FlattenError> {
        let param_context = ParamEvalContext::new_resolved(
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
            None,
        );
        let mut param_evaluator = ParamEvaluator::new(&param_context)
            .map_err(|error| FlattenError::internal(error.to_string()))?;
        let mut new_vals = Vec::new();
        for ParamBinding {
            name,
            binding,
            primitive,
            ..
        } in params
        {
            if !primitive.may_evaluate_as_boolean() || self.binding_has_array_shape(name) {
                continue;
            }
            if let Some(value) = map_param_evaluation(
                param_evaluator.eval_boolean(binding, Some(name)),
                binding,
                "evaluating a Boolean parameter binding",
            )? {
                new_vals.push(((*name).to_string(), value));
            }
        }

        let mut progress = false;
        for (name, val) in new_vals {
            if self.boolean_parameter_values.get(&name).copied() != Some(val) {
                self.boolean_parameter_values.insert(name, val);
                progress = true;
            }
        }
        Ok(progress)
    }

    /// Try to evaluate real parameters in one pass.
    pub(super) fn eval_real_params(
        &mut self,
        params: &[ParamBinding<'_>],
    ) -> Result<bool, FlattenError> {
        let param_context = ParamEvalContext::new_resolved(
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
            None,
        );
        let mut param_evaluator = ParamEvaluator::new(&param_context)
            .map_err(|error| FlattenError::internal(error.to_string()))?;
        let mut new_vals = Vec::new();
        for ParamBinding {
            name,
            binding,
            primitive,
            ..
        } in params
        {
            if !primitive.may_evaluate_as_real() || self.binding_has_array_shape(name) {
                continue;
            }
            let value = map_param_evaluation(
                param_evaluator.eval_real(binding, Some(name)),
                binding,
                "evaluating a Real parameter binding",
            )?;
            if let Some(value) = value {
                new_vals.push(((*name).to_string(), value));
            }
        }

        let mut progress = false;
        for (name, val) in new_vals {
            if self
                .real_parameter_values
                .get(&name)
                .copied()
                .is_none_or(|existing| existing != val)
            {
                self.real_parameter_values.insert(name, val);
                progress = true;
            }
        }
        Ok(progress)
    }

    /// Look up an integer parameter value, resolving through aliases if needed.
    pub(crate) fn get_integer_param(&self, name: &str) -> Option<i64> {
        if let Some(val) = self.parameter_values.get(name).copied() {
            return Some(val);
        }
        let resolved = self.resolve_alias(name);
        if resolved != name
            && let Some(val) = self.parameter_values.get(&resolved).copied()
        {
            return Some(val);
        }
        None
    }

    /// Look up a boolean parameter value, resolving through aliases if needed.
    pub(crate) fn get_boolean_param(&self, name: &str) -> Option<bool> {
        // Try direct lookup first
        if let Some(val) = self.boolean_parameter_values.get(name) {
            return Some(*val);
        }
        // Try alias resolution
        let resolved = self.resolve_alias(name);
        if resolved != name {
            return self.boolean_parameter_values.get(&resolved).copied();
        }
        None
    }

    /// A predefined element type does not make an array-valued declaration a
    /// scalar parameter. Shape discovery runs before the scalar fixed-point
    /// passes, so its inventory is the authoritative guard against projecting
    /// an array value through `eval_integer`/`eval_real`/`eval_boolean`.
    fn binding_has_array_shape(&self, name: &str) -> bool {
        self.array_dimensions
            .get(name)
            .is_some_and(|dimensions| !dimensions.is_empty())
    }
}

fn map_param_evaluation<T>(
    result: Result<Option<T>, rumoca_eval_flat::constant::EvalError>,
    binding: &Expression,
    operation: &'static str,
) -> Result<Option<T>, FlattenError> {
    crate::constant_eval::map_optional_evaluation(result, operation, binding.span())
}

fn unqualified_varref_name(expr: &Expression) -> Option<&str> {
    let Expression::VarRef {
        name, subscripts, ..
    } = expr
    else {
        return None;
    };
    if !subscripts.is_empty() {
        return None;
    }
    let path = rumoca_core::ComponentPath::from_flat_path(name.as_str());
    (path.len() == 1).then_some(name.as_str())
}

fn modifier_source_scope(name: &str) -> Option<String> {
    let variable_path = rumoca_core::ComponentPath::from_flat_path(name);
    let component_scope = variable_path.parent()?;
    let source_scope = component_scope.parent()?;
    Some(source_scope.to_flat_string())
}
