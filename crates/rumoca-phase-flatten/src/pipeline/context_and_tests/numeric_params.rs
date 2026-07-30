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
    #[cfg(test)]
    pub(crate) fn eval_integer_params(&mut self, params: &[(String, Expression)]) -> bool {
        let params = params
            .iter()
            .map(|(name, binding)| ParamBinding {
                name: name.as_str(),
                binding,
                may_be_record_alias: false,
                binding_from_modification: false,
            })
            .collect::<Vec<_>>();
        self.eval_integer_param_bindings(&params)
    }

    #[cfg(test)]
    pub(crate) fn eval_modified_integer_params(&mut self, params: &[(String, Expression)]) -> bool {
        let params = params
            .iter()
            .map(|(name, binding)| ParamBinding {
                name: name.as_str(),
                binding,
                may_be_record_alias: false,
                binding_from_modification: true,
            })
            .collect::<Vec<_>>();
        self.eval_integer_param_bindings(&params)
    }

    pub(super) fn eval_integer_param_bindings(&mut self, params: &[ParamBinding<'_>]) -> bool {
        // Keep seeded integer values consistent with already-evaluated reals.
        // This prevents stale defaults from shadowing the evaluated binding for
        // the same parameter name in structural integer contexts.
        let mut progress = false;
        for (name, real_val) in &self.real_parameter_values {
            if !real_val.is_finite() || real_val.fract() != 0.0 {
                continue;
            }
            let int_val = *real_val as i64;
            if let Some(existing) = self.parameter_values.get(name).copied()
                && existing != int_val
            {
                self.parameter_values.insert(name.clone(), int_val);
                progress = true;
            }
        }

        // Collect new values to avoid cloning HashMaps for borrow splitting.
        // Build eval context once per pass (not per parameter).
        let eval_ctx = build_eval_context(
            &self.parameter_values,
            &self.real_parameter_values,
            &self.boolean_parameter_values,
            &self.array_dimensions,
            &self.functions,
        );
        let mut param_evaluator = ParamEvaluator::new(&ParamEvalContext {
            known_ints: &self.parameter_values,
            known_reals: &self.real_parameter_values,
            known_bools: &self.boolean_parameter_values,
            known_enums: &self.enum_parameter_values,
            array_dims: &self.array_dimensions,
            functions: &self.functions,
            var_context: None,
        });

        let new_vals: Vec<(String, i64)> = params
            .iter()
            .filter_map(
                |ParamBinding {
                     name,
                     binding,
                     binding_from_modification,
                     ..
                 }| {
                    if let Some(val) = self.try_eval_modifier_scoped_integer_alias(
                        name,
                        binding,
                        *binding_from_modification,
                    ) {
                        return Some(((*name).to_string(), val));
                    }

                    // Try evaluation with full context including functions.
                    if let Some(val) = param_evaluator.eval_integer(binding, Some(name)) {
                        return Some(((*name).to_string(), val));
                    }

                    // Fallback to rumoca_eval_const for complex expressions
                    rumoca_eval_flat::constant::try_eval_integer(binding, &eval_ctx)
                        .map(|val| ((*name).to_string(), val))
                },
            )
            .collect();

        for (name, val) in new_vals {
            if self.parameter_values.get(&name).copied() != Some(val) {
                self.parameter_values.insert(name.clone(), val);
                progress = true;
            }
            if let Some(real_val) = self.real_parameter_values.get_mut(&name)
                && (real_val.fract() == 0.0)
                && (*real_val as i64 != val)
            {
                *real_val = val as f64;
                progress = true;
            }
        }
        progress
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
    pub(super) fn eval_boolean_params(&mut self, params: &[ParamBinding<'_>]) -> bool {
        let mut param_evaluator = ParamEvaluator::new(&ParamEvalContext {
            known_ints: &self.parameter_values,
            known_reals: &self.real_parameter_values,
            known_bools: &self.boolean_parameter_values,
            known_enums: &self.enum_parameter_values,
            array_dims: &self.array_dimensions,
            functions: &self.functions,
            var_context: None,
        });
        let new_vals: Vec<(String, bool)> = params
            .iter()
            .filter_map(|ParamBinding { name, binding, .. }| {
                param_evaluator
                    .eval_boolean(binding, Some(name))
                    .map(|v| ((*name).to_string(), v))
            })
            .collect();

        let mut progress = false;
        for (name, val) in new_vals {
            if self.boolean_parameter_values.get(&name).copied() != Some(val) {
                self.boolean_parameter_values.insert(name, val);
                progress = true;
            }
        }
        progress
    }

    /// Try to evaluate real parameters in one pass.
    pub(super) fn eval_real_params(&mut self, params: &[ParamBinding<'_>]) -> bool {
        let mut param_evaluator = ParamEvaluator::new(&ParamEvalContext {
            known_ints: &self.parameter_values,
            known_reals: &self.real_parameter_values,
            known_bools: &self.boolean_parameter_values,
            known_enums: &self.enum_parameter_values,
            array_dims: &self.array_dimensions,
            functions: &self.functions,
            var_context: None,
        });
        let new_vals: Vec<(String, f64)> = params
            .iter()
            .filter_map(|ParamBinding { name, binding, .. }| {
                if let Some(val) = param_evaluator.eval_real(binding, Some(name)) {
                    return Some(((*name).to_string(), val));
                }
                // Try user-defined function evaluation for function call bindings
                self.try_eval_real_func_call(name, binding)
                    .map(|val| ((*name).to_string(), val))
            })
            .collect();

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
        progress
    }

    /// Try evaluating a function call binding as a real value.
    fn try_eval_real_func_call(&self, name: &str, binding: &Expression) -> Option<f64> {
        let Expression::FunctionCall {
            name: func_name,
            args,
            ..
        } = binding
        else {
            return None;
        };
        let int_ctx = ParamEvalContext {
            known_ints: &self.parameter_values,
            known_reals: &self.real_parameter_values,
            known_bools: &self.boolean_parameter_values,
            known_enums: &self.enum_parameter_values,
            array_dims: &self.array_dimensions,
            functions: &self.functions,
            var_context: Some(name),
        };
        eval_user_func_real(func_name, args, &int_ctx)
    }

    fn integral_real_param(&self, name: &str) -> Option<i64> {
        self.real_parameter_values.get(name).and_then(|val| {
            if val.is_finite() && val.fract() == 0.0 {
                Some(*val as i64)
            } else {
                None
            }
        })
    }

    /// Look up an integer parameter value, resolving through aliases if needed.
    pub(crate) fn get_integer_param(&self, name: &str) -> Option<i64> {
        // Try direct lookup in integer parameters first
        if let Some(val) = self.parameter_values.get(name).copied() {
            // Prefer the evaluated real value when both maps disagree.
            // Later constant/default injection can seed stale integer values.
            return Some(self.integral_real_param(name).unwrap_or(val));
        }
        // Try alias resolution for integers
        let resolved = self.resolve_alias(name);
        if resolved != name
            && let Some(val) = self.parameter_values.get(&resolved).copied()
        {
            return Some(self.integral_real_param(&resolved).unwrap_or(val));
        }
        // Fallback: try real parameters that are whole numbers (e.g., Real m = 3)
        let real_name = if resolved != name { &resolved } else { name };
        if let Some(val) = self
            .real_parameter_values
            .get(real_name)
            .or_else(|| self.real_parameter_values.get(name))
            .copied()
            && val.fract() == 0.0
            && val.is_finite()
        {
            return Some(val as i64);
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
