use super::*;

fn modified_integer_param_bindings(
    flat: &Model,
) -> rustc_hash::FxHashMap<String, Option<Expression>> {
    flat.variables
        .iter()
        .filter(|(_, var)| var.binding_from_modification && var.is_discrete_type)
        .map(|(name, var)| (name.to_string(), var.binding.clone()))
        .collect()
}

fn literal_integer(expr: &Expression) -> Option<i64> {
    match expr {
        Expression::Literal {
            value: rumoca_core::Literal::Integer(value),
            ..
        } => Some(*value),
        _ => None,
    }
}

fn has_nested_modified_integer_target(expr: &Expression) -> bool {
    matches!(
        expr,
        Expression::VarRef {
            name,
            subscripts,
            ..
        } if subscripts.is_empty() && name.is_nested()
    )
}

impl Context {
    pub(crate) fn reconcile_modified_binding_dimensions(&self, flat: &mut Model) -> bool {
        let mut changed = false;
        let modified_integer_params = modified_integer_param_bindings(flat);
        for (name, var) in &mut flat.variables {
            if !var.binding_from_modification {
                continue;
            }
            let Some(inferred_dims) = self.effective_modified_binding_dims(
                name.as_str(),
                &var.dims,
                &modified_integer_params,
            ) else {
                continue;
            };
            if inferred_dims.is_empty() || var.dims == inferred_dims {
                continue;
            }
            if dims_are_better(&inferred_dims, &var.dims)
                || same_rank_concrete_dims(&inferred_dims, &var.dims)
            {
                var.dims = inferred_dims;
                changed = true;
            }
        }
        changed
    }

    pub(super) fn reconcile_modified_integer_parameter_values(&mut self, flat: &Model) -> bool {
        let modified_integer_params = modified_integer_param_bindings(flat);
        let new_vals = modified_integer_params
            .iter()
            .filter_map(|(name, binding)| {
                binding
                    .as_ref()
                    .filter(|expr| has_nested_modified_integer_target(expr))
                    .and_then(|_| {
                        self.modified_integer_binding_value(name, &modified_integer_params, 0)
                    })
                    .map(|value| (name.clone(), value))
            })
            .collect::<Vec<_>>();
        let mut changed = false;
        for (name, value) in new_vals {
            if self.parameter_values.get(&name).copied() != Some(value) {
                self.parameter_values.insert(name.clone(), value);
                changed = true;
            }
            if let Some(real_value) = self.real_parameter_values.get_mut(&name)
                && real_value.fract() == 0.0
                && *real_value as i64 != value
            {
                *real_value = value as f64;
                changed = true;
            }
        }
        changed
    }

    fn effective_modified_binding_dims(
        &self,
        name: &str,
        _current_dims: &[i64],
        _modified_integer_params: &rustc_hash::FxHashMap<String, Option<Expression>>,
    ) -> Option<Vec<i64>> {
        self.array_dimensions.get(name).cloned()
    }

    fn modified_integer_binding_value(
        &self,
        name: &str,
        modified_integer_params: &rustc_hash::FxHashMap<String, Option<Expression>>,
        depth: usize,
    ) -> Option<i64> {
        const MAX_BINDING_DEPTH: usize = 8;
        if depth >= MAX_BINDING_DEPTH {
            return None;
        }
        let binding = modified_integer_params.get(name)?.as_ref()?;
        if let Some(value) = literal_integer(binding) {
            return Some(value);
        }
        let Expression::VarRef {
            name: target,
            subscripts,
            ..
        } = binding
        else {
            return None;
        };
        if !subscripts.is_empty() {
            return None;
        }
        if target.is_nested() {
            return self
                .modified_integer_binding_value(target.as_str(), modified_integer_params, depth + 1)
                .or_else(|| self.get_integer_param(target.as_str()));
        }
        let source_scope = modifier_source_scope(name)?;
        rumoca_core::EvalLookup::lookup_integer(self, target.as_str(), source_scope.as_str())
    }
}
