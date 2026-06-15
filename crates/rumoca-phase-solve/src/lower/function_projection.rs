use super::*;
use crate::projection_suffix::parse_output_projection_suffix;

#[derive(Debug, Clone)]
pub(super) struct FunctionOutputProjection {
    pub(super) base_function_name: rumoca_core::VarName,
    pub(super) output_name: String,
    pub(super) output_field: Option<String>,
    pub(super) scope_indices: Vec<usize>,
    pub(super) indices: Vec<usize>,
}

impl<'a> LowerBuilder<'a> {
    pub(super) fn lookup_function_output_projection(
        &self,
        name: &rumoca_core::Reference,
    ) -> Option<FunctionOutputProjection> {
        let requested = name.as_str();
        rumoca_core::find_map_top_level_splits_rev(requested, |base_name, suffix| {
            let function = self.lookup_function_key(base_name)?;
            let projection_suffix = parse_output_projection_suffix(suffix)?;
            let output_name = projection_suffix.output_name;
            let output_field = projection_suffix.output_field;
            let raw_indices = projection_suffix.indices;

            let (output, output_name, output_field) =
                if let Some(output) = function.outputs.iter().find(|out| out.name == output_name) {
                    (output, output_name, output_field)
                } else if output_field.is_none()
                    && matches!(output_name.as_str(), "re" | "im")
                    && function.outputs.len() == 1
                    && output_is_complex_record(&function.outputs[0])
                {
                    (
                        &function.outputs[0],
                        function.outputs[0].name.clone(),
                        Some(output_name),
                    )
                } else {
                    return None;
                };
            if let Some(field) = output_field.as_deref()
                && (!output_is_complex_record(output) || !matches!(field, "re" | "im"))
            {
                return None;
            }

            let indices = if output_has_dynamic_dims(output) {
                normalize_dynamic_projection_indices(&raw_indices)?
            } else {
                normalize_projection_indices(&output.dims, &raw_indices)?
            };
            let scope_indices = if output_has_dynamic_dims(output) {
                raw_indices.clone()
            } else {
                scope_indices_for_projection(&output.dims, &raw_indices, &indices)
            };
            Some(FunctionOutputProjection {
                base_function_name: function.name.clone(),
                output_name,
                output_field,
                scope_indices,
                indices,
            })
        })
    }

    pub(super) fn lower_projected_function_call(
        &mut self,
        projection: &FunctionOutputProjection,
        args: &[rumoca_core::Expression],
        span: rumoca_core::Span,
        caller_scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        let Some(function) = self
            .lookup_function_key(projection.base_function_name.as_str())
            .cloned()
        else {
            return Err(LowerError::MissingFunction {
                name: projection.base_function_name.as_str().to_string(),
            });
        };

        if let Some(reg) =
            self.lower_fft_projection_reg(projection, args, caller_scope, call_depth)?
        {
            return Ok(reg);
        }

        if function.external.is_some() {
            if let Some(reg) =
                self.lower_projected_random_intrinsic(projection, args, caller_scope, call_depth)?
            {
                return Ok(reg);
            }
            if let Some(reg) = self.try_lower_intrinsic_function_call_key(
                projection.base_function_name.as_str(),
                args,
                span,
                caller_scope,
                call_depth,
            )? && projection.indices.is_empty()
            {
                return Ok(reg);
            }
            return Err(LowerError::Unsupported {
                reason: format!(
                    "external function call `{}` cannot be inlined",
                    projection.base_function_name.as_str()
                ),
            });
        }

        if projection.indices.is_empty() && projection.output_field.is_none() {
            let values = self.lower_user_function_call_named_output_values(
                &projection.base_function_name,
                &projection.output_name,
                args,
                caller_scope,
                call_depth,
            )?;
            let [reg] = values.as_slice() else {
                return Err(LowerError::InvalidFunction {
                    name: projection.base_function_name.as_str().to_string(),
                    reason: format!(
                        "projected scalar output `{}` resolved to {} values",
                        projection.output_name,
                        values.len()
                    ),
                }
                .with_fallback_span(span));
            };
            return Ok(*reg);
        }

        self.with_local_lower_frame(|this| {
            let bindings = this.bind_function_inputs_for_name(
                projection.base_function_name.as_str(),
                &function.inputs,
                args,
                caller_scope,
                call_depth,
            )?;
            let mut scope = bindings.scope;
            this.local_const_bindings.extend(bindings.const_bindings);
            this.initialize_function_output_scope(&function, &mut scope, call_depth)?;
            let _returned = this.lower_statements(&function.body, &mut scope, call_depth + 1)?;
            let projection_key = format_projection_scope_key(projection);
            resolve_projected_function_reg(projection, &scope, &projection_key).ok_or_else(|| {
                LowerError::InvalidFunction {
                    name: projection.base_function_name.as_str().to_string(),
                    reason: format!(
                        "projected output `{}` could not be resolved",
                        projection_key
                    ),
                }
            })
        })
    }

    pub(super) fn bind_assignment_values(
        &mut self,
        scope: &mut Scope,
        target: &str,
        values: &[Reg],
    ) {
        self.clear_local_const_assignment(target);
        if let Some(dims) = self.local_binding_dims.get(target).cloned() {
            self.bind_assignment_values_with_dims(scope, target, values, &dims);
            return;
        }

        self.bind_flat_assignment_values(scope, target, values);
    }

    pub(super) fn bind_assignment_values_with_dims(
        &mut self,
        scope: &mut Scope,
        target: &str,
        values: &[Reg],
        dims: &[i64],
    ) {
        self.clear_local_const_assignment(target);
        let resolved_dims = resolve_array_dims_for_value_count(dims, values.len());
        if resolved_dims.is_empty() {
            self.bind_flat_assignment_values(scope, target, values);
            return;
        }

        self.set_known_local_array_dims(target, resolved_dims.clone(), values.len());
        clear_indexed_scope_bindings(scope, target);
        self.local_indexed_bindings.shift_remove(target);
        if values.is_empty() {
            scope.insert(generated_scope_key(target), self.emit_const(0.0));
            return;
        }

        scope.insert(generated_scope_key(target), values[0]);
        let target_path = generated_scope_key(target);
        for (idx, value) in values.iter().enumerate() {
            let Some(indices) = projection_indices_for_dims(&resolved_dims, idx) else {
                continue;
            };
            self.local_indexed_bindings
                .entry(target.to_string())
                .or_default()
                .push(super::LocalIndexedBinding {
                    reg: *value,
                    indices: indices.clone(),
                });
            let key = format_subscript_binding_key(target, &indices);
            scope.insert(generated_scope_key(&key), *value);
            scope.insert_indexed(&target_path, &indices, *value);
        }
    }

    fn bind_flat_assignment_values(&mut self, scope: &mut Scope, target: &str, values: &[Reg]) {
        self.clear_local_const_assignment(target);
        clear_indexed_scope_bindings(scope, target);
        self.clear_local_array_metadata(target);
        if values.is_empty() {
            scope.insert(generated_scope_key(target), self.emit_const(0.0));
            return;
        }

        scope.insert(generated_scope_key(target), values[0]);
        let target_path = generated_scope_key(target);
        for (idx, value) in values.iter().enumerate() {
            self.local_indexed_bindings
                .entry(target.to_string())
                .or_default()
                .push(super::LocalIndexedBinding {
                    reg: *value,
                    indices: vec![idx + 1],
                });
            let key = format_subscript_binding_key(target, &[idx + 1]);
            scope.insert(generated_scope_key(&key), *value);
            scope.insert_indexed(&target_path, &[idx + 1], *value);
        }
    }

    pub(super) fn clear_local_const_assignment(&mut self, target: &str) {
        self.local_const_bindings.shift_remove(target);
    }

    pub(super) fn initial_function_param_values(
        &mut self,
        param: &rumoca_core::FunctionParam,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Vec<Reg>, LowerError> {
        if let Some(default) = param.default.as_ref() {
            let values = self.lower_array_like_values(default, scope, call_depth + 1)?;
            if param.dims.is_empty()
                && values.len() == 1
                && let Ok(value) = self.eval_compile_time_expr(default, &self.local_const_bindings)
            {
                self.local_const_bindings.insert(param.name.clone(), value);
            }
            return Ok(values);
        }

        Err(LowerError::InvalidFunction {
            name: param.name.clone(),
            reason: "defaultless function output/local has no initial value".to_string(),
        })
    }
}

fn positional_field_projection_index(projection: &FunctionOutputProjection) -> Option<usize> {
    if !projection.indices.is_empty() {
        return None;
    }
    constructor_positional_field_index(projection.output_field.as_deref()?)
}

fn scoped_reg(scope: &Scope, key: &str) -> Option<Reg> {
    scope.get(&generated_scope_key(key)).copied()
}

fn resolve_projected_function_reg(
    projection: &FunctionOutputProjection,
    scope: &Scope,
    projection_key: &str,
) -> Option<Reg> {
    scoped_reg(scope, projection_key)
        .or_else(|| positional_field_projection_reg(projection, scope))
        .or_else(|| base_projection_reg(projection, scope))
}

fn positional_field_projection_reg(
    projection: &FunctionOutputProjection,
    scope: &Scope,
) -> Option<Reg> {
    let index = positional_field_projection_index(projection)?;
    let indexed_key = format_subscript_binding_key(&projection.output_name, &[index + 1]);
    scoped_reg(scope, &indexed_key)
}

fn base_projection_reg(projection: &FunctionOutputProjection, scope: &Scope) -> Option<Reg> {
    (projection.indices.len() == 1 && projection.indices[0] == 1)
        .then(|| format_projection_base_scope_key(projection))
        .and_then(|base_key| scoped_reg(scope, &base_key))
}

fn projection_indices_for_dims(dims: &[i64], flat_index: usize) -> Option<Vec<usize>> {
    if dims.is_empty() {
        return Some(Vec::new());
    }
    if dims.iter().any(|dim| *dim < 0) {
        return Some(vec![flat_index + 1]);
    }
    let total = dims
        .iter()
        .try_fold(1usize, |acc, dim| acc.checked_mul(*dim as usize))?;
    if flat_index >= total {
        return None;
    }
    let mut remainder = flat_index;
    let mut indices = vec![1usize; dims.len()];
    for dim_idx in (0..dims.len()).rev() {
        let dim = dims[dim_idx] as usize;
        indices[dim_idx] = remainder % dim + 1;
        remainder /= dim;
    }
    Some(indices)
}

pub(super) fn format_subscript_binding_key(base: &str, indices: &[usize]) -> String {
    dae::format_subscript_key(base, indices)
}

pub(super) fn clear_indexed_scope_bindings(scope: &mut Scope, target: &str) {
    let target_path = generated_scope_key(target);
    scope.clear_indexed(&target_path);
    let prefix = format!("{target}[");
    let keys = scope
        .keys()
        .into_iter()
        .filter(|key| generated_scope_key_name(key).is_some_and(|name| name.starts_with(&prefix)))
        .collect::<Vec<_>>();
    for key in keys {
        scope.shift_remove(&key);
    }
}

fn output_is_complex_record(output: &rumoca_core::FunctionParam) -> bool {
    rumoca_core::qualified_type_name_matches(&output.type_name, "Complex")
}

fn output_has_dynamic_dims(output: &rumoca_core::FunctionParam) -> bool {
    !output.shape_expr.is_empty() && output.dims.iter().any(|dim| *dim <= 0)
}

fn normalize_dynamic_projection_indices(raw_indices: &[usize]) -> Option<Vec<usize>> {
    if raw_indices.is_empty() {
        return Some(Vec::new());
    }
    raw_indices
        .iter()
        .all(|idx| *idx >= 1)
        .then(|| raw_indices.to_vec())
}

fn normalize_projection_indices(output_dims: &[i64], raw_indices: &[usize]) -> Option<Vec<usize>> {
    if output_dims.is_empty() {
        return raw_indices.is_empty().then_some(Vec::new());
    }
    if raw_indices.is_empty() {
        return Some(Vec::new());
    }
    if output_dims.iter().any(|dim| *dim < 0) {
        // MLS §12.4.3: tuple assignment targets must agree with the
        // corresponding output component type. When DAE carries a symbolic
        // output dimension as 0, the scalarized assignment target supplies the
        // valid one-based projection.
        return raw_indices.iter().all(|idx| *idx >= 1).then(|| {
            if raw_indices.len() == 1 {
                vec![raw_indices[0]]
            } else {
                raw_indices.to_vec()
            }
        });
    }

    let total = output_dims
        .iter()
        .try_fold(1usize, |acc, dim| acc.checked_mul(*dim as usize))?;

    if raw_indices.len() == 1 && output_dims.len() == 1 {
        let index = raw_indices[0];
        if index >= 1 && index <= total {
            return Some(vec![index]);
        }
        return None;
    }

    if raw_indices.len() != output_dims.len() {
        return None;
    }

    let mut flat = 0usize;
    for (idx, dim) in raw_indices.iter().zip(output_dims.iter()) {
        if *dim < 0 {
            return None;
        }
        let dim_usize = *dim as usize;
        if *idx == 0 || *idx > dim_usize {
            return None;
        }
        flat = flat.checked_mul(dim_usize)?;
        flat = flat.checked_add(*idx - 1)?;
    }
    Some(vec![flat + 1])
}

fn scope_indices_for_projection(
    output_dims: &[i64],
    raw_indices: &[usize],
    normalized_indices: &[usize],
) -> Vec<usize> {
    if output_dims.is_empty() || raw_indices.len() == output_dims.len() {
        return raw_indices.to_vec();
    }

    let Some(linear_index) = normalized_indices.first().copied() else {
        return raw_indices.to_vec();
    };
    if output_dims.iter().any(|dim| *dim <= 0) {
        return raw_indices.to_vec();
    }

    // MLS §10.6: array-valued function outputs retain their declared
    // dimensions. Function bodies assign `R[3,3]`, not a flattened `R[9]`
    // pseudo-component, so projected calls must use dimensional subscripts
    // when reading the inlined function scope.
    let mut remainder = linear_index.saturating_sub(1);
    let mut indices = vec![1usize; output_dims.len()];
    for dim_idx in (0..output_dims.len()).rev() {
        let dim = output_dims[dim_idx].max(1) as usize;
        indices[dim_idx] = remainder % dim + 1;
        remainder /= dim;
    }
    indices
}

fn format_projection_base_scope_key(projection: &FunctionOutputProjection) -> String {
    if let Some(field) = projection.output_field.as_ref() {
        format!("{}.{}", projection.output_name, field)
    } else {
        projection.output_name.clone()
    }
}

fn format_projection_scope_key(projection: &FunctionOutputProjection) -> String {
    if projection.scope_indices.is_empty() {
        return format_projection_base_scope_key(projection);
    }
    format!(
        "{}[{}]",
        format_projection_base_scope_key(projection),
        projection
            .scope_indices
            .iter()
            .map(std::string::ToString::to_string)
            .collect::<Vec<_>>()
            .join(",")
    )
}
