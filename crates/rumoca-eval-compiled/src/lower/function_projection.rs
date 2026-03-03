use super::*;

#[derive(Debug, Clone)]
pub(super) struct FunctionOutputProjection {
    pub(super) base_function_name: dae::VarName,
    pub(super) output_name: String,
    pub(super) output_field: Option<String>,
    pub(super) indices: Vec<usize>,
}

impl<'a> LowerBuilder<'a> {
    pub(super) fn lookup_function_output_projection(
        &self,
        name: &dae::VarName,
    ) -> Option<FunctionOutputProjection> {
        let requested = name.as_str();
        let mut split_positions: Vec<usize> =
            requested.match_indices('.').map(|(idx, _)| idx).collect();
        split_positions.reverse();
        for split_idx in split_positions {
            let base_name = &requested[..split_idx];
            let suffix = &requested[split_idx + 1..];
            let base_var = dae::VarName::new(base_name);
            let Some(function) = self.lookup_function(&base_var) else {
                continue;
            };
            let Some((output_name, output_field, raw_indices)) =
                parse_output_projection_suffix(suffix)
            else {
                continue;
            };

            let Some(output) = function.outputs.iter().find(|out| out.name == output_name) else {
                continue;
            };
            if let Some(field) = output_field.as_deref()
                && (!output_is_complex_record(output) || !matches!(field, "re" | "im"))
            {
                continue;
            }

            let Some(indices) = normalize_projection_indices(&output.dims, &raw_indices) else {
                continue;
            };
            return Some(FunctionOutputProjection {
                base_function_name: function.name.clone(),
                output_name,
                output_field,
                indices,
            });
        }
        None
    }

    pub(super) fn lower_projected_function_call(
        &mut self,
        projection: &FunctionOutputProjection,
        args: &[dae::Expression],
        caller_scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        let Some(function) = self
            .lookup_function(&projection.base_function_name)
            .cloned()
        else {
            return Err(LowerError::MissingFunction {
                name: projection.base_function_name.as_str().to_string(),
            });
        };

        if function.external.is_some() {
            if let Some(reg) = self.try_lower_intrinsic_function_call(
                &projection.base_function_name,
                args,
                caller_scope,
                call_depth,
            )? && projection.indices.is_empty()
            {
                return Ok(reg);
            }
            return Err(LowerError::Unsupported {
                reason: format!(
                    "external function call `{}` cannot be inlined in PR2",
                    projection.base_function_name.as_str()
                ),
            });
        }

        let mut scope = Scope::new();

        for (idx, input) in function.inputs.iter().enumerate() {
            let reg = if let Some(arg_expr) = args.get(idx) {
                self.lower_expr(arg_expr, caller_scope, call_depth + 1)?
            } else if let Some(default) = input.default.as_ref() {
                self.lower_expr(default, &scope, call_depth + 1)?
            } else {
                self.emit_const(0.0)
            };
            scope.insert(input.name.clone(), reg);
        }

        for param in function.outputs.iter().chain(function.locals.iter()) {
            let values = if let Some(default) = param.default.as_ref() {
                self.lower_array_like_values(default, &scope, call_depth + 1)?
            } else {
                vec![self.emit_const(0.0)]
            };
            self.bind_assignment_values(&mut scope, &param.name, &values);
        }

        let _returned = self.lower_statements(&function.body, &mut scope, call_depth + 1)?;

        let projection_key = format_projection_scope_key(projection);
        if let Some(reg) = scope.get(&projection_key).copied() {
            return Ok(reg);
        }

        if projection.indices.len() == 1 && projection.indices[0] == 1 {
            let base_key = format_projection_base_scope_key(projection);
            if let Some(reg) = scope.get(&base_key).copied() {
                return Ok(reg);
            }
        }

        Err(LowerError::InvalidFunction {
            name: projection.base_function_name.as_str().to_string(),
            reason: format!(
                "projected output `{}` could not be resolved",
                projection_key
            ),
        })
    }

    pub(super) fn bind_assignment_values(
        &mut self,
        scope: &mut Scope,
        target: &str,
        values: &[Reg],
    ) {
        clear_indexed_scope_bindings(scope, target);
        if values.is_empty() {
            scope.insert(target.to_string(), self.emit_const(0.0));
            return;
        }

        scope.insert(target.to_string(), values[0]);
        for (idx, value) in values.iter().enumerate() {
            let key = format_subscript_binding_key(target, &[idx + 1]);
            scope.insert(key, *value);
        }
    }
}

pub(super) fn format_subscript_binding_key(base: &str, indices: &[usize]) -> String {
    if indices.len() == 1 {
        format!("{base}[{}]", indices[0])
    } else {
        let suffix = indices
            .iter()
            .map(std::string::ToString::to_string)
            .collect::<Vec<_>>()
            .join(",");
        format!("{base}[{suffix}]")
    }
}

fn clear_indexed_scope_bindings(scope: &mut Scope, target: &str) {
    let prefix = format!("{target}[");
    let keys = scope
        .keys()
        .filter(|key| key.starts_with(&prefix))
        .cloned()
        .collect::<Vec<_>>();
    for key in keys {
        scope.shift_remove(&key);
    }
}

fn parse_output_projection_suffix(suffix: &str) -> Option<(String, Option<String>, Vec<usize>)> {
    if suffix.is_empty() {
        return None;
    }

    let (output_with_field, indices) = if let Some(open) = suffix.find('[') {
        if !suffix.ends_with(']') || open == 0 {
            return None;
        }
        let inner = &suffix[open + 1..suffix.len() - 1];
        let parsed_indices = inner
            .split(',')
            .map(str::trim)
            .map(|token| token.parse::<usize>().ok())
            .collect::<Option<Vec<_>>>()?;
        (suffix[..open].to_string(), parsed_indices)
    } else {
        (suffix.to_string(), Vec::new())
    };

    if let Some((output_name, field)) = output_with_field.split_once('.') {
        if output_name.is_empty() || field.is_empty() {
            return None;
        }
        return Some((output_name.to_string(), Some(field.to_string()), indices));
    }

    Some((output_with_field, None, indices))
}

fn output_is_complex_record(output: &dae::FunctionParam) -> bool {
    output
        .type_name
        .rsplit('.')
        .next()
        .is_some_and(|leaf| leaf == "Complex")
}

fn normalize_projection_indices(output_dims: &[i64], raw_indices: &[usize]) -> Option<Vec<usize>> {
    if output_dims.is_empty() {
        return raw_indices.is_empty().then_some(Vec::new());
    }
    if raw_indices.is_empty() {
        return None;
    }

    let total = output_dims.iter().try_fold(1usize, |acc, dim| {
        if *dim <= 0 {
            None
        } else {
            acc.checked_mul(*dim as usize)
        }
    })?;

    if raw_indices.len() == 1 {
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
        if *dim <= 0 {
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

fn format_projection_base_scope_key(projection: &FunctionOutputProjection) -> String {
    if let Some(field) = projection.output_field.as_ref() {
        format!("{}.{}", projection.output_name, field)
    } else {
        projection.output_name.clone()
    }
}

fn format_projection_scope_key(projection: &FunctionOutputProjection) -> String {
    if projection.indices.is_empty() {
        return format_projection_base_scope_key(projection);
    }
    format!(
        "{}[{}]",
        format_projection_base_scope_key(projection),
        projection
            .indices
            .iter()
            .map(std::string::ToString::to_string)
            .collect::<Vec<_>>()
            .join(",")
    )
}
