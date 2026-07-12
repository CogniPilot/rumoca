use super::*;

pub(in crate::lower) fn expression_binding_expressions(
    expr: &rumoca_core::Expression,
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
    owner_span: rumoca_core::Span,
) -> Result<Option<Vec<rumoca_core::Expression>>, LowerError> {
    let expr_span = projection_expr_or_owner_span(expr, owner_span)?;
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => Ok(Some(binding_expressions_for_subscripted_reference(
            name,
            subscripts,
            expr_span,
            dae_model,
            structural_bindings,
        )?)),
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            let Some(name) = binding_base_reference(base) else {
                return Ok(None);
            };
            Ok(Some(binding_expressions_for_subscripted_reference(
                name,
                subscripts,
                expr_span,
                dae_model,
                structural_bindings,
            )?))
        }
        _ => Ok(None),
    }
}

fn binding_base_reference(expr: &rumoca_core::Expression) -> Option<&rumoca_core::Reference> {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => Some(name),
        _ => None,
    }
}

fn binding_expressions_for_subscripted_reference(
    name: &rumoca_core::Reference,
    subscripts: &[rumoca_core::Subscript],
    span: rumoca_core::Span,
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
) -> Result<Vec<rumoca_core::Expression>, LowerError> {
    if let Some(dims) = variable_dims(dae_model, name.as_str())? {
        let selections = if subscripts.is_empty() {
            let mut selections = derivative_vec_with_capacity(
                dims.len(),
                "derivative slice selection dimension count",
                span,
            )?;
            for dim in dims {
                selections.push(one_based_index_range(
                    dim,
                    "derivative full-slice index count",
                    span,
                )?);
            }
            selections
        } else {
            slice_selections(subscripts, &dims, structural_bindings, span)?
        };
        let expression_count = slice_selection_count(&selections, span)?;
        let mut expressions = derivative_vec_with_capacity(
            expression_count,
            "derivative slice expression count",
            span,
        )?;
        let mut current =
            derivative_vec_with_capacity(selections.len(), "derivative slice index depth", span)?;
        collect_slice_reference_expressions(
            name,
            span,
            &selections,
            0,
            &mut current,
            &mut expressions,
        )?;
        return Ok(expressions);
    }

    if let Some(dims) = scalarized_child_dims(dae_model, name.as_str(), span)? {
        let selections = if subscripts.is_empty() {
            let mut selections = derivative_vec_with_capacity(
                dims.len(),
                "scalarized derivative slice selection dimension count",
                span,
            )?;
            for dim in dims {
                selections.push(one_based_index_range(
                    dim,
                    "scalarized derivative full-slice index count",
                    span,
                )?);
            }
            selections
        } else {
            slice_selections(subscripts, &dims, structural_bindings, span)?
        };
        let key_count = slice_selection_count(&selections, span)?;
        let mut keys =
            derivative_vec_with_capacity(key_count, "scalarized derivative slice key count", span)?;
        let mut current =
            derivative_vec_with_capacity(selections.len(), "derivative slice key depth", span)?;
        collect_slice_keys(name.as_str(), &selections, span, 0, &mut current, &mut keys)?;
        let mut expressions = derivative_vec_with_capacity(
            keys.len(),
            "scalarized derivative slice expression count",
            span,
        )?;
        for key in keys {
            let variable = variable_by_name(dae_model, &key)
                .ok_or_else(|| LowerError::MissingBinding { name: key.clone() })?;
            expressions.push(dae_variable_ref_expr(&key, variable, span, Vec::new())?);
        }
        return Ok(expressions);
    }

    if subscripts.is_empty() {
        return scalar_binding_expression(name, span, dae_model);
    }

    let indices = binding_subscript_indices(name, subscripts, structural_bindings, span)?;
    let scalarized_key = dae::format_subscript_key(name.as_str(), &indices);
    let variable =
        variable_by_name(dae_model, &scalarized_key).ok_or_else(|| LowerError::MissingBinding {
            name: scalarized_key.clone(),
        })?;
    single_expression_vec(
        dae_variable_ref_expr(&scalarized_key, variable, span, Vec::new())?,
        "derivative scalarized binding expression count",
        span,
    )
}

fn scalar_binding_expression(
    name: &rumoca_core::Reference,
    span: rumoca_core::Span,
    dae_model: &dae::Dae,
) -> Result<Vec<rumoca_core::Expression>, LowerError> {
    if let Some(variable) = variable_by_name(dae_model, name.as_str()) {
        return single_expression_vec(
            dae_variable_ref_expr(name.as_str(), variable, span, Vec::new())?,
            "derivative scalar binding expression count",
            span,
        );
    }
    let Some(variable) = scalarized_aggregate_variable(dae_model, name.as_str(), span)? else {
        return Err(LowerError::MissingBinding {
            name: name.as_str().to_string(),
        });
    };
    let scalar = rumoca_core::parse_scalar_name(name.as_str()).ok_or_else(|| {
        LowerError::contract_violation(
            format!("validated scalar binding `{name}` lost its scalar suffix"),
            span,
        )
    })?;
    let scalar_subscripts = scalar
        .indices
        .iter()
        .copied()
        .map(|index| {
            checked_generated_derivative_subscript(
                index,
                span,
                "scalarized aggregate binding index",
            )
        })
        .collect::<Result<Vec<_>, _>>()?;
    single_expression_vec(
        dae_variable_ref_expr(scalar.base, variable, span, scalar_subscripts)?,
        "derivative scalarized aggregate binding expression count",
        span,
    )
}

fn binding_subscript_indices(
    name: &rumoca_core::Reference,
    subscripts: &[rumoca_core::Subscript],
    structural_bindings: &IndexMap<String, f64>,
    span: rumoca_core::Span,
) -> Result<Vec<usize>, LowerError> {
    compile_time_subscript_indices_with_owner(subscripts, structural_bindings, span).map_err(
        |err| {
            err.with_context(format!(
                "resolve derivative binding slice `{}`",
                name.as_str()
            ))
        },
    )
}

fn collect_slice_reference_expressions(
    name: &rumoca_core::Reference,
    span: rumoca_core::Span,
    selections: &[Vec<usize>],
    depth: usize,
    current: &mut Vec<usize>,
    expressions: &mut Vec<rumoca_core::Expression>,
) -> Result<(), LowerError> {
    if depth == selections.len() {
        let mut subscripts = derivative_vec_with_capacity(
            current.len(),
            "derivative slice reference subscript count",
            span,
        )?;
        for index in current.iter().copied() {
            let index = checked_usize_to_i64(index, "derivative slice reference subscript", span)?;
            subscripts.push(rumoca_core::Subscript::index(index, span));
        }
        expressions.push(rumoca_core::Expression::VarRef {
            name: name.clone(),
            subscripts,
            span,
        });
        return Ok(());
    }
    for &index in &selections[depth] {
        current.push(index);
        collect_slice_reference_expressions(
            name,
            span,
            selections,
            depth + 1,
            current,
            expressions,
        )?;
        current.pop();
    }
    Ok(())
}

pub(in crate::lower) fn dae_variable_ref_expr(
    key: &str,
    variable: &dae::Variable,
    span: rumoca_core::Span,
    subscripts: Vec<rumoca_core::Subscript>,
) -> Result<rumoca_core::Expression, LowerError> {
    let name = match variable.origin {
        dae::VariableOrigin::Generated => rumoca_core::Reference::generated(key),
        dae::VariableOrigin::Source => {
            let component_ref =
                variable
                    .component_ref
                    .clone()
                    .ok_or_else(|| {
                        LowerError::contract_violation(
                            format!(
                            "source DAE variable `{key}` lost structured component-reference metadata before derivative projection"
                        ),
                            span,
                        )
                    })?;
            rumoca_core::Reference::from_component_reference(component_ref)
        }
    };
    Ok(rumoca_core::Expression::VarRef {
        name,
        subscripts,
        span,
    })
}

pub(in crate::lower) fn binding_base_name(
    expr: &rumoca_core::Expression,
    owner_span: rumoca_core::Span,
) -> Result<String, LowerError> {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => Ok(name.as_str().to_string()),
        _ => Err(unsupported_at(
            "unsupported sliced derivative binding base",
            projection_expr_or_owner_span(expr, owner_span)?,
        )),
    }
}

pub(in crate::lower) fn binding_keys_for_subscripted_name(
    base: &str,
    subscripts: &[rumoca_core::Subscript],
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
    fallback_span: rumoca_core::Span,
) -> Result<Vec<String>, LowerError> {
    let Some(dims) = variable_dims(dae_model, base)? else {
        if subscripts.is_empty() {
            if variable_by_name(dae_model, base).is_some() {
                return Ok(vec![base.to_string()]);
            }
            if scalarized_aggregate_variable(dae_model, base, fallback_span)?.is_some() {
                return Ok(vec![base.to_string()]);
            }
            if let Some(dims) = scalarized_child_dims(dae_model, base, fallback_span)? {
                return scalar_keys_for_dims(base, &dims, fallback_span);
            }
            return Err(LowerError::MissingBinding {
                name: base.to_string(),
            });
        }
        let span = subscript_list_span_or_owner(subscripts, fallback_span)?;
        let dims = scalarized_child_dims(dae_model, base, span)?;
        if let Some(dims) = dims {
            let selections =
                slice_selections(subscripts, &dims, structural_bindings, fallback_span)?;
            let key_count = slice_selection_count(&selections, span)?;
            let mut keys = derivative_vec_with_capacity(
                key_count,
                "scalarized binding slice key count",
                span,
            )?;
            let mut current = derivative_vec_with_capacity(
                selections.len(),
                "scalarized binding slice key depth",
                span,
            )?;
            collect_slice_keys(base, &selections, span, 0, &mut current, &mut keys)?;
            return Ok(keys);
        }
        let scalarized_key =
            scalarized_binding_key(base, subscripts, structural_bindings, fallback_span)?;
        if variable_by_name(dae_model, &scalarized_key).is_some() {
            return Ok(vec![scalarized_key]);
        }
        return Err(LowerError::MissingBinding {
            name: scalarized_key,
        });
    };
    if subscripts.is_empty() {
        return scalar_keys_for_dims(base, &dims, fallback_span);
    }
    let span = subscript_list_span_or_owner(subscripts, fallback_span)?;
    let selections = slice_selections(subscripts, &dims, structural_bindings, span)?;
    let key_count = slice_selection_count(&selections, span)?;
    let mut keys = derivative_vec_with_capacity(key_count, "binding slice key count", span)?;
    let mut current =
        derivative_vec_with_capacity(selections.len(), "binding slice key depth", span)?;
    collect_slice_keys(base, &selections, span, 0, &mut current, &mut keys)?;
    Ok(keys)
}

pub(in crate::lower) fn scalarized_binding_key(
    base: &str,
    subscripts: &[rumoca_core::Subscript],
    structural_bindings: &IndexMap<String, f64>,
    owner_span: rumoca_core::Span,
) -> Result<String, LowerError> {
    let span = subscript_list_span_or_owner(subscripts, owner_span)?;
    let indices = compile_time_subscript_indices_with_owner(subscripts, structural_bindings, span)
        .map_err(|err| err.with_context(format!("resolve scalarized binding key `{base}`")))?;
    Ok(dae::format_subscript_key(base, &indices))
}

pub(in crate::lower) fn variable_by_name<'a>(
    dae_model: &'a dae::Dae,
    base: &str,
) -> Option<&'a dae::Variable> {
    let name = rumoca_core::VarName::new(base);
    dae_model
        .variables
        .states
        .get(&name)
        .or_else(|| dae_model.variables.algebraics.get(&name))
        .or_else(|| dae_model.variables.outputs.get(&name))
        .or_else(|| dae_model.variables.inputs.get(&name))
        .or_else(|| dae_model.variables.parameters.get(&name))
        .or_else(|| dae_model.variables.constants.get(&name))
        .or_else(|| dae_model.variables.discrete_reals.get(&name))
        .or_else(|| dae_model.variables.discrete_valued.get(&name))
}

pub(in crate::lower) fn scalarized_aggregate_variable<'a>(
    dae_model: &'a dae::Dae,
    name: &str,
    span: rumoca_core::Span,
) -> Result<Option<&'a dae::Variable>, LowerError> {
    let Some(scalar) = rumoca_core::parse_scalar_name(name) else {
        return Ok(None);
    };
    let Some(variable) = variable_by_name(dae_model, scalar.base) else {
        return Ok(None);
    };
    variable.try_size().map_err(variable_shape_contract_error)?;
    if scalar.indices.len() != variable.dims.len() {
        return Err(LowerError::contract_violation(
            format!(
                "scalarized DAE binding `{name}` has rank {}, but aggregate `{}` has rank {}",
                scalar.indices.len(),
                scalar.base,
                variable.dims.len()
            ),
            span,
        ));
    }
    for (axis, (&index, &dim)) in scalar.indices.iter().zip(&variable.dims).enumerate() {
        if index <= 0 || index > dim {
            return Err(LowerError::contract_violation(
                format!(
                    "scalarized DAE binding `{name}` index {index} is out of bounds for dimension {} of `{}`",
                    axis + 1,
                    scalar.base
                ),
                span,
            ));
        }
    }
    Ok(Some(variable))
}

pub(in crate::lower) fn variable_dims(
    dae_model: &dae::Dae,
    base: &str,
) -> Result<Option<Vec<usize>>, LowerError> {
    let Some(var) = variable_by_name(dae_model, base) else {
        return Ok(None);
    };
    if var.dims.is_empty() {
        return Ok(None);
    }
    variable_dims_from_metadata(var).map(Some)
}

fn variable_dims_from_metadata(var: &dae::Variable) -> Result<Vec<usize>, LowerError> {
    var.try_size().map_err(variable_shape_contract_error)?;
    let mut dims = derivative_vec_with_capacity(
        var.dims.len(),
        "DAE variable dimension count",
        var.source_span,
    )?;
    for dim in &var.dims {
        dims.push(usize::try_from(*dim).map_err(|_| {
            LowerError::contract_violation(
                format!("DAE variable `{}` has invalid dimension {dim}", var.name),
                var.source_span,
            )
        })?);
    }
    checked_usize_scalar_count(&dims, "DAE variable dimensions", var.source_span)?;
    Ok(dims)
}

fn variable_shape_contract_error(err: dae::VariableShapeContractError) -> LowerError {
    LowerError::contract_violation(err.to_string(), err.span())
}

pub(super) fn scalarized_child_dims(
    dae_model: &dae::Dae,
    base: &str,
    fallback_span: rumoca_core::Span,
) -> Result<Option<Vec<usize>>, LowerError> {
    let mut indices = scalarized_child_indices(dae_model, base, fallback_span)?;
    if indices.is_empty() {
        return Ok(None);
    }
    indices.sort();
    indices.dedup();
    let Some(first) = indices.first() else {
        return Ok(None);
    };
    let rank = first.len();
    if rank == 0 || !indices.iter().all(|index| index.len() == rank) {
        return Ok(None);
    }
    let mut dims =
        derivative_vec_with_capacity(rank, "scalarized child dimension count", fallback_span)?;
    dims.extend(std::iter::repeat_n(0usize, rank));
    for index in &indices {
        for (dim, value) in dims.iter_mut().zip(index.iter().copied()) {
            *dim = (*dim).max(value);
        }
    }
    let size = checked_usize_scalar_count(&dims, "scalarized child dimensions", fallback_span)?;
    if size != indices.len() {
        return Ok(None);
    }
    let dims_i64 = checked_usize_dims_to_i64(&dims, "scalarized child dimension", fallback_span)?;
    let covers_compact_domain = (0..size).all(|flat_index| {
        dae::flat_index_to_subscripts(&dims_i64, flat_index)
            .is_some_and(|subscripts| indices.binary_search(&subscripts).is_ok())
    });
    Ok(covers_compact_domain.then_some(dims))
}

pub(super) fn scalarized_record_root_exists(dae_model: &dae::Dae, base: &str) -> bool {
    let field_prefix = format!("{base}.");
    dae_variable_names(dae_model).any(|name| name.as_str().starts_with(&field_prefix))
}

fn scalarized_child_indices(
    dae_model: &dae::Dae,
    base: &str,
    fallback_span: rumoca_core::Span,
) -> Result<Vec<Vec<usize>>, LowerError> {
    let span = fallback_span;
    let mut indices = derivative_vec_with_capacity(0, "scalarized child index count", span)?;
    for name in dae_variable_names(dae_model) {
        if dae::component_base_name(name.as_str()).as_deref() != Some(base) {
            continue;
        }
        let Some(parsed) = dae::parse_embedded_subscripts(name.as_str()) else {
            continue;
        };
        let Some(index) = scalarized_child_index_from_subscripts(&parsed, span)? else {
            continue;
        };
        reserve_derivative_capacity(&mut indices, 1, "scalarized child index count", span)?;
        indices.push(index);
    }
    Ok(indices)
}

fn scalarized_child_index_from_subscripts(
    parsed: &[i64],
    span: rumoca_core::Span,
) -> Result<Option<Vec<usize>>, LowerError> {
    let mut index =
        derivative_vec_with_capacity(parsed.len(), "scalarized child subscript count", span)?;
    for value in parsed {
        let Ok(value) = usize::try_from(*value) else {
            return Ok(None);
        };
        if value == 0 {
            return Ok(None);
        }
        index.push(value);
    }
    Ok(Some(index))
}

fn dae_variable_names(dae_model: &dae::Dae) -> impl Iterator<Item = &rumoca_core::VarName> {
    dae_model
        .variables
        .states
        .keys()
        .chain(dae_model.variables.algebraics.keys())
        .chain(dae_model.variables.outputs.keys())
        .chain(dae_model.variables.inputs.keys())
        .chain(dae_model.variables.parameters.keys())
        .chain(dae_model.variables.constants.keys())
        .chain(dae_model.variables.discrete_reals.keys())
        .chain(dae_model.variables.discrete_valued.keys())
}

pub(in crate::lower) fn indexed_rhs_expressions(
    expr: &rumoca_core::Expression,
    target: &rumoca_core::Expression,
    expected: usize,
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
    owner_span: rumoca_core::Span,
) -> Result<Vec<rumoca_core::Expression>, LowerError> {
    let span = projection_expr_pair_or_owner_span(target, expr, owner_span)?;
    let dims = derivative_target_result_dims(target, dae_model, structural_bindings, span)?;
    if checked_usize_scalar_count(&dims, "array derivative RHS target shape", span)? != expected {
        return Err(unsupported_at(
            "array derivative RHS shape does not match target shape",
            span,
        ));
    }
    let dims_i64 = checked_usize_dims_to_i64(&dims, "array derivative RHS dimension", span)?;
    let mut expressions =
        derivative_vec_with_capacity(expected, "indexed derivative RHS expression count", span)?;
    for flat_index in 0..expected {
        let Some(indices) = dae::flat_index_to_subscripts(&dims_i64, flat_index) else {
            return Err(unsupported_at(
                "array derivative RHS could not compute scalar subscripts",
                span,
            ));
        };
        let mut subscripts = derivative_vec_with_capacity(
            indices.len(),
            "indexed derivative RHS generated subscript count",
            span,
        )?;
        for index in indices {
            let index =
                checked_usize_to_i64(index, "array derivative RHS generated subscript", span)?;
            subscripts.push(checked_generated_derivative_subscript(
                index,
                span,
                "array derivative RHS generated subscript",
            )?);
        }
        expressions.push(rumoca_core::Expression::Index {
            base: Box::new(expr.clone()),
            subscripts,
            span,
        });
    }
    Ok(expressions)
}

pub(in crate::lower) fn derivative_target_result_dims(
    target: &rumoca_core::Expression,
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
    owner_span: rumoca_core::Span,
) -> Result<Vec<usize>, LowerError> {
    let span = projection_expr_or_owner_span(target, owner_span)?;
    match target {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => result_dims_for_subscripted_binding(
            name.as_str(),
            subscripts,
            dae_model,
            structural_bindings,
            span,
        ),
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            let base = binding_base_name(base, span)?;
            result_dims_for_subscripted_binding(
                &base,
                subscripts,
                dae_model,
                structural_bindings,
                span,
            )
        }
        _ => Err(unsupported_at(
            "array derivative RHS target shape is unsupported",
            span,
        )),
    }
}

pub(in crate::lower) fn result_dims_for_subscripted_binding(
    base: &str,
    subscripts: &[rumoca_core::Subscript],
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
    fallback_span: rumoca_core::Span,
) -> Result<Vec<usize>, LowerError> {
    let Some(dims) = variable_dims(dae_model, base)? else {
        return Err(unsupported_at(
            "array derivative RHS target has no array shape",
            fallback_span,
        ));
    };
    if subscripts.is_empty() {
        return Ok(dims);
    }
    result_dims_for_subscripts(&dims, subscripts, structural_bindings, fallback_span)
}

pub(in crate::lower) fn result_dims_for_subscripts(
    dims: &[usize],
    subscripts: &[rumoca_core::Subscript],
    structural_bindings: &IndexMap<String, f64>,
    fallback_span: rumoca_core::Span,
) -> Result<Vec<usize>, LowerError> {
    if subscripts.is_empty() {
        let mut copied = derivative_vec_with_capacity(
            dims.len(),
            "derivative result dimension count",
            fallback_span,
        )?;
        copied.extend_from_slice(dims);
        return Ok(copied);
    }
    let selections = slice_selections(subscripts, dims, structural_bindings, fallback_span)?;
    let span = subscript_list_span_or_owner(subscripts, fallback_span)?;
    let mut result_dims =
        derivative_vec_with_capacity(selections.len(), "derivative slice result rank", span)?;
    for selection in selections {
        result_dims.push(selection.len());
    }
    Ok(result_dims)
}
