use super::*;

pub(super) fn exact_declared_function_output_dims(
    function: &rumoca_core::Function,
    call_span: rumoca_core::Span,
) -> Result<Option<Vec<i64>>, LowerError> {
    let [output] = function.outputs.as_slice() else {
        return Ok(None);
    };
    if output.dims.is_empty() || output.dims.iter().all(|dim| *dim > 0) {
        let span = if output.span.is_dummy() {
            call_span
        } else {
            output.span
        };
        Ok(Some(copy_projection_dims(
            &output.dims,
            "declared function output dimension count",
            span,
        )?))
    } else {
        Ok(None)
    }
}

pub(super) fn projected_declared_output_dims(
    output: &rumoca_core::FunctionParam,
    indices: &[usize],
    call_span: rumoca_core::Span,
) -> Result<Option<Vec<i64>>, LowerError> {
    if !output.dims.iter().all(|dim| *dim > 0) {
        return Ok(None);
    }
    let output_span = if output.span.is_dummy() {
        call_span
    } else {
        output.span
    };
    if indices.is_empty() {
        return copy_projection_dims(
            &output.dims,
            "projected function output dimension count",
            output_span,
        )
        .map(Some);
    }
    if indices.len() > output.dims.len() {
        return Err(LowerError::contract_violation(
            format!(
                "projected function output `{}` has {} indices for {} dimensions",
                output.name,
                indices.len(),
                output.dims.len()
            ),
            call_span,
        ));
    }
    for (index, dim) in indices.iter().zip(output.dims.iter()) {
        let dim = usize::try_from(*dim).map_err(|_| {
            LowerError::contract_violation(
                format!(
                    "projected function output `{}` has invalid dimension {dim}",
                    output.name
                ),
                output.span,
            )
        })?;
        if *index == 0 || *index > dim {
            return Err(LowerError::contract_violation(
                format!(
                    "projected function output `{}` index {index} is outside dimension {dim}",
                    output.name
                ),
                call_span,
            ));
        }
    }
    copy_projection_dims(
        &output.dims[indices.len()..],
        "projected function output remaining dimension count",
        output_span,
    )
    .map(Some)
}

pub(super) fn is_same_plain_var_ref(expr: &rumoca_core::Expression, name: &str) -> bool {
    matches!(
        expr,
        rumoca_core::Expression::VarRef {
            name: expr_name,
            subscripts,
            ..
        } if subscripts.is_empty() && expr_name.as_str() == name
    )
}

pub(super) fn named_argument_spans(
    args: &[rumoca_core::Expression],
    owner_span: rumoca_core::Span,
) -> Result<IndexMap<String, rumoca_core::Span>, LowerError> {
    let span = args
        .first()
        .map(|arg| inherited_projection_source_span(arg.span(), owner_span))
        .unwrap_or(owner_span);
    let mut spans = IndexMap::new();
    spans.try_reserve(args.len()).map_err(|_| {
        LowerError::contract_violation(
            "named argument span count capacity exceeds host memory limits",
            span,
        )
    })?;
    for arg in args {
        let rumoca_core::Expression::FunctionCall { name, span, .. } = arg else {
            continue;
        };
        let Some(named) = name.as_str().strip_prefix(NAMED_FUNCTION_ARG_PREFIX) else {
            continue;
        };
        spans.insert(named.to_string(), *span);
    }
    Ok(spans)
}

pub(super) fn named_actual_span(
    named_spans: &IndexMap<String, rumoca_core::Span>,
    input: &rumoca_core::FunctionParam,
    actual: &rumoca_core::Expression,
) -> rumoca_core::Span {
    actual
        .span()
        .or_else(|| named_spans.get(input.name.as_str()).copied())
        .unwrap_or(input.span)
}

pub(super) fn single_field_path(
    name: &str,
    span: rumoca_core::Span,
) -> Result<Vec<String>, LowerError> {
    let mut path = projection_vec_with_capacity(1, "projected field path count", span)?;
    path.push(name.to_string());
    Ok(path)
}

pub(super) fn reserve_projection_capacity<T>(
    values: &mut Vec<T>,
    additional: usize,
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<(), LowerError> {
    values.try_reserve_exact(additional).map_err(|_| {
        LowerError::contract_violation(
            format!("{context} capacity exceeds host memory limits"),
            span,
        )
    })
}

pub(super) fn append_projected_outputs(
    outputs: &mut Vec<ProjectedFunctionOutput>,
    mut additional: Vec<ProjectedFunctionOutput>,
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<(), LowerError> {
    reserve_projection_capacity(outputs, additional.len(), context, span)?;
    outputs.append(&mut additional);
    Ok(())
}

pub(super) fn declared_dims(
    function: &rumoca_core::Function,
    name: &str,
) -> Result<Option<Vec<i64>>, LowerError> {
    Ok(declared_param_dims(function, name)?.filter(|dims| !dims.is_empty()))
}

pub(super) fn declared_param_dims(
    function: &rumoca_core::Function,
    name: &str,
) -> Result<Option<Vec<i64>>, LowerError> {
    let Some(param) = function
        .outputs
        .iter()
        .chain(function.locals.iter())
        .chain(function.inputs.iter())
        .find(|param| param.name == name)
    else {
        return Ok(None);
    };
    Ok(Some(copy_projection_dims(
        &param.dims,
        "declared parameter dimension count",
        param.span,
    )?))
}

pub(super) fn formal_actual_projection_dims(
    formal: &rumoca_core::FunctionParam,
    actual_dims: Option<Vec<i64>>,
    context: String,
    span: rumoca_core::Span,
) -> Result<Option<Vec<i64>>, LowerError> {
    if let Some(actual_dims) = actual_dims {
        if actual_dims == formal.dims {
            return Ok(Some(actual_dims));
        }
        if let Some(resolved_dims) =
            resolve_formal_projection_dims(&formal.dims, &actual_dims, &context, span)?
        {
            return Ok(Some(resolved_dims));
        }
        if formal.dims.is_empty() && formal_accepts_structured_actual(formal) {
            return Ok(Some(actual_dims));
        }
        return Err(dimension_mismatch_error(
            &context,
            &formal.dims,
            &actual_dims,
            span,
        ));
    }
    if formal.dims.is_empty() {
        Ok(None)
    } else {
        Ok(Some(copy_projection_dims(
            &formal.dims,
            "formal parameter dimension count",
            formal.span,
        )?))
    }
}

fn resolve_formal_projection_dims(
    formal_dims: &[i64],
    actual_dims: &[i64],
    context: &str,
    span: rumoca_core::Span,
) -> Result<Option<Vec<i64>>, LowerError> {
    if formal_dims.len() != actual_dims.len() || formal_dims.iter().all(|dim| *dim > 0) {
        return Ok(None);
    }
    let mut resolved = projection_vec_with_capacity(
        formal_dims.len(),
        "resolved formal projection dimension count",
        span,
    )?;
    for (formal, actual) in formal_dims.iter().zip(actual_dims) {
        if *formal > 0 {
            if formal != actual {
                return Ok(None);
            }
            resolved.push(*formal);
        } else if *actual < 0 {
            return Err(LowerError::contract_violation(
                format!("{context} has negative actual dimension {actual}"),
                span,
            ));
        } else {
            resolved.push(*actual);
        }
    }
    Ok(Some(resolved))
}

pub(super) fn constructor_input_projection_dims(
    input: &rumoca_core::FunctionParam,
    actual_dims: Option<Vec<i64>>,
    span: rumoca_core::Span,
) -> Result<Option<Vec<i64>>, LowerError> {
    let dims = formal_actual_projection_dims(
        input,
        actual_dims,
        format!("record constructor input `{}`", input.name),
        span,
    )?;
    match dims {
        Some(dims) => Ok(Some(dims)),
        None if formal_accepts_structured_actual(input) => Ok(None),
        None => Ok(Some(Vec::new())),
    }
}

pub(super) fn assignment_projection_dims(
    function: &rumoca_core::Function,
    target: &str,
    value_dims: Option<Vec<i64>>,
    span: rumoca_core::Span,
) -> Result<Option<Vec<i64>>, LowerError> {
    let declared = declared_param_dims(function, target)?;
    match (value_dims, declared) {
        (Some(value_dims), Some(declared)) if value_dims != declared => {
            let context = format!("function `{}` assignment to `{target}`", function.name);
            match resolve_formal_projection_dims(&declared, &value_dims, &context, span)? {
                Some(resolved) => Ok(Some(resolved)),
                None => Err(dimension_mismatch_error(
                    &context,
                    &declared,
                    &value_dims,
                    span,
                )),
            }
        }
        (Some(value_dims), _) if value_dims.is_empty() => Ok(Some(value_dims)),
        (Some(value_dims), _) => Ok(Some(value_dims)),
        (None, Some(declared)) => Ok(Some(declared)),
        (None, None) => Ok(None),
    }
}

pub(super) fn formal_accepts_structured_actual(formal: &rumoca_core::FunctionParam) -> bool {
    formal.type_class == Some(rumoca_core::ClassType::Record)
        || !matches!(
            formal.type_name.as_str(),
            "Real" | "Integer" | "Boolean" | "String"
        )
}

pub(super) fn dimension_mismatch_error(
    context: &str,
    expected: &[i64],
    actual: &[i64],
    span: rumoca_core::Span,
) -> LowerError {
    if !expected.is_empty()
        && !actual.is_empty()
        && let (Ok(expected_count), Ok(actual_count)) = (
            scalar_count_for_dims(expected, "expected array dimensions", span),
            scalar_count_for_dims(actual, "actual array dimensions", span),
        )
    {
        return unsupported_at(
            format!(
                "array expression shape {} requires {expected_count} scalar values, got {actual_count}",
                format_i64_dims(expected)
            ),
            span,
        );
    }
    unsupported_at(
        format!(
            "{context} expects dimensions {}, got {}",
            format_i64_dims(expected),
            format_i64_dims(actual)
        ),
        span,
    )
}

pub(super) fn is_ignorable_projection_statement(statement: &rumoca_core::Statement) -> bool {
    match statement {
        rumoca_core::Statement::Empty { .. } => true,
        rumoca_core::Statement::FunctionCall { comp, .. } => {
            comp.to_var_name().as_str() == "assert"
        }
        _ => false,
    }
}

pub(super) fn projection_assignment_target(
    component_ref: &rumoca_core::ComponentReference,
) -> Result<ProjectionAssignmentTarget, LowerError> {
    let span = component_ref.span;
    let mut base_ref = component_ref.clone();
    let last = base_ref.parts.last_mut().ok_or_else(|| {
        LowerError::contract_violation(
            "function assignment target has an empty component reference",
            span,
        )
    })?;
    if last.subs.is_empty() {
        return Ok(ProjectionAssignmentTarget {
            base: component_ref.to_var_name().as_str().to_string(),
            selectors: None,
            span,
        });
    }
    let mut selectors = projection_vec_with_capacity(
        last.subs.len(),
        "function assignment target subscript count",
        span,
    )?;
    for subscript in &last.subs {
        let selector = match subscript {
            rumoca_core::Subscript::Index { value, .. } if *value > 0 => {
                Ok(ProjectionAssignmentSelector::Index(*value))
            }
            rumoca_core::Subscript::Colon { .. } => Ok(ProjectionAssignmentSelector::All),
            _ => Err(unsupported_at(
                "dynamic function assignment target subscripts cannot be projected",
                subscript.span(),
            )),
        }?;
        selectors.push(selector);
    }
    last.subs.clear();
    Ok(ProjectionAssignmentTarget {
        base: rumoca_core::Reference::from_component_reference(base_ref)
            .as_str()
            .to_string(),
        selectors: Some(selectors),
        span,
    })
}

pub(super) struct FunctionScopeSubstituter<'a> {
    pub(super) scope: &'a FunctionProjectionScope,
    pub(super) materialize_arrays: bool,
    pub(super) error: Option<LowerError>,
}

impl FunctionScopeSubstituter<'_> {
    fn indexed_scope_value(
        &mut self,
        expr: &rumoca_core::Expression,
    ) -> Option<rumoca_core::Expression> {
        let rumoca_core::Expression::Index {
            base,
            subscripts,
            span,
        } = expr
        else {
            return None;
        };
        let rumoca_core::Expression::VarRef {
            name,
            subscripts: base_subscripts,
            ..
        } = base.as_ref()
        else {
            return None;
        };
        if !base_subscripts.is_empty() {
            return None;
        }
        let values = self.scope.scalars.get(name.as_str())?;
        let dims = self.scope.dims.get(name.as_str())?;
        let indices = subscripts
            .iter()
            .map(|subscript| match subscript {
                rumoca_core::Subscript::Index { value, .. } if *value > 0 => Some(*value),
                _ => None,
            })
            .collect::<Option<Vec<_>>>()?;
        match flat_index_from_indices(dims, &indices, *span, "projected indexed substitution") {
            Ok(Some(index)) => values
                .get(index)
                .cloned()
                .map(|value| value.with_span(*span)),
            Ok(None) => None,
            Err(error) => {
                self.error = Some(error);
                Some(expr.clone())
            }
        }
    }

    fn projected_array_binding(
        &mut self,
        expr: &rumoca_core::Expression,
        name: &rumoca_core::Reference,
        span: rumoca_core::Span,
    ) -> rumoca_core::Expression {
        if !self.materialize_arrays {
            return expr.clone();
        }
        let Some(values) = self.scope.scalars.get(name.as_str()) else {
            return expr.clone();
        };
        let Some(dims) = self.scope.dims.get(name.as_str()) else {
            self.error = Some(LowerError::contract_violation(
                format!(
                    "projected array `{}` has scalar values but no dimensions",
                    name.as_str()
                ),
                span,
            ));
            return expr.clone();
        };
        projected_array_expression(values, dims, span).unwrap_or_else(|error| {
            self.error = Some(error);
            expr.clone()
        })
    }
}

impl ExpressionRewriter for FunctionScopeSubstituter<'_> {
    fn rewrite_expression(&mut self, expr: &rumoca_core::Expression) -> rumoca_core::Expression {
        if let Some(value) = self.indexed_scope_value(expr) {
            return value;
        }
        let rumoca_core::Expression::VarRef {
            name,
            subscripts,
            span,
        } = expr
        else {
            return self.walk_expression(expr);
        };
        if !subscripts.is_empty() {
            return self.walk_expression(expr);
        }
        if self.scope.scalars.contains_key(name.as_str()) {
            return self.projected_array_binding(expr, name, *span);
        }
        if let Some(expr) = self.scope.full.get(name.as_str()) {
            return expr.clone().with_span(*span);
        }
        if let Some(scalar) = rumoca_core::parse_scalar_name(name.as_str())
            && let Some(values) = self.scope.scalars.get(scalar.base)
        {
            let Ok(dim) = checked_usize_to_i64(values.len(), "projected scalar value count", *span)
            else {
                return self.walk_expression(expr);
            };
            let dims = vec![dim];
            let idx = match flat_index_from_indices(
                &dims,
                &scalar.indices,
                *span,
                "projected scalar substitution flat index",
            ) {
                Ok(Some(idx)) => Some(idx),
                Ok(None) => None,
                Err(error) => {
                    self.error = Some(error);
                    return expr.clone();
                }
            };
            if let Some(idx) = idx
                && let Some(expr) = values.get(idx)
            {
                return expr.clone().with_span(*span);
            }
        }
        self.walk_expression(expr)
    }

    fn walk_function_call_expression(
        &mut self,
        name: &rumoca_core::Reference,
        args: &[rumoca_core::Expression],
        is_constructor: bool,
        span: rumoca_core::Span,
    ) -> rumoca_core::Expression {
        let previous = self.materialize_arrays;
        self.materialize_arrays = true;
        let args = self.rewrite_expressions(args);
        self.materialize_arrays = previous;
        rumoca_core::Expression::FunctionCall {
            name: name.clone(),
            args,
            is_constructor,
            span,
        }
    }
}

pub(super) fn scalar_count_for_dims(
    dims: &[i64],
    context: &str,
    span: rumoca_core::Span,
) -> Result<usize, LowerError> {
    if dims.is_empty() {
        return Ok(1);
    }
    dims.iter().try_fold(1usize, |acc, dim| {
        let dim = usize::try_from(*dim).map_err(|_| {
            LowerError::contract_violation(
                format!("{context} contains invalid dimension `{dim}`"),
                span,
            )
        })?;
        acc.checked_mul(dim).ok_or_else(|| {
            LowerError::contract_violation(format!("{context} overflow scalar count"), span)
        })
    })
}

pub(super) fn required_flat_index_to_subscripts(
    dims: &[i64],
    flat_index: usize,
    span: rumoca_core::Span,
) -> Result<Vec<usize>, LowerError> {
    if dims.is_empty() && flat_index == 0 {
        return Ok(Vec::new());
    }
    dae::flat_index_to_subscripts(dims, flat_index).ok_or_else(|| {
        LowerError::contract_violation(
            format!(
                "flat index {flat_index} is out of bounds for dimensions {}",
                format_i64_dims(dims)
            ),
            span,
        )
    })
}

pub(super) fn valid_product_dim(
    dim: i64,
    span: rumoca_core::Span,
    context: &str,
) -> Result<usize, LowerError> {
    usize::try_from(dim).map_err(|_| {
        LowerError::contract_violation(format!("{context} has invalid dimension `{dim}`"), span)
    })
}

pub(super) fn array_expression_dims(
    elements: &[rumoca_core::Expression],
    is_matrix: bool,
    child_dims: &[Option<Vec<i64>>],
    span: rumoca_core::Span,
) -> Result<Option<Vec<i64>>, LowerError> {
    if !is_matrix {
        return array_sequence_dims(elements.len(), child_dims, span).map(Some);
    }
    if let [Some(dims)] = child_dims
        && dims.len() > 1
    {
        return copy_projection_dims(dims, "matrix expression dimension count", span).map(Some);
    }
    let Some(first) = elements.first() else {
        return Ok(None);
    };
    if matches!(
        first,
        rumoca_core::Expression::Array { .. } | rumoca_core::Expression::Tuple { .. }
    ) && elements.iter().all(|element| {
        matches!(
            element,
            rumoca_core::Expression::Array { .. } | rumoca_core::Expression::Tuple { .. }
        )
    }) {
        return matrix_row_literal_dims(elements.len(), child_dims, span).map(Some);
    }
    if let Some(dims) = matrix_column_concat_dims(elements.len(), child_dims, span)? {
        return Ok(Some(dims));
    }
    if child_dims.iter().all(|dims| {
        dims.as_ref()
            .is_none_or(|candidate_dims| candidate_dims.is_empty())
    }) {
        return Ok(Some(copy_projection_dims(
            &[
                1,
                checked_usize_to_i64(elements.len(), "matrix column count", span)?,
            ],
            "matrix expression dimension count",
            span,
        )?));
    }
    Ok(None)
}

fn array_sequence_dims(
    element_count: usize,
    child_dims: &[Option<Vec<i64>>],
    span: rumoca_core::Span,
) -> Result<Vec<i64>, LowerError> {
    let Some(Some(first_dims)) = child_dims.first() else {
        return copy_projection_dims(
            &[checked_usize_to_i64(
                element_count,
                "array expression dimension",
                span,
            )?],
            "array expression dimension count",
            span,
        );
    };
    if first_dims.is_empty()
        || child_dims
            .iter()
            .skip(1)
            .any(|dims| dims.as_ref() != Some(first_dims))
    {
        return copy_projection_dims(
            &[checked_usize_to_i64(
                element_count,
                "array expression dimension",
                span,
            )?],
            "array expression dimension count",
            span,
        );
    }
    let mut dims = projection_vec_with_capacity(
        first_dims.len() + 1,
        "array expression dimension count",
        span,
    )?;
    dims.push(checked_usize_to_i64(
        element_count,
        "array expression dimension",
        span,
    )?);
    dims.extend(first_dims.iter().copied());
    Ok(dims)
}

fn matrix_row_literal_dims(
    row_count: usize,
    child_dims: &[Option<Vec<i64>>],
    span: rumoca_core::Span,
) -> Result<Vec<i64>, LowerError> {
    let Some(cols) = child_dims
        .first()
        .and_then(Option::as_ref)
        .and_then(|dims| matrix_row_literal_column_count(dims))
    else {
        return copy_projection_dims(
            &[
                checked_usize_to_i64(row_count, "matrix row count", span)?,
                1,
            ],
            "matrix expression dimension count",
            span,
        );
    };
    if child_dims.iter().all(|dims| {
        dims.as_ref()
            .and_then(|candidate_dims| matrix_row_literal_column_count(candidate_dims))
            == Some(cols)
    }) {
        copy_projection_dims(
            &[
                checked_usize_to_i64(row_count, "matrix row count", span)?,
                cols,
            ],
            "matrix expression dimension count",
            span,
        )
    } else {
        Ok(Vec::new())
    }
}

fn matrix_row_literal_column_count(dims: &[i64]) -> Option<i64> {
    match dims {
        [cols] => Some(*cols),
        [1, cols] => Some(*cols),
        _ => None,
    }
}

fn matrix_column_concat_dims(
    operand_count: usize,
    child_dims: &[Option<Vec<i64>>],
    span: rumoca_core::Span,
) -> Result<Option<Vec<i64>>, LowerError> {
    let Some(Some(first)) = child_dims.first() else {
        return Ok(None);
    };
    match first.as_slice() {
        [rows]
            if *rows > 0
                && child_dims.iter().all(|dims| {
                    matches!(dims.as_deref(), Some([candidate]) if *candidate == *rows)
                }) =>
        {
            Ok(Some(copy_projection_dims(
                &[
                    *rows,
                    checked_usize_to_i64(operand_count, "matrix column count", span)?,
                ],
                "matrix expression dimension count",
                span,
            )?))
        }
        [rows, _]
            if *rows > 0
                && child_dims.iter().all(|dims| {
                    matches!(dims.as_deref(), Some([candidate_rows, _]) if *candidate_rows == *rows)
                }) =>
        {
            let cols = child_dims
                .iter()
                .filter_map(|dims| dims.as_ref().and_then(|dims| dims.get(1)).copied())
                .sum();
            Ok(Some(copy_projection_dims(
                &[*rows, cols],
                "matrix expression dimension count",
                span,
            )?))
        }
        _ => Ok(None),
    }
}

pub(super) fn binary_mul_dims(
    lhs_dims: &[i64],
    rhs_dims: &[i64],
    span: rumoca_core::Span,
) -> Result<Option<Vec<i64>>, LowerError> {
    Ok(match (lhs_dims, rhs_dims) {
        ([], []) => Some(Vec::new()),
        ([], dims) if !dims.is_empty() => Some(copy_projection_dims(
            dims,
            "scalar lhs product dimension count",
            span,
        )?),
        (dims, []) if !dims.is_empty() => Some(copy_projection_dims(
            dims,
            "scalar rhs product dimension count",
            span,
        )?),
        ([lhs_rows, lhs_cols], [rhs_rows, rhs_cols]) if lhs_cols == rhs_rows => {
            Some(copy_projection_dims(
                &[*lhs_rows, *rhs_cols],
                "matrix product dimension count",
                span,
            )?)
        }
        ([rows, cols], [n]) if cols == n => Some(copy_projection_dims(
            &[*rows],
            "matrix-vector product dimension count",
            span,
        )?),
        ([n], [rows, cols]) if n == rows => Some(copy_projection_dims(
            &[*cols],
            "vector-matrix product dimension count",
            span,
        )?),
        ([n], [m]) if n == m => Some(Vec::new()),
        _ => None,
    })
}

pub(super) fn elementwise_binary_dims(
    lhs_dims: &[i64],
    rhs_dims: &[i64],
    span: rumoca_core::Span,
) -> Result<Option<Vec<i64>>, LowerError> {
    match (lhs_dims, rhs_dims) {
        ([], []) => Ok(Some(Vec::new())),
        (dims, []) | ([], dims) if !dims.is_empty() => Ok(Some(copy_projection_dims(
            dims,
            "elementwise scalar dimension count",
            span,
        )?)),
        (lhs, rhs) if lhs == rhs => Ok(Some(copy_projection_dims(
            lhs,
            "elementwise dimension count",
            span,
        )?)),
        _ => Ok(None),
    }
}

pub(super) fn copy_projection_dims(
    dims: &[i64],
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<Vec<i64>, LowerError> {
    if dims.is_empty() {
        return Ok(Vec::new());
    }
    let mut copied = projection_vec_with_capacity(dims.len(), context, span)?;
    copied.extend_from_slice(dims);
    Ok(copied)
}

pub(super) fn projected_field_output_dims(
    outputs: &[ProjectedFunctionOutput],
    field: &str,
    span: rumoca_core::Span,
) -> Result<Option<Vec<i64>>, LowerError> {
    let mut selector_indices =
        projection_vec_with_capacity(outputs.len(), "function field selector index count", span)?;
    for output in outputs {
        let Some((head, _tail)) = output.field_path.split_first() else {
            continue;
        };
        if head == field {
            selector_indices.push(output.selector_indices.as_slice());
        }
    }
    selector_dims_from_indices(&selector_indices, span)
}

pub(super) fn selector_dims_from_indices(
    selector_indices: &[&[usize]],
    span: rumoca_core::Span,
) -> Result<Option<Vec<i64>>, LowerError> {
    let Some(first) = selector_indices.first() else {
        return Ok(None);
    };
    if first.is_empty() {
        return Ok((selector_indices.len() == 1).then_some(Vec::new()));
    }
    if selector_indices
        .iter()
        .any(|indices| indices.len() != first.len())
    {
        return Ok(None);
    }
    let mut dims =
        projection_vec_with_capacity(first.len(), "function field selector dimension count", span)?;
    dims.extend(std::iter::repeat_n(0usize, first.len()));
    for indices in selector_indices {
        for (dim, index) in dims.iter_mut().zip(*indices) {
            *dim = (*dim).max(*index);
        }
    }
    let dims = checked_usize_dims_to_i64(&dims, "function field selector dimension", span)?;
    let count = scalar_count_for_dims(&dims, "function field selector dimensions", span)?;
    Ok((count == selector_indices.len()).then_some(dims))
}

pub(super) fn flatten_array_elements(
    elements: &[rumoca_core::Expression],
    span: rumoca_core::Span,
) -> Result<Vec<rumoca_core::Expression>, LowerError> {
    let count = flattened_array_element_count(elements, span)?;
    let mut flattened = projection_vec_with_capacity(count, "flattened array element count", span)?;
    for element in elements {
        match element {
            rumoca_core::Expression::Array { elements: row, .. } => {
                for value in row {
                    flattened.push(value.clone());
                }
            }
            _ => flattened.push(element.clone()),
        }
    }
    Ok(flattened)
}

pub(super) fn flattened_array_element_count(
    elements: &[rumoca_core::Expression],
    span: rumoca_core::Span,
) -> Result<usize, LowerError> {
    elements.iter().try_fold(0usize, |count, element| {
        let additional = match element {
            rumoca_core::Expression::Array { elements: row, .. } => row.len(),
            _ => 1,
        };
        count.checked_add(additional).ok_or_else(|| {
            LowerError::contract_violation(
                "flattened array element count overflows host index range",
                span,
            )
        })
    })
}

pub(super) fn flat_index_from_indices(
    dims: &[i64],
    indices: &[i64],
    span: rumoca_core::Span,
    context: &str,
) -> Result<Option<usize>, LowerError> {
    if dims.len() != indices.len() || dims.is_empty() {
        return Ok(None);
    }
    let mut flat = 0usize;
    let mut stride = 1usize;
    for (&dim, &index) in dims.iter().rev().zip(indices.iter().rev()) {
        if dim <= 0 || index <= 0 || index > dim {
            return Ok(None);
        }
        let dim = usize::try_from(dim).map_err(|_| {
            LowerError::contract_violation(
                format!("{context} dimension `{dim}` exceeds host index range"),
                span,
            )
        })?;
        let index = usize::try_from(index).map_err(|_| {
            LowerError::contract_violation(
                format!("{context} subscript `{index}` exceeds host index range"),
                span,
            )
        })?;
        let offset = index
            .checked_sub(1)
            .and_then(|offset| offset.checked_mul(stride))
            .ok_or_else(|| {
                LowerError::contract_violation(
                    format!("{context} offset overflows host index range"),
                    span,
                )
            })?;
        flat = flat.checked_add(offset).ok_or_else(|| {
            LowerError::contract_violation(
                format!("{context} addition overflows host index range"),
                span,
            )
        })?;
        stride = stride.checked_mul(dim).ok_or_else(|| {
            LowerError::contract_violation(
                format!("{context} stride overflows host index range"),
                span,
            )
        })?;
    }
    Ok(Some(flat))
}

pub(super) fn sum_expressions(
    mut terms: Vec<rumoca_core::Expression>,
    span: rumoca_core::Span,
) -> rumoca_core::Expression {
    if terms.is_empty() {
        return rumoca_core::Expression::Literal {
            value: Literal::Real(0.0),
            span,
        };
    }
    let first = terms.remove(0);
    terms
        .into_iter()
        .fold(first, |lhs, rhs| rumoca_core::Expression::Binary {
            op: OpBinary::Add,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span,
        })
}
