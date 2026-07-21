use super::*;

/// Recursively index into an expression tree at 1-based Modelica index `i`.
///
/// - `Array { elements }` → return `elements[i-1]`
/// - `VarRef { name, subscripts: [] }` for array vars → add `Subscript::generated_index(i, rumoca_core::Span::DUMMY)`
/// - `FunctionCall { is_constructor: true }` → project positional constructor arg `i`
/// - `Binary/Unary/BuiltinCall/If/FunctionCall/Index` → recurse into children
/// - Scalars (Literal, etc.) → broadcast unchanged
pub(super) struct IndexProjectionContext<'a> {
    pub(super) i: usize,
    pub(super) context_span: Option<Span>,
    pub(super) var_dims: &'a HashMap<String, Vec<i64>>,
    pub(super) var_spans: &'a HashMap<String, Span>,
    pub(super) structural_values: &'a HashMap<String, i64>,
    pub(super) complex_fields: &'a HashMap<String, [Option<String>; 2]>,
    pub(super) component_index_map: &'a HashMap<String, HashMap<usize, String>>,
    pub(super) function_output_index_map: &'a FunctionOutputProjectionMap,
    pub(super) function_output_dims_map: &'a HashMap<rumoca_core::FunctionInstanceId, Vec<i64>>,
    pub(super) dynamic_function_output_map: &'a HashMap<rumoca_core::FunctionInstanceId, String>,
    pub(super) record_field_projection_map: &'a RecordFieldProjectionMap,
    pub(super) constructor_input_map: &'a ConstructorInputMap,
    pub(super) expected_dims: Option<&'a [i64]>,
    pub(super) allow_dynamic_function_projection: bool,
}

impl<'a> IndexProjectionContext<'a> {
    fn with_index(&self, i: usize) -> IndexProjectionContext<'a> {
        IndexProjectionContext {
            i,
            context_span: self.context_span,
            var_dims: self.var_dims,
            var_spans: self.var_spans,
            structural_values: self.structural_values,
            complex_fields: self.complex_fields,
            component_index_map: self.component_index_map,
            function_output_index_map: self.function_output_index_map,
            function_output_dims_map: self.function_output_dims_map,
            dynamic_function_output_map: self.dynamic_function_output_map,
            record_field_projection_map: self.record_field_projection_map,
            constructor_input_map: self.constructor_input_map,
            expected_dims: self.expected_dims,
            allow_dynamic_function_projection: self.allow_dynamic_function_projection,
        }
    }

    fn nested(&self) -> IndexProjectionContext<'a> {
        let mut nested = self.with_index(self.i);
        nested.allow_dynamic_function_projection = false;
        nested
    }

    fn project_at(&self, expr: &Expression, i: usize) -> Result<Expression, StructuralError> {
        let mut nested = self.with_index(i);
        nested.allow_dynamic_function_projection = false;
        nested.project(expr)
    }

    fn project_static_selection(
        &self,
        base: &Expression,
        subscripts: &[Subscript],
        span: Span,
    ) -> Result<Option<Expression>, StructuralError> {
        let Some(dims) = self.expression_dims(base) else {
            return Ok(None);
        };
        let Some(projected_dims) =
            apply_subscripts_to_dims(&dims, subscripts, self.structural_values)
        else {
            return Ok(None);
        };
        let selected_subscripts = if projected_dims.is_empty() {
            subscripts.to_vec()
        } else {
            let count = output_scalar_count(&projected_dims, span)?;
            if self.i == 0 || self.i > count {
                return Ok(None);
            }
            let Some(selected) =
                project_subscripted_dims(&dims, subscripts, self.i, span, self.structural_values)?
            else {
                return Ok(None);
            };
            selected
        };
        let Some(index) =
            linear_index_for_static_subscripts(&dims, &selected_subscripts, self.structural_values)
        else {
            return Ok(None);
        };
        self.project_at(base, index).map(Some)
    }

    fn map_exprs(&self, exprs: &[Expression]) -> Result<Vec<Expression>, StructuralError> {
        let nested = self.nested();
        exprs.iter().map(|expr| nested.project(expr)).collect()
    }

    fn project_var_ref(
        &self,
        name: &Reference,
        subscripts: &[Subscript],
        fallback: &Expression,
    ) -> Result<Expression, StructuralError> {
        if let Some(dims) = self.var_dims.get(name.as_str()) {
            return project_dimmed_var_ref(name, dims, subscripts, fallback, self);
        }

        if !subscripts.is_empty() {
            return Ok(fallback.clone());
        }

        if let Some(fields) = self.complex_fields.get(name.as_str()) {
            let projected = match self.i {
                1 => fields[0].as_ref(),
                2 => fields[1].as_ref(),
                _ => None,
            };
            if let Some(projected_name) = projected {
                let span =
                    projection_source_span(name, fallback, self.var_spans, self.context_span)?;
                return Ok(Expression::VarRef {
                    name: rumoca_core::Reference::new(projected_name.clone()),
                    subscripts: vec![],
                    span,
                });
            }
        }

        if let Some(by_index) = self.component_index_map.get(name.as_str())
            && let Some(projected_name) = by_index.get(&self.i)
        {
            let span = projection_source_span(name, fallback, self.var_spans, self.context_span)?;
            return Ok(Expression::VarRef {
                name: rumoca_core::Reference::new(projected_name.clone()),
                subscripts: vec![],
                span,
            });
        }

        Ok(fallback.clone())
    }

    pub(super) fn expression_shape(&self, expr: &Expression) -> ExpressionShape {
        match expr {
            Expression::Literal { value: _, .. } => ExpressionShape::Scalar,
            Expression::VarRef {
                name, subscripts, ..
            } => self.var_ref_shape(name, subscripts),
            Expression::Array {
                elements,
                is_matrix,
                ..
            } => array_literal_shape(elements, *is_matrix),
            Expression::Unary { rhs, .. } => self.expression_shape(rhs),
            Expression::Binary { op, lhs, rhs, .. } => {
                let lhs_shape = self.expression_shape(lhs);
                let rhs_shape = self.expression_shape(rhs);
                if matches!(op, OpBinary::Mul) {
                    combine_matrix_mul_shapes(lhs_shape, rhs_shape)
                } else if matches!(op, OpBinary::MulElem) {
                    combine_elementwise_shapes(lhs_shape, rhs_shape)
                } else if matches!(
                    op,
                    OpBinary::Add | OpBinary::AddElem | OpBinary::Sub | OpBinary::SubElem
                ) {
                    combine_additive_shapes(lhs_shape, rhs_shape)
                } else if matches!(op, OpBinary::Div | OpBinary::DivElem) {
                    combine_division_shapes(lhs_shape, rhs_shape)
                } else {
                    ExpressionShape::Scalar
                }
            }
            Expression::If { else_branch, .. } => self.expression_shape(else_branch),
            Expression::BuiltinCall { function, args, .. } => self.builtin_shape(*function, args),
            Expression::FunctionCall {
                name,
                is_constructor,
                ..
            } if *is_constructor => {
                match constructor_inputs_for_call(name, self.constructor_input_map)
                    .map(<[rumoca_core::FunctionParam]>::len)
                {
                    Some(field_count) if field_count > 1 => ExpressionShape::Vector(field_count),
                    Some(_) => ExpressionShape::Scalar,
                    None => ExpressionShape::Other,
                }
            }
            Expression::FunctionCall { name, .. } => self.function_call_shape(name),
            Expression::Index {
                base, subscripts, ..
            } => self
                .expression_dims(base)
                .and_then(|dims| {
                    apply_subscripts_to_dims(&dims, subscripts, self.structural_values)
                })
                .map(|dims| shape_from_dims(&dims))
                .unwrap_or(ExpressionShape::Other),
            _ => ExpressionShape::Other,
        }
    }

    fn expression_dims(&self, expr: &Expression) -> Option<Vec<i64>> {
        match self.expression_shape(expr) {
            ExpressionShape::Scalar => Some(Vec::new()),
            ExpressionShape::Vector(n) => Some(vec![n as i64]),
            ExpressionShape::Matrix(r, c) => Some(vec![r as i64, c as i64]),
            ExpressionShape::Other => None,
        }
    }

    fn function_call_shape(&self, name: &Reference) -> ExpressionShape {
        let Some(resolved) = name.resolved_function() else {
            return ExpressionShape::Other;
        };
        let Some(reference) = name.component_ref() else {
            return ExpressionShape::Other;
        };
        let Some(selection) = reference.parts.get(resolved.base_part_count..) else {
            return ExpressionShape::Other;
        };
        if !selection.is_empty()
            && self
                .function_output_index_map
                .get(&resolved.instance_id)
                .is_some_and(|outputs| outputs.values().any(|output| output == selection))
        {
            return ExpressionShape::Scalar;
        }
        let Some(dims) = self.function_output_dims_map.get(&resolved.instance_id) else {
            return ExpressionShape::Other;
        };
        match selection {
            [] => shape_from_dims(dims),
            [output] => apply_subscripts_to_dims(dims, &output.subs, self.structural_values)
                .map(|dims| shape_from_dims(&dims))
                .unwrap_or(ExpressionShape::Other),
            _ => ExpressionShape::Other,
        }
    }

    fn var_ref_shape(&self, name: &Reference, subscripts: &[Subscript]) -> ExpressionShape {
        if let Some(dims) = self.var_dims.get(name.as_str()) {
            return apply_subscripts_to_dims(dims, subscripts, self.structural_values)
                .map(|dims| shape_from_dims(&dims))
                .unwrap_or(ExpressionShape::Other);
        }
        if self.complex_fields.contains_key(name.as_str()) {
            return ExpressionShape::Vector(2);
        }
        if let Some(by_index) = self.component_index_map.get(name.as_str()) {
            return ExpressionShape::Vector(by_index.len());
        }
        ExpressionShape::Scalar
    }

    fn builtin_shape(
        &self,
        function: rumoca_core::BuiltinFunction,
        args: &[Expression],
    ) -> ExpressionShape {
        let dimension_shape = |dimensions: &[Expression]| {
            let dims = dimensions
                .iter()
                .map(integer_literal_value)
                .map(|value| value.and_then(|value| usize::try_from(value).ok()))
                .collect::<Option<Vec<_>>>();
            match dims.as_deref() {
                Some([n]) => ExpressionShape::Vector(*n),
                Some([rows, cols]) => ExpressionShape::Matrix(*rows, *cols),
                _ => ExpressionShape::Other,
            }
        };
        match function {
            rumoca_core::BuiltinFunction::Der
            | rumoca_core::BuiltinFunction::Pre
            | rumoca_core::BuiltinFunction::NoEvent => args
                .first()
                .map(|arg| self.expression_shape(arg))
                .unwrap_or(ExpressionShape::Scalar),
            rumoca_core::BuiltinFunction::Transpose => {
                match args.first().map(|arg| self.expression_shape(arg)) {
                    Some(ExpressionShape::Matrix(r, c)) => ExpressionShape::Matrix(c, r),
                    Some(ExpressionShape::Vector(n)) => ExpressionShape::Vector(n),
                    Some(shape) => shape,
                    None => ExpressionShape::Other,
                }
            }
            rumoca_core::BuiltinFunction::Cross => ExpressionShape::Vector(3),
            rumoca_core::BuiltinFunction::Skew => ExpressionShape::Matrix(3, 3),
            rumoca_core::BuiltinFunction::OuterProduct => match args {
                [lhs, rhs] => match (self.expression_shape(lhs), self.expression_shape(rhs)) {
                    (ExpressionShape::Vector(rows), ExpressionShape::Vector(cols)) => {
                        ExpressionShape::Matrix(rows, cols)
                    }
                    _ => ExpressionShape::Other,
                },
                _ => ExpressionShape::Other,
            },
            rumoca_core::BuiltinFunction::Zeros | rumoca_core::BuiltinFunction::Ones => {
                dimension_shape(args)
            }
            rumoca_core::BuiltinFunction::Fill => dimension_shape(args.get(1..).unwrap_or(&[])),
            rumoca_core::BuiltinFunction::Identity => args
                .first()
                .and_then(integer_literal_value)
                .and_then(|n| (n > 0).then_some(ExpressionShape::Matrix(n as usize, n as usize)))
                .unwrap_or(ExpressionShape::Other),
            rumoca_core::BuiltinFunction::Diagonal => {
                match args.first().map(|arg| self.expression_shape(arg)) {
                    Some(ExpressionShape::Vector(n)) => ExpressionShape::Matrix(n, n),
                    _ => ExpressionShape::Other,
                }
            }
            rumoca_core::BuiltinFunction::Vector => args
                .first()
                .map(|arg| match self.expression_shape(arg) {
                    ExpressionShape::Scalar => ExpressionShape::Vector(1),
                    ExpressionShape::Vector(n) => ExpressionShape::Vector(n),
                    ExpressionShape::Matrix(r, c) => ExpressionShape::Vector(r * c),
                    ExpressionShape::Other => ExpressionShape::Other,
                })
                .unwrap_or(ExpressionShape::Other),
            rumoca_core::BuiltinFunction::Matrix => args
                .first()
                .map(|arg| match self.expression_shape(arg) {
                    ExpressionShape::Scalar => ExpressionShape::Matrix(1, 1),
                    ExpressionShape::Vector(n) => ExpressionShape::Matrix(n, 1),
                    shape => shape,
                })
                .unwrap_or(ExpressionShape::Other),
            rumoca_core::BuiltinFunction::Scalar
            | rumoca_core::BuiltinFunction::Sum
            | rumoca_core::BuiltinFunction::Product
            | rumoca_core::BuiltinFunction::Size
            | rumoca_core::BuiltinFunction::Ndims => ExpressionShape::Scalar,
            _ => ExpressionShape::Scalar,
        }
    }

    fn project_matrix_mul(
        &self,
        lhs: &Expression,
        rhs: &Expression,
        span: Span,
    ) -> Result<Option<Expression>, StructuralError> {
        let lhs_shape = self.expression_shape(lhs);
        let rhs_shape = self.expression_shape(rhs);
        match (lhs_shape, rhs_shape) {
            (ExpressionShape::Vector(n), ExpressionShape::Vector(m)) if n == m => {
                let mut terms = Vec::with_capacity(n);
                for k in 1..=n {
                    terms.push(mul_expr_with_span(
                        self.project_at(lhs, k)?,
                        self.project_at(rhs, k)?,
                        span,
                    ));
                }
                Ok(Some(sum_terms_with_span(terms, span)))
            }
            (ExpressionShape::Matrix(rows, cols), ExpressionShape::Vector(n)) if cols == n => {
                if self.i < 1 || self.i > rows {
                    return Ok(None);
                }
                let row = self.i;
                let mut terms = Vec::with_capacity(cols);
                for k in 1..=cols {
                    terms.push(mul_expr_with_span(
                        self.project_at(lhs, matrix_linear_index(row, k, cols))?,
                        self.project_at(rhs, k)?,
                        span,
                    ));
                }
                Ok(Some(sum_terms_with_span(terms, span)))
            }
            (ExpressionShape::Vector(n), ExpressionShape::Matrix(rows, cols)) if n == rows => {
                if self.i < 1 || self.i > cols {
                    return Ok(None);
                }
                let col = self.i;
                let mut terms = Vec::with_capacity(n);
                for k in 1..=n {
                    terms.push(mul_expr_with_span(
                        self.project_at(lhs, k)?,
                        self.project_at(rhs, matrix_linear_index(k, col, cols))?,
                        span,
                    ));
                }
                Ok(Some(sum_terms_with_span(terms, span)))
            }
            (
                ExpressionShape::Matrix(lhs_rows, lhs_cols),
                ExpressionShape::Matrix(rhs_rows, rhs_cols),
            ) if lhs_cols == rhs_rows => {
                let result_count = lhs_rows * rhs_cols;
                if self.i < 1 || self.i > result_count {
                    return Ok(None);
                }
                let (row, col) = row_major_subscripts_2d(self.i, rhs_cols);
                let mut terms = Vec::with_capacity(lhs_cols);
                for k in 1..=lhs_cols {
                    terms.push(mul_expr_with_span(
                        self.project_at(lhs, matrix_linear_index(row, k, lhs_cols))?,
                        self.project_at(rhs, matrix_linear_index(k, col, rhs_cols))?,
                        span,
                    ));
                }
                Ok(Some(sum_terms_with_span(terms, span)))
            }
            _ => Ok(None),
        }
    }

    fn project_transpose(
        &self,
        args: &[Expression],
    ) -> Result<Option<Expression>, StructuralError> {
        let Some(arg) = args.first() else {
            return Ok(None);
        };
        match self.expression_shape(arg) {
            ExpressionShape::Matrix(rows, cols) => {
                let result_count = rows * cols;
                if self.i < 1 || self.i > result_count {
                    return Ok(None);
                }
                let (row, col) = row_major_subscripts_2d(self.i, rows);
                Ok(Some(
                    self.project_at(arg, matrix_linear_index(col, row, cols))?,
                ))
            }
            ExpressionShape::Vector(n) if self.i >= 1 && self.i <= n => {
                Ok(Some(self.project_at(arg, self.i)?))
            }
            ExpressionShape::Scalar if self.i == 1 => Ok(Some(self.project_at(arg, 1)?)),
            _ => Ok(None),
        }
    }

    fn project_cross(
        &self,
        args: &[Expression],
        span: Span,
    ) -> Result<Option<Expression>, StructuralError> {
        let Some(lhs) = args.first() else {
            return Ok(None);
        };
        let Some(rhs) = args.get(1) else {
            return Ok(None);
        };
        if self.expression_shape(lhs) != ExpressionShape::Vector(3)
            || self.expression_shape(rhs) != ExpressionShape::Vector(3)
        {
            return Ok(None);
        }

        let component = match self.i {
            1 => sub_expr_with_span(
                mul_expr_with_span(self.project_at(lhs, 2)?, self.project_at(rhs, 3)?, span),
                mul_expr_with_span(self.project_at(lhs, 3)?, self.project_at(rhs, 2)?, span),
                span,
            ),
            2 => sub_expr_with_span(
                mul_expr_with_span(self.project_at(lhs, 3)?, self.project_at(rhs, 1)?, span),
                mul_expr_with_span(self.project_at(lhs, 1)?, self.project_at(rhs, 3)?, span),
                span,
            ),
            3 => sub_expr_with_span(
                mul_expr_with_span(self.project_at(lhs, 1)?, self.project_at(rhs, 2)?, span),
                mul_expr_with_span(self.project_at(lhs, 2)?, self.project_at(rhs, 1)?, span),
                span,
            ),
            _ => return Ok(None),
        };
        Ok(Some(component))
    }

    fn project_skew(
        &self,
        args: &[Expression],
        span: Span,
    ) -> Result<Option<Expression>, StructuralError> {
        let Some(arg) = args.first() else {
            return Ok(None);
        };
        if self.expression_shape(arg) != ExpressionShape::Vector(3) {
            return Ok(None);
        }
        let zero = || Expression::Literal {
            value: Literal::Real(0.0),
            span,
        };
        let neg = |value| Expression::Unary {
            op: OpUnary::Minus,
            rhs: Box::new(value),
            span,
        };
        let value = match self.i {
            1 | 5 | 9 => zero(),
            2 => neg(self.project_at(arg, 3)?),
            3 => self.project_at(arg, 2)?,
            4 => self.project_at(arg, 3)?,
            6 => neg(self.project_at(arg, 1)?),
            7 => neg(self.project_at(arg, 2)?),
            8 => self.project_at(arg, 1)?,
            _ => return Ok(None),
        };
        Ok(Some(value))
    }

    fn project_outer_product(
        &self,
        args: &[Expression],
        span: Span,
    ) -> Result<Option<Expression>, StructuralError> {
        let [lhs, rhs] = args else {
            return Ok(None);
        };
        let (ExpressionShape::Vector(rows), ExpressionShape::Vector(cols)) =
            (self.expression_shape(lhs), self.expression_shape(rhs))
        else {
            return Ok(None);
        };
        if self.i == 0 || self.i > rows * cols {
            return Ok(None);
        }
        let (row, col) = row_major_subscripts_2d(self.i, cols);
        Ok(Some(mul_expr_with_span(
            self.project_at(lhs, row)?,
            self.project_at(rhs, col)?,
            span,
        )))
    }

    fn project_constant_array_builtin(
        &self,
        function: rumoca_core::BuiltinFunction,
        args: &[Expression],
        span: Span,
    ) -> Option<Expression> {
        let shape = self.builtin_shape(function, args);
        let count = shape_scalar_count(shape)?;
        if self.i == 0 || self.i > count {
            return None;
        }
        let value = match function {
            rumoca_core::BuiltinFunction::Zeros => 0.0,
            rumoca_core::BuiltinFunction::Ones => 1.0,
            _ => return None,
        };
        Some(Expression::Literal {
            value: Literal::Real(value),
            span,
        })
    }

    fn project_identity(&self, args: &[Expression], span: Span) -> Option<Expression> {
        let n = args
            .first()
            .and_then(integer_literal_value)
            .and_then(|value| usize::try_from(value).ok())?;
        if self.i == 0 || self.i > n * n {
            return None;
        }
        let (row, col) = row_major_subscripts_2d(self.i, n);
        Some(Expression::Literal {
            value: Literal::Integer(i64::from(row == col)),
            span,
        })
    }

    // SPEC_0021: Exception - cohesive lowering of scalarized linear-algebra Expression forms.
    #[allow(clippy::too_many_lines)]
    pub(super) fn lower_scalar_linear_algebra(
        &self,
        expr: &Expression,
    ) -> Result<Expression, StructuralError> {
        match expr {
            Expression::Binary { op, lhs, rhs, span } => {
                if matches!(op, OpBinary::Mul)
                    && self.expression_shape(expr) == ExpressionShape::Scalar
                    && let Some(projected) = self.project_matrix_mul(lhs, rhs, *span)?
                {
                    return self.lower_scalar_linear_algebra(&projected);
                }
                let lowered_lhs = self.lower_scalar_linear_algebra(lhs)?;
                let lowered_rhs = self.lower_scalar_linear_algebra(rhs)?;
                Ok(Expression::Binary {
                    op: op.clone(),
                    lhs: Box::new(lowered_lhs),
                    rhs: Box::new(lowered_rhs),
                    span: *span,
                })
            }
            Expression::Unary { op, rhs, span } => Ok(Expression::Unary {
                op: op.clone(),
                rhs: Box::new(self.lower_scalar_linear_algebra(rhs)?),
                span: *span,
            }),
            Expression::BuiltinCall {
                function,
                args,
                span,
            } => Ok(Expression::BuiltinCall {
                function: *function,
                args: args
                    .iter()
                    .map(|arg| self.lower_scalar_linear_algebra(arg))
                    .collect::<Result<Vec<_>, _>>()?,
                span: *span,
            }),
            Expression::If {
                branches,
                else_branch,
                span,
            } => Ok(Expression::If {
                branches: branches
                    .iter()
                    .map(|(condition, value)| {
                        Ok((condition.clone(), self.lower_scalar_linear_algebra(value)?))
                    })
                    .collect::<Result<Vec<_>, StructuralError>>()?,
                else_branch: Box::new(self.lower_scalar_linear_algebra(else_branch)?),
                span: *span,
            }),
            Expression::FunctionCall {
                name,
                args,
                is_constructor,
                span,
            } => Ok(Expression::FunctionCall {
                name: name.clone(),
                args: args
                    .iter()
                    .map(|arg| self.lower_scalar_linear_algebra(arg))
                    .collect::<Result<Vec<_>, _>>()?,
                is_constructor: *is_constructor,
                span: *span,
            }),
            Expression::Array {
                elements,
                is_matrix,
                span,
            } => Ok(Expression::Array {
                elements: elements
                    .iter()
                    .map(|element| self.lower_scalar_linear_algebra(element))
                    .collect::<Result<Vec<_>, _>>()?,
                is_matrix: *is_matrix,
                span: *span,
            }),
            Expression::Tuple { elements, span } => Ok(Expression::Tuple {
                elements: elements
                    .iter()
                    .map(|element| self.lower_scalar_linear_algebra(element))
                    .collect::<Result<Vec<_>, _>>()?,
                span: *span,
            }),
            Expression::Index {
                base,
                subscripts,
                span,
            } => {
                if let Some(projected) = self.project_static_selection(base, subscripts, *span)? {
                    return Ok(projected);
                }
                Ok(Expression::Index {
                    base: Box::new(self.lower_scalar_linear_algebra(base)?),
                    subscripts: subscripts.clone(),
                    span: *span,
                })
            }
            Expression::ArrayComprehension {
                expr,
                indices,
                filter,
                span,
            } => Ok(Expression::ArrayComprehension {
                expr: Box::new(self.lower_scalar_linear_algebra(expr)?),
                indices: indices.clone(),
                filter: filter
                    .as_ref()
                    .map(|value| self.lower_scalar_linear_algebra(value).map(Box::new))
                    .transpose()?,
                span: *span,
            }),
            _ => Ok(expr.clone()),
        }
    }

    fn project_wrapped_dynamic_output(
        &self,
        expr: &Expression,
    ) -> Result<Option<Expression>, StructuralError> {
        project_wrapped_dynamic_function_output(
            expr,
            self.i,
            self.expected_dims,
            self.context_span,
            self.allow_dynamic_function_projection,
            self.dynamic_function_output_map,
        )
    }

    fn array_element_scalar_count(&self, expr: &Expression) -> Option<usize> {
        if let Expression::Array { elements, .. } = expr {
            return elements.iter().try_fold(0usize, |count, element| {
                count.checked_add(self.array_element_scalar_count(element)?)
            });
        }
        shape_scalar_count(self.expression_shape(expr))
    }

    fn project_array_literal_scalar(
        &self,
        elements: &[Expression],
    ) -> Result<Option<Expression>, StructuralError> {
        let Some(first) = elements.first() else {
            return Ok(None);
        };
        let Some(element_width) = self.array_element_scalar_count(first) else {
            return Ok(None);
        };
        if element_width == 0 {
            return Ok(None);
        }
        let Some(flat_index) = self.i.checked_sub(1) else {
            return Ok(None);
        };
        let element = elements.get(flat_index / element_width);
        let Some(element) = element else {
            return Ok(None);
        };
        Ok(Some(
            self.project_at(element, flat_index % element_width + 1)?,
        ))
    }

    fn project_static_array_comprehension(
        &self,
        expr: &Expression,
        indices: &[rumoca_core::ComprehensionIndex],
        filter: Option<&Expression>,
        span: Span,
    ) -> Result<Option<Expression>, StructuralError> {
        if filter.is_some() || indices.is_empty() {
            return Ok(None);
        }
        let Some(body_width) = self.array_element_scalar_count(expr) else {
            return Ok(None);
        };
        if body_width == 0 {
            return Ok(None);
        }
        let Some(ranges) = indices
            .iter()
            .map(|index| range_subscript_indices(&index.range, self.structural_values))
            .collect::<Option<Vec<_>>>()
        else {
            return Ok(None);
        };
        let iteration_dims = ranges
            .iter()
            .map(|range| range.len() as i64)
            .collect::<Vec<_>>();
        let iteration_count = output_scalar_count(&iteration_dims, span)?;
        let total = iteration_count.checked_mul(body_width).ok_or_else(|| {
            structural_contract_violation(
                "array comprehension scalar count exceeds host limits".to_string(),
                span,
            )
        })?;
        if self.i == 0 || self.i > total {
            return Ok(None);
        }
        let flat_index = self.i - 1;
        let iteration_index = flat_index / body_width;
        let Some(positions) = dae::flat_index_to_subscripts(&iteration_dims, iteration_index)
        else {
            return Ok(None);
        };
        let mut values = HashMap::new();
        for ((index, range), position) in indices.iter().zip(&ranges).zip(positions) {
            let Some(value) = position
                .checked_sub(1)
                .and_then(|position| range.get(position))
            else {
                return Ok(None);
            };
            values.insert(index.name.clone(), *value);
        }
        let body = StaticComprehensionSubstituter { values }.rewrite_expression(expr);
        self.project_at(&body, flat_index % body_width + 1)
            .map(Some)
    }

    pub(super) fn project(&self, expr: &Expression) -> Result<Expression, StructuralError> {
        if let Some(projected) = self.project_wrapped_dynamic_output(expr)? {
            return Ok(projected);
        }
        match expr {
            Expression::Array { elements, .. } => Ok(self
                .project_array_literal_scalar(elements)?
                .unwrap_or_else(|| expr.clone())),
            Expression::VarRef {
                name, subscripts, ..
            } => self.project_var_ref(name, subscripts, expr),
            Expression::Binary { op, lhs, rhs, span } => {
                if matches!(op, OpBinary::Mul)
                    && let Some(projected) = self.project_matrix_mul(lhs, rhs, *span)?
                {
                    return Ok(projected);
                }
                Ok(Expression::Binary {
                    op: op.clone(),
                    lhs: Box::new(self.nested().project(lhs)?),
                    rhs: Box::new(self.nested().project(rhs)?),
                    span: *span,
                })
            }
            Expression::Unary { op, rhs, span } => Ok(Expression::Unary {
                op: op.clone(),
                rhs: Box::new(self.nested().project(rhs)?),
                span: *span,
            }),
            Expression::BuiltinCall {
                function,
                args,
                span,
            } => self.project_builtin_call(*function, args, *span),
            Expression::If {
                branches,
                else_branch,
                span,
            } => Ok(Expression::If {
                branches: branches
                    .iter()
                    .map(|(cond, val)| Ok((cond.clone(), self.nested().project(val)?)))
                    .collect::<Result<Vec<_>, StructuralError>>()?,
                else_branch: Box::new(self.nested().project(else_branch)?),
                span: *span,
            }),
            Expression::FunctionCall {
                name,
                args,
                is_constructor,
                span,
            } => self.project_function_call(name, args, *is_constructor, *span),
            Expression::Index {
                base,
                subscripts,
                span,
            } => {
                if let Some(projected) = self.project_static_selection(base, subscripts, *span)? {
                    return Ok(projected);
                }
                Ok(Expression::Index {
                    base: Box::new(self.nested().project(base)?),
                    subscripts: subscripts.clone(),
                    span: *span,
                })
            }
            Expression::ArrayComprehension {
                expr: body,
                indices,
                filter,
                span,
            } => Ok(self
                .project_static_array_comprehension(body, indices, filter.as_deref(), *span)?
                .unwrap_or_else(|| expr.clone())),
            Expression::FieldAccess { base, field, span } => {
                self.project_field_access(expr, base, field, *span)
            }
            _ => Ok(expr.clone()),
        }
    }

    fn project_builtin_call(
        &self,
        function: rumoca_core::BuiltinFunction,
        args: &[Expression],
        span: Span,
    ) -> Result<Expression, StructuralError> {
        if matches!(
            function,
            rumoca_core::BuiltinFunction::Sum | rumoca_core::BuiltinFunction::Product
        ) || matches!(
            function,
            rumoca_core::BuiltinFunction::Min | rumoca_core::BuiltinFunction::Max
        ) && args.len() == 1
        {
            return Ok(Expression::BuiltinCall {
                function,
                args: args.to_vec(),
                span,
            });
        }
        let projected = match function {
            rumoca_core::BuiltinFunction::Transpose => self.project_transpose(args)?,
            rumoca_core::BuiltinFunction::Cross => self.project_cross(args, span)?,
            rumoca_core::BuiltinFunction::Skew => self.project_skew(args, span)?,
            rumoca_core::BuiltinFunction::OuterProduct => self.project_outer_product(args, span)?,
            rumoca_core::BuiltinFunction::Zeros | rumoca_core::BuiltinFunction::Ones => {
                self.project_constant_array_builtin(function, args, span)
            }
            rumoca_core::BuiltinFunction::Identity => self.project_identity(args, span),
            _ => None,
        };
        if let Some(projected) = projected {
            return Ok(projected);
        }
        Ok(Expression::BuiltinCall {
            function,
            args: self.map_exprs(args)?,
            span,
        })
    }

    fn project_field_access(
        &self,
        expr: &Expression,
        base: &Expression,
        field: &str,
        span: Span,
    ) -> Result<Expression, StructuralError> {
        if let Some(projected) = self.project_record_array_member_slice(base, field)? {
            return Ok(projected);
        }
        if let Some(projected) = self.project_record_function_field(expr)? {
            return Ok(projected);
        }
        if self.i == 0 {
            return Ok(expr.clone());
        }
        Ok(Expression::Index {
            base: Box::new(expr.clone()),
            subscripts: vec![generated_index_subscript(
                self.i,
                span,
                "structural record-field projection subscript",
            )?],
            span,
        })
    }

    fn project_record_function_field(
        &self,
        expr: &Expression,
    ) -> Result<Option<Expression>, StructuralError> {
        fn selected_call<'a>(
            expr: &'a Expression,
            fields: &mut Vec<&'a str>,
        ) -> Option<(&'a Reference, &'a [Expression], bool, Span)> {
            match expr {
                Expression::FieldAccess { base, field, .. } => {
                    let call = selected_call(base, fields)?;
                    fields.push(field);
                    Some(call)
                }
                Expression::FunctionCall {
                    name,
                    args,
                    is_constructor,
                    span,
                } if !is_constructor => Some((name, args, *is_constructor, *span)),
                _ => None,
            }
        }

        let mut fields = Vec::new();
        let Some((name, args, is_constructor, span)) = selected_call(expr, &mut fields) else {
            return Ok(None);
        };
        let instance_id = name
            .resolved_function()
            .map(|resolved| resolved.instance_id)
            .ok_or_else(|| {
                structural_contract_violation(
                    "record function call lacks resolved instance identity".to_string(),
                    span,
                )
            })?;
        let Some(projection) = self.record_field_projection_map.get(&instance_id) else {
            return Ok(None);
        };
        let record_fields = if fields
            .first()
            .is_some_and(|field| *field == projection.output_name)
        {
            &fields[1..]
        } else {
            fields.as_slice()
        };
        if record_fields.is_empty() {
            return Ok(None);
        }
        let field_path = record_fields.join(".");
        let Some(selector) = projection
            .fields
            .get(&field_path)
            .and_then(|by_index| by_index.get(&self.i))
        else {
            return Ok(None);
        };
        if selector.first().map(|part| part.ident.as_str()) != Some(projection.output_name.as_str())
        {
            return Err(structural_contract_violation(
                format!(
                    "record function projection for `{field_path}` selects output other than `{}`",
                    projection.output_name
                ),
                span,
            ));
        }
        let projected_name = name.with_appended_parts(selector, span).ok_or_else(|| {
            structural_contract_violation(
                "projected function call lacks structured identity".to_string(),
                span,
            )
        })?;
        Ok(Some(Expression::FunctionCall {
            name: projected_name,
            args: args.to_vec(),
            is_constructor,
            span,
        }))
    }

    fn project_function_call(
        &self,
        name: &Reference,
        args: &[Expression],
        is_constructor: bool,
        span: rumoca_core::Span,
    ) -> Result<Expression, StructuralError> {
        if is_constructor {
            return self.project_constructor_call(name, args, span);
        }
        let resolved = name.resolved_function().ok_or_else(|| {
            structural_contract_violation(
                "function call lacks resolved instance identity".to_string(),
                span,
            )
        })?;
        let instance_id = resolved.instance_id;
        let is_bare_call = name
            .component_ref()
            .is_some_and(|reference| reference.parts.len() == resolved.base_part_count);
        if !is_bare_call {
            return Ok(Expression::FunctionCall {
                name: name.clone(),
                args: args.to_vec(),
                is_constructor,
                span,
            });
        }
        if self.allow_dynamic_function_projection
            && let Some(output_name) = self.dynamic_function_output_map.get(&instance_id)
            && let Some(dims) = self.expected_dims
            && let Ok(count) = output_scalar_count(dims, span)
            && self.i >= 1
            && self.i <= count
        {
            let indices = dae::flat_index_to_subscripts(dims, self.i - 1).ok_or_else(|| {
                structural_contract_violation(
                    "invalid dynamic function output index".to_string(),
                    span,
                )
            })?;
            let subs = indices
                .into_iter()
                .map(|index| checked_projection_subscript(index, span))
                .collect::<Result<Vec<_>, _>>()?;
            let projected_name = name
                .with_appended_parts(
                    &[rumoca_core::ComponentRefPart {
                        ident: output_name.clone(),
                        span,
                        subs,
                    }],
                    span,
                )
                .ok_or_else(|| {
                    structural_contract_violation(
                        "dynamic function call lacks structured identity".to_string(),
                        span,
                    )
                })?;
            return Ok(Expression::FunctionCall {
                name: projected_name,
                args: args.to_vec(),
                is_constructor: false,
                span,
            });
        }
        if let Some(by_index) = self.function_output_index_map.get(&instance_id)
            && let Some(projected_output) = by_index.get(&self.i)
        {
            let projected_name = name
                .with_appended_parts(projected_output, span)
                .ok_or_else(|| {
                    structural_contract_violation(
                        "projected function call lacks structured identity".to_string(),
                        span,
                    )
                })?;
            return Ok(Expression::FunctionCall {
                name: projected_name,
                args: args.to_vec(),
                is_constructor: false,
                span,
            });
        }
        Ok(Expression::FunctionCall {
            name: name.clone(),
            args: self.map_exprs(args)?,
            is_constructor,
            span,
        })
    }

    fn project_constructor_call(
        &self,
        name: &Reference,
        args: &[Expression],
        span: rumoca_core::Span,
    ) -> Result<Expression, StructuralError> {
        if name
            .as_str()
            .starts_with(rumoca_core::NAMED_FUNCTION_ARG_PREFIX)
        {
            return Ok(Expression::FunctionCall {
                name: name.clone(),
                args: args.to_vec(),
                is_constructor: true,
                span,
            });
        }
        let fields = bind_constructor_fields(name, args, span, self.constructor_input_map)?;
        if let Some(field) = self.i.checked_sub(1).and_then(|index| fields.get(index)) {
            return self.project(field);
        }
        Ok(Expression::FunctionCall {
            name: name.clone(),
            args: args.to_vec(),
            is_constructor: true,
            span,
        })
    }

    /// Projects element `i` of a record-array member slice such as
    /// `ac.pin[:].v` into the scalarized component variable `ac.pin[i].v`.
    /// Declines when the selection is not a full one-dimensional colon slice
    /// over a structured base or the element variable is unknown.
    fn project_record_array_member_slice(
        &self,
        base: &Expression,
        field: &str,
    ) -> Result<Option<Expression>, StructuralError> {
        let Expression::Index {
            base: inner,
            subscripts,
            span,
        } = base
        else {
            return Ok(None);
        };
        let Expression::VarRef {
            name,
            subscripts: ref_subscripts,
            ..
        } = inner.as_ref()
        else {
            return Ok(None);
        };
        if !ref_subscripts.is_empty()
            || subscripts.len() != 1
            || !matches!(subscripts[0], Subscript::Colon { .. })
            || self.i == 0
        {
            return Ok(None);
        }
        let Some(component_ref) = name.component_ref() else {
            return Ok(None);
        };
        let mut element_ref = component_ref.clone();
        let Some(part) = element_ref.parts.last_mut() else {
            return Ok(None);
        };
        part.subs = vec![generated_index_subscript(
            self.i,
            *span,
            "structural record-array member slice subscript",
        )?];
        element_ref.parts.push(rumoca_core::ComponentRefPart {
            ident: field.to_string(),
            span: *span,
            subs: Vec::new(),
        });
        // Existence of the element variable is enforced by the Solve
        // reference resolver, which fails loudly on unknown references.
        let reference = rumoca_core::Reference::from_component_reference(element_ref);
        Ok(Some(Expression::VarRef {
            name: reference,
            subscripts: vec![],
            span: *span,
        }))
    }
}

fn project_dimmed_var_ref(
    name: &Reference,
    dims: &[i64],
    subscripts: &[Subscript],
    fallback: &Expression,
    projection: &IndexProjectionContext<'_>,
) -> Result<Expression, StructuralError> {
    if !subscripts.is_empty() {
        let span = projection_source_span(
            name,
            fallback,
            projection.var_spans,
            projection.context_span,
        )?;
        return Ok(project_subscripted_dims(
            dims,
            subscripts,
            projection.i,
            span,
            projection.structural_values,
        )?
        .map(|projected_subscripts| Expression::VarRef {
            name: name.clone(),
            subscripts: projected_subscripts,
            span,
        })
        .unwrap_or_else(|| fallback.clone()));
    }

    let span = projection_source_span(
        name,
        fallback,
        projection.var_spans,
        projection.context_span,
    )?;
    let scalar_count = output_scalar_count(dims, span)?;
    if scalar_count > 1 && projection.i <= scalar_count {
        return Ok(Expression::VarRef {
            name: name.clone(),
            subscripts: linear_subscripts_for_dims_with_span(dims, projection.i, span)?,
            span,
        });
    }

    Ok(fallback.clone())
}
