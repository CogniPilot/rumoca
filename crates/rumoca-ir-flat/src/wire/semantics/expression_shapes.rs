use super::*;

impl WireSemanticChecker<'_> {
    pub(super) fn infer_expression_shape(
        &self,
        expression: &Expression,
    ) -> Result<WireExpressionShape, FlatWireError> {
        match expression {
            Expression::Literal { .. } | Expression::StringConversion { .. } => {
                Ok(WireExpressionShape::scalar())
            }
            Expression::VarRef {
                name, subscripts, ..
            } => self.infer_reference_shape(name, subscripts),
            Expression::FunctionCall {
                name,
                args,
                call_kind: FunctionCallKind::Invocation,
                is_constructor,
                ..
            } => self.infer_function_call_shape(name, args, *is_constructor),
            Expression::Unary { rhs, .. } => {
                let shape = self.infer_expression_shape(rhs)?;
                require_non_record_shape(shape, "a unary operand cannot be a record value")
            }
            Expression::Binary { lhs, op, rhs, .. } => self.infer_binary_shape(op, lhs, rhs),
            Expression::If {
                branches,
                else_branch,
                ..
            } => self.infer_conditional_shape(branches, else_branch),
            Expression::Array { elements, .. } => self.infer_array_shape(elements),
            Expression::Index {
                base, subscripts, ..
            } => {
                let mut shape = self.infer_expression_shape(base)?;
                shape.dimensions = remaining_dimensions(subscripts, &shape.dimensions)
                    .map_err(|reason| FlatWireError::InvalidSubscript { reason })?;
                Ok(shape)
            }
            Expression::BuiltinCall { function, args, .. } => {
                self.infer_builtin_shape(*function, args)
            }
            Expression::Range {
                start, step, end, ..
            } => infer_range_shape(start, step.as_deref(), end),
            Expression::ArrayComprehension {
                expr,
                indices,
                filter,
                ..
            } => self.infer_comprehension_shape(expr, indices, filter.as_deref()),
            Expression::FieldAccess {
                base,
                field,
                field_def_id,
                ..
            } => self.infer_field_shape(base, field, *field_def_id),
            Expression::FunctionCall { .. }
            | Expression::Tuple { .. }
            | Expression::Empty { .. } => Err(expression_shape_error(
                "the expression form has no checked single-value shape authority",
            )),
        }
    }

    fn infer_reference_shape(
        &self,
        name: &Reference,
        subscripts: &[Subscript],
    ) -> Result<WireExpressionShape, FlatWireError> {
        let mut shape = if name.structured_binder().is_some() {
            WireExpressionShape::scalar()
        } else if let Some(instance_id) = name.instance_id() {
            let target = self
                .targets
                .by_occurrence
                .get(&instance_id)
                .ok_or_else(|| {
                    expression_shape_error("a reference has no exact occurrence target")
                })?;
            match target.kind {
                WireTargetKind::Variable => WireExpressionShape {
                    dimensions: self
                        .model
                        .variables
                        .get(target.name)
                        .ok_or_else(|| {
                            expression_shape_error("a variable occurrence has no reverse target")
                        })?
                        .dims
                        .clone(),
                    record: None,
                },
                WireTargetKind::Record => self.infer_record_occurrence_shape(target.name)?,
            }
        } else if self.in_function_scope {
            self.function_component_shape(name.component_ref().ok_or_else(|| {
                expression_shape_error("a function-local reference lacks exact path identity")
            })?)
            .map_err(expression_shape_error)?
        } else {
            return Err(expression_shape_error(
                "a model reference lacks exact occurrence identity",
            ));
        };
        shape.dimensions = remaining_dimensions(subscripts, &shape.dimensions)
            .map_err(|reason| FlatWireError::InvalidSubscript { reason })?;
        Ok(shape)
    }

    fn infer_conditional_shape(
        &self,
        branches: &[(Expression, Expression)],
        else_branch: &Expression,
    ) -> Result<WireExpressionShape, FlatWireError> {
        let expected = self.infer_expression_shape(else_branch)?;
        for (_, value) in branches {
            if self.infer_expression_shape(value)? != expected {
                return Err(expression_shape_error(
                    "conditional branches do not have one exact shape and record identity",
                ));
            }
        }
        Ok(expected)
    }

    fn infer_record_occurrence_shape(
        &self,
        name: &VarName,
    ) -> Result<WireExpressionShape, FlatWireError> {
        let record =
            self.model.record_instances.get(name).ok_or_else(|| {
                expression_shape_error("a record occurrence has no reverse target")
            })?;
        Ok(WireExpressionShape {
            dimensions: record.dims.clone(),
            record: Some(record_value_identity(self.model, record)?),
        })
    }

    fn infer_function_call_shape(
        &self,
        name: &Reference,
        args: &[Expression],
        is_constructor: bool,
    ) -> Result<WireExpressionShape, FlatWireError> {
        let resolved = name
            .resolved_function()
            .ok_or_else(|| expression_shape_error("a value call lacks exact function identity"))?;
        let target = self
            .targets
            .by_function_instance
            .get(&resolved.instance_id)
            .ok_or_else(|| expression_shape_error("a value call target is absent"))?;
        if target.function.is_constructor != is_constructor {
            return Err(expression_shape_error(
                "a value call's constructor kind contradicts its exact target",
            ));
        }
        let prefix = self.validate_call_argument_shapes(
            target,
            args,
            resolved,
            FunctionCallKind::Invocation,
        )?;
        if is_constructor {
            let model = self.model;
            validate_record_constructor_layout(model, target.function)?;
            let declaration = target.function.def_id.ok_or_else(|| {
                expression_shape_error("a constructor lacks exact record declaration identity")
            })?;
            return Ok(WireExpressionShape {
                dimensions: prefix,
                record: Some(record_declaration_identity(model, declaration)?),
            });
        }
        let output = target.function.outputs.first().ok_or_else(|| {
            expression_shape_error("a value call has no exact leading output slot")
        })?;
        let mut dimensions = prefix;
        dimensions.extend_from_slice(output.dimensions());
        Ok(WireExpressionShape {
            dimensions,
            record: function_param_record_identity(self.model, output)?,
        })
    }

    fn infer_array_shape(
        &self,
        elements: &[Expression],
    ) -> Result<WireExpressionShape, FlatWireError> {
        let first = elements.first().ok_or_else(|| {
            expression_shape_error("an empty array literal lacks context-owned shape evidence")
        })?;
        let mut shape = self.infer_expression_shape(first)?;
        for element in &elements[1..] {
            if self.infer_expression_shape(element)? != shape {
                return Err(expression_shape_error(
                    "array literal elements do not share one exact shape and record identity",
                ));
            }
        }
        shape.dimensions.insert(
            0,
            i64::try_from(elements.len())
                .map_err(|_| expression_shape_error("array literal cardinality exceeds i64"))?,
        );
        Ok(shape)
    }

    fn infer_comprehension_shape(
        &self,
        expr: &Expression,
        indices: &[rumoca_core::ComprehensionIndex],
        filter: Option<&Expression>,
    ) -> Result<WireExpressionShape, FlatWireError> {
        if filter.is_some() {
            return Err(expression_shape_error(
                "a filtered comprehension lacks static result-cardinality evidence",
            ));
        }
        let mut extents = Vec::with_capacity(indices.len());
        for index in indices {
            let range = self.infer_expression_shape(&index.range)?;
            let [extent] = range.dimensions.as_slice() else {
                return Err(expression_shape_error(
                    "a comprehension index range lacks exact rank-one shape",
                ));
            };
            extents.push(*extent);
        }
        let mut shape = self.infer_expression_shape(expr)?;
        extents.extend(shape.dimensions);
        shape.dimensions = extents;
        Ok(shape)
    }

    fn infer_field_shape(
        &self,
        base: &Expression,
        field_name: &str,
        field_def_id: DefId,
    ) -> Result<WireExpressionShape, FlatWireError> {
        let mut base = self.infer_expression_shape(base)?;
        let identity = base.record.ok_or_else(|| {
            expression_shape_error("a field projection base lacks exact record identity")
        })?;
        let model = self.model;
        let field = model
            .record_types
            .get(&identity.declaration)
            .and_then(|layout| {
                layout
                    .fields
                    .iter()
                    .find(|field| field.def_id == field_def_id && field.name == field_name)
            })
            .ok_or_else(|| {
                expression_shape_error("a projected field is absent from its exact record layout")
            })?;
        base.dimensions.extend_from_slice(&field.dims);
        base.record = record_field_value_identity(model, field)?;
        Ok(base)
    }

    fn infer_builtin_shape(
        &self,
        function: rumoca_core::BuiltinFunction,
        args: &[Expression],
    ) -> Result<WireExpressionShape, FlatWireError> {
        use rumoca_core::BuiltinFunction as B;
        if !function.accepts_argument_count(args.len()) {
            return Err(expression_shape_error(
                "a builtin call has invalid argument cardinality",
            ));
        }
        match function {
            B::Initial | B::Terminal | B::Clock | B::Interval | B::Ndims => {
                self.infer_scalar_builtin_shape(function, args)
            }
            B::Zeros | B::Ones | B::Fill | B::Linspace | B::Identity => {
                self.infer_constructor_builtin_shape(function, args)
            }
            B::Size
            | B::Scalar
            | B::Vector
            | B::Matrix
            | B::Transpose
            | B::Diagonal
            | B::OuterProduct
            | B::Symmetric
            | B::Cross
            | B::Skew
            | B::Cat
            | B::Sum
            | B::Product
            | B::Min
            | B::Max
            | B::Div
            | B::Mod
            | B::Rem
            | B::Atan2
            | B::Homotopy
            | B::SemiLinear => self.infer_array_builtin_shape(function, args),
            B::Der
            | B::Pre
            | B::Abs
            | B::Sign
            | B::Sqrt
            | B::Floor
            | B::Ceil
            | B::Sin
            | B::Cos
            | B::Tan
            | B::Asin
            | B::Acos
            | B::Atan
            | B::Sinh
            | B::Cosh
            | B::Tanh
            | B::Exp
            | B::Log
            | B::Log10
            | B::Edge
            | B::Change
            | B::Hold
            | B::Previous
            | B::NoClock
            | B::NoEvent
            | B::Integer => self.infer_unary_builtin_shape(function, args),
            B::Smooth
            | B::Delay
            | B::SubSample
            | B::SuperSample
            | B::ShiftSample
            | B::BackSample
            | B::Sample => self.infer_temporal_builtin_shape(function, args),
            B::Reinit => Err(expression_shape_error(
                "reinit is not a value expression with a checked result shape",
            )),
        }
    }

    fn infer_scalar_builtin_shape(
        &self,
        function: rumoca_core::BuiltinFunction,
        args: &[Expression],
    ) -> Result<WireExpressionShape, FlatWireError> {
        use rumoca_core::BuiltinFunction as B;
        match function {
            B::Initial | B::Terminal => Ok(WireExpressionShape::scalar()),
            B::Clock | B::Interval => {
                for argument in args {
                    require_scalar_expression(self, argument)?;
                }
                Ok(WireExpressionShape::scalar())
            }
            B::Ndims => {
                self.infer_expression_shape(&args[0])?;
                Ok(WireExpressionShape::scalar())
            }
            _ => Err(expression_shape_error(
                "builtin is not a scalar shape operation",
            )),
        }
    }

    fn infer_constructor_builtin_shape(
        &self,
        function: rumoca_core::BuiltinFunction,
        args: &[Expression],
    ) -> Result<WireExpressionShape, FlatWireError> {
        use rumoca_core::BuiltinFunction as B;
        match function {
            B::Zeros | B::Ones => dimension_argument_shape(args),
            B::Fill => {
                require_scalar_expression(self, &args[0])?;
                dimension_argument_shape(&args[1..])
            }
            B::Linspace => {
                require_scalar_expression(self, &args[0])?;
                require_scalar_expression(self, &args[1])?;
                let extent = positive_literal_dimension(&args[2])?;
                if extent < 2 {
                    return Err(expression_shape_error(
                        "linspace requires a final dimension of at least two",
                    ));
                }
                Ok(plain_shape(vec![extent]))
            }
            B::Identity => {
                let extent = nonnegative_literal_dimension(&args[0])?;
                Ok(plain_shape(vec![extent, extent]))
            }
            _ => Err(expression_shape_error(
                "builtin is not a constructor shape operation",
            )),
        }
    }

    fn infer_array_builtin_shape(
        &self,
        function: rumoca_core::BuiltinFunction,
        args: &[Expression],
    ) -> Result<WireExpressionShape, FlatWireError> {
        use rumoca_core::BuiltinFunction as B;
        match function {
            B::Size | B::Scalar | B::Vector | B::Matrix | B::Transpose | B::Diagonal => {
                self.infer_shape_conversion_builtin(function, args)
            }
            B::OuterProduct => self.infer_outer_product_shape(args),
            B::Symmetric => require_square_matrix(self.infer_expression_shape(&args[0])?),
            B::Cross => self.infer_cross_shape(args),
            B::Skew => {
                require_vector_extent(self.infer_expression_shape(&args[0])?, 3)?;
                Ok(plain_shape(vec![3, 3]))
            }
            B::Cat => self.infer_cat_shape(args),
            B::Sum | B::Product => {
                require_plain_shape(self.infer_expression_shape(&args[0])?)?;
                Ok(WireExpressionShape::scalar())
            }
            B::Min | B::Max => {
                if args.len() == 1 {
                    require_plain_shape(self.infer_expression_shape(&args[0])?)?;
                    Ok(WireExpressionShape::scalar())
                } else {
                    self.infer_plain_broadcast_shape(args)
                }
            }
            B::Div | B::Mod | B::Rem | B::Atan2 | B::SemiLinear => {
                self.infer_plain_broadcast_shape(args)
            }
            B::Homotopy => self.infer_exact_equal_shape(args),
            _ => Err(expression_shape_error(
                "builtin is not an array shape operation",
            )),
        }
    }

    fn infer_unary_builtin_shape(
        &self,
        function: rumoca_core::BuiltinFunction,
        args: &[Expression],
    ) -> Result<WireExpressionShape, FlatWireError> {
        use rumoca_core::BuiltinFunction as B;
        if matches!(function, B::Reinit) {
            return Err(expression_shape_error(
                "reinit is not a value expression with a checked result shape",
            ));
        }
        require_non_record_shape(
            self.infer_expression_shape(&args[0])?,
            "this builtin does not accept a record value",
        )
    }

    fn infer_temporal_builtin_shape(
        &self,
        function: rumoca_core::BuiltinFunction,
        args: &[Expression],
    ) -> Result<WireExpressionShape, FlatWireError> {
        use rumoca_core::BuiltinFunction as B;
        let (first, rest, reason) = match function {
            B::Smooth => (
                &args[0],
                &args[1..],
                "smooth does not accept a record value",
            ),
            B::Delay => (&args[0], &args[1..], "delay does not accept a record value"),
            B::SubSample | B::SuperSample | B::ShiftSample | B::BackSample => (
                &args[0],
                &args[1..],
                "clock conversion does not accept a record value",
            ),
            B::Sample => (
                &args[0],
                &args[1..],
                "sample does not accept a record value",
            ),
            _ => {
                return Err(expression_shape_error(
                    "builtin is not a temporal shape operation",
                ));
            }
        };
        if matches!(function, B::Smooth) {
            require_scalar_expression(self, first)?;
            return require_non_record_shape(self.infer_expression_shape(&args[1])?, reason);
        }
        for argument in rest {
            require_scalar_expression(self, argument)?;
        }
        require_non_record_shape(self.infer_expression_shape(first)?, reason)
    }

    fn infer_binary_shape(
        &self,
        op: &rumoca_core::OpBinary,
        lhs: &Expression,
        rhs: &Expression,
    ) -> Result<WireExpressionShape, FlatWireError> {
        use rumoca_core::OpBinary as O;
        let lhs = require_non_record_shape(
            self.infer_expression_shape(lhs)?,
            "a binary operand cannot be a record value",
        )?;
        let rhs = require_non_record_shape(
            self.infer_expression_shape(rhs)?,
            "a binary operand cannot be a record value",
        )?;
        match op {
            O::Add | O::Sub => require_equal_plain_shapes(lhs, rhs),
            O::Mul => multiply_shapes(lhs, rhs),
            O::Div => require_scalar_rhs_shape(lhs, rhs),
            O::Exp => {
                require_scalar_shape(lhs)?;
                require_scalar_shape(rhs)?;
                Ok(WireExpressionShape::scalar())
            }
            O::AddElem | O::SubElem | O::MulElem | O::DivElem | O::ExpElem => {
                broadcast_shapes(lhs, rhs)
            }
            O::Eq | O::Neq => {
                require_equal_plain_shapes(lhs, rhs)?;
                Ok(WireExpressionShape::scalar())
            }
            O::Lt | O::Le | O::Gt | O::Ge | O::And | O::Or => {
                require_scalar_shape(lhs)?;
                require_scalar_shape(rhs)?;
                Ok(WireExpressionShape::scalar())
            }
            O::Assign | O::Empty => Err(expression_shape_error(
                "assignment or empty binary syntax has no value-expression shape",
            )),
        }
    }

    fn infer_shape_conversion_builtin(
        &self,
        function: rumoca_core::BuiltinFunction,
        args: &[Expression],
    ) -> Result<WireExpressionShape, FlatWireError> {
        use rumoca_core::BuiltinFunction as B;
        match function {
            B::Size => {
                let value = self.infer_expression_shape(&args[0])?;
                if args.len() == 2 {
                    require_scalar_expression(self, &args[1])?;
                    Ok(WireExpressionShape::scalar())
                } else {
                    let rank = i64::try_from(value.dimensions.len())
                        .map_err(|_| expression_shape_error("builtin result rank exceeds i64"))?;
                    Ok(plain_shape(vec![rank]))
                }
            }
            B::Scalar => {
                let value = require_plain_shape(self.infer_expression_shape(&args[0])?)?;
                let count = checked_dimension_product(&value.dimensions)?;
                if count != 1 {
                    return Err(expression_shape_error(
                        "scalar requires an exact single-element argument",
                    ));
                }
                Ok(WireExpressionShape::scalar())
            }
            B::Vector => {
                let value = require_plain_shape(self.infer_expression_shape(&args[0])?)?;
                Ok(plain_shape(vec![checked_dimension_product(
                    &value.dimensions,
                )?]))
            }
            B::Matrix => infer_matrix_builtin_shape(self.infer_expression_shape(&args[0])?),
            B::Transpose => {
                let mut value = require_plain_shape(self.infer_expression_shape(&args[0])?)?;
                if value.dimensions.len() < 2 {
                    return Err(expression_shape_error(
                        "transpose requires exact rank of at least two",
                    ));
                }
                value.dimensions.swap(0, 1);
                Ok(value)
            }
            B::Diagonal => {
                let value = require_plain_shape(self.infer_expression_shape(&args[0])?)?;
                let [extent] = value.dimensions.as_slice() else {
                    return Err(expression_shape_error("diagonal requires an exact vector"));
                };
                Ok(plain_shape(vec![*extent, *extent]))
            }
            _ => Err(expression_shape_error(
                "an unowned builtin reached the checked shape-conversion owner",
            )),
        }
    }

    fn infer_plain_broadcast_shape(
        &self,
        args: &[Expression],
    ) -> Result<WireExpressionShape, FlatWireError> {
        let mut result = WireExpressionShape::scalar();
        for argument in args {
            result = broadcast_shapes(
                result,
                require_non_record_shape(
                    self.infer_expression_shape(argument)?,
                    "this builtin does not accept a record value",
                )?,
            )?;
        }
        Ok(result)
    }

    fn infer_exact_equal_shape(
        &self,
        args: &[Expression],
    ) -> Result<WireExpressionShape, FlatWireError> {
        let first = require_non_record_shape(
            self.infer_expression_shape(&args[0])?,
            "this builtin does not accept a record value",
        )?;
        for argument in &args[1..] {
            let candidate = require_non_record_shape(
                self.infer_expression_shape(argument)?,
                "this builtin does not accept a record value",
            )?;
            if candidate != first {
                return Err(expression_shape_error(
                    "builtin arguments do not have one exact result shape",
                ));
            }
        }
        Ok(first)
    }

    fn infer_outer_product_shape(
        &self,
        args: &[Expression],
    ) -> Result<WireExpressionShape, FlatWireError> {
        let left = require_vector(self.infer_expression_shape(&args[0])?)?;
        let right = require_vector(self.infer_expression_shape(&args[1])?)?;
        Ok(plain_shape(vec![left, right]))
    }

    fn infer_cross_shape(&self, args: &[Expression]) -> Result<WireExpressionShape, FlatWireError> {
        require_vector_extent(self.infer_expression_shape(&args[0])?, 3)?;
        require_vector_extent(self.infer_expression_shape(&args[1])?, 3)?;
        Ok(plain_shape(vec![3]))
    }

    fn infer_cat_shape(&self, args: &[Expression]) -> Result<WireExpressionShape, FlatWireError> {
        let axis = positive_literal_dimension(&args[0])?;
        let axis = usize::try_from(axis - 1)
            .map_err(|_| expression_shape_error("cat axis exceeds usize"))?;
        let mut result = require_plain_shape(self.infer_expression_shape(&args[1])?)?;
        if axis >= result.dimensions.len() {
            return Err(expression_shape_error(
                "cat axis exceeds the exact argument rank",
            ));
        }
        for argument in &args[2..] {
            let candidate = require_plain_shape(self.infer_expression_shape(argument)?)?;
            if candidate.dimensions.len() != result.dimensions.len()
                || candidate
                    .dimensions
                    .iter()
                    .zip(&result.dimensions)
                    .enumerate()
                    .any(|(index, (candidate, expected))| index != axis && candidate != expected)
            {
                return Err(expression_shape_error(
                    "cat arguments disagree outside the exact concatenation axis",
                ));
            }
            result.dimensions[axis] = result.dimensions[axis]
                .checked_add(candidate.dimensions[axis])
                .ok_or_else(|| expression_shape_error("cat result dimension overflows i64"))?;
        }
        Ok(result)
    }
}
