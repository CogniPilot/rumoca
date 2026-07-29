use rumoca_core::Span;
use rumoca_ir_dae as dae;

/// Failure to derive one exact scalar view from a checked DAE expression.
#[derive(Debug, Clone, thiserror::Error)]
pub enum ProjectionError {
    #[error("scalar projection {index} is outside an expression containing {count} scalars")]
    ScalarOutOfBounds {
        index: usize,
        count: usize,
        span: Span,
    },
    #[error("array subscript is not compile-time computable")]
    DynamicSubscript { span: Span },
    #[error("Modelica index {index} is outside an axis of extent {extent}")]
    IndexOutOfBounds { index: i64, extent: u32, span: Span },
    #[error("integer evaluation overflowed during scalar projection")]
    IntegerOverflow { span: Span },
    #[error("function scalar projection exceeded the checked recursion limit")]
    FunctionRecursion { span: Span },
    #[error("record field projection has no checked aggregate definition")]
    UnsupportedRecordOperation { span: Span },
}

/// Visit every coordinate on which one scalar result depends.
///
/// `scalar_index` is row-major within `root`. For a structured equation body,
/// pass its domain and the one-based coordinates of the domain point being
/// projected. The callback receives the typed coordinate and its row-major
/// scalar index. Runtime array selection reports the exact union of every
/// potentially selected base scalar plus its subscript dependencies. This is a
/// conservative incidence proof, not a guessed runtime value.
pub fn for_each_scalar_coordinate<'dae>(
    view: dae::DaeView<'dae>,
    root: dae::ExprId<'dae>,
    scalar_index: usize,
    domain_point: Option<(dae::DomainId<'dae>, &[i64])>,
    mut visit: impl FnMut(dae::CoordinateView<'dae>, usize),
) -> Result<(), ProjectionError> {
    let mut projection = Projection {
        view,
        domain_points: match domain_point {
            Some((domain, point)) => vec![(domain, point.to_vec())],
            None => Vec::new(),
        },
        integer_stack: vec![false; view.expression_count()],
        function_arguments: Vec::new(),
        visit: &mut visit,
    };
    projection.expression(root, scalar_index)
}

struct Projection<'visit, 'dae, F> {
    view: dae::DaeView<'dae>,
    domain_points: Vec<(dae::DomainId<'dae>, Vec<i64>)>,
    integer_stack: Vec<bool>,
    function_arguments: Vec<(dae::FunctionId<'dae>, Vec<dae::ExprId<'dae>>)>,
    visit: &'visit mut F,
}

impl<'dae, F> Projection<'_, 'dae, F>
where
    F: FnMut(dae::CoordinateView<'dae>, usize),
{
    fn expression(
        &mut self,
        expression: dae::ExprId<'dae>,
        scalar_index: usize,
    ) -> Result<(), ProjectionError> {
        let node = self.node(expression);
        self.expect_scalar_index(node, scalar_index)?;
        match node.operation() {
            dae::ExpressionOperation::Literal(_) | dae::ExpressionOperation::Range { .. } => Ok(()),
            dae::ExpressionOperation::Coordinate(coordinate) => {
                if let dae::CoordinateView::FunctionParameter(parameter) = coordinate {
                    return self.function_parameter(
                        parameter,
                        scalar_index,
                        node.provenance().span(),
                    );
                }
                if !matches!(coordinate, dae::CoordinateView::Binder(_)) {
                    (self.visit)(coordinate, scalar_index);
                }
                Ok(())
            }
            dae::ExpressionOperation::Unary { operand, .. } => {
                self.expression(operand, scalar_index)
            }
            dae::ExpressionOperation::Binary { operator, lhs, rhs } => {
                self.binary(operator, lhs, rhs, scalar_index)
            }
            dae::ExpressionOperation::Conditional(operands) => {
                self.conditional(operands, scalar_index)
            }
            dae::ExpressionOperation::Array(elements) => self.array(elements, scalar_index),
            dae::ExpressionOperation::Record(fields) => {
                for field in fields.iter() {
                    self.all_scalars(field)?;
                }
                Ok(())
            }
            dae::ExpressionOperation::Field { base, field } => {
                self.record_field(base, field as usize, scalar_index)
            }
            dae::ExpressionOperation::Comprehension { domain, body } => {
                self.comprehension(domain, body, scalar_index)
            }
            dae::ExpressionOperation::FunctionValue { definition, .. } => {
                self.expression(definition.rhs(), scalar_index)
            }
            dae::ExpressionOperation::FunctionFoldParameter { .. } => Ok(()),
            dae::ExpressionOperation::FunctionFoldOutput { fold, .. } => {
                let fold = self
                    .view
                    .function_fold(fold)
                    .expect("checked function fold identity resolves");
                for expression in fold
                    .initial_values()
                    .rhs_iter()
                    .chain(fold.update_values().rhs_iter())
                {
                    self.all_scalars(expression)?;
                }
                Ok(())
            }
            dae::ExpressionOperation::Index { base, subscripts } => {
                match self.indexed_base_scalar(
                    base,
                    subscripts,
                    node.value_type().dimensions(),
                    scalar_index,
                ) {
                    Ok(base_index) => self.expression(base, base_index),
                    Err(ProjectionError::DynamicSubscript { .. }) => {
                        self.all_scalars(base)?;
                        self.subscripts(subscripts)
                    }
                    Err(error) => Err(error),
                }
            }
            dae::ExpressionOperation::ArrayUpdate {
                base,
                value,
                subscripts,
            } => {
                self.expression(base, scalar_index)?;
                self.all_scalars(value)?;
                self.subscripts(subscripts)
            }
            dae::ExpressionOperation::Builtin { builtin, arguments } => {
                self.builtin(builtin, arguments, scalar_index)
            }
            dae::ExpressionOperation::Call {
                function,
                output,
                arguments,
            } => self.function_call(
                function,
                output,
                arguments,
                scalar_index,
                node.provenance().span(),
            ),
        }
    }

    fn subscripts(&mut self, subscripts: dae::SubscriptsView<'dae>) -> Result<(), ProjectionError> {
        for subscript in subscripts.iter() {
            match subscript {
                dae::SubscriptView::Index { expression, .. }
                | dae::SubscriptView::Slice { expression, .. } => {
                    self.all_scalars(expression)?;
                }
                dae::SubscriptView::Whole { .. } => {}
            }
        }
        Ok(())
    }

    fn function_parameter(
        &mut self,
        parameter: dae::FunctionParameterId<'dae>,
        scalar_index: usize,
        span: Span,
    ) -> Result<(), ProjectionError> {
        let Some((function, arguments)) = self.function_arguments.pop() else {
            return Err(ProjectionError::FunctionRecursion { span });
        };
        let argument = (function == parameter.function())
            .then(|| arguments.get(parameter.ordinal() as usize).copied())
            .flatten();
        let projected = argument
            .ok_or(ProjectionError::FunctionRecursion { span })
            .and_then(|argument| self.expression(argument, scalar_index));
        self.function_arguments.push((function, arguments));
        projected
    }

    fn function_call(
        &mut self,
        function: dae::FunctionId<'dae>,
        output: u32,
        arguments: dae::ExpressionOperands<'dae>,
        scalar_index: usize,
        span: Span,
    ) -> Result<(), ProjectionError> {
        if self.function_arguments.len() >= 256 {
            return Err(ProjectionError::FunctionRecursion { span });
        }
        let result = self
            .view
            .function(function)
            .and_then(|definition| definition.result_values().rhs(output as usize))
            .ok_or(ProjectionError::FunctionRecursion { span })?;
        self.function_arguments
            .push((function, arguments.iter().collect()));
        let projected = self.expression(result, scalar_index);
        self.function_arguments.pop();
        projected
    }

    fn record_field(
        &mut self,
        expression: dae::ExprId<'dae>,
        field: usize,
        scalar_index: usize,
    ) -> Result<(), ProjectionError> {
        let node = self.node(expression);
        match node.operation() {
            dae::ExpressionOperation::Record(fields) => self.expression(
                fields
                    .get(field)
                    .expect("checked record field ordinal is in range"),
                scalar_index,
            ),
            dae::ExpressionOperation::Call {
                function,
                output,
                arguments,
            } => {
                if self.function_arguments.len() >= 256 {
                    return Err(ProjectionError::FunctionRecursion {
                        span: node.provenance().span(),
                    });
                }
                let result = self
                    .view
                    .function(function)
                    .and_then(|definition| definition.result_values().rhs(output as usize))
                    .ok_or(ProjectionError::FunctionRecursion {
                        span: node.provenance().span(),
                    })?;
                self.function_arguments
                    .push((function, arguments.iter().collect()));
                let projected = self.record_field(result, field, scalar_index);
                self.function_arguments.pop();
                projected
            }
            dae::ExpressionOperation::FunctionValue { definition, .. } => {
                self.record_field(definition.rhs(), field, scalar_index)
            }
            dae::ExpressionOperation::Conditional(operands) => {
                let fallback = operands
                    .get(operands.len() - 1)
                    .expect("checked conditional has a fallback");
                for ordinal in (0..operands.len() - 1).step_by(2) {
                    self.expression(
                        operands
                            .get(ordinal)
                            .expect("checked conditional condition ordinal"),
                        0,
                    )?;
                    self.record_field(
                        operands
                            .get(ordinal + 1)
                            .expect("checked conditional value ordinal"),
                        field,
                        scalar_index,
                    )?;
                }
                self.record_field(fallback, field, scalar_index)
            }
            _ => Err(ProjectionError::UnsupportedRecordOperation {
                span: node.provenance().span(),
            }),
        }
    }

    fn conditional(
        &mut self,
        operands: dae::ExpressionOperands<'dae>,
        scalar_index: usize,
    ) -> Result<(), ProjectionError> {
        let fallback = operands
            .get(operands.len() - 1)
            .expect("checked conditional has a fallback");
        for ordinal in (0..operands.len() - 1).step_by(2) {
            self.expression(
                operands
                    .get(ordinal)
                    .expect("checked conditional condition ordinal"),
                0,
            )?;
            self.expression(
                operands
                    .get(ordinal + 1)
                    .expect("checked conditional value ordinal"),
                scalar_index,
            )?;
        }
        self.expression(fallback, scalar_index)
    }

    fn binary(
        &mut self,
        operator: dae::BinaryOperator,
        lhs: dae::ExprId<'dae>,
        rhs: dae::ExprId<'dae>,
        scalar_index: usize,
    ) -> Result<(), ProjectionError> {
        if operator == dae::BinaryOperator::Multiply {
            return self.multiplication(lhs, rhs, scalar_index);
        }
        let lhs_index = if self.scalar_count(lhs) == 1 {
            0
        } else {
            scalar_index
        };
        let rhs_index = if self.scalar_count(rhs) == 1 {
            0
        } else {
            scalar_index
        };
        self.expression(lhs, lhs_index)?;
        self.expression(rhs, rhs_index)
    }

    fn multiplication(
        &mut self,
        lhs: dae::ExprId<'dae>,
        rhs: dae::ExprId<'dae>,
        scalar_index: usize,
    ) -> Result<(), ProjectionError> {
        let lhs_dimensions = self.node(lhs).value_type().dimensions();
        let rhs_dimensions = self.node(rhs).value_type().dimensions();
        let pairs = multiplication_scalar_pairs(lhs_dimensions, rhs_dimensions, scalar_index);
        for (lhs_index, rhs_index) in pairs {
            self.expression(lhs, lhs_index)?;
            self.expression(rhs, rhs_index)?;
        }
        Ok(())
    }

    fn array(
        &mut self,
        elements: dae::ExpressionOperands<'dae>,
        scalar_index: usize,
    ) -> Result<(), ProjectionError> {
        let first = elements.get(0).expect("checked array is nonempty");
        let element_count = self.scalar_count(first);
        let element_ordinal = scalar_index / element_count;
        let element_index = scalar_index % element_count;
        self.expression(
            elements
                .get(element_ordinal)
                .expect("checked array scalar index selects an element"),
            element_index,
        )
    }

    fn comprehension(
        &mut self,
        domain: dae::DomainId<'dae>,
        body: dae::ExprId<'dae>,
        scalar_index: usize,
    ) -> Result<(), ProjectionError> {
        let domain_view = self
            .view
            .domain(domain)
            .expect("checked comprehension domain resolves");
        let body_count = self.scalar_count(body);
        let point_index = scalar_index / body_count;
        let body_index = scalar_index % body_count;
        let point = domain_view
            .structured()
            .index_tuple_at(point_index)
            .expect("checked comprehension domain remains valid")
            .expect("checked comprehension scalar index selects its domain");
        self.domain_points.push((domain, point));
        let result = self.expression(body, body_index);
        self.domain_points.pop();
        result
    }

    fn builtin(
        &mut self,
        builtin: dae::PureBuiltin,
        arguments: dae::ExpressionOperands<'dae>,
        scalar_index: usize,
    ) -> Result<(), ProjectionError> {
        match builtin {
            dae::PureBuiltin::Abs
            | dae::PureBuiltin::Sign
            | dae::PureBuiltin::Sqrt
            | dae::PureBuiltin::Floor
            | dae::PureBuiltin::Ceil
            | dae::PureBuiltin::Integer
            | dae::PureBuiltin::Sin
            | dae::PureBuiltin::Cos
            | dae::PureBuiltin::Tan
            | dae::PureBuiltin::Asin
            | dae::PureBuiltin::Acos
            | dae::PureBuiltin::Atan
            | dae::PureBuiltin::Sinh
            | dae::PureBuiltin::Cosh
            | dae::PureBuiltin::Tanh
            | dae::PureBuiltin::Exp
            | dae::PureBuiltin::Log
            | dae::PureBuiltin::Log10 => self.expression(
                arguments
                    .get(0)
                    .expect("checked unary builtin has one argument"),
                scalar_index,
            ),
            dae::PureBuiltin::Atan2
            | dae::PureBuiltin::Div
            | dae::PureBuiltin::Mod
            | dae::PureBuiltin::Rem
            | dae::PureBuiltin::Homotopy => {
                for argument in arguments.iter() {
                    self.expression(argument, scalar_index)?;
                }
                Ok(())
            }
            dae::PureBuiltin::Smooth => self.expression(
                arguments.get(1).expect("checked smooth value argument"),
                scalar_index,
            ),
            dae::PureBuiltin::NoEvent => self.expression(
                arguments.get(0).expect("checked noEvent value argument"),
                scalar_index,
            ),
            dae::PureBuiltin::Sum | dae::PureBuiltin::Product => self.all_scalars(
                arguments
                    .get(0)
                    .expect("checked reduction has one argument"),
            ),
            dae::PureBuiltin::Min | dae::PureBuiltin::Max if arguments.len() == 1 => self
                .all_scalars(
                    arguments
                        .get(0)
                        .expect("checked reduction has one argument"),
                ),
            dae::PureBuiltin::Min | dae::PureBuiltin::Max => {
                for argument in arguments.iter() {
                    self.expression(argument, scalar_index)?;
                }
                Ok(())
            }
            dae::PureBuiltin::Size => {
                if let Some(dimension) = arguments.get(1) {
                    self.expression(dimension, 0)?;
                }
                Ok(())
            }
            dae::PureBuiltin::Zeros | dae::PureBuiltin::Ones => {
                for dimension in arguments.iter() {
                    self.expression(dimension, 0)?;
                }
                Ok(())
            }
            dae::PureBuiltin::Fill => {
                for argument in arguments.iter() {
                    self.expression(argument, 0)?;
                }
                Ok(())
            }
            dae::PureBuiltin::Linspace => {
                for argument in arguments.iter() {
                    self.expression(argument, 0)?;
                }
                Ok(())
            }
            dae::PureBuiltin::Cross => {
                let (first, second) = [(1, 2), (2, 0), (0, 1)][scalar_index];
                for argument in arguments.iter() {
                    self.expression(argument, first)?;
                    self.expression(argument, second)?;
                }
                Ok(())
            }
        }
    }

    fn all_scalars(&mut self, expression: dae::ExprId<'dae>) -> Result<(), ProjectionError> {
        for index in 0..self.scalar_count(expression) {
            self.expression(expression, index)?;
        }
        Ok(())
    }

    fn indexed_base_scalar(
        &mut self,
        base: dae::ExprId<'dae>,
        subscripts: dae::SubscriptsView<'dae>,
        result_dimensions: &[u32],
        result_index: usize,
    ) -> Result<usize, ProjectionError> {
        let base_node = self.node(base);
        let result_coordinates = row_major_coordinates(result_dimensions, result_index)
            .expect("checked indexed result scalar is within its shape");
        let mut result_axis = 0usize;
        let mut base_coordinates = Vec::with_capacity(base_node.value_type().dimensions().len());
        for (axis, &extent) in base_node.value_type().dimensions().iter().enumerate() {
            match subscripts.get(axis) {
                Some(dae::SubscriptView::Index {
                    expression,
                    provenance,
                }) => {
                    let index = self.integer(expression, 0)?;
                    base_coordinates.push(checked_index(index, extent, provenance.span())?);
                }
                Some(dae::SubscriptView::Whole { .. }) | None => {
                    base_coordinates.push(result_coordinates[result_axis]);
                    result_axis += 1;
                }
                Some(dae::SubscriptView::Slice {
                    expression,
                    provenance,
                }) => {
                    let rank = self.node(expression).value_type().dimensions().len();
                    let slice_coordinates =
                        &result_coordinates[result_axis..result_axis.saturating_add(rank)];
                    let slice_index = flatten_coordinates(
                        self.node(expression).value_type().dimensions(),
                        slice_coordinates,
                    )
                    .expect("checked result projection selects a slice element");
                    let index = self.integer(expression, slice_index)?;
                    base_coordinates.push(checked_index(index, extent, provenance.span())?);
                    result_axis += rank;
                }
            }
        }
        Ok(
            flatten_coordinates(base_node.value_type().dimensions(), &base_coordinates)
                .expect("checked index projection maps into its base"),
        )
    }

    fn integer(
        &mut self,
        expression: dae::ExprId<'dae>,
        scalar_index: usize,
    ) -> Result<i64, ProjectionError> {
        let raw = expression.index() as usize;
        if self.integer_stack[raw] {
            return Err(ProjectionError::DynamicSubscript {
                span: self.node(expression).provenance().span(),
            });
        }
        self.integer_stack[raw] = true;
        let result = self.integer_inner(expression, scalar_index);
        self.integer_stack[raw] = false;
        result
    }

    fn integer_inner(
        &mut self,
        expression: dae::ExprId<'dae>,
        scalar_index: usize,
    ) -> Result<i64, ProjectionError> {
        let node = self.node(expression);
        self.expect_scalar_index(node, scalar_index)?;
        let span = node.provenance().span();
        match node.operation() {
            dae::ExpressionOperation::Literal(
                dae::DaeLiteral::Integer(value) | dae::DaeLiteral::Enumeration(value),
            ) => Ok(*value),
            dae::ExpressionOperation::Range { start, step, .. } => {
                let offset = i64::try_from(scalar_index)
                    .map_err(|_| ProjectionError::IntegerOverflow { span })?;
                start
                    .checked_add(
                        step.checked_mul(offset)
                            .ok_or(ProjectionError::IntegerOverflow { span })?,
                    )
                    .ok_or(ProjectionError::IntegerOverflow { span })
            }
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Binder(binder)) => {
                let Some((_, point)) = self
                    .domain_points
                    .iter()
                    .rev()
                    .find(|(domain, _)| *domain == binder.domain())
                else {
                    return Err(ProjectionError::DynamicSubscript { span });
                };
                point
                    .get(binder.ordinal() as usize)
                    .copied()
                    .ok_or(ProjectionError::DynamicSubscript { span })
            }
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::FunctionParameter(
                parameter,
            )) => self.integer_parameter(parameter, scalar_index, span),
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Parameter(parameter)) => {
                let variable = self
                    .view
                    .variable(parameter.into())
                    .expect("checked parameter coordinate resolves");
                let binding = variable
                    .binding()
                    .ok_or(ProjectionError::DynamicSubscript { span })?;
                self.integer(binding, scalar_index)
            }
            dae::ExpressionOperation::Unary { operator, operand } => {
                let value = self.integer(operand, scalar_index)?;
                match operator {
                    dae::UnaryOperator::Plus => Ok(value),
                    dae::UnaryOperator::Negate => value
                        .checked_neg()
                        .ok_or(ProjectionError::IntegerOverflow { span }),
                    dae::UnaryOperator::Not => Err(ProjectionError::DynamicSubscript { span }),
                }
            }
            dae::ExpressionOperation::Binary { operator, lhs, rhs } => {
                let lhs = self.integer(lhs, scalar_index)?;
                let rhs = self.integer(rhs, scalar_index)?;
                integer_binary(operator, lhs, rhs, span)
            }
            dae::ExpressionOperation::Call {
                function,
                output,
                arguments,
            } => self.integer_call(function, output, arguments, scalar_index, span),
            dae::ExpressionOperation::Array(elements) => {
                let first = elements.get(0).expect("checked array is nonempty");
                let element_count = self.scalar_count(first);
                self.integer(
                    elements
                        .get(scalar_index / element_count)
                        .expect("checked integer array projection selects an element"),
                    scalar_index % element_count,
                )
            }
            dae::ExpressionOperation::Index { base, subscripts } => {
                let base_index = self.indexed_base_scalar(
                    base,
                    subscripts,
                    node.value_type().dimensions(),
                    scalar_index,
                )?;
                self.integer(base, base_index)
            }
            dae::ExpressionOperation::FunctionValue { definition, .. } => {
                self.integer(definition.rhs(), scalar_index)
            }
            _ => Err(ProjectionError::DynamicSubscript { span }),
        }
    }

    fn integer_parameter(
        &mut self,
        parameter: dae::FunctionParameterId<'dae>,
        scalar_index: usize,
        span: Span,
    ) -> Result<i64, ProjectionError> {
        let Some((function, arguments)) = self.function_arguments.pop() else {
            return Err(ProjectionError::FunctionRecursion { span });
        };
        let argument = (function == parameter.function())
            .then(|| arguments.get(parameter.ordinal() as usize).copied())
            .flatten();
        let projected = argument
            .ok_or(ProjectionError::FunctionRecursion { span })
            .and_then(|argument| self.integer(argument, scalar_index));
        self.function_arguments.push((function, arguments));
        projected
    }

    fn integer_call(
        &mut self,
        function: dae::FunctionId<'dae>,
        output: u32,
        arguments: dae::ExpressionOperands<'dae>,
        scalar_index: usize,
        span: Span,
    ) -> Result<i64, ProjectionError> {
        if self.function_arguments.len() >= 256 {
            return Err(ProjectionError::FunctionRecursion { span });
        }
        let result = self
            .view
            .function(function)
            .and_then(|definition| definition.result_values().rhs(output as usize))
            .ok_or(ProjectionError::FunctionRecursion { span })?;
        self.function_arguments
            .push((function, arguments.iter().collect()));
        let value = self.integer(result, scalar_index);
        self.function_arguments.pop();
        value
    }

    fn node(&self, expression: dae::ExprId<'dae>) -> dae::ExpressionView<'dae> {
        self.view
            .expression(expression)
            .expect("branded expression resolves in its owning DAE")
    }

    fn scalar_count(&self, expression: dae::ExprId<'dae>) -> usize {
        self.node(expression)
            .value_type()
            .scalar_count()
            .expect("checked expression shape has a representable scalar count")
    }

    fn expect_scalar_index(
        &self,
        node: dae::ExpressionView<'dae>,
        index: usize,
    ) -> Result<(), ProjectionError> {
        let count = node
            .value_type()
            .scalar_count()
            .expect("checked expression shape has a representable scalar count");
        if index < count {
            return Ok(());
        }
        Err(ProjectionError::ScalarOutOfBounds {
            index,
            count,
            span: node.provenance().span(),
        })
    }
}

fn checked_index(index: i64, extent: u32, span: Span) -> Result<u32, ProjectionError> {
    if index < 1 || index > i64::from(extent) {
        return Err(ProjectionError::IndexOutOfBounds {
            index,
            extent,
            span,
        });
    }
    Ok(u32::try_from(index - 1).expect("positive in-range u32 index"))
}

fn integer_binary(
    operator: dae::BinaryOperator,
    lhs: i64,
    rhs: i64,
    span: Span,
) -> Result<i64, ProjectionError> {
    let overflow = || ProjectionError::IntegerOverflow { span };
    match operator {
        dae::BinaryOperator::Add | dae::BinaryOperator::ElementwiseAdd => {
            lhs.checked_add(rhs).ok_or_else(overflow)
        }
        dae::BinaryOperator::Subtract | dae::BinaryOperator::ElementwiseSubtract => {
            lhs.checked_sub(rhs).ok_or_else(overflow)
        }
        dae::BinaryOperator::Multiply | dae::BinaryOperator::ElementwiseMultiply => {
            lhs.checked_mul(rhs).ok_or_else(overflow)
        }
        dae::BinaryOperator::Divide | dae::BinaryOperator::ElementwiseDivide if rhs != 0 => {
            lhs.checked_div(rhs).ok_or_else(overflow)
        }
        dae::BinaryOperator::Power | dae::BinaryOperator::ElementwisePower if rhs >= 0 => lhs
            .checked_pow(u32::try_from(rhs).map_err(|_| overflow())?)
            .ok_or_else(overflow),
        _ => Err(ProjectionError::DynamicSubscript { span }),
    }
}

fn multiplication_scalar_pairs(lhs: &[u32], rhs: &[u32], scalar: usize) -> Vec<(usize, usize)> {
    match (lhs, rhs) {
        ([], _) => vec![(0, scalar)],
        (_, []) => vec![(scalar, 0)],
        ([inner], [rhs_inner]) if inner == rhs_inner => {
            (0..*inner as usize).map(|term| (term, term)).collect()
        }
        ([_, inner], [rhs_inner]) if inner == rhs_inner => {
            let start = scalar * *inner as usize;
            (0..*inner as usize)
                .map(|term| (start + term, term))
                .collect()
        }
        ([inner], [rhs_inner, columns]) if inner == rhs_inner => (0..*inner as usize)
            .map(|term| (term, term * *columns as usize + scalar))
            .collect(),
        ([_, inner], [rhs_inner, columns]) if inner == rhs_inner => {
            let columns = *columns as usize;
            let row = scalar / columns;
            let column = scalar % columns;
            let lhs_start = row * *inner as usize;
            (0..*inner as usize)
                .map(|term| (lhs_start + term, term * columns + column))
                .collect()
        }
        _ => unreachable!("checked multiplication has a valid algebraic shape"),
    }
}

fn row_major_coordinates(extents: &[u32], index: usize) -> Option<Vec<u32>> {
    let scalar_count = extents
        .iter()
        .try_fold(1usize, |count, extent| count.checked_mul(*extent as usize))?;
    if index >= scalar_count {
        return None;
    }
    let mut remainder = index;
    let mut coordinates = Vec::with_capacity(extents.len());
    for extent in extents.iter().rev() {
        if *extent == 0 {
            return None;
        }
        coordinates.push(u32::try_from(remainder % *extent as usize).ok()?);
        remainder /= *extent as usize;
    }
    coordinates.reverse();
    Some(coordinates)
}

fn flatten_coordinates(extents: &[u32], coordinates: &[u32]) -> Option<usize> {
    if extents.len() != coordinates.len() {
        return None;
    }
    extents
        .iter()
        .zip(coordinates)
        .try_fold(0usize, |flat, (extent, coordinate)| {
            if coordinate >= extent {
                return None;
            }
            flat.checked_mul(*extent as usize)?
                .checked_add(*coordinate as usize)
        })
}

#[cfg(test)]
mod tests {
    use rumoca_core::{
        SourceMap, Span, StructuredIndexBinder, StructuredIndexDomain, TypeId, VarName,
    };

    use super::*;

    fn provenance(source: rumoca_core::SourceId, start: usize, end: usize) -> dae::DaeProvenance {
        dae::DaeProvenance::source(Span::from_offsets(source, start, end)).unwrap()
    }

    #[test]
    fn literal_and_slice_indices_select_exact_coordinate_scalars() {
        let mut sources = SourceMap::new();
        let source = sources.add("projection.mo", "Real x[3]; x[2]; x[{3,1}];");
        let declaration = provenance(source, 0, 10);
        let first_use = provenance(source, 11, 15);
        let second_use = provenance(source, 17, 25);
        let model = dae::Dae::construct(sources, |model| {
            let real_array = model.types(|types| {
                types.intern(
                    TypeId::new(0),
                    dae::ValueType::array(dae::ScalarType::Real, [3]),
                    declaration,
                )
            })?;
            let x = model.variables(|variables| {
                variables.algebraic(
                    VarName::new("x"),
                    real_array,
                    declaration,
                    dae::VariableAttributes::default(),
                )
            })?;
            model.expressions(|expressions| {
                let x_first = expressions
                    .at(first_use)
                    .coordinate(dae::CoordinateInput::Algebraic(x))?;
                let two = expressions
                    .at(first_use)
                    .literal(dae::DaeLiteral::Integer(2))?;
                expressions.at(first_use).index(
                    x_first,
                    [dae::Subscript::Index {
                        expression: two,
                        provenance: first_use,
                    }],
                )?;
                let x_second = expressions
                    .at(second_use)
                    .coordinate(dae::CoordinateInput::Algebraic(x))?;
                let three = expressions
                    .at(second_use)
                    .literal(dae::DaeLiteral::Integer(3))?;
                let one = expressions
                    .at(second_use)
                    .literal(dae::DaeLiteral::Integer(1))?;
                let selection = expressions.at(second_use).array([three, one])?;
                expressions.at(second_use).index(
                    x_second,
                    [dae::Subscript::Value {
                        expression: selection,
                        provenance: second_use,
                    }],
                )?;
                Ok(())
            })
        })
        .unwrap();

        model.inspect(|view| {
            let scalar = view.expression_id(2).unwrap();
            let mut selected = Vec::new();
            for_each_scalar_coordinate(view, scalar, 0, None, |coordinate, index| {
                assert!(matches!(coordinate, dae::CoordinateView::Algebraic(_)));
                selected.push(index);
            })
            .unwrap();
            assert_eq!(selected, [1]);

            let slice = view.expression_id(7).unwrap();
            selected.clear();
            for_each_scalar_coordinate(view, slice, 0, None, |_, scalar| selected.push(scalar))
                .unwrap();
            for_each_scalar_coordinate(view, slice, 1, None, |_, scalar| selected.push(scalar))
                .unwrap();
            assert_eq!(selected, [2, 0]);
        });
    }

    #[test]
    fn binder_substitution_uses_the_exact_domain_value() {
        let mut sources = SourceMap::new();
        let source = sources.add("binder.mo", "Real x[3]; for i in 1:3 loop x[i]; end for;");
        let declaration = provenance(source, 0, 10);
        let owner = provenance(source, 11, 43);
        let use_site = provenance(source, 29, 33);
        let model = dae::Dae::construct(sources, |model| {
            let real_array = model.types(|types| {
                types.intern(
                    TypeId::new(0),
                    dae::ValueType::array(dae::ScalarType::Real, [3]),
                    declaration,
                )
            })?;
            let x = model.variables(|variables| {
                variables.algebraic(
                    VarName::new("x"),
                    real_array,
                    declaration,
                    dae::VariableAttributes::default(),
                )
            })?;
            let domain = model.domains(|domains| {
                domains.structured(
                    StructuredIndexDomain {
                        binders: vec![StructuredIndexBinder {
                            id: 0,
                            display_name: "i".to_string(),
                            lower: 1,
                            upper: 3,
                            step: 1,
                        }],
                    },
                    owner,
                )
            })?;
            let binder = model.domains(|domains| domains.binder(domain, 0, use_site))?;
            model.expressions(|expressions| {
                let x = expressions
                    .at(use_site)
                    .coordinate(dae::CoordinateInput::Algebraic(x))?;
                let i = expressions.at(use_site).binder(binder)?;
                expressions.at(use_site).index(
                    x,
                    [dae::Subscript::Index {
                        expression: i,
                        provenance: use_site,
                    }],
                )?;
                Ok(())
            })
        })
        .unwrap();

        model.inspect(|view| {
            let domain = view.domain_id(0).unwrap();
            let expression = view.expression_id(2).unwrap();
            let mut selected = Vec::new();
            for_each_scalar_coordinate(view, expression, 0, Some((domain, &[2])), |_, index| {
                selected.push(index);
            })
            .unwrap();
            assert_eq!(selected, [1]);
        });
    }

    #[test]
    fn dynamic_index_reports_all_potential_values_and_its_index_dependency() {
        let mut sources = SourceMap::new();
        let source = sources.add("dynamic.mo", "Real x[3]; input Integer i; x[i];");
        let x_at = provenance(source, 0, 10);
        let i_at = provenance(source, 12, 26);
        let use_at = provenance(source, 28, 32);
        let model = dae::Dae::construct(sources, |model| {
            let real_array = model.types(|types| {
                types.intern(
                    TypeId::new(0),
                    dae::ValueType::array(dae::ScalarType::Real, [3]),
                    x_at,
                )
            })?;
            let integer = model.types(|types| {
                types.intern(
                    TypeId::new(1),
                    dae::ValueType::scalar(dae::ScalarType::Integer),
                    i_at,
                )
            })?;
            let (x, i) = model.variables(|variables| {
                Ok((
                    variables.algebraic(
                        VarName::new("x"),
                        real_array,
                        x_at,
                        dae::VariableAttributes::default(),
                    )?,
                    variables.input(
                        VarName::new("i"),
                        integer,
                        dae::InputVariability::Discrete,
                        i_at,
                        dae::VariableAttributes::default(),
                    )?,
                ))
            })?;
            model.expressions(|expressions| {
                let x = expressions
                    .at(use_at)
                    .coordinate(dae::CoordinateInput::Algebraic(x))?;
                let i = expressions
                    .at(use_at)
                    .coordinate(dae::CoordinateInput::Input(i))?;
                expressions.at(use_at).index(
                    x,
                    [dae::Subscript::Index {
                        expression: i,
                        provenance: use_at,
                    }],
                )?;
                Ok(())
            })
        })
        .unwrap();

        model.inspect(|view| {
            let mut selected = Vec::new();
            for_each_scalar_coordinate(
                view,
                view.expression_id(2).unwrap(),
                0,
                None,
                |coordinate, scalar| selected.push((coordinate, scalar)),
            )
            .unwrap();
            assert_eq!(selected.len(), 4);
            assert_eq!(selected[0].1, 0);
            assert_eq!(selected[1].1, 1);
            assert_eq!(selected[2].1, 2);
            assert_eq!(selected[3].1, 0);
            assert!(matches!(selected[3].0, dae::CoordinateView::Input(_)));
        });
    }
}
