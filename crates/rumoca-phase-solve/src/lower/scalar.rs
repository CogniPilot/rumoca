use super::*;

pub(super) struct ScaledDerivativeProgram<'dae> {
    pub(super) numerator: dae::ExprId<'dae>,
    pub(super) numerator_scalar: usize,
    pub(super) coefficient: dae::ExprId<'dae>,
    pub(super) coefficient_scalar: usize,
    pub(super) negate: bool,
    pub(super) span: Span,
}

#[derive(Clone, Copy)]
enum ReductionKind {
    Sum,
    Product,
    Minimum,
    Maximum,
}

impl ReductionKind {
    fn operator(self) -> solve::BinaryOp {
        match self {
            Self::Sum => solve::BinaryOp::Add,
            Self::Product => solve::BinaryOp::Mul,
            Self::Minimum => solve::BinaryOp::Min,
            Self::Maximum => solve::BinaryOp::Max,
        }
    }

    fn identity(self) -> Option<f64> {
        match self {
            Self::Sum => Some(0.0),
            Self::Product => Some(1.0),
            Self::Minimum | Self::Maximum => None,
        }
    }
}

pub(super) struct ScalarCompiler<'layout, 'dae> {
    view: dae::DaeView<'dae>,
    layout: &'layout LoweredLayout<'dae>,
    domain_points: Vec<(dae::DomainId<'dae>, Vec<i64>)>,
    function_arguments: Vec<(dae::FunctionId<'dae>, Vec<dae::ExprId<'dae>>)>,
    function_fold_values: Vec<(dae::FunctionFoldId<'dae>, Vec<Vec<solve::Reg>>)>,
    active_clock: Option<dae::ClockId<'dae>>,
    ops: Vec<solve::LinearOp>,
    next_register: solve::Reg,
}

impl<'layout, 'dae> ScalarCompiler<'layout, 'dae> {
    pub(super) fn new(
        view: dae::DaeView<'dae>,
        layout: &'layout LoweredLayout<'dae>,
        domain_point: Option<(dae::DomainId<'dae>, &[i64])>,
    ) -> Self {
        Self {
            view,
            layout,
            domain_points: domain_point
                .map(|(domain, values)| vec![(domain, values.to_vec())])
                .unwrap_or_default(),
            function_arguments: Vec::new(),
            function_fold_values: Vec::new(),
            active_clock: None,
            ops: Vec::new(),
            next_register: 0,
        }
    }

    pub(super) fn program(
        mut self,
        expression: dae::ExprId<'dae>,
        scalar: usize,
    ) -> Result<Vec<solve::LinearOp>, LowerError> {
        let output = self.expression(expression, scalar)?;
        self.ops.push(solve::LinearOp::StoreOutput { src: output });
        Ok(self.ops)
    }

    pub(super) fn clocked_program(
        mut self,
        clock: dae::ClockId<'dae>,
        expression: dae::ExprId<'dae>,
        scalar: usize,
    ) -> Result<Vec<solve::LinearOp>, LowerError> {
        self.active_clock = Some(clock);
        let output = self.expression(expression, scalar)?;
        self.ops.push(solve::LinearOp::StoreOutput { src: output });
        Ok(self.ops)
    }

    pub(super) fn scaled_derivative_program(
        mut self,
        input: ScaledDerivativeProgram<'dae>,
    ) -> Result<Vec<solve::LinearOp>, LowerError> {
        let mut numerator = self.expression(input.numerator, input.numerator_scalar)?;
        if input.negate {
            numerator = self.unary(dae::UnaryOperator::Negate, numerator, input.span)?;
        }
        let coefficient = self.expression(input.coefficient, input.coefficient_scalar)?;
        let output = self.binary(
            dae::BinaryOperator::Divide,
            numerator,
            coefficient,
            input.span,
        )?;
        self.ops.push(solve::LinearOp::StoreOutput { src: output });
        Ok(self.ops)
    }

    pub(super) fn condition_program(
        mut self,
        condition: dae::ConditionId<'dae>,
    ) -> Result<Vec<solve::LinearOp>, LowerError> {
        let output = self.condition(condition)?;
        self.ops.push(solve::LinearOp::StoreOutput { src: output });
        Ok(self.ops)
    }

    pub(super) fn edge_condition_program(
        mut self,
        trigger: dae::ConditionId<'dae>,
        guard: dae::ConditionId<'dae>,
        trigger_memory: usize,
        span: Span,
    ) -> Result<Vec<solve::LinearOp>, LowerError> {
        let edge = self.trigger_edge(trigger, trigger_memory, span)?;
        let guard = self.condition(guard)?;
        let output = self.binary(dae::BinaryOperator::And, edge, guard, span)?;
        self.ops.push(solve::LinearOp::StoreOutput { src: output });
        Ok(self.ops)
    }

    pub(super) fn root_program(
        mut self,
        relation: dae::RelationId<'dae>,
    ) -> Result<Vec<solve::LinearOp>, LowerError> {
        let relation = self
            .view
            .relation(relation)
            .expect("checked relation identity resolves");
        let node = self.node(relation.expression());
        let span = relation.provenance().span();
        let output = match node.operation() {
            dae::ExpressionOperation::Binary {
                operator: dae::BinaryOperator::Less | dae::BinaryOperator::LessEqual,
                lhs,
                rhs,
            } => {
                let lhs = self.expression(lhs, 0)?;
                let rhs = self.expression(rhs, 0)?;
                self.binary(dae::BinaryOperator::Subtract, lhs, rhs, span)?
            }
            dae::ExpressionOperation::Binary {
                operator: dae::BinaryOperator::Greater | dae::BinaryOperator::GreaterEqual,
                lhs,
                rhs,
            } => {
                let lhs = self.expression(lhs, 0)?;
                let rhs = self.expression(rhs, 0)?;
                self.binary(dae::BinaryOperator::Subtract, rhs, lhs, span)?
            }
            _ => {
                let condition = self.expression(relation.expression(), 0)?;
                let when_true = self.constant(-1.0, span)?;
                let when_false = self.constant(1.0, span)?;
                self.select(condition, when_true, when_false, span)?
            }
        };
        self.ops.push(solve::LinearOp::StoreOutput { src: output });
        Ok(self.ops)
    }

    pub(super) fn guarded_assignments_program(
        mut self,
        branches: &[(
            dae::ConditionId<'dae>,
            dae::ConditionId<'dae>,
            dae::ExprId<'dae>,
            usize,
            usize,
        )],
        target: solve::ScalarSlot,
        span: Span,
    ) -> Result<Vec<solve::LinearOp>, LowerError> {
        let mut selected = self.load_slot(target, span)?;
        for &(trigger, guard, value, scalar, trigger_memory) in branches.iter().rev() {
            let edge = self.trigger_edge(trigger, trigger_memory, span)?;
            let guard = self.condition(guard)?;
            let condition = self.binary(dae::BinaryOperator::And, edge, guard, span)?;
            let value = self.expression(value, scalar)?;
            selected = self.select(condition, value, selected, span)?;
        }
        self.ops
            .push(solve::LinearOp::StoreOutput { src: selected });
        Ok(self.ops)
    }

    pub(super) fn clocked_guarded_assignments_program(
        mut self,
        clock: dae::ClockId<'dae>,
        branches: &[(
            dae::ConditionId<'dae>,
            dae::ConditionId<'dae>,
            dae::ExprId<'dae>,
            usize,
            usize,
        )],
        target: solve::ScalarSlot,
        span: Span,
    ) -> Result<Vec<solve::LinearOp>, LowerError> {
        self.active_clock = Some(clock);
        let mut selected = self.load_slot(target, span)?;
        for &(_, guard, value, scalar, _) in branches.iter().rev() {
            let condition = self.condition(guard)?;
            let value = self.expression(value, scalar)?;
            selected = self.select(condition, value, selected, span)?;
        }
        self.ops
            .push(solve::LinearOp::StoreOutput { src: selected });
        Ok(self.ops)
    }

    fn trigger_edge(
        &mut self,
        trigger: dae::ConditionId<'dae>,
        trigger_memory: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let current = self.condition(trigger)?;
        let previous = self.load_slot(solve::scalar_slot_p(trigger_memory), span)?;
        let not_previous = self.unary(dae::UnaryOperator::Not, previous, span)?;
        self.binary(dae::BinaryOperator::And, current, not_previous, span)
    }

    pub(super) fn packed_pair(
        mut self,
        lhs: dae::ExprId<'dae>,
        rhs: dae::ExprId<'dae>,
    ) -> Result<(solve::Reg, solve::Reg, solve::Reg, Vec<solve::LinearOp>), LowerError> {
        let lhs_start = self.pack_expression(lhs)?;
        let rhs_start = self.pack_expression(rhs)?;
        Ok((lhs_start, rhs_start, self.next_register, self.ops))
    }

    fn pack_expression(&mut self, expression: dae::ExprId<'dae>) -> Result<solve::Reg, LowerError> {
        let count = scalar_count(self.view, expression);
        let mut values = Vec::with_capacity(count);
        for scalar in 0..count {
            values.push(self.expression(expression, scalar)?);
        }
        let span = self.node(expression).provenance().span();
        let start = self.next_register;
        for value in values {
            let dst = self.register(span)?;
            self.ops.push(solve::LinearOp::Move { dst, src: value });
        }
        Ok(start)
    }

    fn expression(
        &mut self,
        expression: dae::ExprId<'dae>,
        scalar: usize,
    ) -> Result<solve::Reg, LowerError> {
        let node = self.node(expression);
        self.expect_scalar(node, scalar)?;
        match node.operation() {
            dae::ExpressionOperation::Literal(value) => {
                self.literal(value, node.provenance().span())
            }
            dae::ExpressionOperation::Coordinate(coordinate) => {
                self.coordinate(coordinate, scalar, node.provenance().span())
            }
            dae::ExpressionOperation::Unary { operator, operand } => {
                let operand = self.expression(operand, scalar)?;
                self.unary(operator, operand, node.provenance().span())
            }
            dae::ExpressionOperation::Binary { operator, lhs, rhs } => {
                self.binary_expression(operator, lhs, rhs, scalar, node.provenance().span())
            }
            dae::ExpressionOperation::Conditional(operands) => {
                self.conditional(operands, scalar, node.provenance().span())
            }
            dae::ExpressionOperation::Array(elements) => {
                let (element, element_scalar) = self.select_array(elements, scalar);
                self.expression(element, element_scalar)
            }
            dae::ExpressionOperation::Record(_) => Err(LowerError::contract(
                "record value escaped a checked field projection",
                node.provenance().span(),
            )),
            dae::ExpressionOperation::Field { base, field } => {
                self.record_field(base, field as usize, scalar, node.provenance().span())
            }
            dae::ExpressionOperation::Range { start, step, .. } => {
                self.range(start, step, scalar, node.provenance().span())
            }
            dae::ExpressionOperation::Comprehension { domain, body } => {
                self.comprehension(domain, body, scalar)
            }
            dae::ExpressionOperation::Index { base, subscripts } => {
                self.index(base, subscripts, node.value_type().dimensions(), scalar)
            }
            dae::ExpressionOperation::ArrayUpdate {
                base,
                value,
                subscripts,
            } => {
                let selected = ScalarSelector::from_points(self.view, &self.domain_points)
                    .array_update_value_scalar(
                        base,
                        subscripts,
                        self.node(value).value_type().dimensions(),
                        scalar,
                    )?;
                match selected {
                    Some(value_scalar) => self.expression(value, value_scalar),
                    None => self.expression(base, scalar),
                }
            }
            dae::ExpressionOperation::Builtin { builtin, arguments } => {
                self.builtin(builtin, arguments, scalar, node.provenance().span())
            }
            dae::ExpressionOperation::Call {
                function,
                output,
                arguments,
            } => self.function_call(
                function,
                output,
                arguments,
                scalar,
                node.provenance().span(),
            ),
            dae::ExpressionOperation::FunctionValue { definition, .. } => {
                self.expression(definition.rhs(), scalar)
            }
            dae::ExpressionOperation::FunctionFoldParameter { fold, carried, .. } => {
                self.function_fold_parameter(fold, carried, scalar, node.provenance().span())
            }
            dae::ExpressionOperation::FunctionFoldOutput { fold, carried, .. } => {
                self.function_fold_output(fold, carried, scalar, node.provenance().span())
            }
        }
    }

    fn function_fold_parameter(
        &self,
        fold: dae::FunctionFoldId<'dae>,
        carried: u32,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let values = self
            .function_fold_values
            .iter()
            .rev()
            .find_map(|(active, values)| (*active == fold).then_some(values))
            .ok_or_else(|| {
                LowerError::contract("function loop parameter escaped its checked fold", span)
            })?;
        values
            .get(carried as usize)
            .and_then(|value| value.get(scalar))
            .copied()
            .ok_or_else(|| {
                LowerError::contract("function loop parameter scalar is out of range", span)
            })
    }

    fn record_field(
        &mut self,
        expression: dae::ExprId<'dae>,
        field: usize,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let node = self.node(expression);
        match node.operation() {
            dae::ExpressionOperation::Record(fields) => self.expression(
                fields
                    .get(field)
                    .ok_or_else(|| LowerError::contract("record field is out of range", span))?,
                scalar,
            ),
            dae::ExpressionOperation::Call {
                function,
                output,
                arguments,
            } => {
                if self.function_arguments.len() >= 256 {
                    return Err(LowerError::non_computable(
                        "function lowering exceeded the checked recursion limit",
                        span,
                    ));
                }
                let result = self
                    .view
                    .function(function)
                    .and_then(|definition| definition.result_values().rhs(output as usize))
                    .ok_or_else(|| {
                        LowerError::contract("function result ordinal is out of range", span)
                    })?;
                self.function_arguments
                    .push((function, arguments.iter().collect()));
                let lowered = self.record_field(result, field, scalar, span);
                self.function_arguments.pop();
                lowered
            }
            dae::ExpressionOperation::FunctionValue { definition, .. } => {
                self.record_field(definition.rhs(), field, scalar, span)
            }
            dae::ExpressionOperation::Conditional(operands) => {
                let fallback = operands
                    .get(operands.len() - 1)
                    .expect("checked conditional has a fallback");
                let mut selected = self.record_field(fallback, field, scalar, span)?;
                for ordinal in (0..operands.len() - 1).step_by(2).rev() {
                    let condition = self.expression(
                        operands
                            .get(ordinal)
                            .expect("checked conditional condition ordinal"),
                        0,
                    )?;
                    let value = self.record_field(
                        operands
                            .get(ordinal + 1)
                            .expect("checked conditional value ordinal"),
                        field,
                        scalar,
                        span,
                    )?;
                    selected = self.select(condition, value, selected, span)?;
                }
                Ok(selected)
            }
            _ => Err(LowerError::contract(
                "record field has no checked aggregate definition",
                span,
            )),
        }
    }

    fn function_fold_output(
        &mut self,
        fold: dae::FunctionFoldId<'dae>,
        carried: u32,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let fold_view = self
            .view
            .function_fold(fold)
            .ok_or_else(|| LowerError::contract("function fold identity does not resolve", span))?;
        let mut values = fold_view
            .initial_values()
            .rhs_iter()
            .map(|initial| {
                (0..scalar_count(self.view, initial))
                    .map(|element| self.expression(initial, element))
                    .collect::<Result<Vec<_>, _>>()
            })
            .collect::<Result<Vec<_>, _>>()?;
        let domain = self
            .view
            .domain(fold_view.domain())
            .expect("checked function fold domain resolves");
        let structured = domain.structured();
        let point_count = structured.scalar_count().map_err(|error| {
            LowerError::contract(
                format!("checked function fold domain became invalid: {error}"),
                span,
            )
        })?;
        for point in 0..point_count {
            let indices = structured
                .index_tuple_at(point)
                .expect("checked function fold domain remains valid")
                .expect("checked function fold point is in range");
            self.domain_points.push((fold_view.domain(), indices));
            self.function_fold_values.push((fold, values));
            let updates = fold_view
                .update_values()
                .rhs_iter()
                .map(|update| {
                    (0..scalar_count(self.view, update))
                        .map(|element| self.expression(update, element))
                        .collect::<Result<Vec<_>, _>>()
                })
                .collect::<Result<Vec<_>, _>>();
            let (_, previous) = self
                .function_fold_values
                .pop()
                .expect("function fold frame was just pushed");
            self.domain_points.pop();
            values = updates?;
            debug_assert_eq!(
                previous.len(),
                values.len(),
                "checked DAE fold preserves carried arity"
            );
        }
        values
            .get(carried as usize)
            .and_then(|value| value.get(scalar))
            .copied()
            .ok_or_else(|| {
                LowerError::contract("function fold output scalar is out of range", span)
            })
    }

    fn comprehension(
        &mut self,
        domain: dae::DomainId<'dae>,
        body: dae::ExprId<'dae>,
        scalar: usize,
    ) -> Result<solve::Reg, LowerError> {
        let domain_view = self
            .view
            .domain(domain)
            .expect("checked comprehension domain resolves");
        let body_count = scalar_count(self.view, body);
        let point = scalar / body_count;
        let body_scalar = scalar % body_count;
        let values = domain_view
            .structured()
            .index_tuple_at(point)
            .expect("checked domain remains valid")
            .expect("checked comprehension scalar point is in range");
        self.domain_points.push((domain, values));
        let result = self.expression(body, body_scalar);
        self.domain_points.pop();
        result
    }

    fn index(
        &mut self,
        base: dae::ExprId<'dae>,
        subscripts: dae::SubscriptsView<'dae>,
        dimensions: &[u32],
        scalar: usize,
    ) -> Result<solve::Reg, LowerError> {
        let selector = ScalarSelector::from_points(self.view, &self.domain_points);
        match selector.indexed_base_scalar(base, subscripts, dimensions, scalar) {
            Ok(selected) => self.expression(base, selected),
            Err(LowerError::NonComputable { reason, .. })
                if reason == "array subscript is not compile-time computable" =>
            {
                self.dynamic_vector_index(base, subscripts, dimensions, scalar)
            }
            Err(error) => Err(error),
        }
    }

    fn dynamic_vector_index(
        &mut self,
        base: dae::ExprId<'dae>,
        subscripts: dae::SubscriptsView<'dae>,
        result_dimensions: &[u32],
        result_scalar: usize,
    ) -> Result<solve::Reg, LowerError> {
        let span = self.node(base).provenance().span();
        let [extent] = self.node(base).value_type().dimensions() else {
            return Err(LowerError::non_computable(
                "runtime indexing currently requires a rank-one array",
                span,
            ));
        };
        if !result_dimensions.is_empty() || result_scalar != 0 || subscripts.len() != 1 {
            return Err(LowerError::non_computable(
                "runtime indexing currently requires one scalar index and one scalar result",
                span,
            ));
        }
        let Some(dae::SubscriptView::Index {
            expression,
            provenance,
        }) = subscripts.get(0)
        else {
            return Err(LowerError::non_computable(
                "runtime slices do not yet have a computable Solve owner",
                span,
            ));
        };
        let span = provenance.span();
        let runtime_index = self.expression(expression, 0)?;
        let zero = self.constant(0.0, span)?;
        let mut selected = self.binary(dae::BinaryOperator::Divide, zero, zero, span)?;
        for ordinal in 0..*extent as usize {
            let candidate = self.expression(base, ordinal)?;
            let modelica_index = self.constant((ordinal + 1) as f64, span)?;
            let matches = self.binary(
                dae::BinaryOperator::Equal,
                runtime_index,
                modelica_index,
                span,
            )?;
            selected = self.select(matches, candidate, selected, span)?;
        }
        Ok(selected)
    }

    fn range(
        &mut self,
        start: i64,
        step: i64,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let offset = i64::try_from(scalar)
            .map_err(|_| LowerError::contract("range scalar ordinal overflow", span))?;
        let scaled = step
            .checked_mul(offset)
            .ok_or_else(|| LowerError::contract("range scalar multiplication overflow", span))?;
        let value = start
            .checked_add(scaled)
            .ok_or_else(|| LowerError::contract("range scalar addition overflow", span))?;
        self.constant(value as f64, span)
    }

    fn literal(&mut self, value: &dae::DaeLiteral, span: Span) -> Result<solve::Reg, LowerError> {
        let value = match value {
            dae::DaeLiteral::Real(value) => *value,
            dae::DaeLiteral::Integer(value) => *value as f64,
            dae::DaeLiteral::Enumeration(value) => *value as f64,
            dae::DaeLiteral::Boolean(value) => f64::from(*value),
            dae::DaeLiteral::String(_) => {
                return Err(LowerError::unsupported(
                    "String values are not numeric Solve coordinates",
                    span,
                ));
            }
        };
        self.constant(value, span)
    }

    fn coordinate(
        &mut self,
        coordinate: dae::CoordinateView<'dae>,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        if let dae::CoordinateView::FunctionParameter(parameter) = coordinate {
            return self.function_parameter(parameter, scalar, span);
        }
        if let dae::CoordinateView::Binder(binder) = coordinate {
            let Some((_, values)) = self
                .domain_points
                .iter()
                .rev()
                .find(|(domain, _)| *domain == binder.domain())
            else {
                return Err(LowerError::non_computable(
                    "domain binder escaped its structured owner",
                    span,
                ));
            };
            let value = values
                .get(binder.ordinal() as usize)
                .copied()
                .ok_or_else(|| LowerError::contract("binder ordinal is out of range", span))?;
            return self.constant(value as f64, span);
        }
        if matches!(coordinate, dae::CoordinateView::Time) {
            let dst = self.register(span)?;
            self.ops.push(solve::LinearOp::LoadTime { dst });
            return Ok(dst);
        }
        if matches!(coordinate, dae::CoordinateView::Derivative(_)) {
            return Err(LowerError::non_computable(
                "derivative coordinate escaped checked structural substitution",
                span,
            ));
        }
        let slot = if let dae::CoordinateView::Delay(delay_id) = coordinate {
            delay_value_scalar_slot(self.layout, delay_id.index(), scalar, span)?
        } else if let dae::CoordinateView::Previous(previous_id) = coordinate {
            let previous = self
                .view
                .previous(previous_id)
                .expect("checked previous identity resolves");
            if self.active_clock != Some(previous.clock()) {
                return Err(LowerError::non_computable(
                    "previous coordinate escaped its owning clock schedule",
                    span,
                ));
            }
            previous_value_scalar_slot(self.layout, previous_id.index(), scalar, span)?
        } else if let Some(variable) = pre_coordinate_variable(coordinate) {
            pre_variable_scalar_slot(self.layout, variable, scalar, span)?
        } else {
            let variable = coordinate_variable(coordinate).ok_or_else(|| {
                LowerError::unsupported(
                    "runtime-managed condition, previous, or terminal coordinate",
                    span,
                )
            })?;
            variable_scalar_slot(self.layout, variable, scalar, span)?
        };
        let dst = self.register(span)?;
        match slot {
            solve::ScalarSlot::Y { index, .. } => {
                self.ops.push(solve::LinearOp::LoadY { dst, index });
            }
            solve::ScalarSlot::P { index, .. } => {
                self.ops.push(solve::LinearOp::LoadP { dst, index });
            }
            solve::ScalarSlot::Time | solve::ScalarSlot::Constant(_) => {
                unreachable!("variable layouts contain only Y/P slots")
            }
        }
        Ok(dst)
    }

    fn function_parameter(
        &mut self,
        parameter: dae::FunctionParameterId<'dae>,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let Some((function, arguments)) = self.function_arguments.pop() else {
            return Err(LowerError::non_computable(
                "function parameter escaped its checked call owner",
                span,
            ));
        };
        let argument = (function == parameter.function())
            .then(|| arguments.get(parameter.ordinal() as usize).copied())
            .flatten();
        let lowered = argument
            .ok_or_else(|| {
                LowerError::non_computable(
                    "function parameter escaped its checked call owner",
                    span,
                )
            })
            .and_then(|argument| self.expression(argument, scalar));
        self.function_arguments.push((function, arguments));
        lowered
    }

    fn condition(&mut self, condition: dae::ConditionId<'dae>) -> Result<solve::Reg, LowerError> {
        let condition = self
            .view
            .condition(condition)
            .expect("checked condition identity resolves");
        let span = condition.provenance().span();
        match condition.operation() {
            dae::ConditionOperation::Initial => {
                let index = self
                    .layout
                    .solve_layout
                    .initial_event_parameter_index
                    .ok_or_else(|| {
                        LowerError::non_computable(
                            "initial condition has no checked Solve storage",
                            span,
                        )
                    })?;
                self.load_slot(solve::scalar_slot_p(index), span)
            }
            dae::ConditionOperation::Relation(relation) => {
                let expression = self
                    .view
                    .relation(relation)
                    .expect("checked condition relation resolves")
                    .expression();
                self.expression(expression, 0)
            }
            dae::ConditionOperation::Discrete(expression) => self.expression(expression, 0),
            dae::ConditionOperation::Clock(clock) => match self.active_clock {
                Some(active) if active == clock => self.constant(1.0, span),
                Some(_) => Err(LowerError::non_computable(
                    "clocked condition refers to a different activation owner",
                    span,
                )),
                None => Err(LowerError::unsupported(
                    "clock condition has no owning Solve schedule",
                    span,
                )),
            },
            dae::ConditionOperation::Not(operand) => {
                let operand = self.condition(operand)?;
                self.unary(dae::UnaryOperator::Not, operand, span)
            }
            dae::ConditionOperation::And(lhs, rhs) => {
                let lhs = self.condition(lhs)?;
                let rhs = self.condition(rhs)?;
                self.binary(dae::BinaryOperator::And, lhs, rhs, span)
            }
            dae::ConditionOperation::Or(lhs, rhs) => {
                let lhs = self.condition(lhs)?;
                let rhs = self.condition(rhs)?;
                self.binary(dae::BinaryOperator::Or, lhs, rhs, span)
            }
        }
    }

    fn load_slot(&mut self, slot: solve::ScalarSlot, span: Span) -> Result<solve::Reg, LowerError> {
        let dst = self.register(span)?;
        match slot {
            solve::ScalarSlot::Y { index, .. } => {
                self.ops.push(solve::LinearOp::LoadY { dst, index });
            }
            solve::ScalarSlot::P { index, .. } => {
                self.ops.push(solve::LinearOp::LoadP { dst, index });
            }
            solve::ScalarSlot::Time => self.ops.push(solve::LinearOp::LoadTime { dst }),
            solve::ScalarSlot::Constant(value) => {
                self.ops.push(solve::LinearOp::Const { dst, value });
            }
        }
        Ok(dst)
    }

    fn select(
        &mut self,
        condition: solve::Reg,
        when_true: solve::Reg,
        when_false: solve::Reg,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let dst = self.register(span)?;
        self.ops.push(solve::LinearOp::Select {
            dst,
            cond: condition,
            if_true: when_true,
            if_false: when_false,
        });
        Ok(dst)
    }

    fn function_call(
        &mut self,
        function: dae::FunctionId<'dae>,
        output: u32,
        arguments: dae::ExpressionOperands<'dae>,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        if self.function_arguments.len() >= 256 {
            return Err(LowerError::non_computable(
                "function lowering exceeded the checked recursion limit",
                span,
            ));
        }
        let definition = self
            .view
            .function(function)
            .ok_or_else(|| LowerError::contract("function identity does not resolve", span))?;
        let result = definition
            .result_values()
            .rhs(output as usize)
            .ok_or_else(|| LowerError::contract("function result ordinal is out of range", span))?;
        self.function_arguments
            .push((function, arguments.iter().collect()));
        let lowered = self.expression(result, scalar);
        self.function_arguments.pop();
        lowered
    }

    fn unary(
        &mut self,
        operator: dae::UnaryOperator,
        operand: solve::Reg,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        if operator == dae::UnaryOperator::Plus {
            return Ok(operand);
        }
        let op = match operator {
            dae::UnaryOperator::Plus => unreachable!(),
            dae::UnaryOperator::Negate => solve::UnaryOp::Neg,
            dae::UnaryOperator::Not => solve::UnaryOp::Not,
        };
        let dst = self.register(span)?;
        self.ops.push(solve::LinearOp::Unary {
            dst,
            op,
            arg: operand,
        });
        Ok(dst)
    }

    fn binary(
        &mut self,
        operator: dae::BinaryOperator,
        lhs: solve::Reg,
        rhs: solve::Reg,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let dst = self.register(span)?;
        let operation = match operator {
            dae::BinaryOperator::Add | dae::BinaryOperator::ElementwiseAdd => {
                solve::LinearOp::Binary {
                    dst,
                    op: solve::BinaryOp::Add,
                    lhs,
                    rhs,
                }
            }
            dae::BinaryOperator::Subtract | dae::BinaryOperator::ElementwiseSubtract => {
                solve::LinearOp::Binary {
                    dst,
                    op: solve::BinaryOp::Sub,
                    lhs,
                    rhs,
                }
            }
            dae::BinaryOperator::Multiply | dae::BinaryOperator::ElementwiseMultiply => {
                solve::LinearOp::Binary {
                    dst,
                    op: solve::BinaryOp::Mul,
                    lhs,
                    rhs,
                }
            }
            dae::BinaryOperator::Divide | dae::BinaryOperator::ElementwiseDivide => {
                solve::LinearOp::Binary {
                    dst,
                    op: solve::BinaryOp::Div,
                    lhs,
                    rhs,
                }
            }
            dae::BinaryOperator::Power | dae::BinaryOperator::ElementwisePower => {
                solve::LinearOp::Binary {
                    dst,
                    op: solve::BinaryOp::Pow,
                    lhs,
                    rhs,
                }
            }
            dae::BinaryOperator::And => solve::LinearOp::Binary {
                dst,
                op: solve::BinaryOp::And,
                lhs,
                rhs,
            },
            dae::BinaryOperator::Or => solve::LinearOp::Binary {
                dst,
                op: solve::BinaryOp::Or,
                lhs,
                rhs,
            },
            comparison => solve::LinearOp::Compare {
                dst,
                op: compare_operator(comparison),
                lhs,
                rhs,
            },
        };
        self.ops.push(operation);
        Ok(dst)
    }

    fn binary_expression(
        &mut self,
        operator: dae::BinaryOperator,
        lhs: dae::ExprId<'dae>,
        rhs: dae::ExprId<'dae>,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        if operator == dae::BinaryOperator::Multiply {
            return self.multiply_expression(lhs, rhs, scalar, span);
        }
        if operator == dae::BinaryOperator::Power && !self.node(lhs).value_type().is_scalar() {
            return Err(LowerError::unsupported(
                "matrix power does not yet have checked Solve lowering",
                span,
            ));
        }
        let lhs_scalar = if scalar_count(self.view, lhs) == 1 {
            0
        } else {
            scalar
        };
        let rhs_scalar = if scalar_count(self.view, rhs) == 1 {
            0
        } else {
            scalar
        };
        let lhs = self.expression(lhs, lhs_scalar)?;
        let rhs = self.expression(rhs, rhs_scalar)?;
        self.binary(operator, lhs, rhs, span)
    }

    fn multiply_expression(
        &mut self,
        lhs: dae::ExprId<'dae>,
        rhs: dae::ExprId<'dae>,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let lhs_dimensions = self.node(lhs).value_type().dimensions().to_vec();
        let rhs_dimensions = self.node(rhs).value_type().dimensions().to_vec();
        match (lhs_dimensions.as_slice(), rhs_dimensions.as_slice()) {
            ([], _) => {
                let lhs = self.expression(lhs, 0)?;
                let rhs = self.expression(rhs, scalar)?;
                self.binary(dae::BinaryOperator::Multiply, lhs, rhs, span)
            }
            (_, []) => {
                let lhs = self.expression(lhs, scalar)?;
                let rhs = self.expression(rhs, 0)?;
                self.binary(dae::BinaryOperator::Multiply, lhs, rhs, span)
            }
            ([inner], [rhs_inner]) if inner == rhs_inner => {
                self.dot_product(lhs, rhs, *inner as usize, 0, 1, 0, 1, span)
            }
            ([_, inner], [rhs_inner]) if inner == rhs_inner => {
                let row_start = scalar
                    .checked_mul(*inner as usize)
                    .ok_or_else(|| LowerError::contract("matrix row offset overflow", span))?;
                self.dot_product(lhs, rhs, *inner as usize, row_start, 1, 0, 1, span)
            }
            ([inner], [rhs_inner, columns]) if inner == rhs_inner => self.dot_product(
                lhs,
                rhs,
                *inner as usize,
                0,
                1,
                scalar,
                *columns as usize,
                span,
            ),
            ([_, inner], [rhs_inner, columns]) if inner == rhs_inner => {
                let columns = *columns as usize;
                let row = scalar / columns;
                let column = scalar % columns;
                let row_start = row
                    .checked_mul(*inner as usize)
                    .ok_or_else(|| LowerError::contract("matrix row offset overflow", span))?;
                self.dot_product(
                    lhs,
                    rhs,
                    *inner as usize,
                    row_start,
                    1,
                    column,
                    columns,
                    span,
                )
            }
            _ => Err(LowerError::contract(
                "checked multiplication shape has no scalar projection",
                span,
            )),
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn dot_product(
        &mut self,
        lhs: dae::ExprId<'dae>,
        rhs: dae::ExprId<'dae>,
        count: usize,
        lhs_start: usize,
        lhs_stride: usize,
        rhs_start: usize,
        rhs_stride: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let mut sum = None;
        for term in 0..count {
            let lhs_index =
                lhs_start
                    .checked_add(term.checked_mul(lhs_stride).ok_or_else(|| {
                        LowerError::contract("dot-product lhs offset overflow", span)
                    })?)
                    .ok_or_else(|| LowerError::contract("dot-product lhs offset overflow", span))?;
            let rhs_index =
                rhs_start
                    .checked_add(term.checked_mul(rhs_stride).ok_or_else(|| {
                        LowerError::contract("dot-product rhs offset overflow", span)
                    })?)
                    .ok_or_else(|| LowerError::contract("dot-product rhs offset overflow", span))?;
            let lhs_term = self.expression(lhs, lhs_index)?;
            let rhs_term = self.expression(rhs, rhs_index)?;
            let product = self.binary(dae::BinaryOperator::Multiply, lhs_term, rhs_term, span)?;
            sum = Some(match sum {
                Some(previous) => self.binary(dae::BinaryOperator::Add, previous, product, span)?,
                None => product,
            });
        }
        match sum {
            Some(sum) => Ok(sum),
            None => self.constant(0.0, span),
        }
    }

    fn conditional(
        &mut self,
        operands: dae::ExpressionOperands<'dae>,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let fallback = operands
            .get(operands.len() - 1)
            .expect("checked conditional has a fallback");
        let mut selected = self.expression(fallback, scalar)?;
        for index in (0..operands.len() - 1).step_by(2).rev() {
            let condition =
                self.expression(operands.get(index).expect("checked condition ordinal"), 0)?;
            let value = self.expression(
                operands
                    .get(index + 1)
                    .expect("checked conditional value ordinal"),
                scalar,
            )?;
            let dst = self.register(span)?;
            self.ops.push(solve::LinearOp::Select {
                dst,
                cond: condition,
                if_true: value,
                if_false: selected,
            });
            selected = dst;
        }
        Ok(selected)
    }

    fn builtin(
        &mut self,
        builtin: dae::PureBuiltin,
        arguments: dae::ExpressionOperands<'dae>,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
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
            | dae::PureBuiltin::Log10 => {
                let argument = self.expression(
                    arguments.get(0).expect("checked unary builtin argument"),
                    scalar,
                )?;
                let dst = self.register(span)?;
                self.ops.push(solve::LinearOp::Unary {
                    dst,
                    op: unary_builtin(builtin),
                    arg: argument,
                });
                Ok(dst)
            }
            dae::PureBuiltin::Atan2 => self.atan2(arguments, scalar, span),
            dae::PureBuiltin::Div | dae::PureBuiltin::Mod | dae::PureBuiltin::Rem => {
                self.quotient(builtin, arguments, scalar, span)
            }
            dae::PureBuiltin::Smooth => self.expression(
                arguments.get(1).expect("checked smooth value argument"),
                scalar,
            ),
            dae::PureBuiltin::NoEvent => self.expression(
                arguments.get(0).expect("checked noEvent value argument"),
                scalar,
            ),
            dae::PureBuiltin::Homotopy => self.homotopy(arguments, scalar, span),
            dae::PureBuiltin::Sum => self.reduction(
                arguments.get(0).expect("checked reduction argument"),
                ReductionKind::Sum,
                span,
            ),
            dae::PureBuiltin::Product => self.reduction(
                arguments.get(0).expect("checked reduction argument"),
                ReductionKind::Product,
                span,
            ),
            dae::PureBuiltin::Min | dae::PureBuiltin::Max => {
                let reduction = if builtin == dae::PureBuiltin::Min {
                    ReductionKind::Minimum
                } else {
                    ReductionKind::Maximum
                };
                if arguments.len() == 1 {
                    self.reduction(
                        arguments.get(0).expect("checked reduction argument"),
                        reduction,
                        span,
                    )
                } else {
                    let values = arguments
                        .iter()
                        .map(|argument| self.expression(argument, scalar))
                        .collect::<Result<Vec<_>, _>>()?;
                    self.fold_registers(values, reduction.operator(), span)
                }
            }
            dae::PureBuiltin::Size => self.size_builtin(arguments, scalar, span),
            dae::PureBuiltin::Zeros => self.constant(0.0, span),
            dae::PureBuiltin::Ones => self.constant(1.0, span),
            dae::PureBuiltin::Fill => {
                self.expression(arguments.get(0).expect("checked fill value argument"), 0)
            }
            dae::PureBuiltin::Linspace => self.linspace(arguments, scalar, span),
            dae::PureBuiltin::Cross => self.cross(arguments, scalar, span),
        }
    }

    fn atan2(
        &mut self,
        arguments: dae::ExpressionOperands<'dae>,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let lhs = self.expression(
            arguments.get(0).expect("checked atan2 first argument"),
            scalar,
        )?;
        let rhs = self.expression(
            arguments.get(1).expect("checked atan2 second argument"),
            scalar,
        )?;
        let dst = self.register(span)?;
        self.ops.push(solve::LinearOp::Binary {
            dst,
            op: solve::BinaryOp::Atan2,
            lhs,
            rhs,
        });
        Ok(dst)
    }

    fn quotient(
        &mut self,
        builtin: dae::PureBuiltin,
        arguments: dae::ExpressionOperands<'dae>,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let lhs = self.expression(
            arguments.get(0).expect("checked quotient first argument"),
            scalar,
        )?;
        let rhs = self.expression(
            arguments.get(1).expect("checked quotient second argument"),
            scalar,
        )?;
        let ratio = self.binary(dae::BinaryOperator::Divide, lhs, rhs, span)?;
        let quotient = self.solve_unary(
            if builtin == dae::PureBuiltin::Mod {
                solve::UnaryOp::Floor
            } else {
                solve::UnaryOp::Trunc
            },
            ratio,
            span,
        )?;
        if builtin == dae::PureBuiltin::Div {
            return Ok(quotient);
        }
        let multiple = self.binary(dae::BinaryOperator::Multiply, quotient, rhs, span)?;
        self.binary(dae::BinaryOperator::Subtract, lhs, multiple, span)
    }

    fn solve_unary(
        &mut self,
        op: solve::UnaryOp,
        argument: solve::Reg,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let dst = self.register(span)?;
        self.ops.push(solve::LinearOp::Unary {
            dst,
            op,
            arg: argument,
        });
        Ok(dst)
    }

    fn homotopy(
        &mut self,
        arguments: dae::ExpressionOperands<'dae>,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let actual = self.expression(
            arguments.get(0).expect("checked homotopy actual argument"),
            scalar,
        )?;
        let simplified = self.expression(
            arguments
                .get(1)
                .expect("checked homotopy simplified argument"),
            scalar,
        )?;
        let index = self
            .layout
            .solve_layout
            .initial_homotopy_parameter_index
            .ok_or_else(|| {
                LowerError::non_computable("homotopy expression has no checked Solve storage", span)
            })?;
        let lambda = self.load_slot(solve::scalar_slot_p(index), span)?;
        let delta = self.binary(dae::BinaryOperator::Subtract, actual, simplified, span)?;
        let scaled = self.binary(dae::BinaryOperator::Multiply, lambda, delta, span)?;
        self.binary(dae::BinaryOperator::Add, simplified, scaled, span)
    }

    fn linspace(
        &mut self,
        arguments: dae::ExpressionOperands<'dae>,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let start = self.expression(arguments.get(0).expect("checked linspace start"), 0)?;
        let stop = self.expression(arguments.get(1).expect("checked linspace stop"), 0)?;
        let count = u32::try_from(
            ScalarSelector::from_points(self.view, &self.domain_points)
                .integer(arguments.get(2).expect("checked linspace extent"), 0)?,
        )
        .expect("checked linspace extent is in the u32 domain");
        let ordinal = u32::try_from(scalar).expect("linspace scalar is below its u32 extent");
        let fraction = self.constant(f64::from(ordinal) / f64::from(count - 1), span)?;
        let difference = self.binary(dae::BinaryOperator::Subtract, stop, start, span)?;
        let scaled = self.binary(dae::BinaryOperator::Multiply, difference, fraction, span)?;
        self.binary(dae::BinaryOperator::Add, start, scaled, span)
    }

    fn cross(
        &mut self,
        arguments: dae::ExpressionOperands<'dae>,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let (first, second) = [(1, 2), (2, 0), (0, 1)][scalar];
        let lhs = self.expression(arguments.get(0).expect("checked cross lhs"), first)?;
        let rhs = self.expression(arguments.get(1).expect("checked cross rhs"), second)?;
        let positive = self.binary(dae::BinaryOperator::Multiply, lhs, rhs, span)?;
        let lhs = self.expression(arguments.get(0).expect("checked cross lhs"), second)?;
        let rhs = self.expression(arguments.get(1).expect("checked cross rhs"), first)?;
        let negative = self.binary(dae::BinaryOperator::Multiply, lhs, rhs, span)?;
        self.binary(dae::BinaryOperator::Subtract, positive, negative, span)
    }

    fn reduction(
        &mut self,
        expression: dae::ExprId<'dae>,
        reduction: ReductionKind,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let count = scalar_count(self.view, expression);
        if count == 0 {
            return reduction.identity().map_or_else(
                || {
                    Err(LowerError::non_computable(
                        "minimum and maximum require a nonempty array",
                        span,
                    ))
                },
                |identity| self.constant(identity, span),
            );
        }
        let mut values = Vec::with_capacity(count);
        for scalar in 0..count {
            values.push(self.expression(expression, scalar)?);
        }
        self.fold_registers(values, reduction.operator(), span)
    }

    fn fold_registers(
        &mut self,
        values: Vec<solve::Reg>,
        operator: solve::BinaryOp,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let mut values = values.into_iter();
        let mut result = values
            .next()
            .expect("checked reduction supplies an identity or a nonempty operand");
        for value in values {
            let dst = self.register(span)?;
            self.ops.push(solve::LinearOp::Binary {
                dst,
                op: operator,
                lhs: result,
                rhs: value,
            });
            result = dst;
        }
        Ok(result)
    }

    fn size_builtin(
        &mut self,
        arguments: dae::ExpressionOperands<'dae>,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let value = arguments.get(0).expect("checked size value");
        let dimensions = self.node(value).value_type().dimensions();
        let dimension = if let Some(dimension) = arguments.get(1) {
            ScalarSelector::from_points(self.view, &self.domain_points).integer(dimension, 0)?
        } else {
            i64::try_from(scalar + 1)
                .map_err(|_| LowerError::contract("size dimension overflow", span))?
        };
        let zero_based = usize::try_from(dimension - 1)
            .map_err(|_| LowerError::non_computable("size dimension must be positive", span))?;
        let extent = dimensions.get(zero_based).copied().ok_or_else(|| {
            LowerError::non_computable(
                format!(
                    "size dimension {dimension} exceeds rank {}",
                    dimensions.len()
                ),
                span,
            )
        })?;
        self.constant(f64::from(extent), span)
    }

    fn select_array(
        &self,
        elements: dae::ExpressionOperands<'dae>,
        scalar: usize,
    ) -> (dae::ExprId<'dae>, usize) {
        let first = elements.get(0).expect("checked array is nonempty");
        let element_count = scalar_count(self.view, first);
        (
            elements
                .get(scalar / element_count)
                .expect("checked scalar selects an array element"),
            scalar % element_count,
        )
    }

    fn constant(&mut self, value: f64, span: Span) -> Result<solve::Reg, LowerError> {
        let dst = self.register(span)?;
        self.ops.push(solve::LinearOp::Const { dst, value });
        Ok(dst)
    }

    fn register(&mut self, span: Span) -> Result<solve::Reg, LowerError> {
        let register = self.next_register;
        self.next_register = self
            .next_register
            .checked_add(1)
            .ok_or_else(|| LowerError::contract("Solve register index overflow", span))?;
        Ok(register)
    }

    fn node(&self, expression: dae::ExprId<'dae>) -> dae::ExpressionView<'dae> {
        self.view
            .expression(expression)
            .expect("branded expression resolves in its DAE")
    }

    fn expect_scalar(
        &self,
        node: dae::ExpressionView<'dae>,
        scalar: usize,
    ) -> Result<(), LowerError> {
        let count = node
            .value_type()
            .scalar_count()
            .expect("checked expression scalar capacity");
        if scalar < count {
            return Ok(());
        }
        Err(LowerError::contract(
            format!("scalar projection {scalar} exceeds expression size {count}"),
            node.provenance().span(),
        ))
    }
}

#[derive(Clone)]
pub(super) struct ScalarSelector<'dae> {
    view: dae::DaeView<'dae>,
    domain_points: Vec<(dae::DomainId<'dae>, Vec<i64>)>,
}

impl<'dae> ScalarSelector<'dae> {
    pub(super) const fn view(&self) -> dae::DaeView<'dae> {
        self.view
    }

    pub(super) fn new(
        view: dae::DaeView<'dae>,
        domain_point: Option<(dae::DomainId<'dae>, &[i64])>,
    ) -> Self {
        Self {
            view,
            domain_points: domain_point
                .map(|(domain, values)| vec![(domain, values.to_vec())])
                .unwrap_or_default(),
        }
    }

    fn from_points(
        view: dae::DaeView<'dae>,
        domain_points: &[(dae::DomainId<'dae>, Vec<i64>)],
    ) -> Self {
        Self {
            view,
            domain_points: domain_points.to_vec(),
        }
    }

    pub(super) fn coordinate(
        &self,
        expression: dae::ExprId<'dae>,
        scalar: usize,
    ) -> Result<Option<(dae::CoordinateView<'dae>, usize)>, LowerError> {
        let node = self.node(expression);
        match node.operation() {
            dae::ExpressionOperation::Coordinate(coordinate) => Ok(Some((coordinate, scalar))),
            dae::ExpressionOperation::Unary {
                operator: dae::UnaryOperator::Plus,
                operand,
            } => self.coordinate(operand, scalar),
            dae::ExpressionOperation::Array(elements) => {
                let first = elements.get(0).expect("checked array is nonempty");
                let count = scalar_count(self.view, first);
                self.coordinate(
                    elements
                        .get(scalar / count)
                        .expect("checked scalar selects an array element"),
                    scalar % count,
                )
            }
            dae::ExpressionOperation::Comprehension { domain, body } => {
                let body_count = scalar_count(self.view, body);
                let point = scalar / body_count;
                let values = self
                    .view
                    .domain(domain)
                    .expect("checked comprehension domain resolves")
                    .structured()
                    .index_tuple_at(point)
                    .expect("checked domain remains valid")
                    .expect("checked scalar selects a domain point");
                let mut nested = self.clone();
                nested.domain_points.push((domain, values));
                nested.coordinate(body, scalar % body_count)
            }
            dae::ExpressionOperation::Index { base, subscripts } => {
                let selected = self.indexed_base_scalar(
                    base,
                    subscripts,
                    node.value_type().dimensions(),
                    scalar,
                )?;
                self.coordinate(base, selected)
            }
            _ => Ok(None),
        }
    }

    pub(super) fn select_array_element(
        &self,
        mut expression: dae::ExprId<'dae>,
        mut scalar: usize,
    ) -> Result<(dae::ExprId<'dae>, usize), LowerError> {
        loop {
            let node = self.node(expression);
            let dae::ExpressionOperation::Array(elements) = node.operation() else {
                return Ok((expression, scalar));
            };
            let Some(first) = elements.get(0) else {
                return Err(LowerError::contract(
                    "structured row cannot select an empty aggregate body",
                    node.provenance().span(),
                ));
            };
            let element_count = scalar_count(self.view, first);
            if element_count == 0 {
                return Err(LowerError::contract(
                    "structured row cannot select an empty aggregate element",
                    node.provenance().span(),
                ));
            }
            expression = elements.get(scalar / element_count).ok_or_else(|| {
                LowerError::contract(
                    "structured row selects outside its checked aggregate body",
                    node.provenance().span(),
                )
            })?;
            scalar %= element_count;
        }
    }

    fn indexed_base_scalar(
        &self,
        base: dae::ExprId<'dae>,
        subscripts: dae::SubscriptsView<'dae>,
        result_dimensions: &[u32],
        result_scalar: usize,
    ) -> Result<usize, LowerError> {
        let base_dimensions = self.node(base).value_type().dimensions();
        let result_coordinates = row_major_coordinates(result_dimensions, result_scalar)
            .ok_or_else(|| {
                LowerError::contract(
                    "indexed result scalar is outside its checked shape",
                    self.node(base).provenance().span(),
                )
            })?;
        let mut result_axis = 0usize;
        let mut base_coordinates = Vec::with_capacity(base_dimensions.len());
        for (axis, &extent) in base_dimensions.iter().enumerate() {
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
                    let (slice_scalar, end) = self.slice_scalar(
                        expression,
                        &result_coordinates,
                        result_axis,
                        provenance.span(),
                    )?;
                    let index = self.integer(expression, slice_scalar)?;
                    base_coordinates.push(checked_index(index, extent, provenance.span())?);
                    result_axis = end;
                }
            }
        }
        flatten_coordinates(base_dimensions, &base_coordinates).ok_or_else(|| {
            LowerError::contract(
                "indexed scalar projection is outside its base shape",
                self.node(base).provenance().span(),
            )
        })
    }

    fn array_update_value_scalar(
        &self,
        base: dae::ExprId<'dae>,
        subscripts: dae::SubscriptsView<'dae>,
        value_dimensions: &[u32],
        base_scalar: usize,
    ) -> Result<Option<usize>, LowerError> {
        let base_node = self.node(base);
        let base_dimensions = base_node.value_type().dimensions();
        let base_coordinates =
            row_major_coordinates(base_dimensions, base_scalar).ok_or_else(|| {
                LowerError::contract(
                    "array update scalar is outside its checked base shape",
                    base_node.provenance().span(),
                )
            })?;
        let mut value_coordinates = Vec::with_capacity(value_dimensions.len());
        for (axis, (&extent, &coordinate)) in
            base_dimensions.iter().zip(&base_coordinates).enumerate()
        {
            let Some(axis_coordinates) =
                self.array_update_axis_coordinates(subscripts.get(axis), extent, coordinate)?
            else {
                return Ok(None);
            };
            value_coordinates.extend(axis_coordinates);
        }
        flatten_coordinates(value_dimensions, &value_coordinates)
            .map(Some)
            .ok_or_else(|| {
                LowerError::contract(
                    "array update selection does not match its checked value shape",
                    base_node.provenance().span(),
                )
            })
    }

    fn array_update_axis_coordinates(
        &self,
        subscript: Option<dae::SubscriptView<'dae>>,
        extent: u32,
        coordinate: u32,
    ) -> Result<Option<Vec<u32>>, LowerError> {
        match subscript {
            Some(dae::SubscriptView::Index {
                expression,
                provenance,
            }) => {
                let index = checked_index(self.integer(expression, 0)?, extent, provenance.span())?;
                Ok((coordinate == index).then(Vec::new))
            }
            Some(dae::SubscriptView::Whole { .. }) | None => Ok(Some(vec![coordinate])),
            Some(dae::SubscriptView::Slice {
                expression,
                provenance,
            }) => {
                let slice_scalar = self.array_update_slice_scalar(
                    expression,
                    extent,
                    coordinate,
                    provenance.span(),
                )?;
                let Some(slice_scalar) = slice_scalar else {
                    return Ok(None);
                };
                row_major_coordinates(
                    self.node(expression).value_type().dimensions(),
                    slice_scalar,
                )
                .map(Some)
                .ok_or_else(|| {
                    LowerError::contract(
                        "array update slice scalar is outside its checked shape",
                        provenance.span(),
                    )
                })
            }
        }
    }

    fn array_update_slice_scalar(
        &self,
        expression: dae::ExprId<'dae>,
        extent: u32,
        coordinate: u32,
        span: Span,
    ) -> Result<Option<usize>, LowerError> {
        for slice_scalar in 0..scalar_count(self.view, expression) {
            let index = checked_index(self.integer(expression, slice_scalar)?, extent, span)?;
            if index == coordinate {
                return Ok(Some(slice_scalar));
            }
        }
        Ok(None)
    }

    fn slice_scalar(
        &self,
        expression: dae::ExprId<'dae>,
        result_coordinates: &[u32],
        result_axis: usize,
        span: Span,
    ) -> Result<(usize, usize), LowerError> {
        let slice_dimensions = self.node(expression).value_type().dimensions();
        let end = result_axis
            .checked_add(slice_dimensions.len())
            .ok_or_else(|| LowerError::contract("slice rank overflow", span))?;
        let slice_coordinates = result_coordinates
            .get(result_axis..end)
            .ok_or_else(|| LowerError::contract("slice coordinates exceed result rank", span))?;
        let scalar = flatten_coordinates(slice_dimensions, slice_coordinates).ok_or_else(|| {
            LowerError::contract("slice scalar projection is outside its shape", span)
        })?;
        Ok((scalar, end))
    }

    pub(super) fn constant_real(
        &self,
        expression: dae::ExprId<'dae>,
        scalar: usize,
    ) -> Result<f64, LowerError> {
        self.constant_real_inner(expression, scalar, &mut Vec::new())
    }

    fn constant_real_inner(
        &self,
        expression: dae::ExprId<'dae>,
        scalar: usize,
        active: &mut Vec<dae::ExprId<'dae>>,
    ) -> Result<f64, LowerError> {
        let node = self.node(expression);
        let span = node.provenance().span();
        check_constant_recursion(active, expression, span)?;
        active.push(expression);
        let result = match node.operation() {
            dae::ExpressionOperation::Literal(dae::DaeLiteral::Real(value)) => Ok(*value),
            dae::ExpressionOperation::Literal(
                dae::DaeLiteral::Integer(value) | dae::DaeLiteral::Enumeration(value),
            ) => Ok(*value as f64),
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Parameter(parameter)) => {
                let variable = self
                    .view
                    .variable(parameter.into())
                    .expect("checked parameter coordinate resolves");
                let binding = variable.binding().ok_or_else(|| {
                    LowerError::non_computable(
                        "affine derivative coefficient parameter has no static binding",
                        span,
                    )
                })?;
                self.constant_real_inner(binding, scalar, active)
            }
            dae::ExpressionOperation::Unary { operator, operand } => {
                let value = self.constant_real_inner(operand, scalar, active)?;
                match operator {
                    dae::UnaryOperator::Plus => Ok(value),
                    dae::UnaryOperator::Negate => Ok(-value),
                    dae::UnaryOperator::Not => Err(LowerError::non_computable(
                        "affine derivative coefficient is not numeric",
                        span,
                    )),
                }
            }
            dae::ExpressionOperation::Binary { operator, lhs, rhs } => {
                let lhs_scalar = scalar_operand(self.view, lhs, scalar);
                let rhs_scalar = scalar_operand(self.view, rhs, scalar);
                let lhs = self.constant_real_inner(lhs, lhs_scalar, active)?;
                let rhs = self.constant_real_inner(rhs, rhs_scalar, active)?;
                constant_binary(operator, lhs, rhs, span)
            }
            _ => Err(LowerError::non_computable(
                "affine derivative coefficient is not compile-time numeric",
                span,
            )),
        };
        active.pop();
        result.and_then(|value| finite_constant(value, span))
    }

    fn integer(&self, expression: dae::ExprId<'dae>, scalar: usize) -> Result<i64, LowerError> {
        let node = self.node(expression);
        let span = node.provenance().span();
        match node.operation() {
            dae::ExpressionOperation::Literal(
                dae::DaeLiteral::Integer(value) | dae::DaeLiteral::Enumeration(value),
            ) => Ok(*value),
            dae::ExpressionOperation::Range { start, step, .. } => {
                let offset = i64::try_from(scalar)
                    .map_err(|_| LowerError::contract("integer scalar overflow", span))?;
                start
                    .checked_add(
                        step.checked_mul(offset)
                            .ok_or_else(|| LowerError::contract("integer overflow", span))?,
                    )
                    .ok_or_else(|| LowerError::contract("integer overflow", span))
            }
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Binder(binder)) => {
                let Some((_, values)) = self
                    .domain_points
                    .iter()
                    .rev()
                    .find(|(domain, _)| *domain == binder.domain())
                else {
                    return Err(LowerError::non_computable(
                        "binder-valued subscript has no active domain",
                        span,
                    ));
                };
                values
                    .get(binder.ordinal() as usize)
                    .copied()
                    .ok_or_else(|| LowerError::contract("binder ordinal is out of range", span))
            }
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Parameter(parameter)) => {
                let variable = self
                    .view
                    .variable(parameter.into())
                    .expect("checked parameter coordinate resolves");
                let binding = variable.binding().ok_or_else(|| {
                    LowerError::non_computable(
                        "runtime parameter cannot select a static array subscript",
                        span,
                    )
                })?;
                self.integer(binding, scalar)
            }
            dae::ExpressionOperation::Unary { operator, operand } => {
                let value = self.integer(operand, scalar)?;
                match operator {
                    dae::UnaryOperator::Plus => Ok(value),
                    dae::UnaryOperator::Negate => value
                        .checked_neg()
                        .ok_or_else(|| LowerError::contract("integer overflow", span)),
                    dae::UnaryOperator::Not => Err(LowerError::non_computable(
                        "Boolean negation is not an integer subscript",
                        span,
                    )),
                }
            }
            dae::ExpressionOperation::Binary { operator, lhs, rhs } => integer_binary(
                operator,
                self.integer(lhs, scalar)?,
                self.integer(rhs, scalar)?,
                span,
            ),
            dae::ExpressionOperation::Array(elements) => {
                let first = elements.get(0).expect("checked array is nonempty");
                let count = scalar_count(self.view, first);
                self.integer(
                    elements
                        .get(scalar / count)
                        .expect("checked integer scalar selects an array element"),
                    scalar % count,
                )
            }
            dae::ExpressionOperation::Index { base, subscripts } => {
                let selected = self.indexed_base_scalar(
                    base,
                    subscripts,
                    node.value_type().dimensions(),
                    scalar,
                )?;
                self.integer(base, selected)
            }
            _ => Err(LowerError::non_computable(
                "array subscript is not compile-time computable",
                span,
            )),
        }
    }

    fn node(&self, expression: dae::ExprId<'dae>) -> dae::ExpressionView<'dae> {
        self.view
            .expression(expression)
            .expect("branded expression resolves in its DAE")
    }
}

fn scalar_operand<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    scalar: usize,
) -> usize {
    if scalar_count(view, expression) == 1 {
        0
    } else {
        scalar
    }
}

fn check_constant_recursion<'dae>(
    active: &[dae::ExprId<'dae>],
    expression: dae::ExprId<'dae>,
    span: Span,
) -> Result<(), LowerError> {
    if active.contains(&expression) {
        return Err(LowerError::non_computable(
            "affine derivative coefficient has a cyclic parameter definition",
            span,
        ));
    }
    if active.len() >= 256 {
        return Err(LowerError::non_computable(
            "affine derivative coefficient exceeded the checked recursion limit",
            span,
        ));
    }
    Ok(())
}

fn constant_binary(
    operator: dae::BinaryOperator,
    lhs: f64,
    rhs: f64,
    span: Span,
) -> Result<f64, LowerError> {
    let value = match operator {
        dae::BinaryOperator::Add | dae::BinaryOperator::ElementwiseAdd => lhs + rhs,
        dae::BinaryOperator::Subtract | dae::BinaryOperator::ElementwiseSubtract => lhs - rhs,
        dae::BinaryOperator::Multiply | dae::BinaryOperator::ElementwiseMultiply => lhs * rhs,
        dae::BinaryOperator::Divide | dae::BinaryOperator::ElementwiseDivide if rhs != 0.0 => {
            lhs / rhs
        }
        dae::BinaryOperator::Power | dae::BinaryOperator::ElementwisePower => lhs.powf(rhs),
        dae::BinaryOperator::Divide | dae::BinaryOperator::ElementwiseDivide => {
            return Err(LowerError::non_computable(
                "affine derivative coefficient divides by zero",
                span,
            ));
        }
        _ => {
            return Err(LowerError::non_computable(
                "affine derivative coefficient is not numeric",
                span,
            ));
        }
    };
    finite_constant(value, span)
}

fn finite_constant(value: f64, span: Span) -> Result<f64, LowerError> {
    if value.is_finite() {
        Ok(value)
    } else {
        Err(LowerError::non_computable(
            "affine derivative coefficient is not finite",
            span,
        ))
    }
}
