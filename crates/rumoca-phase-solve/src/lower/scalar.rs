mod arrays;
mod builtins;
mod conditions;
mod constants;
mod coordinates;
mod functions;
mod operators;
mod selector;

use super::*;

pub(super) struct ScaledDerivativeProgram<'dae> {
    pub(super) numerator: dae::ExprId<'dae>,
    pub(super) numerator_scalar: usize,
    pub(super) coefficient: dae::ExprId<'dae>,
    pub(super) coefficient_scalar: usize,
    pub(super) negate: bool,
    pub(super) span: Span,
}

/// Parameter bindings a program must recompute instead of loading from storage.
///
/// The parameter set evaluates a binding once, before the initialization system
/// has solved anything, so a binding that reads an MLS §8.6 initialization
/// unknown only ever held a number derived from that unknown's `start` guess.
/// A row lowered with these substitutions reads the binding itself, so the value
/// it sees is the one the current iterate implies rather than the stale seed.
#[derive(Default)]
pub(super) struct ParameterBindingSubstitutions<'dae> {
    bindings: HashMap<u32, dae::ExprId<'dae>>,
}

impl<'dae> ParameterBindingSubstitutions<'dae> {
    pub(super) const fn new(bindings: HashMap<u32, dae::ExprId<'dae>>) -> Self {
        Self { bindings }
    }

    /// The binding to recompute for a parameter, when this parameter is one the
    /// initialization system re-derives rather than reads.
    pub(super) fn binding(&self, parameter: u32) -> Option<dae::ExprId<'dae>> {
        self.bindings.get(&parameter).copied()
    }
}

pub(super) struct ScalarCompiler<'layout, 'dae> {
    view: dae::DaeView<'dae>,
    layout: &'layout LoweredLayout<'dae>,
    domain_points: Vec<(dae::DomainId<'dae>, Vec<i64>)>,
    function_arguments: Vec<(dae::FunctionId<'dae>, Vec<dae::ExprId<'dae>>)>,
    function_fold_values: Vec<(dae::FunctionFoldId<'dae>, Vec<Vec<solve::Reg>>)>,
    active_clock: Option<dae::ClockId<'dae>>,
    derivative_definitions: Option<&'layout DerivativeRowIndex<'dae>>,
    active_derivatives: Vec<(u32, usize)>,
    parameter_substitutions: Option<&'layout ParameterBindingSubstitutions<'dae>>,
    active_parameters: Vec<u32>,
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
            derivative_definitions: None,
            active_derivatives: Vec::new(),
            parameter_substitutions: None,
            active_parameters: Vec::new(),
            ops: Vec::new(),
            next_register: 0,
        }
    }

    /// Let this program resolve a derivative coordinate through the continuous
    /// row the structural proof matched to that derivative.
    pub(super) const fn with_derivative_definitions(
        mut self,
        definitions: &'layout DerivativeRowIndex<'dae>,
    ) -> Self {
        self.derivative_definitions = Some(definitions);
        self
    }

    /// Let this program recompute a calculated parameter from its binding
    /// instead of loading the number the parameter set stored for it.
    pub(super) const fn with_parameter_substitutions(
        mut self,
        substitutions: &'layout ParameterBindingSubstitutions<'dae>,
    ) -> Self {
        self.parameter_substitutions = Some(substitutions);
        self
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

    /// Compile the signed sum `Σ ±termᵢ` into one program.
    ///
    /// An empty sum is the value zero: the terms a structural proof hands over
    /// are already reduced, so a displacement that cancelled leaves nothing to
    /// add rather than a missing row.
    pub(super) fn signed_sum_program(
        mut self,
        terms: &[(dae::ExprId<'dae>, bool)],
        span: Span,
    ) -> Result<Vec<solve::LinearOp>, LowerError> {
        let mut total: Option<solve::Reg> = None;
        for (expression, negated) in terms.iter().copied() {
            let value = self.expression(expression, 0)?;
            let operator = if negated {
                dae::BinaryOperator::Subtract
            } else {
                dae::BinaryOperator::Add
            };
            total = Some(match (total, negated) {
                (None, false) => value,
                (None, true) => self.unary(dae::UnaryOperator::Negate, value, span)?,
                (Some(total), _) => self.binary(operator, total, value, span)?,
            });
        }
        let output = match total {
            Some(total) => total,
            None => self.constant(0.0, span)?,
        };
        self.ops.push(solve::LinearOp::StoreOutput { src: output });
        Ok(self.ops)
    }

    /// Compile `slot - Σ ±termᵢ` into one residual program.
    ///
    /// The coordinate is read from its storage rather than through an
    /// expression, because the equation being restated is the declaration's own
    /// `v = v.start` — a claim about the coordinate, which the DAE need not
    /// contain an expression for.
    pub(super) fn slot_residual_program(
        mut self,
        slot: solve::ScalarSlot,
        terms: &[(dae::ExprId<'dae>, bool)],
        span: Span,
    ) -> Result<Vec<solve::LinearOp>, LowerError> {
        let coordinate = self.register(span)?;
        match slot {
            solve::ScalarSlot::Y { index, .. } => self.ops.push(solve::LinearOp::LoadY {
                dst: coordinate,
                index,
            }),
            solve::ScalarSlot::P { index, .. } => self.ops.push(solve::LinearOp::LoadP {
                dst: coordinate,
                index,
            }),
            solve::ScalarSlot::Time | solve::ScalarSlot::Constant(_) => {
                return Err(LowerError::contract(
                    "a stated initial value names a coordinate with no runtime storage",
                    span,
                ));
            }
        }
        let mut residual = coordinate;
        for (expression, negated) in terms.iter().copied() {
            let value = self.expression(expression, 0)?;
            let operator = if negated {
                dae::BinaryOperator::Add
            } else {
                dae::BinaryOperator::Subtract
            };
            residual = self.binary(operator, residual, value, span)?;
        }
        self.ops
            .push(solve::LinearOp::StoreOutput { src: residual });
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
            dae::ExpressionOperation::Range(range) => self.range(
                range.start().value(),
                range.effective_step(),
                scalar,
                node.provenance().span(),
            ),
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
            dae::ExpressionOperation::StringConversion { .. } => Err(LowerError::contract(
                "String conversion escaped its checked event-message owner",
                node.provenance().span(),
            )),
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
