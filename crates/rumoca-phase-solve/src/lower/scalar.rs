mod arrays;
mod builtins;
mod call_scoped_actions;
mod conditions;
mod constants;
mod coordinates;
mod functions;
mod operators;
mod selector;

use std::collections::HashSet;

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

#[derive(Clone, PartialEq, Eq, Hash)]
enum ScalarContextFrame<'dae> {
    Activation {
        parent: u64,
        condition: dae::ExprId<'dae>,
        expected: bool,
    },
    Function {
        parent: u64,
        function: dae::FunctionId<'dae>,
        arguments: Vec<dae::ExprId<'dae>>,
    },
    Domain {
        parent: u64,
        domain: dae::DomainId<'dae>,
        values: Vec<i64>,
    },
    Fold {
        parent: u64,
        fold: dae::FunctionFoldId<'dae>,
        values: Vec<Vec<solve::Reg>>,
    },
    Parameter {
        parent: u64,
        parameter: u32,
    },
    Derivative {
        parent: u64,
        state: u32,
        scalar: usize,
    },
}

#[derive(Clone, Copy)]
struct ActivationGuard<'dae> {
    condition: dae::ExprId<'dae>,
    expected: bool,
}

#[derive(Clone, PartialEq, Eq, Hash)]
struct FunctionFoldCacheDependency<'dae> {
    fold: dae::FunctionFoldId<'dae>,
    values: Vec<Vec<solve::Reg>>,
    domain_point: Vec<i64>,
}

#[derive(Clone, PartialEq, Eq, Hash)]
struct FunctionFoldOutputCacheKey<'dae> {
    function_context: u64,
    fold: dae::FunctionFoldId<'dae>,
    carried: u32,
    scalar: usize,
    dependencies: Vec<FunctionFoldCacheDependency<'dae>>,
}

#[derive(Clone, PartialEq, Eq, Hash)]
struct ActiveCallAssertion<'dae> {
    function: dae::FunctionId<'dae>,
    arguments: Vec<dae::ExprId<'dae>>,
}

#[derive(Clone)]
struct FunctionArgumentsFrame<'dae> {
    function: dae::FunctionId<'dae>,
    arguments: Vec<dae::ExprId<'dae>>,
    activation_base: usize,
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
    function_arguments: Vec<FunctionArgumentsFrame<'dae>>,
    function_fold_values: Vec<(dae::FunctionFoldId<'dae>, Vec<Vec<solve::Reg>>)>,
    activation_path: Vec<ActivationGuard<'dae>>,
    active_clock: Option<dae::ClockId<'dae>>,
    sampled_source: bool,
    derivative_definitions: Option<&'layout DerivativeRowIndex<'dae>>,
    active_derivatives: Vec<(u32, usize)>,
    parameter_substitutions: Option<&'layout ParameterBindingSubstitutions<'dae>>,
    active_parameters: Vec<u32>,
    ops: Vec<solve::LinearOp>,
    next_register: solve::Reg,
    integer_registers: Vec<Option<i64>>,
    expression_cache: HashMap<(u64, dae::ExprId<'dae>, usize), solve::Reg>,
    function_fold_output_cache: HashMap<FunctionFoldOutputCacheKey<'dae>, solve::Reg>,
    active_call_assertions: HashSet<ActiveCallAssertion<'dae>>,
    call_action_compilation: bool,
    context_ids: HashMap<ScalarContextFrame<'dae>, u64>,
    context_frames: HashMap<u64, ScalarContextFrame<'dae>>,
    context_stack: Vec<u64>,
    context_id: u64,
    next_context_id: u64,
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
            activation_path: Vec::new(),
            active_clock: None,
            sampled_source: false,
            derivative_definitions: None,
            active_derivatives: Vec::new(),
            parameter_substitutions: None,
            active_parameters: Vec::new(),
            ops: Vec::new(),
            next_register: 0,
            integer_registers: Vec::new(),
            expression_cache: HashMap::new(),
            function_fold_output_cache: HashMap::new(),
            active_call_assertions: HashSet::new(),
            call_action_compilation: false,
            context_ids: HashMap::new(),
            context_frames: HashMap::new(),
            context_stack: Vec::new(),
            context_id: 0,
            next_context_id: 1,
        }
    }

    fn enter_context(&mut self, frame: ScalarContextFrame<'dae>) {
        let id = match self.context_ids.get(&frame).copied() {
            Some(id) => id,
            None => {
                let id = self.next_context_id;
                self.next_context_id += 1;
                self.context_ids.insert(frame.clone(), id);
                self.context_frames.insert(id, frame);
                id
            }
        };
        self.context_stack.push(self.context_id);
        self.context_id = id;
    }

    fn leave_context(&mut self) {
        self.context_id = self
            .context_stack
            .pop()
            .expect("semantic scalar context has a parent");
    }

    fn push_activation(&mut self, condition: dae::ExprId<'dae>, expected: bool) {
        self.enter_context(ScalarContextFrame::Activation {
            parent: self.context_id,
            condition,
            expected,
        });
        self.activation_path.push(ActivationGuard {
            condition,
            expected,
        });
    }

    fn pop_activation(&mut self) {
        self.activation_path
            .pop()
            .expect("activation path has a guard");
        self.leave_context();
    }

    fn suspend_context(&mut self) -> u64 {
        let suspended = self.context_id;
        self.context_id = self.context_stack.pop().unwrap_or(0);
        suspended
    }

    fn resume_context(&mut self, suspended: u64) {
        self.context_stack.push(self.context_id);
        self.context_id = suspended;
    }

    fn function_fold_cache_key(
        &self,
        fold: dae::FunctionFoldId<'dae>,
        carried: u32,
        scalar: usize,
    ) -> FunctionFoldOutputCacheKey<'dae> {
        let dependencies = self
            .function_fold_values
            .iter()
            .filter(|(active, _)| self.function_fold_reads_active_fold(fold, *active))
            .map(|(active, values)| {
                let domain = self
                    .view
                    .function_fold(*active)
                    .expect("active function fold resolves")
                    .domain();
                let domain_point = self
                    .domain_points
                    .iter()
                    .rev()
                    .find_map(|(candidate, point)| (*candidate == domain).then(|| point.clone()))
                    .expect("active function fold owns a domain point");
                FunctionFoldCacheDependency {
                    fold: *active,
                    values: values.clone(),
                    domain_point,
                }
            })
            .collect();
        FunctionFoldOutputCacheKey {
            function_context: self.owning_function_context(fold.function()),
            fold,
            carried,
            scalar,
            dependencies,
        }
    }

    fn function_fold_reads_active_fold(
        &self,
        fold: dae::FunctionFoldId<'dae>,
        active: dae::FunctionFoldId<'dae>,
    ) -> bool {
        let fold = self
            .view
            .function_fold(fold)
            .expect("checked function fold resolves");
        fold.initial_values()
            .rhs_iter()
            .chain(fold.update_values().rhs_iter())
            .any(|root| {
                let mut reads_active = false;
                dae::for_each_expression(self.view, root, |_, expression| {
                    reads_active |= matches!(
                        expression.operation(),
                        dae::ExpressionOperation::FunctionFoldParameter {
                            fold: candidate,
                            ..
                        } if candidate == active
                    );
                });
                reads_active
            })
    }

    fn owning_function_context(&self, function: dae::FunctionId<'dae>) -> u64 {
        let mut context = self.context_id;
        while context != 0 {
            let frame = self
                .context_frames
                .get(&context)
                .expect("non-root scalar context has a frame");
            match frame {
                ScalarContextFrame::Function {
                    function: candidate,
                    ..
                } if *candidate == function => return context,
                ScalarContextFrame::Activation { parent, .. }
                | ScalarContextFrame::Function { parent, .. }
                | ScalarContextFrame::Domain { parent, .. }
                | ScalarContextFrame::Fold { parent, .. }
                | ScalarContextFrame::Parameter { parent, .. }
                | ScalarContextFrame::Derivative { parent, .. } => context = *parent,
            }
        }
        0
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

    /// Compile the source of MLS §16.5.1 `sample(u)` against event-entry
    /// snapshot lanes. The surrounding row still owns the exact clock; this
    /// flag changes only coordinate reads inside `u` to their left limits.
    pub(super) fn sampled_program(
        mut self,
        clock: dae::ClockId<'dae>,
        expression: dae::ExprId<'dae>,
        scalar: usize,
    ) -> Result<Vec<solve::LinearOp>, LowerError> {
        self.active_clock = Some(clock);
        self.sampled_source = true;
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
        let key = (self.context_id, expression, scalar);
        if let Some(register) = self.cached_dominating_expression(expression, scalar) {
            return Ok(register);
        }
        let node = self.node(expression);
        self.expect_scalar(node, scalar)?;
        let result = match node.operation() {
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
            } => self.array_update(base, value, subscripts, scalar),
            dae::ExpressionOperation::Builtin { builtin, arguments } => self.builtin(
                builtin,
                arguments,
                node.value_type().dimensions(),
                scalar,
                node.provenance().span(),
            ),
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
            dae::ExpressionOperation::ClockTransfer {
                source,
                source_clock,
                target_clock,
                ..
            } => self.clock_transfer(
                source,
                source_clock,
                target_clock,
                scalar,
                node.provenance().span(),
            ),
        }?;
        self.expression_cache.insert(key, result);
        Ok(result)
    }

    /// Reuse only a value computed on the current activation path.
    ///
    /// Every DAE expression is pure. An ancestor activation dominates its
    /// descendants, so its value (and any call assertion scheduled while
    /// computing it) is already available there. A function boundary stops
    /// the search because the same body expression can have different actual
    /// arguments in another call, and siblings never dominate one another.
    fn cached_dominating_expression(
        &self,
        expression: dae::ExprId<'dae>,
        scalar: usize,
    ) -> Option<solve::Reg> {
        let mut context = self.context_id;
        loop {
            if let Some(register) = self
                .expression_cache
                .get(&(context, expression, scalar))
                .copied()
            {
                return Some(register);
            }
            if context == 0 {
                return None;
            }
            match self
                .context_frames
                .get(&context)
                .expect("non-root scalar context has a frame")
            {
                ScalarContextFrame::Function { .. } => return None,
                ScalarContextFrame::Activation { parent, .. }
                | ScalarContextFrame::Domain { parent, .. }
                | ScalarContextFrame::Fold { parent, .. }
                | ScalarContextFrame::Parameter { parent, .. }
                | ScalarContextFrame::Derivative { parent, .. } => context = *parent,
            }
        }
    }

    fn current_function_frame_matches(&self, call: &ActiveCallAssertion<'dae>) -> bool {
        self.function_arguments.last().is_some_and(|frame| {
            frame.function == call.function && frame.arguments == call.arguments
        })
    }

    fn array_update(
        &mut self,
        base: dae::ExprId<'dae>,
        value: dae::ExprId<'dae>,
        subscripts: dae::SubscriptsView<'dae>,
        scalar: usize,
    ) -> Result<solve::Reg, LowerError> {
        let selected = ScalarSelector::from_points(self.view, &self.domain_points)
            .array_update_value_scalar(
                base,
                subscripts,
                self.node(value).value_type().dimensions(),
                scalar,
            );
        let selected = match selected {
            Ok(selected) => selected,
            Err(LowerError::NonComputable { reason, .. })
                if reason == "array subscript is not compile-time computable" =>
            {
                return self.dynamic_scalar_array_update(base, value, subscripts, scalar);
            }
            Err(error) => return Err(error),
        };
        match selected {
            Some(value_scalar) => self.expression(value, value_scalar),
            None => self.expression(base, scalar),
        }
    }

    fn clock_transfer(
        &mut self,
        source: dae::ExprId<'dae>,
        source_clock: dae::ClockId<'dae>,
        target_clock: dae::ClockId<'dae>,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        if self.active_clock != Some(target_clock) {
            return Err(LowerError::contract(
                "clock transfer escaped its target clock schedule",
                span,
            ));
        }
        let target = self.active_clock.replace(source_clock);
        let value = self.expression(source, scalar);
        self.active_clock = target;
        value
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
        let value = match self.integer_register(condition) {
            Some(0) => self.integer_register(when_false),
            Some(_) => self.integer_register(when_true),
            None if self.integer_register(when_true) == self.integer_register(when_false) => {
                self.integer_register(when_true)
            }
            None => None,
        };
        self.set_integer_register(dst, value);
        Ok(dst)
    }

    fn constant(&mut self, value: f64, span: Span) -> Result<solve::Reg, LowerError> {
        let dst = self.register(span)?;
        self.ops.push(solve::LinearOp::Const { dst, value });
        self.set_integer_register(dst, exact_i64(value));
        Ok(dst)
    }

    fn integer_register(&self, register: solve::Reg) -> Option<i64> {
        self.integer_registers
            .get(register as usize)
            .copied()
            .flatten()
    }

    fn set_integer_register(&mut self, register: solve::Reg, value: Option<i64>) {
        let slot = self
            .integer_registers
            .get_mut(register as usize)
            .expect("registered Solve scalar owns constant metadata");
        *slot = value;
    }

    fn register(&mut self, span: Span) -> Result<solve::Reg, LowerError> {
        let register = self.next_register;
        self.next_register = self
            .next_register
            .checked_add(1)
            .ok_or_else(|| LowerError::contract("Solve register index overflow", span))?;
        self.integer_registers.push(None);
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

fn exact_i64(value: f64) -> Option<i64> {
    if value.is_finite()
        && value.fract() == 0.0
        && value >= i64::MIN as f64
        && value <= i64::MAX as f64
    {
        Some(value as i64)
    } else {
        None
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
