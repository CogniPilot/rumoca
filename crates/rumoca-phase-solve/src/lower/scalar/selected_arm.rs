//! Selected-arm evaluation of a model conditional (SPEC_0040 DAE-C22).
//!
//! An eager selection computes every arm and discards all but one, which is
//! sound only when every arm is total: builtin arithmetic, relations, and pure
//! builtins over coordinates and literals with literal subscripts, or with
//! integer subscripts of family binders proven inside the indexed dimension,
//! evaluate to a possibly non-finite IEEE 754 value and never fail or act, and
//! the selection discards the unselected value and its tangent. A conditional
//! with any other arm (a call, which can assert, fail, or act, or a subscript
//! not proven in bounds) is lowered as a checked function-conditional program
//! that runs only the selected arm.

use super::*;

impl<'layout, 'dae> ScalarCompiler<'layout, 'dae> {
    /// Whether a conditional can be lowered to an eager selection: every
    /// operand after the first condition is total, where a node the first
    /// condition already computes counts as evaluated, since that condition
    /// runs whichever arm is selected.
    pub(super) fn conditional_is_total(&self, operands: dae::ExpressionOperands<'dae>) -> bool {
        let mut visited = HashSet::new();
        if let Some(first) = operands.get(0) {
            self.mark_evaluated(first, &mut visited);
        }
        (1..operands.len()).all(|index| {
            operands
                .get(index)
                .is_some_and(|operand| self.is_total(operand, &mut visited))
        })
    }

    fn mark_evaluated(
        &self,
        expression: dae::ExprId<'dae>,
        visited: &mut HashSet<dae::ExprId<'dae>>,
    ) {
        if !visited.insert(expression) {
            return;
        }
        match self.node(expression).operation() {
            dae::ExpressionOperation::Unary { operand, .. } => {
                self.mark_evaluated(operand, visited);
            }
            dae::ExpressionOperation::Binary { lhs, rhs, .. } => {
                self.mark_evaluated(lhs, visited);
                self.mark_evaluated(rhs, visited);
            }
            dae::ExpressionOperation::Field { base, .. } => self.mark_evaluated(base, visited),
            dae::ExpressionOperation::Array(elements)
            | dae::ExpressionOperation::Record(elements)
            | dae::ExpressionOperation::Builtin {
                arguments: elements,
                ..
            }
            | dae::ExpressionOperation::Call {
                arguments: elements,
                ..
            } => {
                for element in (0..elements.len()).filter_map(|index| elements.get(index)) {
                    self.mark_evaluated(element, visited);
                }
            }
            _ => {}
        }
    }

    fn is_total(
        &self,
        expression: dae::ExprId<'dae>,
        visited: &mut HashSet<dae::ExprId<'dae>>,
    ) -> bool {
        if !visited.insert(expression) {
            return true;
        }
        match self.node(expression).operation() {
            dae::ExpressionOperation::Literal(_) | dae::ExpressionOperation::Coordinate(_) => true,
            dae::ExpressionOperation::Unary { operand, .. } => self.is_total(operand, visited),
            dae::ExpressionOperation::Binary { lhs, rhs, .. } => {
                self.is_total(lhs, visited) && self.is_total(rhs, visited)
            }
            dae::ExpressionOperation::Field { base, .. } => self.is_total(base, visited),
            dae::ExpressionOperation::Conditional(operands)
            | dae::ExpressionOperation::Array(operands)
            | dae::ExpressionOperation::Record(operands)
            | dae::ExpressionOperation::Builtin {
                arguments: operands,
                ..
            } => (0..operands.len()).all(|index| {
                operands
                    .get(index)
                    .is_some_and(|operand| self.is_total(operand, visited))
            }),
            dae::ExpressionOperation::Index { base, subscripts } => {
                let extents = self.node(base).value_type().dimensions();
                self.is_total(base, visited)
                    && subscripts.iter().enumerate().all(|(axis, subscript)| {
                        self.subscript_is_total(subscript, extents.get(axis).copied())
                    })
            }
            _ => false,
        }
    }

    /// Whether one subscript of an indexed read cannot fail: a whole axis, a
    /// literal, or an index proven inside the axis of extent `extent`.
    fn subscript_is_total(&self, subscript: dae::SubscriptView<'dae>, extent: Option<u32>) -> bool {
        let literal = |expression| {
            matches!(
                self.node(expression).operation(),
                dae::ExpressionOperation::Literal(_)
            )
        };
        match subscript {
            dae::SubscriptView::Whole { .. } => true,
            dae::SubscriptView::Slice { expression, .. } => literal(expression),
            dae::SubscriptView::Index { expression, .. } => {
                literal(expression)
                    || extent.is_some_and(|extent| self.subscript_in_bounds(expression, extent))
            }
        }
    }

    /// Whether an integer subscript over family binders lies in `1..=extent`
    /// at every point of its domains, so reading it cannot fail.
    fn subscript_in_bounds(&self, expression: dae::ExprId<'dae>, extent: u32) -> bool {
        self.binder_subscript_interval(expression)
            .is_some_and(|(low, high)| low >= 1 && high <= i64::from(extent))
    }

    /// The closed integer interval an affine subscript of binders and integer
    /// literals takes over its domains. A binder of the scalar row being
    /// lowered contributes its exact value; any other binder spans its
    /// declared `for` range. Any other operand has no proven interval.
    fn binder_subscript_interval(&self, expression: dae::ExprId<'dae>) -> Option<(i64, i64)> {
        match self.node(expression).operation() {
            dae::ExpressionOperation::Literal(dae::DaeLiteral::Integer(value)) => {
                Some((*value, *value))
            }
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Binder(binder)) => {
                if let Some((_, point)) = self
                    .domain_points
                    .iter()
                    .rev()
                    .find(|(domain, _)| *domain == binder.domain())
                {
                    let value = *point.get(binder.ordinal() as usize)?;
                    return Some((value, value));
                }
                let range = self
                    .view
                    .domain(binder.domain())?
                    .structured()
                    .binders
                    .get(binder.ordinal() as usize)?;
                Some((range.lower.min(range.upper), range.lower.max(range.upper)))
            }
            dae::ExpressionOperation::Unary { operator, operand } => {
                let (low, high) = self.binder_subscript_interval(operand)?;
                match operator {
                    dae::UnaryOperator::Plus => Some((low, high)),
                    dae::UnaryOperator::Negate => Some((high.checked_neg()?, low.checked_neg()?)),
                    _ => None,
                }
            }
            dae::ExpressionOperation::Binary { operator, lhs, rhs } => {
                let (a_low, a_high) = self.binder_subscript_interval(lhs)?;
                let (b_low, b_high) = self.binder_subscript_interval(rhs)?;
                match operator {
                    dae::BinaryOperator::Add => {
                        Some((a_low.checked_add(b_low)?, a_high.checked_add(b_high)?))
                    }
                    dae::BinaryOperator::Subtract => {
                        Some((a_low.checked_sub(b_high)?, a_high.checked_sub(b_low)?))
                    }
                    dae::BinaryOperator::Multiply => {
                        let products = [
                            a_low.checked_mul(b_low)?,
                            a_low.checked_mul(b_high)?,
                            a_high.checked_mul(b_low)?,
                            a_high.checked_mul(b_high)?,
                        ];
                        Some((*products.iter().min()?, *products.iter().max()?))
                    }
                    _ => None,
                }
            }
            _ => None,
        }
    }

    /// Lower scalar `scalar` of a model conditional as a checked
    /// function-conditional program: each condition runs under the preceding
    /// conditions being false, and only the selected arm runs.
    pub(super) fn selected_arm_conditional(
        &mut self,
        operands: dae::ExpressionOperands<'dae>,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        if !self.function_arguments.is_empty()
            || !self.function_fold_values.is_empty()
            || self.deferred_fold_captures.is_some()
            || self.deferred_function_conditional_captures.is_some()
        {
            return Err(LowerError::contract(
                "a conditional arm that is not total escaped a context that owns its \
                 selected-arm program",
                span,
            ));
        }
        let ids = operands.iter().collect::<Vec<_>>();
        let (Some((&fallback, arm_ids)), true) = (ids.split_last(), ids.len() % 2 == 1) else {
            return Err(LowerError::contract(
                "conditional operands are not condition-value pairs and a fallback",
                span,
            ));
        };
        // A literal condition is decided here: a false arm is never reached,
        // and a true one is the value every later arm falls back to.
        let mut live = Vec::new();
        let mut fallback_value = fallback;
        for pair in arm_ids.chunks_exact(2) {
            let (condition, value) = (pair[0], pair[1]);
            match self.node(condition).operation() {
                dae::ExpressionOperation::Literal(dae::DaeLiteral::Boolean(false)) => {}
                dae::ExpressionOperation::Literal(dae::DaeLiteral::Boolean(true)) => {
                    fallback_value = value;
                    break;
                }
                _ => live.push((condition, value)),
            }
        }
        if live.is_empty() {
            return self.expression(fallback_value, scalar);
        }
        let conditions = live
            .iter()
            .map(|&(condition, _)| condition)
            .collect::<Vec<_>>();
        let captures = self
            .symbolic_domain_points
            .iter()
            .flat_map(|(_, registers)| registers.iter().copied())
            .collect::<Vec<_>>();
        let mut arms = Vec::with_capacity(conditions.len());
        for (ordinal, &condition) in conditions.iter().enumerate() {
            let prior = conditions[..ordinal]
                .iter()
                .map(|&condition| (condition, false))
                .collect::<Vec<_>>();
            let condition_region = self
                .selected_arm_region(&prior, span)?
                .selected_arm_output(condition, 0)?;
            let mut selected = prior;
            selected.push((condition, true));
            let value = live[ordinal].1;
            let result_region = self
                .selected_arm_region(&selected, span)?
                .selected_arm_output(value, scalar)?;
            arms.push((condition_region, result_region));
        }
        let all_prior = conditions
            .iter()
            .map(|&condition| (condition, false))
            .collect::<Vec<_>>();
        let fallback = self
            .selected_arm_region(&all_prior, span)?
            .selected_arm_output(fallback_value, scalar)?;
        let program = match solve::FunctionConditionalProgram::checked(
            captures.len(),
            vec![1],
            arms,
            fallback,
        ) {
            Ok(program) => program,
            Err(error) => {
                return Err(LowerError::contract(
                    format!("selected-arm conditional proof failed: {error}"),
                    span,
                ));
            }
        };
        let capture_start = self.next_register;
        for source in captures {
            let destination = self.register(span)?;
            self.ops.push(solve::LinearOp::Move {
                dst: destination,
                src: source,
            });
        }
        let result = self.register(span)?;
        self.ops.push(solve::LinearOp::FunctionConditional {
            dst_start: result,
            capture_start,
            program: Arc::new(program),
        });
        Ok(result)
    }

    fn selected_arm_output(
        mut self,
        expression: dae::ExprId<'dae>,
        scalar: usize,
    ) -> Result<Vec<solve::LinearOp>, LowerError> {
        let output = self.expression(expression, scalar)?;
        self.ops.push(solve::LinearOp::StoreOutput { src: output });
        Ok(self.ops)
    }

    /// A compiler for one region of a selected-arm program: the enclosing
    /// evaluation context with its own register file, the symbolic domain
    /// points loaded from the program's captures, and the branch guards on
    /// its activation path so scheduled call assertions keep the same path.
    fn selected_arm_region(
        &self,
        branch_guards: &[(dae::ExprId<'dae>, bool)],
        span: Span,
    ) -> Result<Self, LowerError> {
        let mut compiler = Self::new(self.view, self.layout, None);
        compiler.domain_points = self.domain_points.clone();
        let mut index = 0usize;
        for (domain, registers) in &self.symbolic_domain_points {
            let mut local = Vec::with_capacity(registers.len());
            for _ in registers {
                let destination = compiler.register(span)?;
                compiler
                    .ops
                    .push(solve::LinearOp::LoadFunctionConditionalCapture {
                        dst: destination,
                        index,
                    });
                local.push(destination);
                index += 1;
            }
            compiler.symbolic_domain_points.push((*domain, local));
        }
        compiler.activation_path = self
            .activation_path
            .iter()
            .map(|guard| ActivationGuard {
                register: None,
                ..*guard
            })
            .chain(
                branch_guards
                    .iter()
                    .map(|&(condition, expected)| ActivationGuard {
                        condition: ActivationCondition::Expression(condition),
                        register: None,
                        expected,
                    }),
            )
            .collect();
        compiler.fold_guard_base = compiler.activation_path.len();
        compiler.active_clock = self.active_clock;
        compiler.sampled_source = self.sampled_source;
        compiler.derivative_definitions = self.derivative_definitions;
        compiler.affine_derivative_systems = self.affine_derivative_systems;
        compiler.active_derivatives = self.active_derivatives.clone();
        compiler.derivative_seeds = self.derivative_seeds.clone();
        compiler.parameter_substitutions = self.parameter_substitutions;
        compiler.active_parameters = self.active_parameters.clone();
        compiler.active_call_assertions = self.active_call_assertions.clone();
        compiler.call_action_compilation = self.call_action_compilation;
        compiler.suppress_function_assertions = self.suppress_function_assertions;
        compiler.context_ids = self.context_ids.clone();
        compiler.context_frames = self.context_frames.clone();
        compiler.context_stack = self.context_stack.clone();
        compiler.context_id = self.context_id;
        compiler.next_context_id = self.next_context_id;
        compiler.function_conditional_owners = self.function_conditional_owners;
        Ok(compiler)
    }
}
