//! Selected-arm evaluation of a model conditional (SPEC_0040 DAE-C22).
//!
//! An eager selection computes every arm and discards all but one, which is
//! sound only when every arm cannot fault (the shared DAE fault classification
//! of `lower::dae_faults`, SOLVE-C102): IEEE 754 arithmetic, relations, pure
//! builtins, reads, Integer arithmetic proven inside the Integer domain, and
//! subscripts proven inside the indexed dimension evaluate to a value, possibly
//! non-finite, and the selection discards the unselected value and its tangent.
//! A conditional with any other arm (a call, which can assert, fail, or act, or
//! an operation that may fault) is lowered as a checked function-conditional
//! program that runs only the selected arm.

use super::*;
use crate::lower::dae_faults::{CoordinateRead, CoordinateReads, DaeFaults, FaultMemo};

/// A coordinate with a same-tick current definition reads that definition.
impl<'dae> CoordinateReads<'dae> for ScalarCompiler<'_, 'dae> {
    fn read(&self, coordinate: dae::CoordinateView<'dae>) -> CoordinateRead<'dae> {
        match self.current_definition(coordinate) {
            Ok(Some((variable, definition))) => {
                if self.current_definition_is_acyclic(variable, definition) {
                    CoordinateRead::Defined(definition)
                } else {
                    CoordinateRead::Unproven
                }
            }
            Ok(None) => CoordinateRead::Free,
            Err(_) => CoordinateRead::Unproven,
        }
    }
}

impl<'layout, 'dae> ScalarCompiler<'layout, 'dae> {
    fn faults(&self) -> DaeFaults<'_, 'dae> {
        DaeFaults::new(self.view, &self.domain_points)
            .with_coordinates(self)
            .differentiable()
            .scalar_signum()
    }

    pub(super) fn call_arguments_are_total(&self, arguments: &[dae::ExprId<'dae>]) -> bool {
        let mut memo = FaultMemo::new();
        arguments
            .iter()
            .all(|&argument| self.faults().is_total(&mut memo, argument))
    }

    /// Whether a conditional can be lowered to an eager selection: every
    /// operand after the first condition is total, where a node the first
    /// condition already computes counts as evaluated, since that condition
    /// runs whichever arm is selected.
    pub(super) fn conditional_is_total(&self, operands: dae::ExpressionOperands<'dae>) -> bool {
        let mut memo = FaultMemo::new();
        if let Some(first) = operands.get(0) {
            self.mark_evaluated(first, &mut memo);
        }
        (1..operands.len()).all(|index| {
            operands
                .get(index)
                .is_some_and(|operand| self.faults().is_total(&mut memo, operand))
        })
    }

    fn mark_evaluated(&self, expression: dae::ExprId<'dae>, memo: &mut FaultMemo<'dae>) {
        if memo
            .insert(expression, solve::FaultClass::CannotFault)
            .is_some()
        {
            return;
        }
        match self.node(expression).operation() {
            dae::ExpressionOperation::Coordinate(coordinate) => {
                if let Ok(Some((_, definition))) = self.current_definition(coordinate) {
                    self.mark_evaluated(definition, memo);
                }
            }
            dae::ExpressionOperation::Unary { operand, .. } => {
                self.mark_evaluated(operand, memo);
            }
            dae::ExpressionOperation::Binary { lhs, rhs, .. } => {
                self.mark_evaluated(lhs, memo);
                self.mark_evaluated(rhs, memo);
            }
            dae::ExpressionOperation::Field { base, .. } => self.mark_evaluated(base, memo),
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
                    self.mark_evaluated(element, memo);
                }
            }
            _ => {}
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
        // Immutable conditions are decided in the original lexical domain
        // context before constructing an inactive arm's indexed reads. A
        // tunable guard retains the checked runtime selected-arm program.
        let mut live = Vec::new();
        let mut fallback_value = fallback;
        let selector = ScalarSelector::from_points(self.view, &self.domain_points);
        for pair in arm_ids.chunks_exact(2) {
            let (condition, value) = (pair[0], pair[1]);
            match selector.translation_guard(condition)? {
                Some(false) => {}
                Some(true) => {
                    fallback_value = value;
                    break;
                }
                None => live.push((condition, value)),
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
        self.emit_conditional_program(captures, program, span)
    }

    pub(super) fn emit_conditional_program(
        &mut self,
        captures: Vec<solve::Reg>,
        program: solve::FunctionConditionalProgram,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let capture_start = self.next_register;
        for source in captures {
            let destination = self.register(span)?;
            self.emit(solve::LinearOp::Move {
                dst: destination,
                src: source,
            })?;
        }
        let result = self.register(span)?;
        self.emit(solve::LinearOp::FunctionConditional {
            dst_start: result,
            capture_start,
            program: Arc::new(program),
        })?;
        Ok(result)
    }

    fn selected_arm_output(
        mut self,
        expression: dae::ExprId<'dae>,
        scalar: usize,
    ) -> Result<Vec<solve::LinearOp>, LowerError> {
        let output = self.expression(expression, scalar)?;
        self.emit(solve::LinearOp::StoreOutput { src: output })?;
        self.finish_operations()
    }

    /// A compiler for one region of a selected-arm program: the enclosing
    /// evaluation context with its own register file, the symbolic domain
    /// points loaded from the program's captures, and the branch guards on
    /// its activation path so scheduled call assertions keep the same path.
    pub(super) fn selected_arm_region(
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
                compiler.emit(solve::LinearOp::LoadFunctionConditionalCapture {
                    dst: destination,
                    index,
                })?;
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
        compiler.inherit_current_definitions(self);
        compiler.derivative_definitions = self.derivative_definitions;
        compiler.affine_derivative_systems = self.affine_derivative_systems;
        compiler.active_derivatives = self.active_derivatives.clone();
        compiler.derivative_seeds = self.derivative_seeds.clone();
        compiler.parameter_substitutions = self.parameter_substitutions;
        compiler.active_parameters = self.active_parameters.clone();
        compiler.active_call_assertions = self.active_call_assertions.clone();
        compiler.call_action_compilation = self.call_action_compilation;
        compiler.call_use = self.call_use;
        compiler.suppress_function_assertions = self.suppress_function_assertions;
        compiler.contexts = Rc::clone(&self.contexts);
        compiler.context_stack = self.context_stack.clone();
        compiler.context_id = self.context_id;
        compiler.function_conditional_owners = self.function_conditional_owners;
        Ok(compiler)
    }
}
