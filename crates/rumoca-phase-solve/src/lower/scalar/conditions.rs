use rumoca_core::Span;
use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

use super::{ActivationCondition, ActivationGuard, ScalarCompiler};
use crate::LowerError;

impl<'layout, 'dae> ScalarCompiler<'layout, 'dae> {
    pub(in crate::lower) fn condition_program(
        mut self,
        condition: dae::ConditionId<'dae>,
    ) -> Result<Vec<solve::LinearOp>, LowerError> {
        let output = self.condition(condition)?;
        self.ops.push(solve::LinearOp::StoreOutput { src: output });
        Ok(self.ops)
    }

    /// Compile a condition that lives inside `clock`'s partition.
    ///
    /// A clocked relation reads `previous(...)` and clock-owned declarations, which only
    /// resolve while their owning schedule is the active one (MLS §16.5).
    pub(in crate::lower) fn clocked_condition_program(
        mut self,
        clock: dae::ClockId<'dae>,
        condition: dae::ConditionId<'dae>,
    ) -> Result<Vec<solve::LinearOp>, LowerError> {
        self.active_clock = Some(clock);
        let output = self.condition(condition)?;
        self.ops.push(solve::LinearOp::StoreOutput { src: output });
        Ok(self.ops)
    }

    pub(in crate::lower) fn edge_condition_program(
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

    pub(in crate::lower) fn clocked_action_condition_program(
        mut self,
        clock: dae::ClockId<'dae>,
        guard: dae::ConditionId<'dae>,
        span: Span,
    ) -> Result<Vec<solve::LinearOp>, LowerError> {
        self.active_clock = Some(clock);
        let guard = self.condition(guard)?;
        let activation = self
            .layout
            .clock_activations
            .get(clock.index() as usize)
            .copied()
            .ok_or_else(|| {
                LowerError::contract("clocked action has no activation parameter", span)
            })?;
        let activation = self.load_slot(solve::scalar_slot_p(activation), span)?;
        let output = self.binary(dae::BinaryOperator::And, activation, guard, span)?;
        self.ops.push(solve::LinearOp::StoreOutput { src: output });
        Ok(self.ops)
    }

    /// Compile roots owned by one exact source occurrence as one program.
    /// Function-call conditions under that owner then retain their shared DAE
    /// dependencies and are evaluated with the source call's cardinality.
    pub(in crate::lower) fn root_program_outputs(
        mut self,
        relations: impl IntoIterator<Item = dae::RelationId<'dae>>,
    ) -> Result<Vec<solve::LinearOp>, LowerError> {
        for relation in relations {
            let relation = self
                .view
                .relation(relation)
                .expect("checked relation identity resolves");
            let output =
                self.root_expression(relation.expression(), relation.provenance().span())?;
            self.ops.push(solve::LinearOp::StoreOutput { src: output });
        }
        Ok(self.ops)
    }

    pub(in crate::lower) fn root_expression_program(
        mut self,
        expression: dae::ExprId<'dae>,
        span: Span,
    ) -> Result<Vec<solve::LinearOp>, LowerError> {
        let output = self.root_expression(expression, span)?;
        self.ops.push(solve::LinearOp::StoreOutput { src: output });
        Ok(self.ops)
    }

    fn root_expression(
        &mut self,
        expression: dae::ExprId<'dae>,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let node = self.node(expression);
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
                let condition = self.expression(expression, 0)?;
                let when_true = self.constant(-1.0, span)?;
                let when_false = self.constant(1.0, span)?;
                self.select(condition, when_true, when_false, span)?
            }
        };
        Ok(output)
    }

    pub(in crate::lower) fn guarded_assignment_group_program(
        mut self,
        clock: Option<dae::ClockId<'dae>>,
        targets: &[super::super::events::GuardedTarget<'dae>],
    ) -> Result<Vec<solve::LinearOp>, LowerError> {
        self.active_clock = clock;
        let branch_count = self.guarded_dynamic_branch_count(targets)?;
        if branch_count == 0 {
            self.store_guarded_assignment_fallbacks(targets)?;
            return Ok(self.ops);
        }
        let target_widths = targets
            .iter()
            .map(|target| target.width)
            .collect::<Vec<_>>();
        let mut arms = Vec::with_capacity(branch_count);
        let mut prior = Vec::with_capacity(branch_count);
        for ordinal in 0..branch_count {
            let branch = self.dynamic_guarded_branches(&targets[0])[ordinal];
            let owner = Self::guarded_activation_owner(clock, branch);
            let condition =
                self.guarded_assignment_condition_region(&prior, owner, targets[0].span)?;
            let mut selected = prior.clone();
            selected.push((owner, true));
            let result =
                self.guarded_assignment_result_region(targets, clock, ordinal, &selected)?;
            arms.push((condition, result));
            prior.push((owner, false));
        }
        let fallback = self.guarded_assignment_fallback_region(targets, clock, &prior)?;
        let span = targets[0].span;
        let program = solve::FunctionConditionalProgram::checked(0, target_widths, arms, fallback)
            .map_err(|error| {
                LowerError::contract(
                    format!("guarded-assignment program proof failed: {error}"),
                    span,
                )
            })?;
        let result_count = program.result_count();
        let dst_start = self.next_register;
        for _ in 0..result_count {
            self.register(span)?;
        }
        self.ops.push(solve::LinearOp::FunctionConditional {
            dst_start,
            capture_start: 0,
            program: std::sync::Arc::new(program),
        });
        self.ops.push(solve::LinearOp::StoreOutputRange {
            start: dst_start,
            count: result_count,
            stride: 1,
        });
        Ok(self.ops)
    }

    fn guarded_dynamic_branch_count(
        &self,
        targets: &[super::super::events::GuardedTarget<'dae>],
    ) -> Result<usize, LowerError> {
        let first = targets
            .first()
            .expect("guarded-target partition is always nonempty");
        let count = self.dynamic_guarded_branches(first).len();
        for target in &targets[1..] {
            if self.dynamic_guarded_branches(target).len() != count {
                return Err(LowerError::contract(
                    "guarded-assignment group has incompatible branch topology",
                    first.span,
                ));
            }
        }
        Ok(count)
    }

    fn dynamic_guarded_branches<'scope>(
        &self,
        target: &'scope super::super::events::GuardedTarget<'dae>,
    ) -> &'scope [super::super::events::GuardedAssignment<'dae>] {
        &target.branches[..target.dynamic_branch_count]
    }

    fn guarded_unconditional_branch(
        &self,
        target: &super::super::events::GuardedTarget<'dae>,
    ) -> Option<super::super::events::GuardedAssignment<'dae>> {
        target
            .fallback_branch
            .and_then(|index| target.branches.get(index).copied())
    }

    const fn guarded_activation_owner(
        clock: Option<dae::ClockId<'dae>>,
        branch: super::super::events::GuardedAssignment<'dae>,
    ) -> ActivationCondition<'dae> {
        let (trigger, guard, _, trigger_memory) = branch;
        ActivationCondition::GuardedAssignment {
            clock,
            trigger,
            guard,
            trigger_memory,
        }
    }

    fn guarded_region_compiler(
        &self,
        clock: Option<dae::ClockId<'dae>>,
        path: &[(ActivationCondition<'dae>, bool)],
    ) -> ScalarCompiler<'layout, 'dae> {
        let mut compiler = Self::new(self.view, self.layout, None);
        compiler.active_clock = clock;
        compiler.activation_path = path
            .iter()
            .copied()
            .map(|(condition, expected)| ActivationGuard {
                condition,
                register: None,
                expected,
            })
            .collect();
        compiler.fold_guard_base = compiler.activation_path.len();
        compiler
    }

    fn guarded_assignment_condition_region(
        &self,
        prior: &[(ActivationCondition<'dae>, bool)],
        owner: ActivationCondition<'dae>,
        span: Span,
    ) -> Result<Vec<solve::LinearOp>, LowerError> {
        let mut compiler = self.guarded_region_compiler(self.active_clock, prior);
        let condition = compiler.materialize_activation_condition(owner, span)?;
        compiler
            .ops
            .push(solve::LinearOp::StoreOutput { src: condition });
        Ok(compiler.ops)
    }

    fn guarded_assignment_result_region(
        &self,
        targets: &[super::super::events::GuardedTarget<'dae>],
        clock: Option<dae::ClockId<'dae>>,
        ordinal: usize,
        path: &[(ActivationCondition<'dae>, bool)],
    ) -> Result<Vec<solve::LinearOp>, LowerError> {
        let mut compiler = self.guarded_region_compiler(clock, path);
        for target in targets {
            let branch = compiler.dynamic_guarded_branches(target)[ordinal];
            compiler.store_guarded_assignment_value(branch.2, target.width, target.span)?;
        }
        Ok(compiler.ops)
    }

    fn guarded_assignment_fallback_region(
        &self,
        targets: &[super::super::events::GuardedTarget<'dae>],
        clock: Option<dae::ClockId<'dae>>,
        path: &[(ActivationCondition<'dae>, bool)],
    ) -> Result<Vec<solve::LinearOp>, LowerError> {
        let mut compiler = self.guarded_region_compiler(clock, path);
        compiler.store_guarded_assignment_fallbacks(targets)?;
        Ok(compiler.ops)
    }

    fn store_guarded_assignment_fallbacks(
        &mut self,
        targets: &[super::super::events::GuardedTarget<'dae>],
    ) -> Result<(), LowerError> {
        for target in targets {
            if let Some((_, _, value, _)) = self.guarded_unconditional_branch(target) {
                self.store_guarded_assignment_value(value, target.width, target.span)?;
            } else {
                let start = self.load_guarded_target_range(target)?;
                self.ops.push(solve::LinearOp::StoreOutputRange {
                    start,
                    count: target.width,
                    stride: 1,
                });
            }
        }
        Ok(())
    }

    /// Reconstruct one checked activation owner in a compiler fork that cannot
    /// reuse the original register file. The exact owner, rather than a
    /// lowered Boolean expression graph, is retained in `activation_path`.
    pub(super) fn materialize_activation_condition(
        &mut self,
        condition: ActivationCondition<'dae>,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        match condition {
            ActivationCondition::Expression(expression) => self.expression(expression, 0),
            ActivationCondition::GuardedAssignment {
                clock,
                trigger,
                guard,
                trigger_memory,
            } => {
                if self.active_clock != clock {
                    return Err(LowerError::contract(
                        "guarded-assignment activation escaped its typed clock owner",
                        span,
                    ));
                }
                if clock.is_some() {
                    return self.condition(guard);
                }
                let edge = self.trigger_edge(trigger, trigger_memory, span)?;
                let guard = self.condition(guard)?;
                self.binary(dae::BinaryOperator::And, edge, guard, span)
            }
        }
    }

    fn store_guarded_assignment_value(
        &mut self,
        value: dae::ExprId<'dae>,
        width: usize,
        span: Span,
    ) -> Result<(), LowerError> {
        if self.node(value).value_type().scalar_count() != Some(width) {
            return Err(LowerError::contract(
                "guarded-assignment value width differs from its target range",
                span,
            ));
        }
        let start = self.pack_expression(value)?;
        self.ops.push(solve::LinearOp::StoreOutputRange {
            start,
            count: width,
            stride: 1,
        });
        Ok(())
    }

    fn load_guarded_target_range(
        &mut self,
        target: &super::super::events::GuardedTarget<'dae>,
    ) -> Result<solve::Reg, LowerError> {
        if target.width == 0 {
            return Err(LowerError::contract(
                "guarded-assignment target range is empty",
                target.span,
            ));
        }
        let (input, input_start) = match target.target_base {
            solve::ScalarSlot::Y { index, .. } => (solve::TensorInputKind::Y, index),
            solve::ScalarSlot::P { index, .. } => (solve::TensorInputKind::P, index),
            solve::ScalarSlot::Time | solve::ScalarSlot::Constant(_) => {
                return Err(LowerError::contract(
                    "guarded-assignment hold range is not mutable storage",
                    target.span,
                ));
            }
        };
        let dst_start = self.next_register;
        for _ in 0..target.width {
            self.register(target.span)?;
        }
        self.ops.push(solve::LinearOp::TensorLoad {
            dst_start,
            input,
            input_start,
            count: target.width,
            seed_start: None,
            lanes: 1,
        });
        Ok(dst_start)
    }

    /// The rising edge that activates `trigger`.
    ///
    /// MLS §8.3.5.1 gives a `when` the activation `edge(b)` over one `Boolean b`
    /// holding the condition, and a *vector* `when` one `bi` per element with
    /// the activation `edge(b1) or … or edge(bn)`. The two are different
    /// functions of the same operands — `{u, not u}` rises at every switch of
    /// `u` while `u or not u` never rises — so a vector activation is expanded
    /// leaf by leaf here rather than read as a single buffered disjunction.
    fn trigger_edge(
        &mut self,
        trigger: dae::ConditionId<'dae>,
        trigger_memory: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        if matches!(
            self.view
                .condition(trigger)
                .expect("checked condition identity resolves")
                .operation(),
            dae::ConditionOperation::AnyRise(_, _)
        ) {
            return self.any_element_edge(trigger, span);
        }
        self.buffered_edge(trigger, trigger_memory, span)
    }

    /// `edge(b1) or … or edge(bn)` over the leaves of an `AnyRise` tree.
    fn any_element_edge(
        &mut self,
        condition: dae::ConditionId<'dae>,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let operation = self
            .view
            .condition(condition)
            .expect("checked condition identity resolves")
            .operation();
        let dae::ConditionOperation::AnyRise(lhs, rhs) = operation else {
            // A leaf of the vector: its own buffer, its own edge.
            let memory = crate::lower::events::condition_memory(self.layout, condition, span)?;
            return self.buffered_edge(condition, memory, span);
        };
        let lhs = self.any_element_edge(lhs, span)?;
        let rhs = self.any_element_edge(rhs, span)?;
        self.binary(dae::BinaryOperator::Or, lhs, rhs, span)
    }

    fn buffered_edge(
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

    pub(super) fn condition(
        &mut self,
        condition: dae::ConditionId<'dae>,
    ) -> Result<solve::Reg, LowerError> {
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
                None => {
                    let index = self
                        .layout
                        .clock_activations
                        .get(clock.index() as usize)
                        .copied()
                        .ok_or_else(|| {
                            LowerError::contract(
                                "clock condition has no derived Solve activation lane",
                                span,
                            )
                        })?;
                    self.load_slot(solve::scalar_slot_p(index), span)
                }
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
            // The *level* of a vector activation is the disjunction of its
            // elements; only its edge is per-element (`trigger_edge`). Reading
            // it as a level is what a guard does, and `edge(bi)` implies `bi`,
            // so the guard never narrows the activation it accompanies.
            dae::ConditionOperation::Or(lhs, rhs) | dae::ConditionOperation::AnyRise(lhs, rhs) => {
                let lhs = self.condition(lhs)?;
                let rhs = self.condition(rhs)?;
                self.binary(dae::BinaryOperator::Or, lhs, rhs, span)
            }
            // An unguarded algorithm section and a section-level `assert` are
            // not `when`s; they carry no §8.5 buffer, so `edge` over this reads
            // the level itself (see `lower_condition_memory`).
            dae::ConditionOperation::Always => self.constant(1.0, span),
        }
    }
}
