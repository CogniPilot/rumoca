use rumoca_core::Span;
use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

use super::{ActivationCondition, ActivationGuard, ScalarCompiler, ScalarContextFrame};
use crate::LowerError;

impl<'layout, 'dae> ScalarCompiler<'layout, 'dae> {
    pub(in crate::lower) fn condition_program(
        mut self,
        condition: dae::ConditionId<'dae>,
    ) -> Result<Vec<solve::LinearOp>, LowerError> {
        let output = self.condition(condition)?;
        self.emit(solve::LinearOp::StoreOutput { src: output })?;
        self.finish_operations()
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
        self.emit(solve::LinearOp::StoreOutput { src: output })?;
        self.finish_operations()
    }

    pub(in crate::lower) fn edge_condition_program(
        mut self,
        trigger: dae::ConditionId<'dae>,
        guard: dae::ConditionId<'dae>,
        trigger_memory: usize,
        span: Span,
    ) -> Result<Vec<solve::LinearOp>, LowerError> {
        let edge = self.trigger_edge(trigger, trigger_memory, span)?;
        let output =
            self.activated_action_guard(edge, guard, Some((trigger, trigger_memory)), span)?;
        self.emit(solve::LinearOp::StoreOutput { src: output })?;
        self.finish_operations()
    }

    /// A warning-level action (MLS §8.3.7): `edge(trigger) and guard and not
    /// holds`, where `holds` is an ordinary Boolean expression evaluated
    /// without events.
    pub(in crate::lower) fn warning_condition_program(
        mut self,
        trigger: dae::ConditionId<'dae>,
        guard: dae::ConditionId<'dae>,
        trigger_memory: usize,
        holds: dae::ExprId<'dae>,
        span: Span,
    ) -> Result<Vec<solve::LinearOp>, LowerError> {
        let edge = self.trigger_edge(trigger, trigger_memory, span)?;
        let guard = self.condition(guard)?;
        let active = self.binary(dae::BinaryOperator::And, edge, guard, span)?;
        let output = self.violated(active, holds, span)?;
        self.emit(solve::LinearOp::StoreOutput { src: output })?;
        self.finish_operations()
    }

    /// A warning-level action owned by a clock: active on its ticks.
    pub(in crate::lower) fn clocked_warning_condition_program(
        mut self,
        clock: dae::ClockId<'dae>,
        guard: dae::ConditionId<'dae>,
        holds: dae::ExprId<'dae>,
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
        let active = self.binary(dae::BinaryOperator::And, activation, guard, span)?;
        let output = self.violated(active, holds, span)?;
        self.emit(solve::LinearOp::StoreOutput { src: output })?;
        self.finish_operations()
    }

    fn violated(
        &mut self,
        active: solve::Reg,
        holds: dae::ExprId<'dae>,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let mut selected = self.selected_arm_region(&[], span)?;
        let holds = selected.expression(holds, 0)?;
        let violated = selected.unary(dae::UnaryOperator::Not, holds, span)?;
        selected
            .ops
            .push(solve::LinearOp::StoreOutput { src: violated });
        self.emit_activated_action_region(active, selected.ops, span)
    }

    /// `prefix and guard` for an event action. A `when` body's `assert` has
    /// the guard `activation and failure` (algorithm and when-equation
    /// lowering in phase-dae put the activation first). MLS 3.7 §11.2.7 runs a
    /// when body only on its activation and §8.6 not during initialization, so
    /// the failure conjunct, which may index or divide by values that are only
    /// valid once the body is active, is evaluated in a region entered only
    /// when `prefix` and the activation hold. Any other guard lowers as before.
    /// An unclocked `trigger` (with its edge memory) is the region's outer
    /// activation, so the call-scoped programs of a call compiled in the region
    /// run only on that activation and own no root.
    fn activated_action_guard(
        &mut self,
        prefix: solve::Reg,
        guard: dae::ConditionId<'dae>,
        trigger: Option<(dae::ConditionId<'dae>, usize)>,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let node = self
            .view
            .condition(guard)
            .expect("checked condition identity resolves");
        let dae::ConditionOperation::And(activation_condition, failure) = node.operation() else {
            let guard = self.condition(guard)?;
            return self.binary(dae::BinaryOperator::And, prefix, guard, span);
        };
        let activation = self.condition(activation_condition)?;
        let active = self.binary(dae::BinaryOperator::And, prefix, activation, span)?;
        let mut selected = self.selected_arm_region(&[], span)?;
        if let Some((trigger, trigger_memory)) = trigger {
            // Materialized only where a fork reads it: the region itself runs
            // only when it holds.
            selected.activation_path.push(ActivationGuard {
                condition: ActivationCondition::GuardedAssignment {
                    clock: None,
                    periodic: false,
                    trigger,
                    guard: activation_condition,
                    trigger_memory,
                },
                register: None,
                expected: true,
            });
            selected.fold_guard_base = selected.activation_path.len();
        }
        let failed = selected.condition(failure)?;
        let failed = selected.unless_a_called_assertion_failed(failed, span)?;
        selected
            .ops
            .push(solve::LinearOp::StoreOutput { src: failed });
        self.emit_activated_action_region(active, selected.ops, span)
    }

    /// `failed` and every error-level predicate of the typed calls this
    /// region invoked. A failed error-level assert aborts its function call
    /// (MLS 3.7 §8.3.7), so the call's own action reports it and the assertion
    /// reading the aborted call's value does not. The region's call cache
    /// holds exactly the calls compiled here; predicates are combined in
    /// register order.
    fn unless_a_called_assertion_failed(
        &mut self,
        failed: solve::Reg,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let mut predicates = Vec::new();
        for (start, registered) in self.typed_pure_call_cache.values() {
            predicates.extend(error_predicate_registers(*start, registered, span)?);
        }
        predicates.sort_unstable();
        predicates.into_iter().try_fold(failed, |failed, holds| {
            self.binary(dae::BinaryOperator::And, failed, holds, span)
        })
    }

    /// A one-output conditional: `selected` when `active`, otherwise false.
    /// `active` is a capture after the selected-arm region's own domain-point
    /// captures, so the region never reads the outer register file.
    fn emit_activated_action_region(
        &mut self,
        active: solve::Reg,
        selected: Vec<solve::LinearOp>,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let mut captures: Vec<solve::Reg> = self
            .symbolic_domain_points
            .iter()
            .flat_map(|(_, registers)| registers.iter().copied())
            .collect();
        let active_index = captures.len();
        captures.push(active);
        let mut condition = self.selected_arm_region(&[], span)?;
        let gate = condition.register(span)?;
        condition
            .ops
            .push(solve::LinearOp::LoadFunctionConditionalCapture {
                dst: gate,
                index: active_index,
            });
        condition
            .ops
            .push(solve::LinearOp::StoreOutput { src: gate });
        let mut fallback = self.selected_arm_region(&[], span)?;
        let inactive = fallback.constant(0.0, span)?;
        fallback
            .ops
            .push(solve::LinearOp::StoreOutput { src: inactive });
        let program = solve::FunctionConditionalProgram::checked(
            captures.len(),
            vec![1],
            vec![(condition.ops, selected)],
            fallback.ops,
        )
        .map_err(|error| {
            LowerError::contract(
                format!("event-action activation proof failed: {error}"),
                span,
            )
        })?;
        self.emit_conditional_program(captures, program, span)
    }

    pub(in crate::lower) fn clocked_action_condition_program(
        mut self,
        clock: dae::ClockId<'dae>,
        guard: dae::ConditionId<'dae>,
        span: Span,
    ) -> Result<Vec<solve::LinearOp>, LowerError> {
        self.active_clock = Some(clock);
        let activation = self
            .layout
            .clock_activations
            .get(clock.index() as usize)
            .copied()
            .ok_or_else(|| {
                LowerError::contract("clocked action has no activation parameter", span)
            })?;
        let activation = self.load_slot(solve::scalar_slot_p(activation), span)?;
        let output = self.activated_action_guard(activation, guard, None, span)?;
        self.emit(solve::LinearOp::StoreOutput { src: output })?;
        self.finish_operations()
    }

    /// Compile roots owned by one exact source occurrence as one program.
    /// Function-call conditions under that owner then retain their shared DAE
    /// dependencies and are evaluated with the source call's cardinality.
    pub(in crate::lower) fn root_program_outputs(
        mut self,
        relations: impl IntoIterator<Item = (dae::RelationId<'dae>, Option<dae::ExprId<'dae>>)>,
    ) -> Result<Vec<solve::LinearOp>, LowerError> {
        for (relation, guard) in relations {
            let relation = self
                .view
                .relation(relation)
                .expect("checked relation identity resolves");
            let output = self.activated_root_expression(
                relation.expression(),
                guard,
                relation.provenance().span(),
            )?;
            self.emit(solve::LinearOp::StoreOutput { src: output })?;
        }
        self.finish_operations()
    }

    pub(in crate::lower) fn root_expression_program(
        mut self,
        expression: dae::ExprId<'dae>,
        guard: Option<dae::ExprId<'dae>>,
        span: Span,
    ) -> Result<Vec<solve::LinearOp>, LowerError> {
        let output = self.activated_root_expression(expression, guard, span)?;
        self.emit(solve::LinearOp::StoreOutput { src: output })?;
        self.finish_operations()
    }

    pub(super) fn root_expression(
        &mut self,
        expression: dae::ExprId<'dae>,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let node = self.node(expression);
        if node.value_type().scalar_type() == dae::ScalarType::Boolean
            && let Some(condition) = self.exact_integer_expression(expression, 0)?
        {
            // Integer relations change at discrete events (MLS §3.8.5).
            // Their signed truth surface must not subtract rounded operands
            // or introduce overflow by subtracting two valid Integers.
            let when_true = self.constant(-1.0, span)?;
            let when_false = self.constant(1.0, span)?;
            return self.select(condition, when_true, when_false, span);
        }
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
                let condition = self.unbuffered_expression(expression, 0)?;
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
            return self.finish_operations();
        }
        let target_widths = targets
            .iter()
            .map(|target| target.width)
            .collect::<Vec<_>>();
        let mut arms = Vec::with_capacity(branch_count);
        let mut prior = Vec::with_capacity(branch_count);
        for ordinal in 0..branch_count {
            let branch = self.dynamic_guarded_branches(&targets[0])[ordinal];
            let owner = Self::guarded_activation_owner(clock, targets[0].clock.is_some(), branch);
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
        let result_count = program.result_count;
        let dst_start = self.register_range(result_count, span)?;
        self.emit(solve::LinearOp::FunctionConditional {
            dst_start,
            capture_start: 0,
            program: std::sync::Arc::new(program),
        })?;
        self.emit(solve::LinearOp::StoreOutputRange {
            start: dst_start,
            count: result_count,
            stride: 1,
        })?;
        self.finish_operations()
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

    /// A periodic clock ticks once per instant, so its partition's rows are
    /// active at the level of their guard. An unclocked `when` and an MLS
    /// §16.3 event clock activate on the rise of their trigger instead: an
    /// event clock ticks when `edge(pre(condition))` becomes true, and a
    /// condition that stays true through later iterations or events does not
    /// tick it again.
    const fn guarded_activation_owner(
        clock: Option<dae::ClockId<'dae>>,
        periodic: bool,
        branch: super::super::events::GuardedAssignment<'dae>,
    ) -> ActivationCondition<'dae> {
        let (trigger, guard, _, trigger_memory) = branch;
        ActivationCondition::GuardedAssignment {
            clock,
            periodic,
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
        compiler.call_use = self.call_use;
        compiler.active_clock = clock;
        compiler.derivative_definitions = self.derivative_definitions;
        compiler.affine_derivative_systems = self.affine_derivative_systems;
        compiler.active_derivatives = self.active_derivatives.clone();
        compiler.derivative_seeds = self.derivative_seeds.clone();
        compiler.same_tick_definitions = self.same_tick_definitions.clone();
        compiler.function_conditional_owners = self.function_conditional_owners;
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
        compiler.emit(solve::LinearOp::StoreOutput { src: condition })?;
        compiler.finish_operations()
    }

    fn guarded_assignment_result_region(
        &self,
        targets: &[super::super::events::GuardedTarget<'dae>],
        clock: Option<dae::ClockId<'dae>>,
        ordinal: usize,
        path: &[(ActivationCondition<'dae>, bool)],
    ) -> Result<Vec<solve::LinearOp>, LowerError> {
        let mut compiler = self.guarded_region_compiler(clock, path);
        compiler.project_current_definitions = compiler.same_tick_definitions.is_some();
        compiler.enter_context(ScalarContextFrame::CurrentDefinitions {
            parent: compiler.context_id,
        });
        for target in targets {
            let branch = compiler.dynamic_guarded_branches(target)[ordinal];
            compiler.sampled_source = target.sampled;
            compiler.store_guarded_assignment_value(branch.2, target.width, target.span)?;
        }
        compiler.finish_operations()
    }

    fn guarded_assignment_fallback_region(
        &self,
        targets: &[super::super::events::GuardedTarget<'dae>],
        clock: Option<dae::ClockId<'dae>>,
        path: &[(ActivationCondition<'dae>, bool)],
    ) -> Result<Vec<solve::LinearOp>, LowerError> {
        let mut compiler = self.guarded_region_compiler(clock, path);
        compiler.store_guarded_assignment_fallbacks(targets)?;
        compiler.finish_operations()
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
                self.emit(solve::LinearOp::StoreOutputRange {
                    start,
                    count: target.width,
                    stride: 1,
                })?;
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
            owner @ ActivationCondition::GuardedAssignment { .. }
                if self.project_current_definitions =>
            {
                self.with_entry_definition_reads(owner, span)
            }
            ActivationCondition::GuardedAssignment {
                clock,
                periodic,
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
                if periodic {
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
        if !self.project_current_definitions && self.same_tick_definitions.is_some() {
            self.project_current_definitions = true;
            self.enter_context(ScalarContextFrame::CurrentDefinitions {
                parent: self.context_id,
            });
            let result = self.store_guarded_assignment_value(value, width, span);
            self.leave_context();
            self.project_current_definitions = false;
            return result;
        }
        if self.node(value).value_type().scalar_count() != Some(width) {
            return Err(LowerError::contract(
                "guarded-assignment value width differs from its target range",
                span,
            ));
        }
        let start = self.pack_expression(value)?;
        self.emit(solve::LinearOp::StoreOutputRange {
            start,
            count: width,
            stride: 1,
        })?;
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
        let dst_start = self.register_range(target.width, target.span)?;
        self.emit(solve::LinearOp::TensorLoad {
            dst_start,
            input,
            input_start,
            count: target.width,
            seed_start: None,
            lanes: 1,
        })?;
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
                self.activated_relation_expression(expression, span)
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

/// The registers of a typed call's error-level predicates, whose results start
/// at `start` in the call site's output order.
fn error_predicate_registers(
    start: solve::Reg,
    registered: &crate::lower::typed_functions::RegisteredCall<'_>,
    span: Span,
) -> Result<Vec<solve::Reg>, LowerError> {
    let outputs = registered.site.outputs();
    registered
        .callee
        .assertions
        .iter()
        .filter(|assertion| assertion.level == dae::AssertionLevel::Error)
        .map(|assertion| {
            let offset = outputs
                .get(..assertion.predicate_output)
                .ok_or_else(|| LowerError::contract("typed call lost its issued predicate", span))?
                .iter()
                .try_fold(0usize, |offset, output| {
                    offset.checked_add(output.value_type().scalar_count() as usize)
                })
                .ok_or_else(|| {
                    LowerError::contract("typed call predicate offset overflows", span)
                })?;
            super::functions::function_conditional_reg_offset(
                start,
                offset,
                span,
                "typed pure-call predicate",
            )
        })
        .collect()
}
