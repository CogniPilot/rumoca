mod arrays;
mod builtins;
mod call_scoped_actions;
mod conditions;
mod constants;
mod coordinates;
mod functions;
mod operators;
mod selector;

use std::cell::RefCell;
use std::collections::HashSet;
use std::sync::Arc;

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
        condition: ActivationCondition<'dae>,
        expected: bool,
    },
    Function {
        parent: u64,
        call: dae::ExprId<'dae>,
        function: dae::FunctionId<'dae>,
        arguments: Vec<dae::ExprId<'dae>>,
    },
    Domain {
        parent: u64,
        domain: dae::DomainId<'dae>,
        values: Vec<i64>,
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

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum ActivationCondition<'dae> {
    Expression(dae::ExprId<'dae>),
    GuardedAssignment {
        clock: Option<dae::ClockId<'dae>>,
        trigger: dae::ConditionId<'dae>,
        guard: dae::ConditionId<'dae>,
        trigger_memory: usize,
    },
}

#[derive(Clone, Copy, PartialEq, Eq)]
struct ActivationGuard<'dae> {
    condition: ActivationCondition<'dae>,
    register: Option<solve::Reg>,
    expected: bool,
}

#[derive(Clone, PartialEq, Eq, Hash)]
struct FunctionFoldCacheDependency<'dae> {
    fold: dae::FunctionFoldId<'dae>,
    values: Vec<Vec<solve::Reg>>,
    domain_point: Vec<i64>,
    domain_registers: Vec<solve::Reg>,
}

#[derive(Clone, PartialEq, Eq, Hash)]
struct FunctionFoldOutputCacheKey<'dae> {
    function_context: u64,
    fold: dae::FunctionFoldId<'dae>,
    dependencies: Vec<FunctionFoldCacheDependency<'dae>>,
}

#[derive(Clone, PartialEq, Eq, Hash)]
struct ActiveCallAssertion<'dae> {
    call: dae::ExprId<'dae>,
    function: dae::FunctionId<'dae>,
    arguments: Vec<dae::ExprId<'dae>>,
}

#[derive(Clone, PartialEq, Eq, Hash)]
struct FunctionArgumentsFrame<'dae> {
    call: dae::ExprId<'dae>,
    function: dae::FunctionId<'dae>,
    arguments: Vec<dae::ExprId<'dae>>,
    activation_base: usize,
}

#[derive(Clone, PartialEq, Eq)]
pub(in crate::lower) struct DeferredCallAssertion<'dae> {
    condition: dae::ExprId<'dae>,
    fold: Option<dae::FunctionFoldId<'dae>>,
    domain_points: Vec<(dae::DomainId<'dae>, Vec<i64>)>,
    symbolic_domain_points: Vec<(dae::DomainId<'dae>, Vec<solve::Reg>)>,
    function_arguments: Vec<FunctionArgumentsFrame<'dae>>,
    activation_path: Vec<ActivationGuard<'dae>>,
    active_clock: dae::ClockId<'dae>,
    sampled_source: bool,
    active_parameters: Vec<u32>,
    active_call_assertions: HashSet<ActiveCallAssertion<'dae>>,
}

impl<'dae> DeferredCallAssertion<'dae> {
    pub(in crate::lower) const fn active_clock(&self) -> dae::ClockId<'dae> {
        self.active_clock
    }

    pub(in crate::lower) fn same_specialization(&self, other: &Self) -> bool {
        self.condition == other.condition
            && self.fold == other.fold
            && self.domain_points == other.domain_points
            && self
                .symbolic_domain_points
                .iter()
                .map(|(domain, registers)| (*domain, registers.len()))
                .eq(other
                    .symbolic_domain_points
                    .iter()
                    .map(|(domain, registers)| (*domain, registers.len())))
            && self.function_arguments == other.function_arguments
            && self
                .activation_path
                .iter()
                .map(|guard| (guard.condition, guard.expected))
                .eq(other
                    .activation_path
                    .iter()
                    .map(|guard| (guard.condition, guard.expected)))
            && self.active_clock == other.active_clock
            && self.sampled_source == other.sampled_source
    }
}

struct DeferredFoldCaptures<'dae> {
    fold_values: Vec<(dae::FunctionFoldId<'dae>, Vec<Vec<solve::Reg>>)>,
    symbolic_domain_points: Vec<(dae::DomainId<'dae>, Vec<solve::Reg>)>,
    packed_expressions: HashMap<dae::ExprId<'dae>, (solve::Reg, usize)>,
    packed_capture_ranges: HashMap<dae::ExprId<'dae>, usize>,
    sources: Vec<solve::Reg>,
    locals: HashMap<solve::Reg, solve::Reg>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum FunctionConditionalCaptureSource<'dae> {
    DefinitionRange {
        context: u64,
        definition: dae::FunctionDefinitionId<'dae>,
        count: usize,
    },
    DefinitionRecordFieldRange {
        context: u64,
        definition: dae::FunctionDefinitionId<'dae>,
        field: usize,
        count: usize,
    },
}

impl<'dae> FunctionConditionalCaptureSource<'dae> {
    const fn width(self) -> usize {
        match self {
            Self::DefinitionRange { count, .. }
            | Self::DefinitionRecordFieldRange { count, .. } => count,
        }
    }

    const fn with_context(self, context: u64) -> Self {
        match self {
            Self::DefinitionRange {
                definition, count, ..
            } => Self::DefinitionRange {
                context,
                definition,
                count,
            },
            Self::DefinitionRecordFieldRange {
                definition,
                field,
                count,
                ..
            } => Self::DefinitionRecordFieldRange {
                context,
                definition,
                field,
                count,
            },
        }
    }
}

struct DeferredFunctionConditionalCaptures<'dae> {
    owner_function: dae::FunctionId<'dae>,
    owner_context: u64,
    sources: Vec<FunctionConditionalCaptureSource<'dae>>,
    locals: Vec<(FunctionConditionalCaptureSource<'dae>, solve::Reg)>,
}

#[derive(Clone, PartialEq, Eq, Hash)]
enum FunctionConditionalOwnerValue<'dae> {
    Definitions(Vec<dae::FunctionDefinitionId<'dae>>),
    Expression(dae::ExprId<'dae>),
}

#[derive(Clone, PartialEq, Eq, Hash)]
struct FunctionConditionalOwnerKey<'dae> {
    value: FunctionConditionalOwnerValue<'dae>,
    function_arguments: Vec<FunctionArgumentsFrame<'dae>>,
    activation_path: Vec<(ActivationCondition<'dae>, bool)>,
    domain_points: Vec<(dae::DomainId<'dae>, Vec<i64>)>,
    active_clock: Option<dae::ClockId<'dae>>,
    sampled_source: bool,
    active_parameters: Vec<u32>,
}

#[derive(Clone)]
struct CachedFunctionConditionalProgram<'dae> {
    program: Arc<solve::FunctionConditionalProgram>,
    capture_sources: Vec<FunctionConditionalCaptureSource<'dae>>,
}

#[derive(Default)]
pub(super) struct FunctionConditionalOwnerRegistry<'dae> {
    owners: HashMap<FunctionConditionalOwnerKey<'dae>, CachedFunctionConditionalProgram<'dae>>,
    next_owner: u64,
}

impl FunctionConditionalOwnerRegistry<'_> {
    fn issue(&mut self) -> Option<solve::FunctionConditionalOwnerId> {
        self.next_owner = self.next_owner.checked_add(1)?;
        solve::FunctionConditionalOwnerId::checked(self.next_owner)
    }
}

struct CachedFunctionFoldProgram {
    program: Arc<solve::FunctionFoldProgram>,
    capture_sources: Vec<solve::Reg>,
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
    symbolic_domain_points: Vec<(dae::DomainId<'dae>, Vec<solve::Reg>)>,
    function_arguments: Vec<FunctionArgumentsFrame<'dae>>,
    function_fold_values: Vec<(dae::FunctionFoldId<'dae>, Vec<Vec<solve::Reg>>)>,
    deferred_fold_captures: Option<DeferredFoldCaptures<'dae>>,
    deferred_function_conditional_captures: Option<DeferredFunctionConditionalCaptures<'dae>>,
    function_conditional_owners: Option<&'layout RefCell<FunctionConditionalOwnerRegistry<'dae>>>,
    activation_path: Vec<ActivationGuard<'dae>>,
    /// Prefix of `activation_path` already enforced by the owning compact
    /// fold. Nested folds materialize only guards introduced after this point.
    fold_guard_base: usize,
    active_clock: Option<dae::ClockId<'dae>>,
    sampled_source: bool,
    derivative_definitions: Option<&'layout DerivativeRowIndex<'dae>>,
    active_derivatives: Vec<(u32, usize)>,
    parameter_substitutions: Option<&'layout ParameterBindingSubstitutions<'dae>>,
    active_parameters: Vec<u32>,
    ops: Vec<solve::LinearOp>,
    next_register: solve::Reg,
    integer_registers: Vec<Option<i64>>,
    expression_cache: rustc_hash::FxHashMap<(u64, dae::ExprId<'dae>, usize), solve::Reg>,
    packed_expression_cache: rustc_hash::FxHashMap<(u64, dae::ExprId<'dae>), solve::Reg>,
    typed_pure_call_cache: rustc_hash::FxHashMap<
        (u64, dae::ExprId<'dae>),
        (
            solve::Reg,
            crate::lower::typed_functions::RegisteredCall<'dae>,
        ),
    >,
    matrix_multiply_cache:
        HashMap<(u64, dae::ExprId<'dae>, dae::ExprId<'dae>), (solve::Reg, usize)>,
    tensor_binary_cache:
        HashMap<(u64, solve::BinaryOp, dae::ExprId<'dae>, dae::ExprId<'dae>), (solve::Reg, usize)>,
    tensor_transpose_cache: HashMap<(u64, dae::ExprId<'dae>), (solve::Reg, usize)>,
    tensor_concatenate_cache: HashMap<(u64, dae::ExprId<'dae>), (solve::Reg, usize)>,
    tensor_update_cache: HashMap<(u64, dae::ExprId<'dae>), (solve::Reg, usize)>,
    tensor_generate_cache: HashMap<(u64, dae::ExprId<'dae>), (solve::Reg, usize)>,
    tensor_load_cache: HashMap<(u64, dae::ExprId<'dae>), (solve::Reg, usize)>,
    fold_register_pack_cache: HashMap<Vec<solve::Reg>, solve::Reg>,
    record_field_cache: HashMap<(u64, dae::ExprId<'dae>, usize), solve::Reg>,
    function_definition_scalar_cache:
        HashMap<(u64, dae::FunctionDefinitionId<'dae>, usize), solve::Reg>,
    function_definition_aggregate_cache:
        HashMap<(u64, dae::FunctionDefinitionId<'dae>), solve::Reg>,
    function_fold_output_cache: HashMap<FunctionFoldOutputCacheKey<'dae>, Vec<Vec<solve::Reg>>>,
    function_fold_program_cache: HashMap<
        (
            dae::FunctionFoldId<'dae>,
            Option<dae::FunctionFoldId<'dae>>,
            Vec<usize>,
        ),
        CachedFunctionFoldProgram,
    >,
    active_call_assertions: HashSet<ActiveCallAssertion<'dae>>,
    call_action_compilation: bool,
    suppress_function_assertions: bool,
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
            symbolic_domain_points: Vec::new(),
            function_arguments: Vec::new(),
            function_fold_values: Vec::new(),
            deferred_fold_captures: None,
            deferred_function_conditional_captures: None,
            function_conditional_owners: None,
            activation_path: Vec::new(),
            fold_guard_base: 0,
            active_clock: None,
            sampled_source: false,
            derivative_definitions: None,
            active_derivatives: Vec::new(),
            parameter_substitutions: None,
            active_parameters: Vec::new(),
            ops: Vec::new(),
            next_register: 0,
            integer_registers: Vec::new(),
            expression_cache: rustc_hash::FxHashMap::default(),
            packed_expression_cache: rustc_hash::FxHashMap::default(),
            typed_pure_call_cache: rustc_hash::FxHashMap::default(),
            matrix_multiply_cache: HashMap::new(),
            tensor_binary_cache: HashMap::new(),
            tensor_transpose_cache: HashMap::new(),
            tensor_concatenate_cache: HashMap::new(),
            tensor_update_cache: HashMap::new(),
            tensor_generate_cache: HashMap::new(),
            tensor_load_cache: HashMap::new(),
            fold_register_pack_cache: HashMap::new(),
            record_field_cache: HashMap::new(),
            function_definition_scalar_cache: HashMap::new(),
            function_definition_aggregate_cache: HashMap::new(),
            function_fold_output_cache: HashMap::new(),
            function_fold_program_cache: HashMap::new(),
            active_call_assertions: HashSet::new(),
            call_action_compilation: false,
            suppress_function_assertions: false,
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

    fn push_activation(
        &mut self,
        condition: dae::ExprId<'dae>,
        register: solve::Reg,
        expected: bool,
    ) {
        self.push_activation_owner(
            ActivationCondition::Expression(condition),
            register,
            expected,
        );
    }

    fn push_activation_owner(
        &mut self,
        condition: ActivationCondition<'dae>,
        register: solve::Reg,
        expected: bool,
    ) {
        self.enter_context(ScalarContextFrame::Activation {
            parent: self.context_id,
            condition,
            expected,
        });
        self.activation_path.push(ActivationGuard {
            condition,
            register: Some(register),
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
                    .unwrap_or_default();
                let domain_registers = self
                    .symbolic_domain_points
                    .iter()
                    .rev()
                    .find_map(|(candidate, point)| (*candidate == domain).then(|| point.clone()))
                    .unwrap_or_default();
                FunctionFoldCacheDependency {
                    fold: *active,
                    values: values.clone(),
                    domain_point,
                    domain_registers,
                }
            })
            .collect();
        FunctionFoldOutputCacheKey {
            function_context: self.owning_function_context(fold.function()),
            fold,
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

    pub(super) const fn with_function_conditional_owners(
        mut self,
        owners: &'layout RefCell<FunctionConditionalOwnerRegistry<'dae>>,
    ) -> Self {
        self.function_conditional_owners = Some(owners);
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

    /// Compile several scalar projections into one source-owned program.
    ///
    /// Array and record equations are one semantic owner even when structural
    /// matching pairs each scalar residual independently. Keeping their
    /// projections in one compiler preserves shared function-call and
    /// expression ownership before Solve IR is emitted.
    pub(super) fn program_outputs(
        mut self,
        outputs: impl IntoIterator<Item = (dae::ExprId<'dae>, usize)>,
    ) -> Result<Vec<solve::LinearOp>, LowerError> {
        let profile =
            tracing::enabled!(target: "rumoca_phase_solve::profile::ir", tracing::Level::DEBUG);
        for (expression, scalar) in outputs {
            let before = self.ops.len();
            let output = self.expression(expression, scalar)?;
            self.ops.push(solve::LinearOp::StoreOutput { src: output });
            if profile && self.ops.len() - before >= 100 {
                tracing::debug!(
                    target: "rumoca_phase_solve::profile::ir",
                    "rumoca-program-output expr={expression:?} scalar={scalar} count={} kind={:?} emitted={}",
                    scalar_count(self.view, expression),
                    self.node(expression).kind(),
                    self.ops.len() - before,
                );
            }
        }
        Ok(self.ops)
    }

    /// Compile complete aggregate expressions before projecting their scalar
    /// outputs. This is the continuous-equation counterpart of the clocked
    /// aggregate entry points below.
    pub(super) fn aggregate_program(
        mut self,
        expressions: impl IntoIterator<Item = dae::ExprId<'dae>>,
    ) -> Result<Vec<solve::LinearOp>, LowerError> {
        self.aggregate_program_outputs(expressions)
    }

    /// Compile one aggregate expression as one source-owned multi-output
    /// program. Aggregate lowering runs before final scalar output projection,
    /// so calls, folds, and tensor kernels are constructed exactly once.
    pub(super) fn clocked_aggregate_program(
        mut self,
        clock: dae::ClockId<'dae>,
        expression: dae::ExprId<'dae>,
    ) -> Result<Vec<solve::LinearOp>, LowerError> {
        self.active_clock = Some(clock);
        self.aggregate_program_outputs([expression])
    }

    pub(super) fn sampled_aggregate_program(
        mut self,
        clock: dae::ClockId<'dae>,
        expression: dae::ExprId<'dae>,
    ) -> Result<Vec<solve::LinearOp>, LowerError> {
        self.active_clock = Some(clock);
        self.sampled_source = true;
        self.aggregate_program_outputs([expression])
    }

    pub(super) fn clocked_aggregate_programs(
        mut self,
        clock: dae::ClockId<'dae>,
        expressions: impl IntoIterator<Item = dae::ExprId<'dae>>,
        sampled: bool,
    ) -> Result<Vec<solve::LinearOp>, LowerError> {
        self.active_clock = Some(clock);
        self.sampled_source = sampled;
        self.aggregate_program_outputs(expressions)
    }

    fn aggregate_program_outputs(
        &mut self,
        expressions: impl IntoIterator<Item = dae::ExprId<'dae>>,
    ) -> Result<Vec<solve::LinearOp>, LowerError> {
        for expression in expressions {
            let before = self.ops.len();
            let start = self.pack_expression(expression)?;
            let count = scalar_count(self.view, expression);
            if count == 1 {
                self.ops.push(solve::LinearOp::StoreOutput { src: start });
            } else {
                self.ops.push(solve::LinearOp::StoreOutputRange {
                    start,
                    count,
                    stride: 1,
                });
            }
            if tracing::enabled!(target: "rumoca_phase_solve::profile::ir", tracing::Level::DEBUG)
                && self.ops.len() - before >= 100
            {
                tracing::debug!(
                    target: "rumoca_phase_solve::profile::ir",
                    "rumoca-aggregate-output expr={expression:?} count={count} kind={:?} emitted={}",
                    self.node(expression).kind(),
                    self.ops.len() - before,
                );
            }
        }
        Ok(std::mem::take(&mut self.ops))
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

    pub(super) fn pack_expression(
        &mut self,
        expression: dae::ExprId<'dae>,
    ) -> Result<solve::Reg, LowerError> {
        let key = (self.context_id, expression);
        if let Some(&start) = self.packed_expression_cache.get(&key) {
            return Ok(start);
        }
        if let Some((source, count)) = self
            .deferred_fold_captures
            .as_ref()
            .and_then(|captures| captures.packed_expressions.get(&expression))
            .copied()
        {
            let span = self.node(expression).provenance().span();
            let mut values = Vec::with_capacity(count);
            for offset in 0..count {
                let offset = u32::try_from(offset).map_err(|_| {
                    LowerError::contract("fold tensor capture offset exceeds u32", span)
                })?;
                let source = source.checked_add(offset).ok_or_else(|| {
                    LowerError::contract("fold tensor capture register overflows", span)
                })?;
                values.push(self.deferred_fold_capture(source, span)?);
            }
            let start = self.pack_registers(&values, span)?;
            self.packed_expression_cache.insert(key, start);
            return Ok(start);
        }
        let start = self.pack_expression_uncached(expression)?;
        self.packed_expression_cache.insert(key, start);
        Ok(start)
    }

    fn pack_expression_uncached(
        &mut self,
        expression: dae::ExprId<'dae>,
    ) -> Result<solve::Reg, LowerError> {
        let span = self.node(expression).provenance().span();
        match self.node(expression).operation() {
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::FunctionParameter(
                parameter,
            )) => return self.pack_function_parameter(parameter, span),
            dae::ExpressionOperation::Coordinate(coordinate) => {
                if let Some(start) = self.pack_coordinate(expression, coordinate, span)? {
                    return Ok(start);
                }
            }
            dae::ExpressionOperation::FunctionValue { definition, .. } => {
                return self.pack_function_definition(definition, span);
            }
            dae::ExpressionOperation::Call {
                function,
                output,
                arguments,
                ..
            } => return self.pack_function_call(expression, function, output, arguments, span),
            dae::ExpressionOperation::Conditional(operands)
                if !self.node(expression).value_type().is_record()
                    && self.node(expression).function_scope().is_some() =>
            {
                return self.pack_lazy_conditional_value(expression, operands, span);
            }
            dae::ExpressionOperation::Field { base, field } => {
                return self.pack_record_field(base, field as usize, span);
            }
            dae::ExpressionOperation::ArrayUpdate {
                base,
                value,
                subscripts,
            } if !self.node(expression).value_type().is_record() => {
                return self.pack_array_update(expression, base, value, subscripts, span);
            }
            dae::ExpressionOperation::Builtin { builtin, arguments } => {
                if matches!(
                    builtin,
                    dae::PureBuiltin::Zeros
                        | dae::PureBuiltin::Ones
                        | dae::PureBuiltin::Fill
                        | dae::PureBuiltin::Identity
                ) {
                    return self.pack_tensor_generator(expression, builtin, arguments, span);
                }
                if builtin == dae::PureBuiltin::Transpose {
                    return self.pack_transpose(expression, arguments, span);
                }
                if builtin == dae::PureBuiltin::Cross {
                    return self.pack_cross(arguments, span);
                }
                if matches!(
                    builtin,
                    dae::PureBuiltin::PromotedCat1 | dae::PureBuiltin::PromotedCat2
                ) {
                    return self.pack_promoted_concatenation(expression, builtin, arguments, span);
                }
                let alias = match builtin {
                    dae::PureBuiltin::Smooth => arguments.get(1),
                    dae::PureBuiltin::NoEvent | dae::PureBuiltin::Vector => arguments.get(0),
                    _ => None,
                };
                if let Some(alias) = alias {
                    return self.pack_expression(alias);
                }
            }
            _ => {}
        }
        let count = scalar_count(self.view, expression);
        let before = self.ops.len();
        let mut values = Vec::with_capacity(count);
        for scalar in 0..count {
            values.push(self.expression(expression, scalar)?);
        }
        if tracing::enabled!(target: "rumoca_phase_solve::profile::fold", tracing::Level::DEBUG)
            && count >= 10
            && values
                .iter()
                .copied()
                .enumerate()
                .any(|(offset, register)| {
                    values.first().copied().and_then(|start| {
                        u32::try_from(offset)
                            .ok()
                            .and_then(|offset| start.checked_add(offset))
                    }) != Some(register)
                })
        {
            let detail = match self.node(expression).operation() {
                dae::ExpressionOperation::Call { function, .. } => self
                    .view
                    .function(function)
                    .map(|function| function.name().to_string())
                    .unwrap_or_else(|| "<missing function>".to_string()),
                dae::ExpressionOperation::Builtin { builtin, .. } => format!("{builtin:?}"),
                _ => format!("{:?}", self.node(expression).kind()),
            };
            tracing::debug!(
                target: "rumoca_phase_solve::profile::ir",
                "rumoca-pack-profile count={count} emitted={} owner={detail}",
                self.ops.len() - before,
            );
        }
        self.pack_registers(&values, span)
    }

    fn pack_registers(
        &mut self,
        values: &[solve::Reg],
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        if let Some(&start) = values.first()
            && values
                .iter()
                .copied()
                .enumerate()
                .all(|(offset, register)| {
                    u32::try_from(offset)
                        .ok()
                        .and_then(|offset| start.checked_add(offset))
                        == Some(register)
                })
        {
            return Ok(start);
        }
        let start = self.next_register;
        for &value in values {
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
        if let Some((source, count)) = self
            .deferred_fold_captures
            .as_ref()
            .and_then(|captures| captures.packed_expressions.get(&expression))
            .copied()
            && scalar < count
        {
            let offset = u32::try_from(scalar).map_err(|_| {
                LowerError::contract(
                    "fold expression capture offset exceeds u32",
                    self.node(expression).provenance().span(),
                )
            })?;
            let source = source.checked_add(offset).ok_or_else(|| {
                LowerError::contract(
                    "fold expression capture register overflows",
                    self.node(expression).provenance().span(),
                )
            })?;
            let result =
                self.deferred_fold_capture(source, self.node(expression).provenance().span())?;
            self.expression_cache.insert(key, result);
            return Ok(result);
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
                ..
            } => self.function_call(
                expression,
                function,
                output,
                arguments,
                scalar,
                node.provenance().span(),
            ),
            dae::ExpressionOperation::FunctionValue { definition, .. } => {
                self.function_definition_value(definition, scalar, node.provenance().span())
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
                | ScalarContextFrame::Parameter { parent, .. }
                | ScalarContextFrame::Derivative { parent, .. } => context = *parent,
            }
        }
    }

    fn current_function_frame_matches(&self, call: &ActiveCallAssertion<'dae>) -> bool {
        self.function_arguments.last().is_some_and(|frame| {
            frame.call == call.call
                && frame.function == call.function
                && frame.arguments == call.arguments
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
                if reason == "array subscript is not compile-time computable"
                    || reason == "binder-valued subscript has no active domain" =>
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
