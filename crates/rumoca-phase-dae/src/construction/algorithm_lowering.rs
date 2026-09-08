use super::conditions::{algorithm_condition_owner_clock, lower_algorithm_condition};
use super::*;

#[derive(Clone, Copy)]
pub(super) struct AlgorithmBaseEnvironment<'scope, 'shape, 'dae> {
    pub(super) coordinates: &'scope ModelCoordinates<'dae>,
    pub(super) functions: &'scope FunctionRegistry<'shape, 'dae>,
    pub(super) sample_lattices: &'scope [(Span, PeriodicClockSchedule)],
}

#[derive(Clone, Copy)]
struct EventAlgorithmEnvironment<'scope, 'shape, 'dae> {
    base: AlgorithmBaseEnvironment<'scope, 'shape, 'dae>,
    transaction: EventTransactionSink<'scope, 'dae>,
}

#[derive(Clone, Copy)]
enum EventTransactionSink<'scope, 'dae> {
    NoTargets,
    Steps(&'scope RefCell<Vec<dae::ModelEventStep<'dae>>>),
}

enum EventTransactionBuffer<'dae> {
    NoTargets,
    Steps {
        targets: Vec<VarName>,
        steps: RefCell<Vec<dae::ModelEventStep<'dae>>>,
    },
}

impl<'dae> EventTransactionBuffer<'dae> {
    fn issued(product: &EventLoweringProduct<'_>) -> Self {
        if product.targets().is_empty() {
            Self::NoTargets
        } else {
            Self::Steps {
                targets: product.targets().to_vec(),
                steps: RefCell::new(Vec::new()),
            }
        }
    }

    fn sink(&self) -> EventTransactionSink<'_, 'dae> {
        match self {
            Self::NoTargets => EventTransactionSink::NoTargets,
            Self::Steps { steps, .. } => EventTransactionSink::Steps(steps),
        }
    }
}

#[derive(Clone, Copy)]
struct AlgorithmOwner<'dae> {
    discrete_owner: Option<DiscreteValueOwnerHandle>,
    /// The enclosing `if`/`when` branch activation, or `None` at the section's
    /// own statement level.
    ///
    /// This stays optional because it answers a question only a *branch* can
    /// answer: whether a condition written here composes with an enclosing one
    /// or is itself the event trigger. It is not the activation a statement
    /// executes under — [`AlgorithmOwner::activation`] is.
    parent: Option<EventGuard<'dae>>,
    /// The section's unconditional activation, issued once for the whole
    /// algorithm section before any statement is lowered.
    unconditional: EventGuard<'dae>,
    span: Span,
}

impl<'dae> AlgorithmOwner<'dae> {
    /// The activation every statement beneath this owner executes under.
    ///
    /// MLS §11.1 runs the statements of an algorithm section that are not
    /// inside a `when` every time the section runs, so a statement written
    /// outside every branch does not lack an activation — its activation is
    /// `true`. Reading it through this one accessor is what keeps the discrete
    /// value definition, the model-event transaction step, and the assertion
    /// guard of the same statement on the same condition.
    fn activation(&self) -> EventGuard<'dae> {
        self.parent.unwrap_or(self.unconditional)
    }
}

/// Issue the unconditional activation of one model algorithm section.
///
/// One section owns exactly one such activation, minted before its first
/// statement, so every consumer of a top-level statement borrows the same
/// checked `Always` condition instead of minting a private one per statement.
fn unconditional_algorithm_activation<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    span: Span,
) -> Result<EventGuard<'dae>, dae::DaeConstructionError> {
    let always = always_condition(construction, span)?;
    Ok(EventGuard {
        trigger: always,
        condition: always,
        owner_clock: None,
        branch_provenance: dae::DaeProvenance::generated(
            dae::DaeGeneration::AlgorithmEquation,
            span,
        )?,
        always: true,
        parent_activation: None,
    })
}

pub(super) struct ModelAlgorithmsRequest<'scope, 'flat, 'shape, 'dae> {
    pub(super) environment: AlgorithmBaseEnvironment<'scope, 'shape, 'dae>,
    pub(super) algorithms: &'scope ModelAlgorithmSequence<'flat>,
    pub(super) topology: &'scope DiscreteValueTopologyPlan,
}

pub(super) fn lower_algorithms<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    discrete_values: &mut DiscreteValueStaging<'dae>,
    request: ModelAlgorithmsRequest<'_, '_, '_, 'dae>,
) -> Result<(), dae::DaeConstructionError> {
    // Claim every sampled algorithm coordinate before lowering any body. A
    // consumer is allowed to precede its producer in Flat order; the unique
    // clock owner is an analysis fact, not an artifact of lowering order.
    for (_, plan) in request.algorithms.entries() {
        if let ModelAlgorithmPlan::Event(product) = plan {
            preclaim_algorithm_clock_targets(
                construction,
                request.environment,
                product.statements(),
                None,
            )?;
        }
    }
    for (algorithm, plan) in request.algorithms.entries() {
        lower_model_algorithm_entry(construction, discrete_values, &request, algorithm, plan)?;
    }
    Ok(())
}

fn lower_model_algorithm_entry<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    discrete_values: &mut DiscreteValueStaging<'dae>,
    request: &ModelAlgorithmsRequest<'_, '_, '_, 'dae>,
    algorithm: &flat::Algorithm,
    plan: &ModelAlgorithmPlan<'_>,
) -> Result<(), dae::DaeConstructionError> {
    let owner_provenance =
        dae::DaeProvenance::generated(dae::DaeGeneration::AlgorithmEquation, algorithm.span)?;
    let targets = match plan {
        ModelAlgorithmPlan::Event(product) => product.targets().to_vec(),
        _ => model_algorithm_targets(request.algorithms.flat(), algorithm),
    };
    let discrete_owner = discrete_values.owner(
        owner_provenance,
        targets.clone(),
        request.environment.coordinates,
        request.topology,
    )?;
    let mut lowering = ModelAlgorithmLowering {
        construction,
        discrete_values,
        discrete_owner,
        coordinates: request.environment.coordinates,
        functions: request.environment.functions,
    };
    match plan {
        ModelAlgorithmPlan::Declarative { target } => {
            lower_declarative_model_algorithm(&mut lowering, algorithm, target)?;
        }
        ModelAlgorithmPlan::TotalArrayDefinition {
            target,
            domain,
            binder_spans,
        } => {
            lower_total_array_model_algorithm(
                &mut lowering,
                algorithm,
                target,
                domain,
                binder_spans,
            )?;
        }
        ModelAlgorithmPlan::SeparatedArraySum {
            array_target,
            scalar_target,
            domain,
            binder_spans,
        } => {
            lower_separated_array_sum_model_algorithm(
                &mut lowering,
                algorithm,
                array_target,
                scalar_target,
                domain,
                binder_spans,
            )?;
        }
        ModelAlgorithmPlan::Event(product) => lower_event_algorithm(
            &mut lowering,
            request.environment,
            algorithm,
            product,
            owner_provenance,
        )?,
    }
    Ok(())
}

fn lower_event_algorithm<'dae>(
    lowering: &mut ModelAlgorithmLowering<'_, '_, 'dae>,
    base: AlgorithmBaseEnvironment<'_, '_, 'dae>,
    algorithm: &flat::Algorithm,
    product: &EventLoweringProduct<'_>,
    owner_provenance: dae::DaeProvenance,
) -> Result<(), dae::DaeConstructionError> {
    let mut values = seed_event_algorithm_values(
        lowering.construction,
        base.coordinates,
        product.targets().iter().cloned(),
        algorithm.span,
    )?;
    let transaction = EventTransactionBuffer::issued(product);
    let environment = EventAlgorithmEnvironment {
        base,
        transaction: transaction.sink(),
    };
    let unconditional = unconditional_algorithm_activation(lowering.construction, algorithm.span)?;
    lower_algorithm_statements(
        lowering.construction,
        lowering.discrete_values,
        environment,
        AlgorithmOwner {
            discrete_owner: lowering.discrete_owner,
            parent: None,
            unconditional,
            span: algorithm.span,
        },
        &mut values,
        product.statements(),
    )?;
    finish_event_transaction(
        lowering.construction,
        base.coordinates,
        transaction,
        algorithm.span,
        owner_provenance,
    )
}

fn finish_event_transaction<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &ModelCoordinates<'dae>,
    transaction: EventTransactionBuffer<'dae>,
    span: Span,
    provenance: dae::DaeProvenance,
) -> Result<(), dae::DaeConstructionError> {
    match transaction {
        EventTransactionBuffer::NoTargets => Ok(()),
        EventTransactionBuffer::Steps { targets, steps } => {
            let transaction_targets = targets
                .into_iter()
                .map(|target| model_event_target(coordinates, &target, span))
                .collect::<Result<Vec<_>, _>>()?;
            construction.model_events(|events| {
                events.transaction(transaction_targets, steps.into_inner(), provenance)
            })?;
            Ok(())
        }
    }
}

fn seed_event_algorithm_values<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &ModelCoordinates<'dae>,
    targets: impl IntoIterator<Item = VarName>,
    span: Span,
) -> Result<HashMap<VarName, dae::ExprId<'dae>>, dae::DaeConstructionError> {
    let provenance = dae::DaeProvenance::generated(dae::DaeGeneration::AlgorithmEquation, span)?;
    let mut values = HashMap::new();
    for target in targets {
        let coordinate = coordinates.event(&target, span)?.previous();
        let value = construction
            .expressions(|expressions| expressions.at(provenance).coordinate(coordinate))?;
        values.insert(target, value);
    }
    Ok(values)
}

fn preclaim_algorithm_clock_targets<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    environment: AlgorithmBaseEnvironment<'_, '_, 'dae>,
    statements: &[EventStatementPlan<'_>],
    inherited: Option<dae::PeriodicClockId<'dae>>,
) -> Result<(), dae::DaeConstructionError> {
    if let Some(clock) = inherited {
        return own_clocked_algorithm_targets(
            construction,
            environment.coordinates,
            clock.into(),
            statements,
        );
    }
    for statement in statements {
        match statement {
            EventStatementPlan::When { blocks, .. } => {
                for block in blocks {
                    let clock =
                        algorithm_condition_owner_clock(environment.functions, &block.condition)?;
                    preclaim_algorithm_clock_targets(
                        construction,
                        environment,
                        &block.statements,
                        clock,
                    )?;
                }
            }
            EventStatementPlan::If {
                blocks,
                else_product,
                ..
            } => {
                for block in blocks {
                    let clock =
                        algorithm_condition_owner_clock(environment.functions, &block.condition)?;
                    preclaim_algorithm_clock_targets(
                        construction,
                        environment,
                        &block.statements,
                        clock,
                    )?;
                }
                if let EventElseProduct::Statements(statements) = else_product {
                    preclaim_algorithm_clock_targets(construction, environment, statements, None)?;
                }
            }
            _ => {}
        }
    }
    Ok(())
}

fn lower_algorithm_statements<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    discrete_values: &mut DiscreteValueStaging<'dae>,
    environment: EventAlgorithmEnvironment<'_, '_, 'dae>,
    owner: AlgorithmOwner<'dae>,
    values: &mut HashMap<VarName, dae::ExprId<'dae>>,
    statements: &[EventStatementPlan<'_>],
) -> Result<(), dae::DaeConstructionError> {
    for statement in statements {
        lower_algorithm_statement(
            construction,
            discrete_values,
            environment,
            owner,
            values,
            statement,
        )?;
    }
    Ok(())
}

fn lower_algorithm_statement<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    discrete_values: &mut DiscreteValueStaging<'dae>,
    environment: EventAlgorithmEnvironment<'_, '_, 'dae>,
    owner: AlgorithmOwner<'dae>,
    values: &mut HashMap<VarName, dae::ExprId<'dae>>,
    statement: &EventStatementPlan<'_>,
) -> Result<(), dae::DaeConstructionError> {
    match statement {
        EventStatementPlan::Assignment {
            component,
            value,
            span,
            route,
        } => lower_algorithm_assignment_statement(
            construction,
            discrete_values,
            environment,
            owner,
            values,
            AlgorithmAssignment {
                comp: component,
                value,
                span: *span,
                route,
            },
        ),
        EventStatementPlan::If {
            blocks,
            else_product,
            span,
        } => lower_algorithm_if(
            construction,
            discrete_values,
            environment,
            AlgorithmIfInput {
                owner,
                values,
                blocks,
                else_product,
                span: *span,
            },
        ),
        EventStatementPlan::When { blocks, span } => lower_algorithm_when(
            construction,
            discrete_values,
            environment,
            owner,
            values,
            blocks,
            *span,
        ),
        EventStatementPlan::FunctionCall {
            component,
            arguments,
            span,
            plan,
        } => lower_planned_function_call(
            construction,
            discrete_values,
            environment,
            owner,
            values,
            AlgorithmFunctionCall {
                component,
                arguments,
                span: *span,
                plan,
            },
        ),
        EventStatementPlan::TensorLoop(plan) => lower_planned_tensor_loop(
            construction,
            discrete_values,
            environment,
            owner,
            values,
            plan,
        ),
        EventStatementPlan::Assert {
            condition,
            message,
            level,
            span,
        } => lower_algorithm_assertion(
            construction,
            environment,
            owner,
            condition,
            message,
            level.as_deref(),
            *span,
        ),
    }
}

fn lower_planned_function_call<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    discrete_values: &mut DiscreteValueStaging<'dae>,
    environment: EventAlgorithmEnvironment<'_, '_, 'dae>,
    owner: AlgorithmOwner<'dae>,
    values: &mut HashMap<VarName, dae::ExprId<'dae>>,
    call: AlgorithmFunctionCall<'_>,
) -> Result<(), dae::DaeConstructionError> {
    let span = call.span;
    let context = algorithm_statement_context(environment, owner, values);
    let updates =
        lower_algorithm_call_statement(construction, discrete_values, owner, context, call)?;
    record_model_event_step(environment, owner, &updates, span)?;
    values.extend(updates);
    Ok(())
}

fn lower_planned_tensor_loop<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    discrete_values: &mut DiscreteValueStaging<'dae>,
    environment: EventAlgorithmEnvironment<'_, '_, 'dae>,
    owner: AlgorithmOwner<'dae>,
    values: &mut HashMap<VarName, dae::ExprId<'dae>>,
    plan: &ModelEventTensorLoopPlan<'_>,
) -> Result<(), dae::DaeConstructionError> {
    let context = algorithm_statement_context(environment, owner, values);
    let updates = lower_algorithm_for_statement(
        construction,
        discrete_values,
        owner.discrete_owner,
        context,
        plan,
    )?;
    record_model_event_step(environment, owner, &updates, plan.span)?;
    values.extend(updates);
    Ok(())
}

/// One algorithm assignment, as written in source.
struct AlgorithmAssignment<'a> {
    comp: &'a rumoca_core::ComponentReference,
    value: &'a Expression,
    span: rumoca_core::Span,
    route: &'a EventAssignmentRoute<'a>,
}

/// Lower one algorithm assignment, routing a direct call assignment through the
/// event-call path when construction issued a call plan for it.
fn lower_algorithm_assignment_statement<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    discrete_values: &mut DiscreteValueStaging<'dae>,
    environment: EventAlgorithmEnvironment<'_, '_, 'dae>,
    owner: AlgorithmOwner<'dae>,
    values: &mut HashMap<VarName, dae::ExprId<'dae>>,
    assignment: AlgorithmAssignment<'_>,
) -> Result<(), dae::DaeConstructionError> {
    let AlgorithmAssignment {
        comp,
        value,
        span,
        route,
    } = assignment;
    let context = algorithm_statement_context(environment, owner, values);
    let updates = match route {
        EventAssignmentRoute::FunctionCall {
            component,
            arguments,
            plan,
        } => lower_algorithm_call_statement(
            construction,
            discrete_values,
            owner,
            context,
            AlgorithmFunctionCall {
                component,
                arguments,
                span,
                plan,
            },
        )?,
        EventAssignmentRoute::Coordinate => lower_algorithm_assignment(
            construction,
            discrete_values,
            owner.discrete_owner,
            context,
            LoweredAlgorithmAssignment {
                component: comp,
                value,
                span,
                structured_plan: None,
            },
        )?,
        EventAssignmentRoute::Structured(plan) => lower_algorithm_assignment(
            construction,
            discrete_values,
            owner.discrete_owner,
            context,
            LoweredAlgorithmAssignment {
                component: comp,
                value,
                span,
                structured_plan: Some(plan),
            },
        )?,
    };
    record_model_event_step(environment, owner, &updates, span)?;
    values.extend(updates);
    Ok(())
}

fn model_event_target<'dae>(
    coordinates: &ModelCoordinates<'dae>,
    name: &VarName,
    span: Span,
) -> Result<dae::ModelEventTarget<'dae>, dae::DaeConstructionError> {
    Ok(coordinates.event(name, span)?.target())
}

fn record_model_event_step<'dae>(
    environment: EventAlgorithmEnvironment<'_, '_, 'dae>,
    owner: AlgorithmOwner<'dae>,
    updates: &[(VarName, dae::ExprId<'dae>)],
    span: Span,
) -> Result<(), dae::DaeConstructionError> {
    let steps = match (environment.transaction, updates.is_empty()) {
        (EventTransactionSink::NoTargets, true) | (EventTransactionSink::Steps(_), true) => {
            return Ok(());
        }
        (EventTransactionSink::NoTargets, false) => {
            return Err(dae::DaeConstructionError::InvalidExpressionForm { span });
        }
        (EventTransactionSink::Steps(steps), false) => steps,
    };
    let guard = owner.activation();
    let provenance = dae::DaeProvenance::source(span)?;
    let mut definitions = Vec::with_capacity(updates.len());
    for (target, value) in updates {
        let event_target = model_event_target(environment.base.coordinates, target, span)?;
        definitions.push(dae::ModelEventDefinition::new(
            event_target,
            *value,
            provenance,
        ));
    }
    steps.borrow_mut().push(dae::ModelEventStep::new(
        guard.trigger,
        guard.condition,
        guard.owner_clock.map(Into::into),
        definitions,
        provenance,
    ));
    Ok(())
}

fn algorithm_statement_context<'scope, 'shape, 'dae>(
    environment: EventAlgorithmEnvironment<'scope, 'shape, 'dae>,
    owner: AlgorithmOwner<'dae>,
    values: &'scope HashMap<VarName, dae::ExprId<'dae>>,
) -> AlgorithmStatementContext<'scope, 'shape, 'dae> {
    AlgorithmStatementContext {
        coordinates: environment.base.coordinates,
        functions: environment.base.functions,
        values,
        parent: Some(owner.activation()),
        owner_span: owner.span,
    }
}

fn lower_algorithm_call_statement<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    discrete_values: &mut DiscreteValueStaging<'dae>,
    owner: AlgorithmOwner<'dae>,
    context: AlgorithmStatementContext<'_, '_, 'dae>,
    call: AlgorithmFunctionCall<'_>,
) -> Result<Vec<(VarName, dae::ExprId<'dae>)>, dae::DaeConstructionError> {
    lower_algorithm_function_call(
        construction,
        discrete_values,
        owner.discrete_owner,
        context,
        call,
    )
}

fn lower_algorithm_for_statement<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    discrete_values: &mut DiscreteValueStaging<'dae>,
    discrete_owner: Option<DiscreteValueOwnerHandle>,
    context: AlgorithmStatementContext<'_, '_, 'dae>,
    plan: &ModelEventTensorLoopPlan<'_>,
) -> Result<Vec<(VarName, dae::ExprId<'dae>)>, dae::DaeConstructionError> {
    lower_algorithm_tensor_loop(construction, discrete_values, discrete_owner, context, plan)
}

fn lower_algorithm_assertion<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    environment: EventAlgorithmEnvironment<'_, '_, 'dae>,
    owner: AlgorithmOwner<'dae>,
    condition: &Expression,
    message: &Expression,
    level: Option<&Expression>,
    span: Span,
) -> Result<(), dae::DaeConstructionError> {
    let (condition, _) = lower_condition(
        construction,
        environment.base.coordinates,
        environment.base.functions,
        environment.base.sample_lattices,
        condition,
    )?;
    let failed = negate_condition(construction, condition, span)?;
    let activation = owner.activation();
    // An unconditional activation contributes nothing to conjoin: the failure
    // condition alone is the action guard.
    let action_guard = if activation.always {
        failed
    } else {
        combine_conditions(construction, activation.condition, failed, false, span)?
    };
    let trigger = activation.trigger;
    let message = lower_expression(
        construction,
        environment.base.coordinates,
        environment.base.functions,
        message,
        None,
    )?;
    let level = lower_optional_expression(
        construction,
        environment.base.coordinates,
        environment.base.functions,
        level,
    )?;
    let provenance = dae::DaeProvenance::source(span)?;
    construction.events(|events| {
        events.assert_with_level(trigger, action_guard, message, level, provenance)
    })?;
    Ok(())
}

struct AlgorithmIfInput<'values, 'source, 'dae> {
    owner: AlgorithmOwner<'dae>,
    values: &'values mut HashMap<VarName, dae::ExprId<'dae>>,
    blocks: &'source [EventBlockPlan<'source>],
    else_product: &'source EventElseProduct<'source>,
    span: Span,
}

fn lower_algorithm_if<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    discrete_values: &mut DiscreteValueStaging<'dae>,
    environment: EventAlgorithmEnvironment<'_, '_, 'dae>,
    input: AlgorithmIfInput<'_, '_, 'dae>,
) -> Result<(), dae::DaeConstructionError> {
    let AlgorithmIfInput {
        owner,
        values,
        blocks,
        else_product,
        span,
    } = input;
    let incoming = values.clone();
    let mut previous = None;
    let mut condition_values = Vec::with_capacity(blocks.len());
    let mut branch_values = Vec::with_capacity(blocks.len());
    for block in blocks {
        let branch = lower_algorithm_if_branch(
            construction,
            discrete_values,
            environment,
            AlgorithmIfBranchInput {
                owner,
                incoming: &incoming,
                previous,
                block,
                span,
            },
        )?;
        condition_values.push(branch.value);
        branch_values.push(branch.values);
        previous = Some(branch.cumulative);
    }
    let else_values = lower_algorithm_if_else(
        construction,
        discrete_values,
        environment,
        AlgorithmIfElseInput {
            owner,
            previous,
            incoming: &incoming,
            else_product,
            span,
        },
    )?;
    let updates = join_algorithm_if_values(
        construction,
        values,
        AlgorithmIfJoin {
            environment,
            incoming: &incoming,
            conditions: &condition_values,
            branches: &branch_values,
            else_values: &else_values,
            span,
        },
    )?;
    record_model_event_join(environment, owner, &updates, span)
}

struct AlgorithmIfBranchInput<'values, 'source, 'dae> {
    owner: AlgorithmOwner<'dae>,
    incoming: &'values HashMap<VarName, dae::ExprId<'dae>>,
    previous: Option<dae::ConditionId<'dae>>,
    block: &'source EventBlockPlan<'source>,
    span: Span,
}

struct AlgorithmIfBranch<'dae> {
    value: Option<dae::ExprId<'dae>>,
    values: HashMap<VarName, dae::ExprId<'dae>>,
    cumulative: dae::ConditionId<'dae>,
}

fn lower_algorithm_if_branch<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    discrete_values: &mut DiscreteValueStaging<'dae>,
    environment: EventAlgorithmEnvironment<'_, '_, 'dae>,
    input: AlgorithmIfBranchInput<'_, '_, 'dae>,
) -> Result<AlgorithmIfBranch<'dae>, dae::DaeConstructionError> {
    let (value, condition, owner_clock) = lower_algorithm_if_condition(
        construction,
        environment,
        input.owner,
        input.incoming,
        &input.block.condition,
    )?;
    let available = match input.previous {
        Some(previous) => {
            let not_previous = negate_condition(construction, previous, input.span)?;
            combine_conditions(construction, condition, not_previous, false, input.span)?
        }
        None => condition,
    };
    let condition_span = input.block.condition.span();
    let guard = algorithm_if_guard(
        construction,
        input.owner.parent,
        available,
        owner_clock,
        condition_span,
        input.span,
    )?;
    if let Some(clock) = guard.owner_clock {
        own_clocked_algorithm_targets(
            construction,
            environment.base.coordinates,
            clock.into(),
            &input.block.statements,
        )?;
    }
    let mut values = input.incoming.clone();
    lower_algorithm_statements(
        construction,
        discrete_values,
        environment,
        AlgorithmOwner {
            parent: Some(guard),
            span: input.span,
            ..input.owner
        },
        &mut values,
        &input.block.statements,
    )?;
    let cumulative = match input.previous {
        Some(previous) => combine_conditions(construction, previous, condition, true, input.span)?,
        None => condition,
    };
    Ok(AlgorithmIfBranch {
        value,
        values,
        cumulative,
    })
}

struct AlgorithmIfElseInput<'values, 'source, 'dae> {
    owner: AlgorithmOwner<'dae>,
    previous: Option<dae::ConditionId<'dae>>,
    incoming: &'values HashMap<VarName, dae::ExprId<'dae>>,
    else_product: &'source EventElseProduct<'source>,
    span: Span,
}

fn lower_algorithm_if_else<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    discrete_values: &mut DiscreteValueStaging<'dae>,
    environment: EventAlgorithmEnvironment<'_, '_, 'dae>,
    input: AlgorithmIfElseInput<'_, '_, 'dae>,
) -> Result<HashMap<VarName, dae::ExprId<'dae>>, dae::DaeConstructionError> {
    let mut values = input.incoming.clone();
    match input.else_product {
        EventElseProduct::Absent => {}
        EventElseProduct::Statements(statements) => {
            lower_algorithm_else(
                construction,
                discrete_values,
                environment,
                AlgorithmElseInput {
                    owner: input.owner,
                    previous: input.previous,
                    values: &mut values,
                    statements,
                    span: input.span,
                },
            )?;
        }
    }
    Ok(values)
}

fn lower_algorithm_if_condition<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    environment: EventAlgorithmEnvironment<'_, '_, 'dae>,
    owner: AlgorithmOwner<'dae>,
    values: &HashMap<VarName, dae::ExprId<'dae>>,
    product: &AlgorithmConditionProduct<'_>,
) -> Result<
    (
        Option<dae::ExprId<'dae>>,
        dae::ConditionId<'dae>,
        Option<dae::PeriodicClockId<'dae>>,
    ),
    dae::DaeConstructionError,
> {
    let expression = product.source();
    if is_event_condition(expression) {
        let (condition, clock) = lower_algorithm_condition(
            construction,
            environment.base.coordinates,
            environment.base.functions,
            product,
        )?;
        return Ok((None, condition, clock));
    }
    let value = lower_model_algorithm_expression(
        construction,
        environment.base.coordinates,
        environment.base.functions,
        values,
        expression,
    )?;
    if owner.parent.is_none() {
        let (condition, clock) = lower_algorithm_condition(
            construction,
            environment.base.coordinates,
            environment.base.functions,
            product,
        )?;
        return Ok((Some(value), condition, clock));
    }
    let span = expression
        .span()
        .ok_or(dae::DaeConstructionError::InvalidExpressionForm { span: owner.span })?;
    let provenance = dae::DaeProvenance::source(span)?;
    let condition = construction.conditions(|conditions| conditions.reserve(provenance))?;
    construction.conditions(|conditions| {
        conditions.define(condition, dae::ConditionInput::Discrete(value), provenance)
    })?;
    Ok((Some(value), condition, None))
}

struct AlgorithmIfJoin<'scope, 'shape, 'values, 'dae> {
    environment: EventAlgorithmEnvironment<'scope, 'shape, 'dae>,
    incoming: &'values HashMap<VarName, dae::ExprId<'dae>>,
    conditions: &'values [Option<dae::ExprId<'dae>>],
    branches: &'values [HashMap<VarName, dae::ExprId<'dae>>],
    else_values: &'values HashMap<VarName, dae::ExprId<'dae>>,
    span: Span,
}

fn join_algorithm_if_values<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    values: &mut HashMap<VarName, dae::ExprId<'dae>>,
    input: AlgorithmIfJoin<'_, '_, '_, 'dae>,
) -> Result<Vec<(VarName, dae::ExprId<'dae>)>, dae::DaeConstructionError> {
    let Some(conditions) = input.conditions.iter().copied().collect::<Option<Vec<_>>>() else {
        // An event guard has no pure Boolean SSA value. Analysis marks every
        // value written beneath it unavailable to later sequential reads, so
        // the enclosing value environment deliberately remains unchanged.
        return Ok(Vec::new());
    };
    let mut targets = HashSet::new();
    targets.extend(input.else_values.iter().filter_map(|(target, value)| {
        (input.incoming.get(target) != Some(value)).then_some(target.clone())
    }));
    for branch in input.branches {
        targets.extend(branch.iter().filter_map(|(target, value)| {
            (input.incoming.get(target) != Some(value)).then_some(target.clone())
        }));
    }
    let provenance =
        dae::DaeProvenance::generated(dae::DaeGeneration::AlgorithmEquation, input.span)?;
    let mut updates = Vec::with_capacity(targets.len());
    for target in targets {
        let else_value =
            algorithm_ssa_value(construction, &input, input.else_values, &target, provenance)?;
        let mut arms = Vec::with_capacity(input.branches.len());
        for branch in input.branches {
            arms.push(algorithm_ssa_value(
                construction,
                &input,
                branch,
                &target,
                provenance,
            )?);
        }
        let joined = construction.expressions(|expressions| {
            expressions
                .at(provenance)
                .conditional(conditions.iter().copied().zip(arms), else_value)
        })?;
        values.insert(target.clone(), joined);
        updates.push((target, joined));
    }
    Ok(updates)
}

/// Retain the construction-issued SSA join that gives a source `if` one
/// value per target under its enclosing activation.
///
/// The guarded source steps remain in the transaction as ordered provenance.
/// This generated step is the checked bridge from those steps to the lazy
/// conditional expression consumed by executable lowering; it prevents a
/// backend from reconstructing independent source `if` statements as one
/// mutually-exclusive guarded-assignment ladder.
fn record_model_event_join<'dae>(
    environment: EventAlgorithmEnvironment<'_, '_, 'dae>,
    owner: AlgorithmOwner<'dae>,
    updates: &[(VarName, dae::ExprId<'dae>)],
    span: Span,
) -> Result<(), dae::DaeConstructionError> {
    let steps = match (environment.transaction, updates.is_empty()) {
        (EventTransactionSink::NoTargets, true) | (EventTransactionSink::Steps(_), true) => {
            return Ok(());
        }
        (EventTransactionSink::NoTargets, false) => {
            return Err(dae::DaeConstructionError::InvalidExpressionForm { span });
        }
        (EventTransactionSink::Steps(steps), false) => steps,
    };
    let activation = owner.activation();
    let provenance = dae::DaeProvenance::generated(dae::DaeGeneration::AlgorithmEquation, span)?;
    let mut definitions = Vec::with_capacity(updates.len());
    for (target, value) in updates {
        let event_target = model_event_target(environment.base.coordinates, target, span)?;
        definitions.push(dae::ModelEventDefinition::new(
            event_target,
            *value,
            provenance,
        ));
    }
    steps.borrow_mut().push(dae::ModelEventStep::new(
        activation.trigger,
        activation.condition,
        activation.owner_clock.map(Into::into),
        definitions,
        provenance,
    ));
    Ok(())
}

fn algorithm_ssa_value<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    input: &AlgorithmIfJoin<'_, '_, '_, 'dae>,
    branch: &HashMap<VarName, dae::ExprId<'dae>>,
    target: &VarName,
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    if let Some(value) = branch.get(target).or_else(|| input.incoming.get(target)) {
        return Ok(*value);
    }
    let coordinate = input
        .environment
        .base
        .coordinates
        .get(target)
        .copied()
        .ok_or(dae::DaeConstructionError::InvalidExpressionForm { span: input.span })?;
    construction
        .expressions(|expressions| expressions.at(provenance).coordinate(coordinate.current()))
}

fn algorithm_if_guard<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    parent: Option<EventGuard<'dae>>,
    available: dae::ConditionId<'dae>,
    owner_clock: Option<dae::PeriodicClockId<'dae>>,
    provenance_span: Span,
    span: Span,
) -> Result<EventGuard<'dae>, dae::DaeConstructionError> {
    let branch_provenance = dae::DaeProvenance::source(provenance_span)?;
    match parent {
        Some(parent) => Ok(EventGuard {
            trigger: parent.trigger,
            condition: combine_conditions(construction, parent.condition, available, false, span)?,
            owner_clock: parent.owner_clock.or(owner_clock),
            branch_provenance,
            always: false,
            parent_activation: Some((parent.trigger, parent.condition)),
        }),
        None => Ok(EventGuard {
            trigger: available,
            condition: available,
            owner_clock,
            branch_provenance,
            always: false,
            parent_activation: None,
        }),
    }
}

struct AlgorithmElseInput<'values, 'source, 'dae> {
    owner: AlgorithmOwner<'dae>,
    previous: Option<dae::ConditionId<'dae>>,
    values: &'values mut HashMap<VarName, dae::ExprId<'dae>>,
    statements: &'source [EventStatementPlan<'source>],
    span: Span,
}

fn lower_algorithm_else<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    discrete_values: &mut DiscreteValueStaging<'dae>,
    environment: EventAlgorithmEnvironment<'_, '_, 'dae>,
    input: AlgorithmElseInput<'_, '_, 'dae>,
) -> Result<(), dae::DaeConstructionError> {
    let AlgorithmElseInput {
        owner,
        previous,
        values,
        statements,
        span,
    } = input;
    let available = match previous {
        Some(previous) => negate_condition(construction, previous, span)?,
        None => always_condition(construction, span)?,
    };
    let guard = algorithm_if_guard(construction, owner.parent, available, None, span, span)?;
    lower_algorithm_statements(
        construction,
        discrete_values,
        environment,
        AlgorithmOwner {
            parent: Some(guard),
            span,
            ..owner
        },
        values,
        statements,
    )
}

fn lower_algorithm_when<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    discrete_values: &mut DiscreteValueStaging<'dae>,
    environment: EventAlgorithmEnvironment<'_, '_, 'dae>,
    owner: AlgorithmOwner<'dae>,
    values: &mut HashMap<VarName, dae::ExprId<'dae>>,
    blocks: &[EventBlockPlan<'_>],
    span: Span,
) -> Result<(), dae::DaeConstructionError> {
    let mut guarded_blocks = Vec::with_capacity(blocks.len());
    for block in blocks {
        let (condition, owner_clock) = lower_algorithm_condition(
            construction,
            environment.base.coordinates,
            environment.base.functions,
            &block.condition,
        )?;
        // MLS §8.3.5 activates each branch of a `when`/`elsewhen` chain on its
        // own rising edge; the textual order of the branches resolves the
        // simultaneous ones. See `lower_chain_guards` for the equation form —
        // the algorithm form has to agree with it or the same chain would mean
        // two things depending on which section it was written in.
        let available = condition;
        let guard = match owner.parent {
            Some(parent) => EventGuard {
                trigger: available,
                condition: combine_conditions(
                    construction,
                    parent.condition,
                    available,
                    false,
                    span,
                )?,
                owner_clock: parent.owner_clock.or(owner_clock),
                branch_provenance: dae::DaeProvenance::source(block_condition_span(block, span)?)?,
                always: false,
                parent_activation: Some((parent.trigger, parent.condition)),
            },
            None => EventGuard {
                trigger: available,
                condition: available,
                owner_clock,
                branch_provenance: dae::DaeProvenance::source(block_condition_span(block, span)?)?,
                always: false,
                parent_activation: None,
            },
        };
        guarded_blocks.push((block, guard));
    }
    for (block, guard) in &guarded_blocks {
        if let Some(clock) = guard.owner_clock {
            own_clocked_algorithm_targets(
                construction,
                environment.base.coordinates,
                clock.into(),
                &block.statements,
            )?;
        }
    }
    for (block, guard) in guarded_blocks {
        let mut branch_values = values.clone();
        lower_algorithm_statements(
            construction,
            discrete_values,
            environment,
            AlgorithmOwner {
                parent: Some(guard),
                span,
                ..owner
            },
            &mut branch_values,
            &block.statements,
        )?;
    }
    Ok(())
}

fn block_condition_span(
    block: &EventBlockPlan<'_>,
    owner_span: Span,
) -> Result<Span, dae::DaeConstructionError> {
    let _ = owner_span;
    Ok(block.condition.span())
}
