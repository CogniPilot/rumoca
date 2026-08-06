use super::*;

#[derive(Clone, Copy)]
pub(super) struct AlgorithmEnvironment<'scope, 'shape, 'dae> {
    pub(super) coordinates: &'scope HashMap<VarName, Coordinate<'dae>>,
    pub(super) functions: &'scope FunctionRegistry<'shape, 'dae>,
    pub(super) sample_lattices: &'scope [(Span, PeriodicClockSchedule)],
    pub(super) tensor_loops: Option<&'scope HashMap<Span, ModelEventTensorLoopPlan>>,
    pub(super) function_calls: Option<&'scope HashMap<Span, ModelEventFunctionCallPlan>>,
}

#[derive(Clone, Copy)]
struct AlgorithmOwner<'dae> {
    discrete_owner: Option<DiscreteValueOwnerHandle>,
    parent: Option<EventGuard<'dae>>,
    span: Span,
}

pub(super) struct ModelAlgorithmsRequest<'scope, 'shape, 'dae> {
    pub(super) flat: &'scope flat::Model,
    pub(super) environment: AlgorithmEnvironment<'scope, 'shape, 'dae>,
    pub(super) plans: &'scope [ModelAlgorithmPlan],
    pub(super) topology: &'scope DiscreteValueTopologyPlan,
}

pub(super) fn lower_algorithms<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    discrete_values: &mut DiscreteValueStaging<'dae>,
    request: ModelAlgorithmsRequest<'_, '_, 'dae>,
) -> Result<(), dae::DaeConstructionError> {
    debug_assert_eq!(request.flat.algorithms.len(), request.plans.len());
    // Claim every sampled algorithm coordinate before lowering any body. A
    // consumer is allowed to precede its producer in Flat order; the unique
    // clock owner is an analysis fact, not an artifact of lowering order.
    for (algorithm, plan) in request.flat.algorithms.iter().zip(request.plans) {
        let environment = match plan {
            ModelAlgorithmPlan::Event {
                tensor_loops,
                function_calls,
            } => AlgorithmEnvironment {
                tensor_loops: Some(tensor_loops),
                function_calls: Some(function_calls),
                ..request.environment
            },
            _ => request.environment,
        };
        preclaim_algorithm_clock_targets(construction, environment, &algorithm.statements, None)?;
    }
    for (algorithm, plan) in request.flat.algorithms.iter().zip(request.plans) {
        let owner_provenance =
            dae::DaeProvenance::generated(dae::DaeGeneration::AlgorithmEquation, algorithm.span)?;
        let discrete_owner = discrete_values.owner(
            owner_provenance,
            model_algorithm_targets(request.flat, algorithm),
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
            ModelAlgorithmPlan::Event {
                tensor_loops,
                function_calls,
            } => {
                let mut values = HashMap::new();
                let environment = AlgorithmEnvironment {
                    tensor_loops: Some(tensor_loops),
                    function_calls: Some(function_calls),
                    ..request.environment
                };
                lower_algorithm_statements(
                    lowering.construction,
                    lowering.discrete_values,
                    environment,
                    AlgorithmOwner {
                        discrete_owner,
                        parent: None,
                        span: algorithm.span,
                    },
                    &mut values,
                    &algorithm.statements,
                )?;
            }
        }
    }
    Ok(())
}

fn preclaim_algorithm_clock_targets<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    environment: AlgorithmEnvironment<'_, '_, 'dae>,
    statements: &[rumoca_core::Statement],
    inherited: Option<dae::PeriodicClockId<'dae>>,
) -> Result<(), dae::DaeConstructionError> {
    if let Some(clock) = inherited {
        return own_clocked_algorithm_targets(
            construction,
            environment.coordinates,
            clock.into(),
            environment
                .function_calls
                .expect("event analysis supplies clocked function-call plans"),
            statements,
        );
    }
    for statement in statements {
        match statement {
            rumoca_core::Statement::When { blocks, .. } => {
                for block in blocks {
                    let clock = condition_owner_clock(environment.functions, &block.cond)?;
                    preclaim_algorithm_clock_targets(
                        construction,
                        environment,
                        &block.stmts,
                        clock,
                    )?;
                }
            }
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                ..
            } => {
                for block in cond_blocks {
                    let clock = condition_owner_clock(environment.functions, &block.cond)?;
                    preclaim_algorithm_clock_targets(
                        construction,
                        environment,
                        &block.stmts,
                        clock,
                    )?;
                }
                if let Some(statements) = else_block {
                    preclaim_algorithm_clock_targets(construction, environment, statements, None)?;
                }
            }
            rumoca_core::Statement::For { equations, .. } => {
                preclaim_algorithm_clock_targets(construction, environment, equations, None)?;
            }
            _ => {}
        }
    }
    Ok(())
}

fn lower_algorithm_statements<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    discrete_values: &mut DiscreteValueStaging<'dae>,
    environment: AlgorithmEnvironment<'_, '_, 'dae>,
    owner: AlgorithmOwner<'dae>,
    values: &mut HashMap<VarName, dae::ExprId<'dae>>,
    statements: &[rumoca_core::Statement],
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
    environment: AlgorithmEnvironment<'_, '_, 'dae>,
    owner: AlgorithmOwner<'dae>,
    values: &mut HashMap<VarName, dae::ExprId<'dae>>,
    statement: &rumoca_core::Statement,
) -> Result<(), dae::DaeConstructionError> {
    let context = algorithm_statement_context(environment, owner, values);
    match statement {
        rumoca_core::Statement::Assignment { comp, value, span } => {
            let updates = lower_algorithm_assignment(
                construction,
                discrete_values,
                owner.discrete_owner,
                context,
                comp,
                value,
                *span,
            )?;
            values.extend(updates);
            Ok(())
        }
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            span,
        } => lower_algorithm_if(
            construction,
            discrete_values,
            environment,
            AlgorithmIfInput {
                owner,
                values,
                blocks: cond_blocks,
                fallback: else_block.as_deref().unwrap_or_default(),
                span: *span,
            },
        ),
        rumoca_core::Statement::When { blocks, span } => lower_algorithm_when(
            construction,
            discrete_values,
            environment,
            owner,
            values,
            blocks,
            *span,
        ),
        rumoca_core::Statement::FunctionCall {
            comp,
            args,
            outputs,
            span,
        } => {
            let updates = lower_algorithm_call_statement(
                construction,
                discrete_values,
                owner,
                context,
                AlgorithmFunctionCall {
                    component: comp,
                    arguments: args,
                    outputs,
                    span: *span,
                    plan: &environment
                        .function_calls
                        .expect("event analysis supplies function-call receiver plans")[span],
                },
            )?;
            values.extend(updates);
            Ok(())
        }
        rumoca_core::Statement::For {
            equations, span, ..
        } => {
            let updates = lower_algorithm_for_statement(
                construction,
                discrete_values,
                environment,
                owner.discrete_owner,
                context,
                equations,
                *span,
            )?;
            values.extend(updates);
            Ok(())
        }
        rumoca_core::Statement::Assert {
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
        _ => unreachable!("algorithm analysis restricts the checked statement grammar"),
    }
}

fn algorithm_statement_context<'scope, 'shape, 'dae>(
    environment: AlgorithmEnvironment<'scope, 'shape, 'dae>,
    owner: AlgorithmOwner<'dae>,
    values: &'scope HashMap<VarName, dae::ExprId<'dae>>,
) -> AlgorithmStatementContext<'scope, 'shape, 'dae> {
    AlgorithmStatementContext {
        coordinates: environment.coordinates,
        functions: environment.functions,
        values,
        parent: owner.parent,
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
    environment: AlgorithmEnvironment<'_, '_, 'dae>,
    discrete_owner: Option<DiscreteValueOwnerHandle>,
    context: AlgorithmStatementContext<'_, '_, 'dae>,
    statements: &[rumoca_core::Statement],
    span: Span,
) -> Result<Vec<(VarName, dae::ExprId<'dae>)>, dae::DaeConstructionError> {
    let plan = &environment
        .tensor_loops
        .expect("event analysis supplies tensor-loop plans")[&span];
    lower_algorithm_tensor_loop(
        construction,
        discrete_values,
        discrete_owner,
        context,
        plan,
        statements,
        span,
    )
}

fn lower_algorithm_assertion<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    environment: AlgorithmEnvironment<'_, '_, 'dae>,
    owner: AlgorithmOwner<'dae>,
    condition: &Expression,
    message: &Expression,
    level: Option<&Expression>,
    span: Span,
) -> Result<(), dae::DaeConstructionError> {
    let (condition, _) = lower_condition(
        construction,
        environment.coordinates,
        environment.functions,
        environment.sample_lattices,
        condition,
    )?;
    let failed = negate_condition(construction, condition, span)?;
    let (trigger, action_guard) = match owner.parent {
        Some(parent) => (
            parent.trigger,
            combine_conditions(construction, parent.condition, failed, false, span)?,
        ),
        None => (always_condition(construction, span)?, failed),
    };
    let message = lower_expression(
        construction,
        environment.coordinates,
        environment.functions,
        message,
        None,
    )?;
    let level = lower_optional_expression(
        construction,
        environment.coordinates,
        environment.functions,
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
    blocks: &'source [rumoca_core::StatementBlock],
    fallback: &'source [rumoca_core::Statement],
    span: Span,
}

fn lower_algorithm_if<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    discrete_values: &mut DiscreteValueStaging<'dae>,
    environment: AlgorithmEnvironment<'_, '_, 'dae>,
    input: AlgorithmIfInput<'_, '_, 'dae>,
) -> Result<(), dae::DaeConstructionError> {
    let AlgorithmIfInput {
        owner,
        values,
        blocks,
        fallback: else_block,
        span,
    } = input;
    let incoming = values.clone();
    let mut previous = None;
    let mut condition_values = Vec::with_capacity(blocks.len());
    let mut branch_values = Vec::with_capacity(blocks.len());
    for block in blocks {
        let (condition_value, condition, owner_clock) =
            lower_algorithm_if_condition(construction, environment, owner, &incoming, &block.cond)?;
        condition_values.push(condition_value);
        let available = match previous {
            Some(previous) => {
                let not_previous = negate_condition(construction, previous, span)?;
                combine_conditions(construction, condition, not_previous, false, span)?
            }
            None => condition,
        };
        let condition_span = block
            .cond
            .span()
            .expect("analysis proves algorithm condition provenance");
        let guard = algorithm_if_guard(
            construction,
            owner.parent,
            available,
            owner_clock,
            condition_span,
            span,
        )?;
        if let Some(clock) = guard.owner_clock {
            own_clocked_algorithm_targets(
                construction,
                environment.coordinates,
                clock.into(),
                environment
                    .function_calls
                    .expect("event analysis supplies clocked function-call plans"),
                &block.stmts,
            )?;
        }
        let mut branch = incoming.clone();
        lower_algorithm_statements(
            construction,
            discrete_values,
            environment,
            AlgorithmOwner {
                parent: Some(guard),
                span,
                ..owner
            },
            &mut branch,
            &block.stmts,
        )?;
        branch_values.push(branch);
        previous = Some(match previous {
            Some(previous) => combine_conditions(construction, previous, condition, true, span)?,
            None => condition,
        });
    }
    let mut fallback = incoming.clone();
    if !else_block.is_empty() {
        lower_algorithm_else(
            construction,
            discrete_values,
            environment,
            AlgorithmElseInput {
                owner,
                previous,
                values: &mut fallback,
                statements: else_block,
                span,
            },
        )?;
    }
    join_algorithm_if_values(
        construction,
        values,
        AlgorithmIfJoin {
            environment,
            incoming: &incoming,
            conditions: &condition_values,
            branches: &branch_values,
            fallback: &fallback,
            span,
        },
    )
}

fn lower_algorithm_if_condition<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    environment: AlgorithmEnvironment<'_, '_, 'dae>,
    owner: AlgorithmOwner<'dae>,
    values: &HashMap<VarName, dae::ExprId<'dae>>,
    expression: &Expression,
) -> Result<
    (
        Option<dae::ExprId<'dae>>,
        dae::ConditionId<'dae>,
        Option<dae::PeriodicClockId<'dae>>,
    ),
    dae::DaeConstructionError,
> {
    if is_event_condition(expression) {
        let (condition, clock) = lower_condition(
            construction,
            environment.coordinates,
            environment.functions,
            environment.sample_lattices,
            expression,
        )?;
        return Ok((None, condition, clock));
    }
    let value = lower_model_algorithm_expression(
        construction,
        environment.coordinates,
        environment.functions,
        values,
        expression,
    )?;
    if owner.parent.is_none() {
        let (condition, clock) = lower_condition(
            construction,
            environment.coordinates,
            environment.functions,
            environment.sample_lattices,
            expression,
        )?;
        return Ok((Some(value), condition, clock));
    }
    let span = expression
        .span()
        .expect("analysis proves event-algorithm condition provenance");
    let provenance = dae::DaeProvenance::source(span)?;
    let condition = construction.conditions(|conditions| conditions.reserve(provenance))?;
    construction.conditions(|conditions| {
        conditions.define(condition, dae::ConditionInput::Discrete(value), provenance)
    })?;
    Ok((Some(value), condition, None))
}

struct AlgorithmIfJoin<'scope, 'shape, 'values, 'dae> {
    environment: AlgorithmEnvironment<'scope, 'shape, 'dae>,
    incoming: &'values HashMap<VarName, dae::ExprId<'dae>>,
    conditions: &'values [Option<dae::ExprId<'dae>>],
    branches: &'values [HashMap<VarName, dae::ExprId<'dae>>],
    fallback: &'values HashMap<VarName, dae::ExprId<'dae>>,
    span: Span,
}

fn join_algorithm_if_values<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    values: &mut HashMap<VarName, dae::ExprId<'dae>>,
    input: AlgorithmIfJoin<'_, '_, '_, 'dae>,
) -> Result<(), dae::DaeConstructionError> {
    let Some(conditions) = input.conditions.iter().copied().collect::<Option<Vec<_>>>() else {
        // An event guard has no pure Boolean SSA value. Analysis marks every
        // value written beneath it unavailable to later sequential reads, so
        // the enclosing value environment deliberately remains unchanged.
        return Ok(());
    };
    let mut targets = HashSet::new();
    targets.extend(input.fallback.keys().cloned());
    for branch in input.branches {
        targets.extend(branch.keys().cloned());
    }
    let provenance =
        dae::DaeProvenance::generated(dae::DaeGeneration::AlgorithmEquation, input.span)?;
    for target in targets {
        let fallback =
            algorithm_ssa_value(construction, &input, input.fallback, &target, provenance)?;
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
                .conditional(conditions.iter().copied().zip(arms), fallback)
        })?;
        values.insert(target, joined);
    }
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
    construction.expressions(|expressions| {
        expressions
            .at(provenance)
            .coordinate(input.environment.coordinates[target].current())
    })
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
    statements: &'source [rumoca_core::Statement],
    span: Span,
}

fn lower_algorithm_else<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    discrete_values: &mut DiscreteValueStaging<'dae>,
    environment: AlgorithmEnvironment<'_, '_, 'dae>,
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
    environment: AlgorithmEnvironment<'_, '_, 'dae>,
    owner: AlgorithmOwner<'dae>,
    values: &mut HashMap<VarName, dae::ExprId<'dae>>,
    blocks: &[rumoca_core::StatementBlock],
    span: Span,
) -> Result<(), dae::DaeConstructionError> {
    let mut guarded_blocks = Vec::with_capacity(blocks.len());
    for block in blocks {
        let (condition, owner_clock) = lower_condition(
            construction,
            environment.coordinates,
            environment.functions,
            environment.sample_lattices,
            &block.cond,
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
                branch_provenance: dae::DaeProvenance::source(
                    block
                        .cond
                        .span()
                        .expect("analysis proves algorithm condition provenance"),
                )?,
                always: false,
                parent_activation: Some((parent.trigger, parent.condition)),
            },
            None => EventGuard {
                trigger: available,
                condition: available,
                owner_clock,
                branch_provenance: dae::DaeProvenance::source(
                    block
                        .cond
                        .span()
                        .expect("analysis proves algorithm condition provenance"),
                )?,
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
                environment.coordinates,
                clock.into(),
                environment
                    .function_calls
                    .expect("event analysis supplies clocked function-call plans"),
                &block.stmts,
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
            &block.stmts,
        )?;
    }
    Ok(())
}
