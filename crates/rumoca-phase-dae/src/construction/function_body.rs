mod ordered_conditionals;
pub(super) use ordered_conditionals::{
    lower_ordered_function_conditional, require_current_function_value,
    require_proven_function_reads, require_proven_loop_reads,
};

use super::analysis::selected_loop_entry;
use super::*;

mod integer_reduction;
mod loop_exit;
mod selected_loop;
mod tuple_loop;
pub(super) use integer_reduction::lower_integer_reduction;
use loop_exit::continue_while_selected;
use tuple_loop::lower_function_loop_multi_output_call;

pub(super) fn lower_generated_boolean_assignment<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    mut body: dae::FunctionBody<'dae>,
    target: &VarName,
    value: &Expression,
    span: Span,
    proven_reads: &[(VarName, Span)],
) -> Result<dae::FunctionBody<'dae>, dae::DaeConstructionError> {
    require_proven_function_reads(construction, symbols, &mut body, proven_reads)?;
    let lowered = lower_function_expression_scoped(
        construction,
        symbols.coordinates,
        symbols.functions,
        symbols.shapes,
        &body,
        symbols.binders,
        value,
    )?;
    let target = function_value_coordinate(symbols.coordinates, target);
    let provenance = dae::DaeProvenance::source(span)?;
    construction.functions(|functions| functions.assign(&mut body, target, lowered, provenance))?;
    Ok(body)
}

/// Lower one loop statement that defines no carried value of its own plan
/// kind: an assertion, or a branch selection captured on every iteration of its
/// enclosing loop at the selection's source position in that iteration.
fn lower_loop_action_statement<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    loop_body: &mut dae::FunctionLoop<'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    (statement, plan): (&rumoca_core::Statement, &FunctionStatementPlan),
) -> Result<bool, dae::DaeConstructionError> {
    let (target, value, span, proven_reads) = match plan {
        FunctionStatementPlan::ProvenAssertion => return Ok(true),
        FunctionStatementPlan::RuntimeAssertion => {
            lower_function_loop_assertion(construction, symbols, loop_body, binders, statement)?;
            return Ok(true);
        }
        FunctionStatementPlan::GeneratedBooleanAssignment {
            target,
            value,
            span,
            proven_reads,
            selected: None,
            ..
        } => (target, value, *span, proven_reads),
        _ => return Ok(false),
    };
    require_proven_loop_reads(construction, symbols, loop_body, proven_reads)?;
    let lowered = lower_function_expression_scoped(
        construction,
        symbols.coordinates,
        symbols.functions,
        symbols.shapes,
        loop_body.body(),
        binders,
        value,
    )?;
    let target = function_value_coordinate(symbols.coordinates, target);
    let provenance = dae::DaeProvenance::source(span)?;
    construction
        .functions(|functions| functions.assign_loop(loop_body, target, lowered, provenance))?;
    Ok(true)
}

pub(super) fn lower_guarded_function_return<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    body: dae::FunctionBody<'dae>,
    function: &rumoca_core::Function,
    branch_plans: &[Vec<FunctionStatementPlan>],
    tail_plans: &[FunctionStatementPlan],
    targets: &[VarName],
) -> Result<dae::FunctionBody<'dae>, dae::DaeConstructionError> {
    let mut body = body;
    let Some((
        rumoca_core::Statement::If {
            cond_blocks, span, ..
        },
        tail,
    )) = function.body.split_first()
    else {
        unreachable!("analysis proves a leading guarded return")
    };
    let conditions = cond_blocks
        .iter()
        .map(|block| {
            lower_function_expression(
                construction,
                symbols.coordinates,
                symbols.functions,
                symbols.shapes,
                &body,
                &block.cond,
            )
        })
        .collect::<Result<Vec<_>, _>>()?;
    let returned = targets
        .iter()
        .map(|target| {
            cond_blocks
                .iter()
                .zip(branch_plans)
                .map(|(block, plans)| {
                    lower_guarded_return_value(
                        construction,
                        symbols,
                        &body,
                        &block.stmts[..block.stmts.len() - 1],
                        plans,
                        target,
                    )
                })
                .collect::<Result<Vec<_>, _>>()
        })
        .collect::<Result<Vec<_>, _>>()?;

    body = lower_function_statements(construction, symbols, body, tail, tail_plans, None)?;
    let provenance =
        dae::DaeProvenance::generated(dae::DaeGeneration::FunctionConditionLowering, *span)?;
    for (target, returned) in targets.iter().zip(returned) {
        let target = function_value_coordinate(symbols.coordinates, target);
        let fallback =
            construction.functions(|functions| functions.read(&body, target, provenance))?;
        let branches = conditions.iter().copied().zip(returned);
        let value = construction.expressions(|expressions| {
            expressions.at(provenance).conditional(branches, fallback)
        })?;
        construction
            .functions(|functions| functions.assign(&mut body, target, value, provenance))?;
    }
    Ok(body)
}

fn lower_guarded_return_value<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    body: &dae::FunctionBody<'dae>,
    statements: &[rumoca_core::Statement],
    plans: &[FunctionStatementPlan],
    selected: &VarName,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let assignment = statements
        .iter()
        .zip(plans)
        .find_map(|(statement, plan)| match (statement, plan) {
            (
                rumoca_core::Statement::Assignment { value, .. },
                FunctionStatementPlan::Assignment(assignment),
            ) if assignment.target() == selected => Some(value),
            _ => None,
        })
        .expect("analysis proves every returning branch defines every output");
    lower_function_expression(
        construction,
        symbols.coordinates,
        symbols.functions,
        symbols.shapes,
        body,
        assignment,
    )
}

pub(super) struct FunctionConditional<'scope, 'statement, 'dae> {
    pub(super) binders: &'scope HashMap<VarName, dae::DomainBinderId<'dae>>,
    pub(super) blocks: &'statement [rumoca_core::StatementBlock],
    pub(super) fallback: Option<&'statement [rumoca_core::Statement]>,
    pub(super) branch_plans: &'statement [Vec<FunctionStatementPlan>],
    pub(super) condition_reads: &'statement [Vec<(VarName, Span)>],
    pub(super) fallback_plans: Option<&'statement [FunctionStatementPlan]>,
    pub(super) targets: &'statement [VarName],
    pub(super) span: Span,
}

pub(super) fn lower_function_value_seed<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    seed: &FunctionValueSeed,
    span: Span,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let provenance =
        dae::DaeProvenance::generated(dae::DaeGeneration::FunctionAggregateLowering, span)?;
    lower_seed_value(construction, seed, provenance).map(|(_, value)| value)
}

fn lower_seed_value<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    seed: &FunctionValueSeed,
    provenance: dae::DaeProvenance,
) -> Result<(dae::ValueTypeId<'dae>, dae::ExprId<'dae>), dae::DaeConstructionError> {
    match seed {
        FunctionValueSeed::Scalar { dimensions, scalar } => {
            let value_type = construction.types(|types| {
                types.derived(
                    dae::ValueType::array(*scalar, dimensions.clone()),
                    provenance,
                )
            })?;
            let element = match scalar {
                dae::ScalarType::Real => construction.expressions(|expressions| {
                    expressions
                        .at(provenance)
                        .literal(dae::DaeLiteral::Real(0.0))
                })?,
                dae::ScalarType::Integer => construction.expressions(|expressions| {
                    expressions
                        .at(provenance)
                        .literal(dae::DaeLiteral::Integer(0))
                })?,
                dae::ScalarType::Enumeration => construction
                    .expressions(|expressions| expressions.at(provenance).enumeration_literal(1))?,
                dae::ScalarType::Boolean => construction.expressions(|expressions| {
                    expressions
                        .at(provenance)
                        .literal(dae::DaeLiteral::Boolean(false))
                })?,
                dae::ScalarType::String => construction.expressions(|expressions| {
                    expressions
                        .at(provenance)
                        .literal(dae::DaeLiteral::String(String::new()))
                })?,
                dae::ScalarType::Record => {
                    unreachable!("record seeds own a recursive field tree")
                }
            };
            let value = lower_seed_array(construction, dimensions, element, provenance)?;
            Ok((value_type, value))
        }
        FunctionValueSeed::Record {
            name,
            dimensions,
            fields,
        } => {
            let fields = fields
                .iter()
                .map(|(name, seed)| {
                    lower_seed_value(construction, seed, provenance)
                        .map(|(value_type, value)| (name.clone(), value_type, value))
                })
                .collect::<Result<Vec<_>, _>>()?;
            let scalar_type = construction.types(|types| {
                types.record(
                    name.clone(),
                    fields
                        .iter()
                        .map(|(name, value_type, _)| (name.clone(), *value_type)),
                    provenance,
                )
            })?;
            let record = construction.expressions(|expressions| {
                expressions
                    .at(provenance)
                    .record(scalar_type, fields.iter().map(|(_, _, value)| *value))
            })?;
            let value = lower_seed_array(construction, dimensions, record, provenance)?;
            let value_type = construction.types(|types| {
                types.record_array(
                    name.clone(),
                    fields
                        .iter()
                        .map(|(name, value_type, _)| (name.clone(), *value_type)),
                    dimensions.clone(),
                    provenance,
                )
            })?;
            Ok((value_type, value))
        }
    }
}

fn lower_seed_array<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    dimensions: &[u32],
    element: dae::ExprId<'dae>,
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    if dimensions.is_empty() {
        return Ok(element);
    }
    let binders = dimensions
        .iter()
        .enumerate()
        .map(|(ordinal, extent)| StructuredIndexBinder {
            provenance: rumoca_core::IndexBinderProvenance::Generated,
            id: ordinal,
            display_name: format!("seed{ordinal}"),
            lower: 1,
            upper: i64::from(*extent),
            step: 1,
        })
        .collect::<Vec<_>>();
    let domain = construction
        .domains(|domains| domains.structured(StructuredIndexDomain { binders }, provenance))?;
    construction
        .expressions(|expressions| expressions.at(provenance).comprehension(domain, element))
}

pub(super) struct TotalArrayDefinition<'scope, 'statement, 'dae> {
    pub(super) symbols: FunctionSymbols<'scope, 'dae>,
    pub(super) domain: dae::DomainId<'dae>,
    pub(super) binders: &'scope HashMap<VarName, dae::DomainBinderId<'dae>>,
    pub(super) statements: &'statement [rumoca_core::Statement],
    pub(super) plans: &'statement [FunctionStatementPlan],
    pub(super) owner: dae::DaeProvenance,
}

pub(super) fn lower_total_function_array_definition<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    mut body: dae::FunctionBody<'dae>,
    input: TotalArrayDefinition<'_, '_, 'dae>,
) -> Result<dae::FunctionBody<'dae>, dae::DaeConstructionError> {
    if input
        .plans
        .iter()
        .any(|plan| matches!(plan, FunctionStatementPlan::RuntimeAssertion))
    {
        let mut loop_body = construction
            .functions(|functions| functions.begin_loop(body, input.domain, [], input.owner))?;
        lower_total_function_assertions(construction, &input, &mut loop_body)?;
        body = construction.functions(|functions| functions.finish_loop(loop_body, input.owner))?;
    }
    for (statement, plan) in input.statements.iter().zip(input.plans) {
        if matches!(plan, FunctionStatementPlan::Assignment(_)) {
            lower_one_total_function_array_definition(
                construction,
                &mut body,
                &input,
                statement,
                plan,
            )?;
        }
    }
    Ok(body)
}

fn lower_total_function_assertions<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    input: &TotalArrayDefinition<'_, '_, 'dae>,
    loop_body: &mut dae::FunctionLoop<'dae>,
) -> Result<(), dae::DaeConstructionError> {
    for (statement, plan) in input.statements.iter().zip(input.plans) {
        if !matches!(plan, FunctionStatementPlan::RuntimeAssertion) {
            continue;
        }
        let assertion = function_assertion(statement, input.symbols.functions.flat)
            .expect("analysis already validates the total-definition assertion")
            .expect("a runtime assertion plan owns an assertion statement");
        let condition = lower_function_expression_scoped(
            construction,
            input.symbols.coordinates,
            input.symbols.functions,
            input.symbols.shapes,
            loop_body.body(),
            input.binders,
            assertion.condition,
        )?;
        let message = lower_function_expression_scoped(
            construction,
            input.symbols.coordinates,
            input.symbols.functions,
            input.symbols.shapes,
            loop_body.body(),
            input.binders,
            assertion.message,
        )?;
        let provenance = dae::DaeProvenance::source(assertion.span)?;
        construction.functions(|functions| {
            functions.assertion_loop_with_level(
                loop_body,
                condition,
                message,
                assertion.level,
                provenance,
            )
        })?;
    }
    Ok(())
}

fn lower_one_total_function_array_definition<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    body: &mut dae::FunctionBody<'dae>,
    input: &TotalArrayDefinition<'_, '_, 'dae>,
    statement: &rumoca_core::Statement,
    plan: &FunctionStatementPlan,
) -> Result<(), dae::DaeConstructionError> {
    let (
        rumoca_core::Statement::Assignment { value, span, .. },
        FunctionStatementPlan::Assignment(assignment),
    ) = (statement, plan)
    else {
        unreachable!("analysis proves total array-definition statements")
    };
    require_proven_function_reads(construction, input.symbols, body, assignment.proven_reads())?;
    let element = lower_function_expression_scoped(
        construction,
        input.symbols.coordinates,
        input.symbols.functions,
        input.symbols.shapes,
        body,
        input.binders,
        value,
    )?;
    let generated = dae::DaeProvenance::generated(
        dae::DaeGeneration::FunctionLoopLowering,
        input.owner.span(),
    )?;
    let array = construction.expressions(|expressions| {
        expressions
            .at(generated)
            .comprehension(input.domain, element)
    })?;
    let provenance = dae::DaeProvenance::source(*span)?;
    let target = function_value_coordinate(input.symbols.coordinates, assignment.target());
    construction.functions(|functions| functions.assign(body, target, array, provenance))
}

pub(super) struct FunctionFold<'scope, 'statement, 'dae> {
    pub(super) domain: dae::DomainId<'dae>,
    pub(super) structured: &'statement StructuredIndexDomain,
    pub(super) binders: &'scope HashMap<VarName, dae::DomainBinderId<'dae>>,
    pub(super) statements: &'statement [rumoca_core::Statement],
    pub(super) plans: &'statement [FunctionStatementPlan],
    pub(super) targets: &'statement [VarName],
    pub(super) iteration_locals: &'statement [VarName],
    pub(super) entry_reads: &'statement [(VarName, Span)],
    pub(super) entry_seeds: &'statement [(VarName, FunctionValueSeed)],
    pub(super) selected_condition: Option<&'statement Expression>,
    pub(super) owner: dae::DaeProvenance,
}

pub(super) fn lower_function_fold<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    mut body: dae::FunctionBody<'dae>,
    input: FunctionFold<'_, '_, 'dae>,
) -> Result<dae::FunctionBody<'dae>, dae::DaeConstructionError> {
    if let Some(condition) = input.selected_condition
        && selected_loop::needs_selected_entry(
            construction,
            symbols,
            &body,
            (input.targets, input.entry_reads),
            input.owner,
        )?
    {
        return selected_loop::lower_selected_parent(construction, symbols, body, condition, input);
    }
    let mut seeds = Vec::new();
    collect_function_sequence_seeds(input.plans, &mut seeds);
    let provenance = dae::DaeProvenance::generated(
        dae::DaeGeneration::FunctionAggregateLowering,
        input.owner.span(),
    )?;
    for (target, seed) in seeds {
        if input.iteration_locals.contains(target) {
            continue;
        }
        let seeded = lower_function_value_seed(construction, seed, input.owner.span())?;
        let target = function_value_coordinate(symbols.coordinates, target);
        construction
            .functions(|functions| functions.assign(&mut body, target, seeded, provenance))?;
    }
    for (target, seed) in input.entry_seeds {
        let value = function_value_coordinate(symbols.coordinates, target);
        match construction
            .functions(|functions| functions.current_definition_id(&body, value, provenance))
        {
            Ok(_) => continue,
            Err(dae::DaeConstructionError::IncompleteDefinition {
                kind: "function value",
                index,
                ..
            }) if index == value.ordinal() => {}
            Err(error) => return Err(error),
        }
        let seeded = lower_function_value_seed(construction, seed, input.owner.span())?;
        construction
            .functions(|functions| functions.assign(&mut body, value, seeded, provenance))?;
    }
    require_proven_function_reads(construction, symbols, &mut body, input.entry_reads)?;
    let target_ids = input
        .targets
        .iter()
        .map(|target| function_value_coordinate(symbols.coordinates, target))
        .collect::<Vec<_>>();
    let iteration_local_ids = input
        .iteration_locals
        .iter()
        .map(|target| function_value_coordinate(symbols.coordinates, target))
        .collect::<Vec<_>>();
    let mut loop_body = construction.functions(|functions| {
        functions.begin_loop_with_iteration_locals(
            body,
            input.domain,
            target_ids,
            iteration_local_ids,
            input.owner,
        )
    })?;
    let owner = input.owner;
    loop_body = selected_loop::lower_child_transition(construction, symbols, loop_body, input)?;
    construction.functions(|functions| functions.finish_loop(loop_body, owner))
}

fn lower_function_loop_statements<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    mut loop_body: dae::FunctionLoop<'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    statements: &[rumoca_core::Statement],
    plans: &[FunctionStatementPlan],
) -> Result<dae::FunctionLoop<'dae>, dae::DaeConstructionError> {
    debug_assert_eq!(statements.len(), plans.len());
    let mut index = 0usize;
    while index < statements.len() {
        if let FunctionStatementPlan::RecordAssembly(assembly) = &plans[index] {
            let count = assembly.statement_count;
            lower_function_loop_record_assembly(
                construction,
                symbols,
                &mut loop_body,
                binders,
                &statements[index..index + count],
                assembly,
            )?;
            index += count;
            continue;
        }
        loop_body = lower_one_function_loop_statement(
            construction,
            symbols,
            loop_body,
            binders,
            &statements[index],
            &plans[index],
        )?;
        index += 1;
    }
    Ok(loop_body)
}

fn lower_one_function_loop_statement<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    mut loop_body: dae::FunctionLoop<'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    statement: &rumoca_core::Statement,
    plan: &FunctionStatementPlan,
) -> Result<dae::FunctionLoop<'dae>, dae::DaeConstructionError> {
    if let FunctionStatementPlan::GeneratedBooleanAssignment {
        selected: Some(selected),
        ..
    } = plan
    {
        return lower_one_function_loop_statement(
            construction,
            symbols,
            loop_body,
            binders,
            &selected.source,
            &selected.plan,
        );
    }
    if lower_loop_action_statement(
        construction,
        symbols,
        &mut loop_body,
        binders,
        (statement, plan),
    )? {
        return Ok(loop_body);
    }
    if lower_loop_multi_output_statement(
        construction,
        symbols,
        &mut loop_body,
        binders,
        statement,
        plan,
    )? {
        return Ok(loop_body);
    }
    if matches!(
        (statement, plan),
        (
            rumoca_core::Statement::For { .. },
            FunctionStatementPlan::For { .. }
        )
    ) {
        return lower_nested_function_loop(
            construction,
            symbols,
            loop_body,
            binders,
            statement,
            plan,
        );
    }
    if matches!(
        (statement, plan),
        (
            rumoca_core::Statement::If { .. },
            FunctionStatementPlan::If { .. } | FunctionStatementPlan::ProvenBranch { .. }
        )
    ) {
        return lower_function_loop_conditional_statement(
            construction,
            symbols,
            loop_body,
            binders,
            statement,
            plan,
        );
    }
    lower_function_loop_assignment(
        construction,
        symbols,
        &mut loop_body,
        binders,
        statement,
        plan,
    )?;
    Ok(loop_body)
}

fn lower_function_loop_conditional_statement<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    loop_body: dae::FunctionLoop<'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    statement: &rumoca_core::Statement,
    plan: &FunctionStatementPlan,
) -> Result<dae::FunctionLoop<'dae>, dae::DaeConstructionError> {
    if let (
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            span,
        },
        FunctionStatementPlan::If {
            branches,
            fallback,
            targets,
            condition_reads,
        },
    ) = (statement, plan)
    {
        return lower_loop_conditional(
            construction,
            symbols,
            loop_body,
            FunctionConditional {
                binders,
                blocks: cond_blocks,
                fallback: else_block.as_deref(),
                branch_plans: branches,
                condition_reads,
                fallback_plans: fallback.as_deref(),
                targets,
                span: *span,
            },
        );
    }
    if let (
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            ..
        },
        FunctionStatementPlan::ProvenBranch {
            selected,
            statements: selected_plans,
        },
    ) = (statement, plan)
    {
        let selected_statements =
            selected_conditional_statements(cond_blocks, else_block.as_deref(), *selected);
        return lower_function_loop_statements(
            construction,
            symbols,
            loop_body,
            binders,
            selected_statements,
            selected_plans,
        );
    }
    unreachable!("a loop conditional plan owns its source conditional")
}

fn lower_loop_multi_output_statement<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    loop_body: &mut dae::FunctionLoop<'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    statement: &rumoca_core::Statement,
    plan: &FunctionStatementPlan,
) -> Result<bool, dae::DaeConstructionError> {
    let (
        rumoca_core::Statement::FunctionCall {
            comp, args, span, ..
        },
        FunctionStatementPlan::MultiOutputCall {
            outputs,
            argument_reads,
        },
    ) = (statement, plan)
    else {
        return Ok(false);
    };
    lower_function_loop_multi_output_call(
        construction,
        symbols,
        loop_body,
        binders,
        FunctionMultiOutputCall {
            callee: comp,
            args,
            span: *span,
            outputs,
            argument_reads,
        },
    )?;
    Ok(true)
}

fn lower_loop_conditional<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    mut loop_body: dae::FunctionLoop<'dae>,
    conditional: FunctionConditional<'_, '_, 'dae>,
) -> Result<dae::FunctionLoop<'dae>, dae::DaeConstructionError> {
    let provenance = dae::DaeProvenance::generated(
        dae::DaeGeneration::FunctionConditionLowering,
        conditional.span,
    )?;
    assert_eq!(conditional.blocks.len(), conditional.branch_plans.len());
    assert_eq!(conditional.blocks.len(), conditional.condition_reads.len());
    let mut owners = Vec::with_capacity(conditional.blocks.len());
    for ((block, plans), reads) in conditional
        .blocks
        .iter()
        .zip(conditional.branch_plans)
        .zip(conditional.condition_reads)
    {
        require_proven_loop_reads(construction, symbols, &mut loop_body, reads)?;
        let condition = lower_function_expression_scoped(
            construction,
            symbols.coordinates,
            symbols.functions,
            symbols.shapes,
            loop_body.body(),
            conditional.binders,
            &block.cond,
        )?;
        let (child, finish) = construction
            .functions(|functions| functions.begin_branch_loop(loop_body, provenance))?;
        let child =
            lower_function_statements(construction, symbols, child, &block.stmts, plans, None)?;
        let (parent, owner) =
            construction.functions(|functions| functions.finish_branch_loop(finish, child))?;
        loop_body = parent;
        owners.push((condition, owner));
    }
    let (child, finish) =
        construction.functions(|functions| functions.begin_branch_loop(loop_body, provenance))?;
    let child = match (conditional.fallback, conditional.fallback_plans) {
        (Some(source), Some(plans)) => {
            lower_function_statements(construction, symbols, child, source, plans, None)?
        }
        (None, None) => child,
        _ => unreachable!("conditional fallback source and plan agree"),
    };
    let (mut loop_body, fallback) =
        construction.functions(|functions| functions.finish_branch_loop(finish, child))?;
    construction.functions(|functions| {
        functions.finish_conditional_loop(&mut loop_body, owners, fallback, provenance)
    })?;
    for name in conditional.targets {
        let target = function_value_coordinate(symbols.coordinates, name);
        if !construction
            .functions(|functions| functions.loop_carries(&loop_body, target, provenance))?
        {
            continue;
        }
        match construction.functions(|functions| {
            functions.current_definition_id(loop_body.body(), target, provenance)
        }) {
            Ok(_) => {}
            Err(dae::DaeConstructionError::IncompleteDefinition {
                kind: "function value",
                index,
                ..
            }) if index == target.ordinal() => {
                construction.functions(|functions| {
                    functions.require_assigned_loop(&mut loop_body, target, provenance)
                })?;
            }
            Err(error) => return Err(error),
        }
    }
    Ok(loop_body)
}

fn lower_nested_function_loop<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    parent: dae::FunctionLoop<'dae>,
    enclosing_binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    statement: &rumoca_core::Statement,
    plan: &FunctionStatementPlan,
) -> Result<dae::FunctionLoop<'dae>, dae::DaeConstructionError> {
    let (
        rumoca_core::Statement::For {
            indices,
            equations,
            span,
        },
        FunctionStatementPlan::For {
            domain,
            binder_spans,
            lowering,
            statements,
            source_depth,
        },
    ) = (statement, plan)
    else {
        unreachable!("a nested function-loop plan owns a for statement")
    };
    if domain.scalar_count().is_ok_and(|count| count == 0) {
        return Ok(parent);
    }
    let owner = dae::DaeProvenance::source(*span)?;
    let domain_provenance = match binder_spans.as_slice() {
        [span] => dae::DaeProvenance::source(*span)?,
        _ => owner,
    };
    let child_domain = construction
        .domains(|domains| domains.nested(parent.domain(), domain.clone(), domain_provenance))?;
    let (child_indices, child_statements) =
        flattened_function_loop_source(indices, equations, *source_depth);
    let child_binders =
        lower_function_binders(construction, child_domain, &child_indices, binder_spans)?;
    let mut binders = enclosing_binders.clone();
    binders.extend(child_binders);
    let mut child_shapes = symbols.shapes.clone();
    for binder in &domain.binders {
        child_shapes.bind_integer_bounds(
            VarName::new(&binder.display_name),
            binder.lower.min(binder.upper),
            binder.lower.max(binder.upper),
        );
    }
    let child_symbols = FunctionSymbols {
        coordinates: symbols.coordinates,
        functions: symbols.functions,
        shapes: &child_shapes,
        binders: &binders,
        domain: Some(child_domain),
    };
    let FunctionLoopLowering::Fold {
        targets,
        iteration_locals,
        selected_entry,
        entry_reads,
        ..
    } = lowering
    else {
        unreachable!("a nested total definition is compacted before fold lowering")
    };
    let input = FunctionFold {
        domain: child_domain,
        structured: domain,
        binders: &binders,
        statements: child_statements,
        plans: statements,
        targets,
        iteration_locals,
        entry_reads,
        entry_seeds: &[],
        selected_condition: None,
        owner,
    };
    if let Some(entry) = selected_entry
        && selected_loop::needs_selected_entry(
            construction,
            symbols,
            parent.body(),
            (targets, entry_reads),
            owner,
        )?
    {
        let condition = selected_loop_entry(entry, *span);
        return selected_loop::lower_selected_child(
            construction,
            symbols,
            parent,
            condition,
            child_symbols,
            input,
        );
    }
    lower_unselected_child_loop(construction, symbols, parent, child_symbols, input)
}

fn lower_unselected_child_loop<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    mut parent: dae::FunctionLoop<'dae>,
    child_symbols: FunctionSymbols<'_, 'dae>,
    input: FunctionFold<'_, '_, 'dae>,
) -> Result<dae::FunctionLoop<'dae>, dae::DaeConstructionError> {
    require_proven_loop_reads(construction, symbols, &mut parent, input.entry_reads)?;
    let target_ids = input
        .targets
        .iter()
        .map(|target| function_value_coordinate(symbols.coordinates, target));
    let iteration_local_ids = input
        .iteration_locals
        .iter()
        .map(|target| function_value_coordinate(symbols.coordinates, target));
    let child = construction.functions(|functions| {
        functions.begin_nested_loop_with_iteration_locals(
            parent,
            input.domain,
            target_ids,
            iteration_local_ids,
            input.owner,
        )
    })?;
    let owner = input.owner;
    let child = selected_loop::lower_child_transition(construction, child_symbols, child, input)?;
    construction.functions(|functions| functions.finish_nested_loop(child, owner))
}

fn lower_function_loop_assertion<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    loop_body: &mut dae::FunctionLoop<'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    statement: &rumoca_core::Statement,
) -> Result<(), dae::DaeConstructionError> {
    let assertion = function_assertion(statement, symbols.functions.flat)
        .expect("analysis already validates the loop assertion")
        .expect("a runtime assertion plan owns an assertion statement");
    let condition = lower_function_expression_scoped(
        construction,
        symbols.coordinates,
        symbols.functions,
        symbols.shapes,
        loop_body.body(),
        binders,
        assertion.condition,
    )?;
    let message = lower_function_expression_scoped(
        construction,
        symbols.coordinates,
        symbols.functions,
        symbols.shapes,
        loop_body.body(),
        binders,
        assertion.message,
    )?;
    let provenance = dae::DaeProvenance::source(assertion.span)?;
    construction.functions(|functions| {
        functions.assertion_loop_with_level(
            loop_body,
            condition,
            message,
            assertion.level,
            provenance,
        )
    })
}

fn lower_function_loop_assignment<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    loop_body: &mut dae::FunctionLoop<'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    statement: &rumoca_core::Statement,
    plan: &FunctionStatementPlan,
) -> Result<(), dae::DaeConstructionError> {
    let (
        rumoca_core::Statement::Assignment { value, span, .. },
        FunctionStatementPlan::Assignment(assignment),
    ) = (statement, plan)
    else {
        unreachable!("function loop analysis admits only checked transition statements")
    };
    let target = function_value_coordinate(symbols.coordinates, assignment.target());
    require_proven_loop_reads(construction, symbols, loop_body, assignment.proven_reads())?;
    let mut value = lower_function_expression_scoped(
        construction,
        symbols.coordinates,
        symbols.functions,
        symbols.shapes,
        loop_body.body(),
        binders,
        value,
    )?;
    let provenance = dae::DaeProvenance::source(*span)?;
    let subscripts = assignment.subscripts();
    if !subscripts.is_empty() {
        require_proven_loop_reads(construction, symbols, loop_body, assignment.base_reads())?;
        value = lower_function_array_update(
            construction,
            FunctionArrayUpdate {
                symbols: LoweringSymbols {
                    coordinates: symbols.coordinates,
                    functions: symbols.functions,
                    shapes: symbols.shapes,
                    function_body: Some(loop_body.body()),
                    values: None,
                    owner_clock: None,
                    source_comprehension_binders: None,
                },
                binders,
                // Analysis proves a loop-carried value already owns every
                // element, so the transition updates its current value.
                base: None,
                target,
                subscripts,
                value,
                provenance,
            },
        )?;
    }
    construction.functions(|functions| functions.assign_loop(loop_body, target, value, provenance))
}

pub(super) fn function_value_coordinate<'dae>(
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    target: &VarName,
) -> dae::FunctionValueId<'dae> {
    let Coordinate::FunctionValue(target) = coordinates[target] else {
        unreachable!("function analysis accepts only mutable function values")
    };
    target
}
