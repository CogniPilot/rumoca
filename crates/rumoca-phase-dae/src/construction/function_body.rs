mod branch_assertions;
mod path_definedness;

use super::*;
use branch_assertions::{
    BranchAssertion, BranchState, guard_branch_assertions, lower_branch_assertion,
};
use path_definedness::lower_path_definedness;
pub(super) use path_definedness::{DefinednessPredicates, PartialTarget};

pub(super) fn lower_generated_boolean_assignment<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    mut body: dae::FunctionBody<'dae>,
    target: &VarName,
    value: &Expression,
    span: Span,
) -> Result<dae::FunctionBody<'dae>, dae::DaeConstructionError> {
    let lowered = lower_function_expression(
        construction,
        symbols.coordinates,
        symbols.functions,
        symbols.shapes,
        &body,
        value,
    )?;
    let target = function_value_coordinate(symbols.coordinates, target);
    let provenance = dae::DaeProvenance::source(span)?;
    construction.functions(|functions| functions.assign(&mut body, target, lowered, provenance))?;
    Ok(body)
}

pub(super) fn lower_integer_reduction<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    body: dae::FunctionBody<'dae>,
    function: &rumoca_core::Function,
    initial_plans: &[FunctionStatementPlan],
    result: &VarName,
    reduction: &FunctionIntegerReduction,
) -> Result<dae::FunctionBody<'dae>, dae::DaeConstructionError> {
    let initial_count = match reduction {
        FunctionIntegerReduction::WhileExclusive => 2,
        FunctionIntegerReduction::ForInclusiveCapped => 1,
    };
    let mut body = lower_function_statements(
        construction,
        symbols,
        body,
        &function.body[..initial_count],
        initial_plans,
        None,
    )?;
    let target = function_value_coordinate(symbols.coordinates, result);
    match reduction {
        FunctionIntegerReduction::WhileExclusive => {
            lower_while_sum(construction, symbols, &mut body, function, target)?;
        }
        FunctionIntegerReduction::ForInclusiveCapped => {
            lower_capped_for_sum(construction, symbols, &mut body, function, target)?;
        }
    }
    Ok(body)
}

fn lower_while_sum<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    body: &mut dae::FunctionBody<'dae>,
    function: &rumoca_core::Function,
    target: dae::FunctionValueId<'dae>,
) -> Result<(), dae::DaeConstructionError> {
    let rumoca_core::Statement::While { block, span } = &function.body[2] else {
        unreachable!("analysis proves a terminal while reduction")
    };
    let Expression::Binary { rhs: bound, .. } = &block.cond else {
        unreachable!("analysis proves an exclusive while bound")
    };
    let rumoca_core::Statement::Assignment {
        value: Expression::Binary {
            rhs: one_source, ..
        },
        ..
    } = &block.stmts[1]
    else {
        unreachable!("analysis proves a unit induction update")
    };
    let owner = dae::DaeProvenance::generated(dae::DaeGeneration::FunctionLoopLowering, *span)?;
    let zero = construction.functions(|functions| functions.read(body, target, owner))?;
    let bound = lower_function_expression(
        construction,
        symbols.coordinates,
        symbols.functions,
        symbols.shapes,
        body,
        bound,
    )?;
    let one = lower_function_expression(
        construction,
        symbols.coordinates,
        symbols.functions,
        symbols.shapes,
        body,
        one_source,
    )?;
    let positive = construction.expressions(|expressions| {
        expressions
            .at(owner)
            .binary(dae::BinaryOperator::Greater, bound, zero)
    })?;
    let count = construction
        .expressions(|expressions| expressions.at(owner).conditional([(positive, bound)], zero))?;
    let value = lower_integer_series(construction, owner, zero, one, count, false)?;
    construction.functions(|functions| functions.assign(body, target, value, owner))
}

fn lower_capped_for_sum<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    body: &mut dae::FunctionBody<'dae>,
    function: &rumoca_core::Function,
    target: dae::FunctionValueId<'dae>,
) -> Result<(), dae::DaeConstructionError> {
    let rumoca_core::Statement::For {
        indices,
        equations,
        span,
    } = &function.body[1]
    else {
        unreachable!("analysis proves a terminal capped for reduction")
    };
    let Expression::Range {
        start: one, end, ..
    } = &indices[0].range
    else {
        unreachable!("analysis proves a unit runtime range")
    };
    let rumoca_core::Statement::If { cond_blocks, .. } = &equations[0] else {
        unreachable!("analysis proves a leading break guard")
    };
    let Expression::Binary { rhs: cap, .. } = &cond_blocks[0].cond else {
        unreachable!("analysis proves a constant break cap")
    };
    let owner = dae::DaeProvenance::generated(dae::DaeGeneration::FunctionLoopLowering, *span)?;
    let zero = construction.functions(|functions| functions.read(body, target, owner))?;
    let one = lower_function_expression(
        construction,
        symbols.coordinates,
        symbols.functions,
        symbols.shapes,
        body,
        one,
    )?;
    let bound = lower_function_expression(
        construction,
        symbols.coordinates,
        symbols.functions,
        symbols.shapes,
        body,
        end,
    )?;
    let cap = lower_function_expression(
        construction,
        symbols.coordinates,
        symbols.functions,
        symbols.shapes,
        body,
        cap,
    )?;
    let empty = construction.expressions(|expressions| {
        expressions
            .at(owner)
            .binary(dae::BinaryOperator::Less, bound, one)
    })?;
    let capped = construction.expressions(|expressions| {
        expressions
            .at(owner)
            .binary(dae::BinaryOperator::Greater, bound, cap)
    })?;
    let count = construction.expressions(|expressions| {
        expressions
            .at(owner)
            .conditional([(empty, zero), (capped, cap)], bound)
    })?;
    let value = lower_integer_series(construction, owner, zero, one, count, true)?;
    construction.functions(|functions| functions.assign(body, target, value, owner))
}

fn lower_integer_series<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    owner: dae::DaeProvenance,
    zero: dae::ExprId<'dae>,
    one: dae::ExprId<'dae>,
    count: dae::ExprId<'dae>,
    inclusive: bool,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let adjacent = construction.expressions(|expressions| {
        expressions.at(owner).binary(
            if inclusive {
                dae::BinaryOperator::Add
            } else {
                dae::BinaryOperator::Subtract
            },
            count,
            one,
        )
    })?;
    let product = construction.expressions(|expressions| {
        expressions
            .at(owner)
            .binary(dae::BinaryOperator::Multiply, count, adjacent)
    })?;
    let two = construction
        .expressions(|expressions| expressions.at(owner).literal(dae::DaeLiteral::Integer(2)))?;
    let quotient = construction.expressions(|expressions| {
        expressions
            .at(owner)
            .binary(dae::BinaryOperator::Divide, product, two)
    })?;
    let sum = construction.expressions(|expressions| {
        expressions
            .at(owner)
            .builtin(dae::PureBuiltin::Integer, [quotient])
    })?;
    construction.expressions(|expressions| {
        expressions
            .at(owner)
            .binary(dae::BinaryOperator::Add, zero, sum)
    })
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
    pub(super) symbols: FunctionSymbols<'scope, 'dae>,
    pub(super) binders: &'scope HashMap<VarName, dae::DomainBinderId<'dae>>,
    pub(super) blocks: &'statement [rumoca_core::StatementBlock],
    pub(super) fallback: Option<&'statement [rumoca_core::Statement]>,
    pub(super) branch_plans: &'statement [Vec<FunctionStatementPlan>],
    pub(super) fallback_plans: Option<&'statement [FunctionStatementPlan]>,
    pub(super) targets: &'statement [VarName],
    /// Targets this conditional leaves path-partial (top-level sequence only).
    pub(super) partial: &'scope [PartialTarget<'dae>],
    pub(super) span: Span,
}

/// Lower one MLS §11.5 function conditional into its checked value owners.
///
/// A branch is an ordinary algorithm section: its assignments run in order, a
/// later assignment reads what an earlier one wrote, and the last write to a
/// value is the one the branch defines. Each branch therefore builds its own
/// value environment first, and the join then owns one conditional expression
/// per value the conditional defines on all of its paths. Returns the
/// definedness predicate of every path-partial target.
pub(super) fn lower_function_conditional<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    body: &mut dae::FunctionBody<'dae>,
    input: FunctionConditional<'_, '_, 'dae>,
) -> Result<Vec<(VarName, dae::ExprId<'dae>)>, dae::DaeConstructionError> {
    let provenance =
        dae::DaeProvenance::generated(dae::DaeGeneration::FunctionConditionLowering, input.span)?;
    let lowered = lower_function_conditional_values(construction, body, input)?;
    for assertion in &lowered.assertions {
        construction.functions(|functions| {
            functions.assertion_with_level(
                body,
                assertion.condition,
                assertion.message,
                assertion.level,
                assertion.provenance,
            )
        })?;
    }
    construction.functions(|functions| {
        functions.assign_conditional_all(
            body,
            &lowered.targets,
            &lowered.conditions,
            &lowered.branches,
            &lowered.fallback,
            provenance,
        )
    })?;
    Ok(lowered.defined)
}

struct LoweredFunctionConditional<'dae> {
    targets: Vec<dae::FunctionValueId<'dae>>,
    conditions: Vec<dae::ExprId<'dae>>,
    branches: Vec<Vec<dae::ExprId<'dae>>>,
    fallback: Vec<dae::ExprId<'dae>>,
    /// Definedness predicate of each path-partial target after the join.
    defined: Vec<(VarName, dae::ExprId<'dae>)>,
    /// Branch assertions guarded by their branch selection, in source order;
    /// the enclosing action owner appends them before the joined commit so
    /// they read the pre-conditional definitions.
    assertions: Vec<BranchAssertion<'dae>>,
}

fn lower_function_conditional_values<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    body: &dae::FunctionBody<'dae>,
    input: FunctionConditional<'_, '_, 'dae>,
) -> Result<LoweredFunctionConditional<'dae>, dae::DaeConstructionError> {
    let conditions = input
        .blocks
        .iter()
        .map(|block| {
            lower_function_expression_scoped(
                construction,
                input.symbols.coordinates,
                input.symbols.functions,
                input.symbols.shapes,
                body,
                input.binders,
                &block.cond,
            )
        })
        .collect::<Result<Vec<_>, _>>()?;
    let mut branch_values = Vec::with_capacity(input.blocks.len());
    for (block, plans) in input.blocks.iter().zip(input.branch_plans) {
        branch_values.push(lower_conditional_branch(
            construction,
            body,
            ConditionalBranch {
                symbols: input.symbols,
                binders: input.binders,
                statements: &block.stmts,
                plans,
            },
        )?);
    }
    let fallback_values = match (input.fallback, input.fallback_plans) {
        (Some(statements), Some(plans)) => Some(lower_conditional_branch(
            construction,
            body,
            ConditionalBranch {
                symbols: input.symbols,
                binders: input.binders,
                statements,
                plans,
            },
        )?),
        (None, None) => None,
        _ => unreachable!("function conditional fallback plan matches source shape"),
    };
    let provenance =
        dae::DaeProvenance::generated(dae::DaeGeneration::FunctionConditionLowering, input.span)?;
    // Correlate every branch value against the shared pre-conditional
    // definitions, then let the DAE constructor derive and commit all joined
    // definitions together. A target's branch value may read a sibling target's
    // pre-conditional definition (`X[i] := value` while `value := value - L·X[k]`),
    // and those reads can be mutual, so no per-target assignment order keeps
    // every read fact current — only an atomic commit does.
    let (targets, branches, fallback) = join_branch_targets(
        construction,
        body,
        &input,
        &branch_values,
        fallback_values.as_ref(),
        provenance,
    )?;
    let mut assertions = Vec::new();
    for (ordinal, branch) in branch_values.iter().enumerate() {
        guard_branch_assertions(
            construction,
            &conditions,
            Some(ordinal),
            &branch.assertions,
            &mut assertions,
        )?;
    }
    if let Some(branch) = &fallback_values {
        guard_branch_assertions(
            construction,
            &conditions,
            None,
            &branch.assertions,
            &mut assertions,
        )?;
    }
    let defined = lower_path_definedness(
        construction,
        &input,
        &conditions,
        &branch_values,
        fallback_values.as_ref(),
        provenance,
    )?;
    Ok(LoweredFunctionConditional {
        targets,
        conditions,
        branches,
        fallback,
        defined,
        assertions,
    })
}

struct ConditionalBranch<'scope, 'statement, 'dae> {
    symbols: FunctionSymbols<'scope, 'dae>,
    binders: &'scope HashMap<VarName, dae::DomainBinderId<'dae>>,
    statements: &'statement [rumoca_core::Statement],
    plans: &'statement [FunctionStatementPlan],
}

/// Build the value every assignment of one branch leaves behind.
///
/// The environment shadows the enclosing body for exactly the values the branch
/// has already written, which is what keeps the source assignment order.
/// Expression IDs are shared DAG nodes, so keeping a branch-local
/// definition here neither repeats its evaluation nor leaks it outside the
/// conditional that owns it.
fn lower_conditional_branch<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    body: &dae::FunctionBody<'dae>,
    input: ConditionalBranch<'_, '_, 'dae>,
) -> Result<BranchState<'dae>, dae::DaeConstructionError> {
    let ConditionalBranch {
        symbols,
        binders,
        statements,
        plans,
    } = input;
    debug_assert_eq!(statements.len(), plans.len());
    let mut state = BranchState::default();
    lower_conditional_statements(
        construction,
        body,
        symbols,
        binders,
        statements,
        plans,
        &mut state,
    )?;
    Ok(state)
}

fn lower_conditional_statements<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    body: &dae::FunctionBody<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    statements: &[rumoca_core::Statement],
    plans: &[FunctionStatementPlan],
    state: &mut BranchState<'dae>,
) -> Result<(), dae::DaeConstructionError> {
    debug_assert_eq!(statements.len(), plans.len());
    let mut index = 0usize;
    while index < statements.len() {
        if let Some(count) = lower_conditional_record_assembly(
            construction,
            body,
            symbols,
            &statements[index..],
            &plans[index],
            &mut state.values,
        )? {
            index += count;
            continue;
        }
        let statement = &statements[index];
        let plan = &plans[index];
        lower_one_conditional_statement(
            construction,
            body,
            symbols,
            binders,
            statement,
            plan,
            state,
        )?;
        index += 1;
    }
    Ok(())
}

fn lower_one_conditional_statement<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    body: &dae::FunctionBody<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    statement: &rumoca_core::Statement,
    plan: &FunctionStatementPlan,
    state: &mut BranchState<'dae>,
) -> Result<(), dae::DaeConstructionError> {
    match (statement, plan) {
        (_, FunctionStatementPlan::ProvenAssertion) => Ok(()),
        (statement, FunctionStatementPlan::RuntimeAssertion) => {
            lower_branch_assertion(construction, body, symbols, binders, statement, state)
        }
        (
            rumoca_core::Statement::Assignment { value, span, .. },
            FunctionStatementPlan::Assignment(assignment),
        ) => lower_conditional_assignment(
            construction,
            body,
            ConditionalAssignment {
                symbols,
                binders,
                assignment,
                value,
                span: *span,
            },
            &mut state.values,
        ),
        (
            rumoca_core::Statement::FunctionCall {
                comp, args, span, ..
            },
            FunctionStatementPlan::MultiOutputCall { outputs },
        ) => lower_conditional_multi_output_call(
            construction,
            body,
            symbols,
            binders,
            FunctionMultiOutputCall {
                callee: comp,
                args,
                span: *span,
                outputs,
            },
            &mut state.values,
        ),
        (
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                span,
            },
            FunctionStatementPlan::If {
                branches,
                fallback,
                targets,
            },
        ) => lower_nested_conditional(
            construction,
            body,
            NestedFunctionConditional {
                symbols,
                binders,
                blocks: cond_blocks,
                fallback: else_block.as_deref(),
                branch_plans: branches,
                fallback_plans: fallback.as_deref(),
                targets,
                span: *span,
            },
            state,
        ),
        (
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                ..
            },
            FunctionStatementPlan::ProvenBranch {
                selected,
                statements,
            },
        ) => lower_selected_conditional(
            construction,
            body,
            symbols,
            binders,
            (cond_blocks, else_block.as_deref(), *selected),
            statements,
            state,
        ),
        _ => unreachable!("analysis accepts only expression-owned statements in a function branch"),
    }
}

fn lower_conditional_record_assembly<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    body: &dae::FunctionBody<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    statements: &[rumoca_core::Statement],
    plan: &FunctionStatementPlan,
    values: &mut HashMap<VarName, dae::ExprId<'dae>>,
) -> Result<Option<usize>, dae::DaeConstructionError> {
    let FunctionStatementPlan::RecordAssembly(assembly) = plan else {
        return Ok(None);
    };
    let count = assembly.statement_count;
    let (_, record, _) =
        lower_function_record_value(construction, symbols, body, &statements[..count], assembly)?;
    values.insert(assembly.target.clone(), record);
    Ok(Some(count))
}

fn lower_selected_conditional<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    body: &dae::FunctionBody<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    conditional: (
        &[rumoca_core::StatementBlock],
        Option<&[rumoca_core::Statement]>,
        Option<usize>,
    ),
    plans: &[FunctionStatementPlan],
    state: &mut BranchState<'dae>,
) -> Result<(), dae::DaeConstructionError> {
    let source = selected_conditional_statements(conditional.0, conditional.1, conditional.2);
    lower_conditional_statements(construction, body, symbols, binders, source, plans, state)
}

fn lower_conditional_multi_output_call<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    body: &dae::FunctionBody<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    call: FunctionMultiOutputCall<'_>,
    values: &mut HashMap<VarName, dae::ExprId<'dae>>,
) -> Result<(), dae::DaeConstructionError> {
    let provenance = dae::DaeProvenance::source(call.span)?;
    let operands = lower_call_operands(
        construction,
        LoweringSymbols {
            coordinates: symbols.coordinates,
            functions: symbols.functions,
            shapes: symbols.shapes,
            function_body: Some(body),
            values: Some(values),
            owner_clock: None,
        },
        binders,
        call.callee,
        call.args,
        provenance,
    )?;
    let selected = call
        .outputs
        .iter()
        .enumerate()
        .filter_map(|(ordinal, output)| output.as_ref().map(|output| (ordinal, output)))
        .collect::<Vec<_>>();
    let results = operands.results(
        construction,
        selected.iter().map(|(ordinal, _)| *ordinal),
        provenance,
    )?;
    for ((_, output), mut value) in selected.into_iter().zip(results) {
        let target = function_value_coordinate(symbols.coordinates, output.target());
        if !output.subscripts().is_empty() {
            // As for a branch assignment: the seed was assigned where the
            // enclosing sequence or loop begins.
            let base = values.get(output.target()).copied();
            value = lower_function_array_update(
                construction,
                FunctionArrayUpdate {
                    symbols: LoweringSymbols {
                        coordinates: symbols.coordinates,
                        functions: symbols.functions,
                        shapes: symbols.shapes,
                        function_body: Some(body),
                        values: Some(values),
                        owner_clock: None,
                    },
                    binders,
                    base,
                    target,
                    subscripts: output.subscripts(),
                    value,
                    provenance,
                },
            )?;
        }
        values.insert(output.target().clone(), value);
    }
    Ok(())
}

struct ConditionalAssignment<'scope, 'statement, 'dae> {
    symbols: FunctionSymbols<'scope, 'dae>,
    binders: &'scope HashMap<VarName, dae::DomainBinderId<'dae>>,
    assignment: &'statement FunctionAssignmentPlan,
    value: &'statement Expression,
    span: Span,
}

fn lower_conditional_assignment<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    body: &dae::FunctionBody<'dae>,
    input: ConditionalAssignment<'_, '_, 'dae>,
    values: &mut HashMap<VarName, dae::ExprId<'dae>>,
) -> Result<(), dae::DaeConstructionError> {
    let ConditionalAssignment {
        symbols,
        binders,
        assignment,
        value,
        span,
    } = input;
    let target = function_value_coordinate(symbols.coordinates, assignment.target());
    let provenance = dae::DaeProvenance::source(span)?;
    let mut lowered = lower_expression_scoped(
        construction,
        LoweringSymbols {
            coordinates: symbols.coordinates,
            functions: symbols.functions,
            shapes: symbols.shapes,
            function_body: Some(body),
            values: Some(values),
            owner_clock: None,
        },
        binders,
        value,
        None,
    )?;
    let subscripts = assignment.subscripts();
    // A branch updates the value in scope: a write earlier in this branch, or
    // else the enclosing definition. An element write's seed is assigned once
    // where its enclosing sequence or compact loop begins
    // (`collect_function_sequence_seeds`); reseeding inside a loop body would
    // discard what earlier iterations wrote.
    let base = values.get(assignment.target()).copied();
    if !subscripts.is_empty() {
        lowered = lower_function_array_update(
            construction,
            FunctionArrayUpdate {
                symbols: LoweringSymbols {
                    coordinates: symbols.coordinates,
                    functions: symbols.functions,
                    shapes: symbols.shapes,
                    function_body: Some(body),
                    values: Some(values),
                    owner_clock: None,
                },
                binders,
                base,
                target,
                subscripts,
                value: lowered,
                provenance,
            },
        )?;
    }
    values.insert(assignment.target().clone(), lowered);
    Ok(())
}

struct NestedFunctionConditional<'scope, 'statement, 'dae> {
    symbols: FunctionSymbols<'scope, 'dae>,
    binders: &'scope HashMap<VarName, dae::DomainBinderId<'dae>>,
    blocks: &'statement [rumoca_core::StatementBlock],
    fallback: Option<&'statement [rumoca_core::Statement]>,
    branch_plans: &'statement [Vec<FunctionStatementPlan>],
    fallback_plans: Option<&'statement [FunctionStatementPlan]>,
    targets: &'statement [VarName],
    span: Span,
}

fn lower_nested_conditional<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    body: &dae::FunctionBody<'dae>,
    input: NestedFunctionConditional<'_, '_, 'dae>,
    state: &mut BranchState<'dae>,
) -> Result<(), dae::DaeConstructionError> {
    let mut conditions = Vec::with_capacity(input.blocks.len());
    for block in input.blocks {
        conditions.push(lower_expression_scoped(
            construction,
            LoweringSymbols {
                coordinates: input.symbols.coordinates,
                functions: input.symbols.functions,
                shapes: input.symbols.shapes,
                function_body: Some(body),
                values: Some(&state.values),
                owner_clock: None,
            },
            input.binders,
            &block.cond,
            None,
        )?);
    }
    let incoming = state.values.clone();
    let mut branch_values = Vec::with_capacity(input.blocks.len());
    for (ordinal, (block, plans)) in input.blocks.iter().zip(input.branch_plans).enumerate() {
        let branch =
            lower_nested_branch(construction, body, &input, &block.stmts, plans, &incoming)?;
        guard_branch_assertions(
            construction,
            &conditions,
            Some(ordinal),
            &branch.assertions,
            &mut state.assertions,
        )?;
        branch_values.push(branch.values);
    }
    let fallback_values = match (input.fallback, input.fallback_plans) {
        (Some(statements), Some(plans)) => {
            let branch =
                lower_nested_branch(construction, body, &input, statements, plans, &incoming)?;
            guard_branch_assertions(
                construction,
                &conditions,
                None,
                &branch.assertions,
                &mut state.assertions,
            )?;
            branch.values
        }
        (None, None) => incoming.clone(),
        _ => unreachable!("nested function conditional fallback plan matches source shape"),
    };
    let provenance =
        dae::DaeProvenance::generated(dae::DaeGeneration::FunctionConditionLowering, input.span)?;
    for target in input.targets {
        let target_id = function_value_coordinate(input.symbols.coordinates, target);
        let mut arms = Vec::with_capacity(branch_values.len());
        for branch in &branch_values {
            arms.push(match branch.get(target) {
                Some(value) => *value,
                None => conditional_incoming_value(
                    construction,
                    body,
                    target_id,
                    target,
                    &incoming,
                    provenance,
                )?,
            });
        }
        let fallback = match fallback_values.get(target) {
            Some(value) => *value,
            None => conditional_incoming_value(
                construction,
                body,
                target_id,
                target,
                &incoming,
                provenance,
            )?,
        };
        let branches = conditions.iter().copied().zip(arms);
        let joined = construction.expressions(|expressions| {
            expressions.at(provenance).conditional(branches, fallback)
        })?;
        state.values.insert(target.clone(), joined);
    }
    Ok(())
}

/// Lower one arm of a nested runtime conditional from the incoming definitions.
fn lower_nested_branch<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    body: &dae::FunctionBody<'dae>,
    input: &NestedFunctionConditional<'_, '_, 'dae>,
    statements: &[rumoca_core::Statement],
    plans: &[FunctionStatementPlan],
    incoming: &HashMap<VarName, dae::ExprId<'dae>>,
) -> Result<BranchState<'dae>, dae::DaeConstructionError> {
    let mut branch = BranchState::entering(incoming);
    lower_conditional_statements(
        construction,
        body,
        input.symbols,
        input.binders,
        statements,
        plans,
        &mut branch,
    )?;
    Ok(branch)
}

fn conditional_incoming_value<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    body: &dae::FunctionBody<'dae>,
    target_id: dae::FunctionValueId<'dae>,
    target: &VarName,
    incoming: &HashMap<VarName, dae::ExprId<'dae>>,
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    match incoming.get(target) {
        Some(value) => Ok(*value),
        None => construction.functions(|functions| functions.read(body, target_id, provenance)),
    }
}

/// Build the aggregate one element-wise function definition starts from.
///
/// MLS §12.4.4 gives an unwritten function value no initial value, so an
/// element write needs an aggregate of the declared shape to update. Analysis
/// only plans a seed once it has proven the algorithm writes every declared
/// element before anything reads the value, which makes the seed a dead value
/// rather than a default: the certificate, not the constant, carries the
/// meaning.
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
    pub(super) binders: &'scope HashMap<VarName, dae::DomainBinderId<'dae>>,
    pub(super) statements: &'statement [rumoca_core::Statement],
    pub(super) plans: &'statement [FunctionStatementPlan],
    pub(super) targets: &'statement [VarName],
    pub(super) iteration_locals: &'statement [VarName],
    pub(super) owner: dae::DaeProvenance,
}

pub(super) fn lower_function_fold<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    mut body: dae::FunctionBody<'dae>,
    input: FunctionFold<'_, '_, 'dae>,
) -> Result<dae::FunctionBody<'dae>, dae::DaeConstructionError> {
    let mut seeds = Vec::new();
    collect_function_sequence_seeds(input.plans, &mut seeds);
    let provenance = dae::DaeProvenance::generated(
        dae::DaeGeneration::FunctionAggregateLowering,
        input.owner.span(),
    )?;
    for (target, seed) in seeds {
        let seeded = lower_function_value_seed(construction, seed, input.owner.span())?;
        let target = function_value_coordinate(symbols.coordinates, target);
        construction
            .functions(|functions| functions.assign(&mut body, target, seeded, provenance))?;
    }
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
    loop_body = lower_function_loop_statements(
        construction,
        symbols,
        loop_body,
        input.binders,
        input.statements,
        input.plans,
    )?;
    construction.functions(|functions| functions.finish_loop(loop_body, input.owner))
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
    if matches!(plan, FunctionStatementPlan::ProvenAssertion) {
        return Ok(loop_body);
    }
    if matches!(plan, FunctionStatementPlan::RuntimeAssertion) {
        lower_function_loop_assertion(construction, symbols, &mut loop_body, binders, statement)?;
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
        },
    ) = (statement, plan)
    {
        return lower_loop_conditional(
            construction,
            symbols,
            loop_body,
            FunctionConditional {
                symbols,
                binders,
                blocks: cond_blocks,
                fallback: else_block.as_deref(),
                branch_plans: branches,
                fallback_plans: fallback.as_deref(),
                targets,
                partial: &[],
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
        FunctionStatementPlan::MultiOutputCall { outputs },
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
    let lowered = lower_function_conditional_values(
        construction,
        loop_body.body(),
        FunctionConditional {
            symbols,
            ..conditional
        },
    )?;
    for assertion in &lowered.assertions {
        construction.functions(|functions| {
            functions.assertion_loop_with_level(
                &mut loop_body,
                assertion.condition,
                assertion.message,
                assertion.level,
                assertion.provenance,
            )
        })?;
    }
    construction.functions(|functions| {
        functions.assign_conditional_all_loop(
            &mut loop_body,
            &lowered.targets,
            &lowered.conditions,
            &lowered.branches,
            &lowered.fallback,
            provenance,
        )
    })?;
    Ok(loop_body)
}

fn lower_function_loop_multi_output_call<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    loop_body: &mut dae::FunctionLoop<'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    call: FunctionMultiOutputCall<'_>,
) -> Result<(), dae::DaeConstructionError> {
    let provenance = dae::DaeProvenance::source(call.span)?;
    let operands = lower_call_operands(
        construction,
        LoweringSymbols {
            coordinates: symbols.coordinates,
            functions: symbols.functions,
            shapes: symbols.shapes,
            function_body: Some(loop_body.body()),
            values: None,
            owner_clock: None,
        },
        binders,
        call.callee,
        call.args,
        provenance,
    )?;
    let selected = call
        .outputs
        .iter()
        .enumerate()
        .filter_map(|(ordinal, output)| output.as_ref().map(|output| (ordinal, output)))
        .collect::<Vec<_>>();
    let results = operands.results(
        construction,
        selected.iter().map(|(ordinal, _)| *ordinal),
        provenance,
    )?;
    for ((_, output), mut value) in selected.into_iter().zip(results) {
        let target = function_value_coordinate(symbols.coordinates, output.target());
        if !output.subscripts().is_empty() {
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
                    },
                    binders,
                    base: None,
                    target,
                    subscripts: output.subscripts(),
                    value,
                    provenance,
                },
            )?;
        }
        construction
            .functions(|functions| functions.assign_loop(loop_body, target, value, provenance))?;
    }
    Ok(())
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
    };
    let FunctionLoopLowering::Fold {
        targets,
        iteration_locals,
    } = lowering
    else {
        unreachable!("a nested total definition is compacted before fold lowering")
    };
    let target_ids = targets
        .iter()
        .map(|target| function_value_coordinate(symbols.coordinates, target))
        .collect::<Vec<_>>();
    let iteration_local_ids = iteration_locals
        .iter()
        .map(|target| function_value_coordinate(symbols.coordinates, target))
        .collect::<Vec<_>>();
    let child = construction.functions(|functions| {
        functions.begin_nested_loop_with_iteration_locals(
            parent,
            child_domain,
            target_ids,
            iteration_local_ids,
            owner,
        )
    })?;
    let child = lower_function_loop_statements(
        construction,
        child_symbols,
        child,
        &binders,
        child_statements,
        statements,
    )?;
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

/// The value one completing branch gives `target`, standing in on a branch
/// that never completes.
///
/// A branch ending in `assert(false, ...)` fails its call (MLS §8.3.7), so the
/// value it would leave is never observed; the analysis certificate admits the
/// read after the conditional on exactly that ground. The select still needs
/// an operand on that path, and any defined value is equivalent there.
fn completed_branch_value<'dae>(
    branch_values: &[BranchState<'dae>],
    fallback_values: Option<&BranchState<'dae>>,
    never_completes: &[bool],
    target: &VarName,
) -> Option<dae::ExprId<'dae>> {
    branch_values
        .iter()
        .chain(fallback_values)
        .zip(never_completes)
        .filter(|(_, diverges)| !**diverges)
        .find_map(|(branch, _)| branch.values.get(target).copied())
}

/// A branch's own value for a target, or the target's pre-conditional
/// definition when the branch leaves it unchanged.
///
/// A path-partial target with no pre-conditional definition takes `dead`, its
/// typed seed literal: MLS §12.4.4 makes any use of the target on this
/// path an error, which the top-level definedness assertion owns, so the
/// operand is never observed.
fn read_unless_defined<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    body: &dae::FunctionBody<'dae>,
    value: Option<dae::ExprId<'dae>>,
    target: dae::FunctionValueId<'dae>,
    dead: Option<dae::ExprId<'dae>>,
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    if let Some(value) = value {
        return Ok(value);
    }
    let read = construction.functions(|functions| functions.read(body, target, provenance));
    match (read, dead) {
        (
            Err(dae::DaeConstructionError::IncompleteDefinition {
                kind: "function value",
                index,
                ..
            }),
            Some(dead),
        ) if index == target.ordinal() => Ok(dead),
        (read, _) => read,
    }
}

/// Joined target operands of one conditional: the target coordinates, each
/// branch's operand per target, and the fallback's operand per target.
type JoinedBranchTargets<'dae> = (
    Vec<dae::FunctionValueId<'dae>>,
    Vec<Vec<dae::ExprId<'dae>>>,
    Vec<dae::ExprId<'dae>>,
);

/// Correlate every branch's value for every target with the shared
/// pre-conditional definitions; a branch that never completes takes a
/// completing branch's value (see `completed_branch_value`).
fn join_branch_targets<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    body: &dae::FunctionBody<'dae>,
    input: &FunctionConditional<'_, '_, 'dae>,
    branch_values: &[BranchState<'dae>],
    fallback_values: Option<&BranchState<'dae>>,
    provenance: dae::DaeProvenance,
) -> Result<JoinedBranchTargets<'dae>, dae::DaeConstructionError> {
    let mut targets = Vec::with_capacity(input.targets.len());
    let mut branches = vec![Vec::with_capacity(input.targets.len()); branch_values.len()];
    let mut fallback = Vec::with_capacity(input.targets.len());
    let never_completes = input
        .blocks
        .iter()
        .map(|block| branch_never_completes(&block.stmts))
        .chain(input.fallback.map(branch_never_completes))
        .collect::<Vec<_>>();
    let fallback_diverges = input.fallback.is_some() && never_completes.last() == Some(&true);
    for target in input.targets {
        let target_id = function_value_coordinate(input.symbols.coordinates, target);
        targets.push(target_id);
        let completed =
            completed_branch_value(branch_values, fallback_values, &never_completes, target);
        let operand = |state: Option<&BranchState<'dae>>, diverges: bool| {
            state
                .and_then(|state| state.values.get(target).copied())
                .or(completed.filter(|_| diverges))
        };
        let dead = input
            .partial
            .iter()
            .find(|partial| &partial.name == target)
            .map(|partial| partial.dead);
        for ((lowered, branch), diverges) in
            branches.iter_mut().zip(branch_values).zip(&never_completes)
        {
            let value = operand(Some(branch), *diverges);
            lowered.push(read_unless_defined(
                construction,
                body,
                value,
                target_id,
                dead,
                provenance,
            )?);
        }
        let value = operand(fallback_values, fallback_diverges);
        fallback.push(read_unless_defined(
            construction,
            body,
            value,
            target_id,
            dead,
            provenance,
        )?);
    }
    Ok((targets, branches, fallback))
}
