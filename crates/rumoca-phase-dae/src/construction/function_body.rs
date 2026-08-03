use super::*;

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

    body = lower_function_statements(construction, symbols, body, tail, tail_plans)?;
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
    pub(super) blocks: &'statement [rumoca_core::StatementBlock],
    pub(super) fallback: Option<&'statement [rumoca_core::Statement]>,
    pub(super) branch_plans: &'statement [Vec<FunctionStatementPlan>],
    pub(super) fallback_plans: Option<&'statement [FunctionStatementPlan]>,
    pub(super) targets: &'statement [VarName],
    pub(super) span: Span,
}

/// Lower one MLS §11.5 function conditional into its checked value owners.
///
/// A branch is an ordinary algorithm section: its assignments run in order, a
/// later assignment reads what an earlier one wrote, and the last write to a
/// value is the one the branch defines. Each branch therefore builds its own
/// value environment first, and the join then owns one conditional expression
/// per value the conditional defines on all of its paths.
pub(super) fn lower_function_conditional<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    body: &mut dae::FunctionBody<'dae>,
    input: FunctionConditional<'_, '_, 'dae>,
) -> Result<(), dae::DaeConstructionError> {
    let conditions = input
        .blocks
        .iter()
        .map(|block| {
            lower_function_expression(
                construction,
                input.symbols.coordinates,
                input.symbols.functions,
                input.symbols.shapes,
                body,
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
                statements,
                plans,
            },
        )?),
        (None, None) => None,
        _ => unreachable!("function conditional fallback plan matches source shape"),
    };
    let provenance =
        dae::DaeProvenance::generated(dae::DaeGeneration::FunctionConditionLowering, input.span)?;
    for target in input.targets {
        let target_id = function_value_coordinate(input.symbols.coordinates, target);
        let mut values = Vec::with_capacity(branch_values.len());
        for branch in &branch_values {
            values.push(match branch.get(target) {
                Some(value) => *value,
                None => construction
                    .functions(|functions| functions.read(body, target_id, provenance))?,
            });
        }
        let fallback = match fallback_values
            .as_ref()
            .and_then(|values| values.get(target))
        {
            Some(value) => *value,
            None => {
                construction.functions(|functions| functions.read(body, target_id, provenance))?
            }
        };
        let branches = conditions.iter().copied().zip(values);
        let value = construction.expressions(|expressions| {
            expressions.at(provenance).conditional(branches, fallback)
        })?;
        construction.functions(|functions| functions.assign(body, target_id, value, provenance))?;
    }
    Ok(())
}

struct ConditionalBranch<'scope, 'statement, 'dae> {
    symbols: FunctionSymbols<'scope, 'dae>,
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
    body: &mut dae::FunctionBody<'dae>,
    input: ConditionalBranch<'_, '_, 'dae>,
) -> Result<HashMap<VarName, dae::ExprId<'dae>>, dae::DaeConstructionError> {
    let ConditionalBranch {
        symbols,
        statements,
        plans,
    } = input;
    debug_assert_eq!(statements.len(), plans.len());
    let mut values = HashMap::new();
    lower_conditional_statements(construction, body, symbols, statements, plans, &mut values)?;
    Ok(values)
}

fn lower_conditional_statements<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    body: &mut dae::FunctionBody<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    statements: &[rumoca_core::Statement],
    plans: &[FunctionStatementPlan],
    values: &mut HashMap<VarName, dae::ExprId<'dae>>,
) -> Result<(), dae::DaeConstructionError> {
    debug_assert_eq!(statements.len(), plans.len());
    for (statement, plan) in statements.iter().zip(plans) {
        match (statement, plan) {
            (_, FunctionStatementPlan::ProvenAssertion) => {}
            (_, FunctionStatementPlan::RuntimeAssertion) => {
                unreachable!("runtime conditional assertions are rejected during planning")
            }
            (
                rumoca_core::Statement::Assignment { value, span, .. },
                FunctionStatementPlan::Assignment(assignment),
            ) => lower_conditional_assignment(
                construction,
                body,
                symbols,
                assignment,
                value,
                *span,
                values,
            )?,
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
                    blocks: cond_blocks,
                    fallback: else_block.as_deref(),
                    branch_plans: branches,
                    fallback_plans: fallback.as_deref(),
                    targets,
                    span: *span,
                },
                values,
            )?,
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
            ) => {
                let source =
                    selected_conditional_statements(cond_blocks, else_block.as_deref(), *selected);
                lower_conditional_statements(
                    construction,
                    body,
                    symbols,
                    source,
                    statements,
                    values,
                )?;
            }
            _ => unreachable!(
                "analysis accepts only expression-owned statements in a function branch"
            ),
        }
    }
    Ok(())
}

fn lower_conditional_assignment<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    body: &dae::FunctionBody<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    assignment: &FunctionAssignmentPlan,
    value: &Expression,
    span: Span,
    values: &mut HashMap<VarName, dae::ExprId<'dae>>,
) -> Result<(), dae::DaeConstructionError> {
    let binders = HashMap::new();
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
        &binders,
        value,
        None,
    )?;
    let subscripts = assignment.subscripts();
    let base = conditional_assignment_base(construction, assignment, span, values)?;
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
                binders: &binders,
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

fn conditional_assignment_base<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    assignment: &FunctionAssignmentPlan,
    span: Span,
    values: &mut HashMap<VarName, dae::ExprId<'dae>>,
) -> Result<Option<dae::ExprId<'dae>>, dae::DaeConstructionError> {
    if assignment.subscripts().is_empty() {
        return Ok(None);
    }
    if let Some(seed) = assignment.seed() {
        let seeded = lower_function_value_seed(construction, seed, span)?;
        values.insert(assignment.target().clone(), seeded);
    }
    Ok(values.get(assignment.target()).copied())
}

struct NestedFunctionConditional<'scope, 'statement, 'dae> {
    symbols: FunctionSymbols<'scope, 'dae>,
    blocks: &'statement [rumoca_core::StatementBlock],
    fallback: Option<&'statement [rumoca_core::Statement]>,
    branch_plans: &'statement [Vec<FunctionStatementPlan>],
    fallback_plans: Option<&'statement [FunctionStatementPlan]>,
    targets: &'statement [VarName],
    span: Span,
}

fn lower_nested_conditional<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    body: &mut dae::FunctionBody<'dae>,
    input: NestedFunctionConditional<'_, '_, 'dae>,
    values: &mut HashMap<VarName, dae::ExprId<'dae>>,
) -> Result<(), dae::DaeConstructionError> {
    let binders = HashMap::new();
    let mut conditions = Vec::with_capacity(input.blocks.len());
    for block in input.blocks {
        conditions.push(lower_expression_scoped(
            construction,
            LoweringSymbols {
                coordinates: input.symbols.coordinates,
                functions: input.symbols.functions,
                shapes: input.symbols.shapes,
                function_body: Some(body),
                values: Some(values),
                owner_clock: None,
            },
            &binders,
            &block.cond,
            None,
        )?);
    }
    let incoming = values.clone();
    let mut branch_values = Vec::with_capacity(input.blocks.len());
    for (block, plans) in input.blocks.iter().zip(input.branch_plans) {
        let mut branch = incoming.clone();
        lower_conditional_statements(
            construction,
            body,
            input.symbols,
            &block.stmts,
            plans,
            &mut branch,
        )?;
        branch_values.push(branch);
    }
    let fallback_values = match (input.fallback, input.fallback_plans) {
        (Some(statements), Some(plans)) => {
            let mut branch = incoming.clone();
            lower_conditional_statements(
                construction,
                body,
                input.symbols,
                statements,
                plans,
                &mut branch,
            )?;
            branch
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
        values.insert(target.clone(), joined);
    }
    Ok(())
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
    let binders = seed
        .dimensions
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
    let literal = match seed.scalar {
        dae::ScalarType::Real => dae::DaeLiteral::Real(0.0),
        dae::ScalarType::Integer => dae::DaeLiteral::Integer(0),
        dae::ScalarType::Boolean => dae::DaeLiteral::Boolean(false),
        dae::ScalarType::String | dae::ScalarType::Enumeration | dae::ScalarType::Record => {
            unreachable!("analysis seeds only numeric and Boolean aggregates")
        }
    };
    let element =
        construction.expressions(|expressions| expressions.at(provenance).literal(literal))?;
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
    body: &mut dae::FunctionBody<'dae>,
    input: TotalArrayDefinition<'_, '_, 'dae>,
) -> Result<(), dae::DaeConstructionError> {
    let (
        [rumoca_core::Statement::Assignment { value, span, .. }],
        [FunctionStatementPlan::Assignment(assignment)],
    ) = (input.statements, input.plans)
    else {
        unreachable!("analysis proves one total array-definition statement")
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
    pub(super) owner: dae::DaeProvenance,
}

pub(super) fn lower_function_fold<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    body: dae::FunctionBody<'dae>,
    input: FunctionFold<'_, '_, 'dae>,
) -> Result<dae::FunctionBody<'dae>, dae::DaeConstructionError> {
    let target_ids = input
        .targets
        .iter()
        .map(|target| function_value_coordinate(symbols.coordinates, target))
        .collect::<Vec<_>>();
    let mut loop_body = construction
        .functions(|functions| functions.begin_loop(body, input.domain, target_ids, input.owner))?;
    lower_function_loop_statements(
        construction,
        symbols,
        &mut loop_body,
        input.binders,
        input.statements,
        input.plans,
    )?;
    construction.functions(|functions| functions.finish_loop(loop_body, input.owner))
}

fn lower_function_loop_statements<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    loop_body: &mut dae::FunctionLoop<'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    statements: &[rumoca_core::Statement],
    plans: &[FunctionStatementPlan],
) -> Result<(), dae::DaeConstructionError> {
    debug_assert_eq!(statements.len(), plans.len());
    for (statement, plan) in statements.iter().zip(plans) {
        let (
            rumoca_core::Statement::Assignment { value, span, .. },
            FunctionStatementPlan::Assignment(assignment),
        ) = (statement, plan)
        else {
            unreachable!("nested function loops are rejected during analysis")
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
        construction
            .functions(|functions| functions.assign_loop(loop_body, target, value, provenance))?;
    }
    Ok(())
}

pub(super) fn flattened_function_loop<'statement>(
    indices: &'statement [rumoca_core::ForIndex],
    equations: &'statement [rumoca_core::Statement],
    source_depth: usize,
) -> (
    Vec<&'statement rumoca_core::ForIndex>,
    &'statement [rumoca_core::Statement],
) {
    let mut flattened = indices.iter().collect::<Vec<_>>();
    let mut statements = equations;
    for _ in 1..source_depth {
        let [
            rumoca_core::Statement::For {
                indices, equations, ..
            },
        ] = statements
        else {
            unreachable!("function analysis accepts only perfect nested loops")
        };
        flattened.extend(indices);
        statements = equations;
    }
    (flattened, statements)
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
