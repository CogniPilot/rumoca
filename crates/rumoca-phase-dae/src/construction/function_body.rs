use super::*;

pub(super) fn lower_integer_reduction<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    body: &mut dae::FunctionBody<'dae>,
    function: &rumoca_core::Function,
    initial_plans: &[FunctionStatementPlan],
    result: &VarName,
    reduction: &FunctionIntegerReduction,
) -> Result<(), dae::DaeConstructionError> {
    let initial_count = match reduction {
        FunctionIntegerReduction::WhileExclusive => 2,
        FunctionIntegerReduction::ForInclusiveCapped => 1,
    };
    lower_function_statements(
        construction,
        symbols,
        body,
        &function.body[..initial_count],
        initial_plans,
    )?;
    let target = function_value_coordinate(symbols.coordinates, result);
    match reduction {
        FunctionIntegerReduction::WhileExclusive => {
            lower_while_sum(construction, symbols, body, function, target)
        }
        FunctionIntegerReduction::ForInclusiveCapped => {
            lower_capped_for_sum(construction, symbols, body, function, target)
        }
    }
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
    body: &mut dae::FunctionBody<'dae>,
    function: &rumoca_core::Function,
    branch_plans: &[Vec<FunctionStatementPlan>],
    tail_plans: &[FunctionStatementPlan],
    targets: &[VarName],
) -> Result<(), dae::DaeConstructionError> {
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
                body,
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
                        body,
                        &block.stmts[..block.stmts.len() - 1],
                        plans,
                        target,
                    )
                })
                .collect::<Result<Vec<_>, _>>()
        })
        .collect::<Result<Vec<_>, _>>()?;

    lower_function_statements(construction, symbols, body, tail, tail_plans)?;
    let provenance =
        dae::DaeProvenance::generated(dae::DaeGeneration::FunctionConditionLowering, *span)?;
    for (target, returned) in targets.iter().zip(returned) {
        let target = function_value_coordinate(symbols.coordinates, target);
        let fallback =
            construction.functions(|functions| functions.read(body, target, provenance))?;
        let branches = conditions.iter().copied().zip(returned);
        let value = construction.expressions(|expressions| {
            expressions.at(provenance).conditional(branches, fallback)
        })?;
        construction.functions(|functions| functions.assign(body, target, value, provenance))?;
    }
    Ok(())
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
                FunctionStatementPlan::Assignment { target, .. },
            ) if target == selected => Some(value),
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
    let provenance =
        dae::DaeProvenance::generated(dae::DaeGeneration::FunctionConditionLowering, input.span)?;
    for target in input.targets {
        let values = input
            .blocks
            .iter()
            .zip(input.branch_plans)
            .map(|(block, plans)| {
                lower_conditional_branch_value(
                    construction,
                    body,
                    input.symbols,
                    &block.stmts,
                    plans,
                    target,
                )
            })
            .collect::<Result<Vec<_>, _>>()?;
        let target_id = function_value_coordinate(input.symbols.coordinates, target);
        let fallback = match (input.fallback, input.fallback_plans) {
            (Some(statements), Some(plans)) => lower_conditional_branch_value(
                construction,
                body,
                input.symbols,
                statements,
                plans,
                target,
            )?,
            (None, None) => {
                construction.functions(|functions| functions.read(body, target_id, provenance))?
            }
            _ => unreachable!("function conditional fallback plan matches source shape"),
        };
        let branches = conditions.iter().copied().zip(values);
        let value = construction.expressions(|expressions| {
            expressions.at(provenance).conditional(branches, fallback)
        })?;
        construction.functions(|functions| functions.assign(body, target_id, value, provenance))?;
    }
    Ok(())
}

fn lower_conditional_branch_value<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    body: &mut dae::FunctionBody<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    statements: &[rumoca_core::Statement],
    plans: &[FunctionStatementPlan],
    selected: &VarName,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let assignment = conditional_branch_assignment(statements, plans, selected);
    let target = function_value_coordinate(symbols.coordinates, selected);
    let mut value = lower_function_expression(
        construction,
        symbols.coordinates,
        symbols.functions,
        symbols.shapes,
        body,
        assignment.value,
    )?;
    let provenance = dae::DaeProvenance::source(assignment.span)?;
    let subscripts = &assignment
        .component
        .parts
        .last()
        .expect("function assignment target was validated")
        .subs;
    debug_assert_eq!(assignment.subscript_count, subscripts.len());
    if !subscripts.is_empty() {
        let binders = HashMap::new();
        value = lower_function_array_update(
            construction,
            FunctionArrayUpdate {
                symbols: LoweringSymbols {
                    coordinates: symbols.coordinates,
                    functions: symbols.functions,
                    shapes: symbols.shapes,
                    function_body: Some(body),
                    values: None,
                },
                binders: &binders,
                target,
                subscripts,
                value,
                provenance,
            },
        )?;
    }
    Ok(value)
}

struct ConditionalBranchAssignment<'statement> {
    component: &'statement rumoca_core::ComponentReference,
    value: &'statement Expression,
    span: Span,
    subscript_count: usize,
}

fn conditional_branch_assignment<'statement>(
    statements: &'statement [rumoca_core::Statement],
    plans: &[FunctionStatementPlan],
    selected: &VarName,
) -> ConditionalBranchAssignment<'statement> {
    statements
        .iter()
        .zip(plans)
        .find_map(|(statement, plan)| match (statement, plan) {
            (
                rumoca_core::Statement::Assignment {
                    comp, value, span, ..
                },
                FunctionStatementPlan::Assignment {
                    target,
                    subscript_count,
                },
            ) if target == selected => Some(ConditionalBranchAssignment {
                component: comp,
                value,
                span: *span,
                subscript_count: *subscript_count,
            }),
            _ => None,
        })
        .expect("analysis proves every function branch defines each selected value")
}

pub(super) struct TotalArrayDefinition<'scope, 'statement, 'dae> {
    pub(super) symbols: FunctionSymbols<'scope, 'dae>,
    pub(super) domain: dae::DomainId<'dae>,
    pub(super) binders: &'scope HashMap<VarName, dae::DomainBinderId<'dae>>,
    pub(super) statements: &'statement [rumoca_core::Statement],
    pub(super) plans: &'statement [FunctionStatementPlan],
    pub(super) target: &'statement VarName,
    pub(super) owner: dae::DaeProvenance,
}

pub(super) fn lower_total_function_array_definition<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    body: &mut dae::FunctionBody<'dae>,
    input: TotalArrayDefinition<'_, '_, 'dae>,
) -> Result<(), dae::DaeConstructionError> {
    let (
        [rumoca_core::Statement::Assignment { value, span, .. }],
        [
            FunctionStatementPlan::Assignment {
                target: planned_target,
                ..
            },
        ],
    ) = (input.statements, input.plans)
    else {
        unreachable!("analysis proves one total array-definition statement")
    };
    debug_assert_eq!(input.target, planned_target);
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
    let assignment = dae::DaeProvenance::source(*span)?;
    let target = function_value_coordinate(input.symbols.coordinates, input.target);
    construction.functions(|functions| functions.assign(body, target, array, assignment))
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
    body: &mut dae::FunctionBody<'dae>,
    input: FunctionFold<'_, '_, 'dae>,
) -> Result<(), dae::DaeConstructionError> {
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
    construction.functions(|functions| functions.finish_loop(body, loop_body, input.owner))
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
            rumoca_core::Statement::Assignment {
                comp, value, span, ..
            },
            FunctionStatementPlan::Assignment {
                target,
                subscript_count,
            },
        ) = (statement, plan)
        else {
            unreachable!("nested function loops are rejected during analysis")
        };
        let target = function_value_coordinate(symbols.coordinates, target);
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
        let subscripts = &comp
            .parts
            .last()
            .expect("function assignment target was validated")
            .subs;
        debug_assert_eq!(*subscript_count, subscripts.len());
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
                    },
                    binders,
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
