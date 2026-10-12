//! MLS §11.2.2: integer `while` and capped `for` sums lowered to closed form.

use super::*;

pub(in crate::construction) fn lower_integer_reduction<'dae>(
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
