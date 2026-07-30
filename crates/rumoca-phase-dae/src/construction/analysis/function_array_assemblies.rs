use super::function_ranges::static_integer_expression;
use super::*;

pub(super) fn coalesce_function_array_assemblies(
    statements: &[rumoca_core::Statement],
    plans: &mut [FunctionStatementPlan],
    context: FunctionValidationContext<'_>,
) -> Result<(), ToDaeError> {
    let mut start = 0usize;
    while start < plans.len() {
        let Some((target, direct_count)) =
            direct_prefix(statements, plans, start, context.static_integers)
        else {
            start += 1;
            continue;
        };
        let loop_index = start + direct_count;
        if loop_index >= plans.len()
            || !total_suffix_loop(
                &target,
                direct_count,
                &statements[loop_index],
                &plans[loop_index],
                context.function,
            )
        {
            start += 1;
            continue;
        }
        let loop_plan = std::mem::replace(
            &mut plans[loop_index],
            FunctionStatementPlan::ArrayAssemblyMember,
        );
        plans[start] = FunctionStatementPlan::ArrayAssembly(FunctionArrayAssemblyPlan {
            target,
            direct_count,
            loop_plan: Box::new(loop_plan),
        });
        for plan in &mut plans[start + 1..loop_index] {
            *plan = FunctionStatementPlan::ArrayAssemblyMember;
        }
        start = loop_index + 1;
    }
    Ok(())
}

fn direct_prefix(
    statements: &[rumoca_core::Statement],
    plans: &[FunctionStatementPlan],
    start: usize,
    static_integers: &HashMap<VarName, i64>,
) -> Option<(VarName, usize)> {
    let FunctionStatementPlan::Assignment(first) = plans.get(start)? else {
        return None;
    };
    if first.subscripts().len() != 1 {
        return None;
    }
    let target = first.target().clone();
    let mut count = 0usize;
    for (statement, plan) in statements[start..].iter().zip(&plans[start..]) {
        let (
            rumoca_core::Statement::Assignment { value, .. },
            FunctionStatementPlan::Assignment(found),
        ) = (statement, plan)
        else {
            break;
        };
        if found.target() != &target
            || assignment_index(found.subscripts(), static_integers)
                != i64::try_from(count + 1).ok()
            || expression_reads(value, &target)
        {
            break;
        }
        count += 1;
    }
    (count > 0).then_some((target, count))
}

fn total_suffix_loop(
    target: &VarName,
    direct_count: usize,
    statement: &rumoca_core::Statement,
    plan: &FunctionStatementPlan,
    function: &rumoca_core::Function,
) -> bool {
    let (
        rumoca_core::Statement::For {
            indices, equations, ..
        },
        FunctionStatementPlan::For {
            domain,
            statements,
            source_depth: 1,
            ..
        },
    ) = (statement, plan)
    else {
        return false;
    };
    let (
        [index],
        [rumoca_core::Statement::Assignment { value, .. }],
        [FunctionStatementPlan::Assignment(assignment)],
        [binder],
    ) = (
        indices.as_slice(),
        equations.as_slice(),
        statements.as_slice(),
        domain.binders.as_slice(),
    )
    else {
        return false;
    };
    let Some(extent) = function
        .outputs
        .iter()
        .chain(&function.locals)
        .find(|value| value.name == target.as_str())
        .and_then(|value| match value.dimensions() {
            [extent] => usize::try_from(*extent).ok(),
            _ => None,
        })
    else {
        return false;
    };
    assignment.target() == target
        && assignment.subscripts().len() == 1
        && direct_count < extent
        && binder.lower == i64::try_from(direct_count + 1).unwrap_or(i64::MAX)
        && binder.upper == i64::try_from(extent).unwrap_or(i64::MIN)
        && binder.step == 1
        && matches!(
            assignment.subscripts(),
            [subscript] if subscript_is_binder(subscript, &index.ident)
        )
        && !expression_reads(value, target)
}

fn assignment_index(
    subscripts: &[rumoca_core::Subscript],
    static_integers: &HashMap<VarName, i64>,
) -> Option<i64> {
    match subscripts {
        [rumoca_core::Subscript::Index { value, .. }] => Some(*value),
        [rumoca_core::Subscript::Expr { expr, .. }] => {
            static_integer_expression(expr, static_integers)
        }
        _ => None,
    }
}

fn expression_reads(expression: &Expression, target: &VarName) -> bool {
    let mut references = Vec::new();
    expression.collect_var_refs(&mut references);
    references.iter().any(|reference| reference == target)
}
