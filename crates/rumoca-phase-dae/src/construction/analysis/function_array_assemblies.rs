use super::function_ranges::static_integer_expression;
use super::*;

pub(super) fn coalesce_function_array_assemblies(
    statements: &[rumoca_core::Statement],
    plans: &mut [FunctionStatementPlan],
    context: FunctionValidationContext<'_>,
) -> Result<(), ToDaeError> {
    let mut start = 0usize;
    while start < plans.len() {
        let Some((target, target_def_id, direct_members)) =
            direct_prefix(statements, plans, start, context.static_integers)
        else {
            start += 1;
            continue;
        };
        let Some(extent) =
            rank_one_extent(&target, target_def_id, context.function, context.shapes)
        else {
            start += 1;
            continue;
        };
        let direct_count = direct_members.len();
        let loop_index = start + direct_count;
        if direct_count == extent {
            plans[start] =
                FunctionStatementPlan::ArrayAssembly(AnalyzedFunctionArrayAssemblyPlan {
                    target,
                    target_def_id,
                    direct_members,
                    extent,
                    suffix_index: None,
                    loop_plan: None,
                    seed: None,
                });
            for plan in &mut plans[start + 1..loop_index] {
                *plan = FunctionStatementPlan::ArrayAssemblyMember;
            }
            start = loop_index;
            continue;
        }
        let Some(suffix_index) = statements
            .get(loop_index)
            .zip(plans.get(loop_index))
            .and_then(|(statement, plan)| {
                total_suffix_loop(
                    &target,
                    target_def_id,
                    direct_count,
                    extent,
                    statement,
                    plan,
                )
            })
        else {
            start += 1;
            continue;
        };
        let loop_plan = std::mem::replace(
            &mut plans[loop_index],
            FunctionStatementPlan::ArrayAssemblyMember,
        );
        plans[start] = FunctionStatementPlan::ArrayAssembly(AnalyzedFunctionArrayAssemblyPlan {
            target,
            target_def_id,
            direct_members,
            extent,
            suffix_index: Some(suffix_index),
            loop_plan: Some(Box::new(loop_plan)),
            seed: None,
        });
        for plan in &mut plans[start + 1..loop_index] {
            *plan = FunctionStatementPlan::ArrayAssemblyMember;
        }
        start = loop_index + 1;
    }
    Ok(())
}

fn rank_one_extent(
    target: &VarName,
    target_def_id: rumoca_core::DefId,
    function: &rumoca_core::Function,
    shapes: &ShapeEnvironment,
) -> Option<usize> {
    function
        .outputs
        .iter()
        .chain(&function.locals)
        .find(|value| value.name == target.as_str() && value.def_id == Some(target_def_id))?;
    shapes
        .function_value_shape(target, target_def_id)
        .and_then(|shape| match shape.as_slice() {
            [extent] => usize::try_from(*extent).ok(),
            _ => None,
        })
}

fn direct_prefix(
    statements: &[rumoca_core::Statement],
    plans: &[FunctionStatementPlan],
    start: usize,
    static_integers: &HashMap<VarName, i64>,
) -> Option<(
    VarName,
    rumoca_core::DefId,
    Vec<AnalyzedFunctionArrayDirectMember>,
)> {
    let FunctionStatementPlan::Assignment(first) = plans.get(start)? else {
        return None;
    };
    if first.subscripts().len() != 1 {
        return None;
    }
    let target = first.target().clone();
    let target_def_id = first.target_def_id();
    let mut direct_members = Vec::new();
    for (statement, plan) in statements[start..].iter().zip(&plans[start..]) {
        let (
            rumoca_core::Statement::Assignment { value, .. },
            FunctionStatementPlan::Assignment(found),
        ) = (statement, plan)
        else {
            break;
        };
        let Some(position) = direct_members.len().checked_add(1) else {
            break;
        };
        let Some(one_based_index) = i64::try_from(position).ok() else {
            break;
        };
        if found.target() != &target
            || found.target_def_id() != target_def_id
            || assignment_index(found.subscripts(), static_integers) != Some(one_based_index)
            || expression_reads(value, &target, target_def_id)
            || !expression_can_join_eager_aggregate(value)
        {
            break;
        }
        direct_members.push(AnalyzedFunctionArrayDirectMember {
            subscripts: found.subscripts().to_vec().into_boxed_slice(),
            one_based_index,
        });
    }
    (!direct_members.is_empty()).then_some((target, target_def_id, direct_members))
}

fn total_suffix_loop(
    target: &VarName,
    target_def_id: rumoca_core::DefId,
    direct_count: usize,
    extent: usize,
    statement: &rumoca_core::Statement,
    plan: &FunctionStatementPlan,
) -> Option<rumoca_core::ForIndex> {
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
        return None;
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
        return None;
    };
    let lower = direct_count
        .checked_add(1)
        .and_then(|value| i64::try_from(value).ok());
    let upper = i64::try_from(extent).ok();
    (assignment.target() == target
        && assignment.target_def_id() == target_def_id
        && assignment.subscripts().len() == 1
        && direct_count < extent
        && Some(binder.lower) == lower
        && Some(binder.upper) == upper
        && binder.step == 1
        && matches!(
            assignment.subscripts(),
            [subscript] if subscript_is_binder(subscript, &index.ident)
        )
        && !expression_reads(value, target, target_def_id)
        && expression_can_join_eager_aggregate(value))
    .then(|| index.clone())
}

/// Prove that adjacent algorithm assignments may become one eager aggregate
/// value without hiding or reordering a callable effect.
///
/// Even a function treated as pure by its written prefix can reach a bare
/// external function, and assertions remain call-scoped actions. Until a
/// call-graph effect certificate is carried by the plan, retain every callable
/// expression as its source statement. The admitted expression vocabulary is
/// closed recursively, so calls hidden in subscripts, branches, ranges, and
/// comprehensions cannot bypass the boundary.
fn expression_can_join_eager_aggregate(expression: &Expression) -> bool {
    if matches!(
        expression,
        Expression::BuiltinCall { .. }
            | Expression::FunctionCall { .. }
            | Expression::StringConversion { .. }
    ) {
        return false;
    }
    expression_children(expression)
        .into_iter()
        .all(expression_can_join_eager_aggregate)
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

fn expression_reads(
    expression: &Expression,
    target: &VarName,
    target_def_id: rumoca_core::DefId,
) -> bool {
    expression.contains_subexpression(|candidate| {
        let Expression::VarRef { name, .. } = candidate else {
            return false;
        };
        match name.target_def_id() {
            Some(identity) => identity == target_def_id,
            None => name.var_name() == target,
        }
    })
}
