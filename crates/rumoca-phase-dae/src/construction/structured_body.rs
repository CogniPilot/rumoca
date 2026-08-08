use super::*;

pub(super) fn lower_structured_body<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: LoweringSymbols<'_, 'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    body: &Expression,
    generated_root: Option<dae::DaeGeneration>,
    _owner_span: Span,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    if let Some(selected) = selected_constant_branch(body, symbols.functions.constants) {
        return lower_expression_scoped(construction, symbols, binders, selected, generated_root);
    }
    let normalized = normalize_conditional_residual(body);
    let body = normalized.as_ref().unwrap_or(body);
    let generated_root = normalized
        .is_some()
        .then_some(dae::DaeGeneration::ConditionLowering)
        .or(generated_root);
    lower_expression_scoped(construction, symbols, binders, body, generated_root)
}

fn selected_constant_branch<'expression>(
    body: &'expression Expression,
    constants: &EvalContext,
) -> Option<&'expression Expression> {
    let Expression::If {
        branches,
        else_branch,
        ..
    } = body
    else {
        return None;
    };
    for (condition, value) in branches {
        match eval_expr(condition, constants)
            .ok()
            .and_then(|value| value.as_bool())
        {
            Some(true) => return Some(value),
            Some(false) => {}
            None => return None,
        }
    }
    Some(else_branch)
}

/// Normalize an equation-level conditional whose every branch defines the
/// same left-hand expression. The equivalence
/// `if c then x-a else x-b = 0` iff `x-(if c then a else b) = 0` exposes one
/// causal definition without scalarizing an aggregate target.
pub(super) fn normalize_conditional_residual(body: &Expression) -> Option<Expression> {
    let Expression::If {
        branches,
        else_branch,
        span,
    } = body
    else {
        return None;
    };
    let (target, fallback) = subtraction_residual(else_branch)?;
    let branches = branches
        .iter()
        .map(|(condition, value)| {
            let (candidate, rhs) = subtraction_residual(value)?;
            candidate
                .semantically_eq_ignoring_spans(target)
                .then_some((condition.clone(), rhs.clone()))
        })
        .collect::<Option<Vec<_>>>()?;
    let selection = Expression::If {
        branches,
        else_branch: Box::new(fallback.clone()),
        span: *span,
    };
    Some(Expression::Binary {
        op: OpBinary::Sub,
        lhs: Box::new(target.clone()),
        rhs: Box::new(selection),
        span: *span,
    })
}

fn subtraction_residual(expression: &Expression) -> Option<(&Expression, &Expression)> {
    let Expression::Binary {
        op: OpBinary::Sub,
        lhs,
        rhs,
        ..
    } = expression
    else {
        return None;
    };
    Some((lhs, rhs))
}
