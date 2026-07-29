use super::*;

struct ConditionalDerivativeResidual<'expression> {
    derivative: &'expression Expression,
    branches: Vec<(&'expression Expression, &'expression Expression)>,
    fallback: &'expression Expression,
}

pub(super) fn lower_structured_body<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: LoweringSymbols<'_, 'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    body: &Expression,
    generated_root: Option<dae::DaeGeneration>,
    owner_span: Span,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    if let Some(selected) = selected_constant_branch(body, symbols.functions.constants) {
        return lower_expression_scoped(construction, symbols, binders, selected, generated_root);
    }
    let Some(residual) = conditional_derivative_residual(body) else {
        return lower_expression_scoped(construction, symbols, binders, body, generated_root);
    };
    lower_conditional_derivative_residual(construction, symbols, binders, residual, owner_span)
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

fn lower_conditional_derivative_residual<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: LoweringSymbols<'_, 'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    residual: ConditionalDerivativeResidual<'_>,
    owner_span: Span,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let generated =
        dae::DaeProvenance::generated(dae::DaeGeneration::ArrayEquationProjection, owner_span)?;
    let derivative = lower_expression_scoped(
        construction,
        symbols,
        binders,
        residual.derivative,
        Some(dae::DaeGeneration::ArrayEquationProjection),
    )?;
    let mut branches = Vec::with_capacity(residual.branches.len());
    for (condition, rhs) in residual.branches {
        branches.push((
            lower_expression_scoped(construction, symbols, binders, condition, None)?,
            lower_expression_scoped(construction, symbols, binders, rhs, None)?,
        ));
    }
    let fallback =
        lower_expression_scoped(construction, symbols, binders, residual.fallback, None)?;
    let rhs = construction
        .expressions(|expressions| expressions.at(generated).conditional(branches, fallback))?;
    construction.expressions(|expressions| {
        expressions
            .at(generated)
            .binary(dae::BinaryOperator::Subtract, derivative, rhs)
    })
}

fn conditional_derivative_residual(body: &Expression) -> Option<ConditionalDerivativeResidual<'_>> {
    let Expression::If {
        branches,
        else_branch,
        ..
    } = body
    else {
        return None;
    };
    let (derivative, fallback) = explicit_derivative_residual(else_branch)?;
    let branches = branches
        .iter()
        .map(|(condition, value)| {
            let (candidate, rhs) = explicit_derivative_residual(value)?;
            candidate
                .semantically_eq_ignoring_spans(derivative)
                .then_some((condition, rhs))
        })
        .collect::<Option<Vec<_>>>()?;
    Some(ConditionalDerivativeResidual {
        derivative,
        branches,
        fallback,
    })
}

fn explicit_derivative_residual(expression: &Expression) -> Option<(&Expression, &Expression)> {
    let Expression::Binary {
        op: OpBinary::Sub,
        lhs,
        rhs,
        ..
    } = expression
    else {
        return None;
    };
    matches!(
        lhs.as_ref(),
        Expression::BuiltinCall {
            function: BuiltinFunction::Der,
            ..
        }
    )
    .then_some((lhs, rhs))
}
