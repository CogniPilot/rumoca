use super::*;

#[derive(Clone, Debug, PartialEq)]
pub(in crate::construction) struct ComprehensionPlan {
    pub(in crate::construction) domain: StructuredIndexDomain,
    pub(in crate::construction) binder_spans: Vec<Span>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(in crate::construction) struct ComprehensionKey {
    owner: Span,
    binders: Vec<(String, Span)>,
}

impl ComprehensionKey {
    pub(in crate::construction) fn new(
        owner: Span,
        indices: &[rumoca_core::ComprehensionIndex],
    ) -> Option<Self> {
        indices
            .iter()
            .map(|index| Some((index.name.clone(), index.range.span()?)))
            .collect::<Option<Vec<_>>>()
            .map(|binders| Self { owner, binders })
    }
}

pub(super) fn analyze_comprehensions<'expression>(
    expressions: impl IntoIterator<Item = &'expression Expression>,
    constants: &EvalContext,
) -> Result<HashMap<ComprehensionKey, ComprehensionPlan>, ToDaeError> {
    let mut plans = HashMap::new();
    for expression in expressions {
        analyze_expression(expression, constants, &mut plans)?;
    }
    Ok(plans)
}

fn analyze_expression(
    expression: &Expression,
    constants: &EvalContext,
    plans: &mut HashMap<ComprehensionKey, ComprehensionPlan>,
) -> Result<(), ToDaeError> {
    if let Expression::ArrayComprehension {
        indices,
        filter,
        span,
        ..
    } = expression
    {
        require_span(*span, "array comprehension")?;
        if filter.is_some() {
            return Err(ToDaeError::unsupported_flat(
                "filtered array comprehension",
                "canonical DAE requires an unfiltered rectangular domain",
                *span,
            ));
        }
        if indices.is_empty() {
            return Err(ToDaeError::unsupported_flat(
                "array comprehension domain",
                "a comprehension must declare at least one index",
                *span,
            ));
        }
        let key =
            ComprehensionKey::new(*span, indices).ok_or_else(|| ToDaeError::MissingProvenance {
                owner: "array comprehension range".to_string(),
            })?;
        let plan = comprehension_plan(indices, constants, *span)?;
        if let Some(previous) = plans.insert(key, plan.clone())
            && previous != plan
        {
            return Err(ToDaeError::unsupported_flat(
                "array comprehension provenance",
                format!(
                    "one source span denotes incompatible compact domains: {previous:?} versus {plan:?}"
                ),
                *span,
            ));
        }
    }
    for child in expression_children(expression) {
        analyze_expression(child, constants, plans)?;
    }
    Ok(())
}

fn comprehension_plan(
    indices: &[rumoca_core::ComprehensionIndex],
    constants: &EvalContext,
    span: Span,
) -> Result<ComprehensionPlan, ToDaeError> {
    let mut names = HashSet::new();
    let mut binders = Vec::with_capacity(indices.len());
    let mut binder_spans = Vec::with_capacity(indices.len());
    for (ordinal, index) in indices.iter().enumerate() {
        if !names.insert(&index.name) {
            return Err(ToDaeError::unsupported_flat(
                "array comprehension domain",
                format!("binder `{}` is declared more than once", index.name),
                span,
            ));
        }
        let range_span = expression_span(&index.range)?;
        let (lower, step, upper) = evaluated_range(&index.range, constants)?;
        binders.push(StructuredIndexBinder {
            id: ordinal,
            display_name: index.name.clone(),
            lower,
            upper,
            step,
        });
        binder_spans.push(range_span);
    }
    let domain = StructuredIndexDomain { binders };
    domain.scalar_count().map_err(|error| {
        ToDaeError::unsupported_flat(
            "array comprehension domain",
            format!("invalid compact domain: {error}"),
            span,
        )
    })?;
    Ok(ComprehensionPlan {
        domain,
        binder_spans,
    })
}

fn evaluated_range(
    expression: &Expression,
    constants: &EvalContext,
) -> Result<(i64, i64, i64), ToDaeError> {
    let Expression::Range {
        start, step, end, ..
    } = expression
    else {
        return Err(ToDaeError::unsupported_flat(
            "array comprehension domain",
            "a checked comprehension index requires an explicit range",
            expression_span(expression)?,
        ));
    };
    let lower = evaluated_integer(start, constants)?;
    let step = step
        .as_deref()
        .map(|value| evaluated_integer(value, constants))
        .transpose()?
        .unwrap_or(1);
    let upper = evaluated_integer(end, constants)?;
    Ok((lower, step, upper))
}

fn evaluated_integer(expression: &Expression, constants: &EvalContext) -> Result<i64, ToDaeError> {
    let span = expression_span(expression)?;
    eval_expr(expression, constants)
        .ok()
        .and_then(|value| value.as_integer())
        .ok_or_else(|| {
            ToDaeError::unsupported_flat(
                "array comprehension domain",
                "range bound is not a parameter-evaluable scalar Integer",
                span,
            )
        })
}
