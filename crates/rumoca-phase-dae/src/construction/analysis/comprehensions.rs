use super::*;

#[derive(Clone, Debug, PartialEq)]
pub(in crate::construction) struct ComprehensionPlan {
    pub(in crate::construction) domain: StructuredIndexDomain,
    pub(in crate::construction) binder_spans: Vec<Span>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(in crate::construction) struct ComprehensionKey {
    owner: Span,
    binders: Vec<(VarName, Span, u64)>,
}

impl ComprehensionKey {
    pub(in crate::construction) fn new(
        owner: Span,
        indices: &[rumoca_core::ComprehensionIndex],
    ) -> Option<Self> {
        indices
            .iter()
            .map(|index| {
                Some((
                    VarName::new(&index.name),
                    index.range.span()?,
                    rumoca_core::expression_semantic_fingerprint(&index.range),
                ))
            })
            .collect::<Option<Vec<_>>>()
            .map(|binders| Self { owner, binders })
    }
}

#[derive(Debug)]
struct ComprehensionPlanEntry {
    indices: Vec<rumoca_core::ComprehensionIndex>,
    plan: ComprehensionPlan,
}

/// Exact model-scope comprehension certificates.
///
/// A `Span` proves source provenance, not semantic occurrence identity. One
/// source declaration can be instantiated more than once and each exact Flat
/// occurrence can carry a different rewritten range. The outer key is only a
/// fast fingerprint bucket; every insertion and lookup confirms the complete
/// exact Flat indices (including resolved reference identities) before using a
/// plan. This is the same bucket-plus-equality discipline as the shared
/// expression identity API, so a fingerprint collision cannot merge owners.
#[derive(Debug, Default)]
pub(in crate::construction) struct ComprehensionPlans {
    buckets: HashMap<ComprehensionKey, Vec<ComprehensionPlanEntry>>,
}

impl ComprehensionPlans {
    fn insert(
        &mut self,
        key: ComprehensionKey,
        indices: &[rumoca_core::ComprehensionIndex],
        plan: ComprehensionPlan,
        span: Span,
    ) -> Result<(), ToDaeError> {
        let bucket = self.buckets.entry(key).or_default();
        if let Some(previous) = bucket.iter().find(|entry| entry.indices == indices) {
            if previous.plan != plan {
                return Err(ToDaeError::unsupported_flat(
                    "array comprehension provenance",
                    format!(
                        "one exact Flat occurrence denotes incompatible compact domains: {:?} versus {plan:?}",
                        previous.plan
                    ),
                    span,
                ));
            }
            return Ok(());
        }
        bucket.push(ComprehensionPlanEntry {
            indices: indices.to_vec(),
            plan,
        });
        Ok(())
    }

    pub(in crate::construction) fn get(
        &self,
        key: &ComprehensionKey,
        indices: &[rumoca_core::ComprehensionIndex],
    ) -> Option<&ComprehensionPlan> {
        self.buckets
            .get(key)?
            .iter()
            .find(|entry| entry.indices == indices)
            .map(|entry| &entry.plan)
    }

    #[cfg(test)]
    fn len(&self) -> usize {
        self.buckets.values().map(Vec::len).sum()
    }
}

pub(super) fn analyze_comprehensions<'expression>(
    expressions: impl IntoIterator<Item = &'expression Expression>,
    constants: &EvalContext,
) -> Result<ComprehensionPlans, ToDaeError> {
    let mut plans = ComprehensionPlans::default();
    for expression in expressions {
        analyze_expression(expression, constants, &mut plans)?;
    }
    Ok(plans)
}

fn analyze_expression(
    expression: &Expression,
    constants: &EvalContext,
    plans: &mut ComprehensionPlans,
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
        plans.insert(key, indices, plan, *span)?;
    }
    for child in expression_children(expression) {
        analyze_expression(child, constants, plans)?;
    }
    Ok(())
}

/// The compact domain one MLS §10.4.1 array constructor owns inside a
/// value-proven function specialization.
///
/// # Acceptance contract (SPEC_0008 §"Acceptance Contract Before Rejection")
///
/// MLS §10.4.1 gives `{expr for i in u}` one dimension per iterator, whose
/// extent is the number of values the iterator's range denotes. MLS §12.2 lets
/// a function body write that range over the function's inputs, so inside a
/// specialization that fixes those inputs the range is an ordinary
/// translation-time constant.
///
/// **Accepted.** An explicit `start:end` or `start:step:end` range whose three
/// bounds this specialization settles as exact Integers under the phase's one
/// extent predicate `evaluate_shape_integer` (MLS §4.4.2's evaluable domain) —
/// a proven input value, an argument's proven shape axis through `size`, and
/// exact Integer arithmetic over either — for every iterator. Nesting is
/// accepted through the element expression, so `{{e for j in 1:m} for i in
/// 1:m}` is a rectangular domain per iterator.
///
/// **Typed-rejected.** A filtered comprehension (no rectangular domain), an
/// iterator over an array expression rather than a range, and a bound this
/// specialization does not settle — a runtime value, a loop index, an input the
/// key does not carry by value. Each is reported at the bound that failed; no
/// extent is guessed. A *triangular* domain is in this set: MLS §10.4.1 opens
/// each index as a fresh scalar of the comprehension, so the ranges are read
/// left to right in a scope where an earlier index has a shape and no value,
/// and `{e for i in 1:m, j in 1:i}` is reported at `i` rather than folded to
/// some rectangular over-approximation.
///
/// **Owner.** This function is the only place a function-body comprehension's
/// domain is folded. The shape proof, the lowering, and this validator all read
/// it through the same [`ShapeEnvironment`], so no two of them can disagree
/// about an extent.
///
/// **Evidence.** `rumoca/tests/function_proven_branch_test.rs`:
/// `each_specialization_owns_its_own_constructor_extent` (accepted, and two
/// specializations of one span with different extents),
/// `a_constructor_range_the_specialization_cannot_settle_is_rejected`.
pub(in crate::construction) fn specialized_comprehension_plan(
    indices: &[rumoca_core::ComprehensionIndex],
    filter: Option<&Expression>,
    values: &ShapeEnvironment,
    span: Span,
) -> Result<ComprehensionPlan, ToDaeError> {
    require_span(span, "array comprehension")?;
    if filter.is_some() {
        return Err(ToDaeError::unsupported_flat(
            "filtered array comprehension",
            "canonical DAE requires an unfiltered rectangular domain",
            span,
        ));
    }
    if indices.is_empty() {
        return Err(ToDaeError::unsupported_flat(
            "array comprehension domain",
            "a comprehension must declare at least one index",
            span,
        ));
    }
    let mut scoped = values.clone();
    let mut binders = Vec::with_capacity(indices.len());
    let mut binder_spans = Vec::with_capacity(indices.len());
    for (ordinal, index) in indices.iter().enumerate() {
        let range_span = expression_span(&index.range)?;
        let (lower, step, upper) = proven_comprehension_range(&index.range, &scoped, range_span)?;
        binders.push(StructuredIndexBinder {
            id: ordinal,
            display_name: index.name.clone(),
            lower,
            upper,
            step,
        });
        binder_spans.push(range_span);
        // MLS §10.4.1 opens the index as a fresh scalar of the comprehension, so
        // a later range written over it must read the binder — which has no
        // proven value — and never the value of a shadowed outer coordinate.
        scoped.insert(VarName::new(&index.name), Vec::new());
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

fn proven_comprehension_range(
    expression: &Expression,
    values: &ShapeEnvironment,
    span: Span,
) -> Result<(i64, i64, i64), ToDaeError> {
    let Expression::Range {
        start, step, end, ..
    } = expression
    else {
        return Err(ToDaeError::unsupported_flat(
            "array comprehension domain",
            "a checked comprehension index requires an explicit range",
            span,
        ));
    };
    let lower = proven_comprehension_bound(start, values, "start")?;
    let step = match step.as_deref() {
        Some(step) => proven_comprehension_bound(step, values, "step")?,
        None => 1,
    };
    let upper = proven_comprehension_bound(end, values, "end")?;
    if step == 0 {
        return Err(ToDaeError::unsupported_flat(
            "array comprehension domain",
            "range step cannot be zero",
            span,
        ));
    }
    Ok((lower, step, upper))
}

/// Fold one iterator bound with the extent predicate the phase already owns.
///
/// MLS §10.4.1 requires the iterator's range to be evaluable, and MLS §12.2
/// lets a function write it over the formals — both their values and their
/// shapes. `evaluate_shape_integer` is the single rule this phase folds such an
/// expression with, and it is what a declared dimension (MLS §4.4.2) and a
/// for-statement domain (MLS §11.2.2) already read, so `1:size(b, 1)` and
/// `1:integer(m/2)` are settled here exactly when they are settled there.
fn proven_comprehension_bound(
    expression: &Expression,
    values: &ShapeEnvironment,
    owner: &str,
) -> Result<i64, ToDaeError> {
    let span = expression_span(expression)?;
    evaluate_shape_integer(expression, values).map_err(|error| {
        ToDaeError::unsupported_flat(
            "array comprehension domain",
            format!(
                "range {owner} is not an exact Integer this function specialization settles at \
                 translation time: {error}"
            ),
            span,
        )
    })
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

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::{BytePos, ComprehensionIndex, SourceId};

    fn literal(value: i64, span: Span) -> Expression {
        Expression::Literal {
            value: rumoca_core::Literal::Integer(value),
            span,
        }
    }

    fn comprehension(extent: i64, span: Span) -> Expression {
        Expression::ArrayComprehension {
            expr: Box::new(literal(0, span)),
            indices: vec![ComprehensionIndex {
                name: "i".to_string(),
                range: Expression::Range {
                    start: Box::new(literal(1, span)),
                    step: None,
                    end: Box::new(literal(extent, span)),
                    span,
                },
            }],
            filter: None,
            span,
        }
    }

    /// One declaration source can be instantiated into multiple exact Flat
    /// occurrences. Source provenance stays identical, while specialization
    /// has already rewritten each occurrence's compact range to its own exact
    /// bound; provenance is therefore not a semantic occurrence identity.
    #[test]
    fn distinct_flat_occurrences_may_specialize_one_source_span() {
        let span = Span::new(
            SourceId::from_source_name("fixture.mo"),
            BytePos(10),
            BytePos(20),
        );
        let extent_two = comprehension(2, span);
        let extent_three = comprehension(3, span);

        let plans = analyze_comprehensions([&extent_two, &extent_three], &EvalContext::new())
            .expect("distinct exact occurrences may retain distinct compact domains");

        assert_eq!(plans.len(), 2);
    }

    /// The exact-expression check is authoritative, not the fingerprint: an
    /// analysis contradiction for one exact Flat occurrence remains ED019.
    #[test]
    fn one_exact_flat_occurrence_cannot_change_domains() {
        let span = Span::new(
            SourceId::from_source_name("fixture.mo"),
            BytePos(30),
            BytePos(40),
        );
        let occurrence = comprehension(2, span);
        let Expression::ArrayComprehension { indices, .. } = &occurrence else {
            unreachable!("fixture is a comprehension")
        };
        let key = ComprehensionKey::new(span, indices).expect("fixture has exact provenance");
        let expected = comprehension_plan(indices, &EvalContext::new(), span)
            .expect("fixture has a constant compact domain");
        let mut conflicting = expected.clone();
        conflicting.domain.binders[0].upper = 3;
        let mut plans = ComprehensionPlans::default();
        plans
            .insert(key.clone(), indices, expected, span)
            .expect("first exact certificate is accepted");

        let error = plans
            .insert(key, indices, conflicting, span)
            .expect_err("one exact occurrence cannot own two domains");

        assert!(matches!(
            error,
            ToDaeError::UnsupportedFlatSemantics { feature, .. }
                if feature == "array comprehension provenance"
        ));
    }
}
