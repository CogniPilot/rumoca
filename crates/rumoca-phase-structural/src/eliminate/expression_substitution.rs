use super::*;

/// Apply substitutions in-order to an expression.
pub fn apply_substitutions_to_expr(
    expr: &Expression,
    substitutions: &[Substitution],
) -> Result<Expression, StructuralError> {
    apply_substitutions_to_expr_with_derivatives(expr, substitutions, None, |_| Ok(None))
}

pub(crate) fn apply_substitutions_to_expr_with_derivatives(
    expr: &Expression,
    substitutions: &[Substitution],
    aggregate_constructor: Option<&Reference>,
    derivative_replacement_for: impl FnMut(&Substitution) -> Result<Option<Expression>, StructuralError>,
) -> Result<Expression, StructuralError> {
    let plan = SubstitutionApplicationPlan::new(substitutions, aggregate_constructor);
    apply_substitutions_to_expr_with_plan(expr, substitutions, &plan, derivative_replacement_for)
}

/// Apply `substitutions` to `expr` in list order, visiting only the positions
/// the plan's candidate index says can possibly match.
///
/// The index is a filter, never the decision: every candidate is still checked
/// with `expr_contains_substitution_target` against the *current* expression,
/// and candidates are consumed in ascending position order, so the result is
/// identical to the naive `for sub in substitutions` loop.
pub(super) fn apply_substitutions_to_expr_with_plan(
    expr: &Expression,
    substitutions: &[Substitution],
    plan: &SubstitutionApplicationPlan,
    mut derivative_replacement_for: impl FnMut(
        &Substitution,
    ) -> Result<Option<Expression>, StructuralError>,
) -> Result<Expression, StructuralError> {
    let mut out = apply_record_field_aggregate_substitutions(expr, plan);
    if substitutions.is_empty() {
        // Aggregate/complex reconstruction replays the plan's groups with an
        // intentionally empty sequential list (see
        // `apply_aggregate_substitutions_to_dae_partitions`). No position can
        // be consumed from an empty list, so a longer index is legal here and
        // only here.
        return Ok(out);
    }
    let index = plan.index();
    ensure_plan_covers_substitutions(expr, index, substitutions)?;
    let mut candidates = index.candidates_for_expression(&out);
    let mut cursor = 0usize;
    while let Some(position) = candidates.position_at(cursor) {
        cursor += 1;
        let Some(sub) = substitutions.get(position as usize) else {
            continue;
        };
        if !expr_contains_substitution_target(&out, sub) {
            continue;
        }
        let derivative_replacement = if expr_contains_derivative_substitution_target(&out, sub) {
            derivative_replacement_for(sub)?
        } else {
            None
        };
        out = SubstituteVarRewriter {
            substitution: sub,
            replacement: &sub.expr,
            replacement_dims: &sub.replacement_dims,
            derivative_replacement: derivative_replacement.as_ref(),
        }
        .rewrite_expression(&out)?;
        // A spliced replacement can expose a target of a *later* substitution.
        index.extend_candidates_after(&sub.expr, position, &mut candidates);
        if let Some(replacement) = derivative_replacement.as_ref() {
            index.extend_candidates_after(replacement, position, &mut candidates);
        }
    }
    Ok(out)
}

/// Reject a plan whose candidate index does not describe exactly the positions
/// of `substitutions`.
///
/// The index is addressed by list position. If it is shorter than the list, the
/// positions past its end are simply never emitted as candidates, so those
/// substitutions are never offered to `expr_contains_substitution_target` and
/// vanish without a word - the "substitution silently skipped" failure mode.
/// SPEC_0008 forbids that silent recovery, so the mismatch is a hard contract
/// violation rather than a debug-only assertion.
fn ensure_plan_covers_substitutions(
    expr: &Expression,
    index: &SubstitutionIndex,
    substitutions: &[Substitution],
) -> Result<(), StructuralError> {
    if index.len() == substitutions.len() {
        return Ok(());
    }
    let reason = format!(
        "substitution plan indexes {} positions but the substitution list has {}; \
         applying it would silently skip substitutions",
        index.len(),
        substitutions.len()
    );
    match expr.span().filter(|span| !span.is_dummy()) {
        Some(span) => Err(StructuralError::ContractViolation { reason, span }),
        None => Err(StructuralError::UnspannedContractViolation { reason }),
    }
}

#[derive(Clone)]
pub(super) struct SubstitutionApplicationPlan {
    aggregate_alias_groups: IndexMap<VarName, AggregateAliasSubstitutionGroup>,
    complex_groups: IndexMap<String, ComplexFieldSubstitutionGroup>,
    index: SubstitutionIndex,
    aggregate_constructor: Option<Reference>,
}

impl SubstitutionApplicationPlan {
    pub(super) fn new(
        substitutions: &[Substitution],
        aggregate_constructor: Option<&Reference>,
    ) -> Self {
        Self {
            aggregate_alias_groups: aggregate_alias_substitution_groups(substitutions),
            complex_groups: complex_field_substitution_groups(substitutions, aggregate_constructor),
            index: SubstitutionIndex::new(substitutions),
            aggregate_constructor: aggregate_constructor.cloned(),
        }
    }

    /// Extend the plan with one more substitution appended to the end of the
    /// list it was built from.
    ///
    /// Both group builders are order-preserving folds over the list, so folding
    /// one more element is exactly equivalent to rebuilding the plan from the
    /// extended list (pinned by `plan_push_matches_full_rebuild`).
    pub(super) fn push(&mut self, substitution: &Substitution) {
        if let Some((base, indices)) = scalar_substitution_target_key(substitution) {
            self.aggregate_alias_groups
                .entry(base.into_var_name())
                .or_default()
                .insert(indices, substitution.expr.clone());
        }
        if let Some(constructor) = self.aggregate_constructor.clone()
            && let Some((base, field)) = split_complex_field_suffix(substitution.var_name.as_str())
        {
            self.complex_groups
                .entry(base.to_string())
                .or_insert_with(|| ComplexFieldSubstitutionGroup::new(constructor))
                .insert(field, substitution.expr.clone());
        }
        self.index.push(substitution);
    }

    pub(super) fn index(&self) -> &SubstitutionIndex {
        &self.index
    }
}

fn apply_record_field_aggregate_substitutions(
    expr: &Expression,
    plan: &SubstitutionApplicationPlan,
) -> Expression {
    if plan.aggregate_alias_groups.is_empty() && plan.complex_groups.is_empty() {
        return expr.clone();
    }
    RecordFieldAggregateRewriter {
        aggregate_alias_groups: &plan.aggregate_alias_groups,
        complex_groups: &plan.complex_groups,
    }
    .rewrite_expression(expr)
}

#[derive(Debug, Clone, Default)]
struct AggregateAliasSubstitutionGroup {
    dims: Vec<usize>,
    replacement_base: Option<Reference>,
    values: IndexMap<Vec<usize>, Expression>,
}

impl AggregateAliasSubstitutionGroup {
    fn insert(&mut self, indices: Vec<usize>, expr: Expression) {
        if indices.len() > self.dims.len() {
            self.dims.resize(indices.len(), 0);
        }
        for (idx, value) in indices.iter().enumerate() {
            self.dims[idx] = self.dims[idx].max(*value);
        }
        self.replacement_base = replacement_aggregate_base(&expr, &indices, &self.replacement_base);
        self.values.insert(indices, expr);
    }

    fn to_replacement_expr(&self, span: rumoca_core::Span) -> Option<Expression> {
        let expected_len = self.expected_len();
        if self.dims.is_empty() || expected_len <= 1 || self.values.len() != expected_len {
            return None;
        }
        if let Some(base) = &self.replacement_base {
            return Some(Expression::VarRef {
                name: base.clone(),
                subscripts: Vec::new(),
                span,
            });
        }
        self.array_expr_at_depth(0, &mut Vec::new(), span)
    }

    fn to_indexed_replacement_expr(
        &self,
        subscripts: &[rumoca_core::Subscript],
        span: rumoca_core::Span,
    ) -> Option<Expression> {
        let expected_len = self.expected_len();
        if self.dims.is_empty() || expected_len <= 1 || self.values.len() != expected_len {
            return None;
        }
        Some(Expression::Index {
            base: Box::new(self.array_expr_at_depth(0, &mut Vec::new(), span)?),
            subscripts: subscripts.to_vec(),
            span,
        })
    }

    fn expected_len(&self) -> usize {
        self.dims.iter().product()
    }

    fn array_expr_at_depth(
        &self,
        depth: usize,
        current: &mut Vec<usize>,
        span: rumoca_core::Span,
    ) -> Option<Expression> {
        if depth >= self.dims.len() {
            return self.values.get(current).cloned();
        }
        let mut elements = Vec::with_capacity(self.dims[depth]);
        for index in 1..=self.dims[depth] {
            current.push(index);
            elements.push(self.array_expr_at_depth(depth + 1, current, span)?);
            current.pop();
        }
        Some(Expression::Array {
            elements,
            is_matrix: depth == 0 && self.dims.len() == 2,
            span,
        })
    }
}

fn replacement_aggregate_base(
    expr: &Expression,
    expected_indices: &[usize],
    existing_base: &Option<Reference>,
) -> Option<Reference> {
    let (base, indices) = scalar_var_ref_key(expr)?;
    (indices == expected_indices
        && existing_base
            .as_ref()
            .is_none_or(|existing| references_same_base(existing, &base)))
    .then_some(base)
}

fn aggregate_alias_substitution_groups(
    substitutions: &[Substitution],
) -> IndexMap<VarName, AggregateAliasSubstitutionGroup> {
    let mut groups = IndexMap::new();
    for substitution in substitutions {
        let Some((base, indices)) = scalar_substitution_target_key(substitution) else {
            continue;
        };
        groups
            .entry(base.into_var_name())
            .or_insert_with(AggregateAliasSubstitutionGroup::default)
            .insert(indices, substitution.expr.clone());
    }
    groups
}

fn scalar_substitution_target_key(substitution: &Substitution) -> Option<(Reference, Vec<usize>)> {
    if let Some(var_ref) = &substitution.var_ref
        && let Some(key) = scalar_var_ref_key_from_reference(var_ref)
    {
        return Some(key);
    }
    let scalar = rumoca_core::parse_scalar_name(substitution.var_name.as_str())?;
    let indices = scalar
        .indices
        .iter()
        .copied()
        .map(usize::try_from)
        .collect::<Result<Vec<_>, _>>()
        .ok()?;
    if indices.iter().all(|index| *index > 0) {
        Some((Reference::new(scalar.base), indices))
    } else {
        None
    }
}

fn scalar_var_ref_key(expr: &Expression) -> Option<(Reference, Vec<usize>)> {
    let Expression::VarRef {
        name, subscripts, ..
    } = expr
    else {
        return None;
    };
    if !subscripts.is_empty() {
        return None;
    }
    scalar_var_ref_key_from_reference(name)
}

pub(super) fn scalar_var_ref_key_from_reference(
    reference: &Reference,
) -> Option<(Reference, Vec<usize>)> {
    let component_ref = reference.component_ref()?;
    let mut base = component_ref.clone();
    let mut indices = Vec::new();
    for part in &mut base.parts {
        indices.extend(positive_usize_subscripts(&part.subs)?);
        part.subs.clear();
    }
    (!indices.is_empty()).then_some((Reference::from_component_reference(base), indices))
}

fn positive_usize_subscripts(subscripts: &[rumoca_core::Subscript]) -> Option<Vec<usize>> {
    subscripts
        .iter()
        .map(positive_usize_subscript)
        .collect::<Option<Vec<_>>>()
}

fn positive_usize_subscript(subscript: &rumoca_core::Subscript) -> Option<usize> {
    let index = match subscript {
        rumoca_core::Subscript::Index { value, .. } => *value,
        rumoca_core::Subscript::Expr { expr, .. } => match expr.as_ref() {
            Expression::Literal {
                value: rumoca_core::Literal::Integer(value),
                ..
            } => *value,
            Expression::Literal {
                value: rumoca_core::Literal::Real(value),
                ..
            } if value.is_finite() && value.fract() == 0.0 => *value as i64,
            _ => return None,
        },
        rumoca_core::Subscript::Colon { .. } => return None,
    };
    usize::try_from(index).ok().filter(|value| *value > 0)
}

fn subscripts_are_static_scalar_indices(subscripts: &[rumoca_core::Subscript]) -> bool {
    !subscripts.is_empty()
        && subscripts
            .iter()
            .all(|subscript| positive_usize_subscript(subscript).is_some())
}

fn references_same_base(lhs: &Reference, rhs: &Reference) -> bool {
    lhs.var_name().id() == rhs.var_name().id()
}

fn reference_has_scalar_indices(reference: &Reference) -> bool {
    scalar_var_ref_key_from_reference(reference).is_some()
}

fn substitution_has_scalar_indices(substitution: &Substitution) -> bool {
    substitution
        .var_ref
        .as_ref()
        .is_some_and(reference_has_scalar_indices)
        || rumoca_core::parse_scalar_name(substitution.var_name.as_str()).is_some()
}

fn reference_complex_field(reference: &Reference) -> Option<&str> {
    reference
        .component_ref()?
        .last_ident()
        .filter(|field| matches!(*field, "re" | "im"))
}

fn substitution_complex_field(substitution: &Substitution) -> Option<&str> {
    substitution
        .var_ref
        .as_ref()
        .and_then(reference_complex_field)
}

fn substitution_indexed_base_matches(name: &Reference, substitution: &Substitution) -> bool {
    let Some(var_ref) = &substitution.var_ref else {
        return false;
    };
    let Some((base, _)) = scalar_var_ref_key_from_reference(var_ref) else {
        return false;
    };
    references_same_base(name, &base)
}

#[derive(Debug, Clone)]
struct ComplexFieldSubstitutionGroup {
    constructor: Reference,
    re: Option<Expression>,
    im: Option<Expression>,
}

impl ComplexFieldSubstitutionGroup {
    fn new(constructor: Reference) -> Self {
        Self {
            constructor,
            re: None,
            im: None,
        }
    }

    fn insert(&mut self, field: &str, expr: Expression) {
        match field {
            "re" => self.re = Some(expr),
            "im" => self.im = Some(expr),
            _ => {}
        }
    }

    fn to_constructor_expr(&self, span: rumoca_core::Span) -> Option<Expression> {
        Some(Expression::FunctionCall {
            name: self.constructor.clone(),
            args: vec![self.re.clone()?, self.im.clone()?],
            is_constructor: true,
            span,
        })
    }
}

fn complex_field_substitution_groups(
    substitutions: &[Substitution],
    aggregate_constructor: Option<&Reference>,
) -> IndexMap<String, ComplexFieldSubstitutionGroup> {
    let mut groups = IndexMap::new();
    let Some(constructor) = aggregate_constructor else {
        return groups;
    };
    for substitution in substitutions {
        let Some((base, field)) = split_complex_field_suffix(substitution.var_name.as_str()) else {
            continue;
        };
        groups
            .entry(base.to_string())
            .or_insert_with(|| ComplexFieldSubstitutionGroup::new(constructor.clone()))
            .insert(field, substitution.expr.clone());
    }
    groups
}

struct RecordFieldAggregateRewriter<'a> {
    aggregate_alias_groups: &'a IndexMap<VarName, AggregateAliasSubstitutionGroup>,
    complex_groups: &'a IndexMap<String, ComplexFieldSubstitutionGroup>,
}

impl RecordFieldAggregateRewriter<'_> {
    fn aggregate_replacement(
        &self,
        name: &Reference,
        subscripts: &[rumoca_core::Subscript],
        span: rumoca_core::Span,
    ) -> Option<Expression> {
        let group = self.aggregate_alias_groups.get(name.var_name())?;
        if subscripts.is_empty() {
            group.to_replacement_expr(span)
        } else if subscripts_are_static_scalar_indices(subscripts) {
            None
        } else {
            group.to_indexed_replacement_expr(subscripts, span)
        }
    }
}

impl ExpressionRewriter for RecordFieldAggregateRewriter<'_> {
    fn rewrite_var_ref_expression(
        &mut self,
        name: &Reference,
        subscripts: &[rumoca_core::Subscript],
        span: rumoca_core::Span,
    ) -> Expression {
        if let Some(replacement) = self.aggregate_replacement(name, subscripts, span) {
            return replacement;
        }
        if !subscripts.is_empty() {
            return self.walk_var_ref_expression(name, subscripts, span);
        }
        self.complex_groups
            .get(name.as_str())
            .and_then(|group| group.to_constructor_expr(span))
            .unwrap_or_else(|| self.walk_var_ref_expression(name, subscripts, span))
    }

    fn walk_field_access_expression(
        &mut self,
        base: &Expression,
        field: &str,
        span: rumoca_core::Span,
    ) -> Expression {
        if let Some((name, subscripts)) = rumoca_ir_dae::indexed_field_var_ref(base, field)
            && let Some(replacement) = self.aggregate_replacement(&name, &subscripts, span)
        {
            return replacement;
        }
        Expression::FieldAccess {
            base: Box::new(self.rewrite_expression(base)),
            field: field.to_owned(),
            span,
        }
    }
}

pub fn resolve_substitutions_in_expr(
    expr: &Expression,
    substitutions: &[Substitution],
) -> Result<Expression, StructuralError> {
    let plan = SubstitutionApplicationPlan::new(substitutions, None);
    resolve_substitutions_in_expr_with_plan(expr, substitutions, &plan)
}

pub fn resolve_substitutions_in_exprs(
    expressions: &mut [Expression],
    substitutions: &[Substitution],
) -> Result<(), StructuralError> {
    let plan = SubstitutionApplicationPlan::new(substitutions, None);
    for expression in expressions {
        *expression = resolve_substitutions_in_expr_with_plan(expression, substitutions, &plan)?;
    }
    Ok(())
}

fn resolve_substitutions_in_expr_with_plan(
    expr: &Expression,
    substitutions: &[Substitution],
    plan: &SubstitutionApplicationPlan,
) -> Result<Expression, StructuralError> {
    let mut out = expr.clone();
    for _ in 0..substitutions.len() {
        let next = apply_substitutions_to_expr_with_plan(&out, substitutions, plan, |_| Ok(None))?;
        if next == out {
            return Ok(out);
        }
        out = next;
    }
    Ok(out)
}

pub(super) fn expr_contains_unsliced_multiscalar_ref(
    expr: &Expression,
    dae: &Dae,
) -> Result<bool, StructuralError> {
    let mut refs = Vec::new();
    collect_var_ref_nodes(expr, &mut refs);
    let scope = DaeVariableScope::new(dae);
    for (name, subscripts) in refs {
        if !subscripts.is_empty() || name.as_str() == "time" {
            continue;
        }
        match scope.shape_for_reference(&name)? {
            DaeVariableShape::Dimensions(dims) => {
                if scalar_count_from_dims(name.var_name(), &dims)? > 1 {
                    return Ok(true);
                }
            }
            DaeVariableShape::StructuredAggregate => {}
        }
    }
    Ok(false)
}

pub(super) fn embedded_alias_indices_for_substitution(
    name: &Reference,
    subscripts: &[rumoca_core::Subscript],
    substitution: &Substitution,
) -> Option<Vec<i64>> {
    if !subscripts.is_empty() || name.var_name().id() == substitution.var_name.id() {
        return None;
    }
    if reference_complex_field(name).is_some() || substitution_complex_field(substitution).is_some()
    {
        return None;
    }
    if substitution_has_scalar_indices(substitution) {
        return None;
    }
    let var_ref = substitution.var_ref.as_ref()?;
    let (name_base, indices) = scalar_var_ref_key_from_reference(name)?;
    if !references_same_base(&name_base, var_ref) {
        return None;
    }
    Some(indices.into_iter().map(|index| index as i64).collect())
}

fn index_replacement_expr(
    replacement: &Expression,
    indices: &[i64],
    fallback_span: rumoca_core::Span,
) -> Result<Expression, StructuralError> {
    let provenance = projection_owner_span(replacement, fallback_span)?;
    let span = provenance.span();
    if indices.is_empty() {
        return Ok(replacement.clone().with_span(span));
    }
    let extra_subscripts = indices
        .iter()
        .copied()
        .map(|index| rumoca_core::Subscript::generated_index_with_provenance(index, provenance))
        .collect::<Vec<_>>();
    Ok(match replacement {
        Expression::VarRef {
            name, subscripts, ..
        } => {
            let mut projected_subscripts = subscripts.clone();
            projected_subscripts.extend(extra_subscripts);
            Expression::VarRef {
                name: name.clone(),
                subscripts: projected_subscripts,
                span,
            }
        }
        _ => Expression::Index {
            base: Box::new(replacement.clone()),
            subscripts: extra_subscripts,
            span,
        },
    })
}

fn index_replacement_expr_with_subscripts(
    replacement: &Expression,
    subscripts: &[rumoca_core::Subscript],
    fallback_span: rumoca_core::Span,
) -> Result<Expression, StructuralError> {
    let span = projection_owner_span(replacement, fallback_span)?.span();
    if subscripts.is_empty() {
        return Ok(replacement.clone().with_span(span));
    }
    Ok(match replacement {
        Expression::VarRef {
            name,
            subscripts: replacement_subscripts,
            ..
        } => {
            let mut projected_subscripts = replacement_subscripts.clone();
            projected_subscripts.extend(subscripts.iter().cloned());
            Expression::VarRef {
                name: name.clone(),
                subscripts: projected_subscripts,
                span,
            }
        }
        _ => Expression::Index {
            base: Box::new(replacement.clone()),
            subscripts: subscripts.to_vec(),
            span,
        },
    })
}

fn projection_owner_span(
    replacement: &Expression,
    fallback_span: rumoca_core::Span,
) -> Result<rumoca_core::ProvenanceSpan, StructuralError> {
    let span = if fallback_span.is_dummy() {
        replacement.span().unwrap_or(fallback_span)
    } else {
        fallback_span
    };
    span.require_provenance("structural substitution projection")
        .map_err(|err| StructuralError::UnspannedContractViolation {
            reason: err.to_string(),
        })
}

fn replacement_indices_for_alias(
    indices: &[i64],
    var_dims: &[i64],
    replacement_dims: &[i64],
) -> Vec<i64> {
    if indices.is_empty() {
        return Vec::new();
    }
    if replacement_dims.is_empty() {
        return if var_dims.is_empty() {
            indices.to_vec()
        } else {
            Vec::new()
        };
    }
    if replacement_dims.len() >= indices.len() {
        return indices.to_vec();
    }
    if var_dims.len() == indices.len() && replacement_dims.len() < var_dims.len() {
        let start = indices.len() - replacement_dims.len();
        return indices[start..].to_vec();
    }
    indices.to_vec()
}

pub(super) fn var_ref_matches_unknown_for_substitution(
    name: &Reference,
    subscripts: &[rumoca_core::Subscript],
    substitution: &Substitution,
) -> bool {
    if name.var_name().id() == substitution.var_name.id() {
        return subscripts.is_empty() || subscripts_all_one(subscripts);
    }

    if subscripts.is_empty() && substitution_indexed_base_matches(name, substitution) {
        return false;
    }

    // Substitution must preserve complex field semantics: do not allow
    // base<->field alias matching here, otherwise `.re/.im` projections can be
    // applied to already-scalar replacement expressions.
    let name_field = reference_complex_field(name);
    let unknown_field = substitution_complex_field(substitution);
    if name_field.is_some() || unknown_field.is_some() {
        return name.var_name().id() == substitution.var_name.id()
            && (subscripts.is_empty() || subscripts_all_one(subscripts));
    }

    if subscripts.is_empty()
        && !reference_has_scalar_indices(name)
        && substitution_has_scalar_indices(substitution)
    {
        return false;
    }

    var_ref_matches_unknown(name, subscripts, &substitution.var_name)
}

pub(super) fn aggregate_subscript_ref_matches_var(
    name: &Reference,
    subscripts: &[rumoca_core::Subscript],
    substitution: &Substitution,
) -> bool {
    !substitution.var_dims.is_empty()
        && !subscripts.is_empty()
        && name.var_name().id() == substitution.var_name.id()
}

pub(super) struct SubstituteVarRewriter<'a> {
    pub(super) substitution: &'a Substitution,
    pub(super) replacement: &'a Expression,
    pub(super) replacement_dims: &'a [i64],
    pub(super) derivative_replacement: Option<&'a Expression>,
}

impl FallibleExpressionRewriter for SubstituteVarRewriter<'_> {
    type Error = StructuralError;

    fn rewrite_expression(&mut self, expr: &Expression) -> Result<Expression, Self::Error> {
        match expr {
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args,
                ..
            } if self.der_call_matches_scalar_substitution(args) => self
                .derivative_replacement
                .cloned()
                .map_or_else(|| self.walk_expression(expr), Ok),
            Expression::BuiltinCall {
                function: BuiltinFunction::Pre | BuiltinFunction::Edge | BuiltinFunction::Change,
                ..
            } => {
                // Preserve event-operator arguments to maintain MLS Appendix B
                // pre/change/edge semantics during symbolic substitution.
                Ok(expr.clone())
            }
            Expression::ArrayComprehension {
                expr: inner,
                indices,
                filter,
                span,
            } => Ok(Expression::ArrayComprehension {
                expr: Box::new(self.rewrite_expression(inner)?),
                indices: indices.clone(),
                filter: filter
                    .as_ref()
                    .map(|filter| self.rewrite_expression(filter).map(Box::new))
                    .transpose()?,
                span: *span,
            }),
            _ => self.walk_expression(expr),
        }
    }

    fn rewrite_var_ref_expression(
        &mut self,
        name: &Reference,
        subscripts: &[rumoca_core::Subscript],
        span: rumoca_core::Span,
    ) -> Result<Expression, Self::Error> {
        if let Some(indices) =
            embedded_alias_indices_for_substitution(name, subscripts, self.substitution)
        {
            // MLS §10.6: if a vector alias is eliminated before scalarization,
            // scalarized references to that alias must map to the same scalar
            // component of the replacement expression.
            let replacement_indices = replacement_indices_for_alias(
                &indices,
                &self.substitution.var_dims,
                self.replacement_dims,
            );
            index_replacement_expr(self.replacement, &replacement_indices, span)
        } else if aggregate_subscript_ref_matches_var(name, subscripts, self.substitution) {
            index_replacement_expr_with_subscripts(self.replacement, subscripts, span)
        } else if var_ref_matches_unknown_for_substitution(name, subscripts, self.substitution) {
            if !subscripts.is_empty() && !self.substitution.var_dims.is_empty() {
                return index_replacement_expr_with_subscripts(self.replacement, subscripts, span);
            }
            Ok(replacement_with_owner_span(self.replacement, span))
        } else {
            self.walk_var_ref_expression(name, subscripts, span)
        }
    }
}

fn replacement_with_owner_span(
    replacement: &Expression,
    owner_span: rumoca_core::Span,
) -> Expression {
    if replacement.span().is_some() || owner_span.is_dummy() {
        replacement.clone()
    } else {
        replacement.clone().with_span(owner_span)
    }
}

impl SubstituteVarRewriter<'_> {
    fn der_call_matches_scalar_substitution(&self, args: &[Expression]) -> bool {
        der_call_matches_scalar_substitution(args, self.substitution)
    }
}

pub(super) fn der_call_matches_scalar_substitution(
    args: &[Expression],
    substitution: &Substitution,
) -> bool {
    if !substitution.var_dims.is_empty() {
        return false;
    }
    let [
        Expression::VarRef {
            name, subscripts, ..
        },
    ] = args
    else {
        return false;
    };
    subscripts.is_empty()
        && var_ref_matches_unknown_for_substitution(name, subscripts, substitution)
}
