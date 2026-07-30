use super::*;

pub(super) fn definition_type(
    storage: &Storage,
    definition: ExprId<'_>,
    at: DaeProvenance,
) -> Result<u32, DaeConstructionError> {
    storage
        .expressions
        .value_types
        .get(definition.index() as usize)
        .copied()
        .ok_or_else(|| crate::model::unknown("expression", definition.index(), at))
}
pub(super) fn merged_binder_domain<'dae>(
    storage: &Storage,
    expressions: impl IntoIterator<Item = ExprId<'dae>>,
    at: DaeProvenance,
) -> Result<Option<u32>, DaeConstructionError> {
    expressions
        .into_iter()
        .try_fold(None, |domain, expression| {
            merge_binder_domain(
                storage,
                domain,
                storage.expr_binder_domain(expression, at)?,
                at,
            )
        })
}

pub(super) fn merge_binder_domain(
    storage: &Storage,
    lhs: Option<u32>,
    rhs: Option<u32>,
    at: DaeProvenance,
) -> Result<Option<u32>, DaeConstructionError> {
    match (lhs, rhs) {
        (None, domain) | (domain, None) => Ok(domain),
        (Some(lhs), Some(rhs)) if lhs == rhs => Ok(Some(lhs)),
        (Some(lhs), Some(rhs)) if storage.domain_is_ancestor_or_same(lhs, rhs, at)? => {
            Ok(Some(rhs))
        }
        (Some(lhs), Some(rhs)) if storage.domain_is_ancestor_or_same(rhs, lhs, at)? => {
            Ok(Some(lhs))
        }
        (Some(expected), Some(found)) => Err(DaeConstructionError::InvalidBinderScope {
            expected_domain: Some(expected),
            found_domain: found,
            span: at.span(),
        }),
    }
}

pub(super) fn max_variability(
    storage: &Storage,
    expressions: &[ExprId<'_>],
    at: DaeProvenance,
) -> Result<ExpressionVariability, DaeConstructionError> {
    expressions
        .iter()
        .try_fold(ExpressionVariability::Constant, |maximum, expression| {
            Ok(maximum.max(storage.expr_variability(*expression, at)?))
        })
}
