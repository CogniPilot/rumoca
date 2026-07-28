//! The compact index box behind a candidate stencil domain.
//!
//! The shrinking-prefix search proposes a prefix of a family's row-major
//! enumeration as a stencil domain. A tensor node can only iterate a cross
//! product of arithmetic sequences, so the proposal is admissible exactly when
//! those tuples ARE such a box -- rebuilt here per dimension and then verified
//! by re-enumeration. A prefix that straddles a row boundary is not, and
//! declines as `NonCompactCandidateDomain`.

use indexmap::IndexSet;

use super::{
    stencil_contract_violation, stencil_vec_with_capacity, structured_domain_index_tuples,
};
use crate::lower::LowerError;

pub(super) fn compact_domain_from_tuples(
    source_domain: &rumoca_core::StructuredIndexDomain,
    index_tuples: &[Vec<i64>],
    span: rumoca_core::Span,
) -> Result<Option<rumoca_core::StructuredIndexDomain>, LowerError> {
    if index_tuples.is_empty() {
        return Ok(None);
    }
    let dimension_count = source_domain.binders.len();
    if index_tuples
        .iter()
        .any(|index_tuple| index_tuple.len() != dimension_count)
    {
        return Ok(None);
    }
    let mut binders =
        stencil_vec_with_capacity(dimension_count, "compact domain binder count", span)?;
    for dimension in 0..dimension_count {
        let Some(binder) = compact_binder(source_domain, index_tuples, dimension, span)? else {
            return Ok(None);
        };
        binders.push(binder);
    }
    let domain = rumoca_core::StructuredIndexDomain { binders };
    Ok((structured_domain_index_tuples(&domain, span)? == index_tuples).then_some(domain))
}

fn compact_binder(
    source_domain: &rumoca_core::StructuredIndexDomain,
    index_tuples: &[Vec<i64>],
    dimension: usize,
    span: rumoca_core::Span,
) -> Result<Option<rumoca_core::StructuredIndexBinder>, LowerError> {
    let mut values = IndexSet::new();
    values.try_reserve(index_tuples.len()).map_err(|_| {
        stencil_contract_violation(
            "compact domain binder value count exceeds host memory limits",
            span,
        )
    })?;
    for index_tuple in index_tuples {
        let value = index_tuple[dimension];
        values.insert(value);
    }
    let Some(first) = values.first().copied() else {
        return Ok(None);
    };
    let step = if let Some(second) = values.get_index(1).copied() {
        let step = second.checked_sub(first).ok_or_else(|| {
            stencil_contract_violation("compact domain binder step overflows i64", span)
        })?;
        if step == 0 {
            return Ok(None);
        }
        for value in values.iter().skip(2) {
            let delta = value.checked_sub(first).ok_or_else(|| {
                stencil_contract_violation("compact domain binder delta overflows i64", span)
            })?;
            let remainder = delta.checked_rem(step).ok_or_else(|| {
                stencil_contract_violation("compact domain binder remainder overflows i64", span)
            })?;
            if remainder != 0 {
                return Ok(None);
            }
        }
        step
    } else {
        1
    };
    let Some(upper) = values.last().copied() else {
        return Ok(None);
    };
    Ok(Some(rumoca_core::StructuredIndexBinder {
        id: source_domain.binders[dimension].id,
        display_name: source_domain.binders[dimension].display_name.clone(),
        lower: first,
        upper,
        step,
    }))
}
