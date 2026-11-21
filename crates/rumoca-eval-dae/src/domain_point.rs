//! Exact binder values in the currently active lexical domains.

use rumoca_ir_dae::DomainId;

/// Why an active binder value could not be read.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DomainPointError {
    InactiveDomain,
    InvalidOrdinal,
}

/// Read a binder from the innermost active point for its exact DAE domain.
pub fn domain_point_integer<'dae>(
    domain: DomainId<'dae>,
    ordinal: usize,
    points: &[(DomainId<'dae>, Vec<i64>)],
) -> Result<i64, DomainPointError> {
    let (_, values) = points
        .iter()
        .rev()
        .find(|(id, _)| *id == domain)
        .ok_or(DomainPointError::InactiveDomain)?;
    values
        .get(ordinal)
        .copied()
        .ok_or(DomainPointError::InvalidOrdinal)
}
