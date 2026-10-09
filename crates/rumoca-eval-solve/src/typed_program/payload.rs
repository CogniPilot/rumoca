//! The scalar payload of one runtime value.

use std::ops::Deref;
use std::sync::Arc;

use rumoca_ir_solve::SolveValueKind;

/// The elements of a [`super::TypedValue`]: one scalar held inline, so an
/// operation on scalars allocates nothing, or a shared immutable aggregate.
///
/// A payload of exactly one element is always the inline form, so two payloads
/// of equal contents are equal values whichever way they were built.
#[derive(Clone, Debug)]
pub(super) enum Payload {
    Scalar(SolveValueKind),
    Aggregate(Arc<[SolveValueKind]>),
}

impl Payload {
    pub(super) fn of(elements: Vec<SolveValueKind>) -> Self {
        match elements.as_slice() {
            [element] => Self::Scalar(*element),
            _ => Self::Aggregate(elements.into()),
        }
    }

    /// Whether another value shares this payload's allocation.
    #[cfg(test)]
    pub(super) fn is_shared(&self) -> bool {
        matches!(self, Self::Aggregate(elements) if Arc::strong_count(elements) > 1)
    }

    /// The elements to rewrite: in place when nothing else shares them, on a
    /// copy otherwise.
    pub(super) fn make_mut(&mut self) -> &mut [SolveValueKind] {
        match self {
            Self::Scalar(element) => std::slice::from_mut(element),
            Self::Aggregate(elements) => Arc::make_mut(elements),
        }
    }
}

impl Deref for Payload {
    type Target = [SolveValueKind];

    fn deref(&self) -> &[SolveValueKind] {
        match self {
            Self::Scalar(element) => std::slice::from_ref(element),
            Self::Aggregate(elements) => elements,
        }
    }
}

impl PartialEq for Payload {
    fn eq(&self, other: &Self) -> bool {
        **self == **other
    }
}

impl Eq for Payload {}

#[cfg(test)]
impl Payload {
    /// How many values hold this payload's aggregate allocation.
    pub(super) fn holders(&self) -> usize {
        match self {
            Self::Scalar(_) => 1,
            Self::Aggregate(elements) => Arc::strong_count(elements),
        }
    }

    /// Whether both payloads are one aggregate allocation.
    pub(super) fn is_same_allocation(&self, other: &Self) -> bool {
        matches!((self, other), (Self::Aggregate(lhs), Self::Aggregate(rhs)) if Arc::ptr_eq(lhs, rhs))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn one_element_is_inline_and_equality_follows_the_contents() {
        let element = SolveValueKind::Boolean(true);
        assert!(matches!(Payload::of(vec![element]), Payload::Scalar(_)));
        assert_eq!(
            Payload::of(vec![element]),
            Payload::Aggregate(Arc::from([element]))
        );
        let pair = Payload::of(vec![element, element]);
        assert!(matches!(pair, Payload::Aggregate(_)));
        assert_ne!(pair, Payload::of(vec![element]));
        assert!(Payload::of(Vec::new()).is_empty());
    }

    #[test]
    fn rewriting_a_shared_aggregate_copies_it_and_an_unshared_one_does_not() {
        let (zero, one) = (SolveValueKind::Integer(0), SolveValueKind::Integer(1));
        let mut payload = Payload::of(vec![zero, zero]);
        let before = payload.clone();
        assert!(payload.is_shared());
        payload.make_mut()[0] = one;
        assert_eq!(&*before, [zero, zero], "the sharer keeps its values");
        assert!(!payload.is_shared(), "the copy is private");
        payload.make_mut()[1] = one;
        assert_eq!(&*payload, [one, one]);
    }
}
