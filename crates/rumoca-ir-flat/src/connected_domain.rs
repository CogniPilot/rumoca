//! Checked connected subdomain of one Flat declaration (MLS §9.2).

use std::collections::BTreeSet;
use std::fmt;

use serde::{Deserialize, Serialize};

/// Elements of one Flat declaration that participate in connection equations.
///
/// MLS §9.2 requires `f = 0` for every flow element that belongs to no
/// connection set and exactly one flow-sum contribution for every element that
/// does. A declaration-wide Boolean cannot express `connect(c[1], ...)` on a
/// compact `c[3]`: marking the whole declaration would suppress the zero-flow
/// rows of `c[2]` and `c[3]`, and leaving it clear would zero `c[1]` on top of
/// its flow sum. The domain therefore records which elements are connected.
///
/// The representation is a prefix-free set of leading selections (MLS §10.5):
/// the empty selection denotes every element, `[1]` of a `[2, 3]` declaration
/// denotes the three elements `[1, *]`, and a full-rank selection denotes one
/// scalar. A scalar declaration is denoted only by the empty selection. Every
/// selection is validated against the declaration's dimensions when it is
/// inserted, so an out-of-range or over-ranked mark is unrepresentable rather
/// than silently ignored. Keeping the set prefix-free makes the connected
/// element count an exact sum over disjoint blocks, which is what decides
/// whether a declaration is unconnected, partially connected, or wholly
/// connected without enumerating its elements.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(transparent)]
pub struct ConnectedDomain {
    selections: BTreeSet<Vec<i64>>,
}

/// How much of a declaration's element domain is connected.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConnectedCoverage {
    /// No element participates in a connection equation.
    Unconnected,
    /// Some but not all elements participate.
    Partial,
    /// Every element participates.
    Whole,
}

/// A selection that no element of the declaration can satisfy.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConnectedDomainError {
    /// A selection carries more subscripts than the declaration has dimensions.
    RankExceeded { selected: usize, rank: usize },
    /// A declaration extent is negative, so no element domain exists.
    NegativeExtent { axis: usize, extent: i64 },
    /// A subscript lies outside its declared extent.
    CoordinateOutOfRange {
        axis: usize,
        coordinate: i64,
        extent: i64,
    },
    /// The declaration's element count does not fit the host index range.
    CardinalityOverflow,
}

impl fmt::Display for ConnectedDomainError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::RankExceeded { selected, rank } => write!(
                formatter,
                "{selected} subscripts select a declaration of rank {rank}"
            ),
            Self::NegativeExtent { axis, extent } => write!(
                formatter,
                "dimension {} has negative extent {extent}",
                axis + 1
            ),
            Self::CoordinateOutOfRange {
                axis,
                coordinate,
                extent,
            } => write!(
                formatter,
                "subscript {coordinate} on dimension {} is outside its extent {extent}",
                axis + 1
            ),
            Self::CardinalityOverflow => {
                write!(formatter, "array cardinality exceeds the host index range")
            }
        }
    }
}

impl std::error::Error for ConnectedDomainError {}

impl ConnectedDomain {
    /// The domain of a declaration none of whose elements is connected.
    pub const fn unconnected() -> Self {
        Self {
            selections: BTreeSet::new(),
        }
    }

    /// True when no element is connected.
    pub fn is_unconnected(&self) -> bool {
        self.selections.is_empty()
    }

    /// Leading selections recorded on this domain, in lexicographic order.
    pub fn selections(&self) -> impl Iterator<Item = &[i64]> {
        self.selections.iter().map(Vec::as_slice)
    }

    /// True when every element denoted by `selection` is connected.
    ///
    /// Membership is decided by the prefix relation alone, so it does not need
    /// the declaration shape: a recorded selection covers `selection` exactly
    /// when it is a prefix of it.
    pub fn covers(&self, selection: &[i64]) -> bool {
        self.selections
            .iter()
            .any(|recorded| selection.starts_with(recorded))
    }

    /// Mark every element denoted by a leading `selection` of a declaration
    /// with dimensions `dims` as connected.
    ///
    /// The selection is checked against `dims` before any mutation. A selection
    /// denoting zero elements (some trailing extent is zero) marks nothing,
    /// because an empty value has no element to connect.
    pub fn mark(&mut self, dims: &[i64], selection: &[i64]) -> Result<(), ConnectedDomainError> {
        validate_selection(dims, selection)?;
        if dims[selection.len()..].contains(&0) {
            return Ok(());
        }
        if self.covers(selection) {
            return Ok(());
        }
        // A longer recorded selection under the new one is now redundant, and
        // keeping it would double count its block when coverage is measured.
        self.selections
            .retain(|recorded| !recorded.starts_with(selection));
        self.selections.insert(selection.to_vec());
        Ok(())
    }

    /// Add every selection of `other` to this domain.
    ///
    /// Both domains must have been marked against the same declaration shape;
    /// the union keeps the prefix-free invariant by re-inserting each selection
    /// through [`Self::mark`]-equivalent normalization.
    pub fn union_with(&mut self, other: &Self) {
        for selection in &other.selections {
            if self.covers(selection) {
                continue;
            }
            self.selections
                .retain(|recorded| !recorded.starts_with(selection.as_slice()));
            self.selections.insert(selection.clone());
        }
    }

    /// Coverage of a declaration with dimensions `dims`.
    ///
    /// Every recorded selection is re-validated against `dims`, so a domain
    /// read against a shape it was not marked for is an error rather than a
    /// silently wrong answer.
    pub fn coverage(&self, dims: &[i64]) -> Result<ConnectedCoverage, ConnectedDomainError> {
        let total = scalar_count(dims)?;
        let mut connected = 0usize;
        for selection in &self.selections {
            validate_selection(dims, selection)?;
            connected = connected
                .checked_add(scalar_count(&dims[selection.len()..])?)
                .ok_or(ConnectedDomainError::CardinalityOverflow)?;
        }
        Ok(if connected == 0 {
            ConnectedCoverage::Unconnected
        } else if connected >= total {
            ConnectedCoverage::Whole
        } else {
            ConnectedCoverage::Partial
        })
    }

    /// Full-rank coordinates of the declaration that are not connected, in
    /// row-major order.
    ///
    /// This enumerates the declaration's element domain, so callers bound the
    /// cardinality before asking for a partially connected declaration's
    /// complement.
    pub fn unconnected_coordinates(
        &self,
        dims: &[i64],
    ) -> Result<Vec<Vec<i64>>, ConnectedDomainError> {
        self.coordinates_where(dims, false)
    }

    /// Full-rank coordinates of the declaration that are connected, in
    /// row-major order. Enumerates the element domain like
    /// [`Self::unconnected_coordinates`].
    pub fn connected_coordinates(
        &self,
        dims: &[i64],
    ) -> Result<Vec<Vec<i64>>, ConnectedDomainError> {
        self.coordinates_where(dims, true)
    }

    fn coordinates_where(
        &self,
        dims: &[i64],
        connected: bool,
    ) -> Result<Vec<Vec<i64>>, ConnectedDomainError> {
        for selection in &self.selections {
            validate_selection(dims, selection)?;
        }
        let total = scalar_count(dims)?;
        let mut matching = Vec::new();
        let mut coordinate = vec![1i64; dims.len()];
        for _ in 0..total {
            if self.covers(&coordinate) == connected {
                matching.push(coordinate.clone());
            }
            advance_row_major(&mut coordinate, dims);
        }
        Ok(matching)
    }
}

fn validate_selection(dims: &[i64], selection: &[i64]) -> Result<(), ConnectedDomainError> {
    for (axis, extent) in dims.iter().enumerate() {
        if *extent < 0 {
            return Err(ConnectedDomainError::NegativeExtent {
                axis,
                extent: *extent,
            });
        }
    }
    if selection.len() > dims.len() {
        return Err(ConnectedDomainError::RankExceeded {
            selected: selection.len(),
            rank: dims.len(),
        });
    }
    for (axis, (coordinate, extent)) in selection.iter().zip(dims).enumerate() {
        if *coordinate < 1 || *coordinate > *extent {
            return Err(ConnectedDomainError::CoordinateOutOfRange {
                axis,
                coordinate: *coordinate,
                extent: *extent,
            });
        }
    }
    Ok(())
}

fn scalar_count(dims: &[i64]) -> Result<usize, ConnectedDomainError> {
    let mut count = 1usize;
    for extent in dims {
        let extent =
            usize::try_from(*extent).map_err(|_| ConnectedDomainError::CardinalityOverflow)?;
        count = count
            .checked_mul(extent)
            .ok_or(ConnectedDomainError::CardinalityOverflow)?;
    }
    Ok(count)
}

/// Step a full-rank coordinate to its row-major successor, wrapping each axis
/// at its extent. Stepping past the last coordinate wraps back to the first,
/// which callers never observe because they step exactly `scalar_count` times.
fn advance_row_major(coordinate: &mut [i64], dims: &[i64]) {
    for axis in (0..dims.len()).rev() {
        if coordinate[axis] < dims[axis] {
            coordinate[axis] += 1;
            return;
        }
        coordinate[axis] = 1;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn scalar_declaration_is_wholly_connected_by_the_empty_selection() {
        let mut domain = ConnectedDomain::unconnected();
        assert_eq!(
            domain.coverage(&[]).unwrap(),
            ConnectedCoverage::Unconnected
        );
        domain.mark(&[], &[]).unwrap();
        assert_eq!(domain.coverage(&[]).unwrap(), ConnectedCoverage::Whole);
        assert!(domain.unconnected_coordinates(&[]).unwrap().is_empty());
    }

    #[test]
    fn one_element_of_three_is_partial_and_names_the_other_two() {
        let mut domain = ConnectedDomain::unconnected();
        domain.mark(&[3], &[1]).unwrap();
        assert_eq!(domain.coverage(&[3]).unwrap(), ConnectedCoverage::Partial);
        assert_eq!(
            domain.unconnected_coordinates(&[3]).unwrap(),
            vec![vec![2], vec![3]]
        );
    }

    #[test]
    fn every_element_marked_individually_is_whole() {
        let mut domain = ConnectedDomain::unconnected();
        for index in 1..=3 {
            domain.mark(&[3], &[index]).unwrap();
        }
        assert_eq!(domain.coverage(&[3]).unwrap(), ConnectedCoverage::Whole);
        assert!(domain.unconnected_coordinates(&[3]).unwrap().is_empty());
    }

    #[test]
    fn leading_selection_denotes_its_trailing_block_in_row_major_order() {
        let mut domain = ConnectedDomain::unconnected();
        domain.mark(&[2, 2], &[1]).unwrap();
        assert_eq!(
            domain.coverage(&[2, 2]).unwrap(),
            ConnectedCoverage::Partial
        );
        assert_eq!(
            domain.unconnected_coordinates(&[2, 2]).unwrap(),
            vec![vec![2, 1], vec![2, 2]]
        );
        domain.mark(&[2, 2], &[2, 2]).unwrap();
        assert_eq!(
            domain.unconnected_coordinates(&[2, 2]).unwrap(),
            vec![vec![2, 1]]
        );
    }

    #[test]
    fn a_wider_selection_absorbs_narrower_ones_without_double_counting() {
        let mut domain = ConnectedDomain::unconnected();
        domain.mark(&[2, 2], &[1, 1]).unwrap();
        domain.mark(&[2, 2], &[1, 2]).unwrap();
        domain.mark(&[2, 2], &[1]).unwrap();
        assert_eq!(domain.selections().collect::<Vec<_>>(), vec![&[1][..]]);
        assert_eq!(
            domain.coverage(&[2, 2]).unwrap(),
            ConnectedCoverage::Partial
        );
        domain.mark(&[2, 2], &[]).unwrap();
        assert_eq!(
            domain.selections().collect::<Vec<_>>(),
            vec![&[][..] as &[i64]]
        );
        assert_eq!(domain.coverage(&[2, 2]).unwrap(), ConnectedCoverage::Whole);
    }

    #[test]
    fn union_keeps_the_prefix_free_invariant() {
        let mut left = ConnectedDomain::unconnected();
        left.mark(&[2, 2], &[1, 1]).unwrap();
        let mut right = ConnectedDomain::unconnected();
        right.mark(&[2, 2], &[1]).unwrap();
        right.mark(&[2, 2], &[2, 2]).unwrap();
        left.union_with(&right);
        assert_eq!(
            left.selections().collect::<Vec<_>>(),
            vec![&[1][..], &[2, 2][..]]
        );
        assert_eq!(
            left.unconnected_coordinates(&[2, 2]).unwrap(),
            vec![vec![2, 1]]
        );
    }

    #[test]
    fn invalid_selections_are_refused_before_any_mutation() {
        let mut domain = ConnectedDomain::unconnected();
        assert_eq!(
            domain.mark(&[3], &[0]),
            Err(ConnectedDomainError::CoordinateOutOfRange {
                axis: 0,
                coordinate: 0,
                extent: 3
            })
        );
        assert_eq!(
            domain.mark(&[3], &[4]),
            Err(ConnectedDomainError::CoordinateOutOfRange {
                axis: 0,
                coordinate: 4,
                extent: 3
            })
        );
        assert_eq!(
            domain.mark(&[3], &[1, 1]),
            Err(ConnectedDomainError::RankExceeded {
                selected: 2,
                rank: 1
            })
        );
        assert_eq!(
            domain.mark(&[-1], &[]),
            Err(ConnectedDomainError::NegativeExtent {
                axis: 0,
                extent: -1
            })
        );
        assert!(domain.is_unconnected());
    }

    #[test]
    fn a_domain_read_against_a_foreign_shape_is_an_error() {
        let mut domain = ConnectedDomain::unconnected();
        domain.mark(&[3], &[3]).unwrap();
        assert!(domain.coverage(&[2]).is_err());
        assert!(domain.unconnected_coordinates(&[2]).is_err());
        assert!(domain.coverage(&[]).is_err());
    }

    #[test]
    fn an_empty_value_has_nothing_to_connect() {
        let mut domain = ConnectedDomain::unconnected();
        domain.mark(&[2, 0], &[1]).unwrap();
        assert!(domain.is_unconnected());
        assert_eq!(
            domain.coverage(&[2, 0]).unwrap(),
            ConnectedCoverage::Unconnected
        );
        assert!(domain.unconnected_coordinates(&[2, 0]).unwrap().is_empty());
    }
}
