//! The parameter coordinates that stay fixed while an importer remains in
//! continuous-time mode.

use crate::SolveLayout;

/// Parameter coordinates that cannot change while the importer remains in
/// continuous-time mode.
///
/// Declared parameters occupy the immutable prefix; external inputs follow it
/// and change between the importer's calls, so a value that reads one is not
/// static. The hidden homotopy slot is outside that prefix because
/// initialization sweeps it, but every exit from initialization pins it to the
/// actual-system endpoint (`lambda = 1`). It is therefore equally static for
/// continuous refresh and root-search purposes.
///
/// One owner: the algebraic refresh plan and the root search classification
/// both read this domain.
#[derive(Clone, Copy, Debug)]
pub struct ContinuousStaticParameters {
    pub immutable_prefix: usize,
    pub homotopy_endpoint: Option<usize>,
}

impl ContinuousStaticParameters {
    pub const fn from_layout(layout: &SolveLayout) -> Self {
        Self {
            immutable_prefix: layout.parameter_count,
            homotopy_endpoint: layout.initial_homotopy_parameter_index,
        }
    }

    pub fn contains(self, index: usize) -> bool {
        index < self.immutable_prefix || self.homotopy_endpoint == Some(index)
    }
}
