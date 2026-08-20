//! Parameter domain that is invariant during FMI continuous-time mode.

use rumoca_ir_solve as solve;

/// Parameter coordinates that cannot change while the importer remains in
/// continuous-time mode.
///
/// Declared parameters occupy the immutable prefix. The hidden homotopy slot
/// is outside that prefix because initialization sweeps it, but every exit from
/// initialization pins it to the actual-system endpoint (`lambda = 1`). It is
/// therefore equally static for continuous refresh and root-search purposes.
#[derive(Clone, Copy)]
pub(super) struct ContinuousStaticParameters {
    pub(super) immutable_prefix: usize,
    pub(super) homotopy_endpoint: Option<usize>,
}

impl ContinuousStaticParameters {
    pub(super) const fn from_layout(layout: &solve::SolveLayout) -> Self {
        Self {
            immutable_prefix: layout.parameter_count,
            homotopy_endpoint: layout.initial_homotopy_parameter_index,
        }
    }

    pub(super) fn contains(self, index: usize) -> bool {
        index < self.immutable_prefix || self.homotopy_endpoint == Some(index)
    }
}
