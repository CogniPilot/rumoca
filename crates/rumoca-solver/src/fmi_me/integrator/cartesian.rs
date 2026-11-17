//! Numerical-only view of a checked same-component Cartesian partition.

use std::rc::Rc;

/// Opaque host-issued state pairs. There is no public constructor and no Solve
/// row, component handle, event state, or backend choice crosses this boundary.
#[derive(Clone, Debug)]
pub struct MeCartesianPartition {
    pairs: Rc<[[usize; 2]]>,
}

impl MeCartesianPartition {
    pub(super) fn from_kernel(kernel: &crate::fmi_me::SolveMeKernel) -> Option<Self> {
        let plan = kernel.source_model().problem.cartesian_pairs.as_ref()?;
        Some(Self {
            pairs: Rc::from(plan.pairs()),
        })
    }

    /// Numerical indices only, in the checked producer's stable order.
    pub fn pairs(&self) -> impl ExactSizeIterator<Item = (usize, usize)> + '_ {
        self.pairs
            .iter()
            .map(|&[position, velocity]| (position, velocity))
    }
}
