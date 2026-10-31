//! Which state seeds reach each program of a causal seed sweep, read from the
//! construction-issued [`solve::LeafDependencies`] certificate.
//!
//! With every seed coordinate outside the states zero, a program's tangent is
//! exactly zero unless one of its targets depends on an active state: the
//! certificate's leaves are the slots no exact program assigns, so a non-state
//! leaf keeps its zero seed through the whole sweep. Such a program may be
//! skipped; its targets already hold zero.
use super::*;

#[cfg(test)]
mod tests;

/// One schedule's state masks, `words` 64-bit words per program.
pub(super) struct StateCones {
    words: usize,
    masks: Vec<u64>,
}

impl StateCones {
    /// Masks from each program's target certificates. A target without a
    /// certificate, or with unbounded dependence, depends on every state.
    pub(super) fn from_leaves(
        state_count: usize,
        programs: &[Vec<Option<&solve::LeafDependencies>>],
    ) -> Option<Self> {
        let words = state_count.div_ceil(64);
        if words == 0 {
            return None;
        }
        let mut masks = vec![0u64; programs.len() * words];
        for (targets, mask) in programs.iter().zip(masks.chunks_exact_mut(words)) {
            for leaves in targets {
                set_state_bits(*leaves, state_count, mask);
            }
        }
        Some(Self { words, masks })
    }

    /// The programs whose mask meets `active`, in order.
    pub(super) fn select(&self, active: &[u64], selected: &mut Vec<usize>) {
        selected.clear();
        for (program, mask) in self.masks.chunks_exact(self.words).enumerate() {
            if mask.iter().zip(active).any(|(bits, on)| bits & on != 0) {
                selected.push(program);
            }
        }
    }
}

fn set_state_bits(leaves: Option<&solve::LeafDependencies>, state_count: usize, mask: &mut [u64]) {
    let Some(solve::LeafDependencies::Bounded(leaves)) = leaves else {
        mask.fill(u64::MAX);
        return;
    };
    for interval in leaves.intervals() {
        for state in *interval.start()..=(*interval.end()).min(state_count.saturating_sub(1)) {
            if let Some(word) = mask.get_mut(state / 64) {
                *word |= 1 << (state % 64);
            }
        }
    }
}

/// The bit mask of the states whose seed is nonzero, when every seed
/// coordinate outside the states is zero; `None` otherwise.
pub(super) fn active_states(state_count: usize, seed: &[f64]) -> Option<Vec<u64>> {
    let (states, rest) = seed.split_at_checked(state_count)?;
    if rest.iter().any(|&value| value != 0.0) {
        return None;
    }
    let mut active = vec![0u64; state_count.div_ceil(64)];
    for (state, value) in states.iter().enumerate() {
        if *value != 0.0
            && let Some(word) = active.get_mut(state / 64)
        {
            *word |= 1 << (state % 64);
        }
    }
    Some(active)
}
