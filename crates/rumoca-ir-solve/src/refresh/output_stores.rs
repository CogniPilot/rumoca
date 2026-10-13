//! Compact output ordinals and their exact canonical store prefixes.

use crate::{LinearOp, Reg};

/// One compact record per scalar/ranged store, without enumerating its lanes.
/// Register arithmetic follows the canonical assignment output iterator;
/// overflowing lanes are omitted rather than issued as a register identity.
pub struct ScalarProgramOutputStores {
    stores: Vec<OutputStore>,
}

struct OutputStore {
    first: usize,
    end: usize,
    position: usize,
    start: Reg,
    stride: usize,
    ranged: bool,
}

#[derive(Clone, Copy)]
pub(super) struct CanonicalOutputRange {
    pub first: usize,
    pub end: usize,
    pub position: usize,
}

impl ScalarProgramOutputStores {
    /// Index the source's scalar/ranged stores. `None` means the optional
    /// output ordinal inventory overflows; exact per-output queries remain
    /// available from their canonical owner.
    #[must_use]
    pub fn new(source: &[LinearOp]) -> Option<Self> {
        let mut stores = Vec::new();
        let mut first = 0usize;
        for (position, operation) in source.iter().enumerate() {
            let (start, count, stride) = match *operation {
                LinearOp::StoreOutput { src } => (src, 1, 0),
                LinearOp::StoreOutputRange {
                    start,
                    count,
                    stride,
                } => (start, count, stride),
                _ => continue,
            };
            let valid_count = ((Reg::MAX - start) as usize)
                .checked_div(stride)
                .map_or(count, |last| count.min(last.saturating_add(1)));
            let end = first.checked_add(valid_count)?;
            if end != first {
                stores.push(OutputStore {
                    first,
                    end,
                    position,
                    start,
                    stride,
                    ranged: matches!(operation, LinearOp::StoreOutputRange { .. }),
                });
            }
            first = end;
        }
        Some(Self { stores })
    }

    /// The register and store instruction position of one canonical output.
    #[must_use]
    pub fn output(&self, output_offset: usize) -> Option<(Reg, usize)> {
        let index = self
            .stores
            .partition_point(|store| store.end <= output_offset);
        let store = self.stores.get(index)?;
        let offset = output_offset.checked_sub(store.first)?;
        let register = Reg::try_from(offset.checked_mul(store.stride)?)
            .ok()
            .and_then(|offset| store.start.checked_add(offset))?;
        Some((register, store.position))
    }

    /// Number of retained store instructions, independent of output width.
    #[must_use]
    pub fn store_count(&self) -> usize {
        self.stores.len()
    }

    pub(super) fn range(&self, output_offset: usize) -> Option<CanonicalOutputRange> {
        let index = self
            .stores
            .partition_point(|store| store.end <= output_offset);
        let store = self.stores.get(index)?;
        (store.ranged && output_offset >= store.first).then_some(CanonicalOutputRange {
            first: store.first,
            end: store.end,
            position: store.position,
        })
    }
}
