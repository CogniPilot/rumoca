//! Exact byte correspondence outside proposed source replacements.
//!
//! This proves only a relation between the supplied byte slices. The caller
//! must obtain the regions from the real AST/API/conformance gate and bind the
//! baseline to the immutable ledger. A witness here never authorizes a region,
//! retires a finding, or establishes source-capture identity on its own.

#[cfg(test)]
mod tests;

use std::collections::BTreeSet;
use std::ops::Range;

const MAX_SOURCE_BYTES: usize = 256 * 1024 * 1024;
const MAX_REGIONS: usize = 4096;

/// Untrusted proposed replacement coordinates, in original source order.
/// An empty range represents an insertion/deletion anchor, not missing proof.
#[derive(Debug, Clone)]
pub(crate) struct RegionPair {
    pub(crate) identity: u32,
    pub(crate) before: Range<usize>,
    pub(crate) after: Range<usize>,
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum ComplementRefusal {
    SourceLimit,
    RegionLimit,
    NoRegions,
    DuplicateIdentity(u32),
    InvalidRange(u32),
    UnorderedOrOverlapping(u32),
    ChangedComplement,
    ForeignInput,
}

/// Mathematical complement equality; deliberately not a migration permission.
pub(crate) struct ComplementWitness<'input> {
    before: &'input [u8],
    after: &'input [u8],
    regions: &'input [RegionPair],
}

impl ComplementWitness<'_> {
    /// These must be the actual supplied slices, not equal copied payloads.
    /// The enclosing joint gate separately checks the captured owner's identity.
    pub(crate) fn check_inputs(
        &self,
        before: &[u8],
        after: &[u8],
        regions: &[RegionPair],
    ) -> Result<(), ComplementRefusal> {
        if !std::ptr::eq(self.before, before)
            || !std::ptr::eq(self.after, after)
            || !std::ptr::eq(self.regions, regions)
        {
            return Err(ComplementRefusal::ForeignInput);
        }
        Ok(())
    }
}

/// Prove every prefix, inter-region segment and suffix byte unchanged.
/// Coordinates are never sorted, repaired, clipped or interpreted as authority.
pub(crate) fn prove_complement<'input>(
    before: &'input [u8],
    after: &'input [u8],
    regions: &'input [RegionPair],
) -> Result<ComplementWitness<'input>, ComplementRefusal> {
    if before.len() > MAX_SOURCE_BYTES || after.len() > MAX_SOURCE_BYTES {
        return Err(ComplementRefusal::SourceLimit);
    }
    if regions.len() > MAX_REGIONS {
        return Err(ComplementRefusal::RegionLimit);
    }
    if regions.is_empty() {
        return Err(ComplementRefusal::NoRegions);
    }
    let mut identities = BTreeSet::new();
    let (mut old_end, mut new_end) = (0, 0);
    for region in regions {
        if !identities.insert(region.identity) {
            return Err(ComplementRefusal::DuplicateIdentity(region.identity));
        }
        if region.before.start > region.before.end
            || region.before.end > before.len()
            || region.after.start > region.after.end
            || region.after.end > after.len()
        {
            return Err(ComplementRefusal::InvalidRange(region.identity));
        }
        if region.before.start < old_end || region.after.start < new_end {
            return Err(ComplementRefusal::UnorderedOrOverlapping(region.identity));
        }
        if before[old_end..region.before.start] != after[new_end..region.after.start] {
            return Err(ComplementRefusal::ChangedComplement);
        }
        old_end = region.before.end;
        new_end = region.after.end;
    }
    if before[old_end..] != after[new_end..] {
        return Err(ComplementRefusal::ChangedComplement);
    }
    Ok(ComplementWitness {
        before,
        after,
        regions,
    })
}
