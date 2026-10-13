//! Private source-bound carrier planning; descriptive records confer no authority.
mod emit;
mod plan;

use super::{TypedCallCompileError, layout::CellRange};
use rumoca_ir_solve as solve;

/// One invocation frame. Fold counters identify the actual compact-domain point.
#[derive(Clone, Debug)]
pub struct ObservationFrame {
    pub(super) owner: solve::SolvePureCallOwnerId,
    pub(super) regions: Vec<(usize, usize)>,
    pub(super) operation: usize,
    pub(super) loops: Vec<rumoca_core::StructuredIndexDomain>,
    /// Empty for the root: its actual arguments belong to the checked adapter.
    pub(super) arguments: Vec<solve::SolveValueType>,
}

#[derive(Clone, Debug)]
pub struct ObservationSource {
    pub(super) predicate: usize,
    #[cfg(any(test, all(target_arch = "wasm32", target_os = "unknown")))]
    pub(super) origin_predicate: usize,
    pub(super) provenance: rumoca_core::Span,
    pub(super) frames: Vec<ObservationFrame>,
    pub(super) captures: Vec<(usize, solve::SolveValueType)>,
    pub(super) projected_captures: Vec<usize>,
    pub(super) payload_bytes: u32,
}

impl ObservationFrame {
    pub const fn owner(&self) -> solve::SolvePureCallOwnerId {
        self.owner
    }
    pub fn region_path(&self) -> &[(usize, usize)] {
        &self.regions
    }
    pub const fn operation(&self) -> usize {
        self.operation
    }
    pub fn domains(&self) -> &[rumoca_core::StructuredIndexDomain] {
        &self.loops
    }
}

impl ObservationSource {
    pub const fn predicate_output(&self) -> usize {
        self.predicate
    }
    pub const fn source_span(&self) -> rumoca_core::Span {
        self.provenance
    }
    pub fn frames(&self) -> &[ObservationFrame] {
        &self.frames
    }
}

#[derive(Clone, Debug)]
pub(super) struct ObservationForwarding {
    pub regions: Vec<(usize, usize)>,
    pub operation: usize,
    pub child_tag: usize,
    pub parent_tag: usize,
    pub prefix_bytes: u32,
}

/// Header: initialized record count. Records: source tag then source-shaped bits.
/// No ordinary output shares this range, and no operation may recycle it.
#[derive(Clone)]
pub(super) struct ObservationPlan {
    pub range: CellRange,
    pub capacity: u32,
    pub stride: u32,
    pub sources: Vec<ObservationSource>,
    pub forwarding: Vec<ObservationForwarding>,
}

impl ObservationPlan {
    pub(super) fn construct(
        owner: &solve::SolvePureCallOwner,
        callees: &[Option<super::layout::FramePlan>],
    ) -> Result<Option<Self>, TypedCallCompileError> {
        if owner.assertion_flow().is_none() {
            return Err(TypedCallCompileError::SiteMismatch);
        }
        let mut plan = Self {
            range: CellRange {
                base: 2,
                offset: 0,
                bytes: 0,
            },
            capacity: 0,
            stride: 8,
            sources: Vec::new(),
            forwarding: Vec::new(),
        };
        plan.capacity = plan.collect(
            owner,
            owner.body(),
            callees,
            &plan::SourceContext::default(),
        )?;
        if plan.sources.is_empty() {
            return Ok(None);
        }
        for source in &plan.sources {
            plan.stride = plan.stride.max(add(8, source.payload_bytes)?);
        }
        plan.range.bytes = add(8, mul(plan.capacity, plan.stride)?)?;
        Ok(Some(plan))
    }

    pub(super) fn direct_source(
        &self,
        regions: &[(usize, usize)],
        operation: usize,
    ) -> Option<usize> {
        self.sources.iter().position(|source| {
            source.frames.len() == 1
                && source.frames[0].regions == regions
                && source.frames[0].operation == operation
        })
    }
}

fn add(a: u32, b: u32) -> Result<u32, TypedCallCompileError> {
    a.checked_add(b)
        .filter(|n| *n <= i32::MAX as u32)
        .ok_or(TypedCallCompileError::SizeLimit)
}

fn mul(a: u32, b: u32) -> Result<u32, TypedCallCompileError> {
    a.checked_mul(b)
        .filter(|n| *n <= i32::MAX as u32)
        .ok_or(TypedCallCompileError::SizeLimit)
}

fn count(n: usize) -> Result<u32, TypedCallCompileError> {
    u32::try_from(n).map_err(|_| TypedCallCompileError::SizeLimit)
}
