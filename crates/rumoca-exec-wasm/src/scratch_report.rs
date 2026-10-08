//! Where the native whole-program scratch goes: per owner frame, nested region
//! and call site high-water marks, plus the program-level components.

/// One program body: a call owner or a region nested in one.
///
/// `high_water_bytes` is the highest scratch offset the body addresses, so the
/// storage the body itself needs is `high_water_bytes - base_bytes`.
/// `unshared_bytes` is what the same body would need if no storage were shared
/// between sequential operations, conditional arms or call frames.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ScratchFrame {
    pub base_bytes: u32,
    pub input_bytes: u32,
    pub output_bytes: u32,
    pub high_water_bytes: u32,
    pub unshared_bytes: u32,
    /// Freshly allocated slots; borrowed inputs and outputs are not counted.
    pub slot_bytes: u32,
    /// Freshly allocated registers; aliased registers are not counted.
    pub register_bytes: u32,
    pub register_count: u32,
    pub largest_register_bytes: u32,
    pub regions: Vec<ScratchRegion>,
    pub calls: Vec<ScratchCall>,
}

/// A region of one operation: `then`/`else`, `body`, or `transition`/`predicate`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ScratchRegion {
    pub operation: usize,
    pub role: &'static str,
    pub frame: ScratchFrame,
}

/// A call site: the callee frame it places in its caller's scratch.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ScratchCall {
    pub operation: usize,
    pub owner: usize,
    pub input_bytes: u32,
    pub output_bytes: u32,
    pub scratch_bytes: u32,
    pub offset_bytes: u32,
}

/// A call owner reachable from the program, laid out once for every site.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ScratchOwner {
    pub owner: usize,
    pub provenance: rumoca_core::Span,
    pub frame: ScratchFrame,
}

/// The whole-program scratch of one native assignment module.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ScratchReport {
    pub total_bytes: u32,
    pub work_y_bytes: u32,
    pub call_input_bytes: u32,
    pub call_output_bytes: u32,
    pub call_scratch_bytes: u32,
    pub memo_bytes: u32,
    pub typed_lane_bytes: u32,
    pub p_copy_bytes: u32,
    /// Unshared scratch of the widest root call owner, the figure the layout
    /// charged before sequential operations shared one frame.
    pub unshared_call_scratch_bytes: u32,
    /// The root owner whose frame sets the shared call input/output/scratch.
    pub widest_owner: Option<usize>,
    pub owners: Vec<ScratchOwner>,
}
