//! Scratch cells that only static addresses reach live in wasm locals.
//!
//! The emitter keeps every aggregate element, carried value and region frame
//! in the scratch frame. A cell the code reaches only as an eight-byte load
//! or store at a constant offset from the scratch pointer needs no address: it
//! is a variable. This pass finds those cells in the assembled function, gives
//! each a wasm local, and expands the constant-length copies and fills between
//! such cells and other static addresses into cell moves.
//!
//! A cell is promoted only when every access to its bytes is understood. Any
//! other use of an address into the scratch frame (a dynamic index, a call
//! frame, an address stored as a value, a copy of unknown length) leaves the
//! cells that address can reach in memory, as does any cell the caller reads
//! after the function returns. Nothing is tested at run time.
mod rewrite;
mod scan;
mod webs;

use super::layout::CellRange;
use std::collections::BTreeMap;
use wasm_encoder::{Instruction as I, ValType};

/// Copies and fills of at most this many bytes between static addresses are
/// expanded into cell moves (the cost model of SOLVE-C103).
pub(super) const EXPANDED_BYTES: u32 = super::cost::INLINE_MOVE_BYTES;

/// The whole allocation an address push names, recorded by the emitter when
/// the code goes on to reach more than the first cell of it.
///
/// The extent is the layout's [`CellRange`] itself: an exact-size whole
/// allocation (SOLVE-C72), so the cells the address can reach are the cells of
/// the allocation it was made from and no others. It is built only from that
/// range, never from a byte count, and the scan accepts it only for the
/// address it names (same frame pointer, same offset), so an extent cannot
/// describe a different address than the one it is recorded at.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) struct Extent {
    /// Instruction index of the first of the instructions that push the
    /// address.
    at: usize,
    range: CellRange,
}

impl Extent {
    pub(super) fn of(at: usize, range: CellRange) -> Self {
        Self { at, range }
    }

    /// The same extent for code that starts `by` instructions later.
    pub(super) fn shifted(self, by: usize) -> Self {
        Self {
            at: self.at + by,
            ..self
        }
    }

    pub(super) fn at(&self) -> usize {
        self.at
    }

    /// The end of the scratch bytes the allocation covers, when the address
    /// pushed at `at` is `offset` bytes into the scratch frame.
    pub(super) fn scratch_end(&self, at: usize, offset: u32) -> Option<u32> {
        (self.at == at && self.range.base == SCRATCH_FRAME && self.range.offset == offset)
            .then(|| offset.saturating_add(self.range.bytes))
    }
}

/// The parameter that holds the scratch pointer.
const SCRATCH_FRAME: u32 = 2;

/// The emitted instructions of one function and the allocations whose
/// addresses the code reaches more than one cell from.
pub(super) struct Body {
    pub code: Vec<I<'static>>,
    pub spans: Vec<Extent>,
}

/// What the function is called with and what its caller reads back.
pub(super) struct Frame {
    pub scratch_bytes: u32,
    /// Leading scratch bytes the caller reads after the function returns.
    pub reserved_bytes: u32,
    /// Locals the function declares before promotion, parameters included.
    pub locals: u32,
    /// Parameter count of each imported math function, by function index.
    pub math_arity: BTreeMap<u32, usize>,
}

pub(super) struct Promoted {
    pub code: Vec<I<'static>>,
    pub locals: Vec<(u32, ValType)>,
}

/// The function with its promotable cells in locals, or `None` when no cell
/// qualifies or the code uses an instruction this pass does not model.
pub(super) fn promote(body: &Body, frame: &Frame) -> Option<Promoted> {
    let scan = scan::scan(body, frame)?;
    let resolution = webs::resolve(&scan, body, frame)?;
    rewrite::apply(body, &scan, &resolution, frame)
}

#[cfg(test)]
mod tests;
