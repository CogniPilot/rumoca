//! The one owner of iteration over the cells of a static shape.
//!
//! Every tensor operation prints its per-cell body through [`Emitter::cells`],
//! [`Emitter::cells_from`] or [`Emitter::cells_shaped`], and reaches cells only
//! through [`Emitter::cell_address`], [`Emitter::cell_at`] and
//! [`Emitter::axis`]. The owner prints the same cells in the same order in
//! the form Solve IR issued for the operation
//! (`TypedProgram::straight_cells`); it makes no choice of its own:
//!
//! - straight cells: the body once per cell, with each axis index a constant,
//!   so every address is a constant offset (no counter, no branch);
//! - otherwise: one `i32` counter per axis of extent above one, in nested
//!   loops in row-major order, with each address its base plus constant
//!   multiples of the counters (no divide or remainder). Only past
//!   [`AXIS_LOCALS`] axes do the leading axes share one counter and take
//!   their indices from it by division.
//!
//! Either way the body runs the same instructions on the same cells in the
//! same order, so the values computed are bit for bit the same.
//!
//! The address of a range reached by reference (a pointer cell) does not
//! change while one operation runs: it is loaded once, before the operation's
//! cells, into one of [`BASE_LOCALS`] locals.
use super::emit::Emitter;
use super::layout::CellRange;
use wasm_encoder::{BlockType, Instruction as I, MemArg};

/// First of the transient `i32` locals this owner uses.
pub(super) const FIRST_CELL_LOCAL: u32 = 13;
/// Loop counters for the axes of one operation's iterations.
const AXIS_LOCALS: u32 = 6;
/// Loop-invariant base addresses of ranges reached by reference; a range past
/// them reloads its address at each use.
const BASE_LOCALS: u32 = 4;
/// The transient `i32` locals this owner uses.
pub(super) const CELL_LOCALS: u32 = AXIS_LOCALS + BASE_LOCALS;

/// The index along one axis of the cell the body is printed for.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum Index {
    /// Known when the code is printed.
    At(u32),
    /// Held in an `i32` local.
    Counter(u32),
    /// `(local / stride) % extent`: an axis sharing a counter with the axes
    /// after it.
    Digit {
        local: u32,
        stride: u32,
        extent: u32,
    },
}

/// One iteration over a static shape, innermost last.
#[derive(Clone, Debug, Default)]
pub(super) struct Level {
    axes: Vec<Index>,
    /// Row-major cell strides of the level's shape.
    strides: Vec<u32>,
}

/// Base addresses loaded once for the operation being printed.
#[derive(Clone, Debug, Default)]
pub(super) struct Bases {
    /// Where the loads go: the first instruction of the operation's cells.
    at: usize,
    /// Pointer base (`CellRange::base`) held in each base local.
    held: Vec<u32>,
}

impl Emitter<'_> {
    /// Print `body` once per cell `0..count` of a flat shape.
    pub(super) fn cells(&mut self, count: u32, body: impl FnMut(&mut Self)) {
        self.iterate(0, &[count], body);
    }

    /// Print `body` once per cell `start..count` of a flat shape.
    pub(super) fn cells_from(&mut self, start: u32, count: u32, body: impl FnMut(&mut Self)) {
        self.iterate(start, &[count], body);
    }

    /// Print `body` once per cell of `shape`, in row-major order.
    pub(super) fn cells_shaped(&mut self, shape: &[u32], body: impl FnMut(&mut Self)) {
        self.iterate(0, shape, body);
    }

    /// The index along axis `axis` of the cell being printed, counting the
    /// axes of every enclosing iteration of this operation, outermost first.
    pub(super) fn axis(&self, axis: usize) -> Index {
        *self
            .cell_levels
            .iter()
            .flat_map(|level| &level.axes)
            .nth(axis)
            .expect("axis of an enclosing cell iteration")
    }

    /// Push the address of the cell of the innermost iteration in `range`.
    pub(super) fn cell_address(&mut self, range: CellRange) {
        let level = self
            .cell_levels
            .last()
            .expect("cell address inside a cell iteration");
        let terms = level
            .axes
            .iter()
            .copied()
            .zip(level.strides.iter().copied())
            .collect::<Vec<_>>();
        self.cell_at(range, 0, &terms);
    }

    /// Push the address of cell `constant + Σ index × stride` of `range`.
    pub(super) fn cell_at(&mut self, range: CellRange, constant: u32, terms: &[(Index, u32)]) {
        let mut cell = constant;
        let mut counters = Vec::new();
        for &(index, stride) in terms {
            match index {
                Index::At(at) => cell += at * stride,
                index => counters.push((index, stride)),
            }
        }
        if counters.is_empty() {
            self.address(CellRange {
                offset: range.offset + cell * 8,
                bytes: 8,
                ..range
            });
            return;
        }
        self.address_span(range);
        for (index, stride) in counters {
            self.push_index(index);
            self.push(I::I32Const((stride * 8) as i32));
            self.push(I::I32Mul);
            self.push(I::I32Add);
        }
        if cell > 0 {
            self.push(I::I32Const((cell * 8) as i32));
            self.push(I::I32Add);
        }
    }

    /// Push an axis index as an `i32`.
    pub(super) fn push_index(&mut self, index: Index) {
        match index {
            Index::At(at) => self.push(I::I32Const(at as i32)),
            Index::Counter(local) => self.push(I::LocalGet(local)),
            Index::Digit {
                local,
                stride,
                extent,
            } => {
                self.push(I::LocalGet(local));
                self.push(I::I32Const(stride as i32));
                self.push(I::I32DivU);
                self.push(I::I32Const(extent as i32));
                self.push(I::I32RemU);
            }
        }
    }

    /// The local that holds the address of `base`, a pointer cell, while an
    /// operation's cells are printed.
    pub(super) fn held_base(&mut self, base: u32) -> Option<u32> {
        let bases = self.cell_bases.as_mut()?;
        let slot = match bases.held.iter().position(|held| *held == base) {
            Some(slot) => slot,
            None if bases.held.len() < BASE_LOCALS as usize => {
                bases.held.push(base);
                bases.held.len() - 1
            }
            None => return None,
        };
        Some(FIRST_CELL_LOCAL + AXIS_LOCALS + slot as u32)
    }

    fn iterate(&mut self, start: u32, shape: &[u32], body: impl FnMut(&mut Self)) {
        let outermost = self.cell_bases.is_none();
        if outermost {
            self.cell_bases = Some(Bases {
                at: self.code.len(),
                held: Vec::new(),
            });
        }
        if self.straight_cells {
            self.unrolled(start, shape, body);
        } else {
            let first = self.cell_levels.iter().map(|l| l.axes.len()).sum::<usize>();
            self.looped(start, shape, first, body);
        }
        if outermost {
            let bases = self.cell_bases.take().expect("bases of this iteration");
            self.load_bases(&bases);
        }
    }

    fn unrolled(&mut self, start: u32, shape: &[u32], mut body: impl FnMut(&mut Self)) {
        let strides = row_major_strides(shape);
        for cell in start..shape.iter().product() {
            let axes = shape
                .iter()
                .zip(&strides)
                .map(|(extent, stride)| Index::At(cell / stride % extent))
                .collect();
            self.cell_levels.push(Level {
                axes,
                strides: strides.clone(),
            });
            body(self);
            self.cell_levels.pop();
        }
    }

    /// One loop per axis of extent above one, counting in local
    /// `FIRST_CELL_LOCAL` plus the axis' position among the operation's axes.
    /// When the operation has more axes than counters, the leading axes share
    /// the first counter over their combined extent.
    fn looped(&mut self, start: u32, shape: &[u32], first: usize, body: impl FnOnce(&mut Self)) {
        if shape.iter().product::<u32>() <= start {
            return;
        }
        let free = (AXIS_LOCALS as usize)
            .checked_sub(first)
            .filter(|free| *free > 0)
            .expect("a counter for each nested cell iteration");
        let fused = shape.len().saturating_sub(free - 1).max(1);
        let local = |position: usize| FIRST_CELL_LOCAL + (first + position) as u32;
        let mut loops = vec![(shape[..fused].iter().product::<u32>(), local(0))];
        let mut axes = Vec::with_capacity(shape.len());
        let mut stride = loops[0].0;
        for &extent in &shape[..fused] {
            stride /= extent;
            axes.push(match (extent, fused) {
                (1, _) => Index::At(0),
                (_, 1) => Index::Counter(local(0)),
                _ => Index::Digit {
                    local: local(0),
                    stride,
                    extent,
                },
            });
        }
        for (position, &extent) in shape[fused..].iter().enumerate() {
            loops.push((extent, local(position + 1)));
            axes.push(match extent {
                1 => Index::At(0),
                _ => Index::Counter(local(position + 1)),
            });
        }
        loops.retain(|(extent, _)| *extent > 1);
        for (ordinal, &(extent, local)) in loops.iter().enumerate() {
            // Only a flat shape starts past its first cell.
            let from = if ordinal == 0 && fused == 1 { start } else { 0 };
            self.open_loop(local, from, extent);
        }
        self.cell_levels.push(Level {
            axes,
            strides: row_major_strides(shape),
        });
        body(self);
        self.cell_levels.pop();
        for &(_, local) in loops.iter().rev() {
            self.close_loop(local);
        }
    }

    fn open_loop(&mut self, local: u32, from: u32, extent: u32) {
        self.push(I::I32Const(from as i32));
        self.push(I::LocalSet(local));
        self.push(I::Block(BlockType::Empty));
        self.push(I::Loop(BlockType::Empty));
        self.push(I::LocalGet(local));
        self.push(I::I32Const(extent as i32));
        self.push(I::I32GeU);
        self.push(I::BrIf(1));
    }

    fn close_loop(&mut self, local: u32) {
        self.push(I::LocalGet(local));
        self.push(I::I32Const(1));
        self.push(I::I32Add);
        self.push(I::LocalSet(local));
        self.push(I::Br(0));
        self.push(I::End);
        self.push(I::End);
    }

    /// Insert, before the operation's cells, the load of each base address
    /// its cells used.
    fn load_bases(&mut self, bases: &Bases) {
        let mut loads = Vec::new();
        for (slot, base) in bases.held.iter().enumerate() {
            let range = CellRange {
                base: *base,
                offset: 0,
                bytes: 0,
            };
            let (frame, cell) = range.pointer_frame().expect("held bases are pointer cells");
            loads.extend([
                I::LocalGet(frame),
                I::I32Const(cell as i32),
                I::I32Add,
                I::I32Load(MemArg {
                    offset: 0,
                    align: 2,
                    memory_index: 0,
                }),
                I::LocalSet(FIRST_CELL_LOCAL + AXIS_LOCALS + slot as u32),
            ]);
        }
        if loads.is_empty() {
            return;
        }
        let inserted = loads.len();
        self.code.splice(bases.at..bases.at, loads);
        for extent in &mut self.spans {
            if extent.at() >= bases.at {
                *extent = extent.shifted(inserted);
            }
        }
    }
}

fn row_major_strides(shape: &[u32]) -> Vec<u32> {
    let mut strides = vec![1; shape.len()];
    for axis in (0..shape.len().saturating_sub(1)).rev() {
        strides[axis] = strides[axis + 1] * shape[axis + 1];
    }
    strides
}
