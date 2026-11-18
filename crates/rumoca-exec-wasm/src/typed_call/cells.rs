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
//!   multiples of the counters (no divide or remainder).
//!
//! Either way the body runs the same instructions on the same cells in the
//! same order, so the values computed are bit for bit the same.
//!
//! The address of a range reached by reference (a pointer cell) does not
//! change while one operation runs: it is loaded once, before the operation's
//! cells, into a local of its own.
//!
//! Each operation takes its counters and base locals from the function's
//! cell locals, in order of first use; [`cell_locals`] sizes them from the
//! program before the function's locals are declared.
use super::emit::Emitter;
use super::layout::CellRange;
use rumoca_ir_solve as solve;
use wasm_encoder::{BlockType, Instruction as I, MemArg};

/// The index along one axis of the cell the body is printed for.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum Index {
    /// Known when the code is printed.
    At(u32),
    /// Held in an `i32` local.
    Counter(u32),
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
    /// Each pointer base (`CellRange::base`) held, and its local.
    held: Vec<(u32, u32)>,
    /// Cell locals the operation has taken.
    taken: u32,
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
        }
    }

    /// The local that holds the address of `base`, a pointer cell, while an
    /// operation's cells are printed.
    pub(super) fn held_base(&mut self, base: u32) -> Option<u32> {
        let bases = self.cell_bases.as_ref()?;
        if let Some((_, local)) = bases.held.iter().find(|(held, _)| *held == base) {
            return Some(*local);
        }
        let local = self.take_cell_local();
        let bases = self.cell_bases.as_mut()?;
        bases.held.push((base, local));
        Some(local)
    }

    /// The next cell local of the operation being printed.
    fn take_cell_local(&mut self) -> u32 {
        let bases = self
            .cell_bases
            .as_mut()
            .expect("cell locals are taken inside a cell iteration");
        assert!(
            bases.taken < self.cell_locals,
            "the function's cell locals cover each operation"
        );
        bases.taken += 1;
        self.cell_local_base + bases.taken - 1
    }

    fn iterate(&mut self, start: u32, shape: &[u32], body: impl FnMut(&mut Self)) {
        let outermost = self.cell_bases.is_none();
        if outermost {
            self.cell_bases = Some(Bases {
                at: self.code.len(),
                held: Vec::new(),
                taken: 0,
            });
        }
        if self.straight_cells {
            self.unrolled(start, shape, body);
        } else {
            self.looped(start, shape, body);
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

    /// One loop per axis of extent above one, each with its own counter.
    fn looped(&mut self, start: u32, shape: &[u32], body: impl FnOnce(&mut Self)) {
        if shape.iter().product::<u32>() <= start {
            return;
        }
        let axes = shape
            .iter()
            .map(|extent| match extent {
                1 => Index::At(0),
                _ => Index::Counter(self.take_cell_local()),
            })
            .collect::<Vec<_>>();
        let loops = axes
            .iter()
            .zip(shape)
            .enumerate()
            .filter_map(|(axis, (index, extent))| match *index {
                Index::Counter(local) => Some((axis, local, *extent)),
                Index::At(_) => None,
            })
            .collect::<Vec<_>>();
        for &(axis, local, extent) in &loops {
            // Only a flat shape starts past its first cell.
            let from = if axis == 0 { start } else { 0 };
            self.open_loop(local, from, extent);
        }
        self.cell_levels.push(Level {
            axes,
            strides: row_major_strides(shape),
        });
        body(self);
        self.cell_levels.pop();
        for &(_, local, _) in loops.iter().rev() {
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
        for &(base, local) in &bases.held {
            let range = CellRange {
                base,
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
                I::LocalSet(local),
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

/// The cell locals a function needs: for its most demanding operation, in
/// the program or any region of it, a counter for each axis it may loop over
/// (its largest register rank, plus the summed axis of a product) and a base
/// local for each register it names, plus one for an input slot.
pub(super) fn cell_locals(program: &solve::TypedProgram) -> u32 {
    use solve::SolveOperation as O;
    let types = program.register_types();
    program
        .operations()
        .iter()
        .map(|operation| {
            let operation = operation.operation();
            let (mut registers, mut rank) = (0u32, 0u32);
            let mut count = |register: solve::SolveRegisterId| {
                registers += 1;
                rank = rank.max(types[register.index()].dimensions().len() as u32);
            };
            operation.visit_input_registers(&mut count);
            operation.visit_output_registers(&mut count);
            let own = rank + 1 + registers;
            let nested = match operation {
                O::Fold {
                    transition,
                    continuation,
                    ..
                } => cell_locals(transition.body()).max(
                    continuation
                        .as_ref()
                        .map_or(0, |predicate| cell_locals(predicate.body())),
                ),
                O::Map { body, .. } => cell_locals(body.body()),
                O::Conditional {
                    if_true, if_false, ..
                } => cell_locals(if_true.body()).max(cell_locals(if_false.body())),
                O::AssignedConditional { program } => cell_locals(program.if_true().body())
                    .max(cell_locals(program.if_false().body())),
                O::CheckAssertion {
                    message: solve::SolveAssertionMessage::Captures { program },
                    ..
                } => cell_locals(program.body()),
                _ => 0,
            };
            own.max(nested)
        })
        .fold(2, u32::max)
}
