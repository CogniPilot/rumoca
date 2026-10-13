//! Scalar registers in wasm locals: reads and writes by home.
//!
//! The planner (`layout::homes`) decides which single-cell registers have no
//! address; every operation that may read or define one goes through these
//! helpers, so a register in a local is never reached as a memory range.
use super::emit::{CELL, Emitter};
use super::layout::{CellRange, Home, LocalCounts};
use rumoca_ir_solve as solve;
use wasm_encoder::{Instruction as I, ValType};

/// Locals 0..13 are the call parameters and the emitter's transient locals.
pub(super) const FIRST_PROMOTED_LOCAL: u32 = 13;

/// The transient locals every body declares, then the promoted ones.
pub(super) fn locals_header(total: LocalCounts) -> Vec<(u32, ValType)> {
    let mut locals = vec![
        (1, ValType::I32),
        (4, ValType::I64),
        (2, ValType::F64),
        (3, ValType::I32),
    ];
    if total.integers > 0 {
        locals.push((total.integers, ValType::I64));
    }
    if total.reals > 0 {
        locals.push((total.reals, ValType::F64));
    }
    locals
}

/// The load of one eight-byte cell as a Real or as raw Integer/Boolean bits.
pub(super) fn cell_load(real: bool) -> I<'static> {
    if real {
        I::F64Load(CELL)
    } else {
        I::I64Load(CELL)
    }
}

/// The store matching [`cell_load`].
pub(super) fn cell_store(real: bool) -> I<'static> {
    if real {
        I::F64Store(CELL)
    } else {
        I::I64Store(CELL)
    }
}

impl Emitter<'_> {
    pub(super) fn home(&self, register: solve::SolveRegisterId) -> Home {
        self.plan.homes[register.index()]
    }

    pub(super) fn is_real(&self, register: solve::SolveRegisterId) -> bool {
        matches!(self.scalar(register), solve::SolveScalarType::Real { .. })
    }

    fn promoted_local(&self, ordinal: u32, real: bool) -> u32 {
        if real {
            FIRST_PROMOTED_LOCAL + self.local_total.integers + self.local_floor.reals + ordinal
        } else {
            FIRST_PROMOTED_LOCAL + self.local_floor.integers + ordinal
        }
    }

    /// Push the value of a single-cell register.
    pub(super) fn push_scalar(&mut self, register: solve::SolveRegisterId) {
        if let Some(value) = self.known_integer(register) {
            self.push_constant(value, false);
            return;
        }
        match self.home(register) {
            Home::Immediate { bits, real } => self.push_constant(bits, real),
            Home::Local { ordinal, real } => {
                let local = self.promoted_local(ordinal, real);
                self.push(I::LocalGet(local));
            }
            // The planner leaves no read of an unread register.
            Home::Unused => self.push(I::Unreachable),
            Home::Memory => {
                let real = self.is_real(register);
                self.address(self.reg(register));
                self.push(cell_load(real));
            }
        }
    }

    pub(super) fn push_constant(&mut self, bits: i64, real: bool) {
        self.push(if real {
            I::F64Const(f64::from_bits(bits as u64).into())
        } else {
            I::I64Const(bits)
        });
    }

    /// Prepare to store a value: a register in memory needs its address first.
    pub(super) fn begin_result(&mut self, register: solve::SolveRegisterId) {
        if self.home(register) == Home::Memory {
            self.address(self.reg(register));
        }
    }

    /// Consume the value on the stack into the register `begin_result` opened.
    pub(super) fn end_result(&mut self, register: solve::SolveRegisterId) {
        match self.home(register) {
            Home::Memory => {
                self.push(cell_store(self.is_real(register)));
            }
            Home::Local { ordinal, real } => {
                let local = self.promoted_local(ordinal, real);
                self.push(I::LocalSet(local));
            }
            // A folded constant and an unread register define no code.
            Home::Immediate { .. } | Home::Unused => self.push(I::Drop),
        }
    }

    /// Push one cell of a register: its scalar value, or the cell selected by
    /// the cell index (local 3) of an aggregate.
    pub(super) fn load_register(&mut self, register: solve::SolveRegisterId, real: bool) {
        let range = self.reg(register);
        if self.home(register) != Home::Memory || range.bytes == 8 {
            self.push_scalar(register);
        } else {
            self.load_cell(range, real);
        }
    }

    /// Copy a register into a slot: a scalar from its home, any other value by
    /// range.
    pub(super) fn store_slot(&mut self, slot: CellRange, source: solve::SolveRegisterId) {
        if self.home(source) == Home::Memory {
            self.copy(slot, self.reg(source));
        } else {
            self.address(slot);
            self.push_scalar(source);
            self.push(cell_store(self.is_real(source)));
        }
    }

    /// Write the scalar `value` into every cell of `target`: zero by one bulk
    /// fill, any other value by one store followed by doubling copies, so the
    /// code is logarithmic in the extent.
    pub(super) fn fill_aggregate(&mut self, target: CellRange, value: solve::SolveRegisterId) {
        const INLINE_BYTES: u32 = super::cost::INLINE_MOVE_BYTES;
        if target.bytes == 0 {
            return;
        }
        if matches!(self.home(value), Home::Immediate { bits: 0, .. }) {
            self.address(target);
            self.push(I::I32Const(0));
            self.push(I::I32Const(target.bytes as i32));
            self.push(I::MemoryFill(0));
            return;
        }
        if target.bytes <= INLINE_BYTES {
            // A fill the bulk call would not pay for is one store per cell at a
            // constant address.
            for cell in 0..target.bytes / 8 {
                self.address(CellRange {
                    offset: target.offset + cell * 8,
                    bytes: 8,
                    ..target
                });
                self.push_scalar(value);
                self.push(cell_store(self.is_real(value)));
            }
            return;
        }
        self.address(target);
        self.push_scalar(value);
        self.push(cell_store(self.is_real(value)));
        let mut filled = 8;
        while filled < target.bytes {
            let length = filled.min(target.bytes - filled);
            self.address(CellRange {
                offset: target.offset + filled,
                ..target
            });
            self.address(target);
            self.push(I::I32Const(length as i32));
            self.push(I::MemoryCopy {
                src_mem: 0,
                dst_mem: 0,
            });
            filled += length;
        }
    }
}
