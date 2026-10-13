use std::{ops::Range, sync::Arc};

use super::RegisterVersionInventory;
use crate::{LinearOp, Reg, ScalarProgramRegisterFlow, linear_op::op_read_register_ranges};

/// One source-point operand piece, backed by one immutable write version.
#[derive(Clone, Debug)]
pub struct RegisterOperandPiece {
    pub(crate) registers: Range<usize>,
    pub(crate) values: Range<usize>,
    pub(crate) producer: usize,
    pub(crate) selection_start: usize,
    pub(crate) selection_stride: usize,
}

impl RegisterOperandPiece {
    /// Original checked selection origin and stride, shared by its operand pieces.
    pub fn selection(&self) -> (usize, usize) {
        (self.selection_start, self.selection_stride)
    }

    pub fn producer(&self) -> usize {
        self.producer
    }
    pub fn registers(&self) -> Range<usize> {
        self.registers.clone()
    }
    pub fn values(&self) -> Range<usize> {
        self.values.clone()
    }
    pub fn selected(&self, register: usize) -> bool {
        register >= self.selection_start
            && (if self.selection_stride == 0 {
                register == self.selection_start
            } else {
                (register - self.selection_start).is_multiple_of(self.selection_stride)
            })
    }
}

#[derive(Clone, Debug)]
pub struct RegisterOperationVersions {
    operands: Box<[RegisterOperandPiece]>,
    destination: Option<(Reg, Range<usize>)>,
}

impl RegisterOperationVersions {
    pub fn operands(&self) -> &[RegisterOperandPiece] {
        &self.operands
    }
    pub fn destination(&self) -> Option<(Reg, Range<usize>)> {
        self.destination.clone()
    }
    pub fn source(&self, register: Reg) -> Option<(usize, usize)> {
        let register = register as usize;
        self.operands.iter().find_map(|piece| {
            (piece.registers.contains(&register) && piece.selected(register)).then(|| {
                (
                    piece.producer,
                    piece.values.start + register - piece.registers.start,
                )
            })
        })
    }
}

/// Source-point register versions of one checked linear program. Every write
/// owns a compact storage range; operands retain the exact prior write owner.
#[derive(Clone, Debug)]
pub struct RegisterVersionProgram {
    operations: Box<[RegisterOperationVersions]>,
    storage_count: usize,
    single_writer: bool,
}

#[derive(Clone)]
struct Write {
    operation: usize,
    storage: usize,
}

impl RegisterVersionProgram {
    pub fn derive(row: &[LinearOp]) -> Option<Self> {
        ScalarProgramRegisterFlow::derive(row).ok()?;
        Self::derive_checked(row)
    }

    /// Prove single-writer admission without constructing unused operand views
    /// or version payload storage for the common immutable-register profile.
    /// `capture_count` is set for a region whose captures come from an
    /// enclosing function conditional with that many values.
    pub fn for_mutable_program_with_captures(
        row: &[LinearOp],
        capture_count: Option<usize>,
    ) -> Option<Option<Self>> {
        match capture_count {
            Some(count) => ScalarProgramRegisterFlow::derive_with_conditional_captures(row, count),
            None => ScalarProgramRegisterFlow::derive(row),
        }
        .ok()?;
        let mut inventory = RegisterVersionInventory::default();
        let owner = Arc::new(());
        for op in row {
            let Some(start) = op.dst_register() else {
                continue;
            };
            let count = op.dst_register_count();
            if overlaps(&inventory, start, count) {
                return Some(Some(Self::derive_checked(row)?));
            }
            inventory.install(start, count, owner.clone());
        }
        Some(None)
    }

    fn derive_checked(row: &[LinearOp]) -> Option<Self> {
        let mut inventory = RegisterVersionInventory::<Write>::default();
        let mut operations = Vec::with_capacity(row.len());
        let mut storage_count = 0usize;
        let mut single_writer = true;
        for (index, op) in row.iter().enumerate() {
            let operands = capture_operands(&inventory, op)?;
            let destination = install_destination(
                &mut inventory,
                op,
                index,
                &mut storage_count,
                &mut single_writer,
            )?;
            operations.push(RegisterOperationVersions {
                operands: operands.into(),
                destination,
            });
        }
        Some(Self {
            operations: operations.into(),
            storage_count,
            single_writer,
        })
    }
    pub fn operations(&self) -> &[RegisterOperationVersions] {
        &self.operations
    }
    pub fn storage_count(&self) -> usize {
        self.storage_count
    }
    pub fn single_writer(&self) -> bool {
        self.single_writer
    }
}

fn capture_operands(
    inventory: &RegisterVersionInventory<Write>,
    op: &LinearOp,
) -> Option<Vec<RegisterOperandPiece>> {
    let mut operands = Vec::new();
    for read in op_read_register_ranges(op)? {
        let view = inventory.strided_view(read.start, read.count, read.stride, 1)?;
        for (low, high, piece) in view.pieces {
            let first = if read.stride == 0 {
                0
            } else {
                low.div_ceil(read.stride).checked_mul(read.stride)?
            };
            if first < low || first >= high {
                continue;
            }
            let width = high - low;
            let values = piece.family.storage + piece.offset;
            operands.push(RegisterOperandPiece {
                registers: read.start as usize + low..read.start as usize + high,
                values: values..values + width,
                producer: piece.family.operation,
                selection_start: read.start as usize,
                selection_stride: read.stride,
            });
        }
    }
    Some(operands)
}

fn install_destination(
    inventory: &mut RegisterVersionInventory<Write>,
    op: &LinearOp,
    index: usize,
    storage_count: &mut usize,
    single_writer: &mut bool,
) -> Option<Option<(Reg, Range<usize>)>> {
    let Some(start) = op.dst_register() else {
        return Some(None);
    };
    let count = op.dst_register_count();
    let end = storage_count.checked_add(count)?;
    let register_end = u64::from(start).checked_add(count as u64)?;
    if register_end > u64::from(Reg::MAX) + 1 {
        return None;
    }
    if overlaps(inventory, start, count) {
        *single_writer = false;
    }
    inventory.install(
        start,
        count,
        Arc::new(Write {
            operation: index,
            storage: *storage_count,
        }),
    );
    let range = *storage_count..end;
    *storage_count = end;
    Some(Some((start, range)))
}

fn overlaps<T>(inventory: &RegisterVersionInventory<T>, start: Reg, count: usize) -> bool {
    let end = u64::from(start) + count as u64;
    let first = inventory
        .range(..=start)
        .next_back()
        .map_or(start, |(&key, _)| key);
    inventory
        .range(first..)
        .take_while(|(key, _)| u64::from(**key) < end)
        .any(|(_, piece)| piece.end > u64::from(start))
}
