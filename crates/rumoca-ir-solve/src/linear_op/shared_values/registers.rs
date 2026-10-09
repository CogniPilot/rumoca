//! Register fields of the operations a shared-value segment renames, and the
//! exact registers any operation reads.

use super::super::*;

/// How an operation uses one register field.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum Role {
    /// The first register the operation writes.
    Destination,
    /// One scalar source register.
    Scalar,
    /// The first register of a source range.
    RangeStart,
}

/// Whether a segment renames `op`: a pure value of scalar or range sources.
/// An operation with an effect, a nested body, a table, a seed, or an indexed
/// load is not renamed, nor is a copy or an output store (the segment builder
/// handles those itself). [`visit_registers`] visits every register field of
/// exactly these operations.
pub(super) fn renamable(op: &LinearOp) -> bool {
    match op {
        LinearOp::Const { .. }
        | LinearOp::LoadTime { .. }
        | LinearOp::LoadP { .. }
        | LinearOp::Unary { .. }
        | LinearOp::Binary { .. }
        | LinearOp::Compare { .. }
        | LinearOp::Select { .. }
        | LinearOp::DotProduct { .. }
        | LinearOp::MatrixMultiply { .. }
        | LinearOp::TensorBinary { .. }
        | LinearOp::TensorCross { .. }
        | LinearOp::TensorTranspose { .. }
        | LinearOp::TensorConcatenate { .. }
        | LinearOp::TensorFill { .. }
        | LinearOp::TensorIdentity { .. }
        | LinearOp::TensorLoad {
            input: TensorInputKind::P,
            seed_start: None,
            lanes: 1,
            ..
        } => true,
        LinearOp::PureCall { site, .. } => pure_value_call(site),
        _ => false,
    }
}

/// Visit every register field of a [`renamable`] operation in field order,
/// sources before the destination.
pub(super) fn visit_registers(op: &mut LinearOp, visit: &mut dyn FnMut(Role, &mut Reg)) {
    use Role::{Destination as D, Scalar as S};
    match op {
        LinearOp::Const { dst, .. } | LinearOp::LoadTime { dst } | LinearOp::LoadP { dst, .. } => {
            visit(D, dst);
        }
        LinearOp::Unary { dst, arg, .. } => {
            visit(S, arg);
            visit(D, dst);
        }
        LinearOp::Binary { dst, lhs, rhs, .. } | LinearOp::Compare { dst, lhs, rhs, .. } => {
            visit(S, lhs);
            visit(S, rhs);
            visit(D, dst);
        }
        LinearOp::Select {
            dst,
            cond,
            if_true,
            if_false,
        } => {
            visit(S, cond);
            visit(S, if_true);
            visit(S, if_false);
            visit(D, dst);
        }
        _ => visit_range_registers(op, visit),
    }
}

/// The tensor and call operations, whose sources are register ranges.
fn visit_range_registers(op: &mut LinearOp, visit: &mut dyn FnMut(Role, &mut Reg)) {
    use Role::{Destination as D, RangeStart as R};
    match op {
        LinearOp::DotProduct {
            dst,
            lhs_start,
            rhs_start,
            ..
        } => {
            visit(R, lhs_start);
            visit(R, rhs_start);
            visit(D, dst);
        }
        LinearOp::MatrixMultiply {
            dst_start,
            lhs_start,
            rhs_start,
            ..
        }
        | LinearOp::TensorBinary {
            dst_start,
            lhs_start,
            rhs_start,
            ..
        }
        | LinearOp::TensorCross {
            dst_start,
            lhs_start,
            rhs_start,
            ..
        } => {
            visit(R, lhs_start);
            visit(R, rhs_start);
            visit(D, dst_start);
        }
        LinearOp::TensorTranspose {
            dst_start,
            src_start: start,
            ..
        }
        | LinearOp::TensorFill {
            dst_start,
            value_start: start,
            ..
        } => {
            visit(R, start);
            visit(D, dst_start);
        }
        LinearOp::TensorConcatenate {
            dst_start, sources, ..
        } => {
            for source in sources.iter_mut() {
                visit(R, &mut source.start);
            }
            visit(D, dst_start);
        }
        LinearOp::TensorIdentity { dst_start, .. } | LinearOp::TensorLoad { dst_start, .. } => {
            visit(D, dst_start);
        }
        LinearOp::PureCall {
            dst_start,
            input_starts,
            ..
        } => {
            for start in input_starts.iter_mut() {
                visit(R, start);
            }
            visit(D, dst_start);
        }
        _ => {}
    }
}

/// A call whose outputs are values only: a call carrying assertion
/// predicates has an effect and is never renamed.
fn pure_value_call(site: &SolvePureCallSite) -> bool {
    site.outputs()
        .iter()
        .all(|output| output.kind() == crate::SolvePureCallOutputKind::Result)
}

/// Every register `op` reads, ascending, as its register-flow validation
/// proves them; `None` when `op` cannot be validated at the top level.
pub(super) fn read_registers(op: &LinearOp) -> Option<Vec<Reg>> {
    op_read_registers(op)
}

/// Visit every register field of an operation a fused segment holds: a
/// [`renamable`] value, a solver-slot load, a copy, or an output store.
fn visit_segment_registers(op: &mut LinearOp, visit: &mut dyn FnMut(&mut Reg)) {
    match op {
        LinearOp::LoadY { dst, .. }
        | LinearOp::TensorLoad {
            dst_start: dst,
            input: TensorInputKind::Y,
            ..
        } => visit(dst),
        LinearOp::Move { dst, src } => {
            visit(src);
            visit(dst);
        }
        LinearOp::StoreOutput { src } => visit(src),
        _ => visit_registers(op, &mut |_, register| visit(register)),
    }
}

/// Whether the value key of `op` records the offsets between the registers
/// it reads ([`super::symbolic::value_shape`]): true for every operation but a
/// [`renamable`] one whose sources are all scalar fields, whose key names its
/// operands by field alone.
pub(super) fn records_operand_offsets(op: &LinearOp) -> bool {
    if !renamable(op) {
        return true;
    }
    let mut ranged = false;
    visit_registers(&mut op.clone(), &mut |role, _| {
        ranged |= role == Role::RangeStart;
    });
    ranged
}

/// `ops`, a fused segment, renumbered onto the registers it uses, in their
/// original order. An operation whose key records operand offsets keeps
/// every register from the lowest to the highest it reads, so those offsets
/// are unchanged and every range stays contiguous; any other operation keeps
/// only the registers it reads. Every written register is kept. The cost
/// follows the operations' own operands, not the distance between them.
pub(super) fn compact_registers(mut ops: Vec<LinearOp>) -> Vec<LinearOp> {
    let mut used = Vec::new();
    for op in &ops {
        let reads = read_registers(op).unwrap_or_default();
        match (reads.first(), reads.last()) {
            (Some(&lowest), Some(&highest)) if records_operand_offsets(op) => {
                used.extend(lowest..=highest);
            }
            _ => used.extend(reads),
        }
        if let Some(start) = op.dst_register() {
            used.extend((0..op.dst_register_count()).map(|offset| start + offset as Reg));
        }
    }
    used.sort_unstable();
    used.dedup();
    for op in &mut ops {
        visit_segment_registers(op, &mut |register| {
            if let Ok(dense) = used.binary_search(register) {
                *register = dense as Reg;
            }
        });
    }
    ops
}

/// A table keyed by register, held densely: a program's registers are numbered
/// from zero, so an entry costs its value and not a hash slot, and a wide
/// operation's outputs fill one contiguous run.
#[derive(Clone, Debug)]
pub(super) struct RegisterTable<T> {
    entries: Vec<Option<T>>,
    len: usize,
}

impl<T> Default for RegisterTable<T> {
    fn default() -> Self {
        Self {
            entries: Vec::new(),
            len: 0,
        }
    }
}

impl<T: Copy> RegisterTable<T> {
    /// The value of `register`.
    pub(super) fn get(&self, register: Reg) -> Option<T> {
        self.entries.get(register as usize).copied().flatten()
    }

    /// Set the value of `register`.
    pub(super) fn insert(&mut self, register: Reg, value: T) {
        let index = register as usize;
        if index >= self.entries.len() {
            self.entries.resize(index + 1, None);
        }
        if let Some(entry) = self.entries.get_mut(index) {
            self.len += usize::from(entry.is_none());
            *entry = Some(value);
        }
    }

    /// Drop the value of `register`.
    pub(super) fn remove(&mut self, register: Reg) {
        if let Some(entry) = self.entries.get_mut(register as usize) {
            self.len -= usize::from(entry.is_some());
            *entry = None;
        }
    }

    /// How many registers hold a value.
    pub(super) const fn len(&self) -> usize {
        self.len
    }
}

/// A set of registers, held densely like [`RegisterTable`].
#[derive(Clone, Debug, Default)]
pub(super) struct RegisterSet {
    members: Vec<bool>,
}

impl RegisterSet {
    pub(super) fn contains(&self, register: Reg) -> bool {
        self.members
            .get(register as usize)
            .copied()
            .unwrap_or(false)
    }

    /// Add `register`; `false` when it was already a member.
    pub(super) fn insert(&mut self, register: Reg) -> bool {
        let index = register as usize;
        if index >= self.members.len() {
            self.members.resize(index + 1, false);
        }
        match self.members.get_mut(index) {
            Some(member) => !std::mem::replace(member, true),
            None => false,
        }
    }

    pub(super) fn remove(&mut self, register: Reg) {
        if let Some(member) = self.members.get_mut(register as usize) {
            *member = false;
        }
    }
}
