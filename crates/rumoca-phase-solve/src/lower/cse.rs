use indexmap::IndexMap;
use rumoca_ir_solve::{BinaryOp, CompareOp, Reg, UnaryOp};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(super) enum SlotLoadKey {
    Time,
    Y(usize),
    P(usize),
    Constant(u64),
}

#[derive(Debug, Clone, Default)]
pub(super) struct RowCse {
    pub(super) slots: IndexMap<SlotLoadKey, Reg>,
    pub(super) unary: IndexMap<(UnaryOp, Reg), Reg>,
    pub(super) binary: IndexMap<(BinaryOp, Reg, Reg), Reg>,
    pub(super) compare: IndexMap<(CompareOp, Reg, Reg), Reg>,
    pub(super) select: IndexMap<(Reg, Reg, Reg), Reg>,
}

pub(super) fn canonical_binary_key(op: BinaryOp, lhs: Reg, rhs: Reg) -> (BinaryOp, Reg, Reg) {
    if matches!(
        op,
        BinaryOp::Add
            | BinaryOp::Mul
            | BinaryOp::And
            | BinaryOp::Or
            | BinaryOp::Min
            | BinaryOp::Max
    ) && rhs < lhs
    {
        return (op, rhs, lhs);
    }
    (op, lhs, rhs)
}
