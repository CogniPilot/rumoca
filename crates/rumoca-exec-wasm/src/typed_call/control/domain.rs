//! Source-owned Cartesian domains advance the innermost binder first.
use super::*;

impl Emitter<'_> {
    pub(super) fn advance_binders(
        &mut self,
        slots: &[CellRange],
        domain: &StructuredIndexDomain,
        last: &[i64],
    ) {
        // The caller exits before advancing the final tuple. A reset occurs
        // only when this axis is at its exact attained endpoint; otherwise the
        // checked domain proves the next addition is representable in i64.
        self.push(I::Block(BlockType::Empty));
        for ((slot, binder), final_value) in slots.iter().zip(&domain.binders).zip(last).rev() {
            self.address(*slot);
            self.push(I::I64Load(CELL));
            self.push(I::I64Const(*final_value));
            self.push(I::I64Eq);
            self.push(I::If(BlockType::Empty));
            self.store_integer(*slot, binder.lower);
            self.push(I::Else);
            self.increment(*slot, binder.step);
            self.push(I::Br(1));
            self.push(I::End);
        }
        self.push(I::End);
    }
}
