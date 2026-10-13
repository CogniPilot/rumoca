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

impl Emitter<'_> {
    /// Advance the binders held in the locals from `first`, innermost first,
    /// as `advance_binders` advances their slots.
    pub(super) fn advance_binder_locals(
        &mut self,
        first: u32,
        domain: &StructuredIndexDomain,
        last: &[i64],
    ) {
        self.push(I::Block(BlockType::Empty));
        for (ordinal, (binder, final_value)) in domain.binders.iter().zip(last).enumerate().rev() {
            let local = first + ordinal as u32;
            self.push(I::LocalGet(local));
            self.push(I::I64Const(*final_value));
            self.push(I::I64Eq);
            self.push(I::If(BlockType::Empty));
            self.push(I::I64Const(binder.lower));
            self.push(I::LocalSet(local));
            self.push(I::Else);
            self.push(I::LocalGet(local));
            self.push(I::I64Const(binder.step));
            self.push(I::I64Add);
            self.push(I::LocalSet(local));
            self.push(I::Br(1));
            self.push(I::End);
        }
        self.push(I::End);
    }
}
