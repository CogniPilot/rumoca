//! Moving a planned body's private storage up, past registers placed later.
use super::{
    CallFrame, CellRange, FramePlan, PointerCell, ReferenceArm, TypedCallCompileError, advance,
};

impl FramePlan {
    /// Move every scratch range at or above `from` up by `delta` bytes.
    ///
    /// A body is planned before the results of the operation that owns it are
    /// known, so its private storage starts at the operation's base. Ranges the
    /// body borrows from its parent lie below `from` and stay where they are.
    pub(super) fn relocate(&mut self, from: u32, delta: u32) -> Result<(), TypedCallCompileError> {
        if delta == 0 {
            return Ok(());
        }
        if let Some(observations) = &mut self.observations {
            move_range(&mut observations.range, from, delta)?;
        }
        for range in self
            .slots
            .iter_mut()
            .chain(&mut self.registers)
            .chain(&mut self.assigned_states)
            .chain(&mut self.assigned_inputs)
            .chain(&mut self.assigned_outputs)
            .chain(std::iter::once(&mut self.counter))
        {
            move_range(range, from, delta)?;
        }
        for reference in self.references.iter_mut().flatten().flatten() {
            if reference.cell >= from {
                advance(&mut reference.cell, delta)?;
            }
            for arm in &mut reference.arms {
                move_arm(arm, from, delta)?;
            }
        }
        for frame in self.calls.iter_mut().flatten() {
            move_frame(frame, from, delta)?;
        }
        for region in self.regions.iter_mut().flatten() {
            region.relocate(from, delta)?;
        }
        advance(&mut self.base, delta)?;
        advance(&mut self.scratch_bytes, delta)?;
        advance(&mut self.cursor, delta)
    }
}

fn move_frame(frame: &mut CallFrame, from: u32, delta: u32) -> Result<(), TypedCallCompileError> {
    for range in [&mut frame.input, &mut frame.output, &mut frame.scratch] {
        move_range(range, from, delta)?;
    }
    for destination in &mut frame.buffers {
        move_range(&mut destination.buffer, from, delta)?;
    }
    Ok(())
}

fn move_range(range: &mut CellRange, from: u32, delta: u32) -> Result<(), TypedCallCompileError> {
    match range.pointer_cell() {
        Some(PointerCell::Scratch(cell)) if cell >= from => advance(&mut range.base, delta)?,
        Some(_) => {}
        None if range.base == 2 && range.offset >= from => advance(&mut range.offset, delta)?,
        None => {}
    }
    Ok(())
}

fn move_arm(arm: &mut ReferenceArm, from: u32, delta: u32) -> Result<(), TypedCallCompileError> {
    match arm {
        ReferenceArm::Pass => Ok(()),
        ReferenceArm::Build(range) => move_range(range, from, delta),
        ReferenceArm::Forward(cell) if *cell >= from => advance(cell, delta),
        ReferenceArm::Forward(_) => Ok(()),
    }
}
