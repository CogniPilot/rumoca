//! Carried state published into P (SPEC_0040 SOLVE-C101).
//!
//! The entry publishes a carried run of work Y into the P cells that hold the
//! previous value of the same state. A value the program returns unchanged is
//! already in those cells, so the part of a run that a call builds in place
//! (a piece) is committed only when the call did not return the paired cells.
use super::layout::CommitPiece;
use super::*;
use std::ops::Range;

const FLAG: MemArg = MemArg {
    offset: 0,
    align: 2,
    memory_index: 0,
};

impl BodyEmitter<'_> {
    /// A call output built in the work Y cells `first..` whose carried pieces
    /// are `pieces`: the result cell (`address` local) names the cells the
    /// callee built the output in, or storage it returned unchanged. A piece
    /// whose paired P cells the result names needs no commit (flag 0); a piece
    /// it names elsewhere is copied into the work Y cells if it is not already
    /// there and committed at the end (flag 1). The rest of the output is
    /// copied into the work Y cells when it is elsewhere.
    pub(super) fn commit_carried_output(
        &mut self,
        first: usize,
        pieces: &[CommitPiece],
        (address, bytes): (u32, u32),
    ) -> Result<(), String> {
        let target = u32::try_from(arena::layouts::cell_bytes(first)?)
            .map_err(|_| "native carried output overflows")?;
        for piece in pieces {
            self.push_result(address, piece.at);
            self.push(Instruction::LocalGet(P_PTR_PARAM));
            self.push(Instruction::I32Const(piece.p as i32));
            self.push(Instruction::I32Add);
            self.push(Instruction::I32Eq);
            self.push(Instruction::If(BlockType::Empty));
            self.set_flag(piece.flag, 0);
            self.push(Instruction::Else);
            self.set_flag(piece.flag, 1);
            self.copy_if_elsewhere(target, address, piece.at..piece.at + piece_bytes(piece));
            self.push(Instruction::End);
        }
        for gap in uncovered(bytes, pieces) {
            self.copy_if_elsewhere(target, address, gap);
        }
        Ok(())
    }

    /// `result + offset`: the address inside the callee's result.
    fn push_result(&mut self, address: u32, offset: u32) {
        self.push(Instruction::LocalGet(address));
        self.push(Instruction::I32Const(offset as i32));
        self.push(Instruction::I32Add);
    }

    /// Copy `part` of the result into the work Y cells at `target` when the
    /// result is not already there.
    fn copy_if_elsewhere(&mut self, target: u32, address: u32, part: Range<u32>) {
        let push_target = |this: &mut Self| {
            this.push(Instruction::LocalGet(OUT_PTR_PARAM));
            this.push(Instruction::I32Const((target + part.start) as i32));
            this.push(Instruction::I32Add);
        };
        push_target(self);
        self.push_result(address, part.start);
        self.push(Instruction::I32Ne);
        self.push(Instruction::If(BlockType::Empty));
        push_target(self);
        self.push_result(address, part.start);
        self.push(Instruction::I32Const((part.end - part.start) as i32));
        self.push(Instruction::MemoryCopy {
            src_mem: 0,
            dst_mem: 0,
        });
        self.push(Instruction::End);
    }

    fn set_flag(&mut self, flag: u32, value: i32) {
        self.call_address(flag);
        self.push(Instruction::I32Const(value));
        self.push(Instruction::I32Store(FLAG));
    }

    /// Publish the work Y cells that are not carried to the host Y, and each
    /// carried run to its P cells: the pieces a call store decided when their
    /// flag is set, every other cell of the run unconditionally.
    pub(super) fn publish_work_y(&mut self) -> Result<(), String> {
        let plan = self.calls.ok_or("missing native call layout")?;
        let layout = plan.view();
        let mut cursor = 0u32;
        let mut hosted = Vec::new();
        for run in &layout.carried {
            hosted.push(cursor..run.y.start);
            cursor = run.y.end;
        }
        hosted.push(cursor..layout.host_y_bytes);
        for range in hosted.into_iter().filter(|range| range.start < range.end) {
            self.copy_work_y(Some(layout.saved_y), range.clone(), range.start);
        }
        let mut pieces = layout.pieces.borrow().clone();
        pieces.sort_by_key(|piece| piece.y.start);
        for run in &layout.carried {
            let mut cursor = run.y.start;
            let own = pieces
                .iter()
                .filter(|piece| run.y.start <= piece.y.start && piece.y.end <= run.y.end);
            for piece in own {
                self.publish_run_part(run.y.start, run.p, cursor..piece.y.start);
                self.call_address(piece.flag);
                self.push(Instruction::I32Load(FLAG));
                self.push(Instruction::If(BlockType::Empty));
                self.publish_run_part(run.y.start, run.p, piece.y.clone());
                self.push(Instruction::End);
                cursor = piece.y.end;
            }
            self.publish_run_part(run.y.start, run.p, cursor..run.y.end);
        }
        Ok(())
    }

    /// Copy the work Y bytes `part` of the run starting at work byte `y` to the
    /// P bytes of the run starting at `p`.
    fn publish_run_part(&mut self, y: u32, p: u32, part: Range<u32>) {
        if part.start < part.end {
            self.copy_work_y(None, part.clone(), p + (part.start - y));
        }
    }

    /// Copy `range` of the work Y: to the host Y local `host` at `at`, or to P.
    fn copy_work_y(&mut self, host: Option<u32>, range: Range<u32>, at: u32) {
        match host {
            Some(local) => self.push(Instruction::LocalGet(local)),
            None => self.push(Instruction::LocalGet(P_PTR_PARAM)),
        }
        self.push(Instruction::I32Const(at as i32));
        self.push(Instruction::I32Add);
        self.push(Instruction::LocalGet(Y_PTR_PARAM));
        self.push(Instruction::I32Const(range.start as i32));
        self.push(Instruction::I32Add);
        self.push(Instruction::I32Const((range.end - range.start) as i32));
        self.push(Instruction::MemoryCopy {
            src_mem: 0,
            dst_mem: 0,
        });
    }
}

fn piece_bytes(piece: &CommitPiece) -> u32 {
    piece.y.end - piece.y.start
}

/// The parts of an output of `bytes` bytes that no piece covers, in order.
pub(super) fn uncovered(bytes: u32, pieces: &[CommitPiece]) -> Vec<Range<u32>> {
    let mut parts = Vec::new();
    let mut cursor = 0u32;
    for piece in pieces {
        if cursor < piece.at {
            parts.push(cursor..piece.at);
        }
        cursor = piece.at + piece_bytes(piece);
    }
    if cursor < bytes {
        parts.push(cursor..bytes);
    }
    parts
}

impl BodyEmitter<'_> {
    /// Publish the staged typed output lanes: each carried lane run into the
    /// input lane cells of its previous value, every other byte to the output
    /// lanes (SOLVE-C101).
    pub(super) fn publish_lanes(&mut self) -> Result<(), String> {
        let plan = self.calls.ok_or("missing native call layout")?;
        let layout = plan.view();
        let mut cursor = 0u32;
        for run in &layout.carried_lanes {
            self.copy_lanes(layout.input_lane_bytes + cursor, cursor..run.from.start);
            self.copy_lanes(run.to, run.from.clone());
            cursor = run.from.end;
        }
        self.copy_lanes(layout.input_lane_bytes + cursor, cursor..layout.lane_bytes);
        Ok(())
    }

    /// Copy the staged lane bytes `from` to `to` bytes past the host lane base.
    fn copy_lanes(&mut self, to: u32, from: Range<u32>) {
        if from.start >= from.end {
            return;
        }
        let lanes = self.calls.map_or(0, |plan| plan.view().lanes);
        self.push(Instruction::LocalGet(OUT_PTR_PARAM));
        self.push(Instruction::I32Const(to as i32));
        self.push(Instruction::I32Add);
        self.call_address(lanes + from.start);
        self.push(Instruction::I32Const((from.end - from.start) as i32));
        self.push(Instruction::MemoryCopy {
            src_mem: 0,
            dst_mem: 0,
        });
    }
}
