//! Final bounded Gauss-Jordan lowering of the original dense component.
use super::*;

#[derive(Clone, Copy)]
struct Frame {
    start: Reg,
    size: usize,
    column: u32,
    pivot: u32,
    row: u32,
    entry: u32,
}

pub(super) fn workspace_cells(rows: &[Vec<LinearOp>]) -> Result<usize, String> {
    let mut maximum = 0;
    conditional::visit_operations(rows, |operation| {
        if let LinearOp::LinearSolveComponent { n, .. } = operation {
            let cells = n
                .checked_add(1)
                .and_then(|stride| n.checked_mul(stride))
                .ok_or("dense augmented workspace extent overflows")?;
            maximum = maximum.max(cells);
        }
        Ok(())
    })?;
    Ok(maximum)
}

impl BodyEmitter<'_> {
    pub(super) fn emit_dense_linear_solve(&mut self, operation: &LinearOp) -> Result<(), String> {
        let LinearOp::LinearSolveComponent {
            dst,
            matrix_start,
            rhs_start,
            n,
            component,
        } = *operation
        else {
            return Err("expected original dense solve component".into());
        };
        let arena = self.arena_plan()?;
        let frame = Frame {
            start: arena
                .dense_workspace
                .ok_or("dense operation has no issued workspace")?,
            size: n,
            column: arena.inner_counter + 1,
            pivot: arena.inner_counter + 2,
            row: arena.counter,
            entry: arena.inner_counter,
        };
        let status = self.dense_fault(n, component)?;
        self.copy_dense_inputs(frame, matrix_start, rhs_start)?;
        self.loop_start(frame.column, n)?;
        self.dense_column(frame, status)?;
        self.loop_end(frame.column);
        self.loop_start(frame.row, n)?;
        self.dense_load(frame, frame.row, None)?;
        self.dense_require_finite(status);
        self.loop_end(frame.row);
        self.push(Instruction::I32Const(
            i32::try_from(component).map_err(|_| "dense component exceeds i32")?,
        ));
        self.push(Instruction::LocalSet(frame.row));
        self.dense_load(frame, frame.row, None)?;
        self.set_reg(dst)
    }

    fn dense_fault(&mut self, size: usize, component: usize) -> Result<i32, String> {
        let status = self.model_fault_status()?;
        self.linear_solve_faults
            .push(crate::NativeLinearSolveFault {
                status,
                size,
                component,
                kernel: self.kernel_ordinal,
                program: self.program_ordinal,
                operation: self.operation_ordinal,
                region_path: self.region_path.clone(),
                provenance: self
                    .program_span
                    .ok_or("dense solve has no original source span")?,
            });
        i32::try_from(status).map_err(|_| "dense status exceeds i32".into())
    }

    fn copy_dense_inputs(&mut self, frame: Frame, matrix: Reg, rhs: Reg) -> Result<(), String> {
        self.loop_start(frame.row, frame.size)?;
        self.loop_start(frame.entry, frame.size)?;
        self.dense_address(frame, frame.row, Some(frame.entry))?;
        self.push_arena_address(matrix, Some(frame.row), frame.size)?;
        self.push(Instruction::LocalGet(frame.entry));
        self.push(Instruction::I32Const(8));
        self.push(Instruction::I32Mul);
        self.push(Instruction::I32Add);
        self.push(Instruction::F64Load(arena::memarg()));
        self.push(Instruction::F64Store(arena::memarg()));
        self.loop_end(frame.entry);
        self.dense_address(frame, frame.row, None)?;
        self.push_arena_address(rhs, Some(frame.row), 1)?;
        self.push(Instruction::F64Load(arena::memarg()));
        self.push(Instruction::F64Store(arena::memarg()));
        self.loop_end(frame.row);
        Ok(())
    }

    fn dense_column(&mut self, frame: Frame, status: i32) -> Result<(), String> {
        self.select_dense_pivot(frame)?;
        self.dense_load(frame, frame.pivot, Some(frame.column))?;
        self.push(Instruction::LocalSet(LOCAL_BASE));
        self.push(Instruction::LocalGet(LOCAL_BASE));
        self.push(Instruction::F64Const(0.0.into()));
        self.push(Instruction::F64Eq);
        self.return_status_if(status);
        self.push(Instruction::LocalGet(LOCAL_BASE));
        self.dense_require_finite(status);
        self.swap_dense_rows(frame)?;
        self.normalize_dense_row(frame)?;
        self.eliminate_dense_column(frame)
    }

    fn select_dense_pivot(&mut self, frame: Frame) -> Result<(), String> {
        self.push(Instruction::LocalGet(frame.column));
        self.push(Instruction::LocalSet(frame.pivot));
        self.dense_loop_from(frame.row, frame.column, frame.size)?;
        // abs() clears the sign, so unsigned bit order is total_cmp order,
        // including NaN payloads; >= retains max_by's final equal element.
        self.dense_load(frame, frame.row, Some(frame.column))?;
        self.push(Instruction::F64Abs);
        self.push(Instruction::I64ReinterpretF64);
        self.dense_load(frame, frame.pivot, Some(frame.column))?;
        self.push(Instruction::F64Abs);
        self.push(Instruction::I64ReinterpretF64);
        self.push(Instruction::I64GeU);
        self.push(Instruction::If(BlockType::Empty));
        self.push(Instruction::LocalGet(frame.row));
        self.push(Instruction::LocalSet(frame.pivot));
        self.push(Instruction::End);
        self.loop_end(frame.row);
        Ok(())
    }

    fn swap_dense_rows(&mut self, frame: Frame) -> Result<(), String> {
        self.loop_start(frame.entry, frame.size + 1)?;
        self.dense_load(frame, frame.column, Some(frame.entry))?;
        self.push(Instruction::LocalSet(LOCAL_BASE));
        self.dense_load(frame, frame.pivot, Some(frame.entry))?;
        self.push(Instruction::LocalSet(LOCAL_BASE + 1));
        self.dense_address(frame, frame.column, Some(frame.entry))?;
        self.push(Instruction::LocalGet(LOCAL_BASE + 1));
        self.push(Instruction::F64Store(arena::memarg()));
        self.dense_address(frame, frame.pivot, Some(frame.entry))?;
        self.push(Instruction::LocalGet(LOCAL_BASE));
        self.push(Instruction::F64Store(arena::memarg()));
        self.loop_end(frame.entry);
        Ok(())
    }

    fn normalize_dense_row(&mut self, frame: Frame) -> Result<(), String> {
        self.dense_load(frame, frame.column, Some(frame.column))?;
        self.push(Instruction::LocalSet(LOCAL_BASE));
        self.dense_loop_from(frame.entry, frame.column, frame.size + 1)?;
        self.dense_address(frame, frame.column, Some(frame.entry))?;
        self.dense_load(frame, frame.column, Some(frame.entry))?;
        self.push(Instruction::LocalGet(LOCAL_BASE));
        self.push(Instruction::F64Div);
        self.push(Instruction::F64Store(arena::memarg()));
        self.loop_end(frame.entry);
        Ok(())
    }

    fn eliminate_dense_column(&mut self, frame: Frame) -> Result<(), String> {
        self.loop_start(frame.row, frame.size)?;
        self.push(Instruction::LocalGet(frame.row));
        self.push(Instruction::LocalGet(frame.column));
        self.push(Instruction::I32Ne);
        self.push(Instruction::If(BlockType::Empty));
        self.dense_load(frame, frame.row, Some(frame.column))?;
        self.push(Instruction::LocalSet(LOCAL_BASE));
        self.dense_loop_from(frame.entry, frame.column, frame.size + 1)?;
        self.dense_address(frame, frame.row, Some(frame.entry))?;
        self.dense_load(frame, frame.row, Some(frame.entry))?;
        self.push(Instruction::LocalGet(LOCAL_BASE));
        self.dense_load(frame, frame.column, Some(frame.entry))?;
        self.push(Instruction::F64Mul);
        self.push(Instruction::F64Sub);
        self.push(Instruction::F64Store(arena::memarg()));
        self.loop_end(frame.entry);
        self.push(Instruction::End);
        self.loop_end(frame.row);
        Ok(())
    }

    fn dense_require_finite(&mut self, status: i32) {
        self.push(Instruction::LocalSet(LOCAL_BASE));
        self.push(Instruction::LocalGet(LOCAL_BASE));
        self.push(Instruction::LocalGet(LOCAL_BASE));
        self.push(Instruction::F64Sub);
        self.push(Instruction::F64Const(0.0.into()));
        self.push(Instruction::F64Ne);
        self.return_status_if(status);
    }

    fn dense_load(&mut self, frame: Frame, row: u32, column: Option<u32>) -> Result<(), String> {
        self.dense_address(frame, row, column)?;
        self.push(Instruction::F64Load(arena::memarg()));
        Ok(())
    }

    fn dense_address(&mut self, frame: Frame, row: u32, column: Option<u32>) -> Result<(), String> {
        let stride = i32::try_from(frame.size + 1).map_err(|_| "dense row stride exceeds i32")?;
        let start = frame
            .start
            .checked_mul(8)
            .ok_or("dense workspace byte address overflows")?;
        self.push(Instruction::LocalGet(row));
        self.push(Instruction::I32Const(stride));
        self.push(Instruction::I32Mul);
        match column {
            Some(counter) => self.push(Instruction::LocalGet(counter)),
            None => self.push(Instruction::I32Const(stride - 1)),
        }
        self.push(Instruction::I32Add);
        self.push(Instruction::I32Const(8));
        self.push(Instruction::I32Mul);
        self.push(Instruction::I32Const(start as i32));
        self.push(Instruction::I32Add);
        self.relocate_arena_address()
    }

    fn dense_loop_from(&mut self, counter: u32, from: u32, count: usize) -> Result<(), String> {
        let count = i32::try_from(count).map_err(|_| "dense loop bound exceeds i32")?;
        self.push(Instruction::LocalGet(from));
        self.push(Instruction::LocalSet(counter));
        self.push(Instruction::Block(BlockType::Empty));
        self.push(Instruction::Loop(BlockType::Empty));
        self.push(Instruction::LocalGet(counter));
        self.push(Instruction::I32Const(count));
        self.push(Instruction::I32GeU);
        self.push(Instruction::BrIf(1));
        Ok(())
    }
}
