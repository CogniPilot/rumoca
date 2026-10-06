//! Dense products over checked direct input views; no temporary memory ABI.
//!
//! Operands are snapshots in the scalar oracle. Only input-backed, independent
//! views are admitted here, so rereading them in the generated loop has the
//! same meaning. Computed/coupled register operands require a future owned
//! scratch-storage contract and fail rather than being silently approximated.
use super::*;
use rumoca_ir_solve::{ScalarProgramRegisterFlow, TensorInputKind};

#[derive(Clone, Copy)]
struct InputView {
    pointer: u32,
    start: usize,
    stride: i32,
}

pub(super) struct MatrixPlan {
    lhs: InputView,
    rhs: InputView,
    rows: usize,
    inner: usize,
    columns: usize,
    output: usize,
}

impl MatrixPlan {
    pub(super) fn new(
        node: &ComputeNode,
        layout: &VarLayout,
        output: usize,
    ) -> Result<Self, String> {
        let ComputeNode::MatMul {
            lhs_ops,
            lhs_start,
            rhs_ops,
            rhs_start,
            m,
            k,
            n,
            ..
        } = node
        else {
            return Err("WASM matrix plan requires MatMul".into());
        };
        let lhs_count = product(*m, *k)?;
        let rhs_count = product(*k, *n)?;
        let count = product(*m, *n)?;
        let end = output
            .checked_add(count)
            .ok_or("WASM matrix output overflow")?;
        checked_slot_count(end)?;
        let lhs_end = (*lhs_start as usize)
            .checked_add(lhs_count)
            .ok_or("WASM matrix register overflow")?;
        let rhs_end = (*rhs_start as usize)
            .checked_add(rhs_count)
            .ok_or("WASM matrix register overflow")?;
        if lhs_count > 0
            && rhs_count > 0
            && (*lhs_start as usize) < rhs_end
            && (*rhs_start as usize) < lhs_end
        {
            return Err("WASM matrix operand register ranges alias".into());
        }
        Ok(Self {
            lhs: input_view(lhs_ops, *lhs_start, lhs_count, layout)?,
            rhs: input_view(rhs_ops, *rhs_start, rhs_count, layout)?,
            rows: *m,
            inner: *k,
            columns: *n,
            output,
        })
    }

    pub(super) fn output_end(&self) -> Result<usize, String> {
        self.output
            .checked_add(product(self.rows, self.columns)?)
            .ok_or_else(|| "WASM matrix output overflow".into())
    }
}

fn product(a: usize, b: usize) -> Result<usize, String> {
    let count = a.checked_mul(b).ok_or("WASM matrix shape overflow")?;
    checked_slot_count(count)?;
    Ok(count)
}

fn input_view(
    ops: &[LinearOp],
    register: Reg,
    count: usize,
    layout: &VarLayout,
) -> Result<InputView, String> {
    if count == 0 && ops.is_empty() {
        return Ok(InputView {
            pointer: Y_PTR_PARAM,
            start: 0,
            stride: 1,
        });
    }
    // This canonical checker establishes lane widths, initialized registers,
    // and overflow-safe range ownership before this backend selects a subset.
    ScalarProgramRegisterFlow::derive(ops).map_err(|e| e.to_string())?;
    affine::validate_loads(ops, &[], layout)?;
    if let [
        LinearOp::TensorLoad {
            dst_start,
            input,
            input_start,
            count: length,
            seed_start: None,
            lanes: 1,
        },
    ] = ops
        && *dst_start == register
        && *length == count
    {
        return Ok(InputView {
            pointer: input_pointer(*input),
            start: *input_start,
            stride: 1,
        });
    }
    scalar_load_view(ops, register, count)
}

fn input_pointer(input: TensorInputKind) -> u32 {
    match input {
        TensorInputKind::Y => Y_PTR_PARAM,
        TensorInputKind::P => P_PTR_PARAM,
    }
}

fn scalar_load_view(ops: &[LinearOp], register: Reg, count: usize) -> Result<InputView, String> {
    if ops.len() != count || count == 0 {
        return Err("WASM matrix operands require independent direct input views".into());
    }
    let mut result = InputView {
        pointer: 0,
        start: 0,
        stride: 0,
    };
    for (offset, op) in ops.iter().enumerate() {
        let (dst, index, pointer) = match *op {
            LinearOp::LoadY { dst, index } => (dst, index, Y_PTR_PARAM),
            LinearOp::LoadP { dst, index } => (dst, index, P_PTR_PARAM),
            _ => return Err("WASM matrix computed operand setup is unsupported".into()),
        };
        if dst as usize != register as usize + offset {
            return Err("WASM matrix input registers must be contiguous".into());
        }
        if offset == 0 {
            result.pointer = pointer;
            result.start = index;
        }
        if offset == 1 {
            result.stride = i32::try_from(index as i128 - result.start as i128)
                .map_err(|_| "WASM matrix input stride overflow")?;
        }
        if pointer != result.pointer
            || index as i128 != result.start as i128 + offset as i128 * i128::from(result.stride)
        {
            return Err("WASM matrix input view is not affine".into());
        }
    }
    Ok(result)
}

impl BodyEmitter<'_> {
    pub(super) fn guard_matrix_buffers(
        &mut self,
        layout: &VarLayout,
        outputs: usize,
        temporary: u32,
    ) -> Result<(), String> {
        if outputs == 0 {
            return Ok(());
        }
        // Snapshot operands cannot be reread safely when a caller aliases the
        // output buffer onto an input. Reject before clearing any output.
        self.push(Instruction::LocalGet(OUT_PTR_PARAM));
        self.push(Instruction::I32Const(
            i32::try_from(outputs * 8).map_err(|_| "WASM matrix output bytes exceed i32")?,
        ));
        self.push(Instruction::I32Add);
        self.push(Instruction::LocalTee(temporary));
        self.push(Instruction::LocalGet(OUT_PTR_PARAM));
        self.push(Instruction::I32LtU);
        self.trap_if();
        for (pointer, count) in [
            (Y_PTR_PARAM, layout.y_scalars()),
            (P_PTR_PARAM, layout.p_scalars()),
        ] {
            if count == 0 {
                continue;
            }
            self.push(Instruction::LocalGet(pointer));
            self.push(Instruction::I32Const(
                i32::try_from(count * 8).map_err(|_| "WASM matrix input bytes exceed i32")?,
            ));
            self.push(Instruction::I32Add);
            self.push(Instruction::LocalTee(temporary + 1));
            self.push(Instruction::LocalGet(pointer));
            self.push(Instruction::I32LtU);
            self.trap_if();
            self.push(Instruction::LocalGet(OUT_PTR_PARAM));
            self.push(Instruction::LocalGet(temporary + 1));
            self.push(Instruction::I32LtU);
            self.push(Instruction::LocalGet(pointer));
            self.push(Instruction::LocalGet(temporary));
            self.push(Instruction::I32LtU);
            self.push(Instruction::I32And);
            self.trap_if();
        }
        Ok(())
    }

    fn trap_if(&mut self) {
        self.push(Instruction::If(BlockType::Empty));
        self.push(Instruction::Unreachable);
        self.push(Instruction::End);
    }

    pub(super) fn emit_matrix(
        &mut self,
        plan: &MatrixPlan,
        counter: u32,
        accumulator: u32,
    ) -> Result<(), String> {
        let count = product(plan.rows, plan.columns)?;
        if count == 0 {
            return Ok(());
        }
        self.loop_start(counter, count)?;
        if plan.inner == 0 {
            self.push(Instruction::F64Const(0.0.into()));
            self.push(Instruction::LocalSet(accumulator));
        } else {
            self.push(Instruction::I32Const(0));
            self.push(Instruction::LocalSet(counter + 1));
            self.matrix_product(plan, counter);
            self.push(Instruction::LocalSet(accumulator));
            self.loop_start_from(counter + 1, 1, plan.inner)?;
            self.push(Instruction::LocalGet(accumulator));
            self.matrix_product(plan, counter);
            self.push(Instruction::F64Add);
            self.push(Instruction::LocalSet(accumulator));
            self.loop_end(counter + 1);
        }
        self.push(Instruction::LocalGet(OUT_PTR_PARAM));
        self.push(Instruction::LocalGet(counter));
        self.push(Instruction::I32Const(plan.output as i32));
        self.push(Instruction::I32Add);
        self.push(Instruction::I32Const(8));
        self.push(Instruction::I32Mul);
        self.push(Instruction::I32Add);
        self.push(Instruction::LocalGet(accumulator));
        self.push(Instruction::F64Store(MemArg {
            offset: 0,
            align: 3,
            memory_index: 0,
        }));
        self.loop_end(counter);
        Ok(())
    }

    fn matrix_product(&mut self, plan: &MatrixPlan, counter: u32) {
        self.push(Instruction::LocalGet(plan.lhs.pointer));
        self.push(Instruction::LocalGet(counter));
        self.push(Instruction::I32Const(plan.columns as i32));
        self.push(Instruction::I32DivU);
        self.push(Instruction::I32Const(plan.inner as i32));
        self.push(Instruction::I32Mul);
        self.push(Instruction::LocalGet(counter + 1));
        self.push(Instruction::I32Add);
        self.view_load(plan.lhs);
        self.push(Instruction::LocalGet(plan.rhs.pointer));
        self.push(Instruction::LocalGet(counter + 1));
        self.push(Instruction::I32Const(plan.columns as i32));
        self.push(Instruction::I32Mul);
        self.push(Instruction::LocalGet(counter));
        self.push(Instruction::I32Const(plan.columns as i32));
        self.push(Instruction::I32RemU);
        self.push(Instruction::I32Add);
        self.view_load(plan.rhs);
        self.push(Instruction::F64Mul);
    }

    fn view_load(&mut self, view: InputView) {
        self.push(Instruction::I32Const(view.stride));
        self.push(Instruction::I32Mul);
        self.push(Instruction::I32Const(view.start as i32));
        self.push(Instruction::I32Add);
        self.push(Instruction::I32Const(8));
        self.push(Instruction::I32Mul);
        self.push(Instruction::I32Add);
        self.push(Instruction::F64Load(MemArg {
            offset: 0,
            align: 3,
            memory_index: 0,
        }));
    }
}
