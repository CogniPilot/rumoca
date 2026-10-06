//! Compact expression kernels. SPEC_0032 §4: domain extents become runtime
//! loops at final emission; the checked tensor owner is never scalarized here.

mod affine;
mod assignments;
pub(super) mod call_program;
#[cfg(test)]
mod cell_loop_tests;
mod exact_assignments;
mod matrix;
pub(crate) use assignments::emit_native_assignment_module;
pub(crate) use call_program::emit_native_call_assignment_module;
pub(crate) use exact_assignments::{emit_exact_assignment_module, emit_private_program_module};

use super::*;
use affine::AffinePlan;
use matrix::MatrixPlan;
use rumoca_ir_solve::{ComputeBlock, ComputeNode, ScalarProgramBlock, VarLayout};

enum NodePlan<'a> {
    Scalar {
        block: &'a ScalarProgramBlock,
        outputs: Vec<usize>,
    },
    Affine(AffinePlan<'a>),
    Matrix(MatrixPlan),
}

struct KernelPlan<'a> {
    nodes: Vec<NodePlan<'a>>,
    programs: Vec<Vec<LinearOp>>,
    outputs: usize,
    rank: usize,
}

pub(crate) fn emit_compute_module(
    block: &ComputeBlock,
    layout: &VarLayout,
) -> Result<Vec<u8>, String> {
    block
        .validate_shape_contract("WASM expression kernel")
        .map_err(|e| e.to_string())?;
    layout
        .validate_shape_contract()
        .map_err(|e| e.to_string())?;
    let plan = KernelPlan::new(block, layout)?;
    let registers = max_registers(&plan.programs)?;
    let imports = collect_imports(&plan.programs)?;
    let mut module = Module::new();
    let types = add_type_section(&mut module);
    let catalog = add_import_section(&mut module, &imports, &types);
    add_function_section(&mut module, types.eval_type);
    add_export_section(&mut module, catalog.eval_function_index);
    let accumulator = LOCAL_BASE
        .checked_add(u32::try_from(registers).map_err(|_| "WASM registers overflow")?)
        .ok_or("WASM accumulator overflow")?;
    let mut locals =
        locals_for_register_count(registers.checked_add(1).ok_or("WASM registers overflow")?)?;
    let integer_count =
        u32::try_from((plan.rank + 1).max(2)).map_err(|_| "WASM loop rank overflow")?;
    locals.push((integer_count, ValType::I32));
    let counter = u32::try_from(registers)
        .ok()
        .and_then(|r| r.checked_add(1))
        .and_then(|r| LOCAL_BASE.checked_add(r))
        .ok_or("WASM loop local overflow")?;
    counter
        .checked_add(integer_count)
        .ok_or("WASM loop local overflow")?;
    let mut function = Function::new(locals);
    let mut emitter = BodyEmitter::new(&catalog, &mut function);
    if plan
        .nodes
        .iter()
        .any(|node| matches!(node, NodePlan::Matrix(_)))
    {
        emitter.guard_matrix_buffers(layout, plan.outputs, counter)?;
    }
    emitter.emit_zero_output(plan.outputs, counter)?;
    for node in &plan.nodes {
        match node {
            NodePlan::Scalar { block, outputs } => emitter.emit_mapped_programs(block, outputs)?,
            NodePlan::Affine(affine) => emitter.emit_affine(affine, counter)?,
            NodePlan::Matrix(matrix) => emitter.emit_matrix(matrix, counter, accumulator)?,
        }
    }
    function.instruction(&Instruction::End);
    let mut code = CodeSection::new();
    code.function(&function);
    module.section(&code);
    Ok(module.finish())
}

impl<'a> KernelPlan<'a> {
    fn new(block: &'a ComputeBlock, layout: &VarLayout) -> Result<Self, String> {
        let outputs = block.len().map_err(|e| e.to_string())?;
        checked_slot_count(outputs)?;
        checked_slot_count(layout.y_scalars())?;
        checked_slot_count(layout.p_scalars())?;
        let mut plan = Self {
            nodes: Vec::new(),
            programs: Vec::new(),
            outputs,
            rank: 0,
        };
        let mut cursor = 0;
        for node in &block.nodes {
            match node {
                ComputeNode::ScalarPrograms(source) => {
                    let targets = scalar_outputs(source, cursor)?;
                    cursor = targets.iter().max().map_or(cursor, |i| cursor.max(i + 1));
                    source
                        .programs()
                        .iter()
                        .try_for_each(|program| affine::validate_loads(program, &[], layout))?;
                    plan.programs.extend(source.programs().iter().cloned());
                    plan.nodes.push(NodePlan::Scalar {
                        block: source,
                        outputs: targets,
                    });
                }
                ComputeNode::Map { .. } | ComputeNode::AffineStencil { .. } => {
                    let affine = AffinePlan::new(node, layout)?;
                    plan.rank = plan.rank.max(affine.extents.len());
                    cursor = cursor.max(affine.output_count);
                    plan.programs.push(affine.ops.to_vec());
                    plan.nodes.push(NodePlan::Affine(affine));
                }
                ComputeNode::MatMul { .. } => {
                    let matrix = MatrixPlan::new(node, layout, cursor)?;
                    cursor = matrix.output_end()?;
                    plan.nodes.push(NodePlan::Matrix(matrix));
                }
                ComputeNode::LinSolve { .. } => {
                    return Err("WASM compact LinSolve kernels are not supported".into());
                }
            }
        }
        Ok(plan)
    }
}

fn scalar_outputs(block: &ScalarProgramBlock, cursor: usize) -> Result<Vec<usize>, String> {
    if !block.uses_local_contiguous_output_indices() {
        return Ok(block.output_indices().to_vec());
    }
    let end = cursor
        .checked_add(block.stored_output_count())
        .ok_or("WASM output cursor overflow")?;
    checked_slot_count(end)?;
    Ok((cursor..end).collect())
}

fn checked_slot_count(count: usize) -> Result<(), String> {
    if count > (u32::MAX as usize) / 8 {
        return Err("WASM f64 storage exceeds the wasm32 address space".into());
    }
    Ok(())
}

impl BodyEmitter<'_> {
    pub(in crate::emit) fn cells(
        &mut self,
        counter: u32,
        count: usize,
        body: impl FnOnce(&mut Self) -> Result<(), String>,
    ) -> Result<(), String> {
        // Keep the general loop's checked extent, entry index and increment.
        i32::try_from(count).map_err(|_| "WASM loop extent exceeds i32")?;
        if count == 1 {
            self.push(Instruction::I32Const(0));
            self.push(Instruction::LocalSet(counter));
            body(self)?;
            self.increment_loop_counter(counter);
        } else {
            self.loop_start(counter, count)?;
            body(self)?;
            self.loop_end(counter);
        }
        Ok(())
    }

    pub(in crate::emit) fn loop_start(&mut self, counter: u32, count: usize) -> Result<(), String> {
        self.loop_start_from(counter, 0, count)
    }

    fn loop_start_from(&mut self, counter: u32, start: i32, count: usize) -> Result<(), String> {
        let count = i32::try_from(count).map_err(|_| "WASM loop extent exceeds i32")?;
        self.push(Instruction::I32Const(start));
        self.push(Instruction::LocalSet(counter));
        self.push(Instruction::Block(BlockType::Empty));
        self.push(Instruction::Loop(BlockType::Empty));
        self.push(Instruction::LocalGet(counter));
        self.push(Instruction::I32Const(count));
        self.push(Instruction::I32GeU);
        self.push(Instruction::BrIf(1));
        Ok(())
    }

    pub(in crate::emit) fn loop_end(&mut self, counter: u32) {
        self.increment_loop_counter(counter);
        self.push(Instruction::Br(0));
        self.push(Instruction::End);
        self.push(Instruction::End);
    }

    fn increment_loop_counter(&mut self, counter: u32) {
        self.push(Instruction::LocalGet(counter));
        self.push(Instruction::I32Const(1));
        self.push(Instruction::I32Add);
        self.push(Instruction::LocalSet(counter));
    }

    fn emit_zero_output(&mut self, count: usize, counter: u32) -> Result<(), String> {
        self.loop_start(counter, count)?;
        self.push(Instruction::LocalGet(OUT_PTR_PARAM));
        self.push(Instruction::LocalGet(counter));
        self.push(Instruction::I32Const(8));
        self.push(Instruction::I32Mul);
        self.push(Instruction::I32Add);
        self.push(Instruction::F64Const(0.0.into()));
        self.push(Instruction::F64Store(MemArg {
            offset: 0,
            align: 3,
            memory_index: 0,
        }));
        self.loop_end(counter);
        Ok(())
    }

    fn emit_mapped_programs(
        &mut self,
        block: &ScalarProgramBlock,
        targets: &[usize],
    ) -> Result<(), String> {
        let mut targets = targets.iter();
        for (index, program) in block.programs().iter().enumerate() {
            self.program_ordinal = index;
            self.program_span = block.program_spans().get(index).copied();
            self.emit_mapped_program(program, &mut targets)?;
        }
        if targets.next().is_some() {
            return Err("WASM scalar output pairing mismatch".into());
        }
        Ok(())
    }

    fn emit_mapped_program(
        &mut self,
        program: &[LinearOp],
        targets: &mut std::slice::Iter<'_, usize>,
    ) -> Result<(), String> {
        for (index, op) in program.iter().enumerate() {
            self.operation_ordinal = index;
            match *op {
                LinearOp::StoreOutput { src } => self.emit_mapped_store(src, targets)?,
                LinearOp::StoreOutputRange {
                    start,
                    count,
                    stride,
                } => self.emit_mapped_range(start, count, stride, targets)?,
                LinearOp::PureCall { .. } if self.calls.is_some() => {
                    self.emit_native_pure_call(op.clone(), Some(&program[..index]))?;
                }
                _ => self.emit_op(op.clone())?,
            }
        }
        Ok(())
    }

    fn emit_mapped_range(
        &mut self,
        start: Reg,
        count: usize,
        stride: usize,
        targets: &mut std::slice::Iter<'_, usize>,
    ) -> Result<(), String> {
        if self.arena.is_some() {
            let output = take_contiguous_range(targets, count)?;
            return self.emit_arena_range(start, count, stride, output);
        }
        for index in 0..count {
            let source = checked_range_register(start, stride, index)?;
            self.emit_mapped_store(source, targets)?;
        }
        Ok(())
    }

    fn emit_mapped_store(
        &mut self,
        source: Reg,
        targets: &mut std::slice::Iter<'_, usize>,
    ) -> Result<(), String> {
        self.next_output_slot = *targets.next().ok_or("WASM missing scalar output target")? as u64;
        self.emit_store_output(source)
    }
}

fn take_contiguous_range(
    targets: &mut std::slice::Iter<'_, usize>,
    count: usize,
) -> Result<usize, String> {
    let output = *targets
        .as_slice()
        .first()
        .ok_or("WASM missing range output")?;
    for ordinal in 0..count {
        if targets.next().copied() != output.checked_add(ordinal) {
            return Err("native arena range requires contiguous output targets".into());
        }
    }
    Ok(output)
}

fn checked_range_register(start: Reg, stride: usize, index: usize) -> Result<Reg, String> {
    let offset = index
        .checked_mul(stride)
        .and_then(|n| Reg::try_from(n).ok())
        .ok_or("WASM output register range overflow")?;
    start
        .checked_add(offset)
        .ok_or_else(|| "WASM output register range overflow".into())
}
