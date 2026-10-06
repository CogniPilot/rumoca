use super::*;
use rumoca_ir_solve::{AffineStencilConstStride, AffineStencilLoadStride};

pub(super) struct AffinePlan<'a> {
    pub(super) ops: &'a [LinearOp],
    pub(super) extents: Vec<usize>,
    pub(super) output_count: usize,
    count: usize,
    loads: Vec<Option<Vec<i32>>>,
    constants: Vec<Option<Vec<f64>>>,
    output_start: usize,
    output_strides: Vec<i32>,
}

impl<'a> AffinePlan<'a> {
    pub(super) fn new(node: &'a ComputeNode, layout: &VarLayout) -> Result<Self, String> {
        let (domain, output_map, ops, loads, constants) = match node {
            ComputeNode::Map {
                domain,
                output_map,
                base_ops,
                load_strides,
                const_strides,
                ..
            }
            | ComputeNode::AffineStencil {
                domain,
                output_map,
                base_ops,
                load_strides,
                const_strides,
                ..
            } => (
                domain,
                output_map,
                base_ops.as_slice(),
                load_strides.as_slice(),
                const_strides.as_slice(),
            ),
            _ => return Err("WASM affine plan requires an affine tensor node".into()),
        };
        let extents = domain.extents().map_err(|e| e.to_string())?;
        let count = domain.scalar_count().map_err(|e| e.to_string())?;
        checked_slot_count(count)?;
        let output_count = output_map
            .output_count(domain)
            .map_err(|e| format!("invalid WASM output map: {e:?}"))?;
        let loads = load_strides(ops.len(), extents.len(), loads)?;
        let constants = constant_strides(ops.len(), extents.len(), constants)?;
        let mut output_strides = vec![0i32; extents.len()];
        for term in &output_map.strides {
            let value = i32::try_from(term.stride).map_err(|_| "WASM output stride exceeds i32")?;
            output_strides[term.dimension] = output_strides[term.dimension]
                .checked_add(value)
                .ok_or("WASM output stride sum exceeds i32")?;
        }
        if ScalarProgramBlock::program_output_count(ops) != 1 {
            return Err("WASM affine body must produce exactly one scalar per domain point".into());
        }
        if count != 0 {
            validate_loads(ops, &loads, layout_with_extents(layout, &extents))?;
        }
        Ok(Self {
            ops,
            extents,
            output_count,
            count,
            loads,
            constants,
            output_start: output_map.start,
            output_strides,
        })
    }
}

// A tiny borrowed validation context keeps array limits and ordinal extents
// together; no domain coordinates are enumerated to check an address range.
pub(super) struct LoadContext<'a> {
    layout: &'a VarLayout,
    extents: &'a [usize],
}
fn layout_with_extents<'a>(layout: &'a VarLayout, extents: &'a [usize]) -> LoadContext<'a> {
    LoadContext { layout, extents }
}
impl<'a> From<&'a VarLayout> for LoadContext<'a> {
    fn from(layout: &'a VarLayout) -> Self {
        Self {
            layout,
            extents: &[],
        }
    }
}

pub(super) fn validate_loads<'a>(
    ops: &[LinearOp],
    strides: &[Option<Vec<i32>>],
    context: impl Into<LoadContext<'a>>,
) -> Result<(), String> {
    let LoadContext { layout, extents } = context.into();
    for (position, op) in ops.iter().enumerate() {
        if let LinearOp::FunctionConditional { program, .. } = op {
            for arm in &program.arms {
                validate_loads(&arm.condition, &[], layout)?;
                validate_loads(&arm.result, &[], layout)?;
            }
            validate_loads(&program.fallback, &[], layout)?;
        }
        let (base, limit, range) = match *op {
            LinearOp::LoadY { index, .. } | LinearOp::LoadSeed { index, .. } => {
                (index, layout.y_scalars(), 1)
            }
            LinearOp::LoadP { index, .. } => (index, layout.p_scalars(), 1),
            LinearOp::LoadIndexedP { base, count, .. } => (base, layout.p_scalars(), count.max(1)),
            LinearOp::LoadIndexedSeed { base, count, .. } => {
                (base, layout.y_scalars(), count.max(1))
            }
            LinearOp::TensorLoad {
                input,
                input_start,
                count,
                seed_start,
                ..
            } => {
                if seed_start.is_some_and(|start| {
                    start
                        .checked_add(count)
                        .is_none_or(|end| end > layout.y_scalars())
                }) {
                    return Err("WASM tensor seed range exceeds layout".into());
                }
                let limit = match input {
                    rumoca_ir_solve::TensorInputKind::Y => layout.y_scalars(),
                    rumoca_ir_solve::TensorInputKind::P => layout.p_scalars(),
                };
                if count == 0 && input_start > limit {
                    return Err("WASM empty tensor start exceeds layout".into());
                }
                if count == 0 {
                    continue;
                }
                (input_start, limit, count)
            }
            _ => continue,
        };
        let (mut minimum, mut maximum) = (base as i128, base as i128 + range as i128 - 1);
        if let Some(Some(terms)) = strides.get(position) {
            for (&stride, &extent) in terms.iter().zip(extents) {
                let offset = i128::from(stride) * (extent.saturating_sub(1) as i128);
                minimum += offset.min(0);
                maximum += offset.max(0);
            }
        }
        if minimum < 0 || maximum >= limit as i128 {
            return Err(format!(
                "WASM load operation {position} accesses {minimum}..={maximum} outside {limit} slots"
            ));
        }
    }
    Ok(())
}

fn load_strides(
    count: usize,
    rank: usize,
    strides: &[AffineStencilLoadStride],
) -> Result<Vec<Option<Vec<i32>>>, String> {
    let mut result = vec![None; count];
    for stride in strides {
        let values = result[stride.op_position].get_or_insert_with(|| vec![0i32; rank]);
        for term in &stride.terms {
            let value = i32::try_from(term.stride).map_err(|_| "WASM load stride exceeds i32")?;
            values[term.dimension] = values[term.dimension]
                .checked_add(value)
                .ok_or("WASM load stride sum exceeds i32")?;
        }
    }
    Ok(result)
}

fn constant_strides(
    count: usize,
    rank: usize,
    strides: &[AffineStencilConstStride],
) -> Result<Vec<Option<Vec<f64>>>, String> {
    let mut result = vec![None; count];
    for stride in strides {
        let values = result[stride.op_position].get_or_insert_with(|| vec![0f64; rank]);
        for term in &stride.terms {
            values[term.dimension] += term.stride;
        }
    }
    Ok(result)
}

impl BodyEmitter<'_> {
    pub(super) fn emit_affine(
        &mut self,
        plan: &AffinePlan<'_>,
        counter: u32,
    ) -> Result<(), String> {
        if plan.count == 0 {
            return Ok(());
        }
        self.loop_start(counter, plan.count)?;
        self.emit_coordinates(&plan.extents, counter)?;
        for (position, op) in plan.ops.iter().enumerate() {
            match *op {
                LinearOp::LoadY { dst, index }
                | LinearOp::LoadP { dst, index }
                | LinearOp::LoadSeed { dst, index }
                    if plan.loads[position].is_some() =>
                {
                    let ptr = match op {
                        LinearOp::LoadP { .. } => P_PTR_PARAM,
                        LinearOp::LoadSeed { .. } => SEED_PTR_PARAM,
                        _ => Y_PTR_PARAM,
                    };
                    self.emit_affine_address(
                        ptr,
                        index,
                        plan.loads[position].as_ref().unwrap(),
                        counter,
                    )?;
                    self.push(Instruction::F64Load(MemArg {
                        offset: 0,
                        align: 3,
                        memory_index: 0,
                    }));
                    self.set_reg(dst)?;
                }
                LinearOp::Const { dst, value } if plan.constants[position].is_some() => {
                    self.emit_affine_constant(
                        dst,
                        value,
                        plan.constants[position].as_ref().unwrap(),
                        counter,
                    )?;
                }
                LinearOp::StoreOutput { src }
                | LinearOp::StoreOutputRange {
                    start: src,
                    count: 1,
                    ..
                } => {
                    self.emit_affine_address(
                        OUT_PTR_PARAM,
                        plan.output_start,
                        &plan.output_strides,
                        counter,
                    )?;
                    self.push_reg(src)?;
                    self.push(Instruction::F64Store(MemArg {
                        offset: 0,
                        align: 3,
                        memory_index: 0,
                    }));
                }
                LinearOp::StoreOutputRange { count: 0, .. } => {}
                LinearOp::StoreOutputRange { .. } => {
                    return Err("WASM affine StoreOutputRange is not supported".into());
                }
                _ => self.emit_op(op.clone())?,
            }
        }
        self.loop_end(counter);
        Ok(())
    }

    fn emit_coordinates(&mut self, extents: &[usize], counter: u32) -> Result<(), String> {
        let mut trailing = 1usize;
        for (axis, &extent) in extents.iter().enumerate().rev() {
            self.push(Instruction::LocalGet(counter));
            self.push(Instruction::I32Const(
                i32::try_from(trailing).map_err(|_| "WASM ordinal stride overflow")?,
            ));
            self.push(Instruction::I32DivU);
            self.push(Instruction::I32Const(
                i32::try_from(extent).map_err(|_| "WASM domain extent overflow")?,
            ));
            self.push(Instruction::I32RemU);
            self.push(Instruction::LocalSet(counter + 1 + axis as u32));
            trailing = trailing
                .checked_mul(extent)
                .ok_or("WASM domain count overflow")?;
        }
        Ok(())
    }

    fn emit_affine_address(
        &mut self,
        ptr: u32,
        base: usize,
        strides: &[i32],
        counter: u32,
    ) -> Result<(), String> {
        self.push(Instruction::LocalGet(ptr));
        self.push(Instruction::I32Const(
            i32::try_from(base).map_err(|_| "WASM slot base overflow")?,
        ));
        for (axis, &stride) in strides
            .iter()
            .enumerate()
            .filter(|(_, stride)| **stride != 0)
        {
            self.push(Instruction::LocalGet(counter + 1 + axis as u32));
            self.push(Instruction::I32Const(stride));
            self.push(Instruction::I32Mul);
            self.push(Instruction::I32Add);
        }
        self.push(Instruction::I32Const(8));
        self.push(Instruction::I32Mul);
        self.push(Instruction::I32Add);
        Ok(())
    }

    fn emit_affine_constant(
        &mut self,
        dst: Reg,
        value: f64,
        strides: &[f64],
        counter: u32,
    ) -> Result<(), String> {
        self.push(Instruction::F64Const(value.into()));
        // Match shared scalarization's dimension-order multiply then add,
        // including zero strides; no reassociation or fused multiply-add.
        for (axis, &stride) in strides.iter().enumerate() {
            self.push(Instruction::LocalGet(counter + 1 + axis as u32));
            self.push(Instruction::F64ConvertI32U);
            self.push(Instruction::F64Const(stride.into()));
            self.push(Instruction::F64Mul);
            self.push(Instruction::F64Add);
        }
        self.set_reg(dst)
    }
}
