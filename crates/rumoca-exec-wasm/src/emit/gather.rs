//! Checked compact model-scope register gathers at the final WASM boundary.
use super::*;
use rumoca_ir_solve::TensorIndex;

impl BodyEmitter<'_> {
    fn gather_fault(&mut self, kind: crate::TypedCallFaultKind) -> Result<u32, String> {
        let base = self
            .gather_status_base
            .ok_or("model gather requires checked status ABI")?;
        let status = u32::try_from(self.gather_faults.len())
            .ok()
            .and_then(|n| base.checked_add(n))
            .filter(|&n| n <= i32::MAX as u32)
            .ok_or("model gather fault status overflow")?;
        self.gather_faults.push(crate::NativeGatherFault {
            status,
            kind,
            kernel: self.kernel_ordinal,
            program: self.program_ordinal,
            operation: self.operation_ordinal,
            region_path: self.region_path.clone(),
            provenance: self
                .program_span
                .ok_or("model gather has no source program span")?,
        });
        Ok(status)
    }

    pub(super) fn emit_checked_gather(
        &mut self,
        dst: Reg,
        base: Reg,
        stride: usize,
        dimensions: &[u32],
        indices: &[TensorIndex],
    ) -> Result<(), String> {
        let arena = self.arena_plan()?;
        self.calls
            .ok_or("model gather requires checked status ABI")?;
        self.check_gather_frame(base, stride, dimensions, indices)?;
        let conversion = self.gather_fault(crate::TypedCallFaultKind::IntegerConversion)?;
        let bounds = self.gather_fault(crate::TypedCallFaultKind::IndexBounds)?;
        self.check_gather_axes(dimensions, indices, conversion, bounds)?;
        // Checked extents and arena size bound this row-major address in i32.
        self.push(Instruction::I32Const(0));
        self.push(Instruction::LocalSet(arena.inner_counter));
        for (&extent, index) in dimensions.iter().zip(indices) {
            self.push(Instruction::LocalGet(arena.inner_counter));
            self.push(Instruction::I32Const(extent as i32));
            self.push(Instruction::I32Mul);
            match *index {
                TensorIndex::Constant(value) => self.push(Instruction::I32Const(value as i32)),
                TensorIndex::Runtime(register) => {
                    self.push_reg(register)?;
                    self.push(Instruction::I32TruncF64U);
                    self.push(Instruction::I32Const(1));
                    self.push(Instruction::I32Sub);
                }
            }
            self.push(Instruction::I32Add);
            self.push(Instruction::LocalSet(arena.inner_counter));
        }
        self.push_arena_address(base, Some(arena.inner_counter), stride)?;
        self.push(Instruction::F64Load(arena::memarg()));
        // The gather never writes input memory or changes Y before whole-program success.
        self.set_reg(dst)
    }

    fn check_gather_frame(
        &self,
        base: Reg,
        stride: usize,
        dimensions: &[u32],
        indices: &[TensorIndex],
    ) -> Result<(), String> {
        let arena = self.arena_plan()?;
        if dimensions.is_empty() || dimensions.len() != indices.len() || stride == 0 {
            return Err("model gather has invalid checked shape".into());
        }
        let count = dimensions
            .iter()
            .try_fold(1usize, |n, &extent| {
                (extent != 0)
                    .then(|| n.checked_mul(extent as usize))
                    .flatten()
            })
            .ok_or("model gather extent overflow")?;
        // Canonical register flow proves every source cell initialized. Bound the
        // final affine address in the current checked arena frame as well.
        let last = count
            .checked_sub(1)
            .and_then(|n| n.checked_mul(stride))
            .and_then(|n| u32::try_from(n).ok())
            .and_then(|n| base.checked_add(n))
            .and_then(|n| n.checked_add(self.register_base))
            .ok_or("model gather range overflow")?;
        let end = self.region_frame_end.unwrap_or(
            u32::try_from(arena.root_registers).map_err(|_| "model gather root frame overflow")?,
        );
        if last >= end {
            return Err("model gather exceeds its checked frame".into());
        }
        Ok(())
    }

    fn check_gather_axes(
        &mut self,
        dimensions: &[u32],
        indices: &[TensorIndex],
        conversion: u32,
        bounds: u32,
    ) -> Result<(), String> {
        // Resolve every axis before any candidate source cell is accessed.
        for (&extent, index) in dimensions.iter().zip(indices) {
            match *index {
                TensorIndex::Constant(value) if value >= extent => {
                    return Err("model gather constant coordinate exceeds its shape".into());
                }
                TensorIndex::Constant(_) => {}
                TensorIndex::Runtime(register) => {
                    self.check_gather_runtime_axis(register, extent, conversion, bounds)?;
                }
            }
        }
        Ok(())
    }

    fn check_gather_runtime_axis(
        &mut self,
        register: Reg,
        extent: u32,
        conversion: u32,
        bounds: u32,
    ) -> Result<(), String> {
        self.push_reg(register)?;
        self.push(Instruction::LocalTee(LOCAL_BASE));
        self.push(Instruction::F64Trunc);
        self.push(Instruction::LocalGet(LOCAL_BASE));
        self.push(Instruction::F64Ne);
        self.return_status_if(conversion as i32);
        for (limit, compare) in [
            (-9223372036854775808.0, Instruction::F64Lt),
            (9223372036854775808.0, Instruction::F64Ge),
        ] {
            self.push(Instruction::LocalGet(LOCAL_BASE));
            self.push(Instruction::F64Const(limit.into()));
            self.push(compare);
            self.return_status_if(conversion as i32);
        }
        for (limit, compare) in [
            (1.0, Instruction::F64Lt),
            (f64::from(extent), Instruction::F64Gt),
        ] {
            self.push(Instruction::LocalGet(LOCAL_BASE));
            self.push(Instruction::F64Const(limit.into()));
            self.push(compare);
            self.return_status_if(bounds as i32);
        }
        Ok(())
    }
}
