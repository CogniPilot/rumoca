//! Compact copies between constant, planar, and interleaved AD register runs.

mod piecewise;

use super::*;

#[derive(Clone, Copy)]
enum RegisterRun {
    Contiguous(Reg),
    Repeated(Reg),
}

fn register_run(registers: &[Reg], offset: usize, stride: usize) -> Option<RegisterRun> {
    let &first = registers.get(offset)?;
    let values = registers[offset..].iter().copied().step_by(stride);
    if values.clone().all(|register| register == first) {
        return Some(RegisterRun::Repeated(first));
    }
    values
        .enumerate()
        .all(|(offset, register)| {
            Reg::try_from(offset)
                .ok()
                .and_then(|offset| first.checked_add(offset))
                == Some(register)
        })
        .then_some(RegisterRun::Contiguous(first))
}

impl AdBuilder<'_> {
    fn emit_register_run(&mut self, run: RegisterRun, count: usize) -> Result<Reg, LowerError> {
        match run {
            RegisterRun::Contiguous(start) => Ok(start),
            RegisterRun::Repeated(value_start) => {
                self.emit_result_operation(count, |dst_start| LinearOp::TensorFill {
                    dst_start,
                    value_start,
                    count,
                    lanes: 1,
                })
            }
        }
    }

    pub(super) fn pack_compact_registers(
        &mut self,
        registers: &[Reg],
    ) -> Result<Option<Reg>, LowerError> {
        if let Some(run) = register_run(registers, 0, 1) {
            return self.emit_register_run(run, registers.len()).map(Some);
        }
        if registers.len() < 4 || !registers.len().is_multiple_of(2) {
            return self.pack_piecewise_registers(registers);
        }
        let (Some(left), Some(right)) =
            (register_run(registers, 0, 2), register_run(registers, 1, 2))
        else {
            return self.pack_piecewise_registers(registers);
        };
        let count = registers.len() / 2;
        let left = self.emit_register_run(left, count)?;
        let right = self.emit_register_run(right, count)?;
        self.emit_interleaved_planes(left, right, count).map(Some)
    }

    fn emit_interleaved_planes(
        &mut self,
        left: Reg,
        right: Reg,
        count: usize,
    ) -> Result<Reg, LowerError> {
        let dimension =
            u32::try_from(count).map_err(|_| unsupported("interleaved AD extent exceeds u32"))?;
        let sources = [left, right].map(|start| rumoca_ir_solve::TensorConcatenateSource {
            start,
            dimensions: Box::new([dimension, 1]),
        });
        let size = checked_ad_product(count, 2, self.span, "interleaved AD output")?;
        self.emit_result_operation(size, |dst_start| LinearOp::TensorConcatenate {
            dst_start,
            sources: Box::new(sources),
            dimensions: Box::new([dimension, 2]),
            axis: 1,
            lanes: 1,
        })
    }

    pub(super) fn interleaved_dual_range(
        &self,
        start: Reg,
        count: usize,
    ) -> Result<Option<Reg>, LowerError> {
        if count == 0 {
            return Ok(None);
        }
        let first = self.lookup(start)?;
        for offset in 0..count {
            let source = checked_ad_reg_offset(start, offset, self.span, "interleaved AD source")?;
            let expected = checked_ad_reg_offset(
                first.re,
                checked_ad_product(offset, 2, self.span, "interleaved AD offset")?,
                self.span,
                "interleaved AD primal",
            )?;
            let actual = self.lookup(source)?;
            if actual.re != expected || Some(actual.du) != expected.checked_add(1) {
                return Ok(None);
            }
        }
        Ok(Some(first.re))
    }

    pub(super) fn planar_dual_range(
        &mut self,
        source: Reg,
        count: usize,
    ) -> Result<Option<(Reg, Reg)>, LowerError> {
        if count == 1 {
            let value = self.lookup(source)?;
            return Ok(Some((value.re, value.du)));
        }
        let Some(input) = self.interleaved_dual_range(source, count)? else {
            return Ok(None);
        };
        let start = if let Some(&start) = self.tangent_planes.get(&(input, count)) {
            start
        } else {
            let size = checked_ad_product(count, 2, self.span, "planar AD result")?;
            let start =
                self.emit_result_operation(size, |dst_start| LinearOp::TensorTranspose {
                    dst_start,
                    src_start: input,
                    rows: 2,
                    columns: count,
                    element_width: 1,
                    lanes: 1,
                })?;
            self.tangent_planes.insert((input, count), start);
            start
        };
        let tangent = checked_ad_reg_offset(start, count, self.span, "planar AD tangent")?;
        Ok(Some((start, tangent)))
    }
}
