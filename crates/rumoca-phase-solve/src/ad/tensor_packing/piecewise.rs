//! Pack fragmented AD register views without expanding long regular runs.

#[cfg(test)]
mod tests;

use super::*;

fn runs(registers: &[Reg]) -> Vec<(RegisterRun, usize)> {
    let mut result = Vec::new();
    let mut cursor = 0;
    while let Some(&first) = registers.get(cursor) {
        let repeated = registers.get(cursor + 1) == Some(&first);
        let mut count = 1;
        while let Some(&register) = registers.get(cursor + count) {
            let expected = if repeated {
                Some(first)
            } else {
                Reg::try_from(count)
                    .ok()
                    .and_then(|offset| first.checked_add(offset))
            };
            if expected != Some(register) {
                break;
            }
            count += 1;
        }
        let run = if repeated {
            RegisterRun::Repeated(first)
        } else {
            RegisterRun::Contiguous(first)
        };
        result.push((run, count));
        cursor += count;
    }
    result
}

impl AdBuilder<'_> {
    pub(super) fn pack_piecewise_registers(
        &mut self,
        registers: &[Reg],
    ) -> Result<Option<Reg>, LowerError> {
        let flat = runs(registers);
        if !registers.len().is_multiple_of(2) {
            return self.pack_compressed_runs(&flat, registers.len());
        }
        let left = registers.iter().copied().step_by(2).collect::<Vec<_>>();
        let right = registers
            .iter()
            .copied()
            .skip(1)
            .step_by(2)
            .collect::<Vec<_>>();
        let left = runs(&left);
        let right = runs(&right);
        if left.len() + right.len() >= flat.len() {
            return self.pack_compressed_runs(&flat, registers.len());
        }
        let count = registers.len() / 2;
        let left = self.emit_piecewise_runs(&left, count)?;
        let right = self.emit_piecewise_runs(&right, count)?;
        self.emit_interleaved_planes(left, right, count).map(Some)
    }

    fn pack_compressed_runs(
        &mut self,
        runs: &[(RegisterRun, usize)],
        count: usize,
    ) -> Result<Option<Reg>, LowerError> {
        if runs.len() >= count {
            return Ok(None);
        }
        self.emit_piecewise_runs(runs, count).map(Some)
    }

    fn emit_piecewise_runs(
        &mut self,
        runs: &[(RegisterRun, usize)],
        count: usize,
    ) -> Result<Reg, LowerError> {
        if let [(run, size)] = runs {
            return self.emit_register_run(*run, *size);
        }
        let mut sources = Vec::with_capacity(runs.len());
        for &(run, size) in runs {
            let dimension = u32::try_from(size)
                .map_err(|_| unsupported("AD register run extent exceeds u32"))?;
            sources.push(rumoca_ir_solve::TensorConcatenateSource {
                start: self.emit_register_run(run, size)?,
                dimensions: Box::new([dimension]),
            });
        }
        let dimension =
            u32::try_from(count).map_err(|_| unsupported("AD packed extent exceeds u32"))?;
        self.emit_result_operation(count, |dst_start| LinearOp::TensorConcatenate {
            dst_start,
            sources: sources.into_boxed_slice(),
            dimensions: Box::new([dimension]),
            axis: 0,
            lanes: 1,
        })
    }
}
