//! Numerical AD in an explicitly issued projection seed space.

use super::*;

impl SeedMode<'_> {
    pub(super) fn y_is_active(self, index: usize) -> bool {
        match self {
            Self::SolverYSubset { active } => active.binary_search(&index).is_ok(),
            Self::SolverYOnly | Self::SolverYAndP { .. } => true,
        }
    }
}

fn seed_run_boundary(
    active: &[usize],
    active_cursor: &mut usize,
    start: usize,
    end: usize,
) -> (bool, usize) {
    if active.get(*active_cursor) != Some(&start) {
        return (
            false,
            active.get(*active_cursor).copied().unwrap_or(end).min(end),
        );
    }
    let mut boundary = start;
    while boundary < end && active.get(*active_cursor) == Some(&boundary) {
        boundary += 1;
        *active_cursor += 1;
    }
    (true, boundary)
}

impl AdBuilder<'_> {
    pub(super) fn lower_domain_tensor_load(
        &mut self,
        dst_start: Reg,
        input_start: usize,
        count: usize,
        active: &[usize],
    ) -> Result<(), LowerError> {
        let end = input_start
            .checked_add(count)
            .ok_or_else(|| unsupported("projection tensor seed extent overflows"))?;
        let mut cursor = input_start;
        let mut active_cursor = active.partition_point(|&index| index < input_start);
        while cursor < end {
            let (seeded, boundary) = seed_run_boundary(active, &mut active_cursor, cursor, end);
            let destination = checked_ad_reg_offset(
                dst_start,
                cursor - input_start,
                self.span,
                "projection tensor seed run",
            )?;
            self.lower_seed_run(destination, cursor, boundary - cursor, seeded)?;
            cursor = boundary;
        }
        Ok(())
    }

    fn lower_seed_run(
        &mut self,
        dst_start: Reg,
        input_start: usize,
        count: usize,
        active: bool,
    ) -> Result<(), LowerError> {
        let input = rumoca_ir_solve::TensorInputKind::Y;
        if active {
            self.lower_planar_domain_load(dst_start, input_start, count)
        } else {
            self.emit_zero_tangent_result(dst_start, count, |dst_start| LinearOp::TensorLoad {
                dst_start,
                input,
                input_start,
                count,
                seed_start: None,
                lanes: 1,
            })
        }
    }

    fn lower_planar_domain_load(
        &mut self,
        destination: Reg,
        input_start: usize,
        count: usize,
    ) -> Result<(), LowerError> {
        let size = checked_ad_product(count, 2, self.span, "domain tensor dual size")?;
        let dual = self.emit_result_operation(size, |dst_start| LinearOp::TensorLoad {
            dst_start,
            input: rumoca_ir_solve::TensorInputKind::Y,
            input_start,
            count,
            seed_start: Some(input_start),
            lanes: 2,
        })?;
        let planar = self.emit_result_operation(size, |dst_start| LinearOp::TensorTranspose {
            dst_start,
            src_start: dual,
            rows: 2,
            columns: count,
            element_width: 1,
            lanes: 1,
        })?;
        for offset in 0..count {
            let source =
                checked_ad_reg_offset(destination, offset, self.span, "domain tensor source")?;
            let re = checked_ad_reg_offset(planar, offset, self.span, "domain tensor primal")?;
            let du =
                checked_ad_reg_offset(planar, count + offset, self.span, "domain tensor tangent")?;
            self.bind(source, DualReg { re, du })?;
        }
        Ok(())
    }

    pub(super) fn lower_active_tensor_load(
        &mut self,
        dst_start: Reg,
        input: rumoca_ir_solve::TensorInputKind,
        input_start: usize,
        count: usize,
        seed_start: Option<usize>,
    ) -> Result<(), LowerError> {
        let dual_start = self.next_reg;
        for _ in 0..checked_ad_product(count, 2, self.span, "tensor load dual output")? {
            self.alloc_reg()?;
        }
        self.ops.push(LinearOp::TensorLoad {
            dst_start: dual_start,
            input,
            input_start,
            count,
            seed_start,
            lanes: 2,
        });
        for offset in 0..count {
            let primal = checked_ad_reg_offset(dst_start, offset, self.span, "tensor load output")?;
            let dual = checked_ad_reg_offset(
                dual_start,
                checked_ad_product(offset, 2, self.span, "tensor load dual lane")?,
                self.span,
                "tensor load dual output",
            )?;
            self.bind(
                primal,
                DualReg {
                    re: dual,
                    du: dual + 1,
                },
            )?;
        }
        Ok(())
    }
}

pub(crate) fn lower_projection_domain(
    domain: rumoca_ir_solve::ProjectionJacobianSeedDomain,
) -> Result<rumoca_ir_solve::ProjectionJacobianApplication, LowerError> {
    let source = domain.primal();
    let span = source.first_source_span();
    let programs = lower_scalar_program_rows_ad(
        source.programs(),
        source.program_spans(),
        SeedMode::SolverYSubset {
            active: domain.active_y(),
        },
        "projection-domain scalar program count",
    )?;
    let derivative = ScalarProgramBlock::with_output_indices(
        programs,
        source.program_spans().to_vec(),
        source.output_indices().to_vec(),
    )?;
    domain.with_lowered_derivative(derivative).ok_or_else(|| {
        ad_optional_contract_violation(
            "projection-domain AD did not retain its issued seed and output identities".into(),
            span,
        )
    })
}
