use super::*;

impl<'a> AdBuilder<'a> {
    pub(super) fn lower_table_bounds(
        &mut self,
        dst: Reg,
        table_id: Reg,
        max: bool,
    ) -> Result<(), SolveDerivationError> {
        let table = self.lookup(table_id)?;
        let re = self.emit_table_bounds(table.re, max)?;
        let du = self.zero_reg()?;
        self.bind(dst, DualReg { re, du })
    }

    pub(super) fn lower_table_lookup(
        &mut self,
        dst: Reg,
        table_id: Reg,
        column: Reg,
        input: Reg,
    ) -> Result<(), SolveDerivationError> {
        let table = self.lookup(table_id)?;
        let column = self.lookup(column)?;
        let input = self.lookup(input)?;
        let re = self.emit_table_lookup(table.re, column.re, input.re)?;
        let slope = self.emit_table_lookup_slope(table.re, column.re, input.re)?;
        let du = self.emit_binary(BinaryOp::Mul, slope, input.du)?;
        self.bind(dst, DualReg { re, du })
    }

    pub(super) fn lower_table_next_event(
        &mut self,
        dst: Reg,
        table_id: Reg,
        time: Reg,
    ) -> Result<(), SolveDerivationError> {
        let table = self.lookup(table_id)?;
        let time = self.lookup(time)?;
        let re = self.emit_table_next_event(table.re, time.re)?;
        let du = self.zero_reg()?;
        self.bind(dst, DualReg { re, du })
    }

    pub(super) fn lower_unary(
        &mut self,
        dst: Reg,
        op: UnaryOp,
        arg: Reg,
    ) -> Result<(), SolveDerivationError> {
        let x = self.lookup(arg)?;
        if self.tangent_is_zero(x) {
            let re = self.emit_unary(op, x.re)?;
            return self.bind(dst, DualReg { re, du: x.du });
        }
        let out = self.unary_dual(op, x)?;
        self.bind(dst, out)
    }

    pub(super) fn lower_binary(
        &mut self,
        dst: Reg,
        op: BinaryOp,
        lhs: Reg,
        rhs: Reg,
    ) -> Result<(), SolveDerivationError> {
        let l = self.lookup(lhs)?;
        let r = self.lookup(rhs)?;
        let out = self.binary_dual(op, l, r)?;
        self.bind(dst, out)
    }

    pub(super) fn lower_dot_product(
        &mut self,
        dst: Reg,
        operands: (StridedOperand, StridedOperand),
        count: usize,
    ) -> Result<(), SolveDerivationError> {
        let (lhs_operand, rhs_operand) = operands;
        let mut sum: Option<DualReg> = None;
        for term in 0..count {
            let lhs = self.lookup(lhs_operand.start + (term * lhs_operand.stride) as Reg)?;
            let rhs = self.lookup(rhs_operand.start + (term * rhs_operand.stride) as Reg)?;
            let product = self.binary_dual(BinaryOp::Mul, lhs, rhs)?;
            sum = Some(match sum {
                Some(sum) => self.binary_dual(BinaryOp::Add, sum, product)?,
                None => product,
            });
        }
        let sum = match sum {
            Some(sum) => sum,
            None => {
                let zero = self.zero_reg()?;
                DualReg { re: zero, du: zero }
            }
        };
        self.bind(dst, sum)
    }

    pub(super) fn lower_matrix_multiply(
        &mut self,
        dst_start: Reg,
        lhs_start: Reg,
        rhs_start: Reg,
        shape: MatrixProductShape,
    ) -> Result<(), SolveDerivationError> {
        let MatrixProductShape {
            rows,
            inner,
            columns,
            lanes,
        } = shape;
        if lanes != 1 {
            return Err(unsupported(
                "forward AD expects a primal matrix multiply with one lane",
            ));
        }
        let lhs_count = checked_ad_product(rows, inner, self.span, "matrix multiply lhs")?;
        let rhs_count = checked_ad_product(inner, columns, self.span, "matrix multiply rhs")?;
        let output_count = checked_ad_product(rows, columns, self.span, "matrix multiply output")?;
        if self.lower_inactive_bilinear_factors(
            dst_start,
            [(lhs_start, lhs_count), (rhs_start, rhs_count)],
            output_count,
            |dst_start, lhs_start, rhs_start| LinearOp::MatrixMultiply {
                dst_start,
                lhs_start,
                rhs_start,
                rows,
                inner,
                columns,
                lanes: 1,
            },
        )? {
            return Ok(());
        }
        let lhs_start = self.pack_dual_register_range(lhs_start, lhs_count)?;
        let rhs_start = self.pack_dual_register_range(rhs_start, rhs_count)?;
        let dual_start = self.next_reg;
        for _ in 0..checked_ad_product(output_count, 2, self.span, "matrix multiply dual output")? {
            self.alloc_reg()?;
        }
        self.ops.push(LinearOp::MatrixMultiply {
            dst_start: dual_start,
            lhs_start,
            rhs_start,
            rows,
            inner,
            columns,
            lanes: 2,
        });
        for offset in 0..output_count {
            let primal =
                checked_ad_reg_offset(dst_start, offset, self.span, "matrix multiply output")?;
            let dual = checked_ad_reg_offset(
                dual_start,
                checked_ad_product(offset, 2, self.span, "matrix multiply dual lane")?,
                self.span,
                "matrix multiply dual output",
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

    pub(super) fn lower_tensor_binary(
        &mut self,
        dst_start: Reg,
        op: BinaryOp,
        operands: (StridedOperand, StridedOperand),
        count: usize,
        lanes: usize,
    ) -> Result<(), SolveDerivationError> {
        let (
            StridedOperand {
                start: lhs_start,
                stride: lhs_stride,
            },
            StridedOperand {
                start: rhs_start,
                stride: rhs_stride,
            },
        ) = operands;
        if lanes != 1 || lhs_stride > 1 || rhs_stride > 1 {
            return Err(unsupported(
                "forward AD expects a primal compact tensor binary with zero-or-one strides",
            ));
        }
        let lhs_count = if lhs_stride == 0 { 1 } else { count };
        let rhs_count = if rhs_stride == 0 { 1 } else { count };
        if op == BinaryOp::Mul
            && self.lower_inactive_bilinear_factors(
                dst_start,
                [(lhs_start, lhs_count), (rhs_start, rhs_count)],
                count,
                |dst_start, lhs_start, rhs_start| LinearOp::TensorBinary {
                    dst_start,
                    op,
                    lhs_start,
                    rhs_start,
                    count,
                    lhs_stride,
                    rhs_stride,
                    lanes: 1,
                },
            )?
        {
            return Ok(());
        }
        if self.range_tangent_is_zero(lhs_start, lhs_count)?
            && self.range_tangent_is_zero(rhs_start, rhs_count)?
        {
            let lhs_start = self.pack_primal_range(lhs_start, lhs_count)?;
            let rhs_start = self.pack_primal_range(rhs_start, rhs_count)?;
            return self.emit_zero_tangent_result(dst_start, count, |dst_start| {
                LinearOp::TensorBinary {
                    dst_start,
                    op,
                    lhs_start,
                    rhs_start,
                    count,
                    lhs_stride,
                    rhs_stride,
                    lanes: 1,
                }
            });
        }
        let lhs_start = self.pack_dual_register_range(lhs_start, lhs_count)?;
        let rhs_start = self.pack_dual_register_range(rhs_start, rhs_count)?;
        let dual_start = self.next_reg;
        for _ in 0..checked_ad_product(count, 2, self.span, "tensor binary dual output")? {
            self.alloc_reg()?;
        }
        self.ops.push(LinearOp::TensorBinary {
            dst_start: dual_start,
            op,
            lhs_start,
            rhs_start,
            count,
            lhs_stride,
            rhs_stride,
            lanes: 2,
        });
        for offset in 0..count {
            let primal =
                checked_ad_reg_offset(dst_start, offset, self.span, "tensor binary output")?;
            let dual = checked_ad_reg_offset(
                dual_start,
                checked_ad_product(offset, 2, self.span, "tensor binary dual lane")?,
                self.span,
                "tensor binary dual output",
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

    pub(super) fn lower_tensor_cross(
        &mut self,
        dst_start: Reg,
        lhs_start: Reg,
        rhs_start: Reg,
        lanes: usize,
    ) -> Result<(), SolveDerivationError> {
        if lanes != 1 {
            return Err(unsupported(
                "forward AD expects a primal tensor cross product with one lane",
            ));
        }
        if self.lower_inactive_bilinear_factors(
            dst_start,
            [(lhs_start, 3), (rhs_start, 3)],
            3,
            |dst_start, lhs_start, rhs_start| LinearOp::TensorCross {
                dst_start,
                lhs_start,
                rhs_start,
                lanes: 1,
            },
        )? {
            return Ok(());
        }
        let lhs_start = self.pack_dual_register_range(lhs_start, 3)?;
        let rhs_start = self.pack_dual_register_range(rhs_start, 3)?;
        let dual_start = self.next_reg;
        for _ in 0..6 {
            self.alloc_reg()?;
        }
        self.ops.push(LinearOp::TensorCross {
            dst_start: dual_start,
            lhs_start,
            rhs_start,
            lanes: 2,
        });
        for offset in 0..3 {
            let primal =
                checked_ad_reg_offset(dst_start, offset, self.span, "tensor cross product output")?;
            let dual = checked_ad_reg_offset(
                dual_start,
                checked_ad_product(offset, 2, self.span, "tensor cross product dual lane")?,
                self.span,
                "tensor cross product dual output",
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

    pub(super) fn lower_tensor_transpose(
        &mut self,
        dst_start: Reg,
        src_start: Reg,
        rows: usize,
        columns: usize,
        element_width: usize,
        lanes: usize,
    ) -> Result<(), SolveDerivationError> {
        if lanes != 1 {
            return Err(unsupported(
                "forward AD expects a primal tensor transpose with one lane",
            ));
        }
        let count = checked_ad_product(rows, columns, self.span, "tensor transpose")?;
        let count = checked_ad_product(
            count,
            element_width,
            self.span,
            "tensor transpose element width",
        )?;
        if self.range_tangent_is_zero(src_start, count)? {
            let src_start = self.pack_primal_range(src_start, count)?;
            return self.emit_zero_tangent_result(dst_start, count, |dst_start| {
                LinearOp::TensorTranspose {
                    dst_start,
                    src_start,
                    rows,
                    columns,
                    element_width,
                    lanes: 1,
                }
            });
        }
        let src_start = self.pack_dual_register_range(src_start, count)?;
        let dual_start = self.next_reg;
        for _ in 0..checked_ad_product(count, 2, self.span, "tensor transpose dual output")? {
            self.alloc_reg()?;
        }
        self.ops.push(LinearOp::TensorTranspose {
            dst_start: dual_start,
            src_start,
            rows,
            columns,
            element_width,
            lanes: 2,
        });
        for offset in 0..count {
            let primal =
                checked_ad_reg_offset(dst_start, offset, self.span, "tensor transpose output")?;
            let dual = checked_ad_reg_offset(
                dual_start,
                checked_ad_product(offset, 2, self.span, "tensor transpose dual lane")?,
                self.span,
                "tensor transpose dual output",
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

    pub(super) fn lower_tensor_concatenate(
        &mut self,
        dst_start: Reg,
        sources: &[rumoca_ir_solve::TensorConcatenateSource],
        dimensions: Box<[u32]>,
        axis: usize,
        lanes: usize,
    ) -> Result<(), SolveDerivationError> {
        if lanes != 1 {
            return Err(unsupported(
                "forward AD expects a primal tensor concatenate with one lane",
            ));
        }
        let mut dual_sources = Vec::with_capacity(sources.len());
        for source in sources.iter() {
            let count = source.dimensions.iter().try_fold(1usize, |count, extent| {
                checked_ad_product(
                    count,
                    *extent as usize,
                    self.span,
                    "tensor concatenate source",
                )
            })?;
            dual_sources.push(rumoca_ir_solve::TensorConcatenateSource {
                start: self.pack_dual_register_range(source.start, count)?,
                dimensions: source.dimensions.clone(),
            });
        }
        let count = dimensions.iter().try_fold(1usize, |count, extent| {
            checked_ad_product(
                count,
                *extent as usize,
                self.span,
                "tensor concatenate output",
            )
        })?;
        let dual_start = self.next_reg;
        for _ in 0..checked_ad_product(count, 2, self.span, "tensor concatenate dual output")? {
            self.alloc_reg()?;
        }
        self.ops.push(LinearOp::TensorConcatenate {
            dst_start: dual_start,
            sources: dual_sources.into_boxed_slice(),
            dimensions,
            axis,
            lanes: 2,
        });
        for offset in 0..count {
            let primal =
                checked_ad_reg_offset(dst_start, offset, self.span, "tensor concatenate output")?;
            let dual = checked_ad_reg_offset(
                dual_start,
                checked_ad_product(offset, 2, self.span, "tensor concatenate dual lane")?,
                self.span,
                "tensor concatenate dual output",
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

    /// One tensor-update axis for the dual program: coordinates are read at
    /// their primal value, and a slice repacks its index range primal-only.
    pub(super) fn primal_update_subscript(
        &mut self,
        subscript: &rumoca_ir_solve::TensorUpdateSubscript,
    ) -> Result<rumoca_ir_solve::TensorUpdateSubscript, SolveDerivationError> {
        Ok(match subscript {
            rumoca_ir_solve::TensorUpdateSubscript::Whole => {
                rumoca_ir_solve::TensorUpdateSubscript::Whole
            }
            rumoca_ir_solve::TensorUpdateSubscript::Index(
                rumoca_ir_solve::TensorIndex::Constant(coordinate),
            ) => rumoca_ir_solve::TensorUpdateSubscript::Index(
                rumoca_ir_solve::TensorIndex::Constant(*coordinate),
            ),
            rumoca_ir_solve::TensorUpdateSubscript::Index(
                rumoca_ir_solve::TensorIndex::Runtime(register),
            ) => rumoca_ir_solve::TensorUpdateSubscript::Index(
                rumoca_ir_solve::TensorIndex::Runtime(self.lookup(*register)?.re),
            ),
            rumoca_ir_solve::TensorUpdateSubscript::Slice { start, dimensions } => {
                let slice_count = dimensions.iter().try_fold(1usize, |count, extent| {
                    checked_ad_product(count, *extent as usize, self.span, "tensor update slice")
                })?;
                let mut primal = Vec::with_capacity(slice_count);
                for offset in 0..slice_count {
                    let register =
                        checked_ad_reg_offset(*start, offset, self.span, "tensor update slice")?;
                    primal.push(self.lookup(register)?.re);
                }
                rumoca_ir_solve::TensorUpdateSubscript::Slice {
                    start: self.pack_registers(&primal)?,
                    dimensions: dimensions.clone(),
                }
            }
        })
    }

    // SPEC_0021: Exception - exhaustive tensor-update subscript AD dispatch.
    #[allow(clippy::excessive_nesting)]
    pub(super) fn lower_tensor_update(
        &mut self,
        dst_start: Reg,
        base_start: Reg,
        value_start: Reg,
        dimensions: Box<[u32]>,
        subscripts: &[rumoca_ir_solve::TensorUpdateSubscript],
        lanes: usize,
    ) -> Result<(), SolveDerivationError> {
        if lanes != 1 {
            return Err(unsupported(
                "forward AD expects a primal tensor update with one lane",
            ));
        }
        let count = dimensions.iter().try_fold(1usize, |count, extent| {
            checked_ad_product(count, *extent as usize, self.span, "tensor update output")
        })?;
        let base_start = self.pack_dual_register_range(base_start, count)?;
        let mut value_count = 1usize;
        let mut dual_subscripts = Vec::with_capacity(subscripts.len());
        for (&extent, subscript) in dimensions.iter().zip(subscripts.iter()) {
            let axis_count = subscript
                .value_extent(extent)
                .ok_or_else(|| unsupported("tensor update value extent overflow"))?;
            value_count =
                checked_ad_product(value_count, axis_count, self.span, "tensor update value")?;
            dual_subscripts.push(self.primal_update_subscript(subscript)?);
        }
        let value_start = self.pack_dual_register_range(value_start, value_count)?;
        let dual_start = self.next_reg;
        for _ in 0..checked_ad_product(count, 2, self.span, "tensor update dual output")? {
            self.alloc_reg()?;
        }
        self.ops.push(LinearOp::TensorUpdate {
            dst_start: dual_start,
            base_start,
            value_start,
            dimensions,
            subscripts: dual_subscripts.into_boxed_slice(),
            lanes: 2,
        });
        for offset in 0..count {
            let primal =
                checked_ad_reg_offset(dst_start, offset, self.span, "tensor update output")?;
            let dual = checked_ad_reg_offset(
                dual_start,
                checked_ad_product(offset, 2, self.span, "tensor update dual lane")?,
                self.span,
                "tensor update dual output",
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

    pub(super) fn lower_tensor_fill(
        &mut self,
        dst_start: Reg,
        value_start: Reg,
        count: usize,
        lanes: usize,
    ) -> Result<(), SolveDerivationError> {
        if lanes != 1 {
            return Err(unsupported(
                "forward AD expects a primal tensor fill with one lane",
            ));
        }
        let value = self.lookup(value_start)?;
        if self.tangent_is_zero(value) {
            return self.emit_zero_tangent_result(dst_start, count, |dst_start| {
                LinearOp::TensorFill {
                    dst_start,
                    value_start: value.re,
                    count,
                    lanes: 1,
                }
            });
        }
        let value_start = self.pack_registers(&[value.re, value.du])?;
        let dual_start = self.next_reg;
        for _ in 0..checked_ad_product(count, 2, self.span, "tensor fill dual output")? {
            self.alloc_reg()?;
        }
        self.ops.push(LinearOp::TensorFill {
            dst_start: dual_start,
            value_start,
            count,
            lanes: 2,
        });
        for offset in 0..count {
            let primal = checked_ad_reg_offset(dst_start, offset, self.span, "tensor fill output")?;
            let dual = checked_ad_reg_offset(
                dual_start,
                checked_ad_product(offset, 2, self.span, "tensor fill dual lane")?,
                self.span,
                "tensor fill dual output",
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

    pub(super) fn lower_tensor_identity(
        &mut self,
        dst_start: Reg,
        size: usize,
        lanes: usize,
    ) -> Result<(), SolveDerivationError> {
        if lanes != 1 {
            return Err(unsupported(
                "forward AD expects a primal tensor identity with one lane",
            ));
        }
        let count = checked_ad_product(size, size, self.span, "tensor identity")?;
        self.emit_zero_tangent_result(dst_start, count, |dst_start| LinearOp::TensorIdentity {
            dst_start,
            size,
            lanes: 1,
        })
    }

    pub(super) fn lower_tensor_load(
        &mut self,
        dst_start: Reg,
        input: rumoca_ir_solve::TensorInputKind,
        input_start: usize,
        count: usize,
        seed_start: Option<usize>,
        lanes: usize,
    ) -> Result<(), SolveDerivationError> {
        if lanes != 1 || seed_start.is_some() {
            return Err(unsupported(
                "forward AD expects a primal tensor load with one lane and no seed",
            ));
        }
        if input == rumoca_ir_solve::TensorInputKind::Y
            && let SeedMode::SolverYSubset { active } = self.seed_mode
        {
            return self.lower_domain_tensor_load(dst_start, input_start, count, active);
        }
        let seed_start = match (input, self.seed_mode) {
            (rumoca_ir_solve::TensorInputKind::Y, _) => Some(input_start),
            (
                rumoca_ir_solve::TensorInputKind::P,
                SeedMode::SolverYOnly | SeedMode::SolverYSubset { .. },
            ) => None,
            (rumoca_ir_solve::TensorInputKind::P, SeedMode::SolverYAndP { .. }) => {
                Some(self.p_seed_index(input_start)?)
            }
        };
        if seed_start.is_none() {
            return self.emit_zero_tangent_result(dst_start, count, |dst_start| {
                LinearOp::TensorLoad {
                    dst_start,
                    input,
                    input_start,
                    count,
                    seed_start: None,
                    lanes: 1,
                }
            });
        }
        self.lower_active_tensor_load(dst_start, input, input_start, count, seed_start)
    }

    pub(super) fn lower_compare(
        &mut self,
        dst: Reg,
        op: CompareOp,
        lhs: Reg,
        rhs: Reg,
    ) -> Result<(), SolveDerivationError> {
        let l = self.lookup(lhs)?;
        let r = self.lookup(rhs)?;
        let re = self.emit_compare(op, l.re, r.re)?;
        let du = self.zero_reg()?;
        self.bind(dst, DualReg { re, du })
    }

    pub(super) fn lower_select(
        &mut self,
        dst: Reg,
        cond: Reg,
        if_true: Reg,
        if_false: Reg,
    ) -> Result<(), SolveDerivationError> {
        let c = self.lookup(cond)?;
        let t = self.lookup(if_true)?;
        let f = self.lookup(if_false)?;
        let re = self.emit_select(c.re, t.re, f.re)?;
        let du = self.emit_select(c.re, t.du, f.du)?;
        self.bind(dst, DualReg { re, du })
    }

    pub(super) fn lower_store(&mut self, src: Reg) -> Result<(), SolveDerivationError> {
        let d = self.lookup(src)?;
        match self.store_output_mode {
            StoreOutputMode::Derivative => {
                self.ops.push(LinearOp::StoreOutput { src: d.du });
            }
            StoreOutputMode::Primal => {
                self.ops.push(LinearOp::StoreOutput { src: d.re });
            }
            StoreOutputMode::Dual => {
                self.ops.push(LinearOp::StoreOutput { src: d.re });
                self.ops.push(LinearOp::StoreOutput { src: d.du });
            }
        }
        Ok(())
    }

    // SPEC_0021: Exception - exhaustive output-range AD source dispatch.
    #[allow(clippy::excessive_nesting)]
    pub(super) fn lower_store_range(
        &mut self,
        start: Reg,
        count: usize,
        stride: usize,
    ) -> Result<(), SolveDerivationError> {
        let mut sources = Vec::with_capacity(match self.store_output_mode {
            StoreOutputMode::Dual => count.saturating_mul(2),
            StoreOutputMode::Derivative | StoreOutputMode::Primal => count,
        });
        for ordinal in 0..count {
            let offset = ordinal
                .checked_mul(stride)
                .and_then(|offset| Reg::try_from(offset).ok())
                .ok_or_else(|| unsupported("function-conditional AD output range overflow"))?;
            let dual = self.lookup(start.checked_add(offset).ok_or_else(|| {
                unsupported("function-conditional AD output register overflow")
            })?)?;
            match self.store_output_mode {
                StoreOutputMode::Derivative => sources.push(dual.du),
                StoreOutputMode::Primal => sources.push(dual.re),
                StoreOutputMode::Dual => sources.extend([dual.re, dual.du]),
            }
        }
        let (&first, rest) = sources
            .split_first()
            .ok_or_else(|| unsupported("function-conditional AD output range is empty"))?;
        let derived_stride = rest
            .first()
            .map_or(1, |second| second.checked_sub(first).unwrap_or(0) as usize);
        if derived_stride == 0
            || sources
                .iter()
                .copied()
                .enumerate()
                .any(|(ordinal, register)| {
                    usize::try_from(first)
                        .ok()
                        .and_then(|base| base.checked_add(ordinal.saturating_mul(derived_stride)))
                        != usize::try_from(register).ok()
                })
        {
            let packed_start = self.pack_registers(&sources)?;
            self.ops.push(LinearOp::StoreOutputRange {
                start: packed_start,
                count: sources.len(),
                stride: 1,
            });
            return Ok(());
        }
        self.ops.push(LinearOp::StoreOutputRange {
            start: first,
            count: sources.len(),
            stride: derived_stride,
        });
        Ok(())
    }
}
