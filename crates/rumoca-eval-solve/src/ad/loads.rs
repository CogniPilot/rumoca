use super::*;

impl<'a> AdBuilder<'a> {
    pub(super) fn lower_const(&mut self, dst: Reg, value: f64) -> Result<(), SolveDerivationError> {
        let re = self.emit_const(value)?;
        let du = self.zero_reg()?;
        self.bind(dst, DualReg { re, du })
    }

    pub(super) fn lower_load_time(&mut self, dst: Reg) -> Result<(), SolveDerivationError> {
        let re = self.emit_load_time()?;
        let du = self.zero_reg()?;
        self.bind(dst, DualReg { re, du })
    }

    pub(super) fn lower_random_initial_state(
        &mut self,
        dst: Reg,
        generator: RandomGenerator,
        local_seed: Reg,
        global_seed: Reg,
        state_len: usize,
        state_index: usize,
    ) -> Result<(), SolveDerivationError> {
        let local_seed = self.lookup(local_seed)?.re;
        let global_seed = self.lookup(global_seed)?.re;
        let re = self.alloc_reg()?;
        self.ops.push(LinearOp::RandomInitialState {
            dst: re,
            generator,
            local_seed,
            global_seed,
            state_len,
            state_index,
        });
        let du = self.zero_reg()?;
        self.bind(dst, DualReg { re, du })
    }

    pub(super) fn lower_random_result(
        &mut self,
        dst: Reg,
        generator: RandomGenerator,
        state_start: Reg,
        state_len: usize,
    ) -> Result<(), SolveDerivationError> {
        let state_start = self.pack_random_primal_state(state_start, state_len)?;
        let re = self.alloc_reg()?;
        self.ops.push(LinearOp::RandomResult {
            dst: re,
            generator,
            state_start,
            state_len,
        });
        let du = self.zero_reg()?;
        self.bind(dst, DualReg { re, du })
    }

    pub(super) fn lower_random_state(
        &mut self,
        dst: Reg,
        generator: RandomGenerator,
        state_start: Reg,
        state_len: usize,
        state_index: usize,
    ) -> Result<(), SolveDerivationError> {
        let state_start = self.pack_random_primal_state(state_start, state_len)?;
        let re = self.alloc_reg()?;
        self.ops.push(LinearOp::RandomState {
            dst: re,
            generator,
            state_start,
            state_len,
            state_index,
        });
        let du = self.zero_reg()?;
        self.bind(dst, DualReg { re, du })
    }

    pub(super) fn pack_random_primal_state(
        &mut self,
        state_start: Reg,
        state_len: usize,
    ) -> Result<Reg, SolveDerivationError> {
        let values = collect_dual_range(
            self,
            state_start,
            state_len,
            self.span,
            "random AD state value count",
            "random state range",
        )?;
        let values = real_regs_from_duals(&values, "random AD primal state count", self.span)?;
        self.pack_registers(&values)
    }

    pub(super) fn lower_move(&mut self, dst: Reg, src: Reg) -> Result<(), SolveDerivationError> {
        let value = self.lookup(src)?;
        self.bind(dst, value)
    }

    pub(super) fn lower_linear_solve_component(
        &mut self,
        dst: Reg,
        matrix_start: Reg,
        rhs_start: Reg,
        n: usize,
        component: usize,
    ) -> Result<(), SolveDerivationError> {
        if n == 0 || component >= n {
            return Err(unsupported("invalid LinearSolveComponent shape in AD row"));
        }

        let span = self.span;
        let matrix_len = checked_ad_product(n, n, span, "LinearSolveComponent AD matrix range")?;
        let matrix = collect_dual_range(
            self,
            matrix_start,
            matrix_len,
            span,
            "LinearSolveComponent AD matrix value count",
            "matrix range",
        )?;
        let rhs = collect_dual_range(
            self,
            rhs_start,
            n,
            span,
            "LinearSolveComponent AD RHS value count",
            "rhs range",
        )?;

        let matrix_re = real_regs_from_duals(
            &matrix,
            "LinearSolveComponent AD matrix real value count",
            span,
        )?;
        let rhs_re =
            real_regs_from_duals(&rhs, "LinearSolveComponent AD RHS real value count", span)?;
        let matrix_start_re = self.pack_registers(&matrix_re)?;
        let rhs_start_re = self.pack_registers(&rhs_re)?;

        let mut solution_regs =
            ad_vec_with_capacity(n, "LinearSolveComponent AD solution value count", span)?;
        for solution_component in 0..n {
            let solution = self.alloc_reg()?;
            self.ops.push(LinearOp::LinearSolveComponent {
                dst: solution,
                matrix_start: matrix_start_re,
                rhs_start: rhs_start_re,
                n,
                component: solution_component,
            });
            solution_regs.push(solution);
        }

        let mut tangent_rhs =
            ad_vec_with_capacity(n, "LinearSolveComponent AD tangent RHS value count", span)?;
        for row in 0..n {
            let mut acc = rhs[row].du;
            for col in 0..n {
                let matrix_du = matrix[row * n + col].du;
                let product = self.emit_binary(BinaryOp::Mul, matrix_du, solution_regs[col])?;
                acc = self.emit_binary(BinaryOp::Sub, acc, product)?;
            }
            tangent_rhs.push(acc);
        }

        let tangent_rhs_start = self.pack_registers(&tangent_rhs)?;
        let du = self.alloc_reg()?;
        self.ops.push(LinearOp::LinearSolveComponent {
            dst: du,
            matrix_start: matrix_start_re,
            rhs_start: tangent_rhs_start,
            n,
            component,
        });

        self.bind(
            dst,
            DualReg {
                re: solution_regs[component],
                du,
            },
        )
    }

    pub(super) fn lower_load_y(
        &mut self,
        dst: Reg,
        index: usize,
    ) -> Result<(), SolveDerivationError> {
        let re = self.emit_load_y(index)?;
        let du = if self.seed_mode.y_is_active(index) {
            self.emit_load_seed(index)?
        } else {
            self.zero_reg()?
        };
        self.bind(dst, DualReg { re, du })
    }

    pub(super) fn lower_load_p(
        &mut self,
        dst: Reg,
        index: usize,
    ) -> Result<(), SolveDerivationError> {
        let re = self.emit_load_p(index)?;
        let du = match self.seed_mode {
            SeedMode::SolverYOnly | SeedMode::SolverYSubset { .. } => self.zero_reg()?,
            SeedMode::SolverYAndP { .. } => self.emit_load_seed(self.p_seed_index(index)?)?,
        };
        self.bind(dst, DualReg { re, du })
    }

    /// Forward-mode dual of a runtime-indexed parameter load. The value is
    /// loaded at the same runtime offset; its tangent is zero under solver-y AD
    /// and, under parameter-seed AD, the seed at the matching offset shifted
    /// into the seed region (`p_seed_index` is affine, so the whole run shifts
    /// by `p_seed_offset` while the index register is reused unchanged).
    pub(super) fn lower_load_indexed_p(
        &mut self,
        dst: Reg,
        base: usize,
        count: usize,
        index: Reg,
    ) -> Result<(), SolveDerivationError> {
        let idx = self.lookup(index)?;
        let re = self.emit_load_indexed_p(base, count, idx.re)?;
        let du = match self.seed_mode {
            SeedMode::SolverYOnly | SeedMode::SolverYSubset { .. } => self.zero_reg()?,
            SeedMode::SolverYAndP { .. } => {
                self.emit_load_indexed_seed(self.p_seed_index(base)?, count, idx.re)?
            }
        };
        self.bind(dst, DualReg { re, du })
    }

    pub(super) fn lower_load_indexed_register(
        &mut self,
        dst: Reg,
        base: Reg,
        stride: usize,
        dimensions: Box<[u32]>,
        indices: &[rumoca_ir_solve::TensorIndex],
    ) -> Result<(), SolveDerivationError> {
        let count = dimensions
            .iter()
            .try_fold(1usize, |count, &extent| count.checked_mul(extent as usize))
            .ok_or_else(|| unsupported("runtime tensor projection AD extent overflow"))?;
        let mut real =
            ad_vec_with_capacity(count, "runtime tensor projection AD real count", self.span)?;
        let mut tangent = ad_vec_with_capacity(
            count,
            "runtime tensor projection AD tangent count",
            self.span,
        )?;
        for offset in 0..count {
            let source = base
                .checked_add(
                    Reg::try_from(offset.checked_mul(stride).ok_or_else(|| {
                        unsupported("runtime tensor projection AD stride overflow")
                    })?)
                    .map_err(|_| unsupported("runtime tensor projection AD stride overflow"))?,
                )
                .ok_or_else(|| unsupported("runtime tensor projection AD source overflow"))?;
            let dual = self.lookup(source)?;
            real.push(dual.re);
            tangent.push(dual.du);
        }
        let indices = indices
            .iter()
            .map(|index| match *index {
                rumoca_ir_solve::TensorIndex::Constant(coordinate) => {
                    Ok(rumoca_ir_solve::TensorIndex::Constant(coordinate))
                }
                rumoca_ir_solve::TensorIndex::Runtime(register) => Ok(
                    rumoca_ir_solve::TensorIndex::Runtime(self.lookup(register)?.re),
                ),
            })
            .collect::<Result<Vec<_>, SolveDerivationError>>()?
            .into_boxed_slice();
        let real_base = self.pack_registers(&real)?;
        let re = self.alloc_reg()?;
        self.ops.push(LinearOp::LoadIndexedRegister {
            dst: re,
            base: real_base,
            stride: 1,
            dimensions: dimensions.clone(),
            indices: indices.clone(),
        });
        let tangent_base = self.pack_registers(&tangent)?;
        let du = self.alloc_reg()?;
        self.ops.push(LinearOp::LoadIndexedRegister {
            dst: du,
            base: tangent_base,
            stride: 1,
            dimensions,
            indices,
        });
        self.bind(dst, DualReg { re, du })
    }

    pub(super) fn lower_load_indexed_fold_carried(
        &mut self,
        dst: Reg,
        base: usize,
        stride: usize,
        dimensions: Box<[u32]>,
        indices: &[rumoca_ir_solve::TensorIndex],
    ) -> Result<(), SolveDerivationError> {
        let indices = indices
            .iter()
            .map(|index| match *index {
                rumoca_ir_solve::TensorIndex::Constant(coordinate) => {
                    Ok(rumoca_ir_solve::TensorIndex::Constant(coordinate))
                }
                rumoca_ir_solve::TensorIndex::Runtime(register) => Ok(
                    rumoca_ir_solve::TensorIndex::Runtime(self.lookup(register)?.re),
                ),
            })
            .collect::<Result<Vec<_>, SolveDerivationError>>()?
            .into_boxed_slice();
        let dual_base = base
            .checked_mul(2)
            .ok_or_else(|| unsupported("indexed function-fold AD base overflow"))?;
        let dual_stride = stride
            .checked_mul(2)
            .ok_or_else(|| unsupported("indexed function-fold AD stride overflow"))?;
        let re = self.alloc_reg()?;
        self.ops.push(LinearOp::LoadIndexedFoldCarried {
            dst: re,
            base: dual_base,
            stride: dual_stride,
            dimensions: dimensions.clone(),
            indices: indices.clone(),
        });
        let du = self.alloc_reg()?;
        self.ops.push(LinearOp::LoadIndexedFoldCarried {
            dst: du,
            base: dual_base + 1,
            stride: dual_stride,
            dimensions,
            indices,
        });
        self.bind(dst, DualReg { re, du })
    }

    pub(super) fn lower_load_indexed_fold_capture(
        &mut self,
        dst: Reg,
        base: usize,
        stride: usize,
        dimensions: Box<[u32]>,
        indices: &[rumoca_ir_solve::TensorIndex],
    ) -> Result<(), SolveDerivationError> {
        let indices = indices
            .iter()
            .map(|index| match *index {
                rumoca_ir_solve::TensorIndex::Constant(coordinate) => {
                    Ok(rumoca_ir_solve::TensorIndex::Constant(coordinate))
                }
                rumoca_ir_solve::TensorIndex::Runtime(register) => Ok(
                    rumoca_ir_solve::TensorIndex::Runtime(self.lookup(register)?.re),
                ),
            })
            .collect::<Result<Vec<_>, SolveDerivationError>>()?
            .into_boxed_slice();
        let dual_base = base
            .checked_mul(2)
            .ok_or_else(|| unsupported("indexed function-fold AD capture base overflow"))?;
        let dual_stride = stride
            .checked_mul(2)
            .ok_or_else(|| unsupported("indexed function-fold AD capture stride overflow"))?;
        let re = self.alloc_reg()?;
        self.ops.push(LinearOp::LoadIndexedFoldCapture {
            dst: re,
            base: dual_base,
            stride: dual_stride,
            dimensions: dimensions.clone(),
            indices: indices.clone(),
        });
        let du = self.alloc_reg()?;
        self.ops.push(LinearOp::LoadIndexedFoldCapture {
            dst: du,
            base: dual_base + 1,
            stride: dual_stride,
            dimensions,
            indices,
        });
        self.bind(dst, DualReg { re, du })
    }

    // SPEC_0021: Exception - exhaustive tensor-subscript AD dispatch.
    #[allow(clippy::excessive_nesting)]
    pub(super) fn lower_store_fold_tensor_update(
        &mut self,
        store: FoldTensorUpdateStore<'_>,
    ) -> Result<(), SolveDerivationError> {
        let FoldTensorUpdateStore {
            source_base,
            source_stride,
            dimensions,
            updates,
            nodes,
            result,
            lanes,
        } = store;
        if source_stride != 1 || lanes != 1 {
            return Err(unsupported("nested dual-lane function-fold tensor update"));
        }
        let mut dual_updates = Vec::with_capacity(updates.len());
        for update in updates {
            if update.value_stride != 1 {
                return Err(unsupported("nested dual-lane function-fold tensor patch"));
            }
            let subscripts = update
                .subscripts
                .iter()
                .map(|subscript| self.primal_update_subscript(subscript))
                .collect::<Result<Vec<_>, SolveDerivationError>>()?
                .into_boxed_slice();
            let condition = update
                .condition
                .map(|condition| self.lookup(condition).map(|dual| dual.re))
                .transpose()?;
            let value_count = update
                .value_count(dimensions)
                .ok_or_else(|| unsupported("function-fold tensor update AD extent overflow"))?;
            let mut values = Vec::with_capacity(value_count.saturating_mul(2));
            for element in 0..value_count {
                let element = Reg::try_from(element)
                    .map_err(|_| unsupported("function-fold tensor update AD register overflow"))?;
                let source = update
                    .value_start
                    .checked_add(element)
                    .ok_or_else(|| unsupported("function-fold tensor update AD value overflow"))?;
                let dual = self.lookup(source)?;
                values.extend([dual.re, dual.du]);
            }
            let value_start = self.pack_registers(&values)?;
            dual_updates.push(rumoca_ir_solve::FoldTensorUpdate {
                subscripts,
                condition,
                value_start,
                value_stride: 2,
            });
        }
        let nodes = nodes
            .iter()
            .map(|node| match *node {
                rumoca_ir_solve::FoldTensorNode::Update { base, update } => {
                    Ok(rumoca_ir_solve::FoldTensorNode::Update { base, update })
                }
                rumoca_ir_solve::FoldTensorNode::Select {
                    condition,
                    if_true,
                    if_false,
                } => Ok(rumoca_ir_solve::FoldTensorNode::Select {
                    condition: self.lookup(condition)?.re,
                    if_true,
                    if_false,
                }),
            })
            .collect::<Result<Vec<_>, SolveDerivationError>>()?;
        self.ops.push(LinearOp::StoreOutputFoldTensorUpdate {
            source_base: source_base
                .checked_mul(2)
                .ok_or_else(|| unsupported("function-fold tensor update AD base overflow"))?,
            source_stride: 2,
            dimensions: dimensions.into(),
            updates: dual_updates.into_boxed_slice(),
            nodes: nodes.into_boxed_slice(),
            result,
            lanes: 2,
        });
        Ok(())
    }

    // SPEC_0021: Exception - exhaustive nested-fold initial-source AD dispatch.
    #[allow(clippy::excessive_nesting)]
    pub(super) fn lower_store_output_function_fold(
        &mut self,
        initial: Box<[rumoca_ir_solve::FoldInitialSource]>,
        capture_start: Reg,
        program: Arc<rumoca_ir_solve::FunctionFoldProgram>,
        result: (usize, usize),
        condition: Option<Reg>,
        nested_when_true: bool,
    ) -> Result<(), SolveDerivationError> {
        let (result_base, count) = result;
        let mut dual_initial = Vec::with_capacity(initial.len());
        for source in initial {
            match source {
                rumoca_ir_solve::FoldInitialSource::ParentCarried { base, count } => {
                    dual_initial.push(rumoca_ir_solve::FoldInitialSource::ParentCarried {
                        base: base.checked_mul(2).ok_or_else(|| {
                            unsupported("nested fold AD parent initial base overflow")
                        })?,
                        count: count.checked_mul(2).ok_or_else(|| {
                            unsupported("nested fold AD parent initial count overflow")
                        })?,
                    });
                }
                rumoca_ir_solve::FoldInitialSource::Registers { start, count } => {
                    let mut values = Vec::with_capacity(count.saturating_mul(2));
                    for offset in 0..count {
                        let source = start
                            .checked_add(Reg::try_from(offset).map_err(|_| {
                                unsupported("nested fold AD initial offset overflow")
                            })?)
                            .ok_or_else(|| {
                                unsupported("nested fold AD initial register overflow")
                            })?;
                        let dual = self.lookup(source)?;
                        values.extend([dual.re, dual.du]);
                    }
                    dual_initial.push(rumoca_ir_solve::FoldInitialSource::Registers {
                        start: self.pack_registers(&values)?,
                        count: count
                            .checked_mul(2)
                            .ok_or_else(|| unsupported("nested fold AD initial count overflow"))?,
                    });
                }
            }
        }
        let mut captures = Vec::with_capacity(program.capture_count.saturating_mul(2));
        for offset in 0..program.capture_count {
            let source = capture_start
                .checked_add(
                    Reg::try_from(offset)
                        .map_err(|_| unsupported("nested fold AD capture offset overflow"))?,
                )
                .ok_or_else(|| unsupported("nested fold AD capture register overflow"))?;
            let dual = self.lookup(source)?;
            captures.extend([dual.re, dual.du]);
        }
        let capture_start = self.pack_registers(&captures)?;
        let program = self.derived_fold_program(&program)?;
        let condition = condition
            .map(|condition| self.lookup(condition).map(|value| value.re))
            .transpose()?;
        self.ops.push(LinearOp::StoreOutputFunctionFold {
            initial: dual_initial.into_boxed_slice(),
            capture_start,
            program,
            result_base: result_base
                .checked_mul(2)
                .ok_or_else(|| unsupported("nested fold AD result base overflow"))?,
            count: count
                .checked_mul(2)
                .ok_or_else(|| unsupported("nested fold AD result count overflow"))?,
            condition,
            nested_when_true,
        });
        Ok(())
    }
}
