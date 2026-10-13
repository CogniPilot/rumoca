use super::*;

impl<'a> AdBuilder<'a> {
    // SPEC_0021: Exception - exhaustive directional-ABI lowering for typed pure calls.
    #[expect(
        clippy::too_many_lines,
        reason = "all primal and tangent input/output forms are validated in one dispatcher"
    )]
    pub(super) fn lower_pure_call(
        &mut self,
        dst_start: Reg,
        input_starts: &[Reg],
        site: rumoca_ir_solve::SolvePureCallSite,
    ) -> Result<(), SolveDerivationError> {
        if input_starts.len() != site.inputs().len() {
            return Err(unsupported("typed pure-call AD input interface mismatch"));
        }
        // A call whose inputs carry no tangent has a zero tangent result and
        // needs no directional relation (a SOLVE-C62 group member has none).
        if self.lower_zero_tangent_call(dst_start, input_starts, &site)? {
            return Ok(());
        }
        let directional = site.directional().cloned().ok_or_else(|| {
            unsupported("typed pure-call directional owner has not been constructed")
        })?;
        let mut directional_inputs = Vec::with_capacity(directional.inputs().len());
        for (&start, value_type) in input_starts.iter().zip(site.inputs()) {
            let count = value_type.scalar_count() as usize;
            let mut primals = Vec::with_capacity(count);
            let mut tangents = Vec::with_capacity(count);
            for offset in 0..count {
                let register =
                    checked_ad_reg_offset(start, offset, self.span, "typed pure-call AD input")?;
                let value = self.lookup(register)?;
                primals.push(value.re);
                tangents.push(value.du);
            }
            directional_inputs.push(self.pack_registers(&primals)?);
            if matches!(
                value_type.element_type(),
                rumoca_ir_solve::SolveScalarType::Real { .. }
            ) {
                directional_inputs.push(self.pack_registers(&tangents)?);
            }
        }
        if directional_inputs.len() != directional.inputs().len() {
            return Err(unsupported(
                "typed pure-call directional input ABI does not match primal owner",
            ));
        }
        let output_count = directional
            .output_scalar_count()
            .ok_or_else(|| unsupported("typed pure-call directional output width overflows"))?;
        let directional_start = self.next_reg;
        for _ in 0..output_count {
            self.alloc_reg()?;
        }
        self.ops.push(LinearOp::PureCallDirectional {
            dst_start: directional_start,
            input_starts: directional_inputs.into_boxed_slice(),
            site: directional,
        });

        let mut primal_offset = 0usize;
        let mut directional_offset = 0usize;
        for output in site.outputs() {
            let count = output.value_type().scalar_count() as usize;
            let has_tangent = output.carries_tangent();
            let primal_start = checked_ad_reg_offset(
                directional_start,
                directional_offset,
                self.span,
                "typed pure-call directional primal output",
            )?;
            directional_offset = directional_offset
                .checked_add(count)
                .ok_or_else(|| unsupported("typed pure-call directional output overflow"))?;
            let tangent_start = has_tangent
                .then(|| {
                    checked_ad_reg_offset(
                        directional_start,
                        directional_offset,
                        self.span,
                        "typed pure-call directional tangent output",
                    )
                })
                .transpose()?;
            if has_tangent {
                directional_offset = directional_offset
                    .checked_add(count)
                    .ok_or_else(|| unsupported("typed pure-call tangent output overflow"))?;
            }
            for element in 0..count {
                let primal_register = checked_ad_reg_offset(
                    dst_start,
                    primal_offset + element,
                    self.span,
                    "typed pure-call primal result",
                )?;
                let re = checked_ad_reg_offset(
                    primal_start,
                    element,
                    self.span,
                    "typed pure-call directional primal element",
                )?;
                let du = match tangent_start {
                    Some(start) => checked_ad_reg_offset(
                        start,
                        element,
                        self.span,
                        "typed pure-call directional tangent element",
                    )?,
                    None => self.zero_reg()?,
                };
                self.bind(primal_register, DualReg { re, du })?;
            }
            primal_offset = primal_offset
                .checked_add(count)
                .ok_or_else(|| unsupported("typed pure-call primal output overflow"))?;
        }
        if directional_offset != output_count {
            return Err(unsupported(
                "typed pure-call directional output ABI does not match primal owner",
            ));
        }
        Ok(())
    }

    pub(super) fn lower_load_fold_carried(
        &mut self,
        dst: Reg,
        index: usize,
    ) -> Result<(), SolveDerivationError> {
        let primal_index = index
            .checked_mul(2)
            .ok_or_else(|| unsupported("function-fold AD carried index overflow"))?;
        let re = self.alloc_reg()?;
        self.ops.push(LinearOp::LoadFoldCarried {
            dst: re,
            index: primal_index,
        });
        let du = self.alloc_reg()?;
        self.ops.push(LinearOp::LoadFoldCarried {
            dst: du,
            index: primal_index + 1,
        });
        self.bind(dst, DualReg { re, du })
    }

    pub(super) fn lower_load_fold_index(
        &mut self,
        dst: Reg,
        dimension: usize,
    ) -> Result<(), SolveDerivationError> {
        let re = self.alloc_reg()?;
        self.ops
            .push(LinearOp::LoadFoldIndex { dst: re, dimension });
        let du = self.zero_reg()?;
        self.bind(dst, DualReg { re, du })
    }

    pub(super) fn lower_load_fold_capture(
        &mut self,
        dst: Reg,
        index: usize,
    ) -> Result<(), SolveDerivationError> {
        let primal_index = index
            .checked_mul(2)
            .ok_or_else(|| unsupported("function-fold AD capture index overflow"))?;
        let re = self.alloc_reg()?;
        self.ops.push(LinearOp::LoadFoldCapture {
            dst: re,
            index: primal_index,
        });
        let du = self.alloc_reg()?;
        self.ops.push(LinearOp::LoadFoldCapture {
            dst: du,
            index: primal_index + 1,
        });
        self.bind(dst, DualReg { re, du })
    }

    pub(super) fn lower_load_function_conditional_capture(
        &mut self,
        dst: Reg,
        index: usize,
    ) -> Result<(), SolveDerivationError> {
        let primal_index = index
            .checked_mul(2)
            .ok_or_else(|| unsupported("function-conditional AD capture index overflow"))?;
        let re = self.alloc_reg()?;
        self.ops.push(LinearOp::LoadFunctionConditionalCapture {
            dst: re,
            index: primal_index,
        });
        let du = self.alloc_reg()?;
        self.ops.push(LinearOp::LoadFunctionConditionalCapture {
            dst: du,
            index: primal_index + 1,
        });
        self.bind(dst, DualReg { re, du })
    }

    pub(super) fn lower_load_function_conditional_capture_range(
        &mut self,
        dst_start: Reg,
        index_start: usize,
        count: usize,
    ) -> Result<(), SolveDerivationError> {
        let dual_index_start = index_start
            .checked_mul(2)
            .ok_or_else(|| unsupported("function-conditional AD capture range index overflow"))?;
        let dual_count = count
            .checked_mul(2)
            .ok_or_else(|| unsupported("function-conditional AD capture range count overflow"))?;
        let dual_start = self.next_reg;
        for _ in 0..dual_count {
            self.alloc_reg()?;
        }
        self.ops
            .push(LinearOp::LoadFunctionConditionalCaptureRange {
                dst_start: dual_start,
                index_start: dual_index_start,
                count: dual_count,
            });
        for offset in 0..count {
            let primal = checked_ad_reg_offset(
                dst_start,
                offset,
                self.span,
                "function conditional capture range primal",
            )?;
            let dual = checked_ad_reg_offset(
                dual_start,
                checked_ad_product(
                    offset,
                    2,
                    self.span,
                    "function conditional capture range dual lane",
                )?,
                self.span,
                "function conditional capture range dual",
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

    pub(super) fn derived_fold_program(
        &mut self,
        primal: &Arc<rumoca_ir_solve::FunctionFoldProgram>,
    ) -> Result<Arc<rumoca_ir_solve::FunctionFoldProgram>, SolveDerivationError> {
        let key = Arc::as_ptr(primal) as usize;
        if let Some(program) = self.fold_program_cache.borrow().get(&key).cloned() {
            return Ok(program);
        }

        let mut update = Self::new_with_optional_span_and_fold_cache(
            self.seed_mode,
            self.span,
            self.fold_program_cache.clone(),
        );
        update.conditional_program_cache = self.conditional_program_cache.clone();
        update.store_output_mode = StoreOutputMode::Dual;
        for operation in &primal.update {
            update.lower_op(operation.clone())?;
        }
        let carried_count = primal
            .carried_count
            .checked_mul(2)
            .ok_or_else(|| unsupported("function-fold AD carried count overflow"))?;
        let capture_count = primal
            .capture_count
            .checked_mul(2)
            .ok_or_else(|| unsupported("function-fold AD capture count overflow"))?;
        // The continuation decides control flow only: it reads the primal
        // lanes and stops primal and tangent lanes together.
        let continuation = primal
            .continuation
            .as_ref()
            .map(|continuation| {
                self.derive_conditional_region(&continuation.ops, StoreOutputMode::Primal)
            })
            .transpose()?;
        let program = Arc::new(
            rumoca_ir_solve::FunctionFoldProgram::checked(
                primal.domain.clone(),
                carried_count,
                capture_count,
                update.ops,
            )
            .and_then(|program| match continuation {
                Some(ops) => program.with_continuation(ops),
                None => Ok(program),
            })
            .map_err(|error| {
                unsupported(&format!("function-fold AD register proof failed: {error}"))
            })?,
        );
        self.fold_program_cache
            .borrow_mut()
            .insert(key, program.clone());
        Ok(program)
    }

    // SPEC_0021: Exception - exhaustive derivative dispatch over conditional region operations.
    #[allow(clippy::excessive_nesting)]
    pub(super) fn derive_conditional_region(
        &self,
        primal: &[LinearOp],
        store_output_mode: StoreOutputMode,
    ) -> Result<Vec<LinearOp>, SolveDerivationError> {
        let mut region = Self::new_with_optional_span_and_fold_cache(
            self.seed_mode,
            self.span,
            self.fold_program_cache.clone(),
        );
        region.conditional_program_cache = self.conditional_program_cache.clone();
        region.store_output_mode = store_output_mode;
        for operation in primal {
            region.lower_op(operation.clone())?;
        }
        Ok(region.ops)
    }

    pub(super) fn derived_conditional_program(
        &mut self,
        primal: &Arc<rumoca_ir_solve::FunctionConditionalProgram>,
    ) -> Result<Arc<rumoca_ir_solve::FunctionConditionalProgram>, SolveDerivationError> {
        let key = Arc::as_ptr(primal) as usize;
        if let Some(program) = self.conditional_program_cache.borrow().get(&key).cloned() {
            return Ok(program);
        }

        let capture_count = checked_ad_product(
            primal.capture_count,
            2,
            self.span,
            "function-conditional AD captures",
        )?;
        let target_widths = primal
            .target_widths
            .iter()
            .map(|width| checked_ad_product(*width, 2, self.span, "function-conditional AD target"))
            .collect::<Result<Vec<_>, _>>()?;
        let mut arms =
            ad_vec_with_capacity(primal.arms.len(), "function-conditional AD arms", self.span)?;
        for arm in &primal.arms {
            arms.push((
                self.derive_conditional_region(&arm.condition, StoreOutputMode::Primal)?,
                self.derive_conditional_region(&arm.result, StoreOutputMode::Dual)?,
            ));
        }
        let fallback = self.derive_conditional_region(&primal.fallback, StoreOutputMode::Dual)?;
        let checked = match primal.owner {
            Some(owner) => rumoca_ir_solve::FunctionConditionalProgram::checked_owned(
                owner,
                capture_count,
                target_widths,
                arms,
                fallback,
            ),
            None => rumoca_ir_solve::FunctionConditionalProgram::checked(
                capture_count,
                target_widths,
                arms,
                fallback,
            ),
        }
        .map_err(|error| {
            unsupported(&format!(
                "function-conditional AD register proof failed: {error}"
            ))
        })?;
        let program = Arc::new(checked);
        self.conditional_program_cache
            .borrow_mut()
            .insert(key, program.clone());
        Ok(program)
    }

    pub(super) fn lower_function_fold(
        &mut self,
        dst_start: Reg,
        initial_start: Reg,
        capture_start: Reg,
        program: Arc<rumoca_ir_solve::FunctionFoldProgram>,
    ) -> Result<(), SolveDerivationError> {
        self.lower_function_fold_with_activation(
            dst_start,
            initial_start,
            capture_start,
            None,
            program,
        )
    }

    pub(super) fn lower_function_conditional(
        &mut self,
        dst_start: Reg,
        capture_start: Reg,
        program: Arc<rumoca_ir_solve::FunctionConditionalProgram>,
    ) -> Result<(), SolveDerivationError> {
        let packed_captures =
            self.pack_dual_register_range(capture_start, program.capture_count)?;
        let derived_program = self.derived_conditional_program(&program)?;
        let result_start = self.next_reg;
        for _ in 0..derived_program.result_count {
            self.alloc_reg()?;
        }
        self.ops.push(LinearOp::FunctionConditional {
            dst_start: result_start,
            capture_start: packed_captures,
            program: derived_program,
        });
        for offset in 0..program.result_count {
            let primal_dst =
                checked_ad_reg_offset(dst_start, offset, self.span, "function conditional output")?;
            let lane =
                checked_ad_product(offset, 2, self.span, "function conditional AD result lane")?;
            let re = checked_ad_reg_offset(
                result_start,
                lane,
                self.span,
                "function conditional AD primal result",
            )?;
            self.bind(primal_dst, DualReg { re, du: re + 1 })?;
        }
        Ok(())
    }

    pub(super) fn lower_guarded_function_fold(
        &mut self,
        dst_start: Reg,
        initial_start: Reg,
        capture_start: Reg,
        activation: Reg,
        program: Arc<rumoca_ir_solve::FunctionFoldProgram>,
    ) -> Result<(), SolveDerivationError> {
        let activation = self.lookup(activation)?.re;
        self.lower_function_fold_with_activation(
            dst_start,
            initial_start,
            capture_start,
            Some(activation),
            program,
        )
    }

    pub(super) fn lower_function_fold_with_activation(
        &mut self,
        dst_start: Reg,
        initial_start: Reg,
        capture_start: Reg,
        activation: Option<Reg>,
        program: Arc<rumoca_ir_solve::FunctionFoldProgram>,
    ) -> Result<(), SolveDerivationError> {
        let packed_initial = self.pack_dual_register_range(initial_start, program.carried_count)?;
        let packed_captures =
            self.pack_dual_register_range(capture_start, program.capture_count)?;
        let derived_program = self.derived_fold_program(&program)?;
        let carried_count = derived_program.carried_count;
        let folded_start = self.next_reg;
        for _ in 0..carried_count {
            self.alloc_reg()?;
        }
        self.ops.push(match activation {
            Some(activation) => LinearOp::GuardedFunctionFold {
                dst_start: folded_start,
                initial_start: packed_initial,
                capture_start: packed_captures,
                activation,
                program: derived_program,
            },
            None => LinearOp::FunctionFold {
                dst_start: folded_start,
                initial_start: packed_initial,
                capture_start: packed_captures,
                program: derived_program,
            },
        });
        for offset in 0..program.carried_count {
            let offset_reg = Reg::try_from(offset)
                .map_err(|_| unsupported("function-fold AD destination offset overflow"))?;
            let primal_dst = dst_start
                .checked_add(offset_reg)
                .ok_or_else(|| unsupported("function-fold AD destination overflow"))?;
            let lane = offset_reg
                .checked_mul(2)
                .ok_or_else(|| unsupported("function-fold AD lane overflow"))?;
            self.bind(
                primal_dst,
                DualReg {
                    re: folded_start + lane,
                    du: folded_start + lane + 1,
                },
            )?;
        }
        Ok(())
    }
}
