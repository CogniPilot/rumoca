//! Scalar-register ABI conversion surrounds the unchanged checked typed helpers.
use super::glue::{CallGlue, HostRun, Staging};
use super::layout::CommitPiece;
use super::*;
use crate::typed_call::program::{CallAbi, CallLayout, OutputAbi};

const CELL: MemArg = MemArg {
    offset: 0,
    align: 3,
    memory_index: 0,
};

impl BodyEmitter<'_> {
    pub(in crate::emit) fn emit_native_pure_call(
        &mut self,
        op: LinearOp,
        prefix: Option<&[LinearOp]>,
        glue: Option<&CallGlue>,
    ) -> Result<(), String> {
        let memo = self.calls.and_then(|plan| {
            prefix.and_then(|prefix| super::memo::find(&plan.view().memos, prefix, &op))
        });
        let LinearOp::PureCall {
            dst_start,
            input_starts,
            site,
        } = op
        else {
            return Err("expected a primal issued call".into());
        };
        let plan = self
            .calls
            .ok_or("native pure call requires its model table")?;
        let call = plan.call(&site)?;
        // A shared call (SOLVE-C73) is issued once, at its first occurrence in
        // execution order; every later occurrence reads the memo tuple, so no
        // flag or branch decides it at run time.
        if memo.is_none_or(|memo| memo.issue()) {
            self.pass_call_inputs(site.inputs(), &input_starts, &call, glue)?;
            self.pass_call_buffers(&call, glue, memo)?;
            for offset in [plan.view().input, plan.view().output, plan.view().scratch] {
                self.call_address(offset);
            }
            self.push(Instruction::Call(call.function));
            self.push(Instruction::LocalTee(plan.view().status));
            self.push(Instruction::If(BlockType::Empty));
            self.push(Instruction::LocalGet(plan.view().status));
            self.push(Instruction::Return);
            self.push(Instruction::End);
            if let Some(memo) = memo {
                self.call_address(memo.tuple);
                self.call_address(call_results(plan, &call.abi));
                self.push(Instruction::I32Const(memo.bytes as i32));
                self.push(Instruction::MemoryCopy {
                    src_mem: 0,
                    dst_mem: 0,
                });
            }
        }
        // A memoized call leaves its results in the memo, where every use reads them.
        let results = memo.map_or_else(|| call_results(plan, &call.abi), |memo| memo.tuple);
        self.read_call_outputs(dst_start, site.outputs(), (&call.abi, results), glue)
    }

    pub(in crate::emit) fn emit_native_directional_call(
        &mut self,
        operation: &LinearOp,
    ) -> Result<(), String> {
        let LinearOp::PureCallDirectional {
            dst_start,
            input_starts,
            site,
        } = operation
        else {
            return Err("expected an issued directional call".into());
        };
        let plan = self
            .calls
            .ok_or("directional call requires its original model table")?;
        let call = plan.directional_call(site)?;
        self.pass_call_inputs(site.inputs(), input_starts, &call, None)?;
        self.pass_call_buffers(&call, None, None)?;
        for offset in [plan.view().input, plan.view().output, plan.view().scratch] {
            self.call_address(offset);
        }
        self.push(Instruction::Call(call.function));
        self.push(Instruction::LocalTee(plan.view().status));
        self.push(Instruction::If(BlockType::Empty));
        self.push(Instruction::LocalGet(plan.view().status));
        self.push(Instruction::Return);
        self.push(Instruction::End);
        self.read_call_outputs(
            *dst_start,
            site.outputs(),
            (&call.abi, call_results(plan, &call.abi)),
            None,
        )
    }

    /// Fill the callee's input tuple: small values are packed, and a value
    /// passed by reference is read in place from host Y or P when the glue
    /// proved its registers are exactly those cells, and is otherwise staged.
    pub(in crate::emit) fn pass_call_inputs(
        &mut self,
        inputs: &[solve::SolveValueType],
        input_starts: &[Reg],
        call: &CallLayout,
        glue: Option<&CallGlue>,
    ) -> Result<(), String> {
        let tuple = self.calls.ok_or("missing native call layout")?.view().input;
        let staging = Staging::of(&call.abi, call.input)?;
        let mut flat = 0u32;
        for (k, (&start, value)) in input_starts.iter().zip(inputs).enumerate() {
            let cell = tuple + call.abi.inputs[k].offset;
            if call.abi.inputs[k].by_reference {
                let host = glue.and_then(|glue| glue.inputs.get(k).copied().flatten());
                self.pass_reference_input(
                    start,
                    value,
                    flat,
                    host,
                    (cell, tuple + staging.inputs[k]),
                )?;
            } else {
                self.pack_input(start, cell, value, flat)?;
            }
            flat = flat
                .checked_add(value.scalar_count())
                .ok_or("native typed input overflow")?;
        }
        Ok(())
    }

    /// Store in `cell` the address of one input: the host cells the glue
    /// proved are the registers, or a staging buffer filled from them.
    fn pass_reference_input(
        &mut self,
        start: Reg,
        value: &solve::SolveValueType,
        flat: u32,
        host: Option<HostRun>,
        (cell, staged): (u32, u32),
    ) -> Result<(), String> {
        if host.is_none() {
            self.pack_input(start, staged, value, flat)?;
        }
        self.call_address(cell);
        match host {
            Some(run) => self.push_host_address(run)?,
            None => self.call_address(staged),
        }
        self.push(Instruction::I64ExtendI32U);
        self.push(Instruction::I64Store(CELL));
        Ok(())
    }

    fn push_host_address(&mut self, run: HostRun) -> Result<(), String> {
        match run {
            HostRun::Cells { input, first_cell } => {
                self.push(Instruction::LocalGet(match input {
                    solve::TensorInputKind::Y => Y_PTR_PARAM,
                    solve::TensorInputKind::P => P_PTR_PARAM,
                }));
                self.push(Instruction::I32Const(arena::layouts::cell_bytes(
                    first_cell,
                )?));
            }
            HostRun::Lane { offset } => {
                self.push_typed_lanes_base()?;
                self.push(Instruction::I32Const(
                    i32::try_from(offset).map_err(|_| "native input lane overflow")?,
                ));
            }
        }
        self.push(Instruction::I32Add);
        Ok(())
    }

    /// Store, in each cell the callee reserves for it, the address of the
    /// buffer it builds a by-reference output in: the call's memo buffer, the
    /// Y cells the glue proved the output is stored to, or a staging buffer.
    pub(in crate::emit) fn pass_observation_buffers(
        &mut self,
        call: &CallLayout,
    ) -> Result<(), String> {
        self.pass_call_buffers(call, None, None)
    }

    fn pass_call_buffers(
        &mut self,
        call: &CallLayout,
        glue: Option<&CallGlue>,
        memo: Option<&super::memo::Memo>,
    ) -> Result<(), String> {
        let tuple = self.calls.ok_or("missing native call layout")?.view().input;
        let staging = Staging::of(&call.abi, call.input)?;
        for (k, output) in call.abi.outputs.iter().enumerate() {
            let Some(cell) = output.buffer_cell else {
                continue;
            };
            self.call_address(tuple + cell);
            let in_place = glue.and_then(|glue| glue.outputs.get(k).copied().flatten());
            match (memo, in_place) {
                (Some(memo), _) => self.call_address(memo.buffers[k]),
                (None, Some(first)) => {
                    self.push(Instruction::LocalGet(OUT_PTR_PARAM));
                    self.push(Instruction::I32Const(arena::layouts::cell_bytes(first)?));
                    self.push(Instruction::I32Add);
                }
                (None, None) => self.call_address(tuple + staging.outputs[k]),
            }
            self.push(Instruction::I64ExtendI32U);
            self.push(Instruction::I64Store(CELL));
        }
        Ok(())
    }

    /// Move each output of the call just made to its registers, except an
    /// output built in the Y cells it is stored to.
    fn read_call_outputs(
        &mut self,
        dst_start: Reg,
        outputs: &[solve::SolvePureCallOutput],
        (abi, results): (&CallAbi, u32),
        glue: Option<&CallGlue>,
    ) -> Result<(), String> {
        let (mut register, mut flat) = (dst_start, 0u32);
        for (k, output) in outputs.iter().enumerate() {
            let value = output.value_type();
            let cell = results + abi.outputs[k].offset;
            if glue.is_some_and(|glue| glue.unread[k]) {
                // Nothing reads its registers.
            } else if abi.outputs[k].buffer_cell.is_some() {
                let in_place = glue.and_then(|glue| glue.outputs.get(k).copied().flatten());
                let carried = glue
                    .and_then(|glue| glue.carried.get(k))
                    .map_or(&[][..], Vec::as_slice);
                self.read_reference_output(register, cell, &abi.outputs[k], (in_place, carried))?;
            } else {
                self.unpack_output(register, cell, value, flat)?;
            }
            flat = flat
                .checked_add(value.scalar_count())
                .ok_or("native typed output overflow")?;
            register = register
                .checked_add(value.scalar_count())
                .ok_or("native typed output overflow")?;
        }
        Ok(())
    }

    /// The address in a by-reference result cell is the buffer the callee
    /// built the output in, or the storage of a value it returned unchanged.
    /// An output built in its Y cells is copied there when the cell names
    /// other storage.
    fn read_reference_output(
        &mut self,
        register: Reg,
        cell: u32,
        output: &OutputAbi,
        (in_place, carried): (Option<usize>, &[CommitPiece]),
    ) -> Result<(), String> {
        let status = self
            .calls
            .ok_or("missing native call layout")?
            .view()
            .status;
        self.call_address(cell);
        self.push(Instruction::I64Load(CELL));
        self.push(Instruction::I32WrapI64);
        self.push(Instruction::LocalSet(status));
        let bytes = i32::try_from(output.bytes).map_err(|_| "native call output overflow")?;
        match in_place {
            Some(first) if !carried.is_empty() => {
                self.commit_carried_output(first, carried, (status, output.bytes))?;
            }
            Some(first) => {
                let target = arena::layouts::cell_bytes(first)?;
                let push_target = |this: &mut Self| {
                    this.push(Instruction::LocalGet(OUT_PTR_PARAM));
                    this.push(Instruction::I32Const(target));
                    this.push(Instruction::I32Add);
                };
                push_target(self);
                self.push(Instruction::LocalGet(status));
                self.push(Instruction::I32Ne);
                self.push(Instruction::If(BlockType::Empty));
                push_target(self);
                self.push(Instruction::LocalGet(status));
                self.push(Instruction::I32Const(bytes));
                self.push(Instruction::MemoryCopy {
                    src_mem: 0,
                    dst_mem: 0,
                });
                self.push(Instruction::End);
            }
            None => {
                self.push_arena_address(register, None, 1)?;
                self.push(Instruction::LocalGet(status));
                self.push(Instruction::I32Const(bytes));
                self.push(Instruction::MemoryCopy {
                    src_mem: 0,
                    dst_mem: 1,
                });
            }
        }
        Ok(())
    }

    /// Where the results of the call `op`, preceded by `prefix`, are read after
    /// it ran: its memo when it has one, otherwise the head of its scratch.
    pub(in crate::emit) fn call_results_base(
        &self,
        site: &solve::SolvePureCallSite,
        (prefix, op): (&[LinearOp], &LinearOp),
    ) -> Result<u32, String> {
        let plan = self.calls.ok_or("missing native call layout")?;
        if let Some(memo) = super::memo::find(&plan.view().memos, prefix, op) {
            return Ok(memo.tuple);
        }
        Ok(call_results(plan, &plan.call(site)?.abi))
    }

    /// The offset of the first of `count` consecutive result cells of a call whose
    /// results start at `results`, counted in scalars across its outputs.
    pub(in crate::emit) fn result_cell_offset(
        &self,
        site: &solve::SolvePureCallSite,
        (cell, results): (u32, u32),
    ) -> Result<u32, String> {
        let plan = self.calls.ok_or("missing native call layout")?;
        let abi = plan.call(site)?.abi;
        let mut first = 0u32;
        for (output, layout) in site.outputs().iter().zip(&abi.outputs) {
            let count = output.value_type().scalar_count();
            let end = first
                .checked_add(count)
                .ok_or("native call cell overflow")?;
            if cell < end {
                return Ok(results + layout.offset + (cell - first) * 8);
            }
            first = end;
        }
        Err("native call cell is outside the call outputs".into())
    }

    pub(in crate::emit) fn call_address(&mut self, offset: u32) {
        self.push(Instruction::LocalGet(SEED_PTR_PARAM));
        self.push(Instruction::I32Const(offset as i32));
        self.push(Instruction::I32Add);
    }

    pub(super) fn call_cell_address(&mut self, offset: u32, counter: u32) {
        self.call_address(offset);
        self.push(Instruction::LocalGet(counter));
        self.push(Instruction::I32Const(8));
        self.push(Instruction::I32Mul);
        self.push(Instruction::I32Add);
    }

    /// Pack one call input value. An Integer cell the stage binds to a typed
    /// input lane (SOLVE-C69) copies that `i64` exactly; every other cell is
    /// converted from its Real register.
    fn pack_input(
        &mut self,
        start: Reg,
        offset: u32,
        value: &solve::SolveValueType,
        flat: u32,
    ) -> Result<(), String> {
        let counter = self
            .arena
            .as_ref()
            .ok_or("missing native register arena")?
            .counter;
        let count = value.scalar_count();
        let end = flat
            .checked_add(count)
            .ok_or("native typed input overflow")?;
        let runs = if self.region_path.is_empty() {
            self.native_stage
                .map(|stage| {
                    stage
                        .integer_argument_runs(self.operation_ordinal)
                        .filter(|run| run.cells().start < end && flat < run.cells().end)
                        .cloned()
                        .collect::<Vec<_>>()
                })
                .unwrap_or_default()
        } else {
            Vec::new()
        };
        if runs.is_empty() && is_binary64(value) {
            // Real cells need no conversion: one bulk copy from the register file.
            self.call_address(offset);
            self.push_arena_address(start, None, 1)?;
            self.push(Instruction::I32Const(arena::layouts::cell_bytes(
                count as usize,
            )?));
            self.push(Instruction::MemoryCopy {
                src_mem: 1,
                dst_mem: 0,
            });
            return Ok(());
        }
        self.cells(counter, count as usize, |this| {
            this.pack_bound_input(start, offset, value, flat, counter, &runs)
        })
    }

    fn pack_bound_input(
        &mut self,
        start: Reg,
        offset: u32,
        value: &solve::SolveValueType,
        flat: u32,
        counter: u32,
        runs: &[solve::NativeIntegerArgumentRun],
    ) -> Result<(), String> {
        let Some((run, remaining)) = runs.split_first() else {
            return self.pack_cell(start, offset, value, counter);
        };
        self.push(Instruction::LocalGet(counter));
        self.push(Instruction::I32Const(flat as i32));
        self.push(Instruction::I32Add);
        self.push(Instruction::I32Const(run.cells().start as i32));
        self.push(Instruction::I32GeU);
        self.push(Instruction::LocalGet(counter));
        self.push(Instruction::I32Const(flat as i32));
        self.push(Instruction::I32Add);
        self.push(Instruction::I32Const(run.cells().end as i32));
        self.push(Instruction::I32LtU);
        self.push(Instruction::I32And);
        self.push(Instruction::If(BlockType::Empty));
        self.call_cell_address(offset, counter);
        self.push_typed_lanes_base()?;
        self.push(Instruction::LocalGet(counter));
        self.push(Instruction::I32Const(flat as i32));
        self.push(Instruction::I32Add);
        self.push(Instruction::I32Const(run.cells().start as i32));
        self.push(Instruction::I32Sub);
        self.push(Instruction::I32Const(8));
        self.push(Instruction::I32Mul);
        self.push(Instruction::I32Add);
        self.push(Instruction::I64Load(MemArg {
            offset: run.lane_offset() as u64,
            align: 3,
            memory_index: 0,
        }));
        self.push(Instruction::I64Store(MemArg {
            offset: 0,
            align: 3,
            memory_index: 0,
        }));
        self.push(Instruction::Else);
        self.pack_bound_input(start, offset, value, flat, counter, remaining)?;
        self.push(Instruction::End);
        Ok(())
    }

    fn pack_cell(
        &mut self,
        start: Reg,
        offset: u32,
        value: &solve::SolveValueType,
        counter: u32,
    ) -> Result<(), String> {
        self.call_cell_address(offset, counter);
        self.push_arena_address(start, Some(counter), 1)?;
        self.push(Instruction::F64Load(arena::memarg()));
        match value.element_type() {
            solve::SolveScalarType::Real {
                format: solve::SolveRealFormat::Binary64,
                ..
            } => {
                self.push(Instruction::F64Store(MemArg {
                    offset: 0,
                    align: 3,
                    memory_index: 0,
                }));
            }
            solve::SolveScalarType::Boolean => {
                self.push(Instruction::F64Const(0.0.into()));
                self.push(Instruction::F64Ne);
                self.push(Instruction::I64ExtendI32U);
                self.push(Instruction::I64Store(MemArg {
                    offset: 0,
                    align: 3,
                    memory_index: 0,
                }));
            }
            solve::SolveScalarType::Integer(domain) => self.pack_integer(domain),
            _ => return Err("native scalar call requires Binary64".into()),
        }
        Ok(())
    }

    fn pack_integer(&mut self, domain: solve::SolveIntegerDomain) {
        let integer = self.calls.expect("checked call layout").view().integer;
        self.push(Instruction::LocalSet(LOCAL_BASE));
        self.push(Instruction::LocalGet(LOCAL_BASE));
        self.push(Instruction::LocalGet(LOCAL_BASE));
        self.push(Instruction::F64Ne);
        self.return_status_if(2);
        for (limit, compare) in [
            (i64::MIN as f64, Instruction::F64Lt),
            (-(i64::MIN as f64), Instruction::F64Ge),
        ] {
            self.push(Instruction::LocalGet(LOCAL_BASE));
            self.push(Instruction::F64Const(limit.into()));
            self.push(compare);
            self.return_status_if(2);
        }
        self.push(Instruction::LocalGet(LOCAL_BASE));
        self.push(Instruction::I64TruncF64S);
        self.push(Instruction::LocalTee(integer));
        self.push(Instruction::F64ConvertI64S);
        self.push(Instruction::LocalGet(LOCAL_BASE));
        self.push(Instruction::F64Ne);
        self.return_status_if(2);
        for (limit, compare) in [
            (domain.minimum(), Instruction::I64LtS),
            (domain.maximum(), Instruction::I64GtS),
        ] {
            self.push(Instruction::LocalGet(integer));
            self.push(Instruction::I64Const(limit));
            self.push(compare);
            self.return_status_if(2);
        }
        self.push(Instruction::LocalGet(integer));
        self.push(Instruction::I64Store(MemArg {
            offset: 0,
            align: 3,
            memory_index: 0,
        }));
    }

    fn unpack_output(
        &mut self,
        start: Reg,
        offset: u32,
        value: &solve::SolveValueType,
        flat: u32,
    ) -> Result<(), String> {
        let counter = self
            .arena
            .as_ref()
            .ok_or("missing native register arena")?
            .counter;
        let count = value.scalar_count();
        let authority = self
            .region_path
            .is_empty()
            .then_some(())
            .and(self.native_stage);
        let admitted = authority
            .is_some_and(|stage| stage.integer_result_cells(self.operation_ordinal).is_some());
        if is_binary64(value) {
            self.push_arena_address(start, None, 1)?;
            self.call_address(offset);
            self.push(Instruction::I32Const(arena::layouts::cell_bytes(
                count as usize,
            )?));
            self.push(Instruction::MemoryCopy {
                src_mem: 0,
                dst_mem: 1,
            });
            return Ok(());
        }
        if !admitted || !matches!(value.element_type(), solve::SolveScalarType::Integer(_)) {
            return self.cells(counter, count as usize, |this| {
                this.unpack_cell(start, offset, value, counter, true)
            });
        }
        let end = flat
            .checked_add(count)
            .ok_or("native typed output overflow")?;
        let checks = authority
            .and_then(|stage| stage.integer_result_checks(self.operation_ordinal))
            .unwrap_or_default()
            .iter()
            .filter_map(|check| check.in_range(flat as usize..end as usize))
            .collect::<Vec<_>>();
        self.cells(counter, count as usize, |this| {
            if !checks.is_empty() {
                this.input_check_condition(flat as usize, counter, &checks)?;
                this.push(Instruction::If(BlockType::Empty));
                this.call_cell_address(offset, counter);
                this.push(Instruction::I64Load(MemArg {
                    offset: 0,
                    align: 3,
                    memory_index: 0,
                }));
                this.require_exact_binary64();
                this.push(Instruction::End);
            }
            this.unpack_cell(start, offset, value, counter, false)
        })
    }

    fn unpack_cell(
        &mut self,
        start: Reg,
        offset: u32,
        value: &solve::SolveValueType,
        counter: u32,
        checked: bool,
    ) -> Result<(), String> {
        self.push_arena_address(start, Some(counter), 1)?;
        self.call_cell_address(offset, counter);
        match value.element_type() {
            solve::SolveScalarType::Real {
                format: solve::SolveRealFormat::Binary64,
                ..
            } => {
                self.push(Instruction::F64Load(MemArg {
                    offset: 0,
                    align: 3,
                    memory_index: 0,
                }));
            }
            solve::SolveScalarType::Integer(_) => {
                self.push(Instruction::I64Load(MemArg {
                    offset: 0,
                    align: 3,
                    memory_index: 0,
                }));
                // A read Real view of an Integer result must be exact (SOLVE-C69).
                if checked {
                    let integer = self
                        .calls
                        .ok_or("missing native call layout")?
                        .view()
                        .integer;
                    self.push(Instruction::LocalTee(integer));
                    self.require_exact_binary64();
                    self.push(Instruction::LocalGet(integer));
                }
                self.push(Instruction::F64ConvertI64S);
            }
            solve::SolveScalarType::Boolean => {
                self.push(Instruction::I64Load(MemArg {
                    offset: 0,
                    align: 3,
                    memory_index: 0,
                }));
                self.push(Instruction::F64ConvertI64U);
            }
            _ => return Err("native scalar call requires Binary64".into()),
        }
        self.push(Instruction::F64Store(arena::memarg()));
        Ok(())
    }
}

impl BodyEmitter<'_> {
    /// Write one typed input lane's Real view into its slot of P (SOLVE-C69;
    /// the entry saved the slots and restores them): an Integer by
    /// IntegerToReal, checked to be exact where the view has an unbound reader;
    /// a Boolean byte must be 0 or 1. Both refuse with status 2 instead of
    /// rounding or reinterpreting.
    pub(in crate::emit) fn write_input_run(
        &mut self,
        input: &solve::NativeInputRun,
        checks: &[solve::NativeInputCheck],
    ) -> Result<(), String> {
        let counter = self
            .arena
            .as_ref()
            .ok_or("missing native register arena")?
            .counter;
        let view = u32::try_from(input.p_range().start)
            .ok()
            .and_then(|n| n.checked_mul(8))
            .ok_or("native input view overflow")?;
        let lane = u32::try_from(input.lane_offset()).map_err(|_| "native input lane overflow")?;
        match input.lane() {
            solve::NativeOutputLane::Integer => {
                self.cells(counter, input.scalar_count(), |this| {
                    this.push_p_cell_address(view, counter);
                    this.push_lane_cell_address(input, counter);
                    this.push(Instruction::I64Load(MemArg {
                        offset: lane as u64,
                        align: 3,
                        memory_index: 0,
                    }));
                    this.push(Instruction::F64ConvertI64S);
                    this.push(Instruction::F64Store(CELL));
                    Ok(())
                })?;
                self.check_integer_views(input, checks, counter)
            }
            solve::NativeOutputLane::Boolean => {
                let status = self
                    .calls
                    .ok_or("missing native call layout")?
                    .view()
                    .status;
                self.cells(counter, input.scalar_count(), |this| {
                    this.push_lane_cell_address(input, counter);
                    this.push(Instruction::I32Load8U(MemArg {
                        offset: lane as u64,
                        align: 0,
                        memory_index: 0,
                    }));
                    this.push(Instruction::LocalTee(status));
                    this.push(Instruction::I32Const(1));
                    this.push(Instruction::I32GtU);
                    this.return_status_if(2);
                    this.push_p_cell_address(view, counter);
                    this.push(Instruction::LocalGet(status));
                    this.push(Instruction::F64ConvertI32U);
                    this.push(Instruction::F64Store(CELL));
                    Ok(())
                })
            }
            solve::NativeOutputLane::Real => Err("a Real input has no typed input lane".into()),
        }
    }

    /// Refuse an Integer view an unbound reader reads that the Binary64 view
    /// would round. The reads that select one cell are decided here, once per
    /// cell; the reads that select several share one loop over the run.
    fn check_integer_views(
        &mut self,
        input: &solve::NativeInputRun,
        checks: &[solve::NativeInputCheck],
        counter: u32,
    ) -> Result<(), String> {
        let range = input.p_range();
        let (single, ranged): (Vec<_>, Vec<_>) = checks
            .iter()
            .filter_map(|check| check.in_range(range.clone()))
            .partition(|check| check.selected().count() == 1 || check.selected().stride() == 0);
        let cells = single
            .iter()
            .map(|check| check.selected().start())
            .filter(|cell| single.iter().any(|check| check.contains(*cell)))
            .collect::<std::collections::BTreeSet<_>>();
        let lane = u64::try_from(input.lane_offset()).map_err(|_| "native input lane overflow")?;
        for cell in cells {
            let offset = cell
                .checked_sub(range.start)
                .and_then(|n| n.checked_mul(8))
                .and_then(|n| u64::try_from(n).ok())
                .and_then(|n| n.checked_add(lane))
                .ok_or("native input lane overflow")?;
            self.push(Instruction::LocalGet(OUT_PTR_PARAM));
            self.push(Instruction::I64Load(MemArg {
                offset,
                align: 3,
                memory_index: 0,
            }));
            self.require_exact_binary64();
        }
        if ranged.is_empty() {
            return Ok(());
        }
        let p_start = range.start;
        self.cells(counter, input.scalar_count(), |this| {
            this.input_check_condition(p_start, counter, &ranged)?;
            this.push(Instruction::If(BlockType::Empty));
            this.push_lane_cell_address(input, counter);
            this.push(Instruction::I64Load(MemArg {
                offset: lane,
                align: 3,
                memory_index: 0,
            }));
            this.require_exact_binary64();
            this.push(Instruction::End);
            Ok(())
        })
    }

    /// The address of cell `counter` of an input's typed lane, before the
    /// lane's own offset.
    fn push_lane_cell_address(&mut self, input: &solve::NativeInputRun, counter: u32) {
        self.push(Instruction::LocalGet(OUT_PTR_PARAM));
        self.push(Instruction::LocalGet(counter));
        self.push(Instruction::I32Const(input.lane().width() as i32));
        self.push(Instruction::I32Mul);
        self.push(Instruction::I32Add);
    }

    fn push_p_cell_address(&mut self, offset: u32, counter: u32) {
        self.push(Instruction::LocalGet(P_PTR_PARAM));
        self.push(Instruction::I32Const(offset as i32));
        self.push(Instruction::I32Add);
        self.push(Instruction::LocalGet(counter));
        self.push(Instruction::I32Const(8));
        self.push(Instruction::I32Mul);
        self.push(Instruction::I32Add);
    }

    fn selection_condition(
        &mut self,
        base: usize,
        counter: u32,
        selected: solve::NativeInputSelection,
    ) -> Result<(), String> {
        let constant = |value: usize| {
            u32::try_from(value)
                .map(|value| value as i32)
                .map_err(|_| "native typed selection overflow")
        };
        if selected.count() == 0 {
            self.push(Instruction::I32Const(0));
            return Ok(());
        }
        let cell = |this: &mut Self| -> Result<(), String> {
            this.push(Instruction::LocalGet(counter));
            this.push(Instruction::I32Const(constant(base)?));
            this.push(Instruction::I32Add);
            Ok(())
        };
        cell(self)?;
        self.push(Instruction::I32Const(constant(selected.start())?));
        if selected.stride() == 0 {
            self.push(Instruction::I32Eq);
            return Ok(());
        }
        self.push(Instruction::I32GeU);
        cell(self)?;
        self.push(Instruction::I32Const(constant(selected.start())?));
        self.push(Instruction::I32Sub);
        self.push(Instruction::I32Const(constant(selected.stride())?));
        self.push(Instruction::I32RemU);
        self.push(Instruction::I32Eqz);
        self.push(Instruction::I32And);
        cell(self)?;
        self.push(Instruction::I32Const(constant(selected.start())?));
        self.push(Instruction::I32Sub);
        self.push(Instruction::I32Const(constant(selected.stride())?));
        self.push(Instruction::I32DivU);
        self.push(Instruction::I32Const(constant(selected.count())?));
        self.push(Instruction::I32LtU);
        self.push(Instruction::I32And);
        Ok(())
    }

    fn input_check_condition(
        &mut self,
        base: usize,
        counter: u32,
        checks: &[solve::NativeInputCheck],
    ) -> Result<(), String> {
        self.push(Instruction::I32Const(0));
        for check in checks {
            self.selection_condition(base, counter, check.selected())?;
            for bound in check.bound() {
                self.selection_condition(base, counter, *bound)?;
                self.push(Instruction::I32Eqz);
                self.push(Instruction::I32And);
            }
            self.push(Instruction::I32Or);
        }
        Ok(())
    }

    /// Consume the i64 on the stack; return status 2 unless its magnitude is
    /// at most 2^53, so its Binary64 conversion is exact.
    fn require_exact_binary64(&mut self) {
        self.push(Instruction::I64Const(1 << 53));
        self.push(Instruction::I64Add);
        self.push(Instruction::I64Const(1 << 54));
        self.push(Instruction::I64GtU);
        self.return_status_if(2);
    }
}

/// Whether every cell of the value is a Binary64 Real, stored as its own bits.
fn is_binary64(value: &solve::SolveValueType) -> bool {
    matches!(
        value.element_type(),
        solve::SolveScalarType::Real {
            format: solve::SolveRealFormat::Binary64,
            ..
        }
    )
}

/// Where a call leaves its outputs: the head of its scratch, or its output
/// buffer for a callee that publishes there.
fn call_results(plan: &CallProgramPlan, abi: &CallAbi) -> u32 {
    if abi.outputs_in_scratch {
        plan.view().scratch
    } else {
        plan.view().output
    }
}
