//! Whole-call entries that call the per-program functions of a row set (#683).
//!
//! Selectable residual rows and Jacobian rows are compiled one native function
//! per program, so any program can be called alone. A whole-block call then
//! enters one batch function instead of every program from Rust. The batch is
//! a thin function that calls those per-program functions in order, each at its
//! output offset and on the caller's scratch arena: the program bodies are
//! emitted once, so a whole-call entry costs one call per program to build.

use super::{
    AbiParam, CompileError, CraneliftEmitter, FuncId, FunctionBuilder, FunctionBuilderContext,
    InstBuilder, LinearOp, Linkage, Module, NEXT_RESIDUAL_KERNEL_ID, Ordering, PendingResidual,
    RESIDUAL_BATCH_OPS, ResidualFuncIds, RowKind, ScalarProgramBlock, declare_far_call_in_func,
    residual_program_cost, scratch, status, to_backend_err, types,
};

/// Whole-call entries for selectable rows, which have one entry per program:
/// consecutive programs that each have one function are grouped as residual
/// batches are, and a group of more than one becomes a call batch over those
/// functions; any other program serves a whole call through its own entry.
pub(super) fn compile_residual_call_batches(
    emitter: &mut CraneliftEmitter,
    rows: &[Vec<LinearOp>],
    programs: &[PendingResidual],
) -> Result<Vec<PendingResidual>, CompileError> {
    let mut whole = Vec::new();
    let mut start = 0usize;
    while start < programs.len() {
        let mut end = start;
        let mut operation_count = 0usize;
        while end < programs.len() && matches!(programs[end].0, ResidualFuncIds::Whole(_)) {
            let next = operation_count.saturating_add(residual_program_cost(&rows[end]));
            if end > start && next > RESIDUAL_BATCH_OPS {
                break;
            }
            operation_count = next;
            end += 1;
        }
        if end - start > 1 {
            let functions = programs[start..end]
                .iter()
                .filter_map(|program| match program.0 {
                    ResidualFuncIds::Whole(function) => Some(function),
                    ResidualFuncIds::Chunked(_) => None,
                })
                .collect::<Vec<_>>();
            let output_count = programs[start..end].iter().map(|program| program.3).sum();
            let function =
                emitter.compile_call_batch(&rows[start..end], &functions, RowKind::Residual)?;
            whole.push((
                ResidualFuncIds::Whole(function),
                0,
                programs[start].2,
                output_count,
            ));
            start = end;
        } else {
            whole.push(programs[start].clone());
            start += 1;
        }
    }
    Ok(whole)
}

impl CraneliftEmitter {
    /// A batch that calls its programs' own functions in order, each at its output
    /// offset and on the caller's scratch arena, instead of emitting their bodies
    /// again: a whole-block call is one native entry, at the build cost of one call
    /// per program. The first failing program ends the batch with its status.
    pub(super) fn compile_call_batch(
        &mut self,
        rows: &[Vec<LinearOp>],
        functions: &[FuncId],
        kind: RowKind,
    ) -> Result<FuncId, CompileError> {
        let pointer_type = self.module.target_config().pointer_type();
        let mut signature = self.module.make_signature();
        signature.returns.push(AbiParam::new(types::I8));
        signature.params.push(AbiParam::new(pointer_type)); // y
        signature.params.push(AbiParam::new(pointer_type)); // p
        signature.params.push(AbiParam::new(types::F64)); // t
        if kind.has_seed() {
            signature.params.push(AbiParam::new(pointer_type)); // v
        }
        signature.params.push(AbiParam::new(pointer_type)); // out
        signature.params.push(AbiParam::new(pointer_type)); // caller-owned scratch
        let profile_id = NEXT_RESIDUAL_KERNEL_ID.fetch_add(1, Ordering::Relaxed);
        let name = format!("rumoca_call_batch_{profile_id}");
        let func_id = self
            .module
            .declare_function(&name, Linkage::Local, &signature)
            .map_err(to_backend_err)?;
        let mut context = self.module.make_context();
        context.func.signature = signature;
        let mut fb_ctx = FunctionBuilderContext::new();
        {
            let mut fb = FunctionBuilder::new(&mut context.func, &mut fb_ctx);
            let entry = fb.create_block();
            fb.append_block_params_for_function_params(entry);
            fb.switch_to_block(entry);
            fb.seal_block(entry);
            let params = fb.block_params(entry).to_vec();
            scratch::enter(&mut fb);
            let out_index = if kind.has_seed() { 4 } else { 3 };
            let mut output_base = 0usize;
            for (row, function) in rows.iter().zip(functions) {
                let callee = declare_far_call_in_func(&mut self.module, *function, fb.func);
                let offset = i64::try_from(output_base * std::mem::size_of::<f64>())
                    .map_err(|_| CompileError::Backend("batch output offset overflows".into()))?;
                let out = fb.ins().iadd_imm(params[out_index], offset);
                let mut arguments = params.clone();
                arguments[out_index] = out;
                let call = fb.ins().call(callee, &arguments);
                let status = fb.inst_results(call)[0];
                status::propagate(&mut fb, status);
                output_base += ScalarProgramBlock::program_output_count(row);
            }
            status::succeed(&mut fb);
            fb.finalize();
        }
        // The batch allocates nothing itself; each callee that does enters its own frame.
        scratch::elide_unused_frame(&mut context.func);
        self.module.queue_function(func_id, &mut context)?;
        self.module.clear_context(&mut context);
        Ok(func_id)
    }
}
