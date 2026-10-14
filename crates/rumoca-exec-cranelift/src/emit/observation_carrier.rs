//! Private execution-local observations. Only the checked compiled adapter enters it.
use super::CompileError;
use rumoca_ir_solve as solve;
use std::{cell::Cell, collections::BTreeMap};

pub(super) const ROOT_ENTER: u32 = 1;
pub(super) const ROOT_EXIT: u32 = 2;
pub(super) const CALL_ENTER: u32 = 3;
pub(super) const CALL_EXIT: u32 = 4;
pub(super) const RECORD: u32 = 5;
pub(super) const LOOP_ENTER: u32 = 6;
pub(super) const LOOP_EXIT: u32 = 7;
pub(super) const REGION_ENTER: u32 = 8;
pub(super) const REGION_EXIT: u32 = 9;

#[derive(Clone)]
struct Frame {
    owner: solve::SolvePureCallOwnerId,
    _arguments: Vec<u64>,
    operation: usize,
    loops: Vec<(usize, i64)>,
    regions: Vec<(usize, i64)>,
    first_record: usize,
}

pub(crate) struct Record {
    pub(crate) operation: usize,
    pub(crate) predicate: usize,
    pub(crate) span: rumoca_core::Span,
    pub(crate) captures: Vec<(usize, Vec<u64>)>,
    // Retained actual invocation arguments and compact-domain coordinates.
    _frames: Vec<Frame>,
    _origin_operation: usize,
}

pub(crate) struct Storage {
    capacity: usize,
    roots: BTreeMap<usize, usize>,
    frames: Vec<Frame>,
    records: Vec<Record>,
}

pub(crate) struct Carrier<'source> {
    source: &'source solve::RetainedEventProgram,
    capacity: usize,
    roots: BTreeMap<usize, usize>,
    frames: Vec<Frame>,
    pub(crate) records: Vec<Record>,
}

thread_local! {
    static ACTIVE: Cell<*mut Carrier<'static>> = const { Cell::new(std::ptr::null_mut()) };
}

impl<'source> Carrier<'source> {
    pub(crate) fn new(
        source: &'source solve::RetainedEventProgram,
        code: &[solve::LinearOp],
    ) -> Result<Self, CompileError> {
        let capacity = scalar_bound(source, source.program(), &mut Vec::new())?;
        let mut roots = BTreeMap::new();
        for invocation in source.invocations() {
            let slice = scalar_region(code, invocation.region_path())
                .ok_or_else(|| error("compiled region differs from its issued source"))?;
            let operation = slice
                .get(invocation.operation())
                .ok_or_else(|| error("compiled observation operation is absent"))?;
            roots
                .entry(operation as *const _ as usize)
                .or_insert(invocation.index());
        }
        let mut records = Vec::new();
        records
            .try_reserve_exact(capacity)
            .map_err(|_| error("observation capacity cannot be allocated"))?;
        Ok(Self {
            source,
            capacity,
            roots,
            frames: Vec::new(),
            records,
        })
    }

    pub(crate) fn into_storage(self) -> Storage {
        Storage {
            capacity: self.capacity,
            roots: self.roots,
            frames: self.frames,
            records: self.records,
        }
    }
    pub(crate) fn resume(source: &'source solve::RetainedEventProgram, storage: Storage) -> Self {
        Self {
            source,
            capacity: storage.capacity,
            roots: storage.roots,
            frames: storage.frames,
            records: storage.records,
        }
    }

    pub(crate) fn execute<T>(
        &mut self,
        call: impl FnOnce() -> Result<T, CompileError>,
    ) -> Result<T, CompileError> {
        struct Guard(*mut Carrier<'static>);
        impl Drop for Guard {
            fn drop(&mut self) {
                ACTIVE.with(|active| active.set(self.0));
            }
        }
        self.records.clear();
        self.frames.clear();
        let previous = ACTIVE.with(|active| active.replace((self as *mut Self).cast()));
        let _guard = Guard(previous);
        let result = call();
        if result.is_ok() && !self.frames.is_empty() {
            return Err(error("native invocation frames did not close"));
        }
        result
    }

    fn root_enter(&mut self, owner: u32, bits: &[u64], marker: i64) -> Result<(), CompileError> {
        if !self.frames.is_empty() {
            return Err(error("overlapping source invocations"));
        }
        let index = *self.roots.get(&(marker as usize)).ok_or_else(|| {
            error("native observation is foreign to its compiled source operation")
        })?;
        let invocation = self
            .source
            .invocation_by_index(index)
            .ok_or_else(|| error("native observation lost its issued source binding"))?;
        if invocation.owner().id().index() != owner {
            return Err(error(
                "native observation owner differs from its issued source",
            ));
        }
        if bits.len() != invocation.owner().input_coordinate().input_cells() as usize {
            return Err(error("native observation argument width differs"));
        }
        self.frames.push(Frame {
            owner: invocation.owner().id(),
            _arguments: bits.to_vec(),
            operation: index,
            loops: Vec::new(),
            regions: Vec::new(),
            first_record: self.records.len(),
        });
        Ok(())
    }

    fn child_enter(
        &mut self,
        operation: Option<&solve::SolveSpannedOperation>,
        bits: &[u64],
    ) -> Result<(), CompileError> {
        let operation = operation.ok_or_else(|| error("missing native call source"))?;
        let solve::SolveOperation::Call { owner, .. } = operation.operation() else {
            return Err(error("native frame is not an issued call"));
        };
        let child = self
            .source
            .table()
            .owner(*owner)
            .ok_or_else(|| error("native child owner is foreign"))?;
        if bits.len() != child.input_coordinate().input_cells() as usize {
            return Err(error("native child argument width differs"));
        }
        self.frames.push(Frame {
            owner: *owner,
            _arguments: bits.to_vec(),
            operation: operation as *const _ as usize,
            loops: Vec::new(),
            regions: Vec::new(),
            first_record: self.records.len(),
        });
        Ok(())
    }

    fn event(
        &mut self,
        kind: u32,
        owner: u32,
        operation: Option<&solve::SolveSpannedOperation>,
        bits: &[u64],
        ordinal: i64,
    ) -> Result<(), CompileError> {
        match kind {
            ROOT_ENTER => self.root_enter(owner, bits, ordinal)?,
            ROOT_EXIT => {
                if self.frames.len() != 1 {
                    return Err(error("unclosed native child invocation"));
                }
                self.frames.pop();
            }
            CALL_ENTER => self.child_enter(operation, bits)?,
            CALL_EXIT => {
                self.forward(operation.ok_or_else(|| error("missing forwarding source"))?)?
            }
            RECORD => self.record(
                operation.ok_or_else(|| error("missing assertion source"))?,
                bits,
            )?,
            LOOP_ENTER => self
                .frames
                .last_mut()
                .ok_or_else(|| error("loop outside invocation"))?
                .loops
                .push((
                    operation.ok_or_else(|| error("missing loop source"))? as *const _ as usize,
                    ordinal,
                )),
            LOOP_EXIT => {
                self.frames
                    .last_mut()
                    .ok_or_else(|| error("loop outside invocation"))?
                    .loops
                    .pop()
                    .ok_or_else(|| error("unbalanced native loop"))?;
            }
            REGION_ENTER => self
                .frames
                .last_mut()
                .ok_or_else(|| error("region outside invocation"))?
                .regions
                .push((
                    operation.ok_or_else(|| error("missing region source"))? as *const _ as usize,
                    ordinal,
                )),
            REGION_EXIT => {
                self.frames
                    .last_mut()
                    .ok_or_else(|| error("region outside invocation"))?
                    .regions
                    .pop()
                    .ok_or_else(|| error("unbalanced native region"))?;
            }
            _ => return Err(error("unknown observation event")),
        }
        Ok(())
    }

    fn record(
        &mut self,
        operation: &solve::SolveSpannedOperation,
        bits: &[u64],
    ) -> Result<(), CompileError> {
        if self.records.len() >= self.capacity {
            return Err(error(
                "native observations exceeded their proved execution bound",
            ));
        }
        let solve::SolveOperation::CheckAssertion {
            predicate_output,
            message_outputs,
            ..
        } = operation.operation()
        else {
            return Err(error("native record is not an issued assertion"));
        };
        let frame = self
            .frames
            .last()
            .ok_or_else(|| error("assertion outside invocation"))?;
        let owner = self
            .source
            .table()
            .owner(frame.owner)
            .ok_or_else(|| error("assertion owner is foreign"))?;
        let mut offset = 0usize;
        let mut captures = Vec::new();
        for &output in message_outputs {
            let width = owner
                .outputs()
                .get(output)
                .ok_or_else(|| error("capture output is foreign"))?
                .value_type()
                .scalar_count() as usize;
            let end = offset
                .checked_add(width)
                .ok_or_else(|| error("capture width overflows"))?;
            captures.push((
                output,
                bits.get(offset..end)
                    .ok_or_else(|| error("capture was not initialized"))?
                    .to_vec(),
            ));
            offset = end;
        }
        if offset != bits.len() {
            return Err(error("capture width differs"));
        }
        self.records.push(Record {
            operation: self.frames[0].operation,
            predicate: *predicate_output,
            span: operation.provenance(),
            captures,
            _frames: self.frames.clone(),
            _origin_operation: operation as *const _ as usize,
        });
        Ok(())
    }

    fn forward(&mut self, operation: &solve::SolveSpannedOperation) -> Result<(), CompileError> {
        let child = self
            .frames
            .pop()
            .ok_or_else(|| error("missing native child frame"))?;
        let parent = self
            .frames
            .last()
            .ok_or_else(|| error("missing native parent frame"))?;
        let solve::SolveOperation::Call {
            owner,
            assertion_forwarding,
            ..
        } = operation.operation()
        else {
            return Err(error("invalid native forwarding source"));
        };
        if *owner != child.owner || child.operation != operation as *const _ as usize {
            return Err(error("native forwarding changed its exact invocation"));
        }
        let owner = self
            .source
            .table()
            .owner(parent.owner)
            .ok_or_else(|| error("parent owner is foreign"))?;
        for record in &mut self.records[child.first_record..] {
            let predicate = assertion_forwarding
                .iter()
                .find(|binding| binding.child_predicate() == record.predicate)
                .ok_or_else(|| error("native child has no issued forwarding"))?
                .parent_predicate();
            let outputs: Vec<_> = owner
                .outputs()
                .iter()
                .enumerate()
                .filter_map(|(index, output)| {
                    (output.message_predicate_output(index) == Some(predicate)).then_some(index)
                })
                .collect();
            if outputs.len() != record.captures.len() {
                return Err(error("native capture forwarding width differs"));
            }
            for ((index, _), output) in record.captures.iter_mut().zip(outputs) {
                *index = output;
            }
            record.predicate = predicate;
        }
        Ok(())
    }
}

fn program_bound(
    table: &solve::SolvePureCallTable,
    program: &solve::TypedProgram,
    chain: &mut Vec<solve::SolvePureCallOwnerId>,
) -> Result<usize, CompileError> {
    program
        .operations()
        .iter()
        .try_fold(0usize, |sum, operation| {
            let count = match operation.operation() {
                solve::SolveOperation::CheckAssertion { .. } => 1,
                solve::SolveOperation::Call { owner, .. } => owner_bound(table, *owner, chain)?,
                solve::SolveOperation::Conditional {
                    if_true, if_false, ..
                } => program_bound(table, if_true.body(), chain)?.max(program_bound(
                    table,
                    if_false.body(),
                    chain,
                )?),
                solve::SolveOperation::AssignedConditional { program } => program_bound(
                    table,
                    program.if_true().body(),
                    chain,
                )?
                .max(program_bound(table, program.if_false().body(), chain)?),
                solve::SolveOperation::Fold {
                    domain,
                    transition,
                    continuation,
                    ..
                } => {
                    let passes = domain
                        .scalar_count()
                        .map_err(|_| error("invalid observation domain"))?;
                    let body = program_bound(table, transition.body(), chain)?;
                    let condition = continuation
                        .as_deref()
                        .map(|body| program_bound(table, body.body(), chain))
                        .transpose()?
                        .unwrap_or(0);
                    passes
                        .checked_mul(
                            body.checked_add(condition)
                                .ok_or_else(|| error("observation bound overflows"))?,
                        )
                        .ok_or_else(|| error("observation bound overflows"))?
                }
                _ => 0,
            };
            sum.checked_add(count)
                .ok_or_else(|| error("observation bound overflows"))
        })
}

fn owner_bound(
    table: &solve::SolvePureCallTable,
    owner: solve::SolvePureCallOwnerId,
    chain: &mut Vec<solve::SolvePureCallOwnerId>,
) -> Result<usize, CompileError> {
    let child = table
        .owner(owner)
        .ok_or_else(|| error("foreign observation owner"))?;
    if child
        .assertion_flow()
        .is_some_and(|flow| flow.sources().is_empty())
    {
        return Ok(0);
    }
    if chain.contains(&owner) {
        return Err(error("recursive observation capacity is not finite"));
    }
    chain.push(owner);
    let bound = program_bound(table, child.body(), chain);
    chain.pop();
    bound
}

fn error(message: &str) -> CompileError {
    CompileError::Backend(message.to_owned())
}

/// Called only by code emitted with this private ABI while its checked adapter
/// pins all source descriptors and argument/capture tapes.
pub(super) unsafe extern "C" fn observe(
    kind: u32,
    owner: u32,
    operation: *const solve::SolveSpannedOperation,
    bits: *const u64,
    width: usize,
    ordinal: i64,
) -> u8 {
    ACTIVE.with(|active| {
        let pointer = active.get();
        if pointer.is_null() {
            return 0;
        }
        // SAFETY: execute's guard pins the exclusive carrier until generated code returns.
        let carrier = unsafe { &mut *pointer };
        // SAFETY: emitted code passes a pinned source operation or a null root marker.
        let operation = unsafe { operation.as_ref() };
        let bits = if width == 0 {
            &[]
        } else {
            // SAFETY: the native compiler proves this initialized tape's exact width.
            unsafe { std::slice::from_raw_parts(bits, width) }
        };
        if carrier.event(kind, owner, operation, bits, ordinal).is_ok() {
            0
        } else {
            super::scratch::FAILURE
        }
    })
}

pub(super) fn emit(
    module: &mut cranelift_jit::JITModule,
    builder: &mut cranelift_frontend::FunctionBuilder<'_>,
    kind: u32,
    owner: u32,
    operation: Option<&solve::SolveSpannedOperation>,
    tape: (cranelift_codegen::ir::Value, usize),
    ordinal: Option<cranelift_codegen::ir::Value>,
) -> Result<(), CompileError> {
    use cranelift_codegen::ir::{AbiParam, InstBuilder, types};
    use cranelift_module::Module;
    let (bits, width) = tape;
    let pointer = module.target_config().pointer_type();
    let mut signature = module.make_signature();
    signature.params.extend(
        [
            types::I32,
            types::I32,
            pointer,
            pointer,
            pointer,
            types::I64,
        ]
        .map(AbiParam::new),
    );
    signature.returns.push(AbiParam::new(types::I8));
    let id = module
        .declare_function(
            "rumoca_native_observe",
            cranelift_module::Linkage::Import,
            &signature,
        )
        .map_err(super::to_backend_err)?;
    let function = super::owned_jit_module::declare_far_call_in_func(module, id, builder.func);
    let kind = builder.ins().iconst(types::I32, i64::from(kind));
    let owner = builder.ins().iconst(types::I32, i64::from(owner));
    let address = operation.map_or(0, |operation| operation as *const _ as usize);
    let operation = builder.ins().iconst(pointer, address as i64);
    let width = builder.ins().iconst(
        pointer,
        i64::try_from(width).map_err(|_| error("native observation width exceeds i64"))?,
    );
    let ordinal = ordinal.unwrap_or_else(|| builder.ins().iconst(types::I64, 0));
    let call = builder
        .ins()
        .call(function, &[kind, owner, operation, bits, width, ordinal]);
    let status = builder.inst_results(call)[0];
    super::status::propagate(builder, status);
    Ok(())
}

fn scalar_region<'program>(
    mut program: &'program [solve::LinearOp],
    path: &[(usize, usize)],
) -> Option<&'program [solve::LinearOp]> {
    for &(operation, region) in path {
        program = match program.get(operation)? {
            solve::LinearOp::FunctionConditional { program, .. } => {
                conditional_region(program, region)?
            }
            solve::LinearOp::FunctionFold { program, .. }
            | solve::LinearOp::GuardedFunctionFold { program, .. }
            | solve::LinearOp::StoreOutputFunctionFold { program, .. } => {
                program.regions().nth(region)?
            }
            _ => return None,
        };
    }
    Some(program)
}

fn scalar_bound(
    source: &solve::RetainedEventProgram,
    program: &[solve::LinearOp],
    path: &mut Vec<(usize, usize)>,
) -> Result<usize, CompileError> {
    program
        .iter()
        .enumerate()
        .try_fold(0usize, |sum, (index, op)| {
            let mut region = |ordinal, child: &[solve::LinearOp]| {
                path.push((index, ordinal));
                let bound = scalar_bound(source, child, path);
                path.pop();
                bound
            };
            let count = match op {
                solve::LinearOp::PureCallObservation { .. } => {
                    let invocation = source
                        .invocation_at(path, index)
                        .ok_or_else(|| error("missing issued observation"))?;
                    program_bound(source.table(), invocation.owner().body(), &mut Vec::new())?
                }
                solve::LinearOp::FunctionConditional { program, .. } => {
                    let mut remaining = region(program.arms.len() * 2, &program.fallback)?;
                    for (arm, body) in program.arms.iter().enumerate().rev() {
                        let condition = region(2 * arm, &body.condition)?;
                        let result = region(2 * arm + 1, &body.result)?;
                        remaining = condition
                            .checked_add(result.max(remaining))
                            .ok_or_else(|| error("observation bound overflows"))?;
                    }
                    remaining
                }
                solve::LinearOp::FunctionFold { program, .. }
                | solve::LinearOp::GuardedFunctionFold { program, .. }
                | solve::LinearOp::StoreOutputFunctionFold { program, .. } => {
                    let body = region(0, &program.update)?;
                    let condition = program
                        .continuation
                        .as_ref()
                        .map(|body| region(1, &body.ops))
                        .transpose()?
                        .unwrap_or(0);
                    let passes = program
                        .domain
                        .scalar_count()
                        .map_err(|_| error("invalid scalar observation domain"))?;
                    // Scalar continuations may also run once after the final transition.
                    let transitions = passes
                        .checked_mul(body)
                        .ok_or_else(|| error("observation bound overflows"))?;
                    let tests = passes
                        .checked_add(1)
                        .and_then(|count| count.checked_mul(condition))
                        .ok_or_else(|| error("observation bound overflows"))?;
                    transitions
                        .checked_add(tests)
                        .ok_or_else(|| error("observation bound overflows"))?
                }
                _ => 0,
            };
            sum.checked_add(count)
                .ok_or_else(|| error("observation bound overflows"))
        })
}

fn conditional_region(
    program: &solve::FunctionConditionalProgram,
    region: usize,
) -> Option<&[solve::LinearOp]> {
    if region == program.arms.len() * 2 {
        return Some(&program.fallback);
    }
    let arm = program.arms.get(region / 2)?;
    Some(if region.is_multiple_of(2) {
        &arm.condition
    } else {
        &arm.result
    })
}
