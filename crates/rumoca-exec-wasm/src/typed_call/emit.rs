use super::layout::{CellRange, FramePlan, Home, LocalCounts, PointerCell};
use super::link::LinkedOwners;
use super::scalars::{FIRST_PROMOTED_LOCAL, cell_load, cell_store, locals_header};
use super::{TypedCallCompileError, TypedCallFault, TypedCallFaultKind};
use crate::emit::MathImport;
use crate::outline::Outliner;
use rumoca_ir_solve as solve;
use wasm_encoder::{
    BlockType, CodeSection, EntityType, ExportKind, ExportSection, Function, FunctionSection,
    ImportSection, Instruction as I, MemArg, MemoryType, Module, TypeSection, ValType,
};

pub(super) const CELL: MemArg = MemArg {
    offset: 0,
    align: 3,
    memory_index: 0,
};

pub(super) struct Emitter<'a> {
    pub code: Vec<I<'static>>,
    /// The whole allocation of each address push whose range the code may
    /// reach beyond one cell.
    pub spans: Vec<super::promote::Extent>,
    pub owner: &'a solve::SolvePureCallOwner,
    /// The unit of `linked` this function is built from.
    pub unit: usize,
    pub plan: &'a FramePlan,
    pub program: &'a solve::TypedProgram,
    pub region_path: Vec<(usize, usize)>,
    pub loop_counters: Vec<CellRange>,
    /// Cell of each assigned state of the body being emitted: its own cell, or
    /// the input cell an `Input` operation aliases.
    pub state_cells: Vec<CellRange>,
    /// Loops of this function being emitted, innermost last: each holds its pass
    /// counter in an `i64` local above the promoted ones.
    pub loop_locals: u32,
    /// First local reserved for loop counters.
    pub loop_local_base: u32,
    /// Whether the owner records observations, which read loop counter cells.
    pub observed: bool,
    /// What the next region body emitted runs under: set by a loop emitter just
    /// before it, taken by `region_body`.
    pub pending_loop: Option<super::control::RegionLoop>,
    /// The loop whose pass the body being emitted is, if any.
    pub region_loop: Option<super::control::RegionLoop>,
    /// The result slots the loop body just emitted stored straight into their
    /// final cell, which need no copy after the pass.
    pub written_outputs: Vec<usize>,
    /// The known Integer or Boolean value the region body just emitted stored to
    /// its Output slot, when the emitter knows it.
    pub region_result: Option<i64>,
    /// The Integer or Boolean value each register of the body being emitted
    /// holds on every execution of the code emitted for it, when known.
    pub known: Vec<Option<i64>>,
    /// The known value of each input slot of the body being emitted.
    pub slot_facts: super::known::Facts,
    /// The known input slot values the next region body emitted starts with.
    pub pending_known: Option<super::known::Facts>,
    /// The cells of each aggregate register of the body being emitted, when
    /// every one is known.
    pub known_cells: Vec<Option<super::known::Cells>>,
    /// The bounds each Integer register of the body being emitted stays
    /// within: the planned ones, refined by what the emitter knows.
    pub intervals: Vec<Option<super::layout::Interval>>,
    pub linked: &'a LinkedOwners,
    /// Promoted locals of the enclosing bodies, below this body's own.
    pub local_floor: LocalCounts,
    /// Promoted locals the whole function declares.
    pub local_total: LocalCounts,
    fault_offset: u32,
    faults: Vec<TypedCallFault>,
}

pub(super) fn compile(
    table: &solve::SolvePureCallTable,
    linked: &LinkedOwners,
    root: solve::SolvePureCallOwnerId,
) -> Result<(Vec<u8>, Vec<TypedCallFault>), TypedCallCompileError> {
    let mut functions = Vec::new();
    let mut faults = Vec::new();
    for owner in table.owners() {
        if linked.functions[owner.id().index() as usize].is_none() {
            continue;
        }
        let (function, mut owner_faults) = compile_owner(owner, linked, faults.len())?;
        functions.push(function);
        faults.append(&mut owner_faults);
    }
    let exported =
        linked.functions[root.index() as usize].ok_or(TypedCallCompileError::SiteMismatch)?;
    Ok((module(functions, exported, &linked.math_imports), faults))
}

/// Preconditions the native call program establishes once, at construction,
/// for every helper it links. Only [`ProgramHelpers`](super::program::ProgramHelpers)
/// holds this proof, so an internal helper never re-checks them at run time:
/// the three buffers are disjoint, eight-byte aligned spans inside the admitted
/// layout (`CallProgramLayout`, checked once at entry by `guard_call_spans`),
/// and every packed Integer or Boolean input cell is already in its domain
/// (`pack_cell`/`pack_integer`, or an exact callee result).
#[derive(Clone, Copy)]
pub(super) struct CallProgramFacts(());

impl CallProgramFacts {
    pub(super) const fn established_by_call_program_layout() -> Self {
        Self(())
    }
}

/// Where an internal helper's functions may be cut and numbered.
#[derive(Clone, Copy)]
pub(super) struct Outlining {
    pub budget: usize,
    /// Function index of the first continuation this helper may issue.
    pub first_part: u32,
}

/// One internal helper: its callable entry plus the continuations it calls.
pub(super) struct HelperBody {
    pub entry: Function,
    pub parts: Vec<Function>,
    pub faults: Vec<TypedCallFault>,
}

/// The guarded, host-facing export: validates its own buffers and inputs.
pub(super) fn compile_owner(
    owner: &solve::SolvePureCallOwner,
    linked: &LinkedOwners,
    fault_offset: usize,
) -> Result<(Function, Vec<TypedCallFault>), TypedCallCompileError> {
    let unit = owner.id().index() as usize;
    let mut emitter = Emitter::new(
        owner,
        unit,
        linked,
        linked.unit_emit_plan(unit)?,
        fault_offset,
    )?;
    let buffers = emitter.fault(
        None,
        "abi",
        TypedCallFaultKind::InvalidBuffer,
        owner.provenance(),
    );
    emitter.guard_buffers(buffers);
    emitter.reset_observations();
    emitter.validate_inputs();
    emitter.emit_body()?;
    emitter.publish_outputs();
    let body = emitter.take_chunk();
    let function = emitter.assemble(&[&body]);
    Ok((function, emitter.faults))
}

/// A helper of the native call program. Its caller proved the buffers and
/// input domains, so none are re-checked; the operation list is cut at
/// operation boundaries into continuations when it exceeds the byte budget.
pub(super) fn compile_helper(
    (owner, unit): (&solve::SolvePureCallOwner, usize),
    linked: &LinkedOwners,
    fault_offset: usize,
    facts: CallProgramFacts,
    outlining: Outlining,
) -> Result<HelperBody, TypedCallCompileError> {
    // A body that fits one function keeps its top-level scalars in locals. A
    // body that must be outlined shares its top-level registers between
    // continuations through memory, so it is emitted from the plain plan.
    let promoted = linked.unit_emit_plan(unit)?;
    let plain = linked.unit_plan(unit)?;
    let helper = compile_helper_with(
        (owner, unit),
        linked,
        promoted,
        fault_offset,
        (facts, outlining),
    )?;
    if !helper.parts.is_empty() && !std::ptr::eq(promoted, plain) {
        return compile_helper_with(
            (owner, unit),
            linked,
            plain,
            fault_offset,
            (facts, outlining),
        );
    }
    Ok(helper)
}

fn compile_helper_with(
    (owner, unit): (&solve::SolvePureCallOwner, usize),
    linked: &LinkedOwners,
    plan: &FramePlan,
    fault_offset: usize,
    (_facts, outlining): (CallProgramFacts, Outlining),
) -> Result<HelperBody, TypedCallCompileError> {
    let mut emitter = Emitter::new(owner, unit, linked, plan, fault_offset)?;
    let header = locals_header(emitter.local_total);
    let mut outliner = Outliner::new(header.clone(), outlining.budget);
    let prologue = emitter.encode(|e| {
        e.reset_observations();
        Ok(())
    })?;
    let mut chunks = Vec::new();
    for (index, operation) in linked.form.body(owner)?.operations().iter().enumerate() {
        chunks.push(emitter.encode(|e| e.operation(index, operation))?);
    }
    let epilogue = emitter.encode(|e| {
        e.publish_outputs();
        Ok(())
    })?;
    let whole = std::iter::once(&prologue)
        .chain(&chunks)
        .chain(std::iter::once(&epilogue))
        .collect::<Vec<_>>();
    let inline = emitter.assemble(&whole);
    if inline.byte_len() <= outlining.budget {
        return Ok(HelperBody {
            entry: inline,
            parts: Vec::new(),
            faults: emitter.faults,
        });
    }
    for chunk in &chunks {
        let bytes = outliner.instructions(chunk.function(&header));
        outliner
            .push(&bytes)
            .map_err(|too_large| emitter.over_budget(too_large.bytes, outlining.budget))?;
    }
    let parts = outliner.finish();
    let entry = emitter.continuation_entry((&prologue, &epilogue), &parts, outlining)?;
    Ok(HelperBody {
        entry,
        parts,
        faults: emitter.faults,
    })
}

impl<'a> Emitter<'a> {
    fn new(
        owner: &'a solve::SolvePureCallOwner,
        unit: usize,
        linked: &'a LinkedOwners,
        plan: &'a FramePlan,
        fault_offset: usize,
    ) -> Result<Self, TypedCallCompileError> {
        let program = linked.form.body(owner)?;
        // Include check/capacity faults and each issued forwarding descriptor.
        // Return codes must remain positive i32 values rather than wrap at export.
        if operation_budget(program)
            .and_then(|count| {
                count
                    .checked_mul(4)?
                    .checked_add(1)?
                    .checked_add(fault_offset)
            })
            .and_then(|count| {
                count.checked_add(
                    plan.observations
                        .as_ref()
                        .map_or(0, |plan| plan.forwarding.len()),
                )
            })
            .is_none_or(|count| count > i32::MAX as usize)
        {
            return Err(TypedCallCompileError::SizeLimit);
        }
        let mut total = plan.demand;
        total.integers = total
            .integers
            .checked_add(super::control::max_loop_locals(program))
            .ok_or(TypedCallCompileError::SizeLimit)?;
        Ok(Self {
            code: Vec::new(),
            spans: Vec::new(),
            owner,
            unit,
            plan,
            program,
            region_path: Vec::new(),
            loop_counters: Vec::new(),
            state_cells: plan.assigned_states.clone(),
            loop_locals: 0,
            loop_local_base: FIRST_PROMOTED_LOCAL + plan.demand.integers,
            observed: plan.observations.is_some(),
            pending_loop: None,
            region_loop: None,
            written_outputs: Vec::new(),
            region_result: None,
            known: vec![None; program.register_types().len()],
            slot_facts: super::known::Facts::default(),
            known_cells: vec![None; program.register_types().len()],
            intervals: plan.intervals.clone(),
            pending_known: None,
            linked,
            local_floor: LocalCounts::default(),
            local_total: total,
            fault_offset: fault_offset as u32,
            faults: Vec::new(),
        })
    }

    /// No external output writes precede this success-only publication point.
    fn publish_outputs(&mut self) {
        let bytes = self.plan.output_bytes;
        if !self.plan.outputs_in_scratch {
            self.copy(
                CellRange {
                    base: 1,
                    offset: 0,
                    bytes,
                },
                CellRange {
                    base: 2,
                    offset: 0,
                    bytes,
                },
            );
        }
        self.push(I::I32Const(0));
        self.push(I::End);
    }

    /// The instructions emitted since the last call.
    pub(super) fn take_chunk(&mut self) -> Chunk {
        Chunk {
            code: std::mem::take(&mut self.code),
            spans: std::mem::take(&mut self.spans),
        }
    }

    /// Emit one step into its own chunk, leaving the code so far untouched.
    fn encode(
        &mut self,
        step: impl FnOnce(&mut Self) -> Result<(), TypedCallCompileError>,
    ) -> Result<Chunk, TypedCallCompileError> {
        let saved = self.take_chunk();
        let result = step(self);
        let emitted = self.take_chunk();
        self.code = saved.code;
        self.spans = saved.spans;
        result?;
        Ok(emitted)
    }

    /// One function from consecutive chunks, with the scratch cells that only
    /// static addresses reach held in locals (SOLVE-C99).
    pub(super) fn assemble(&self, chunks: &[&Chunk]) -> Function {
        let header = locals_header(self.local_total);
        let mut code = Vec::new();
        let mut spans = Vec::new();
        for chunk in chunks {
            spans.extend(chunk.spans.iter().map(|extent| extent.shifted(code.len())));
            code.extend(chunk.code.iter().cloned());
        }
        let body = super::promote::Body { code, spans };
        match self
            .promotion_frame()
            .and_then(|frame| super::promote::promote(&body, &frame))
        {
            Some(promoted) => {
                let locals = header
                    .iter()
                    .chain(&promoted.locals)
                    .copied()
                    .collect::<Vec<_>>();
                Chunk::function_with(&locals, &promoted.code)
            }
            None => Chunk::function_with(&header, &body.code),
        }
    }

    fn over_budget(&self, bytes: usize, budget: usize) -> TypedCallCompileError {
        TypedCallCompileError::FunctionBudgetExceeded {
            owner: self.owner.id(),
            provenance: self.owner.provenance(),
            bytes,
            operations: self.program.operations().len(),
            budget,
            over_budget_owners: 1,
        }
    }

    /// The callable entry of an outlined helper: it runs each continuation on
    /// the same frame and returns the first non-zero status.
    fn continuation_entry(
        &mut self,
        (prologue, epilogue): (&Chunk, &Chunk),
        parts: &[Function],
        outlining: Outlining,
    ) -> Result<Function, TypedCallCompileError> {
        let mut code = prologue.code.clone();
        for index in 0..parts.len() {
            let target = u32::try_from(index)
                .ok()
                .and_then(|n| outlining.first_part.checked_add(n))
                .ok_or(TypedCallCompileError::SizeLimit)?;
            code.extend((0..3).map(I::LocalGet));
            code.extend([
                I::Call(target),
                I::LocalTee(3),
                I::If(BlockType::Empty),
                I::LocalGet(3),
                I::Return,
                I::End,
            ]);
        }
        code.extend(epilogue.code.iter().cloned());
        let entry = Chunk::function_with(&locals_header(self.local_total), &code);
        if entry.byte_len() > outlining.budget {
            return Err(self.over_budget(entry.byte_len(), outlining.budget));
        }
        Ok(entry)
    }
}

fn operation_budget(program: &solve::TypedProgram) -> Option<usize> {
    let mut count = program
        .operations()
        .len()
        .checked_add(program.slots().len())?;
    for operation in program.operations() {
        match operation.operation() {
            solve::SolveOperation::CheckAssertion {
                message: solve::SolveAssertionMessage::Captures { program },
                ..
            } => count = count.checked_add(operation_budget(program.body())?)?,
            solve::SolveOperation::AssignedConditional { program } => {
                count = count.checked_add(operation_budget(program.if_true().body())?)?;
                count = count.checked_add(operation_budget(program.if_false().body())?)?;
            }
            solve::SolveOperation::Conditional {
                if_true, if_false, ..
            } => {
                count = count.checked_add(operation_budget(if_true.body())?)?;
                count = count.checked_add(operation_budget(if_false.body())?)?;
            }
            solve::SolveOperation::Map { body, .. } => {
                count = count.checked_add(operation_budget(body.body())?)?
            }
            solve::SolveOperation::Fold {
                transition,
                continuation,
                ..
            } => {
                count = count.checked_add(operation_budget(transition.body())?)?;
                if let Some(predicate) = continuation {
                    count = count.checked_add(operation_budget(predicate.body())?)?;
                }
            }
            _ => {}
        }
    }
    Some(count)
}

fn module(bodies: Vec<Function>, exported: u32, math_imports: &[MathImport]) -> Vec<u8> {
    let mut module = Module::new();
    let mut types = TypeSection::new();
    types.ty().function([ValType::I32; 3], [ValType::I32]);
    types.ty().function([ValType::F64], [ValType::F64]);
    types.ty().function([ValType::F64; 2], [ValType::F64]);
    module.section(&types);
    let mut imports = ImportSection::new();
    for import in math_imports {
        imports.import(
            "env",
            import.symbol(),
            EntityType::Function(if import.is_binary() { 2 } else { 1 }),
        );
    }
    imports.import(
        "env",
        "memory",
        EntityType::Memory(MemoryType {
            minimum: 1,
            maximum: None,
            memory64: false,
            shared: false,
            page_size_log2: None,
        }),
    );
    module.section(&imports);
    let mut functions = FunctionSection::new();
    for _ in &bodies {
        functions.function(0);
    }
    module.section(&functions);
    let mut exports = ExportSection::new();
    exports.export("eval_typed_call", ExportKind::Func, exported);
    module.section(&exports);
    let mut code = CodeSection::new();
    for function in bodies {
        code.function(&function);
    }
    module.section(&code);
    module.finish()
}

impl<'a> Emitter<'a> {
    pub(super) fn emit_body(&mut self) -> Result<(), TypedCallCompileError> {
        for (index, operation) in self.program.operations().iter().enumerate() {
            self.operation(index, operation)?;
        }
        Ok(())
    }
    pub(super) fn push(&mut self, instruction: I<'static>) {
        self.code.push(instruction);
    }

    pub(super) fn reg(&self, r: solve::SolveRegisterId) -> CellRange {
        self.plan.registers[r.index()]
    }

    pub(super) fn scalar(&self, r: solve::SolveRegisterId) -> solve::SolveScalarType {
        self.program.register_types()[r.index()].element_type()
    }

    pub(super) fn address(&mut self, range: CellRange) {
        if let Some(cell) = range.pointer_cell() {
            // The range is reached by reference: its address is in an input
            // tuple cell (a caller's argument) or a scratch cell (a conditional
            // result that passes a parent value through).
            let (frame, cell) = match cell {
                PointerCell::Input(cell) => (0, cell),
                PointerCell::Scratch(cell) => (2, cell),
            };
            self.push(I::LocalGet(frame));
            self.push(I::I32Const(cell as i32));
            self.push(I::I32Add);
            self.push(I::I32Load(MemArg {
                offset: 0,
                align: 2,
                memory_index: 0,
            }));
        } else {
            self.push(I::LocalGet(range.base));
        }
        self.push(I::I32Const(range.offset as i32));
        self.push(I::I32Add);
    }

    /// Push the address of `range` when the code goes on to reach any cell of
    /// it, not only the first.
    pub(super) fn address_span(&mut self, range: CellRange) {
        self.spans
            .push(super::promote::Extent::of(self.code.len(), range));
        self.address(range);
    }

    pub(super) fn cell_address(&mut self, range: CellRange) {
        self.address_span(range);
        self.push(I::LocalGet(3));
        self.push(I::I32Const(8));
        self.push(I::I32Mul);
        self.push(I::I32Add);
    }

    pub(super) fn load_cell(&mut self, range: CellRange, real: bool) {
        self.cell_address(range);
        self.push(if real {
            I::F64Load(CELL)
        } else {
            I::I64Load(CELL)
        });
    }

    pub(super) fn copy(&mut self, destination: CellRange, source: CellRange) {
        // A zero-size destination is a value no running operation reads.
        if destination == source || destination.bytes == 0 {
            return;
        }
        self.address(destination);
        self.address(source);
        self.copy_addresses(destination.bytes);
    }

    /// Consume destination and source addresses already on the operand stack.
    pub(super) fn copy_addresses(&mut self, bytes: u32) {
        if bytes == 8 {
            // Read all bits before writing: also preserves overlapping cell copies.
            self.push(I::I64Load(CELL));
            self.push(I::I64Store(CELL));
            return;
        }
        self.push(I::I32Const(bytes as i32));
        self.push(I::MemoryCopy {
            src_mem: 0,
            dst_mem: 0,
        });
    }

    pub(super) fn cells(&mut self, count: u32, body: impl FnOnce(&mut Self)) {
        self.cells_from(0, count, body);
    }

    /// Run `body` once per cell index `start..count` held in local 3.
    pub(super) fn cells_from(&mut self, start: u32, count: u32, body: impl FnOnce(&mut Self)) {
        self.push(I::I32Const(start as i32));
        self.push(I::LocalSet(3));
        if count == start + 1 {
            body(self);
            self.push(I::I32Const(count as i32));
            self.push(I::LocalSet(3));
            return;
        }
        self.push(I::Block(BlockType::Empty));
        self.push(I::Loop(BlockType::Empty));
        self.push(I::LocalGet(3));
        self.push(I::I32Const(count as i32));
        self.push(I::I32GeU);
        self.push(I::BrIf(1));
        body(self);
        self.push(I::LocalGet(3));
        self.push(I::I32Const(1));
        self.push(I::I32Add);
        self.push(I::LocalSet(3));
        self.push(I::Br(0));
        self.push(I::End);
        self.push(I::End);
    }

    pub(super) fn fail_if(&mut self, status: u32) {
        self.push(I::If(BlockType::Empty));
        self.push(I::I32Const(status as i32));
        self.push(I::Return);
        self.push(I::End);
    }

    pub(super) fn fault(
        &mut self,
        operation: Option<usize>,
        opcode: &'static str,
        kind: TypedCallFaultKind,
        provenance: rumoca_core::Span,
    ) -> u32 {
        let status = self.fault_offset + self.faults.len() as u32 + 1;
        self.faults.push(TypedCallFault {
            status,
            owner: self.owner.id(),
            operation,
            region_path: self.region_path.clone(),
            opcode,
            kind,
            provenance,
        });
        status
    }

    pub(super) fn unsupported(
        &self,
        operation: usize,
        opcode: &'static str,
        provenance: rumoca_core::Span,
    ) -> TypedCallCompileError {
        TypedCallCompileError::UnsupportedOperation {
            owner: self.owner.id(),
            operation,
            region_path: self.region_path.clone(),
            opcode,
            provenance,
        }
    }

    fn guard_buffers(&mut self, status: u32) {
        let sizes = [
            self.plan.input_bytes,
            self.plan.output_bytes,
            self.plan.scratch_bytes,
        ];
        for (pointer, bytes) in sizes.into_iter().enumerate() {
            self.push(I::LocalGet(pointer as u32));
            self.push(I::I32Const(7));
            self.push(I::I32And);
            self.fail_if(status);
            self.end_address(pointer as u32, bytes);
            self.push(I::MemorySize(0));
            self.push(I::I64ExtendI32U);
            self.push(I::I64Const(65536));
            self.push(I::I64Mul);
            self.push(I::I64GtU);
            self.fail_if(status);
        }
        for (a, b) in [(0usize, 1usize), (0, 2), (1, 2)] {
            if sizes[a] == 0 || sizes[b] == 0 {
                continue;
            }
            self.end_address(a as u32, sizes[a]);
            self.push(I::LocalGet(b as u32));
            self.push(I::I64ExtendI32U);
            self.push(I::I64GtU);
            self.end_address(b as u32, sizes[b]);
            self.push(I::LocalGet(a as u32));
            self.push(I::I64ExtendI32U);
            self.push(I::I64GtU);
            self.push(I::I32And);
            self.fail_if(status);
        }
    }

    fn end_address(&mut self, pointer: u32, bytes: u32) {
        self.push(I::LocalGet(pointer));
        self.push(I::I64ExtendI32U);
        self.push(I::I64Const(i64::from(bytes)));
        self.push(I::I64Add);
    }

    fn validate_inputs(&mut self) {
        for slot in self.program.slots() {
            if slot.storage() != solve::SolveStorageClass::Input {
                continue;
            }
            let scalar = slot.value_type().element_type();
            if matches!(scalar, solve::SolveScalarType::Real { .. }) {
                continue;
            }
            let status = self.fault(
                None,
                "input",
                TypedCallFaultKind::InvalidInput,
                slot.provenance(),
            );
            let range = self.plan.slots[slot.id().index()];
            self.cells(range.bytes / 8, |e| {
                e.load_cell(range, false);
                e.push(I::LocalSet(7));
                e.validate_input_cell(scalar, status);
            });
        }
    }

    fn validate_input_cell(&mut self, scalar: solve::SolveScalarType, status: u32) {
        match scalar {
            solve::SolveScalarType::Boolean => {
                self.push(I::LocalGet(7));
                self.push(I::I64Const(1));
                self.push(I::I64GtU);
                self.fail_if(status);
            }
            solve::SolveScalarType::Integer(domain) => self.integer_domain(domain, status),
            _ => unreachable!("Real input handled before cell validation"),
        }
    }

    fn operation(
        &mut self,
        index: usize,
        spanned: &'a solve::SolveSpannedOperation,
    ) -> Result<(), TypedCallCompileError> {
        use solve::SolveOperation as O;
        if self.plan.liveness.is_dead(index) {
            return Ok(());
        }
        let op = spanned.operation();
        let mut unused = false;
        op.visit_output_registers(|register| {
            unused |= matches!(self.home(register), Home::Unused | Home::Immediate { .. });
        });
        if unused {
            return Ok(());
        }
        if let Some(value) = self.fold_known(op) {
            let mut destination = None;
            op.visit_output_registers(|register| destination = Some(register));
            if let Some(register) = destination {
                self.define_known(register, value);
                return Ok(());
            }
        }
        if self.fold_projection(op) {
            return Ok(());
        }
        self.record_cells(op);
        self.refine_interval(op);
        match op {
            O::AssignedState { operation } => self.assigned_operation(operation),
            O::AssignedConditional { program } => self.assigned_conditional(index, program),
            O::CheckAssertion { .. } => self.assertion_operation(index, spanned),
            O::Map { .. } => self.map_operation(index, spanned),
            O::Conditional { .. } | O::Fold { .. } => self.control_operation(index, spanned),
            O::Unary { .. } | O::Binary { .. } | O::Compare { .. } | O::Convert { .. } => {
                self.number_operation(index, spanned)
            }
            O::BroadcastBinary { .. } => self.broadcast_operation(index, spanned),
            O::Transpose { .. }
            | O::ProjectView { .. }
            | O::ProjectSlice { .. }
            | O::UpdateView { .. }
            | O::UpdateSlice { .. } => self.view_operation(index, spanned),
            O::Scale { .. } | O::Identity { .. } => self.tensor_operation(index, spanned),
            O::MatrixMultiply { .. } => self.matrix_operation(index, spanned),
            O::Call { .. } => self.call_operation(index, spanned),
            O::Cross { .. }
            | O::Reduce { .. }
            | O::Diagonal { .. }
            | O::Concatenate { .. }
            | O::SelectElement { .. } => self.aggregate_operation(index, spanned),
            O::ProjectElementDynamic { .. } | O::UpdateElement { .. } => {
                self.dynamic_operation(index, spanned);
                Ok(())
            }
            O::Constant { destination, value } => {
                let bits = match value.kind() {
                    solve::SolveValueKind::Real64(bits) => bits as i64,
                    solve::SolveValueKind::Integer(value) => value,
                    solve::SolveValueKind::Boolean(value) => i64::from(value),
                    solve::SolveValueKind::Real32(_) => {
                        return Err(TypedCallCompileError::UnsupportedArithmetic);
                    }
                };
                if !matches!(self.home(*destination), Home::Immediate { .. }) {
                    self.begin_result(*destination);
                    self.push_constant(bits, self.is_real(*destination));
                    self.end_result(*destination);
                }
                if !self.is_real(*destination) {
                    self.known[destination.index()] = Some(bits);
                }
                Ok(())
            }
            O::Select {
                destination,
                condition,
                if_true,
                if_false,
            } => {
                if self.program.register_types()[destination.index()].scalar_count() == 1 {
                    self.scalar_select(*destination, *condition, *if_true, *if_false);
                } else {
                    self.selected_copy(*destination, *condition, *if_true, *if_false);
                }
                Ok(())
            }
            _ => self.data_operation(index, spanned),
        }
    }

    fn data_operation(
        &mut self,
        index: usize,
        spanned: &solve::SolveSpannedOperation,
    ) -> Result<(), TypedCallCompileError> {
        use solve::SolveOperation as O;
        match spanned.operation() {
            O::Load { destination, slot } => {
                if self.binder_load(*destination, slot.index()) {
                } else if let Some(value) = self.known_load(*destination, slot.index()) {
                    self.define_known(*destination, value);
                } else if self.home(*destination) == Home::Memory {
                    self.copy(self.reg(*destination), self.plan.slots[slot.index()]);
                } else {
                    self.begin_result(*destination);
                    self.address(self.plan.slots[slot.index()]);
                    self.push(cell_load(self.is_real(*destination)));
                    self.end_result(*destination);
                }
            }
            O::Store { slot, source } => {
                if !self.store_loop_output(slot.index(), *source) {
                    match self.plan.ref_outputs.get(&slot.index()) {
                        Some(output) => self.store_reference_output(*output, slot.index(), *source),
                        None => self.store_slot(self.plan.slots[slot.index()], *source),
                    }
                }
            }
            O::Fill { destination, value } => self.fill_aggregate(self.reg(*destination), *value),
            O::ConstructAggregate {
                destination,
                elements,
            } => {
                let dst = self.reg(*destination);
                let mut offset = dst.offset;
                for source in elements {
                    let source = self.reg(*source);
                    self.copy(
                        CellRange {
                            offset,
                            bytes: source.bytes,
                            ..dst
                        },
                        source,
                    );
                    offset += source.bytes;
                }
            }
            O::ProjectElement {
                destination,
                aggregate,
                indices,
            } => {
                let shape = self.program.register_types()[aggregate.index()].dimensions();
                let cell = indices
                    .iter()
                    .zip(shape)
                    .fold(0, |cell, (index, extent)| cell * extent + index);
                let source = self.reg(*aggregate);
                let element = CellRange {
                    offset: source.offset + cell * 8,
                    bytes: 8,
                    ..source
                };
                if self.home(*destination) == Home::Memory {
                    self.copy(self.reg(*destination), element);
                } else {
                    self.begin_result(*destination);
                    self.address(element);
                    self.push(cell_load(self.is_real(*destination)));
                    self.end_result(*destination);
                }
            }
            _ => {
                return Err(self.unsupported(
                    index,
                    "unimplemented typed operation",
                    spanned.provenance(),
                ));
            }
        }
        Ok(())
    }

    /// A single-cell selection: both values exist, so one `select` picks.
    fn scalar_select(
        &mut self,
        destination: solve::SolveRegisterId,
        condition: solve::SolveRegisterId,
        if_true: solve::SolveRegisterId,
        if_false: solve::SolveRegisterId,
    ) {
        self.begin_result(destination);
        self.push_scalar(if_true);
        self.push_scalar(if_false);
        self.push_scalar(condition);
        self.push(I::I64Eqz);
        self.push(I::I32Eqz);
        self.push(I::Select);
        self.end_result(destination);
    }

    fn selected_copy(
        &mut self,
        destination: solve::SolveRegisterId,
        condition: solve::SolveRegisterId,
        if_true: solve::SolveRegisterId,
        if_false: solve::SolveRegisterId,
    ) {
        self.address(self.reg(condition));
        self.push(I::I64Load(CELL));
        self.push(I::I64Eqz);
        self.push(I::I32Eqz);
        self.push(I::If(BlockType::Empty));
        self.copy(self.reg(destination), self.reg(if_true));
        self.push(I::Else);
        self.copy(self.reg(destination), self.reg(if_false));
        self.push(I::End);
    }

    fn dynamic_operation(&mut self, index: usize, spanned: &solve::SolveSpannedOperation) {
        use solve::SolveOperation as O;
        let (dst, aggregate, indices, value) = match spanned.operation() {
            O::ProjectElementDynamic {
                destination,
                aggregate,
                indices,
            } => (*destination, *aggregate, indices, None),
            O::UpdateElement {
                destination,
                aggregate,
                value,
                indices,
            } => (*destination, *aggregate, indices, Some(*value)),
            _ => unreachable!("dynamic operation dispatch"),
        };
        if let Some(cell) = self.static_element(aggregate, indices) {
            let source = self.reg(aggregate);
            let element = |range: CellRange| CellRange {
                offset: range.offset + cell * 8,
                bytes: 8,
                ..range
            };
            if let Some(value) = value {
                self.copy(self.reg(dst), source);
                self.address(element(self.reg(dst)));
                self.push_scalar(value);
                self.push(cell_store(self.is_real(value)));
            } else if self.home(dst) == Home::Memory {
                self.copy(self.reg(dst), element(source));
            } else {
                self.begin_result(dst);
                self.address(element(source));
                self.push(cell_load(self.is_real(dst)));
                self.end_result(dst);
            }
            return;
        }
        let opcode = if value.is_some() {
            "update_element"
        } else {
            "project_element_dynamic"
        };
        let status = self.fault(
            Some(index),
            opcode,
            TypedCallFaultKind::IndexBounds,
            spanned.provenance(),
        );
        self.dynamic_index(aggregate, indices, status);
        if let Some(value) = value {
            self.copy(self.reg(dst), self.reg(aggregate));
            self.indexed_address(self.reg(dst));
            self.push_scalar(value);
            self.push(cell_store(self.is_real(value)));
        } else if self.home(dst) == Home::Memory {
            self.address(self.reg(dst));
            self.indexed_address(self.reg(aggregate));
            self.push(I::I64Load(CELL));
            self.push(I::I64Store(CELL));
        } else {
            self.begin_result(dst);
            self.indexed_address(self.reg(aggregate));
            self.push(cell_load(self.is_real(dst)));
            self.end_result(dst);
        }
    }

    fn dynamic_index(
        &mut self,
        aggregate: solve::SolveRegisterId,
        indices: &[solve::SolveRegisterId],
        status: u32,
    ) {
        self.push(I::I64Const(0));
        self.push(I::LocalSet(4));
        for (index, extent) in indices
            .iter()
            .zip(self.program.register_types()[aggregate.index()].dimensions())
        {
            self.push_scalar(*index);
            self.push(I::LocalSet(5));
            // A bounded index needs no check.
            let in_range = self
                .interval(*index)
                .is_some_and(|(lowest, highest)| lowest >= 1 && highest <= i64::from(*extent));
            if !in_range {
                self.push(I::LocalGet(5));
                self.push(I::I64Const(1));
                self.push(I::I64LtS);
                self.fail_if(status);
                self.push(I::LocalGet(5));
                self.push(I::I64Const(i64::from(*extent)));
                self.push(I::I64GtS);
                self.fail_if(status);
            }
            self.push(I::LocalGet(4));
            self.push(I::I64Const(i64::from(*extent)));
            self.push(I::I64Mul);
            self.push(I::LocalGet(5));
            self.push(I::I64Const(1));
            self.push(I::I64Sub);
            self.push(I::I64Add);
            self.push(I::LocalSet(4));
        }
    }

    pub(super) fn indexed_address(&mut self, range: CellRange) {
        self.address_span(range);
        self.push(I::LocalGet(4));
        self.push(I::I64Const(8));
        self.push(I::I64Mul);
        self.push(I::I32WrapI64);
        self.push(I::I32Add);
    }
}

/// The instructions emitted for one step, and the allocations of the address
/// pushes among them whose range the code may reach beyond a single cell.
pub(super) struct Chunk {
    pub code: Vec<I<'static>>,
    pub spans: Vec<super::promote::Extent>,
}

impl Chunk {
    /// A function declaring `header` that runs this chunk.
    pub(super) fn function(&self, header: &[(u32, ValType)]) -> Function {
        Self::function_with(header, &self.code)
    }

    pub(super) fn function_with(locals: &[(u32, ValType)], code: &[I<'static>]) -> Function {
        let mut function = Function::new(locals.iter().copied());
        for instruction in code {
            function.instruction(instruction);
        }
        function
    }
}

impl Emitter<'_> {
    /// What the cell promotion of an assembled function needs to know of this
    /// body, or `None` for a body whose scratch the host reads as it fails.
    fn promotion_frame(&self) -> Option<super::promote::Frame> {
        if self.plan.observations.is_some() {
            return None;
        }
        Some(super::promote::Frame {
            scratch_bytes: self.plan.scratch_bytes,
            reserved_bytes: if self.plan.outputs_in_scratch {
                self.plan.output_bytes
            } else {
                0
            },
            locals: FIRST_PROMOTED_LOCAL + self.local_total.integers + self.local_total.reals,
            math_arity: self
                .linked
                .math_indices
                .iter()
                .map(|(import, index)| (*index, if import.is_binary() { 2 } else { 1 }))
                .collect(),
        })
    }
}

#[cfg(test)]
mod tests;
