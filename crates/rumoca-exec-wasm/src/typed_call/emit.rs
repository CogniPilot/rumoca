use super::layout::{CellRange, FramePlan};
use super::link::LinkedOwners;
use super::{TypedCallCompileError, TypedCallFault, TypedCallFaultKind};
use crate::emit::MathImport;
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
    pub function: Function,
    pub owner: &'a solve::SolvePureCallOwner,
    pub plan: &'a FramePlan,
    pub program: &'a solve::TypedProgram,
    pub region_path: Vec<(usize, usize)>,
    pub linked: &'a LinkedOwners,
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

pub(super) fn compile_owner(
    owner: &solve::SolvePureCallOwner,
    linked: &LinkedOwners,
    fault_offset: usize,
) -> Result<(Function, Vec<TypedCallFault>), TypedCallCompileError> {
    // At most one runtime fault record per operation/input slot, plus the ABI.
    // Return codes must remain positive i32 values rather than wrap at export.
    if operation_budget(owner.body())
        .and_then(|count| count.checked_add(1)?.checked_add(fault_offset))
        .is_none_or(|count| count > i32::MAX as usize)
    {
        return Err(TypedCallCompileError::SizeLimit);
    }
    let plan = linked.plan(owner.id())?;
    let mut emitter = Emitter {
        function: Function::new([(1, ValType::I32), (4, ValType::I64), (2, ValType::F64)]),
        owner,
        plan,
        program: owner.body(),
        region_path: Vec::new(),
        linked,
        fault_offset: fault_offset as u32,
        faults: Vec::new(),
    };
    let buffers = emitter.fault(
        None,
        "abi",
        TypedCallFaultKind::InvalidBuffer,
        owner.provenance(),
    );
    emitter.guard_buffers(buffers);
    emitter.validate_inputs();
    emitter.emit_body()?;
    // No external output writes precede this success-only publication point.
    emitter.copy(
        CellRange {
            base: 1,
            offset: 0,
            bytes: plan.output_bytes,
        },
        CellRange {
            base: 2,
            offset: 0,
            bytes: plan.output_bytes,
        },
    );
    emitter.push(I::I32Const(0));
    emitter.push(I::End);
    Ok((emitter.function, emitter.faults))
}

fn operation_budget(program: &solve::TypedProgram) -> Option<usize> {
    let mut count = program
        .operations()
        .len()
        .checked_add(program.slots().len())?;
    for operation in program.operations() {
        match operation.operation() {
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
    pub(super) fn push(&mut self, instruction: I<'_>) {
        self.function.instruction(&instruction);
    }

    pub(super) fn reg(&self, r: solve::SolveRegisterId) -> CellRange {
        self.plan.registers[r.index()]
    }

    pub(super) fn scalar(&self, r: solve::SolveRegisterId) -> solve::SolveScalarType {
        self.program.register_types()[r.index()].element_type()
    }

    pub(super) fn address(&mut self, range: CellRange) {
        self.push(I::LocalGet(range.base));
        self.push(I::I32Const(range.offset as i32));
        self.push(I::I32Add);
    }

    pub(super) fn cell_address(&mut self, range: CellRange) {
        self.address(range);
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
        if destination == source {
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
        let op = spanned.operation();
        match op {
            O::CheckAssertion { .. } => {
                Err(self.unsupported(index, "CheckAssertion", spanned.provenance()))
            }
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
            O::Call {
                assertion_forwarding,
                ..
            } if !assertion_forwarding.is_empty() => {
                Err(self.unsupported(index, "CallAssertionForwarding", spanned.provenance()))
            }
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
                self.address(self.reg(*destination));
                let bits = match value.kind() {
                    solve::SolveValueKind::Real64(bits) => bits as i64,
                    solve::SolveValueKind::Integer(value) => value,
                    solve::SolveValueKind::Boolean(value) => i64::from(value),
                    solve::SolveValueKind::Real32(_) => {
                        return Err(TypedCallCompileError::UnsupportedArithmetic);
                    }
                };
                self.push(I::I64Const(bits));
                self.push(I::I64Store(CELL));
                Ok(())
            }
            O::Select {
                destination,
                condition,
                if_true,
                if_false,
            } => {
                self.selected_copy(*destination, *condition, *if_true, *if_false);
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
                self.copy(self.reg(*destination), self.plan.slots[slot.index()])
            }
            O::Store { slot, source } => {
                self.copy(self.plan.slots[slot.index()], self.reg(*source))
            }
            O::Fill { destination, value } => {
                let (dst, src) = (self.reg(*destination), self.reg(*value));
                self.cells(dst.bytes / 8, |e| {
                    e.cell_address(dst);
                    e.address(src);
                    e.push(I::I64Load(CELL));
                    e.push(I::I64Store(CELL));
                });
            }
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
                self.copy(
                    self.reg(*destination),
                    CellRange {
                        offset: source.offset + cell * 8,
                        bytes: 8,
                        ..source
                    },
                );
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
            self.address(self.reg(value));
            self.push(I::I64Load(CELL));
            self.push(I::I64Store(CELL));
        } else {
            self.address(self.reg(dst));
            self.indexed_address(self.reg(aggregate));
            self.push(I::I64Load(CELL));
            self.push(I::I64Store(CELL));
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
            self.address(self.reg(*index));
            self.push(I::I64Load(CELL));
            self.push(I::LocalSet(5));
            self.push(I::LocalGet(5));
            self.push(I::I64Const(1));
            self.push(I::I64LtS);
            self.fail_if(status);
            self.push(I::LocalGet(5));
            self.push(I::I64Const(i64::from(*extent)));
            self.push(I::I64GtS);
            self.fail_if(status);
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
        self.address(range);
        self.push(I::LocalGet(4));
        self.push(I::I64Const(8));
        self.push(I::I64Mul);
        self.push(I::I32WrapI64);
        self.push(I::I32Add);
    }
}

#[cfg(test)]
mod tests;
