//! Native final-boundary lowering for the checked typed pure-call table.
//!
//! One compiler-issued owner becomes one machine-code helper. Aggregate
//! registers remain compact typed ranges in IR and are materialized as one
//! contiguous 64-bit-cell range only inside this execution adapter.

mod control;
mod storage;
mod tensor;

use super::host_runtime::register_math_symbols;
use super::owned_jit_module::OwnedJitModule;
use super::{
    CompileError, MathImports, emit_binary_op, emit_unary_op, finalize_jit_module, to_backend_err,
};
use cranelift_codegen::ir::condcodes::{FloatCC, IntCC};
use cranelift_codegen::ir::{
    AbiParam, InstBuilder, MemFlags, StackSlotData, StackSlotKind, Type, Value, types,
};
use cranelift_codegen::settings;
use cranelift_codegen::verify_function;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{FuncId, Linkage, Module};
use rumoca_ir_solve as solve;
use std::collections::HashMap;
use std::sync::atomic::{AtomicUsize, Ordering};
use storage::compact_register_tape;

const CELL_BYTES: usize = std::mem::size_of::<u64>();
static NEXT_TABLE_ID: AtomicUsize = AtomicUsize::new(0);

#[derive(Clone)]
pub(super) struct PureCallImport {
    pub(super) function: FuncId,
    pub(super) inputs: Box<[solve::SolveValueType]>,
    pub(super) outputs: Box<[solve::SolvePureCallOutput]>,
    pub(super) directional: Option<Box<PureCallDirectionalImport>>,
}

#[derive(Clone)]
pub(super) struct PureCallDirectionalImport {
    pub(super) function: FuncId,
    pub(super) inputs: Box<[solve::SolveValueType]>,
    pub(super) outputs: Box<[solve::SolvePureCallOutput]>,
}

struct PureCallSymbol {
    owner: solve::SolvePureCallOwnerId,
    name: String,
    address: *const u8,
    inputs: Box<[solve::SolveValueType]>,
    outputs: Box<[solve::SolvePureCallOutput]>,
}

/// Owns the code memory for one completely compiled checked call table.
pub(crate) struct CompiledPureCallTable {
    symbols: Box<[PureCallSymbol]>,
    directional_symbols: Box<[Option<PureCallSymbol>]>,
    _module: OwnedJitModule,
}

impl CompiledPureCallTable {
    pub(crate) fn compile(table: &solve::SolvePureCallTable) -> Result<Self, CompileError> {
        let table_id = NEXT_TABLE_ID.fetch_add(1, Ordering::Relaxed);
        let mut compiler = TableCompiler::new(table_id, table)?;
        compiler.compile_all(table)?;
        compiler.finish(table)
    }

    pub(super) fn register_symbols(&self, builder: &mut JITBuilder) {
        for symbol in &self.symbols {
            builder.symbol(&symbol.name, symbol.address);
        }
        for symbol in self.directional_symbols.iter().flatten() {
            builder.symbol(&symbol.name, symbol.address);
        }
    }

    pub(super) fn declare_imports(
        &self,
        module: &mut JITModule,
    ) -> Result<HashMap<solve::SolvePureCallOwnerId, PureCallImport>, CompileError> {
        let pointer_type = module.target_config().pointer_type();
        let mut signature = module.make_signature();
        signature.params.push(AbiParam::new(pointer_type));
        signature.params.push(AbiParam::new(pointer_type));
        let mut imports = HashMap::with_capacity(self.symbols.len());
        for symbol in &self.symbols {
            let function = module
                .declare_function(&symbol.name, Linkage::Import, &signature)
                .map_err(to_backend_err)?;
            let directional = declare_directional_import(
                module,
                &signature,
                self.directional_symbols
                    .get(symbol.owner.index() as usize)
                    .and_then(Option::as_ref),
            )?;
            imports.insert(
                symbol.owner,
                PureCallImport {
                    function,
                    inputs: symbol.inputs.clone(),
                    outputs: symbol.outputs.clone(),
                    directional,
                },
            );
        }
        Ok(imports)
    }

    pub(crate) fn call_cells(
        &self,
        site: &solve::SolvePureCallSite,
        input: &[u64],
        output: &mut [u64],
    ) -> Result<(), CompileError> {
        let symbol = self
            .symbols
            .get(site.owner().index() as usize)
            .filter(|symbol| {
                symbol.owner == site.owner()
                    && symbol.inputs.as_ref() == site.inputs()
                    && symbol.outputs.as_ref() == site.outputs()
            })
            .ok_or_else(|| {
                CompileError::Input(format!(
                    "typed call site {} does not match its compiled owner",
                    site.owner().index()
                ))
            })?;
        let input_count = scalar_type_count(&symbol.inputs, "typed call input")?;
        let output_count = symbol.outputs.iter().try_fold(0usize, |count, value| {
            count
                .checked_add(value.value_type().scalar_count() as usize)
                .ok_or_else(|| CompileError::Input("typed call output count overflows".into()))
        })?;
        if input.len() != input_count || output.len() != output_count {
            return Err(CompileError::Input(format!(
                "typed call payload has {}/{} cells; expected {input_count}/{output_count}",
                input.len(),
                output.len()
            )));
        }
        type TypedCall = unsafe extern "C" fn(*const u64, *mut u64);
        // SAFETY: `symbol.address` is the finalized address of a Cranelift
        // function constructed with exactly the two-pointer `TypedCall` ABI.
        // The checked site/interface and cell extents are proved above, and
        // the owning JIT module remains alive in `self` for this call.
        let call: TypedCall = unsafe { std::mem::transmute(symbol.address) };
        // SAFETY: both slices have the exact checked cell extents required by
        // the compiled owner and remain valid for the duration of the call.
        unsafe { call(input.as_ptr(), output.as_mut_ptr()) };
        Ok(())
    }
}

fn declare_directional_import(
    module: &mut JITModule,
    signature: &cranelift_codegen::ir::Signature,
    symbol: Option<&PureCallSymbol>,
) -> Result<Option<Box<PureCallDirectionalImport>>, CompileError> {
    let Some(symbol) = symbol else {
        return Ok(None);
    };
    let function = module
        .declare_function(&symbol.name, Linkage::Import, signature)
        .map_err(to_backend_err)?;
    Ok(Some(Box::new(PureCallDirectionalImport {
        function,
        inputs: symbol.inputs.clone(),
        outputs: symbol.outputs.clone(),
    })))
}

fn scalar_type_count(
    types: &[solve::SolveValueType],
    context: &str,
) -> Result<usize, CompileError> {
    types.iter().try_fold(0usize, |count, value_type| {
        count
            .checked_add(value_type.scalar_count() as usize)
            .ok_or_else(|| CompileError::Input(format!("{context} count overflows")))
    })
}

struct TableCompiler {
    table_id: usize,
    module: OwnedJitModule,
    math: MathImports,
    functions: Vec<FuncId>,
    directional_functions: Vec<Option<FuncId>>,
}

impl TableCompiler {
    fn new(table_id: usize, table: &solve::SolvePureCallTable) -> Result<Self, CompileError> {
        let mut builder = JITBuilder::with_flags(
            &[("opt_level", "speed")],
            cranelift_module::default_libcall_names(),
        )
        .map_err(to_backend_err)?;
        register_math_symbols(&mut builder);
        let mut module = OwnedJitModule::new(JITModule::new(builder));
        let pointer_type = module.target_config().pointer_type();
        let mut signature = module.make_signature();
        signature.params.push(AbiParam::new(pointer_type));
        signature.params.push(AbiParam::new(pointer_type));
        let mut functions = Vec::with_capacity(table.owners().len());
        let mut directional_functions = Vec::with_capacity(table.owners().len());
        for owner in table.owners() {
            let name = owner_symbol(table_id, owner.id());
            functions.push(
                module
                    .declare_function(&name, Linkage::Export, &signature)
                    .map_err(to_backend_err)?,
            );
            let directional_function = if owner.directional().is_some() {
                Some(
                    module
                        .declare_function(
                            &directional_owner_symbol(table_id, owner.id()),
                            Linkage::Export,
                            &signature,
                        )
                        .map_err(to_backend_err)?,
                )
            } else {
                None
            };
            directional_functions.push(directional_function);
        }
        Ok(Self {
            table_id,
            module,
            math: MathImports::default(),
            functions,
            directional_functions,
        })
    }

    fn compile_all(&mut self, table: &solve::SolvePureCallTable) -> Result<(), CompileError> {
        for owner in table.owners() {
            self.compile_owner(table, owner)?;
            if owner.directional().is_some() {
                self.compile_directional_owner(table, owner)?;
            }
        }
        finalize_jit_module(&mut self.module)
    }

    fn compile_owner(
        &mut self,
        table: &solve::SolvePureCallTable,
        owner: &solve::SolvePureCallOwner,
    ) -> Result<(), CompileError> {
        let function = *self
            .functions
            .get(owner.id().index() as usize)
            .ok_or_else(|| CompileError::Backend("typed owner function is missing".into()))?;
        let functions = self.functions.iter().copied().map(Some).collect::<Vec<_>>();
        self.compile_program(
            table,
            owner.id(),
            function,
            owner.body(),
            owner.inputs().len(),
            owner.outputs().len(),
            &functions,
            false,
        )
    }

    fn compile_directional_owner(
        &mut self,
        table: &solve::SolvePureCallTable,
        owner: &solve::SolvePureCallOwner,
    ) -> Result<(), CompileError> {
        let directional = owner
            .directional()
            .ok_or_else(|| CompileError::Backend("typed directional owner is missing".into()))?;
        let function = self
            .directional_functions
            .get(owner.id().index() as usize)
            .copied()
            .flatten()
            .ok_or_else(|| CompileError::Backend("typed directional function is missing".into()))?;
        let functions = self.directional_functions.clone();
        self.compile_program(
            table,
            owner.id(),
            function,
            directional.body(),
            directional.inputs().len(),
            directional.outputs().len(),
            &functions,
            true,
        )
    }

    // SPEC_0021: Exception - validated boundary keeps proof-relevant inputs explicit.
    #[allow(clippy::too_many_arguments)]
    fn compile_program(
        &mut self,
        table: &solve::SolvePureCallTable,
        _owner: solve::SolvePureCallOwnerId,
        function: FuncId,
        program: &solve::TypedProgram,
        input_count: usize,
        output_count: usize,
        functions: &[Option<FuncId>],
        directional: bool,
    ) -> Result<(), CompileError> {
        let pointer_type = self.module.target_config().pointer_type();
        let mut context = self.module.make_context();
        context
            .func
            .signature
            .params
            .push(AbiParam::new(pointer_type));
        context
            .func
            .signature
            .params
            .push(AbiParam::new(pointer_type));
        let mut builder_context = FunctionBuilderContext::new();
        {
            let mut builder = FunctionBuilder::new(&mut context.func, &mut builder_context);
            let entry = builder.create_block();
            builder.append_block_params_for_function_params(entry);
            builder.switch_to_block(entry);
            builder.seal_block(entry);
            let parameters = builder.block_params(entry).to_vec();
            let layout = ProgramLayout::new(program, input_count, output_count)?;
            let tape = create_tape(&mut builder, pointer_type, layout.tape_cells)?;
            let invocation_layout = InvocationCacheLayout::new(table, program, directional)?;
            let invocation_cache = invocation_layout
                .has_entries()
                .then(|| create_invocation_cache(&mut builder, pointer_type, &invocation_layout))
                .transpose()?;
            let mut lowerer = ProgramLowerer {
                builder: &mut builder,
                module: &mut self.module,
                math: &mut self.math,
                pointer_type,
                input: parameters[0],
                output: parameters[1],
                tape,
                layout: &layout,
                functions,
                invocation_cache,
                invocation_layout: invocation_cache.map(|_| &invocation_layout),
                flags: MemFlags::new(),
            };
            lowerer.lower(program)?;
            builder.ins().return_(&[]);
            builder.finalize();
        }
        let flags = settings::Flags::new(settings::builder());
        verify_function(&context.func, &flags).map_err(to_backend_err)?;
        self.module
            .define_function(function, &mut context)
            .map_err(to_backend_err)?;
        self.module.clear_context(&mut context);
        Ok(())
    }

    fn finish(
        self,
        table: &solve::SolvePureCallTable,
    ) -> Result<CompiledPureCallTable, CompileError> {
        let mut symbols = Vec::with_capacity(table.owners().len());
        let mut directional_symbols = Vec::with_capacity(table.owners().len());
        for owner in table.owners() {
            let function = self.functions[owner.id().index() as usize];
            let address = self.module.get_finalized_function(function);
            if address.is_null() {
                return Err(CompileError::Backend(format!(
                    "Cranelift returned a null typed owner {} function",
                    owner.id().index()
                )));
            }
            symbols.push(PureCallSymbol {
                owner: owner.id(),
                name: owner_symbol(self.table_id, owner.id()),
                address,
                inputs: owner.inputs().into(),
                outputs: owner.outputs().into(),
            });
            let directional_symbol = match (
                owner.directional(),
                self.directional_functions[owner.id().index() as usize],
            ) {
                (Some(directional), Some(function)) => {
                    let address = self.module.get_finalized_function(function);
                    validate_finalized_address(address, "directional", owner.id().index())?;
                    Some(PureCallSymbol {
                        owner: owner.id(),
                        name: directional_owner_symbol(self.table_id, owner.id()),
                        address,
                        inputs: directional.inputs().into(),
                        outputs: directional.outputs().into(),
                    })
                }
                (None, None) => None,
                _ => {
                    return Err(CompileError::Backend(
                        "typed directional owner/function mismatch".into(),
                    ));
                }
            };
            directional_symbols.push(directional_symbol);
        }
        Ok(CompiledPureCallTable {
            symbols: symbols.into_boxed_slice(),
            directional_symbols: directional_symbols.into_boxed_slice(),
            _module: self.module,
        })
    }
}

fn owner_symbol(table: usize, owner: solve::SolvePureCallOwnerId) -> String {
    format!("rumoca_typed_pure_call_{table}_{}", owner.index())
}

fn directional_owner_symbol(table: usize, owner: solve::SolvePureCallOwnerId) -> String {
    format!(
        "rumoca_typed_pure_call_directional_{table}_{}",
        owner.index()
    )
}

fn validate_finalized_address(
    address: *const u8,
    kind: &str,
    owner: u32,
) -> Result<(), CompileError> {
    if address.is_null() {
        return Err(CompileError::Backend(format!(
            "Cranelift returned a null typed {kind} owner {owner} function"
        )));
    }
    Ok(())
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum StorageBase {
    Input,
    Output,
    Tape,
}

#[derive(Clone)]
struct ValueLocation {
    base: StorageBase,
    cell: u32,
    value_type: solve::SolveValueType,
}

struct ProgramLayout {
    slots: Box<[ValueLocation]>,
    registers: Box<[ValueLocation]>,
    tape_cells: u32,
}

#[derive(Clone, Copy)]
struct InvocationCacheEntry {
    flag_cell: u32,
    output_cell: u32,
    output_cells: u32,
}

/// Final-boundary storage for repeated references to one compiler-issued call
/// invocation in a non-iterative structured scope.
///
/// Exact owner ids provide the semantic identity. Conditional descendants
/// share the scope; compact map/fold bodies are deliberately excluded because
/// each domain point has a distinct binder coordinate.
struct InvocationCacheLayout {
    entries: HashMap<solve::SolvePureCallOwnerId, InvocationCacheEntry>,
    cells: u32,
}

impl InvocationCacheLayout {
    fn new(
        table: &solve::SolvePureCallTable,
        program: &solve::TypedProgram,
        directional: bool,
    ) -> Result<Self, CompileError> {
        let mut counts = vec![0u32; table.owners().len()];
        collect_non_iterative_calls(program, &mut counts)?;
        let repeated = counts
            .iter()
            .enumerate()
            .filter_map(|(index, count)| (*count > 1).then_some(index))
            .collect::<Vec<_>>();
        let flag_cells = u32::try_from(repeated.len())
            .map_err(|_| CompileError::Backend("typed invocation flags overflow".into()))?;
        let mut next_output = flag_cells;
        let mut entries = HashMap::with_capacity(repeated.len());
        for (flag, index) in repeated.into_iter().enumerate() {
            let owner = table
                .owners()
                .get(index)
                .ok_or_else(|| CompileError::Backend("typed invocation owner is missing".into()))?;
            let outputs = invocation_outputs(owner, directional)?;
            let output_cells = outputs.iter().try_fold(0u32, |count, output| {
                checked_cells(
                    count,
                    output.value_type().scalar_count(),
                    "typed invocation output",
                )
            })?;
            let flag_cell = u32::try_from(flag)
                .map_err(|_| CompileError::Backend("typed invocation flag overflows".into()))?;
            entries.insert(
                owner.id(),
                InvocationCacheEntry {
                    flag_cell,
                    output_cell: next_output,
                    output_cells,
                },
            );
            next_output =
                checked_cells(next_output, output_cells, "typed invocation cache layout")?;
        }
        Ok(Self {
            entries,
            cells: next_output,
        })
    }

    fn has_entries(&self) -> bool {
        !self.entries.is_empty()
    }
}

fn invocation_outputs(
    owner: &solve::SolvePureCallOwner,
    directional: bool,
) -> Result<&[solve::SolvePureCallOutput], CompileError> {
    if !directional {
        return Ok(owner.outputs());
    }
    owner
        .directional()
        .map(solve::SolvePureCallDirectionalOwner::outputs)
        .ok_or_else(|| {
            CompileError::Backend("directional invocation references unavailable owner".into())
        })
}

fn collect_non_iterative_calls(
    program: &solve::TypedProgram,
    counts: &mut [u32],
) -> Result<(), CompileError> {
    for operation in program.operations() {
        match operation.operation() {
            solve::SolveOperation::Call { owner, .. } => {
                let count = counts.get_mut(owner.index() as usize).ok_or_else(|| {
                    CompileError::Backend("typed invocation owner is out of bounds".into())
                })?;
                *count = count.checked_add(1).ok_or_else(|| {
                    CompileError::Backend("typed invocation count overflows".into())
                })?;
            }
            solve::SolveOperation::Conditional {
                if_true, if_false, ..
            } => {
                collect_non_iterative_calls(if_true.body(), counts)?;
                collect_non_iterative_calls(if_false.body(), counts)?;
            }
            solve::SolveOperation::Map { .. } | solve::SolveOperation::Fold { .. } => {}
            _ => {}
        }
    }
    Ok(())
}

impl ProgramLayout {
    fn region(region: &solve::SolveProgramRegion) -> Result<Self, CompileError> {
        Self::new(region.body(), region.inputs().len(), region.outputs().len())
    }

    fn new(
        program: &solve::TypedProgram,
        input_slots: usize,
        output_slots: usize,
    ) -> Result<Self, CompileError> {
        let mut input_cell = 0u32;
        let mut output_cell = 0u32;
        let mut tape_cell = 0u32;
        let mut slots = Vec::with_capacity(program.slots().len());
        for (index, slot) in program.slots().iter().enumerate() {
            let count = slot.value_type().scalar_count();
            let (base, cell) = if index < input_slots {
                let cell = input_cell;
                input_cell = checked_cells(input_cell, count, "typed input layout")?;
                (StorageBase::Input, cell)
            } else if index < input_slots.saturating_add(output_slots) {
                let cell = output_cell;
                output_cell = checked_cells(output_cell, count, "typed output layout")?;
                (StorageBase::Output, cell)
            } else {
                let cell = tape_cell;
                tape_cell = checked_cells(tape_cell, count, "typed slot layout")?;
                (StorageBase::Tape, cell)
            };
            slots.push(ValueLocation {
                base,
                cell,
                value_type: slot.value_type().clone(),
            });
        }
        let register_base = tape_cell;
        let mut registers = Vec::with_capacity(program.register_types().len());
        for value_type in program.register_types() {
            let cell = tape_cell;
            tape_cell = checked_cells(
                tape_cell,
                value_type.scalar_count(),
                "typed register layout",
            )?;
            registers.push(ValueLocation {
                base: StorageBase::Tape,
                cell,
                value_type: value_type.clone(),
            });
        }
        let (last_uses, definitions) = register_flow(program, registers.len())?;
        alias_read_only_loads(program, &slots, &mut registers)?;
        alias_final_output_stores(program, &slots, &last_uses, &definitions, &mut registers)?;
        alias_consumed_functional_updates(program, &last_uses, &mut registers)?;
        tape_cell = compact_register_tape(register_base, &last_uses, &definitions, &mut registers)?;
        Ok(Self {
            slots: slots.into_boxed_slice(),
            registers: registers.into_boxed_slice(),
            tape_cells: tape_cell,
        })
    }
}

type RegisterFlow = (Vec<Option<usize>>, Vec<Option<usize>>);

fn register_flow(
    program: &solve::TypedProgram,
    register_count: usize,
) -> Result<RegisterFlow, CompileError> {
    let mut last_uses = vec![None; register_count];
    let mut definitions = vec![None; register_count];
    for (operation_index, operation) in program.operations().iter().enumerate() {
        operation.operation().visit_input_registers(|register| {
            if let Some(last_use) = last_uses.get_mut(register.index()) {
                *last_use = Some(operation_index);
            }
        });
        let mut invalid_definition = false;
        operation.operation().visit_output_registers(|register| {
            let Some(definition) = definitions.get_mut(register.index()) else {
                invalid_definition = true;
                return;
            };
            if definition.replace(operation_index).is_some() {
                invalid_definition = true;
            }
        });
        if invalid_definition {
            return Err(CompileError::Backend(
                "checked typed register definition is invalid".into(),
            ));
        }
    }
    Ok((last_uses, definitions))
}

fn alias_read_only_loads(
    program: &solve::TypedProgram,
    slots: &[ValueLocation],
    registers: &mut [ValueLocation],
) -> Result<(), CompileError> {
    for operation in program.operations() {
        let solve::SolveOperation::Load { destination, slot } = operation.operation() else {
            continue;
        };
        let slot_definition = program
            .slots()
            .get(slot.index())
            .ok_or_else(|| CompileError::Backend("typed load slot is missing".into()))?;
        if slot_definition.access() != solve::SolveSlotAccess::ReadOnly {
            continue;
        }
        let source = slots
            .get(slot.index())
            .ok_or_else(|| CompileError::Backend("typed read-only load slot is missing".into()))?
            .clone();
        // Only the call/region input buffer is an immutable external view.
        // Constant slots also have ReadOnly access but live on the mutable
        // tape; aliasing one into a consumed functional update could corrupt a
        // later load from that constant slot.
        if source.base != StorageBase::Input {
            continue;
        }
        let target = registers
            .get_mut(destination.index())
            .ok_or_else(|| CompileError::Backend("typed load destination is missing".into()))?;
        if target.value_type != source.value_type {
            return Err(CompileError::Backend(
                "checked typed load type differs".into(),
            ));
        }
        *target = source;
    }
    Ok(())
}

fn alias_final_output_stores(
    program: &solve::TypedProgram,
    slots: &[ValueLocation],
    last_uses: &[Option<usize>],
    definitions: &[Option<usize>],
    registers: &mut [ValueLocation],
) -> Result<(), CompileError> {
    for (operation_index, operation) in program.operations().iter().enumerate() {
        let solve::SolveOperation::Store { slot, source } = operation.operation() else {
            continue;
        };
        if last_uses.get(source.index()) != Some(&Some(operation_index)) {
            continue;
        }
        let destination = slots
            .get(slot.index())
            .filter(|location| location.base == StorageBase::Output)
            .ok_or_else(|| CompileError::Backend("typed output store slot is missing".into()))?
            .clone();
        let definition = definitions
            .get(source.index())
            .copied()
            .flatten()
            .ok_or_else(|| {
                CompileError::Backend("typed stored register definition is missing".into())
            })?;
        if output_slot_is_observed(program, *slot, definition, operation_index) {
            continue;
        }
        let source_location = registers
            .get_mut(source.index())
            .ok_or_else(|| CompileError::Backend("typed stored register is missing".into()))?;
        if source_location.value_type != destination.value_type {
            return Err(CompileError::Backend(
                "checked typed output store type differs".into(),
            ));
        }
        *source_location = destination;
    }
    Ok(())
}

fn output_slot_is_observed(
    program: &solve::TypedProgram,
    slot: solve::SolveSlotId,
    definition: usize,
    store: usize,
) -> bool {
    program.operations()[definition..store]
        .iter()
        .any(|operation| {
            matches!(
                operation.operation(),
                solve::SolveOperation::Load { slot: candidate, .. }
                    | solve::SolveOperation::Store { slot: candidate, .. }
                    if *candidate == slot
            )
        })
}

fn alias_consumed_functional_updates(
    program: &solve::TypedProgram,
    last_uses: &[Option<usize>],
    registers: &mut [ValueLocation],
) -> Result<(), CompileError> {
    for (operation_index, operation) in program.operations().iter().enumerate() {
        let (destination, aggregate) = match operation.operation() {
            solve::SolveOperation::UpdateElement {
                destination,
                aggregate,
                ..
            }
            | solve::SolveOperation::UpdateSlice {
                destination,
                aggregate,
                ..
            }
            | solve::SolveOperation::UpdateView {
                destination,
                aggregate,
                ..
            } => (*destination, *aggregate),
            _ => continue,
        };
        if last_uses.get(aggregate.index()) != Some(&Some(operation_index)) {
            continue;
        }
        let source = registers
            .get(aggregate.index())
            .ok_or_else(|| CompileError::Backend("typed update source is missing".into()))?
            .clone();
        let target = registers
            .get_mut(destination.index())
            .ok_or_else(|| CompileError::Backend("typed update destination is missing".into()))?;
        if source.base != StorageBase::Tape || target.base != StorageBase::Tape {
            continue;
        }
        if target.value_type != source.value_type {
            return Err(CompileError::Backend(
                "checked functional update type differs".into(),
            ));
        }
        *target = source;
    }
    Ok(())
}

fn checked_cells(start: u32, count: u32, context: &str) -> Result<u32, CompileError> {
    start
        .checked_add(count)
        .ok_or_else(|| CompileError::Backend(format!("{context} overflows")))
}

fn create_tape(
    builder: &mut FunctionBuilder<'_>,
    pointer_type: Type,
    cells: u32,
) -> Result<Value, CompileError> {
    let bytes = cells
        .max(1)
        .checked_mul(CELL_BYTES as u32)
        .ok_or_else(|| CompileError::Backend("typed native tape size overflows".into()))?;
    let slot =
        builder.create_sized_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, bytes, 3));
    Ok(builder.ins().stack_addr(pointer_type, slot, 0))
}

fn create_invocation_cache(
    builder: &mut FunctionBuilder<'_>,
    pointer_type: Type,
    layout: &InvocationCacheLayout,
) -> Result<Value, CompileError> {
    let cache = create_tape(builder, pointer_type, layout.cells)?;
    let zero = builder.ins().iconst(types::I64, 0);
    for entry in layout.entries.values() {
        builder.ins().store(
            MemFlags::new(),
            zero,
            cache,
            cell_offset(entry.flag_cell, "typed invocation flag")?,
        );
    }
    Ok(cache)
}

fn cell_offset(cell: u32, context: &str) -> Result<i32, CompileError> {
    i32::try_from(
        (cell as usize)
            .checked_mul(CELL_BYTES)
            .ok_or_else(|| CompileError::Backend(format!("{context} offset overflows")))?,
    )
    .map_err(|_| CompileError::Backend(format!("{context} offset exceeds i32")))
}

fn cell_pointer(
    builder: &mut FunctionBuilder<'_>,
    base: Value,
    cell: u32,
    context: &str,
) -> Result<Value, CompileError> {
    let bytes = i64::from(cell)
        .checked_mul(CELL_BYTES as i64)
        .ok_or_else(|| CompileError::Backend(format!("{context} pointer overflows")))?;
    Ok(builder.ins().iadd_imm(base, bytes))
}

struct ProgramLowerer<'a, 'b> {
    builder: &'a mut FunctionBuilder<'b>,
    module: &'a mut JITModule,
    math: &'a mut MathImports,
    pointer_type: Type,
    input: Value,
    output: Value,
    tape: Value,
    layout: &'a ProgramLayout,
    functions: &'a [Option<FuncId>],
    invocation_cache: Option<Value>,
    invocation_layout: Option<&'a InvocationCacheLayout>,
    flags: MemFlags,
}

impl ProgramLowerer<'_, '_> {
    fn lower(&mut self, program: &solve::TypedProgram) -> Result<(), CompileError> {
        for operation in program.operations() {
            self.lower_operation(operation.operation())?;
        }
        Ok(())
    }

    // SPEC_0021: exhaustive dispatch over the closed typed operation vocabulary.
    // SPEC_0021: Exception - cohesive exhaustive flow stays contiguous so ordering remains auditable.
    #[allow(clippy::too_many_lines)]
    fn lower_operation(&mut self, operation: &solve::SolveOperation) -> Result<(), CompileError> {
        match operation {
            solve::SolveOperation::Constant { destination, value } => {
                self.lower_constant(*destination, value)
            }
            solve::SolveOperation::Load { destination, slot } => {
                let source = self.slot(*slot)?.clone();
                let destination = self.register(*destination)?.clone();
                self.copy(&source, &destination)
            }
            solve::SolveOperation::Store { slot, source } => {
                let source = self.register(*source)?.clone();
                let destination = self.slot(*slot)?.clone();
                self.copy(&source, &destination)
            }
            solve::SolveOperation::Unary {
                destination,
                operator,
                operand,
            } => self.lower_unary(*destination, *operator, *operand),
            solve::SolveOperation::Binary {
                destination,
                operator,
                lhs,
                rhs,
            } => self.lower_binary(*destination, *operator, *lhs, *rhs),
            solve::SolveOperation::Compare {
                destination,
                operator,
                lhs,
                rhs,
            } => self.lower_compare(*destination, *operator, *lhs, *rhs),
            solve::SolveOperation::Convert {
                destination,
                operator,
                operand,
            } => self.lower_convert(*destination, *operator, *operand),
            solve::SolveOperation::Select {
                destination,
                condition,
                if_true,
                if_false,
            } => self.lower_select(*destination, *condition, *if_true, *if_false),
            solve::SolveOperation::Conditional {
                condition,
                captures,
                destinations,
                if_true,
                if_false,
            } => self.lower_conditional(*condition, captures, destinations, if_true, if_false),
            solve::SolveOperation::Map {
                domain,
                captures,
                destination,
                body,
            } => self.lower_map(domain, captures, *destination, body),
            solve::SolveOperation::Fold {
                domain,
                initial,
                captures,
                destinations,
                transition,
            } => self.lower_fold(domain, initial, captures, destinations, transition),
            solve::SolveOperation::MatrixMultiply {
                destination,
                lhs,
                rhs,
            } => self.lower_matrix_multiply(*destination, *lhs, *rhs),
            solve::SolveOperation::Scale {
                destination,
                aggregate,
                scalar,
            } => self.lower_scale(*destination, *aggregate, *scalar),
            solve::SolveOperation::BroadcastBinary {
                destination,
                operator,
                aggregate,
                scalar,
                scalar_on_lhs,
            } => self.lower_broadcast_binary(
                *destination,
                *operator,
                *aggregate,
                *scalar,
                *scalar_on_lhs,
            ),
            solve::SolveOperation::Transpose {
                destination,
                operand,
            } => self.lower_transpose(*destination, *operand),
            solve::SolveOperation::Cross {
                destination,
                lhs,
                rhs,
            } => self.lower_cross(*destination, *lhs, *rhs),
            solve::SolveOperation::Reduce {
                destination,
                operator,
                operand,
            } => self.lower_reduce(*destination, *operator, *operand),
            solve::SolveOperation::Identity { destination } => self.lower_identity(*destination),
            solve::SolveOperation::Diagonal {
                destination,
                operand,
            } => self.lower_diagonal(*destination, *operand),
            solve::SolveOperation::Concatenate {
                destination,
                axis,
                operands,
            } => self.lower_concatenate(*destination, *axis, operands),
            solve::SolveOperation::Fill { destination, value } => {
                self.lower_fill(*destination, *value)
            }
            solve::SolveOperation::ConstructAggregate {
                destination,
                elements,
            } => self.lower_construct_aggregate(*destination, elements),
            solve::SolveOperation::ProjectElement {
                destination,
                aggregate,
                indices,
            } => self.lower_project_element(*destination, *aggregate, indices),
            solve::SolveOperation::ProjectElementDynamic {
                destination,
                aggregate,
                indices,
            } => self.lower_project_element_dynamic(*destination, *aggregate, indices),
            solve::SolveOperation::ProjectSlice {
                destination,
                aggregate,
                origin,
            } => self.lower_project_slice(*destination, *aggregate, origin),
            solve::SolveOperation::ProjectView {
                destination,
                aggregate,
                axes,
            } => self.lower_project_view(*destination, *aggregate, axes),
            solve::SolveOperation::SelectElement {
                destination,
                aggregate,
                indices,
                out_of_range,
            } => self.lower_select_element(*destination, *aggregate, indices, *out_of_range),
            solve::SolveOperation::UpdateElement {
                destination,
                aggregate,
                value,
                indices,
            } => self.lower_update_element(*destination, *aggregate, *value, indices),
            solve::SolveOperation::UpdateSlice {
                destination,
                aggregate,
                value,
                origin,
            } => self.lower_update_slice(*destination, *aggregate, *value, origin),
            solve::SolveOperation::UpdateView {
                destination,
                aggregate,
                value,
                axes,
            } => self.lower_update_view(*destination, *aggregate, *value, axes),
            solve::SolveOperation::Call {
                owner,
                arguments,
                destinations,
            } => self.lower_call(*owner, arguments, destinations),
        }
    }

    fn slot(&self, slot: solve::SolveSlotId) -> Result<&ValueLocation, CompileError> {
        self.layout
            .slots
            .get(slot.index())
            .ok_or_else(|| CompileError::Backend("typed slot layout is missing".into()))
    }

    fn register(&self, register: solve::SolveRegisterId) -> Result<&ValueLocation, CompileError> {
        self.layout
            .registers
            .get(register.index())
            .ok_or_else(|| CompileError::Backend("typed register layout is missing".into()))
    }

    fn base(&self, location: &ValueLocation) -> Value {
        match location.base {
            StorageBase::Input => self.input,
            StorageBase::Output => self.output,
            StorageBase::Tape => self.tape,
        }
    }

    fn lower_constant(
        &mut self,
        destination: solve::SolveRegisterId,
        value: &solve::SolveValue,
    ) -> Result<(), CompileError> {
        let destination = self.register(destination)?.clone();
        let scalar = match value.kind() {
            solve::SolveValueKind::Real32(bits) => {
                self.builder.ins().f32const(f32::from_bits(bits))
            }
            solve::SolveValueKind::Real64(bits) => {
                self.builder.ins().f64const(f64::from_bits(bits))
            }
            solve::SolveValueKind::Integer(value) => self.builder.ins().iconst(types::I64, value),
            solve::SolveValueKind::Boolean(value) => {
                self.builder.ins().iconst(types::I64, i64::from(value))
            }
        };
        let zero = self.builder.ins().iconst(types::I64, 0);
        self.store_scalar(&destination, zero, scalar)
    }

    fn lower_unary(
        &mut self,
        destination: solve::SolveRegisterId,
        operator: solve::SolveUnaryOperator,
        operand: solve::SolveRegisterId,
    ) -> Result<(), CompileError> {
        let destination = self.register(destination)?.clone();
        let operand = self.register(operand)?.clone();
        self.for_each_element(destination.value_type.scalar_count(), |this, index| {
            let value = this.load_scalar(&operand, index)?;
            let result = this.unary_element(operator, operand.value_type.element_type(), value)?;
            this.store_scalar(&destination, index, result)
        })
    }

    fn lower_binary(
        &mut self,
        destination: solve::SolveRegisterId,
        operator: solve::SolveBinaryOperator,
        lhs: solve::SolveRegisterId,
        rhs: solve::SolveRegisterId,
    ) -> Result<(), CompileError> {
        let destination = self.register(destination)?.clone();
        let lhs = self.register(lhs)?.clone();
        let rhs = self.register(rhs)?.clone();
        self.for_each_element(destination.value_type.scalar_count(), |this, index| {
            let lhs_value = this.load_scalar(&lhs, index)?;
            let rhs_value = this.load_scalar(&rhs, index)?;
            let result = this.binary_element(
                operator,
                destination.value_type.element_type(),
                lhs_value,
                rhs_value,
            )?;
            this.store_scalar(&destination, index, result)
        })
    }

    fn lower_compare(
        &mut self,
        destination: solve::SolveRegisterId,
        operator: solve::SolveCompareOperator,
        lhs: solve::SolveRegisterId,
        rhs: solve::SolveRegisterId,
    ) -> Result<(), CompileError> {
        let destination = self.register(destination)?.clone();
        let lhs = self.register(lhs)?.clone();
        let rhs = self.register(rhs)?.clone();
        self.for_each_element(destination.value_type.scalar_count(), |this, index| {
            let lhs_value = this.load_scalar(&lhs, index)?;
            let rhs_value = this.load_scalar(&rhs, index)?;
            let condition = this.compare_element(
                operator,
                lhs.value_type.element_type(),
                lhs_value,
                rhs_value,
            );
            let result = this.builder.ins().uextend(types::I64, condition);
            this.store_scalar(&destination, index, result)
        })
    }

    fn lower_convert(
        &mut self,
        destination: solve::SolveRegisterId,
        operator: solve::SolveConversionOperator,
        operand: solve::SolveRegisterId,
    ) -> Result<(), CompileError> {
        let destination = self.register(destination)?.clone();
        let operand = self.register(operand)?.clone();
        self.for_each_element(destination.value_type.scalar_count(), |this, index| {
            let value = this.load_scalar(&operand, index)?;
            let result = this.convert_element(
                operator,
                operand.value_type.element_type(),
                &destination.value_type,
                value,
            )?;
            this.store_scalar(&destination, index, result)
        })
    }

    fn lower_select(
        &mut self,
        destination: solve::SolveRegisterId,
        condition: solve::SolveRegisterId,
        if_true: solve::SolveRegisterId,
        if_false: solve::SolveRegisterId,
    ) -> Result<(), CompileError> {
        let destination = self.register(destination)?.clone();
        let condition = self.register(condition)?.clone();
        let if_true = self.register(if_true)?.clone();
        let if_false = self.register(if_false)?.clone();
        let zero = self.builder.ins().iconst(types::I64, 0);
        let condition = self.load_scalar(&condition, zero)?;
        let condition = self.builder.ins().icmp_imm(IntCC::NotEqual, condition, 0);
        self.for_each_element(destination.value_type.scalar_count(), |this, index| {
            let when_true = this.load_scalar(&if_true, index)?;
            let when_false = this.load_scalar(&if_false, index)?;
            let selected = this.builder.ins().select(condition, when_true, when_false);
            this.store_scalar(&destination, index, selected)
        })
    }

    fn lower_call(
        &mut self,
        owner: solve::SolvePureCallOwnerId,
        arguments: &[solve::SolveRegisterId],
        destinations: &[solve::SolveRegisterId],
    ) -> Result<(), CompileError> {
        let cached = self
            .invocation_layout
            .and_then(|layout| layout.entries.get(&owner))
            .copied();
        if let (Some(cache), Some(entry)) = (self.invocation_cache, cached) {
            return self.lower_cached_call(owner, arguments, destinations, cache, entry);
        }
        self.lower_uncached_call(owner, arguments, destinations)
    }

    fn lower_cached_call(
        &mut self,
        owner: solve::SolvePureCallOwnerId,
        arguments: &[solve::SolveRegisterId],
        destinations: &[solve::SolveRegisterId],
        cache: Value,
        entry: InvocationCacheEntry,
    ) -> Result<(), CompileError> {
        let expected_cells = self.register_cell_count(destinations, "typed cached call outputs")?;
        if expected_cells != entry.output_cells {
            return Err(CompileError::Backend(
                "typed cached call output width mismatch".into(),
            ));
        }
        let flag = self.builder.ins().load(
            types::I64,
            self.flags,
            cache,
            cell_offset(entry.flag_cell, "typed invocation flag")?,
        );
        let output = cell_pointer(
            self.builder,
            cache,
            entry.output_cell,
            "typed invocation output",
        )?;
        let ready = self.builder.ins().icmp_imm(IntCC::NotEqual, flag, 0);
        let miss = self.builder.create_block();
        let continuation = self.builder.create_block();
        self.builder.ins().brif(ready, continuation, &[], miss, &[]);

        self.builder.switch_to_block(miss);
        self.builder.seal_block(miss);
        self.call_owner_into(owner, arguments, output)?;
        let one = self.builder.ins().iconst(types::I64, 1);
        self.builder.ins().store(
            self.flags,
            one,
            cache,
            cell_offset(entry.flag_cell, "typed invocation flag")?,
        );
        self.builder.ins().jump(continuation, &[]);

        self.builder.switch_to_block(continuation);
        self.builder.seal_block(continuation);
        self.unpack_registers(output, destinations)
    }

    fn lower_uncached_call(
        &mut self,
        owner: solve::SolvePureCallOwnerId,
        arguments: &[solve::SolveRegisterId],
        destinations: &[solve::SolveRegisterId],
    ) -> Result<(), CompileError> {
        let output_cells = self.register_cell_count(destinations, "nested typed output")?;
        let output = create_tape(self.builder, self.pointer_type, output_cells)?;
        self.call_owner_into(owner, arguments, output)?;
        self.unpack_registers(output, destinations)
    }

    fn call_owner_into(
        &mut self,
        owner: solve::SolvePureCallOwnerId,
        arguments: &[solve::SolveRegisterId],
        output: Value,
    ) -> Result<(), CompileError> {
        let function = self
            .functions
            .get(owner.index() as usize)
            .copied()
            .flatten()
            .ok_or_else(|| CompileError::Backend("nested typed call owner is missing".into()))?;
        let argument_cells = arguments.iter().try_fold(0u32, |count, argument| {
            checked_cells(
                count,
                self.register(*argument)?.value_type.scalar_count(),
                "nested typed input",
            )
        })?;
        let input = create_tape(self.builder, self.pointer_type, argument_cells)?;
        let mut cell = 0u32;
        for argument in arguments {
            let source = self.register(*argument)?.clone();
            let destination = ValueLocation {
                base: StorageBase::Tape,
                cell,
                value_type: source.value_type.clone(),
            };
            self.copy_between_bases(&source, &destination, self.base(&source), input)?;
            cell = checked_cells(cell, source.value_type.scalar_count(), "nested typed input")?;
        }
        let local = self
            .module
            .declare_func_in_func(function, self.builder.func);
        self.builder.ins().call(local, &[input, output]);
        Ok(())
    }

    fn copy(
        &mut self,
        source: &ValueLocation,
        destination: &ValueLocation,
    ) -> Result<(), CompileError> {
        self.copy_between_bases(
            source,
            destination,
            self.base(source),
            self.base(destination),
        )
    }

    fn copy_between_bases(
        &mut self,
        source: &ValueLocation,
        destination: &ValueLocation,
        source_base: Value,
        destination_base: Value,
    ) -> Result<(), CompileError> {
        if source.value_type != destination.value_type {
            return Err(CompileError::Backend(
                "typed native copy type mismatch".into(),
            ));
        }
        if source.base == destination.base
            && source.cell == destination.cell
            && source_base == destination_base
        {
            return Ok(());
        }
        self.for_each_element(source.value_type.scalar_count(), |this, index| {
            let value = this.load_scalar_from(source, source_base, index)?;
            this.store_scalar_to(destination, destination_base, index, value)
        })
    }

    fn for_each_element(
        &mut self,
        count: u32,
        mut body: impl FnMut(&mut Self, Value) -> Result<(), CompileError>,
    ) -> Result<(), CompileError> {
        if count == 1 {
            let zero = self.builder.ins().iconst(types::I64, 0);
            return body(self, zero);
        }
        let header = self.builder.create_block();
        let loop_body = self.builder.create_block();
        let exit = self.builder.create_block();
        self.builder.append_block_param(header, types::I64);
        let zero = self.builder.ins().iconst(types::I64, 0);
        self.builder.ins().jump(header, &[zero.into()]);
        self.builder.switch_to_block(header);
        let index = self.builder.block_params(header)[0];
        let in_range =
            self.builder
                .ins()
                .icmp_imm(IntCC::UnsignedLessThan, index, i64::from(count));
        self.builder.ins().brif(in_range, loop_body, &[], exit, &[]);
        self.builder.switch_to_block(loop_body);
        body(self, index)?;
        let next = self.builder.ins().iadd_imm(index, 1);
        self.builder.ins().jump(header, &[next.into()]);
        self.builder.seal_block(loop_body);
        self.builder.seal_block(header);
        self.builder.switch_to_block(exit);
        self.builder.seal_block(exit);
        Ok(())
    }

    fn load_scalar(
        &mut self,
        location: &ValueLocation,
        index: Value,
    ) -> Result<Value, CompileError> {
        self.load_scalar_from(location, self.base(location), index)
    }

    fn load_scalar_from(
        &mut self,
        location: &ValueLocation,
        base: Value,
        index: Value,
    ) -> Result<Value, CompileError> {
        let address = self.cell_address(base, location.cell, index);
        Ok(self.builder.ins().load(
            scalar_cranelift_type(location.value_type.element_type()),
            self.flags,
            address,
            0,
        ))
    }

    fn store_scalar(
        &mut self,
        location: &ValueLocation,
        index: Value,
        value: Value,
    ) -> Result<(), CompileError> {
        self.store_scalar_to(location, self.base(location), index, value)
    }

    fn store_scalar_to(
        &mut self,
        location: &ValueLocation,
        base: Value,
        index: Value,
        value: Value,
    ) -> Result<(), CompileError> {
        let address = self.cell_address(base, location.cell, index);
        self.builder.ins().store(self.flags, value, address, 0);
        Ok(())
    }

    fn cell_address(&mut self, base: Value, start: u32, index: Value) -> Value {
        let index = self.builder.ins().iadd_imm(index, i64::from(start));
        let bytes = self.builder.ins().imul_imm(index, CELL_BYTES as i64);
        self.builder.ins().iadd(base, bytes)
    }

    fn unary_element(
        &mut self,
        operator: solve::SolveUnaryOperator,
        scalar: solve::SolveScalarType,
        value: Value,
    ) -> Result<Value, CompileError> {
        match scalar {
            solve::SolveScalarType::Real { format, .. } => {
                let promoted = promote_real(self.builder, format, value);
                let result = emit_unary_op(
                    self.builder,
                    self.module,
                    self.math,
                    map_unary(operator),
                    promoted,
                )?;
                Ok(demote_real(self.builder, format, result))
            }
            solve::SolveScalarType::Integer(_) => Ok(match operator {
                solve::SolveUnaryOperator::Negate => self.builder.ins().ineg(value),
                solve::SolveUnaryOperator::Abs => {
                    let negative = self.builder.ins().icmp_imm(IntCC::SignedLessThan, value, 0);
                    let negated = self.builder.ins().ineg(value);
                    self.builder.ins().select(negative, negated, value)
                }
                solve::SolveUnaryOperator::Sign => {
                    let zero = self.builder.ins().iconst(types::I64, 0);
                    let one = self.builder.ins().iconst(types::I64, 1);
                    let negative_one = self.builder.ins().iconst(types::I64, -1);
                    let positive = self
                        .builder
                        .ins()
                        .icmp_imm(IntCC::SignedGreaterThan, value, 0);
                    let negative = self.builder.ins().icmp_imm(IntCC::SignedLessThan, value, 0);
                    let nonpositive = self.builder.ins().select(negative, negative_one, zero);
                    self.builder.ins().select(positive, one, nonpositive)
                }
                _ => {
                    return Err(CompileError::Backend(
                        "invalid typed Integer unary operator".into(),
                    ));
                }
            }),
            solve::SolveScalarType::Boolean => match operator {
                solve::SolveUnaryOperator::Not => {
                    let condition = self.builder.ins().icmp_imm(IntCC::Equal, value, 0);
                    Ok(self.builder.ins().uextend(types::I64, condition))
                }
                _ => Err(CompileError::Backend(
                    "invalid typed Boolean unary operator".into(),
                )),
            },
        }
    }

    fn binary_element(
        &mut self,
        operator: solve::SolveBinaryOperator,
        scalar: solve::SolveScalarType,
        lhs: Value,
        rhs: Value,
    ) -> Result<Value, CompileError> {
        match scalar {
            solve::SolveScalarType::Real { format, .. } => {
                let lhs = promote_real(self.builder, format, lhs);
                let rhs = promote_real(self.builder, format, rhs);
                let result = emit_binary_op(
                    self.builder,
                    self.module,
                    self.math,
                    map_binary(operator),
                    lhs,
                    rhs,
                )?;
                Ok(demote_real(self.builder, format, result))
            }
            solve::SolveScalarType::Integer(_) => Ok(match operator {
                solve::SolveBinaryOperator::Add => self.builder.ins().iadd(lhs, rhs),
                solve::SolveBinaryOperator::Subtract => self.builder.ins().isub(lhs, rhs),
                solve::SolveBinaryOperator::Multiply => self.builder.ins().imul(lhs, rhs),
                solve::SolveBinaryOperator::Divide => self.builder.ins().sdiv(lhs, rhs),
                solve::SolveBinaryOperator::Min | solve::SolveBinaryOperator::Max => {
                    let code = if operator == solve::SolveBinaryOperator::Min {
                        IntCC::SignedLessThan
                    } else {
                        IntCC::SignedGreaterThan
                    };
                    let condition = self.builder.ins().icmp(code, lhs, rhs);
                    self.builder.ins().select(condition, lhs, rhs)
                }
                _ => {
                    return Err(CompileError::Backend(
                        "typed Integer binary operator is unsupported".into(),
                    ));
                }
            }),
            solve::SolveScalarType::Boolean => Ok(match operator {
                solve::SolveBinaryOperator::And => self.builder.ins().band(lhs, rhs),
                solve::SolveBinaryOperator::Or => self.builder.ins().bor(lhs, rhs),
                _ => {
                    return Err(CompileError::Backend(
                        "invalid typed Boolean binary operator".into(),
                    ));
                }
            }),
        }
    }

    fn compare_element(
        &mut self,
        operator: solve::SolveCompareOperator,
        scalar: solve::SolveScalarType,
        lhs: Value,
        rhs: Value,
    ) -> Value {
        match scalar {
            solve::SolveScalarType::Real { format, .. } => {
                let lhs = promote_real(self.builder, format, lhs);
                let rhs = promote_real(self.builder, format, rhs);
                self.builder
                    .ins()
                    .fcmp(map_float_compare(operator), lhs, rhs)
            }
            solve::SolveScalarType::Integer(_) => {
                self.builder.ins().icmp(map_int_compare(operator), lhs, rhs)
            }
            solve::SolveScalarType::Boolean => {
                let code = if operator == solve::SolveCompareOperator::Equal {
                    IntCC::Equal
                } else {
                    IntCC::NotEqual
                };
                self.builder.ins().icmp(code, lhs, rhs)
            }
        }
    }

    fn convert_element(
        &mut self,
        operator: solve::SolveConversionOperator,
        source: solve::SolveScalarType,
        destination: &solve::SolveValueType,
        value: Value,
    ) -> Result<Value, CompileError> {
        Ok(match operator {
            solve::SolveConversionOperator::IntegerToReal => {
                let value = self.builder.ins().fcvt_from_sint(types::F64, value);
                match destination.element_type() {
                    solve::SolveScalarType::Real { format, .. } => {
                        demote_real(self.builder, format, value)
                    }
                    _ => {
                        return Err(CompileError::Backend(
                            "invalid Integer-to-Real destination".into(),
                        ));
                    }
                }
            }
            solve::SolveConversionOperator::RealToIntegerTowardZero => {
                let source = promote_conversion_source(self.builder, source, value)?;
                self.builder.ins().fcvt_to_sint(types::I64, source)
            }
            solve::SolveConversionOperator::RealToIntegerTowardNegativeInfinity => {
                let source = promote_conversion_source(self.builder, source, value)?;
                let floored = emit_unary_op(
                    self.builder,
                    self.module,
                    self.math,
                    rumoca_ir_solve::UnaryOp::Floor,
                    source,
                )?;
                self.builder.ins().fcvt_to_sint(types::I64, floored)
            }
        })
    }
}

fn scalar_cranelift_type(scalar: solve::SolveScalarType) -> Type {
    match scalar {
        solve::SolveScalarType::Real {
            format: solve::SolveRealFormat::Binary32,
            ..
        } => types::F32,
        solve::SolveScalarType::Real {
            format: solve::SolveRealFormat::Binary64,
            ..
        } => types::F64,
        solve::SolveScalarType::Integer(_) | solve::SolveScalarType::Boolean => types::I64,
    }
}

fn promote_real(
    builder: &mut FunctionBuilder<'_>,
    format: solve::SolveRealFormat,
    value: Value,
) -> Value {
    match format {
        solve::SolveRealFormat::Binary32 => builder.ins().fpromote(types::F64, value),
        solve::SolveRealFormat::Binary64 => value,
    }
}

fn demote_real(
    builder: &mut FunctionBuilder<'_>,
    format: solve::SolveRealFormat,
    value: Value,
) -> Value {
    match format {
        solve::SolveRealFormat::Binary32 => builder.ins().fdemote(types::F32, value),
        solve::SolveRealFormat::Binary64 => value,
    }
}

fn promote_conversion_source(
    builder: &mut FunctionBuilder<'_>,
    source: solve::SolveScalarType,
    value: Value,
) -> Result<Value, CompileError> {
    match source {
        solve::SolveScalarType::Real { format, .. } => Ok(promote_real(builder, format, value)),
        _ => Err(CompileError::Backend(
            "invalid Real-to-Integer source".into(),
        )),
    }
}

fn map_unary(operator: solve::SolveUnaryOperator) -> rumoca_ir_solve::UnaryOp {
    use rumoca_ir_solve::UnaryOp as Target;
    match operator {
        solve::SolveUnaryOperator::Negate => Target::Neg,
        solve::SolveUnaryOperator::Not => Target::Not,
        solve::SolveUnaryOperator::Abs => Target::Abs,
        solve::SolveUnaryOperator::Sign => Target::Sign,
        solve::SolveUnaryOperator::Sqrt => Target::Sqrt,
        solve::SolveUnaryOperator::Floor => Target::Floor,
        solve::SolveUnaryOperator::Ceiling => Target::Ceil,
        solve::SolveUnaryOperator::Truncate => Target::Trunc,
        solve::SolveUnaryOperator::Sin => Target::Sin,
        solve::SolveUnaryOperator::Cos => Target::Cos,
        solve::SolveUnaryOperator::Tan => Target::Tan,
        solve::SolveUnaryOperator::Asin => Target::Asin,
        solve::SolveUnaryOperator::Acos => Target::Acos,
        solve::SolveUnaryOperator::Atan => Target::Atan,
        solve::SolveUnaryOperator::Sinh => Target::Sinh,
        solve::SolveUnaryOperator::Cosh => Target::Cosh,
        solve::SolveUnaryOperator::Tanh => Target::Tanh,
        solve::SolveUnaryOperator::Exp => Target::Exp,
        solve::SolveUnaryOperator::Log => Target::Log,
        solve::SolveUnaryOperator::Log10 => Target::Log10,
    }
}

fn map_binary(operator: solve::SolveBinaryOperator) -> rumoca_ir_solve::BinaryOp {
    use rumoca_ir_solve::BinaryOp as Target;
    match operator {
        solve::SolveBinaryOperator::Add => Target::Add,
        solve::SolveBinaryOperator::Subtract => Target::Sub,
        solve::SolveBinaryOperator::Multiply => Target::Mul,
        solve::SolveBinaryOperator::Divide => Target::Div,
        solve::SolveBinaryOperator::Power => Target::Pow,
        solve::SolveBinaryOperator::And => Target::And,
        solve::SolveBinaryOperator::Or => Target::Or,
        solve::SolveBinaryOperator::Atan2 => Target::Atan2,
        solve::SolveBinaryOperator::Min => Target::Min,
        solve::SolveBinaryOperator::Max => Target::Max,
    }
}

fn map_float_compare(operator: solve::SolveCompareOperator) -> FloatCC {
    match operator {
        solve::SolveCompareOperator::Equal => FloatCC::Equal,
        solve::SolveCompareOperator::NotEqual => FloatCC::NotEqual,
        solve::SolveCompareOperator::Less => FloatCC::LessThan,
        solve::SolveCompareOperator::LessEqual => FloatCC::LessThanOrEqual,
        solve::SolveCompareOperator::Greater => FloatCC::GreaterThan,
        solve::SolveCompareOperator::GreaterEqual => FloatCC::GreaterThanOrEqual,
    }
}

fn map_int_compare(operator: solve::SolveCompareOperator) -> IntCC {
    match operator {
        solve::SolveCompareOperator::Equal => IntCC::Equal,
        solve::SolveCompareOperator::NotEqual => IntCC::NotEqual,
        solve::SolveCompareOperator::Less => IntCC::SignedLessThan,
        solve::SolveCompareOperator::LessEqual => IntCC::SignedLessThanOrEqual,
        solve::SolveCompareOperator::Greater => IntCC::SignedGreaterThan,
        solve::SolveCompareOperator::GreaterEqual => IntCC::SignedGreaterThanOrEqual,
    }
}

#[cfg(test)]
mod storage_tests {
    use super::*;
    use rumoca_core::{SourceId, Span};

    fn span(offset: usize) -> Span {
        Span::from_offsets(
            SourceId::from_source_name("typed_storage.modelica"),
            offset,
            offset + 1,
        )
    }

    fn arithmetic() -> solve::SolveArithmeticProfile {
        solve::SolveArithmeticProfile::construct(
            solve::SolveRealFormat::Binary64,
            solve::SolveRoundingMode::NearestTiesToEven,
            solve::SolveIntegerDomain::construct(i32::MIN.into(), i32::MAX.into()).unwrap(),
        )
    }

    #[test]
    fn checked_storage_forwards_one_functional_update_without_mutating_input() {
        let arithmetic = arithmetic();
        let tensor =
            solve::SolveValueType::tensor(solve::SolveScalarType::real(arithmetic), vec![4])
                .unwrap();
        let program = solve::TypedProgram::construct(arithmetic, |builder| {
            let input = builder.declare_slot(
                tensor.clone(),
                solve::SolveStorageClass::Input,
                solve::SolveSlotAccess::ReadOnly,
                span(0),
            )?;
            let output = builder.declare_slot(
                tensor.clone(),
                solve::SolveStorageClass::Output,
                solve::SolveSlotAccess::ReadWrite,
                span(1),
            )?;
            let aggregate = builder.load(input, span(2))?;
            let index =
                builder.constant(solve::SolveValue::integer(arithmetic, 2).unwrap(), span(3))?;
            let value = builder.constant(solve::SolveValue::real(arithmetic, 9.0), span(4))?;
            let updated = builder.update_element(aggregate, value, &[index], span(5))?;
            builder.store(output, updated, span(6))
        })
        .unwrap();

        let layout = ProgramLayout::new(&program, 1, 1).unwrap();
        let solve::SolveOperation::Load {
            destination: aggregate,
            ..
        } = program.operations()[0].operation()
        else {
            panic!("first operation is the aggregate load")
        };
        let solve::SolveOperation::UpdateElement {
            destination: updated,
            ..
        } = program.operations()[3].operation()
        else {
            panic!("fourth operation is the functional update")
        };
        let aggregate = &layout.registers[aggregate.index()];
        let updated = &layout.registers[updated.index()];
        assert_eq!(aggregate.base, StorageBase::Input);
        assert_eq!(aggregate.cell, 0);
        assert_eq!(updated.base, StorageBase::Output);
        assert_eq!(updated.cell, 0);
    }

    #[test]
    fn checked_storage_never_aliases_a_read_only_constant_tape_slot() {
        let arithmetic = arithmetic();
        let tensor =
            solve::SolveValueType::tensor(solve::SolveScalarType::real(arithmetic), vec![4])
                .unwrap();
        let program = solve::TypedProgram::construct(arithmetic, |builder| {
            let constant = builder.declare_slot(
                tensor,
                solve::SolveStorageClass::Constant,
                solve::SolveSlotAccess::ReadOnly,
                span(10),
            )?;
            let aggregate = builder.load(constant, span(11))?;
            let index =
                builder.constant(solve::SolveValue::integer(arithmetic, 2).unwrap(), span(12))?;
            let value = builder.constant(solve::SolveValue::real(arithmetic, 9.0), span(13))?;
            let _updated = builder.update_element(aggregate, value, &[index], span(14))?;
            Ok(())
        })
        .unwrap();

        let layout = ProgramLayout::new(&program, 0, 0).unwrap();
        let solve::SolveOperation::Load {
            destination: aggregate,
            slot,
        } = program.operations()[0].operation()
        else {
            panic!("first operation is the constant-slot load")
        };
        let slot = &layout.slots[slot.index()];
        let aggregate = &layout.registers[aggregate.index()];
        assert_eq!(slot.base, StorageBase::Tape);
        assert_eq!(aggregate.base, StorageBase::Tape);
        assert_ne!(aggregate.cell, slot.cell);
    }

    #[test]
    fn checked_storage_reuses_tape_only_after_the_last_use() {
        let arithmetic = arithmetic();
        let program = solve::TypedProgram::construct(arithmetic, |builder| {
            let first = builder.constant(solve::SolveValue::real(arithmetic, 2.0), span(20))?;
            let live = builder.unary(solve::SolveUnaryOperator::Negate, first, span(21))?;
            let reused = builder.constant(solve::SolveValue::real(arithmetic, 3.0), span(22))?;
            let _result =
                builder.binary(solve::SolveBinaryOperator::Add, live, reused, span(23))?;
            Ok(())
        })
        .unwrap();

        let layout = ProgramLayout::new(&program, 0, 0).unwrap();
        let first = &layout.registers[0];
        let live = &layout.registers[1];
        let reused = &layout.registers[2];
        assert_eq!(first.cell, reused.cell);
        assert_ne!(first.cell, live.cell);
    }

    #[test]
    fn checked_storage_keeps_same_operation_inputs_and_output_disjoint() {
        let arithmetic = arithmetic();
        let program = solve::TypedProgram::construct(arithmetic, |builder| {
            let lhs = builder.constant(solve::SolveValue::real(arithmetic, 2.0), span(30))?;
            let rhs = builder.constant(solve::SolveValue::real(arithmetic, 3.0), span(31))?;
            let _result = builder.binary(solve::SolveBinaryOperator::Add, lhs, rhs, span(32))?;
            Ok(())
        })
        .unwrap();

        let layout = ProgramLayout::new(&program, 0, 0).unwrap();
        let cells = layout
            .registers
            .iter()
            .map(|location| location.cell)
            .collect::<std::collections::HashSet<_>>();
        assert_eq!(cells.len(), 3);
    }
}
