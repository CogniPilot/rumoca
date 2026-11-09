//! Prepared Binary64 entry over borrowed, whole-value input regions.
use super::*;

/// A native entry whose checked interface is entirely Binary64 Real.
///
/// Input regions partition the owner's flattened inputs without splitting a
/// semantic value. The entry borrows those regions and the output directly;
/// it neither constructs typed cells nor checks their values at call time.
/// Only a total, single-frame body is admitted, so errors are detected before
/// any caller output is written. Other owners keep their typed call boundary.
pub struct CompiledF64Call {
    address: *const u8,
    regions: Box<[usize]>,
    pointers: Box<[*const f64]>,
    output_count: usize,
    owner: CompiledPureCallTable,
}

impl CompiledF64Call {
    pub fn compile(
        table: &solve::SolvePureCallTable,
        site: &solve::SolvePureCallSite,
        regions: &[usize],
    ) -> Result<Self, CompileError> {
        Self::compile_with(table, site, regions, admit)
    }

    fn compile_with(
        table: &solve::SolvePureCallTable,
        site: &solve::SolvePureCallSite,
        regions: &[usize],
        admission: fn(&solve::SolvePureCallOwner) -> Result<(), CompileError>,
    ) -> Result<Self, CompileError> {
        if !table.matches_site(site) {
            return Err(input_error(
                "direct f64 site does not match its source table",
            ));
        }
        let source = table
            .owner(site.owner())
            .ok_or_else(|| input_error("missing f64 owner"))?;
        admission(source)?;
        let slots = input_regions(source.inputs(), regions)?;
        let output_count = source.outputs().iter().try_fold(0usize, |count, output| {
            count
                .checked_add(output.value_type().scalar_count() as usize)
                .ok_or_else(|| input_error("direct f64 output extent overflows"))
        })?;
        let _source_scope = crate::expansion::call_table_scope(Some(table));
        let table_id = NEXT_TABLE_ID.fetch_add(1, Ordering::Relaxed);
        let support = support::decide(table_id, table)?;
        if !support[source.id().index() as usize].primal_is_native() {
            return Err(input_error("direct f64 owner has no native form"));
        }
        let mut compiler = TableCompiler::new(table_id, table, Emission::Define, &support)?;
        compiler.compile_all(table, &support)?;
        let function = compiler.compile_f64_entry(table, source, &slots)?;
        finalize_jit_module(&mut compiler.module)?;
        let address = compiler.module.get_finalized_function(function);
        validate_finalized_address(address, "direct f64", source.id().index())?;
        let owner = compiler.finish(table, support)?;
        Ok(Self {
            address,
            regions: regions.into(),
            pointers: vec![std::ptr::null(); regions.len()].into_boxed_slice(),
            output_count,
            owner,
        })
    }

    /// Borrow exact input regions and a disjoint output for this invocation.
    /// All shape checks precede native entry; no per-value conversion occurs.
    pub fn call(&mut self, input: &[&[f64]], output: &mut [f64]) -> Result<(), CompileError> {
        if input.len() != self.regions.len() || output.len() != self.output_count {
            return Err(input_error("direct f64 region/output extent mismatch"));
        }
        for ((values, expected), pointer) in input
            .iter()
            .zip(self.regions.iter())
            .zip(self.pointers.iter_mut())
        {
            if values.len() != *expected {
                return Err(input_error("direct f64 input region extent mismatch"));
            }
            *pointer = values.as_ptr();
        }
        type Entry =
            unsafe extern "C" fn(*const *const f64, *mut f64, *mut scratch::NativeScratch) -> u8;
        // SAFETY: compile_f64_entry issues exactly this pointer-table/output/
        // scratch/status ABI. The complete interface, slot partition and cell
        // offsets were checked at preparation; the extents were checked above.
        // Rust borrows make the output disjoint from every input. The retained
        // owner keeps code and scratch alive, and every f64 bit pattern is valid.
        let entry: Entry = unsafe { std::mem::transmute(self.address) };
        let status = self
            .owner
            .scratch
            .invoke(|arena| unsafe { entry(self.pointers.as_ptr(), output.as_mut_ptr(), arena) });
        status::check(status)
    }
}

/// Prepared Binary64 execution with success-only output publication.
/// Uses the same whole-region lowering as CompiledF64Call, but permits a
/// faulting single-frame owner because writes go only to owned f64 staging.
/// Numerical-domain policy belongs to the owner's explicit IR guards.
pub struct CompiledCheckedF64Call {
    code: CompiledF64Call,
    staging: Vec<f64>,
}

impl CompiledCheckedF64Call {
    pub fn compile(
        table: &solve::SolvePureCallTable,
        site: &solve::SolvePureCallSite,
        regions: &[usize],
    ) -> Result<Self, CompileError> {
        let code = CompiledF64Call::compile_with(table, site, regions, admit_checked)?;
        let mut staging = Vec::new();
        staging
            .try_reserve_exact(code.output_count)
            .map_err(|_| input_error("checked f64 staging allocation failed"))?;
        staging.resize(code.output_count, 0.0);
        Ok(Self { code, staging })
    }

    /// Failed shape/domain/native calls leave every caller output bit intact.
    /// There are no per-evaluation allocations or typed-cell conversions.
    pub fn call(&mut self, input: &[&[f64]], output: &mut [f64]) -> Result<(), CompileError> {
        if output.len() != self.staging.len() {
            return Err(input_error("checked f64 output extent mismatch"));
        }
        self.code.call(input, &mut self.staging)?;
        output.copy_from_slice(&self.staging);
        Ok(())
    }
}

fn input_error(message: &str) -> CompileError {
    CompileError::Input(message.into())
}

fn binary64(value: &solve::SolveValueType) -> bool {
    matches!(
        value.element_type(),
        solve::SolveScalarType::Real {
            format: solve::SolveRealFormat::Binary64,
            ..
        }
    )
}

fn admit(owner: &solve::SolvePureCallOwner) -> Result<(), CompileError> {
    admit_checked(owner)?;
    if !owner.body().region_fault_class().is_prunable() {
        return Err(input_error(
            "direct f64 entry requires a total single-frame body",
        ));
    }
    Ok(())
}

fn admit_checked(owner: &solve::SolvePureCallOwner) -> Result<(), CompileError> {
    if !owner.inputs().iter().all(binary64)
        || !owner
            .outputs()
            .iter()
            .all(|value| binary64(value.value_type()))
    {
        return Err(input_error(
            "direct f64 entry requires a Binary64 Real interface",
        ));
    }
    let body = owner.body();
    if !body.assigned_state_inputs().is_empty()
        || !body.assigned_state_outputs().is_empty()
        || body
            .operations()
            .iter()
            .any(|op| nested_frame(op.operation()))
    {
        return Err(input_error(
            "direct f64 entry requires a total single-frame body",
        ));
    }
    Ok(())
}

// These forms can allocate a later native frame/tape. Excluding them keeps
// every scratch acquisition before the first direct output publication;
// semantic fault/effect admission comes from the canonical IR fault owner.
fn nested_frame(operation: &solve::SolveOperation) -> bool {
    use solve::SolveOperation as O;
    matches!(
        operation,
        O::Conditional { .. }
            | O::Map { .. }
            | O::Fold { .. }
            | O::Call { .. }
            | O::CheckAssertion { .. }
            | O::AssignedState { .. }
            | O::AssignedConditional { .. }
            | O::Native { .. }
            | O::LinearSolve { .. }
    )
}

/// One slot's containing region and region-relative cell. No tensor is split.
fn input_regions(
    inputs: &[solve::SolveValueType],
    regions: &[usize],
) -> Result<Vec<(usize, u32)>, CompileError> {
    if regions.is_empty() {
        return Err(input_error(
            "direct f64 entry needs at least one input region",
        ));
    }
    let mut slots = Vec::with_capacity(inputs.len());
    let mut region = 0;
    let mut offset = 0usize;
    for value in inputs {
        let count = value.scalar_count() as usize;
        while region < regions.len() && offset == regions[region] {
            region += 1;
            offset = 0;
        }
        if count == 0 {
            // Empty slots are never read, but still need a valid placement.
            slots.push((region.min(regions.len().saturating_sub(1)), 0));
            continue;
        }
        let end = offset
            .checked_add(count)
            .ok_or_else(|| input_error("direct f64 region extent overflows"))?;
        if regions.get(region).is_none_or(|size| end > *size) {
            return Err(input_error(
                "direct f64 input region splits or omits a typed value",
            ));
        }
        slots.push((
            region,
            u32::try_from(offset)
                .map_err(|_| input_error("direct f64 region offset exceeds u32"))?,
        ));
        offset = end;
    }
    while region < regions.len() && offset == regions[region] {
        region += 1;
        offset = 0;
    }
    if region != regions.len() {
        return Err(input_error(
            "direct f64 regions do not cover the exact typed input",
        ));
    }
    Ok(slots)
}

impl TableCompiler {
    fn compile_f64_entry(
        &mut self,
        table: &solve::SolvePureCallTable,
        owner: &solve::SolvePureCallOwner,
        slots: &[(usize, u32)],
    ) -> Result<FuncId, CompileError> {
        let config = self.module.target_config();
        let pointer_type = config.pointer_type();
        let mut context = self.module.make_context();
        program_signature(&mut context.func.signature, pointer_type, false);
        let function = self
            .module
            .declare_function(
                &format!(
                    "rumoca_f64_regions_{}_{}",
                    self.table_id,
                    owner.id().index()
                ),
                Linkage::Export,
                &context.func.signature,
            )
            .map_err(to_backend_err)?;
        let mut builder_context = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut context.func, &mut builder_context);
        let entry = builder.create_block();
        builder.append_block_params_for_function_params(entry);
        builder.switch_to_block(entry);
        builder.seal_block(entry);
        let parameters = builder.block_params(entry).to_vec();
        scratch::enter(&mut builder);
        let layout = ProgramLayout::new(
            owner.body(),
            (owner.inputs().len(), owner.outputs().len()),
            IssuedHomes::of(owner.body()),
        )?;
        let tape = create_tape(&mut builder, pointer_type, layout.tape_cells)?;
        let placement = region_placement(
            &mut builder,
            pointer_type,
            parameters[0],
            owner.inputs(),
            slots,
        )?;
        let mut lowerer = ProgramLowerer {
            scalar_values: vec![None; layout.registers.len()],
            scalar_forwarding: Some(scalar_forwarding::ScalarForwarding::default()),
            emission: Emission::Define,
            builder: &mut builder,
            module: &mut self.module,
            math: &mut self.math,
            pointer_type,
            input: parameters[0],
            output: parameters[1],
            tape,
            layout: &layout,
            outputs: owner.outputs(),
            functions: &self.functions,
            recursive_calls: None,
            flags: MemFlags::new(),
            operation: std::ptr::null(),
            observation: None,
            table: Some(table),
            inline_depth: 0,
            input_placement: Some(placement),
            output_placement: None,
        };
        lowerer.lower(owner.body())?;
        status::succeed(&mut builder);
        builder.finalize();
        self.module.queue_function(function, &mut context)?;
        self.module.clear_context(&mut context);
        Ok(function)
    }
}

fn region_placement(
    builder: &mut FunctionBuilder<'_>,
    pointer_type: Type,
    pointers: Value,
    inputs: &[solve::SolveValueType],
    slots: &[(usize, u32)],
) -> Result<Vec<placement::PlacedSlot>, CompileError> {
    let mut start = 0;
    inputs
        .iter()
        .zip(slots)
        .map(|(value, &(region, offset))| {
            let bytes = region
                .checked_mul(pointer_type.bytes() as usize)
                .and_then(|n| i32::try_from(n).ok())
                .ok_or_else(|| input_error("direct f64 pointer table offset overflows"))?;
            let root = builder
                .ins()
                .load(pointer_type, MemFlags::new(), pointers, bytes);
            let count = value.scalar_count();
            let slot = placement::PlacedSlot::on_region(
                builder,
                start,
                placement::PhysicalCells {
                    root,
                    cell: offset,
                    count,
                },
            );
            start = checked_cells(start, count, "direct f64 input slots")?;
            Ok(slot)
        })
        .collect()
}

#[cfg(test)]
mod tests;
