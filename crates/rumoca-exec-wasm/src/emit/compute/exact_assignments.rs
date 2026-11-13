//! Emit each complete issued tuple before committing its ordered Y targets.
mod observations;
mod ready;
#[cfg(test)]
mod tests;

use super::*;
use crate::exact_assignments::plan::Plan;
use crate::native_program::CompiledNativeCallProgramWasm;
use crate::typed_call::program::ProgramHelpers;
use call_program::layout::CallProgramPlan;
use ready::Ready;
use rumoca_ir_solve as solve;

pub(crate) fn emit_exact_assignment_module(
    source: &solve::ComputeBlock,
    owners: &solve::ContinuousRefreshOwners,
    schedule: &solve::ExactRefreshAssignmentSchedule,
    layout: &VarLayout,
    table: &solve::SolvePureCallTable,
) -> Result<CompiledNativeCallProgramWasm, String> {
    let chunks = schedule.chunks().collect::<Vec<_>>();
    emit_module(
        Ready::new(source, owners, schedule, layout, table)?,
        layout,
        false,
        &chunks,
    )
}

/// Issued ordered rows: every row commits its slot targets before the next
/// row reads them, one function per issued chunk. `layout` holds the issued
/// slots as its Y.
pub(crate) fn emit_issued_assignment_module(
    rows: &solve::IssuedAssignmentRows,
    layout: &VarLayout,
    table: &solve::SolvePureCallTable,
) -> Result<CompiledNativeCallProgramWasm, String> {
    let chunks = rows.chunks().collect::<Vec<_>>();
    emit_module(
        Ready::issued_rows(rows, layout, table)?,
        layout,
        false,
        &chunks,
    )
}

/// The standalone per-program module the shared entries replace, kept as the
/// reference their results are checked against.
#[cfg(test)]
pub(crate) fn emit_private_program_module(
    block: &solve::ScalarProgramBlock,
    layout: &VarLayout,
    table: &solve::SolvePureCallTable,
) -> Result<CompiledNativeCallProgramWasm, String> {
    emit_module(Ready::private(block, layout, table)?, layout, true, &[])
}

pub(crate) fn emit_event_program_module(
    source: &solve::RetainedEventProgram,
    layout: &VarLayout,
) -> Result<
    (
        CompiledNativeCallProgramWasm,
        Vec<crate::typed_call::retained_event::InvocationStorage>,
    ),
    String,
> {
    let ready = Ready::event(source, layout)?;
    let records = ready.observations.clone();
    emit_module(ready, layout, true, &[]).map(|artifact| (artifact, records))
}

#[cfg(test)]
pub(crate) fn emit_private_directional_module(
    block: &solve::ScalarProgramBlock,
    layout: &VarLayout,
    table: &solve::SolvePureCallTable,
    seed_len: usize,
) -> Result<(CompiledNativeCallProgramWasm, u32), String> {
    let ready = Ready::directional(block, layout, table, seed_len)?;
    let seed_offset = ready
        .seed_offset
        .ok_or("directional entry has no issued seed storage")?;
    emit_module(ready, layout, true, &[]).map(|artifact| (artifact, seed_offset))
}

pub(crate) use ready::AdmissionCache;

/// What admission measured of one private entry: the exact module holding it
/// alone, and the parts a shared module would carry for it.
#[derive(Clone)]
pub(crate) struct AdmittedEntry {
    module_bytes: usize,
    body_bytes: usize,
    linked: std::rc::Rc<ready::LinkedSites>,
}

/// Refuse a private program now with every refusal its later shared module
/// would raise for it alone, including a module over `limit` bytes.
pub(crate) fn admit_private_entry(
    block: &solve::ScalarProgramBlock,
    seed: Option<usize>,
    layout: &VarLayout,
    table: &solve::SolvePureCallTable,
    cache: &mut AdmissionCache,
    limit: usize,
) -> Result<AdmittedEntry, String> {
    let (ready, emitted, linked) = Ready::admit((block, seed), layout, table, cache)?;
    let body_bytes = entry_bytes(&emitted);
    let seed_offset = ready.plan.programs[0].seed.and(ready.seed_offset);
    let (module, _) = private_entries_module(&ready, &[(emitted, seed_offset)])?;
    let module_bytes = module.bytes.len();
    if module_bytes > limit {
        return Err(format!(
            "WASM private entry needs a module of {module_bytes} bytes, over the {limit} bytes a browser compiles synchronously"
        ));
    }
    Ok(AdmittedEntry {
        module_bytes,
        body_bytes,
        linked,
    })
}

/// Per-entry results of one private module; storage and linkage are shared.
pub(crate) struct PrivateEntryArtifact {
    pub(crate) export: String,
    pub(crate) gather_faults: Vec<crate::NativeGatherFault>,
    pub(crate) linear_solve_faults: Vec<crate::NativeLinearSolveFault>,
    pub(crate) seed_offset: Option<u32>,
}

/// Modules for many admitted private programs: each carries the types, math
/// imports and the pure-call helpers of its own entries once, exports one entry
/// per program, and defines one register memory sized at construction for its
/// widest entry. Entries run one at a time and keep no state there between
/// calls (a root register layout has no undefined source), so they share it.
///
/// Consecutive entries fill a module while the estimate from their admissions
/// stays within `budget`; an entry over it has the module it was admitted with.
/// Emitted sizes differ from the estimate only by the encoding of indices and
/// offsets, and a module over `limit` is divided in two. A module of one entry
/// is the one its admission measured within `limit`.
pub(crate) fn emit_private_entries_modules(
    entries: &[(&solve::ScalarProgramBlock, Option<usize>, &AdmittedEntry)],
    layout: &VarLayout,
    table: &solve::SolvePureCallTable,
    (budget, limit): (usize, usize),
) -> Result<Vec<(CompiledNativeCallProgramWasm, Vec<PrivateEntryArtifact>)>, String> {
    let mut modules = Vec::new();
    let mut start = 0;
    while start < entries.len() {
        let first = entries[start].2;
        let mut linked = vec![&first.linked];
        let mut bytes = first.module_bytes;
        let mut end = start + 1;
        while let Some((_, _, next)) = entries.get(end) {
            bytes += next.body_bytes;
            if !linked
                .iter()
                .any(|known| std::rc::Rc::ptr_eq(known, &next.linked))
            {
                bytes += next.linked.helper_bytes();
                linked.push(&next.linked);
            }
            if bytes > budget {
                break;
            }
            end += 1;
        }
        emit_private_group(&entries[start..end], layout, table, limit, &mut modules)?;
        start = end;
    }
    Ok(modules)
}

fn emit_private_group(
    entries: &[(&solve::ScalarProgramBlock, Option<usize>, &AdmittedEntry)],
    layout: &VarLayout,
    table: &solve::SolvePureCallTable,
    limit: usize,
    modules: &mut Vec<(CompiledNativeCallProgramWasm, Vec<PrivateEntryArtifact>)>,
) -> Result<(), String> {
    let sources = entries
        .iter()
        .map(|(block, seed, _)| (*block, *seed))
        .collect::<Vec<_>>();
    let ready = Ready::private_entries(&sources, layout, table)?;
    let mut prefix = Module::new();
    let types = call_program::add_types(&mut prefix);
    let catalog = add_import_section(&mut prefix, &ready.imports, &types);
    let emitted = (0..entries.len())
        .map(|entry| {
            let seed_offset = ready.plan.programs[entry].seed.and(ready.seed_offset);
            emit_programs(
                &catalog,
                &ready,
                layout,
                true,
                entry..entry + 1,
                seed_offset,
                Placement::STANDALONE,
            )
            .map(|function| (function, seed_offset))
        })
        .collect::<Result<Vec<_>, String>>()?;
    let module = private_entries_module(&ready, &emitted)?;
    if module.0.bytes.len() > limit && entries.len() > 1 {
        let (left, right) = entries.split_at(entries.len() / 2);
        emit_private_group(left, layout, table, limit, modules)?;
        return emit_private_group(right, layout, table, limit, modules);
    }
    modules.push(module);
    Ok(())
}

// An entry's body plus a generous bound on its declaration, export and
// body-size records.
fn entry_bytes(emitted: &EmittedFunction) -> usize {
    emitted.function.byte_len() + 32
}

fn private_entries_module(
    ready: &Ready,
    entries: &[(EmittedFunction, Option<u32>)],
) -> Result<(CompiledNativeCallProgramWasm, Vec<PrivateEntryArtifact>), String> {
    let count = u32::try_from(entries.len()).map_err(|_| "WASM entry count overflow")?;
    let mut module = Module::new();
    let types = call_program::add_types(&mut module);
    let catalog = add_import_section(&mut module, &ready.imports, &types);
    let first = catalog
        .eval_function_index
        .checked_add(
            u32::try_from(ready.bodies.len()).map_err(|_| "WASM exact helper count overflow")?,
        )
        .and_then(|index| index.checked_add(count).map(|_| index))
        .ok_or("WASM exact entry index overflow")?;
    let mut functions = FunctionSection::new();
    for _ in ready.bodies.iter() {
        functions.function(3);
    }
    for _ in 0..count {
        functions.function(types.eval_type);
    }
    module.section(&functions);
    ready.arena.add_memory(&mut module);
    let mut exports = ExportSection::new();
    exports.export("memory", ExportKind::Memory, 0);
    let names = (0..count)
        .map(|entry| format!("e{entry}"))
        .collect::<Vec<_>>();
    for (offset, name) in (0..count).zip(&names) {
        exports.export(name, ExportKind::Func, first + offset);
    }
    module.section(&exports);
    let mut code = CodeSection::new();
    for body in ready.bodies.iter() {
        code.function(body);
    }
    let mut artifacts = Vec::with_capacity(names.len());
    for (export, (emitted, seed_offset)) in names.into_iter().zip(entries) {
        code.function(&emitted.function);
        artifacts.push(PrivateEntryArtifact {
            export,
            gather_faults: emitted.gather_faults.clone(),
            linear_solve_faults: emitted.linear_solve_faults.clone(),
            seed_offset: *seed_offset,
        });
    }
    module.section(&code);
    let artifact = CompiledNativeCallProgramWasm {
        bytes: module.finish(),
        capacity_policy: crate::CheckedNativeWasmCapacityPolicy::default(),
        scratch_bytes: ready.calls.view().bytes,
        scratch_report: ready.scratch_report.clone(),
        faults: ready.faults.clone(),
        gather_faults: Vec::new(),
        linear_solve_faults: Vec::new(),
        math_imports: ready.imports.iter().map(|import| import.symbol()).collect(),
        carried: Default::default(),
    };
    Ok((artifact, artifacts))
}

fn emit_module(
    ready: Ready,
    layout: &VarLayout,
    private: bool,
    chunks: &[std::ops::Range<usize>],
) -> Result<CompiledNativeCallProgramWasm, String> {
    // Only exact schedules pass issued chunks; every other module passes none.
    if chunks.len() > 1 {
        return emit_chunked_module(ready, layout, chunks);
    }
    let mut module = Module::new();
    let types = call_program::add_types(&mut module);
    let mut catalog = add_import_section(&mut module, &ready.imports, &types);
    catalog.eval_function_index = catalog
        .eval_function_index
        .checked_add(
            u32::try_from(ready.bodies.len()).map_err(|_| "WASM exact helper count overflow")?,
        )
        .ok_or("WASM exact entry index overflow")?;
    let mut functions = FunctionSection::new();
    for _ in ready.bodies.iter() {
        functions.function(3);
    }
    functions.function(types.eval_type);
    module.section(&functions);
    ready.arena.add_memory(&mut module);
    let mut exports = ExportSection::new();
    exports.export("memory", ExportKind::Memory, 0);
    exports.export(
        if private {
            "eval_private"
        } else {
            "eval_assignments"
        },
        ExportKind::Func,
        catalog.eval_function_index,
    );
    module.section(&exports);
    let emitted = emit_function(&catalog, &ready, layout, private)?;
    let mut code = CodeSection::new();
    for body in ready.bodies.iter() {
        code.function(body);
    }
    code.function(&emitted.function);
    module.section(&code);
    Ok(CompiledNativeCallProgramWasm {
        bytes: module.finish(),
        capacity_policy: crate::CheckedNativeWasmCapacityPolicy::default(),
        scratch_bytes: ready.calls.view().bytes,
        scratch_report: ready.scratch_report,
        faults: ready.faults,
        gather_faults: emitted.gather_faults,
        linear_solve_faults: emitted.linear_solve_faults,
        math_imports: ready.imports.iter().map(|import| import.symbol()).collect(),
        carried: Default::default(),
    })
}

struct EmittedFunction {
    function: Function,
    gather_faults: Vec<crate::NativeGatherFault>,
    linear_solve_faults: Vec<crate::NativeLinearSolveFault>,
}

fn emit_function(
    catalog: &ImportCatalog,
    ready: &Ready,
    layout: &VarLayout,
    private: bool,
) -> Result<EmittedFunction, String> {
    emit_programs(
        catalog,
        ready,
        layout,
        private,
        0..ready.plan.programs.len(),
        ready.seed_offset,
        Placement::STANDALONE,
    )
}

/// Where a function's programs sit in their schedule: the ordinal of its first
/// program, and the source fault statuses earlier functions already issued.
#[derive(Clone, Copy)]
struct Placement {
    kernel_base: usize,
    status_offset: u32,
}

impl Placement {
    /// Ordinals and statuses local to the function, as for a standalone module.
    const STANDALONE: Self = Self {
        kernel_base: 0,
        status_offset: 0,
    };
}

/// One function per issued chunk, called in order by `eval_assignments`, which
/// returns the first nonzero status. Programs keep their schedule ordinals and
/// their source fault statuses, so a chunked module reports exactly what the
/// single function would.
fn emit_chunked_module(
    ready: Ready,
    layout: &VarLayout,
    chunks: &[std::ops::Range<usize>],
) -> Result<CompiledNativeCallProgramWasm, String> {
    let mut module = Module::new();
    let types = call_program::add_types(&mut module);
    let mut catalog = add_import_section(&mut module, &ready.imports, &types);
    let first_chunk = catalog
        .eval_function_index
        .checked_add(
            u32::try_from(ready.bodies.len()).map_err(|_| "WASM exact helper count overflow")?,
        )
        .ok_or("WASM exact chunk index overflow")?;
    catalog.eval_function_index = first_chunk
        .checked_add(u32::try_from(chunks.len()).map_err(|_| "WASM exact chunk count overflow")?)
        .ok_or("WASM exact entry index overflow")?;
    let mut functions = FunctionSection::new();
    for _ in ready.bodies.iter() {
        functions.function(3);
    }
    for _ in chunks {
        functions.function(types.eval_type);
    }
    functions.function(types.eval_type);
    module.section(&functions);
    ready.arena.add_memory(&mut module);
    let mut exports = ExportSection::new();
    exports.export("memory", ExportKind::Memory, 0);
    exports.export(
        "eval_assignments",
        ExportKind::Func,
        catalog.eval_function_index,
    );
    module.section(&exports);
    let mut gather_faults = Vec::new();
    let mut linear_solve_faults = Vec::new();
    let mut bodies = Vec::with_capacity(chunks.len());
    for range in chunks {
        let status_offset = gather_faults
            .len()
            .checked_add(linear_solve_faults.len())
            .and_then(|count| u32::try_from(count).ok())
            .ok_or("WASM exact chunk status overflow")?;
        let mut emitted = emit_programs(
            &catalog,
            &ready,
            layout,
            false,
            range.clone(),
            ready.seed_offset,
            Placement {
                kernel_base: range.start,
                status_offset,
            },
        )?;
        gather_faults.append(&mut emitted.gather_faults);
        linear_solve_faults.append(&mut emitted.linear_solve_faults);
        bodies.push(emitted.function);
    }
    let mut entry = Function::new([(1, ValType::I32)]);
    for chunk in 0..chunks.len() {
        let target = u32::try_from(chunk)
            .ok()
            .and_then(|n| first_chunk.checked_add(n))
            .ok_or("WASM exact chunk index overflow")?;
        for argument in 0..LOCAL_BASE {
            entry.instruction(&Instruction::LocalGet(argument));
        }
        entry.instruction(&Instruction::Call(target));
        entry.instruction(&Instruction::LocalTee(LOCAL_BASE));
        entry.instruction(&Instruction::If(BlockType::Empty));
        entry.instruction(&Instruction::LocalGet(LOCAL_BASE));
        entry.instruction(&Instruction::Return);
        entry.instruction(&Instruction::End);
    }
    entry.instruction(&Instruction::I32Const(0));
    entry.instruction(&Instruction::End);
    let mut code = CodeSection::new();
    for body in ready.bodies.iter() {
        code.function(body);
    }
    for body in &bodies {
        code.function(body);
    }
    code.function(&entry);
    module.section(&code);
    Ok(CompiledNativeCallProgramWasm {
        bytes: module.finish(),
        capacity_policy: crate::CheckedNativeWasmCapacityPolicy::default(),
        scratch_bytes: ready.calls.view().bytes,
        scratch_report: ready.scratch_report,
        faults: ready.faults,
        gather_faults,
        linear_solve_faults,
        math_imports: ready.imports.iter().map(|import| import.symbol()).collect(),
        carried: Default::default(),
    })
}

fn emit_programs(
    catalog: &ImportCatalog,
    ready: &Ready,
    layout: &VarLayout,
    private: bool,
    programs: std::ops::Range<usize>,
    seed_offset: Option<u32>,
    placement: Placement,
) -> Result<EmittedFunction, String> {
    let mut locals = ready.arena.locals();
    locals.extend([(2, ValType::I32), (1, ValType::I64)]);
    #[cfg(feature = "diagnostic-root-scalar-locals")]
    let diagnostic_scalar_base = LOCAL_BASE
        .checked_add(locals.iter().try_fold(0u32, |sum, &(count, _)| {
            sum.checked_add(count)
                .ok_or("diagnostic existing local count overflow")
        })?)
        .ok_or("diagnostic scalar local base overflow")?;
    #[cfg(feature = "diagnostic-root-scalar-locals")]
    {
        let scalar_count = ready.arena.scalar_local_count();
        if diagnostic_scalar_base
            .checked_sub(LOCAL_BASE)
            .and_then(|n| n.checked_add(scalar_count))
            .is_none_or(|total| total > crate::typed_call::cost::ENGINE_LOCAL_LIMIT)
        {
            return Err("diagnostic root function local capacity exceeded".into());
        }
        locals.push((scalar_count, ValType::F64));
    }
    let mut function = Function::new(locals);
    let stops = ready
        .faults
        .iter()
        .filter(|fault| fault.kind == crate::TypedCallFaultKind::AssertionFailed)
        .map(|fault| fault.status)
        .collect::<Vec<_>>();
    let mut emitter = BodyEmitter::new(catalog, &mut function);
    #[cfg(feature = "diagnostic-root-scalar-locals")]
    {
        emitter.diagnostic_scalar_base = Some(diagnostic_scalar_base);
    }
    emitter.private_seed_offset = seed_offset;
    emitter.seed_slots = ready.plan.seed_slots;
    if !ready.observations.is_empty() {
        emitter.event_invocations = Some(&ready.observations);
        emitter.event_stops = &stops;
    }
    emitter.arena = Some(ready.arena.clone());
    // Each entry of a shared module is called on its own.
    ready.calls.begin_function();
    emitter.calls = Some(&ready.calls);
    emitter.gather_status_base = Some(
        u32::try_from(ready.faults.len())
            .ok()
            .and_then(|n| n.checked_add(3))
            .and_then(|n| n.checked_add(placement.status_offset))
            .ok_or("exact gather status overflow")?,
    );
    emitter.begin_exact_schedule(layout)?;
    emitter.reset_event_invocations();
    emitter.root_program_cursor = programs.start;
    let end = programs.end;
    for (kernel, program) in ready.plan.programs[programs].iter().enumerate() {
        // Ordinals count from the function's place in its schedule.
        emitter.kernel_ordinal = placement.kernel_base + kernel;
        emitter.push(Instruction::LocalGet(SEED_PTR_PARAM));
        emitter.push(Instruction::LocalSet(OUT_PTR_PARAM));
        let outputs = (0..program.block.stored_output_count()).collect::<Vec<_>>();
        emitter.emit_mapped_programs(&program.block, &outputs)?;
        if !private {
            emit_commit(&mut emitter, &program.targets)?;
        }
    }
    emitter.require_root_programs_through(end)?;
    emitter.push(Instruction::I32Const(0));
    let gather_faults = std::mem::take(&mut emitter.gather_faults);
    let linear_solve_faults = std::mem::take(&mut emitter.linear_solve_faults);
    function.instruction(&Instruction::End);
    Ok(EmittedFunction {
        function,
        gather_faults,
        linear_solve_faults,
    })
}

fn emit_commit(emitter: &mut BodyEmitter<'_>, targets: &[usize]) -> Result<(), String> {
    // The complete prefix and tuple have succeeded. All addresses were checked
    // at admission and entry, so the ordered commit has no intermediate faults.
    for (output, &target) in targets.iter().enumerate() {
        emitter.push(Instruction::LocalGet(Y_PTR_PARAM));
        emitter.push(Instruction::LocalGet(SEED_PTR_PARAM));
        emitter.push(Instruction::F64Load(memarg_for_index(output)?));
        emitter.push(Instruction::F64Store(memarg_for_index(target)?));
    }
    Ok(())
}
