//! Register allocator choice per native function.
//!
//! Cranelift's backtracking allocator reuses spill slots and keeps frames
//! small, but its cost per function dominates code generation when a model
//! has many small kernels. The single-pass allocator is several times faster
//! and gives every spilled value its own slot, so its frame grows with the
//! function's value count. A function of at most [`SINGLE_PASS_VALUE_LIMIT`]
//! SSA values therefore compiles single-pass, where that growth is bounded,
//! and every larger function keeps the backtracking allocator. Every frame
//! larger than the guard page is also probed (see
//! [`super::host_runtime::host_jit_builder`]), so no frame can step past the
//! guard page whichever allocator formed it. The allocator changes only
//! register and stack assignment, never the computed values.
//!
//! Compiling a finished function needs only its target, so the functions an
//! emitter builds are queued ([`QueuedFunctions`]) and compiled on all cores
//! together, then defined in the order they were queued: code layout and
//! every computed value are those of compiling them one at a time.

use std::cell::RefCell;

use cranelift_codegen::isa::{OwnedTargetIsa, TargetIsa};
use cranelift_codegen::settings::{self, Configurable};
use cranelift_module::{FuncId, Module, ModuleReloc};
use rayon::prelude::*;

/// Functions with at most this many SSA values use the single-pass allocator.
pub(super) const SINGLE_PASS_VALUE_LIMIT: usize = 512;

thread_local! {
    /// The single-pass counterpart of a module target, with that target's
    /// shared flags as its key.
    static SINGLE_PASS_TARGET: RefCell<Option<(settings::Flags, OwnedTargetIsa)>> =
        const { RefCell::new(None) };
}

/// Whether a function of `values` SSA values compiles with the single-pass allocator.
pub(super) const fn uses_single_pass(values: usize) -> bool {
    values <= SINGLE_PASS_VALUE_LIMIT
}

/// `target` with only its register allocator switched to single-pass.
fn single_pass_target(target: &dyn TargetIsa) -> Result<OwnedTargetIsa, String> {
    derived_target(target, "regalloc_algorithm", "single_pass")
}

/// `target` with only its shared setting `name` set to `value`.
fn derived_target(
    target: &dyn TargetIsa,
    name: &str,
    setting: &str,
) -> Result<OwnedTargetIsa, String> {
    let mut shared = settings::builder();
    for value in target.flags().iter() {
        shared
            .set(value.name, &value.value_string())
            .map_err(|error| error.to_string())?;
    }
    shared
        .set(name, setting)
        .map_err(|error| error.to_string())?;
    let mut isa = cranelift_codegen::isa::lookup(target.triple().clone())
        .map_err(|error| error.to_string())?;
    for value in target.isa_flags() {
        isa.set(value.name, &value.value_string())
            .map_err(|error| error.to_string())?;
    }
    isa.finish(settings::Flags::new(shared))
        .map_err(|error| error.to_string())
}

/// Compare ISA settings without allocating a formatted string per setting.
fn same_isa_flags(left: &dyn TargetIsa, right: &dyn TargetIsa) -> bool {
    let left = left.isa_flags();
    let right = right.isa_flags();
    left.len() == right.len()
        && left.iter().zip(&right).all(|(left, right)| {
            left.name == right.name
                && left.kind() == right.kind()
                && left.as_bool() == right.as_bool()
                && left.as_num() == right.as_num()
                && left.as_enum() == right.as_enum()
        })
}

/// The cached single-pass counterpart of `target`.
fn single_pass_for(target: &dyn TargetIsa) -> Result<OwnedTargetIsa, String> {
    SINGLE_PASS_TARGET.with(|cached| {
        let mut cached = cached.borrow_mut();
        if let Some((flags, isa)) = cached.as_ref()
            && flags == target.flags()
            && isa.triple() == target.triple()
            && same_isa_flags(&**isa, target)
        {
            return Ok(isa.clone());
        }
        let isa = single_pass_target(target)?;
        *cached = Some((target.flags().clone(), isa.clone()));
        Ok(isa)
    })
}

/// One function's machine code and relocations, ready to define.
struct CompiledFunction {
    function: FuncId,
    alignment: u32,
    bytes: Vec<u8>,
    relocations: Vec<ModuleReloc>,
}

/// Compile `context`, built for `function`, with `target`.
fn compile_bytes(
    target: &dyn TargetIsa,
    function: FuncId,
    context: &mut cranelift_codegen::Context,
) -> Result<CompiledFunction, String> {
    let lowering;
    let target = if has_long_block(&context.func) {
        context
            .optimize(target, &mut Default::default())
            .map_err(|error| format!("{error:?}"))?;
        split_long_blocks(&mut context.func);
        // Lowering verifies against the flow graph, which the cut changed.
        context.compute_cfg();
        context.compute_domtree();
        lowering = derived_target(target, "opt_level", "none")?;
        &*lowering
    } else {
        target
    };
    let alignment = context
        .compile(target, &mut Default::default())
        .map_err(|error| format!("{:?}", error.inner))?
        .buffer
        .alignment;
    let compiled = context.compiled_code().ok_or("compiled code is missing")?;
    let bytes = compiled.code_buffer().to_vec();
    let relocations = compiled
        .buffer
        .relocs()
        .iter()
        .map(|relocation| ModuleReloc::from_mach_reloc(relocation, &context.func, function))
        .collect();
    Ok(CompiledFunction {
        function,
        alignment,
        bytes,
        relocations,
    })
}

/// A block longer than this many IR instructions is cut into pieces of
/// [`BLOCK_SPLIT_INSTS`] (#699).
///
/// Cranelift's `MachBuffer` places branch veneers, constants and trap stubs
/// in islands, and only between blocks. A straight-line block longer than an
/// AArch64 conditional branch reaches (±32 KiB for `tbz`, ±1 MiB for
/// `b.cond`) passes those deadlines: a debug build trips the buffer's range
/// assertion, a release build patches a truncated offset. Unrolled function
/// bodies reach that length (MSL R134a's Helmholtz iterations); every other
/// function compiles unchanged.
const LONG_BLOCK_INSTS: usize = 8_192;
const BLOCK_SPLIT_INSTS: usize = 1_024;

fn has_long_block(func: &cranelift_codegen::ir::Function) -> bool {
    func.dfg.num_insts() > LONG_BLOCK_INSTS
        && func.layout.blocks().any(|block| {
            func.layout
                .block_insts(block)
                .nth(LONG_BLOCK_INSTS)
                .is_some()
        })
}

/// Cut every block of the optimized `func` longer than [`BLOCK_SPLIT_INSTS`]
/// into a chain joined by jumps, so an island fits between the pieces. Each
/// piece's only predecessor is the one before it, so every value stays
/// dominated and the computed values are unchanged. The cut follows the
/// optimization pass and lowering runs without it, since the egraph would
/// place every pure value at its use and so gather the pieces back.
fn split_long_blocks(func: &mut cranelift_codegen::ir::Function) {
    use cranelift_codegen::cursor::{Cursor, FuncCursor};
    use cranelift_codegen::ir::InstBuilder;
    let blocks = func.layout.blocks().collect::<Vec<_>>();
    for block in blocks {
        let insts = func.layout.block_insts(block).collect::<Vec<_>>();
        let mut part = block;
        for &before in insts.iter().step_by(BLOCK_SPLIT_INSTS).skip(1) {
            let next = func.dfg.make_block();
            func.layout.split_block(next, before);
            FuncCursor::new(func).at_bottom(part).ins().jump(next, &[]);
            part = next;
        }
    }
}

fn define_compiled<M: Module>(module: &mut M, compiled: &CompiledFunction) -> Result<(), String> {
    module
        .define_function_bytes(
            compiled.function,
            u64::from(compiled.alignment),
            &compiled.bytes,
            &compiled.relocations,
        )
        .map_err(|error| error.to_string())
}

/// Define `function` from `context` at once, choosing its register allocator
/// by size; emitters queue their functions instead.
#[cfg(test)]
pub(super) fn define_function<M: Module>(
    module: &mut M,
    function: FuncId,
    context: &mut cranelift_codegen::Context,
) -> Result<(), String> {
    if !uses_single_pass(context.func.dfg.num_values()) {
        return module
            .define_function(function, context)
            .map_err(|error| error.to_string());
    }
    let target = single_pass_for(module.isa())?;
    let compiled = compile_bytes(&*target, function, context)?;
    define_compiled(module, &compiled)
}

/// Functions built but not yet compiled, in the order they were built.
#[derive(Default)]
pub(super) struct QueuedFunctions(Vec<QueuedFunction>);

struct QueuedFunction {
    function: FuncId,
    context: cranelift_codegen::Context,
    single_pass: bool,
}

/// Queued functions compiled in one parallel round: enough to occupy every
/// core, few enough that their contexts stay small beside the module.
pub(super) const QUEUE_ROUND: usize = 256;

impl QueuedFunctions {
    /// Queue `function`, built in `context`, which is left as `fresh`. Whether
    /// a round of queued functions is now due.
    pub(super) fn push(
        &mut self,
        function: FuncId,
        context: &mut cranelift_codegen::Context,
        fresh: cranelift_codegen::Context,
    ) -> bool {
        let single_pass = uses_single_pass(context.func.dfg.num_values());
        let context = std::mem::replace(context, fresh);
        self.0.push(QueuedFunction {
            function,
            context,
            single_pass,
        });
        self.0.len() >= QUEUE_ROUND
    }

    /// Compile every queued function in parallel and define them into
    /// `module` in queue order. The first failure in that order is returned.
    pub(super) fn define_into<M: Module>(&mut self, module: &mut M) -> Result<(), String> {
        let queued = std::mem::take(&mut self.0);
        if queued.is_empty() {
            return Ok(());
        }
        let single_pass = single_pass_for(module.isa())?;
        let backtracking = module.isa();
        let compiled = queued
            .into_par_iter()
            .map(|mut queued| {
                let target = if queued.single_pass {
                    &*single_pass
                } else {
                    backtracking
                };
                compile_bytes(target, queued.function, &mut queued.context)
            })
            .collect::<Vec<_>>();
        for compiled in compiled {
            define_compiled(module, &compiled?)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests;
