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

use std::cell::RefCell;

use cranelift_codegen::isa::{OwnedTargetIsa, TargetIsa};
use cranelift_codegen::settings::{self, Configurable};
use cranelift_module::{FuncId, Module, ModuleReloc};

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
    let mut shared = settings::builder();
    for value in target.flags().iter() {
        shared
            .set(value.name, &value.value_string())
            .map_err(|error| error.to_string())?;
    }
    shared
        .set("regalloc_algorithm", "single_pass")
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

/// Define `function` from `context`, choosing its register allocator by size.
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
    let alignment = context
        .compile(&*target, &mut Default::default())
        .map_err(|error| format!("{:?}", error.inner))?
        .buffer
        .alignment;
    let compiled = context
        .compiled_code()
        .ok_or("single-pass compiled code is missing")?;
    let bytes = compiled.code_buffer().to_vec();
    let relocations = compiled
        .buffer
        .relocs()
        .iter()
        .map(|relocation| ModuleReloc::from_mach_reloc(relocation, &context.func, function))
        .collect::<Vec<_>>();
    module
        .define_function_bytes(function, u64::from(alignment), &bytes, &relocations)
        .map_err(|error| error.to_string())
}

#[cfg(test)]
mod tests;
