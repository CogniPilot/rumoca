//! Host execution of a retained native entry through an embedded wasmi engine.
//!
//! The browser engine shares the caller's linear memory, so the entry receives
//! pointers into the caller's buffers. A host engine owns its memory instead:
//! each call lays the caller's buffers out in it, runs the same exported entry
//! with the same status ABI, and copies the written buffers back. Admission is
//! decided once, when the engine is constructed for the compiled module.

use std::{cell::RefCell, rc::Rc};

use wasmi::{Engine, Extern, Linker, Memory, MemoryType, Module, Store, TypedFunc};

use crate::{WasmCompileError, emit::MathImport};

const PAGE_BYTES: usize = 65536;

type Entry = TypedFunc<(i32, i32, f64, i32, i32), i32>;

struct Instance {
    store: Store<()>,
    memory: Memory,
}

pub(crate) struct HostKernelRuntime {
    // Entries of one module share its instance, memory and register storage.
    instance: Rc<RefCell<Instance>>,
    entry: Entry,
}

fn backend(context: &str, error: impl std::fmt::Display) -> WasmCompileError {
    WasmCompileError::Backend(format!("{context}: {error}"))
}

impl HostKernelRuntime {
    pub(crate) fn new_named(module_bytes: &[u8], export: &str) -> Result<Self, WasmCompileError> {
        let mut entries = Self::new_entries(module_bytes, &[export])?;
        entries
            .pop()
            .ok_or_else(|| backend("missing eval export", export))
    }

    /// Instantiate once and bind every named entry to that one instance.
    pub(crate) fn new_entries<S: AsRef<str>>(
        module_bytes: &[u8],
        exports: &[S],
    ) -> Result<Vec<Self>, WasmCompileError> {
        let (store, memory, instance) = instantiate_host_module(module_bytes)?;
        let entries = exports
            .iter()
            .map(|export| {
                instance
                    .get_typed_func::<(i32, i32, f64, i32, i32), i32>(&store, export.as_ref())
                    .map_err(|e| backend("missing eval export", e))
            })
            .collect::<Result<Vec<_>, _>>()?;
        let shared = Rc::new(RefCell::new(Instance { store, memory }));
        Ok(entries
            .into_iter()
            .map(|entry| Self {
                instance: Rc::clone(&shared),
                entry,
            })
            .collect())
    }

    pub(crate) fn call_native(
        &self,
        y: &mut [f64],
        p: &[f64],
        time: f64,
        scratch: &mut [u64],
        lanes: &mut [u64],
    ) -> Result<u32, WasmCompileError> {
        let mut guard = self.instance.borrow_mut();
        let Instance { store, memory } = &mut *guard;
        let entry = &self.entry;
        let y_at = 0usize;
        let p_at = y_at + y.len() * 8;
        let scratch_at = p_at + p.len() * 8;
        let lanes_at = scratch_at + scratch.len() * 8;
        let end = lanes_at + lanes.len() * 8;
        ensure_memory_capacity(store, *memory, end)?;
        let bytes = |cells: &[u64]| {
            cells
                .iter()
                .flat_map(|c| c.to_le_bytes())
                .collect::<Vec<_>>()
        };
        let reals = |cells: &[f64]| {
            cells
                .iter()
                .flat_map(|c| c.to_le_bytes())
                .collect::<Vec<_>>()
        };
        let write = |store: &mut Store<()>, at: usize, data: Vec<u8>| {
            memory
                .write(store, at, &data)
                .map_err(|e| backend("memory write failed", e))
        };
        write(store, y_at, reals(y))?;
        write(store, p_at, reals(p))?;
        write(store, scratch_at, bytes(scratch))?;
        write(store, lanes_at, bytes(lanes))?;
        let pointer = |at: usize| i32::try_from(at).map_err(|e| backend("pointer offset", e));
        let lanes_ptr = if lanes.is_empty() {
            0
        } else {
            pointer(lanes_at)?
        };
        let status = entry
            .call(
                &mut *store,
                (
                    pointer(y_at)?,
                    pointer(p_at)?,
                    time,
                    pointer(scratch_at)?,
                    lanes_ptr,
                ),
            )
            .map_err(|e| backend("native assignment call failed", e))?;
        let mut raw = vec![0u8; end];
        memory
            .read(&*store, 0, &mut raw)
            .map_err(|e| backend("memory read failed", e))?;
        let word = |at: usize| u64::from_le_bytes(raw[at..at + 8].try_into().unwrap());
        for (index, cell) in y.iter_mut().enumerate() {
            *cell = f64::from_bits(word(y_at + index * 8));
        }
        for (index, cell) in scratch.iter_mut().enumerate() {
            *cell = word(scratch_at + index * 8);
        }
        for (index, cell) in lanes.iter_mut().enumerate() {
            *cell = word(lanes_at + index * 8);
        }
        Ok(status as u32)
    }
}

/// One engine/import policy shared by the native and typed-call ABIs.
pub(crate) fn instantiate_host_module(
    module_bytes: &[u8],
) -> Result<(Store<()>, Memory, wasmi::Instance), WasmCompileError> {
    let engine = Engine::default();
    let module =
        Module::new(&engine, module_bytes).map_err(|e| backend("module create failed", e))?;
    let mut store = Store::new(&engine, ());
    let required = module
        .imports()
        .find_map(|import| match import.ty() {
            wasmi::ExternType::Memory(ty) if import.name() == "memory" => Some(*ty),
            _ => None,
        })
        .ok_or_else(|| WasmCompileError::Backend("module imports no memory".into()))?;
    let memory = Memory::new(
        &mut store,
        MemoryType::new(
            u32::try_from(required.minimum().max(1)).map_err(|e| backend("memory minimum", e))?,
            None,
        ),
    )
    .map_err(|e| backend("memory create failed", e))?;
    let mut linker = Linker::new(&engine);
    linker
        .define("env", "memory", Extern::Memory(memory))
        .map_err(|e| backend("memory import set failed", e))?;
    define_math(&mut linker)?;
    let instance = linker
        .instantiate(&mut store, &module)
        .and_then(|pre| pre.start(&mut store))
        .map_err(|e| backend("module instantiate failed", e))?;
    Ok((store, memory, instance))
}

pub(crate) fn ensure_memory_capacity(
    store: &mut Store<()>,
    memory: Memory,
    end: usize,
) -> Result<(), WasmCompileError> {
    let have = memory.data_size(&*store);
    if end > have {
        memory
            .grow(store, (end - have).div_ceil(PAGE_BYTES) as u64)
            .map_err(|e| backend("memory grow failed", e))?;
    }
    Ok(())
}

/// Bind every math ABI symbol to the same `libm` routines the guests export.
fn define_math(linker: &mut Linker<()>) -> Result<(), WasmCompileError> {
    for import in MathImport::ALL {
        let name = import.symbol();
        if matches!(import, MathImport::Pow | MathImport::Atan2) {
            linker
                .func_wrap("env", name, move |x: f64, y: f64| {
                    crate::math_abi::evaluate(import, x, y)
                })
                .map(|_| ())
        } else {
            linker
                .func_wrap("env", name, move |x: f64| {
                    crate::math_abi::evaluate(import, x, 0.0)
                })
                .map(|_| ())
        }
        .map_err(|e| backend("math import set failed", e))?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::HostKernelRuntime;
    use crate::WasmCompileError;

    #[test]
    fn a_module_the_engine_cannot_decode_is_refused_with_its_context() {
        let Err(WasmCompileError::Backend(message)) =
            HostKernelRuntime::new_named(b"not a wasm module", "eval_assignments")
        else {
            panic!("undecodable module bytes must be refused as a backend error");
        };
        assert!(message.starts_with("module create failed: "), "{message}");
    }
}
