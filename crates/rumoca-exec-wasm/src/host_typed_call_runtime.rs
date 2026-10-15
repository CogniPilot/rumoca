//! Private wasmi execution of the issued three-pointer typed-call ABI.

use std::cell::RefCell;
use wasmi::{Memory, Store, TypedFunc};

use crate::host_runtime::{ensure_memory_capacity, instantiate_host_module};
use crate::{CompiledTypedCallWasm, WasmCompileError};

type Entry = TypedFunc<(i32, i32, i32), i32>;

struct Instance {
    store: Store<()>,
    memory: Memory,
    entry: Entry,
}

pub(crate) struct HostTypedCallRuntime {
    instance: RefCell<Instance>,
    input_bytes: usize,
    output_bytes: usize,
    scratch_bytes: usize,
    output_at: usize,
    scratch_at: usize,
    end: usize,
    pointers: (i32, i32, i32),
}

fn backend(context: &str, error: impl std::fmt::Display) -> WasmCompileError {
    WasmCompileError::Backend(format!("{context}: {error}"))
}

impl HostTypedCallRuntime {
    pub(crate) fn new(artifact: &CompiledTypedCallWasm) -> Result<Self, WasmCompileError> {
        let (mut store, memory, instance) = instantiate_host_module(artifact.module_bytes())?;
        let entry = instance
            .get_typed_func::<(i32, i32, i32), i32>(&store, "eval_typed_call")
            .map_err(|e| backend("missing typed call export", e))?;
        let layout = artifact.layout();
        // The gaps keep empty spans aligned and distinct. Retain this checked
        // placement rather than reconstructing pointers at every invocation.
        let overflow = || WasmCompileError::Input("typed call memory footprint overflows".into());
        let output_at = (layout.input_bytes as usize)
            .checked_add(8)
            .ok_or_else(overflow)?;
        let scratch_at = output_at
            .checked_add(layout.output_bytes as usize)
            .and_then(|end| end.checked_add(8))
            .ok_or_else(overflow)?;
        let end = scratch_at
            .checked_add(layout.scratch_bytes as usize)
            .ok_or_else(overflow)?;
        let pointer =
            |at| i32::try_from(at).map_err(|error| backend("typed call memory footprint", error));
        pointer(end)?;
        let pointers = (0, pointer(output_at)?, pointer(scratch_at)?);
        ensure_memory_capacity(&mut store, memory, end)?;
        Ok(Self {
            instance: RefCell::new(Instance {
                store,
                memory,
                entry,
            }),
            input_bytes: layout.input_bytes as usize,
            output_bytes: layout.output_bytes as usize,
            scratch_bytes: layout.scratch_bytes as usize,
            output_at,
            scratch_at,
            end,
            pointers,
        })
    }

    pub(crate) fn call_typed(
        &self,
        input: &[u64],
        output: &mut [u64],
        scratch: &mut [u64],
    ) -> Result<u32, WasmCompileError> {
        for (cells, expected) in [
            (input.len(), self.input_bytes),
            (output.len(), self.output_bytes),
            (scratch.len(), self.scratch_bytes),
        ] {
            if cells.checked_mul(8) != Some(expected) {
                return Err(WasmCompileError::Input(
                    "typed call buffer footprint differs from its issued layout".into(),
                ));
            }
        }
        let (output_at, scratch_at, end) = (self.output_at, self.scratch_at, self.end);
        let mut guard = self.instance.borrow_mut();
        let Instance {
            store,
            memory,
            entry,
        } = &mut *guard;
        ensure_memory_capacity(store, *memory, end)?;
        write_cells(store, *memory, 0, input)?;
        write_cells(store, *memory, output_at, output)?;
        write_cells(store, *memory, scratch_at, scratch)?;
        let status = entry
            .call(&mut *store, self.pointers)
            .map_err(|e| backend("typed call failed", e))? as u32;
        let data = memory.data(&*store);
        let written_output = data.get(output_at..scratch_at - 8).ok_or_else(|| {
            WasmCompileError::Input("typed call memory footprint overflows".into())
        })?;
        let written_scratch = data.get(scratch_at..end).ok_or_else(|| {
            WasmCompileError::Input("typed call memory footprint overflows".into())
        })?;
        // Stopped/faulting invocations expose initialized observations only;
        // ordinary output cells remain private until the issued success status.
        if status == 0 {
            read_cells(written_output, output);
        }
        read_cells(written_scratch, scratch);
        Ok(status)
    }
}

fn write_cells(
    store: &mut Store<()>,
    memory: Memory,
    offset: usize,
    cells: &[u64],
) -> Result<(), WasmCompileError> {
    let bytes = cells
        .iter()
        .flat_map(|cell| cell.to_le_bytes())
        .collect::<Vec<_>>();
    memory
        .write(store, offset, &bytes)
        .map_err(|e| backend("typed call memory write failed", e))
}

fn read_cells(bytes: &[u8], cells: &mut [u64]) {
    for (bytes, cell) in bytes.chunks_exact(8).zip(cells) {
        let mut word = [0; 8];
        word.copy_from_slice(bytes);
        *cell = u64::from_le_bytes(word);
    }
}
