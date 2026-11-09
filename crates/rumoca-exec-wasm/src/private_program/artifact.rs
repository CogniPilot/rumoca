//! Data transport for original compiled private entries, not another emitter.
//! The outer current-build model product owns trust and source/option binding.
use super::*;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

#[derive(Clone, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
struct EntryWire {
    module: usize,
    export: String,
    source_key: String,
    gather: Vec<NativeGatherFault>,
    linear: Vec<NativeLinearSolveFault>,
    y: usize,
    p: usize,
    outputs: usize,
    seed: Option<directional::SeedStorage>,
}

/// Opaque bytes produced from actual compiler-issued entries. Deserialization
/// alone grants no execution: `bind` requires the current checked source owner.
/// This is a trusted build asset, not a signature or an authenticity proof.
#[derive(Clone, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct PrecompiledPrivatePrograms {
    version: u32,
    modules: Vec<Rc<crate::program_payload::ProgramPayload>>,
    entries: Vec<EntryWire>,
}

impl PrecompiledPrivatePrograms {
    pub fn from_compiled(programs: &[CompiledPrivateProgramWasm]) -> Self {
        let mut indices = BTreeMap::new();
        let mut modules = Vec::new();
        let entries = programs
            .iter()
            .map(|program| {
                let id = Rc::as_ptr(&program.module) as usize;
                let module = *indices.entry(id).or_insert_with(|| {
                    let index = modules.len();
                    modules.push(Rc::clone(&program.module));
                    index
                });
                EntryWire {
                    module,
                    export: program.export.clone(),
                    source_key: program.source_key.clone(),
                    gather: program.gather_faults.clone(),
                    linear: program.linear_solve_faults.clone(),
                    y: program.y,
                    p: program.p,
                    outputs: program.outputs,
                    seed: program.seed.clone(),
                }
            })
            .collect();
        Self {
            version: 1,
            modules,
            entries,
        }
    }

    pub fn bind(
        &self,
        source: &rumoca_ir_solve::PreparedKernelSource,
        requests: &[(&ScalarProgramBlock, Option<usize>)],
        layout: &VarLayout,
    ) -> Result<Vec<CompiledPrivateProgramWasm>, WasmCompileError> {
        if self.version != 1 || self.entries.len() != requests.len() {
            return Err(WasmCompileError::Input(
                "precompiled private inventory differs".into(),
            ));
        }
        for (entry, &(block, seed)) in self.entries.iter().zip(requests) {
            let expected = source
                .private_program(block, seed)
                .map_err(WasmCompileError::Input)?;
            if entry.source_key != expected
                || entry.y != layout.y_scalars()
                || entry.p != layout.p_scalars()
                || entry.outputs != block.stored_output_count()
                || entry.seed.as_ref().map(|seed| seed.count) != seed
                || entry.module >= self.modules.len()
            {
                return Err(WasmCompileError::Input(
                    "precompiled private entry belongs to another checked source".into(),
                ));
            }
        }
        let mut output = Vec::with_capacity(self.entries.len());
        for (index, module) in self.modules.iter().enumerate() {
            let entries = self
                .entries
                .iter()
                .enumerate()
                .filter(|(_, entry)| entry.module == index)
                .collect::<Vec<_>>();
            if entries.is_empty()
                || module.module_bytes().len() > super::SYNC_COMPILE_BYTES
                || module.scratch_bytes()
                    > crate::CheckedNativeWasmCapacityPolicy::default().public_scratch_bytes()
            {
                return Err(WasmCompileError::Input(
                    "precompiled private module extent differs".into(),
                ));
            }
            let exports = entries
                .iter()
                .map(|(_, entry)| entry.export.as_str())
                .collect::<Vec<_>>();
            #[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
            let runtimes = crate::WasmKernelRuntime::new_entries(module.module_bytes(), &exports)?;
            #[cfg(all(feature = "host-engine", not(target_arch = "wasm32")))]
            let runtimes = crate::host_runtime::HostKernelRuntime::new_entries(
                module.module_bytes(),
                &exports,
            )?;
            #[cfg(not(any(
                all(target_arch = "wasm32", target_os = "unknown"),
                all(feature = "host-engine", not(target_arch = "wasm32"))
            )))]
            let runtimes = vec![(); entries.len()];
            let module = Rc::clone(module);
            for ((ordinal, entry), runtime) in entries.into_iter().zip(runtimes) {
                #[cfg(not(any(
                    all(target_arch = "wasm32", target_os = "unknown"),
                    all(feature = "host-engine", not(target_arch = "wasm32"))
                )))]
                let () = runtime;
                output.push((
                    ordinal,
                    CompiledPrivateProgramWasm {
                        module: Rc::clone(&module),
                        export: entry.export.clone(),
                        gather_faults: entry.gather.clone(),
                        linear_solve_faults: entry.linear.clone(),
                        y: entry.y,
                        p: entry.p,
                        outputs: entry.outputs,
                        seed: entry.seed.clone(),
                        source_key: entry.source_key.clone(),
                        #[cfg(any(
                            all(target_arch = "wasm32", target_os = "unknown"),
                            all(feature = "host-engine", not(target_arch = "wasm32"))
                        ))]
                        runtime,
                    },
                ));
            }
        }
        output.sort_unstable_by_key(|(ordinal, _)| *ordinal);
        Ok(output.into_iter().map(|(_, program)| program).collect())
    }
}

#[cfg(all(test, feature = "host-engine"))]
mod tests;
