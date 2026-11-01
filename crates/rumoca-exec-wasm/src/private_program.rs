//! Execute complete checked value programs without publishing solver storage.
use crate::{
    CompiledNativeCallProgramWasm, NativeGatherFault, NativeLinearSolveFault, TypedCallFault,
    WasmCompileError,
};
use rumoca_ir_solve::{ScalarProgramBlock, SolvePureCallTable, VarLayout};
use std::rc::Rc;
mod directional;
pub use directional::compile_private_directional_program_wasm;

/// One entry of a module shared by every program compiled with it.
pub struct CompiledPrivateProgramWasm {
    module: Rc<CompiledNativeCallProgramWasm>,
    export: String,
    gather_faults: Vec<NativeGatherFault>,
    // Read only by an admitted engine's status mapping.
    #[cfg_attr(
        not(any(
            all(target_arch = "wasm32", target_os = "unknown"),
            all(feature = "host-engine", not(target_arch = "wasm32"))
        )),
        allow(dead_code)
    )]
    linear_solve_faults: Vec<NativeLinearSolveFault>,
    y: usize,
    p: usize,
    outputs: usize,
    seed: Option<directional::SeedStorage>,
    #[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
    runtime: crate::WasmKernelRuntime,
    #[cfg(all(feature = "host-engine", not(target_arch = "wasm32")))]
    runtime: crate::host_runtime::HostKernelRuntime,
}

impl CompiledPrivateProgramWasm {
    /// The whole shared module, including every other entry.
    pub fn module_bytes(&self) -> &[u8] {
        self.module.module_bytes()
    }
    /// This entry's exported function in `module_bytes`.
    pub fn export_name(&self) -> &str {
        &self.export
    }
    pub fn scratch_bytes(&self) -> u32 {
        self.module.scratch_bytes()
    }
    pub fn faults(&self) -> &[TypedCallFault] {
        self.module.faults()
    }
    pub fn gather_faults(&self) -> &[NativeGatherFault] {
        &self.gather_faults
    }
    pub const fn output_count(&self) -> usize {
        self.outputs
    }
    pub fn call(
        &self,
        y: &[f64],
        p: &[f64],
        time: f64,
        scratch: &mut [f64],
    ) -> Result<(), WasmCompileError> {
        self.call_with_seed(y, p, time, None, scratch)
    }

    pub fn call_directional(
        &self,
        y: &[f64],
        p: &[f64],
        time: f64,
        seed: &[f64],
        scratch: &mut [f64],
    ) -> Result<(), WasmCompileError> {
        self.call_with_seed(y, p, time, Some(seed), scratch)
    }

    #[cfg_attr(
        not(any(
            all(target_arch = "wasm32", target_os = "unknown"),
            all(feature = "host-engine", not(target_arch = "wasm32"))
        )),
        allow(dead_code)
    )]
    fn status_error(&self, status: u32) -> WasmCompileError {
        self.module
            .entry_status_error(status, &self.gather_faults, &self.linear_solve_faults)
    }

    fn call_with_seed(
        &self,
        y: &[f64],
        p: &[f64],
        time: f64,
        seed: Option<&[f64]>,
        scratch: &mut [f64],
    ) -> Result<(), WasmCompileError> {
        if y.len() != self.y
            || p.len() != self.p
            || scratch
                .len()
                .checked_mul(8)
                .is_none_or(|n| n < self.scratch_bytes() as usize)
        {
            return Err(WasmCompileError::Input(
                "private program lengths differ from issued layout".into(),
            ));
        }
        match (self.seed, seed) {
            (Some(storage), Some(seed)) if seed.len() >= storage.count => {
                scratch[storage.offset..storage.offset + storage.count]
                    .copy_from_slice(&seed[..storage.count]);
            }
            (None, None) => {}
            _ => {
                return Err(WasmCompileError::Input(
                    "private program seed differs from its issued coordinate storage".into(),
                ));
            }
        }
        #[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
        {
            let status = self.runtime.call_private(y, p, time, scratch)?;
            if status != 0 {
                return Err(self.status_error(status));
            }
            Ok(())
        }
        #[cfg(all(feature = "host-engine", not(target_arch = "wasm32")))]
        {
            let mut cells = scratch
                .iter()
                .map(|value| value.to_bits())
                .collect::<Vec<_>>();
            let status = self
                .runtime
                .call_native(&mut y.to_vec(), p, time, &mut cells, &mut [])?;
            if status != 0 {
                return Err(self.status_error(status));
            }
            for (value, bits) in scratch.iter_mut().zip(cells) {
                *value = f64::from_bits(bits);
            }
            Ok(())
        }
        #[cfg(not(any(
            all(target_arch = "wasm32", target_os = "unknown"),
            all(feature = "host-engine", not(target_arch = "wasm32"))
        )))]
        {
            let _ = (y, p, time, scratch);
            Err(WasmCompileError::Input(
                "compiled private programs have no admitted engine on this target".into(),
            ))
        }
    }
}

pub fn compile_private_program_wasm(
    block: &ScalarProgramBlock,
    layout: &VarLayout,
    table: &SolvePureCallTable,
) -> Result<CompiledPrivateProgramWasm, WasmCompileError> {
    single(compile_private_programs_wasm(
        &[(block, None)],
        layout,
        table,
    )?)
}

/// The largest module a browser compiles synchronously: Chromium refuses a
/// module over 8 MiB on its main thread, where preparation compiles. No
/// private module exceeds it, and an entry that would alone is refused.
const SYNC_COMPILE_BYTES: usize = 8 << 20;

/// The estimated size private entries are packed to, half the limit, so that
/// a packed module rarely needs dividing.
const PRIVATE_MODULE_BYTES: usize = SYNC_COMPILE_BYTES / 2;

/// Compile many complete programs into shared modules with one entry each. A
/// `Some(seed_len)` entry is directional and reads that many seed coordinates.
/// Entries are returned in order and share their module's checked storage.
pub fn compile_private_programs_wasm(
    entries: &[(&ScalarProgramBlock, Option<usize>)],
    layout: &VarLayout,
    table: &SolvePureCallTable,
) -> Result<Vec<CompiledPrivateProgramWasm>, WasmCompileError> {
    compile_private_programs_in_modules(
        entries,
        layout,
        table,
        (PRIVATE_MODULE_BYTES, SYNC_COMPILE_BYTES),
    )
}

fn compile_private_programs_in_modules(
    entries: &[(&ScalarProgramBlock, Option<usize>)],
    layout: &VarLayout,
    table: &SolvePureCallTable,
    (budget, limit): (usize, usize),
) -> Result<Vec<CompiledPrivateProgramWasm>, WasmCompileError> {
    layout
        .validate_shape_contract()
        .map_err(|error| WasmCompileError::Input(error.to_string()))?;
    let mut cache = crate::emit::AdmissionCache::default();
    let admitted = entries
        .iter()
        .map(|&(block, seed)| {
            crate::emit::admit_private_entry(block, seed, layout, table, &mut cache, limit)
        })
        .collect::<Result<Vec<_>, _>>()
        .map_err(WasmCompileError::Backend)?;
    let entries = entries
        .iter()
        .zip(&admitted)
        .map(|(&(block, seed), admitted)| (block, seed, admitted))
        .collect::<Vec<_>>();
    compile_admitted(&entries, layout, table, (budget, limit))
}

fn compile_admitted(
    entries: &[(
        &ScalarProgramBlock,
        Option<usize>,
        &crate::emit::AdmittedEntry,
    )],
    layout: &VarLayout,
    table: &SolvePureCallTable,
    sizes: (usize, usize),
) -> Result<Vec<CompiledPrivateProgramWasm>, WasmCompileError> {
    if entries.is_empty() {
        return Err(WasmCompileError::Input(
            "a private module needs at least one program".into(),
        ));
    }
    let modules = crate::emit::emit_private_entries_modules(entries, layout, table, sizes)
        .map_err(WasmCompileError::Backend)?;
    let mut sources = entries.iter();
    let mut compiled = Vec::with_capacity(entries.len());
    for (module, artifacts) in modules {
        let exports = artifacts
            .iter()
            .map(|artifact| artifact.export.as_str())
            .collect::<Vec<_>>();
        #[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
        let runtimes = crate::WasmKernelRuntime::new_entries(module.module_bytes(), &exports)?;
        #[cfg(all(feature = "host-engine", not(target_arch = "wasm32")))]
        let runtimes =
            crate::host_runtime::HostKernelRuntime::new_entries(module.module_bytes(), &exports)?;
        #[cfg(not(any(
            all(target_arch = "wasm32", target_os = "unknown"),
            all(feature = "host-engine", not(target_arch = "wasm32"))
        )))]
        let runtimes = vec![(); exports.len()];
        let module = Rc::new(module);
        for ((artifact, runtime), &(block, seed_len, _)) in
            artifacts.into_iter().zip(runtimes).zip(sources.by_ref())
        {
            #[cfg(not(any(
                all(target_arch = "wasm32", target_os = "unknown"),
                all(feature = "host-engine", not(target_arch = "wasm32"))
            )))]
            let () = runtime;
            let seed = match (seed_len, artifact.seed_offset) {
                (Some(count), Some(offset)) => Some(directional::SeedStorage {
                    offset: offset as usize / 8,
                    count,
                }),
                (None, None) => None,
                _ => {
                    return Err(WasmCompileError::Backend(
                        "private entry seed storage differs from its request".into(),
                    ));
                }
            };
            compiled.push(CompiledPrivateProgramWasm {
                module: Rc::clone(&module),
                export: artifact.export,
                gather_faults: artifact.gather_faults,
                linear_solve_faults: artifact.linear_solve_faults,
                y: layout.y_scalars(),
                p: layout.p_scalars(),
                outputs: block.stored_output_count(),
                seed,
                #[cfg(any(
                    all(target_arch = "wasm32", target_os = "unknown"),
                    all(feature = "host-engine", not(target_arch = "wasm32"))
                ))]
                runtime,
            });
        }
    }
    if compiled.len() != entries.len() {
        return Err(WasmCompileError::Backend(
            "private modules returned other than one entry per program".into(),
        ));
    }
    Ok(compiled)
}

/// Admission of programs whose shared modules ([`PrivateProgramAdmission::compile`])
/// are compiled later. Each admitted program meets every refusal its module
/// would raise for it alone, including a module too large for a browser to
/// compile; call-owner linkage is computed once per distinct call-site set and
/// reused for the rest of this admission's requests.
pub struct PrivateProgramAdmission {
    layout: VarLayout,
    table: Rc<SolvePureCallTable>,
    // The layout is checked once, on the first admission.
    checked_layout: std::cell::OnceCell<Result<(), String>>,
    cache: std::cell::RefCell<crate::emit::AdmissionCache>,
    sizes: (usize, usize),
}

/// What admission measured of one program, which its shared module is packed by.
#[derive(Clone, Debug)]
pub struct AdmittedPrivateProgram(crate::emit::AdmittedEntry);

impl PrivateProgramAdmission {
    pub fn new(layout: VarLayout, table: Rc<SolvePureCallTable>) -> Self {
        Self::with_sizes(layout, table, (PRIVATE_MODULE_BYTES, SYNC_COMPILE_BYTES))
    }

    fn with_sizes(layout: VarLayout, table: Rc<SolvePureCallTable>, sizes: (usize, usize)) -> Self {
        Self {
            layout,
            table,
            checked_layout: std::cell::OnceCell::new(),
            cache: Default::default(),
            sizes,
        }
    }

    /// Compile admitted programs, in order, into shared modules.
    pub fn compile(
        &self,
        entries: &[(&ScalarProgramBlock, Option<usize>, &AdmittedPrivateProgram)],
    ) -> Result<Vec<CompiledPrivateProgramWasm>, WasmCompileError> {
        let entries = entries
            .iter()
            .map(|&(block, seed, admitted)| (block, seed, &admitted.0))
            .collect::<Vec<_>>();
        compile_admitted(&entries, &self.layout, &self.table, self.sizes)
    }

    /// `Some(seed_len)` admits a directional entry reading that many seed coordinates.
    pub fn admit(
        &self,
        block: &ScalarProgramBlock,
        seed_len: Option<usize>,
    ) -> Result<AdmittedPrivateProgram, WasmCompileError> {
        self.checked_layout
            .get_or_init(|| {
                self.layout
                    .validate_shape_contract()
                    .map_err(|error| error.to_string())
            })
            .clone()
            .map_err(WasmCompileError::Input)?;
        let mut cache = self
            .cache
            .try_borrow_mut()
            .map_err(|_| WasmCompileError::Backend("private admission is reentrant".into()))?;
        crate::emit::admit_private_entry(
            block,
            seed_len,
            &self.layout,
            &self.table,
            &mut cache,
            self.sizes.1,
        )
        .map(AdmittedPrivateProgram)
        .map_err(WasmCompileError::Backend)
    }
}

fn single(
    mut compiled: Vec<CompiledPrivateProgramWasm>,
) -> Result<CompiledPrivateProgramWasm, WasmCompileError> {
    match (compiled.pop(), compiled.is_empty()) {
        (Some(entry), true) => Ok(entry),
        _ => Err(WasmCompileError::Backend(
            "private module returned other than one entry".into(),
        )),
    }
}

#[cfg(all(test, feature = "host-engine"))]
mod shared_module_tests;
#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_ir_solve::{LinearOp, SolveArithmeticProfile, SolveIntegerDomain, SolveRealFormat};

    #[test]
    fn the_private_tuple_holds_every_stored_output_of_the_block() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("private_program.mo"),
            0,
            1,
        );
        let program = vec![
            LinearOp::LoadY { dst: 0, index: 0 },
            LinearOp::StoreOutput { src: 0 },
            LinearOp::LoadP { dst: 1, index: 0 },
            LinearOp::StoreOutput { src: 1 },
            LinearOp::Const { dst: 2, value: 2. },
            LinearOp::StoreOutput { src: 2 },
        ];
        let block = ScalarProgramBlock::with_program_spans(vec![program], vec![span]).unwrap();
        let layout = VarLayout::from_parts(Default::default(), 1, 1);
        let calls = SolvePureCallTable::builder(SolveArithmeticProfile::construct(
            SolveRealFormat::Binary64,
            SolveIntegerDomain::FULL,
        ))
        .finish();
        let compiled = compile_private_program_wasm(&block, &layout, &calls).unwrap();
        assert_eq!(compiled.output_count(), block.stored_output_count());
        assert_eq!(compiled.output_count(), 3);
    }
}
