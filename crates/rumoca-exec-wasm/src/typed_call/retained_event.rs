//! Source-owned event execution keeps actual invocation carriers private.
#[cfg(all(test, feature = "host-engine", not(target_arch = "wasm32")))]
mod tests;
use super::observations::ObservationPlan;
use super::*;
use crate::{CompiledNativeCallProgramWasm, WasmCompileError};
use std::cell::RefCell;

#[derive(Clone)]
pub(crate) struct InvocationStorage {
    pub path: Vec<(usize, usize)>,
    pub operation: usize,
    pub input: u32,
    pub input_bytes: u32,
    pub output: u32,
    pub output_bytes: u32,
    pub scratch: u32,
    pub scratch_bytes: u32,
    pub status: u32,
    pub carrier: Option<ObservationPlan>,
}

impl InvocationStorage {
    pub(crate) fn new(
        path: Vec<(usize, usize)>,
        operation: usize,
        layout: &program::CallLayout,
        carrier: Option<ObservationPlan>,
        next: &mut u32,
    ) -> Result<Self, String> {
        let mut reserve = |bytes| -> Result<u32, String> {
            let start = *next;
            *next = next
                .checked_add(bytes)
                .ok_or("WASM event carrier exceeds memory32")?;
            crate::CheckedNativeWasmCapacityPolicy::default().require_public_bytes(*next)?;
            Ok(start)
        };
        Ok(Self {
            path,
            operation,
            input: reserve(layout.input)?,
            input_bytes: layout.input,
            output: reserve(layout.output)?,
            output_bytes: layout.output,
            scratch: reserve(layout.scratch)?,
            scratch_bytes: layout.scratch,
            status: reserve(8)?,
            carrier,
        })
    }
}

pub struct CompiledRetainedEventWasm {
    source: solve::RetainedEventProgram,
    artifact: CompiledNativeCallProgramWasm,
    invocations: Vec<InvocationStorage>,
    scratch: RefCell<Vec<u64>>,
    #[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
    runtime: crate::WasmKernelRuntime,
    #[cfg(all(feature = "host-engine", not(target_arch = "wasm32")))]
    runtime: crate::host_runtime::HostKernelRuntime,
}

impl CompiledRetainedEventWasm {
    pub fn compile(source: solve::RetainedEventProgram) -> Result<Self, WasmCompileError> {
        if !cfg!(any(
            all(target_arch = "wasm32", target_os = "unknown"),
            all(feature = "host-engine", not(target_arch = "wasm32"))
        )) {
            return Err(WasmCompileError::Input(
                "event programs have no admitted engine on this target".into(),
            ));
        }
        let layout = &source.source().model().problem.layout;
        let (artifact, invocations) = crate::emit::emit_event_program_module(&source, layout)
            .map_err(WasmCompileError::Backend)?;
        #[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
        let runtime = crate::WasmKernelRuntime::new_with_arena(
            artifact.module_bytes(),
            "eval_private",
            artifact.pooled_arena_bytes,
        )?;
        #[cfg(all(feature = "host-engine", not(target_arch = "wasm32")))]
        let runtime = crate::host_runtime::HostKernelRuntime::new_named(
            artifact.module_bytes(),
            "eval_private",
        )?;
        let scratch = RefCell::new(vec![0; artifact.scratch_bytes() as usize / 8]);
        Ok(Self {
            source,
            artifact,
            invocations,
            scratch,
            #[cfg(any(
                all(target_arch = "wasm32", target_os = "unknown"),
                all(feature = "host-engine", not(target_arch = "wasm32"))
            ))]
            runtime,
        })
    }

    pub fn source(&self) -> &solve::RetainedEventProgram {
        &self.source
    }

    pub fn call(
        &self,
        y: &[f64],
        p: &[f64],
        time: f64,
        out: &mut [f64],
        mut observe: impl FnMut(
            &solve::RetainedAssertionAction<'_>,
            &[(usize, NativeAssertionCells)],
            rumoca_core::Span,
            &solve::SolvePureCallOwner,
        ) -> Result<(), WasmCompileError>,
    ) -> Result<(), WasmCompileError> {
        let model = self.source.source().model();
        let outputs = solve::ScalarProgramBlock::program_output_count(self.source.program());
        if y.len() != model.problem.layout.y_scalars()
            || p.len() != model.problem.layout.p_scalars()
            || out.len() != outputs
        {
            return Err(WasmCompileError::Input(
                "WASM event source buffer footprint differs".into(),
            ));
        }
        let mut scratch = self.scratch.borrow_mut();
        let status = self.execute(y, p, time, &mut scratch)?;
        if status != 0 {
            return Err(self.artifact.status_error(status));
        }
        self.observe(&scratch, &mut observe)?;
        for (value, bits) in out.iter_mut().zip(scratch.iter()) {
            *value = f64::from_bits(*bits);
        }
        Ok(())
    }

    fn execute(
        &self,
        y: &[f64],
        p: &[f64],
        time: f64,
        scratch: &mut [u64],
    ) -> Result<u32, WasmCompileError> {
        #[cfg(any(
            all(target_arch = "wasm32", target_os = "unknown"),
            all(feature = "host-engine", not(target_arch = "wasm32"))
        ))]
        {
            self.runtime
                .call_native(&mut y.to_vec(), p, time, scratch, &mut [])
        }
        #[cfg(not(any(
            all(target_arch = "wasm32", target_os = "unknown"),
            all(feature = "host-engine", not(target_arch = "wasm32"))
        )))]
        {
            let _ = (y, p, time, scratch);
            Err(WasmCompileError::Input(
                "event programs have no admitted engine on this target".into(),
            ))
        }
    }

    fn observe(
        &self,
        scratch: &[u64],
        observe: &mut impl FnMut(
            &solve::RetainedAssertionAction<'_>,
            &[(usize, NativeAssertionCells)],
            rumoca_core::Span,
            &solve::SolvePureCallOwner,
        ) -> Result<(), WasmCompileError>,
    ) -> Result<(), WasmCompileError> {
        for storage in &self.invocations {
            let status = scratch[storage.status as usize / 8];
            if status == u64::MAX {
                continue;
            }
            let invocation = self
                .source
                .invocation_at(&storage.path, storage.operation)
                .ok_or_else(|| {
                    WasmCompileError::Backend(
                        "event invocation lost its exact source binding".into(),
                    )
                })?;
            let checked = invocation.checked();
            let cells =
                |start: u32, bytes: u32| &scratch[start as usize / 8..(start + bytes) as usize / 8];
            let decoder = checked_assertions::decode::Decoder {
                invocation: checked,
                faults: self.artifact.faults(),
                carrier: storage.carrier.as_ref(),
            };
            let decoded = decoder.decode_execution(
                cells(storage.input, storage.input_bytes),
                cells(storage.output, storage.output_bytes),
                cells(storage.scratch, storage.scratch_bytes),
                u32::try_from(status).map_err(|_| {
                    WasmCompileError::Backend("event invocation status is not an i32".into())
                })?,
            )?;
            let reports = match decoded {
                CheckedAssertionWasmOutcome::Complete { observations, .. }
                | CheckedAssertionWasmOutcome::Stopped { observations } => observations,
                CheckedAssertionWasmOutcome::Fault { fault, .. } => {
                    return Err(WasmCompileError::TypedSource(fault));
                }
            };
            let missing_projection = || {
                WasmCompileError::Backend("event record lost its exact action projection".into())
            };
            for report in reports {
                let action = invocation
                    .projections()
                    .find(|projection| projection.action_index() == report.action().action_index())
                    .ok_or_else(missing_projection)?;
                observe(
                    &action,
                    report.captures(),
                    report.source_span(),
                    invocation.owner(),
                )?;
            }
        }
        Ok(())
    }
}
