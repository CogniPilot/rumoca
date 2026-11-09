//! Original event modules and invocation carriers, rebound to checked event source.
use super::*;
use serde::{Deserialize, Serialize};

#[derive(Clone, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct PrecompiledRetainedEvent {
    version: u32,
    source_key: String,
    artifact: Rc<crate::program_payload::ProgramPayload>,
    invocations: Vec<InvocationStorage>,
}
impl PrecompiledRetainedEvent {
    pub fn from_compiled(program: &CompiledRetainedEventWasm) -> Self {
        Self {
            version: 1,
            source_key: program.source_key.clone(),
            artifact: Rc::clone(&program.artifact),
            invocations: program.invocations.clone(),
        }
    }
    pub fn bind(
        &self,
        source: solve::RetainedEventProgram,
    ) -> Result<CompiledRetainedEventWasm, WasmCompileError> {
        let key = solve::PreparedKernelSource::new(
            &source.source().model().problem.layout,
            source.table(),
        )
        .and_then(|key| key.retained_event(&source))
        .map_err(WasmCompileError::Input)?;
        let bytes = self.artifact.scratch_bytes();
        if self.version != 1
            || self.source_key != key
            || !bytes.is_multiple_of(8)
            || bytes > crate::CheckedNativeWasmCapacityPolicy::default().public_scratch_bytes()
            || self.artifact.module_bytes().len() > (8 << 20)
        {
            return Err(WasmCompileError::Input(
                "precompiled event source or extent differs".into(),
            ));
        }
        let unique = self
            .invocations
            .iter()
            .map(|storage| (&storage.path, storage.operation))
            .collect::<std::collections::BTreeSet<_>>();
        if self.invocations.len() != source.invocations().len()
            || unique.len() != self.invocations.len()
        {
            return Err(WasmCompileError::Input(
                "precompiled event invocation inventory differs".into(),
            ));
        }
        let range = |start: u32, count: u32| {
            start.is_multiple_of(8)
                && count.is_multiple_of(8)
                && start.checked_add(count).is_some_and(|end| end <= bytes)
        };
        for storage in &self.invocations {
            if source
                .invocation_at(&storage.path, storage.operation)
                .is_none()
                || !range(storage.input, storage.input_bytes)
                || !range(storage.output, storage.output_bytes)
                || !range(storage.scratch, storage.scratch_bytes)
                || !range(storage.status, 8)
            {
                return Err(WasmCompileError::Input(
                    "precompiled event invocation differs".into(),
                ));
            }
            let Some(carrier) = &storage.carrier else {
                continue;
            };
            let extent = carrier
                .capacity
                .checked_mul(carrier.stride)
                .and_then(|n| n.checked_add(8));
            if carrier.range.base != 2
                || !carrier.range.offset.is_multiple_of(8)
                || carrier.stride < 8
                || !carrier.stride.is_multiple_of(8)
                || extent != Some(carrier.range.bytes)
                || carrier
                    .range
                    .offset
                    .checked_add(carrier.range.bytes)
                    .is_none_or(|end| end > storage.scratch_bytes)
            {
                return Err(WasmCompileError::Input(
                    "precompiled event carrier extent differs".into(),
                ));
            }
        }
        #[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
        let runtime =
            crate::WasmKernelRuntime::new_named(self.artifact.module_bytes(), "eval_private")?;
        #[cfg(all(feature = "host-engine", not(target_arch = "wasm32")))]
        let runtime = crate::host_runtime::HostKernelRuntime::new_named(
            self.artifact.module_bytes(),
            "eval_private",
        )?;
        Ok(CompiledRetainedEventWasm {
            source,
            artifact: Rc::clone(&self.artifact),
            source_key: key,
            invocations: self.invocations.clone(),
            scratch: RefCell::new(vec![0; bytes as usize / 8]),
            #[cfg(any(
                all(target_arch = "wasm32", target_os = "unknown"),
                all(feature = "host-engine", not(target_arch = "wasm32"))
            ))]
            runtime,
        })
    }
}
