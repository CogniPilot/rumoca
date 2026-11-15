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
    /// Whether the emission of `artifact` issued the read-only-Y certificate.
    read_only_y: bool,
}
/// Whether two invocation tables are equal in every field (offsets,
/// extents, status word, carrier range, sources and forwarding), compared
/// through their one wire form.
#[cfg(any(
    test,
    all(
        feature = "fixed-scalar-linked-receiver",
        target_arch = "wasm32",
        target_os = "wasi",
        target_env = "p2"
    )
))]
pub(super) fn same_invocations(a: &[InvocationStorage], b: &[InvocationStorage]) -> bool {
    matches!(
        (serde_json::to_vec(a), serde_json::to_vec(b)),
        (Ok(a), Ok(b)) if a == b
    )
}

impl PrecompiledRetainedEvent {
    pub fn from_compiled(program: &CompiledRetainedEventWasm) -> Self {
        Self {
            version: 2,
            source_key: program.source_key.clone(),
            artifact: Rc::clone(&program.artifact),
            invocations: program.invocations.clone(),
            read_only_y: program.read_only_y.is_some(),
        }
    }
    /// Emit the module again from the source being bound: saved bytes equal
    /// to that emission carry the certificate it issued. Any other module, or
    /// a source the emitter refuses, runs on a copy of Y as before.
    #[cfg(not(all(
        feature = "fixed-scalar-linked-receiver",
        target_arch = "wasm32",
        target_os = "wasi",
        target_env = "p2"
    )))]
    fn emitted_read_only_y(
        &self,
        source: &solve::RetainedEventProgram,
    ) -> Option<crate::emit::ReadOnlyY> {
        let _emit = rumoca_core::browser_profile::Span::new("retained_event_bind_emit");
        let layout = &source.source().model().problem.layout;
        let (module, _, read_only_y) =
            crate::emit::emit_event_program_module(source, layout, crate::emit::Linkage::Browser)
                .ok()?;
        (module.bytes == self.artifact.module_bytes())
            .then_some(read_only_y)
            .flatten()
    }

    /// The read-only-Y certificate of a build-certified program, from the flag
    /// its certifying load checked against the emission.
    fn carried_read_only_y(&self) -> Option<crate::emit::ReadOnlyY> {
        self.read_only_y.then(crate::emit::ReadOnlyY::carried)
    }

    /// A linked guest binds the issued Event entry at `at`, which must carry
    /// this program's key, scratch extent and fault tables. `Certify` derives
    /// the key and, in a linked guest, the emission again and refuses any
    /// difference; `Trusted` binds the carried program, invocation storage and
    /// read-only-Y flag after their structural checks.
    pub fn bind(
        &self,
        binding: crate::SavedBinding,
        source: solve::RetainedEventProgram,
        #[cfg(all(
            feature = "fixed-scalar-linked-receiver",
            target_arch = "wasm32",
            target_os = "wasi",
            target_env = "p2"
        ))]
        at: crate::LinkedPosition<'_>,
    ) -> Result<CompiledRetainedEventWasm, WasmCompileError> {
        let certify = binding == crate::SavedBinding::Certify;
        let key = if certify {
            solve::PreparedKernelSource::new(
                &source.source().model().problem.layout,
                source.table(),
            )
            .and_then(|key| key.retained_event(&source))
            .map_err(WasmCompileError::Input)?
        } else {
            self.source_key.clone()
        };
        let bytes = self.artifact.scratch_bytes();
        if self.version != 2
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
        // A certifying linked guest decodes with invocation storage emitted
        // again from the checked source, never with the carried offsets and
        // carriers, and refuses saved bytes that differ from that emission.
        #[cfg(all(
            feature = "fixed-scalar-linked-receiver",
            target_arch = "wasm32",
            target_os = "wasi",
            target_env = "p2"
        ))]
        let (invocations, read_only_y) = if !certify {
            (self.invocations.clone(), self.carried_read_only_y())
        } else {
            let layout = &source.source().model().problem.layout;
            let (module, invocations, read_only_y) = crate::emit::emit_event_program_module(
                &source,
                layout,
                crate::emit::Linkage::Browser,
            )
            .map_err(WasmCompileError::Backend)?;
            // A build-certified load runs the carried table, so the
            // certification compares every field of it, not only its shape.
            if module.bytes != self.artifact.module_bytes()
                || read_only_y.is_some() != self.read_only_y
                || !same_invocations(&invocations, &self.invocations)
            {
                return Err(WasmCompileError::Input(
                    "precompiled event differs from its issued emission".into(),
                ));
            }
            (invocations, read_only_y)
        };
        #[cfg(not(all(
            feature = "fixed-scalar-linked-receiver",
            target_arch = "wasm32",
            target_os = "wasi",
            target_env = "p2"
        )))]
        let (invocations, read_only_y) = (
            self.invocations.clone(),
            if certify {
                self.emitted_read_only_y(&source)
            } else {
                self.carried_read_only_y()
            },
        );
        mark_y_census(read_only_y.as_ref());
        #[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
        let runtime =
            crate::WasmKernelRuntime::new_named(self.artifact.module_bytes(), "eval_private")?;
        #[cfg(all(feature = "host-engine", not(target_arch = "wasm32")))]
        let runtime = crate::host_runtime::HostKernelRuntime::new_named(
            self.artifact.module_bytes(),
            "eval_private",
        )?;
        #[cfg(all(
            feature = "fixed-scalar-linked-receiver",
            target_arch = "wasm32",
            target_os = "wasi",
            target_env = "p2"
        ))]
        let runtime = crate::WasmKernelRuntime::issued(
            at,
            crate::LinkedEntryKind::Event,
            &key,
            bytes,
            self.artifact.fault_tables(),
        )?;
        Ok(CompiledRetainedEventWasm {
            source,
            artifact: Rc::clone(&self.artifact),
            source_key: key,
            invocations,
            read_only_y,
            scratch: RefCell::new(vec![0; bytes as usize / 8]),
            #[cfg(any(
                all(target_arch = "wasm32", target_os = "unknown"),
                all(feature = "host-engine", not(target_arch = "wasm32")),
                all(
                    feature = "fixed-scalar-linked-receiver",
                    target_arch = "wasm32",
                    target_os = "wasi",
                    target_env = "p2"
                )
            ))]
            runtime,
        })
    }
}
