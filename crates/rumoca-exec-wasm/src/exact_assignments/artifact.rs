//! Mechanical persisted form of the original emitted assignment module.
#[cfg(test)]
mod tampering;
#[cfg(all(test, feature = "host-engine"))]
mod tests;
use super::*;
use serde::{Deserialize, Serialize};

#[derive(Clone, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct PrecompiledExactAssignments {
    version: u32,
    source_key: String,
    payload: crate::program_payload::ProgramPayload,
    y: usize,
    p: usize,
    gather: Vec<crate::NativeGatherFault>,
    linear: Vec<crate::NativeLinearSolveFault>,
}

impl PrecompiledExactAssignments {
    pub fn from_compiled(program: &CompiledExactAssignmentWasm) -> Self {
        Self {
            version: 1,
            source_key: program.source_key.clone(),
            payload: program.artifact.clone(),
            y: program.y_count,
            p: program.p_count,
            gather: program.gather_faults.clone(),
            linear: program.linear_solve_faults.clone(),
        }
    }

    /// Bind a captured schedule to the actual source, owners and selected
    /// schedule of the current build. `Certify` derives the key and the
    /// emission again and refuses any difference; `Trusted` binds the carried
    /// program after its structural checks.
    pub fn bind(
        &self,
        binding: crate::SavedBinding,
        source: &ComputeBlock,
        owners: &ContinuousRefreshOwners,
        schedule: &ExactRefreshAssignmentSchedule,
        layout: &VarLayout,
        table: &SolvePureCallTable,
        #[cfg(all(
            feature = "fixed-scalar-linked-receiver",
            target_arch = "wasm32",
            target_os = "wasi",
            target_env = "p2"
        ))]
        at: crate::LinkedPosition<'_>,
    ) -> Result<CompiledExactAssignmentWasm, WasmCompileError> {
        let emitted = match binding {
            crate::SavedBinding::Certify => {
                let expected = rumoca_ir_solve::PreparedKernelSource::new(layout, table)
                    .and_then(|key| key.exact_assignment_schedule(source, owners, schedule))
                    .map_err(WasmCompileError::Input)?;
                self.check_header(Some(&expected), layout)?;
                Some(
                    crate::emit::emit_exact_assignment_module(
                        source,
                        owners,
                        schedule,
                        layout,
                        table,
                        crate::emit::Linkage::Browser,
                    )
                    .map_err(WasmCompileError::Backend)?,
                )
            }
            crate::SavedBinding::Trusted => {
                self.check_header(None, layout)?;
                None
            }
        };
        self.bind_emitted(
            emitted,
            #[cfg(all(
                feature = "fixed-scalar-linked-receiver",
                target_arch = "wasm32",
                target_os = "wasi",
                target_env = "p2"
            ))]
            (at, crate::LinkedEntryKind::Exact),
        )
    }

    /// Bind captured issued rows to the same issued rows of the current build,
    /// with [`Self::bind`]'s `binding`.
    pub fn bind_issued(
        &self,
        binding: crate::SavedBinding,
        rows: &IssuedAssignmentRows,
        layout: &VarLayout,
        table: &SolvePureCallTable,
        #[cfg(all(
            feature = "fixed-scalar-linked-receiver",
            target_arch = "wasm32",
            target_os = "wasi",
            target_env = "p2"
        ))]
        at: crate::LinkedPosition<'_>,
    ) -> Result<CompiledExactAssignmentWasm, WasmCompileError> {
        let emitted = match binding {
            crate::SavedBinding::Certify => {
                let expected = rumoca_ir_solve::PreparedKernelSource::new(layout, table)
                    .and_then(|key| key.issued_assignment_rows(rows))
                    .map_err(WasmCompileError::Input)?;
                self.check_header(Some(&expected), layout)?;
                Some(
                    crate::emit::emit_issued_assignment_module(
                        rows,
                        layout,
                        table,
                        crate::emit::Linkage::Browser,
                    )
                    .map_err(WasmCompileError::Backend)?,
                )
            }
            crate::SavedBinding::Trusted => {
                self.check_header(None, layout)?;
                None
            }
        };
        self.bind_emitted(
            emitted,
            #[cfg(all(
                feature = "fixed-scalar-linked-receiver",
                target_arch = "wasm32",
                target_os = "wasi",
                target_env = "p2"
            ))]
            (at, crate::LinkedEntryKind::Issued),
        )
    }

    /// The carried header against the layout being bound, and, when
    /// certifying, the key derived again from the checked source.
    fn check_header(
        &self,
        expected: Option<&str>,
        layout: &VarLayout,
    ) -> Result<(), WasmCompileError> {
        if self.version != 1
            || expected.is_some_and(|expected| self.source_key != expected)
            || self.y != layout.y_scalars()
            || self.p != layout.p_scalars()
            || self.payload.module_bytes().len() > (8 << 20)
            || self.payload.scratch_bytes()
                > crate::CheckedNativeWasmCapacityPolicy::default().public_scratch_bytes()
        {
            return Err(WasmCompileError::Input(
                "precompiled assignments belong to another checked source or extent".into(),
            ));
        }
        Ok(())
    }

    /// When certifying, compare the saved program with the current source's
    /// emission, which activates nothing, before any saved code is linked; a
    /// difference is refused, never replaced by the fresh emission. A linked
    /// guest then binds the issued entry at its position, which must carry
    /// this program's key, scratch extent and fault tables.
    fn bind_emitted(
        &self,
        emitted: Option<crate::native_program::CompiledNativeCallProgramWasm>,
        #[cfg(all(
            feature = "fixed-scalar-linked-receiver",
            target_arch = "wasm32",
            target_os = "wasi",
            target_env = "p2"
        ))]
        (at, kind): (crate::LinkedPosition<'_>, crate::LinkedEntryKind),
    ) -> Result<CompiledExactAssignmentWasm, WasmCompileError> {
        if let Some(emitted) = emitted
            && (self.payload.scratch_bytes() > emitted.capacity_policy.public_scratch_bytes()
                || self.gather != emitted.gather_faults
                || self.linear != emitted.linear_solve_faults
                || !self.payload.same_emission(&emitted.into()))
        {
            return Err(WasmCompileError::Input(
                "precompiled assignments differ from their authoritative issued emission".into(),
            ));
        }
        #[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
        let runtime =
            crate::WasmKernelRuntime::new_named(self.payload.module_bytes(), "eval_assignments")?;
        #[cfg(all(
            feature = "fixed-scalar-linked-receiver",
            target_arch = "wasm32",
            target_os = "wasi",
            target_env = "p2"
        ))]
        let runtime = crate::WasmKernelRuntime::issued(
            at,
            kind,
            &self.source_key,
            self.payload.scratch_bytes(),
            self.payload.fault_tables(),
        )?;
        Ok(CompiledExactAssignmentWasm {
            artifact: self.payload.clone(),
            gather_faults: self.gather.clone(),
            linear_solve_faults: self.linear.clone(),
            source_key: self.source_key.clone(),
            y_count: self.y,
            p_count: self.p,
            #[cfg(any(
                all(target_arch = "wasm32", target_os = "unknown"),
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
