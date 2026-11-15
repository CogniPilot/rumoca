//! The fixed scalar FMI guest's load of a saved product. It has no engine to
//! instantiate the saved modules: each saved program binds, as it is
//! requested, to the entry the guest issues from the same checked source,
//! reached through the issued dispatch. The open scope is closed by
//! certifying the linked artifact against everything the guest issued.
use super::*;

/// An open load of one saved product. Every program its component requests
/// while this is open binds to its linked entry, certified first; nothing
/// compiles. The admission it holds pinned the saved digest at open.
pub struct LinkedGuestLoad {
    scope: ProgramScope,
}

/// Admit the linked artifact for the saved product before any backend exists
/// (its entry shape, and when certifying its digest, dispatch regenerated
/// from its own entries and module exports), then read the saved product and
/// open a scope over its saved programs.
/// The context the browser load configures from is not read; the guest's
/// instance configuration comes from the FMI host.
///
/// The guest constructs the component it publishes (`published`, the replay
/// its modelDescription and capsule describe), never the saved one: the saved
/// component must have the same canonical wire bytes, so the saved programs
/// and the linked artifact belong to exactly the published model; the build
/// certified that, and only a certifying load compares it again.
///
/// `certify` selects the build-time certification load: every requested
/// program is issued again from its checked source and certified, and the
/// saved component is compared with the published one. A published product
/// was certified by its build, so its ordinary load binds by position into
/// the admitted artifact, and its binders key, emit and compare nothing again
/// (SPEC_0044 owner decision, Nov 15: no per-entry regeneration on load).
pub fn load_linked_guest(
    saved: &[u8],
    artifact: &[u8],
    published: &FmiComponent,
    certify: bool,
) -> Result<LinkedGuestLoad, SimulationDiagnosticError> {
    let mut reader = rmp_serde::Deserializer::new(Cursor::new(artifact));
    let carried = rumoca_exec_wasm::LinkedArtifact::deserialize(&mut reader).map_err(refusal)?;
    if reader.get_ref().position() != artifact.len() as u64 {
        return Err(refusal("trailing linked artifact bytes"));
    }
    // Only a certifying load (the builder's) derives the saved digest, the
    // dispatch and the export coverage again; a build-certified load admits
    // the carried plan's structure.
    let admission = if certify {
        carried.admit(saved)
    } else {
        carried.admit_trusted()
    }
    .map_err(refusal)?;
    let mut reader = rmp_serde::Deserializer::new(Cursor::new(saved));
    let wire = RuntimeWire::<serde::de::IgnoredAny>::deserialize(&mut reader).map_err(refusal)?;
    if reader.get_ref().position() != saved.len() as u64 {
        return Err(refusal("trailing runtime bytes"));
    }
    if wire.version != 3 {
        return Err(refusal("unsupported precompiled runtime schema"));
    }
    let canonical = |component: &FmiComponent| {
        let root = component.runtime_view().retain_assertion_root();
        encode(&rumoca_eval_solve::fmi_wire::retained_fmi_component_wire(&root).map_err(refusal)?)
    };
    if certify && canonical(&wire.component)? != canonical(published)? {
        return Err(refusal(
            "the saved product's component differs from the published component",
        ));
    }
    let scope = ProgramScope::load_linked(wire.programs, admission, certify).map_err(refusal)?;
    Ok(LinkedGuestLoad { scope })
}

impl LinkedGuestLoad {
    /// The compiled backend for `model`. Its programs come only from the
    /// saved product through this load; a model with no compiled admission
    /// is refused, never interpreted.
    pub fn execution_backend(
        &self,
        model: &rumoca_ir_solve::SolveModel,
    ) -> Result<rumoca_solver::fmi_me::MeExecutionBackend, SimulationDiagnosticError> {
        let opts = SimOptions {
            execution_policy: rumoca_solver::SimExecutionPolicy::NativeRequired,
            ..SimOptions::default()
        };
        crate::wasm_execution::admitted_native_execution_backend(&opts, model)?
            .ok_or_else(|| refusal("the linked guest model has no compiled admission"))
    }

    /// Close the load: every saved program was requested and bound, and the
    /// whole product this guest issued equals the admitted artifact.
    pub fn finish(self) -> Result<(), SimulationDiagnosticError> {
        self.scope.finish_load_linked().map_err(refusal)
    }
}
