//! The fixed scalar FMI guest's load of a saved product. It has no engine to
//! instantiate the saved modules: each saved program binds, as it is
//! requested, to the entry the guest issues from the same checked source,
//! reached through the issued dispatch. The open scope is closed by
//! certifying the linked artifact against everything the guest issued.
use super::*;

/// An open load of one saved product. Every program its component requests
/// while this is open binds to its linked entry; nothing compiles.
/// It keeps the saved bytes it was opened with, so it certifies against
/// exactly the product its programs came from.
pub struct LinkedGuestLoad<'a> {
    scope: ProgramScope,
    saved: &'a [u8],
    artifact: rumoca_exec_wasm::LinkedArtifact,
}

/// Read the saved product and the linked artifact before any backend exists:
/// the component's checked replay, and an open scope over the saved programs.
/// The context the browser load configures from is not read; the guest's
/// instance configuration comes from the FMI host.
pub fn load_linked_guest<'a>(
    saved: &'a [u8],
    artifact: &[u8],
) -> Result<(LinkedGuestLoad<'a>, FmiComponent), SimulationDiagnosticError> {
    let mut reader = rmp_serde::Deserializer::new(Cursor::new(artifact));
    let carried = rumoca_exec_wasm::LinkedArtifact::deserialize(&mut reader).map_err(refusal)?;
    if reader.get_ref().position() != artifact.len() as u64 {
        return Err(refusal("trailing linked artifact bytes"));
    }
    let mut reader = rmp_serde::Deserializer::new(Cursor::new(saved));
    let wire = RuntimeWire::<serde::de::IgnoredAny>::deserialize(&mut reader).map_err(refusal)?;
    if reader.get_ref().position() != saved.len() as u64 {
        return Err(refusal("trailing runtime bytes"));
    }
    if wire.version != 3 {
        return Err(refusal("unsupported precompiled runtime schema"));
    }
    let scope = ProgramScope::load(wire.programs).map_err(refusal)?;
    Ok((
        LinkedGuestLoad {
            scope,
            saved,
            artifact: carried,
        },
        wire.component,
    ))
}

impl LinkedGuestLoad<'_> {
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
    /// linked artifact is exactly what this guest issued for the saved
    /// product bytes this load was opened with.
    pub fn finish(self) -> Result<(), SimulationDiagnosticError> {
        self.scope
            .finish_load_linked(self.saved, &self.artifact)
            .map_err(refusal)
    }
}
