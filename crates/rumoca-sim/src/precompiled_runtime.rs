//! Binary transport of checked construction inputs and original emitted programs.
//! No new derivative, storage, backend or numerical decisions.
use crate::{SimOptions, SimulationDiagnosticError, SimulationSessionLease};
use serde::{Serialize, de::DeserializeOwned};
fn refusal(message: impl ToString) -> SimulationDiagnosticError {
    SimulationDiagnosticError::RuntimePreparation {
        message: message.to_string(),
        span: None,
    }
}
#[cfg(all(target_arch = "wasm32", feature = "solver-rk45"))]
mod browser {
    use super::*;
    use crate::wasm_execution::precompiled::{CorePrograms, ProgramScope};
    use rumoca_ir_solve::fmi::FmiComponent;
    use serde::Deserialize;
    use std::io::Cursor;

    #[derive(Serialize)]
    struct RuntimeWireRef<'a, T> {
        version: u32,
        context: T,
        component: rumoca_eval_solve::fmi_wire::FmiComponentWireRef<'a>,
        programs: CorePrograms,
    }
    #[derive(Deserialize)]
    #[serde(deny_unknown_fields)]
    struct RuntimeWire<T> {
        #[serde(deserialize_with = "current_version")]
        version: u32,
        context: T,
        #[serde(deserialize_with = "rumoca_eval_solve::fmi_wire::deserialize_fmi_component")]
        component: FmiComponent,
        programs: CorePrograms,
    }
    // Reject an unsupported producer version as soon as its field is read,
    // before replaying the component that follows it in the canonical wire.
    fn current_version<'de, D: serde::Deserializer<'de>>(reader: D) -> Result<u32, D::Error> {
        let version = u32::deserialize(reader)?;
        if version != 2 {
            return Err(serde::de::Error::custom(
                "unsupported precompiled runtime schema",
            ));
        }
        Ok(version)
    }
    fn encode(value: &impl Serialize) -> Result<Vec<u8>, SimulationDiagnosticError> {
        let mut bytes = Vec::new();
        // Named maps preserve optional/default fields and checked table read order.
        value
            .serialize(&mut rmp_serde::Serializer::new(&mut bytes).with_struct_map())
            .map_err(refusal)?;
        Ok(bytes)
    }
    fn replay(bytes: &[u8]) -> Result<FmiComponent, SimulationDiagnosticError> {
        let mut reader = rmp_serde::Deserializer::new(Cursor::new(bytes));
        let component =
            rumoca_eval_solve::fmi_wire::deserialize_fmi_component(&mut reader).map_err(refusal)?;
        if reader.get_ref().position() != bytes.len() as u64 {
            return Err(refusal("trailing component bytes"));
        }
        Ok(component)
    }
    fn session(
        component: FmiComponent,
        opts: SimOptions,
    ) -> Result<crate::SimulationSession, SimulationDiagnosticError> {
        let root = component.runtime_view().retain_assertion_root();
        let backend =
            crate::wasm_execution::admitted_native_execution_backend(&opts, root.model())?;
        let receipt = crate::wasm_execution::execution_receipt(&opts, root.model());
        let artifact =
            rumoca_solver::fmi_me::MeModelArtifact::new(component).with_execution_receipt(receipt);
        crate::SimulationSession::from_prepared_rk(artifact, opts, backend)
    }
    pub(super) fn export<T: Serialize>(
        dae: &rumoca_ir_dae::Dae,
        opts: SimOptions,
        context: T,
    ) -> Result<Vec<u8>, SimulationDiagnosticError> {
        if opts.solver_mode != crate::SimSolverMode::RkLike {
            return Err(refusal(
                "precompiled browser product requires the declared rk-like profile",
            ));
        }
        crate::backend_options::validate(&opts)?;
        let component_bytes = {
            let lowered =
                crate::solve_lowering::lower_correlated_for_simulation_with_overrides(dae, &opts)?;
            let component = crate::solve_lowering::finish_component(lowered)?;
            let root = component.runtime_view().retain_assertion_root();
            encode(
                &rumoca_eval_solve::fmi_wire::retained_fmi_component_wire(&root)
                    .map_err(refusal)?,
            )?
        };
        // Export and browser load both use the same canonical checked replay.
        let replayed = replay(&component_bytes)?;
        drop(component_bytes);
        let root = replayed.runtime_view().retain_assertion_root();
        let scope = ProgramScope::capture().map_err(refusal)?;
        let prepared = session(replayed, opts)?;
        let _ = prepared.state()?;
        drop(prepared);
        let programs = scope.finish_capture().map_err(refusal)?;
        encode(&RuntimeWireRef {
            version: 2,
            context,
            component: rumoca_eval_solve::fmi_wire::retained_fmi_component_wire(&root)
                .map_err(refusal)?,
            programs,
        })
    }
    pub(super) fn load<T: DeserializeOwned>(
        payload: &[u8],
        configure: impl FnOnce(T) -> SimOptions,
    ) -> Result<SimulationSessionLease, SimulationDiagnosticError> {
        let parse = rumoca_core::browser_profile::Span::new("decode_binary_and_checked_replay");
        let mut reader = rmp_serde::Deserializer::new(Cursor::new(payload));
        let wire = RuntimeWire::<T>::deserialize(&mut reader).map_err(refusal)?;
        if reader.get_ref().position() != payload.len() as u64 {
            return Err(refusal("trailing runtime bytes"));
        }
        drop(parse);
        if wire.version != 2 {
            return Err(refusal("unsupported precompiled runtime schema"));
        }
        let scope = ProgramScope::load(wire.programs).map_err(refusal)?;
        let initialization = rumoca_core::browser_profile::Span::new("session_construct_and_init");
        let prepared = session(wire.component, configure(wire.context))?;
        drop(initialization);
        let initial_state = rumoca_core::browser_profile::Span::new("initial_observation");
        let _ = prepared.state()?;
        drop(initial_state);
        scope.finish_load().map_err(refusal)?;
        Ok(SimulationSessionLease::from_prepared(prepared))
    }
}
/// Build one binary payload with typed context and component (no escaped JSON).
pub fn export_precompiled_runtime<T: Serialize>(
    dae: &rumoca_ir_dae::Dae,
    opts: SimOptions,
    context: T,
) -> Result<Vec<u8>, SimulationDiagnosticError> {
    #[cfg(all(target_arch = "wasm32", feature = "solver-rk45"))]
    {
        browser::export(dae, opts, context)
    }
    #[cfg(not(all(target_arch = "wasm32", feature = "solver-rk45")))]
    {
        let _ = (dae, opts, context);
        Err(refusal(
            "precompiled browser export requires the wasm32 rk-like build",
        ))
    }
}
/// Read the same checked wire and bind the original complete module inventory.
pub fn load_precompiled_runtime<T: DeserializeOwned>(
    payload: &[u8],
    configure: impl FnOnce(T) -> SimOptions,
) -> Result<SimulationSessionLease, SimulationDiagnosticError> {
    #[cfg(all(target_arch = "wasm32", feature = "solver-rk45"))]
    {
        browser::load(payload, configure)
    }
    #[cfg(not(all(target_arch = "wasm32", feature = "solver-rk45")))]
    {
        let _ = (payload, configure);
        Err(refusal(
            "precompiled browser load requires the wasm32 rk-like build",
        ))
    }
}
