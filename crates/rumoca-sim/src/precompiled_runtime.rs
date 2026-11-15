//! Binary transport of checked construction inputs and original emitted programs.
//! No new derivative, storage, backend or numerical decisions.
use crate::{SimOptions, SimulationDiagnosticError, SimulationSessionLease};
use serde::{Serialize, de::DeserializeOwned};
#[cfg(all(test, not(target_arch = "wasm32")))]
mod tests;
fn refusal(message: impl ToString) -> SimulationDiagnosticError {
    SimulationDiagnosticError::RuntimePreparation {
        message: message.to_string(),
        span: None,
    }
}
/// The solver profiles a precompiled browser product may declare: the
/// explicit plugins of the rk45 session. Both evaluate only state derivatives
/// (neither requests a model state Jacobian), so they prepare and replay the
/// same program inventory.
#[cfg(any(test, all(target_arch = "wasm32", feature = "solver-rk45")))]
pub(crate) fn admits_solver_profile(mode: crate::SimSolverMode) -> bool {
    matches!(
        mode,
        crate::SimSolverMode::RkLike | crate::SimSolverMode::FixedStep
    )
}
#[cfg(all(
    feature = "linked-guest",
    target_arch = "wasm32",
    target_os = "wasi",
    target_env = "p2"
))]
pub use browser::linked_guest::{LinkedGuestLoad, load_linked_guest};
#[cfg(all(target_arch = "wasm32", feature = "solver-rk45"))]
mod browser {
    use super::*;
    #[cfg(all(
        feature = "linked-guest",
        target_arch = "wasm32",
        target_os = "wasi",
        target_env = "p2"
    ))]
    pub(super) mod linked_guest;
    use crate::wasm_execution::precompiled::{CorePrograms, ProgramScope};
    use rumoca_ir_solve::fmi::FmiComponent;
    use serde::Deserialize;
    use std::io::Cursor;

    #[cfg(feature = "lowering")]
    #[derive(Serialize)]
    struct RuntimeWireRef<'a, T> {
        version: u32,
        context: T,
        component: rumoca_eval_solve::fmi_wire::FmiComponentWireRef<'a>,
        programs: CorePrograms,
    }
    #[derive(Deserialize)]
    #[serde(deny_unknown_fields)]
    struct RuntimeWire<T, C = ReplayedComponent> {
        #[serde(deserialize_with = "current_version")]
        version: u32,
        context: T,
        component: C,
        programs: CorePrograms,
    }
    /// The checked replay of a saved component. A load that never reads the
    /// saved component (a build-certified linked load) names `IgnoredAny`
    /// instead, so it skips the bytes without replaying or validating them.
    struct ReplayedComponent(FmiComponent);
    impl<'de> Deserialize<'de> for ReplayedComponent {
        fn deserialize<D: serde::Deserializer<'de>>(reader: D) -> Result<Self, D::Error> {
            rumoca_eval_solve::fmi_wire::deserialize_fmi_component(reader).map(Self)
        }
    }
    // Reject an unsupported producer version as soon as its field is read,
    // before replaying the component that follows it in the canonical wire.
    fn current_version<'de, D: serde::Deserializer<'de>>(reader: D) -> Result<u32, D::Error> {
        let version = u32::deserialize(reader)?;
        if version != 3 {
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
    #[cfg(feature = "lowering")]
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
    #[cfg(feature = "lowering")]
    pub(super) fn export<T: Serialize>(
        dae: &rumoca_ir_dae::Dae,
        opts: SimOptions,
        context: T,
    ) -> Result<Vec<u8>, SimulationDiagnosticError> {
        let scope = ProgramScope::capture().map_err(refusal)?;
        export_in(dae, opts, context, scope, simulation_component, |scope| {
            scope.finish_capture().map(|programs| (programs, ()))
        })
        .map(|(bytes, ())| bytes)
    }
    /// The saved product and, from the same capture, the artifact of the
    /// linked product issued beside it, bound to those saved bytes; both use
    /// the same named-map wire.
    #[cfg(feature = "lowering")]
    pub(super) fn export_linked<T: Serialize>(
        dae: &rumoca_ir_dae::Dae,
        opts: SimOptions,
        context: T,
    ) -> Result<(Vec<u8>, Vec<u8>), SimulationDiagnosticError> {
        let scope = ProgramScope::capture_linked().map_err(refusal)?;
        let (bytes, linked) = export_in(
            dae,
            opts,
            context,
            scope,
            fmi_scalar_component,
            ProgramScope::finish_capture_linked,
        )?;
        let artifact = encode(&linked.artifact(&bytes))?;
        Ok((bytes, artifact))
    }
    /// The browser product's component: the simulation lowering.
    #[cfg(feature = "lowering")]
    fn simulation_component(
        dae: &rumoca_ir_dae::Dae,
        opts: &SimOptions,
    ) -> Result<FmiComponent, SimulationDiagnosticError> {
        let lowered =
            crate::solve_lowering::lower_correlated_for_simulation_with_overrides(dae, opts)?;
        crate::solve_lowering::finish_component(lowered)
    }
    /// The compiled FMU's component, from the one construction the FMU
    /// packager also uses, so the captured programs belong to the component
    /// the FMU guest constructs.
    #[cfg(feature = "lowering")]
    fn fmi_scalar_component(
        dae: &rumoca_ir_dae::Dae,
        opts: &SimOptions,
    ) -> Result<FmiComponent, SimulationDiagnosticError> {
        crate::scalar_package_component(dae, opts)
    }
    #[cfg(feature = "lowering")]
    fn export_in<T: Serialize, L>(
        dae: &rumoca_ir_dae::Dae,
        opts: SimOptions,
        context: T,
        scope: ProgramScope,
        lower: fn(
            &rumoca_ir_dae::Dae,
            &SimOptions,
        ) -> Result<FmiComponent, SimulationDiagnosticError>,
        finish: impl FnOnce(ProgramScope) -> Result<(CorePrograms, L), String>,
    ) -> Result<(Vec<u8>, L), SimulationDiagnosticError> {
        if !admits_solver_profile(opts.solver_mode) {
            return Err(refusal(
                "precompiled browser product requires the declared rk-like or fixed-step profile",
            ));
        }
        crate::backend_options::validate(&opts)?;
        let component_bytes = {
            let component = lower(dae, &opts)?;
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
        let prepared = session(replayed, opts)?;
        let _ = prepared.state()?;
        drop(prepared);
        let (programs, linked) = finish(scope).map_err(refusal)?;
        let bytes = encode(&RuntimeWireRef {
            version: 3,
            context,
            component: rumoca_eval_solve::fmi_wire::retained_fmi_component_wire(&root)
                .map_err(refusal)?,
            programs,
        })?;
        Ok((bytes, linked))
    }
    pub(super) fn load<T: DeserializeOwned>(
        payload: &[u8],
        configure: impl FnOnce(T) -> SimOptions,
    ) -> Result<SimulationSessionLease, SimulationDiagnosticError> {
        let wire = {
            let _parse =
                rumoca_core::browser_profile::Span::new("decode_binary_and_checked_replay");
            let mut reader = rmp_serde::Deserializer::new(Cursor::new(payload));
            let wire = RuntimeWire::<T>::deserialize(&mut reader).map_err(refusal)?;
            if reader.get_ref().position() != payload.len() as u64 {
                return Err(refusal("trailing runtime bytes"));
            }
            wire
        };
        if wire.version != 3 {
            return Err(refusal("unsupported precompiled runtime schema"));
        }
        let scope = ProgramScope::load(wire.programs).map_err(refusal)?;
        let prepared = {
            let _initialization =
                rumoca_core::browser_profile::Span::new("session_construct_and_init");
            session(wire.component.0, configure(wire.context))?
        };
        {
            let _initial_state = rumoca_core::browser_profile::Span::new("initial_observation");
            let _ = prepared.state()?;
        }
        scope.finish_load().map_err(refusal)?;
        Ok(SimulationSessionLease::from_prepared(prepared))
    }
}
/// Build one binary payload with typed context and component (no escaped JSON).
/// Exporting lowers the model, so it exists only with `lowering`.
#[cfg(feature = "lowering")]
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
/// The saved product and the linked product issued beside it by the same
/// capture: every module in link order, and per entry its kind, index, key
/// words and binding. A product with any kind that has no linked form (an
/// uncompiled kind, or a record site the linked assembler refused) is refused
/// by name; nothing partial is issued.
#[cfg(feature = "lowering")]
pub fn export_precompiled_linked_runtime<T: Serialize>(
    dae: &rumoca_ir_dae::Dae,
    opts: SimOptions,
    context: T,
) -> Result<(Vec<u8>, Vec<u8>), SimulationDiagnosticError> {
    #[cfg(all(target_arch = "wasm32", feature = "solver-rk45"))]
    {
        browser::export_linked(dae, opts, context)
    }
    #[cfg(not(all(target_arch = "wasm32", feature = "solver-rk45")))]
    {
        let _ = (dae, opts, context);
        Err(refusal(
            "precompiled linked export requires the wasm32 rk-like build",
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
