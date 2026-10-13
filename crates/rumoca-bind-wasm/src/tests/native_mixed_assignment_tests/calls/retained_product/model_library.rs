//! Library refinement uses actual source constructors, without engine admission.

use super::*;
use rumoca_exec_wasm::{NativeModelLibrary, PreparedNativeWasmCode};
use rumoca_ir_solve::RetainedNativeScheduleError;
use rumoca_ir_solve::fmi::{FmiComponent, FmiDeploymentCapabilities, FmiDeploymentView};
use serde::de::IntoDeserializer;

mod linkage;
mod source_bundle;

fn preparation(component: &FmiComponent) -> PreparedNativeWasmCode {
    let root = component.runtime_view().retain_assertion_root();
    rumoca_exec_wasm::compile_retained_native_preparation_wasm(root.native_preparation().unwrap())
        .expect("existing typed source has an emitted native preparation")
}

fn deployment(component: FmiComponent) -> FmiDeploymentView {
    component
        .into_codegen_view()
        .try_deployment(FmiDeploymentCapabilities::default())
        .unwrap()
}

#[test]
fn native_model_library_retains_original_emission_and_deployment_owners() {
    let _lock = session_test_guard();
    with_fixture_component(|component| {
        let root = component.runtime_view().retain_assertion_root();
        let code = preparation(&component);
        #[cfg(not(all(target_arch = "wasm32", target_os = "unknown")))]
        assert_eq!(
            code.require_execution_entry().is_err(),
            !rumoca_exec_wasm::HOST_ENGINE_ENTRY,
            "the private static receiver entry is issued exactly when the host engine is linked: \
             absent without it (ordinary emission), present with it"
        );
        let library = NativeModelLibrary::from_preparation(code.clone(), deployment(component))
            .expect("the emitter and deployment retain the same original allocation");
        let retained = library.deployment().runtime_view().retain_assertion_root();
        assert!(retained.shares_root_owner(&root));
        assert!(library.inputs().root().shares_root_owner(&retained));
        assert!(std::ptr::eq(retained.metadata(), root.metadata()));
        assert!(std::ptr::eq(library.inputs(), code.inputs()));
        assert!(std::ptr::eq(library.compiled(), code.compiled()));
        assert!(std::ptr::eq(
            library.inputs().schedule(),
            code.inputs().schedule()
        ));
        assert!(std::ptr::eq(
            library.inputs().schedule().stages(),
            code.inputs().schedule().stages()
        ));
        assert!(std::ptr::eq(
            library.inputs().schedule().input_runs(),
            code.inputs().schedule().input_runs()
        ));
        assert!(std::ptr::eq(
            library.inputs().schedule().derived_outputs(),
            code.inputs().schedule().derived_outputs()
        ));
        assert!(std::ptr::eq(
            library.compiled().module_bytes(),
            code.compiled().module_bytes()
        ));
        assert!(std::ptr::eq(
            library.compiled().faults(),
            code.compiled().faults()
        ));
        assert!(std::ptr::eq(
            library.compiled().scratch_report(),
            code.compiled().scratch_report()
        ));
        let emitted = &library.inputs().root().model().problem.continuous;
        let original = &root.model().problem.continuous;
        assert!(
            emitted
                .algebraic_projection_plan
                .blocks
                .shares_stage_owner(&original.algebraic_projection_plan.blocks)
        );
        assert!(!library.compiled().faults().is_empty());
        assert!(!library.inputs().schedule().stages().is_empty());
        assert!(!library.inputs().schedule().input_runs().is_empty());
        assert!(std::ptr::eq(
            &library.inputs().root().model().pure_calls,
            &root.model().pure_calls
        ));
        assert_eq!(
            library.compiled().scratch_bytes(),
            code.compiled().scratch_bytes()
        );
        assert_eq!(retained.resolved_start_time(), None);
        assert!(matches!(
            retained.native_schedule(),
            Err(rumoca_ir_solve::RetainedNativeScheduleError::UnresolvedStartTime)
        ));
        Ok(String::new())
    })
    .unwrap();
}

#[test]
fn native_model_library_refuses_independently_constructed_equal_source() {
    let _lock = session_test_guard();
    let mut code = None;
    with_fixture_component(|component| {
        code = Some(preparation(&component));
        Ok(String::new())
    })
    .unwrap();
    with_fixture_component(|component| {
        let code = code.take().unwrap();
        let foreign = component.runtime_view().retain_assertion_root();
        assert!(!foreign.shares_root_owner(code.inputs().root()));
        assert!(!std::ptr::eq(
            foreign.metadata(),
            code.inputs().root().metadata()
        ));
        assert!(matches!(
            NativeModelLibrary::from_preparation(code, deployment(component)),
            Err(RetainedNativeScheduleError::ForeignPreparation)
        ));
        Ok(String::new())
    })
    .unwrap();
}

#[test]
fn native_model_library_refuses_equal_capsule_replay_until_new_emission() {
    let _lock = session_test_guard();
    with_fixture_component(|component| {
        let code = preparation(&component);
        let wire = serde_json::to_value(
            rumoca_sim::retained_fmi_component_wire(code.inputs().root()).unwrap(),
        )
        .unwrap();
        let replayed = rumoca_sim::deserialize_fmi_component(wire.into_deserializer()).unwrap();
        let fresh = replayed.runtime_view().retain_assertion_root();
        assert!(!fresh.shares_root_owner(code.inputs().root()));
        assert!(!std::ptr::eq(
            fresh.metadata(),
            code.inputs().root().metadata()
        ));
        let fresh_code = preparation(&replayed);
        // Even equal emitted bytes do not transfer the original source authority.
        assert_eq!(
            fresh_code.compiled().module_bytes(),
            code.compiled().module_bytes()
        );
        assert!(matches!(
            NativeModelLibrary::from_preparation(code.clone(), deployment(replayed)),
            Err(RetainedNativeScheduleError::ForeignPreparation)
        ));
        NativeModelLibrary::from_preparation(code, deployment(component))
            .expect("the original source correlation remains valid");
        Ok(String::new())
    })
    .unwrap();
}
