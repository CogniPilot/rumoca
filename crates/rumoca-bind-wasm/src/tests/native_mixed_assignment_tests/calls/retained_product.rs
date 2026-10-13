//! Complete typed native execution retains its original FMI source allocation.

use super::*;
use rumoca_exec_wasm::CompiledRetainedNativeProgramWasm;
mod model_library;
mod transport;

#[test]
fn retained_native_product_reuses_exact_sources_and_atomic_typed_execution() {
    let _lock = session_test_guard();
    with_fixture_component(|component| {
        let original = component.runtime_view().retain_assertion_root();
        assert!(std::ptr::eq(original.metadata(), component.metadata()));
        assert!(std::ptr::eq(
            original.metadata().variables(),
            component.variables()
        ));
        assert!(matches!(
            original.native_schedule(),
            Err(rumoca_ir_solve::RetainedNativeScheduleError::UnresolvedStartTime)
        ));
        let root = original.resolved_periodic_schedules_at(0.0).unwrap();
        let product =
            rumoca_exec_wasm::compile_retained_native_program_wasm(root.native_schedule().unwrap())
                .expect("exact retained typed native schedule compiles");
        assert!(product.inputs().root().shares_root_owner(&root));
        assert!(std::ptr::eq(
            product.inputs().schedule(),
            root.model()
                .problem
                .continuous
                .refresh_owners
                .native_assignment_schedule()
                .unwrap()
        ));
        assert!(std::ptr::eq(product.inputs().root().model(), root.model()));
        assert!(std::ptr::eq(
            &product.inputs().root().model().pure_calls,
            &root.model().pure_calls
        ));
        let original_inventory = component.metadata().variables().as_ptr();
        drop(component);
        assert_eq!(root.metadata().variables().as_ptr(), original_inventory);
        assert!(std::ptr::eq(
            product.inputs().root().metadata(),
            root.metadata()
        ));
        let artifact = execution_description(&product);
        let mut execution = CallExecution::new(&artifact);
        let mut parameters = product.inputs().root().model().parameters.to_vec();
        let sample = slot(&artifact, "samples", "P");
        let result = slot(&artifact, "result", "Y");
        for (enabled, value, k, expected) in [
            (1, 5.0, 2, 1.0),
            (0, 5.0, 1, 2.0),
            (0, -5.0, 1, 3.0),
            (1, -5.0, 2, 1.0),
        ] {
            execution.set_input("first", enabled);
            execution.set_input("k", k);
            parameters[sample] = value;
            assert_eq!(execution.run(&parameters)[result], expected);
        }
        execution.set_input("first", 0);
        execution.set_input("k", 2);
        let mut before = vec![0; execution.y * 8];
        execution
            .memory
            .read(&execution.store, 0, &mut before)
            .unwrap();
        let (status, _) = execution.run_typed(&parameters);
        let fault = product
            .compiled()
            .faults()
            .iter()
            .find(|fault| fault.status == status as u32)
            .expect("failed active gather retains its original typed source fault");
        fault
            .provenance
            .require_provenance("native fixture fault")
            .unwrap();
        assert_eq!(
            fault.kind,
            rumoca_exec_wasm::TypedCallFaultKind::IndexBounds
        );
        assert!(root.model().pure_calls.owner(fault.owner).is_some());
        let mut after = vec![0; before.len()];
        execution
            .memory
            .read(&execution.store, 0, &mut after)
            .unwrap();
        assert_eq!(after, before);
        execution.set_input("first", 1);
        assert_eq!(execution.run(&parameters)[result], 1.0);
        assert!(product.inputs().root().shares_root_owner(&root));
        Ok(String::new())
    })
    .unwrap();
}

/// Reuse the existing Wasmi harness without compiling another model/module.
fn execution_description(product: &CompiledRetainedNativeProgramWasm) -> serde_json::Value {
    let model = product.inputs().root().model();
    let schedule = product.inputs().schedule();
    let layout = &model.problem.layout;
    let storage = (layout.y_scalars() + layout.p_scalars()) * 8;
    let typed = (storage + product.compiled().scratch_bytes() as usize).next_multiple_of(8);
    let output = typed + schedule.input_lane_bytes();
    let pages = (output + schedule.lane_bytes()).div_ceil(65536).max(1);
    let input_lanes = schedule
        .scalar_input_lanes()
        .map(|input| {
            let name = layout
                .scalar_name_for_slot(rumoca_ir_solve::scalar_slot_p(input.p_index()))
                .expect("typed scalar fixture input has its original binding");
            serde_json::json!({"name":name, "p_index":input.p_index(),
            "representation":input.lane().as_str(), "byte_offset":input.lane_offset()})
        })
        .collect::<Vec<_>>();
    serde_json::json!({
        "profile":"native-direct-program-f64-v3",
        "module_bytes":product.compiled().module_bytes(),
        "var_layout":layout, "parameters":model.parameters.as_slice(),
        "input_lanes":input_lanes,
        "abi":{"memory_pages":pages, "y_count":layout.y_scalars(),
            "p_offset":layout.y_scalars()*8, "scratch_offset":storage,
            "typed_lanes_offset":typed, "input_lanes_bytes":schedule.input_lane_bytes(),
            "output_lanes_offset":output, "output_lanes_bytes":schedule.lane_bytes()}
    })
}

/// Keep the original DAE and Solve metadata correlated through the sole FMI projection.
fn with_fixture_component(
    artifact: impl FnOnce(rumoca_ir_solve::fmi::FmiComponent) -> Result<String, crate::WasmError>,
) -> Result<String, crate::WasmError> {
    with_source_component(returns::SOURCE, "ReturnPredicates", artifact)
}

fn with_source_component(
    source: &str,
    model: &str,
    artifact: impl FnOnce(rumoca_ir_solve::fmi::FmiComponent) -> Result<String, crate::WasmError>,
) -> Result<String, crate::WasmError> {
    crate::with_singleton_session(|session| {
        session.update_document("input.mo", source);
        let requested = crate::qualify_input_model_name(session, model);
        let compilation = crate::compile_requested_model(session, &requested)?;
        let component = rumoca_sim::lower_fmi_component(&compilation.dae)
            .map_err(|error| crate::WasmError::new(error.to_string()))?;
        artifact(component)
    })
}
