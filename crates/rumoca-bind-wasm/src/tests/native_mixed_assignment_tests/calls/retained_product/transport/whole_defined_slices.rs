//! Actual emitted native execution of whole-defined per-iteration slice scratch.

use super::*;
use rumoca_exec_wasm::NativeModelLibrary;
use rumoca_ir_solve::{SolveValueKind, fmi::FmiDeploymentCapabilities};

const SOURCE: &str = include_str!(
    "../../../../../../../../crates/rumoca/tests/fixtures/function_guarded_loops/WholeDefinedSliceScratch.mo"
);

#[test]
fn whole_defined_slice_scratch_executes_actual_native_wasm() {
    let _lock = session_test_guard();
    with_source_component(SOURCE, "WholeDefinedSliceScratch", |component| {
        let original = component.runtime_view().retain_assertion_root();
        assert!(!original.model().pure_calls.owners().is_empty());
        let code = rumoca_exec_wasm::compile_retained_native_preparation_wasm(
            original.native_preparation().unwrap(),
        )
        .unwrap();
        let deployment = component
            .into_codegen_view()
            .try_deployment(FmiDeploymentCapabilities::default())
            .unwrap();
        let library = NativeModelLibrary::from_preparation(code.clone(), deployment).unwrap();
        assert!(library.inputs().root().shares_root_owner(&original));
        assert!(std::ptr::eq(library.compiled(), code.compiled()));
        let root = original.resolved_periodic_schedules_at(0.0).unwrap();
        let host = code.bind_resolved(root.clone()).unwrap().into_host();
        let artifact = execution_description(host.product());
        let mut execution = CallExecution::uninitialized(&artifact);
        let y = vec![0.0; root.model().problem.layout.y_scalars()];
        let mut transport = host
            .transport_from_source(&y, &root.model().parameters)
            .unwrap();
        for (requested, values, expected) in [
            (true, [1.0_f64, 2.0], 13.0_f64),
            (false, [1.0, 2.0], 0.0),
            (true, [-1.0, 2.0], 8.0),
            (true, [3.0, 4.0], 25.0),
            (true, [1.0, 2.0], 13.0),
        ] {
            transport
                .set_typed_input(
                    slot(&artifact, "requested", "P"),
                    NativeTypedInput::Boolean(requested),
                )
                .unwrap();
            for (index, value) in values.iter().enumerate() {
                transport
                    .set_float64_input_batches(&[(
                        slot(&artifact, &format!("values[{}]", index + 1), "P"),
                        std::slice::from_ref(value),
                    )])
                    .unwrap();
            }
            let (_, p) = transport.coordinates();
            for (index, value) in values.iter().enumerate() {
                assert_eq!(
                    p[slot(&artifact, &format!("values[{}]", index + 1), "P")].to_bits(),
                    value.to_bits()
                );
            }
            let typed = transport
                .input_values()
                .map(|(input, value)| (input.p_index(), value.unwrap()))
                .collect::<Vec<_>>();
            assert_eq!(typed.len(), 1);
            assert!(typed.contains(&(
                slot(&artifact, "requested", "P"),
                SolveValueKind::Boolean(requested),
            )));
            let lease = transport.enter().unwrap();
            let (status, actual) = execute_transport(&mut execution, &lease);
            assert_eq!(status, 0);
            assert_eq!(
                actual[slot(&artifact, "y", "Y")].to_bits(),
                expected.to_bits()
            );
        }
        Ok(String::new())
    })
    .unwrap();
}
