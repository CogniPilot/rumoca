//! Actual emitted native calls read every argument before any receiving write.
use super::*;
use rumoca_exec_wasm::NativeModelLibrary;
use rumoca_ir_solve::{SolveValueKind, fmi::FmiDeploymentCapabilities};
const SOURCE: &str = include_str!(
    "../../../../../../../../crates/rumoca/tests/fixtures/function_guarded_loops/GuardedTupleReads.mo"
);
#[test]
fn source_proven_tuple_arguments_execute_actual_native_wasm_before_receiver_writes() {
    let _lock = session_test_guard();
    with_source_component(SOURCE, "GuardedTupleReads", |component| {
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
            (
                true,
                [1.0_f64, 2.0, 3.0, 4.0],
                [30.0_f64, 60.0, 60.0, 30.0, 10.0],
            ),
            (false, [1.0, 2.0, 3.0, 4.0], [-1.0, 0.0, 60.0, -1.0, -1.0]),
            (true, [2.0, 3.0, 4.0, 5.0], [42.0, 84.0, 84.0, 42.0, 14.0]),
            (true, [0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.0, 0.0, 0.0]),
            (true, [1.0, 2.0, 3.0, 4.0], [30.0, 60.0, 60.0, 30.0, 10.0]),
        ] {
            transport
                .set_typed_input(
                    slot(&artifact, "requested", "P"),
                    NativeTypedInput::Boolean(requested),
                )
                .unwrap();
            for (index, value) in values.iter().enumerate() {
                let input = slot(
                    &artifact,
                    &format!("values[{},{}]", index / 2 + 1, index % 2 + 1),
                    "P",
                );
                transport
                    .set_float64_input_batches(&[(input, std::slice::from_ref(value))])
                    .unwrap();
                assert_eq!(transport.coordinates().1[input].to_bits(), value.to_bits());
            }
            let typed = transport
                .input_values()
                .map(|(input, value)| (input.p_index(), value.unwrap()))
                .collect::<Vec<_>>();
            assert!(typed.contains(&(
                slot(&artifact, "requested", "P"),
                SolveValueKind::Boolean(requested)
            )));
            let lease = transport.enter().unwrap();
            let (status, actual) = execute_transport(&mut execution, &lease);
            assert_eq!(status, 0);
            for (name, expected) in [
                "bodyResult",
                "loopResult",
                "foldResult",
                "recordResult",
                "omittedResult",
            ]
            .into_iter()
            .zip(expected)
            {
                assert_eq!(
                    actual[slot(&artifact, name, "Y")].to_bits(),
                    expected.to_bits(),
                    "{name}"
                );
            }
        }
        Ok(String::new())
    })
    .unwrap();
}
