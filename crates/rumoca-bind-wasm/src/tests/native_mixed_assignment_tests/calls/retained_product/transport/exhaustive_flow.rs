//! Actual emitted native execution of normalized exhaustive arm definitions.

use super::*;
use rumoca_exec_wasm::NativeModelLibrary;
use rumoca_ir_solve::{SolveValueKind, fmi::FmiDeploymentCapabilities};

const SOURCE: &str = include_str!(
    "../../../../../../../../crates/rumoca/tests/fixtures/function_guarded_loops/ExhaustiveLoopFlow.mo"
);

#[test]
fn exhaustive_tuple_and_loop_arm_definitions_execute_actual_native_wasm() {
    let _lock = session_test_guard();
    with_source_component(SOURCE, "ExhaustiveLoopFlow", |component| {
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
        for (use_pair, enabled, trials, expected) in [
            (true, true, 1, 13.0_f64),
            (false, true, 1, 28.0),
            (true, true, 2, 26.0),
            (false, true, 2, 56.0),
            (true, false, 2, 0.0),
            (false, true, 0, 0.0),
            (true, true, 1, 13.0),
        ] {
            for (name, value) in [("usePair", use_pair), ("enabled", enabled)] {
                transport
                    .set_typed_input(slot(&artifact, name, "P"), NativeTypedInput::Boolean(value))
                    .unwrap();
            }
            transport
                .set_typed_input(
                    slot(&artifact, "trials", "P"),
                    NativeTypedInput::Integer(trials),
                )
                .unwrap();
            for (index, value) in [2.0_f64, -1.0, 3.0].iter().enumerate() {
                transport
                    .set_float64_input_batches(&[(
                        slot(&artifact, &format!("values[{}]", index + 1), "P"),
                        std::slice::from_ref(value),
                    )])
                    .unwrap();
            }
            let (_, p) = transport.coordinates();
            for (index, value) in [2.0_f64, -1.0, 3.0].iter().enumerate() {
                assert_eq!(
                    p[slot(&artifact, &format!("values[{}]", index + 1), "P")].to_bits(),
                    value.to_bits()
                );
            }
            let typed = transport
                .input_values()
                .map(|(input, value)| (input.p_index(), value.unwrap()))
                .collect::<Vec<_>>();
            assert_eq!(typed.len(), 3);
            for (name, value) in [
                ("usePair", SolveValueKind::Boolean(use_pair)),
                ("enabled", SolveValueKind::Boolean(enabled)),
                ("trials", SolveValueKind::Integer(trials)),
            ] {
                assert!(typed.contains(&(slot(&artifact, name, "P"), value)));
            }
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
