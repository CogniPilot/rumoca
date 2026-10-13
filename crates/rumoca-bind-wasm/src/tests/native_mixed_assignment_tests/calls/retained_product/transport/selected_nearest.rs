//! Numerical evidence uses the actual emitted model and its original root.
use super::*;
use rumoca_exec_wasm::NativeModelLibrary;
use rumoca_ir_solve::{SolveValueKind, fmi::FmiDeploymentCapabilities};
const SOURCE: &str = include_str!(
    "../../../../../../../../crates/rumoca/tests/fixtures/function_guarded_loops/SelectedNearest.mo"
);
#[test]
fn marker_owned_selected_nested_entries_execute_actual_native_wasm() {
    let _lock = session_test_guard();
    with_source_component(SOURCE, "SelectedNearest", |component| {
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
        for (requested, count, first, expected) in [
            (true, 2_i64, 3.0_f64, 6.0_f64),
            (false, 2, 3.0, 0.0),
            (true, 0, 3.0, 15.0),
            (true, 2, 5.0, 10.0),
            (true, 2, 3.0, 6.0),
        ] {
            for (name, value) in [("requested", requested), ("accepted", true)] {
                transport
                    .set_typed_input(slot(&artifact, name, "P"), NativeTypedInput::Boolean(value))
                    .unwrap();
            }
            transport
                .set_typed_input(
                    slot(&artifact, "wordCount", "P"),
                    NativeTypedInput::Integer(count),
                )
                .unwrap();
            for (name, values) in [
                ("samples", vec![first, 0.0, 1.0, 0.0, 4.0, 0.0]),
                ("words", vec![0.0, 0.0, 2.0, 0.0]),
            ] {
                write_matrix_input(&mut transport, &artifact, name, &values);
            }
            let typed = transport
                .input_values()
                .map(|(input, value)| (input.p_index(), value.unwrap()))
                .collect::<Vec<_>>();
            for (name, expected_input) in [
                ("requested", SolveValueKind::Boolean(requested)),
                ("accepted", SolveValueKind::Boolean(true)),
                ("wordCount", SolveValueKind::Integer(count)),
            ] {
                assert!(typed.contains(&(slot(&artifact, name, "P"), expected_input)));
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

fn write_matrix_input(
    transport: &mut NativeTransport,
    artifact: &serde_json::Value,
    name: &str,
    values: &[f64],
) {
    for (index, value) in values.iter().enumerate() {
        let row = index / 2 + 1;
        let column = index % 2 + 1;
        let input = slot(artifact, &format!("{name}[{row},{column}]"), "P");
        transport
            .set_float64_input_batches(&[(input, std::slice::from_ref(value))])
            .unwrap();
        let (_, actual_inputs) = transport.coordinates();
        assert_eq!(actual_inputs[input].to_bits(), value.to_bits());
    }
}
