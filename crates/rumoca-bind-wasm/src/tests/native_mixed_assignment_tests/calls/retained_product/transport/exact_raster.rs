//! Actual emitted native bytes; Wasmi is confined to the existing evidence harness.

use super::*;
use rumoca_exec_wasm::NativeModelLibrary;
use rumoca_ir_solve::{SolveValueKind, fmi::FmiDeploymentCapabilities};

const SOURCE: &str = include_str!(
    "../../../../../../../../crates/rumoca/tests/fixtures/function_guarded_loops/ExactRaster.mo"
);

#[derive(Clone, Copy)]
struct Case {
    enabled: bool,
    grid: bool,
    first: f64,
    count: f64,
    valid: f64,
}

const NORMAL: Case = Case {
    enabled: true,
    grid: false,
    first: 3.0,
    count: 2.0,
    valid: 1.0,
};

#[test]
fn unchanged_raster_function_executes_actual_retained_native_wasm() {
    let _lock = session_test_guard();
    with_source_component(SOURCE, "ExactRaster", |component| {
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
        assert!(!library.compiled().faults().is_empty());
        for fault in library.compiled().faults() {
            fault
                .provenance
                .require_provenance("original raster source fault")
                .unwrap();
            assert!(original.model().pure_calls.owner(fault.owner).is_some());
        }
        let root = original.resolved_periodic_schedules_at(0.0).unwrap();
        let host = code.bind_resolved(root.clone()).unwrap().into_host();
        assert!(std::ptr::eq(host.product().compiled(), library.compiled()));
        assert!(std::ptr::eq(
            host.product().inputs().schedule(),
            library.inputs().schedule()
        ));
        let artifact = execution_description(host.product());
        let mut execution = CallExecution::uninitialized(&artifact);
        let y = vec![0.0; root.model().problem.layout.y_scalars()];
        let mut transport = host
            .transport_from_source(&y, &root.model().parameters)
            .unwrap();
        set_inputs(&mut transport, &artifact, NORMAL);
        let original_inputs = transport.snapshot().unwrap();
        for case in [
            NORMAL,
            Case {
                enabled: false,
                count: 0.0,
                ..NORMAL
            },
            Case {
                grid: true,
                count: 0.0,
                valid: 0.0,
                ..NORMAL
            },
            Case {
                first: 4.0,
                ..NORMAL
            },
        ] {
            set_inputs(&mut transport, &artifact, case);
            observe(&mut execution, &mut transport, &artifact, case);
        }
        transport.restore(&original_inputs).unwrap();
        observe(&mut execution, &mut transport, &artifact, NORMAL);
        assert!(host.product().inputs().root().shares_root_owner(&root));
        Ok(String::new())
    })
    .unwrap();
}

fn set_inputs(transport: &mut NativeTransport, artifact: &serde_json::Value, case: Case) {
    transport
        .set_typed_input(
            slot(artifact, "enabled", "P"),
            NativeTypedInput::Boolean(case.enabled),
        )
        .unwrap();
    transport
        .set_typed_input(
            slot(artifact, "grid", "P"),
            NativeTypedInput::Boolean(case.grid),
        )
        .unwrap();
    for (index, value) in [case.first, 2.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
        .iter()
        .enumerate()
    {
        transport
            .set_float64_input_batches(&[(
                slot(artifact, &format!("scores[{}]", index + 1), "P"),
                std::slice::from_ref(value),
            )])
            .unwrap();
    }
}

fn observe(
    execution: &mut CallExecution,
    transport: &mut NativeTransport,
    artifact: &serde_json::Value,
    case: Case,
) {
    // Verify the exact assigned input owner before observing any numerical output.
    let (_, p) = transport.coordinates();
    for (index, value) in [case.first, 2.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
        .iter()
        .enumerate()
    {
        assert_eq!(
            p[slot(artifact, &format!("scores[{}]", index + 1), "P")].to_bits(),
            value.to_bits()
        );
    }
    let typed = transport
        .input_values()
        .map(|(input, value)| (input.p_index(), value.unwrap()))
        .collect::<Vec<_>>();
    assert_eq!(typed.len(), 2);
    for (name, value) in [("enabled", case.enabled), ("grid", case.grid)] {
        assert!(typed.contains(&(slot(artifact, name, "P"), SolveValueKind::Boolean(value))));
    }
    let lease = transport.enter().unwrap();
    let (status, actual) = execute_transport(execution, &lease);
    assert_eq!(
        status, 0,
        "the actual native entry must complete before observations"
    );
    for (name, expected) in [("count", case.count), ("valid", case.valid)] {
        assert_eq!(
            actual[slot(artifact, name, "Y")].to_bits(),
            expected.to_bits()
        );
    }
    let expected = if case.count == 2.0 {
        [[0.0, 0.0, case.first], [1.0, 0.0, 2.0], [0.0; 3]]
    } else {
        [[0.0; 3]; 3]
    };
    for (row, values) in expected.iter().enumerate() {
        for (column, value) in values.iter().enumerate() {
            let name = format!("feature{}{}", row + 1, column + 1);
            assert_eq!(
                actual[slot(artifact, &name, "Y")].to_bits(),
                value.to_bits(),
                "{name}"
            );
        }
    }
}
