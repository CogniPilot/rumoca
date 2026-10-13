mod assigned_loop_exit_scale;
mod compact_call_transfer;
mod preparation;

use super::*;
mod calibration;
mod capsule;
mod exact_raster;
mod exhaustive_flow;
mod real_presence;
mod record_gated_flow;
mod record_loop_entry;
mod record_root_reads;
mod selected_nearest;
mod source_functions;
mod tuple_argument_reads;
mod whole_defined_slices;
use rumoca_exec_wasm::{
    NativeInvocationLease, NativeTransport, NativeTransportError, NativeTypedInput,
    NativeTypedInputBatch,
};

#[test]
fn retained_native_source_starts_preserve_exact_bits_and_missing_presence() {
    let _lock = session_test_guard();
    let source = r#"
model ExactSourceStarts
  input Integer values[2](start={9007199254740993, 9223372036854775807});
  input Boolean enabled[2](start=fill(true, 2));
  output Integer result;
equation
  result = values[1];
end ExactSourceStarts;
model MissingSourceStart
  input Integer value;
  output Integer result;
equation
  result = value;
end MissingSourceStart;
"#;
    with_source_component(source, "ExactSourceStarts", |component| {
        let root = component
            .runtime_view()
            .retain_assertion_root()
            .resolved_periodic_schedules_at(0.0)
            .unwrap();
        let host =
            rumoca_exec_wasm::compile_retained_native_program_wasm(root.native_schedule().unwrap())
                .unwrap()
                .into_host();
        assert!(std::ptr::eq(
            host.product().inputs().root().metadata(),
            root.metadata()
        ));
        let y = vec![0.0; root.model().problem.layout.y_scalars()];
        let transport = host
            .transport_from_source(&y, &root.model().parameters)
            .unwrap();
        let values = transport
            .input_values()
            .map(|(_, value)| value.unwrap())
            .collect::<Vec<_>>();
        assert!(values.contains(&rumoca_ir_solve::SolveValueKind::Integer(
            9_007_199_254_740_993
        )));
        assert!(values.contains(&rumoca_ir_solve::SolveValueKind::Integer(i64::MAX)));
        assert_eq!(
            values
                .iter()
                .filter(|value| **value == rumoca_ir_solve::SolveValueKind::Boolean(true))
                .count(),
            2
        );
        Ok(String::new())
    })
    .unwrap();
    with_source_component(source, "MissingSourceStart", |component| {
        let root = component
            .runtime_view()
            .retain_assertion_root()
            .resolved_periodic_schedules_at(0.0)
            .unwrap();
        let host =
            rumoca_exec_wasm::compile_retained_native_program_wasm(root.native_schedule().unwrap())
                .unwrap()
                .into_host();
        let y = vec![0.0; root.model().problem.layout.y_scalars()];
        let mut transport = host
            .transport_from_source(&y, &root.model().parameters)
            .unwrap();
        assert!(matches!(
            transport.enter(),
            Err(NativeTransportError::MissingInput { .. })
        ));
        let input = host.product().inputs().schedule().input_runs()[0]
            .p_range()
            .start;
        transport
            .set_typed_input(input, NativeTypedInput::Integer(i64::MAX))
            .unwrap();
        assert!(transport.enter().is_ok());
        Ok(String::new())
    })
    .unwrap();
}

#[test]
fn retained_native_transport_isolates_resources_and_refuses_reentry() {
    let _lock = session_test_guard();
    with_fixture_component(|component| {
            let root = component.runtime_view().retain_assertion_root()
                .resolved_periodic_schedules_at(0.0).unwrap();
            let product = rumoca_exec_wasm::compile_retained_native_program_wasm(
                root.native_schedule().unwrap(),
            ).unwrap();
            let host = product.into_host();
            let product = host.product();
            let artifact = execution_description(product);
            let mut execution = CallExecution::uninitialized(&artifact);
            let p = root.model().parameters.to_vec();
            let y = vec![13.0; root.model().problem.layout.y_scalars()];
            assert!(matches!(host.transport(&[], &p), Err(NativeTransportError::BufferShape)));
            let mut first = host.transport(&y, &p).unwrap();
            let mut second = host.transport(&y, &p).unwrap();
            let unassigned = first.snapshot().unwrap();
            assert_eq!(second.restore(&unassigned), Err(NativeTransportError::ForeignSnapshot));
            assert!(matches!(first.enter(), Err(NativeTransportError::MissingInput { .. })));
            let boolean = slot(&artifact, "first", "P");
            let integer = slot(&artifact, "k", "P");
            let samples = slot(&artifact, "samples", "P");
            first.set_float64_input_batches(&[(samples, &[5.0])]).unwrap();
            second.set_float64_input_batches(&[(samples, &[5.0])]).unwrap();
            assert_eq!(first.set_typed_input(boolean, NativeTypedInput::Integer(1)),
                Err(NativeTransportError::InputType { p_index: boolean }));
            assert_eq!(first.set_typed_input(p.len(), NativeTypedInput::Integer(1)),
                Err(NativeTransportError::UnknownInput { p_index: p.len() }));
            first.set_typed_input(boolean, NativeTypedInput::Boolean(true)).unwrap();
            assert!(matches!(first.enter(), Err(NativeTransportError::MissingInput { p_index }) if p_index == integer));
            first.set_typed_input(integer, NativeTypedInput::Integer(i64::MAX)).unwrap();
            second.set_typed_input(boolean, NativeTypedInput::Boolean(false)).unwrap();
            second.set_typed_input(integer, NativeTypedInput::Integer(1)).unwrap();
            let original = first.snapshot().unwrap();
            first.set_typed_input(integer, NativeTypedInput::Integer(1)).unwrap();
            first.set_coordinates(&vec![99.0; y.len()], &p).unwrap();
            first.restore(&original).unwrap();
            let result = slot(&artifact, "result", "Y");
            for _ in 0..2 {
                exercise_resource(&mut execution, &mut first, &mut second, 1.0, result, integer);
                exercise_resource(&mut execution, &mut second, &mut first, 2.0, result, integer);
            }
            first.restore(&unassigned).unwrap();
            assert!(matches!(first.enter(), Err(NativeTransportError::MissingInput { .. })));
            first.restore(&original).unwrap();
            second.set_typed_input(integer, NativeTypedInput::Integer(2)).unwrap();
            {
                let lease = second.enter().unwrap();
                let (status, actual) = execute_transport(&mut execution, &lease);
                assert!(product.compiled().faults().iter().any(|fault|
                    fault.status == status as u32 && fault.kind == rumoca_exec_wasm::TypedCallFaultKind::IndexBounds));
                assert_eq!(actual, y);
            }
            // Error return releases the gate, without accepting or publishing a result.
            let lease = first.enter().unwrap();
            assert_eq!(execute_transport(&mut execution, &lease).0, 0);
            Ok(String::new())
    }).unwrap();
}

#[test]
fn retained_native_input_array_batch_checks_before_writing_exact_bits() {
    let _lock = session_test_guard();
    with_source_component(typed_inputs::SOURCE, "Step", |component| {
        let root = component
            .runtime_view()
            .retain_assertion_root()
            .resolved_periodic_schedules_at(0.0)
            .unwrap();
        let host =
            rumoca_exec_wasm::compile_retained_native_program_wasm(root.native_schedule().unwrap())
                .unwrap()
                .into_host();
        let artifact = execution_description(host.product());
        let mut execution = CallExecution::uninitialized(&artifact);
        let mut transport = host
            .transport(&root.model().initial_y, &root.model().parameters)
            .unwrap();
        write_zero_typed_inputs(&host, &mut transport);
        transport
            .set_float64_input_batches(&[(slot(&artifact, "previous.position", "P"), &[0.0; 3])])
            .unwrap();
        let variable = root
            .metadata()
            .variables()
            .iter()
            .find(|variable| variable.name() == "previous.observations")
            .unwrap();
        let storage = variable.storage().unwrap();
        assert_eq!(storage.scalar_count(), 2);
        let values = [(1_i64 << 53) + 1, -((1_i64 << 53) + 1)];
        transport
            .set_typed_inputs(storage.base(), NativeTypedInputBatch::Integer(&values))
            .unwrap();
        let literal_snapshot = transport.snapshot().unwrap();
        transport
            .set_typed_inputs(
                storage.base(),
                NativeTypedInputBatch::IntegerRepeat {
                    value: i64::MAX,
                    count: storage.scalar_count(),
                },
            )
            .unwrap();
        assert_integer_inputs(&transport, storage.base(), &[i64::MAX; 2]);
        transport.restore(&literal_snapshot).unwrap();
        let before = transport.enter().unwrap().typed_lanes().to_vec();
        assert!(
            transport
                .set_typed_input_batches(&[
                    (storage.base(), NativeTypedInputBatch::Integer(&[7, 8])),
                    (usize::MAX, NativeTypedInputBatch::Integer(&[9])),
                ])
                .is_err()
        );
        assert_eq!(transport.enter().unwrap().typed_lanes(), before);
        assert!(
            transport
                .set_typed_inputs(storage.base(), NativeTypedInputBatch::Integer(&[4, 5, 6]))
                .is_err()
        );
        let lease = transport.enter().unwrap();
        assert_eq!(lease.typed_lanes(), before);
        for (ordinal, value) in values.iter().enumerate() {
            let descriptor = host
                .product()
                .inputs()
                .schedule()
                .scalar_input_lanes()
                .find(|input| input.p_index() == storage.base() + ordinal)
                .unwrap();
            assert_eq!(
                lease.typed_lanes()[descriptor.lane_offset() / 8].to_le_bytes(),
                value.to_le_bytes()
            );
        }
        assert_eq!(execute_transport(&mut execution, &lease).0, 0);
        drop(lease);
        assert_integer_inputs(&transport, storage.base(), &values);
        // A raw evidence-host call cannot mint a production outcome or make
        // unpublished zero-filled output storage look like accepted values.
        assert_outputs_unpublished(&transport);
        Ok(String::new())
    })
    .unwrap();
}

/// Explicit host writes, never inferred from Float64 start metadata.
fn write_zero_typed_inputs(
    host: &rumoca_exec_wasm::RetainedNativeWasmHost,
    transport: &mut NativeTransport,
) {
    for input in host.product().inputs().schedule().scalar_input_lanes() {
        let value = match input.lane() {
            rumoca_ir_solve::NativeOutputLane::Integer => NativeTypedInput::Integer(0),
            rumoca_ir_solve::NativeOutputLane::Boolean => NativeTypedInput::Boolean(false),
            rumoca_ir_solve::NativeOutputLane::Real => panic!("typed input has no Real lane"),
        };
        transport.set_typed_input(input.p_index(), value).unwrap();
    }
}

/// Every Integer input lane in the stored range carries the exact expected value.
fn assert_integer_inputs(transport: &NativeTransport, base: usize, expected: &[i64]) {
    for (input, value) in transport.input_values() {
        if (base..base + expected.len()).contains(&input.p_index()) {
            assert_eq!(
                value.unwrap(),
                rumoca_ir_solve::SolveValueKind::Integer(expected[input.p_index() - base])
            );
        }
    }
}

/// A raw evidence-host call cannot mint a production outcome or make
/// unpublished zero-filled output storage look like accepted values.
fn assert_outputs_unpublished(transport: &NativeTransport) {
    assert!(transport.output_values().next().is_some());
    assert!(transport.output_values().all(|(output, value)| matches!(
        value,
        Err(NativeTransportError::MissingOutput { p_index }) if p_index == output.p_index()
    )));
}

/// Exercise the actual checked entry; raw outcomes stay local to this test.
fn execute_transport(
    execution: &mut CallExecution,
    lease: &NativeInvocationLease<'_>,
) -> (i32, Vec<f64>) {
    let CallEntry::Checked(call) = execution.call else {
        panic!("checked native entry required")
    };
    let y = lease
        .y()
        .iter()
        .flat_map(|v| v.to_le_bytes())
        .collect::<Vec<_>>();
    let p = lease
        .p()
        .iter()
        .flat_map(|v| v.to_le_bytes())
        .collect::<Vec<_>>();
    execution.memory.write(&mut execution.store, 0, &y).unwrap();
    execution
        .memory
        .write(&mut execution.store, execution.p, &p)
        .unwrap();
    execution
        .memory
        .write(
            &mut execution.store,
            execution.scratch,
            &word_bytes(lease.scratch()),
        )
        .unwrap();
    execution
        .memory
        .write(
            &mut execution.store,
            execution.typed,
            &word_bytes(lease.typed_lanes()),
        )
        .unwrap();
    let typed_pointer = if lease.typed_lanes().is_empty() {
        0
    } else {
        execution.typed as i32
    };
    let status = call
        .call(
            &mut execution.store,
            (
                0,
                execution.p as i32,
                0.0,
                execution.scratch as i32,
                typed_pointer,
            ),
        )
        .unwrap();
    let mut after_p = vec![0; p.len()];
    execution
        .memory
        .read(&execution.store, execution.p, &mut after_p)
        .unwrap();
    assert_eq!(after_p, p);
    let mut after_y = vec![0; y.len()];
    execution
        .memory
        .read(&execution.store, 0, &mut after_y)
        .unwrap();
    (
        status,
        after_y
            .chunks_exact(8)
            .map(|bytes| f64::from_le_bytes(bytes.try_into().unwrap()))
            .collect(),
    )
}

fn exercise_resource(
    execution: &mut CallExecution,
    resource: &mut NativeTransport,
    other: &mut NativeTransport,
    expected: f64,
    result: usize,
    integer: usize,
) {
    let lease = resource.enter().unwrap();
    assert!(matches!(other.enter(), Err(NativeTransportError::Busy)));
    assert_eq!(
        other.set_typed_input(integer, NativeTypedInput::Integer(3)),
        Err(NativeTransportError::Busy)
    );
    assert_eq!(
        other.set_coordinates(lease.y(), lease.p()),
        Err(NativeTransportError::Busy)
    );
    if expected == 1.0 {
        let lane = lease
            .product()
            .inputs()
            .schedule()
            .scalar_input_lanes()
            .find(|lane| lane.p_index() == integer)
            .unwrap();
        let offset = lane.lane_offset();
        assert_eq!(
            &lease.typed_lanes()[offset / 8].to_le_bytes(),
            &i64::MAX.to_le_bytes()
        );
    }
    let (status, actual) = execute_transport(execution, &lease);
    assert_eq!(status, 0);
    assert_eq!(actual[result], expected);
}

fn word_bytes(words: &[u64]) -> Vec<u8> {
    words.iter().flat_map(|word| word.to_le_bytes()).collect()
}

mod generated_captures;
mod lazy_assignments;
mod partial_base_reads;
mod slot_subscripts;

mod original_build_pairs;
