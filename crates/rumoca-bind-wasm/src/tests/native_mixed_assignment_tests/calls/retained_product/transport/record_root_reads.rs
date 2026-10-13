//! Actual same-product calls preserve selected record-root reads and fresh state.
use super::*;
use rumoca_exec_wasm::NativeModelLibrary;
use rumoca_ir_solve::fmi::FmiDeploymentCapabilities;

const SOURCE: &str = include_str!(
    "../../../../../../../../crates/rumoca/tests/fixtures/function_guarded_loops/RecordRootReads.mo"
);

#[test]
fn selected_nested_and_quoted_record_roots_execute_actual_native_wasm() {
    run_cases(
        SOURCE,
        "RecordRootReads",
        &["score", "quoted", "immutable"],
        &[
            (true, [1.0, 2.0], &[3.0, 4.0, 5.0]),
            (false, [1.0, 2.0], &[-1.0, -1.0, 5.0]),
            (true, [4.0, 5.0], &[6.0, 7.0, 11.0]),
            (true, [1.0, 2.0], &[3.0, 4.0, 5.0]),
        ],
    );
}

#[test]
fn a_written_record_field_executes_without_unused_sibling_defaults() {
    run_optional_requested_cases(
        SOURCE,
        "RecordFieldOnly",
        &["fieldOnly"],
        &[
            (true, [1.0, 2.0], &[2.0]),
            (false, [1.0, 2.0], &[2.0]),
            (true, [4.0, 5.0], &[5.0]),
            (true, [1.0, 2.0], &[2.0]),
        ],
        false,
    );
}

pub(super) fn run_cases(
    source: &str,
    model: &str,
    outputs: &[&str],
    cases: &[(bool, [f64; 2], &[f64])],
) {
    run_optional_requested_cases(source, model, outputs, cases, true);
}

fn run_optional_requested_cases(
    source: &str,
    model: &str,
    outputs: &[&str],
    cases: &[(bool, [f64; 2], &[f64])],
    check_requested: bool,
) {
    let cases = cases
        .iter()
        .map(|&(requested, values, expected)| SourceCase {
            requested: check_requested.then_some(requested),
            values,
            slots: None,
            expected,
        })
        .collect::<Vec<_>>();
    run_source_cases(source, model, outputs, &cases);
}

pub(super) struct SourceCase<'case> {
    pub(super) requested: Option<bool>,
    pub(super) values: [f64; 2],
    pub(super) slots: Option<[i64; 2]>,
    pub(super) expected: &'case [f64],
}

pub(super) fn run_source_cases(
    source: &str,
    model: &str,
    outputs: &[&str],
    cases: &[SourceCase<'_>],
) {
    let _lock = session_test_guard();
    with_source_component(source, model, |component| {
        let original = component.runtime_view().retain_assertion_root();
        assert!(std::ptr::eq(original.metadata(), component.metadata()));
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
        for case in cases {
            set_case_inputs(&mut transport, &artifact, case);
            let lease = transport.enter().unwrap();
            let (status, actual) = execute_transport(&mut execution, &lease);
            assert_eq!(status, 0);
            assert_eq!(outputs.len(), case.expected.len());
            for (&name, &expected) in outputs.iter().zip(case.expected) {
                assert_eq!(
                    actual[slot(&artifact, name, "Y")].to_bits(),
                    expected.to_bits()
                );
            }
        }
        Ok(String::new())
    })
    .unwrap();
}

fn set_case_inputs(
    transport: &mut NativeTransport,
    artifact: &serde_json::Value,
    case: &SourceCase<'_>,
) {
    if let Some(requested) = case.requested {
        transport
            .set_typed_input(
                slot(artifact, "requested", "P"),
                NativeTypedInput::Boolean(requested),
            )
            .unwrap();
    }
    for (index, value) in case.values.iter().enumerate() {
        let input = slot(artifact, &format!("values[{}]", index + 1), "P");
        transport
            .set_float64_input_batches(&[(input, std::slice::from_ref(value))])
            .unwrap();
        assert_eq!(transport.coordinates().1[input].to_bits(), value.to_bits());
    }
    if let Some(slots) = case.slots {
        for (index, value) in slots.into_iter().enumerate() {
            transport
                .set_typed_input(
                    slot(artifact, &format!("slots[{}]", index + 1), "P"),
                    NativeTypedInput::Integer(value),
                )
                .unwrap();
        }
    }
    let inputs = transport
        .input_values()
        .map(|(input, value)| (input.p_index(), value.unwrap()))
        .collect::<Vec<_>>();
    if let Some(requested) = case.requested {
        assert!(inputs.contains(&(
            slot(artifact, "requested", "P"),
            rumoca_ir_solve::SolveValueKind::Boolean(requested)
        )));
    }
    if let Some(slots) = case.slots {
        for (index, value) in slots.into_iter().enumerate() {
            assert!(inputs.contains(&(
                slot(artifact, &format!("slots[{}]", index + 1), "P"),
                rumoca_ir_solve::SolveValueKind::Integer(value)
            )));
        }
    }
}
