//! Actual source-bound native execution, including selected first-fault order.
use super::*;
use rumoca_exec_wasm::NativeModelLibrary;
use rumoca_ir_solve::{SolveValueKind, fmi::FmiDeploymentCapabilities};
const SOURCE: &str = include_str!(
    "../../../../../../../../crates/rumoca/tests/fixtures/function_guarded_loops/LazyAssignment.mo"
);

struct Case {
    values: [f64; 2],
    booleans: Vec<(&'static str, bool)>,
    integers: Vec<(&'static str, i64)>,
    expected: Vec<(&'static str, f64)>,
    fault_expression: Option<&'static str>,
}

#[test]
fn selected_lazy_assignment_arrays_and_fold_calls_execute_actual_native_wasm() {
    let cases = [
        (true, 0, [1.0, 2.0], [3.0, 100.0, 3.0, 6.0]),
        (false, 1, [1.0, 2.0], [-1.0, 3.0, 1.0, -2.0]),
        (false, 1, [-1.0, 2.0], [-1.0, -1.0, -1.0, -2.0]),
        (true, 0, [4.0, 5.0], [9.0, 100.0, 9.0, 18.0]),
        (true, 0, [1.0, 2.0], [3.0, 100.0, 3.0, 6.0]),
    ]
    .into_iter()
    .map(|(enabled, probe, values, expected)| Case {
        values,
        booleans: vec![("enabled", enabled)],
        integers: vec![("probe", probe)],
        expected: [
            "bodyResult",
            "orderedResult",
            "inactiveResult",
            "foldResult",
        ]
        .into_iter()
        .zip(expected)
        .collect(),
        fault_expression: None,
    })
    .collect::<Vec<_>>();
    run_cases("LazyAssignment", &cases);
}

#[test]
fn selected_body_array_reads_execute_actual_native_wasm_independently() {
    run_isolated_cases("LazyBodyOnly", false);
}

#[test]
fn selected_ordered_conditions_execute_actual_native_wasm_independently() {
    run_isolated_cases("LazyOrderedOnly", true);
}

fn run_isolated_cases(model: &str, ordered: bool) {
    let cases = [
        (true, [1.0, 2.0], 3.0, 100.0),
        (false, [1.0, 2.0], -1.0, 3.0),
        (false, [-1.0, 2.0], -1.0, -1.0),
        (true, [4.0, 5.0], 9.0, 100.0),
        (true, [1.0, 2.0], 3.0, 100.0),
    ]
    .into_iter()
    .map(|(enabled, values, body, ordered_value)| Case {
        values,
        booleans: vec![("enabled", enabled)],
        integers: Vec::new(),
        expected: vec![("y", if ordered { ordered_value } else { body })],
        fault_expression: None,
    })
    .collect::<Vec<_>>();
    run_cases(model, &cases);
}

#[test]
fn conditional_faults_precede_arm_reads_and_target_index_faults() {
    let cases = [
        (1, 1, 2, None, 2.0),
        (0, 0, 0, Some("flags[conditionIndex]"), 0.0),
        (1, 0, 0, Some("values[sourceIndex]"), 0.0),
        (2, 0, 0, Some("target[destinationIndex]"), 0.0),
        (2, 0, 2, None, 1.0),
        (1, 1, 2, None, 2.0),
    ]
    .into_iter()
    .map(
        |(condition, source, destination, fault_expression, y)| Case {
            values: [1.0, 2.0],
            booleans: vec![("flags[1]", true), ("flags[2]", false)],
            integers: vec![
                ("conditionIndex", condition),
                ("sourceIndex", source),
                ("destinationIndex", destination),
            ],
            expected: vec![("y", y)],
            fault_expression,
        },
    )
    .collect::<Vec<_>>();
    run_cases("LazyIndexOrder", &cases);
}

fn run_cases(model: &str, cases: &[Case]) {
    run_source_cases(
        SOURCE,
        model,
        cases,
        rumoca_exec_wasm::TypedCallFaultKind::IndexBounds,
    );
}

fn run_source_cases(
    source: &str,
    model: &str,
    cases: &[Case],
    fault_kind: rumoca_exec_wasm::TypedCallFaultKind,
) {
    let _lock = session_test_guard();
    with_source_component(source, model, |component| {
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
        let run = CaseRun {
            source,
            model,
            fault_kind,
            code: &code,
            original: &original,
            artifact: &artifact,
        };
        for case in cases {
            run.write_inputs(&mut transport, case);
            run.assert_inputs_visible(&transport, case);
            let lease = transport.enter().unwrap();
            let (status, actual) = execute_transport(&mut execution, &lease);
            if let Some(expression) = case.fault_expression {
                run.assert_first_fault(expression, status, &actual, &lease);
            } else {
                assert_eq!(status, 0);
                run.assert_expected_outputs(case, &actual);
            }
            if fault_kind == rumoca_exec_wasm::TypedCallFaultKind::AssertionFailed && status == 0 {
                drop(lease);
                let p = transport.coordinates().1.to_vec();
                transport.set_coordinates(&actual, &p).unwrap();
            }
        }
        Ok(String::new())
    })
    .unwrap();
}

/// The emitted program and original root one case table executes against.
struct CaseRun<'a> {
    source: &'a str,
    model: &'a str,
    fault_kind: rumoca_exec_wasm::TypedCallFaultKind,
    code: &'a rumoca_exec_wasm::PreparedNativeWasmCode,
    original: &'a rumoca_ir_solve::RetainedAssertionRoot,
    artifact: &'a serde_json::Value,
}

impl CaseRun<'_> {
    fn write_inputs(&self, transport: &mut NativeTransport, case: &Case) {
        for (index, value) in case.values.iter().enumerate() {
            let input = slot(self.artifact, &format!("values[{}]", index + 1), "P");
            transport
                .set_float64_input_batches(&[(input, std::slice::from_ref(value))])
                .unwrap();
            assert_eq!(transport.coordinates().1[input].to_bits(), value.to_bits());
        }
        for &(name, value) in &case.booleans {
            transport
                .set_typed_input(
                    slot(self.artifact, name, "P"),
                    NativeTypedInput::Boolean(value),
                )
                .unwrap();
        }
        for &(name, value) in &case.integers {
            transport
                .set_typed_input(
                    slot(self.artifact, name, "P"),
                    NativeTypedInput::Integer(value),
                )
                .unwrap();
        }
    }

    fn assert_inputs_visible(&self, transport: &NativeTransport, case: &Case) {
        let inputs = transport
            .input_values()
            .map(|(input, value)| (input.p_index(), value.unwrap()))
            .collect::<Vec<_>>();
        let expected = case
            .booleans
            .iter()
            .map(|&(name, value)| (name, SolveValueKind::Boolean(value)))
            .chain(
                case.integers
                    .iter()
                    .map(|&(name, value)| (name, SolveValueKind::Integer(value))),
            );
        for (name, value) in expected {
            assert!(inputs.contains(&(slot(self.artifact, name, "P"), value)));
        }
    }

    fn assert_expected_outputs(&self, case: &Case, actual: &[f64]) {
        for &(name, value) in &case.expected {
            assert_eq!(
                actual[slot(self.artifact, name, "Y")].to_bits(),
                value.to_bits(),
                "{}.{name}",
                self.model
            );
        }
    }

    fn assert_first_fault(
        &self,
        expression: &str,
        status: i32,
        actual: &[f64],
        lease: &NativeInvocationLease<'_>,
    ) {
        assert_ne!(status, 0, "the selected source-owned read must fault");
        let fault = self
            .code
            .compiled()
            .faults()
            .iter()
            .find(|fault| fault.status == status as u32)
            .expect("the fault belongs to the same emitted source program");
        assert_eq!(fault.kind, self.fault_kind);
        if self.fault_kind == rumoca_exec_wasm::TypedCallFaultKind::AssertionFailed {
            self.assert_assertion_fault_owner(fault);
        }
        fault
            .provenance
            .require_provenance("lazy assignment first fault")
            .unwrap();
        let start = self.source.find(expression).unwrap();
        assert!(
            fault.provenance.start.0 >= start && fault.provenance.end.0 <= start + expression.len(),
            "{expression}: {:?}",
            fault.provenance
        );
        assert_eq!(
            actual,
            lease.y(),
            "a failed invocation preserves its actual incoming outputs"
        );
    }

    fn assert_assertion_fault_owner(&self, fault: &rumoca_exec_wasm::TypedCallFault) {
        assert_eq!(fault.opcode, "CheckAssertion");
        let owner = self.original.model().pure_calls.owner(fault.owner).unwrap();
        assert!(
            fault.region_path.is_empty(),
            "this unconditional source read is in the original owner root"
        );
        let operation = owner
            .body()
            .operations()
            .get(fault.operation.unwrap())
            .unwrap();
        assert_eq!(operation.provenance(), fault.provenance);
        let rumoca_ir_solve::SolveOperation::CheckAssertion {
            predicate_output, ..
        } = operation.operation()
        else {
            panic!("assigned read fault must identify its actual checked operation")
        };
        assert!(
            owner
                .assertion_flow()
                .unwrap()
                .assigned_read_predicates()
                .contains(predicate_output)
        );
    }
}

mod assigned_observations;
