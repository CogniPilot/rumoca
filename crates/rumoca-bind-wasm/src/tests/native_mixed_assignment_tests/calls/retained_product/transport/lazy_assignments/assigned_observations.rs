//! Complete native entries and checked observers retain the original read guard.
use super::*;
use rumoca_eval_solve::{AssertionInvocationEvaluation, TypedValue};
use rumoca_ir_solve::{CheckedAssertionInvocation, RetainedAssertionSource};

#[test]
fn unconditional_body_read_stops_complete_entries_and_checked_observers_in_actual_native_wasm() {
    let original = "y := if enabled then sum(scratch) else -1;";
    assert_eq!(SOURCE.matches(original).count(), 1);
    let source = SOURCE.replace(original, "y := sum(scratch);");
    let _lock = session_test_guard();
    with_source_component(&source, "LazyBodyOnly", |component| {
        let root = component
            .runtime_view()
            .retain_assertion_root()
            .resolved_periodic_schedules_at(0.0)
            .unwrap();
        let code = rumoca_exec_wasm::compile_retained_native_preparation_wasm(
            root.native_preparation().unwrap(),
        )
        .unwrap();
        let host = code.bind_resolved(root.clone()).unwrap().into_host();
        let artifact = execution_description(host.product());
        let mut execution = CallExecution::uninitialized(&artifact);
        let y = vec![37.25; root.model().problem.layout.y_scalars()];
        let mut transport = host
            .transport_from_source(&y, &root.model().parameters)
            .unwrap();
        let run = CaseRun {
            source: &source,
            model: "LazyBodyOnly",
            fault_kind: rumoca_exec_wasm::TypedCallFaultKind::AssertionFailed,
            code: &code,
            original: &root,
            artifact: &artifact,
        };
        let retained = assigned_read_invocation(&root);
        let checked = CheckedAssertionInvocation::new(
            retained.table(),
            retained.program(),
            retained.operation(),
            retained.actions(),
        )
        .unwrap();
        assert!(std::ptr::eq(checked.owner(), retained.owner()));
        let observed = rumoca_exec_wasm::compile_checked_assertion_wasm(checked).unwrap();
        let mut observer = IssuedObservation::new(&observed);
        for (enabled, values, expected) in [
            (true, [1.0, 2.0], Some(3.0)),
            (false, [4.0, 5.0], None),
            (true, [4.0, 5.0], Some(9.0)),
            (false, [1.0, 2.0], None),
            (true, [1.0, 2.0], Some(3.0)),
        ] {
            let case = Case {
                values,
                booleans: vec![("enabled", enabled)],
                integers: Vec::new(),
                expected: expected.map(|value| ("y", value)).into_iter().collect(),
                fault_expression: None,
            };
            run.write_inputs(&mut transport, &case);
            run.assert_inputs_visible(&transport, &case);
            let lease = transport.enter().unwrap();
            let (status, actual) = execute_transport(&mut execution, &lease);
            if enabled {
                assert_eq!(status, 0);
                run.assert_expected_outputs(&case, &actual);
            } else {
                // A complete entry has no later observer to reject this read.
                run.assert_first_fault("y := sum(scratch);", status, &actual, &lease);
            }
            let arguments = typed_arguments(retained.owner(), values, enabled);
            assert_observed_read(&run, &observed, &mut observer, &arguments, &case);
        }
        Ok(String::new())
    })
    .unwrap();
}

fn assigned_read_invocation(
    root: &rumoca_ir_solve::RetainedAssertionRoot,
) -> rumoca_ir_solve::RetainedAssertionInvocation {
    let block = &root.model().problem.events.action_conditions;
    let mut issued = block
        .programs()
        .iter()
        .enumerate()
        .flat_map(|(program, operations)| {
            (0..operations.len()).filter_map(move |operation| {
                root.checked_invocation(
                    RetainedAssertionSource::ActionConditions,
                    program,
                    operation,
                )
                .ok()
            })
        });
    let invocation = issued
        .next()
        .expect("the original read has an issued observer");
    assert!(issued.next().is_none(), "this source has one original call");
    invocation
}

fn typed_arguments(
    owner: &rumoca_ir_solve::SolvePureCallOwner,
    values: [f64; 2],
    enabled: bool,
) -> [TypedValue; 2] {
    [
        TypedValue::construct(
            owner.inputs()[0].clone(),
            values
                .into_iter()
                .map(|value| SolveValueKind::Real64(value.to_bits()))
                .collect(),
        )
        .unwrap(),
        TypedValue::construct(
            owner.inputs()[1].clone(),
            vec![SolveValueKind::Boolean(enabled)],
        )
        .unwrap(),
    ]
}

fn assert_observed_read(
    run: &CaseRun<'_>,
    observed: &rumoca_exec_wasm::CompiledCheckedAssertionWasm<'_>,
    observer: &mut IssuedObservation,
    arguments: &[TypedValue],
    case: &Case,
) {
    let enabled = case.booleans[0].1;
    let packed = [
        case.values[0].to_bits(),
        case.values[1].to_bits(),
        u64::from(enabled),
    ];
    let (status, output) = observer.call(observed, &packed);
    let reference =
        rumoca_eval_solve::eval_assertion_invocation(observed.invocation(), arguments).unwrap();
    if enabled {
        let AssertionInvocationEvaluation::Complete { values, reports } = reference else {
            panic!("a present read must recover without old reports");
        };
        assert_eq!(status, 0);
        assert!(reports.is_empty());
        let value = f64::from_le_bytes(output[..8].try_into().unwrap());
        assert_eq!(
            values[0].elements(),
            &[SolveValueKind::Real64(value.to_bits())]
        );
        assert_eq!(value, case.expected[0].1);
    } else {
        let AssertionInvocationEvaluation::Failed { reports } = reference else {
            panic!("the original checked read must report its absence");
        };
        assert_eq!(reports.len(), 1);
        assert_ne!(status, 0, "the checked source-owned read must stop");
        let fault = observed
            .artifact()
            .faults()
            .iter()
            .find(|fault| fault.status == status as u32)
            .unwrap();
        assert_eq!(fault.kind, run.fault_kind);
        assert_eq!(fault.owner, observed.invocation().owner().id());
        run.assert_assertion_fault_owner(fault);
        assert_eq!(fault.provenance, reports[0].span);
        let projection = observed
            .invocation()
            .projections()
            .iter()
            .find(|projection| projection.action_index() == reports[0].action_index)
            .unwrap();
        assert!(std::ptr::eq(
            projection.action(),
            &run.original.model().problem.events.actions[reports[0].action_index]
        ));
        let start = run.source.find("y := sum(scratch);").unwrap();
        assert!(
            fault.provenance.start.0 >= start
                && fault.provenance.end.0 <= start + "y := sum(scratch);".len()
        );
    }
}

/// Execute the issued checked artifact with the existing three-pointer ABI.
struct IssuedObservation {
    store: wasmi::Store<()>,
    memory: wasmi::Memory,
    function: wasmi::TypedFunc<(i32, i32, i32), i32>,
    pointers: (i32, i32, i32),
}

impl IssuedObservation {
    fn new(compiled: &rumoca_exec_wasm::CompiledCheckedAssertionWasm<'_>) -> Self {
        let engine = wasmi::Engine::default();
        let module = wasmi::Module::new(&engine, compiled.artifact().module_bytes()).unwrap();
        let mut store = wasmi::Store::new(&engine, ());
        let layout = compiled.artifact().layout();
        let output = layout.input_bytes + 8;
        let scratch = output + layout.output_bytes + 8;
        let pages = (scratch + layout.scratch_bytes).div_ceil(65536).max(1);
        let memory = wasmi::Memory::new(&mut store, wasmi::MemoryType::new(pages, None)).unwrap();
        let mut linker = wasmi::Linker::new(&engine);
        linker.define("env", "memory", memory).unwrap();
        let instance = linker
            .instantiate(&mut store, &module)
            .unwrap()
            .start(&mut store)
            .unwrap();
        let function = instance.get_typed_func(&store, "eval_typed_call").unwrap();
        Self {
            store,
            memory,
            function,
            pointers: (0, output as i32, scratch as i32),
        }
    }

    fn call(
        &mut self,
        compiled: &rumoca_exec_wasm::CompiledCheckedAssertionWasm<'_>,
        arguments: &[u64],
    ) -> (i32, Vec<u8>) {
        let input = arguments
            .iter()
            .flat_map(|value| value.to_le_bytes())
            .collect::<Vec<_>>();
        self.memory.write(&mut self.store, 0, &input).unwrap();
        let sentinel = vec![0x5a; compiled.artifact().layout().output_bytes as usize];
        self.memory
            .write(&mut self.store, self.pointers.1 as usize, &sentinel)
            .unwrap();
        let status = self.function.call(&mut self.store, self.pointers).unwrap();
        let mut output = vec![0; sentinel.len()];
        self.memory
            .read(&self.store, self.pointers.1 as usize, &mut output)
            .unwrap();
        if status != 0 {
            assert_eq!(
                output, sentinel,
                "a failed original read publishes no output cells"
            );
        }
        (status, output)
    }
}
