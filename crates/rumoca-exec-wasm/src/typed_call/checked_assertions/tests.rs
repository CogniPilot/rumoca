//! Controls execute the issued WASM, not a model of its effect instructions.
mod fixtures;
mod zero_result_flow;
use super::*;
use fixtures::*;
use rumoca_core::{SourceId, Span};
use std::num::NonZeroU64;
use wasmi::{Caller, Engine, Linker, Memory, MemoryType, Store, TypedFunc};

fn span(offset: usize) -> Span {
    Span::from_offsets(
        SourceId::from_source_name("native-assertion-order.mo"),
        offset,
        offset + 1,
    )
}

struct IssuedExecution {
    store: Store<usize>,
    memory: Memory,
    function: TypedFunc<(i32, i32, i32), i32>,
    pointers: (i32, i32, i32),
}

impl IssuedExecution {
    fn new(compiled: &CompiledCheckedAssertionWasm<'_>) -> Self {
        let engine = Engine::default();
        let module = wasmi::Module::new(&engine, compiled.artifact.module_bytes()).unwrap();
        let mut store = Store::new(&engine, 0usize);
        let layout = compiled.artifact.layout();
        let output = layout.input_bytes + 8;
        let scratch = output + layout.output_bytes + 8;
        let pages = (scratch + layout.scratch_bytes).div_ceil(65536).max(1);
        let memory = Memory::new(&mut store, MemoryType::new(pages, None)).unwrap();
        let mut linker = Linker::new(&engine);
        linker.define("env", "memory", memory).unwrap();
        linker
            .func_wrap("env", "sin", |mut caller: Caller<'_, usize>, value: f64| {
                *caller.data_mut() += 1;
                value.sin()
            })
            .unwrap();
        let instance = linker
            .instantiate(&mut store, &module)
            .unwrap()
            .start(&mut store)
            .unwrap();
        let function = instance
            .get_typed_func::<(i32, i32, i32), i32>(&store, "eval_typed_call")
            .unwrap();
        Self {
            store,
            memory,
            function,
            pointers: (0, output as i32, scratch as i32),
        }
    }

    fn call<'model, 'proof>(
        &mut self,
        compiled: &'proof CompiledCheckedAssertionWasm<'model>,
        arguments: &[u64],
    ) -> CheckedAssertionWasmOutcome<'model, 'proof> {
        let layout = compiled.artifact.layout();
        self.memory
            .write(&mut self.store, 0, &pack(arguments))
            .unwrap();
        let sentinel = vec![0x5a; layout.output_bytes as usize];
        self.memory
            .write(&mut self.store, self.pointers.1 as usize, &sentinel)
            .unwrap();
        let status = self.function.call(&mut self.store, self.pointers).unwrap() as u32;
        let memory = self.memory.data(&self.store);
        let output = &memory
            [self.pointers.1 as usize..self.pointers.1 as usize + layout.output_bytes as usize];
        if status != 0 {
            assert_eq!(output, sentinel);
        }
        let scratch = &memory
            [self.pointers.2 as usize..self.pointers.2 as usize + layout.scratch_bytes as usize];
        compiled
            .decode_execution(arguments, &unpack(output), &unpack(scratch), status)
            .unwrap()
    }
}

fn pack(cells: &[u64]) -> Vec<u8> {
    cells.iter().flat_map(|cell| cell.to_le_bytes()).collect()
}
fn unpack(bytes: &[u8]) -> Vec<u64> {
    bytes
        .chunks_exact(8)
        .map(|cell| u64::from_le_bytes(cell.try_into().unwrap()))
        .collect()
}

#[test]
fn fatal_check_precedes_later_fault_and_preserves_exact_capture_and_arguments() {
    let table = simple_table(solve::SolveAssertionLevel::Error, false, true);
    let site = table.owners()[0].call_site();
    assert!(matches!(
        crate::compile_pure_call_wasm(&table, &site),
        Err(TypedCallCompileError::UnsupportedOutputKind { .. })
    ));
    let program = invocation_program(site.clone());
    let actions = [action(site, solve::SolveAssertionLevel::Error)];
    let checked = solve::CheckedAssertionInvocation::new(&table, &program, 1, &actions).unwrap();
    let issued_action = &checked.projections()[0] as *const _;
    let compiled = compile_checked_assertion_wasm(checked).unwrap();
    assert!(std::ptr::eq(compiled.invocation().table(), &table));
    assert!(std::ptr::eq(
        compiled.invocation().program(),
        program.as_slice()
    ));
    assert!(std::ptr::eq(
        compiled.invocation().actions(),
        actions.as_slice()
    ));
    assert_eq!(compiled.invocation().operation(), 1);
    let retained_action = &compiled.invocation().projections()[0];
    assert!(std::ptr::eq(retained_action, issued_action));
    let retained_module = compiled.artifact().module_bytes().as_ptr();
    let mut execution = IssuedExecution::new(&compiled);
    let CheckedAssertionWasmOutcome::Stopped { observations } = execution.call(&compiled, &[0])
    else {
        panic!("fatal check did not stop")
    };
    assert_eq!(observations.len(), 1);
    assert_eq!(observations[0].source_span(), span(4));
    assert_eq!(
        observations[0].captures()[0].1.bits(),
        &[9_007_199_254_740_993]
    );
    assert_eq!(observations[0].frames()[0].arguments()[0].bits(), &[0]);
    assert!(std::ptr::eq(observations[0].action().action(), &actions[0]));
    assert!(std::ptr::eq(observations[0].action(), retained_action));
    let CheckedAssertionWasmOutcome::Fault {
        fault,
        observations,
    } = execution.call(&compiled, &[1])
    else {
        panic!("later fault was hidden")
    };
    assert_eq!(fault.provenance, span(10));
    assert!(
        observations.is_empty(),
        "previous invocation observations were replayed"
    );
    assert_eq!(compiled.artifact().module_bytes().as_ptr(), retained_module);
    let CheckedAssertionWasmOutcome::Stopped { observations } = execution.call(&compiled, &[0])
    else {
        panic!("retained product did not execute again")
    };
    assert!(std::ptr::eq(observations[0].action(), retained_action));
    assert_eq!(observations[0].frames()[0].arguments()[0].bits(), &[0]);
}

#[test]
fn message_fault_precedes_parent_observation_and_inactive_message_is_skipped() {
    let table = simple_table(solve::SolveAssertionLevel::Error, true, false);
    let site = table.owners()[0].call_site();
    let program = invocation_program(site.clone());
    let actions = [action(site, solve::SolveAssertionLevel::Error)];
    let checked = solve::CheckedAssertionInvocation::new(&table, &program, 1, &actions).unwrap();
    let compiled = compile_checked_assertion_wasm(checked).unwrap();
    let mut execution = IssuedExecution::new(&compiled);
    let CheckedAssertionWasmOutcome::Fault {
        fault,
        observations,
    } = execution.call(&compiled, &[0])
    else {
        panic!("message did not fault")
    };
    assert_eq!(fault.provenance, span(6));
    assert!(observations.is_empty());
    let CheckedAssertionWasmOutcome::Complete {
        values,
        observations,
    } = execution.call(&compiled, &[1])
    else {
        panic!("inactive message executed")
    };
    assert!(observations.is_empty());
    assert_eq!(values[0].bits(), &[42]);
    assert_eq!(values[2].bits(), &[0]);
}

#[test]
fn warning_is_retained_before_later_numerical_fault_without_result_publication() {
    let table = simple_table(solve::SolveAssertionLevel::Warning, false, true);
    let site = table.owners()[0].call_site();
    let program = invocation_program(site.clone());
    let actions = [action(site, solve::SolveAssertionLevel::Warning)];
    let checked = solve::CheckedAssertionInvocation::new(&table, &program, 1, &actions).unwrap();
    let compiled = compile_checked_assertion_wasm(checked).unwrap();
    let CheckedAssertionWasmOutcome::Fault {
        fault,
        observations,
    } = IssuedExecution::new(&compiled).call(&compiled, &[0])
    else {
        panic!("warning changed later fault")
    };
    assert_eq!(fault.provenance, span(10));
    assert_eq!(observations.len(), 1);
    assert_eq!(
        observations[0].action().action().kind,
        solve::SolveEventActionKind::Warning
    );
}

#[test]
fn forwarded_loop_records_distinguish_repeated_child_invocations() {
    let table = loop_table(solve::SolveAssertionLevel::Warning);
    let site = table.owners()[1].call_site();
    let program = invocation_program(site.clone());
    let actions = [action(site, solve::SolveAssertionLevel::Warning)];
    let checked = solve::CheckedAssertionInvocation::new(&table, &program, 1, &actions).unwrap();
    let compiled = compile_checked_assertion_wasm(checked).unwrap();
    assert_eq!(compiled.carrier.as_ref().unwrap().capacity, 3);
    let CheckedAssertionWasmOutcome::Complete {
        values,
        observations,
    } = IssuedExecution::new(&compiled).call(&compiled, &[])
    else {
        panic!("warning fold did not complete")
    };
    assert_eq!(observations.len(), 3);
    for (index, observation) in observations.iter().enumerate() {
        assert_eq!(observation.frames().len(), 2);
        assert_eq!(
            observation.frames()[0].domain_points(),
            &[vec![index as i64 + 1]]
        );
        assert_eq!(
            observation.frames()[1].arguments()[0].bits(),
            &[index as u64 + 1]
        );
        assert_eq!(observation.captures()[0].1.bits(), &[index as u64 + 1]);
        let source = compiled
            .carrier
            .as_ref()
            .unwrap()
            .sources
            .iter()
            .find(|source| source.source_span() == observation.source_span())
            .expect("every decoded record has its planned source");
        assert_eq!(
            source.predicate_output(),
            observation.action().predicate_output()
        );
        assert_eq!(source.frames().len(), observation.frames().len());
        for (index, (descriptor, frame)) in
            source.frames().iter().zip(observation.frames()).enumerate()
        {
            assert_eq!(descriptor.owner(), frame.owner().id());
            assert_eq!(descriptor.region_path(), frame.region_path());
            assert_eq!(descriptor.operation(), frame.operation());
            assert_eq!(descriptor.domains().len(), frame.domain_points().len());
            assert!(!frame.program().operations().is_empty());
            if index > 0 {
                let types = frame
                    .arguments()
                    .iter()
                    .map(|argument| argument.value_type().clone())
                    .collect::<Vec<_>>();
                assert_eq!(types, descriptor.arguments);
            }
        }
    }
    assert_eq!(values[1].bits(), &[1]);
    assert_eq!(values[2].bits(), &[0]);
}

#[test]
fn fatal_forwarded_loop_preserves_child_source_and_stops_first_iteration() {
    let table = loop_table(solve::SolveAssertionLevel::Error);
    let site = table.owners()[1].call_site();
    let program = invocation_program(site.clone());
    let actions = [action(site, solve::SolveAssertionLevel::Error)];
    let checked = solve::CheckedAssertionInvocation::new(&table, &program, 1, &actions).unwrap();
    let compiled = compile_checked_assertion_wasm(checked).unwrap();
    let CheckedAssertionWasmOutcome::Stopped { observations } =
        IssuedExecution::new(&compiled).call(&compiled, &[])
    else {
        panic!("fatal child did not stop parent")
    };
    assert_eq!(observations.len(), 1);
    assert_eq!(observations[0].source_span(), span(24));
    assert_eq!(observations[0].frames()[0].domain_points(), &[vec![1]]);
}

#[test]
fn buffer_refusal_grants_no_carrier_read_and_following_call_resets_it() {
    let table = simple_table(solve::SolveAssertionLevel::Error, false, false);
    let site = table.owners()[0].call_site();
    let program = invocation_program(site.clone());
    let actions = [action(site, solve::SolveAssertionLevel::Error)];
    let checked = solve::CheckedAssertionInvocation::new(&table, &program, 1, &actions).unwrap();
    let compiled = compile_checked_assertion_wasm(checked).unwrap();
    let mut execution = IssuedExecution::new(&compiled);
    let carrier = compiled.carrier.as_ref().unwrap();
    let header = execution.pointers.2 as usize + carrier.range.offset as usize;
    execution
        .memory
        .write(&mut execution.store, header, &u64::MAX.to_le_bytes())
        .unwrap();
    let original_output = execution.pointers.1;
    execution.pointers.1 = 0;
    let CheckedAssertionWasmOutcome::Fault {
        fault,
        observations,
    } = execution.call(&compiled, &[1])
    else {
        panic!("overlapping buffers executed")
    };
    assert_eq!(fault.kind, crate::TypedCallFaultKind::InvalidBuffer);
    assert!(observations.is_empty());
    assert_eq!(
        &execution.memory.data(&execution.store)[header..header + 8],
        &u64::MAX.to_le_bytes()
    );
    execution.pointers.1 = original_output;
    let CheckedAssertionWasmOutcome::Complete { observations, .. } =
        execution.call(&compiled, &[1])
    else {
        panic!("valid call did not recover")
    };
    assert!(observations.is_empty());
}

#[test]
fn source_recipe_cannot_rebind_wrong_program_or_severity() {
    let table = simple_table(solve::SolveAssertionLevel::Error, false, false);
    let site = table.owners()[0].call_site();
    let program = invocation_program(site.clone());
    let wrong_actions = [action(site.clone(), solve::SolveAssertionLevel::Warning)];
    assert!(solve::CheckedAssertionInvocation::new(&table, &program, 1, &wrong_actions).is_none());
    let actions = [action(site, solve::SolveAssertionLevel::Error)];
    assert!(solve::CheckedAssertionInvocation::new(&table, &program, 0, &actions).is_none());
}

#[test]
fn compact_execution_bound_overflow_is_a_typed_refusal() {
    let table = loop_table_with_upper(solve::SolveAssertionLevel::Warning, 1i64 << 31);
    let site = table.owners()[1].call_site();
    let program = invocation_program(site.clone());
    let actions = [action(site, solve::SolveAssertionLevel::Warning)];
    let checked = solve::CheckedAssertionInvocation::new(&table, &program, 1, &actions).unwrap();
    assert!(matches!(
        compile_checked_assertion_wasm(checked),
        Err(WasmCompileError::TypedAdmission(
            TypedCallCompileError::SizeLimit
        ))
    ));
}

#[test]
fn failed_message_math_import_is_linked_and_evaluated_once_on_failed_path() {
    let table = math_message_table();
    let site = table.owners()[0].call_site();
    let program = invocation_program(site.clone());
    let actions = [action(site, solve::SolveAssertionLevel::Error)];
    let checked = solve::CheckedAssertionInvocation::new(&table, &program, 1, &actions).unwrap();
    let compiled = compile_checked_assertion_wasm(checked).unwrap();
    assert_eq!(compiled.artifact().math_imports(), &["sin"]);
    let mut execution = IssuedExecution::new(&compiled);
    let CheckedAssertionWasmOutcome::Stopped { observations } = execution.call(&compiled, &[0])
    else {
        panic!("math message did not complete before stop")
    };
    assert_eq!(
        observations[0].captures()[0].1.bits(),
        &[1.0f64.sin().to_bits()]
    );
    assert_eq!(*execution.store.data(), 1);
    let CheckedAssertionWasmOutcome::Complete {
        values,
        observations,
    } = execution.call(&compiled, &[1])
    else {
        panic!("passed assertion stopped")
    };
    assert!(observations.is_empty());
    assert_eq!(values[2].bits(), &[0.0f64.to_bits()]);
    assert_eq!(*execution.store.data(), 1);
}

#[test]
fn a_host_call_refuses_a_wrong_input_footprint_and_has_no_engine_to_run() {
    let table = simple_table(solve::SolveAssertionLevel::Error, false, false);
    let site = table.owners()[0].call_site();
    let program = invocation_program(site.clone());
    let actions = [action(site, solve::SolveAssertionLevel::Error)];
    let checked = solve::CheckedAssertionInvocation::new(&table, &program, 1, &actions).unwrap();
    let compiled = compile_checked_assertion_wasm(checked).unwrap();
    assert!(matches!(
        compiled.call(&[]),
        Err(WasmCompileError::Input(message))
            if message == "checked invocation input footprint differs"
    ));
    assert!(matches!(
        compiled.call(&[0]),
        Err(WasmCompileError::Input(message))
            if message == "checked native observations have no admitted engine on this target"
    ));
}

#[test]
fn a_fault_status_the_artifact_never_issued_is_an_inconsistent_carrier() {
    let table = simple_table(solve::SolveAssertionLevel::Error, false, false);
    let site = table.owners()[0].call_site();
    let program = invocation_program(site.clone());
    let actions = [action(site, solve::SolveAssertionLevel::Error)];
    let checked = solve::CheckedAssertionInvocation::new(&table, &program, 1, &actions).unwrap();
    let compiled = compile_checked_assertion_wasm(checked).unwrap();
    assert!(matches!(
        compiled.decode_execution(&[0], &[], &[], u32::MAX),
        Err(WasmCompileError::Backend(message))
            if message == "issued native observation carrier is inconsistent"
    ));
}
