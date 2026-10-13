//! Retained native preparations keep their typed input storage and refuse
//! execution on a host that has no sealed engine.

mod retained_native_program {
    pub(super) mod fixture;
}

use retained_native_program::fixture::{self, BOOLEAN, INTEGER, REAL};
use rumoca_exec_wasm::{
    CheckedNativeWasmCapacityPolicy, NativeTransportError, NativeTypedInputBatch,
    PreparedNativeStorage, PreparedNativeWasmCode, WasmCompileError,
    compile_retained_native_preparation_wasm, compile_retained_native_preparation_wasm_carrying,
};
use rumoca_ir_solve::{NativeCarriedHost, NativeCarriedState, SolveValueKind};

const NO_ENGINE: &str = "native FMI execution has no sealed entry for this engine";

fn prepared(component: &rumoca_ir_solve::fmi::FmiComponent) -> PreparedNativeWasmCode {
    let root = component.runtime_view().retain_assertion_root();
    compile_retained_native_preparation_wasm(root.native_preparation().unwrap()).unwrap()
}

fn typed_input(storage: &PreparedNativeStorage, p_index: usize) -> SolveValueKind {
    storage
        .input_values()
        .find(|(lane, _)| lane.p_index() == p_index)
        .map(|(_, value)| value.unwrap())
        .expect("the typed input lane is part of the preparation")
}

#[test]
fn prepared_storage_checks_a_typed_batch_before_it_writes_it() {
    let component = fixture::component();
    let code = prepared(&component);
    let mut storage = code.storage(&[0.0], &[2.0, 0.0, 0.0]).unwrap();
    assert!(matches!(
        storage
            .input_values()
            .find(|(lane, _)| lane.p_index() == INTEGER)
            .unwrap()
            .1,
        Err(NativeTransportError::MissingInput { p_index: INTEGER })
    ));
    storage
        .check_typed_inputs(INTEGER, NativeTypedInputBatch::Integer(&[7]))
        .unwrap();
    assert!(storage.input_values().all(|(_, value)| value.is_err()));
    assert_eq!(
        storage.check_typed_inputs(INTEGER, NativeTypedInputBatch::Boolean(&[true])),
        Err(NativeTransportError::InputType { p_index: INTEGER })
    );
    assert_eq!(
        storage.set_typed_inputs(INTEGER, NativeTypedInputBatch::Boolean(&[true])),
        Err(NativeTransportError::InputType { p_index: INTEGER })
    );
    storage
        .set_typed_inputs(INTEGER, NativeTypedInputBatch::Integer(&[7]))
        .unwrap();
    storage
        .set_typed_inputs(
            BOOLEAN,
            NativeTypedInputBatch::BooleanRepeat {
                value: true,
                count: 1,
            },
        )
        .unwrap();
    assert_eq!(typed_input(&storage, INTEGER), SolveValueKind::Integer(7));
    assert_eq!(
        typed_input(&storage, BOOLEAN),
        SolveValueKind::Boolean(true)
    );
}

#[test]
fn prepared_storage_checks_float64_batches_without_writing_them() {
    let component = fixture::component();
    let code = prepared(&component);
    let mut storage = code.storage(&[0.0], &[2.0, 0.0, 0.0]).unwrap();
    storage
        .check_float64_input_batches(&[(REAL, &[3.5])])
        .unwrap();
    assert_eq!(storage.coordinates().1[REAL], 2.0);
    assert_eq!(
        storage.check_float64_input_batches(&[(REAL, &[f64::NAN])]),
        Err(NativeTransportError::InvalidInput { p_index: REAL })
    );
    assert_eq!(
        storage.set_float64_input_batches(&[(REAL, &[f64::NAN])]),
        Err(NativeTransportError::InvalidInput { p_index: REAL })
    );
    assert_eq!(storage.coordinates().1[REAL], 2.0);
    storage
        .set_float64_input_batches(&[(REAL, &[3.5])])
        .unwrap();
    assert_eq!(storage.coordinates().1[REAL], 3.5);
}

#[test]
fn a_host_admits_an_execution_entry_only_with_the_embedded_engine_at_every_stage() {
    let component = fixture::component();
    let code = prepared(&component);
    // A host built with the embedded engine admits the entry at construction;
    // any other host has none.
    let refused = |result: Result<(), WasmCompileError>| {
        if cfg!(feature = "host-engine") {
            assert!(result.is_ok());
        } else {
            assert!(
                matches!(result, Err(WasmCompileError::Input(message)) if message == NO_ENGINE)
            );
        }
    };
    refused(code.require_execution_entry());
    let storage = code.storage(&[0.0], &[2.0, 0.0, 0.0]).unwrap();
    refused(storage.require_execution_entry());
    let root = component.runtime_view().retain_assertion_root();
    let product = code
        .bind_resolved(root.resolved_periodic_schedules_at(2.0).unwrap())
        .unwrap();
    refused(product.require_execution_entry());
    let transport = storage
        .activate(root.resolved_periodic_schedules_at(2.0).unwrap())
        .unwrap_or_else(|(_, error)| panic!("{error}"));
    refused(transport.require_execution_entry());
}

#[test]
fn carried_state_is_published_into_p_and_leaves_the_code_without_an_entry() {
    let component = fixture::component();
    let root = component.runtime_view().retain_assertion_root();
    let host = NativeCarriedHost::new(
        (1, 3),
        std::iter::once(0..1),
        std::iter::once(REAL..REAL + 1),
    );
    let carried = NativeCarriedState::construct_for_host(&host, &[(0..1, REAL..REAL + 1)]).unwrap();
    let code = compile_retained_native_preparation_wasm_carrying(
        root.native_preparation().unwrap(),
        CheckedNativeWasmCapacityPolicy::default(),
        &carried,
    )
    .unwrap();
    assert_eq!(code.compiled().carried_state(), &carried);
    assert!(prepared(&component).compiled().carried_state().is_empty());
    assert!(matches!(
        code.require_execution_entry(),
        Err(WasmCompileError::Input(message))
            if message == "carried state publishes into P, which this transport passes read-only"
    ));
}

#[cfg(feature = "component-build")]
#[test]
fn a_linkable_model_keeps_the_original_emission_it_refines() {
    use rumoca_exec_wasm::NativeModelLibrary;
    use rumoca_ir_solve::fmi::FmiDeploymentCapabilities;

    let component = fixture::component();
    let code = prepared(&component);
    let deployment = component
        .into_codegen_view()
        .try_deployment(FmiDeploymentCapabilities::default())
        .unwrap();
    let library = NativeModelLibrary::from_preparation(code.clone(), deployment).unwrap();
    let linkable = library.into_linkable().unwrap();
    assert!(std::ptr::eq(
        linkable.original().compiled(),
        code.compiled()
    ));
    assert!(std::ptr::eq(linkable.original().inputs(), code.inputs()));
    assert_ne!(
        linkable.module_bytes(),
        code.compiled().module_bytes(),
        "the refinement omits the public memory export"
    );
}

#[cfg(feature = "component-build")]
#[test]
fn a_model_library_borrows_the_deployment_it_was_built_from() {
    use rumoca_exec_wasm::NativeModelLibrary;
    use rumoca_ir_solve::fmi::FmiDeploymentCapabilities;

    let component = fixture::component();
    let root = component.runtime_view().retain_assertion_root();
    let code = prepared(&component);
    let deployment = component
        .into_codegen_view()
        .try_deployment(FmiDeploymentCapabilities::default())
        .unwrap();
    let library = NativeModelLibrary::from_preparation(code, deployment).unwrap();
    let retained = library.deployment().runtime_view().retain_assertion_root();
    assert!(retained.shares_root_owner(&root));
}

#[cfg(feature = "host-engine")]
mod host_engine {
    use super::*;
    use rumoca_exec_wasm::NativeTypedInput;
    use wasmi::{Engine, Linker, Memory, MemoryType, Module, Store};

    /// Run the emitted module directly, with the caller's buffers laid out
    /// back to back in a fresh linear memory.
    fn direct(
        module_bytes: &[u8],
        (y, p, scratch, lanes): (&[f64], &[f64], &[u64], &[u64]),
        time: f64,
    ) -> (i32, Vec<f64>, Vec<u64>, Vec<u64>) {
        let engine = Engine::default();
        let module = Module::new(&engine, module_bytes).unwrap();
        let mut store = Store::new(&engine, ());
        let (p_at, scratch_at) = (y.len() * 8, (y.len() + p.len()) * 8);
        let lanes_at = scratch_at + scratch.len() * 8;
        let total = lanes_at + lanes.len() * 8;
        let memory = Memory::new(
            &mut store,
            MemoryType::new(total.div_ceil(65536).max(1) as u32, None),
        )
        .unwrap();
        let mut linker = Linker::new(&engine);
        linker.define("env", "memory", memory).unwrap();
        let instance = linker
            .instantiate(&mut store, &module)
            .unwrap()
            .start(&mut store)
            .unwrap();
        let words = y
            .iter()
            .chain(p)
            .map(|v| v.to_bits())
            .chain(scratch.iter().copied())
            .chain(lanes.iter().copied())
            .flat_map(u64::to_le_bytes)
            .collect::<Vec<_>>();
        memory.write(&mut store, 0, &words).unwrap();
        let lanes_ptr = if lanes.is_empty() { 0 } else { lanes_at as i32 };
        let status = instance
            .get_typed_func::<(i32, i32, f64, i32, i32), i32>(&store, "eval_assignments")
            .unwrap()
            .call(
                &mut store,
                (0, p_at as i32, time, scratch_at as i32, lanes_ptr),
            )
            .unwrap();
        let mut raw = vec![0u8; total];
        memory.read(&store, 0, &mut raw).unwrap();
        let word = |at: usize| u64::from_le_bytes(raw[at..at + 8].try_into().unwrap());
        (
            status,
            (0..y.len()).map(|i| f64::from_bits(word(i * 8))).collect(),
            (0..scratch.len())
                .map(|i| word(scratch_at + i * 8))
                .collect(),
            (0..lanes.len()).map(|i| word(lanes_at + i * 8)).collect(),
        )
    }

    #[test]
    fn a_retained_program_runs_through_the_host_entry_like_the_emitted_module() {
        let component = fixture::component();
        let root = component.runtime_view().retain_assertion_root();
        let code = prepared(&component);
        let mut storage = code.storage(&[0.0], &[2.0, 1.0, 1.0]).unwrap();
        storage
            .set_float64_input_batches(&[(REAL, &[3.0])])
            .unwrap();
        storage
            .set_typed_input(INTEGER, NativeTypedInput::Integer(5))
            .unwrap();
        storage
            .set_typed_input(BOOLEAN, NativeTypedInput::Boolean(true))
            .unwrap();
        let mut transport = storage
            .activate(root.resolved_periodic_schedules_at(0.5).unwrap())
            .unwrap_or_else(|(_, error)| panic!("{error}"));
        transport.require_execution_entry().unwrap();
        let lease = transport.enter().unwrap();
        let expected = direct(
            lease.product().compiled().module_bytes(),
            (lease.y(), lease.p(), lease.scratch(), lease.typed_lanes()),
            0.5,
        );
        assert_eq!(expected.0, 0);
        let outcome = lease.execute(0.5).unwrap();
        assert_eq!(
            outcome.y().iter().map(|v| v.to_bits()).collect::<Vec<_>>(),
            expected.1.iter().map(|v| v.to_bits()).collect::<Vec<_>>()
        );
        assert_ne!(outcome.y(), [0.0], "the entry wrote the assigned output");
        assert!(outcome.product().compiled().module_bytes().len() > 8);
        assert_eq!(outcome.derived_outputs().count(), 0);
        outcome.commit_buffers();
        assert_eq!(transport.coordinates().0, expected.1.as_slice());
    }
}
