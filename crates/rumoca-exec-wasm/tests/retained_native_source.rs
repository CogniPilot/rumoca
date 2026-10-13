//! Reported sources run through the retained native host entry, publishing
//! exact typed outputs and source-bound faults.

use std::collections::HashMap;

use rumoca_exec_wasm::{
    NativeTypedInput, TypedCallFaultKind, WasmCompileError,
    compile_retained_native_preparation_wasm,
};
use rumoca_ir_solve::{self as solve, SolveValueKind};

const GUARDED: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../rumoca-compile/src/session/tests/fixtures/reported_bugs/issue_403.mo"
));
const INTEGER: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../rumoca-compile/src/session/tests/fixtures/reported_bugs/issue_411.mo"
));

/// The FMI lowering needs a checked default for each input, which the
/// reported sources spell as a start value.
fn component(source: &str, model: &str) -> solve::fmi::FmiComponent {
    let mut session = rumoca_compile::Session::default();
    let source = source
        .replace("(start = 4);", " = 4;")
        .replace("(start = 0);", " = 0;");
    session.add_document("RetainedSource.mo", &source).unwrap();
    let compiled = session
        .compile_model(model)
        .expect("the reported source compiles");
    rumoca_phase_solve::fmi::lower_to_fmi_component(&compiled.dae, &HashMap::new())
        .expect("the compiled model lowers to an FMI component")
}

/// Run one step with the single Integer input `name` set to `value`.
fn execute_with<R>(
    component: &solve::fmi::FmiComponent,
    name: &str,
    value: i64,
    inspect: impl FnOnce(Result<rumoca_exec_wasm::NativeInvocationOutcome<'_>, WasmCompileError>) -> R,
) -> R {
    let root = component.runtime_view().retain_assertion_root();
    let code = compile_retained_native_preparation_wasm(root.native_preparation().unwrap())
        .expect("the source has an emitted native preparation");
    let model = root.model();
    let mut storage = code
        .storage(&model.initial_y, &model.parameters)
        .expect("the source layout sizes the buffers");
    let Some(solve::ScalarSlot::P { index, .. }) = model.problem.layout.binding(name) else {
        panic!("{name} is a parameter-storage input");
    };
    storage
        .set_typed_input(index, NativeTypedInput::Integer(value))
        .unwrap();
    let mut transport = storage
        .activate(root.resolved_periodic_schedules_at(0.0).unwrap())
        .unwrap_or_else(|(_, error)| panic!("{error}"));
    transport.require_execution_entry().unwrap();
    let lease = transport.enter().unwrap();
    inspect(lease.execute(0.0))
}

#[test]
fn an_active_out_of_range_access_surfaces_its_source_bound_typed_fault() {
    let active = GUARDED.replace("k, 3);", "k, 4);");
    assert_ne!(active, GUARDED);
    let component = component(&active, "GuardedOperands");
    execute_with(&component, "k", 4, |result| match result {
        Err(WasmCompileError::TypedSource(fault)) => {
            assert_eq!(fault.kind, TypedCallFaultKind::IndexBounds);
            assert!(!fault.provenance.is_dummy());
        }
        Err(other) => panic!("expected the typed index fault, got {other}"),
        Ok(_) => panic!("the active invalid access must not complete"),
    });
    execute_with(&component, "k", 3, |result| {
        assert!(result.is_ok(), "the in-range access completes");
    });
}

#[test]
fn a_derived_integer_output_is_read_back_exactly_from_its_typed_lane() {
    let component = component(INTEGER, "IntegerPrecision");
    for input in [0_i64, 1, 9_007_199_254_740_993, i64::MIN] {
        let expected = (9_007_199_254_740_993_i128 + i128::from(input)).rem_euclid(7);
        execute_with(&component, "n", input, |result| {
            let outcome = result.expect("the step completes");
            let outputs = outcome.derived_outputs().collect::<Vec<_>>();
            assert_eq!(outputs.len(), 1);
            assert!(
                matches!(outputs[0].1, SolveValueKind::Integer(got) if i128::from(got) == expected),
                "input {input}: {:?}",
                outputs[0].1
            );
        });
    }
}
