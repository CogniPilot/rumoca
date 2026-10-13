//! Export defaults initialize and reset the existing native cell/presence owner.

use super::*;
use rumoca_exec_wasm::{NativeTypedInput, PreparedNativeStorage};
use rumoca_ir_solve::SolveValueKind;

#[test]
fn fmi_export_defaults_initialize_and_reset_original_native_cells_after_replay() {
    let dae = compile(
        "model NativeDefaults input Real u[2](each min=2); input Real z(start=-0.0); input Integer i(min=7); input Boolean b; output Integer result; equation result=i; end NativeDefaults;",
        "NativeDefaults",
    );
    let component = super::super::super::fmi::lower_fmi_export_component(&dae).unwrap();
    check_component(&component);
    check_component(&replay(wire(&component)).unwrap());
}

fn check_component(component: &FmiComponent) {
    let root = component.runtime_view().retain_assertion_root();
    let code = rumoca_exec_wasm::compile_retained_native_preparation_wasm(
        root.native_preparation().unwrap(),
    )
    .unwrap();
    let base = |name| variable(component, name).storage().unwrap().base();
    let u = base("u");
    let z = base("z");
    let i = base("i");
    let b = base("b");
    for name in ["u", "i", "b"] {
        assert!(variable(component, name).declaration_start().is_missing());
    }
    let mut storage = code.original_storage_from_source().unwrap();
    check_defaults(&storage, u, z);
    storage
        .set_float64_input_batches(&[(u, &[9.0, 10.0])])
        .unwrap();
    storage
        .set_typed_input(i, NativeTypedInput::Integer(9))
        .unwrap();
    storage
        .set_typed_input(b, NativeTypedInput::Boolean(true))
        .unwrap();
    assert_eq!(&storage.coordinates().1[u..u + 2], &[9.0, 10.0]);
    let mut active = storage
        .activate(root.resolved_periodic_schedules_at(2.0).unwrap())
        .unwrap_or_else(|(_, error)| panic!("{error}"));
    drop(active.enter().unwrap());
    let storage = active
        .reset_to_preparation()
        .unwrap_or_else(|(_, error)| panic!("{error}"));
    check_defaults(&storage, u, z);
    assert!(variable(component, "u").declaration_start().is_missing());
}

fn check_defaults(storage: &PreparedNativeStorage, u: usize, z: usize) {
    storage.check_float64_inputs(u, 2).unwrap();
    assert_eq!(&storage.coordinates().1[u..u + 2], &[2.0, 2.0]);
    assert_eq!(storage.coordinates().1[z].to_bits(), (-0.0_f64).to_bits());
    let values = storage
        .input_values()
        .map(|(_, value)| value.unwrap())
        .collect::<Vec<_>>();
    assert!(values.contains(&SolveValueKind::Integer(7)));
    assert!(values.contains(&SolveValueKind::Boolean(false)));
    assert!(!values.contains(&SolveValueKind::Integer(9)));
    assert!(!values.contains(&SolveValueKind::Boolean(true)));
}
