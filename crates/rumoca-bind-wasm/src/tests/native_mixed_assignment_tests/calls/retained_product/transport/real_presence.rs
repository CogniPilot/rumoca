//! Real payload storage shares the original typed input presence owner.

use super::*;

const SOURCE: &str = r#"
model RealPresenceProbe
  input Real missing[2];
  input Real authored[2](start={-0.0,0.0});
  input Real bound = 0.0;
  input Integer exact(start=0);
  input Boolean disabled(start=false);
  parameter Real coefficient = 1.0;
  output Integer result;
equation
  result = exact;
end RealPresenceProbe;
"#;

#[test]
fn native_real_source_presence_and_checked_batches_are_atomic() {
    let _lock = session_test_guard();
    with_source_component(SOURCE, "RealPresenceProbe", |component| {
        let root = component.runtime_view().retain_assertion_root();
        let code = rumoca_exec_wasm::compile_retained_native_preparation_wasm(root.native_preparation().unwrap()).unwrap();
        let base = |name| root.metadata().variables().iter().find(|variable| variable.name() == name).unwrap().storage().unwrap().base();
        let missing = base("missing");
        let authored = base("authored");
        let bound = base("bound");
        let exact = base("exact");
        let coefficient = base("coefficient");
        let mut storage = code.original_storage_from_source().unwrap();
        assert_eq!(storage.coordinates().1[authored].to_bits(), (-0.0f64).to_bits());
        assert_eq!(storage.coordinates().1[authored + 1].to_bits(), 0.0f64.to_bits());
        storage.check_float64_inputs(authored, 2).unwrap();
        storage.check_float64_inputs(bound, 1).unwrap();
        assert_eq!(storage.check_float64_inputs(missing, 2), Err(NativeTransportError::MissingInput { p_index: missing }));
        assert!(storage.input_values().any(|(_, value)| value == Ok(rumoca_ir_solve::SolveValueKind::Boolean(false))));
        let (y, p) = storage.coordinates();
        let y = y.to_vec();
        let mut p = p.to_vec();
        p[missing] = 3.0;
        p[missing + 1] = 4.0;
        p[authored] = 99.0;
        let supplied = code.storage_from_source(&y, &p).unwrap();
        assert_eq!(supplied.coordinates().1[authored].to_bits(), (-0.0f64).to_bits());
        assert_eq!(supplied.coordinates().1[missing], 3.0);
        assert_eq!(supplied.check_float64_inputs(missing, 2), Err(NativeTransportError::MissingInput { p_index: missing }));
        storage.set_coordinates(&y, &p).unwrap();
        assert_eq!(storage.check_float64_inputs(missing, 2), Err(NativeTransportError::MissingInput { p_index: missing }));
        let original: Vec<_> = storage.coordinates().1.iter().map(|value| value.to_bits()).collect();
        for batches in [
            vec![(missing, &[8.0,9.0][..]), (exact, &[1.0][..])],
            vec![(coefficient, &[3.0][..]), (exact, &[1.0][..])],
            vec![(missing, &[8.0,9.0][..]), (authored, &[1.0,2.0,3.0][..])],
            vec![(missing, &[8.0,9.0][..]), (bound, &[f64::NAN][..])],
            vec![(missing, &[8.0,9.0][..]), (usize::MAX, &[1.0][..])],
        ] {
            assert!(storage.set_float64_input_batches(&batches).is_err());
            assert_eq!(storage.coordinates().1.iter().map(|value| value.to_bits()).collect::<Vec<_>>(), original);
            assert_eq!(storage.check_float64_inputs(missing, 2), Err(NativeTransportError::MissingInput { p_index: missing }));
        }
        // SPEC_0044 ME-LSW-003e: a Real parameter is written through its P cell.
        storage.set_float64_input_batches(&[(coefficient, &[2.0])]).unwrap();
        assert_eq!(storage.coordinates().1[coefficient], 2.0);
        assert_eq!(storage.check_float64_inputs(missing, 2), Err(NativeTransportError::MissingInput { p_index: missing }));
        assert_eq!(storage.set_float64_input_batches(&[(exact, &[1.0])]), Err(NativeTransportError::InputType { p_index: exact }));
        assert_eq!(storage.set_float64_input_batches(&[(bound, &[f64::INFINITY])]), Err(NativeTransportError::InvalidInput { p_index: bound }));
        storage.set_float64_input_batches(&[(missing, &[5.0,6.0]), (authored, &[-0.0,7.0])]).unwrap();
        storage.check_float64_inputs(missing, 2).unwrap();
        assert_eq!(&storage.coordinates().1[missing..missing + 2], &[5.0,6.0]);
        let mut foreign = code.original_storage_from_source().unwrap();
        assert_eq!(foreign.restore(&storage.snapshot().unwrap()), Err(NativeTransportError::ForeignSnapshot));
        assert_eq!(foreign.check_float64_inputs(missing, 2), Err(NativeTransportError::MissingInput { p_index: missing }));
        let first_root = root.resolved_periodic_schedules_at(2.0).unwrap();
        let mut active = storage.activate(first_root).unwrap_or_else(|(_, error)| panic!("{error}"));
        let lease = active.enter().unwrap();
        assert_eq!(foreign.set_float64_input_batches(&[(missing, &[1.0,2.0])]), Err(NativeTransportError::Busy));
        assert_eq!(foreign.check_float64_inputs(authored, 2), Err(NativeTransportError::Busy));
        drop(lease);
        let storage = active.reset_to_preparation().unwrap_or_else(|(_, error)| panic!("{error}"));
        assert_eq!(storage.coordinates().1[authored].to_bits(), (-0.0f64).to_bits());
        assert_eq!(storage.coordinates().1[authored + 1].to_bits(), 0.0f64.to_bits());
        assert_eq!(storage.check_float64_inputs(missing, 2), Err(NativeTransportError::MissingInput { p_index: missing }));
        let second_root = root.resolved_periodic_schedules_at(5.0).unwrap();
        let mut active = storage.activate(second_root).unwrap_or_else(|(_, error)| panic!("{error}"));
        assert!(matches!(active.enter(), Err(NativeTransportError::MissingInput { p_index }) if p_index == missing));
        active.set_float64_input_batches(&[(missing, &[11.0,12.0])]).unwrap();
        assert!(active.enter().is_ok());
        Ok(String::new())
    }).unwrap();
}
