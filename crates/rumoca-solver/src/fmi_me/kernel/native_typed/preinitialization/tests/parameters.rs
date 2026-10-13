//! Parameters and constants are typed cells, settable only before ExitInitializationMode.

use super::*;

const LIMIT: u32 = 5;
const FLAG: u32 = 6;
const GAIN: u32 = 7;
const COUNT: u32 = 8;

fn read_all(
    get_integer: impl Fn(&[u32], &mut [i64]) -> Result<(), MeError>,
    get_boolean: impl Fn(&[u32], &mut [bool]) -> Result<(), MeError>,
    get_real: impl Fn(&[u32], &mut [f64]) -> Result<(), MeError>,
) -> (i64, bool, f64, i64) {
    let (mut limit, mut flag, mut gain, mut count) = ([0], [false], [0.0], [0]);
    get_integer(&[LIMIT], &mut limit).unwrap();
    get_boolean(&[FLAG], &mut flag).unwrap();
    get_real(&[GAIN], &mut gain).unwrap();
    get_integer(&[COUNT], &mut count).unwrap();
    (limit[0], flag[0], gain[0], count[0])
}

#[test]
fn pending_parameters_read_their_source_values_and_constants_stay_read_only() {
    let component = fixture::parameter_component();
    let mut pending = pending(&component);
    let read = |pending: &MePendingNativeComponent| {
        read_all(
            |r, v| pending.get_native_integer(r, v),
            |r, v| pending.get_boolean(r, v),
            |r, v| pending.get_native_float64(r, v),
        )
    };
    assert_eq!(read(&pending), (5, true, 1.5, 7));
    assert!(pending.set_native_integer(&[COUNT], &[9]).is_err());
    assert_eq!(read(&pending), (5, true, 1.5, 7));
    pending.set_native_integer(&[LIMIT], &[11]).unwrap();
    pending.set_boolean(&[FLAG], &[false]).unwrap();
    pending.set_native_float64(&[GAIN], &[2.5]).unwrap();
    assert_eq!(read(&pending), (11, false, 2.5, 7));
}

#[test]
fn an_integer_parameter_the_program_reads_as_real_refuses_a_value_beyond_two_to_the_53() {
    let component = fixture::parameter_component();
    let mut pending = pending(&component);
    let beyond = (1_i64 << 53) + 1;
    assert!(pending.set_native_integer(&[LIMIT], &[beyond]).is_err());
    assert!(pending.set_native_integer(&[LIMIT], &[-beyond]).is_err());
    let mut limit = [0];
    pending.get_native_integer(&[LIMIT], &mut limit).unwrap();
    assert_eq!(limit, [5]);
    pending.set_native_integer(&[LIMIT], &[1 << 53]).unwrap();
}

#[test]
fn parameters_are_writable_in_initialization_mode_and_refused_after_it() {
    let component = fixture::parameter_component();
    let mut resource = pending(&component);
    write_inputs(&mut resource);
    resource.set_native_integer(&[LIMIT], &[11]).unwrap();
    let mut kernel = match resource.enter_initialization_mode_owned(&config(2.0)) {
        Ok(kernel) => kernel,
        Err(_) => panic!("the pending owner adopts once"),
    };
    let read = |kernel: &SolveMeKernel| {
        read_all(
            |r, v| kernel.get_native_integer(r, v),
            |r, v| kernel.get_boolean(r, v),
            |r, v| kernel.get_native_float64(r, v),
        )
    };
    assert_eq!(kernel.lifecycle.state(), MeState::InitializationMode);
    assert_eq!(read(&kernel), (11, true, 1.5, 7));
    kernel.set_native_integer(&[LIMIT], &[12]).unwrap();
    kernel.set_boolean(&[FLAG], &[false]).unwrap();
    kernel.set_native_float64(&[GAIN], &[3.5]).unwrap();
    assert!(kernel.set_native_integer(&[COUNT], &[9]).is_err());
    assert_eq!(read(&kernel), (12, false, 3.5, 7));

    kernel
        .lifecycle
        .restore_for_verification(MeState::ContinuousTimeMode);
    assert!(kernel.set_native_integer(&[LIMIT], &[13]).is_err());
    assert!(kernel.set_boolean(&[FLAG], &[true]).is_err());
    assert!(kernel.set_native_float64(&[GAIN], &[4.5]).is_err());
    assert!(kernel.check_native_integer_inputs(&[LIMIT], &[13]).is_err());
    assert_eq!(read(&kernel), (12, false, 3.5, 7));
}

#[test]
fn the_typed_inventory_covers_every_published_typed_parameter_and_constant() {
    let component = fixture::parameter_component();
    let root = component.runtime_view().retain_assertion_root();
    let inputs = root.native_preparation().unwrap();
    let mut covered = Vec::new();
    for variable in root.metadata().variables() {
        if variable.value_kind() == rumoca_ir_solve::SolveVariableValueKind::Real {
            continue;
        }
        rumoca_ir_solve::fmi::check_native_typed_membership(inputs.schedule(), variable)
            .unwrap_or_else(|error| panic!("{}: {error}", variable.name()));
        covered.push(variable.name());
    }
    assert_eq!(covered, ["integer", "enabled", "limit", "flag", "count"]);
}
