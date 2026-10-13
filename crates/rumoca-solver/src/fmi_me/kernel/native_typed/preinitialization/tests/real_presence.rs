//! Exact checked FMI writes, rather than raw P, authorize a Missing Real input.

use super::*;

#[test]
fn pending_real_getter_and_complete_setter_batch_preserve_missing_on_refusal() {
    let component = fixture::missing_real_component();
    let mut resource = pending(&component);
    let raw = resource.coordinates().1[0].to_bits();
    let mut output = [-23.0];
    assert!(resource.get_native_float64(&[2], &mut output).is_err());
    assert_eq!(output, [-23.0]);
    for (references, values) in [
        (vec![2, 3], vec![7.25, 1.0]),
        (vec![2, 2], vec![7.25, f64::NAN]),
    ] {
        assert!(resource.set_native_float64(&references, &values).is_err());
        assert_eq!(resource.coordinates().1[0].to_bits(), raw);
        assert!(resource.get_native_float64(&[2], &mut output).is_err());
        assert_eq!(output, [-23.0]);
    }
    resource.set_native_float64(&[2], &[7.25]).unwrap();
    resource.get_native_float64(&[2], &mut output).unwrap();
    assert_eq!(output, [7.25]);
    resource.reset().unwrap();
    assert_eq!(resource.coordinates().1[0].to_bits(), raw);
    assert!(resource.get_native_float64(&[2], &mut output).is_err());
    assert_eq!(output, [7.25]);
}

#[test]
fn shared_session_float64_setter_grants_presence_and_continuation_restore_removes_it() {
    let component = fixture::missing_real_component();
    let mut kernel = match pending(&component).enter_initialization_mode_owned(&config(2.0)) {
        Ok(kernel) => kernel,
        Err(_) => panic!("ownership adoption does not execute the missing input"),
    };
    let reference = kernel.value_reference("real").unwrap();
    let missing = kernel.fmu_state().unwrap();
    let mut output = [-29.0];
    assert!(
        kernel
            .get_float64(std::slice::from_ref(&reference), &mut output)
            .is_err()
    );
    assert_eq!(output, [-29.0]);
    assert!(kernel.get_native_float64(&[2], &mut output).is_err());
    assert_eq!(output, [-29.0]);
    assert!(
        kernel
            .set_float64(std::slice::from_ref(&reference), &[f64::NAN])
            .is_err()
    );
    assert!(kernel.verification_matches_snapshot(&missing));
    kernel
        .set_float64(std::slice::from_ref(&reference), &[7.25])
        .unwrap();
    kernel.get_native_float64(&[2], &mut output).unwrap();
    assert_eq!(output, [7.25]);
    kernel
        .get_float64(std::slice::from_ref(&reference), &mut output)
        .unwrap();
    assert_eq!(output, [7.25]);
    kernel.reset_to_fmu_state(&missing).unwrap();
    assert!(kernel.get_native_float64(&[2], &mut output).is_err());
    kernel.set_native_float64(&[2], &[-0.0]).unwrap();
    kernel
        .get_float64(std::slice::from_ref(&reference), &mut output)
        .unwrap();
    assert_eq!(output[0].to_bits(), (-0.0_f64).to_bits());
    let assigned = kernel.fmu_state().unwrap();
    let foreign = fixture::missing_real_component();
    let foreign_kernel = match pending(&foreign).enter_initialization_mode_owned(&config(2.0)) {
        Ok(kernel) => kernel,
        Err(_) => panic!("the foreign owner control adopts its own source"),
    };
    let foreign_reference = foreign_kernel.value_reference("real").unwrap();
    let mut outputs = [101.0, 103.0];
    assert!(
        kernel
            .get_float64(&[reference, foreign_reference], &mut outputs)
            .is_err()
    );
    assert_eq!(outputs, [101.0, 103.0]);
    assert!(kernel.verification_matches_snapshot(&assigned));
    let mut prepared = MePendingNativeComponent::from_reset_kernel(kernel)
        .unwrap_or_else(|_| panic!("source reset consumes the same original-pristine resource"));
    assert!(prepared.get_native_float64(&[2], &mut output).is_err());
    prepared.set_native_float64(&[2], &[11.0]).unwrap();
    let kernel = match prepared.enter_initialization_mode_owned(&config(5.0)) {
        Ok(kernel) => kernel,
        Err(_) => panic!("new initialization reuses the same source at its actual time"),
    };
    kernel.get_native_float64(&[2], &mut output).unwrap();
    assert_eq!(output, [11.0]);
    assert_eq!(kernel.native_start_time(), Some(5.0));
}
