use super::*;

/// Typed array access uses the same C ABI as the native FMI 3 component.
pub fn run(component_path: &str, token: &str, args: &[String]) -> Result<()> {
    let references = args
        .iter()
        .map(|arg| arg.parse())
        .collect::<Result<Vec<u32>, _>>()?;
    ensure!(
        references.len() == 4,
        "typed mode needs Integer, Boolean, enumeration, state references"
    );
    let [integers, booleans, enumeration, state] = references[..] else {
        unreachable!()
    };
    let mut fmu = load(component_path, token)?;
    let interface = fmu.world.fmi_fmi3_co_simulation().co_simulation_instance();
    ensure!(
        interface.call_enter_initialization_mode(
            &mut fmu.store,
            fmu.instance,
            None,
            0.0,
            Some(1.0)
        )? == Status::Ok
    );
    ensure!(
        interface.call_set_int32(&mut fmu.store, fmu.instance, &[integers], &[3, -4])?
            == Status::Ok,
        "Integer array setter refused"
    );
    ensure!(
        interface.call_set_boolean(&mut fmu.store, fmu.instance, &[booleans], &[false, true])?
            == Status::Ok,
        "Boolean array setter refused"
    );
    ensure!(
        interface.call_set_int64(&mut fmu.store, fmu.instance, &[enumeration], &[2])? == Status::Ok,
        "enumeration setter refused"
    );
    ensure!(interface.call_exit_initialization_mode(&mut fmu.store, fmu.instance)? == Status::Ok);

    // Repeated references concatenate complete arrays, in request order.
    ensure!(
        interface.call_get_int32(&mut fmu.store, fmu.instance, &[integers, integers])?
            == Ok(vec![3, -4, 3, -4])
    );
    ensure!(
        interface.call_get_boolean(&mut fmu.store, fmu.instance, &[booleans, booleans])?
            == Ok(vec![false, true, false, true])
    );
    ensure!(interface.call_get_int64(&mut fmu.store, fmu.instance, &[enumeration])? == Ok(vec![2]));
    check_rejections(&mut fmu, integers, booleans, enumeration)?;
    advance_and_reset(&mut fmu, integers, booleans, enumeration, state)?;
    println!("OK typed arrays, repeated references, transactional rejection, step, reset");
    Ok(())
}

fn check_rejections(fmu: &mut Fmu, integers: u32, booleans: u32, enumeration: u32) -> Result<()> {
    let interface = fmu.world.fmi_fmi3_co_simulation().co_simulation_instance();
    // C request validation owns type, size, reference, and ordinal rejection.
    ensure!(
        interface.call_get_int64(&mut fmu.store, fmu.instance, &[integers])? == Err(Status::Error)
    );
    ensure!(
        interface.call_get_int32(&mut fmu.store, fmu.instance, &[enumeration])?
            == Err(Status::Error)
    );
    ensure!(
        interface.call_get_float64(&mut fmu.store, fmu.instance, &[integers])?
            == Err(Status::Error)
    );
    ensure!(
        interface.call_get_int32(&mut fmu.store, fmu.instance, &[booleans])? == Err(Status::Error)
    );
    ensure!(
        interface.call_get_boolean(&mut fmu.store, fmu.instance, &[u32::MAX])?
            == Err(Status::Error)
    );
    ensure!(
        interface.call_set_int32(&mut fmu.store, fmu.instance, &[integers], &[7])? == Status::Error
    );
    ensure!(
        interface.call_set_int32(
            &mut fmu.store,
            fmu.instance,
            &[integers, booleans],
            &[7, 8, 9, 10]
        )? == Status::Error
    );
    ensure!(
        interface.call_set_boolean(&mut fmu.store, fmu.instance, &[booleans], &[true])?
            == Status::Error
    );
    ensure!(
        interface.call_set_int64(&mut fmu.store, fmu.instance, &[enumeration], &[4])?
            == Status::Error
    );
    ensure!(
        interface.call_get_int32(&mut fmu.store, fmu.instance, &[integers])? == Ok(vec![3, -4])
    );
    ensure!(
        interface.call_get_boolean(&mut fmu.store, fmu.instance, &[booleans])?
            == Ok(vec![false, true])
    );
    ensure!(interface.call_get_int64(&mut fmu.store, fmu.instance, &[enumeration])? == Ok(vec![2]));
    Ok(())
}

fn advance_and_reset(
    fmu: &mut Fmu,
    integers: u32,
    booleans: u32,
    enumeration: u32,
    state: u32,
) -> Result<()> {
    let interface = fmu.world.fmi_fmi3_co_simulation().co_simulation_instance();
    ensure!(
        interface
            .call_do_step(&mut fmu.store, fmu.instance, 0.0, 0.1, true)?
            .is_ok()
    );
    let state_value = interface
        .call_get_float64(&mut fmu.store, fmu.instance, &[state])?
        .map_err(|status| anyhow::anyhow!("state getter failed: {status:?}"))?;
    ensure!(
        (state_value[0] + 0.4).abs() < 1.0e-10,
        "typed inputs did not drive the shared kernel"
    );
    ensure!(
        interface.call_set_int32(&mut fmu.store, fmu.instance, &[integers], &[11, 12])?
            == Status::Ok
    );
    ensure!(
        interface.call_set_boolean(&mut fmu.store, fmu.instance, &[booleans], &[true, false])?
            == Status::Ok
    );
    ensure!(
        interface
            .call_do_step(&mut fmu.store, fmu.instance, 0.1, 0.1, true)?
            .is_ok()
    );
    let state_value = interface
        .call_get_float64(&mut fmu.store, fmu.instance, &[state])?
        .map_err(|status| anyhow::anyhow!("state getter failed: {status:?}"))?;
    ensure!(
        (state_value[0] - 0.7).abs() < 1.0e-10,
        "updated typed inputs did not drive the shared kernel"
    );
    ensure!(
        interface.call_get_int32(&mut fmu.store, fmu.instance, &[integers])? == Ok(vec![11, 12])
    );
    ensure!(interface.call_reset(&mut fmu.store, fmu.instance)? == Status::Ok);
    ensure!(interface.call_get_int32(&mut fmu.store, fmu.instance, &[integers])? == Ok(vec![1, 2]));
    ensure!(
        interface.call_get_boolean(&mut fmu.store, fmu.instance, &[booleans])?
            == Ok(vec![true, false])
    );
    ensure!(interface.call_get_int64(&mut fmu.store, fmu.instance, &[enumeration])? == Ok(vec![1]));
    Ok(())
}
