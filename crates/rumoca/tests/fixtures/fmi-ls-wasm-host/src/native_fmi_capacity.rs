//! Separate capacity control requiring an actual admitted large Real input.

use super::*;

pub(super) fn run(path: &str, token: &str, args: &[String]) -> Result<()> {
    ensure!(
        args.len() == 2,
        "native-fmi-capacity needs original Real input VR and extent"
    );
    let reference: u32 = args[0].parse()?;
    let extent: usize = args[1].parse()?;
    ensure!(reference != 0 && reference != u32::MAX);
    ensure!(
        (65_536..=1_048_576).contains(&extent),
        "capacity fixture must keep actual buffers bounded"
    );
    let mut fmu = load(path, token)?;
    let interface = fmu.world.fmi_fmi3_co_simulation().co_simulation_instance();
    let payload = vec![-0.0_f64; extent];
    ensure!(
        interface.call_set_float64(&mut fmu.store, fmu.instance, &[reference], &payload)?
            == Status::Ok
    );
    check_payload(&mut fmu, reference, extent)?;
    // The guest is wasm32. Count fits u32, but Float64 byte capacity exceeds isize32.
    let repeats = ((i32::MAX as u64) / ((extent as u64) * 8) + 1) as usize;
    ensure!(repeats <= 4096 && (repeats as u64) * (extent as u64) <= u32::MAX as u64);
    let references = vec![reference; repeats];
    let interface = fmu.world.fmi_fmi3_co_simulation().co_simulation_instance();
    ensure!(
        interface.call_get_float64(&mut fmu.store, fmu.instance, &references)?
            == Err(Status::Error),
        "unaddressable repeated-VR result succeeded; outer trap is also a failure"
    );
    ensure!(
        interface.call_get_float64(&mut fmu.store, fmu.instance, &[reference, u32::MAX])?
            == Err(Status::Error)
    );
    check_payload(&mut fmu, reference, extent)?;
    let interface = fmu.world.fmi_fmi3_co_simulation().co_simulation_instance();
    ensure!(
        interface.call_reset(&mut fmu.store, fmu.instance)? == Status::Ok,
        "capacity refusal lost original owner"
    );
    fmu.instance.resource_drop(&mut fmu.store)?;
    println!(
        "NATIVE_FMI_CAPACITY_OK extent={extent} repeated_references={repeats} status=Error owner-preserved no-trap"
    );
    Ok(())
}

fn check_payload(fmu: &mut Fmu, reference: u32, extent: usize) -> Result<()> {
    let values = fmu
        .world
        .fmi_fmi3_co_simulation()
        .co_simulation_instance()
        .call_get_float64(&mut fmu.store, fmu.instance, &[reference])?
        .map_err(|status| anyhow::anyhow!("actual capacity fixture getter: {status:?}"))?;
    ensure!(
        values.len() == extent,
        "actual getter cardinality differs from source manifest"
    );
    ensure!(values.first().unwrap().to_bits() == (-0.0_f64).to_bits());
    ensure!(values[extent / 2].to_bits() == (-0.0_f64).to_bits());
    ensure!(values.last().unwrap().to_bits() == (-0.0_f64).to_bits());
    Ok(())
}
