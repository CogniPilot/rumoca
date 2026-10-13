//! Canonical FMI world controls for the actual fixed native ReturnPredicates product.
//! VR arguments come from the issuer's original inventory, not a name scan.

use super::*;

struct References {
    samples: u32,
    k: u32,
    first: u32,
    result: u32,
}

impl References {
    fn parse(args: &[String]) -> Result<Self> {
        ensure!(
            args.len() == 4,
            "native-fmi needs samples, k, first, result VRs from the issuer"
        );
        let values = args
            .iter()
            .map(|value| value.parse::<u32>())
            .collect::<Result<Vec<_>, _>>()?;
        ensure!(values.iter().all(|value| *value != 0 && *value != u32::MAX));
        for (index, value) in values.iter().enumerate() {
            ensure!(
                !values[..index].contains(value),
                "issuer VRs must be distinct"
            );
        }
        Ok(Self {
            samples: values[0],
            k: values[1],
            first: values[2],
            result: values[3],
        })
    }
}

pub(super) fn run(path: &str, token: &str, args: &[String]) -> Result<()> {
    let refs = References::parse(args)?;
    let mut fmu = load(path, token)?;
    let first = fmu.instance;
    let second = check_instantiation(&mut fmu, token)?;
    check_defaults(&mut fmu, first, &refs)?;
    check_defaults(&mut fmu, second, &refs)?;
    check_preparation(&mut fmu, first, &refs)?;
    check_rejections(&mut fmu, first, &refs)?;
    initialize(&mut fmu, first, 2.0)?;
    check_result(&mut fmu, first, refs.result, 1.0)?;
    advance(&mut fmu, first, 2.0, 0.125)?;
    check_active_cases(&mut fmu, first, &refs)?;
    check_defaults(&mut fmu, second, &refs)?;
    check_reset_and_isolation(&mut fmu, first, second, &refs)?;
    check_source_fault(&mut fmu, first, &refs)?;
    first.resource_drop(&mut fmu.store)?;
    second.resource_drop(&mut fmu.store)?;
    println!(
        "NATIVE_FMI_WIT_OK init=2 step reset init=5 exact-types request-refusals source-fault-refusal/free resources=2"
    );
    Ok(())
}

fn check_instantiation(fmu: &mut Fmu, token: &str) -> Result<ResourceAny> {
    let interface = fmu.world.fmi_fmi3_co_simulation().co_simulation_instance();
    ensure!(
        interface
            .call_instantiate_co_simulation(
                &mut fmu.store,
                "foreign",
                &format!("{token}:foreign"),
                "",
                false,
                false,
                false,
                false,
                &[],
            )?
            .is_none(),
        "wrong token acquired a resource"
    );
    for (visible, event_mode, early, intermediates) in [
        (true, false, false, Vec::new()),
        (false, true, false, Vec::new()),
        (false, false, true, Vec::new()),
        (false, false, false, vec![0]),
    ] {
        ensure!(
            interface
                .call_instantiate_co_simulation(
                    &mut fmu.store,
                    "unsupported",
                    token,
                    "",
                    visible,
                    false,
                    event_mode,
                    early,
                    &intermediates,
                )?
                .is_none(),
            "unsupported instance capability succeeded"
        );
    }
    interface
        .call_instantiate_co_simulation(
            &mut fmu.store,
            "native-second",
            token,
            "",
            false,
            true,
            false,
            false,
            &[],
        )?
        .context("second original resource rejected")
}

fn check_defaults(fmu: &mut Fmu, instance: ResourceAny, refs: &References) -> Result<()> {
    let interface = fmu.world.fmi_fmi3_co_simulation().co_simulation_instance();
    ensure!(
        interface.call_get_float64(&mut fmu.store, instance, &[refs.samples])? == Ok(vec![5.0])
    );
    ensure!(interface.call_get_int64(&mut fmu.store, instance, &[refs.k])? == Ok(vec![1]));
    ensure!(interface.call_get_boolean(&mut fmu.store, instance, &[refs.first])? == Ok(vec![true]));
    ensure!(
        interface.call_get_float64(&mut fmu.store, instance, &[0])? == Err(Status::Error),
        "preparation invented time"
    );
    ensure!(
        interface.call_get_float64(&mut fmu.store, instance, &[refs.result])? == Err(Status::Error),
        "preparation invented output"
    );
    Ok(())
}

fn check_preparation(fmu: &mut Fmu, instance: ResourceAny, refs: &References) -> Result<()> {
    let interface = fmu.world.fmi_fmi3_co_simulation().co_simulation_instance();
    ensure!(
        interface.call_set_int64(&mut fmu.store, instance, &[refs.k], &[i64::MAX])? == Status::Ok
    );
    ensure!(
        interface.call_set_float64(&mut fmu.store, instance, &[refs.samples], &[-0.0])?
            == Status::Ok
    );
    let values = interface
        .call_get_float64(&mut fmu.store, instance, &[refs.samples, refs.samples])?
        .map_err(|status| anyhow::anyhow!("prepared Float64 getter: {status:?}"))?;
    ensure!(
        values.len() == 2
            && values
                .iter()
                .all(|value| value.to_bits() == (-0.0_f64).to_bits())
    );
    ensure!(
        interface.call_get_int64(&mut fmu.store, instance, &[refs.k, refs.k])?
            == Ok(vec![i64::MAX; 2])
    );
    ensure!(
        interface.call_exit_initialization_mode(&mut fmu.store, instance)? == Status::Error,
        "unentered ExitInit succeeded"
    );
    ensure!(
        interface.call_get_int64(&mut fmu.store, instance, &[refs.k])? == Ok(vec![i64::MAX]),
        "failed ExitInit lost accepted input"
    );
    ensure!(
        interface.call_set_float64(&mut fmu.store, instance, &[refs.samples], &[5.0])?
            == Status::Ok
    );
    Ok(())
}

fn check_rejections(fmu: &mut Fmu, instance: ResourceAny, refs: &References) -> Result<()> {
    let interface = fmu.world.fmi_fmi3_co_simulation().co_simulation_instance();
    ensure!(
        interface.call_set_float64(
            &mut fmu.store,
            instance,
            &[refs.samples, u32::MAX],
            &[99.0, 0.0]
        )? == Status::Error
    );
    ensure!(
        interface.call_set_float64(&mut fmu.store, instance, &[refs.samples], &[f64::NAN])?
            == Status::Error
    );
    ensure!(
        interface.call_set_float64(&mut fmu.store, instance, &[refs.samples], &[])?
            == Status::Error
    );
    ensure!(
        interface.call_set_int64(&mut fmu.store, instance, &[refs.k, refs.first], &[7, 0])?
            == Status::Error
    );
    ensure!(interface.call_set_int64(&mut fmu.store, instance, &[refs.k], &[])? == Status::Error);
    ensure!(
        interface.call_set_boolean(
            &mut fmu.store,
            instance,
            &[refs.first, u32::MAX],
            &[false, false]
        )? == Status::Error
    );
    ensure!(
        interface.call_set_boolean(&mut fmu.store, instance, &[refs.first], &[false, false])?
            == Status::Error
    );
    ensure!(
        interface.call_get_float64(&mut fmu.store, instance, &[refs.samples, refs.k])?
            == Err(Status::Error)
    );
    ensure!(interface.call_get_int32(&mut fmu.store, instance, &[refs.k])? == Err(Status::Error));
    ensure!(
        interface.call_get_boolean(&mut fmu.store, instance, &[u32::MAX])? == Err(Status::Error)
    );
    ensure!(
        interface.call_get_float64(&mut fmu.store, instance, &[refs.samples])? == Ok(vec![5.0]),
        "late refusal changed Real"
    );
    ensure!(
        interface.call_get_int64(&mut fmu.store, instance, &[refs.k])? == Ok(vec![i64::MAX]),
        "late refusal changed Integer"
    );
    ensure!(
        interface.call_get_boolean(&mut fmu.store, instance, &[refs.first])? == Ok(vec![true]),
        "late refusal changed Boolean"
    );
    Ok(())
}

fn initialize(fmu: &mut Fmu, instance: ResourceAny, start: f64) -> Result<()> {
    let interface = fmu.world.fmi_fmi3_co_simulation().co_simulation_instance();
    ensure!(
        interface.call_enter_initialization_mode(
            &mut fmu.store,
            instance,
            None,
            start,
            Some(start + 1.0)
        )? == Status::Ok
    );
    ensure!(interface.call_get_float64(&mut fmu.store, instance, &[0])? == Ok(vec![start]));
    ensure!(
        interface.call_enter_initialization_mode(
            &mut fmu.store,
            instance,
            None,
            start,
            Some(start + 1.0)
        )? == Status::Error,
        "entered Init repeated successfully"
    );
    ensure!(interface.call_exit_initialization_mode(&mut fmu.store, instance)? == Status::Ok);
    ensure!(
        interface.call_exit_initialization_mode(&mut fmu.store, instance)? == Status::Error,
        "completed Init repeated successfully"
    );
    Ok(())
}

fn check_result(fmu: &mut Fmu, instance: ResourceAny, reference: u32, expected: f64) -> Result<()> {
    let values = fmu
        .world
        .fmi_fmi3_co_simulation()
        .co_simulation_instance()
        .call_get_float64(&mut fmu.store, instance, &[reference])?
        .map_err(|status| anyhow::anyhow!("actual result getter: {status:?}"))?;
    ensure!(
        values.len() == 1 && values[0].to_bits() == expected.to_bits(),
        "source result {values:?}, expected {expected:?}"
    );
    Ok(())
}

fn advance(fmu: &mut Fmu, instance: ResourceAny, current: f64, step: f64) -> Result<()> {
    let interface = fmu.world.fmi_fmi3_co_simulation().co_simulation_instance();
    for (bad_current, bad_step) in [
        (current - 1.0, step),
        (current, 0.0),
        (current, f64::NAN),
        (current, 2.0),
    ] {
        ensure!(matches!(
            interface.call_do_step(&mut fmu.store, instance, bad_current, bad_step, true)?,
            Err(Status::Error)
        ));
        ensure!(
            interface.call_get_float64(&mut fmu.store, instance, &[0])? == Ok(vec![current]),
            "refused step changed time"
        );
    }
    let result = interface
        .call_do_step(&mut fmu.store, instance, current, step, true)?
        .map_err(|status| anyhow::anyhow!("actual DoStep: {status:?}"))?;
    ensure!(result.last_successful_time.to_bits() == (current + step).to_bits());
    ensure!(!result.event_handling_needed && !result.terminate_simulation && !result.early_return);
    Ok(())
}

fn check_active_cases(fmu: &mut Fmu, instance: ResourceAny, refs: &References) -> Result<()> {
    let interface = fmu.world.fmi_fmi3_co_simulation().co_simulation_instance();
    ensure!(
        interface.call_get_float64(&mut fmu.store, instance, &[refs.samples, 0, refs.samples])?
            == Ok(vec![5.0, 2.125, 5.0])
    );
    ensure!(
        interface.call_set_float64(&mut fmu.store, instance, &[refs.samples], &[-0.0])?
            == Status::Ok
    );
    let sample = interface
        .call_get_float64(&mut fmu.store, instance, &[refs.samples])?
        .map_err(|status| anyhow::anyhow!("signed-zero getter: {status:?}"))?;
    ensure!(sample.len() == 1 && sample[0].to_bits() == (-0.0_f64).to_bits());
    ensure!(
        interface.call_set_int64(&mut fmu.store, instance, &[refs.k], &[i64::MIN])? == Status::Ok
    );
    check_result(fmu, instance, refs.result, 1.0)?;
    let interface = fmu.world.fmi_fmi3_co_simulation().co_simulation_instance();
    ensure!(
        interface.call_set_float64(&mut fmu.store, instance, &[refs.samples], &[5.0])?
            == Status::Ok
    );
    let status = interface.call_set_int64(&mut fmu.store, instance, &[refs.k], &[2])?;
    ensure!(
        status == Status::Ok,
        "active first=true: k=2 keeps indexed arm inactive: status={status:?}; actual callbacks={:?}",
        fmu.store.data().messages
    );
    check_result(fmu, instance, refs.result, 1.0)?;
    let interface = fmu.world.fmi_fmi3_co_simulation().co_simulation_instance();
    ensure!(interface.call_set_int64(&mut fmu.store, instance, &[refs.k], &[1])? == Status::Ok);
    ensure!(
        interface.call_set_boolean(&mut fmu.store, instance, &[refs.first], &[false])?
            == Status::Ok
    );
    check_result(fmu, instance, refs.result, 2.0)?;
    advance(fmu, instance, 2.125, 0.125)?;
    let interface = fmu.world.fmi_fmi3_co_simulation().co_simulation_instance();
    ensure!(
        interface.call_set_float64(&mut fmu.store, instance, &[refs.samples], &[-5.0])?
            == Status::Ok
    );
    check_result(fmu, instance, refs.result, 3.0)?;
    let interface = fmu.world.fmi_fmi3_co_simulation().co_simulation_instance();
    ensure!(
        interface.call_set_float64(&mut fmu.store, instance, &[refs.samples], &[5.0])?
            == Status::Ok
    );
    check_result(fmu, instance, refs.result, 2.0)?;
    Ok(())
}

fn check_source_fault(fmu: &mut Fmu, instance: ResourceAny, refs: &References) -> Result<()> {
    let interface = fmu.world.fmi_fmi3_co_simulation().co_simulation_instance();
    ensure!(
        interface.call_set_debug_logging(&mut fmu.store, instance, true, &["error".into()])?
            == Status::Ok
    );
    // The healthy reset/isolation path leaves this resource at first=false, k=1.
    ensure!(
        interface.call_set_float64(&mut fmu.store, instance, &[refs.samples], &[5.0])?
            == Status::Ok
    );
    check_result(fmu, instance, refs.result, 2.0)?;
    let messages_before = fmu.store.data().messages.len();
    let interface = fmu.world.fmi_fmi3_co_simulation().co_simulation_instance();
    let status = interface.call_set_int64(&mut fmu.store, instance, &[refs.k], &[2])?;
    ensure!(
        status == Status::Error,
        "reached source fault setter status={status:?}"
    );
    ensure!(
        fmu.store.data().messages[messages_before..]
            .iter()
            .any(|message| message.instance_name == "rumoca-test"
                && message.status == Status::Error
                && message.category == "error"
                && message.message.contains("IndexBounds")
                && message.message.contains("operation:")
                && message.message.contains("provenance:")),
        "original reached source IndexBounds callback missing: {:?}",
        &fmu.store.data().messages[messages_before..]
    );
    // This source fault occurs during postcommit session correlation. The WIT
    // Error outcome ends use of this resource; the caller drops it immediately.
    Ok(())
}

fn check_reset_and_isolation(
    fmu: &mut Fmu,
    first: ResourceAny,
    second: ResourceAny,
    refs: &References,
) -> Result<()> {
    let interface = fmu.world.fmi_fmi3_co_simulation().co_simulation_instance();
    ensure!(interface.call_terminate(&mut fmu.store, first)? == Status::Ok);
    ensure!(
        interface.call_get_float64(&mut fmu.store, first, &[refs.samples])? == Err(Status::Error)
    );
    ensure!(interface.call_reset(&mut fmu.store, first)? == Status::Ok);
    check_defaults(fmu, first, refs)?;
    initialize(fmu, first, 5.0)?;
    advance(fmu, first, 5.0, 0.125)?;
    check_result(fmu, first, refs.result, 1.0)?;
    // Prepared termination uses the actual pending lifecycle, then source Reset.
    let interface = fmu.world.fmi_fmi3_co_simulation().co_simulation_instance();
    ensure!(interface.call_terminate(&mut fmu.store, second)? == Status::Ok);
    ensure!(interface.call_get_int64(&mut fmu.store, second, &[refs.k])? == Err(Status::Error));
    ensure!(interface.call_reset(&mut fmu.store, second)? == Status::Ok);
    check_defaults(fmu, second, refs)?;
    initialize(fmu, second, 5.0)?;
    let interface = fmu.world.fmi_fmi3_co_simulation().co_simulation_instance();
    ensure!(interface.call_set_int64(&mut fmu.store, first, &[refs.k], &[1])? == Status::Ok);
    ensure!(
        interface.call_set_boolean(&mut fmu.store, first, &[refs.first], &[false])? == Status::Ok
    );
    ensure!(
        interface.call_set_float64(&mut fmu.store, first, &[refs.samples], &[-5.0])? == Status::Ok
    );
    check_result(fmu, first, refs.result, 3.0)?;
    check_result(fmu, second, refs.result, 1.0)?;
    ensure!(
        fmu.store
            .data()
            .messages
            .iter()
            .any(|message| message.instance_name == "native-second"
                && message.status == Status::Error
                && message.category == "error"
                && !message.message.is_empty()),
        "actual second-resource error callback missing"
    );
    let interface = fmu.world.fmi_fmi3_co_simulation().co_simulation_instance();
    ensure!(interface.call_terminate(&mut fmu.store, second)? == Status::Ok);
    Ok(())
}
