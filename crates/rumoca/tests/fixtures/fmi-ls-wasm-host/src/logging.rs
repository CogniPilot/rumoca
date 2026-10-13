use super::*;

pub fn run(component_path: &str, token: &str, args: &[String]) -> Result<()> {
    ensure!(
        args.len() == 1,
        "logging mode needs the Boolean input reference"
    );
    let valid: u32 = args[0].parse()?;
    let mut fmu = load(component_path, token)?;
    let interface = fmu.world.fmi_fmi3_co_simulation().co_simulation_instance();
    ensure!(
        interface
            .call_instantiate_co_simulation(
                &mut fmu.store,
                "rejected-instance",
                "wrong-token",
                "",
                false,
                false,
                false,
                false,
                &[],
            )?
            .is_none()
    );
    let second = interface
        .call_instantiate_co_simulation(
            &mut fmu.store,
            "second-instance",
            token,
            "",
            false,
            false,
            false,
            false,
            &[],
        )?
        .context("second instance rejected")?;
    let first = fmu.instance;
    fail_assertion(&mut fmu, first, valid, "rumoca-test")?;
    first.resource_drop(&mut fmu.store)?;
    fail_assertion(&mut fmu, second, valid, "second-instance")?;
    ensure!(
        fmu.world
            .fmi_fmi3_co_simulation()
            .co_simulation_instance()
            .call_reset(&mut fmu.store, second)?
            == Status::Ok
    );
    fail_assertion(&mut fmu, second, valid, "second-instance")?;
    second.resource_drop(&mut fmu.store)?;
    ensure!(fmu.store.data().messages.is_empty());
    println!("OK authored diagnostic, failed instantiate, instance names, free, reset");
    Ok(())
}

fn fail_assertion(fmu: &mut Fmu, instance: ResourceAny, valid: u32, name: &str) -> Result<()> {
    let interface = fmu.world.fmi_fmi3_co_simulation().co_simulation_instance();
    ensure!(
        interface.call_enter_initialization_mode(&mut fmu.store, instance, None, 0.0, Some(1.0),)?
            == Status::Ok
    );
    ensure!(
        interface.call_set_boolean(&mut fmu.store, instance, &[valid], &[false])? == Status::Ok
    );
    ensure!(interface.call_exit_initialization_mode(&mut fmu.store, instance)? == Status::Error);
    ensure!(
        std::mem::take(&mut fmu.store.data_mut().messages)
            == [LogMessage {
                instance_name: name.into(),
                status: Status::Error,
                category: "assertion".into(),
                message: "authored callback diagnostic".into(),
            }],
        "the WIT callback must preserve the exact authored diagnostic and instance"
    );
    Ok(())
}
