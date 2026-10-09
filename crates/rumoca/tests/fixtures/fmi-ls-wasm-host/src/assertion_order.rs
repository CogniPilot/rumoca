use super::*;

pub fn run(component_path: &str, token: &str, args: &[String]) -> Result<()> {
    ensure!(
        args.len() == 4,
        "assertion-order needs Boolean, Int32, Float64 references and the exact first message"
    );
    let references = args[..3]
        .iter()
        .map(|argument| argument.parse::<u32>())
        .collect::<Result<Vec<_>, _>>()?;
    let [valid, index, result] = references[..] else {
        unreachable!()
    };
    let mut fmu = load(component_path, token)?;
    for (condition, subscript, expected) in [
        (false, 1, Status::Error),
        (false, 2, Status::Error),
        (true, 1, Status::Ok),
        (true, 2, Status::Error),
    ] {
        let interface = fmu.world.fmi_fmi3_co_simulation().co_simulation_instance();
        ensure!(interface.call_reset(&mut fmu.store, fmu.instance)? == Status::Ok);
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
            interface.call_set_boolean(&mut fmu.store, fmu.instance, &[valid], &[condition])?
                == Status::Ok
        );
        ensure!(
            interface.call_set_int32(&mut fmu.store, fmu.instance, &[index], &[subscript])?
                == Status::Ok
        );
        ensure!(
            interface.call_exit_initialization_mode(&mut fmu.store, fmu.instance)? == expected,
            "wrong initialization outcome for valid={condition}, index={subscript}"
        );
        let messages = std::mem::take(&mut fmu.store.data_mut().messages);
        let expected_messages = if condition {
            Vec::new()
        } else {
            vec![LogMessage {
                instance_name: "rumoca-test".to_owned(),
                status: Status::Error,
                category: "assertion".to_owned(),
                message: args[3].clone(),
            }]
        };
        ensure!(
            messages == expected_messages,
            "wrong first fault for valid={condition}, index={subscript}: {messages:?}"
        );
        if expected == Status::Ok {
            ensure!(
                interface.call_get_float64(&mut fmu.store, fmu.instance, &[result])?
                    == Ok(vec![1.0])
            );
        } else {
            ensure!(
                interface.call_get_float64(&mut fmu.store, fmu.instance, &[result])?
                    == Err(Status::Error),
                "failed invocation published an ordinary result"
            );
        }
        ensure!(
            fmu.store.data().messages.is_empty(),
            "a public query replayed the failed assertion"
        );
    }
    fmu.instance.resource_drop(&mut fmu.store)?;
    println!("OK first authored fault, bounds control, ordinary result refusal, reset");
    Ok(())
}
