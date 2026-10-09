//! Wasmtime driver for a generated FMI-LS-Wasm Co-Simulation component.
//!
//! Two modes:
//!   lifecycle <component> <token>
//!       Runs generic lifecycle negative controls and prints `OK`.
//!   trace <component> <token> <t_start> <t_stop> <dt> <vr>...
//!       Advances the Co-Simulation from `t_start` to `t_stop` in `dt` steps
//!       and prints a CSV trace (`time` plus one column per value reference).

mod assertion_order;
mod logging;
mod typed_values;

use anyhow::{Context, Result, bail, ensure};
use wasmtime::component::{Component, Linker, ResourceAny};
use wasmtime::{Config, Engine, Store};
use wasmtime_wasi::{WasiCtx, WasiCtxBuilder, WasiCtxView, WasiView};

wasmtime::component::bindgen!({
    world: "co-simulation-fmu",
    path: "wit",
});

use fmi::fmi3::types::Status;

struct HostState {
    wasi: WasiCtx,
    table: wasmtime::component::ResourceTable,
    messages: Vec<LogMessage>,
}

#[derive(Debug, PartialEq)]
struct LogMessage {
    instance_name: String,
    status: Status,
    category: String,
    message: String,
}

impl WasiView for HostState {
    fn ctx(&mut self) -> WasiCtxView<'_> {
        WasiCtxView {
            ctx: &mut self.wasi,
            table: &mut self.table,
        }
    }
}

impl fmi::fmi3::callbacks::Host for HostState {
    fn log_message(
        &mut self,
        instance_name: String,
        status: Status,
        category: String,
        message: String,
    ) {
        self.messages.push(LogMessage {
            instance_name,
            status,
            category,
            message,
        });
    }
    fn clock_update(&mut self) {}
    fn lock_preemption(&mut self) {}
    fn unlock_preemption(&mut self) {}
}

impl fmi::fmi3::intermediate_update_callbacks::Host for HostState {
    fn intermediate_update(&mut self, _: f64, _: bool, _: bool, _: bool, _: bool) -> (bool, f64) {
        (false, 0.0)
    }
}

impl fmi::fmi3::types::Host for HostState {}

struct Fmu {
    world: CoSimulationFmu,
    store: Store<HostState>,
    instance: ResourceAny,
}

fn load(component_path: &str, token: &str) -> Result<Fmu> {
    let mut config = Config::new();
    config.wasm_component_model(true);
    let engine = Engine::new(&config)?;
    let component = Component::from_file(&engine, component_path)?;
    let mut linker = Linker::new(&engine);
    wasmtime_wasi::p2::add_to_linker_sync(&mut linker)?;
    CoSimulationFmu::add_to_linker::<HostState, wasmtime::component::HasSelf<HostState>>(
        &mut linker,
        |state| state,
    )?;
    let mut store = Store::new(
        &engine,
        HostState {
            wasi: WasiCtxBuilder::new().build(),
            table: wasmtime::component::ResourceTable::new(),
            messages: Vec::new(),
        },
    );
    let world = CoSimulationFmu::instantiate(&mut store, &component, &linker)?;
    ensure!(world.fmi_fmi3_common().call_get_version(&mut store)? == "3.0");
    let instance = world
        .fmi_fmi3_co_simulation()
        .co_simulation_instance()
        .call_instantiate_co_simulation(
            &mut store,
            "rumoca-test",
            token,
            "",
            false,
            false,
            false,
            false,
            &[],
        )?
        .context("component rejected the checked token")?;
    Ok(Fmu {
        world,
        store,
        instance,
    })
}

fn get_first(fmu: &mut Fmu, value_reference: u32) -> Result<Option<f64>> {
    Ok(fmu
        .world
        .fmi_fmi3_co_simulation()
        .co_simulation_instance()
        .call_get_float64(&mut fmu.store, fmu.instance, &[value_reference])?
        .ok()
        .and_then(|values| values.first().copied()))
}

fn run_lifecycle(component_path: &str, token: &str) -> Result<()> {
    let mut fmu = load(component_path, token)?;
    let interface = fmu.world.fmi_fmi3_co_simulation().co_simulation_instance();
    ensure!(
        interface.call_instantiate_co_simulation(
            &mut fmu.store, "foreign-token-instance", "foreign-token", "",
            false, false, false, false, &[]
        )?.is_none(),
        "foreign instantiation token acquired an instance"
    );
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
        interface.call_set_input_derivatives(&mut fmu.store, fmu.instance, &[], &[])?
            == Status::Error,
        "unsupported input derivatives reported success"
    );
    ensure!(
        interface.call_enter_step_mode(&mut fmu.store, fmu.instance)? == Status::Error,
        "step-mode transition reported success"
    );
    ensure!(interface.call_exit_initialization_mode(&mut fmu.store, fmu.instance)? == Status::Ok);

    // Find a writable value reference and prove a rejected set is transactional.
    let mut writable = None;
    for value_reference in 1..64u32 {
        if get_first(&mut fmu, value_reference)?.is_none() {
            continue;
        }
        let current = get_first(&mut fmu, value_reference)?.context("readable value")?;
        let status = fmu
            .world
            .fmi_fmi3_co_simulation()
            .co_simulation_instance()
            .call_set_float64(&mut fmu.store, fmu.instance, &[value_reference], &[current])?;
        if status == Status::Ok {
            writable = Some((value_reference, current));
            break;
        }
    }
    let (value_reference, original) = writable.context("no writable value reference found")?;
    let rejected = fmu
        .world
        .fmi_fmi3_co_simulation()
        .co_simulation_instance()
        .call_set_float64(
            &mut fmu.store,
            fmu.instance,
            &[value_reference, u32::MAX],
            &[original + 1.0, 0.0],
        )?;
    ensure!(rejected == Status::Error, "invalid setter must reject");
    ensure!(
        get_first(&mut fmu, value_reference)? == Some(original),
        "rejected setter partially mutated a value"
    );
    ensure!(
        fmu.world
            .fmi_fmi3_co_simulation()
            .co_simulation_instance()
            .call_terminate(&mut fmu.store, fmu.instance)?
            == Status::Ok
    );
    println!("OK");
    Ok(())
}

fn run_trace(component_path: &str, token: &str, args: &[String]) -> Result<()> {
    ensure!(
        args.len() >= 4,
        "trace needs t_start t_stop dt and value references"
    );
    let t_start: f64 = args[0].parse().context("t_start")?;
    let t_stop: f64 = args[1].parse().context("t_stop")?;
    let dt: f64 = args[2].parse().context("dt")?;
    let references: Vec<u32> = args[3..]
        .iter()
        .map(|value| value.parse::<u32>().context("value reference"))
        .collect::<Result<_>>()?;
    ensure!(dt > 0.0 && t_stop > t_start, "invalid trace window");

    let mut fmu = load(component_path, token)?;
    let interface = fmu.world.fmi_fmi3_co_simulation().co_simulation_instance();
    ensure!(
        interface.call_enter_initialization_mode(
            &mut fmu.store,
            fmu.instance,
            None,
            t_start,
            Some(t_stop),
        )? == Status::Ok
    );
    ensure!(interface.call_exit_initialization_mode(&mut fmu.store, fmu.instance)? == Status::Ok);

    let mut header = String::from("time");
    for reference in &references {
        header.push_str(&format!(",{reference}"));
    }
    println!("{header}");

    let steps = ((t_stop - t_start) / dt).round() as i64;
    let mut current = t_start;
    print_row(&mut fmu, current, &references)?;
    for _ in 0..steps {
        let outcome = fmu
            .world
            .fmi_fmi3_co_simulation()
            .co_simulation_instance()
            .call_do_step(&mut fmu.store, fmu.instance, current, dt, false)?;
        match outcome {
            Ok(result) => current = result.last_successful_time,
            Err(status) => bail!("do-step failed at t={current}: {status:?}"),
        }
        print_row(&mut fmu, current, &references)?;
    }
    ensure!(
        fmu.world
            .fmi_fmi3_co_simulation()
            .co_simulation_instance()
            .call_terminate(&mut fmu.store, fmu.instance)?
            == Status::Ok
    );
    Ok(())
}

fn print_row(fmu: &mut Fmu, time: f64, references: &[u32]) -> Result<()> {
    let mut row = format!("{time:.9}");
    for reference in references {
        let value = get_first(fmu, *reference)?
            .with_context(|| format!("value reference {reference} not readable"))?;
        row.push_str(&format!(",{value:.12e}"));
    }
    println!("{row}");
    Ok(())
}

fn main() -> Result<()> {
    let args: Vec<String> = std::env::args().skip(1).collect();
    ensure!(args.len() >= 3, "usage: <mode> <component> <token> [...]");
    let mode = args[0].as_str();
    let component_path = args[1].as_str();
    let token = args[2].as_str();
    match mode {
        "lifecycle" => run_lifecycle(component_path, token),
        "trace" => run_trace(component_path, token, &args[3..]),
        "typed" => typed_values::run(component_path, token, &args[3..]),
        "logging" => logging::run(component_path, token, &args[3..]),
        "assertion-order" => assertion_order::run(component_path, token, &args[3..]),
        other => bail!("unknown mode {other}"),
    }
}
