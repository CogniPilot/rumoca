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
    fn log_message(&mut self, _: String, _: Status, _: String, _: String) {}
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

fn get_value(
    world: &CoSimulationFmu,
    store: &mut Store<HostState>,
    instance: ResourceAny,
    value_reference: u32,
) -> Result<Option<f64>> {
    Ok(world
        .fmi_fmi3_co_simulation()
        .co_simulation_instance()
        .call_get_float64(store, instance, &[value_reference])?
        .ok()
        .and_then(|values| values.first().copied()))
}

fn set_value(
    world: &CoSimulationFmu,
    store: &mut Store<HostState>,
    instance: ResourceAny,
    value_reference: u32,
    value: f64,
) -> Result<Status> {
    world
        .fmi_fmi3_co_simulation()
        .co_simulation_instance()
        .call_set_float64(store, instance, &[value_reference], &[value])
        .map_err(Into::into)
}

fn discover_value_references(
    world: &CoSimulationFmu,
    store: &mut Store<HostState>,
    instance: ResourceAny,
) -> Result<(u32, u32)> {
    let mut readable = Vec::new();
    let mut writable = Vec::new();
    for value_reference in 1..32 {
        let Some(value) = get_value(world, store, instance, value_reference)? else {
            continue;
        };
        readable.push(value_reference);
        if set_value(world, store, instance, value_reference, value)? == Status::Ok {
            writable.push(value_reference);
        }
    }
    ensure!(readable.len() == 2, "expected one input and one state: {readable:?}");
    ensure!(writable.len() == 1, "expected one writable input: {writable:?}");
    let input = writable[0];
    let state = readable
        .into_iter()
        .find(|value_reference| *value_reference != input)
        .context("state value reference")?;
    Ok((input, state))
}

fn main() -> Result<()> {
    let mut args = std::env::args().skip(1);
    let component_path = args.next().context("component path argument")?;
    let token = args.next().context("instantiation token argument")?;
    ensure!(args.next().is_none(), "unexpected host argument");

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
        },
    );
    let world = CoSimulationFmu::instantiate(&mut store, &component, &linker)?;
    ensure!(world.fmi_fmi3_common().call_get_version(&mut store)? == "3.0");

    let interface = world
        .fmi_fmi3_co_simulation()
        .co_simulation_instance();
    let instance = interface
        .call_instantiate_co_simulation(
            &mut store,
            "rumoca-test",
            &token,
            "",
            false,
            false,
            false,
            false,
            &[],
        )?
        .context("component rejected the checked token")?;
    ensure!(
        interface.call_enter_initialization_mode(&mut store, instance, None, 0.0, Some(1.0))?
            == Status::Ok
    );

    ensure!(
        interface.call_set_input_derivatives(&mut store, instance, &[], &[])? == Status::Error,
        "unsupported input derivatives reported success"
    );
    ensure!(
        interface.call_enter_step_mode(&mut store, instance)? == Status::Error,
        "event-mode transition reported success"
    );
    ensure!(interface.call_exit_initialization_mode(&mut store, instance)? == Status::Ok);

    let (input, state) = discover_value_references(&world, &mut store, instance)?;
    let initial_input = get_value(&world, &mut store, instance, input)?.context("input value")?;
    let status = interface.call_set_float64(
        &mut store,
        instance,
        &[input, u32::MAX],
        &[9.0, 0.0],
    )?;
    ensure!(status == Status::Error, "invalid setter must reject");
    ensure!(
        get_value(&world, &mut store, instance, input)? == Some(initial_input),
        "rejected setter partially mutated the input"
    );
    ensure!(set_value(&world, &mut store, instance, input, 0.0)? == Status::Ok);

    let step = interface
        .call_do_step(&mut store, instance, 0.0, 0.1, false)?
        .map_err(|status| anyhow::anyhow!("valid do-step failed: {status:?}"))?;
    ensure!((step.last_successful_time - 0.1).abs() < f64::EPSILON);
    let x = get_value(&world, &mut store, instance, state)?.context("state value")?;
    ensure!((x - (-0.1_f64).exp()).abs() < 1.0e-5, "RK4 state mismatch: {x}");

    let before = x;
    if interface.call_do_step(&mut store, instance, 0.0, 0.1, false)?.is_ok() {
        bail!("mismatched communication point must reject");
    }
    ensure!(get_value(&world, &mut store, instance, state)? == Some(before));
    ensure!(interface.call_terminate(&mut store, instance)? == Status::Ok);
    Ok(())
}
