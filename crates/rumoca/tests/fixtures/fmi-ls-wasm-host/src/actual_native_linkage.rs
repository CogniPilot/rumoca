//! Evidence host for actual emitted-model linkage, without FMI admission.

use anyhow::{Result, ensure};
use wasmtime::component::{Component, Linker};
use wasmtime::{Config, Engine, Store};

pub(super) fn run(path: &str, original_statuses: &str) -> Result<()> {
    let original_statuses = original_statuses
        .split(',')
        .map(str::parse::<u32>)
        .collect::<std::result::Result<Vec<_>, _>>()?;
    ensure!(!original_statuses.is_empty() && original_statuses.iter().all(|code| *code > 2));
    let mut config = Config::new();
    config.wasm_component_model(true);
    let engine = Engine::new(&config)?;
    let component = Component::from_file(&engine, path)?;
    for _ in 0..2 {
        check_instance(&engine, &component, &original_statuses)?;
    }
    println!(
        "ACTUAL_NATIVE_LINKAGE_OK instances=2 regions=2 calls=34; actual emitter linkage only"
    );
    Ok(())
}

fn check_instance(engine: &Engine, component: &Component, original_statuses: &[u32]) -> Result<()> {
    let mut store = Store::new(engine, ());
    let instance = Linker::<()>::new(engine).instantiate(&mut store, component)?;
    let invoke = instance.get_typed_func::<(u32, bool, f64, i64), (f64,)>(&mut store, "invoke")?;
    let status = instance.get_typed_func::<(u32,), (u32,)>(&mut store, "last-status")?;
    let sample_cell = instance.get_typed_func::<(u32,), (f64,)>(&mut store, "input-sample")?;
    let integer_cell = instance.get_typed_func::<(u32,), (i64,)>(&mut store, "input-integer")?;
    let boolean_cell = instance.get_typed_func::<(u32,), (bool,)>(&mut store, "input-boolean")?;
    let accepted = instance.get_typed_func::<(u32,), (f64,)>(&mut store, "accepted-result")?;
    for _ in 0..3 {
        for (region, first, sample, k, expected) in [
            (0, true, 5.0, i64::MAX, 1.0_f64),
            (1, false, 5.0, 1, 2.0),
            (0, false, -5.0, 1, 3.0),
            (1, true, -5.0, i64::MIN, 1.0),
            (0, false, -0.0, 1, 3.0),
        ] {
            let (actual,) = invoke.call(&mut store, (region, first, sample, k))?;
            invoke.post_return(&mut store)?;
            let (code,) = status.call(&mut store, (region,))?;
            status.post_return(&mut store)?;
            let (sample_bits,) = sample_cell.call(&mut store, (region,))?;
            sample_cell.post_return(&mut store)?;
            let (integer,) = integer_cell.call(&mut store, (region,))?;
            integer_cell.post_return(&mut store)?;
            let (boolean,) = boolean_cell.call(&mut store, (region,))?;
            boolean_cell.post_return(&mut store)?;
            ensure!(
                sample_bits.to_bits() == sample.to_bits() && integer == k && boolean == first,
                "actual input cells lost exact bits in region {region}"
            );
            ensure!(
                code == 0 && actual.to_bits() == expected.to_bits(),
                "region {region} status={code} value={actual:?} expected={expected:?}"
            );
        }
    }
    // The active invalid gather preserves the previous complete Y publication.
    let (actual,) = invoke.call(&mut store, (0, false, 5.0, 2))?;
    invoke.post_return(&mut store)?;
    let (code,) = status.call(&mut store, (0,))?;
    status.post_return(&mut store)?;
    ensure!(
        original_statuses.contains(&code) && actual.to_bits() == 3.0_f64.to_bits(),
        "source gather failure changed accepted output: status={code}, value={actual:?}"
    );
    println!("ACTUAL_NATIVE_SOURCE_FAULT region=0 status={code} retained=3");
    let (other,) = status.call(&mut store, (1,))?;
    status.post_return(&mut store)?;
    ensure!(other == 0, "region 0 fault contaminated region 1 status");
    let (other,) = accepted.call(&mut store, (1,))?;
    accepted.post_return(&mut store)?;
    ensure!(
        other.to_bits() == 1.0_f64.to_bits(),
        "region 0 fault contaminated region 1 Y"
    );
    let (actual,) = invoke.call(&mut store, (0, true, 5.0, i64::MAX))?;
    invoke.post_return(&mut store)?;
    let (code,) = status.call(&mut store, (0,))?;
    status.post_return(&mut store)?;
    ensure!(code == 0 && actual.to_bits() == 1.0_f64.to_bits());
    Ok(())
}
