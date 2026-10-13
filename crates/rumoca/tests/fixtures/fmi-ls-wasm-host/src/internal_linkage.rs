//! Execute the internal scalar linkage fixture without claiming FMI semantics.

use anyhow::{Result, ensure};
use wasmtime::component::{Component, Linker};
use wasmtime::{Config, Engine, Store};

pub(super) fn run(component_path: &str) -> Result<()> {
    let mut config = Config::new();
    config.wasm_component_model(true);
    let engine = Engine::new(&config)?;
    let component = Component::from_file(&engine, component_path)
        .map_err(|error| error.context("load exact internal linkage component"))?;
    for _ in 0..2 {
        check_instance(&engine, &component)?;
    }
    println!("INTERNAL_LINKAGE_OK instances=2 calls=12; scalar linkage only");
    Ok(())
}

fn check_instance(engine: &Engine, component: &Component) -> Result<()> {
    let mut store = Store::new(engine, ());
    let linker = Linker::<()>::new(engine);
    let instance = linker.instantiate(&mut store, component)?;
    let calculate = instance.get_typed_func::<(f64,), (f64,)>(&mut store, "calculate")?;
    for value in [3.0_f64, -2.0, 0.0, -0.0, 1.0e-300, 3.0] {
        let (actual,) = calculate.call(&mut store, (value,))?;
        calculate.post_return(&mut store)?;
        let expected = value * 2.0;
        ensure!(
            actual.to_bits() == expected.to_bits(),
            "scalar native linkage differs for {value:?}: {actual:?} versus {expected:?}"
        );
    }
    Ok(())
}
