//! Build-time certification of a linked scalar product (owner rule: no
//! per-entry regeneration or certification at load). The certification
//! component links the SAME data library and kernel bodies as the published
//! one, with the build-only certification core; it is instantiated once here,
//! and its `rumoca-certify-linked` export re-issues every entry from the saved
//! product and certifies the whole product against the linked artifact. A trap
//! or refusal is a build refusal, so no uncertified product is published. The
//! certification component itself is never published.
//!
//! Every import traps except std's hash-map seed (`wasi:random/insecure-seed`),
//! answered with a fixed constant: certification reads no clock, entropy,
//! environment, file, stream or FMI callback. Same policy as the
//! `fmi-ls-wasm-host` fixture's build-time pre-initialisation.

/// The fixed hash-map seed answered during certification. Hash-map keys only
/// need to be unpredictable to untrusted input; no ordering may depend on them.
#[cfg(feature = "linked-build-certify")]
const CERTIFY_SEED: (u64, u64) = (0x7275_6d6f_6361_0002, 0x6365_7274_6966_7901);
#[cfg(feature = "linked-build-certify")]
const SEED_INTERFACE: &str = "wasi:random/insecure-seed@";
#[cfg(feature = "linked-build-certify")]
const CERTIFY_EXPORT: &str = "rumoca-certify-linked";

/// Run the certification component's export once; `Err` refuses publication.
#[cfg(feature = "linked-build-certify")]
pub(super) fn certify_linked_component(component: &[u8]) -> Result<(), String> {
    use wasmtime::component::{Component, Linker};
    use wasmtime::{Config, Engine, Store, StoreContextMut};

    let mut config = Config::new();
    config.wasm_component_model(true);
    let engine = Engine::new(&config).map_err(|error| format!("certification engine: {error:#}"))?;
    let component = Component::new(&engine, component)
        .map_err(|error| format!("compile the certification component: {error:#}"))?;
    let mut linker = Linker::<u64>::new(&engine);
    // Every import traps first. Stubbing visits each imported instance by
    // name and refuses one already defined, so the seed instance is defined
    // after it, replacing that instance's trap with the fixed seed.
    linker
        .define_unknown_imports_as_traps(&component)
        .map_err(|error| format!("certification imports: {error:#}"))?;
    linker.allow_shadowing(true);
    let seeds: Vec<String> = component
        .component_type()
        .imports(&engine)
        .map(|(name, _)| name.to_owned())
        .filter(|name| name.starts_with(SEED_INTERFACE))
        .collect();
    for name in seeds {
        let refused = |error: wasmtime::Error| format!("certification seed import {name}: {error:#}");
        linker
            .instance(&name)
            .map_err(refused)?
            .func_wrap(
                "insecure-seed",
                |mut store: StoreContextMut<'_, u64>, (): ()| {
                    *store.data_mut() += 1;
                    Ok((CERTIFY_SEED,))
                },
            )
            .map_err(refused)?;
    }
    linker.allow_shadowing(false);
    let mut store = Store::new(&engine, 0u64);
    let instance = linker
        .instantiate(&mut store, &component)
        .map_err(|error| format!("instantiate the certification component: {error:#}"))?;
    let certify = instance
        .get_typed_func::<(), ()>(&mut store, CERTIFY_EXPORT)
        .map_err(|error| format!("certification export {CERTIFY_EXPORT}: {error:#}"))?;
    certify
        .call(&mut store, ())
        .map_err(|error| format!("linked certification refused: {error:#}"))?;
    certify
        .post_return(&mut store)
        .map_err(|error| format!("linked certification refused: {error:#}"))?;
    Ok(())
}

/// A packager built without the certification runner publishes no linked
/// product: certification is not optional, only its runner is a build feature.
#[cfg(not(feature = "linked-build-certify"))]
pub(super) fn certify_linked_component(_component: &[u8]) -> Result<(), String> {
    Err("this packager was built without build-time linked certification \
         (rumoca-sim feature linked-build-certify); a linked product is never published uncertified"
        .into())
}

#[cfg(all(test, not(feature = "linked-build-certify")))]
mod tests {
    /// Without the runner nothing is certified, so nothing linked publishes.
    #[test]
    fn a_packager_without_the_runner_refuses_every_linked_product() {
        let refusal = super::certify_linked_component(b"\0asm").unwrap_err();
        assert!(refusal.contains("never published uncertified"));
    }
}
