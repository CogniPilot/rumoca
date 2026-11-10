use super::*;

#[test]
fn native_build_refuses_browser_export_and_load_before_runtime_fallback() {
    let dae = rumoca_ir_dae::Dae::construct(Default::default(), |_| Ok(())).unwrap();
    let export = export_precompiled_runtime(&dae, SimOptions::default(), ()).unwrap_err();
    assert!(
        export
            .to_string()
            .contains("requires the wasm32 rk-like build")
    );
    let load = load_precompiled_runtime::<()>(b"untrusted browser payload", |_| {
        panic!("an unavailable browser runtime must not run the configuration or fallback")
    })
    .err()
    .unwrap();
    assert!(
        load.to_string()
            .contains("requires the wasm32 rk-like build")
    );
}
