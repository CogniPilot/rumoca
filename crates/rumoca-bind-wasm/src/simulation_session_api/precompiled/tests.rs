use super::*;

#[test]
fn browser_key_binds_options_and_native_facade_refuses_instead_of_falling_back() {
    let source =
        "model BrowserWire Real x(start=1,fixed=true); equation der(x)=-x; end BrowserWire;";
    let options = r#"{"dt":0.02,"solver":"rk-like","rtol":0.000001,"atol":0.0000000001}"#;
    let key = precompiled_runtime_key(source, "BrowserWire", "{}", options).unwrap();
    let changed = precompiled_runtime_key(
        source,
        "BrowserWire",
        "{}",
        r#"{"dt":0.01,"solver":"rk-like"}"#,
    )
    .unwrap();
    assert_ne!(key, changed);
    let artifact = container::encode(key.as_bytes(), b"opaque browser payload").unwrap();
    let native =
        crate::simulation_session_api::WasmSimulationSession::from_precompiled_runtime_artifact(
            &artifact,
            source,
            "BrowserWire",
            "{}",
            options,
        )
        .err()
        .unwrap();
    assert!(
        native
            .message()
            .contains("requires the wasm32 rk-like build")
    );
    assert!(
        load(&artifact, source, "BrowserWire", "{}", r#"{"dt":0.01}"#)
            .err()
            .unwrap()
            .message()
            .contains("options mismatch")
    );
    assert!(
        export_precompiled_runtime_artifact(source, "BrowserWire", "{}", options)
            .unwrap_err()
            .message()
            .contains("requires the wasm32 rk-like build")
    );
    browser_profile_enable(true);
    let snapshot: serde_json::Value = serde_json::from_str(&browser_profile_snapshot()).unwrap();
    assert_eq!(
        snapshot["instrumented"], false,
        "a native facade cannot claim browser clock measurements"
    );
    browser_profile_enable(false);
}
