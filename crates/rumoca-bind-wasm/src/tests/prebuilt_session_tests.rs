use super::*;
use sha2::{Digest, Sha256};

const SOURCE: &str = r#"
model Prebuilt
  parameter Real gain=2;
  input Real u=0;
  Real x(start=1, fixed=true);
  output Real y;
equation
  der(x)=Dep.scale(u, gain);
  y=x;
annotation(experiment(StartTime=0.25, StopTime=1, Interval=0.01, Tolerance=1e-8));
end Prebuilt;
"#;
const DEP: &str = "package Dep function scale input Real u; input Real k; output Real y; algorithm y:=u*k; end scale; end Dep;";

fn sources() -> String {
    serde_json::json!({"Dep/package.mo":DEP}).to_string()
}

fn artifact() -> String {
    export_prebuilt_artifact(SOURCE, "Prebuilt", &sources()).unwrap()
}

fn load(
    artifact: &str,
    source: &str,
    model: &str,
    sources: &str,
    options: &str,
) -> Result<WasmSimulationSession, WasmError> {
    WasmSimulationSession::from_prebuilt_artifact(artifact, source, model, sources, options)
}

#[test]
fn prebuilt_key_covers_complete_sources_model_compiler_and_canonical_paths() {
    let original = prebuilt_artifact_key(SOURCE, "Prebuilt", &sources()).unwrap();
    let key: Value = serde_json::from_str(&original).unwrap();
    assert_eq!(key["version"], 1);
    assert_eq!(key["compiler_version"], env!("CARGO_PKG_VERSION"));
    assert_eq!(
        key["compiler_identity"],
        rumoca_tool_build_info::build_identity().unwrap()
    );
    let a = r#"{"B.mo":"model B end B;","A.mo":"model A end A;"}"#;
    let b = r#"{"A.mo":"model A end A;","B.mo":"model B end B;"}"#;
    assert_eq!(
        prebuilt_artifact_key(SOURCE, "Prebuilt", a).unwrap(),
        prebuilt_artifact_key(SOURCE, "Prebuilt", b).unwrap()
    );
    for bad in [
        r#"{"input.mo":""}"#,
        r#"{"../A.mo":""}"#,
        r#"{"/A.mo":""}"#,
        r#"{"a//A.mo":""}"#,
        r#"{"a\\A.mo":""}"#,
        r#"{"A.mo":"","A.mo":""}"#,
        r#"{"A.mo":42}"#,
        "[]",
    ] {
        assert!(
            prebuilt_artifact_key(SOURCE, "Prebuilt", bad).is_err(),
            "{bad}"
        );
    }
}

#[test]
fn prebuilt_session_roundtrips_inputs_parameters_experiment_and_reset_without_compilation() {
    let _guard = session_test_guard();
    let artifact = artifact();
    for policy in ["interpreter", "auto"] {
        clear_source_root_cache().unwrap();
        let opts = serde_json::json!({"solver":"rk-like","execution_policy":policy,
            "parameters":{"gain":3},"initial_inputs":[["u",-0.0]]})
        .to_string();
        with_singleton_session(|session| {
            session.add_document("Dep/package.mo", DEP).unwrap();
            Ok(())
        })
        .unwrap();
        let mut from_source =
            WasmSimulationSession::with_interactive_configuration(SOURCE, "Prebuilt", &opts)
                .unwrap();
        // A fallback compile could no longer resolve Dep in this empty singleton.
        clear_source_root_cache().unwrap();
        let phases = std::rc::Rc::new(std::cell::Cell::new(0));
        let observed = phases.clone();
        let observer = rumoca_compile::compile::install_compile_phase_observer(move |_, _| {
            observed.set(observed.get() + 1);
        });
        let mut prebuilt = load(&artifact, SOURCE, "Prebuilt", &sources(), &opts).unwrap();
        drop(observer);
        assert_eq!(phases.get(), 0, "artifact load must not compile source");
        assert!(!prebuilt.preparation_reused());
        assert_eq!(prebuilt.time(), 0.25);
        assert_eq!(
            prebuilt.input_names().unwrap(),
            from_source.input_names().unwrap()
        );
        assert_eq!(
            prebuilt.variable_names().unwrap(),
            from_source.variable_names().unwrap()
        );
        assert_eq!(
            prebuilt.execution_receipt_json().unwrap(),
            from_source.execution_receipt_json().unwrap()
        );
        assert_eq!(
            prebuilt.state_json().unwrap(),
            from_source.state_json().unwrap()
        );
        for time in [0.3, 0.4, 0.5] {
            prebuilt.set_input("u", 2.0).unwrap();
            from_source.set_input("u", 2.0).unwrap();
            prebuilt.advance_to(time).unwrap();
            from_source.advance_to(time).unwrap();
            assert_eq!(
                prebuilt.state_json().unwrap(),
                from_source.state_json().unwrap()
            );
        }
        assert!((prebuilt.get("x").unwrap().unwrap() - 2.5).abs() < 1e-8);
        prebuilt.reset().unwrap();
        from_source.reset().unwrap();
        assert_eq!(
            prebuilt.state_json().unwrap(),
            from_source.state_json().unwrap()
        );
        assert_eq!(get_source_root_document_count().unwrap(), 0);
    }
    clear_source_root_cache().unwrap();
}

#[test]
fn prebuilt_loader_refuses_source_dependency_model_build_and_schema_mismatches() {
    let artifact = artifact();
    for (source, model, workspace) in [
        (format!("{SOURCE}\n"), "Prebuilt", sources()),
        (SOURCE.into(), "Other", sources()),
        (SOURCE.into(), "Prebuilt", "{}".into()),
        (SOURCE.into(), "Prebuilt", sources().replace("u*k", "u+k")),
    ] {
        assert!(load(&artifact, &source, model, &workspace, "{}").is_err());
    }
    for field in [
        "compiler_identity",
        "compiler_version",
        "source_sha256",
        "sources_sha256",
    ] {
        let mut changed: Value = serde_json::from_str(&artifact).unwrap();
        changed["key"][field] = "wrong".into();
        let error = load(&changed.to_string(), SOURCE, "Prebuilt", &sources(), "{}")
            .err()
            .unwrap();
        assert!(error.message().contains("key mismatch"), "{error}");
    }
    let mut changed: Value = serde_json::from_str(&artifact).unwrap();
    changed["key"]["version"] = 0.into();
    let error = load(&changed.to_string(), SOURCE, "Prebuilt", &sources(), "{}")
        .err()
        .unwrap();
    assert!(error.message().contains("Unsupported prebuilt"));
}

fn changed_payload(artifact: &str, edit: impl FnOnce(&mut Value)) -> String {
    let mut envelope: Value = serde_json::from_str(artifact).unwrap();
    let mut payload: Value =
        serde_json::from_str(envelope["payload_json"].as_str().unwrap()).unwrap();
    edit(&mut payload);
    let payload = payload.to_string();
    envelope["payload_sha256"] = format!("{:x}", Sha256::digest(payload.as_bytes())).into();
    envelope["payload_json"] = payload.into();
    envelope.to_string()
}

#[test]
fn prebuilt_loader_checks_payload_integrity_checked_dae_replay_and_options() {
    let artifact = artifact();
    let mut corrupt: Value = serde_json::from_str(&artifact).unwrap();
    corrupt["payload_json"] = "{}".into();
    let error = load(&corrupt.to_string(), SOURCE, "Prebuilt", &sources(), "{}")
        .err()
        .unwrap();
    assert!(error.message().contains("payload hash mismatch"));
    let stale = changed_payload(&artifact, |payload| {
        payload["dae"]["schema_version"] = 0.into()
    });
    let error = load(&stale, SOURCE, "Prebuilt", &sources(), "{}")
        .err()
        .unwrap();
    assert!(
        error.message().contains("unsupported DAE schema"),
        "{error}"
    );
    for bad in ["[]", r#"{"dt":-1}"#, r#"{"extra":true}"#] {
        assert!(load(&artifact, SOURCE, "Prebuilt", &sources(), bad).is_err());
    }
    assert!(export_prebuilt_artifact("model Broken", "Broken", "{}").is_err());
    assert!(load("{}", SOURCE, "Prebuilt", &sources(), "{}").is_err());
}
