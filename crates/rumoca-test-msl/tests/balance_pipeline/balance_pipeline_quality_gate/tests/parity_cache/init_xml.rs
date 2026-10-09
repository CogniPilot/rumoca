use super::*;

fn fixture(root: &Path) -> (PathBuf, PathBuf, SimulationParityCachePolicy) {
    let active = root.join("first/omc_simulation_reference.json");
    let cache = root.join("cache/key.json");
    let active_root = active.parent().unwrap();
    fs::create_dir_all(active_root.join("sim_traces/omc")).unwrap();
    for (model, xml) in [("A", OMC_INIT_XML), ("B", b"<ModelVariables/>".as_slice())] {
        fs::write(
            active_root.join(format!("sim_traces/omc/{model}.json")),
            model,
        )
        .unwrap();
        write_omc_init_xml(active_root, model, xml);
    }
    let policy = SimulationParityCachePolicy {
        batch_timeout_seconds: 144,
        use_experiment_stop_time: true,
        stop_time_override: None,
    };
    let mut payload = valid_simulation_parity_payload();
    payload["msl_version"] = "4.1.0".into();
    payload["omc_version"] = "omc-test-version".into();
    payload["cache_key"] = "producer-msl-services-provenance".into();
    payload["use_experiment_stop_time"] = true.into();
    payload["timing"] = json!({ "batch_timeout_seconds": 144 });
    payload["models"] = json!({
        "A": { "status": "success", "trace_file": "sim_traces/omc/A.json" },
        "B": { "status": "success", "trace_file": "sim_traces/omc/B.json" }
    });
    fs::write(&active, serde_json::to_vec(&payload).unwrap()).unwrap();
    persist_simulation_parity_cache_entry(&active, &cache).unwrap();
    (active, cache, policy)
}

fn matches(cache: &Path, policy: SimulationParityCachePolicy) -> bool {
    simulation_parity_cache_matches(
        cache,
        &["A".into(), "B".into()],
        "4.1.0",
        "omc-test-version",
        policy,
    )
    .unwrap()
}

#[test]
fn keyed_cache_restores_exact_xml_and_provenance_after_source_cleanup() {
    let dir = tempdir().unwrap();
    let (active, cache, policy) = fixture(dir.path());
    let cached: Value = serde_json::from_slice(&fs::read(&cache).unwrap()).unwrap();
    assert_eq!(cached["cache_key"], "producer-msl-services-provenance");
    assert_eq!(
        cached["cache_omc_artifact_blake3"]["omc_sim_work/A_init.xml"],
        blake3::hash(OMC_INIT_XML).to_hex().to_string()
    );
    fs::remove_dir_all(active.parent().unwrap()).unwrap();
    assert!(matches(&cache, policy));
    let next = dir.path().join("second/omc_simulation_reference.json");
    materialize_simulation_parity_cache_entry(&cache, &next).unwrap();
    let root = next.parent().unwrap();
    assert_eq!(
        fs::read(root.join("omc_sim_work/A_init.xml")).unwrap(),
        OMC_INIT_XML
    );
    assert_eq!(
        fs::read(root.join("omc_sim_work/B_init.xml")).unwrap(),
        b"<ModelVariables/>"
    );
    assert!(fs::read(root.join("sim_traces/omc/A.json")).is_ok());
    let restored: Value = serde_json::from_slice(&fs::read(&next).unwrap()).unwrap();
    assert_eq!(restored["cache_key"], cached["cache_key"]);
    assert_eq!(
        restored["cache_omc_artifact_blake3"],
        cached["cache_omc_artifact_blake3"]
    );
}

#[test]
fn missing_stale_swapped_and_legacy_xml_cannot_supply_a_cache_hit() {
    let dir = tempdir().unwrap();
    let (_, cache, policy) = fixture(dir.path());
    let xml = cache
        .with_extension("artifacts")
        .join("omc_sim_work/A_init.xml");
    assert!(matches(&cache, policy));
    fs::write(&xml, b"<ModelVariables/>").unwrap();
    assert!(
        !matches(&cache, policy),
        "another model's XML must fail its digest"
    );
    fs::remove_file(&xml).unwrap();
    assert!(
        !matches(&cache, policy),
        "missing XML must not retain trace-only credit"
    );
    assert!(
        materialize_simulation_parity_cache_entry(&cache, &dir.path().join("next.json")).is_err()
    );
    fs::write(&xml, OMC_INIT_XML).unwrap();
    assert!(matches(&cache, policy));
    let mut old: Value = serde_json::from_slice(&fs::read(&cache).unwrap()).unwrap();
    let mut missing_digest = old.clone();
    missing_digest["cache_omc_artifact_blake3"]
        .as_object_mut()
        .unwrap()
        .remove("omc_sim_work/A_init.xml");
    fs::write(&cache, serde_json::to_vec(&missing_digest).unwrap()).unwrap();
    assert!(
        !matches(&cache, policy),
        "XML without its bound digest must miss"
    );
    old["cache_trace_blake3"] = old["cache_omc_artifact_blake3"].clone();
    old.as_object_mut()
        .unwrap()
        .remove("cache_omc_artifact_blake3");
    fs::write(&cache, serde_json::to_vec(&old).unwrap()).unwrap();
    assert!(
        !matches(&cache, policy),
        "old trace-only cache schema must miss"
    );
}

#[test]
fn xml_persistence_refuses_missing_and_unsafe_inventory_paths() {
    let dir = tempdir().unwrap();
    let (active, cache, _) = fixture(dir.path());
    let previous = fs::read(&cache).unwrap();
    fs::remove_file(active.parent().unwrap().join("omc_sim_work/A_init.xml")).unwrap();
    assert!(persist_simulation_parity_cache_entry(&active, &cache).is_err());
    assert_eq!(fs::read(&cache).unwrap(), previous);
    let mut payload: Value = serde_json::from_slice(&fs::read(&active).unwrap()).unwrap();
    let model = payload["models"]["A"].clone();
    payload["models"] = json!({ "../escape": model });
    fs::write(&active, serde_json::to_vec(&payload).unwrap()).unwrap();
    assert!(persist_simulation_parity_cache_entry(&active, &cache).is_err());
    assert_eq!(fs::read(&cache).unwrap(), previous);
}

#[test]
fn cache_hit_finish_refuses_lost_state_metrics_before_persistence() {
    let dir = tempdir().unwrap();
    let (active, cache, _) = fixture(dir.path());
    finish_simulation_parity_reference(&active, &cache).unwrap();
    let previous = fs::read(&cache).unwrap();
    for state in [Value::Null, json!({ "models_compared": 0 })] {
        let mut payload = valid_simulation_parity_payload();
        payload["trace_comparison"]["state_selection"] = state;
        fs::write(&active, serde_json::to_vec(&payload).unwrap()).unwrap();
        let failure = finish_simulation_parity_reference(&active, &cache).unwrap_err();
        assert!(failure.to_string().contains("state-selection measurements"));
        assert_eq!(fs::read(&cache).unwrap(), previous);
    }
}
