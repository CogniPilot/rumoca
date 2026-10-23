//! Authored guide scenarios compile through the binding's actual library loader.

use super::user_guide_sources::{guide_fences, repository_root};
use super::*;
use rumoca_compile::source_roots::{PortableSourceRoot, write_portable_source_root_cache};
use std::{collections::BTreeSet, io::Read};

fn scenario_config(path: &Path) -> serde_json::Value {
    let text = std::fs::read_to_string(repository_root().join(path)).unwrap();
    let sources = serde_json::json!({path.to_str().unwrap(): text}).to_string();
    let response = scenario_get_scenario_config_full(&sources, path.to_str().unwrap()).unwrap();
    let response: serde_json::Value = serde_json::from_str(&response).unwrap();
    assert_eq!(response["ok"], true, "{}: {response}", path.display());
    response["config"].clone()
}

fn load_scenario_libraries(configs: &[(PathBuf, serde_json::Value)]) {
    let mut paths = BTreeSet::new();
    for (scenario, config) in configs {
        for relative in config["source_roots"].as_array().into_iter().flatten() {
            let path = repository_root()
                .join(scenario.parent().unwrap())
                .join(relative.as_str().expect("authored source root"));
            assert!(
                path.is_dir(),
                "pinned guide root {} missing; run cargo xtask repo modelica-deps ensure",
                path.display()
            );
            paths.insert(path.canonicalize().unwrap());
        }
    }
    let roots: Vec<_> = paths
        .into_iter()
        .enumerate()
        .map(|(index, path)| PortableSourceRoot::new(format!("guide-{index}"), path).unwrap())
        .collect();
    assert_eq!(roots.len(), 2, "review changes to the guide library roster");
    let cache_dir = unique_test_cache_root();
    std::fs::create_dir_all(&cache_dir).unwrap();
    let output = cache_dir.join("guide-libraries.bin.gz");
    let report = write_portable_source_root_cache(&output, &roots).unwrap();
    assert!(
        report.issues.is_empty(),
        "guide library parse: {:?}",
        report.issues
    );
    assert!(report.definition_count > 0);
    let mut bytes = Vec::new();
    flate2::read::GzDecoder::new(std::fs::File::open(&output).unwrap())
        .read_to_end(&mut bytes)
        .unwrap();
    assert_eq!(
        merge_parsed_source_roots_binary(&bytes).unwrap(),
        u32::try_from(report.definition_count).unwrap()
    );
    std::fs::remove_dir_all(cache_dir).unwrap();
}

#[test]
fn user_guide_scenarios_compile_the_authored_models_with_pinned_libraries() {
    let _guard = session_test_guard();
    clear_source_root_cache().unwrap();
    let paths: BTreeSet<_> = guide_fences()
        .into_iter()
        .filter_map(|fence| fence.scenario_path())
        .collect();
    assert_eq!(paths.len(), 17, "review new guide scenarios explicitly");
    let configs: Vec<_> = paths
        .into_iter()
        .map(|path| {
            let config = scenario_config(&path);
            (path, config)
        })
        .collect();
    load_scenario_libraries(&configs);
    let mut failures = Vec::new();
    for (path, config) in configs {
        let model = config["model"]["name"].as_str().expect("scenario model");
        let file = config["model"]["file"].as_str().expect("scenario file");
        let source = repository_root().join(path.parent().unwrap()).join(file);
        let source = std::fs::read_to_string(source).unwrap();
        let started = std::time::Instant::now();
        match compile(&source, model) {
            Ok(json) => {
                let result: serde_json::Value = serde_json::from_str(&json).unwrap();
                if result["balance"]["is_balanced"] != true {
                    failures.push(format!("{model}: unbalanced checked compilation"));
                }
                eprintln!(
                    "[guide-scenario] {model} {} {:.6}s",
                    path.display(),
                    started.elapsed().as_secs_f64()
                );
            }
            Err(error) => failures.push(format!("{model} {}: {error}", path.display())),
        }
    }
    clear_source_root_cache().unwrap();
    assert!(
        failures.is_empty(),
        "authored guide scenarios:\n{}",
        failures.join("\n")
    );
}
