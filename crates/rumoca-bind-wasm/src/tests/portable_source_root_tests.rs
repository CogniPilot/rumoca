//! Native documentation caches must be consumable by the actual binding API.

use super::*;

#[cfg(not(target_arch = "wasm32"))]
#[test]
fn native_portable_cache_loads_in_browser_binding_and_retains_compilable_sources() {
    use rumoca_compile::source_roots::{PortableSourceRoot, write_portable_source_root_cache};
    use std::io::Read;

    let _guard = session_test_guard();
    clear_source_root_cache().unwrap();
    let root = unique_test_cache_root();
    std::fs::create_dir_all(&root).unwrap();
    std::fs::write(root.join("package.mo"), MINI_MODELICA_LIBRARY).unwrap();
    let output = root.join("cache.bin.gz");
    let report = write_portable_source_root_cache(
        &output,
        &[PortableSourceRoot::new("Modelica", &root).unwrap()],
    )
    .unwrap();
    assert_eq!(report.definition_count, 1);
    assert!(report.issues.is_empty());
    let mut bytes = Vec::new();
    flate2::read::GzDecoder::new(std::fs::File::open(&output).unwrap())
        .read_to_end(&mut bytes)
        .unwrap();
    assert_eq!(merge_parsed_source_roots_binary(&bytes).unwrap(), 1);
    let result = compile(USES_MODELICA_SOURCE, "UsesModelica").unwrap();
    let result: serde_json::Value = serde_json::from_str(&result).unwrap();
    assert_eq!(result["balance"]["is_balanced"], true);
    clear_source_root_cache().unwrap();
    std::fs::remove_dir_all(root).unwrap();
}

#[test]
fn malformed_portable_cache_does_not_publish_a_partial_source_root() {
    use rumoca_compile::source_roots::encode_source_root_snapshot;

    let _guard = session_test_guard();
    clear_source_root_cache().unwrap();
    load_source_roots(&mini_modelica_source_root_json()).unwrap();
    let before = get_source_root_document_count().unwrap();
    let bytes = encode_source_root_snapshot(&[
        ("Fresh.mo".into(), "model Fresh end Fresh;".into()),
        ("Bad.mo".into(), "model".into()),
    ])
    .unwrap();
    assert!(merge_parsed_source_roots_binary(&bytes).is_err());
    assert_eq!(get_source_root_document_count().unwrap(), before);
    clear_source_root_cache().unwrap();
}
