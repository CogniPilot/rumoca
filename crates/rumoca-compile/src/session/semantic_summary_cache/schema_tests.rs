use super::*;

#[test]
fn source_root_primary_rejects_pre_complete_resolution_schema() {
    let temp = tempfile::tempdir().expect("tempdir");
    let path = temp.path().join("summary.bin");
    let docs = Vec::new();
    let summary = SourceRootSemanticSummary::from_documents(&docs);
    write_summary_cache(&path, "root", temp.path(), "key", &summary)
        .expect("write current summary");
    let mut payload = read_summary_payload(File::open(&path).expect("open summary"))
        .expect("read current summary");
    assert!(summary_from_payload(payload.clone(), "root", "key", &docs).is_some());
    payload.manifest.schema_version = 10;
    assert!(
        summary_from_payload(payload, "root", "key", &docs).is_none(),
        "old summaries may certify incomplete dependency graphs"
    );
}
