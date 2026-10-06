//! Opt-in consumer admission of the exact full Modelica source-issued table.
use super::*;
mod eigen;
mod fit;

#[test]
fn unchanged_full_registration_source_executes_complete_native_owner() {
    let Ok(path) = std::env::var("RUMOCA_NATIVE_REGISTRATION_SOURCE_TABLE") else {
        eprintln!("full registration source-table probe not requested");
        return;
    };
    let source_path = std::env::var("RUMOCA_NATIVE_REGISTRATION_SOURCE_FIXTURE").unwrap();
    let json = std::fs::read_to_string(path).unwrap();
    let mut wire: serde_json::Value = serde_json::from_str(&json).unwrap();
    assert_eq!(
        wire["source"].as_str().unwrap(),
        std::fs::read_to_string(source_path).unwrap()
    );
    assert_eq!(
        wire["compilerRevision"],
        "03e6b59a47f1adc48488b89e4c462c5fa50f3aaf-dirty"
    );
    let table: solve::SolvePureCallTable = serde_json::from_value(wire["table"].take()).unwrap();
    assert_eq!(table.owners().len(), 3);
    assert!(table.owners().iter().any(|owner| {
        owner
            .inputs()
            .iter()
            .any(|input| input.dimensions() == [14400, 3])
    }));
    for (index, owner) in table.owners().iter().enumerate() {
        let site = owner.call_site();
        assert!(table.matches_site(&site));
        let compiled = compile_pure_call_wasm(&table, &site).unwrap();
        if index < 2 {
            eigen::check(&table, &site, &compiled);
        } else {
            eprintln!(
                "HORN_FULL_MASK_LOOP_COPY_INSTRUCTIONS count={}",
                returned_storage::loop_full_mask_copies(compiled.module_bytes())
            );
            fit::check(&table, &site, &compiled);
            source_tables::export_source_call(
                &compiled,
                &json,
                "horn-full14400.wasm",
                serde_json::json!({"registration":true,"capacity":14400}),
                None,
            );
        }
        eprintln!(
            "FULL_SOURCE_NATIVE_PASS owner={:?} module_bytes={} scratch_bytes={} math_imports={:?}",
            owner.id(),
            compiled.module_bytes().len(),
            compiled.layout().scratch_bytes,
            compiled.math_imports()
        );
    }
}
