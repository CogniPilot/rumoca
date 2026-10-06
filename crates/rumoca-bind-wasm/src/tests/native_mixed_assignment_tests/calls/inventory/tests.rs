use super::*;

#[test]
fn native_inventory_is_saved_before_a_later_callback_refusal() {
    let _lock = crate::tests::session_test_guard();
    let path = std::env::temp_dir().join(format!(
        "rumoca-native-inventory-{}.json",
        std::process::id()
    ));
    assert!(
        !path.exists(),
        "the fixture must not overwrite an existing file"
    );
    let source = "model InventoryCapture input Real x[3] = zeros(3); output Real y[3]; equation for i in 1:3 loop y[i] = 2*x[i]+1; end for; end InventoryCapture;";
    let result = crate::native_assignment_api::with_prepared_native_model(
        source,
        "InventoryCapture",
        |model, _, _| {
            let text = save(model, Some(&path))?;
            let parsed: Value = serde_json::from_str(&text).unwrap();
            assert!(parsed.get("problem").is_some());
            assert!(parsed.get("pure_calls").is_some());
            assert_eq!(
                parsed["parameters"].as_array().unwrap().len(),
                model.parameters.len()
            );
            assert_eq!(std::fs::read_to_string(&path).unwrap(), text);
            Err(WasmError::new(
                "fixture refusal after complete inventory capture",
            ))
        },
    );
    assert!(
        result
            .unwrap_err()
            .message()
            .contains("fixture refusal after complete inventory capture")
    );
    let parsed: Value = serde_json::from_str(&std::fs::read_to_string(&path).unwrap()).unwrap();
    assert!(
        parsed["problem"]["continuous"]["implicit_rhs"]["nodes"]
            .as_array()
            .is_some()
    );
    std::fs::remove_file(path).unwrap();
}

#[test]
fn native_inventory_diagnostic_source_identity_does_not_round_through_f64() {
    let span = rumoca_core::Span {
        source: rumoca_core::SourceId(u64::MAX),
        start: rumoca_core::BytePos(13),
        end: rumoca_core::BytePos(29),
    };
    let encoded = source_span(span);
    assert_eq!(encoded["source"], u64::MAX.to_string());
    assert_eq!(encoded["start"], 13);
    assert_eq!(encoded["end"], 29);
    assert_eq!(
        source_span(rumoca_core::Span {
            source: rumoca_core::SourceId(0),
            start: rumoca_core::BytePos(0),
            end: rumoca_core::BytePos(0)
        })["source"],
        "0"
    );
}
