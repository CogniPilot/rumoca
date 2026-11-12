use super::*;
use std::sync::atomic::{AtomicU64, Ordering};

fn with_trace_file<T>(payload: &[u8], read: impl FnOnce(&Path) -> T) -> T {
    static NEXT: AtomicU64 = AtomicU64::new(0);
    let path = std::env::temp_dir().join(format!(
        "rumoca-trace-loading-{}-{}.json",
        std::process::id(),
        NEXT.fetch_add(1, Ordering::Relaxed)
    ));
    std::fs::write(&path, payload).expect("write trace fixture");
    let result = read(&path);
    std::fs::remove_file(path).expect("remove trace fixture");
    result
}

#[test]
fn file_loading_preserves_finite_bits_nulls_and_adjacent_event_times() {
    let values = [-0.0, f64::from_bits(1), -f64::MIN_POSITIVE, 0.1, f64::MAX];
    let mut original = trace(
        "buffered_transport",
        vec![0.0, 0.1_f64.next_down(), 0.1, 0.1_f64.next_up(), 1.0],
        vec!["x"],
        vec![values.to_vec()],
    );
    original.data[0][3] = None;
    // Cross multiple reader buffers, including within the channel arrays.
    original.names = (0..512).map(|i| format!("x[{i}]")).collect();
    original.data = vec![original.data[0].clone(); original.names.len()];
    let encoded = serde_json::to_vec(&original).expect("encode finite values");
    assert!(encoded.len() > 8192);
    let decoded = with_trace_file(&encoded, load_trace_json).expect("load trace");
    let bits = |values: &[Option<f64>]| {
        values
            .iter()
            .map(|value| value.map(f64::to_bits))
            .collect::<Vec<_>>()
    };
    assert_eq!(decoded.names, original.names);
    assert_eq!(decoded.model_name, original.model_name);
    assert_eq!(
        decoded
            .times
            .iter()
            .map(|t| t.to_bits())
            .collect::<Vec<_>>(),
        original
            .times
            .iter()
            .map(|t| t.to_bits())
            .collect::<Vec<_>>()
    );
    for (actual, expected) in decoded.data.iter().zip(&original.data) {
        assert_eq!(bits(actual), bits(expected));
    }
    assert_eq!(decoded.data.len(), original.data.len());
}

#[test]
fn file_loading_rejects_truncated_trailing_and_out_of_range_data() {
    for payload in [
        r#"{"times":[0],"names":["x"],"data":[[1"#,
        r#"{"times":[0],"names":["x"],"data":[[1]]} trailing"#,
        r#"{"times":[0],"names":["x"],"data":[[1e400]]}"#,
    ] {
        let error = with_trace_file(payload.as_bytes(), load_trace_json)
            .expect_err("malformed trace cannot be loaded");
        assert!(matches!(error, TraceCompareError::Parse { .. }), "{error}");
    }
}

#[test]
fn file_loading_preserves_missing_file_read_error() {
    with_trace_file(b"", |path| {
        let absent = path.with_extension("absent");
        let error = load_trace_json(&absent).expect_err("absent trace is a read failure");
        assert!(matches!(error, TraceCompareError::Read { .. }), "{error}");
    });
}
