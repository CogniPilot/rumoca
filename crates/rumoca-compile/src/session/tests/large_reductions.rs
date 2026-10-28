use super::*;

/// Original #478 shape, through the same strict Flat boundary used by CLI
/// output. Serializing and dropping the resulting tree must also stay safe;
/// merely parsing a large declaration would not exercise the reduction.
#[test]
fn issue_478_large_sum_reaches_flat_json_without_linear_expression_depth() {
    let source = r#"
model S10000
  parameter Integer N = 10000;
  Real u[N](each start = 1.0);
  Real e;
equation
  der(u) = -u;
  e = sum(u);
end S10000;
"#;
    let mut session = Session::default();
    session
        .add_document("S10000.mo", source)
        .expect("original reduction parses");
    let flat = session
        .compile_model_flat_strict_reachable_uncached_with_recovery("S10000")
        .expect("original reduction reaches Flat");
    assert!(!flat.equations.is_empty());
    let json = serde_json::to_vec(&flat).expect("large reduction Flat JSON serializes");
    assert!(!json.is_empty());
    drop(flat);
    // A shallow Flat tree must survive the next public artifact boundary too;
    // do not accept merely moving the recursive failure into DAE construction.
    let result = session
        .compile_model_dae_strict_reachable_uncached_with_recovery("S10000")
        .expect("original reduction reaches checked DAE");
    let dae_json = serde_json::to_vec(&result.dae).expect("large reduction DAE JSON serializes");
    assert!(!dae_json.is_empty());
    drop(result);
}
