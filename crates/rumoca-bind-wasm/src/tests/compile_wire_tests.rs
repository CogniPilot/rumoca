//! Compile metadata must not pollute the canonical DAE wire format.

use super::*;

#[test]
fn function_quotient_source_loop_roundtrips_canonical_dae() {
    let source = include_str!("fixtures/quotient_loop.mo");
    let mut session = Session::default();
    let json = compile_source_in_session(&mut session, source, "QuotientLoopControl").unwrap();
    let response: serde_json::Value = serde_json::from_str(&json).unwrap();
    for key in ["dae", "dae_native"] {
        let decoded: rumoca_compile::compile::Dae =
            serde_json::from_value(response[key].clone()).unwrap();
        assert_eq!(serde_json::to_value(decoded).unwrap(), response[key]);
    }
}

#[test]
fn compile_payload_roundtrips_through_canonical_dae_decoder() {
    let mut session = Session::default();
    let source = "model CvMap input Real x[3]={1,2,3}; output Real y[3]; equation for i in 1:3 loop y[i]=2*x[i]; end for; end CvMap;";
    let json = compile_source_in_session(&mut session, source, "CvMap").unwrap();
    let response: serde_json::Value = serde_json::from_str(&json).unwrap();
    assert!(response["__rumoca_build"]["version"].is_string());
    for key in ["dae", "dae_native"] {
        let payload = &response[key];
        assert!(payload.get("__rumoca_build").is_none());
        let decoded: rumoca_compile::compile::Dae =
            serde_json::from_value(payload.clone()).unwrap();
        assert_eq!(serde_json::to_value(decoded).unwrap(), *payload);
    }
    assert_eq!(response["dae"], response["dae_native"]);
}
