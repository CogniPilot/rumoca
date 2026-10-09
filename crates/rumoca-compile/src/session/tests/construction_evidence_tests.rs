mod fixtures;

use super::*;
use rumoca_core::PhaseError;
use serde_json::{Value, json};

fn flat_fixture(case: &fixtures::Fixture) -> (flat::Model, SourceMap) {
    let name = "construction-evidence-parity.mo";
    let mut session = Session::default();
    assert_eq!(session.update_document(name, case.source), None);
    let flat = session
        .compile_model_flat_strict_reachable_uncached_with_recovery(case.model)
        .expect("fixture reaches the Flat-to-DAE boundary");
    let mut source_map = SourceMap::new();
    source_map.add(name, case.source);
    (flat, source_map)
}

fn selection_values(selections: &[rumoca_phase_dae::StructuralSelection]) -> Vec<Value> {
    selections
        .iter()
        .map(|s| json!({"span": s.span, "kind": s.kind, "parameters": s.parameters}))
        .collect()
}

fn baseline(case: &fixtures::Fixture) -> Value {
    let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("src/session/tests/construction_evidence_tests/baseline")
        .join(format!("{}.json", case.name));
    serde_json::from_slice(&std::fs::read(path).expect("retained independent baseline"))
        .expect("baseline is valid JSON")
}

#[test]
fn construction_result_preserves_legacy_wire_balance_selections_and_refusals() {
    for case in fixtures::FIXTURES {
        let (flat, source_map) = flat_fixture(case);
        let result = rumoca_phase_dae::to_dae_with_evidence(&flat, source_map.clone());
        assert_eq!(result.is_ok(), case.success, "{}", case.name);
        let expected = baseline(case);
        let actual = match result {
            Ok(result) => {
                let (dae, balance, selections) = result.into_parts();
                let (independent_balance, independent_selections) =
                    rumoca_phase_dae::construction_evidence(&flat).unwrap();
                assert_eq!(balance, independent_balance, "{}", case.name);
                assert_eq!(selections, independent_selections, "{}", case.name);
                let projected = rumoca_phase_dae::to_dae(&flat, source_map).unwrap();
                assert_eq!(
                    serde_json::to_value(&dae).unwrap(),
                    serde_json::to_value(&projected).unwrap(),
                    "{} compatibility projection",
                    case.name
                );
                if case.name == "guard" {
                    assert!(!selections.is_empty(), "structural warnings survive");
                }
                check_native_tensor_shapes(case.name, &dae);
                json!({"dae": dae, "balance": balance,
                       "selections": selection_values(&selections)})
            }
            Err(error) => {
                let projected = rumoca_phase_dae::to_dae(&flat, source_map).err().unwrap();
                assert_eq!(
                    serde_json::to_value(error.to_diagnostic()).unwrap(),
                    serde_json::to_value(projected.to_diagnostic()).unwrap()
                );
                json!({"error": error.to_diagnostic()})
            }
        };
        assert_eq!(actual, expected, "{} frozen pre-cutover result", case.name);
    }
}

fn check_native_tensor_shapes(case: &str, dae: &dae::Dae) {
    let names: &[&str] = match case {
        "tensor" => &["u", "y"],
        "record" => &["previous.values", "next.values"],
        _ => return,
    };
    dae.inspect(|view| {
        for name in names {
            let (_, variable) = view
                .variables()
                .find(|(_, v)| v.name().as_str() == *name)
                .unwrap();
            assert_eq!(variable.value_type().dimensions(), &[2, 3], "{name}");
        }
        assert_eq!(
            view.variables()
                .filter(|(_, v)| names.contains(&v.name().as_str()))
                .count(),
            2
        );
    });
}

#[test]
fn compiler_consumes_same_issued_dae_and_balance() {
    for case in fixtures::FIXTURES.iter().filter(|case| case.success) {
        let mut session = Session::default();
        assert_eq!(
            session.update_document("construction-evidence-parity.mo", case.source),
            None
        );
        let compilation = session.compile_model_strict(case.model).unwrap();
        let (result, _) = compilation.into_parts();
        let expected = baseline(case);
        assert_eq!(
            serde_json::to_value(&result.dae).unwrap(),
            expected["dae"],
            "{}",
            case.name
        );
        assert_eq!(
            serde_json::to_value(result.balance_detail).unwrap(),
            expected["balance"],
            "{}",
            case.name
        );
    }
}

#[test]
fn standalone_evidence_retains_unbalanced_analyze_only_domain() {
    let case = fixtures::FIXTURES
        .iter()
        .find(|case| case.name == "unbalanced")
        .unwrap();
    let (flat, source_map) = flat_fixture(case);
    let (balance, _) = rumoca_phase_dae::construction_evidence(&flat).unwrap();
    assert!(!balance.is_balanced());
    assert!(rumoca_phase_dae::to_dae_with_evidence(&flat, source_map).is_err());
}
