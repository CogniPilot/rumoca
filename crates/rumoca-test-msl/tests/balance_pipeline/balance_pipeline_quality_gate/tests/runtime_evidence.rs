use super::*;

fn runtime_baseline_document() -> Value {
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join(MSL_QUALITY_BASELINE_FILE_REL);
    serde_json::from_slice(&fs::read(path).expect("tracked baseline")).expect("baseline JSON")
}

#[test]
fn runtime_reference_evidence_binds_counts_medians_and_exact_roster() {
    let document = runtime_baseline_document();
    validate_runtime_baseline_evidence(&document).expect("tracked evidence agrees");
    for mutation in 0..6 {
        let mut changed = document.clone();
        match mutation {
            0 => changed["runtime_ratio_stats"]["wall_ratio_both_success"]["median"] = json!(0.1),
            1 => {
                changed["runtime_ratio_stats"]["wall_ratio_both_success"]["sample_count"] = json!(1)
            }
            2 => {
                let ratios = changed["runtime_model_ratios"].as_object_mut().unwrap();
                let first = ratios.keys().next().unwrap().clone();
                ratios.remove(&first);
            }
            3 => {
                let roster = changed["runtime_ratio_cohort_models"]
                    .as_array_mut()
                    .unwrap();
                roster[1] = roster[0].clone();
            }
            4 => {
                let ratios = changed["runtime_model_ratios"].as_object_mut().unwrap();
                let first = ratios.keys().next().unwrap().clone();
                ratios.get_mut(&first).unwrap()["system"] = json!(0.0);
            }
            _ => {
                changed
                    .as_object_mut()
                    .unwrap()
                    .remove("runtime_model_ratios");
            }
        }
        assert!(
            validate_runtime_baseline_evidence(&changed).is_err(),
            "mutation {mutation}"
        );
    }
}

#[test]
fn runtime_margin_keeps_system_floor_and_has_a_distinct_wall_floor() {
    for (label, current, should_fail) in [
        ("system", 65.0, false),
        ("system", 64.999, true),
        ("wall", 60.0, false),
        ("wall", 55.0, false),
        ("wall", 54.999, true),
    ] {
        let mut reasons = Vec::new();
        runtime_cohort::push_runtime_median_reason(&mut reasons, label, current, 100.0);
        assert_eq!(!reasons.is_empty(), should_fail, "{label} {current}");
    }
}

#[test]
fn checked_runtime_read_keeps_typed_duplicate_field_refusal() {
    let document = runtime_baseline_document();
    let mut raw = serde_json::to_string(&document).expect("baseline JSON");
    raw.pop();
    raw.push_str(",\"sim_ok\":0}");
    let dir = tempdir().expect("private fixture");
    let path = dir.path().join("duplicate.json");
    fs::write(&path, raw).expect("write duplicate field control");
    let error = load_msl_quality_baseline(&path).expect_err("typed duplicate field must refuse");
    assert!(error.to_string().contains("duplicate field"), "{error}");
}

#[test]
fn a_finite_huge_runtime_vector_cannot_hide_an_aggregate_contradiction() {
    for value in [1.0e308, f64::from_bits(1)] {
        let mut document = runtime_baseline_document();
        document["runtime_ratio_cohort_models"] = json!(["Fixture.A", "Fixture.B"]);
        document["runtime_model_ratios"] = json!({
            "Fixture.A": {"system": value, "wall": value},
            "Fixture.B": {"system": value, "wall": value},
        });
        for metric in ["system_ratio_both_success", "wall_ratio_both_success"] {
            document["runtime_ratio_stats"][metric]["sample_count"] = json!(2);
            document["runtime_ratio_stats"][metric]["median"] = json!(value);
        }
        validate_runtime_baseline_evidence(&document).expect("matching finite vector");
        if value > 1.0 {
            for metric in ["system_ratio_both_success", "wall_ratio_both_success"] {
                document["runtime_ratio_stats"][metric]["median"] = json!(1.0);
            }
            assert!(validate_runtime_baseline_evidence(&document).is_err());
        }
    }
}
