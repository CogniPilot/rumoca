use super::*;

const REPLY: &str = include_str!("recovered-optimization-reply.txt");
const RECOVERED: &str = include_str!("recovered-optimization-diagnostics.txt");

fn fresh_outcome(error: &str) -> (tempfile::TempDir, OmcSimOutcome) {
    fresh_outcome_with_messages(error, "")
}

fn fresh_outcome_with_messages(error: &str, extra: &str) -> (tempfile::TempDir, OmcSimOutcome) {
    let directory = tempfile::tempdir().expect("owned result directory");
    let path = directory.path().join("result.csv");
    let prior = FreshResultFile::snapshot(&path);
    std::fs::write(&path, "time,y\n0,1\n").expect("same-request result");
    let reply = REPLY
        .replace("result.csv", &path.display().to_string())
        .replace(
            "The simulation finished successfully.",
            &format!("The simulation finished successfully.\n{extra}"),
        );
    let outcome = FreshResultFile::from_request(&path, prior, &reply, error.into());
    (directory, outcome)
}

#[test]
fn explicit_recovered_optimizer_retains_complete_diagnostics() {
    let (_directory, outcome) = fresh_outcome(RECOVERED);
    let result = build_session_model_result(&outcome, 1.0);
    assert_eq!(result.status, "success");
    assert_eq!(result.error, None);
    let diagnostics = result.omc_diagnostics.expect("complete diagnostics");
    assert!(diagnostics.starts_with(RECOVERED));
    assert!(diagnostics.contains("transformation process continued"));
    assert!(
        has_fatal_omc_error(RECOVERED),
        "global error policy stays strict"
    );
}

#[test]
fn ordinary_success_requires_the_same_request_result() {
    let (_directory, outcome) = fresh_outcome("");
    assert_eq!(build_session_model_result(&outcome, 1.0).status, "success");
}

#[test]
fn recovery_does_not_cover_unrelated_or_unrecovered_errors() {
    for diagnostic in [
        "Error: backend failed",
        "Error: makeEquationToResidualExp failed",
        "Error: Internal error function analyseStrongComponentScalar failed",
        &format!("Error: loadFile failed\n{RECOVERED}"),
        &format!("{RECOVERED}\nError: later runtime failed"),
        &RECOVERED.replace("sort components failed", "different backend failed"),
    ] {
        let (_directory, outcome) = fresh_outcome(diagnostic);
        assert_eq!(build_session_model_result(&outcome, 1.0).status, "error");
    }
}

#[test]
fn recovery_does_not_cover_runtime_failure_or_assertion() {
    for message in [
        "Simulation execution failed",
        "LOG_ASSERT | error | assertion failed at time 0.1",
        "LOG_ASSERT | debug | simulation terminated by an assertion at initialization",
        "LOG_STDOUT | error | runtime failed",
        "LOG_ASSERT | debug | Simulation terminated due to too many, i.e. 20, event iterations.",
    ] {
        let (_directory, outcome) = fresh_outcome_with_messages(RECOVERED, message);
        assert!(outcome.fresh_result.is_some());
        assert_eq!(build_session_model_result(&outcome, 1.0).status, "error");
    }
}

#[test]
fn missing_result_or_success_message_cannot_issue_freshness() {
    for missing in [None, Some(""), Some("not-the-issued-result.csv")] {
        let (_directory, mut outcome) = fresh_outcome(RECOVERED);
        outcome.result_file = missing.map(str::to_string);
        assert_eq!(build_session_model_result(&outcome, 1.0).status, "error");
    }
    for message in [
        "The initialization finished successfully",
        "The simulation finished successfully",
    ] {
        let (directory, mut outcome) = fresh_outcome(RECOVERED);
        outcome = FreshResultFile::from_request(
            &directory.path().join("result.csv"),
            Ok(None),
            &outcome.raw_reply.replace(message, "unfinished"),
            RECOVERED.into(),
        );
        assert!(outcome.fresh_result.is_none());
        assert_eq!(build_session_model_result(&outcome, 1.0).status, "error");
    }
}

#[test]
fn unchanged_empty_wrong_path_and_unreadable_prior_are_not_fresh() {
    let (directory, mut outcome) = fresh_outcome(RECOVERED);
    let path = directory.path().join("result.csv");
    assert!(FreshResultFile::issue(&path, FreshResultFile::snapshot(&path), &outcome).is_none());
    assert!(
        FreshResultFile::issue(&path, Err(io::Error::other("prior unreadable")), &outcome)
            .is_none()
    );
    std::fs::write(&path, "").expect("empty result");
    assert!(FreshResultFile::issue(&path, Ok(None), &outcome).is_none());
    std::fs::write(&path, "time,y\n0,1\n").expect("restore bytes");
    outcome.result_file = Some(directory.path().join("another.csv").display().to_string());
    std::fs::write(
        outcome.result_file.as_ref().expect("other path"),
        "time,y\n0,1\n",
    )
    .expect("unrelated result");
    assert!(FreshResultFile::issue(&path, Ok(None), &outcome).is_none());
}

#[test]
fn incomplete_record_and_missing_actual_file_cannot_issue_freshness() {
    let (directory, mut outcome) = fresh_outcome(RECOVERED);
    let path = directory.path().join("result.csv");
    outcome.raw_reply = "not a SimulationResult".into();
    assert!(FreshResultFile::issue(&path, Ok(None), &outcome).is_none());
    outcome.raw_reply = REPLY.into();
    std::fs::remove_file(&path).expect("remove result");
    assert!(FreshResultFile::issue(&path, Ok(None), &outcome).is_none());
    assert_eq!(build_session_model_result(&outcome, 1.0).status, "error");
}

#[test]
fn global_load_error_policy_and_previous_reference_key_remain_strict() {
    use crate::msl_tools::omc_simulation_reference::{
        cached_reference_cache_key, omc_reference_cache_key,
    };
    assert!(has_fatal_omc_error("Error: loadFile failed"));
    assert!(has_fatal_omc_error(RECOVERED));
    let directory = tempfile::tempdir().expect("cache directory");
    let mut legacy = blake3::Hasher::new();
    for bytes in [
        b"omc-pinned-tool-services/required-result-file/v2\0".as_slice(),
        crate::msl_tools::common::OMC_SERVICES_DIR.as_bytes(),
        b"\0",
        crate::msl_tools::common::MSL_VERSION.as_bytes(),
        b"\0test-omc\0",
    ] {
        legacy.update(bytes);
    }
    let cached = directory.path().join("reference.json");
    std::fs::write(
        &cached,
        serde_json::json!({"cache_key":legacy.finalize().to_hex().to_string()}).to_string(),
    )
    .expect("legacy cache");
    assert_ne!(
        cached_reference_cache_key(&cached),
        Some(omc_reference_cache_key("test-omc", directory.path()))
    );
}

#[test]
fn issued_freshness_cannot_be_transferred_to_another_reply_or_parsed_field() {
    let (directory, mut outcome) = fresh_outcome(RECOVERED);
    assert!(outcome.fresh_result.is_some());
    outcome.result_file = Some(
        directory
            .path()
            .join(".")
            .join("result.csv")
            .display()
            .to_string(),
    );
    assert_eq!(build_session_model_result(&outcome, 1.0).status, "error");
    let (_directory, mut outcome) = fresh_outcome(RECOVERED);
    outcome.raw_reply.push(' ');
    assert_eq!(build_session_model_result(&outcome, 1.0).status, "error");
}
