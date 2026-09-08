use std::fs;
use std::num::NonZeroUsize;
use std::path::Path;

use sha2::{Digest as _, Sha256};

use super::admission::profile_cone;
use super::{
    AggregateDenominatorHistory, CandidateClaim, CandidateConeNode, CandidateOnlyAggregateReport,
    CandidateOnlyRegistryAdmission, CapturePlan, CaptureScenario, ContentBinding, CoverageTest,
    Endpoint, EndpointRequest, GoldenModelRecord, GoldenProfile, GoldenRegistry, ModelClaim,
    OutstandingReceipt, OwnerId, ReviewedAdmissionNotImplemented, ReviewedClaim,
    TerminalDisposition, candidate_only_aggregate_report, check_candidate_capture,
    check_candidate_only_registry, checked_aggregate_denominator_history,
    parse_candidate_only_aggregate_denominator, parse_golden_registry,
};

macro_rules! assert_not_impl {
    ($type:ty, $trait:path) => {
        const _: fn() = || {
            trait AmbiguousIfImplemented<Marker> {
                fn marker() {}
            }
            struct Implemented;
            impl<T: ?Sized> AmbiguousIfImplemented<()> for T {}
            impl<T: ?Sized + $trait> AmbiguousIfImplemented<Implemented> for T {}
            let _ = <$type as AmbiguousIfImplemented<_>>::marker;
        };
    };
}

assert_not_impl!(CandidateOnlyRegistryAdmission, Clone);
assert_not_impl!(CandidateOnlyRegistryAdmission, Copy);
assert_not_impl!(CandidateOnlyRegistryAdmission, Default);
assert_not_impl!(CandidateOnlyRegistryAdmission, serde::Serialize);
assert_not_impl!(CandidateOnlyRegistryAdmission, serde::de::DeserializeOwned);
assert_not_impl!(CandidateOnlyAggregateReport, Clone);
assert_not_impl!(CandidateOnlyAggregateReport, Copy);
assert_not_impl!(CandidateOnlyAggregateReport, Default);
assert_not_impl!(CandidateOnlyAggregateReport, serde::de::DeserializeOwned);
assert_not_impl!(GoldenRegistry, Clone);
assert_not_impl!(GoldenRegistry, Default);
assert_not_impl!(GoldenRegistry, serde::Serialize);
assert_not_impl!(GoldenRegistry, serde::de::DeserializeOwned);
assert_not_impl!(AggregateDenominatorHistory, Clone);
assert_not_impl!(AggregateDenominatorHistory, Copy);
assert_not_impl!(AggregateDenominatorHistory, Default);
assert_not_impl!(AggregateDenominatorHistory, serde::Serialize);
assert_not_impl!(AggregateDenominatorHistory, serde::de::DeserializeOwned);

const FOOTPRINT: &str = "footprint.json";
type FixtureEndpoint = (OwnerId, Endpoint, TerminalDisposition);

const PARAMETER_DECAY_ENDPOINTS: &[FixtureEndpoint] = &[
    (
        OwnerId::NativeTrace,
        Endpoint::NativeInterpreterRkTrace,
        TerminalDisposition::Observed,
    ),
    (
        OwnerId::DirectGalecRefusal,
        Endpoint::DirectGalecContinuousRefusal,
        TerminalDisposition::TypedRefusal,
    ),
    (
        OwnerId::RegisteredGalecRefusal,
        Endpoint::RegisteredGalecContinuousRefusal,
        TerminalDisposition::TypedRefusal,
    ),
    (
        OwnerId::RegisteredEfmuRefusal,
        Endpoint::RegisteredEfmuContinuousRefusal,
        TerminalDisposition::TypedRefusal,
    ),
];

const UNIT_DERIVATIVE_ENDPOINTS: &[FixtureEndpoint] = &[
    (
        OwnerId::DaeSolveEquationRefinement,
        Endpoint::DaeSolveProductionRefinement,
        TerminalDisposition::Blocked,
    ),
    (
        OwnerId::NativeTrace,
        Endpoint::NativeInterpreterRkTrace,
        TerminalDisposition::Observed,
    ),
    (
        OwnerId::Fmi3ModelDescription,
        Endpoint::Fmi3ModelDescriptionXml,
        TerminalDisposition::Observed,
    ),
    (
        OwnerId::Fmi3BuildDescription,
        Endpoint::Fmi3BuildDescriptionXml,
        TerminalDisposition::Observed,
    ),
    (
        OwnerId::Fmi3ModelSource,
        Endpoint::Fmi3ModelSourceCc,
        TerminalDisposition::Observed,
    ),
    (
        OwnerId::Fmi3Archive,
        Endpoint::Fmi3ArchiveMembership,
        TerminalDisposition::Observed,
    ),
    (
        OwnerId::Fmi3MeLifecycle,
        Endpoint::Fmi3MeDerivativeStateLifecycle,
        TerminalDisposition::Observed,
    ),
    (
        OwnerId::Fmi3CsLifecycle,
        Endpoint::Fmi3CsDoStepLifecycle,
        TerminalDisposition::Observed,
    ),
    (
        OwnerId::DirectGalecRefusal,
        Endpoint::DirectGalecContinuousRefusal,
        TerminalDisposition::TypedRefusal,
    ),
    (
        OwnerId::RegisteredGalecRefusal,
        Endpoint::RegisteredGalecContinuousRefusal,
        TerminalDisposition::TypedRefusal,
    ),
    (
        OwnerId::RegisteredEfmuRefusal,
        Endpoint::RegisteredEfmuContinuousRefusal,
        TerminalDisposition::TypedRefusal,
    ),
];

#[test]
fn candidate_only_aggregate_is_zero_by_construction_and_rejects_tampered_history() {
    let directory = tempfile::tempdir().expect("candidate aggregate fixture");
    let root = directory.path();
    write_capture_inputs(root);
    let registry = GoldenRegistry {
        models: vec![candidate_model(root)],
    };
    let admission =
        check_candidate_only_registry(root, registry).expect("checked candidate-only registry");
    assert!(NonZeroUsize::new(0).is_none());
    let current = NonZeroUsize::new(42).expect("nonzero denominator");
    let previous = NonZeroUsize::new(50).expect("nonzero prior denominator");
    let history = checked_aggregate_denominator_history(current, Some(previous))
        .expect("representable denominator delta");
    let report = candidate_only_aggregate_report(admission, history);
    assert_eq!(report.admitted_model_count(), 0);
    assert_eq!(report.union_covered_lines(), 0);
    assert_eq!(report.workspace_instrumentable_production_lines(), current);
    assert_eq!(
        report.previous_workspace_instrumentable_production_lines(),
        Some(previous)
    );
    assert_eq!(report.denominator_line_delta(), Some(-8));
    assert_eq!(report.golden_line_coverage_percent(), 0.0);

    let value = serde_json::to_value(&report).expect("serialize checked aggregate");
    let source = serde_json::to_string(&value).expect("aggregate JSON");
    assert_eq!(
        parse_candidate_only_aggregate_denominator(&source, "checked aggregate")
            .expect("read checked aggregate"),
        current
    );
    for (field, replacement) in [
        ("admitted_models", serde_json::json!(["forged"])),
        ("per_model_covered_lines", serde_json::json!({"forged": 1})),
        ("marginal_covered_lines", serde_json::json!({"forged": 1})),
        ("union_covered_lines", serde_json::json!(1)),
        ("golden_line_coverage_percent", serde_json::json!(1.0)),
    ] {
        let mut tampered = value.clone();
        tampered[field] = replacement;
        let source = serde_json::to_string(&tampered).expect("tampered JSON");
        assert!(
            parse_candidate_only_aggregate_denominator(&source, "tampered aggregate").is_err(),
            "tampered admission field `{field}` was accepted"
        );
    }
    for field in [
        "workspace_instrumentable_production_lines",
        "previous_workspace_instrumentable_production_lines",
    ] {
        let mut zero_denominator = value.clone();
        zero_denominator[field] = serde_json::json!(0);
        let source = serde_json::to_string(&zero_denominator).expect("zero denominator JSON");
        assert!(
            parse_candidate_only_aggregate_denominator(&source, "zero denominator").is_err(),
            "zero denominator field `{field}` was accepted"
        );
    }
}

#[test]
fn schema_two_parser_rejects_legacy_ambiguity_and_unknown_fields() {
    let fixture = reviewed_schema_fixture("Kernel");
    parse_golden_registry(&fixture, "valid schema 2").expect("strict reviewed schema");

    let legacy = fixture.replacen("schema_version = 2", "schema_version = 1", 1);
    assert!(parse_golden_registry(&legacy, "legacy schema").is_err());
    let rejected = fixture.replacen("reason =", "verdict = \"reject\"\nreason =", 1);
    assert!(parse_golden_registry(&rejected, "legacy verdict").is_err());
    let unknown = fixture.replacen(
        "profile = \"unit-derivative-v1\"",
        "profile = \"unit-derivative-v1\"\nextra = true",
        1,
    );
    assert!(parse_golden_registry(&unknown, "unknown field").is_err());
    let unknown_kind = fixture.replacen("kind = \"reviewed\"", "kind = \"golden\"", 1);
    assert!(parse_golden_registry(&unknown_kind, "unknown claim kind").is_err());
    let unknown_profile = fixture.replacen("unit-derivative-v1", "scalar", 1);
    assert!(parse_golden_registry(&unknown_profile, "unknown profile").is_err());
    let missing = fixture.replacen("coverage_footprint =", "footprint_note =", 1);
    assert!(parse_golden_registry(&missing, "missing reviewed binding").is_err());
}

#[test]
fn omitted_candidate_footprint_is_explicit_option_semantics_not_serde_default() {
    let fixture = reviewed_schema_fixture("Kernel")
        .replacen("kind = \"reviewed\"", "kind = \"candidate\"", 1)
        .replace(
            &format!(
                "coverage_footprint = {{ path = \"footprint.json\", sha256 = \"{}\" }}\n",
                "00".repeat(32)
            ),
            "",
        )
        .replace(
            "outstanding = [{ endpoint = \"dae-solve-production-refinement\", reason = \"not implemented\" }]\n",
            "",
        );
    let registry = parse_golden_registry(&fixture, "candidate without footprint")
        .expect("Option<T> treats an omitted candidate footprint as None");
    let ModelClaim::Candidate(candidate) = &registry.models()[0].claim else {
        panic!("candidate fixture changed claim kind")
    };
    assert!(candidate.coverage_footprint.is_none());
    assert!(!include_str!("mod.rs").contains("serde(default)"));
}

#[test]
fn duplicate_model_id_is_rejected() {
    let body = reviewed_model_body("Kernel");
    let fixture = format!("schema_version = 2\n{body}{body}");
    assert!(parse_golden_registry(&fixture, "duplicate model").is_err());
}

#[test]
fn empty_registry_and_duplicate_canonical_source_are_rejected() {
    assert!(parse_golden_registry("schema_version = 2\n", "empty registry").is_err());
    let first = reviewed_model_body("Kernel");
    let second = reviewed_model_body("OtherKernel");
    let fixture = format!("schema_version = 2\n{first}{second}");
    assert!(parse_golden_registry(&fixture, "duplicate source").is_err());
}

#[test]
fn candidate_validation_returns_the_exact_borrowed_capture() {
    let directory = tempfile::tempdir().expect("candidate fixture");
    let root = directory.path();
    write_capture_inputs(root);
    let model = candidate_model(root);
    let checked = check_candidate_capture(root, &model).expect("valid candidate capture");
    assert_eq!(checked.id(), "Kernel");
    assert_eq!(checked.source().path, "model.mo");
    assert_eq!(checked.scenarios().len(), 1);

    let mut stale = model;
    candidate_mut(&mut stale).capture.source.sha256 = "00".repeat(32);
    assert_refusal(
        check_candidate_capture(root, &stale),
        "stale content binding",
    );
}

#[test]
fn candidate_footprint_is_bound_as_capture_metadata_but_never_admitted() {
    let directory = tempfile::tempdir().expect("candidate fixture");
    let root = directory.path();
    write_capture_inputs(root);
    let mut model = candidate_model(root);
    candidate_mut(&mut model).coverage_footprint = Some(binding(root, FOOTPRINT));
    check_candidate_capture(root, &model).expect("current candidate footprint binding");

    fs::write(root.join(FOOTPRINT), "changed capture\n").expect("move footprint bytes");
    assert_refusal(
        check_candidate_capture(root, &model),
        "stale content binding",
    );
}

#[test]
fn candidate_paths_digests_scenarios_and_cone_are_fail_closed() {
    let directory = tempfile::tempdir().expect("candidate fixture");
    let root = directory.path();
    write_capture_inputs(root);
    let model = candidate_model(root);

    let mut unsafe_path = model.clone();
    candidate_mut(&mut unsafe_path).capture.source.path = "../model.mo".to_string();
    assert_refusal(check_candidate_capture(root, &unsafe_path), "non-normal");

    let mut uppercase_digest = model.clone();
    candidate_mut(&mut uppercase_digest).capture.source.sha256 = "AA".repeat(32);
    assert_refusal(
        check_candidate_capture(root, &uppercase_digest),
        "invalid SHA-256",
    );

    let mut vacuous = model.clone();
    candidate_mut(&mut vacuous).capture.cone.clear();
    assert_refusal(check_candidate_capture(root, &vacuous), "exact owner set");

    let mut redrawn = model.clone();
    candidate_mut(&mut redrawn).capture.cone[1].predecessors = vec![OwnerId::ResolvedAst];
    assert_refusal(
        check_candidate_capture(root, &redrawn),
        "exact ordered edges",
    );

    let mut narrowed_profile = model.clone();
    narrowed_profile.profile = GoldenProfile::UnitDerivativeV1;
    candidate_mut(&mut narrowed_profile).capture.cone =
        profile_cone_nodes(GoldenProfile::UnitDerivativeV1);
    assert_refusal(
        check_candidate_capture(root, &narrowed_profile),
        "selected profile's endpoints",
    );

    let mut duplicate = model.clone();
    let node = candidate_mut(&mut duplicate).capture.cone[1].clone();
    candidate_mut(&mut duplicate).capture.cone.push(node);
    assert_refusal(check_candidate_capture(root, &duplicate), "exact owner set");

    let mut no_endpoint = model.clone();
    candidate_mut(&mut no_endpoint).capture.scenarios[0]
        .requests
        .clear();
    assert_refusal(
        check_candidate_capture(root, &no_endpoint),
        "no endpoint request",
    );

    let mut contradictory = model.clone();
    candidate_mut(&mut contradictory).capture.scenarios[0].requests[0].disposition =
        TerminalDisposition::TypedRefusal;
    assert_refusal(
        check_candidate_capture(root, &contradictory),
        "contradictory terminal disposition",
    );

    let mut injected = model;
    candidate_mut(&mut injected).capture.scenarios[0]
        .test
        .test_name = "pipeline_test::kernel --include-ignored".to_string();
    assert_refusal(
        check_candidate_capture(root, &injected),
        "invalid exact libtest identity",
    );
}

#[test]
fn compliance_review_instant_is_strict_rfc3339() {
    let directory = tempfile::tempdir().expect("candidate fixture");
    let root = directory.path();
    write_capture_inputs(root);
    let mut model = candidate_model(root);
    candidate_mut(&mut model).last_compliance_check = "2026-09-04".to_string();
    assert_refusal(check_candidate_capture(root, &model), "not RFC 3339");
}

#[test]
fn valid_reviewed_record_fails_with_typed_not_implemented() {
    let directory = tempfile::tempdir().expect("reviewed fixture");
    let root = directory.path();
    write_capture_inputs(root);
    let model = reviewed_model(root);
    let error = result_error(check_candidate_capture(root, &model));
    assert!(
        error
            .downcast_ref::<ReviewedAdmissionNotImplemented>()
            .is_some(),
        "reviewed refusal lost its typed identity: {error:#}"
    );
}

#[test]
fn malformed_reviewed_common_evidence_is_rejected_before_not_implemented() {
    let directory = tempfile::tempdir().expect("reviewed fixture");
    let root = directory.path();
    write_capture_inputs(root);
    let model = reviewed_model(root);

    let mut stale = model.clone();
    reviewed_mut(&mut stale).capture.review_record.sha256 = "00".repeat(32);
    let error = result_error(check_candidate_capture(root, &stale));
    assert!(
        error
            .downcast_ref::<ReviewedAdmissionNotImplemented>()
            .is_none()
    );
    assert!(format!("{error:#}").contains("stale content binding"));

    let mut silent_debt = model;
    reviewed_mut(&mut silent_debt).outstanding.clear();
    assert_refusal(
        check_candidate_capture(root, &silent_debt),
        "no outstanding entry",
    );
}

fn assert_refusal<T>(result: anyhow::Result<T>, needle: &str) {
    let error = result_error(result);
    let rendered = format!("{error:#}");
    assert!(
        rendered.contains(needle),
        "refusal must name `{needle}`: {rendered}"
    );
}

fn result_error<T>(result: anyhow::Result<T>) -> anyhow::Error {
    match result {
        Ok(_) => panic!("invalid record was accepted"),
        Err(error) => error,
    }
}

fn write_capture_inputs(root: &Path) {
    fs::write(root.join("model.mo"), "model Kernel end Kernel;\n").expect("source");
    fs::write(root.join("review.md"), "capture review\n").expect("review");
    fs::write(root.join(FOOTPRINT), "candidate capture\n").expect("footprint");
}

fn capture_plan(root: &Path, profile: GoldenProfile) -> CapturePlan {
    let cone = profile_cone_nodes(profile);
    let endpoints = match profile {
        GoldenProfile::ParameterDecayV1 => PARAMETER_DECAY_ENDPOINTS,
        GoldenProfile::UnitDerivativeV1 => UNIT_DERIVATIVE_ENDPOINTS,
    };
    let requests = endpoints
        .iter()
        .map(|(_, endpoint, disposition)| EndpointRequest {
            endpoint: *endpoint,
            disposition: *disposition,
        })
        .collect();
    CapturePlan {
        source: binding(root, "model.mo"),
        review_record: binding(root, "review.md"),
        cone,
        scenarios: vec![CaptureScenario {
            id: "native".to_string(),
            test: CoverageTest {
                package: "rumoca".to_string(),
                test_target: "suite_core".to_string(),
                features: Vec::new(),
                test_name: "pipeline_test::kernel".to_string(),
            },
            requests,
        }],
    }
}

fn profile_cone_nodes(profile: GoldenProfile) -> Vec<CandidateConeNode> {
    profile_cone(profile)
        .iter()
        .map(|(owner, predecessors)| CandidateConeNode {
            owner: *owner,
            predecessors: predecessors.to_vec(),
        })
        .collect()
}

fn candidate_model(root: &Path) -> GoldenModelRecord {
    GoldenModelRecord {
        id: "Kernel".to_string(),
        profile: GoldenProfile::ParameterDecayV1,
        claim: ModelClaim::Candidate(CandidateClaim {
            reason: "review remains open".to_string(),
            last_compliance_check: "2026-09-04T00:00:00Z".to_string(),
            capture: capture_plan(root, GoldenProfile::ParameterDecayV1),
            coverage_footprint: None,
        }),
    }
}

fn reviewed_model(root: &Path) -> GoldenModelRecord {
    GoldenModelRecord {
        id: "Kernel".to_string(),
        profile: GoldenProfile::UnitDerivativeV1,
        claim: ModelClaim::Reviewed(ReviewedClaim {
            reason: "future reviewed record".to_string(),
            last_compliance_check: "2026-09-04T00:00:00Z".to_string(),
            capture: capture_plan(root, GoldenProfile::UnitDerivativeV1),
            coverage_footprint: binding(root, FOOTPRINT),
            outstanding: vec![OutstandingReceipt {
                endpoint: Endpoint::DaeSolveProductionRefinement,
                reason: "semantic evidence admission is not implemented".to_string(),
            }],
        }),
    }
}

fn candidate_mut(model: &mut GoldenModelRecord) -> &mut CandidateClaim {
    let ModelClaim::Candidate(candidate) = &mut model.claim else {
        panic!("candidate fixture changed claim")
    };
    candidate
}

fn reviewed_mut(model: &mut GoldenModelRecord) -> &mut ReviewedClaim {
    let ModelClaim::Reviewed(reviewed) = &mut model.claim else {
        panic!("reviewed fixture changed claim")
    };
    reviewed
}

fn digest(root: &Path, path: &str) -> String {
    format!(
        "{:x}",
        Sha256::digest(fs::read(root.join(path)).expect("binding input"))
    )
}

fn binding(root: &Path, path: &str) -> ContentBinding {
    ContentBinding {
        path: path.to_string(),
        sha256: digest(root, path),
    }
}

fn reviewed_schema_fixture(id: &str) -> String {
    format!("schema_version = 2\n{}", reviewed_model_body(id))
}

fn reviewed_model_body(id: &str) -> String {
    format!(
        r#"
[[models]]
id = "{id}"
profile = "unit-derivative-v1"

[models.claim]
kind = "reviewed"

[models.claim.details]
reason = "future reviewed record"
last_compliance_check = "2026-09-04T00:00:00Z"
coverage_footprint = {{ path = "footprint.json", sha256 = "{zero}" }}
outstanding = [{{ endpoint = "dae-solve-production-refinement", reason = "not implemented" }}]

[models.claim.details.capture]
source = {{ path = "model.mo", sha256 = "{zero}" }}
review_record = {{ path = "review.md", sha256 = "{zero}" }}
cone = [
  {{ owner = "canonical-source", predecessors = [] }},
  {{ owner = "native-trace", predecessors = ["canonical-source"] }},
  {{ owner = "dae-solve-equation-refinement", predecessors = ["canonical-source"] }},
]

[[models.claim.details.capture.scenarios]]
id = "native"
test = {{ package = "rumoca", test_target = "suite_core", features = [], test_name = "pipeline_test::kernel" }}
requests = [
  {{ endpoint = "native-interpreter-rk-trace", disposition = "observed" }},
  {{ endpoint = "dae-solve-production-refinement", disposition = "blocked" }},
]
"#,
        zero = "00".repeat(32)
    )
}

#[test]
fn golden_scenarios_consume_only_the_canonical_model_fixtures() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(Path::parent)
        .expect("xtask has a workspace root");
    let core = fs::read_to_string(root.join("crates/rumoca/tests/suite_core/pipeline_test.rs"))
        .expect("read core golden scenarios");
    let fmi = fs::read_to_string(
        root.join("crates/rumoca/tests/suite_template_runtime/cli_target_fmi.rs"),
    )
    .expect("read FMI golden scenarios");

    assert!(core.contains("/tests/fixtures/golden/UnitDerivative.mo"));
    assert!(core.contains("/tests/fixtures/golden/ParameterDecay.mo"));
    assert!(fmi.contains("/tests/fixtures/golden/UnitDerivative.mo"));
    assert_eq!(
        core.matches("model UnitDerivative").count(),
        1,
        "only the intentional invalid-default fixture may be inline"
    );
    assert_eq!(core.matches("model ParameterDecay").count(), 0);
    assert_eq!(fmi.matches("model UnitDerivative").count(), 0);
    assert_eq!(fmi.matches("model ParameterDecay").count(), 0);
}
