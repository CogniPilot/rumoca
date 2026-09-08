use super::emit;
use super::manifest::{
    ArtifactSessionInputs, BaselineMeasurement, ChecksumWebEdge, Comparator, ComparatorOutputPins,
    CorrectnessCase, CrossProfile, EfmuArchiveMember, EfmuProductionMember, Entry, Manifest,
    Measured, MeasuredToolPins, Metric, OracleSuccessReceipt, RowEvidenceState,
    RumocaArtifactRevision, RumocaProfile, Target,
};
use super::{judge, select};
use sha2::{Digest, Sha256};
use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::Command;

fn digest() -> String {
    "0".repeat(64)
}

fn artifact_bundle() -> super::artifact_bundle::BundleEvidence {
    super::artifact_bundle::BundleEvidence::synthetic(
        "evidence-bundle/rows/test".into(),
        digest(),
        Vec::new(),
    )
}

fn manifest(entries: Vec<Entry>) -> Manifest {
    Manifest {
        schema_version: super::manifest::SCHEMA_VERSION,
        ratchet_policy: "fall only".into(),
        suite_implementation_history: super::manifest::SuiteImplementationHistory::initial(
            super::suite_identity::sha256(),
        ),
        measured_tools: MeasuredToolPins {
            arm_gcc_sha256: digest(),
            arm_nm_sha256: digest(),
            qemu_sha256: digest(),
            python_sha256: digest(),
            prlimit_sha256: digest(),
            prlimit_version: "prlimit test version".into(),
            git_sha256: digest(),
            git_version: "git test version".into(),
            cargo_sha256: digest(),
            cargo_version: "cargo test version".into(),
            rustc_sha256: digest(),
            rustc_version: "rustc test version".into(),
            compiler_dependencies_sha256: digest(),
            execution_tool_closure_sha256: digest(),
            arm_size_sha256: None,
        },
        comparator_outputs: comparator_output_pins(),
        entries,
    }
}

fn comparator_output_pins() -> ComparatorOutputPins {
    ComparatorOutputPins {
        casadi_c_sha256: digest(),
        casadi_h_sha256: digest(),
    }
}

#[test]
fn measured_tool_pins_require_arm_size_key_but_accept_null() {
    let complete = serde_json::to_value(manifest(vec![entry()]))
        .expect("serialize current embedded-head-to-head manifest");
    assert!(complete["measured_tools"]["arm_size_sha256"].is_null());
    serde_json::from_value::<Manifest>(complete.clone())
        .expect("explicit null is valid reserved-tool absence");

    let mut missing = complete;
    missing["measured_tools"]
        .as_object_mut()
        .expect("measured_tools is an object")
        .remove("arm_size_sha256")
        .expect("current manifest writes arm_size_sha256");
    assert!(
        serde_json::from_value::<Manifest>(missing).is_err(),
        "current manifest must reject an omitted measured_tools.arm_size_sha256"
    );
}

fn instruction_evidence(rumoca: u64, comparator: u64) -> super::Evidence {
    super::Evidence {
        entry_sha256: digest(),
        suite_implementation_sha256: super::suite_identity::sha256(),
        normalized_profile_sha256: digest(),
        completed_suite_sha256: digest(),
        authenticated_inputs_sha256: digest(),
        rumoca_instructions: rumoca,
        comparator_instructions: comparator,
        output_lines: Vec::new(),
        artifact_bundle: artifact_bundle(),
        commands: Vec::new(),
    }
}

fn assert_exact_pin(
    verdict: &super::Verdict,
    expected_delta: i64,
    expected_outcome: super::ComparisonOutcome,
) {
    match &verdict.result {
        super::VerdictResult::ExactPin {
            delta,
            comparison_outcome,
            ..
        } => {
            assert_eq!(*delta, expected_delta);
            assert_eq!(*comparison_outcome, expected_outcome);
        }
        _ => panic!("expected an exact-pin verdict"),
    }
}

fn rejected_findings(verdict: &super::Verdict) -> &[String] {
    match &verdict.result {
        super::VerdictResult::Rejected { findings, .. } => findings,
        _ => panic!("expected a rejected verdict"),
    }
}

fn entry() -> Entry {
    let mut entry = Entry {
        id: "exp-mixed-efmu-production-c-casadi".into(),
        fixture: "fixture.mo".into(),
        fixture_sha256: digest(),
        model: "ExpMixedStep".into(),
        target: Target::Efmu,
        rumoca_profile: RumocaProfile::EfmuFirstProductRetainCalls,
        comparator: Comparator::Casadi372FullF32,
        comparator_generator: "gen.py".into(),
        comparator_generator_sha256: digest(),
        comparator_wrapper: "wrapper.c".into(),
        comparator_wrapper_sha256: digest(),
        cross_profile: CrossProfile::Gcc15CortexM7O3LtoNoFpContractHotAuto700Single700ExactV3,
        normalized_profile_sha256: digest(),
        input_header: "inputs.h".into(),
        input_sha256: digest(),
        harness_sha256: digest(),
        correctness_cases: [
            "current-closed-closed",
            "tiny-all-series",
            "middle-coefficient-series",
            "different-dense-coupling",
        ]
        .into_iter()
        .map(|id| CorrectnessCase {
            id: id.into(),
            expected_output_bits: vec!["00000000".into(); 10],
        })
        .collect(),
        metric: Metric::GuestInstructions,
        evidence_state: RowEvidenceState::TestExactPin,
        max_delta: Some(166),
        match_or_beat_target_delta: 0,
        measured: BaselineMeasurement::Accepted(Measured {
            rumoca: 1250,
            comparator: 1084,
            comment: "initial generated-source measurement".into(),
        }),
        why: "the mixed transcendental kernel is the current laggard".into(),
        rumoca_artifact_history: Vec::new(),
    };
    entry.normalized_profile_sha256 = super::cross::normalized_profile_sha256(&entry);
    entry
}

fn pending_entry() -> Entry {
    let mut row = entry();
    row.id = "exp-mixed-efmu-production-c-casadi-3-7-2-cortex-m7".into();
    row.evidence_state = RowEvidenceState::PendingWholeScopeEvidence;
    row.max_delta = None;
    row.measured = BaselineMeasurement::Pending(());
    row
}

fn efmu_entry() -> Entry {
    let mut row = entry();
    row.id = "exp-mixed-efmu-production-c-casadi".into();
    let BaselineMeasurement::Accepted(measured) = &row.measured else {
        unreachable!("entry fixture always carries an accepted baseline");
    };
    row.rumoca_artifact_history = vec![artifact_revision(&row, measured.rumoca, '1')];
    row.normalized_profile_sha256 = super::cross::normalized_profile_sha256(&row);
    row
}

fn artifact_revision(
    row: &Entry,
    rumoca_metric: u64,
    package_digit: char,
) -> RumocaArtifactRevision {
    let production_c = EfmuProductionMember {
        archive_path: "ProductionCode/sources/production.c".into(),
        sha256: package_digit.to_string().repeat(64),
    };
    let production_h = EfmuProductionMember {
        archive_path: "ProductionCode/sources/production.h".into(),
        sha256: "3".repeat(64),
    };
    let checksum_web_membership = [
        ("AlgorithmCode/model.alg", "AlgorithmCode/manifest.xml", '4'),
        (
            production_c.archive_path.as_str(),
            "ProductionCode/manifest.xml",
            '5',
        ),
        (
            production_h.archive_path.as_str(),
            "ProductionCode/manifest.xml",
            '6',
        ),
        (
            "AlgorithmCode/manifest.xml",
            "ProductionCode/manifest.xml",
            '7',
        ),
        ("AlgorithmCode/manifest.xml", "__content.xml", '8'),
        ("ProductionCode/manifest.xml", "__content.xml", '9'),
    ]
    .into_iter()
    .map(|(producer, consumer, sha1_digit)| ChecksumWebEdge {
        producer_archive_path: producer.into(),
        producer_sha1: sha1_digit.to_string().repeat(40),
        consumer_archive_path: consumer.into(),
    })
    .collect();
    let efmu_package_sha256 = package_digit.to_string().repeat(64);
    let archive_members = super::efmu_artifact::canonical_members()
        .iter()
        .map(|path| EfmuArchiveMember {
            archive_path: (*path).into(),
            sha256: match *path {
                "ProductionCode/sources/production.c" => production_c.sha256.clone(),
                "ProductionCode/sources/production.h" => production_h.sha256.clone(),
                _ => "9".repeat(64),
            },
        })
        .collect();
    RumocaArtifactRevision {
        rumoca_metric,
        rationale: format!("authenticated strict improvement to {rumoca_metric}"),
        artifact_session: ArtifactSessionInputs {
            generation_instant: "2026-08-30T12:34:56Z".into(),
            identity_seed: "00000000-0000-0000-0000-000000000009".into(),
        },
        efmu_package_sha256: efmu_package_sha256.clone(),
        archive_members,
        production_c,
        production_h,
        checksum_web_membership,
        oracle_success_receipt: OracleSuccessReceipt {
            receipt_sha256: "a".repeat(64),
            efmu_package_sha256,
            correctness_cases_sha256: super::manifest::correctness_cases_sha256(
                &row.correctness_cases,
            ),
        },
    }
}

fn valid_efmu_members(session: &ArtifactSessionInputs) -> Vec<(String, Vec<u8>)> {
    let source = b"void production_step(void) {}\n";
    let header = b"void production_step(void);\n";
    let algorithm = b"algorithm code\n";
    let ac_identity = format!(
        "{{{}}}",
        super::efmu_artifact::artifact_identity_for_test(&session.identity_seed, "ac_manifest")
    );
    let pc_identity = format!(
        "{{{}}}",
        super::efmu_artifact::artifact_identity_for_test(&session.identity_seed, "pc_manifest")
    );
    let content_identity = format!(
        "{{{}}}",
        super::efmu_artifact::artifact_identity_for_test(&session.identity_seed, "content")
    );
    let algorithm_sha1 = format!("{:x}", sha1::Sha1::digest(algorithm));
    let ac = format!(
        "<?xml version=\"1.0\"?><Manifest id=\"{ac_identity}\" generationDateAndTime=\"{}\"><Files><File name=\"model.alg\" path=\"./\" needsChecksum=\"true\" checksum=\"{}\"/></Files></Manifest>",
        session.generation_instant,
        algorithm_sha1,
    )
    .into_bytes();
    let ac_sha1 = format!("{:x}", sha1::Sha1::digest(&ac));
    let header_sha1 = format!("{:x}", sha1::Sha1::digest(header));
    let source_sha1 = format!("{:x}", sha1::Sha1::digest(source));
    let pc = format!(
        "<?xml version=\"1.0\"?><Manifest id=\"{pc_identity}\" generationDateAndTime=\"{}\"><ManifestReferences><ManifestReference manifestRefId=\"{ac_identity}\" checksum=\"{}\" origin=\"true\"/></ManifestReferences><Files><File name=\"production.h\" path=\"./sources/\" needsChecksum=\"true\" checksum=\"{}\"/><File name=\"production.c\" path=\"./sources/\" needsChecksum=\"true\" checksum=\"{}\"/></Files></Manifest>",
        session.generation_instant,
        ac_sha1,
        header_sha1,
        source_sha1,
    )
    .into_bytes();
    let pc_sha1 = format!("{:x}", sha1::Sha1::digest(&pc));
    let content = format!(
        "<?xml version=\"1.0\"?><Content id=\"{content_identity}\" generationDateAndTime=\"{}\"><ModelRepresentation name=\"AlgorithmCode\" kind=\"AlgorithmCode\" manifest=\"manifest.xml\" checksum=\"{}\" manifestRefId=\"{ac_identity}\"/><ModelRepresentation name=\"ProductionCode\" kind=\"ProductionCode\" manifest=\"manifest.xml\" checksum=\"{}\" manifestRefId=\"{pc_identity}\"/></Content>",
        session.generation_instant,
        ac_sha1,
        pc_sha1,
    )
    .into_bytes();
    super::efmu_artifact::canonical_members()
        .iter()
        .map(|path| {
            let bytes = match *path {
                "AlgorithmCode/manifest.xml" => ac.clone(),
                "AlgorithmCode/model.alg" => algorithm.to_vec(),
                "ProductionCode/manifest.xml" => pc.clone(),
                "ProductionCode/sources/production.c" => source.to_vec(),
                "ProductionCode/sources/production.h" => header.to_vec(),
                "__content.xml" => content.clone(),
                _ => format!("authenticated fixture bytes for {path}\n").into_bytes(),
            };
            ((*path).to_owned(), bytes)
        })
        .collect()
}

fn write_efmu_archive(path: &Path, members: &[(String, Vec<u8>)]) {
    let file = fs::File::create(path).unwrap();
    let mut archive = zip::ZipWriter::new(file);
    let options = zip::write::SimpleFileOptions::default()
        .compression_method(zip::CompressionMethod::Deflated)
        .last_modified_time(zip::DateTime::default())
        .unix_permissions(0o644);
    for (name, bytes) in members {
        archive.start_file(name, options).unwrap();
        archive.write_all(bytes).unwrap();
    }
    archive.finish().unwrap();
}

fn derived_efmu_fixture(directory: &Path) -> (Entry, PathBuf, RumocaArtifactRevision) {
    let row = efmu_entry();
    let BaselineMeasurement::Accepted(measured) = &row.measured else {
        unreachable!("efmu fixture always carries an accepted baseline");
    };
    let archive_path = directory.join("valid.efmu");
    let session = row.rumoca_artifact_history[0].artifact_session.clone();
    write_efmu_archive(&archive_path, &valid_efmu_members(&session));
    let revision = super::efmu_artifact::derive_revision(
        &archive_path,
        &row,
        measured.rumoca,
        "authenticated exact archive and closed oracle cohort",
        session,
        &row.correctness_cases,
    )
    .unwrap();
    (row, archive_path, revision)
}

#[test]
fn manifest_rejects_unknown_rows_and_non_lowercase_oracle_words() {
    let manifest = manifest(vec![entry()]);
    assert!(select(&manifest, &["missing".into()]).is_err());
    let mut invalid = manifest.clone();
    invalid.entries[0].correctness_cases[0].expected_output_bits[0] = "DEADBEEF".into();
    assert!(super::manifest::validate(&invalid).is_err());

    let mut stale_schema = manifest;
    stale_schema.schema_version = super::manifest::SCHEMA_VERSION - 1;
    assert!(
        super::manifest::validate(&stale_schema).is_err(),
        "the pre-sealed-plan schema must not masquerade as the current wire contract"
    );
}

#[test]
fn row_evidence_state_is_mandatory_and_claim_eligibility_is_unrepresentable() {
    let valid = manifest(vec![entry()]);
    let mut omitted = serde_json::to_value(&valid).unwrap();
    omitted["entries"][0]
        .as_object_mut()
        .unwrap()
        .remove("evidence_state");
    assert!(serde_json::from_value::<Manifest>(omitted).is_err());

    let mut unknown = serde_json::to_value(&valid).unwrap();
    unknown["entries"][0]["evidence_state"]["state"] = serde_json::json!("unknown");
    assert!(serde_json::from_value::<Manifest>(unknown).is_err());

    let mut unsupported = serde_json::to_value(&valid).unwrap();
    unsupported["entries"][0]["evidence_state"] = serde_json::json!({
        "state": "claim-eligible"
    });
    assert!(serde_json::from_value::<Manifest>(unsupported).is_err());

    let mut legacy_migration = serde_json::to_value(valid).unwrap();
    legacy_migration["migration_ledger"] = serde_json::json!([]);
    assert!(
        serde_json::from_value::<Manifest>(legacy_migration).is_err(),
        "schema nine must reject the superseded global migration ledger"
    );
}

#[test]
fn schema_nine_accepts_only_pending_efmu_without_a_baseline_or_history() {
    let row = pending_entry();
    super::manifest::validate(&manifest(vec![row.clone()])).unwrap();
    assert_eq!(row.target.as_str(), "efmu");
    assert!(row.rumoca_artifact_history.is_empty());

    let mut borrowed = row;
    borrowed.rumoca_artifact_history = efmu_entry().rumoca_artifact_history;
    let error = super::manifest::validate(&manifest(vec![borrowed])).unwrap_err();
    assert!(format!("{error:#}").contains("cannot carry artifact history"));
}

#[test]
fn pending_schema_nine_row_refuses_selection_and_any_bypassed_measurement() {
    let row = pending_entry();
    let pending = manifest(vec![row.clone()]);
    let error = select(&pending, &[])
        .err()
        .expect("pending row must refuse");
    assert!(format!("{error:#}").contains("pending-whole-scope-evidence"));

    let verdict = judge(&row, instruction_evidence(1, 1), 0.0);
    let wire = serde_json::to_value(verdict).unwrap();
    assert_eq!(wire["status"], "rejected");
    assert!(wire.get("delta").is_none());
    assert!(wire.get("comparison_outcome").is_none());
    assert!(
        wire["findings"][0]
            .as_str()
            .unwrap()
            .contains("has no accepted baseline")
    );
}

#[test]
fn schema_nine_efmu_inventory_matches_the_builtin_target_assets() {
    let workspace = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let target_path = workspace.join("crates/rumoca-phase-codegen/src/templates/efmu/target.toml");
    let schemas = workspace.join("crates/rumoca-phase-codegen/src/templates/galec/schemas");
    let target: toml::Value = toml::from_str(
        &fs::read_to_string(&target_path).expect("read schema-9 efmu target manifest"),
    )
    .expect("parse schema-9 efmu target manifest");
    let file_paths = target["files"]
        .as_array()
        .expect("efmu [[files]] array")
        .iter()
        .map(|file| {
            (
                file["id"].as_str().expect("packaged file id"),
                file["path"].as_str().expect("packaged file path"),
            )
        })
        .collect::<std::collections::BTreeMap<_, _>>();
    let asset_destinations = target["assets"]
        .as_array()
        .expect("efmu [[assets]] array")
        .iter()
        .map(|asset| {
            (
                asset["source"].as_str().expect("asset source"),
                asset["dest"].as_str().expect("asset destination"),
            )
        })
        .collect::<std::collections::BTreeMap<_, _>>();
    let observed = target["package"]["members"]
        .as_array()
        .expect("efmu [[package.members]] array")
        .iter()
        .map(
            |member| match member["kind"].as_str().expect("package member kind") {
                "file" => file_paths
                    .get(member["file"].as_str().expect("package file reference"))
                    .expect("package file reference resolves")
                    .to_string(),
                "asset" => {
                    let source = member["source"]
                        .as_str()
                        .expect("package asset source reference");
                    let destination = asset_destinations
                        .get(source)
                        .expect("package asset source resolves")
                        .trim_end_matches('/');
                    let relative = member["path"]
                        .as_str()
                        .expect("package asset relative path");
                    assert!(
                        schemas.join(relative).is_file(),
                        "package asset member must name an exact vendored schema: {relative}"
                    );
                    format!("{destination}/{relative}")
                }
                kind => panic!("unknown efmu package member kind `{kind}`"),
            },
        )
        .collect::<Vec<_>>();
    assert_eq!(
        observed,
        super::efmu_artifact::canonical_members(),
        "schema-9 archive authentication must preserve the exact target-issued package-member order"
    );

    let mut declared_assets = observed
        .iter()
        .filter(|path| path.starts_with("schemas/"))
        .cloned()
        .collect::<Vec<_>>();
    declared_assets.sort();
    let mut vendored_assets = walkdir::WalkDir::new(&schemas)
        .into_iter()
        .map(Result::unwrap)
        .filter(|entry| entry.file_type().is_file())
        .map(|entry| {
            format!(
                "schemas/{}",
                entry
                    .path()
                    .strip_prefix(&schemas)
                    .unwrap()
                    .to_string_lossy()
                    .replace('\\', "/")
            )
        })
        .collect::<Vec<_>>();
    vendored_assets.sort();
    assert_eq!(
        declared_assets, vendored_assets,
        "the explicit efmu package sequence must contain every vendored schema exactly once"
    );
}

#[test]
fn efmu_archive_authentication_derives_every_artifact_pin_and_receipt() {
    let temporary = tempfile::tempdir().unwrap();
    let archive_path = temporary.path().join("valid.efmu");
    let mut row = efmu_entry();
    let BaselineMeasurement::Accepted(measured) = &row.measured else {
        unreachable!("efmu fixture always carries an accepted baseline");
    };
    let session = row.rumoca_artifact_history[0].artifact_session.clone();
    write_efmu_archive(&archive_path, &valid_efmu_members(&session));
    let revision = super::efmu_artifact::derive_revision(
        &archive_path,
        &row,
        measured.rumoca,
        "authenticated exact archive and closed oracle cohort",
        session,
        &row.correctness_cases,
    )
    .unwrap();
    super::efmu_artifact::authenticate_revision(
        &archive_path,
        &row,
        &revision,
        &row.correctness_cases,
    )
    .unwrap();
    assert_eq!(
        revision.archive_members.len(),
        super::efmu_artifact::canonical_members().len()
    );
    assert_eq!(
        revision.oracle_success_receipt.receipt_sha256,
        "b2b2f6bf45b315325e2564d90dc919545e64e33996a36132f5f7f9cbe7c9221c"
    );
    assert_eq!(
        revision.production_c.sha256,
        revision
            .archive_members
            .iter()
            .find(|member| member.archive_path == revision.production_c.archive_path)
            .unwrap()
            .sha256
    );
    row.rumoca_artifact_history = vec![revision];
    super::manifest::validate(&manifest(vec![row])).unwrap();
}

#[test]
fn efmu_archive_authentication_rejects_inventory_path_order_and_byte_mutation() {
    let temporary = tempfile::tempdir().unwrap();
    let row = efmu_entry();
    let BaselineMeasurement::Accepted(measured) = &row.measured else {
        unreachable!("efmu fixture always carries an accepted baseline");
    };
    let session = row.rumoca_artifact_history[0].artifact_session.clone();
    let valid = valid_efmu_members(&session);

    let missing_path = temporary.path().join("missing.efmu");
    write_efmu_archive(&missing_path, &valid[..valid.len() - 1]);
    assert!(
        super::efmu_artifact::derive_revision(
            &missing_path,
            &row,
            measured.rumoca,
            "missing member must fail",
            session.clone(),
            &row.correctness_cases,
        )
        .is_err()
    );

    let mut unsafe_path = valid.clone();
    unsafe_path[0].0 = "../AlgorithmCode/manifest.xml".into();
    let unsafe_archive = temporary.path().join("unsafe.efmu");
    write_efmu_archive(&unsafe_archive, &unsafe_path);
    assert!(
        super::efmu_artifact::derive_revision(
            &unsafe_archive,
            &row,
            measured.rumoca,
            "unsafe path must fail",
            session.clone(),
            &row.correctness_cases,
        )
        .is_err()
    );

    let mut reordered = valid.clone();
    reordered.swap(0, 1);
    let reordered_archive = temporary.path().join("reordered.efmu");
    write_efmu_archive(&reordered_archive, &reordered);
    assert!(
        super::efmu_artifact::derive_revision(
            &reordered_archive,
            &row,
            measured.rumoca,
            "member reorder must fail",
            session.clone(),
            &row.correctness_cases,
        )
        .is_err()
    );

    let valid_archive = temporary.path().join("before-byte-mutation.efmu");
    write_efmu_archive(&valid_archive, &valid);
    let revision = super::efmu_artifact::derive_revision(
        &valid_archive,
        &row,
        measured.rumoca,
        "byte mutation must fail",
        session.clone(),
        &row.correctness_cases,
    )
    .unwrap();
    let mut changed_bytes = valid;
    changed_bytes
        .iter_mut()
        .find(|(path, _)| path == "ProductionCode/sources/production.c")
        .unwrap()
        .1
        .extend_from_slice(b"/* mutation */\n");
    let changed_archive = temporary.path().join("after-byte-mutation.efmu");
    write_efmu_archive(&changed_archive, &changed_bytes);
    assert!(
        super::efmu_artifact::authenticate_revision(
            &changed_archive,
            &row,
            &revision,
            &row.correctness_cases,
        )
        .is_err()
    );
}

#[test]
fn efmu_archive_authentication_rejects_checksum_web_and_xml_root_mutation() {
    let temporary = tempfile::tempdir().unwrap();
    let row = efmu_entry();
    let BaselineMeasurement::Accepted(measured) = &row.measured else {
        unreachable!("efmu fixture always carries an accepted baseline");
    };
    let session = row.rumoca_artifact_history[0].artifact_session.clone();
    let valid = valid_efmu_members(&session);

    let mut broken_web = valid;
    let algorithm_sha1 = format!(
        "{:x}",
        sha1::Sha1::digest(
            broken_web
                .iter()
                .find(|(path, _)| path == "AlgorithmCode/model.alg")
                .unwrap()
                .1
                .as_slice()
        )
    );
    let manifest = broken_web
        .iter_mut()
        .find(|(path, _)| path == "AlgorithmCode/manifest.xml")
        .unwrap();
    let xml = String::from_utf8(manifest.1.clone()).unwrap();
    manifest.1 = xml
        .replacen(&algorithm_sha1, &"0".repeat(40), 1)
        .into_bytes();
    let broken_web_path = temporary.path().join("broken-web.efmu");
    write_efmu_archive(&broken_web_path, &broken_web);
    assert!(
        super::efmu_artifact::derive_revision(
            &broken_web_path,
            &row,
            measured.rumoca,
            "broken checksum web must fail",
            session,
            &row.correctness_cases,
        )
        .is_err()
    );

    let mut broken_reference = valid_efmu_members(&row.rumoca_artifact_history[0].artifact_session);
    let ac = broken_reference
        .iter()
        .find(|(path, _)| path == "AlgorithmCode/manifest.xml")
        .unwrap()
        .1
        .clone();
    let ac_sha1 = format!("{:x}", sha1::Sha1::digest(&ac));
    let manifest = broken_reference
        .iter_mut()
        .find(|(path, _)| path == "ProductionCode/manifest.xml")
        .unwrap();
    let xml = String::from_utf8(manifest.1.clone()).unwrap();
    manifest.1 = xml.replacen(&ac_sha1, &"1".repeat(40), 1).into_bytes();
    let broken_reference_path = temporary.path().join("broken-manifest-reference.efmu");
    write_efmu_archive(&broken_reference_path, &broken_reference);
    assert!(
        super::efmu_artifact::derive_revision(
            &broken_reference_path,
            &row,
            measured.rumoca,
            "broken AC to PC reference must fail",
            row.rumoca_artifact_history[0].artifact_session.clone(),
            &row.correctness_cases,
        )
        .is_err()
    );

    let mut wrapped_root = valid_efmu_members(&row.rumoca_artifact_history[0].artifact_session);
    let manifest = wrapped_root
        .iter_mut()
        .find(|(path, _)| path == "AlgorithmCode/manifest.xml")
        .unwrap();
    let xml = String::from_utf8(manifest.1.clone()).unwrap();
    manifest.1 = xml
        .replacen("<Manifest ", "<Envelope><Manifest ", 1)
        .replacen("</Manifest>", "</Manifest></Envelope>", 1)
        .into_bytes();
    let wrapped_root_path = temporary.path().join("wrapped-root.efmu");
    write_efmu_archive(&wrapped_root_path, &wrapped_root);
    assert!(
        super::efmu_artifact::derive_revision(
            &wrapped_root_path,
            &row,
            measured.rumoca,
            "nested fake manifest root must fail",
            row.rumoca_artifact_history[0].artifact_session.clone(),
            &row.correctness_cases,
        )
        .is_err()
    );
}

#[test]
fn efmu_archive_authentication_rejects_cohort_receipt_and_pin_mutation() {
    let temporary = tempfile::tempdir().unwrap();
    let (row, archive_path, revision) = derived_efmu_fixture(temporary.path());
    let mut wrong_cohort = row.correctness_cases.clone();
    wrong_cohort[0].expected_output_bits[0] = "deadbeef".into();
    assert!(
        super::efmu_artifact::authenticate_revision(&archive_path, &row, &revision, &wrong_cohort,)
            .is_err()
    );

    let mut changed_receipt = revision.clone();
    changed_receipt.oracle_success_receipt.receipt_sha256 = "f".repeat(64);
    assert!(
        super::efmu_artifact::authenticate_revision(
            &archive_path,
            &row,
            &changed_receipt,
            &row.correctness_cases,
        )
        .is_err()
    );

    let mut changed_member_pin = revision;
    changed_member_pin.archive_members[0].sha256 = "e".repeat(64);
    assert!(
        super::efmu_artifact::authenticate_revision(
            &archive_path,
            &row,
            &changed_member_pin,
            &row.correctness_cases,
        )
        .is_err()
    );
}

#[test]
fn efmu_artifact_history_wire_is_explicit_but_pending_row_must_keep_it_empty() {
    let valid = manifest(vec![efmu_entry()]);
    let mut missing = serde_json::to_value(&valid).unwrap();
    missing["entries"][0]
        .as_object_mut()
        .unwrap()
        .remove("rumoca_artifact_history");
    assert!(serde_json::from_value::<Manifest>(missing).is_err());

    let mut hidden_predecessor = serde_json::to_value(&valid).unwrap();
    hidden_predecessor["entries"][0]["rumoca_artifact_history"][0]["predecessor_sha256"] =
        serde_json::json!(digest());
    assert!(serde_json::from_value::<Manifest>(hidden_predecessor).is_err());

    let mut missing_session_seed = serde_json::to_value(&valid).unwrap();
    missing_session_seed["entries"][0]["rumoca_artifact_history"][0]["artifact_session"]
        .as_object_mut()
        .unwrap()
        .remove("identity_seed");
    assert!(serde_json::from_value::<Manifest>(missing_session_seed).is_err());

    let mut pending = pending_entry();
    pending.rumoca_artifact_history = efmu_entry().rumoca_artifact_history;
    let error = super::manifest::validate(&manifest(vec![pending])).unwrap_err();
    assert!(format!("{error:#}").contains("cannot carry artifact history"));
}

#[test]
fn efmu_artifact_revision_rejects_invalid_session_package_and_oracle_bindings() {
    let mut row = efmu_entry();
    row.rumoca_artifact_history[0]
        .artifact_session
        .generation_instant = "2026-02-29T12:34:56Z".into();
    assert!(super::manifest::validate(&manifest(vec![row])).is_err());

    let mut row = efmu_entry();
    row.rumoca_artifact_history[0]
        .artifact_session
        .identity_seed = "00000000-0000-0000-0000-00000000000A".into();
    assert!(super::manifest::validate(&manifest(vec![row])).is_err());

    let mut row = efmu_entry();
    row.rumoca_artifact_history[0].efmu_package_sha256 = "short".into();
    assert!(super::manifest::validate(&manifest(vec![row])).is_err());

    let mut row = efmu_entry();
    row.rumoca_artifact_history[0].rumoca_metric = 0;
    assert!(super::manifest::validate(&manifest(vec![row])).is_err());

    let mut row = efmu_entry();
    row.rumoca_artifact_history[0]
        .oracle_success_receipt
        .receipt_sha256 = "short".into();
    assert!(super::manifest::validate(&manifest(vec![row])).is_err());

    let mut row = efmu_entry();
    row.rumoca_artifact_history[0]
        .oracle_success_receipt
        .efmu_package_sha256 = "b".repeat(64);
    let error = super::manifest::validate(&manifest(vec![row])).unwrap_err();
    assert!(format!("{error:#}").contains("different eFMU package"));

    let mut row = efmu_entry();
    row.rumoca_artifact_history[0]
        .oracle_success_receipt
        .correctness_cases_sha256 = "c".repeat(64);
    let error = super::manifest::validate(&manifest(vec![row])).unwrap_err();
    assert!(format!("{error:#}").contains("different correctness cohort"));

    let mut row = efmu_entry();
    row.rumoca_artifact_history[0].rationale = " \n".into();
    assert!(super::manifest::validate(&manifest(vec![row])).is_err());
}

#[test]
fn efmu_artifact_revision_rejects_member_and_checksum_web_substitution() {
    let mut row = efmu_entry();
    row.rumoca_artifact_history[0].archive_members.pop();
    assert!(super::manifest::validate(&manifest(vec![row])).is_err());

    let mut row = efmu_entry();
    row.rumoca_artifact_history[0].archive_members.swap(0, 1);
    assert!(super::manifest::validate(&manifest(vec![row])).is_err());

    let mut row = efmu_entry();
    row.rumoca_artifact_history[0].archive_members[0].sha256 = "D".repeat(64);
    assert!(super::manifest::validate(&manifest(vec![row])).is_err());

    let mut row = efmu_entry();
    row.rumoca_artifact_history[0].production_c.archive_path =
        "ProductionCode/sources/../production.c".into();
    assert!(super::manifest::validate(&manifest(vec![row])).is_err());

    let mut row = efmu_entry();
    row.rumoca_artifact_history[0].production_h.archive_path =
        "ProductionCode/sources/production.c".into();
    assert!(super::manifest::validate(&manifest(vec![row])).is_err());

    let mut row = efmu_entry();
    row.rumoca_artifact_history[0].production_c.archive_path =
        "ProductionCode/sources/alternate.c".into();
    assert!(super::manifest::validate(&manifest(vec![row])).is_err());

    let mut row = efmu_entry();
    row.rumoca_artifact_history[0].production_c.sha256 = "D".repeat(64);
    assert!(super::manifest::validate(&manifest(vec![row])).is_err());

    let mut row = efmu_entry();
    row.rumoca_artifact_history[0].checksum_web_membership.pop();
    let error = super::manifest::validate(&manifest(vec![row])).unwrap_err();
    assert!(format!("{error:#}").contains("exact six"));

    let mut row = efmu_entry();
    row.rumoca_artifact_history[0]
        .checksum_web_membership
        .swap(0, 1);
    let error = super::manifest::validate(&manifest(vec![row])).unwrap_err();
    assert!(format!("{error:#}").contains("edge order or identity"));

    let mut row = efmu_entry();
    row.rumoca_artifact_history[0].checksum_web_membership[2].producer_sha1 = "e".repeat(39);
    assert!(super::manifest::validate(&manifest(vec![row])).is_err());
}

#[test]
fn successful_measurement_cannot_bypass_typed_row_evidence_state() {
    let row = entry();
    let BaselineMeasurement::Accepted(measured) = &row.measured else {
        unreachable!("entry fixture always carries an accepted baseline");
    };
    let verdict = judge(
        &row,
        instruction_evidence(measured.rumoca, measured.comparator),
        0.0,
    );
    assert_exact_pin(
        &verdict,
        row.max_delta.unwrap(),
        super::ComparisonOutcome::Loss,
    );
    let verdict_wire = serde_json::to_value(&verdict).unwrap();
    assert_eq!(verdict_wire["status"], "exact-pin");
    assert_eq!(verdict_wire["delta"], row.max_delta.unwrap());
    assert_eq!(verdict_wire["comparison_outcome"], "loss");
    assert_eq!(
        verdict_wire["evidence"]["suite_implementation_sha256"],
        super::suite_identity::sha256()
    );
    assert!(verdict_wire.get("partial_suite").is_none());
    assert!(verdict_wire.get("findings").is_none());
    let scope = super::SelectionScope::full(vec![row.id.clone()]).unwrap();
    let verdicts = [verdict];
    let checked = super::CheckedVerdicts::bind(&scope, &verdicts).unwrap();
    let wire = serde_json::to_value(super::summary_status(&checked)).unwrap();
    assert_eq!(wire["kind"], "exact-pin-nonclaimable");
    assert_eq!(
        wire["label"],
        "exact-pin nonclaimable: exp-mixed-efmu-production-c-casadi test-exact-pin loss"
    );
    assert!(wire.get("claimable").is_none());

    let source = include_str!("../embedded_head_to_head.rs");
    assert!(!source.contains("claimable: bool"));
    assert!(!source.contains("passed: bool"));
    assert!(!source.contains("dominance: bool"));
}

#[test]
fn every_exact_pin_has_a_loss_tie_or_win_outcome() {
    let loss = entry();
    let BaselineMeasurement::Accepted(loss_measurement) = &loss.measured else {
        unreachable!("entry fixture always carries an accepted baseline");
    };
    let verdict = judge(
        &loss,
        instruction_evidence(loss_measurement.rumoca, loss_measurement.comparator),
        0.0,
    );
    assert_exact_pin(
        &verdict,
        loss.max_delta.unwrap(),
        super::ComparisonOutcome::Loss,
    );

    let mut tie = entry();
    let BaselineMeasurement::Accepted(tie_measurement) = &mut tie.measured else {
        unreachable!("entry fixture always carries an accepted baseline");
    };
    tie_measurement.rumoca = tie_measurement.comparator;
    tie.max_delta = Some(0);
    let BaselineMeasurement::Accepted(tie_measurement) = &tie.measured else {
        unreachable!("entry fixture always carries an accepted baseline");
    };
    let verdict = judge(
        &tie,
        instruction_evidence(tie_measurement.rumoca, tie_measurement.comparator),
        0.0,
    );
    assert_exact_pin(&verdict, 0, super::ComparisonOutcome::Tie);

    let mut win = tie;
    let BaselineMeasurement::Accepted(win_measurement) = &mut win.measured else {
        unreachable!("entry fixture always carries an accepted baseline");
    };
    win_measurement.rumoca -= 1;
    win.max_delta = Some(-1);
    let BaselineMeasurement::Accepted(win_measurement) = &win.measured else {
        unreachable!("entry fixture always carries an accepted baseline");
    };
    let verdict = judge(
        &win,
        instruction_evidence(win_measurement.rumoca, win_measurement.comparator),
        0.0,
    );
    assert_exact_pin(&verdict, -1, super::ComparisonOutcome::Win);
}

#[test]
fn selection_scope_is_nonempty_explicit_and_duplicate_free() {
    let first = entry();
    let BaselineMeasurement::Accepted(first_measurement) = &first.measured else {
        unreachable!("entry fixture always carries an accepted baseline");
    };
    let mut second = entry();
    second.id = "second-row".into();
    let manifest = manifest(vec![first.clone(), second]);
    let selected = select(&manifest, std::slice::from_ref(&first.id)).unwrap();
    let scope = serde_json::to_value(selected.scope()).unwrap();
    assert_eq!(scope["kind"], "partial");
    assert_eq!(scope["selected_rows"], serde_json::json!([first.id]));
    assert_eq!(scope["omitted_rows"], serde_json::json!(["second-row"]));
    assert!(select(&manifest, &["second-row".into(), "second-row".into()]).is_err());
    assert!(super::SelectionScope::full(Vec::new()).is_err());
    let full = super::SelectionScope::full(vec![first.id.clone()]).unwrap();
    assert!(super::CheckedVerdicts::bind(&full, &[]).is_err());
    let mut wrong = judge(
        &first,
        instruction_evidence(first_measurement.rumoca, first_measurement.comparator),
        0.0,
    );
    wrong.id = "wrong-row".into();
    assert!(super::CheckedVerdicts::bind(&full, &[wrong]).is_err());
}

#[test]
fn measured_tool_hash_pins_fail_before_version_execution() {
    let temporary = tempfile::tempdir().unwrap();
    let path = temporary.path().join("qemu-system-arm");
    fs::write(&path, "measured qemu bytes").unwrap();
    let actual = super::sha256_file(&path).unwrap();
    let role =
        super::typed_path::RolePath::<super::typed_path::QemuExecutable>::checked(path).unwrap();
    assert!(
        super::typed_path::AuthenticatedExecutable::checked(role.clone(), &actual).is_ok(),
        "the reviewed digest must construct the retained executable capability"
    );
    let error = super::typed_path::AuthenticatedExecutable::checked(role, &"5".repeat(64))
        .err()
        .expect("the unreviewed executable digest must be rejected");
    assert!(error.to_string().contains("SHA-256 changed before use"));
}

#[test]
fn stale_success_is_invalidated_before_any_fallible_authentication() {
    let temporary = tempfile::tempdir().unwrap();
    let root = temporary.path();
    let summary = root.join(super::SUMMARY_PATH);
    let temporary_summary = root.join(super::SUMMARY_TEMP_PATH);
    let artifact = root.join(super::ARTIFACT_DIR).join("old/success.trace");
    fs::create_dir_all(summary.parent().unwrap()).unwrap();
    fs::create_dir_all(artifact.parent().unwrap()).unwrap();
    fs::write(&summary, "stale success").unwrap();
    fs::write(&temporary_summary, "stale temporary").unwrap();
    fs::write(&artifact, "stale trace").unwrap();

    let _lock = super::RunLock::acquire(root).unwrap();
    super::invalidate_previous_evidence(root).unwrap();
    assert!(super::manifest::load_authenticated(&root.join("missing.json")).is_err());
    assert!(!summary.exists());
    assert!(!temporary_summary.exists());
    assert!(!root.join(super::ARTIFACT_DIR).exists());
}

fn published_setup_failure(root: &Path) -> serde_json::Value {
    serde_json::from_slice(&fs::read(root.join(super::SUMMARY_PATH)).unwrap()).unwrap()
}

#[test]
fn authenticated_manifest_failure_publishes_nonclaimable_empty_setup_ledger() {
    let temporary = tempfile::tempdir().unwrap();
    fs::create_dir_all(temporary.path().join("target/verification")).unwrap();
    let ledger = super::SetupLedger::new();
    let error =
        super::manifest::load_authenticated(&temporary.path().join("missing.json")).unwrap_err();
    super::publish_setup_failure(temporary.path(), &ledger, &error).unwrap();
    let summary = published_setup_failure(temporary.path());
    assert_eq!(summary["schema_version"], super::manifest::SCHEMA_VERSION);
    assert_eq!(summary["status"]["kind"], "setup-failed");
    assert_eq!(summary["status"]["phase"], "authenticate-manifest");
    assert!(summary.get("claimable").is_none());
    assert_eq!(summary["commands"].as_array().unwrap().len(), 0);
    assert!(
        fs::metadata(temporary.path().join(super::SUMMARY_PATH))
            .unwrap()
            .permissions()
            .readonly()
    );
}

#[test]
fn successful_probe_then_version_mismatch_retains_exact_probe_receipt() {
    let temporary = tempfile::tempdir().unwrap();
    fs::create_dir_all(temporary.path().join("target/verification")).unwrap();
    let mut command = super::process::HermeticCommand::from_absolute_path(Path::new("/bin/sh"));
    command.args(["-c", "printf 'unexpected version\\n'"]);
    let (version, receipt) = super::version(command, "synthetic-tool").unwrap();
    let mut ledger = super::SetupLedger::new();
    ledger.phase = super::SetupPhase::AuthenticateMeasuredTools;
    ledger.commands.push(receipt);
    let error = anyhow::anyhow!("version `{version}` does not match reviewed pin");
    super::publish_setup_failure(temporary.path(), &ledger, &error).unwrap();
    let summary = published_setup_failure(temporary.path());
    assert_eq!(summary["status"]["kind"], "setup-failed");
    assert!(summary.get("claimable").is_none());
    assert_eq!(summary["commands"].as_array().unwrap().len(), 1);
    assert!(
        summary["commands"][0]["display_only"]
            .as_str()
            .unwrap()
            .contains("unexpected version")
    );
}

#[test]
fn compiler_build_nonzero_retains_the_failed_attempt_receipt() {
    let temporary = tempfile::tempdir().unwrap();
    fs::create_dir_all(temporary.path().join("target/verification")).unwrap();
    let mut command = super::process::HermeticCommand::from_absolute_path(Path::new("/bin/sh"));
    command.args(["-c", "exit 19"]);
    let error = super::process::require_success(
        &mut command,
        "synthetic compiler build",
        super::process::Limit::WorkspaceBuild,
    )
    .unwrap_err();
    let mut ledger = super::SetupLedger::new();
    ledger.phase = super::SetupPhase::BuildCompiler;
    ledger.absorb_attempted(&error);
    super::publish_setup_failure(temporary.path(), &ledger, &error).unwrap();
    let summary = published_setup_failure(temporary.path());
    assert_eq!(summary["status"]["phase"], "build-compiler");
    assert_eq!(summary["commands"].as_array().unwrap().len(), 1);
    assert!(
        summary["commands"][0]["display_only"]
            .as_str()
            .unwrap()
            .contains("exit 19")
    );
}

#[test]
fn failed_row_serializes_typed_partial_ledger_without_completion_or_measurements() {
    let temporary = tempfile::tempdir().unwrap();
    let row = entry();
    let root = fake_execution_root(temporary.path(), &row);
    let plan = super::cross::bind_execution_plan(&root, &row).unwrap();
    let cursor = super::cross::start_suite(&plan);
    let pending = match cursor.next() {
        super::cross::SuiteProgress::Pending(pending) => pending,
        super::cross::SuiteProgress::Complete(_) => panic!("closed suite unexpectedly empty"),
    };
    let (_cursor, executed, ()) =
        super::cross::execute_suite_step(pending, |_| Ok::<(), anyhow::Error>(())).unwrap();
    let receipt = super::process::test_receipt("first successful command");
    let mut failed = super::process::HermeticCommand::from_absolute_path(Path::new("/bin/sh"));
    failed.args(["-c", "exit 7"]);
    let error = super::process::require_success(
        &mut failed,
        "synthetic later suite command",
        super::process::Limit::Probe,
    )
    .unwrap_err();
    let verdict = super::Verdict::unmeasured(
        &row,
        super::SuiteFailure::from_partial(
            error,
            super::PartialSuiteEvidence {
                completed_steps: vec![executed],
                commands: vec![receipt],
            },
        ),
        0.0,
    );
    let serialized = serde_json::to_value(verdict).unwrap();
    assert_eq!(serialized["status"], "incomplete");
    assert!(serialized.get("delta").is_none());
    assert!(serialized.get("comparison_outcome").is_none());
    assert!(serialized.get("evidence").is_none());
    let partial = serialized["partial_suite"]
        .as_object()
        .expect("failed verdict must retain a typed partial suite ledger");
    assert_eq!(partial["completed_steps"][0]["ordinal"], 0);
    assert_eq!(partial["completed_steps"][0]["id"], "stage-inputs");
    assert_eq!(partial["completed_steps"][0]["executor"], "stage_inputs");
    assert_eq!(partial["commands"].as_array().unwrap().len(), 2);
    assert!(
        partial["commands"][1]["display_only"]
            .as_str()
            .unwrap()
            .contains("/bin/sh -c exit 7")
    );
    let wire = serde_json::to_string(&serialized).unwrap();
    assert!(!wire.contains("completed_suite_sha256"));
    assert!(!wire.contains("rumoca_instructions"));
    assert!(!wire.contains("comparator_instructions"));
}

#[test]
fn no_fp_contract_profile_is_bound_to_the_sealed_build_plan() {
    const PRE_ORDERED_SUITE_DIGEST: &str =
        "bf212852ae80ed578966729739adeb5ff8cd89eef4ff81bccc0194f13a077afa";
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let manifest = super::manifest::load(&super::manifest::path(&root)).unwrap();
    let row = &manifest.entries[0];
    assert_eq!(
        row.cross_profile,
        CrossProfile::Gcc15CortexM7O3LtoNoFpContractHotAuto700Single700ExactV3
    );
    assert_eq!(row.target.as_str(), "efmu");
    assert!(row.generation_bindings().is_ok());
    assert_eq!(
        row.normalized_profile_sha256,
        super::cross::normalized_profile_sha256(row),
        "the manifest must seal the exact normalized schema-9 execution profile"
    );
    assert_ne!(
        row.normalized_profile_sha256, PRE_ORDERED_SUITE_DIGEST,
        "the digest must move when the closed validation-suite sequence enters its identity"
    );
    assert!(row.measured.accepted().is_none());
    assert!(row.max_delta.is_none());
    assert!(row.rumoca_artifact_history.is_empty());
}

#[test]
fn one_authenticated_root_binds_heterogeneous_row_profiles_independently() {
    let temporary = tempfile::tempdir().unwrap();
    let row = entry();
    let root = fake_execution_root(temporary.path(), &row);
    let first = super::cross::bind_execution_plan(&root, &row);
    let alternate = CrossProfile::TestAlternateNoFpContract;
    let mut alternate_row = row.clone();
    alternate_row.cross_profile = alternate;
    alternate_row.normalized_profile_sha256 =
        super::cross::normalized_profile_sha256(&alternate_row);
    let second = super::cross::bind_execution_plan(&root, &alternate_row);
    assert!(first.is_ok());
    assert!(second.is_ok());
}

#[test]
fn metric_change_moves_the_digest_and_binds_a_distinct_measurement_plan() {
    let temporary = tempfile::tempdir().unwrap();
    let row = entry();
    let root = fake_execution_root(temporary.path(), &row);
    let original_digest = row.normalized_profile_sha256.clone();
    let mut alternate = row;
    alternate.metric = Metric::TestAlternate;
    alternate.normalized_profile_sha256 = super::cross::normalized_profile_sha256(&alternate);
    assert_ne!(alternate.normalized_profile_sha256, original_digest);
    let plan = super::cross::bind_execution_plan(&root, &alternate).unwrap();
    assert!(plan.metric_is_test_alternate());
}

#[test]
fn command_api_has_no_raw_profile_digest_revalidation_path() {
    let facade = include_str!("cross.rs");
    let plan = include_str!("cross/plan.rs");
    let generation = include_str!("cross/plan/generation.rs");
    for forbidden in [
        "validate_profile_digest",
        "expected_profile_digest",
        "ExecutionTools::verified",
    ] {
        assert!(!facade.contains(forbidden), "facade contains {forbidden}");
        assert!(!plan.contains(forbidden), "plan contains {forbidden}");
        assert!(
            !generation.contains(forbidden),
            "generation plan contains {forbidden}"
        );
    }
    assert!(!include_str!("emit.rs").contains("manifest::Entry"));
}

#[test]
fn manifest_rejects_every_artifact_path_escape() {
    for id in [
        "../../escape",
        "/tmp/escape",
        ".",
        "..",
        "a/b",
        "UPPER",
        "-edge",
    ] {
        let mut row = entry();
        row.id = id.into();
        let manifest = manifest(vec![row]);
        assert!(
            super::manifest::validate(&manifest).is_err(),
            "unsafe id `{id}` must never reach artifact_root.join(id)"
        );
    }
}

#[test]
fn fixture_tree_tracks_authored_sources_and_ignores_only_generated_casadi_outputs() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let fixture = root.join("infra/verification/embedded-head-to-head/exp-mixed");
    let mut files = Vec::new();
    fn collect(root: &Path, directory: &Path, files: &mut Vec<String>) {
        for entry in fs::read_dir(directory).unwrap() {
            let path = entry.unwrap().path();
            if path.is_dir() {
                collect(root, &path, files);
            } else {
                files.push(
                    path.strip_prefix(root)
                        .unwrap()
                        .to_string_lossy()
                        .into_owned(),
                );
            }
        }
    }
    collect(&fixture, &fixture, &mut files);
    files.sort();
    assert_eq!(
        files,
        [
            "ExpMixedStep.mo",
            "casadi/.gitignore",
            "casadi/float_libm_wrapper.c",
            "casadi/gen_exp_mixed.py",
            "harness/counter_fixtures.c",
            "harness/driver_casadi.c",
            "harness/driver_casadi_correctness.c",
            "harness/driver_rumoca.c",
            "harness/driver_rumoca_correctness.c",
            "harness/inputs.h",
            "harness/linker.ld",
            "harness/startup.S",
            "harness/trace_markers.h",
            "harness/trace_output.c",
            "harness/trace_output.h",
        ]
    );
    assert_eq!(
        fs::read_to_string(fixture.join("casadi/.gitignore")).unwrap(),
        "casadi_exp_mixed.c\ncasadi_exp_mixed.h\n"
    );
    let fixture_relative = "infra/verification/embedded-head-to-head/exp-mixed";
    let tracked = git(&root, &["ls-files", "-z", "--", fixture_relative]);
    let tracked = tracked
        .split('\0')
        .filter(|path| !path.is_empty())
        .map(|path| {
            path.strip_prefix(&format!("{fixture_relative}/"))
                .unwrap()
                .to_owned()
        })
        .collect::<Vec<_>>();
    assert_eq!(
        tracked, files,
        "every authored benchmark fixture must be in the Git index"
    );
    for generated in ["casadi_exp_mixed.c", "casadi_exp_mixed.h"] {
        let status = Command::new("git")
            .arg("-C")
            .arg(&root)
            .args(["check-ignore", "--no-index", "--quiet", "--"])
            .arg(fixture.join("casadi").join(generated))
            .status()
            .unwrap();
        assert!(
            status.success(),
            "generated CasADi output must remain ignored"
        );
    }
}

#[test]
fn checked_manifest_pins_every_source_input() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let manifest = super::manifest::load(&super::manifest::path(&root)).unwrap();
    assert_eq!(
        manifest.entries.len(),
        1,
        "a new comparator must be explicit"
    );
    let temporary = tempfile::tempdir().unwrap();
    let inputs = super::snapshot::stage(
        &root,
        &manifest.entries[0],
        &temporary.path().join("authenticated-inputs"),
    )
    .unwrap();
    assert!(
        inputs
            .staged_paths()
            .into_iter()
            .all(|path| path.starts_with(inputs.root()))
    );
}

#[test]
fn measured_elf_source_is_structurally_isolated_from_the_correctness_cohort() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let harness = root.join("infra/verification/embedded-head-to-head/exp-mixed/harness");
    for column in ["rumoca", "casadi"] {
        let measured = fs::read_to_string(harness.join(format!("driver_{column}.c"))).unwrap();
        let correctness =
            fs::read_to_string(harness.join(format!("driver_{column}_correctness.c"))).unwrap();
        assert_eq!(measured.matches("MEASURED_CALL").count(), 1);
        assert_eq!(
            measured
                .matches("int __attribute__((hot)) main(void)")
                .count(),
            1
        );
        assert!(!measured.contains("COHORT_CALL"));
        assert!(!measured.contains("CORRECTNESS_CASE_COUNT"));
        assert_eq!(correctness.matches("MEASURED_CALL").count(), 1);
        assert_eq!(correctness.matches("COHORT_CALL").count(), 1);
        assert!(!correctness.contains("__attribute__((hot))"));
        assert!(correctness.contains("CORRECTNESS_CASE_COUNT"));
    }
}

#[test]
fn casadi_drivers_use_only_the_pinned_zero_workspace_contract() {
    let root = repository_root();
    let harness = root.join("infra/verification/embedded-head-to-head/exp-mixed/harness");
    for driver in ["driver_casadi.c", "driver_casadi_correctness.c"] {
        let source = fs::read_to_string(harness.join(driver)).unwrap();
        assert!(!source.contains("workspace["));
        assert!(!source.contains("integer_workspace["));
        assert!(!source.contains("512"));
        assert!(!source.contains("16"));
    }

    let temporary = tempfile::tempdir().unwrap();
    let generated_c = temporary.path().join("casadi_exp_mixed.c");
    let generated_h = temporary.path().join("casadi_exp_mixed.h");
    let source = [
        "  if (sz_arg) *sz_arg = 4;",
        "  if (sz_res) *sz_res = 1;",
        "  if (sz_iw) *sz_iw = 0;",
        "  if (sz_w) *sz_w = 0;",
        "  if (sz_arg) *sz_arg = 4*sizeof(const casadi_real*);",
        "  if (sz_res) *sz_res = 1*sizeof(casadi_real*);",
        "  if (sz_iw) *sz_iw = 0*sizeof(casadi_int);",
        "  if (sz_w) *sz_w = 0*sizeof(casadi_real);",
    ]
    .join("\n");
    let header = [
        "#define exp_mixed_full_SZ_ARG 4",
        "#define exp_mixed_full_SZ_RES 1",
        "#define exp_mixed_full_SZ_IW 0",
        "#define exp_mixed_full_SZ_W 0",
    ]
    .join("\n");
    fs::write(&generated_c, &source).unwrap();
    fs::write(&generated_h, &header).unwrap();
    assert!(emit::validate_casadi_zero_workspace_contract(&generated_c, &generated_h).is_ok());
    fs::write(&generated_h, header.replace("SZ_W 0", "SZ_W 1")).unwrap();
    assert!(emit::validate_casadi_zero_workspace_contract(&generated_c, &generated_h).is_err());
}

#[test]
fn compiler_build_ignores_redirected_target_without_destructive_cleanup() {
    let temporary = tempfile::tempdir().unwrap();
    let stale = temporary.path().join("target/debug/rumoca");
    fs::create_dir_all(stale.parent().unwrap()).unwrap();
    fs::write(&stale, "stale compiler").unwrap();
    let redirected = temporary.path().join("redirected-by-parent");

    let artifacts = temporary
        .path()
        .join("target/verification/embedded-head-to-head");
    fs::create_dir_all(&artifacts).unwrap();
    let source = artifacts.join("compiler-source");
    let vendor = artifacts.join("compiler-vendor");
    let bin = artifacts.join("toolchain/bin");
    fs::create_dir_all(&source).unwrap();
    fs::create_dir_all(&vendor).unwrap();
    fs::create_dir_all(&bin).unwrap();
    let cargo = super::typed_path::RolePath::<super::typed_path::CargoExecutable>::checked(
        bin.join("cargo"),
    )
    .unwrap();
    let rustc = super::typed_path::RolePath::<super::typed_path::RustcExecutable>::checked(
        bin.join("rustc"),
    )
    .unwrap();
    let (command, compiler) = super::prepare_compiler_build(super::CompilerBuildPreparation {
        source_root: &source,
        vendor: &vendor,
        cargo: &cargo,
        rustc: &rustc,
        nix_path: std::ffi::OsStr::new("/nix/store/fake/bin"),
        artifact_dir: &artifacts,
    })
    .unwrap();
    assert_eq!(
        compiler,
        artifacts.join("compiler-build-target/x86_64-unknown-linux-gnu/debug/rumoca")
    );
    assert!(
        stale.exists(),
        "pre-build preparation must not remove the developer compiler facade"
    );
    assert!(!compiler.exists(), "fresh compiler target must start empty");
    let arguments = command
        .arguments()
        .map(|argument| argument.to_string_lossy().into_owned())
        .collect::<Vec<_>>();
    assert!(
        arguments.windows(2).any(|pair| {
            pair[0] == "--target-dir"
                && pair[1]
                    == artifacts
                        .join("compiler-build-target")
                        .display()
                        .to_string()
        }),
        "Cargo and the selected executable must share one closed target directory"
    );
    assert!(
        arguments.iter().any(|argument| argument == "--frozen")
            && arguments.iter().any(|argument| argument == "--offline")
            && arguments
                .iter()
                .any(|argument| argument == "--no-default-features"),
        "compiler build must bind frozen offline dependencies and the minimal feature surface"
    );
    assert!(arguments.windows(2).any(|pair| {
        pair[0] == "--features" && pair[1].split(',').any(|feature| feature == "fmu-packaging")
    }));
    assert!(
        arguments
            .iter()
            .any(|argument| { argument == "source.crates-io.replace-with=\"vendored-sources\"" })
    );
    assert!(arguments.iter().any(|argument| {
        argument == &format!("source.vendored-sources.directory=\"{}\"", vendor.display())
    }));
    assert_eq!(command.current_directory(), Some(source.as_path()));
    assert!(arguments.windows(2).any(|pair| {
        pair[0] == "--manifest-path" && pair[1] == source.join("Cargo.toml").display().to_string()
    }));
    assert!(
        arguments.windows(2).any(|pair| {
            pair[0] == "--target" && pair[1] == super::process::COMPILER_HOST_TARGET
        })
    );
    assert_compiler_command_environment(
        &command,
        &cargo,
        &rustc,
        &artifacts,
        &compiler,
        &redirected,
    );
}

fn assert_compiler_command_environment(
    command: &super::process::CompilerBuildCommand,
    cargo: &super::typed_path::RolePath<super::typed_path::CargoExecutable>,
    rustc: &super::typed_path::RolePath<super::typed_path::RustcExecutable>,
    artifacts: &Path,
    compiler: &Path,
    redirected: &Path,
) {
    let source = command.current_directory().unwrap();
    let metadata_temporary = artifacts.join("metadata-tmp");
    let metadata_cargo_home = artifacts.join("metadata-cargo-home");
    let metadata =
        super::process::CompilerMetadataCommand::rumoca(super::process::CompilerMetadataInputs {
            cargo,
            rustc,
            source_root: source,
            temporary_dir: &metadata_temporary,
            cargo_home: &metadata_cargo_home,
            nix_path: std::ffi::OsStr::new("/nix/store/fake/bin"),
            filter_platform: Some(super::process::COMPILER_HOST_TARGET),
        })
        .unwrap();
    let metadata_arguments = metadata
        .arguments()
        .map(|argument| argument.to_string_lossy().into_owned())
        .collect::<Vec<_>>();
    assert!(metadata_arguments.windows(2).any(|pair| {
        pair[0] == "--filter-platform" && pair[1] == super::process::COMPILER_HOST_TARGET
    }));
    assert!(metadata_arguments.windows(2).any(|pair| {
        pair[0] == "--features"
            && pair[1]
                .split(',')
                .any(|feature| feature == "rumoca/fmu-packaging")
    }));
    let environment = command
        .environments()
        .map(|(name, value)| {
            (
                name.to_string_lossy().into_owned(),
                value.map(|value| value.to_string_lossy().into_owned()),
            )
        })
        .collect::<std::collections::BTreeMap<_, _>>();
    assert_eq!(
        environment
            .keys()
            .cloned()
            .collect::<std::collections::BTreeSet<_>>(),
        std::collections::BTreeSet::from([
            "CARGO_BUILD_JOBS".into(),
            "CARGO_HOME".into(),
            "CARGO_INCREMENTAL".into(),
            "CARGO_NET_OFFLINE".into(),
            "PATH".into(),
            "RAYON_NUM_THREADS".into(),
            "RUSTC".into(),
            "TMPDIR".into(),
        ])
    );
    assert_eq!(
        environment["CARGO_HOME"].as_deref(),
        Some(
            artifacts
                .join("compiler-build-cargo-home")
                .to_str()
                .unwrap()
        )
    );
    assert_eq!(environment["CARGO_NET_OFFLINE"].as_deref(), Some("true"));
    assert_ne!(compiler, redirected.join("debug/rumoca"));
}

mod measurement_contract_cases;
use measurement_contract_cases::{fake_execution_root, git, repository_root};
