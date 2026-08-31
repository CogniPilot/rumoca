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
    row.rumoca_artifact_history = vec![artifact_revision(&row, row.measured.rumoca, '1')];
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
    let archive_path = directory.join("valid.efmu");
    let session = row.rumoca_artifact_history[0].artifact_session.clone();
    write_efmu_archive(&archive_path, &valid_efmu_members(&session));
    let revision = super::efmu_artifact::derive_revision(
        &archive_path,
        &row,
        row.measured.rumoca,
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
    let session = row.rumoca_artifact_history[0].artifact_session.clone();
    write_efmu_archive(&archive_path, &valid_efmu_members(&session));
    let revision = super::efmu_artifact::derive_revision(
        &archive_path,
        &row,
        row.measured.rumoca,
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
    let session = row.rumoca_artifact_history[0].artifact_session.clone();
    let valid = valid_efmu_members(&session);

    let missing_path = temporary.path().join("missing.efmu");
    write_efmu_archive(&missing_path, &valid[..valid.len() - 1]);
    assert!(
        super::efmu_artifact::derive_revision(
            &missing_path,
            &row,
            row.measured.rumoca,
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
            row.measured.rumoca,
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
            row.measured.rumoca,
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
        row.measured.rumoca,
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
            row.measured.rumoca,
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
            row.measured.rumoca,
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
            row.measured.rumoca,
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
    let verdict = judge(
        &row,
        instruction_evidence(row.measured.rumoca, row.measured.comparator),
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
    let verdict = judge(
        &loss,
        instruction_evidence(loss.measured.rumoca, loss.measured.comparator),
        0.0,
    );
    assert_exact_pin(
        &verdict,
        loss.max_delta.unwrap(),
        super::ComparisonOutcome::Loss,
    );

    let mut tie = entry();
    tie.measured.rumoca = tie.measured.comparator;
    tie.max_delta = Some(0);
    let verdict = judge(
        &tie,
        instruction_evidence(tie.measured.rumoca, tie.measured.comparator),
        0.0,
    );
    assert_exact_pin(&verdict, 0, super::ComparisonOutcome::Tie);

    let mut win = tie;
    win.measured.rumoca -= 1;
    win.max_delta = Some(-1);
    let verdict = judge(
        &win,
        instruction_evidence(win.measured.rumoca, win.measured.comparator),
        0.0,
    );
    assert_exact_pin(&verdict, -1, super::ComparisonOutcome::Win);
}

#[test]
fn selection_scope_is_nonempty_explicit_and_duplicate_free() {
    let first = entry();
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
        instruction_evidence(first.measured.rumoca, first.measured.comparator),
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

#[cfg(unix)]
#[test]
fn relative_and_symlinked_executables_retain_role_basenames_as_absolute_paths() {
    use std::os::unix::fs::symlink;

    let temporary = tempfile::tempdir().unwrap();
    let bin = temporary.path().join("bin");
    fs::create_dir_all(&bin).unwrap();
    symlink("/bin/sh", bin.join("python")).unwrap();
    let absolute = super::absolute_from(temporary.path(), Path::new("bin/python"));
    assert!(absolute.is_absolute());
    assert!(absolute.is_file());
    let typed = super::typed_path::RolePath::<super::typed_path::PythonExecutable>::checked(
        absolute.clone(),
    )
    .unwrap();
    assert_eq!(typed.as_path(), absolute);
    assert_ne!(fs::canonicalize(typed.as_path()).unwrap(), typed.as_path());
}

#[test]
fn staged_compiler_and_pre_execution_hash_survive_source_mutation() {
    let temporary = tempfile::tempdir().unwrap();
    let shared = temporary.path().join("target/debug/rumoca");
    let artifacts = temporary.path().join("target/verification/gate");
    fs::create_dir_all(shared.parent().unwrap()).unwrap();
    fs::write(&shared, "compiler bytes selected after successful build").unwrap();

    let closure = super::tool_closure::AuthenticatedToolClosure::for_test(temporary.path());
    let staged = super::cross::stage_compiler(&shared, &artifacts, &closure).unwrap();
    let selected_hash = staged.sha256().to_string();
    fs::write(&shared, "concurrent in-place cargo mutation").unwrap();

    assert_eq!(
        fs::read_to_string(staged.path().as_path()).unwrap(),
        "compiler bytes selected after successful build"
    );
    assert_eq!(
        super::sha256_file(staged.path().as_path()).unwrap(),
        selected_hash
    );
    assert_eq!(staged.sha256(), selected_hash);
    assert_ne!(super::sha256_file(&shared).unwrap(), selected_hash);
}

#[cfg(unix)]
#[test]
fn staged_compiler_mutation_is_rejected_at_the_execution_boundary() {
    use std::os::unix::fs::PermissionsExt;

    let temporary = tempfile::tempdir().unwrap();
    let row = entry();
    let root = fake_execution_root(temporary.path(), &row);
    let plan = super::cross::bind_execution_plan(&root, &row).unwrap();
    let compiler = root.compiler_path().as_path();
    let mut permissions = fs::metadata(compiler).unwrap().permissions();
    permissions.set_mode(0o755);
    fs::set_permissions(compiler, permissions).unwrap();
    fs::write(compiler, "mutated staged compiler").unwrap();

    let error = super::cross::rumoca_generation_command(
        &plan,
        &super::typed_path::RolePath::normalized(),
        &super::typed_path::RolePath::normalized(),
        &super::typed_path::RolePath::normalized(),
    )
    .err()
    .expect("changed staged compiler must fail before command construction");
    assert!(
        error
            .to_string()
            .contains("digest changed before execution")
    );
}

#[test]
fn setup_receipts_preserve_git_probe_and_build_execution_order() {
    let baseline = [
        "01-git-version",
        "02-rev-parse",
        "03-ls-tree",
        "04-cat-file",
    ]
    .map(super::process::test_receipt);
    let probes =
        ["05-gcc", "06-nm", "07-qemu", "08-python", "09-prlimit"].map(super::process::test_receipt);
    let compiler_source = [
        "10-source-head-pre",
        "11-source-roster-pre",
        "12-source-deleted-pre",
        "13-source-status-pre",
        "14-source-head-post",
        "15-source-roster-post",
        "16-source-deleted-post",
        "17-source-status-post",
    ]
    .map(super::process::test_receipt);
    let mut ledger = super::SetupLedger::new();
    ledger.commands.extend(baseline);
    ledger.commands.extend(probes);
    ledger.commands.extend(compiler_source);
    ledger
        .commands
        .push(super::process::test_receipt("18-compiler-build"));
    let commands = ledger.commands;
    assert_eq!(
        commands
            .iter()
            .map(super::process::CommandReceipt::display_only)
            .collect::<Vec<_>>(),
        [
            "01-git-version",
            "02-rev-parse",
            "03-ls-tree",
            "04-cat-file",
            "05-gcc",
            "06-nm",
            "07-qemu",
            "08-python",
            "09-prlimit",
            "10-source-head-pre",
            "11-source-roster-pre",
            "12-source-deleted-pre",
            "13-source-status-pre",
            "14-source-head-post",
            "15-source-roster-post",
            "16-source-deleted-post",
            "17-source-status-post",
            "18-compiler-build",
        ]
    );
}

#[test]
fn setup_evidence_retains_mandatory_authenticated_git_and_source_identity() {
    fn setup(git_tool: super::GitToolEvidence) -> super::SetupEvidence {
        super::SetupEvidence {
            commands: Vec::new(),
            versions: super::ToolVersions {
                arm_gcc: "gcc".into(),
                arm_nm: "nm".into(),
                qemu: "qemu".into(),
                python: "python".into(),
                prlimit: "prlimit".into(),
            },
            tool_sha256: super::ToolHashes {
                arm_gcc: "1".repeat(64),
                arm_nm: "2".repeat(64),
                qemu: "3".repeat(64),
                python: "4".repeat(64),
                prlimit: "5".repeat(64),
            },
            git_tool,
            compiler_source: super::compiler_source::CompilerSourceEvidence {
                head_commit: "a".repeat(40),
                workspace_dirty: true,
                workspace_status_sha256: "9".repeat(64),
                closure_sha256: "a".repeat(64),
                roster_count: 42,
            },
            compiler_dependencies: super::compiler_deps::DependencyEvidence {
                closure_sha256: "c".repeat(64),
                registry_packages: 7,
                full_registry_packages: 6,
                resolution_manifest_packages: 1,
                local_packages: 3,
                build_dependency_edges: 2,
                proc_macro_packages: 1,
                manifest_artifact: super::artifact_bundle::FileEvidence {
                    relative_path: "evidence-bundle/setup/compiler-dependencies.json".into(),
                    sha256: "d".repeat(64),
                    bytes: 456,
                },
            },
            compiler_toolchain: super::CompilerToolchainEvidence {
                cargo_version: "cargo reviewed".into(),
                cargo_sha256: "e".repeat(64),
                rustc_version: "rustc reviewed".into(),
                rustc_sha256: "f".repeat(64),
                host_target: super::process::COMPILER_HOST_TARGET.into(),
                rust_sysroot: "/nix/store/rust".into(),
                rust_sysroot_tree_sha256: "0".repeat(64),
                nix_path_sha256: "1".repeat(64),
                nix_store_input_roots: vec!["/nix/store/reviewed-toolchain".into()],
                resolver_cargo_home: "/source/cargo/home".into(),
            },
            tool_closure: super::tool_closure::ToolClosureEvidence {
                sha256: "8".repeat(64),
                roots: vec![super::tool_closure::ToolClosureRootEvidence {
                    store_root: "/nix/store/test".into(),
                    nar_hash: "sha256:test".into(),
                }],
            },
            compiler_build_environment_sha256: "b".repeat(64),
            compiler_sha256: "6".repeat(64),
            compiler_artifact: super::artifact_bundle::FileEvidence {
                relative_path: "evidence-bundle/setup/compiler/rumoca".into(),
                sha256: "6".repeat(64),
                bytes: 123,
            },
            compiler_runtime_environment_sha256: "7".repeat(64),
            suite_implementation_sha256: "2".repeat(64),
            suite_implementation_history_sha256: "3".repeat(64),
        }
    }

    let enabled = serde_json::to_value(setup(super::GitToolEvidence {
        version: "git version reviewed".into(),
        sha256: "8".repeat(64),
    }))
    .unwrap();
    assert_eq!(enabled["git_tool"]["version"], "git version reviewed");
    assert_eq!(enabled["git_tool"]["sha256"], "8".repeat(64));
    assert_eq!(enabled["compiler_source"]["roster_count"], 42);
    assert_eq!(enabled["compiler_source"]["workspace_dirty"], true);
    assert_eq!(enabled["compiler_build_environment_sha256"], "b".repeat(64));
    assert_eq!(enabled["suite_implementation_sha256"], "2".repeat(64));
    assert_eq!(
        enabled["suite_implementation_history_sha256"],
        "3".repeat(64)
    );
}

#[test]
fn checked_input_header_must_be_the_header_resolved_by_the_drivers() {
    let temporary = tempfile::tempdir().unwrap();
    let mut row = entry();
    row.fixture = "case/fixture.mo".into();
    row.input_header = "authenticated-but-unused/inputs.h".into();
    let error = super::snapshot::stage(temporary.path(), &row, &temporary.path().join("snapshot"))
        .err()
        .expect("an authenticated but uncompiled header must be rejected");
    assert!(format!("{error:#}").contains("is not the compiled driver header"));
}

#[test]
fn authenticated_snapshot_is_immune_to_later_workspace_mutation() {
    let root = repository_root();
    let row = real_entry(&root);
    let temporary = tempfile::tempdir().unwrap();
    let workspace = temporary.path().join("workspace");
    copy_checked_workspace(&root, &row, &workspace);
    let inputs = super::snapshot::stage(
        &workspace,
        &row,
        &temporary.path().join("gate/authenticated-inputs"),
    )
    .unwrap();
    let before = inputs
        .staged_paths()
        .into_iter()
        .map(|path| (path.to_path_buf(), fs::read(path).unwrap()))
        .collect::<Vec<_>>();

    for relative in [
        &row.fixture,
        &row.comparator_generator,
        &row.comparator_wrapper,
        &row.input_header,
    ] {
        fs::write(workspace.join(relative), b"mutated after snapshot\n").unwrap();
    }
    let harness = workspace
        .join(&row.fixture)
        .parent()
        .unwrap()
        .join("harness");
    fs::write(
        harness.join("startup.S"),
        b"mutated harness after snapshot\n",
    )
    .unwrap();

    for (path, bytes) in before {
        assert!(path.starts_with(inputs.root()));
        assert_eq!(fs::read(path).unwrap(), bytes);
    }
    assert!(inputs.fixture().as_path().starts_with(inputs.root()));
    assert!(inputs.generator().as_path().starts_with(inputs.root()));
    assert!(inputs.wrapper().as_path().starts_with(inputs.root()));

    assert_generation_bindings_use_snapshot(temporary.path(), &workspace, &row, &inputs);
    assert_build_bindings_use_snapshot(temporary.path(), &workspace, &row, &inputs);
}

#[test]
fn authenticated_snapshot_rejects_extra_and_missing_harness_members() {
    let root = repository_root();
    let row = real_entry(&root);
    for mutation in ["extra", "missing"] {
        let temporary = tempfile::tempdir().unwrap();
        let workspace = temporary.path().join("workspace");
        copy_checked_workspace(&root, &row, &workspace);
        let harness = workspace
            .join(&row.fixture)
            .parent()
            .unwrap()
            .join("harness");
        if mutation == "extra" {
            fs::write(harness.join("unreviewed.c"), "unexpected\n").unwrap();
        } else {
            fs::remove_file(harness.join("startup.S")).unwrap();
        }
        let error = super::snapshot::stage(&workspace, &row, &temporary.path().join("snapshot"))
            .err()
            .expect("a non-exact harness roster must fail closed");
        assert!(format!("{error:#}").contains("harness roster"));
    }
}

#[cfg(unix)]
#[test]
fn adjacent_stale_cannot_override_fresh_casadi_emission() {
    use std::os::unix::fs::PermissionsExt;

    let temporary = tempfile::tempdir().unwrap();
    let output = temporary.path().join("emitted/casadi");
    let fresh_c = [
        "  if (sz_arg) *sz_arg = 4;",
        "  if (sz_res) *sz_res = 1;",
        "  if (sz_iw) *sz_iw = 0;",
        "  if (sz_w) *sz_w = 0;",
        "  if (sz_arg) *sz_arg = 4*sizeof(const casadi_real*);",
        "  if (sz_res) *sz_res = 1*sizeof(casadi_real*);",
        "  if (sz_iw) *sz_iw = 0*sizeof(casadi_int);",
        "  if (sz_w) *sz_w = 0*sizeof(casadi_real);",
    ]
    .join("\n")
        + "\n";
    let fresh_h = [
        "#define exp_mixed_full_SZ_ARG 4",
        "#define exp_mixed_full_SZ_RES 1",
        "#define exp_mixed_full_SZ_IW 0",
        "#define exp_mixed_full_SZ_W 0",
    ]
    .join("\n")
        + "\n";
    let c_arguments = fresh_c
        .lines()
        .map(|line| format!("'{line}'"))
        .collect::<Vec<_>>()
        .join(" ");
    let h_arguments = fresh_h
        .lines()
        .map(|line| format!("'{line}'"))
        .collect::<Vec<_>>()
        .join(" ");
    let generator = format!(
        "while [ \"$1\" != \"--out\" ]; do shift; done\nshift\nout=$1\nprintf '%s\\n' {c_arguments} > \"$out/casadi_exp_mixed.c\"\nprintf '%s\\n' {h_arguments} > \"$out/casadi_exp_mixed.h\"\n"
    );
    let wrapper = "#include \"casadi_exp_mixed.c\"\n";
    let workspace = temporary.path().join("workspace");
    let root = repository_root();
    let mut row = real_entry(&root);
    copy_checked_workspace(&root, &row, &workspace);
    let fixture_root = workspace.join(&row.fixture).parent().unwrap().to_path_buf();
    fs::write(workspace.join(&row.comparator_generator), &generator).unwrap();
    fs::write(workspace.join(&row.comparator_wrapper), wrapper).unwrap();
    fs::write(
        fixture_root.join("casadi/casadi_exp_mixed.c"),
        "stale adjacent c\n",
    )
    .unwrap();
    row.comparator_generator_sha256 = format!("{:x}", Sha256::digest(generator.as_bytes()));
    row.comparator_wrapper_sha256 = format!("{:x}", Sha256::digest(wrapper.as_bytes()));
    let inputs = super::snapshot::stage(
        &workspace,
        &row,
        &temporary.path().join("authenticated-inputs"),
    )
    .unwrap();
    let python_path = temporary.path().join("bin/python");
    fs::create_dir_all(python_path.parent().unwrap()).unwrap();
    fs::write(&python_path, "#!/bin/sh\nexec /bin/sh \"$@\"\n").unwrap();
    let mut permissions = fs::metadata(&python_path).unwrap().permissions();
    permissions.set_mode(0o755);
    fs::set_permissions(&python_path, permissions).unwrap();
    let pins = ComparatorOutputPins {
        casadi_c_sha256: format!("{:x}", Sha256::digest(fresh_c.as_bytes())),
        casadi_h_sha256: format!("{:x}", Sha256::digest(fresh_h.as_bytes())),
    };
    let plan = fake_bound_plan_with_pins(temporary.path(), &row, pins);

    let emission = emit::emit_casadi(&inputs, &plan, &output).unwrap();
    assert!(emission.wrapper_c.as_path().starts_with(inputs.root()));
    assert_eq!(
        fs::read_to_string(emission.wrapper_c.as_path()).unwrap(),
        "#include \"casadi_exp_mixed.c\"\n"
    );
    assert_eq!(
        fs::read_to_string(emission.generated_c.as_path()).unwrap(),
        fresh_c
    );
}

#[cfg(unix)]
#[test]
fn stale_hand_edited_winner_cannot_be_measured() {
    let temporary = tempfile::tempdir().unwrap();
    let (row, inputs) = staged_real_inputs(temporary.path());
    let output = temporary.path().join("emitted/rumoca");
    fs::create_dir_all(&output).unwrap();
    let stale = output.join("ExpMixedStep.c");
    fs::write(&stale, "void hand_edited_winner(void) {}\n").unwrap();
    for name in [
        ".clang-format",
        "ExpMixedStep.h",
        "rumoca_galec_kernels.c",
        "rumoca_galec_kernels.h",
    ] {
        fs::write(output.join(name), "hand edited\n").unwrap();
    }
    let plan = fake_bound_plan(temporary.path(), &row);
    let cache = super::typed_path::RolePath::checked(temporary.path().join("cache")).unwrap();
    let result = emit::emit_rumoca(&inputs, &plan, &output, &cache);
    let error = result.err().expect("zero-exit fake must remain unmeasured");
    assert!(
        !stale.exists(),
        "fresh emission must delete the hand-edited C"
    );
    assert!(format!("{error:#}").contains("emission roster"));
}

#[test]
fn rejected_measurement_has_no_comparison_outcome() {
    let mut row = entry();
    let evidence = super::Evidence {
        entry_sha256: digest(),
        suite_implementation_sha256: super::suite_identity::sha256(),
        normalized_profile_sha256: digest(),
        completed_suite_sha256: digest(),
        authenticated_inputs_sha256: digest(),
        rumoca_instructions: 1251,
        comparator_instructions: 1084,
        output_lines: Vec::new(),
        artifact_bundle: artifact_bundle(),
        commands: Vec::new(),
    };
    let verdict = judge(&row, evidence, 0.0);
    rejected_findings(&verdict);
    let wire = serde_json::to_value(&verdict).unwrap();
    assert_eq!(wire["status"], "rejected");
    assert!(wire.get("delta").is_none());
    assert!(wire.get("comparison_outcome").is_none());

    row.max_delta = Some(0);
    row.measured.rumoca = row.measured.comparator;
    assert!(super::manifest::validate(&manifest(vec![row])).is_ok());

    let mut dominant = entry();
    dominant.max_delta = Some(-1);
    dominant.measured.rumoca = dominant.measured.comparator - 1;
    assert!(super::manifest::validate(&manifest(vec![dominant])).is_ok());
}

#[test]
fn improvements_require_explicit_promotion_and_promoted_rows_reject_old_counts() {
    let mut row = entry();
    let improvement = row.measured.rumoca - 1;
    let verdict = judge(
        &row,
        instruction_evidence(improvement, row.measured.comparator),
        0.0,
    );
    assert!(
        rejected_findings(&verdict)
            .iter()
            .any(|finding| finding.contains("explicitly promote measured.rumoca and max_delta"))
    );

    let old = row.measured.rumoca;
    row.measured.rumoca = improvement;
    *row.max_delta.as_mut().unwrap() -= 1;
    super::manifest::validate(&manifest(vec![row.clone()])).unwrap();
    let old_count = judge(
        &row,
        instruction_evidence(old, row.measured.comparator),
        0.0,
    );
    assert!(
        rejected_findings(&old_count)
            .iter()
            .any(|finding| finding.contains("Rumoca instructions regressed"))
    );
}

#[test]
fn pending_schema_nine_row_cannot_import_a_coordinated_baseline_edit() {
    let prior = manifest(vec![entry()]);
    let mut current = prior.clone();
    current.entries[0].measured.rumoca += 1;
    *current.entries[0].max_delta.as_mut().unwrap() += 1;

    assert!(
        super::manifest::validate(&current).is_ok(),
        "the ordinary self-consistency check intentionally cannot authenticate history"
    );
    let error = super::manifest::validate_non_relaxation(&current, &prior)
        .expect_err("schema nine must reject coordinated baseline edits");
    assert!(
        error.to_string().contains("benchmark identity changed"),
        "{error:#}"
    );
}

#[test]
fn reviewed_row_cannot_be_removed_or_redefined_under_the_same_id() {
    let prior = manifest(vec![entry()]);
    let removed = manifest(Vec::new());
    assert!(super::manifest::validate_non_relaxation(&removed, &prior).is_err());

    let mut redefined = prior.clone();
    redefined.entries[0].fixture_sha256 = "1".repeat(64);
    let error = super::manifest::validate_non_relaxation(&redefined, &prior)
        .expect_err("an easier fixture needs a new benchmark identity");
    assert!(error.to_string().contains("benchmark identity changed"));
}

#[test]
fn reviewed_row_cannot_hide_cross_flag_drift_behind_the_same_profile_name() {
    let prior = manifest(vec![entry()]);
    let mut redefined = prior.clone();
    redefined.entries[0].normalized_profile_sha256 = "1".repeat(64);
    let error = super::manifest::validate_non_relaxation(&redefined, &prior)
        .expect_err("cross flag drift needs a new benchmark identity");
    assert!(
        format!("{error:#}").contains("benchmark identity changed"),
        "{error:#}"
    );
}

#[test]
fn historical_ratchet_rejects_tool_and_generated_output_pin_migration() {
    let prior = manifest(vec![entry()]);
    let mut tool_migration = prior.clone();
    tool_migration.measured_tools.arm_gcc_sha256 = "1".repeat(64);
    assert!(
        super::manifest::validate_non_relaxation(&tool_migration, &prior)
            .unwrap_err()
            .to_string()
            .contains("measured-tool SHA-256 pins changed")
    );

    let mut output_migration = prior.clone();
    output_migration.comparator_outputs.casadi_c_sha256 = "2".repeat(64);
    assert!(
        super::manifest::validate_non_relaxation(&output_migration, &prior)
            .unwrap_err()
            .to_string()
            .contains("comparator-output SHA-256 pins changed")
    );
}

#[test]
fn historical_suite_identity_is_self_contained_and_replacements_are_append_only() {
    let current_digest = super::suite_identity::sha256();
    let prior_digest = "1".repeat(64);
    let mut prior_wire = serde_json::to_value(manifest(vec![entry()])).unwrap();
    prior_wire["suite_implementation_history"]["initial_sha256"] = serde_json::json!(prior_digest);
    let prior: Manifest = serde_json::from_value(prior_wire).unwrap();
    let historical = super::manifest::parse_historical(
        &serde_json::to_string(&prior).unwrap(),
        "historical-runner",
    )
    .expect("historical validation must not compare against today's runner bytes");

    let mut current_wire = serde_json::to_value(&prior).unwrap();
    current_wire["suite_implementation_history"]["replacements"] = serde_json::json!([{
        "implementation_sha256": current_digest,
        "rationale": "reviewed runner-only replacement"
    }]);
    let current: Manifest = serde_json::from_value(current_wire).unwrap();
    super::manifest::validate_historical_non_relaxation(&checked_current(&current), &historical)
        .expect("an appended runner replacement must preserve benchmark row identity");
    assert_eq!(
        current.entries[0].normalized_profile_sha256, prior.entries[0].normalized_profile_sha256,
        "runner identity must not contaminate immutable benchmark protocol identity"
    );

    let mut rewritten = serde_json::to_value(&current).unwrap();
    rewritten["suite_implementation_history"]["initial_sha256"] = serde_json::json!("2".repeat(64));
    let rewritten: Manifest = serde_json::from_value(rewritten).unwrap();
    let error = super::manifest::validate_non_relaxation(&rewritten, &prior)
        .expect_err("runner history cannot be rewritten");
    assert!(format!("{error:#}").contains("history was rewritten"));
}

#[test]
fn suite_implementation_history_rejects_duplicates_and_semantic_authority() {
    let current_digest = super::suite_identity::sha256();
    let mut duplicate = serde_json::to_value(manifest(vec![entry()])).unwrap();
    duplicate["suite_implementation_history"]["replacements"] = serde_json::json!([{
        "implementation_sha256": current_digest,
        "rationale": "invalid reversion"
    }]);
    let duplicate: Manifest = serde_json::from_value(duplicate).unwrap();
    assert!(super::manifest::validate(&duplicate).is_err());

    let prior_digest = "1".repeat(64);
    let mut prior_wire = serde_json::to_value(manifest(vec![entry()])).unwrap();
    prior_wire["suite_implementation_history"]["initial_sha256"] = serde_json::json!(prior_digest);
    let prior: Manifest = serde_json::from_value(prior_wire).unwrap();
    let mut current_wire = serde_json::to_value(&prior).unwrap();
    current_wire["suite_implementation_history"]["replacements"] = serde_json::json!([{
        "implementation_sha256": super::suite_identity::sha256(),
        "rationale": "reviewed runner-only replacement"
    }]);
    current_wire["measured_tools"]["qemu_sha256"] = serde_json::json!("3".repeat(64));
    let current: Manifest = serde_json::from_value(current_wire).unwrap();
    let error = super::manifest::validate_non_relaxation(&current, &prior)
        .expect_err("a runner replacement cannot authorize a tool migration");
    assert!(format!("{error:#}").contains("measured-tool SHA-256 pins changed"));
}

#[test]
fn schema_nine_forbids_tool_and_existing_row_identity_migration() {
    let prior = manifest(vec![entry()]);
    let historical = historical_current(&prior);

    let mut changed_tool = prior.clone();
    changed_tool.measured_tools.compiler_dependencies_sha256 = "2".repeat(64);
    let error = super::manifest::validate_historical_non_relaxation(
        &checked_current(&changed_tool),
        &historical,
    )
    .expect_err("schema nine must not authenticate caller-supplied migration evidence");
    assert!(format!("{error:#}").contains("schema 9 has no migration authority"));

    let mut changed_policy = prior.clone();
    changed_policy.ratchet_policy = "relaxed policy".into();
    let error = super::manifest::validate_non_relaxation(&changed_policy, &prior)
        .expect_err("runner changes cannot authorize benchmark-protocol drift");
    assert!(format!("{error:#}").contains("ratchet policy changed"));

    let mut changed_identity = prior;
    changed_identity.entries[0].normalized_profile_sha256 = "3".repeat(64);
    let error = super::manifest::validate_historical_non_relaxation(
        &checked_current(&changed_identity),
        &historical,
    )
    .expect_err("an existing row identity is immutable");
    assert!(format!("{error:#}").contains("add a new row instead"));

    let mut added_row = changed_identity;
    added_row.entries[0] = entry();
    let mut second = entry();
    second.id = "second-schema-nine-row".into();
    added_row.entries.push(second);
    let error = super::manifest::validate_historical_non_relaxation(
        &checked_current(&added_row),
        &historical,
    )
    .expect_err("schema nine cannot add another competitor row");
    assert!(format!("{error:#}").contains("no authority to add"));
}

#[test]
fn historical_manifest_rejects_uncommitted_schema_lineage() {
    let mut value = serde_json::to_value(manifest(vec![entry()])).unwrap();
    value["schema_version"] = serde_json::json!(7);
    let text = serde_json::to_string(&value).unwrap();
    let error = super::manifest::parse_historical(&text, "test-schema-seven")
        .expect_err("the superseded schema must not acquire a compatibility reader");
    assert!(format!("{error:#}").contains("unsupported; expected 9"));
}

fn historical_current(manifest: &Manifest) -> super::manifest::HistoricalManifest {
    let text = serde_json::to_string(manifest).unwrap();
    super::manifest::parse_historical(&text, "test-current").unwrap()
}

#[test]
fn pending_schema_nine_ci_job_is_suspended_but_retains_baseline_contract() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let workflow = fs::read_to_string(root.join(".github/workflows/ci.yml")).unwrap();
    let job = workflow
        .split("\n  embedded-head-to-head:\n")
        .nth(1)
        .and_then(|tail| tail.split("\n  kani:\n").next())
        .expect("CI owns one embedded-head-to-head job before kani");
    for required in [
        "if: ${{ false }}",
        "fetch-depth: 0",
        "github.event.pull_request.base.sha || github.event.before",
        "--baseline-git-revision \"$EMBEDDED_BASE_SHA\"",
    ] {
        assert!(
            job.contains(required),
            "the PR instruction ratchet lost historical authority `{required}`"
        );
    }
    for forbidden in [
        "git cat-file",
        "git show",
        "baseline_manifest",
        "baseline_args",
    ] {
        assert!(
            !job.contains(forbidden),
            "workflow must delegate Git baseline authority instead of containing `{forbidden}`"
        );
    }
}

#[test]
fn ci_and_nightly_publish_the_same_fail_closed_authenticated_bundle_contract() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    for (workflow, next_job, baseline) in [
        (
            "ci.yml",
            "kani",
            "github.event.pull_request.base.sha || github.event.before",
        ),
        ("nightly.yml", "parser-fuzz", "EMBEDDED_BASE_SHA: HEAD^"),
    ] {
        let text = fs::read_to_string(root.join(".github/workflows").join(workflow)).unwrap();
        let job = text
            .split("\n  embedded-head-to-head:\n")
            .nth(1)
            .and_then(|tail| tail.split(&format!("\n  {next_job}:\n")).next())
            .unwrap();
        for required in [
            "fetch-depth: 0",
            "--arm-toolchain \"$arm_root\"",
            "--qemu \"$qemu\"",
            "--python \"$python\"",
            "--prlimit \"$prlimit\"",
            "--baseline-git-revision \"$EMBEDDED_BASE_SHA\"",
            baseline,
            "Require embedded head-to-head evidence on success",
            "if: always()",
            "target/verification/embedded-head-to-head-summary.json",
            "target/verification/embedded-head-to-head/evidence-bundle/**",
            "if-no-files-found: error",
        ] {
            assert!(job.contains(required), "{workflow} lost `{required}`");
        }
        assert!(!job.contains("if-no-files-found: warn"));
    }
}

#[test]
fn git_baseline_loader_reads_the_exact_present_blob() {
    let baseline = manifest(vec![entry()]);
    let repository = git_repository(Some(&baseline));
    let predecessor = git(repository.path(), &["rev-parse", "HEAD"]);
    commit_file(
        repository.path(),
        "after.txt",
        "later compiler change\n",
        "later compiler change",
    );
    let git = git_executable();
    let loaded = super::manifest::authenticate_git_history(
        &checked_current(&baseline),
        repository.path(),
        &git,
        predecessor.trim(),
    )
    .unwrap();
    assert!(!loaded.evidence().is_bootstrap());
    assert!(loaded.evidence().predecessor_manifest_sha256().is_some());
    assert_eq!(loaded.commands().len(), 7);
    for (receipt, expected) in loaded.commands().iter().zip([
        "rev-parse",
        "rev-parse",
        "merge-base",
        "is-shallow-repository",
        "rev-list",
        "ls-tree",
        "cat-file",
    ]) {
        assert!(receipt.display_only().contains(expected));
        assert!(receipt.display_only().contains("--no-replace-objects"));
        assert!(receipt.is_hermetic());
        for forbidden in [
            "GIT_DIR",
            "GIT_WORK_TREE",
            "GIT_OBJECT_DIRECTORY",
            "GIT_ALTERNATE_OBJECT_DIRECTORIES",
        ] {
            assert!(!receipt.has_environment_name(forbidden));
        }
        assert!(receipt.has_environment_name("GIT_NO_REPLACE_OBJECTS"));
        assert!(receipt.has_environment_name("GIT_CONFIG_NOSYSTEM"));
    }
}

#[test]
fn ambient_fake_git_candidate_is_rejected_by_reviewed_sha_pin() {
    let temporary = tempfile::tempdir().unwrap();
    let fake = temporary.path().join("git");
    fs::write(&fake, "#!/bin/sh\necho forged git\n").unwrap();
    let pins = manifest(vec![entry()]).measured_tools;
    let error = super::authenticate_git_candidate(fake, &pins)
        .err()
        .expect("fake Git must not authenticate");
    assert!(error.to_string().contains("SHA-256 changed before use"));
}

#[test]
fn git_baseline_loader_ignores_replace_refs() {
    let baseline = manifest(vec![entry()]);
    let repository = git_repository(Some(&baseline));
    let original = git(repository.path(), &["rev-parse", "HEAD"]);
    fs::remove_file(repository.path().join(super::manifest::MANIFEST_PATH)).unwrap();
    fs::write(
        repository.path().join("replacement.txt"),
        "replacement tree\n",
    )
    .unwrap();
    git(repository.path(), &["add", "--all"]);
    git(
        repository.path(),
        &[
            "-c",
            "user.name=Rumoca Test",
            "-c",
            "user.email=rumoca@example.invalid",
            "-c",
            "commit.gpgsign=false",
            "commit",
            "--quiet",
            "-m",
            "replacement",
        ],
    );
    let replacement = git(repository.path(), &["rev-parse", "HEAD"]);
    git(
        repository.path(),
        &["replace", original.trim(), replacement.trim()],
    );

    let git = git_executable();
    let loaded = super::manifest::authenticate_git_history(
        &checked_current(&baseline),
        repository.path(),
        &git,
        original.trim(),
    )
    .unwrap();
    assert_eq!(loaded.evidence().predecessor_commit(), original.trim());
    assert!(!loaded.evidence().is_bootstrap());
}

#[test]
fn git_baseline_loader_binds_a_worktree_git_directory_explicitly() {
    let baseline = manifest(vec![entry()]);
    let repository = git_repository(Some(&baseline));
    let predecessor = git(repository.path(), &["rev-parse", "HEAD"]);
    commit_file(
        repository.path(),
        "after.txt",
        "later compiler change\n",
        "later compiler change",
    );
    let worktree_parent = tempfile::tempdir().unwrap();
    let worktree = worktree_parent.path().join("checked-out-worktree");
    git(
        repository.path(),
        &[
            "worktree",
            "add",
            "--quiet",
            "--detach",
            worktree.to_str().unwrap(),
            "HEAD",
        ],
    );
    assert!(worktree.join(".git").is_file());

    let git = git_executable();
    let loaded = super::manifest::authenticate_git_history(
        &checked_current(&baseline),
        &worktree,
        &git,
        predecessor.trim(),
    )
    .unwrap();
    assert!(!loaded.evidence().is_bootstrap());
    assert!(
        loaded
            .commands()
            .iter()
            .all(|receipt| receipt.is_hermetic())
    );
}

#[test]
fn git_baseline_loader_bootstraps_only_on_successful_exact_absence() {
    let repository = git_repository(None);
    let git = git_executable();
    let current = manifest(vec![entry()]);
    let loaded = super::manifest::authenticate_git_history(
        &checked_current(&current),
        repository.path(),
        &git,
        "HEAD",
    )
    .unwrap();
    assert!(loaded.evidence().is_bootstrap());
    assert!(loaded.evidence().predecessor_manifest_sha256().is_none());
    assert_eq!(loaded.commands().len(), 5);
    assert!(loaded.commands()[0].display_only().contains("rev-parse"));
    assert!(loaded.commands()[2].display_only().contains("merge-base"));
    assert!(
        loaded.commands()[3]
            .display_only()
            .contains("is-shallow-repository")
    );
    assert!(loaded.commands()[4].display_only().contains("rev-list"));
    assert!(
        loaded.commands()[4]
            .display_only()
            .contains(super::manifest::MANIFEST_PATH)
    );
    assert!(
        serde_json::to_value(manifest(vec![entry()]))
            .unwrap()
            .get("migration_ledger")
            .is_none()
    );
}

#[test]
fn bootstrap_cannot_introduce_selected_efmu_artifact_history() {
    let mut row = pending_entry();
    row.rumoca_artifact_history = efmu_entry().rumoca_artifact_history;
    let error = super::manifest::validate(&manifest(vec![row])).unwrap_err();
    assert!(format!("{error:#}").contains("cannot carry artifact history"));
}

#[test]
fn committed_first_introduction_binds_the_exact_head_blob() {
    let current = manifest(vec![entry()]);
    let repository = git_repository(Some(&current));
    let loaded = super::manifest::authenticate_git_history(
        &checked_current(&current),
        repository.path(),
        &git_executable(),
        "HEAD",
    )
    .unwrap();
    assert!(loaded.evidence().is_bootstrap());
    assert!(
        loaded
            .commands()
            .iter()
            .any(|receipt| receipt.display_only().contains("cat-file blob"))
    );

    let mut mismatched = current;
    mismatched.ratchet_policy.push_str(" changed");
    let error = super::manifest::authenticate_git_history(
        &checked_current(&mismatched),
        repository.path(),
        &git_executable(),
        "HEAD",
    )
    .err()
    .expect("HEAD-only bootstrap must bind the exact checked manifest bytes");
    assert!(
        format!("{error:#}").contains("differ from the checked current manifest"),
        "{error:#}"
    );
}

#[test]
fn pre_manifest_baseline_cannot_reenter_bootstrap_after_history_exists() {
    let repository = git_repository(None);
    let pre_manifest = git(repository.path(), &["rev-parse", "HEAD"]);
    let current = manifest(vec![entry()]);
    let manifest_path = repository.path().join(super::manifest::MANIFEST_PATH);
    fs::create_dir_all(manifest_path.parent().unwrap()).unwrap();
    fs::write(&manifest_path, serde_json::to_vec_pretty(&current).unwrap()).unwrap();
    git(repository.path(), &["add", "--all"]);
    git(
        repository.path(),
        &[
            "-c",
            "user.name=Rumoca Test",
            "-c",
            "user.email=rumoca@example.invalid",
            "-c",
            "commit.gpgsign=false",
            "commit",
            "--quiet",
            "-m",
            "introduce manifest",
        ],
    );
    commit_file(
        repository.path(),
        "after.txt",
        "later compiler change\n",
        "later compiler change",
    );

    let error = super::manifest::authenticate_git_history(
        &checked_current(&current),
        repository.path(),
        &git_executable(),
        pre_manifest.trim(),
    )
    .err()
    .expect("caller-selected point absence must not authorize bootstrap");
    assert!(
        format!("{error:#}").contains("strict-ancestor history does"),
        "{error:#}"
    );
}

#[test]
fn shallow_history_cannot_authorize_bootstrap_or_predecessor_evidence() {
    let current = manifest(vec![entry()]);
    let origin = git_repository(Some(&current));
    commit_file(
        origin.path(),
        "after.txt",
        "later compiler change\n",
        "later compiler change",
    );
    let clone_parent = tempfile::tempdir().unwrap();
    let clone = clone_parent.path().join("shallow");
    let output = Command::new("git")
        .args(["clone", "--quiet", "--depth", "1"])
        .arg(format!("file://{}", origin.path().display()))
        .arg(&clone)
        .output()
        .unwrap();
    assert!(output.status.success());
    let error = super::manifest::authenticate_git_history(
        &checked_current(&current),
        &clone,
        &git_executable(),
        "HEAD",
    )
    .err()
    .expect("a shallow repository cannot prove global manifest history");
    assert!(format!("{error:#}").contains("require complete Git history"));
}

#[test]
fn git_baseline_loader_rejects_an_invalid_revision() {
    let repository = git_repository(None);
    let git = git_executable();
    let current = manifest(vec![entry()]);
    let error = super::manifest::authenticate_git_history(
        &checked_current(&current),
        repository.path(),
        &git,
        "not-a-revision",
    )
    .err()
    .expect("an unauthenticated revision must never bootstrap");
    assert!(
        format!("{error:#}").contains("authenticate embedded head-to-head baseline commit"),
        "{error:#}"
    );
}

#[test]
fn git_baseline_loader_rejects_a_missing_tree_object() {
    let repository = git_repository(None);
    let tree = git(repository.path(), &["rev-parse", "HEAD^{tree}"]);
    let tree = tree.trim();
    let object = repository
        .path()
        .join(".git/objects")
        .join(&tree[..2])
        .join(&tree[2..]);
    assert!(object.is_file(), "test tree object must be loose");
    fs::remove_file(object).unwrap();

    let git = git_executable();
    let current = manifest(vec![entry()]);
    let error = super::manifest::authenticate_git_history(
        &checked_current(&current),
        repository.path(),
        &git,
        "HEAD",
    )
    .err()
    .expect("a missing tree object must fail instead of bootstrap");
    assert!(
        format!("{error:#}").contains("authenticate embedded head-to-head manifest history"),
        "{error:#}"
    );
}

#[test]
fn authenticated_history_rejects_a_nonancestor_commit_before_bootstrap() {
    let repository = git_repository(None);
    let unrelated = git(repository.path(), &["rev-parse", "HEAD"]);
    git(
        repository.path(),
        &["checkout", "--quiet", "--orphan", "other"],
    );
    fs::write(repository.path().join("other.txt"), "unrelated history\n").unwrap();
    git(repository.path(), &["add", "--all"]);
    git(
        repository.path(),
        &[
            "-c",
            "user.name=Rumoca Test",
            "-c",
            "user.email=rumoca@example.invalid",
            "-c",
            "commit.gpgsign=false",
            "commit",
            "--quiet",
            "-m",
            "unrelated",
        ],
    );
    let current = manifest(vec![entry()]);
    let git = git_executable();
    let error = super::manifest::authenticate_git_history(
        &checked_current(&current),
        repository.path(),
        &git,
        unrelated.trim(),
    )
    .err()
    .expect("a nonancestor must never authorize bootstrap");
    assert!(
        format!("{error:#}").contains("is not an ancestor"),
        "{error:#}"
    );
}

#[test]
fn intermediate_commit_cannot_authorize_schema_nine_semantic_migration() {
    let prior = manifest(vec![entry()]);
    let repository = git_repository(Some(&prior));
    let predecessor = git(repository.path(), &["rev-parse", "HEAD"]);

    fs::write(
        repository.path().join("reviewed-evidence.txt"),
        "independent migration review\n",
    )
    .unwrap();
    git(repository.path(), &["add", "--all"]);
    git(
        repository.path(),
        &[
            "-c",
            "user.name=Rumoca Test",
            "-c",
            "user.email=rumoca@example.invalid",
            "-c",
            "commit.gpgsign=false",
            "commit",
            "--quiet",
            "-m",
            "review migration evidence",
        ],
    );
    let mut current = prior;
    current.measured_tools.compiler_dependencies_sha256 = "2".repeat(64);
    let error = super::manifest::authenticate_git_history(
        &checked_current(&current),
        repository.path(),
        &git_executable(),
        predecessor.trim(),
    )
    .err()
    .expect("an intermediate commit cannot authenticate a schema-eight migration");
    assert!(
        format!("{error:#}").contains("schema 9 has no migration authority"),
        "{error:#}"
    );
}

#[test]
fn authenticated_runner_replacement_is_operable_without_semantic_migration() {
    let mut prior_wire = serde_json::to_value(manifest(vec![entry()])).unwrap();
    prior_wire["suite_implementation_history"]["initial_sha256"] =
        serde_json::json!("1".repeat(64));
    let prior: Manifest = serde_json::from_value(prior_wire).unwrap();
    let repository = git_repository(Some(&prior));
    let predecessor = git(repository.path(), &["rev-parse", "HEAD"]);
    commit_file(
        repository.path(),
        "runner-equivalence.txt",
        "reviewed identical protocol and exact observations\n",
        "review runner replacement",
    );
    let head_commit = git(repository.path(), &["rev-parse", "HEAD"]);

    let mut current_wire = serde_json::to_value(&prior).unwrap();
    current_wire["suite_implementation_history"]["replacements"] = serde_json::json!([{
        "implementation_sha256": super::suite_identity::sha256(),
        "rationale": "reviewed runner-only replacement with unchanged protocol"
    }]);
    let current: Manifest = serde_json::from_value(current_wire).unwrap();
    let authenticated = super::manifest::authenticate_git_history(
        &checked_current(&current),
        repository.path(),
        &git_executable(),
        predecessor.trim(),
    )
    .unwrap();
    let wire = serde_json::to_value(authenticated.evidence()).unwrap();
    assert_eq!(
        wire["kind"],
        "authenticated-predecessor-with-runner-replacement"
    );
    assert_eq!(wire["head_commit"], head_commit.trim());
    assert_eq!(wire["previous_implementation_sha256"], "1".repeat(64));
    assert_eq!(
        wire["current_implementation_sha256"],
        super::suite_identity::sha256()
    );

    let mut multiple_wire = serde_json::to_value(&prior).unwrap();
    multiple_wire["suite_implementation_history"]["replacements"] = serde_json::json!([
        {
            "implementation_sha256": "2".repeat(64),
            "rationale": "first replacement"
        },
        {
            "implementation_sha256": super::suite_identity::sha256(),
            "rationale": "second replacement"
        }
    ]);
    let multiple: Manifest = serde_json::from_value(multiple_wire).unwrap();
    let error = super::manifest::authenticate_git_history(
        &checked_current(&multiple),
        repository.path(),
        &git_executable(),
        predecessor.trim(),
    )
    .err()
    .expect("one comparison cannot launder multiple runner replacements");
    assert!(format!("{error:#}").contains("cannot append multiple"));
}

fn git_repository(manifest: Option<&Manifest>) -> tempfile::TempDir {
    let repository = tempfile::tempdir().unwrap();
    git(repository.path(), &["init", "--quiet"]);
    match manifest {
        Some(manifest) => {
            let path = repository.path().join(super::manifest::MANIFEST_PATH);
            fs::create_dir_all(path.parent().unwrap()).unwrap();
            fs::write(path, serde_json::to_vec_pretty(manifest).unwrap()).unwrap();
        }
        None => {
            fs::write(
                repository.path().join("README.md"),
                "baseline without manifest\n",
            )
            .unwrap();
        }
    }
    git(repository.path(), &["add", "--all"]);
    git(
        repository.path(),
        &[
            "-c",
            "user.name=Rumoca Test",
            "-c",
            "user.email=rumoca@example.invalid",
            "-c",
            "commit.gpgsign=false",
            "commit",
            "--quiet",
            "-m",
            "baseline",
        ],
    );
    repository
}

fn checked_current(manifest: &Manifest) -> super::manifest::CheckedCurrentManifest {
    super::manifest::CheckedCurrentManifest::for_test(manifest.clone())
}

fn commit_file(root: &Path, name: &str, contents: &str, message: &str) {
    fs::write(root.join(name), contents).unwrap();
    git(root, &["add", "--all"]);
    git(
        root,
        &[
            "-c",
            "user.name=Rumoca Test",
            "-c",
            "user.email=rumoca@example.invalid",
            "-c",
            "commit.gpgsign=false",
            "commit",
            "--quiet",
            "-m",
            message,
        ],
    );
}

fn git(root: &Path, args: &[&str]) -> String {
    let output = Command::new("git")
        .arg("-C")
        .arg(root)
        .args(args)
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "git {args:?} failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    String::from_utf8(output.stdout).unwrap()
}

fn git_executable() -> super::typed_path::AuthenticatedExecutable<super::typed_path::GitExecutable>
{
    let path = std::env::split_paths(&std::env::var_os("PATH").unwrap())
        .map(|directory| directory.join("git"))
        .find(|candidate| candidate.is_file())
        .unwrap()
        .canonicalize()
        .unwrap();
    let digest = format!("{:x}", Sha256::digest(fs::read(&path).unwrap()));
    let path = super::typed_path::RolePath::checked(path).unwrap();
    super::typed_path::AuthenticatedExecutable::checked(path, &digest).unwrap()
}

fn assert_generation_bindings_use_snapshot(
    root: &Path,
    workspace: &Path,
    row: &Entry,
    inputs: &super::snapshot::AuthenticatedInputs,
) {
    let plan = fake_bound_plan(root, row);
    let cache = super::typed_path::RolePath::checked(root.join("gate/cache")).unwrap();
    let rumoca_output =
        super::typed_path::RolePath::checked(root.join("gate/emitted/rumoca")).unwrap();
    let rumoca =
        super::cross::rumoca_generation_command(&plan, inputs.fixture(), &cache, &rumoca_output)
            .unwrap();
    let rumoca_args = rumoca.get_args().map(PathBuf::from).collect::<Vec<_>>();
    assert!(rumoca_args.contains(&inputs.fixture().as_path().to_path_buf()));
    assert!(!rumoca_args.iter().any(|path| path.starts_with(workspace)));

    let casadi_output =
        super::typed_path::RolePath::checked(root.join("gate/emitted/casadi")).unwrap();
    let casadi =
        super::cross::casadi_generation_command(&plan, inputs.generator(), &casadi_output).unwrap();
    let casadi_args = casadi.get_args().map(PathBuf::from).collect::<Vec<_>>();
    assert!(casadi_args.contains(&inputs.generator().as_path().to_path_buf()));
    assert!(!casadi_args.iter().any(|path| path.starts_with(workspace)));
}

fn assert_build_bindings_use_snapshot(
    root: &Path,
    workspace: &Path,
    row: &Entry,
    inputs: &super::snapshot::AuthenticatedInputs,
) {
    let emission_root = root.join("gate/emitted/casadi");
    fs::create_dir_all(&emission_root).unwrap();
    fs::write(emission_root.join("casadi_exp_mixed.c"), "test source\n").unwrap();
    fs::write(emission_root.join("casadi_exp_mixed.h"), "test header\n").unwrap();
    let emission = emit::CasadiEmission {
        frozen: super::artifact_guard::FrozenArtifactSet::capture_tree(&emission_root).unwrap(),
        include: super::typed_path::RolePath::checked(emission_root.clone()).unwrap(),
        generated_c: super::typed_path::RolePath::checked(emission_root.join("casadi_exp_mixed.c"))
            .unwrap(),
        generated_h: super::typed_path::RolePath::checked(emission_root.join("casadi_exp_mixed.h"))
            .unwrap(),
        wrapper_c: inputs.wrapper().clone(),
        command: super::process::test_receipt("casadi-generation"),
    };
    let plan = fake_bound_plan(root, row);
    let oracle_root = root.join("gate/oracle");
    fs::create_dir_all(&oracle_root).unwrap();
    fs::write(oracle_root.join("expected_output.h"), "test oracle\n").unwrap();
    let oracle_guard =
        super::artifact_guard::FrozenArtifactSet::capture_tree(&oracle_root).unwrap();
    let oracle = super::typed_path::RolePath::checked(oracle_root).unwrap();
    let context = super::cross::BuildContext {
        plan: &plan,
        harness: inputs.harness(),
        oracle: &oracle,
        oracle_guard: &oracle_guard,
    };
    let paths = super::cross::casadi_measured_bound_paths(
        &emission,
        &root.join("gate/build-casadi"),
        &context,
    )
    .unwrap();
    assert!(paths.contains(&inputs.wrapper().as_path().to_path_buf()));
    assert!(!paths.iter().any(|path| path.starts_with(workspace)));
}

fn fake_bound_plan(root: &Path, row: &Entry) -> super::cross::BoundExecutionPlan {
    fake_bound_plan_with_pins(root, row, comparator_output_pins())
}

fn fake_bound_plan_with_pins(
    root: &Path,
    row: &Entry,
    pins: ComparatorOutputPins,
) -> super::cross::BoundExecutionPlan {
    let execution_root = fake_execution_root_with_pins(root, row, pins);
    super::cross::bind_execution_plan(&execution_root, row).unwrap()
}

fn fake_execution_root(
    root: &Path,
    row: &Entry,
) -> std::sync::Arc<super::cross::AuthenticatedExecutionRoot> {
    fake_execution_root_with_pins(root, row, comparator_output_pins())
}

fn fake_execution_root_with_pins(
    root: &Path,
    row: &Entry,
    comparator_outputs: ComparatorOutputPins,
) -> std::sync::Arc<super::cross::AuthenticatedExecutionRoot> {
    use std::sync::atomic::{AtomicUsize, Ordering};

    static NEXT_PLAN: AtomicUsize = AtomicUsize::new(0);
    let sequence = NEXT_PLAN.fetch_add(1, Ordering::Relaxed);
    let gcc =
        super::typed_path::RolePath::checked(root.join("gate/toolchain/bin/arm-none-eabi-gcc"))
            .unwrap();
    let nm = super::typed_path::RolePath::checked(root.join("gate/toolchain/bin/arm-none-eabi-nm"))
        .unwrap();
    let qemu = super::typed_path::RolePath::checked(root.join("gate/bin/qemu-system-arm")).unwrap();
    let python = super::typed_path::RolePath::checked(root.join("bin/python"))
        .or_else(|_| super::typed_path::RolePath::checked(root.join("gate/bin/python")))
        .unwrap();
    let prlimit = super::typed_path::RolePath::checked(root.join("gate/bin/prlimit")).unwrap();
    for path in [
        gcc.as_path(),
        nm.as_path(),
        qemu.as_path(),
        python.as_path(),
        prlimit.as_path(),
    ] {
        if !path.exists() {
            fs::create_dir_all(path.parent().unwrap()).unwrap();
            fs::write(path, "#!/bin/sh\nexit 0\n").unwrap();
        }
    }
    let source = root.join(format!("fake-compiler-{sequence}/rumoca"));
    fs::create_dir_all(source.parent().unwrap()).unwrap();
    fs::write(&source, "#!/bin/sh\nexit 0\n").unwrap();
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut permissions = fs::metadata(&source).unwrap().permissions();
        permissions.set_mode(0o755);
        fs::set_permissions(&source, permissions).unwrap();
    }
    let closure = super::tool_closure::AuthenticatedToolClosure::for_test(root);
    let compiler = super::cross::stage_compiler(
        &source,
        &root.join(format!("gate-plan-{sequence}")),
        &closure,
    )
    .unwrap();
    let gcc_sha256 = super::sha256_file(gcc.as_path()).unwrap();
    let nm_sha256 = super::sha256_file(nm.as_path()).unwrap();
    let qemu_sha256 = super::sha256_file(qemu.as_path()).unwrap();
    let python_sha256 = super::sha256_file(python.as_path()).unwrap();
    let prlimit_sha256 = super::sha256_file(prlimit.as_path()).unwrap();
    let gcc = super::typed_path::AuthenticatedExecutable::checked(gcc, &gcc_sha256).unwrap();
    let nm = super::typed_path::AuthenticatedExecutable::checked(nm, &nm_sha256).unwrap();
    let qemu = super::typed_path::AuthenticatedExecutable::checked(qemu, &qemu_sha256).unwrap();
    let python =
        super::typed_path::AuthenticatedExecutable::checked(python, &python_sha256).unwrap();
    let prlimit =
        super::typed_path::AuthenticatedExecutable::checked(prlimit, &prlimit_sha256).unwrap();
    super::cross::authenticate_execution_root(super::cross::ExecutionRootInputs {
        gcc,
        nm,
        qemu,
        python,
        prlimit,
        gcc_version: super::cross::expected_gcc_version(row.cross_profile).into(),
        nm_version: super::cross::expected_nm_version(row.cross_profile).into(),
        qemu_version: super::cross::expected_qemu_version(row.cross_profile).into(),
        python_version: super::cross::expected_python_version(row.cross_profile).into(),
        prlimit_version: super::cross::expected_prlimit_version(row.cross_profile).into(),
        compiler,
        comparator_outputs,
        tool_closure: closure,
    })
    .unwrap()
}

fn repository_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../..")
}

fn real_entry(root: &Path) -> Entry {
    super::manifest::load(&super::manifest::path(root))
        .unwrap()
        .entries
        .into_iter()
        .next()
        .unwrap()
}

fn staged_real_inputs(destination: &Path) -> (Entry, super::snapshot::AuthenticatedInputs) {
    let root = repository_root();
    let row = real_entry(&root);
    let inputs =
        super::snapshot::stage(&root, &row, &destination.join("authenticated-inputs")).unwrap();
    (row, inputs)
}

fn copy_checked_workspace(source: &Path, row: &Entry, destination: &Path) {
    for relative in [
        &row.fixture,
        &row.comparator_generator,
        &row.comparator_wrapper,
    ] {
        let target = destination.join(relative);
        fs::create_dir_all(target.parent().unwrap()).unwrap();
        fs::copy(source.join(relative), target).unwrap();
    }
    let source_harness = source.join(&row.fixture).parent().unwrap().join("harness");
    let target_harness = destination
        .join(&row.fixture)
        .parent()
        .unwrap()
        .join("harness");
    fs::create_dir_all(&target_harness).unwrap();
    for member in fs::read_dir(source_harness).unwrap() {
        let member = member.unwrap();
        fs::copy(member.path(), target_harness.join(member.file_name())).unwrap();
    }
}

#[test]
fn slower_comparator_cannot_make_the_delta_ratchet_pass() {
    let row = entry();
    let evidence = super::Evidence {
        entry_sha256: digest(),
        suite_implementation_sha256: super::suite_identity::sha256(),
        normalized_profile_sha256: digest(),
        completed_suite_sha256: digest(),
        authenticated_inputs_sha256: digest(),
        rumoca_instructions: 1250,
        comparator_instructions: 1200,
        output_lines: Vec::new(),
        artifact_bundle: artifact_bundle(),
        commands: Vec::new(),
    };
    let verdict = judge(&row, evidence, 0.0);
    assert!(rejected_findings(&verdict)[0].contains("comparator instructions changed"));
}

#[test]
fn zero_instruction_rumoca_call_has_no_comparison_outcome() {
    let row = entry();
    let evidence = super::Evidence {
        entry_sha256: digest(),
        suite_implementation_sha256: super::suite_identity::sha256(),
        normalized_profile_sha256: digest(),
        completed_suite_sha256: digest(),
        authenticated_inputs_sha256: digest(),
        rumoca_instructions: 0,
        comparator_instructions: 1084,
        output_lines: Vec::new(),
        artifact_bundle: artifact_bundle(),
        commands: Vec::new(),
    };
    let verdict = judge(&row, evidence, 0.0);
    assert!(
        rejected_findings(&verdict)
            .iter()
            .any(|finding| finding.contains("zero-instruction"))
    );
}
