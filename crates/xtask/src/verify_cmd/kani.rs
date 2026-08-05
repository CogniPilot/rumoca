//! Manifest-driven bounded verification with the repository-pinned Kani release.

use anyhow::{Context, Result, ensure};
use serde::{Deserialize, Serialize};
use std::collections::BTreeSet;
use std::fs;
use std::path::{Component, Path};
use std::process::Command;
use std::time::Instant;

const MANIFEST_PATH: &str = "verification/kani-proofs.json";
const MANIFEST_SCHEMA_VERSION: u32 = 2;
const REQUIRED_KANI_VERSION: &str = "0.67.0";
const SOLVER_PACKAGE: &str = "rumoca-solver";
const SOLVER_SOURCE_ROOT: &str = "crates/rumoca-solver/src";
const SUMMARY_PATH: &str = "target/verification/kani-summary.json";
const KNOWN_CLAIM_IDS: &[&str] = &[
    "FS-EQN-001",
    "FS-EQN-002",
    "ME-LIFE-001",
    "ME-LIFE-002",
    "ME-LIFE-003",
    "ME-LIFE-004",
    "ME-ERR-001",
    "ME-BUF-001",
    "ME-STATE-001",
    "ME-BRAND-001",
    "SIM-010",
    "BEHAVIOR-PIN",
];

use super::VerifyKaniArgs;

#[derive(Clone, Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct KaniProofManifest {
    schema_version: u32,
    kani_version: String,
    proofs: Vec<KaniProof>,
}

#[derive(Clone, Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct KaniProof {
    package: String,
    harness: String,
    source: String,
    claims: Vec<String>,
    selection: ProofSelection,
    bound: ProofBound,
    covers: u32,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
struct ProofSelection {
    production_kernel: String,
    symbolic_inputs: String,
    exhaustive_test_infeasible_because: String,
    counterexample_means: String,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case", deny_unknown_fields)]
enum ProofBound {
    Unwind { value: u32, domain: String },
    FiniteDomain { domain: String },
}

pub(super) fn run(root: &Path, args: &VerifyKaniArgs) -> Result<()> {
    let manifest = load_manifest(root)?;
    verify_installed_version(root, &manifest.kani_version)?;
    let shard = args.parse_shard()?;
    let selected = select_manifest_shard(&manifest, shard)?;
    println!(
        "Running {} of {} required Kani {} proof harnesses from {}{}",
        selected.proofs.len(),
        manifest.proofs.len(),
        selected.kani_version,
        MANIFEST_PATH,
        shard.map_or_else(String::new, |(index, count)| format!(
            " (shard {index}/{count})"
        ))
    );
    for proof in &selected.proofs {
        println!("  {} [{}]", proof.harness, proof.claims.join(", "));
    }
    run_solver_proofs(root, &selected, manifest.proofs.len(), shard)
}

fn select_manifest_shard(
    manifest: &KaniProofManifest,
    shard: Option<(usize, usize)>,
) -> Result<KaniProofManifest> {
    let Some((index, count)) = shard else {
        return Ok(manifest.clone());
    };
    let proofs = manifest
        .proofs
        .iter()
        .enumerate()
        .filter(|(proof_index, _)| proof_index % count == index - 1)
        .map(|(_, proof)| proof.clone())
        .collect::<Vec<_>>();
    ensure!(
        !proofs.is_empty(),
        "Kani shard {index}/{count} selects no proof harnesses"
    );
    Ok(KaniProofManifest {
        schema_version: manifest.schema_version,
        kani_version: manifest.kani_version.clone(),
        proofs,
    })
}

fn load_manifest(root: &Path) -> Result<KaniProofManifest> {
    let path = root.join(MANIFEST_PATH);
    let raw = fs::read_to_string(&path)
        .with_context(|| format!("failed to read Kani proof manifest {}", path.display()))?;
    let manifest: KaniProofManifest = serde_json::from_str(&raw)
        .with_context(|| format!("failed to parse Kani proof manifest {}", path.display()))?;
    validate_manifest(root, &manifest)?;
    Ok(manifest)
}

fn validate_manifest(root: &Path, manifest: &KaniProofManifest) -> Result<()> {
    ensure!(
        manifest.schema_version == MANIFEST_SCHEMA_VERSION,
        "unsupported Kani proof manifest schema {}; expected {}",
        manifest.schema_version,
        MANIFEST_SCHEMA_VERSION
    );
    ensure!(
        manifest.kani_version == REQUIRED_KANI_VERSION,
        "Kani proof manifest must pin version {REQUIRED_KANI_VERSION}, found {}",
        manifest.kani_version
    );
    ensure!(!manifest.proofs.is_empty(), "Kani proof manifest is empty");
    let mut selectors = BTreeSet::new();
    for proof in &manifest.proofs {
        ensure!(
            proof.package == SOLVER_PACKAGE,
            "Kani proof package must be {SOLVER_PACKAGE}, found {}",
            proof.package
        );
        ensure!(!proof.harness.is_empty(), "Kani proof harness is empty");
        ensure!(!proof.claims.is_empty(), "{} has no claims", proof.harness);
        ensure!(
            proof.claims.iter().all(|claim| !claim.trim().is_empty()),
            "{} has an empty claim",
            proof.harness
        );
        for claim in &proof.claims {
            let claim_id = claim.split_once(':').map_or(claim.as_str(), |(id, _)| id);
            ensure!(
                KNOWN_CLAIM_IDS.contains(&claim_id),
                "{} names unknown proof claim `{claim}`",
                proof.harness
            );
        }
        validate_selection(proof)?;
        ensure!(
            selectors.insert((proof.package.as_str(), proof.harness.as_str())),
            "duplicate Kani proof selector {}::{}",
            proof.package,
            proof.harness
        );
        let source = Path::new(&proof.source);
        ensure!(
            !source.is_absolute()
                && source
                    .components()
                    .all(|component| matches!(component, Component::Normal(_))),
            "Kani proof source must be a workspace-relative path: {}",
            proof.source
        );
        ensure!(
            root.join(source).is_file(),
            "Kani proof source does not exist: {}",
            proof.source
        );
        let source_text = fs::read_to_string(root.join(source))
            .with_context(|| format!("failed to read Kani proof source {}", proof.source))?;
        let marker = format!("fn {}(", proof.harness);
        let harness_offset = source_text.find(&marker).with_context(|| {
            format!(
                "Kani harness {} was not found in {}",
                proof.harness, proof.source
            )
        })?;
        let proof_offset = source_text[..harness_offset]
            .rfind("#[kani::proof]")
            .with_context(|| format!("{} is not a Kani proof harness", proof.harness))?;
        ensure!(
            harness_offset - proof_offset < 256,
            "{} is not the harness annotated by the preceding #[kani::proof]",
            proof.harness
        );
        validate_declared_bound(proof, &source_text[proof_offset..harness_offset])?;
    }
    let listed: BTreeSet<_> = manifest
        .proofs
        .iter()
        .map(|proof| proof.harness.clone())
        .collect();
    let discovered = discover_solver_proofs(root)?;
    let missing: Vec<_> = discovered.difference(&listed).cloned().collect();
    let extra: Vec<_> = listed.difference(&discovered).cloned().collect();
    ensure!(
        missing.is_empty() && extra.is_empty(),
        "Kani manifest inventory differs from {SOLVER_SOURCE_ROOT}: missing={missing:?}, extra={extra:?}"
    );
    Ok(())
}

fn validate_selection(proof: &KaniProof) -> Result<()> {
    for (field, value) in [
        ("production_kernel", &proof.selection.production_kernel),
        ("symbolic_inputs", &proof.selection.symbolic_inputs),
        (
            "exhaustive_test_infeasible_because",
            &proof.selection.exhaustive_test_infeasible_because,
        ),
        (
            "counterexample_means",
            &proof.selection.counterexample_means,
        ),
    ] {
        ensure!(
            !value.trim().is_empty(),
            "{} has an empty selection.{field}",
            proof.harness
        );
    }
    Ok(())
}

fn validate_declared_bound(proof: &KaniProof, harness_attributes: &str) -> Result<()> {
    match &proof.bound {
        ProofBound::Unwind { value, domain } => {
            ensure!(*value > 0, "{} has a zero unwind bound", proof.harness);
            ensure!(
                !domain.trim().is_empty(),
                "{} has an empty bound domain",
                proof.harness
            );
            ensure!(
                harness_attributes.contains(&format!("#[kani::unwind({value})]")),
                "{} manifest unwind {value} differs from its source attribute",
                proof.harness
            );
        }
        ProofBound::FiniteDomain { domain } => {
            ensure!(
                !domain.trim().is_empty(),
                "{} has an empty finite domain",
                proof.harness
            );
            ensure!(
                !harness_attributes.contains("#[kani::unwind("),
                "{} declares a finite domain but has an unwind attribute",
                proof.harness
            );
        }
    }
    Ok(())
}

fn discover_solver_proofs(root: &Path) -> Result<BTreeSet<String>> {
    let mut harnesses = BTreeSet::new();
    for entry in walkdir::WalkDir::new(root.join(SOLVER_SOURCE_ROOT)) {
        let entry = entry.context("failed to walk solver sources for Kani proofs")?;
        if !entry.file_type().is_file() || entry.path().extension().is_none_or(|ext| ext != "rs") {
            continue;
        }
        let source = fs::read_to_string(entry.path())
            .with_context(|| format!("failed to read {}", entry.path().display()))?;
        for harness in discover_file_proofs(&source, entry.path())? {
            ensure!(
                harnesses.insert(harness.clone()),
                "duplicate Kani harness name `{harness}` in {SOLVER_SOURCE_ROOT}"
            );
        }
    }
    Ok(harnesses)
}

fn discover_file_proofs(source: &str, path: &Path) -> Result<Vec<String>> {
    let mut harnesses = Vec::new();
    let mut proof_pending = false;
    for line in source.lines().map(str::trim) {
        if line == "#[kani::proof]" {
            proof_pending = true;
            continue;
        }
        if !proof_pending {
            continue;
        }
        let Some(signature) = line.strip_prefix("fn ") else {
            continue;
        };
        let harness = signature
            .split_once('(')
            .with_context(|| format!("malformed Kani harness in {}", path.display()))?
            .0
            .trim()
            .to_string();
        harnesses.push(harness);
        proof_pending = false;
    }
    Ok(harnesses)
}

#[derive(Clone, Copy, Default)]
struct ParsedHarnessResult {
    success: Option<bool>,
    elapsed_seconds: Option<f64>,
    covers_satisfied: Option<u32>,
    covers_total: Option<u32>,
}

struct KaniRunSummary<'a> {
    parsed: &'a [ParsedHarnessResult],
    kani_summary: &'a [String],
    package_elapsed_seconds: f64,
    command_success: bool,
    cover_obligations_satisfied: bool,
    manifest_proof_count: usize,
    shard: Option<(usize, usize)>,
}

fn run_solver_proofs(
    root: &Path,
    manifest: &KaniProofManifest,
    manifest_proof_count: usize,
    shard: Option<(usize, usize)>,
) -> Result<()> {
    let mut command = Command::new("cargo");
    command
        // Kani 0.67's parallel text output does not identify the harness on
        // each result block, so per-harness timing and cover attribution is
        // fail-closed only when those blocks remain sequential. Individual
        // CBMC processes can also consume most of a verification host's RAM.
        .args(["kani", "--package", SOLVER_PACKAGE, "--jobs", "1"])
        .current_dir(root);
    if shard.is_some() {
        for proof in &manifest.proofs {
            command.args(["--harness", &proof.harness]);
        }
    }
    crate::resource_budget::apply_to_child(&mut command);
    let started = Instant::now();
    let output = command
        .output()
        .context("failed to execute the Kani proof package")?;
    let package_elapsed_seconds = started.elapsed().as_secs_f64();
    print!("{}", String::from_utf8_lossy(&output.stdout));
    eprint!("{}", String::from_utf8_lossy(&output.stderr));
    let combined = format!(
        "{}\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    let parsed = parse_kani_results(&combined, manifest);
    let cover_obligations_satisfied = manifest
        .proofs
        .iter()
        .zip(&parsed)
        .all(|(proof, result)| covers_match(proof.covers, *result));
    let kani_summary: Vec<_> = combined
        .lines()
        .filter(|line| {
            line.contains("SUMMARY")
                || line.contains("VERIFICATION")
                || line.contains("Verification Time:")
        })
        .map(str::trim)
        .filter(|line| !line.is_empty())
        .map(str::to_string)
        .collect();
    write_summary(
        root,
        manifest,
        KaniRunSummary {
            parsed: &parsed,
            kani_summary: &kani_summary,
            package_elapsed_seconds,
            command_success: output.status.success(),
            cover_obligations_satisfied,
            manifest_proof_count,
            shard,
        },
    )?;
    ensure!(
        output.status.success(),
        "Kani proof package failed with status {}",
        output.status
    );
    for (proof, result) in manifest.proofs.iter().zip(&parsed) {
        ensure!(
            result.success == Some(true) && result.elapsed_seconds.is_some(),
            "successful Kani output lacked a complete result for {}",
            proof.harness
        );
    }
    ensure!(
        cover_obligations_satisfied,
        "one or more Kani reachability-cover obligations were not satisfied"
    );
    Ok(())
}

fn parse_kani_results(output: &str, manifest: &KaniProofManifest) -> Vec<ParsedHarnessResult> {
    let mut results = vec![ParsedHarnessResult::default(); manifest.proofs.len()];
    let mut current = None;
    for line in output.lines() {
        if line.contains("Checking harness ") {
            current = manifest
                .proofs
                .iter()
                .enumerate()
                .filter(|(_, proof)| line.contains(&proof.harness))
                .max_by_key(|(_, proof)| proof.harness.len())
                .map(|(index, _)| index);
            continue;
        }
        let Some(index) = current else {
            continue;
        };
        if line.contains("VERIFICATION:- SUCCESSFUL") {
            results[index].success = Some(true);
        } else if line.contains("VERIFICATION:- FAILED") {
            results[index].success = Some(false);
        }
        if let Some((_, elapsed)) = line.split_once("Verification Time:") {
            results[index].elapsed_seconds = elapsed.trim().trim_end_matches('s').parse().ok();
        }
        if line.contains("cover properties satisfied") {
            let fields: Vec<_> = line.split_whitespace().collect();
            results[index].covers_satisfied = fields.get(1).and_then(|field| field.parse().ok());
            results[index].covers_total = fields.get(3).and_then(|field| field.parse().ok());
        }
    }
    results
}

fn covers_match(expected: u32, result: ParsedHarnessResult) -> bool {
    match (result.covers_satisfied, result.covers_total) {
        (Some(satisfied), Some(total)) => satisfied == expected && total == expected,
        (None, None) => expected == 0,
        _ => false,
    }
}

fn write_summary(root: &Path, manifest: &KaniProofManifest, run: KaniRunSummary<'_>) -> Result<()> {
    let path = run.shard.map_or_else(
        || root.join(SUMMARY_PATH),
        |(index, count)| {
            root.join(format!(
                "target/verification/kani-summary-shard-{index}-of-{count}.json"
            ))
        },
    );
    fs::create_dir_all(path.parent().expect("summary path has a parent"))?;
    let proofs: Vec<_> = manifest
        .proofs
        .iter()
        .zip(run.parsed)
        .map(|(proof, result)| {
            let elapsed_seconds = result
                .elapsed_seconds
                .unwrap_or(run.package_elapsed_seconds);
            let elapsed_source = if result.elapsed_seconds.is_some() {
                "kani_harness"
            } else {
                "package_total_fallback"
            };
            serde_json::json!({
                "harness": proof.harness,
                "claims": proof.claims,
                "selection": proof.selection,
                "declared_bound": proof.bound,
                "expected_cover_obligations": proof.covers,
                "covers_satisfied": result.covers_satisfied,
                "covers_total": result.covers_total,
                "elapsed_seconds": elapsed_seconds,
                "elapsed_source": elapsed_source,
                "success": result.success == Some(true)
                    && result.elapsed_seconds.is_some()
                    && covers_match(proof.covers, *result)
            })
        })
        .collect();
    let success = run.command_success
        && run.cover_obligations_satisfied
        && run
            .parsed
            .iter()
            .all(|result| result.success == Some(true) && result.elapsed_seconds.is_some());
    let summary = serde_json::json!({
        "schema_version": 1,
        "verifier": "kani",
        "kani_version": manifest.kani_version,
        "package": SOLVER_PACKAGE,
        "manifest_proof_count": run.manifest_proof_count,
        "proof_count": proofs.len(),
        "shard": run.shard.map(|(index, count)| serde_json::json!({
            "index": index,
            "count": count,
        })),
        "proofs": proofs,
        "package_elapsed_seconds": run.package_elapsed_seconds,
        "cover_obligations_satisfied": run.cover_obligations_satisfied,
        "kani_summary": run.kani_summary,
        "success": success
    });
    fs::write(&path, serde_json::to_string_pretty(&summary)?)
        .with_context(|| format!("failed to write {}", path.display()))?;
    println!(
        "Kani recorded {} manifest harnesses; summary written to {}",
        manifest.proofs.len(),
        path.display()
    );
    Ok(())
}

fn verify_installed_version(root: &Path, expected: &str) -> Result<()> {
    let output = Command::new("cargo")
        .args(["kani", "--version"])
        .current_dir(root)
        .output()
        .context("failed to execute `cargo kani --version`; enter `nix develop .#kani`")?;
    ensure!(
        output.status.success(),
        "`cargo kani --version` failed: {}",
        String::from_utf8_lossy(&output.stderr).trim()
    );
    let reported = String::from_utf8(output.stdout).context("Kani version output was not UTF-8")?;
    ensure!(
        reported.split_whitespace().any(|field| field == expected),
        "Kani version mismatch: manifest requires {expected}, command reported `{}`",
        reported.trim()
    );
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::{
        ParsedHarnessResult, REQUIRED_KANI_VERSION, covers_match, discover_solver_proofs,
        load_manifest, parse_kani_results, select_manifest_shard,
    };
    use std::collections::BTreeSet;
    use std::path::Path;

    #[test]
    fn checked_in_manifest_names_the_required_solver_proofs() {
        let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
        let manifest = load_manifest(&root).expect("checked-in Kani manifest should be valid");
        assert_eq!(manifest.kani_version, REQUIRED_KANI_VERSION);
        let listed: BTreeSet<_> = manifest
            .proofs
            .iter()
            .map(|proof| (proof.package.clone(), proof.harness.clone()))
            .collect();
        let discovered = discover_solver_proofs(&root)
            .expect("discover solver proofs")
            .into_iter()
            .map(|harness| ("rumoca-solver".to_string(), harness))
            .collect::<BTreeSet<_>>();
        assert_eq!(listed, discovered, "manifest must list every solver proof");
    }

    #[test]
    fn deterministic_shards_partition_the_complete_manifest() {
        let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
        let manifest = load_manifest(&root).expect("checked-in Kani manifest should be valid");
        let expected = manifest
            .proofs
            .iter()
            .map(|proof| proof.harness.clone())
            .collect::<BTreeSet<_>>();
        let mut selected = BTreeSet::new();
        let shard = select_manifest_shard(&manifest, Some((1, 1)))
            .expect("the complete shard should select the proof");
        for proof in shard.proofs {
            assert!(
                selected.insert(proof.harness),
                "a proof must occur in exactly one deterministic shard"
            );
        }
        assert_eq!(selected, expected);
    }

    #[test]
    fn kani_output_records_each_harness_result_and_elapsed_time() {
        let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
        let manifest = load_manifest(&root).expect("checked-in Kani manifest should be valid");
        let harness = &manifest.proofs[0].harness;
        let output = format!(
            "Checking harness rumoca_solver::verification::{harness}...\n\
             VERIFICATION:- SUCCESSFUL\n\
             Verification Time: 0.125s\n"
        );
        let parsed = parse_kani_results(&output, &manifest);
        assert_eq!(parsed[0].success, Some(true));
        assert_eq!(parsed[0].elapsed_seconds, Some(0.125));
    }

    #[test]
    fn interleaved_parallel_output_is_not_accepted_as_complete() {
        let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
        let mut manifest = load_manifest(&root).expect("checked-in Kani manifest should be valid");
        let mut second_proof = manifest.proofs[0].clone();
        second_proof.harness.push_str("_second");
        manifest.proofs.push(second_proof);
        let first = &manifest.proofs[0].harness;
        let second = &manifest.proofs[1].harness;
        let output = format!(
            "Thread 0: Checking harness rumoca_solver::verification::{first}...\n\
             Thread 1: Checking harness rumoca_solver::verification::{second}...\n\
             VERIFICATION:- SUCCESSFUL\n\
             Verification Time: 0.125s\n\
             VERIFICATION:- SUCCESSFUL\n\
             Verification Time: 0.250s\n"
        );
        let parsed = parse_kani_results(&output, &manifest);
        assert_eq!(parsed[0].success, None);
        assert_eq!(parsed[0].elapsed_seconds, None);
        assert_eq!(parsed[1].success, Some(true));
        assert_eq!(parsed[1].elapsed_seconds, Some(0.250));
        assert!(
            parsed
                .iter()
                .any(|result| result.success != Some(true) || result.elapsed_seconds.is_none()),
            "interleaved output must not look like a complete manifest result"
        );
    }

    #[test]
    fn per_harness_cover_summary_fails_closed() {
        assert!(covers_match(0, ParsedHarnessResult::default()));
        assert!(!covers_match(1, ParsedHarnessResult::default()));
        assert!(covers_match(
            3,
            ParsedHarnessResult {
                covers_satisfied: Some(3),
                covers_total: Some(3),
                ..ParsedHarnessResult::default()
            }
        ));
        assert!(!covers_match(
            3,
            ParsedHarnessResult {
                covers_satisfied: Some(2),
                covers_total: Some(3),
                ..ParsedHarnessResult::default()
            }
        ));
    }
}
