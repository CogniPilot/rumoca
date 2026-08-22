//! Manifest-driven bounded verification with the repository-pinned Kani release.

use anyhow::{Context, Result, ensure};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::{Component, Path, PathBuf};
use std::process::Command;
use std::time::Instant;
use syn::visit::Visit;

const MANIFEST_PATH: &str = "infra/verification/kani-proofs.json";
/// Bumped 2 -> 3 when `assumptions` became a mandatory per-proof field: a
/// version-2 manifest omits it and is no longer admissible.
const MANIFEST_SCHEMA_VERSION: u32 = 3;
const REQUIRED_KANI_VERSION: &str = "0.67.0";
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
    /// Trusted premises the harness relies on, required by SPEC_0037 §3a.
    assumptions: Vec<String>,
    bound: ProofBound,
    covers: u32,
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct DiscoveredProof {
    source: PathBuf,
    unwind: Option<u32>,
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
    run_workspace_proofs(root, &selected, manifest.proofs.len(), shard)
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
    let packages = workspace_packages(root)?;
    let discovered = discover_workspace_proofs(root, &packages)?;
    let mut selectors = BTreeSet::new();
    for proof in &manifest.proofs {
        ensure!(
            packages.contains_key(&proof.package),
            "Kani proof names non-workspace package {}",
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
        validate_assumptions(proof)?;
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
        let package_root = packages
            .get(&proof.package)
            .expect("workspace membership checked above");
        ensure!(
            source.starts_with(package_root),
            "Kani proof source {} is outside package {} at {}",
            proof.source,
            proof.package,
            package_root.display()
        );
        let key = (proof.package.clone(), proof.harness.clone());
        let declared = discovered.get(&key).with_context(|| {
            format!(
                "Kani harness {}::{} was not found by the Rust parser",
                proof.package, proof.harness
            )
        })?;
        ensure!(
            declared.source == source,
            "Kani harness {}::{} is declared in {}, not {}",
            proof.package,
            proof.harness,
            declared.source.display(),
            proof.source
        );
        validate_declared_bound(proof, declared.unwind)?;
    }
    let listed: BTreeSet<_> = manifest
        .proofs
        .iter()
        .map(|proof| (proof.package.clone(), proof.harness.clone()))
        .collect();
    let discovered_keys = discovered.keys().cloned().collect::<BTreeSet<_>>();
    let missing: Vec<_> = discovered_keys.difference(&listed).cloned().collect();
    let extra: Vec<_> = listed.difference(&discovered_keys).cloned().collect();
    ensure!(
        missing.is_empty() && extra.is_empty(),
        "Kani manifest inventory differs from the workspace: missing={missing:?}, extra={extra:?}"
    );
    Ok(())
}

fn workspace_packages(root: &Path) -> Result<BTreeMap<String, std::path::PathBuf>> {
    let workspace_manifest = fs::read_to_string(root.join("Cargo.toml"))
        .context("failed to read workspace Cargo.toml for Kani discovery")?;
    let workspace: toml::Value = toml::from_str(&workspace_manifest)
        .context("failed to parse workspace Cargo.toml for Kani discovery")?;
    let members = workspace
        .get("workspace")
        .and_then(|value| value.get("members"))
        .and_then(toml::Value::as_array)
        .context("workspace.members is missing from Cargo.toml")?;
    let mut packages = BTreeMap::new();
    for member in members {
        let member = member
            .as_str()
            .context("workspace member must be a literal path")?;
        ensure!(
            !member.contains('*'),
            "Kani discovery requires explicit workspace members, found glob `{member}`"
        );
        let manifest_path = root.join(member).join("Cargo.toml");
        let manifest_text = fs::read_to_string(&manifest_path)
            .with_context(|| format!("failed to read {}", manifest_path.display()))?;
        let manifest: toml::Value = toml::from_str(&manifest_text)
            .with_context(|| format!("failed to parse {}", manifest_path.display()))?;
        let name = manifest
            .get("package")
            .and_then(|value| value.get("name"))
            .and_then(toml::Value::as_str)
            .with_context(|| format!("{} has no package.name", manifest_path.display()))?;
        ensure!(
            packages
                .insert(name.to_string(), Path::new(member).to_path_buf())
                .is_none(),
            "duplicate workspace package `{name}`"
        );
    }
    Ok(packages)
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

/// SPEC_0037 §3a requires every manifest entry to identify the trusted
/// premises under which its property holds, so an entry without at least one
/// non-empty assumption is inadmissible.
fn validate_assumptions(proof: &KaniProof) -> Result<()> {
    ensure!(
        !proof.assumptions.is_empty(),
        "{} declares no assumptions",
        proof.harness
    );
    ensure!(
        proof
            .assumptions
            .iter()
            .all(|assumption| !assumption.trim().is_empty()),
        "{} has an empty assumption",
        proof.harness
    );
    Ok(())
}

fn validate_declared_bound(proof: &KaniProof, declared_unwind: Option<u32>) -> Result<()> {
    match &proof.bound {
        ProofBound::Unwind { value, domain } => {
            ensure!(*value > 0, "{} has a zero unwind bound", proof.harness);
            ensure!(
                !domain.trim().is_empty(),
                "{} has an empty bound domain",
                proof.harness
            );
            ensure!(
                declared_unwind == Some(*value),
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
                declared_unwind.is_none(),
                "{} declares a finite domain but has an unwind attribute",
                proof.harness
            );
        }
    }
    Ok(())
}

fn discover_workspace_proofs(
    root: &Path,
    packages: &BTreeMap<String, PathBuf>,
) -> Result<BTreeMap<(String, String), DiscoveredProof>> {
    let mut harnesses = BTreeMap::new();
    for (package, package_root) in packages {
        for entry in walkdir::WalkDir::new(root.join(package_root))
            .into_iter()
            .filter_entry(|entry| entry.file_name() != "target")
        {
            let entry = entry.with_context(|| {
                format!("failed to walk {} for Kani proofs", package_root.display())
            })?;
            if !entry.file_type().is_file()
                || entry.path().extension().is_none_or(|ext| ext != "rs")
            {
                continue;
            }
            let source = fs::read_to_string(entry.path())
                .with_context(|| format!("failed to read {}", entry.path().display()))?;
            let syntax = syn::parse_file(&source)
                .with_context(|| format!("failed to parse {} as Rust", entry.path().display()))?;
            let relative_source = package_root.join(
                entry
                    .path()
                    .strip_prefix(root.join(package_root))
                    .with_context(|| {
                        format!("{} is outside its package root", entry.path().display())
                    })?,
            );
            for (harness, unwind) in discover_file_proofs(&syntax, entry.path())? {
                ensure!(
                    harnesses
                        .insert(
                            (package.clone(), harness.clone()),
                            DiscoveredProof {
                                source: relative_source.clone(),
                                unwind,
                            },
                        )
                        .is_none(),
                    "duplicate Kani harness `{package}::{harness}`"
                );
            }
        }
    }
    Ok(harnesses)
}

fn discover_file_proofs(syntax: &syn::File, path: &Path) -> Result<Vec<(String, Option<u32>)>> {
    let mut visitor = KaniProofVisitor::default();
    visitor.visit_file(syntax);
    if let Some(error) = visitor.error {
        return Err(error).with_context(|| format!("invalid Kani attribute in {}", path.display()));
    }
    Ok(visitor.proofs)
}

#[derive(Default)]
struct KaniProofVisitor {
    proofs: Vec<(String, Option<u32>)>,
    error: Option<anyhow::Error>,
}

impl<'ast> Visit<'ast> for KaniProofVisitor {
    fn visit_item_fn(&mut self, function: &'ast syn::ItemFn) {
        if self.error.is_some() {
            return;
        }
        let is_proof = function
            .attrs
            .iter()
            .any(|attribute| is_kani_attribute(attribute, "proof"));
        if is_proof {
            match kani_unwind(&function.attrs) {
                Ok(unwind) => self.proofs.push((function.sig.ident.to_string(), unwind)),
                Err(error) => self.error = Some(error),
            }
        }
        syn::visit::visit_item_fn(self, function);
    }
}

fn kani_unwind(attributes: &[syn::Attribute]) -> Result<Option<u32>> {
    let mut unwind = None;
    for attribute in attributes {
        if !is_kani_attribute(attribute, "unwind") {
            continue;
        }
        let value = attribute
            .parse_args::<syn::LitInt>()
            .context("kani::unwind requires one integer literal")?
            .base10_parse::<u32>()
            .context("kani::unwind value is outside u32")?;
        ensure!(
            unwind.replace(value).is_none(),
            "duplicate kani::unwind attribute"
        );
    }
    Ok(unwind)
}

fn is_kani_attribute(attribute: &syn::Attribute, name: &str) -> bool {
    let mut segments = attribute.path().segments.iter();
    segments
        .next()
        .is_some_and(|segment| segment.ident == "kani")
        && segments.next().is_some_and(|segment| segment.ident == name)
        && segments.next().is_none()
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
    package_elapsed_seconds: &'a BTreeMap<String, f64>,
    command_success: bool,
    cover_obligations_satisfied: bool,
    manifest_proof_count: usize,
    shard: Option<(usize, usize)>,
}

struct KaniPackageOutput {
    combined: String,
    elapsed_seconds: f64,
    success: bool,
}

fn run_workspace_proofs(
    root: &Path,
    manifest: &KaniProofManifest,
    manifest_proof_count: usize,
    shard: Option<(usize, usize)>,
) -> Result<()> {
    let packages = manifest
        .proofs
        .iter()
        .map(|proof| proof.package.clone())
        .collect::<BTreeSet<_>>();
    let mut parsed = vec![ParsedHarnessResult::default(); manifest.proofs.len()];
    let mut package_elapsed_seconds = BTreeMap::new();
    let mut command_success = true;
    let mut kani_summary = Vec::new();
    for package in packages {
        let package_manifest = KaniProofManifest {
            schema_version: manifest.schema_version,
            kani_version: manifest.kani_version.clone(),
            proofs: manifest
                .proofs
                .iter()
                .filter(|proof| proof.package == package)
                .cloned()
                .collect(),
        };
        let output = run_kani_package(root, &package, &package_manifest, shard.is_some())?;
        let package_results = parse_kani_results(&output.combined, &package_manifest);
        merge_package_results(manifest, &package_manifest, package_results, &mut parsed);
        kani_summary.extend(
            output
                .combined
                .lines()
                .filter(|line| {
                    line.contains("SUMMARY")
                        || line.contains("VERIFICATION")
                        || line.contains("Verification Time:")
                })
                .map(str::trim)
                .filter(|line| !line.is_empty())
                .map(|line| format!("[{package}] {line}")),
        );
        command_success &= output.success;
        package_elapsed_seconds.insert(package, output.elapsed_seconds);
    }
    let cover_obligations_satisfied = manifest
        .proofs
        .iter()
        .zip(&parsed)
        .all(|(proof, result)| covers_match(proof.covers, *result));
    write_summary(
        root,
        manifest,
        KaniRunSummary {
            parsed: &parsed,
            kani_summary: &kani_summary,
            package_elapsed_seconds: &package_elapsed_seconds,
            command_success,
            cover_obligations_satisfied,
            manifest_proof_count,
            shard,
        },
    )?;
    ensure!(command_success, "one or more Kani proof packages failed");
    for (proof, result) in manifest.proofs.iter().zip(&parsed) {
        ensure!(
            result.success == Some(true) && result.elapsed_seconds.is_some(),
            "successful Kani output lacked a complete result for {}::{}",
            proof.package,
            proof.harness
        );
    }
    ensure!(
        cover_obligations_satisfied,
        "one or more Kani reachability-cover obligations were not satisfied"
    );
    Ok(())
}

fn merge_package_results(
    selected: &KaniProofManifest,
    package: &KaniProofManifest,
    results: Vec<ParsedHarnessResult>,
    merged: &mut [ParsedHarnessResult],
) {
    for (proof, result) in package.proofs.iter().zip(results) {
        let index = selected
            .proofs
            .iter()
            .position(|candidate| {
                candidate.package == proof.package && candidate.harness == proof.harness
            })
            .expect("package manifest is a subset of the selected manifest");
        merged[index] = result;
    }
}

fn run_kani_package(
    root: &Path,
    package: &str,
    manifest: &KaniProofManifest,
    select_harnesses: bool,
) -> Result<KaniPackageOutput> {
    let mut command = Command::new("cargo");
    command
        // Kani 0.67's parallel text output does not identify the harness on
        // each result block, so per-harness timing and cover attribution is
        // fail-closed only when those blocks remain sequential. Individual
        // CBMC processes can also consume most of a verification host's RAM.
        .args([
            "kani",
            "--package",
            package,
            "--no-default-features",
            "--jobs",
            "1",
        ])
        .current_dir(root);
    if select_harnesses {
        for proof in &manifest.proofs {
            command.args(["--harness", &proof.harness]);
        }
    }
    crate::resource_budget::apply_to_child(&mut command);
    let started = Instant::now();
    let output = command
        .output()
        .with_context(|| format!("failed to execute Kani proof package {package}"))?;
    let elapsed_seconds = started.elapsed().as_secs_f64();
    print!("{}", String::from_utf8_lossy(&output.stdout));
    eprint!("{}", String::from_utf8_lossy(&output.stderr));
    let combined = format!(
        "{}\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    Ok(KaniPackageOutput {
        combined,
        elapsed_seconds,
        success: output.status.success(),
    })
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
                .or_else(|| run.package_elapsed_seconds.get(&proof.package).copied())
                .unwrap_or_default();
            let elapsed_source = if result.elapsed_seconds.is_some() {
                "kani_harness"
            } else {
                "package_total_fallback"
            };
            serde_json::json!({
                "package": proof.package,
                "harness": proof.harness,
                "claims": proof.claims,
                "selection": proof.selection,
                "assumptions": proof.assumptions,
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
        "schema_version": 2,
        "verifier": "kani",
        "kani_version": manifest.kani_version,
        "packages": run.package_elapsed_seconds.keys().collect::<Vec<_>>(),
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
        KaniProof, MANIFEST_SCHEMA_VERSION, ParsedHarnessResult, REQUIRED_KANI_VERSION,
        covers_match, discover_file_proofs, discover_workspace_proofs, load_manifest,
        parse_kani_results, select_manifest_shard, validate_assumptions, validate_manifest,
        workspace_packages,
    };
    use std::collections::BTreeSet;
    use std::fs;
    use std::path::Path;

    #[test]
    fn checked_in_manifest_names_every_workspace_proof() {
        let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
        let manifest = load_manifest(&root).expect("checked-in Kani manifest should be valid");
        assert_eq!(manifest.kani_version, REQUIRED_KANI_VERSION);
        let listed: BTreeSet<_> = manifest
            .proofs
            .iter()
            .map(|proof| (proof.package.clone(), proof.harness.clone()))
            .collect();
        let packages = workspace_packages(&root).expect("discover workspace packages");
        let discovered = discover_workspace_proofs(&root, &packages)
            .expect("discover workspace proofs")
            .into_keys()
            .collect::<BTreeSet<_>>();
        assert_eq!(
            listed, discovered,
            "manifest must list every workspace proof"
        );
    }

    #[test]
    fn workspace_discovery_keys_equal_harness_names_by_package() {
        let temp = tempfile::tempdir().expect("temporary workspace");
        fs::write(
            temp.path().join("Cargo.toml"),
            "[workspace]\nmembers = ['alpha', 'beta']\nresolver = '2'\n",
        )
        .expect("write workspace manifest");
        for (directory, package) in [("alpha", "proof-alpha"), ("beta", "proof-beta")] {
            let root = temp.path().join(directory);
            fs::create_dir_all(root.join("src")).expect("create package source");
            fs::write(
                root.join("Cargo.toml"),
                format!("[package]\nname = '{package}'\nversion = '0.0.0'\nedition = '2024'\n"),
            )
            .expect("write package manifest");
            fs::write(
                root.join("src/lib.rs"),
                "#[kani::proof]\nfn same_name() {}\n",
            )
            .expect("write proof source");
        }
        let packages = workspace_packages(temp.path()).expect("discover packages");
        assert_eq!(packages.len(), 2);
        assert_eq!(
            discover_workspace_proofs(temp.path(), &packages)
                .expect("discover proofs")
                .into_keys()
                .collect::<BTreeSet<_>>(),
            BTreeSet::from([
                ("proof-alpha".to_string(), "same_name".to_string()),
                ("proof-beta".to_string(), "same_name".to_string()),
            ])
        );
    }

    #[test]
    fn rust_parser_discovers_attributes_independent_of_formatting() {
        let source = r##"
            const MISLEADING: &str = "#[kani::proof] fn not_a_proof() {}";
            // #[kani::proof]
            // fn commented_out() {}
            #[kani :: proof]
            #[kani :: unwind(7)]
            fn actual_proof() {}
        "##;
        let syntax = syn::parse_file(source).expect("parse planted Rust source");
        assert_eq!(
            discover_file_proofs(&syntax, Path::new("planted.rs"))
                .expect("discover syntax-owned proof inventory"),
            vec![("actual_proof".to_string(), Some(7))]
        );
    }

    #[test]
    fn checked_in_manifest_states_assumptions_for_every_harness() {
        let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
        let manifest = load_manifest(&root).expect("checked-in Kani manifest should be valid");
        for proof in &manifest.proofs {
            assert!(
                !proof.assumptions.is_empty(),
                "{} must state its assumptions",
                proof.harness
            );
        }
    }

    /// A schema-3 manifest entry with every field except `assumptions`, which
    /// each caller supplies to exercise one arm of the assumptions rule.
    fn example_manifest_entry() -> serde_json::Value {
        serde_json::json!({
            "package": "rumoca-solver",
            "harness": "example",
            "source": "crates/rumoca-solver/src/verification.rs",
            "claims": ["SIM-010"],
            "selection": {
                "production_kernel": "kernel",
                "symbolic_inputs": "inputs",
                "exhaustive_test_infeasible_because": "barrier",
                "counterexample_means": "meaning"
            },
            "bound": { "kind": "finite_domain", "domain": "domain" },
            "covers": 0
        })
    }

    #[test]
    fn manifest_entry_without_assumptions_is_rejected() {
        let entry = example_manifest_entry();
        let parsed = serde_json::from_value::<KaniProof>(entry.clone());
        assert!(
            parsed.is_err(),
            "a manifest entry without assumptions must not parse"
        );

        let mut with_assumptions = entry;
        with_assumptions["assumptions"] = serde_json::json!(["   "]);
        let proof = serde_json::from_value::<KaniProof>(with_assumptions)
            .expect("an entry with an assumptions list parses");
        assert!(
            validate_assumptions(&proof).is_err(),
            "a blank assumption must not satisfy the assumptions requirement"
        );
    }

    /// `assumptions` became mandatory at schema 3, so a manifest still
    /// declaring the schema-2 shape must be rejected outright rather than
    /// silently validated against the newer rules.
    #[test]
    fn superseded_schema_version_two_manifest_is_rejected() {
        assert_eq!(MANIFEST_SCHEMA_VERSION, 3);
        let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
        let mut manifest = load_manifest(&root).expect("checked-in Kani manifest should be valid");
        manifest.schema_version = 2;
        let error = validate_manifest(&root, &manifest)
            .expect_err("a schema-2 manifest must not validate against schema 3");
        assert!(
            error
                .to_string()
                .contains("unsupported Kani proof manifest schema 2"),
            "unexpected rejection reason: {error}"
        );
    }

    #[test]
    fn manifest_entry_with_empty_assumptions_list_is_rejected() {
        let mut proof = example_manifest_entry();
        proof["assumptions"] = serde_json::json!([]);
        let proof = serde_json::from_value::<KaniProof>(proof)
            .expect("an entry with an empty assumptions list parses");
        let error = validate_assumptions(&proof)
            .expect_err("an empty assumptions list must not satisfy the assumptions requirement");
        assert!(
            error.to_string().contains("declares no assumptions"),
            "unexpected rejection reason: {error}"
        );
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
