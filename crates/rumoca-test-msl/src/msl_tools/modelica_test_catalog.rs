use anyhow::{Context, Result, bail};
use clap::Parser;
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::PathBuf;

use super::common::MslPaths;

#[derive(Debug, Parser, Clone)]
pub struct Args {
    /// MSL results JSON (defaults to target/msl/results/msl_results.json)
    #[arg(long)]
    pub results: Option<PathBuf>,
    /// Markdown catalog output path
    #[arg(long)]
    pub out: Option<PathBuf>,
    /// JSON catalog output path
    #[arg(long = "json-out")]
    pub json_out: Option<PathBuf>,
    /// Optional output path for an expanded target-list JSON seeded from ModelicaTest representatives
    #[arg(long = "promote-targets-out")]
    pub promote_targets_out: Option<PathBuf>,
    /// Base target-list JSON to extend when --promote-targets-out is used
    #[arg(long = "base-targets")]
    pub base_targets: Option<PathBuf>,
    /// Number of deterministic representatives to promote per semantic category
    #[arg(long = "per-category", default_value_t = 3)]
    pub per_category: usize,
}

#[derive(Debug, Deserialize)]
struct MslResults {
    #[serde(default)]
    model_results: Vec<MslModelResult>,
}

#[derive(Debug, Clone, Deserialize)]
struct MslModelResult {
    model_name: String,
    #[serde(default)]
    phase_reached: String,
    #[serde(default)]
    error_code: Option<String>,
}

#[derive(Debug, Serialize)]
struct ModelicaTestCatalog {
    source_results: String,
    total_modelica_test_models: usize,
    categories: BTreeMap<String, CategorySummary>,
}

#[derive(Debug, Default, Serialize)]
struct CategorySummary {
    count: usize,
    phase_counts: BTreeMap<String, usize>,
    error_code_counts: BTreeMap<String, usize>,
    examples: Vec<String>,
    representatives: Vec<String>,
}

pub fn run(args: Args) -> Result<()> {
    let paths = MslPaths::current();
    let results_path = args
        .results
        .unwrap_or_else(|| paths.results_dir.join("msl_results.json"));
    let out_path = args
        .out
        .unwrap_or_else(|| paths.results_dir.join("modelica_test_catalog.md"));
    let json_path = args
        .json_out
        .unwrap_or_else(|| paths.results_dir.join("modelica_test_catalog.json"));

    let catalog = build_catalog_with_limit(&results_path, args.per_category)?;
    write_outputs(&catalog, &out_path, &json_path)?;
    if let Some(promote_targets_out) = args.promote_targets_out {
        let base_targets = args.base_targets.unwrap_or_else(|| {
            paths
                .repo_root
                .join("crates/rumoca-test-msl/tests/msl_tests/msl_simulation_targets.json")
        });
        write_promoted_targets(&catalog, &base_targets, &promote_targets_out)?;
        println!(
            "Expanded ModelicaTest target list written to {}",
            promote_targets_out.display()
        );
    }
    println!(
        "ModelicaTest catalog written to {} and {}",
        out_path.display(),
        json_path.display()
    );
    Ok(())
}

#[cfg(test)]
fn build_catalog(results_path: &PathBuf) -> Result<ModelicaTestCatalog> {
    build_catalog_with_limit(results_path, 3)
}

fn build_catalog_with_limit(
    results_path: &PathBuf,
    representatives_per_category: usize,
) -> Result<ModelicaTestCatalog> {
    let text = fs::read_to_string(results_path)
        .with_context(|| format!("failed to read {}", results_path.display()))?;
    let results: MslResults = serde_json::from_str(&text)
        .with_context(|| format!("invalid JSON in {}", results_path.display()))?;
    let mut categories = BTreeMap::new();
    let mut candidates: BTreeMap<String, Vec<MslModelResult>> = BTreeMap::new();

    for result in results
        .model_results
        .iter()
        .filter(|result| result.model_name.starts_with("ModelicaTest."))
    {
        let category = classify_modelica_test_case(&result.model_name);
        let summary: &mut CategorySummary = categories.entry(category.to_string()).or_default();
        summary.count += 1;
        *summary
            .phase_counts
            .entry(result.phase_reached.clone())
            .or_insert(0) += 1;
        if let Some(error_code) = result.error_code.as_deref() {
            *summary
                .error_code_counts
                .entry(error_code.to_string())
                .or_insert(0) += 1;
        }
        if summary.examples.len() < 10 {
            summary.examples.push(result.model_name.clone());
        }
        candidates
            .entry(category.to_string())
            .or_default()
            .push(result.clone());
    }

    let total_modelica_test_models = categories.values().map(|summary| summary.count).sum();
    if total_modelica_test_models == 0 {
        bail!(
            "{} contains no ModelicaTest.* entries; run an MSL compile that includes the MSL-shipped ModelicaTest package first",
            results_path.display()
        );
    }

    for (category, mut entries) in candidates {
        entries.sort_by(|lhs, rhs| {
            modelica_test_phase_rank(&lhs.phase_reached)
                .cmp(&modelica_test_phase_rank(&rhs.phase_reached))
                .then_with(|| lhs.model_name.cmp(&rhs.model_name))
        });
        if let Some(summary) = categories.get_mut(&category) {
            summary.representatives = entries
                .into_iter()
                .take(representatives_per_category)
                .map(|entry| entry.model_name)
                .collect();
        }
    }

    Ok(ModelicaTestCatalog {
        source_results: results_path.display().to_string(),
        total_modelica_test_models,
        categories,
    })
}

fn modelica_test_phase_rank(phase: &str) -> usize {
    match phase {
        "Success" => 0,
        "Simulate" | "Simulation" | "Render" => 1,
        "Solve" | "Structural" => 2,
        "ToDae" | "Dae" => 3,
        "Flatten" => 4,
        "Typecheck" => 5,
        "Instantiate" => 6,
        "Resolve" => 7,
        "Parse" => 8,
        _ => 9,
    }
}

fn classify_modelica_test_case(model_name: &str) -> &'static str {
    let lower = model_name.to_ascii_lowercase();
    if lower.contains("redeclare") || lower.contains("modifier") || lower.contains("modification") {
        return "redeclare_modifiers";
    }
    if lower.contains("stream") || lower.contains("connect") || lower.contains("overdetermined") {
        return "connectors_streams_overconstrained";
    }
    if lower.contains("array") || lower.contains("for") {
        return "arrays_for_equations";
    }
    if lower.contains("initial")
        || lower.contains("event")
        || lower.contains("when")
        || lower.contains("pre")
        || lower.contains("reinit")
    {
        return "initialization_events";
    }
    if lower.contains("function") || lower.contains("external") || lower.contains("table") {
        return "functions_external_tables";
    }
    if lower.contains("clock") || lower.contains("stategraph") || lower.contains("statemachine") {
        return "clocks_state_machines";
    }
    if lower.contains("index") || lower.contains("pantelides") || lower.contains("higher") {
        return "higher_index_structural";
    }
    "other"
}

fn write_outputs(
    catalog: &ModelicaTestCatalog,
    out_path: &PathBuf,
    json_path: &PathBuf,
) -> Result<()> {
    if let Some(parent) = out_path.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("failed to create {}", parent.display()))?;
    }
    if let Some(parent) = json_path.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("failed to create {}", parent.display()))?;
    }
    fs::write(out_path, render_markdown(catalog))
        .with_context(|| format!("failed to write {}", out_path.display()))?;
    fs::write(json_path, serde_json::to_vec_pretty(catalog)?)
        .with_context(|| format!("failed to write {}", json_path.display()))?;
    Ok(())
}

fn write_promoted_targets(
    catalog: &ModelicaTestCatalog,
    base_targets_path: &PathBuf,
    out_path: &PathBuf,
) -> Result<()> {
    let mut targets = read_target_list_if_present(base_targets_path)?;
    let mut seen = targets.iter().cloned().collect::<BTreeSet<_>>();
    for representative in catalog
        .categories
        .values()
        .flat_map(|summary| summary.representatives.iter())
    {
        if seen.insert(representative.clone()) {
            targets.push(representative.clone());
        }
    }
    if let Some(parent) = out_path.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("failed to create {}", parent.display()))?;
    }
    fs::write(out_path, serde_json::to_vec_pretty(&targets)?)
        .with_context(|| format!("failed to write {}", out_path.display()))
}

fn read_target_list_if_present(path: &PathBuf) -> Result<Vec<String>> {
    if !path.is_file() {
        return Ok(Vec::new());
    }
    let text =
        fs::read_to_string(path).with_context(|| format!("failed to read {}", path.display()))?;
    serde_json::from_str(&text).with_context(|| format!("invalid JSON in {}", path.display()))
}

fn render_markdown(catalog: &ModelicaTestCatalog) -> String {
    let mut out = String::new();
    out.push_str("# ModelicaTest Catalog\n\n");
    out.push_str(&format!(
        "- Source results: `{}`\n- ModelicaTest models: {}\n\n",
        catalog.source_results, catalog.total_modelica_test_models
    ));
    out.push_str(
        "| Category | Count | Top phases | Top error codes | Representatives | Example cases |\n",
    );
    out.push_str("|---|---:|---|---|---|---|\n");
    for (category, summary) in &catalog.categories {
        out.push_str(&format!(
            "| {} | {} | {} | {} | {} | {} |\n",
            category,
            summary.count,
            render_counts(&summary.phase_counts),
            render_counts(&summary.error_code_counts),
            summary.representatives.join("<br>"),
            summary.examples.join("<br>")
        ));
    }
    out
}

fn render_counts(counts: &BTreeMap<String, usize>) -> String {
    if counts.is_empty() {
        return "-".to_string();
    }
    let mut pairs: Vec<_> = counts.iter().collect();
    pairs.sort_by(|(a_key, a_count), (b_key, b_count)| {
        b_count.cmp(a_count).then_with(|| a_key.cmp(b_key))
    });
    pairs
        .into_iter()
        .take(5)
        .map(|(key, count)| format!("{key}: {count}"))
        .collect::<Vec<_>>()
        .join("<br>")
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;
    use tempfile::tempdir;

    #[test]
    fn classifier_maps_expected_modelica_test_areas() {
        assert_eq!(
            classify_modelica_test_case("ModelicaTest.Fluid.Connectors.StreamProbe"),
            "connectors_streams_overconstrained"
        );
        assert_eq!(
            classify_modelica_test_case("ModelicaTest.Redeclare.ModifierCase"),
            "redeclare_modifiers"
        );
        assert_eq!(
            classify_modelica_test_case("ModelicaTest.Arrays.ForEquation"),
            "arrays_for_equations"
        );
        assert_eq!(
            classify_modelica_test_case("ModelicaTest.Events.WhenPreReinit"),
            "initialization_events"
        );
    }

    #[test]
    fn catalog_counts_modelica_test_cases_by_category() {
        let dir = tempdir().expect("tempdir");
        let path = dir.path().join("msl_results.json");
        fs::write(
            &path,
            serde_json::to_vec_pretty(&json!({
                "model_results": [
                    {
                        "model_name": "ModelicaTest.Redeclare.ModifierCase",
                        "phase_reached": "Success"
                    },
                    {
                        "model_name": "ModelicaTest.Events.WhenCase",
                        "phase_reached": "Flatten",
                        "error_code": "unsupported-feature:events"
                    },
                    {
                        "model_name": "Modelica.Blocks.Examples.PID_Controller",
                        "phase_reached": "Success"
                    }
                ]
            }))
            .expect("json"),
        )
        .expect("write fixture");

        let catalog = build_catalog(&path).expect("catalog");
        assert_eq!(catalog.total_modelica_test_models, 2);
        assert_eq!(catalog.categories["redeclare_modifiers"].count, 1);
        assert_eq!(catalog.categories["initialization_events"].count, 1);
        assert_eq!(
            catalog.categories["redeclare_modifiers"].representatives,
            vec!["ModelicaTest.Redeclare.ModifierCase"]
        );
        assert_eq!(
            catalog.categories["initialization_events"].error_code_counts["unsupported-feature:events"],
            1
        );
    }

    #[test]
    fn promoted_targets_extend_base_list_with_representatives() {
        let dir = tempdir().expect("tempdir");
        let results_path = dir.path().join("msl_results.json");
        let base_path = dir.path().join("base.json");
        let out_path = dir.path().join("expanded.json");
        fs::write(
            &results_path,
            serde_json::to_vec_pretty(&json!({
                "model_results": [
                    {
                        "model_name": "ModelicaTest.Arrays.ForEquation",
                        "phase_reached": "Flatten"
                    },
                    {
                        "model_name": "ModelicaTest.Arrays.ArraySuccess",
                        "phase_reached": "Success"
                    },
                    {
                        "model_name": "ModelicaTest.Events.WhenCase",
                        "phase_reached": "Success"
                    }
                ]
            }))
            .expect("json"),
        )
        .expect("write results");
        fs::write(
            &base_path,
            serde_json::to_vec_pretty(&vec!["Modelica.Blocks.Examples.A"]).expect("json"),
        )
        .expect("write base");

        let catalog = build_catalog_with_limit(&results_path, 1).expect("catalog");
        write_promoted_targets(&catalog, &base_path, &out_path).expect("promote targets");
        let targets: Vec<String> =
            serde_json::from_slice(&fs::read(&out_path).expect("read targets")).expect("targets");

        assert_eq!(
            targets,
            vec![
                "Modelica.Blocks.Examples.A",
                "ModelicaTest.Arrays.ArraySuccess",
                "ModelicaTest.Events.WhenCase"
            ]
        );
    }
}
