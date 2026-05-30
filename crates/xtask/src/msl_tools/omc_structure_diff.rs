//! `xtask repo msl omc-structure` — cross-reference rumoca's structural analysis
//! (matching / BLT / coupled SCCs / tearing) against OpenModelica's for one MSL
//! model, so a state-selection or tearing divergence is visible automatically
//! instead of hand-reading OMC's generated `_info.json` / `_03lsy.c`.
//!
//! OMC's `<model>_info.json` (transformational-debugger info) exposes the BLT
//! result directly: `assign` equations are scalar blocks, and a `tornsystem`
//! equation is a coupled (algebraic-loop) block whose `unknowns` is the block
//! size, `display` is linear/non-linear, and `defines` is the tear (iteration)
//! variable; its `torn` children are the causally-solved members. We compare
//! that to [`rumoca_sim::structural_report_for_dae`] (which uses the same
//! matching / BLT / tearing the compiler does).

use std::collections::BTreeSet;

use anyhow::{Context, Result, bail};
use clap::Args as ClapArgs;
use rumoca_compile::compile::{Session, SessionConfig, SourceRootKind};
use serde_json::Value;

use super::common::MslPaths;

#[derive(Debug, Clone, ClapArgs)]
pub struct Args {
    /// Exact MSL model name to cross-reference.
    #[arg(long)]
    model: String,
}

/// One coupled (torn) block extracted from OMC's `_info.json`.
#[derive(Debug)]
struct OmcBlock {
    size: usize,
    kind: String,
    tear_vars: Vec<String>,
}

pub fn run(args: Args) -> Result<()> {
    let paths = MslPaths::current();

    // rumoca side: compile in-process (per-model strict-reachable, tolerating
    // unrelated MSL packages the way the worker does) and run the same
    // structural analysis the compiler uses.
    if !paths.msl_dir.is_dir() {
        bail!(
            "MSL cache not found at {}; run an MSL test once to populate target/msl",
            paths.msl_dir.display()
        );
    }
    let mut session = Session::new(SessionConfig::default());
    let load = session.load_source_root_tolerant(
        "msl",
        SourceRootKind::DurableExternal,
        &paths.msl_dir,
        None,
    );
    if !load.diagnostics.is_empty() {
        bail!(
            "failed to load MSL from {}: {}",
            paths.msl_dir.display(),
            load.diagnostics.join("; ")
        );
    }
    // Strict-reachable + recovery: only the target model's reachable files gate
    // success, so unrelated MSL packages (Fluid `Medium`, MultiBody `world`, ...)
    // don't fail the compile.
    let compiled = session
        .compile_model_dae_strict_reachable_uncached_with_recovery(&args.model)
        .map_err(|error| anyhow::anyhow!("rumoca compile failed for '{}': {error}", args.model))?;
    let report =
        rumoca_sim::structural_report_for_dae(&compiled.dae, &rumoca_sim::SimOptions::default())
            .map_err(|error| anyhow::anyhow!("rumoca structural analysis failed: {error}"))?;

    // OMC side: parse the transformational-debugger info emitted by the OMC
    // simulation reference run.
    let info_path = paths.sim_work_dir.join(format!("{}_info.json", args.model));
    let info_text = std::fs::read_to_string(&info_path).with_context(|| {
        format!(
            "OMC info not found at {} — run `xtask repo msl omc-simulation-reference` first",
            info_path.display()
        )
    })?;
    let info: Value = serde_json::from_str(&info_text)
        .with_context(|| format!("parse {}", info_path.display()))?;
    let omc_blocks = parse_omc_torn_systems(&info);

    print_report(&args.model, &report, &omc_blocks);
    Ok(())
}

/// Extract the coupled (`tornsystem`) blocks from OMC's `_info.json` (regular,
/// non-initial section).
fn parse_omc_torn_systems(info: &Value) -> Vec<OmcBlock> {
    let Some(equations) = info.get("equations").and_then(Value::as_array) else {
        return Vec::new();
    };
    equations
        .iter()
        .filter(|eq| eq.get("section").and_then(Value::as_str) != Some("initial"))
        .filter(|eq| eq.get("tag").and_then(Value::as_str) == Some("tornsystem"))
        .map(|eq| OmcBlock {
            size: eq
                .get("unknowns")
                .and_then(Value::as_u64)
                .unwrap_or_default() as usize,
            kind: eq
                .get("display")
                .and_then(Value::as_str)
                .unwrap_or("?")
                .to_string(),
            tear_vars: eq
                .get("defines")
                .and_then(Value::as_array)
                .map(|defines| {
                    defines
                        .iter()
                        .filter_map(Value::as_str)
                        .map(str::to_string)
                        .collect()
                })
                .unwrap_or_default(),
        })
        .collect()
}

fn print_report(model: &str, report: &rumoca_sim::StructuralReport, omc_blocks: &[OmcBlock]) {
    use rumoca_sim::BlockReport;

    println!("omc-structure: model `{model}`\n");

    // rumoca coupled blocks + tear variables.
    let mut rumoca_tear: BTreeSet<String> = BTreeSet::new();
    println!("rumoca coupled blocks ({}):", report.coupled_block_count());
    for block in &report.blocks {
        if let BlockReport::Coupled {
            unknowns, tearing, ..
        } = block
        {
            let tears = tearing
                .as_ref()
                .map(|tearing| tearing.tear_vars.clone())
                .unwrap_or_default();
            rumoca_tear.extend(tears.iter().cloned());
            println!(
                "  {}x SCC  tear: [{}]  unknowns: [{}]",
                unknowns.len(),
                tears.join(", "),
                unknowns.join(", ")
            );
        }
    }

    // OMC torn systems + tear variables.
    let mut omc_tear: BTreeSet<String> = BTreeSet::new();
    println!("\nOMC torn systems ({}):", omc_blocks.len());
    for block in omc_blocks {
        omc_tear.extend(block.tear_vars.iter().cloned());
        println!(
            "  {}x {} system  tear: [{}]",
            block.size,
            block.kind,
            block.tear_vars.join(", ")
        );
    }

    // Diff of the tear-variable sets — the actionable cross-reference.
    println!("\ntear-variable cross-reference:");
    let common: Vec<_> = rumoca_tear.intersection(&omc_tear).cloned().collect();
    let rumoca_only: Vec<_> = rumoca_tear.difference(&omc_tear).cloned().collect();
    let omc_only: Vec<_> = omc_tear.difference(&rumoca_tear).cloned().collect();
    println!("  agree (torn by both):  [{}]", common.join(", "));
    println!("  rumoca-only tears:     [{}]", rumoca_only.join(", "));
    println!("  OMC-only tears:        [{}]", omc_only.join(", "));

    let verdict = if report.coupled_block_count() == omc_blocks.len()
        && rumoca_only.is_empty()
        && omc_only.is_empty()
    {
        "MATCH: coupled-block count and tear variables agree"
    } else {
        "DIVERGENCE: coupled-block structure or tearing differs (see above)"
    };
    println!("\n{verdict}");
    println!(
        "note: rumoca's blocks are reported pre-elimination, so they are larger than \n      \
         OMC's post-alias-elimination systems; the tear-variable diff is the key signal."
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_regular_tornsystems_only() {
        // Mirrors OMC `_info.json`: an initial-section tornsystem is ignored,
        // and the regular one yields size/kind/tear-var.
        let info = serde_json::json!({
            "equations": [
                { "eqIndex": 1, "section": "initial", "tag": "tornsystem",
                  "display": "linear", "unknowns": 2, "defines": ["a"] },
                { "eqIndex": 5, "section": "regular", "tag": "assign", "defines": ["b"] },
                { "eqIndex": 9, "section": "regular", "tag": "tornsystem",
                  "display": "non-linear", "unknowns": 3, "defines": ["zDiode1.v"] }
            ]
        });
        let blocks = parse_omc_torn_systems(&info);
        assert_eq!(blocks.len(), 1, "only the regular tornsystem counts");
        assert_eq!(blocks[0].size, 3);
        assert_eq!(blocks[0].kind, "non-linear");
        assert_eq!(blocks[0].tear_vars, vec!["zDiode1.v".to_string()]);
    }

    #[test]
    fn handles_missing_equations_array() {
        assert!(parse_omc_torn_systems(&serde_json::json!({})).is_empty());
    }
}
