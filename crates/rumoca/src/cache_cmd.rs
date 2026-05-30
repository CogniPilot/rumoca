//! `rumoca cache {status,prune}` handlers. Split out of `main.rs` to keep it
//! under the SPEC_0021 file-size limit.

use anyhow::Result;
use rumoca_compile::cache;

use crate::{CacheArgs, CacheCommand, CachePruneArgs, CacheStatusArgs};

pub(crate) fn run_cache(args: CacheArgs) -> Result<()> {
    match args.command {
        CacheCommand::Status(status_args) => run_cache_status(status_args),
        CacheCommand::Prune(prune_args) => run_cache_prune(prune_args),
    }
}

fn run_cache_status(args: CacheStatusArgs) -> Result<()> {
    let status = cache::cache_status(args.root.as_deref())?;
    println!("Cache root: {}", status.root.display());
    println!(
        "Cache size: {} across {} files and {} directories",
        format_bytes(status.total_bytes),
        status.file_count,
        status.dir_count
    );
    if !status.subcaches.is_empty() {
        println!("Top-level cache families:");
        for subcache in &status.subcaches {
            println!(
                "  {}: {} across {} files and {} directories",
                subcache.name,
                format_bytes(subcache.total_bytes),
                subcache.file_count,
                subcache.dir_count
            );
        }
    }
    Ok(())
}

fn run_cache_prune(args: CachePruneArgs) -> Result<()> {
    let max_bytes = match args.max_size.as_deref() {
        Some(raw) => cache::parse_byte_size(raw)?,
        None => cache::DEFAULT_CACHE_MAX_BYTES,
    };
    let max_age = match args.max_age_days {
        Some(days) => Some(cache::parse_max_age_days(&days.to_string())?),
        None => None,
    };
    let family_budgets = if args.family_max.is_empty() {
        Vec::new()
    } else {
        args.family_max
            .iter()
            .map(|raw| cache::parse_family_budget(raw))
            .collect::<Result<Vec<_>>>()?
    };
    let options = cache::CachePruneOptions {
        max_bytes,
        max_age,
        family_budgets,
    };
    let report = cache::prune_cache_with_options(args.root.as_deref(), &options, args.dry_run)?;
    println!("Cache root: {}", report.before.root.display());
    println!(
        "Cache prune{}: {} -> {} (limit {})",
        if report.dry_run { " dry-run" } else { "" },
        format_bytes(report.before.total_bytes),
        format_bytes(report.after.total_bytes),
        format_bytes(report.max_bytes)
    );
    println!(
        "Removed {} files totaling {}",
        report.removed_files,
        format_bytes(report.removed_bytes)
    );
    if let Some(max_age) = options.max_age {
        println!("Max age: {} days", max_age.as_secs() / (24 * 60 * 60));
    }
    if !options.family_budgets.is_empty() {
        println!("Family budgets:");
        for budget in &options.family_budgets {
            println!("  {}: {}", budget.family, format_bytes(budget.max_bytes));
        }
    }
    Ok(())
}

fn format_bytes(bytes: u64) -> String {
    const UNITS: [&str; 5] = ["B", "KiB", "MiB", "GiB", "TiB"];
    let mut value = bytes as f64;
    let mut unit = 0;
    while value >= 1024.0 && unit + 1 < UNITS.len() {
        value /= 1024.0;
        unit += 1;
    }
    if unit == 0 {
        format!("{bytes} B")
    } else {
        format!("{value:.1} {} ({bytes} B)", UNITS[unit])
    }
}
