use std::collections::BTreeMap;
use std::path::PathBuf;

use anyhow::{Context as _, Result};

#[derive(Default)]
pub(crate) struct LcovFile {
    pub(crate) lines: BTreeMap<u32, u64>,
    pub(crate) function_lines: BTreeMap<String, u32>,
    pub(crate) function_counts: BTreeMap<String, u64>,
}

pub(crate) fn parse_lcov(source: &str) -> Result<BTreeMap<PathBuf, LcovFile>> {
    let mut files: BTreeMap<PathBuf, LcovFile> = BTreeMap::new();
    let mut current: Option<PathBuf> = None;
    for line in source.lines() {
        if let Some(path) = line.strip_prefix("SF:") {
            current = Some(PathBuf::from(path));
            files.entry(PathBuf::from(path)).or_default();
        } else if let Some(value) = line.strip_prefix("DA:") {
            record_line(&mut files, current.as_ref(), value)?;
        } else if let Some(value) = line.strip_prefix("FN:") {
            record_function_line(&mut files, current.as_ref(), value)?;
        } else if let Some(value) = line.strip_prefix("FNDA:") {
            record_function_count(&mut files, current.as_ref(), value)?;
        } else if line == "end_of_record" {
            current = None;
        }
    }
    Ok(files)
}

fn record_line(
    files: &mut BTreeMap<PathBuf, LcovFile>,
    path: Option<&PathBuf>,
    value: &str,
) -> Result<()> {
    let path = path.context("LCOV DA record precedes SF")?;
    let (line, rest) = value.split_once(',').context("malformed LCOV DA record")?;
    let count = rest.split(',').next().context("missing LCOV DA count")?;
    let line = line.parse::<u32>().context("invalid LCOV DA line")?;
    let count = count.parse::<u64>().context("invalid LCOV DA count")?;
    files
        .entry(path.clone())
        .or_default()
        .lines
        .entry(line)
        .and_modify(|old| *old = (*old).max(count))
        .or_insert(count);
    Ok(())
}

fn record_function_line(
    files: &mut BTreeMap<PathBuf, LcovFile>,
    path: Option<&PathBuf>,
    value: &str,
) -> Result<()> {
    let path = path.context("LCOV FN record precedes SF")?;
    let (line, name) = value.split_once(',').context("malformed LCOV FN record")?;
    files
        .entry(path.clone())
        .or_default()
        .function_lines
        .insert(
            name.to_string(),
            line.parse().context("invalid LCOV FN line")?,
        );
    Ok(())
}

fn record_function_count(
    files: &mut BTreeMap<PathBuf, LcovFile>,
    path: Option<&PathBuf>,
    value: &str,
) -> Result<()> {
    let path = path.context("LCOV FNDA record precedes SF")?;
    let (count, name) = value
        .split_once(',')
        .context("malformed LCOV FNDA record")?;
    let count = count.parse::<u64>().context("invalid LCOV FNDA count")?;
    files
        .entry(path.clone())
        .or_default()
        .function_counts
        .entry(name.to_string())
        .and_modify(|old| *old = (*old).max(count))
        .or_insert(count);
    Ok(())
}
