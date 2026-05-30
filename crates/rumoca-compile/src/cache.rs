//! Shared on-disk cache inspection and pruning.

use anyhow::{Context, Result, bail};
use std::collections::{HashMap, HashSet};
use std::fs::{self, OpenOptions};
use std::io::Write;
use std::path::{Path, PathBuf};
use std::time::{Duration, SystemTime, UNIX_EPOCH};

use crate::source_root_cache::resolve_cache_root_dir;

pub const DEFAULT_CACHE_MAX_BYTES: u64 = 10 * 1024 * 1024 * 1024;
const CACHE_ACCESS_METADATA_SUFFIX: &str = ".rumoca-access";
const CACHE_PRUNE_LOCK_STALE_AFTER: Duration = Duration::from_secs(30 * 60);

#[derive(Debug, Clone)]
pub struct CacheStatus {
    pub root: PathBuf,
    pub total_bytes: u64,
    pub file_count: usize,
    pub dir_count: usize,
    pub subcaches: Vec<CacheSubtreeStatus>,
}

#[derive(Debug, Clone)]
pub struct CacheSubtreeStatus {
    pub name: String,
    pub path: PathBuf,
    pub total_bytes: u64,
    pub file_count: usize,
    pub dir_count: usize,
}

#[derive(Debug, Clone)]
pub struct CachePruneReport {
    pub before: CacheStatus,
    pub after: CacheStatus,
    pub max_bytes: u64,
    pub removed_files: usize,
    pub removed_bytes: u64,
    pub dry_run: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CacheFamilyBudget {
    pub family: String,
    pub max_bytes: u64,
}

#[derive(Debug, Clone)]
pub struct CachePruneOptions {
    pub max_bytes: u64,
    pub max_age: Option<Duration>,
    pub family_budgets: Vec<CacheFamilyBudget>,
}

#[derive(Debug, Clone)]
struct CacheFileEntry {
    path: PathBuf,
    family: String,
    bytes: u64,
    last_used: SystemTime,
}

pub fn cache_root_dir() -> PathBuf {
    resolve_cache_root_dir()
}

pub fn cache_status(root: Option<&Path>) -> Result<CacheStatus> {
    let root = root.map(Path::to_path_buf).unwrap_or_else(cache_root_dir);
    let mut status = CacheStatus {
        root: root.clone(),
        total_bytes: 0,
        file_count: 0,
        dir_count: 0,
        subcaches: Vec::new(),
    };
    if !root.exists() {
        return Ok(status);
    }
    collect_cache_status(&root, &mut status)
        .with_context(|| format!("scan cache root {}", root.display()))?;
    status.subcaches = collect_cache_subtrees(&root)
        .with_context(|| format!("scan cache subtrees under {}", root.display()))?;
    Ok(status)
}

/// Default automatic-prune budget: cap total cache size, no age or per-family
/// limits. Explicit limits come from `cache prune` CLI flags.
fn default_prune_options() -> CachePruneOptions {
    CachePruneOptions {
        max_bytes: DEFAULT_CACHE_MAX_BYTES,
        max_age: None,
        family_budgets: Vec::new(),
    }
}

pub(crate) fn record_cache_file_access(path: &Path) {
    if let Err(err) = write_cache_file_access(path, SystemTime::now()) {
        eprintln!(
            "failed to record cache access for {}: {err}",
            path.display()
        );
    }
}

fn write_cache_file_access(path: &Path, accessed: SystemTime) -> std::io::Result<()> {
    let accessed_secs = accessed
        .duration_since(SystemTime::UNIX_EPOCH)
        .unwrap_or_default()
        .as_secs();
    fs::write(cache_access_metadata_path(path), accessed_secs.to_string())
}

pub(crate) fn maybe_prune_cache_after_write(root: Option<&Path>) {
    let options = default_prune_options();
    let prune_root = root.map(Path::to_path_buf).unwrap_or_else(cache_root_dir);
    let Some(_lock) = try_acquire_cache_prune_lock(&prune_root) else {
        return;
    };
    let Ok(Some(report)) = prune_cache_after_write_with_options(Some(&prune_root), &options) else {
        return;
    };
    eprintln!(
        "Rumoca cache auto-prune: removed {} files totaling {} bytes ({} -> {}, limit {} bytes)",
        report.removed_files,
        report.removed_bytes,
        report.before.total_bytes,
        report.after.total_bytes,
        report.max_bytes
    );
}

pub fn parse_byte_size(raw: &str) -> Result<u64> {
    let trimmed = raw.trim();
    if trimmed.is_empty() {
        bail!("empty byte-size value");
    }
    let split_at = trimmed
        .find(|ch: char| !ch.is_ascii_digit())
        .unwrap_or(trimmed.len());
    let (digits, suffix) = trimmed.split_at(split_at);
    if digits.is_empty() {
        bail!("byte-size value '{raw}' does not start with a number");
    }
    let value = digits
        .parse::<u64>()
        .with_context(|| format!("parse byte-size value '{raw}'"))?;
    let multiplier = match suffix.trim().to_ascii_lowercase().as_str() {
        "" | "b" => 1,
        "k" | "kb" | "kib" => 1024,
        "m" | "mb" | "mib" => 1024 * 1024,
        "g" | "gb" | "gib" => 1024 * 1024 * 1024,
        "t" | "tb" | "tib" => 1024_u64.pow(4),
        other => bail!("unsupported byte-size suffix '{other}' in '{raw}'"),
    };
    value
        .checked_mul(multiplier)
        .with_context(|| format!("byte-size value '{raw}' overflows u64"))
}

pub fn parse_max_age_days(raw: &str) -> Result<Duration> {
    let days = raw
        .trim()
        .parse::<u64>()
        .with_context(|| format!("parse max-age days '{raw}'"))?;
    if days == 0 {
        bail!("max-age days must be greater than zero");
    }
    Ok(Duration::from_secs(days.saturating_mul(24 * 60 * 60)))
}

pub fn parse_family_budget(raw: &str) -> Result<CacheFamilyBudget> {
    let Some((family, max_bytes)) = raw.split_once('=') else {
        bail!("family budget '{raw}' must be formatted as FAMILY=SIZE");
    };
    let family = family.trim();
    if family.is_empty() {
        bail!("family budget '{raw}' has an empty family name");
    }
    Ok(CacheFamilyBudget {
        family: family.to_string(),
        max_bytes: parse_byte_size(max_bytes.trim())?,
    })
}

pub fn parse_family_budgets(raw: &str) -> Result<Vec<CacheFamilyBudget>> {
    raw.split(',')
        .map(str::trim)
        .filter(|part| !part.is_empty())
        .map(parse_family_budget)
        .collect()
}

pub fn prune_cache(root: Option<&Path>, max_bytes: u64, dry_run: bool) -> Result<CachePruneReport> {
    prune_cache_with_options(
        root,
        &CachePruneOptions {
            max_bytes,
            max_age: None,
            family_budgets: Vec::new(),
        },
        dry_run,
    )
}

pub fn prune_cache_with_options(
    root: Option<&Path>,
    options: &CachePruneOptions,
    dry_run: bool,
) -> Result<CachePruneReport> {
    let root = root.map(Path::to_path_buf).unwrap_or_else(cache_root_dir);
    let before = cache_status(Some(&root))?;
    let mut files = Vec::new();
    if should_scan_for_prune(&before, options) && root.exists() {
        collect_cache_files(&root, &mut files)
            .with_context(|| format!("scan cache files under {}", root.display()))?;
        files.sort_by(|left, right| {
            cache_file_prune_priority(&root, &left.path)
                .cmp(&cache_file_prune_priority(&root, &right.path))
                .then_with(|| left.last_used.cmp(&right.last_used))
                .then_with(|| left.path.cmp(&right.path))
        });
    }

    let mut projected_bytes = before.total_bytes;
    let mut projected_family_bytes = family_size_map(&before);
    let mut removed_paths = HashSet::new();
    let mut removed_files = 0;
    let mut removed_bytes = 0;

    if let Some(max_age) = options.max_age {
        let now = SystemTime::now();
        for entry in &files {
            if cache_file_exceeds_max_age(entry.last_used, now, max_age) {
                remove_projected_cache_entry(
                    entry,
                    dry_run,
                    &mut removed_paths,
                    &mut projected_bytes,
                    &mut projected_family_bytes,
                    &mut removed_files,
                    &mut removed_bytes,
                );
            }
        }
    }

    for budget in &options.family_budgets {
        for entry in files.iter().filter(|entry| entry.family == budget.family) {
            let family_bytes = projected_family_bytes
                .get(&budget.family)
                .copied()
                .unwrap_or_default();
            if family_bytes <= budget.max_bytes {
                break;
            }
            remove_projected_cache_entry(
                entry,
                dry_run,
                &mut removed_paths,
                &mut projected_bytes,
                &mut projected_family_bytes,
                &mut removed_files,
                &mut removed_bytes,
            );
        }
    }

    for entry in &files {
        if projected_bytes <= options.max_bytes {
            break;
        }
        remove_projected_cache_entry(
            entry,
            dry_run,
            &mut removed_paths,
            &mut projected_bytes,
            &mut projected_family_bytes,
            &mut removed_files,
            &mut removed_bytes,
        );
    }

    if !dry_run && root.exists() {
        let _ = remove_empty_cache_dirs(&root, &root);
    }

    let after = if dry_run {
        CacheStatus {
            total_bytes: projected_bytes,
            file_count: before.file_count.saturating_sub(removed_files),
            dir_count: before.dir_count,
            subcaches: before.subcaches.clone(),
            root: root.clone(),
        }
    } else {
        cache_status(Some(&root))?
    };

    Ok(CachePruneReport {
        before,
        after,
        max_bytes: options.max_bytes,
        removed_files,
        removed_bytes,
        dry_run,
    })
}

fn should_scan_for_prune(status: &CacheStatus, options: &CachePruneOptions) -> bool {
    status.total_bytes > options.max_bytes
        || options.max_age.is_some()
        || !options.family_budgets.is_empty()
}

fn family_size_map(status: &CacheStatus) -> HashMap<String, u64> {
    status
        .subcaches
        .iter()
        .map(|subcache| (subcache.name.clone(), subcache.total_bytes))
        .collect()
}

fn cache_file_exceeds_max_age(modified: SystemTime, now: SystemTime, max_age: Duration) -> bool {
    now.duration_since(modified)
        .map(|age| age > max_age)
        .unwrap_or(false)
}

fn remove_projected_cache_entry(
    entry: &CacheFileEntry,
    dry_run: bool,
    removed_paths: &mut HashSet<PathBuf>,
    projected_bytes: &mut u64,
    projected_family_bytes: &mut HashMap<String, u64>,
    removed_files: &mut usize,
    removed_bytes: &mut u64,
) {
    if !removed_paths.insert(entry.path.clone()) {
        return;
    }
    *projected_bytes = projected_bytes.saturating_sub(entry.bytes);
    if let Some(bytes) = projected_family_bytes.get_mut(&entry.family) {
        *bytes = bytes.saturating_sub(entry.bytes);
    }
    *removed_files += 1;
    *removed_bytes = removed_bytes.saturating_add(entry.bytes);
    if !dry_run {
        let _ = remove_cache_file_and_metadata(&entry.path);
    }
}

fn cache_file_prune_priority(root: &Path, path: &Path) -> u8 {
    let Ok(relative) = path.strip_prefix(root) else {
        return 0;
    };
    if relative
        .components()
        .next()
        .and_then(|component| match component {
            std::path::Component::Normal(name) => name.to_str(),
            _ => None,
        })
        == Some("source-roots")
    {
        1
    } else {
        0
    }
}

struct CachePruneLock {
    path: PathBuf,
    token: String,
}

impl Drop for CachePruneLock {
    fn drop(&mut self) {
        if lock_file_matches_token(&self.path, &self.token) {
            let _ = fs::remove_file(&self.path);
        }
    }
}

fn try_acquire_cache_prune_lock(root: &Path) -> Option<CachePruneLock> {
    let path = cache_prune_lock_path(root);
    remove_stale_cache_prune_lock(&path);
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).ok()?;
    }
    let mut file = OpenOptions::new()
        .write(true)
        .create_new(true)
        .open(&path)
        .ok()?;
    let token = cache_prune_lock_token();
    writeln!(file, "{token}").ok()?;
    Some(CachePruneLock { path, token })
}

fn remove_stale_cache_prune_lock(path: &Path) {
    let Ok(metadata) = fs::metadata(path) else {
        return;
    };
    let Ok(modified) = metadata.modified() else {
        return;
    };
    let Ok(age) = SystemTime::now().duration_since(modified) else {
        return;
    };
    if age <= CACHE_PRUNE_LOCK_STALE_AFTER {
        return;
    }
    let Ok(observed) = fs::read_to_string(path) else {
        return;
    };
    if fs::metadata(path)
        .and_then(|metadata| metadata.modified())
        .ok()
        .and_then(|modified| SystemTime::now().duration_since(modified).ok())
        .is_some_and(|current_age| current_age > CACHE_PRUNE_LOCK_STALE_AFTER)
        && lock_file_matches_token(path, observed.trim_end())
    {
        let _ = fs::remove_file(path);
    }
}

fn cache_prune_lock_token() -> String {
    let now_nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|duration| duration.as_nanos())
        .unwrap_or(0);
    format!("pid={} token={now_nanos}", std::process::id())
}

fn lock_file_matches_token(path: &Path, token: &str) -> bool {
    fs::read_to_string(path)
        .map(|content| content.trim_end() == token)
        .unwrap_or(false)
}

fn cache_prune_lock_path(root: &Path) -> PathBuf {
    let lock_name = root
        .file_name()
        .and_then(|name| name.to_str())
        .filter(|name| !name.is_empty())
        .map(|name| format!(".{name}.rumoca-prune.lock"))
        .unwrap_or_else(|| ".rumoca-cache.rumoca-prune.lock".to_string());
    root.parent()
        .map(|parent| parent.join(&lock_name))
        .unwrap_or_else(|| PathBuf::from(lock_name))
}

#[cfg(test)]
pub(crate) fn prune_cache_after_write(
    root: Option<&Path>,
    max_bytes: u64,
) -> Result<Option<CachePruneReport>> {
    let report = prune_cache(root, max_bytes, false)?;
    if report.removed_files == 0 {
        Ok(None)
    } else {
        Ok(Some(report))
    }
}

pub(crate) fn prune_cache_after_write_with_options(
    root: Option<&Path>,
    options: &CachePruneOptions,
) -> Result<Option<CachePruneReport>> {
    let report = prune_cache_with_options(root, options, false)?;
    if report.removed_files == 0 {
        Ok(None)
    } else {
        Ok(Some(report))
    }
}

fn collect_cache_subtrees(root: &Path) -> std::io::Result<Vec<CacheSubtreeStatus>> {
    let mut entries = fs::read_dir(root)?.collect::<std::io::Result<Vec<_>>>()?;
    entries.sort_by_key(|entry| entry.path());
    let mut subcaches = Vec::new();
    for entry in entries {
        let path = entry.path();
        let name = entry.file_name().to_string_lossy().to_string();
        let mut status = CacheStatus {
            root: path.clone(),
            total_bytes: 0,
            file_count: 0,
            dir_count: 0,
            subcaches: Vec::new(),
        };
        collect_cache_status(&path, &mut status)?;
        subcaches.push(CacheSubtreeStatus {
            name,
            path,
            total_bytes: status.total_bytes,
            file_count: status.file_count,
            dir_count: status.dir_count,
        });
    }
    Ok(subcaches)
}

fn collect_cache_status(path: &Path, status: &mut CacheStatus) -> std::io::Result<()> {
    let metadata = fs::symlink_metadata(path)?;
    if metadata.is_dir() {
        status.dir_count += 1;
        for entry in fs::read_dir(path)? {
            collect_cache_status(&entry?.path(), status)?;
        }
    } else if metadata.is_file() && !is_cache_access_metadata_file(path) {
        status.file_count += 1;
        status.total_bytes += metadata.len();
    }
    Ok(())
}

fn collect_cache_files(path: &Path, out: &mut Vec<CacheFileEntry>) -> std::io::Result<()> {
    collect_cache_files_inner(path, path, out)
}

fn collect_cache_files_inner(
    root: &Path,
    path: &Path,
    out: &mut Vec<CacheFileEntry>,
) -> std::io::Result<()> {
    let metadata = fs::symlink_metadata(path)?;
    if metadata.is_dir() {
        for entry in fs::read_dir(path)? {
            collect_cache_files_inner(root, &entry?.path(), out)?;
        }
    } else if metadata.is_file() && !is_cache_access_metadata_file(path) {
        out.push(CacheFileEntry {
            path: path.to_path_buf(),
            family: cache_family_name(root, path),
            bytes: metadata.len(),
            last_used: read_cache_file_access(path)
                .or_else(|| metadata.modified().ok())
                .unwrap_or(SystemTime::UNIX_EPOCH),
        });
    }
    Ok(())
}

fn cache_access_metadata_path(path: &Path) -> PathBuf {
    let Some(file_name) = path.file_name().and_then(|name| name.to_str()) else {
        return path.with_extension(CACHE_ACCESS_METADATA_SUFFIX.trim_start_matches('.'));
    };
    path.with_file_name(format!("{file_name}{CACHE_ACCESS_METADATA_SUFFIX}"))
}

fn read_cache_file_access(path: &Path) -> Option<SystemTime> {
    let raw = fs::read_to_string(cache_access_metadata_path(path)).ok()?;
    let accessed_secs = raw.trim().parse::<u64>().ok()?;
    Some(SystemTime::UNIX_EPOCH + Duration::from_secs(accessed_secs))
}

fn is_cache_access_metadata_file(path: &Path) -> bool {
    path.file_name()
        .and_then(|name| name.to_str())
        .is_some_and(|name| name.ends_with(CACHE_ACCESS_METADATA_SUFFIX))
}

fn remove_cache_file_and_metadata(path: &Path) -> std::io::Result<()> {
    let remove_result = fs::remove_file(path);
    let _ = fs::remove_file(cache_access_metadata_path(path));
    remove_result
}

fn cache_family_name(root: &Path, path: &Path) -> String {
    path.strip_prefix(root)
        .ok()
        .and_then(|relative| relative.components().next())
        .and_then(|component| match component {
            std::path::Component::Normal(name) => name.to_str(),
            _ => None,
        })
        .unwrap_or("")
        .to_string()
}

fn remove_empty_cache_dirs(root: &Path, path: &Path) -> std::io::Result<()> {
    let metadata = fs::symlink_metadata(path)?;
    if !metadata.is_dir() {
        return Ok(());
    }
    for entry in fs::read_dir(path)? {
        let child = entry?.path();
        if child.is_dir() {
            let _ = remove_empty_cache_dirs(root, &child);
        }
    }
    if path != root && fs::read_dir(path)?.next().is_none() {
        fs::remove_dir(path)?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;

    #[test]
    fn parse_byte_size_accepts_binary_suffixes() {
        assert_eq!(parse_byte_size("42").unwrap(), 42);
        assert_eq!(parse_byte_size("2K").unwrap(), 2 * 1024);
        assert_eq!(parse_byte_size("3MiB").unwrap(), 3 * 1024 * 1024);
        assert_eq!(parse_byte_size("4gb").unwrap(), 4 * 1024 * 1024 * 1024);
    }

    #[test]
    fn parse_family_budgets_accepts_comma_list() {
        assert_eq!(
            parse_family_budgets("results=2G,source-roots=512M").unwrap(),
            vec![
                CacheFamilyBudget {
                    family: "results".to_string(),
                    max_bytes: 2 * 1024 * 1024 * 1024,
                },
                CacheFamilyBudget {
                    family: "source-roots".to_string(),
                    max_bytes: 512 * 1024 * 1024,
                },
            ]
        );
    }

    #[test]
    fn cache_file_age_check_uses_modified_time() {
        let now = SystemTime::UNIX_EPOCH + Duration::from_secs(10 * 24 * 60 * 60);
        let old = SystemTime::UNIX_EPOCH + Duration::from_secs(2 * 24 * 60 * 60);
        let recent = SystemTime::UNIX_EPOCH + Duration::from_secs(9 * 24 * 60 * 60);

        assert!(cache_file_exceeds_max_age(
            old,
            now,
            Duration::from_secs(7 * 24 * 60 * 60)
        ));
        assert!(!cache_file_exceeds_max_age(
            recent,
            now,
            Duration::from_secs(7 * 24 * 60 * 60)
        ));
    }

    #[test]
    fn prune_cache_removes_oldest_files_until_under_budget() {
        let temp = tempfile::tempdir().expect("tempdir");
        let root = temp.path();
        write_file(&root.join("a-old.bin"), 5);
        write_file(&root.join("b-new.bin"), 7);

        let report = prune_cache(Some(root), 8, false).expect("prune");

        assert_eq!(report.before.total_bytes, 12);
        assert!(report.after.total_bytes <= 8);
        assert_eq!(report.removed_files, 1);
        assert_eq!(report.removed_bytes, 5);
        assert!(!root.join("a-old.bin").exists());
        assert!(root.join("b-new.bin").exists());
    }

    #[test]
    fn prune_cache_uses_access_metadata_for_lru_order() {
        let temp = tempfile::tempdir().expect("tempdir");
        let root = temp.path();
        let recent = root.join("a-recent.bin");
        let old = root.join("b-old.bin");
        write_file(&recent, 5);
        write_file(&old, 7);
        write_cache_file_access(&recent, SystemTime::UNIX_EPOCH + Duration::from_secs(20))
            .expect("write recent access metadata");
        write_cache_file_access(&old, SystemTime::UNIX_EPOCH + Duration::from_secs(10))
            .expect("write old access metadata");

        let report = prune_cache(Some(root), 8, false).expect("prune");

        assert_eq!(report.removed_files, 1);
        assert_eq!(report.removed_bytes, 7);
        assert!(recent.exists());
        assert!(!old.exists());
        assert!(!cache_access_metadata_path(&old).exists());
        assert!(cache_access_metadata_path(&recent).exists());
    }

    #[test]
    fn cache_status_ignores_access_metadata_files() {
        let temp = tempfile::tempdir().expect("tempdir");
        let root = temp.path();
        let artifact = root.join("artifact.bin");
        write_file(&artifact, 5);
        write_cache_file_access(&artifact, SystemTime::UNIX_EPOCH + Duration::from_secs(10))
            .expect("write access metadata");

        let status = cache_status(Some(root)).expect("status");

        assert_eq!(status.total_bytes, 5);
        assert_eq!(status.file_count, 1);
    }

    #[test]
    fn prune_cache_after_write_reports_only_when_it_removes_files() {
        let temp = tempfile::tempdir().expect("tempdir");
        let root = temp.path();
        write_file(&root.join("a-old.bin"), 5);
        write_file(&root.join("b-new.bin"), 7);

        let first = prune_cache_after_write(Some(root), 8)
            .expect("first prune")
            .expect("first prune should remove files");
        assert_eq!(first.removed_files, 1);

        let second = prune_cache_after_write(Some(root), 8).expect("second prune");
        assert!(second.is_none());
    }

    #[test]
    fn prune_cache_prefers_non_source_root_families() {
        let temp = tempfile::tempdir().expect("tempdir");
        let root = temp.path();
        fs::create_dir_all(root.join("source-roots/parsed-files")).expect("mkdir source-roots");
        fs::create_dir_all(root.join("results/rumoca_dae")).expect("mkdir results");
        write_file(&root.join("source-roots/parsed-files/a.bin"), 5);
        write_file(&root.join("results/rumoca_dae/b.json"), 7);

        let report = prune_cache(Some(root), 8, false).expect("prune");

        assert_eq!(report.removed_files, 1);
        assert_eq!(report.removed_bytes, 7);
        assert!(root.join("source-roots/parsed-files/a.bin").exists());
        assert!(!root.join("results/rumoca_dae/b.json").exists());
    }

    #[test]
    fn prune_cache_enforces_family_budget_without_total_pressure() {
        let temp = tempfile::tempdir().expect("tempdir");
        let root = temp.path();
        fs::create_dir_all(root.join("source-roots/parsed-files")).expect("mkdir source-roots");
        fs::create_dir_all(root.join("results/rumoca_dae")).expect("mkdir results");
        write_file(&root.join("source-roots/parsed-files/a.bin"), 5);
        write_file(&root.join("results/rumoca_dae/b.json"), 7);
        write_file(&root.join("results/rumoca_dae/c.json"), 6);

        let report = prune_cache_with_options(
            Some(root),
            &CachePruneOptions {
                max_bytes: 100,
                max_age: None,
                family_budgets: vec![CacheFamilyBudget {
                    family: "results".to_string(),
                    max_bytes: 0,
                }],
            },
            false,
        )
        .expect("prune");

        assert_eq!(report.removed_files, 2);
        assert_eq!(report.removed_bytes, 13);
        assert!(root.join("source-roots/parsed-files/a.bin").exists());
        assert!(!root.join("results/rumoca_dae/b.json").exists());
        assert!(!root.join("results/rumoca_dae/c.json").exists());
    }

    #[test]
    fn cache_status_reports_top_level_subcaches() {
        let temp = tempfile::tempdir().expect("tempdir");
        let root = temp.path();
        fs::create_dir_all(root.join("source-roots/parsed-files")).expect("mkdir source-roots");
        fs::create_dir_all(root.join("results/rumoca_dae")).expect("mkdir results");
        write_file(&root.join("source-roots/parsed-files/a.bin"), 5);
        write_file(&root.join("results/rumoca_dae/b.json"), 7);

        let status = cache_status(Some(root)).expect("status");

        assert_eq!(status.total_bytes, 12);
        assert_eq!(status.subcaches.len(), 2);
        assert_eq!(status.subcaches[0].name, "results");
        assert_eq!(status.subcaches[0].total_bytes, 7);
        assert_eq!(status.subcaches[1].name, "source-roots");
        assert_eq!(status.subcaches[1].total_bytes, 5);
    }

    #[test]
    fn auto_prune_lock_allows_one_owner_per_cache_root() {
        let temp = tempfile::tempdir().expect("tempdir");
        let root = temp.path().join("cache");
        fs::create_dir_all(&root).expect("mkdir cache");

        let first = try_acquire_cache_prune_lock(&root).expect("first lock");
        assert!(
            try_acquire_cache_prune_lock(&root).is_none(),
            "second owner should skip while prune lock is held"
        );
        drop(first);

        assert!(
            try_acquire_cache_prune_lock(&root).is_some(),
            "lock should be reusable after owner drops it"
        );
    }

    fn write_file(path: &Path, bytes: usize) {
        let mut file = fs::File::create(path).expect("create file");
        file.write_all(&vec![b'x'; bytes]).expect("write file");
    }
}
