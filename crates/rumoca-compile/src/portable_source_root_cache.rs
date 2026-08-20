//! Portable source-root cache construction for detached compiler clients.
//!
//! Unlike the compiler's fingerprinted on-disk cache, this cache is a
//! self-contained `gzip(bincode(Vec<(uri, StoredDefinition)>))` artifact. It is
//! used when a client cannot access the source-root filesystem, such as the
//! browser-hosted documentation LSP.

use std::ffi::OsStr;
use std::fs;
use std::io::Write;
use std::path::{Component, Path, PathBuf};

use anyhow::{Context, Result, bail};
use flate2::{Compression, write::GzEncoder};
use rayon::prelude::*;
use rumoca_ir_ast::StoredDefinition;

use crate::parse::parse_source_to_ast;

/// A validated source root included in a portable cache.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PortableSourceRoot {
    key: String,
    path: PathBuf,
}

impl PortableSourceRoot {
    /// Create a source-root specification.
    pub fn new(key: impl Into<String>, path: impl Into<PathBuf>) -> Result<Self> {
        let key = key.into();
        let path = path.into();
        if key.is_empty() {
            bail!("portable source-root key must not be empty");
        }
        if path.as_os_str().is_empty() {
            bail!("portable source-root path must not be empty");
        }
        Ok(Self { key, path })
    }

    pub fn key(&self) -> &str {
        &self.key
    }

    pub fn path(&self) -> &Path {
        &self.path
    }
}

/// A source that could not be included in a portable cache.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PortableSourceRootCacheIssue {
    MissingRoot { key: String, path: PathBuf },
    ParseFailed { uri: String, message: String },
}

/// Complete result of a portable-cache construction attempt.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PortableSourceRootCacheReport {
    pub definition_count: usize,
    pub wrote_cache: bool,
    pub issues: Vec<PortableSourceRootCacheIssue>,
}

#[derive(Debug, Clone)]
struct PortableModelicaFile {
    source_path: PathBuf,
    uri: String,
}

/// Parse `roots` and write a portable source-root cache to `output`.
///
/// Missing roots and Modelica syntax failures are explicit in the returned
/// report. Filesystem and serialization failures abort the operation. When no
/// source parses successfully, `output` is removed so its absence remains the
/// unambiguous empty-cache signal.
pub fn write_portable_source_root_cache(
    output: &Path,
    roots: &[PortableSourceRoot],
) -> Result<PortableSourceRootCacheReport> {
    let mut files = Vec::new();
    let mut issues = Vec::new();
    for root in roots {
        if !root.path.exists() {
            issues.push(PortableSourceRootCacheIssue::MissingRoot {
                key: root.key.clone(),
                path: root.path.clone(),
            });
            continue;
        }
        collect_modelica_sources(&root.path, &root.path, &root.key, &mut files)?;
    }

    let parsed = parse_modelica_files(&files)?;
    let mut definitions = Vec::with_capacity(parsed.len());
    for result in parsed {
        match result {
            ParsedFile::Definition(definition) => definitions.push(definition),
            ParsedFile::Failed { uri, message } => {
                issues.push(PortableSourceRootCacheIssue::ParseFailed { uri, message });
            }
        }
    }

    if definitions.is_empty() {
        if output.is_file() {
            fs::remove_file(output)
                .with_context(|| format!("failed to remove stale {}", output.display()))?;
        }
        return Ok(PortableSourceRootCacheReport {
            definition_count: 0,
            wrote_cache: false,
            issues,
        });
    }

    if let Some(parent) = output.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("failed to create {}", parent.display()))?;
    }
    let file = fs::File::create(output)
        .with_context(|| format!("failed to create {}", output.display()))?;
    let mut encoder = GzEncoder::new(file, Compression::best());
    encoder
        .write_all(&bincode::serialize(&definitions)?)
        .with_context(|| format!("failed to write {}", output.display()))?;
    encoder
        .finish()
        .with_context(|| format!("failed to finish {}", output.display()))?;

    Ok(PortableSourceRootCacheReport {
        definition_count: definitions.len(),
        wrote_cache: true,
        issues,
    })
}

fn collect_modelica_sources(
    root: &Path,
    dir: &Path,
    source_root_key: &str,
    files: &mut Vec<PortableModelicaFile>,
) -> Result<()> {
    for entry in sorted_dir_entries(dir)? {
        let path = entry.path();
        let file_type = entry
            .file_type()
            .with_context(|| format!("failed to stat {}", path.display()))?;
        if file_type.is_dir() {
            collect_modelica_sources(root, &path, source_root_key, files)?;
        } else if file_type.is_file() && path.extension() == Some(OsStr::new("mo")) {
            let relative = path
                .strip_prefix(root)
                .with_context(|| format!("failed to relativize {}", path.display()))?;
            let normalized = normalize_source_root_path(relative);
            let normalized = normalized.to_string_lossy().replace('\\', "/");
            files.push(PortableModelicaFile {
                source_path: path,
                uri: format!("{source_root_key}/{normalized}"),
            });
        }
    }
    Ok(())
}

enum ParsedFile {
    Definition((String, StoredDefinition)),
    Failed { uri: String, message: String },
}

fn parse_modelica_files(files: &[PortableModelicaFile]) -> Result<Vec<ParsedFile>> {
    files.par_iter().map(parse_modelica_file).collect()
}

fn parse_modelica_file(file: &PortableModelicaFile) -> Result<ParsedFile> {
    let source = fs::read_to_string(&file.source_path)
        .with_context(|| format!("failed to read {}", file.source_path.display()))?;
    Ok(match parse_source_to_ast(&source, &file.uri) {
        Ok(definition) => ParsedFile::Definition((file.uri.clone(), definition)),
        Err(error) => ParsedFile::Failed {
            uri: file.uri.clone(),
            message: error.to_string(),
        },
    })
}

fn normalize_source_root_path(relative: &Path) -> PathBuf {
    let mut components = relative.components();
    let Some(first) = components.next() else {
        return PathBuf::new();
    };
    let mut normalized = PathBuf::new();
    match first {
        Component::Normal(name) => {
            normalized.push(strip_trailing_version_suffix(&name.to_string_lossy()));
        }
        other => normalized.push(other.as_os_str()),
    }
    for component in components {
        normalized.push(component.as_os_str());
    }
    normalized
}

fn strip_trailing_version_suffix(name: &str) -> String {
    let Some(separator) = name.rfind([' ', '-']) else {
        return name.to_owned();
    };
    let suffix = &name[separator + 1..];
    if suffix.chars().any(|ch| ch.is_ascii_digit())
        && suffix.chars().all(|ch| ch.is_ascii_digit() || ch == '.')
    {
        name[..separator].to_owned()
    } else {
        name.to_owned()
    }
}

fn sorted_dir_entries(dir: &Path) -> Result<Vec<fs::DirEntry>> {
    let mut entries = fs::read_dir(dir)
        .with_context(|| format!("failed to read {}", dir.display()))?
        .collect::<std::result::Result<Vec<_>, _>>()
        .with_context(|| format!("failed to read entries in {}", dir.display()))?;
    entries.sort_by_key(|entry| entry.file_name());
    Ok(entries)
}

#[cfg(test)]
mod tests {
    use std::io::Read;

    use flate2::read::GzDecoder;

    use super::*;

    #[test]
    fn strips_only_numeric_trailing_versions() {
        assert_eq!(strip_trailing_version_suffix("Modelica 4.1.0"), "Modelica");
        assert_eq!(
            strip_trailing_version_suffix("CMM-a642c381"),
            "CMM-a642c381"
        );
        assert_eq!(
            strip_trailing_version_suffix("Utilities-Core"),
            "Utilities-Core"
        );
    }

    #[test]
    fn writes_deterministic_uri_order_and_reports_parse_failures() {
        let temp = tempfile::tempdir().expect("tempdir");
        let root = temp.path().join("ModelicaStandardLibrary-4.1.0");
        fs::create_dir_all(root.join("Modelica 4.1.0")).expect("create source root");
        fs::write(
            root.join("Modelica 4.1.0/B.mo"),
            "within Modelica; model B end B;",
        )
        .expect("write B");
        fs::write(
            root.join("Modelica 4.1.0/A.mo"),
            "within Modelica; model A end A;",
        )
        .expect("write A");
        fs::write(root.join("Modelica 4.1.0/Bad.mo"), "model").expect("write invalid source");

        let output = temp.path().join("cache/cache.bin.gz");
        let spec = PortableSourceRoot::new("msl", &root).expect("valid spec");
        let report =
            write_portable_source_root_cache(&output, &[spec]).expect("write portable cache");
        assert!(report.wrote_cache);
        assert_eq!(report.definition_count, 2);
        assert_eq!(report.issues.len(), 1);
        assert!(matches!(
            &report.issues[0],
            PortableSourceRootCacheIssue::ParseFailed { uri, .. }
                if uri == "msl/Modelica/Bad.mo"
        ));

        let mut bytes = Vec::new();
        GzDecoder::new(fs::File::open(output).expect("open cache"))
            .read_to_end(&mut bytes)
            .expect("decode gzip");
        let definitions: Vec<(String, StoredDefinition)> =
            bincode::deserialize(&bytes).expect("decode bincode");
        assert_eq!(
            definitions
                .iter()
                .map(|(uri, _)| uri.as_str())
                .collect::<Vec<_>>(),
            ["msl/Modelica/A.mo", "msl/Modelica/B.mo"]
        );
    }

    #[test]
    fn empty_result_removes_stale_cache_and_reports_missing_root() {
        let temp = tempfile::tempdir().expect("tempdir");
        let output = temp.path().join("cache.bin.gz");
        fs::write(&output, b"stale").expect("write stale cache");
        let missing = temp.path().join("missing");
        let spec = PortableSourceRoot::new("missing", &missing).expect("valid spec");

        let report = write_portable_source_root_cache(&output, &[spec]).expect("build empty cache");
        assert!(!report.wrote_cache);
        assert_eq!(report.definition_count, 0);
        assert!(!output.exists());
        assert_eq!(
            report.issues,
            [PortableSourceRootCacheIssue::MissingRoot {
                key: "missing".to_string(),
                path: missing,
            }]
        );
    }
}
