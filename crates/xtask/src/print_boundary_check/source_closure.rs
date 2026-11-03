//! Exact, bounded source-byte closure for a gate-owned list of roots.
//!
//! Capturing bytes never authorizes their contents. The eventual production gate
//! supplies the roots from its reviewed route/dependency contract and compares
//! the complete capture to that contract; a renderer cannot choose exclusions.

#[cfg(test)]
mod tests;

use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::io::Read;
use std::path::{Component, Path, PathBuf};
use std::sync::Arc;

use sha2::{Digest, Sha256};

use super::{Checked, Refusal, refuse};

const MAX_FILES: usize = 32_768;
const MAX_FILE_BYTES: u64 = 16 * 1024 * 1024;
const MAX_TOTAL_BYTES: u64 = 256 * 1024 * 1024;

#[derive(Debug, PartialEq, Eq, serde::Serialize)]
enum Entry {
    Directory,
    File {
        bytes: u64,
        sha256: String,
        #[serde(skip)]
        content: Arc<[u8]>,
    },
}

#[derive(Debug, PartialEq, Eq, serde::Serialize)]
pub struct SourceClosure {
    entries: BTreeMap<String, Entry>,
}

/// A directory enumerated by the complete, unfiltered bounded walk. This is
/// coverage of the retained capture, not an atomic filesystem or authorization
/// witness. A list of separately captured files cannot construct this value.
#[derive(Debug)]
pub struct CoveredDirectory<'capture> {
    source: &'capture SourceClosure,
    path: &'capture str,
}

impl CoveredDirectory<'_> {
    pub fn path(&self) -> &str {
        self.path
    }

    /// Enumerate every retained descendant, including unexpected new files.
    /// Structural equality cannot substitute a different capture at consumption.
    pub fn files<'a>(
        &'a self,
        expected: &SourceClosure,
    ) -> Checked<impl Iterator<Item = (&'a str, CapturedFile<'a>)>> {
        if !std::ptr::eq(self.source, expected) {
            return refuse("foreign-covered-directory", self.path);
        }
        Ok(self.source.files().filter(move |(path, _)| {
            path.strip_prefix(self.path)
                .is_some_and(|suffix| suffix.starts_with('/'))
        }))
    }
}

/// A read-only file from the exact bytes hashed during capture. It never
/// opens the live filesystem and cannot be constructed from a hash/flag.
#[derive(Clone, Copy, Debug)]
pub struct CapturedFile<'a> {
    content: &'a [u8],
    sha256: &'a str,
}

impl<'a> CapturedFile<'a> {
    pub fn bytes(self) -> &'a [u8] {
        self.content
    }
    pub fn sha256(self) -> &'a str {
        self.sha256
    }
    pub fn text(self) -> Checked<&'a str> {
        std::str::from_utf8(self.content).map_err(|error| Refusal {
            rule: "source-text-encoding",
            item: error.to_string(),
        })
    }
}

impl SourceClosure {
    pub fn tree(&self, path: &Path) -> Checked<CoveredDirectory<'_>> {
        let key = relative_key(path)?;
        match self.entries.get_key_value(&key) {
            Some((path, Entry::Directory)) => Ok(CoveredDirectory { source: self, path }),
            Some(_) => refuse("source-not-a-directory", key),
            None => refuse("source-tree-not-captured", key),
        }
    }

    pub fn file(&self, path: &Path) -> Checked<CapturedFile<'_>> {
        let key = relative_key(path)?;
        match self.entries.get(&key) {
            Some(Entry::File {
                content, sha256, ..
            }) => Ok(CapturedFile { content, sha256 }),
            Some(Entry::Directory) => refuse("source-not-a-file", key),
            None => refuse("source-not-captured", key),
        }
    }
    pub fn files(&self) -> impl Iterator<Item = (&str, CapturedFile<'_>)> {
        self.entries.iter().filter_map(|(path, entry)| match entry {
            Entry::File {
                content, sha256, ..
            } => Some((path.as_str(), CapturedFile { content, sha256 })),
            Entry::Directory => None,
        })
    }
}

/// Only lexical, repository-relative roots are accepted. Symlinks anywhere
/// below the trusted repository root refuse, including parent components.
pub fn capture(root: &Path, inputs: &BTreeSet<PathBuf>) -> Checked<SourceClosure> {
    if inputs.is_empty() {
        return refuse("empty-source-closure", "no route roots");
    }
    let mut entries = BTreeMap::new();
    let mut total = 0;
    for input in inputs {
        validate_relative(input)?;
        reject_symlink_components(root, input)?;
        for entry in walkdir::WalkDir::new(root.join(input))
            .follow_links(false)
            .sort_by_file_name()
        {
            let entry = entry.map_err(|error| Refusal {
                rule: "source-walk",
                item: error.to_string(),
            })?;
            let relative = entry.path().strip_prefix(root).map_err(|_| Refusal {
                rule: "source-escape",
                item: entry.path().display().to_string(),
            })?;
            let key = relative_key(relative)?;
            if entries.contains_key(&key) {
                continue;
            }
            if entries.len() >= MAX_FILES {
                return refuse("source-entry-bound", key);
            }
            let value = capture_entry(&entry, &key, &mut total)?;
            entries.insert(key, value);
        }
    }
    Ok(SourceClosure { entries })
}

fn capture_entry(entry: &walkdir::DirEntry, key: &str, total: &mut u64) -> Checked<Entry> {
    let kind = entry.file_type();
    if kind.is_symlink() {
        return refuse("source-symlink", key);
    }
    if kind.is_dir() {
        return Ok(Entry::Directory);
    }
    if !kind.is_file() {
        return refuse("source-file-kind", key);
    }
    let (value, size) = hash_file(entry.path())?;
    *total += size;
    if *total > MAX_TOTAL_BYTES {
        return refuse("source-total-bound", key);
    }
    Ok(value)
}

fn validate_relative(path: &Path) -> Checked<()> {
    if path.as_os_str().is_empty()
        || path
            .components()
            .any(|part| !matches!(part, Component::Normal(_)))
    {
        return refuse("source-root", path.display().to_string());
    }
    Ok(())
}

fn relative_key(path: &Path) -> Checked<String> {
    validate_relative(path)?;
    let mut parts = Vec::new();
    for component in path.components() {
        let Component::Normal(part) = component else {
            return refuse("source-root", path.display().to_string());
        };
        let Some(part) = part.to_str().filter(|part| !part.contains('\\')) else {
            return refuse("source-path-encoding", path.display().to_string());
        };
        parts.push(part);
    }
    Ok(parts.join("/"))
}

fn reject_symlink_components(root: &Path, relative: &Path) -> Checked<()> {
    let mut path = root.to_path_buf();
    for component in relative.components() {
        path.push(component);
        let metadata = fs::symlink_metadata(&path).map_err(|error| Refusal {
            rule: "source-metadata",
            item: format!("{}: {error}", path.display()),
        })?;
        if metadata.file_type().is_symlink() {
            return refuse("source-symlink", path.display().to_string());
        }
    }
    Ok(())
}

fn hash_file(path: &Path) -> Checked<(Entry, u64)> {
    let file = fs::File::open(path).map_err(|error| Refusal {
        rule: "source-open",
        item: format!("{}: {error}", path.display()),
    })?;
    let mut bounded = file.take(MAX_FILE_BYTES + 1);
    let mut hash = Sha256::new();
    let mut bytes = 0;
    let mut content = Vec::new();
    let mut buffer = [0; 16 * 1024];
    loop {
        let count = bounded.read(&mut buffer).map_err(|error| Refusal {
            rule: "source-read",
            item: error.to_string(),
        })?;
        if count == 0 {
            break;
        }
        bytes += count as u64;
        if bytes > MAX_FILE_BYTES {
            return refuse("source-file-bound", path.display().to_string());
        }
        hash.update(&buffer[..count]);
        content.extend_from_slice(&buffer[..count]);
    }
    Ok((
        Entry::File {
            bytes,
            sha256: format!("{:x}", hash.finalize()),
            content: content.into(),
        },
        bytes,
    ))
}
