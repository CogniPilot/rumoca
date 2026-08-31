//! One-read durable artifact capture for successful benchmark evidence.

use super::artifact_guard::FrozenArtifactSet;
use anyhow::{Context, Result, ensure};
use serde::Serialize;
use sha2::{Digest, Sha256};
use std::collections::BTreeSet;
use std::fs::{self, File, OpenOptions};
use std::io::{Read, Write};
use std::path::{Component, Path, PathBuf};
use std::sync::Arc;

const MAX_FILE_BYTES: u64 = 64 * 1024 * 1024;
const MAX_BUNDLE_BYTES: u64 = 256 * 1024 * 1024;

pub(super) struct Source {
    logical_path: &'static str,
    source: PathBuf,
}

impl Source {
    pub(super) fn new(logical_path: &'static str, source: &Path) -> Self {
        Self {
            logical_path,
            source: source.to_path_buf(),
        }
    }
}

struct CapturedFile {
    logical_path: &'static str,
    bytes: Vec<u8>,
}

struct CapturedBundle {
    files: Vec<CapturedFile>,
}

#[derive(Clone, Debug, Serialize)]
pub(super) struct BundleEvidence {
    pub(super) relative_directory: String,
    pub(super) closure_sha256: String,
    pub(super) files: Vec<FileEvidence>,
    #[serde(skip)]
    guard: BundleGuard,
}

#[derive(Clone, Debug)]
enum BundleGuard {
    Frozen(Arc<FrozenArtifactSet>),
    #[cfg(test)]
    Synthetic,
}

impl BundleEvidence {
    pub(super) fn verify(&self) -> Result<()> {
        match &self.guard {
            BundleGuard::Frozen(frozen) => frozen.verify(),
            #[cfg(test)]
            BundleGuard::Synthetic => Ok(()),
        }
    }

    #[cfg(test)]
    pub(super) fn synthetic(
        relative_directory: String,
        closure_sha256: String,
        files: Vec<FileEvidence>,
    ) -> Self {
        Self {
            relative_directory,
            closure_sha256,
            files,
            guard: BundleGuard::Synthetic,
        }
    }
}

#[derive(Clone, Debug, Serialize)]
pub(super) struct FileEvidence {
    pub(super) relative_path: String,
    pub(super) sha256: String,
    pub(super) bytes: u64,
}

pub(super) fn capture(
    artifact_root: &Path,
    destination: &Path,
    sources: Vec<Source>,
) -> Result<BundleEvidence> {
    CapturedBundle::read(sources)?.publish(artifact_root, destination)
}

impl CapturedBundle {
    fn read(mut sources: Vec<Source>) -> Result<Self> {
        sources.sort_by_key(|source| source.logical_path);
        ensure!(!sources.is_empty(), "artifact bundle roster is empty");
        let mut seen = BTreeSet::new();
        let mut total = 0_u64;
        let mut files = Vec::with_capacity(sources.len());
        for source in sources {
            validate_logical_path(source.logical_path)?;
            ensure!(
                seen.insert(source.logical_path),
                "duplicate artifact bundle path {}",
                source.logical_path
            );
            let bytes = read_regular_bounded(&source.source)?;
            total = total
                .checked_add(bytes.len() as u64)
                .context("artifact bundle byte count overflow")?;
            ensure!(
                total <= MAX_BUNDLE_BYTES,
                "artifact bundle exceeds {MAX_BUNDLE_BYTES} bytes"
            );
            files.push(CapturedFile {
                logical_path: source.logical_path,
                bytes,
            });
        }
        Ok(Self { files })
    }

    fn publish(self, artifact_root: &Path, destination: &Path) -> Result<BundleEvidence> {
        ensure!(
            artifact_root.is_absolute() && destination.is_absolute(),
            "artifact bundle paths must be absolute"
        );
        let relative_directory = destination
            .strip_prefix(artifact_root)
            .context("artifact bundle destination escaped its root")?
            .to_str()
            .context("artifact bundle destination is not UTF-8")?
            .to_string();
        ensure!(
            !destination.exists(),
            "artifact bundle destination already exists: {}",
            destination.display()
        );
        fs::create_dir_all(destination)
            .with_context(|| format!("failed to create {}", destination.display()))?;
        for captured in &self.files {
            let output = destination.join(captured.logical_path);
            let parent = output.parent().context("bundle file has no parent")?;
            fs::create_dir_all(parent)?;
            let mut file = OpenOptions::new()
                .write(true)
                .create_new(true)
                .open(&output)
                .with_context(|| format!("failed to create {}", output.display()))?;
            file.write_all(&captured.bytes)?;
            file.sync_all()?;
            let mut permissions = file.metadata()?.permissions();
            permissions.set_readonly(true);
            fs::set_permissions(&output, permissions)?;
        }
        verify_published(destination, &self.files)?;
        let frozen = Arc::new(FrozenArtifactSet::capture_tree(destination)?);
        let files = self
            .files
            .iter()
            .map(|file| FileEvidence {
                relative_path: file.logical_path.to_string(),
                sha256: format!("{:x}", Sha256::digest(&file.bytes)),
                bytes: file.bytes.len() as u64,
            })
            .collect::<Vec<_>>();
        Ok(BundleEvidence {
            relative_directory,
            closure_sha256: closure_sha256(&self.files),
            files,
            guard: BundleGuard::Frozen(frozen),
        })
    }
}

fn read_regular_bounded(path: &Path) -> Result<Vec<u8>> {
    let mut file =
        File::open(path).with_context(|| format!("failed to open artifact {}", path.display()))?;
    let metadata = file
        .metadata()
        .with_context(|| format!("failed to inspect artifact {}", path.display()))?;
    ensure!(
        metadata.is_file(),
        "artifact is not a regular file: {}",
        path.display()
    );
    ensure!(
        metadata.len() <= MAX_FILE_BYTES,
        "artifact {} exceeds {MAX_FILE_BYTES} bytes",
        path.display()
    );
    let mut bytes = Vec::with_capacity(metadata.len() as usize);
    Read::by_ref(&mut file)
        .take(MAX_FILE_BYTES + 1)
        .read_to_end(&mut bytes)?;
    ensure!(
        bytes.len() as u64 <= MAX_FILE_BYTES,
        "artifact {} grew beyond {MAX_FILE_BYTES} bytes while reading",
        path.display()
    );
    ensure!(
        bytes.len() as u64 == metadata.len(),
        "artifact {} changed length while reading",
        path.display()
    );
    Ok(bytes)
}

fn verify_published(destination: &Path, expected: &[CapturedFile]) -> Result<()> {
    let mut roster = Vec::new();
    collect_files(destination, destination, &mut roster)?;
    roster.sort();
    let expected_roster = expected
        .iter()
        .map(|file| PathBuf::from(file.logical_path))
        .collect::<Vec<_>>();
    ensure!(
        roster == expected_roster,
        "published artifact roster differs: observed {roster:?}, expected {expected_roster:?}"
    );
    for file in expected {
        let observed = read_regular_bounded(&destination.join(file.logical_path))?;
        ensure!(
            observed == file.bytes,
            "published artifact bytes differ for {}",
            file.logical_path
        );
    }
    Ok(())
}

fn collect_files(root: &Path, directory: &Path, files: &mut Vec<PathBuf>) -> Result<()> {
    for item in fs::read_dir(directory)? {
        let item = item?;
        let metadata = fs::symlink_metadata(item.path())?;
        if metadata.is_dir() {
            collect_files(root, &item.path(), files)?;
        } else {
            ensure!(
                metadata.is_file(),
                "artifact bundle contains a non-regular entry: {}",
                item.path().display()
            );
            files.push(item.path().strip_prefix(root)?.to_path_buf());
        }
    }
    Ok(())
}

fn validate_logical_path(path: &str) -> Result<()> {
    let path = Path::new(path);
    ensure!(!path.as_os_str().is_empty() && !path.is_absolute());
    ensure!(
        path.components()
            .all(|component| matches!(component, Component::Normal(_))),
        "artifact bundle path must be a normalized relative path: {}",
        path.display()
    );
    Ok(())
}

fn closure_sha256(files: &[CapturedFile]) -> String {
    let mut digest = Sha256::new();
    digest.update(b"embedded-head-to-head-artifact-bundle-v1\0");
    for file in files {
        digest.update((file.logical_path.len() as u64).to_le_bytes());
        digest.update(file.logical_path.as_bytes());
        digest.update((file.bytes.len() as u64).to_le_bytes());
        digest.update(&file.bytes);
    }
    format!("{:x}", digest.finalize())
}

#[cfg(test)]
mod tests {
    use super::{CapturedBundle, Source};
    use std::fs;

    fn make_owner_writable(path: &std::path::Path) {
        let mut permissions = fs::metadata(path).unwrap().permissions();
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            permissions.set_mode(permissions.mode() | 0o200);
        }
        #[cfg(not(unix))]
        permissions.set_readonly(false);
        fs::set_permissions(path, permissions).unwrap();
    }

    #[test]
    fn source_mutation_after_one_read_does_not_change_published_bytes() {
        let temporary = tempfile::tempdir().unwrap();
        let source = temporary.path().join("source.c");
        fs::write(&source, b"captured bytes").unwrap();
        let captured = CapturedBundle::read(vec![Source::new("source/model.c", &source)]).unwrap();
        fs::write(&source, b"later mutable bytes").unwrap();
        let destination = temporary.path().join("bundle");
        let evidence = captured.publish(temporary.path(), &destination).unwrap();
        assert_eq!(
            fs::read(destination.join("source/model.c")).unwrap(),
            b"captured bytes"
        );
        assert_eq!(evidence.files.len(), 1);
        assert_eq!(evidence.files[0].bytes, 14);
        evidence.verify().unwrap();
        let published = destination.join("source/model.c");
        make_owner_writable(&published);
        fs::write(&published, b"tampered bytes").unwrap();
        assert!(evidence.verify().is_err());
    }

    #[test]
    fn duplicate_and_escaping_logical_paths_are_rejected() {
        let temporary = tempfile::tempdir().unwrap();
        let source = temporary.path().join("source");
        fs::write(&source, b"bytes").unwrap();
        assert!(
            CapturedBundle::read(vec![
                Source::new("same", &source),
                Source::new("same", &source),
            ])
            .is_err()
        );
        assert!(CapturedBundle::read(vec![Source::new("../escape", &source)]).is_err());
    }
}
