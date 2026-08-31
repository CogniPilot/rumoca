//! One-read, read-only byte capabilities for benchmark process boundaries.

use anyhow::{Context, Result, ensure};
use sha2::{Digest, Sha256};
use std::collections::BTreeSet;
use std::fs::{self, File};
use std::io::Read;
#[cfg(unix)]
use std::os::unix::fs::MetadataExt;
use std::path::{Path, PathBuf};

#[derive(Debug)]
struct FrozenFile {
    relative: PathBuf,
    sha256: String,
    bytes: u64,
}

/// Exact regular-file tree frozen at construction and rechecked after every
/// external consumer. Consumers receive paths only together with this proof.
#[derive(Debug)]
pub(super) struct FrozenArtifactSet {
    root: PathBuf,
    files: Vec<FrozenFile>,
}

impl FrozenArtifactSet {
    pub(super) fn capture_tree(root: &Path) -> Result<Self> {
        ensure!(root.is_absolute(), "frozen artifact root must be absolute");
        let mut relative_files = Vec::new();
        collect_files(root, root, &mut relative_files)?;
        ensure!(!relative_files.is_empty(), "frozen artifact tree is empty");
        relative_files.sort();
        let mut files = Vec::with_capacity(relative_files.len());
        for relative in relative_files {
            let path = root.join(&relative);
            let (sha256, bytes) = file_identity(&path)?;
            freeze_file(&path)?;
            files.push(FrozenFile {
                relative,
                sha256,
                bytes,
            });
        }
        freeze_directories(root)?;
        let frozen = Self {
            root: root.to_path_buf(),
            files,
        };
        frozen.verify()?;
        Ok(frozen)
    }

    pub(super) fn verify(&self) -> Result<()> {
        let mut observed = Vec::new();
        collect_files(&self.root, &self.root, &mut observed)?;
        observed.sort();
        let expected = self
            .files
            .iter()
            .map(|file| file.relative.clone())
            .collect::<Vec<_>>();
        ensure!(observed == expected, "frozen artifact roster changed");
        for file in &self.files {
            let path = self.root.join(&file.relative);
            let (sha256, bytes) = file_identity(&path)?;
            ensure!(
                sha256 == file.sha256 && bytes == file.bytes,
                "frozen artifact changed: {}",
                path.display()
            );
            ensure_read_only(&path)?;
        }
        ensure_directories_read_only(&self.root)
    }

    #[cfg(test)]
    pub(super) fn root(&self) -> &Path {
        &self.root
    }
}

fn collect_files(root: &Path, directory: &Path, files: &mut Vec<PathBuf>) -> Result<()> {
    let metadata = fs::symlink_metadata(directory)
        .with_context(|| format!("failed to inspect {}", directory.display()))?;
    ensure!(metadata.is_dir(), "artifact tree root is not a directory");
    for entry in fs::read_dir(directory)? {
        let entry = entry?;
        let metadata = entry.metadata()?;
        ensure!(
            !entry.file_type()?.is_symlink(),
            "artifact tree contains a symlink: {}",
            entry.path().display()
        );
        if metadata.is_dir() {
            collect_files(root, &entry.path(), files)?;
        } else {
            ensure!(metadata.is_file(), "artifact tree contains a non-file");
            files.push(entry.path().strip_prefix(root)?.to_path_buf());
        }
    }
    Ok(())
}

fn file_identity(path: &Path) -> Result<(String, u64)> {
    let mut file = File::open(path)
        .with_context(|| format!("failed to open frozen artifact {}", path.display()))?;
    let metadata = file.metadata()?;
    ensure!(metadata.is_file(), "frozen artifact is not a regular file");
    ensure_single_link(&metadata, path)?;
    let mut digest = Sha256::new();
    let mut buffer = [0_u8; 64 * 1024];
    let mut bytes = 0_u64;
    loop {
        let count = file.read(&mut buffer)?;
        if count == 0 {
            break;
        }
        bytes = bytes
            .checked_add(count as u64)
            .context("artifact size overflow")?;
        digest.update(&buffer[..count]);
    }
    ensure!(
        bytes == metadata.len(),
        "artifact changed while being captured"
    );
    let path_metadata = fs::metadata(path)?;
    ensure_same_file(&metadata, &path_metadata, path)?;
    Ok((format!("{:x}", digest.finalize()), bytes))
}

#[cfg(unix)]
fn ensure_single_link(metadata: &fs::Metadata, path: &Path) -> Result<()> {
    ensure!(
        metadata.nlink() == 1,
        "frozen artifact has a writable hard-link alias: {}",
        path.display()
    );
    Ok(())
}

#[cfg(not(unix))]
fn ensure_single_link(_metadata: &fs::Metadata, _path: &Path) -> Result<()> {
    Ok(())
}

#[cfg(unix)]
fn ensure_same_file(before: &fs::Metadata, after: &fs::Metadata, path: &Path) -> Result<()> {
    ensure_single_link(after, path)?;
    ensure!(
        before.dev() == after.dev() && before.ino() == after.ino() && before.len() == after.len(),
        "artifact path changed while being captured: {}",
        path.display()
    );
    Ok(())
}

#[cfg(not(unix))]
fn ensure_same_file(before: &fs::Metadata, after: &fs::Metadata, path: &Path) -> Result<()> {
    ensure!(
        after.is_file() && before.len() == after.len(),
        "artifact path changed while being captured: {}",
        path.display()
    );
    Ok(())
}

fn freeze_file(path: &Path) -> Result<()> {
    let mut permissions = fs::metadata(path)?.permissions();
    permissions.set_readonly(true);
    fs::set_permissions(path, permissions)?;
    Ok(())
}

fn freeze_directories(root: &Path) -> Result<()> {
    let mut directories = BTreeSet::new();
    collect_directories(root, &mut directories)?;
    for directory in directories.into_iter().rev() {
        let mut permissions = fs::metadata(&directory)?.permissions();
        permissions.set_readonly(true);
        fs::set_permissions(directory, permissions)?;
    }
    Ok(())
}

fn collect_directories(directory: &Path, directories: &mut BTreeSet<PathBuf>) -> Result<()> {
    directories.insert(directory.to_path_buf());
    for entry in fs::read_dir(directory)? {
        let entry = entry?;
        if entry.file_type()?.is_dir() {
            collect_directories(&entry.path(), directories)?;
        }
    }
    Ok(())
}

fn ensure_read_only(path: &Path) -> Result<()> {
    ensure!(
        fs::metadata(path)?.permissions().readonly(),
        "frozen artifact became writable: {}",
        path.display()
    );
    Ok(())
}

fn ensure_directories_read_only(directory: &Path) -> Result<()> {
    ensure_read_only(directory)?;
    for entry in fs::read_dir(directory)? {
        let entry = entry?;
        if entry.file_type()?.is_dir() {
            ensure_directories_read_only(&entry.path())?;
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::FrozenArtifactSet;
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
    fn frozen_tree_rejects_byte_and_roster_mutation() {
        let temporary = tempfile::tempdir().unwrap();
        let root = temporary.path().join("tree");
        fs::create_dir(&root).unwrap();
        fs::write(root.join("input.c"), "accepted\n").unwrap();
        let frozen = FrozenArtifactSet::capture_tree(&root).unwrap();
        assert!(frozen.verify().is_ok());

        make_owner_writable(frozen.root());
        make_owner_writable(&root.join("input.c"));
        fs::write(root.join("input.c"), "mutated\n").unwrap();
        assert!(frozen.verify().is_err());
    }

    #[cfg(unix)]
    #[test]
    fn frozen_tree_rejects_an_external_hard_link_alias() {
        let temporary = tempfile::tempdir().unwrap();
        let root = temporary.path().join("tree");
        fs::create_dir(&root).unwrap();
        let input = root.join("input.c");
        fs::write(&input, "accepted\n").unwrap();
        fs::hard_link(&input, temporary.path().join("writable-alias.c")).unwrap();
        let error = FrozenArtifactSet::capture_tree(&root)
            .expect_err("a path outside the roster must not retain a writable alias");
        assert!(format!("{error:#}").contains("hard-link alias"));
    }
}
