//! Immutable verification inputs share the dependency archive owner. A revision
//! marker alone cannot certify source bytes: every file is checked against the
//! checksum-verified archive before a qualified root is handed to workers.

use super::*;
use std::collections::BTreeSet;

const CORPUS_MANIFEST: &str = "infra/verification/corpus-dependencies.toml";

#[derive(Debug)]
pub(crate) struct QualifiedSource {
    root: PathBuf,
    revision: String,
    archive_sha256: String,
}

impl QualifiedSource {
    pub(crate) fn root(&self) -> &Path {
        &self.root
    }

    pub(crate) fn revision(&self) -> &str {
        &self.revision
    }

    pub(crate) fn archive_sha256(&self) -> &str {
        &self.archive_sha256
    }
}

pub(crate) fn qualify_flight_source(
    root: &Path,
    supplied: Option<&Path>,
) -> Result<QualifiedSource> {
    let manifest = load_manifest_file(&root.join(CORPUS_MANIFEST))?;
    qualify_source(
        root,
        required_library(&manifest, "flight_models")?,
        supplied,
    )
}

fn qualify_source(
    root: &Path,
    spec: &LibrarySpec,
    supplied: Option<&Path>,
) -> Result<QualifiedSource> {
    let data = verified_archive(root, spec)?;
    let source = supplied.map_or_else(|| library_dir(root, spec), Path::to_path_buf);
    if supplied.is_none() && !source.exists() {
        fs::create_dir_all(&source)?;
        extract_zip(&data, &source, spec.zip_layout)?;
    }
    ensure!(
        source.is_dir(),
        "pinned source root is missing: {}",
        source.display()
    );
    check_source_bytes(&data, &source, spec)?;
    Ok(QualifiedSource {
        root: source,
        revision: spec.version.clone(),
        archive_sha256: spec.sha256.clone(),
    })
}

fn verified_archive(root: &Path, spec: &LibrarySpec) -> Result<Vec<u8>> {
    let path = library_dir(root, spec).with_extension("zip");
    if path.exists() {
        ensure!(
            fs::symlink_metadata(&path)?.file_type().is_file(),
            "archive must be a regular file"
        );
        let data = fs::read(&path)?;
        verify_sha256(&data, &spec.sha256, &path.display().to_string())?;
        return Ok(data);
    }
    let data = download_to_memory(&spec.url)?;
    verify_sha256(&data, &spec.sha256, &spec.url)?;
    let parent = path.parent().context("dependency archive has no parent")?;
    fs::create_dir_all(parent)?;
    let mut file = tempfile::NamedTempFile::new_in(parent)?;
    std::io::Write::write_all(&mut file, &data)?;
    file.as_file().sync_all()?;
    file.persist(&path)
        .with_context(|| format!("failed to retain {}", path.display()))?;
    Ok(data)
}

fn check_source_bytes(data: &[u8], source: &Path, spec: &LibrarySpec) -> Result<()> {
    let mut archive = ZipArchive::new(Cursor::new(data))?;
    let strip = matches!(spec.zip_layout, ZipLayout::AutoStripCommonRoot)
        && archive_has_common_root(&mut archive)?;
    let mut expected = BTreeSet::new();
    for index in 0..archive.len() {
        let mut entry = archive.by_index(index)?;
        let enclosed = entry
            .enclosed_name()
            .context("unsafe dependency archive path")?;
        let Some(relative) = zip_output_relative_path(&enclosed, strip) else {
            continue;
        };
        if entry.is_dir() {
            continue;
        }
        ensure!(
            expected.insert(relative.clone()),
            "duplicate archive file {}",
            relative.display()
        );
        let path = source.join(&relative);
        let metadata = fs::symlink_metadata(&path)
            .with_context(|| format!("pinned source is missing {}", path.display()))?;
        ensure!(
            metadata.file_type().is_file(),
            "pinned source must be regular: {}",
            path.display()
        );
        let mut bytes = Vec::new();
        entry.read_to_end(&mut bytes)?;
        ensure!(
            fs::read(&path)? == bytes,
            "pinned source differs from {} at {}",
            spec.version,
            path.display()
        );
    }
    check_source_inventory(source, &expected)?;
    ensure!(
        cache_layout_valid(source, spec),
        "pinned source is missing required packages"
    );
    Ok(())
}

fn check_source_inventory(source: &Path, expected: &BTreeSet<PathBuf>) -> Result<()> {
    let entries = walkdir::WalkDir::new(source)
        .into_iter()
        .filter_entry(|entry| entry.depth() != 1 || entry.file_name() != ".git");
    for entry in entries {
        let entry = entry?;
        if entry.file_type().is_dir() {
            continue;
        }
        let relative = entry.path().strip_prefix(source)?;
        if relative == Path::new(REVISION_FILE) {
            continue;
        }
        ensure!(
            entry.file_type().is_file() && expected.contains(relative),
            "unexpected pinned source file {}",
            entry.path().display()
        );
    }
    Ok(())
}

#[cfg(test)]
mod tests;
