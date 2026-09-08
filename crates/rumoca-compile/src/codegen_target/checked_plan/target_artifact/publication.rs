//! Transactional publication of already-completed target bytes.

use std::fs;
#[cfg(feature = "fmu-packaging")]
use std::io::Write as _;
use std::path::{Path, PathBuf};

use anyhow::{Context, Result, bail};

#[cfg(feature = "fmu-packaging")]
use super::CompletedPackage;
use super::{
    CompletedPresentation, CompletedRenderedFileRef, CompletedUnpackaged, PublishedTargetArtifact,
};

enum PublicationRecoveryOutcome {
    Complete,
    Incomplete { failures: Vec<anyhow::Error> },
}

impl PublicationRecoveryOutcome {
    fn complete() -> Self {
        Self::Complete
    }

    fn record(&mut self, result: Result<()>) {
        let Err(error) = result else {
            return;
        };
        match self {
            Self::Complete => {
                *self = Self::Incomplete {
                    failures: vec![error],
                };
            }
            Self::Incomplete { failures } => failures.push(error),
        }
    }

    fn rename(&mut self, source: &Path, destination: &Path, action: String) {
        self.record(fs::rename(source, destination).with_context(|| action));
    }

    fn merge(&mut self, other: Self) {
        let Self::Incomplete { failures } = other else {
            return;
        };
        for failure in failures {
            self.record(Err(failure));
        }
    }

    fn preserve_original(self, original: anyhow::Error) -> anyhow::Error {
        let Self::Incomplete { failures } = self else {
            return original;
        };
        let details = failures
            .iter()
            .map(|failure| format!("{failure:#}"))
            .collect::<Vec<_>>()
            .join("; ");
        original.context(format!(
            "publication rollback was incomplete ({} recovery failure(s)): {details}",
            failures.len()
        ))
    }
}

#[cfg(feature = "fmu-packaging")]
impl CompletedPackage {
    pub(super) fn publish(self, host_output_root: &Path) -> Result<PublishedTargetArtifact> {
        let host = resolve_host_root(host_output_root)?;
        let root = checked_host_join(&host, &self.root);
        let archive = self
            .archive
            .as_ref()
            .map(|path| checked_host_join(&host, path));
        if let Some(archive) = &archive
            && (archive == &root || archive.starts_with(&root) || root.starts_with(archive))
        {
            bail!(
                "package archive '{}' must not overlap package root '{}'",
                archive.display(),
                root.display()
            );
        }
        let completion = prepare_completion_message(&self.presentation, &host)?;
        validate_existing_root(&root, &self.required_files)?;
        if let Some(archive) = &archive {
            validate_existing_archive(archive, &self.required_files)?;
        }

        let root_parent = root
            .parent()
            .context("checked package root must have a parent")?;
        fs::create_dir_all(root_parent)
            .with_context(|| format!("Create package parent '{}'", root_parent.display()))?;
        let staged_root = tempfile::Builder::new()
            .prefix(".rumoca-package-")
            .tempdir_in(root_parent)
            .with_context(|| {
                format!(
                    "Create package staging directory in '{}'",
                    root_parent.display()
                )
            })?;
        for member in self.members() {
            let output = staged_root.path().join(member.path());
            if let Some(parent) = output.parent() {
                fs::create_dir_all(parent)
                    .with_context(|| format!("Create directory for '{}'", output.display()))?;
            }
            fs::write(&output, member.bytes())
                .with_context(|| format!("Write staged package member '{}'", output.display()))?;
        }

        let staged_archive = archive
            .as_ref()
            .map(|path| stage_archive(&self, path))
            .transpose()?;
        install_package(staged_root, &root, staged_archive, archive.as_deref())?;
        print_completion_message(completion);
        Ok(PublishedTargetArtifact { root, archive })
    }
}

pub(super) fn publish_unpacked(
    files: CompletedUnpackaged,
    host_output_root: &Path,
) -> Result<PublishedTargetArtifact> {
    let host = resolve_host_root(host_output_root)?;
    let completion = prepare_completion_message(&files.presentation, &host)?;
    fs::create_dir_all(&host)
        .with_context(|| format!("Create target output root '{}'", host.display()))?;
    let stage = tempfile::Builder::new()
        .prefix(".rumoca-files-")
        .tempdir_in(&host)
        .with_context(|| format!("Create target staging directory in '{}'", host.display()))?;
    for file in files.files() {
        stage_file(stage.path(), file)?;
    }
    install_unpacked_files(&host, stage, files.files())?;
    print_completion_message(completion);
    Ok(PublishedTargetArtifact {
        root: host,
        archive: None,
    })
}

fn resolve_host_root(path: &Path) -> Result<PathBuf> {
    if path.as_os_str().is_empty() {
        bail!("target output root must not be empty");
    }
    std::path::absolute(path)
        .with_context(|| format!("Resolve target output root '{}'", path.display()))
}

#[cfg(feature = "fmu-packaging")]
fn checked_host_join(host: &Path, checked_relative: &Path) -> PathBuf {
    debug_assert!(checked_relative.is_relative());
    host.join(checked_relative)
}

#[cfg(feature = "fmu-packaging")]
fn validate_existing_root(root: &Path, required: &[Box<str>]) -> Result<()> {
    if !root.exists() {
        return Ok(());
    }
    let previous_product = root.is_dir()
        && required
            .iter()
            .all(|required| root.join(required.as_ref()).is_file());
    if previous_product {
        return Ok(());
    }
    let empty = root.is_dir()
        && fs::read_dir(root)
            .with_context(|| format!("Read product directory '{}'", root.display()))?
            .next()
            .is_none();
    if empty {
        return Ok(());
    }
    bail!(
        "product output path '{}' is not a prior product carrying every required marker",
        root.display()
    )
}

#[cfg(feature = "fmu-packaging")]
fn validate_existing_archive(path: &Path, required: &[Box<str>]) -> Result<()> {
    if !path.exists() {
        return Ok(());
    }
    let metadata = fs::symlink_metadata(path)
        .with_context(|| format!("Inspect package archive '{}'", path.display()))?;
    if !metadata.file_type().is_file() {
        bail!(
            "package archive path '{}' exists but is not a regular file",
            path.display()
        );
    }
    let file = fs::File::open(path)
        .with_context(|| format!("Open previous package archive '{}'", path.display()))?;
    let mut archive = zip::ZipArchive::new(file).with_context(|| {
        format!(
            "package archive '{}' is not a recognized previous product",
            path.display()
        )
    })?;
    for required in required {
        let entry = archive.by_name(required).with_context(|| {
            format!(
                "package archive '{}' is missing required marker '{}'",
                path.display(),
                required
            )
        })?;
        if !entry.is_file() {
            bail!(
                "package archive '{}' has non-file marker '{}'",
                path.display(),
                required
            );
        }
    }
    Ok(())
}

#[cfg(feature = "fmu-packaging")]
struct StagedArchive {
    directory: tempfile::TempDir,
    file: PathBuf,
}

#[cfg(feature = "fmu-packaging")]
fn stage_archive(package: &CompletedPackage, final_path: &Path) -> Result<StagedArchive> {
    let parent = final_path
        .parent()
        .context("checked archive path must have a parent")?;
    fs::create_dir_all(parent)
        .with_context(|| format!("Create archive parent '{}'", parent.display()))?;
    let directory = tempfile::Builder::new()
        .prefix(".rumoca-archive-")
        .tempdir_in(parent)
        .with_context(|| format!("Create archive staging directory in '{}'", parent.display()))?;
    let file = directory.path().join("package.zip");
    let output = fs::File::create(&file)
        .with_context(|| format!("Create staged archive '{}'", file.display()))?;
    let mut archive = zip::ZipWriter::new(output);
    let options = zip::write::SimpleFileOptions::default()
        .compression_method(zip::CompressionMethod::Deflated)
        .last_modified_time(zip::DateTime::default())
        .unix_permissions(0o644);
    for member in package.members() {
        archive
            .start_file(member.path(), options)
            .with_context(|| format!("Start zip member '{}'", member.path()))?;
        archive
            .write_all(member.bytes())
            .with_context(|| format!("Write zip member '{}'", member.path()))?;
    }
    archive.finish().context("Finish staged package archive")?;
    Ok(StagedArchive { directory, file })
}

#[cfg(feature = "fmu-packaging")]
fn install_package(
    staged_root: tempfile::TempDir,
    final_root: &Path,
    staged_archive: Option<StagedArchive>,
    final_archive: Option<&Path>,
) -> Result<()> {
    let parent = final_root
        .parent()
        .context("checked package root must have a parent")?;
    let transaction = tempfile::Builder::new()
        .prefix(".rumoca-replace-")
        .tempdir_in(parent)
        .with_context(|| format!("Create replacement transaction in '{}'", parent.display()))?;
    let old_root = transaction.path().join("previous-product");
    let had_root = final_root.exists();
    if had_root {
        fs::rename(final_root, &old_root)
            .with_context(|| format!("Stage previous product '{}'", final_root.display()))?;
    }

    let old_archive = staged_archive
        .as_ref()
        .map(|archive| archive.directory.path().join("previous"));
    let had_archive = final_archive.is_some_and(Path::exists);
    if let (Some(final_archive), Some(old_archive)) = (final_archive, old_archive.as_deref())
        && had_archive
        && let Err(error) = fs::rename(final_archive, old_archive)
    {
        let original = anyhow::Error::new(error).context(format!(
            "Stage previous archive '{}'",
            final_archive.display()
        ));
        let mut recovery = PublicationRecoveryOutcome::complete();
        if had_root {
            recovery.rename(
                &old_root,
                final_root,
                format!("Restore previous product '{}'", final_root.display()),
            );
        }
        return Err(recovery.preserve_original(original));
    }

    if let Err(error) = fs::rename(staged_root.path(), final_root) {
        let original = anyhow::Error::new(error)
            .context(format!("Install staged product '{}'", final_root.display()));
        let mut recovery = PublicationRecoveryOutcome::complete();
        if had_archive
            && let (Some(old_archive), Some(final_archive)) =
                (old_archive.as_deref(), final_archive)
        {
            recovery.rename(
                old_archive,
                final_archive,
                format!("Restore previous archive '{}'", final_archive.display()),
            );
        }
        if had_root {
            recovery.rename(
                &old_root,
                final_root,
                format!("Restore previous product '{}'", final_root.display()),
            );
        }
        return Err(recovery.preserve_original(original));
    }

    if let (Some(staged), Some(final_archive)) = (staged_archive.as_ref(), final_archive)
        && let Err(error) = fs::rename(&staged.file, final_archive)
    {
        let original = anyhow::Error::new(error).context(format!(
            "Install package archive '{}'",
            final_archive.display()
        ));
        let mut recovery = PublicationRecoveryOutcome::complete();
        recovery.rename(
            final_root,
            staged_root.path(),
            format!(
                "Return partially installed product '{}' to staging",
                final_root.display()
            ),
        );
        if had_archive && let Some(old_archive) = old_archive.as_deref() {
            recovery.rename(
                old_archive,
                final_archive,
                format!("Restore previous archive '{}'", final_archive.display()),
            );
        }
        if had_root {
            recovery.rename(
                &old_root,
                final_root,
                format!("Restore previous product '{}'", final_root.display()),
            );
        }
        return Err(recovery.preserve_original(original));
    }
    Ok(())
}

fn stage_file(root: &Path, file: CompletedRenderedFileRef<'_>) -> Result<()> {
    let path = root.join(file.path());
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("Create staging directory '{}'", parent.display()))?;
    }
    fs::write(&path, file.content())
        .with_context(|| format!("Write staged target file '{}'", path.display()))?;
    apply_mode(&path, file.mode())
}

fn install_unpacked_files<'a>(
    host: &Path,
    stage: tempfile::TempDir,
    files: impl ExactSizeIterator<Item = CompletedRenderedFileRef<'a>>,
) -> Result<()> {
    let transaction = tempfile::Builder::new()
        .prefix(".rumoca-files-replace-")
        .tempdir_in(host)
        .with_context(|| {
            format!(
                "Create file replacement transaction in '{}'",
                host.display()
            )
        })?;
    let files = files.collect::<Vec<_>>();
    let mut backed_up = Vec::new();
    for (index, file) in files.iter().enumerate() {
        let final_path = host.join(file.path());
        reject_symlink_parents(host, &final_path)?;
        if final_path.exists() {
            let metadata = fs::symlink_metadata(&final_path)
                .with_context(|| format!("Inspect existing target '{}'", final_path.display()))?;
            if !metadata.file_type().is_file() {
                let original = anyhow::anyhow!(
                    "target output '{}' exists but is not a regular file",
                    final_path.display()
                );
                return Err(rollback_backups(&backed_up).preserve_original(original));
            }
            let backup = transaction.path().join(index.to_string());
            fs::rename(&final_path, &backup)
                .with_context(|| format!("Stage previous target '{}'", final_path.display()))?;
            backed_up.push((backup, final_path));
        }
    }

    let mut installed: Vec<PathBuf> = Vec::new();
    for file in &files {
        let staged = stage.path().join(file.path());
        let final_path = host.join(file.path());
        if let Some(parent) = final_path.parent() {
            fs::create_dir_all(parent)
                .with_context(|| format!("Create output directory '{}'", parent.display()))?;
        }
        if let Err(error) = fs::rename(&staged, &final_path) {
            let original = anyhow::Error::new(error)
                .context(format!("Install target file '{}'", final_path.display()));
            let mut recovery = rollback_installed_files(host, stage.path(), &installed);
            recovery.merge(rollback_backups(&backed_up));
            return Err(recovery.preserve_original(original));
        }
        installed.push(final_path);
    }
    Ok(())
}

fn rollback_installed_files(
    host: &Path,
    stage: &Path,
    installed: &[PathBuf],
) -> PublicationRecoveryOutcome {
    let mut recovery = PublicationRecoveryOutcome::complete();
    for installed_path in installed.iter().rev() {
        let relative = installed_path
            .strip_prefix(host)
            .expect("installed target remains under host");
        recovery.rename(
            installed_path,
            &stage.join(relative),
            format!(
                "Return partially installed target '{}' to staging",
                installed_path.display()
            ),
        );
    }
    recovery
}

fn rollback_backups(backups: &[(PathBuf, PathBuf)]) -> PublicationRecoveryOutcome {
    let mut recovery = PublicationRecoveryOutcome::complete();
    for (backup, final_path) in backups.iter().rev() {
        if final_path.exists() {
            recovery.record(fs::remove_file(final_path).with_context(|| {
                format!(
                    "Remove partially installed target '{}'",
                    final_path.display()
                )
            }));
        }
        recovery.rename(
            backup,
            final_path,
            format!("Restore previous target '{}'", final_path.display()),
        );
    }
    recovery
}

fn reject_symlink_parents(host: &Path, final_path: &Path) -> Result<()> {
    let relative = final_path
        .strip_prefix(host)
        .expect("completed target paths remain beneath the host root");
    let mut current = host.to_path_buf();
    for component in relative
        .components()
        .take(relative.components().count() - 1)
    {
        current.push(component);
        match fs::symlink_metadata(&current) {
            Ok(metadata) if metadata.file_type().is_symlink() => {
                bail!(
                    "target parent '{}' must not be a symlink",
                    current.display()
                )
            }
            Ok(metadata) if !metadata.is_dir() => {
                bail!("target parent '{}' must be a directory", current.display())
            }
            Ok(_) => {}
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => break,
            Err(error) => {
                return Err(anyhow::Error::new(error)
                    .context(format!("Inspect target parent '{}'", current.display())));
            }
        }
    }
    Ok(())
}

fn apply_mode(path: &Path, mode: Option<u32>) -> Result<()> {
    let Some(mode) = mode else {
        return Ok(());
    };
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt as _;
        fs::set_permissions(path, fs::Permissions::from_mode(mode))
            .with_context(|| format!("Set target permissions on '{}'", path.display()))?;
    }
    #[cfg(not(unix))]
    let _ = (path, mode);
    Ok(())
}

fn prepare_completion_message(
    presentation: &CompletedPresentation,
    output: &Path,
) -> Result<Option<String>> {
    let output = output
        .to_str()
        .with_context(|| format!("published target path '{}' is not UTF-8", output.display()))?;
    presentation
        .completion_message
        .render(output)
        .map_err(Into::into)
}

fn print_completion_message(message: Option<String>) {
    if let Some(message) = message {
        eprintln!("{message}");
    }
}

#[cfg(all(test, feature = "fmu-packaging"))]
mod tests {
    use std::path::PathBuf;

    use super::{CompletedPackage, CompletedPresentation};

    #[test]
    fn packaged_completion_message_uses_the_host_output_root() {
        let host = tempfile::tempdir().expect("isolated publication root");
        let expected = host.path().to_str().expect("UTF-8 temporary path");
        let completion_message =
            "{% if out_dir == target_name %}published{% else %}{{ missing }}{% endif %}";
        let presentation = CompletedPresentation {
            label: "publication-test".into(),
            description: None,
            completion_message: rumoca_phase_codegen::PreparedCompletionMessage::construct(
                Some(completion_message),
                "Example",
                expected,
            )
            .expect("strict completion message"),
        };
        let package = CompletedPackage {
            presentation,
            root: PathBuf::from("Example"),
            archive: None,
            required_files: Box::new([]),
            members: super::super::CompletedMixedPlan {
                members: Box::new([]),
            },
        };

        let published = package
            .publish(host.path())
            .expect("completion message receives host output root");
        assert_eq!(
            published.root(),
            host.path().join("Example"),
            "package installation still uses the distinct package root"
        );
    }
}
