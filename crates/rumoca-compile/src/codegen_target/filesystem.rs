use super::*;

pub(crate) fn safe_target_join(root: &Path, relative: impl AsRef<Path>) -> Result<PathBuf> {
    let relative = relative.as_ref();
    if relative.as_os_str().is_empty() {
        bail!("Target manifest path must not be empty");
    }
    if relative.is_absolute() {
        bail!(
            "Target manifest path '{}' must be relative",
            relative.display()
        );
    }
    for component in relative.components() {
        match component {
            Component::Normal(_) | Component::CurDir => {}
            Component::ParentDir | Component::RootDir | Component::Prefix(_) => {
                bail!(
                    "Target manifest path '{}' must not escape the target root",
                    relative.display()
                );
            }
        }
    }
    Ok(root.join(relative))
}

/// One template, manifest, or asset member may contribute at most the eFMU
/// precedent's per-member 16 MiB bound to a checked target snapshot.
pub(super) const MAX_TARGET_INPUT_FILE_BYTES: u64 = 16 * 1024 * 1024;
pub(super) const MAX_CHECKED_TARGET_SNAPSHOT_BYTES: u64 = 64 * 1024 * 1024;

pub(super) struct TargetSnapshotBudget {
    retained_bytes: u64,
}

impl TargetSnapshotBudget {
    pub(super) const fn new() -> Self {
        Self { retained_bytes: 0 }
    }

    pub(super) fn admit(&mut self, label: &str, bytes: usize) -> Result<()> {
        let bytes = u64::try_from(bytes)
            .with_context(|| format!("target input '{label}' size cannot be represented as u64"))?;
        if bytes > MAX_TARGET_INPUT_FILE_BYTES {
            bail!(
                "target input '{label}' exceeds the {MAX_TARGET_INPUT_FILE_BYTES}-byte member bound"
            );
        }
        let retained_bytes = self.retained_bytes.checked_add(bytes).with_context(|| {
            format!("checked target snapshot byte count overflow while admitting '{label}'")
        })?;
        if retained_bytes > MAX_CHECKED_TARGET_SNAPSHOT_BYTES {
            bail!(
                "checked target snapshot exceeds the {MAX_CHECKED_TARGET_SNAPSHOT_BYTES}-byte cumulative bound while admitting '{label}'"
            );
        }
        self.retained_bytes = retained_bytes;
        Ok(())
    }
}

pub(super) fn read_utf8_regular_file_bounded(path: &Path) -> Result<String> {
    let bytes = read_regular_file_bounded(path)?;
    String::from_utf8(bytes)
        .with_context(|| format!("target text file must be valid UTF-8: '{}'", path.display()))
}

/// Read exact bytes from one final-component no-follow handle.
///
/// Metadata and bytes come from the same handle. A size change during the read
/// rejects rather than exposing a partial or substituted snapshot.
pub(super) fn read_regular_file_bounded(path: &Path) -> Result<Vec<u8>> {
    let file = open_regular_file_no_follow(path)?;
    let metadata = file
        .metadata()
        .with_context(|| format!("Inspect opened target file '{}'", path.display()))?;
    if !metadata.is_file() || metadata.len() > MAX_TARGET_INPUT_FILE_BYTES {
        bail!(
            "target input '{}' must be a regular file of at most {MAX_TARGET_INPUT_FILE_BYTES} bytes",
            path.display()
        );
    }
    let capacity = usize::try_from(metadata.len()).with_context(|| {
        format!(
            "target input '{}' size cannot be represented on this host",
            path.display()
        )
    })?;
    let mut bytes = Vec::with_capacity(capacity);
    file.take(MAX_TARGET_INPUT_FILE_BYTES + 1)
        .read_to_end(&mut bytes)
        .with_context(|| format!("Read opened target file '{}'", path.display()))?;
    let observed = u64::try_from(bytes.len()).with_context(|| {
        format!(
            "target input '{}' observed size cannot be represented as u64",
            path.display()
        )
    })?;
    if observed > MAX_TARGET_INPUT_FILE_BYTES {
        bail!(
            "target input '{}' grew beyond {MAX_TARGET_INPUT_FILE_BYTES} bytes while reading",
            path.display()
        );
    }
    if observed != metadata.len() {
        bail!(
            "target input '{}' changed length while its checked snapshot was being read",
            path.display()
        );
    }
    Ok(bytes)
}

#[cfg(unix)]
pub(super) fn open_regular_file_no_follow(path: &Path) -> Result<fs::File> {
    use std::os::unix::fs::OpenOptionsExt as _;

    fs::OpenOptions::new()
        .read(true)
        .custom_flags(libc::O_NOFOLLOW | libc::O_NONBLOCK)
        .open(path)
        .with_context(|| {
            format!(
                "Open target input '{}' without following a symbolic link",
                path.display()
            )
        })
}

#[cfg(windows)]
pub(super) fn open_regular_file_no_follow(path: &Path) -> Result<fs::File> {
    use std::os::windows::fs::OpenOptionsExt as _;

    const FILE_FLAG_OPEN_REPARSE_POINT: u32 = 0x0020_0000;
    fs::OpenOptions::new()
        .read(true)
        .custom_flags(FILE_FLAG_OPEN_REPARSE_POINT)
        .open(path)
        .with_context(|| {
            format!(
                "Open target input '{}' without traversing its final reparse point",
                path.display()
            )
        })
}

#[cfg(not(any(unix, windows)))]
pub(super) fn open_regular_file_no_follow(path: &Path) -> Result<fs::File> {
    bail!(
        "directory target inputs are unavailable on this host because '{}' cannot be opened with a supported no-follow policy",
        path.display()
    )
}
