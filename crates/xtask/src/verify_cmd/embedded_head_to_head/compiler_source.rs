//! One-read, gate-owned source closure for building the compiler under test.

use super::manifest;
use super::process;
use super::typed_path::{AuthenticatedExecutable, GitExecutable};
use anyhow::{Context, Result, anyhow, ensure};
use serde::Serialize;
use sha2::{Digest, Sha256};
use std::fs::{self, File, OpenOptions};
use std::io::{Read, Write};
use std::path::{Component, Path, PathBuf};

const MAX_SOURCE_FILE_BYTES: u64 = 128 * 1024 * 1024;
const MAX_SOURCE_CLOSURE_BYTES: u64 = 512 * 1024 * 1024;

pub(super) struct CompilerSourceSnapshot {
    root: PathBuf,
    roster: Vec<PathBuf>,
    frozen_closure_sha256: String,
    evidence: CompilerSourceEvidence,
    commands: Vec<process::CommandReceipt>,
}

#[derive(Serialize)]
pub(super) struct CompilerSourceEvidence {
    pub(super) head_commit: String,
    pub(super) workspace_dirty: bool,
    pub(super) workspace_status_sha256: String,
    pub(super) closure_sha256: String,
    pub(super) roster_count: usize,
}

impl CompilerSourceSnapshot {
    pub(super) fn root(&self) -> &Path {
        &self.root
    }

    pub(super) fn commands(&self) -> &[process::CommandReceipt] {
        &self.commands
    }

    pub(super) fn verify_after_build(&self) -> Result<()> {
        verify_staged_roster(&self.root, &self.roster, &self.frozen_closure_sha256)?;
        ensure_tree_read_only(&self.root)
    }

    pub(super) fn into_evidence(self) -> (CompilerSourceEvidence, Vec<process::CommandReceipt>) {
        (self.evidence, self.commands)
    }
}

pub(super) fn stage(
    workspace: &Path,
    destination: &Path,
    git: &AuthenticatedExecutable<GitExecutable>,
) -> Result<CompilerSourceSnapshot> {
    let mut commands = Vec::new();
    match stage_inner(workspace, destination, git, &mut commands) {
        Ok(mut snapshot) => {
            snapshot.commands = commands;
            Ok(snapshot)
        }
        Err(error) => Err(process::attach_prior_receipts(error, commands)),
    }
}

fn stage_inner(
    workspace: &Path,
    destination: &Path,
    git: &AuthenticatedExecutable<GitExecutable>,
    commands: &mut Vec<process::CommandReceipt>,
) -> Result<CompilerSourceSnapshot> {
    ensure!(
        workspace.is_absolute(),
        "compiler workspace must be absolute"
    );
    ensure!(
        destination.is_absolute() && !destination.exists(),
        "compiler source snapshot must be a fresh absolute path"
    );
    let before = capture_workspace_identity(workspace, git, commands, "capture")?;
    ensure!(
        !before.roster.is_empty(),
        "compiler source roster is empty after deletions"
    );
    fs::create_dir_all(destination)
        .with_context(|| format!("failed to create {}", destination.display()))?;
    let closure_sha256 = stage_roster(workspace, destination, &before.roster)?;
    verify_staged_roster(destination, &before.roster, &closure_sha256)?;
    let after = capture_workspace_identity(workspace, git, commands, "recheck")?;
    ensure!(
        after == before,
        "compiler source HEAD, roster, or dirty identity changed during snapshot"
    );
    ensure!(
        digest_roster(workspace, &before.roster)? == closure_sha256,
        "compiler source bytes, kinds, modes, or symlink targets changed during snapshot"
    );
    freeze_tree(destination)?;
    ensure_tree_read_only(destination)?;
    let frozen_closure_sha256 = digest_roster(destination, &before.roster)?;

    Ok(CompilerSourceSnapshot {
        root: destination.to_path_buf(),
        roster: before.roster.clone(),
        frozen_closure_sha256,
        evidence: CompilerSourceEvidence {
            head_commit: before.head_commit,
            workspace_dirty: !before.status.is_empty(),
            workspace_status_sha256: format!("{:x}", Sha256::digest(&before.status)),
            closure_sha256,
            roster_count: before.roster.len(),
        },
        commands: Vec::new(),
    })
}

#[derive(PartialEq, Eq)]
struct WorkspaceIdentity {
    head_commit: String,
    roster: Vec<PathBuf>,
    status: Vec<u8>,
}

fn capture_workspace_identity(
    workspace: &Path,
    git: &AuthenticatedExecutable<GitExecutable>,
    commands: &mut Vec<process::CommandReceipt>,
    phase: &str,
) -> Result<WorkspaceIdentity> {
    let (head, receipt) = manifest::git_control_output(
        workspace,
        git,
        ["rev-parse", "--verify", "--end-of-options", "HEAD^{commit}"],
    )?;
    require_git_success(&head, &receipt, &format!("{phase} compiler source HEAD"))?;
    commands.push(receipt);
    let (roster, receipt) = manifest::git_control_output(
        workspace,
        git,
        [
            "ls-files",
            "--cached",
            "--others",
            "--exclude-standard",
            "-z",
            "--",
        ],
    )?;
    require_git_success(
        &roster,
        &receipt,
        &format!("{phase} compiler source roster"),
    )?;
    commands.push(receipt);
    let (deleted, receipt) =
        manifest::git_control_output(workspace, git, ["ls-files", "--deleted", "-z", "--"])?;
    require_git_success(
        &deleted,
        &receipt,
        &format!("{phase} deleted compiler sources"),
    )?;
    commands.push(receipt);
    let mut paths = parse_roster(&roster.stdout)?;
    let deleted = parse_optional_roster(&deleted.stdout)?;
    paths.retain(|path| deleted.binary_search(path).is_err());
    let (status, receipt) = manifest::git_control_output(
        workspace,
        git,
        ["status", "--porcelain=v1", "-z", "--untracked-files=all"],
    )?;
    require_git_success(
        &status,
        &receipt,
        &format!("{phase} compiler source status"),
    )?;
    commands.push(receipt);
    Ok(WorkspaceIdentity {
        head_commit: parse_commit(&head.stdout)?,
        roster: paths,
        status: status.stdout,
    })
}

fn freeze_tree(path: &Path) -> Result<()> {
    let metadata = fs::symlink_metadata(path)?;
    if metadata.file_type().is_symlink() {
        return Ok(());
    }
    if metadata.is_dir() {
        for entry in fs::read_dir(path)? {
            freeze_tree(&entry?.path())?;
        }
    }
    let mut permissions = metadata.permissions();
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        permissions.set_mode(permissions.mode() & !0o222);
    }
    #[cfg(not(unix))]
    permissions.set_readonly(true);
    fs::set_permissions(path, permissions)?;
    Ok(())
}

fn ensure_tree_read_only(path: &Path) -> Result<()> {
    let metadata = fs::symlink_metadata(path)?;
    if metadata.file_type().is_symlink() {
        return Ok(());
    }
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        ensure!(
            metadata.permissions().mode() & 0o222 == 0,
            "compiler source snapshot became writable: {}",
            path.display()
        );
    }
    #[cfg(not(unix))]
    ensure!(
        metadata.permissions().readonly(),
        "compiler source snapshot became writable: {}",
        path.display()
    );
    if metadata.is_dir() {
        for entry in fs::read_dir(path)? {
            ensure_tree_read_only(&entry?.path())?;
        }
    }
    Ok(())
}

fn require_git_success(
    output: &std::process::Output,
    receipt: &process::CommandReceipt,
    purpose: &str,
) -> Result<()> {
    if output.status.success() {
        return Ok(());
    }
    Err(process::attach_attempted_receipt(
        anyhow!(
            "{purpose} failed ({})\n{}",
            output.status,
            process::tail(&process::combined(output))
        ),
        receipt.clone(),
    ))
}

fn parse_commit(bytes: &[u8]) -> Result<String> {
    let text = std::str::from_utf8(bytes).context("compiler HEAD identity is not UTF-8")?;
    let commit = text.trim_end_matches(['\r', '\n']);
    ensure!(
        (commit.len() == 40 || commit.len() == 64)
            && commit
                .bytes()
                .all(|byte| byte.is_ascii_digit() || (b'a'..=b'f').contains(&byte)),
        "compiler HEAD identity is malformed"
    );
    Ok(commit.to_owned())
}

fn parse_roster(bytes: &[u8]) -> Result<Vec<PathBuf>> {
    ensure!(
        !bytes.is_empty() && bytes.last() == Some(&0),
        "compiler source roster must be nonempty and NUL terminated"
    );
    parse_optional_roster(bytes)
}

fn parse_optional_roster(bytes: &[u8]) -> Result<Vec<PathBuf>> {
    if bytes.is_empty() {
        return Ok(Vec::new());
    }
    ensure!(
        bytes.last() == Some(&0),
        "compiler source roster must be NUL terminated"
    );
    let mut roster = bytes[..bytes.len() - 1]
        .split(|byte| *byte == 0)
        .map(|entry| {
            ensure!(
                !entry.is_empty(),
                "compiler source roster contains an empty path"
            );
            let text = std::str::from_utf8(entry)
                .context("compiler source roster contains a non-UTF-8 path")?;
            validate_relative_path(Path::new(text))
        })
        .collect::<Result<Vec<_>>>()?;
    roster.sort();
    for pair in roster.windows(2) {
        ensure!(
            pair[0] != pair[1],
            "compiler source roster contains a duplicate"
        );
    }
    Ok(roster)
}

fn validate_relative_path(path: &Path) -> Result<PathBuf> {
    ensure!(!path.as_os_str().is_empty(), "source path is empty");
    let mut normalized = PathBuf::new();
    for component in path.components() {
        match component {
            Component::Normal(name) => {
                ensure!(
                    name != ".git" && name != "target",
                    "source path enters forbidden component {}",
                    path.display()
                );
                normalized.push(name);
            }
            _ => ensure!(
                false,
                "source path is not normalized relative: {}",
                path.display()
            ),
        }
    }
    Ok(normalized)
}

fn stage_roster(source_root: &Path, destination: &Path, roster: &[PathBuf]) -> Result<String> {
    let mut digest = Sha256::new();
    digest.update(b"rumoca-compiler-source-closure-v1\0");
    let mut total_bytes = 0_u64;
    for relative in roster {
        let source = source_root.join(relative);
        let staged = destination.join(relative);
        let metadata = fs::symlink_metadata(&source)
            .with_context(|| format!("failed to inspect compiler source {}", source.display()))?;
        if metadata.file_type().is_file() {
            total_bytes = stage_regular_file(
                &source,
                &staged,
                relative,
                &metadata,
                &mut digest,
                total_bytes,
            )?;
        } else if metadata.file_type().is_symlink() {
            stage_symlink(&source, &staged, relative, &mut digest)?;
        } else {
            ensure!(
                false,
                "compiler source roster contains special file {}",
                source.display()
            );
        }
    }
    Ok(format!("{:x}", digest.finalize()))
}

fn stage_regular_file(
    source: &Path,
    staged: &Path,
    relative: &Path,
    metadata: &fs::Metadata,
    digest: &mut Sha256,
    total_before: u64,
) -> Result<u64> {
    ensure!(
        metadata.len() <= MAX_SOURCE_FILE_BYTES,
        "compiler source file exceeds per-file bound: {}",
        source.display()
    );
    let total = total_before
        .checked_add(metadata.len())
        .context("compiler source closure byte count overflowed")?;
    ensure!(
        total <= MAX_SOURCE_CLOSURE_BYTES,
        "compiler source closure exceeds byte bound"
    );
    if let Some(parent) = staged.parent() {
        fs::create_dir_all(parent)?;
    }
    let mut input = File::open(source)
        .with_context(|| format!("failed to open compiler source {}", source.display()))?;
    let opened_metadata = input.metadata()?;
    ensure!(
        opened_metadata.is_file()
            && opened_metadata.len() == metadata.len()
            && regular_mode(&opened_metadata) == regular_mode(metadata),
        "compiler source changed kind after roster authentication"
    );
    let mut output = OpenOptions::new()
        .write(true)
        .create_new(true)
        .open(staged)?;
    update_entry_header(
        digest,
        b"file",
        relative,
        metadata.len(),
        regular_mode(metadata),
    );
    let mut observed = 0_u64;
    let mut buffer = [0_u8; 64 * 1024];
    loop {
        let count = input.read(&mut buffer)?;
        if count == 0 {
            break;
        }
        observed = observed
            .checked_add(u64::try_from(count)?)
            .context("compiler source size overflowed")?;
        ensure!(
            observed <= metadata.len(),
            "compiler source grew while staged"
        );
        digest.update(&buffer[..count]);
        output.write_all(&buffer[..count])?;
    }
    ensure!(
        observed == metadata.len(),
        "compiler source changed size while staged"
    );
    output.sync_all()?;
    fs::set_permissions(staged, metadata.permissions())?;
    Ok(total)
}

fn stage_symlink(source: &Path, staged: &Path, relative: &Path, digest: &mut Sha256) -> Result<()> {
    let target = fs::read_link(source)?;
    ensure!(
        !target.is_absolute(),
        "compiler source symlink target is absolute"
    );
    let normalized = normalize_symlink_target(relative, &target)?;
    ensure!(
        !normalized.as_os_str().is_empty(),
        "compiler source symlink resolves to snapshot root"
    );
    if let Some(parent) = staged.parent() {
        fs::create_dir_all(parent)?;
    }
    #[cfg(unix)]
    std::os::unix::fs::symlink(&target, staged)?;
    #[cfg(not(unix))]
    ensure!(
        false,
        "compiler source symlinks require Unix staging support"
    );
    let target = target
        .to_str()
        .context("compiler source symlink target is not UTF-8")?;
    update_entry_header(
        digest,
        b"symlink",
        relative,
        u64::try_from(target.len())?,
        0,
    );
    digest.update(target.as_bytes());
    Ok(())
}

fn normalize_symlink_target(relative: &Path, target: &Path) -> Result<PathBuf> {
    let mut normalized = relative.parent().unwrap_or(Path::new("")).to_path_buf();
    for component in target.components() {
        match component {
            Component::CurDir => {}
            Component::Normal(name) => {
                ensure!(
                    name != ".git" && name != "target",
                    "symlink enters forbidden path"
                );
                normalized.push(name);
            }
            Component::ParentDir => {
                ensure!(normalized.pop(), "compiler source symlink escapes snapshot");
            }
            _ => ensure!(false, "compiler source symlink target is not relative"),
        }
    }
    Ok(normalized)
}

fn update_entry_header(digest: &mut Sha256, kind: &[u8], relative: &Path, length: u64, mode: u32) {
    digest.update(kind);
    digest.update(b"\0");
    digest.update(relative.to_string_lossy().as_bytes());
    digest.update(b"\0");
    digest.update(length.to_le_bytes());
    digest.update(b"\0");
    digest.update(mode.to_le_bytes());
    digest.update(b"\0");
}

#[cfg(unix)]
fn regular_mode(metadata: &fs::Metadata) -> u32 {
    use std::os::unix::fs::PermissionsExt;
    metadata.permissions().mode() & 0o777
}

#[cfg(not(unix))]
fn regular_mode(_metadata: &fs::Metadata) -> u32 {
    0
}

fn verify_staged_roster(root: &Path, expected: &[PathBuf], expected_digest: &str) -> Result<()> {
    let mut observed = Vec::new();
    collect_leaves(root, root, &mut observed)?;
    observed.sort();
    ensure!(
        observed == expected,
        "staged compiler source roster changed"
    );
    let digest = digest_roster(root, expected)?;
    ensure!(
        digest == expected_digest,
        "staged compiler source bytes changed during authentication"
    );
    Ok(())
}

fn collect_leaves(root: &Path, directory: &Path, leaves: &mut Vec<PathBuf>) -> Result<()> {
    for entry in fs::read_dir(directory)? {
        let entry = entry?;
        let metadata = fs::symlink_metadata(entry.path())?;
        if metadata.is_dir() {
            collect_leaves(root, &entry.path(), leaves)?;
        } else {
            leaves.push(entry.path().strip_prefix(root)?.to_path_buf());
        }
    }
    Ok(())
}

fn digest_roster(root: &Path, roster: &[PathBuf]) -> Result<String> {
    let mut digest = Sha256::new();
    digest.update(b"rumoca-compiler-source-closure-v1\0");
    let mut total = 0_u64;
    for relative in roster {
        let path = root.join(relative);
        let metadata = fs::symlink_metadata(&path)?;
        if metadata.is_file() {
            ensure!(
                metadata.len() <= MAX_SOURCE_FILE_BYTES,
                "compiler source recheck exceeds per-file bound"
            );
            total = total
                .checked_add(metadata.len())
                .context("compiler source recheck byte count overflowed")?;
            ensure!(
                total <= MAX_SOURCE_CLOSURE_BYTES,
                "compiler source recheck exceeds closure bound"
            );
            let mut file = File::open(&path)?;
            let opened = file.metadata()?;
            ensure!(
                opened.is_file()
                    && opened.len() == metadata.len()
                    && regular_mode(&opened) == regular_mode(&metadata),
                "compiler source changed while rechecked"
            );
            update_entry_header(
                &mut digest,
                b"file",
                relative,
                opened.len(),
                regular_mode(&opened),
            );
            digest_file_contents(&mut file, opened.len(), &mut digest)?;
        } else if metadata.file_type().is_symlink() {
            let target = fs::read_link(&path)?;
            let target = target
                .to_str()
                .context("staged symlink target is not UTF-8")?;
            update_entry_header(
                &mut digest,
                b"symlink",
                relative,
                u64::try_from(target.len())?,
                0,
            );
            digest.update(target.as_bytes());
        } else {
            ensure!(false, "staged source roster contains a special file");
        }
    }
    Ok(format!("{:x}", digest.finalize()))
}

fn digest_file_contents(file: &mut File, expected: u64, digest: &mut Sha256) -> Result<()> {
    let mut observed = 0_u64;
    let mut buffer = [0_u8; 64 * 1024];
    while observed < expected {
        let count = file.read(&mut buffer)?;
        ensure!(count > 0, "compiler source shrank during recheck");
        observed = observed
            .checked_add(u64::try_from(count)?)
            .context("compiler source recheck size overflowed")?;
        ensure!(observed <= expected, "compiler source grew during recheck");
        digest.update(&buffer[..count]);
    }
    let mut extra = [0_u8; 1];
    ensure!(
        file.read(&mut extra)? == 0,
        "compiler source grew during recheck"
    );
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::{
        digest_roster, ensure_tree_read_only, freeze_tree, parse_roster, stage_roster,
        verify_staged_roster,
    };
    use std::fs;
    use std::path::PathBuf;

    #[test]
    fn roster_rejects_duplicates_and_path_escapes() {
        assert!(parse_roster(b"Cargo.toml\0Cargo.toml\0").is_err());
        assert!(parse_roster(b"../Cargo.toml\0").is_err());
        assert!(parse_roster(b"target/forged.rs\0").is_err());
        assert!(parse_roster(b".git/config\0").is_err());
        assert!(parse_roster(b"unterminated").is_err());
    }

    #[test]
    fn staged_bytes_are_frozen_against_later_source_mutation() {
        let temporary = tempfile::tempdir().unwrap();
        let source = temporary.path().join("source");
        let staged = temporary.path().join("staged");
        fs::create_dir_all(source.join("crates/demo/src")).unwrap();
        fs::create_dir_all(&staged).unwrap();
        fs::write(source.join("Cargo.toml"), "workspace bytes\n").unwrap();
        let multi_chunk = vec![b'x'; 128 * 1024 + 3];
        fs::write(source.join("crates/demo/src/lib.rs"), &multi_chunk).unwrap();
        let roster = vec![
            PathBuf::from("Cargo.toml"),
            PathBuf::from("crates/demo/src/lib.rs"),
        ];
        let digest = stage_roster(&source, &staged, &roster).unwrap();
        fs::write(source.join("Cargo.toml"), "mutated workspace bytes\n").unwrap();
        fs::write(
            source.join("crates/demo/src/lib.rs"),
            "mutated source bytes\n",
        )
        .unwrap();
        verify_staged_roster(&staged, &roster, &digest).unwrap();
        assert_eq!(
            fs::read(staged.join("crates/demo/src/lib.rs")).unwrap(),
            multi_chunk
        );
    }

    #[cfg(unix)]
    #[test]
    fn post_build_verification_rejects_source_mutation_and_added_files() {
        use std::os::unix::fs::PermissionsExt;

        let temporary = tempfile::tempdir().unwrap();
        let source = temporary.path().join("source");
        let staged = temporary.path().join("staged");
        fs::create_dir_all(&source).unwrap();
        fs::create_dir_all(&staged).unwrap();
        fs::write(source.join("Cargo.toml"), "authenticated\n").unwrap();
        let roster = vec![PathBuf::from("Cargo.toml")];
        stage_roster(&source, &staged, &roster).unwrap();
        freeze_tree(&staged).unwrap();
        ensure_tree_read_only(&staged).unwrap();
        let frozen_digest = digest_roster(&staged, &roster).unwrap();

        let mut root_permissions = fs::metadata(&staged).unwrap().permissions();
        root_permissions.set_mode(0o755);
        fs::set_permissions(&staged, root_permissions).unwrap();
        let mut file_permissions = fs::metadata(staged.join("Cargo.toml"))
            .unwrap()
            .permissions();
        file_permissions.set_mode(0o644);
        fs::set_permissions(staged.join("Cargo.toml"), file_permissions).unwrap();
        fs::write(staged.join("Cargo.toml"), "build-script mutation\n").unwrap();
        fs::write(staged.join("forged.rs"), "extra\n").unwrap();

        assert!(verify_staged_roster(&staged, &roster, &frozen_digest).is_err());
        assert!(ensure_tree_read_only(&staged).is_err());
    }

    #[cfg(unix)]
    #[test]
    fn relative_internal_symlink_is_preserved_but_escape_is_rejected() {
        use std::os::unix::fs::symlink;

        let temporary = tempfile::tempdir().unwrap();
        let source = temporary.path().join("source");
        let staged = temporary.path().join("staged");
        fs::create_dir_all(source.join("docs/a")).unwrap();
        fs::create_dir_all(source.join("docs/b")).unwrap();
        fs::create_dir_all(&staged).unwrap();
        fs::write(source.join("docs/b/data"), "inside\n").unwrap();
        symlink("../b/data", source.join("docs/a/live")).unwrap();
        let roster = vec![PathBuf::from("docs/a/live"), PathBuf::from("docs/b/data")];
        let digest = stage_roster(&source, &staged, &roster).unwrap();
        verify_staged_roster(&staged, &roster, &digest).unwrap();
        assert_eq!(
            fs::read_link(staged.join("docs/a/live")).unwrap(),
            PathBuf::from("../b/data")
        );

        fs::remove_file(source.join("docs/a/live")).unwrap();
        symlink("../../../outside", source.join("docs/a/live")).unwrap();
        let rejected = temporary.path().join("rejected");
        fs::create_dir_all(&rejected).unwrap();
        assert!(stage_roster(&source, &rejected, &roster).is_err());
    }
}
