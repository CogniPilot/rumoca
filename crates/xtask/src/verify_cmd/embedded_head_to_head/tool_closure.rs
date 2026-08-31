//! Authenticated transitive Nix closure for every benchmark-side executable.

use super::process;
use anyhow::{Context, Result, bail, ensure};
use serde::Serialize;
use sha2::{Digest, Sha256};
use std::collections::BTreeSet;
use std::ffi::OsString;
use std::path::{Path, PathBuf};

#[derive(Clone, Serialize)]
pub(super) struct ToolClosureEvidence {
    pub(super) sha256: String,
    pub(super) roots: Vec<ToolClosureRootEvidence>,
}

#[derive(Clone, Serialize)]
pub(super) struct ToolClosureRootEvidence {
    pub(super) store_root: String,
    pub(super) nar_hash: String,
}

/// Proof that all declared process/runtime inputs belong to one reviewed,
/// immutable transitive Nix-store closure.
pub(super) struct AuthenticatedToolClosure {
    evidence: ToolClosureEvidence,
    roots: Vec<PathBuf>,
    library_path: Option<OsString>,
    commands: Vec<process::CommandReceipt>,
}

impl AuthenticatedToolClosure {
    #[cfg(test)]
    pub(super) fn for_test(root: &Path) -> Self {
        Self {
            evidence: ToolClosureEvidence {
                sha256: "0".repeat(64),
                roots: vec![ToolClosureRootEvidence {
                    store_root: root.display().to_string(),
                    nar_hash: "test".into(),
                }],
            },
            roots: vec![root.canonicalize().expect("test closure root must exist")],
            library_path: None,
            commands: Vec::new(),
        }
    }

    pub(super) fn capture(
        direct_inputs: impl IntoIterator<Item = PathBuf>,
        expected_sha256: &str,
    ) -> Result<Self> {
        // Cargo injects build-script output directories into the xtask
        // process. They are not authority for measured commands: reconstruct
        // the loader path from immutable Nix-store entries and pass only that
        // closed value to the compiler-under-test.
        let ambient_library_path = std::env::var_os("LD_LIBRARY_PATH");
        let library_path = closed_nix_library_path(ambient_library_path.as_deref())?;
        let mut direct_inputs = direct_inputs.into_iter().collect::<Vec<_>>();
        if let Some(value) = &library_path {
            direct_inputs.extend(std::env::split_paths(value));
        }
        let mut commands = Vec::new();
        match capture_inner(direct_inputs, expected_sha256, &mut commands) {
            Ok((evidence, roots)) => Ok(Self {
                evidence,
                roots,
                library_path,
                commands,
            }),
            Err(error) => Err(process::attach_prior_receipts(error, commands)),
        }
    }

    pub(super) fn evidence(&self) -> &ToolClosureEvidence {
        &self.evidence
    }

    pub(super) fn commands(&self) -> &[process::CommandReceipt] {
        &self.commands
    }

    pub(super) fn require_member(&self, path: &Path) -> Result<()> {
        let canonical = path
            .canonicalize()
            .with_context(|| format!("failed to resolve tool-closure member {}", path.display()))?;
        ensure!(
            self.roots.iter().any(|root| canonical.starts_with(root)),
            "process input escaped the authenticated Nix tool closure: {}",
            canonical.display()
        );
        Ok(())
    }

    pub(super) fn compiler_runtime_environment(
        &self,
    ) -> Result<process::CompilerRuntimeEnvironment> {
        if let Some(value) = &self.library_path {
            for path in std::env::split_paths(value) {
                self.require_member(&path)?;
            }
        }
        Ok(process::CompilerRuntimeEnvironment::authenticated(
            self.library_path.clone(),
        ))
    }
}

fn closed_nix_library_path(value: Option<&std::ffi::OsStr>) -> Result<Option<OsString>> {
    let Some(value) = value else {
        return Ok(None);
    };
    let directories = std::env::split_paths(value)
        .filter(|directory| directory.starts_with("/nix/store") && directory.is_dir())
        .collect::<Vec<_>>();
    if directories.is_empty() {
        return Ok(None);
    }
    Ok(Some(
        std::env::join_paths(directories).context("failed to encode closed Nix library path")?,
    ))
}

fn capture_inner(
    direct_inputs: impl IntoIterator<Item = PathBuf>,
    expected_sha256: &str,
    commands: &mut Vec<process::CommandReceipt>,
) -> Result<(ToolClosureEvidence, Vec<PathBuf>)> {
    let nix_store = resolve_nix_store()?;
    let mut inputs = direct_inputs
        .into_iter()
        .map(|path| canonical_store_input(&path))
        .collect::<Result<BTreeSet<_>>>()?;
    inputs.insert(canonical_store_input(&nix_store)?);
    ensure!(!inputs.is_empty(), "tool closure has no direct inputs");

    let mut query = process::HermeticCommand::from_nix_store_program(&nix_store)?;
    query.args(["--query", "--requisites"]);
    query.args(&inputs);
    let (output, receipt) = process::output(&mut query, process::Limit::Probe)?;
    commands.push(receipt);
    ensure!(
        output.status.success(),
        "nix-store requisites query failed: {}",
        process::combined(&output).trim()
    );
    let roots = parse_roots(&output.stdout)?;
    for input in &inputs {
        ensure!(
            roots.iter().any(|root| input.starts_with(root)),
            "nix-store omitted direct input {} from its closure",
            input.display()
        );
    }

    let mut hash = process::HermeticCommand::from_nix_store_program(&nix_store)?;
    hash.args(["--query", "--hash"]);
    hash.args(&roots);
    let (output, receipt) = process::output(&mut hash, process::Limit::Probe)?;
    commands.push(receipt);
    ensure!(output.status.success(), "nix-store hash query failed");
    let nar_hashes = parse_hashes(&output.stdout, roots.len())?;
    let sha256 = closure_sha256(&roots, &nar_hashes);
    ensure!(
        sha256 == expected_sha256,
        "execution tool closure changed: observed {sha256}, expected {expected_sha256}"
    );
    let evidence = ToolClosureEvidence {
        sha256,
        roots: roots
            .iter()
            .zip(nar_hashes)
            .map(|(root, nar_hash)| ToolClosureRootEvidence {
                store_root: root.display().to_string(),
                nar_hash,
            })
            .collect(),
    };
    Ok((evidence, roots))
}

fn resolve_nix_store() -> Result<PathBuf> {
    let path = std::env::var_os("PATH").context("PATH is unavailable for nix-store discovery")?;
    for directory in std::env::split_paths(&path) {
        let candidate = directory.join("nix-store");
        if candidate.is_file() {
            let canonical = candidate.canonicalize()?;
            if canonical.starts_with("/nix/store") {
                // `nix-store` is a multicall entry point. Preserve its
                // invocation basename: executing the canonical `nix` target
                // with legacy nix-store arguments changes the command.
                let invocation = canonical
                    .parent()
                    .context("canonical Nix executable has no parent")?
                    .join("nix-store");
                ensure!(
                    invocation.is_file() && invocation.canonicalize()? == canonical,
                    "immutable Nix package lacks its nix-store multicall entry point"
                );
                return Ok(invocation);
            }
        }
    }
    bail!("nix-store did not resolve to the immutable Nix store")
}

fn canonical_store_input(path: &Path) -> Result<PathBuf> {
    let canonical = path
        .canonicalize()
        .with_context(|| format!("failed to resolve Nix input {}", path.display()))?;
    ensure!(
        canonical.starts_with("/nix/store"),
        "tool/runtime input is outside the Nix store: {}",
        canonical.display()
    );
    Ok(canonical)
}

fn parse_roots(bytes: &[u8]) -> Result<Vec<PathBuf>> {
    let text = std::str::from_utf8(bytes).context("nix-store returned non-UTF-8 requisites")?;
    let roots = text
        .lines()
        .filter(|line| !line.is_empty())
        .map(PathBuf::from)
        .collect::<BTreeSet<_>>()
        .into_iter()
        .collect::<Vec<_>>();
    ensure!(!roots.is_empty(), "nix-store returned an empty closure");
    for root in &roots {
        ensure!(
            is_store_root(root) && root.exists(),
            "nix-store returned an invalid closure root: {}",
            root.display()
        );
    }
    Ok(roots)
}

fn is_store_root(path: &Path) -> bool {
    let Ok(relative) = path.strip_prefix("/nix/store") else {
        return false;
    };
    let mut components = relative.components();
    matches!(components.next(), Some(std::path::Component::Normal(_)))
        && components.next().is_none()
}

fn parse_hashes(bytes: &[u8], expected: usize) -> Result<Vec<String>> {
    let text = std::str::from_utf8(bytes).context("nix-store returned non-UTF-8 hashes")?;
    let hashes = text.lines().map(str::to_owned).collect::<Vec<_>>();
    ensure!(
        hashes.len() == expected && hashes.iter().all(|hash| !hash.trim().is_empty()),
        "nix-store returned an incomplete closure hash roster"
    );
    Ok(hashes)
}

fn closure_sha256(roots: &[PathBuf], hashes: &[String]) -> String {
    let mut digest = Sha256::new();
    digest.update(b"rumoca-embedded-tool-closure-v1\0");
    for (root, hash) in roots.iter().zip(hashes) {
        let root = root.as_os_str().as_encoded_bytes();
        digest.update((root.len() as u64).to_le_bytes());
        digest.update(root);
        digest.update((hash.len() as u64).to_le_bytes());
        digest.update(hash.as_bytes());
    }
    format!("{:x}", digest.finalize())
}

#[cfg(test)]
mod tests {
    use super::{
        canonical_store_input, closed_nix_library_path, closure_sha256, parse_hashes,
        resolve_nix_store,
    };
    use std::fs;
    use std::path::PathBuf;

    #[test]
    fn closure_digest_binds_root_hash_pairing_and_cardinality() {
        let roots = vec![PathBuf::from("/nix/store/a"), PathBuf::from("/nix/store/b")];
        let hashes = vec!["sha256:a".into(), "sha256:b".into()];
        let accepted = closure_sha256(&roots, &hashes);
        assert_ne!(
            accepted,
            closure_sha256(&roots, &[hashes[1].clone(), hashes[0].clone()])
        );
        assert!(parse_hashes(b"sha256:a\n", 2).is_err());
    }

    #[test]
    fn non_nix_runtime_input_cannot_enter_the_closure() {
        let temporary = tempfile::tempdir().unwrap();
        let input = temporary.path().join("lib");
        fs::create_dir(&input).unwrap();
        assert!(canonical_store_input(&input).is_err());
        assert_eq!(
            closed_nix_library_path(Some(input.as_os_str())).unwrap(),
            None
        );
    }

    #[test]
    fn nix_store_discovery_preserves_the_multicall_invocation_name() {
        let invocation = resolve_nix_store().unwrap();
        assert_eq!(invocation.file_name().unwrap(), "nix-store");
        assert!(invocation.canonicalize().unwrap().starts_with("/nix/store"));
    }
}
