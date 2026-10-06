//! Bake a build identity that the CLI and the Python binding can compare.
//!
//! Both surfaces depend on this crate, so both observe the same value for a
//! given build. Two artifacts reporting the same `version` but different
//! identities came from different source states, which a version check alone
//! cannot detect.
//!
//! The identity is written into a generated source file rather than passed
//! through the environment: this repository permits no `RUMOCA_*` variables,
//! and a baked-in constant is what that policy directs callers to use instead.
//!
//! When the commit cannot be determined, the constant is `None` and the
//! reporting surfaces refuse rather than substituting a placeholder. A shared
//! placeholder would compare equal across unrelated builds, which is a false
//! match rather than a missing check.

use std::process::Command;

fn main() {
    println!("cargo::rerun-if-changed=build.rs");
    // The identity must track the checkout, not the last time this crate
    // happened to rebuild: a stale constant reports an identity the binary does
    // not have, which is a false match waiting to happen. Git resolves every
    // path, so linked worktrees (whose `.git` is a file) and packed refs are
    // tracked too: HEAD changes on checkout, the resolved ref (loose or
    // packed) on commit, and the index on staging.
    for path in tracked_git_paths() {
        println!("cargo::rerun-if-changed={}", path.display());
    }

    let identity =
        commit_identity().map_or_else(|| "None".to_owned(), |value| format!("Some(\"{value}\")"));
    let parents = merge_parents()
        .iter()
        .map(|parent| format!("\"{parent}\""))
        .collect::<Vec<_>>()
        .join(", ");
    let generated = format!(
        "pub(crate) const BUILD_IDENTITY: Option<&str> = {identity};\n\
         pub(crate) const BUILD_MERGE_PARENTS: &[&str] = &[{parents}];\n"
    );

    let out_dir = std::env::var("OUT_DIR").expect("cargo sets OUT_DIR for build scripts");
    let path = std::path::Path::new(&out_dir).join("build_identity.rs");
    std::fs::write(&path, generated).expect("write the generated build identity");
}

/// The short commit plus a `-dirty` marker, or `None` when git cannot say.
fn commit_identity() -> Option<String> {
    let commit = git(&["rev-parse", "--short=12", "HEAD"])?;
    let dirty = match git(&["status", "--porcelain", "--untracked-files=no"]) {
        Some(status) if !status.is_empty() => "-dirty",
        Some(_) => "",
        // Unable to tell whether the tree was modified, so do not claim it was
        // clean.
        None => return None,
    };
    Some(format!("{commit}{dirty}"))
}

fn git(args: &[&str]) -> Option<String> {
    let output = Command::new("git").args(args).output().ok()?;
    if !output.status.success() {
        return None;
    }
    Some(String::from_utf8(output.stdout).ok()?.trim().to_owned())
}

/// Every git file whose change can change the identity, resolved by git so a
/// linked worktree tracks its own HEAD and index and the shared refs.
fn tracked_git_paths() -> Vec<std::path::PathBuf> {
    let mut names = vec![
        "HEAD".to_owned(),
        "index".to_owned(),
        "packed-refs".to_owned(),
    ];
    if let Some(head_ref) = git(&["symbolic-ref", "-q", "HEAD"]) {
        names.push(head_ref);
    }
    names
        .iter()
        .filter_map(|name| git(&["rev-parse", "--path-format=absolute", "--git-path", name]))
        .map(std::path::PathBuf::from)
        .filter(|path| path.exists())
        .collect()
}

/// The parents of the built commit when it is a merge, first parent first.
///
/// A pull-request build checks out a synthetic merge of the head into the
/// base; its identity is that merge, and the parents name the head and base it
/// combines. An ordinary commit reports no parents.
fn merge_parents() -> Vec<String> {
    let Some(line) = git(&["rev-list", "--parents", "-n", "1", "HEAD"]) else {
        return Vec::new();
    };
    let parents = line.split_whitespace().skip(1).collect::<Vec<_>>();
    if parents.len() < 2 {
        return Vec::new();
    }
    parents
        .into_iter()
        .map(|parent| parent.chars().take(12).collect())
        .collect()
}
