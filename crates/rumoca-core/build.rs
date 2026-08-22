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

    let identity = commit_identity()
        .map_or_else(|| "None".to_owned(), |value| format!("Some(\"{value}\")"));
    let generated = format!("pub(crate) const BUILD_IDENTITY: Option<&str> = {identity};\n");

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
