//! Capture a build identity that the CLI and the Python binding can compare.
//!
//! Both surfaces depend on this crate, so both observe the same value for a
//! given build. Two artifacts reporting the same `version` but different
//! identities came from different source states, which a version check alone
//! cannot detect.
//!
//! When the commit cannot be determined, no identity is emitted and the
//! reporting surfaces refuse rather than substituting a placeholder. A shared
//! placeholder would compare equal across unrelated builds, which is worse than
//! reporting nothing.

use std::process::Command;

fn main() {
    println!("cargo::rerun-if-changed=build.rs");
    println!("cargo::rerun-if-env-changed=RUMOCA_BUILD_ID");

    if let Ok(id) = std::env::var("RUMOCA_BUILD_ID") {
        let id = id.trim();
        if !id.is_empty() {
            println!("cargo::rustc-env=RUMOCA_BUILD_IDENTITY={id}");
            return;
        }
    }

    let Some(commit) = git(&["rev-parse", "--short=12", "HEAD"]) else {
        return;
    };
    let dirty = match git(&["status", "--porcelain", "--untracked-files=no"]) {
        Some(status) if !status.is_empty() => "-dirty",
        Some(_) => "",
        // Unable to tell whether the tree was modified, so do not claim it was
        // clean.
        None => return,
    };
    println!("cargo::rustc-env=RUMOCA_BUILD_IDENTITY={commit}{dirty}");
}

fn git(args: &[&str]) -> Option<String> {
    let output = Command::new("git").args(args).output().ok()?;
    if !output.status.success() {
        return None;
    }
    Some(String::from_utf8(output.stdout).ok()?.trim().to_owned())
}
