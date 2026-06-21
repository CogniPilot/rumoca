use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use anyhow::{Context, Result, ensure};
use rumoca_compile::compile::core as rumoca_core;

use crate::ReleaseArgs;

#[derive(Debug, Clone)]
struct ReleasePaths {
    cargo_toml: PathBuf,
    pyproject: PathBuf,
    package_json: PathBuf,
}

#[derive(Debug, Clone)]
struct ReleaseEdits {
    cargo_toml: String,
    pyproject: String,
    package_json: String,
}

pub(crate) fn cmd_release(mut args: ReleaseArgs) -> Result<()> {
    validate_semver(&args.version)?;
    normalize_release_flags(&mut args);
    let root = crate::repo_root();
    ensure_release_worktree_state(&root, &args)?;
    let paths = release_paths(&root);
    let edits = build_release_edits(&paths, &args.version)?;

    println!("Release version: {}", args.version);
    if args.dry_run {
        println!("Dry run: no files written.");
    } else {
        write_release_edits(&paths, &edits)?;
        run_quiet_cargo_check(&root)?;
        run_release_git_steps(&root, &args)?;
    }

    Ok(())
}

fn normalize_release_flags(args: &mut ReleaseArgs) {
    if args.push {
        args.commit = true;
        args.tag = true;
    }
}

fn ensure_release_worktree_state(root: &Path, args: &ReleaseArgs) -> Result<()> {
    if args.allow_dirty {
        ensure_release_push_branch(root, args)?;
        return Ok(());
    }
    let mut status = Command::new("git");
    status.arg("status").arg("--porcelain").current_dir(root);
    let output = crate::run_capture(status)?;
    ensure!(
        output.trim().is_empty(),
        "working tree is not clean; pass --allow-dirty to override"
    );
    ensure_release_push_branch(root, args)?;
    Ok(())
}

fn ensure_release_push_branch(root: &Path, args: &ReleaseArgs) -> Result<()> {
    if !args.push {
        return Ok(());
    }
    let mut branch = Command::new("git");
    branch
        .arg("rev-parse")
        .arg("--abbrev-ref")
        .arg("HEAD")
        .current_dir(root);
    let current_branch = crate::run_capture(branch)?;
    ensure!(
        current_branch.trim() == "main",
        "release --push must run from branch 'main' (current: '{}')",
        current_branch.trim()
    );
    Ok(())
}

fn release_paths(root: &Path) -> ReleasePaths {
    ReleasePaths {
        cargo_toml: root.join("Cargo.toml"),
        pyproject: root.join("crates/rumoca-bind-python/pyproject.toml"),
        package_json: root.join("packages/vscode/package.json"),
    }
}

fn build_release_edits(paths: &ReleasePaths, version: &str) -> Result<ReleaseEdits> {
    let cargo_text = read_file_string(&paths.cargo_toml)?;
    let pyproject_text = read_file_string(&paths.pyproject)?;
    let package_text = read_file_string(&paths.package_json)?;

    let cargo_toml = replace_first_line_by_prefix(
        &cargo_text,
        "version = \"",
        &format!("version = \"{version}\""),
    )
    .context("failed to update Cargo.toml version")?;
    let pyproject = replace_first_line_by_prefix(
        &pyproject_text,
        "version = \"",
        &format!("version = \"{version}\""),
    )
    .context("failed to update pyproject.toml version")?;
    let package_json = replace_json_version_line(&package_text, version)
        .context("failed to update package.json version")?;
    serde_json::from_str::<serde_json::Value>(&package_json)
        .context("invalid VSCode package.json after version update")?;

    Ok(ReleaseEdits {
        cargo_toml,
        pyproject,
        package_json,
    })
}

fn write_release_edits(paths: &ReleasePaths, edits: &ReleaseEdits) -> Result<()> {
    fs::write(&paths.cargo_toml, &edits.cargo_toml)
        .with_context(|| format!("failed to write {}", paths.cargo_toml.display()))?;
    fs::write(&paths.pyproject, &edits.pyproject)
        .with_context(|| format!("failed to write {}", paths.pyproject.display()))?;
    // `edits.package_json` already ends in a trailing newline; writing it
    // verbatim keeps the release idempotent (an extra "\n" here appended a
    // blank line to the file on every release run).
    fs::write(&paths.package_json, &edits.package_json)
        .with_context(|| format!("failed to write {}", paths.package_json.display()))?;
    Ok(())
}

fn run_quiet_cargo_check(root: &Path) -> Result<()> {
    let mut cargo_check = Command::new("cargo");
    cargo_check.arg("check").arg("--quiet").current_dir(root);
    crate::run_status(cargo_check)
}

fn run_release_git_steps(root: &Path, args: &ReleaseArgs) -> Result<()> {
    if args.commit {
        run_release_git_commit(root, &args.version)?;
    }
    if args.tag {
        run_release_git_tag(root, &args.version)?;
    }
    if args.push {
        run_release_git_push(root, &args.version)?;
    }
    Ok(())
}

fn run_release_git_commit(root: &Path, version: &str) -> Result<()> {
    let mut add = Command::new("git");
    add.arg("add")
        .arg("Cargo.toml")
        .arg("Cargo.lock")
        .arg("crates/rumoca-bind-python/pyproject.toml")
        .arg("packages/vscode/package.json")
        .current_dir(root);
    crate::run_status(add)?;

    let mut commit = Command::new("git");
    commit
        .arg("commit")
        .arg("-s")
        .arg("-m")
        .arg(format!("Release v{version}"))
        .current_dir(root);
    crate::run_status(commit)
}

fn run_release_git_tag(root: &Path, version: &str) -> Result<()> {
    let tag = format!("v{version}");
    // Tolerate a re-run (e.g. phase 2 retried) as long as the existing tag still
    // points at the commit we are about to release.
    let mut list = Command::new("git");
    list.arg("tag").arg("--list").arg(&tag).current_dir(root);
    if !crate::run_capture(list)?.trim().is_empty() {
        let mut points = Command::new("git");
        points
            .arg("tag")
            .arg("--points-at")
            .arg("HEAD")
            .current_dir(root);
        let at_head = crate::run_capture(points)?;
        ensure!(
            at_head.lines().any(|line| line.trim() == tag),
            "tag {tag} already exists but does not point at HEAD; \
             delete it or choose a new version"
        );
        println!("Tag {tag} already exists at HEAD; reusing it.");
        return Ok(());
    }
    let mut create = Command::new("git");
    create.arg("tag").arg(&tag).current_dir(root);
    crate::run_status(create)
}

fn run_release_git_push(root: &Path, version: &str) -> Result<()> {
    // Push the bump commit and the release tag together. Heavy build jobs and
    // deploy are gated to the tag ref (and PRs), so the main-ref run only lints
    // and tests while the tag-ref run builds and deploys exactly once.
    let mut push_main_and_tag = Command::new("git");
    push_main_and_tag
        .arg("push")
        .arg("origin")
        .arg("main")
        .arg(format!("refs/tags/v{version}"))
        .current_dir(root);
    crate::run_status(push_main_and_tag)
}

fn read_file_string(path: &Path) -> Result<String> {
    fs::read_to_string(path).with_context(|| format!("failed to read {}", path.display()))
}

fn replace_first_line_by_prefix(text: &str, prefix: &str, replacement: &str) -> Option<String> {
    let mut replaced = false;
    let mut out = String::with_capacity(text.len().saturating_add(32));
    for line in text.lines() {
        if !replaced && line.trim_start().starts_with(prefix) {
            out.push_str(replacement);
            out.push('\n');
            replaced = true;
        } else {
            out.push_str(line);
            out.push('\n');
        }
    }
    if replaced { Some(out) } else { None }
}

fn replace_json_version_line(text: &str, version: &str) -> Option<String> {
    let mut replaced = false;
    let mut out = String::with_capacity(text.len().saturating_add(32));
    for line in text.lines() {
        let trimmed = line.trim_start();
        if !replaced && trimmed.starts_with("\"version\"") {
            let indent = &line[..line.len().saturating_sub(trimmed.len())];
            let trailing_comma = if trimmed.ends_with(',') { "," } else { "" };
            out.push_str(indent);
            out.push_str(&format!("\"version\": \"{version}\"{trailing_comma}"));
            out.push('\n');
            replaced = true;
        } else {
            out.push_str(line);
            out.push('\n');
        }
    }
    if replaced { Some(out) } else { None }
}

fn validate_semver(version: &str) -> Result<()> {
    let parts = rumoca_core::split_path_with_indices(version);
    ensure!(
        parts.len() == 3 && parts.iter().all(|part| part.parse::<u64>().is_ok()),
        "invalid version format: {version} (expected X.Y.Z)"
    );
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::{
        ReleaseEdits, ReleasePaths, normalize_release_flags, replace_json_version_line,
        write_release_edits,
    };
    use crate::ReleaseArgs;

    fn args() -> ReleaseArgs {
        ReleaseArgs {
            version: "1.2.3".to_string(),
            dry_run: false,
            allow_dirty: false,
            commit: false,
            tag: false,
            push: false,
        }
    }

    #[test]
    fn push_implies_commit_and_tag() {
        let mut a = args();
        a.push = true;
        normalize_release_flags(&mut a);
        assert!(a.commit, "--push must commit the bump");
        assert!(a.tag, "--push must create the tag (pushed alongside main)");
    }

    #[test]
    fn replace_json_version_line_preserves_order_and_updates_version() {
        let source = r#"{
  "name": "rumoca-modelica",
  "version": "0.7.28",
  "publisher": "JamesGoppert"
}
"#;
        let updated =
            replace_json_version_line(source, "0.8.0").expect("version line should be replaced");
        let expected = r#"{
  "name": "rumoca-modelica",
  "version": "0.8.0",
  "publisher": "JamesGoppert"
}
"#;
        assert_eq!(updated, expected);
    }

    #[test]
    fn write_release_edits_is_idempotent_for_package_json() {
        let dir = tempfile::tempdir().expect("tempdir");
        let paths = ReleasePaths {
            cargo_toml: dir.path().join("Cargo.toml"),
            pyproject: dir.path().join("pyproject.toml"),
            package_json: dir.path().join("package.json"),
        };
        // `replace_json_version_line` output ends in a single trailing newline.
        let edits = ReleaseEdits {
            cargo_toml: "version = \"1.2.3\"\n".to_string(),
            pyproject: "version = \"1.2.3\"\n".to_string(),
            package_json: "{\n  \"version\": \"1.2.3\"\n}\n".to_string(),
        };

        write_release_edits(&paths, &edits).expect("first write");
        let first = std::fs::read_to_string(&paths.package_json).expect("read first");
        write_release_edits(&paths, &edits).expect("second write");
        let second = std::fs::read_to_string(&paths.package_json).expect("read second");

        assert_eq!(first, second, "writing the same edits twice must be stable");
        assert!(
            first.ends_with("}\n") && !first.ends_with("}\n\n"),
            "package.json must keep exactly one trailing newline, got: {first:?}"
        );
    }
}
