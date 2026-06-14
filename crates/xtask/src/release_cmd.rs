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
    ensure!(
        !(args.push && args.push_tag),
        "--push (phase 1: commit + push main) and --push-tag (phase 2: push the tag) \
         are separate steps; run them one at a time"
    );
    normalize_release_flags(&mut args);
    let root = crate::repo_root();
    ensure_release_worktree_state(&root, &args)?;
    let paths = release_paths(&root);

    println!("Release version: {}", args.version);
    if args.dry_run {
        println!("Dry run: no actions taken.");
        return Ok(());
    }

    // Phase 2 (--push-tag / --tag without --commit) tags an already-committed
    // bump, so it must not rewrite the version files; just confirm HEAD is the
    // release commit. Every other mode (re)writes the bump.
    if args.push_tag {
        ensure_version_committed(&root, &args.version)?;
    } else {
        let edits = build_release_edits(&paths, &args.version)?;
        write_release_edits(&paths, &edits)?;
        run_quiet_cargo_check(&root)?;
    }

    run_release_git_steps(&root, &args)?;
    Ok(())
}

fn normalize_release_flags(args: &mut ReleaseArgs) {
    if args.push {
        args.commit = true;
    }
    if args.push_tag {
        args.tag = true;
    }
}

/// Confirm the commit at HEAD already carries version `version` before tagging
/// it in phase 2, so `--push-tag` can't accidentally tag an unbumped commit.
fn ensure_version_committed(root: &Path, version: &str) -> Result<()> {
    let mut show = Command::new("git");
    show.arg("show").arg("HEAD:Cargo.toml").current_dir(root);
    let cargo = crate::run_capture(show).context("failed to read Cargo.toml at HEAD")?;
    let found = cargo.lines().find_map(|line| {
        line.trim_start()
            .strip_prefix("version = \"")
            .and_then(|rest| rest.split('"').next())
    });
    ensure!(
        found == Some(version),
        "HEAD Cargo.toml version is {found:?}, expected \"{version}\"; \
         run `cargo xtask repo release {version} --push` first to commit and push the bump"
    );
    Ok(())
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
    if !args.push && !args.push_tag {
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
        "release --push/--push-tag must run from branch 'main' (current: '{}')",
        current_branch.trim()
    );
    Ok(())
}

fn release_paths(root: &Path) -> ReleasePaths {
    ReleasePaths {
        cargo_toml: root.join("Cargo.toml"),
        pyproject: root.join("crates/rumoca-bind-python/pyproject.toml"),
        package_json: root.join("editors/vscode/package.json"),
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
    fs::write(&paths.package_json, format!("{}\n", edits.package_json))
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
        run_release_git_push_main(root)?;
    }
    if args.push_tag {
        run_release_git_push_tag(root, &args.version)?;
    }
    Ok(())
}

fn run_release_git_commit(root: &Path, version: &str) -> Result<()> {
    let mut add = Command::new("git");
    add.arg("add")
        .arg("Cargo.toml")
        .arg("Cargo.lock")
        .arg("crates/rumoca-bind-python/pyproject.toml")
        .arg("editors/vscode/package.json")
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

fn run_release_git_push_main(root: &Path) -> Result<()> {
    // Phase 1: push only the bump commit. Deploy is gated to the tag push, so
    // this main push runs CI without releasing.
    let mut push_main = Command::new("git");
    push_main
        .arg("push")
        .arg("origin")
        .arg("main")
        .current_dir(root);
    crate::run_status(push_main)
}

fn run_release_git_push_tag(root: &Path, version: &str) -> Result<()> {
    // Phase 2: push only the tag. The bump commit is already on main, so this is
    // the single ref update that triggers the release/deploy CI run.
    let mut push_tag = Command::new("git");
    push_tag
        .arg("push")
        .arg("origin")
        .arg(format!("refs/tags/v{version}"))
        .current_dir(root);
    crate::run_status(push_tag)
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
    use super::{normalize_release_flags, replace_json_version_line};
    use crate::ReleaseArgs;

    fn args() -> ReleaseArgs {
        ReleaseArgs {
            version: "1.2.3".to_string(),
            dry_run: false,
            allow_dirty: false,
            commit: false,
            tag: false,
            push: false,
            push_tag: false,
        }
    }

    #[test]
    fn push_implies_commit_only_not_tag() {
        let mut a = args();
        a.push = true;
        normalize_release_flags(&mut a);
        assert!(a.commit, "phase 1 --push must commit the bump");
        assert!(!a.tag, "phase 1 --push must not tag; the tag is phase 2");
    }

    #[test]
    fn push_tag_implies_tag_only_not_commit() {
        let mut a = args();
        a.push_tag = true;
        normalize_release_flags(&mut a);
        assert!(a.tag, "phase 2 --push-tag must create the tag");
        assert!(
            !a.commit,
            "phase 2 --push-tag must not re-commit; the bump is already on main"
        );
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
}
