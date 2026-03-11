use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use anyhow::{Context, Result, ensure};

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
    let mut tag = Command::new("git");
    tag.arg("tag").arg(format!("v{version}")).current_dir(root);
    crate::run_status(tag)
}

fn run_release_git_push(root: &Path, version: &str) -> Result<()> {
    // Push branch and release tag together so main-push CI can detect the v* tag on this SHA.
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
    let parts = version.split('.').collect::<Vec<_>>();
    ensure!(
        parts.len() == 3 && parts.iter().all(|part| part.parse::<u64>().is_ok()),
        "invalid version format: {version} (expected X.Y.Z)"
    );
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::replace_json_version_line;

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
