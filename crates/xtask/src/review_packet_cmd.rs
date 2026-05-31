use anyhow::{Context, Result, bail};
use clap::Args;
use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

#[derive(Debug, Args, Clone)]
pub(crate) struct ReviewPacketArgs {
    /// Base git ref for the diff packet
    #[arg(long, default_value = "origin/main")]
    base: String,
    /// Head git ref for the diff packet
    #[arg(long, default_value = "HEAD")]
    head: String,
    /// Roadmap phase or milestone identifier
    #[arg(long)]
    phase: String,
    /// Markdown output path
    #[arg(long)]
    out: PathBuf,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ChangedFile {
    status: String,
    path: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Numstat {
    added: String,
    deleted: String,
    path: String,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct CrateDependency {
    crate_name: String,
    kind: String,
    dependency: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct CrateDependencyDiff {
    change: &'static str,
    dependency: CrateDependency,
}

pub(crate) fn run(args: ReviewPacketArgs, repo_root: &Path) -> Result<()> {
    let changed_files = changed_files(repo_root, &args.base, &args.head)?;
    let numstat = numstat(repo_root, &args.base, &args.head)?;
    let base_crate_deps = workspace_crate_dependencies_at_ref(repo_root, &args.base)?;
    let head_crate_deps = workspace_crate_dependencies_at_ref(repo_root, &args.head)?;
    let crate_dep_diff = crate_dependency_diff(&base_crate_deps, &head_crate_deps);
    let markdown = render_review_packet(
        &args.phase,
        &args.base,
        &args.head,
        &changed_files,
        &numstat,
        &head_crate_deps,
        &crate_dep_diff,
    );
    let out = if args.out.is_absolute() {
        args.out
    } else {
        repo_root.join(args.out)
    };
    if let Some(parent) = out.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("failed to create {}", parent.display()))?;
    }
    fs::write(&out, markdown).with_context(|| format!("failed to write {}", out.display()))?;
    println!("review packet wrote {}", out.display());
    Ok(())
}

fn changed_files(repo_root: &Path, base: &str, head: &str) -> Result<Vec<ChangedFile>> {
    let output = git_output(
        repo_root,
        ["diff", "--name-status", "--diff-filter=ACMR"],
        base,
        head,
    )?;
    Ok(output
        .lines()
        .filter_map(parse_changed_file)
        .collect::<Vec<_>>())
}

fn numstat(repo_root: &Path, base: &str, head: &str) -> Result<Vec<Numstat>> {
    let output = git_output(
        repo_root,
        ["diff", "--numstat", "--diff-filter=ACMR"],
        base,
        head,
    )?;
    Ok(output.lines().filter_map(parse_numstat).collect())
}

fn git_output<const N: usize>(
    repo_root: &Path,
    args: [&str; N],
    base: &str,
    head: &str,
) -> Result<String> {
    let output = Command::new("git")
        .args(args)
        .arg(format!("{base}...{head}"))
        .current_dir(repo_root)
        .output()
        .with_context(|| "failed to run git for review packet")?;
    if !output.status.success() {
        bail!(
            "git diff failed for {base}...{head}: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    }
    Ok(String::from_utf8_lossy(&output.stdout).into_owned())
}

fn parse_changed_file(line: &str) -> Option<ChangedFile> {
    let (status, path) = line.split_once('\t')?;
    Some(ChangedFile {
        status: status.to_string(),
        path: path.to_string(),
    })
}

fn parse_numstat(line: &str) -> Option<Numstat> {
    let mut fields = line.split('\t');
    Some(Numstat {
        added: fields.next()?.to_string(),
        deleted: fields.next()?.to_string(),
        path: fields.next()?.to_string(),
    })
}

fn render_review_packet(
    phase: &str,
    base: &str,
    head: &str,
    changed_files: &[ChangedFile],
    numstat: &[Numstat],
    crate_deps: &[CrateDependency],
    crate_dep_diff: &[CrateDependencyDiff],
) -> String {
    let mut out = String::new();
    out.push_str("# Automated Review Packet\n\n");
    out.push_str(&format!("- Phase: `{phase}`\n"));
    out.push_str(&format!("- Base: `{base}`\n"));
    out.push_str(&format!("- Head: `{head}`\n"));
    out.push_str(&format!("- Changed files: `{}`\n\n", changed_files.len()));
    push_changed_files_by_crate(&mut out, changed_files);
    push_numstat(&mut out, numstat);
    push_crate_dag_diff(&mut out, crate_dep_diff);
    push_crate_dag_snapshot(&mut out, crate_deps);
    out.push_str("## Required Companion Checks\n\n");
    out.push_str("- `cargo xtask repo review-scan --base ");
    out.push_str(base);
    out.push_str(" --head ");
    out.push_str(head);
    out.push_str(" --out target/review/findings.json`\n");
    out.push_str("- Relevant Cargo tests and clippy commands from the roadmap phase.\n");
    out
}

fn workspace_crate_dependencies_at_ref(
    repo_root: &Path,
    git_ref: &str,
) -> Result<Vec<CrateDependency>> {
    let output = Command::new("git")
        .args(["ls-tree", "-r", "--name-only", git_ref, "--", "crates"])
        .current_dir(repo_root)
        .output()
        .with_context(|| format!("failed to list crate manifests at {git_ref}"))?;
    if !output.status.success() {
        bail!(
            "git ls-tree failed for {git_ref}: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    }

    let mut deps = Vec::new();
    for manifest_path in String::from_utf8_lossy(&output.stdout)
        .lines()
        .filter(|path| path.ends_with("/Cargo.toml"))
    {
        let Some(crate_name) = crate_name_from_manifest_path(manifest_path) else {
            continue;
        };
        let content = git_show_file(repo_root, git_ref, manifest_path)?;
        deps.extend(cargo_dependencies_from_toml(&crate_name, &content)?);
    }
    deps.sort_by(|lhs, rhs| {
        lhs.crate_name
            .cmp(&rhs.crate_name)
            .then_with(|| lhs.kind.cmp(&rhs.kind))
            .then_with(|| lhs.dependency.cmp(&rhs.dependency))
    });
    Ok(deps)
}

fn git_show_file(repo_root: &Path, git_ref: &str, path: &str) -> Result<String> {
    let output = Command::new("git")
        .arg("show")
        .arg(format!("{git_ref}:{path}"))
        .current_dir(repo_root)
        .output()
        .with_context(|| format!("failed to read {path} at {git_ref}"))?;
    if !output.status.success() {
        bail!(
            "git show failed for {git_ref}:{path}: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    }
    Ok(String::from_utf8_lossy(&output.stdout).into_owned())
}

fn crate_name_from_manifest_path(path: &str) -> Option<String> {
    let mut parts = path.split('/');
    match (parts.next(), parts.next(), parts.next()) {
        (Some("crates"), Some(crate_name), Some("Cargo.toml")) => Some(crate_name.to_string()),
        _ => None,
    }
}

fn crate_dependency_diff(
    base_deps: &[CrateDependency],
    head_deps: &[CrateDependency],
) -> Vec<CrateDependencyDiff> {
    let base: BTreeSet<_> = base_deps.iter().cloned().collect();
    let head: BTreeSet<_> = head_deps.iter().cloned().collect();
    let mut diff = Vec::new();
    diff.extend(
        head.difference(&base)
            .cloned()
            .map(|dependency| CrateDependencyDiff {
                change: "added",
                dependency,
            }),
    );
    diff.extend(
        base.difference(&head)
            .cloned()
            .map(|dependency| CrateDependencyDiff {
                change: "removed",
                dependency,
            }),
    );
    diff.sort_by(|lhs, rhs| {
        lhs.change
            .cmp(rhs.change)
            .then_with(|| lhs.dependency.cmp(&rhs.dependency))
    });
    diff
}

fn cargo_dependencies_from_toml(crate_name: &str, content: &str) -> Result<Vec<CrateDependency>> {
    let value: toml::Value = toml::from_str(content)
        .with_context(|| format!("failed to parse Cargo.toml for {crate_name}"))?;
    let mut deps = Vec::new();
    for section in ["dependencies", "dev-dependencies", "build-dependencies"] {
        let Some(table) = value.get(section).and_then(toml::Value::as_table) else {
            continue;
        };
        for dependency in table.keys().filter(|key| key.starts_with("rumoca-")) {
            deps.push(CrateDependency {
                crate_name: crate_name.to_string(),
                kind: section.to_string(),
                dependency: dependency.to_string(),
            });
        }
    }
    Ok(deps)
}

fn push_crate_dag_diff(out: &mut String, crate_dep_diff: &[CrateDependencyDiff]) {
    out.push_str("## Crate DAG Diff\n\n");
    out.push_str("| change | crate | kind | dependency |\n");
    out.push_str("| --- | --- | --- | --- |\n");
    if crate_dep_diff.is_empty() {
        out.push_str("| _none_ | _none_ | _none_ | _none_ |\n\n");
        return;
    }
    for diff in crate_dep_diff {
        out.push_str(&format!(
            "| `{}` | `{}` | `{}` | `{}` |\n",
            diff.change,
            diff.dependency.crate_name,
            diff.dependency.kind,
            diff.dependency.dependency
        ));
    }
    out.push('\n');
}

fn push_changed_files_by_crate(out: &mut String, changed_files: &[ChangedFile]) {
    out.push_str("## Changed Files By Crate\n\n");
    let mut by_crate: BTreeMap<String, Vec<&ChangedFile>> = BTreeMap::new();
    for file in changed_files {
        by_crate
            .entry(crate_bucket(&file.path))
            .or_default()
            .push(file);
    }
    if by_crate.is_empty() {
        out.push_str("- _none_\n\n");
        return;
    }
    for (crate_name, files) in by_crate {
        out.push_str(&format!("### {crate_name}\n\n"));
        for file in files {
            out.push_str(&format!("- `{}` `{}`\n", file.status, file.path));
        }
        out.push('\n');
    }
}

fn push_numstat(out: &mut String, numstat: &[Numstat]) {
    out.push_str("## Line Delta\n\n");
    out.push_str("| added | deleted | path |\n");
    out.push_str("| ---: | ---: | --- |\n");
    if numstat.is_empty() {
        out.push_str("| 0 | 0 | _none_ |\n\n");
        return;
    }
    for row in numstat {
        out.push_str(&format!(
            "| {} | {} | `{}` |\n",
            row.added, row.deleted, row.path
        ));
    }
    out.push('\n');
}

fn push_crate_dag_snapshot(out: &mut String, crate_deps: &[CrateDependency]) {
    out.push_str("## Crate DAG Snapshot\n\n");
    out.push_str("| crate | kind | dependency |\n");
    out.push_str("| --- | --- | --- |\n");
    if crate_deps.is_empty() {
        out.push_str("| _none_ | _none_ | _none_ |\n\n");
        return;
    }
    for dep in crate_deps {
        out.push_str(&format!(
            "| `{}` | `{}` | `{}` |\n",
            dep.crate_name, dep.kind, dep.dependency
        ));
    }
    out.push('\n');
}

fn crate_bucket(path: &str) -> String {
    let mut parts = path.split('/');
    match (parts.next(), parts.next()) {
        (Some("crates"), Some(crate_name)) => crate_name.to_string(),
        _ => "repo-root".to_string(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn render_review_packet_groups_changed_files_by_crate() {
        let packet = render_review_packet(
            "phase-0",
            "origin/main",
            "HEAD",
            &[
                ChangedFile {
                    status: "M".to_string(),
                    path: "crates/rumoca-phase-solve/src/lib.rs".to_string(),
                },
                ChangedFile {
                    status: "A".to_string(),
                    path: "dev/review.md".to_string(),
                },
            ],
            &[Numstat {
                added: "3".to_string(),
                deleted: "1".to_string(),
                path: "crates/rumoca-phase-solve/src/lib.rs".to_string(),
            }],
            &[CrateDependency {
                crate_name: "rumoca-phase-solve".to_string(),
                kind: "dependencies".to_string(),
                dependency: "rumoca-ir-solve".to_string(),
            }],
            &[CrateDependencyDiff {
                change: "added",
                dependency: CrateDependency {
                    crate_name: "rumoca-phase-solve".to_string(),
                    kind: "dependencies".to_string(),
                    dependency: "rumoca-eval-solve".to_string(),
                },
            }],
        );

        assert!(packet.contains("### rumoca-phase-solve"));
        assert!(packet.contains("### repo-root"));
        assert!(packet.contains("## Crate DAG Diff"));
        assert!(
            packet
                .contains("`added` | `rumoca-phase-solve` | `dependencies` | `rumoca-eval-solve`")
        );
        assert!(packet.contains("## Crate DAG Snapshot"));
        assert!(packet.contains("`rumoca-phase-solve` | `dependencies` | `rumoca-ir-solve`"));
        assert!(packet.contains("cargo xtask repo review-scan --base origin/main --head HEAD"));
    }

    #[test]
    fn cargo_dependencies_from_toml_collects_rumoca_edges() {
        let deps = cargo_dependencies_from_toml(
            "rumoca-example",
            r#"
[dependencies]
anyhow = "1"
rumoca-core = { workspace = true }

[dev-dependencies]
rumoca-ir-dae = { workspace = true }
"#,
        )
        .expect("parse dependencies");

        assert_eq!(
            deps,
            vec![
                CrateDependency {
                    crate_name: "rumoca-example".to_string(),
                    kind: "dependencies".to_string(),
                    dependency: "rumoca-core".to_string(),
                },
                CrateDependency {
                    crate_name: "rumoca-example".to_string(),
                    kind: "dev-dependencies".to_string(),
                    dependency: "rumoca-ir-dae".to_string(),
                },
            ]
        );
    }

    #[test]
    fn crate_dependency_diff_reports_added_and_removed_edges() {
        let base = vec![CrateDependency {
            crate_name: "rumoca-phase-solve".to_string(),
            kind: "dependencies".to_string(),
            dependency: "rumoca-ir-dae".to_string(),
        }];
        let head = vec![CrateDependency {
            crate_name: "rumoca-phase-solve".to_string(),
            kind: "dependencies".to_string(),
            dependency: "rumoca-ir-solve".to_string(),
        }];

        let diff = crate_dependency_diff(&base, &head);

        assert_eq!(
            diff,
            vec![
                CrateDependencyDiff {
                    change: "added",
                    dependency: CrateDependency {
                        crate_name: "rumoca-phase-solve".to_string(),
                        kind: "dependencies".to_string(),
                        dependency: "rumoca-ir-solve".to_string(),
                    },
                },
                CrateDependencyDiff {
                    change: "removed",
                    dependency: CrateDependency {
                        crate_name: "rumoca-phase-solve".to_string(),
                        kind: "dependencies".to_string(),
                        dependency: "rumoca-ir-dae".to_string(),
                    },
                },
            ]
        );
    }
}
