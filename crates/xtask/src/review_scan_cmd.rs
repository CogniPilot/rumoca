use anyhow::{Context, Result, bail};
use clap::Args;
use serde::Serialize;
use std::collections::BTreeMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use walkdir::WalkDir;

#[derive(Debug, Args, Clone)]
pub(crate) struct ReviewScanArgs {
    /// Base git ref for the diff scan
    #[arg(long, default_value = "origin/main")]
    base: String,
    /// Head git ref for the diff scan
    #[arg(long, default_value = "HEAD")]
    head: String,
    /// Write JSON findings to this path instead of stdout
    #[arg(long)]
    out: Option<PathBuf>,
    /// Write a repo-wide audit summary for expect/HashMap/HashSet/f64::EPSILON usage
    #[arg(long)]
    audit_out: Option<PathBuf>,
    /// Exit non-zero if any high-severity finding is produced
    #[arg(long)]
    fail_on_high: bool,
    /// Exit non-zero on hard architectural boundary violations
    #[arg(long)]
    fail_on_forbidden: bool,
}

#[derive(Debug, Serialize, PartialEq, Eq)]
struct ReviewFinding {
    severity: &'static str,
    rule: &'static str,
    path: String,
    line: usize,
    excerpt: String,
}

#[derive(Debug, Default, Serialize, PartialEq, Eq)]
struct AuditMetrics {
    expect_calls: usize,
    hashmap_mentions: usize,
    hashset_mentions: usize,
    f64_epsilon_mentions: usize,
}

#[derive(Debug, Default, Serialize, PartialEq, Eq)]
struct ReviewAudit {
    totals: AuditMetrics,
    crates: BTreeMap<String, AuditMetrics>,
}

struct ReviewRule {
    id: &'static str,
    severity: &'static str,
    needles: fn() -> Vec<String>,
}

const REVIEW_RULES: &[ReviewRule] = &[
    ReviewRule {
        id: "silent-default",
        severity: "high",
        needles: silent_default_needles,
    },
    ReviewRule {
        id: "source-provenance",
        severity: "high",
        needles: source_provenance_needles,
    },
    ReviewRule {
        id: "panic-discipline",
        severity: "medium",
        needles: panic_discipline_needles,
    },
    ReviewRule {
        id: "unsafe-added",
        severity: "high",
        needles: unsafe_needles,
    },
    ReviewRule {
        id: "expect-audit",
        severity: "medium",
        needles: expect_needles,
    },
    ReviewRule {
        id: "unordered-collection-audit",
        severity: "low",
        needles: unordered_collection_needles,
    },
    ReviewRule {
        id: "float-epsilon-audit",
        severity: "low",
        needles: float_epsilon_needles,
    },
];

fn silent_default_needles() -> Vec<String> {
    ["unwrap_or(0.0)", "unwrap_or_default()", "Value::UNDEFINED"]
        .into_iter()
        .map(str::to_string)
        .collect()
}

fn source_provenance_needles() -> Vec<String> {
    ["SourceId(0)", "Span::DUMMY"]
        .into_iter()
        .map(str::to_string)
        .collect()
}

fn panic_discipline_needles() -> Vec<String> {
    [
        concat!("panic", "!("),
        concat!("todo", "!("),
        concat!("unimplemented", "!("),
    ]
    .into_iter()
    .map(str::to_string)
    .collect()
}

fn unsafe_needles() -> Vec<String> {
    [concat!("unsafe", " {")]
        .into_iter()
        .map(str::to_string)
        .collect()
}

fn expect_needles() -> Vec<String> {
    [".expect("].into_iter().map(str::to_string).collect()
}

fn unordered_collection_needles() -> Vec<String> {
    [
        "HashMap<",
        "HashSet<",
        "std::collections::HashMap",
        "std::collections::HashSet",
    ]
    .into_iter()
    .map(str::to_string)
    .collect()
}

fn float_epsilon_needles() -> Vec<String> {
    ["f64::EPSILON"].into_iter().map(str::to_string).collect()
}

pub(crate) fn run(args: ReviewScanArgs, repo_root: &Path) -> Result<()> {
    let files = changed_rust_files(repo_root, &args.base, &args.head)?;
    let findings = scan_files(repo_root, &files)?;
    if let Some(out) = args.audit_out.as_ref() {
        write_audit(repo_root, out)?;
    }
    let json = serde_json::to_string_pretty(&findings)?;
    if let Some(out) = args.out {
        let path = if out.is_absolute() {
            out
        } else {
            repo_root.join(out)
        };
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent)
                .with_context(|| format!("failed to create {}", parent.display()))?;
        }
        fs::write(&path, format!("{json}\n"))
            .with_context(|| format!("failed to write {}", path.display()))?;
        println!(
            "review scan wrote {} finding(s) to {}",
            findings.len(),
            path.display()
        );
    } else {
        println!("{json}");
    }
    if args.fail_on_high {
        let high_count = high_severity_count(&findings);
        if high_count > 0 {
            bail!("review scan found {high_count} high-severity finding(s)");
        }
    }
    if args.fail_on_forbidden {
        let forbidden_count = forbidden_finding_count(&findings);
        if forbidden_count > 0 {
            bail!("review scan found {forbidden_count} forbidden architecture finding(s)");
        }
    }
    Ok(())
}

fn write_audit(repo_root: &Path, out: &Path) -> Result<()> {
    let audit = build_repo_audit(repo_root)?;
    let path = if out.is_absolute() {
        out.to_path_buf()
    } else {
        repo_root.join(out)
    };
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("failed to create {}", parent.display()))?;
    }
    fs::write(
        &path,
        format!("{}\n", serde_json::to_string_pretty(&audit)?),
    )
    .with_context(|| format!("failed to write {}", path.display()))?;
    println!("review audit wrote {}", path.display());
    Ok(())
}

fn build_repo_audit(repo_root: &Path) -> Result<ReviewAudit> {
    let mut audit = ReviewAudit::default();
    for entry in WalkDir::new(repo_root)
        .into_iter()
        .filter_map(|entry| entry.ok())
    {
        let path = entry.path();
        if !path.is_file() || path.extension().and_then(|ext| ext.to_str()) != Some("rs") {
            continue;
        }
        let rel = path
            .strip_prefix(repo_root)
            .unwrap_or(path)
            .to_string_lossy()
            .replace('\\', "/");
        if !is_line_count_checked_rust_source(&rel) {
            continue;
        }
        let content = fs::read_to_string(path)
            .with_context(|| format!("failed to read {}", path.display()))?;
        let metrics = audit_metrics_for_content(&content);
        add_audit_metrics(&mut audit.totals, &metrics);
        add_audit_metrics(
            audit.crates.entry(crate_key_for_path(&rel)).or_default(),
            &metrics,
        );
    }
    Ok(audit)
}

fn audit_metrics_for_content(content: &str) -> AuditMetrics {
    let mut metrics = AuditMetrics::default();
    for line in content.lines() {
        let trimmed = line.trim_start();
        if trimmed.starts_with("//") {
            continue;
        }
        metrics.expect_calls += line.matches(".expect(").count();
        metrics.hashmap_mentions += line.matches("HashMap").count();
        metrics.hashset_mentions += line.matches("HashSet").count();
        metrics.f64_epsilon_mentions += line.matches("f64::EPSILON").count();
    }
    metrics
}

fn add_audit_metrics(total: &mut AuditMetrics, next: &AuditMetrics) {
    total.expect_calls += next.expect_calls;
    total.hashmap_mentions += next.hashmap_mentions;
    total.hashset_mentions += next.hashset_mentions;
    total.f64_epsilon_mentions += next.f64_epsilon_mentions;
}

fn crate_key_for_path(path: &str) -> String {
    let mut parts = path.split('/');
    if parts.next() == Some("crates")
        && let Some(crate_name) = parts.next()
    {
        return crate_name.to_string();
    }
    "workspace".to_string()
}

fn high_severity_count(findings: &[ReviewFinding]) -> usize {
    findings
        .iter()
        .filter(|finding| finding.severity == "high")
        .count()
}

fn forbidden_finding_count(findings: &[ReviewFinding]) -> usize {
    findings
        .iter()
        .filter(|finding| is_forbidden_finding(finding))
        .count()
}

fn is_forbidden_finding(finding: &ReviewFinding) -> bool {
    match finding.rule {
        "boundary-target-encoder"
        | "boundary-eval-dae-backend"
        | "boundary-ir-behavior"
        | "file-size-hard-limit" => true,
        "unsafe-added" => !is_allowed_unsafe_boundary_path(&finding.path),
        _ => false,
    }
}

fn is_allowed_unsafe_boundary_path(path: &str) -> bool {
    path.starts_with("crates/rumoca-exec-cranelift/")
        || path.starts_with("crates/rumoca-exec-mlir/")
        || path.starts_with("crates/rumoca-exec-wasm/")
        || path == "crates/rumoca-sim/src/scheduled_sim/executor.rs"
}

fn changed_rust_files(repo_root: &Path, base: &str, head: &str) -> Result<Vec<PathBuf>> {
    let output = Command::new("git")
        .args(["diff", "--name-only", "--diff-filter=ACMR"])
        .arg(format!("{base}...{head}"))
        .current_dir(repo_root)
        .output()
        .with_context(|| "failed to run git diff for review scan")?;
    if !output.status.success() {
        bail!(
            "git diff failed for {base}...{head}: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    }
    Ok(String::from_utf8_lossy(&output.stdout)
        .lines()
        .filter(|line| line.ends_with(".rs") || line.ends_with("Cargo.toml"))
        .map(PathBuf::from)
        .collect())
}

fn scan_files(repo_root: &Path, files: &[PathBuf]) -> Result<Vec<ReviewFinding>> {
    let mut findings = Vec::new();
    for rel_path in files {
        let path = repo_root.join(rel_path);
        let content = fs::read_to_string(&path)
            .with_context(|| format!("failed to read {}", path.display()))?;
        findings.extend(scan_content(&rel_path.to_string_lossy(), &content));
        findings.extend(scan_file_size(&rel_path.to_string_lossy(), &content));
    }
    Ok(findings)
}

fn scan_content(path: &str, content: &str) -> Vec<ReviewFinding> {
    let mut findings = Vec::new();
    for (line_idx, line) in content.lines().enumerate() {
        let trimmed = line.trim_start();
        if trimmed.starts_with("//") {
            continue;
        }
        for rule in REVIEW_RULES {
            let needles = (rule.needles)();
            if needles.iter().any(|needle| line.contains(needle)) {
                findings.push(ReviewFinding {
                    severity: rule.severity,
                    rule: rule.id,
                    path: path.to_string(),
                    line: line_idx + 1,
                    excerpt: line.trim().to_string(),
                });
            }
        }
        if line_trips_target_encoder_boundary(path, line) {
            findings.push(ReviewFinding {
                severity: "high",
                rule: "boundary-target-encoder",
                path: path.to_string(),
                line: line_idx + 1,
                excerpt: line.trim().to_string(),
            });
        }
        if line_trips_eval_dae_backend_boundary(path, line) {
            findings.push(ReviewFinding {
                severity: "high",
                rule: "boundary-eval-dae-backend",
                path: path.to_string(),
                line: line_idx + 1,
                excerpt: line.trim().to_string(),
            });
        }
        if line_trips_facade_export_boundary(path, line) {
            findings.push(ReviewFinding {
                severity: "medium",
                rule: "boundary-facade-export",
                path: path.to_string(),
                line: line_idx + 1,
                excerpt: line.trim().to_string(),
            });
        }
        if line_trips_ir_behavior_boundary(path, line) {
            findings.push(ReviewFinding {
                severity: "high",
                rule: "boundary-ir-behavior",
                path: path.to_string(),
                line: line_idx + 1,
                excerpt: line.trim().to_string(),
            });
        }
    }
    findings
}

fn scan_file_size(path: &str, content: &str) -> Vec<ReviewFinding> {
    if !path.ends_with(".rs") || !is_line_count_checked_rust_source(path) {
        return Vec::new();
    }
    let line_count = content.lines().count();
    let (severity, rule) = if line_count > 2000 {
        if has_spec_0021_file_size_exception(content) {
            ("low", "file-size-exception-audit")
        } else {
            ("high", "file-size-hard-limit")
        }
    } else if line_count >= 1800 {
        ("low", "file-size-near-limit")
    } else {
        return Vec::new();
    };
    vec![ReviewFinding {
        severity,
        rule,
        path: path.to_string(),
        line: 1,
        excerpt: if rule == "file-size-exception-audit" {
            format!("{line_count} lines with explicit SPEC_0021 file-size exception and split plan")
        } else {
            format!("{line_count} lines")
        },
    }]
}

fn has_spec_0021_file_size_exception(content: &str) -> bool {
    content.contains("SPEC_0021") && content.contains("file-size") && content.contains("split plan")
}

fn is_line_count_checked_rust_source(path: &str) -> bool {
    !path.contains("/generated/")
}

fn line_trips_target_encoder_boundary(path: &str, line: &str) -> bool {
    path.starts_with("crates/rumoca-phase-")
        && !path.starts_with("crates/rumoca-phase-codegen/")
        && (line.contains("wasm_encoder")
            || line.contains("wasm-encoder")
            || line.contains("cranelift")
            || line.contains("inkwell"))
}

fn line_trips_eval_dae_backend_boundary(path: &str, line: &str) -> bool {
    let is_backend_or_runtime = path.starts_with("crates/rumoca-exec-")
        || path.starts_with("crates/rumoca-solver")
        || path.starts_with("crates/rumoca-sim/")
        || path.starts_with("crates/rumoca-phase-codegen/");
    is_backend_or_runtime && line.contains("rumoca-eval-dae")
}

fn line_trips_facade_export_boundary(path: &str, line: &str) -> bool {
    let trimmed = line.trim_start();
    let is_cross_crate_export =
        trimmed.starts_with("pub use rumoca_") || cross_crate_pub_type_alias(trimmed);
    is_cross_crate_export && !is_allowed_facade_path(path)
}

fn cross_crate_pub_type_alias(trimmed: &str) -> bool {
    if !trimmed.starts_with("pub type ") {
        return false;
    }
    trimmed
        .split_once('=')
        .is_some_and(|(_, rhs)| rhs.trim_start().starts_with("rumoca_"))
}

fn is_allowed_facade_path(path: &str) -> bool {
    matches!(
        path,
        "crates/rumoca-compile/src/lib.rs"
            | "crates/rumoca-sim/src/lib.rs"
            | "crates/rumoca-codec/src/lib.rs"
    )
}

fn line_trips_ir_behavior_boundary(path: &str, line: &str) -> bool {
    if !path.starts_with("crates/rumoca-ir-") {
        return false;
    }
    let trimmed = line.trim_start();
    trimmed.starts_with("use rumoca_phase_")
        || trimmed.starts_with("use rumoca_eval_")
        || trimmed.starts_with("pub fn eval")
        || trimmed.starts_with("pub fn lower")
        || trimmed.starts_with("fn eval")
        || trimmed.starts_with("fn lower")
}

#[cfg(test)]
mod tests {
    use super::*;

    fn over_limit_rust_source(header: &str) -> String {
        format!("{header}\n{}", "fn fixture() {}\n".repeat(2001))
    }

    #[test]
    fn scan_content_reports_review_gate_needles() {
        let findings = scan_content(
            "crates/example/src/lib.rs",
            r#"
fn f(value: Option<f64>) -> f64 {
    value.unwrap_or(0.0)
}
fn g() {
    panic!("bad");
}
"#,
        );

        assert_eq!(findings.len(), 2);
        assert_eq!(findings[0].rule, "silent-default");
        assert_eq!(findings[0].line, 3);
        assert_eq!(findings[1].rule, "panic-discipline");
    }

    #[test]
    fn scan_content_reports_boundary_needles() {
        let phase_findings = scan_content(
            "crates/rumoca-phase-solve/Cargo.toml",
            r#"
[dependencies]
wasm-encoder = "0.0"
"#,
        );
        assert!(
            phase_findings
                .iter()
                .any(|finding| finding.rule == "boundary-target-encoder")
        );

        let backend_findings = scan_content(
            "crates/rumoca-exec-cranelift/Cargo.toml",
            r#"
[dependencies]
rumoca-eval-dae = { workspace = true }
"#,
        );
        assert!(
            backend_findings
                .iter()
                .any(|finding| finding.rule == "boundary-eval-dae-backend")
        );

        let export_findings = scan_content(
            "crates/rumoca-phase-solve/src/lib.rs",
            "pub use rumoca_phase_dae::lower;",
        );
        assert!(
            export_findings
                .iter()
                .any(|finding| finding.rule == "boundary-facade-export")
        );

        let ir_findings = scan_content(
            "crates/rumoca-ir-dae/src/lib.rs",
            "pub fn lower_expression() {}",
        );
        assert!(
            ir_findings
                .iter()
                .any(|finding| finding.rule == "boundary-ir-behavior")
        );
    }

    #[test]
    fn scan_file_size_reports_near_and_hard_limits() {
        let near = scan_file_size("crates/example/src/lib.rs", &"x\n".repeat(1800));
        assert_eq!(near[0].rule, "file-size-near-limit");
        assert_eq!(near[0].severity, "low");

        let hard = scan_file_size("crates/example/src/lib.rs", &"x\n".repeat(2001));
        assert_eq!(hard[0].rule, "file-size-hard-limit");
        assert_eq!(hard[0].severity, "high");

        let test_file = scan_file_size("crates/example/src/tests.rs", &"x\n".repeat(2001));
        assert_eq!(test_file[0].rule, "file-size-hard-limit");

        let generated_file = scan_file_size(
            "crates/example/src/generated/parser.rs",
            &"x\n".repeat(2001),
        );
        assert!(generated_file.is_empty());
    }

    #[test]
    fn scan_file_size_accepts_complete_spec_0021_exception_as_non_forbidden_audit() {
        let content = over_limit_rust_source(
            "// SPEC_0021 file-size exception: cohesive fixture. split plan: move fixtures by owner.",
        );

        let findings = scan_file_size("crates/example/src/lib.rs", &content);

        assert!(
            findings
                .iter()
                .all(|finding| finding.rule != "file-size-hard-limit"),
            "a complete SPEC_0021 exception must not produce a hard-limit finding: {findings:#?}"
        );
        assert_eq!(forbidden_finding_count(&findings), 0);
        assert!(
            findings
                .iter()
                .any(|finding| finding.rule == "file-size-exception-audit"),
            "the explicit exception should remain visible to reviewers"
        );
    }

    #[test]
    fn scan_file_size_requires_every_spec_0021_exception_marker() {
        for header in [
            "// file-size exception: cohesive fixture. split plan: move fixtures by owner.",
            "// SPEC_0021 exception: cohesive fixture. split plan: move fixtures by owner.",
            "// SPEC_0021 file-size exception: cohesive fixture.",
        ] {
            let findings =
                scan_file_size("crates/example/src/lib.rs", &over_limit_rust_source(header));

            assert!(
                findings
                    .iter()
                    .any(|finding| finding.rule == "file-size-hard-limit"),
                "incomplete exception marker set must remain a hard failure: {header}"
            );
            assert_eq!(forbidden_finding_count(&findings), 1);
        }
    }

    #[test]
    fn scan_file_size_keeps_new_unexcepted_threshold_crossing_forbidden() {
        let findings = scan_file_size(
            "crates/example/src/new_module.rs",
            &over_limit_rust_source("// New module without an exception"),
        );

        assert!(
            findings
                .iter()
                .any(|finding| finding.rule == "file-size-hard-limit")
        );
        assert_eq!(forbidden_finding_count(&findings), 1);
    }

    #[test]
    fn scan_content_reports_float_epsilon_audit_needles() {
        let findings = scan_content(
            "crates/example/src/lib.rs",
            r#"
fn nearly_zero(value: f64) -> bool {
    value.abs() <= f64::EPSILON
}
"#,
        );

        assert_eq!(findings.len(), 1);
        assert_eq!(findings[0].rule, "float-epsilon-audit");
        assert_eq!(findings[0].severity, "low");
    }

    #[test]
    fn high_severity_count_is_distinguishable_from_low_findings() {
        let findings = vec![
            ReviewFinding {
                severity: "low",
                rule: "file-size-near-limit",
                path: "crates/example/src/lib.rs".to_string(),
                line: 1,
                excerpt: "1800 lines".to_string(),
            },
            ReviewFinding {
                severity: "high",
                rule: "boundary-ir-behavior",
                path: "crates/rumoca-ir-dae/src/lib.rs".to_string(),
                line: 2,
                excerpt: "pub fn lower_expression() {}".to_string(),
            },
        ];

        assert_eq!(high_severity_count(&findings), 1);
        assert_eq!(forbidden_finding_count(&findings), 1);
    }

    #[test]
    fn unsafe_in_execution_boundary_is_audit_not_forbidden() {
        let findings = vec![
            ReviewFinding {
                severity: "high",
                rule: "unsafe-added",
                path: "crates/rumoca-exec-mlir/src/compile.rs".to_string(),
                line: 1,
                excerpt: concat!("let lib = unsafe", " { load() };").to_string(),
            },
            ReviewFinding {
                severity: "high",
                rule: "unsafe-added",
                path: "crates/rumoca-exec-cranelift/src/emit.rs".to_string(),
                line: 2,
                excerpt: concat!("unsafe", " { jit() }").to_string(),
            },
        ];

        assert_eq!(high_severity_count(&findings), 2);
        assert_eq!(forbidden_finding_count(&findings), 0);
    }

    #[test]
    fn unsafe_outside_execution_boundary_is_forbidden() {
        let findings = vec![ReviewFinding {
            severity: "high",
            rule: "unsafe-added",
            path: "crates/rumoca-phase-dae/src/lib.rs".to_string(),
            line: 1,
            excerpt: concat!("unsafe", " { unreachable_unchecked() }").to_string(),
        }];

        assert_eq!(forbidden_finding_count(&findings), 1);
    }

    #[test]
    fn audit_metrics_count_review_risk_needles() {
        let metrics = audit_metrics_for_content(
            r#"
use std::collections::{HashMap, HashSet};

fn demo(value: Option<f64>) {
    let _ = value.expect("present");
    let _ = f64::EPSILON;
}
"#,
        );

        assert_eq!(metrics.expect_calls, 1);
        assert_eq!(metrics.hashmap_mentions, 1);
        assert_eq!(metrics.hashset_mentions, 1);
        assert_eq!(metrics.f64_epsilon_mentions, 1);
        assert_eq!(
            crate_key_for_path("crates/rumoca-phase-dae/src/lib.rs"),
            "rumoca-phase-dae"
        );
    }
}
