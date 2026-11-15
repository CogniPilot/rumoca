//! `cargo xtask ci check-matrix`: the compiled-only profile's compile, build,
//! feature-graph and test stages, run locally in order with each stage's exact
//! command, exit status, duration and output recorded.
//!
//! The stages are the compile-first recipe of the compiled-only profile: the
//! published and certification guests (`wasm32-wasip2`, the packager's fixed
//! flags), the evaluator with and without `reference-eval`, the production
//! feature trees, and the named refusal and host tests. A stage passes only on
//! exit 0 and its own expectation: a tree must name every required node and
//! none forbidden, a test stage must run exactly its named tests with nothing
//! failed or ignored. Stages of one target class share a warm target dir.

use anyhow::{Context, Result, bail, ensure};
use clap::Args;
use serde_json::{Value, json};
use sha2::{Digest, Sha256};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::Instant;

/// The packager's fixed parallelism for the guest builds; the host budget may
/// lower it, never raise it.
const MAX_JOBS: usize = 4;

const GUEST_MANIFEST: &str = "crates/rumoca-runtime-fmi-ls/Cargo.toml";
const PUBLISHED: &str = "scalar-linked-product";
const CERTIFY: &str = "scalar-linked-product,certify-linked";

#[derive(Debug, Args, PartialEq, Eq)]
pub(crate) struct CheckMatrixArgs {
    /// Run only these stages (repeatable); every stage when omitted
    #[arg(long = "stage")]
    pub(crate) stages: Vec<String>,
    /// List the stages and their commands without running them
    #[arg(long)]
    pub(crate) list: bool,
    /// Continue after a failed stage instead of stopping at it
    #[arg(long)]
    pub(crate) keep_going: bool,
    /// Resolve dependencies offline, as the packager does
    #[arg(long)]
    pub(crate) offline: bool,
    /// Directory that receives each stage's output and `summary.json`
    #[arg(long, default_value = "target/check-matrix")]
    pub(crate) out: PathBuf,
    /// Root of the warm per-class target dirs
    #[arg(long, default_value = "target/check-matrix-targets")]
    pub(crate) target_root: PathBuf,
}

/// Which warm target dir a stage builds in: stages of one class compile the
/// same graph with the same flags, so they share one.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum TargetClass {
    GuestCheck,
    GuestPublished,
    GuestCertify,
    Host,
}

impl TargetClass {
    fn dir(self) -> &'static str {
        match self {
            Self::GuestCheck => "guest-check",
            Self::GuestPublished => "guest-published",
            Self::GuestCertify => "guest-certify",
            Self::Host => "host",
        }
    }
}

/// What a stage must show beyond exit 0.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Expect {
    Exit,
    /// The JSON messages name the guest's `.wasm` artifact.
    Artifact,
    /// A non-empty feature tree naming every `require` and no `forbid`.
    Tree {
        require: Vec<&'static str>,
        forbid: Vec<&'static str>,
    },
    /// Exactly these tests ran and passed, nothing failed or was ignored.
    Tests(Vec<&'static str>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Stage {
    pub(crate) name: &'static str,
    pub(crate) args: Vec<String>,
    /// `CARGO_ENCODED_RUSTFLAGS` for this stage.
    pub(crate) rustflags: String,
    pub(crate) class: TargetClass,
    pub(crate) expect: Expect,
}

fn words(text: &str) -> Vec<String> {
    text.split_whitespace().map(str::to_string).collect()
}

/// The packager's guest rustflags (fixed_guest.rs), joined as Cargo encodes them.
fn guest_build_rustflags(rust_lld: &Path) -> String {
    [
        "-Dwarnings".to_string(),
        "-Crelocation-model=pic".to_string(),
        "-Cpanic=abort".to_string(),
        format!("-Clinker={}", rust_lld.display()),
        "-Clink-arg=--pie".to_string(),
        "-Clink-arg=--experimental-pic".to_string(),
        "-Clink-arg=--import-memory=env,memory".to_string(),
        "-Clink-arg=--import-table".to_string(),
        "-Clink-arg=--fatal-warnings".to_string(),
        "-Clink-arg=--threads=4".to_string(),
    ]
    .join("\u{1f}")
}

/// Every stage of the recipe, in run order.
pub(crate) fn stages(rust_lld: &Path, jobs: usize, offline: bool) -> Vec<Stage> {
    let lock = if offline {
        "--offline --locked"
    } else {
        "--locked"
    };
    let guest = format!(
        "{lock} --release -j{jobs} -Zbuild-std=std,panic_abort --target wasm32-wasip2 \
         --manifest-path {GUEST_MANIFEST}"
    );
    let check_flags = "-Dwarnings\u{1f}-Cpanic=abort".to_string();
    let host_flags = "-Dwarnings".to_string();
    let build_flags = guest_build_rustflags(rust_lld);
    let guest_check = |name, features: &str| Stage {
        name,
        args: words(&format!("check {guest} --features {features} --lib")),
        rustflags: check_flags.clone(),
        class: TargetClass::GuestCheck,
        expect: Expect::Exit,
    };
    let guest_build = |name, features: &str, class| Stage {
        name,
        args: words(&format!(
            "build {guest} --message-format=json-render-diagnostics --features {features}"
        )),
        rustflags: build_flags.clone(),
        class,
        expect: Expect::Artifact,
    };
    let host = |name, command: String, expect| Stage {
        name,
        args: words(&command),
        rustflags: host_flags.clone(),
        class: TargetClass::Host,
        expect,
    };
    let guest_forbid = vec![
        "feature \"reference-eval\"",
        "feature \"reference-execution\"",
        "rumoca-phase-solve",
        "rumoca-eval-galec",
    ];
    let guest_require = vec![
        "rumoca-eval-solve",
        "rumoca-solver",
        "rumoca-solver-rk45",
        "feature \"linked-guest\"",
    ];
    let tree = |name, features: &str| {
        host(
            name,
            format!(
                "tree {lock} -e normal,build,features --target wasm32-wasip2 \
                 -p rumoca-runtime-fmi-ls --features {features}"
            ),
            Expect::Tree {
                require: guest_require.clone(),
                forbid: guest_forbid.clone(),
            },
        )
    };
    let mut stages = vec![
        guest_check("CHECK-P", PUBLISHED),
        guest_check("CHECK-C", CERTIFY),
        host(
            "CHECK-E0",
            format!("check {lock} -j{jobs} -p rumoca-eval-solve --lib --tests"),
            Expect::Exit,
        ),
        host(
            "CHECK-E1",
            format!(
                "check {lock} -j{jobs} -p rumoca-eval-solve --lib --tests --features reference-eval"
            ),
            Expect::Exit,
        ),
        guest_build("BUILD-P", PUBLISHED, TargetClass::GuestPublished),
        guest_build("BUILD-C", CERTIFY, TargetClass::GuestCertify),
        tree("TREE-P", PUBLISHED),
        tree("TREE-C", CERTIFY),
        host(
            "TREE-HOST",
            format!("tree {lock} -e normal,build,features -p rumoca-sim --features native-solvers"),
            Expect::Tree {
                require: vec![
                    "feature \"reference-eval\"",
                    "feature \"reference-execution\"",
                    "rumoca-phase-solve",
                ],
                forbid: Vec::new(),
            },
        ),
    ];
    stages.extend(named_test_stages(lock, jobs));
    stages
}

/// The named refusal and lowering tests: each must run exactly its listed
/// tests with nothing failed or ignored.
fn named_test_stages(lock: &str, jobs: usize) -> Vec<Stage> {
    let test = |name, command: &str, names| Stage {
        name,
        args: words(&format!("test {lock} -j{jobs} {command}")),
        rustflags: "-Dwarnings".to_string(),
        class: TargetClass::Host,
        expect: Expect::Tests(names),
    };
    vec![
        test(
            "R1",
            "-p rumoca-eval-solve --lib -- compiled_only_tests",
            vec![
                "prepared::compiled_only_tests::scalar_row_refusal_precedes_operand_and_output_validation",
                "prepared::compiled_only_tests::replacement_retains_validation_and_refreshes_row_requirements",
                "prepared::compiled_only_tests::shared_compute_reuse_retains_scalar_nodes_and_new_requirements",
                "prepared::compiled_only_tests::compiled_lazy_eligibility_is_checked_and_refreshes_on_replacement",
                "typed_program::compiled_only_tests::typed_entries_refuse_before_owner_lookup_with_a_backend_error",
                "typed_program::compiled_only_tests::excluded_lifecycle_methods_leave_caller_state_unchanged_before_binding",
            ],
        ),
        test(
            "R2",
            "-p rumoca-eval-solve --lib -- --exact update_rows::tests::exact_assignment_applies_change_below_solver_tolerance",
            vec!["update_rows::tests::exact_assignment_applies_change_below_solver_tolerance"],
        ),
        test(
            "R3",
            "-p rumoca-solver --lib -- backend_requirement_tests",
            vec!["only_a_build_without_reference_execution_requires_a_compiled_backend"],
        ),
        test(
            "R7",
            "-p rumoca-sim --lib -- lowering_profile_tests",
            vec![
                "the_linked_guest_never_reaches_lowering",
                "phase_solve_is_an_optional_dependency",
                "every_model_consuming_feature_reaches_lowering",
                "the_guest_check_sees_a_transitive_lowering_edge",
            ],
        ),
    ]
}

/// The sysroot's `rust-lld` for the host triple of the `rustc` Cargo will use.
fn rust_lld() -> Result<PathBuf> {
    let rustc = std::env::var("RUSTC").unwrap_or_else(|_| "rustc".to_string());
    let run = |args: &[&str]| -> Result<String> {
        let output = Command::new(&rustc)
            .args(args)
            .output()
            .with_context(|| format!("run `{rustc} {}`", args.join(" ")))?;
        ensure!(
            output.status.success(),
            "`{rustc} {}` failed",
            args.join(" ")
        );
        Ok(String::from_utf8_lossy(&output.stdout).trim().to_string())
    };
    let sysroot = run(&["--print", "sysroot"])?;
    let version = run(&["-vV"])?;
    let host = version
        .lines()
        .find_map(|line| line.strip_prefix("host: "))
        .context("rustc -vV names no host")?;
    Ok(Path::new(&sysroot)
        .join("lib/rustlib")
        .join(host)
        .join("bin/rust-lld"))
}

/// Every needle of a tree expectation: a crate is matched as `name v`, so
/// `rumoca-solver` does not match `rumoca-solver-rk45 v`; a feature literally.
fn tree_mentions(tree: &str, needle: &str) -> usize {
    let pattern = if needle.starts_with("feature ") {
        needle.to_string()
    } else {
        format!("{needle} v")
    };
    tree.lines().filter(|line| line.contains(&pattern)).count()
}

pub(crate) fn check_tree(tree: &str, require: &[&str], forbid: &[&str]) -> Result<String> {
    ensure!(!tree.trim().is_empty(), "the feature tree is empty");
    let mut report = Vec::new();
    let mut problems = Vec::new();
    for needle in require {
        let count = tree_mentions(tree, needle);
        report.push(format!("require {needle}: {count}"));
        if count == 0 {
            problems.push(format!("missing {needle}"));
        }
    }
    for needle in forbid {
        let count = tree_mentions(tree, needle);
        report.push(format!("forbid {needle}: {count}"));
        if count > 0 {
            problems.push(format!("forbidden {needle} occurs {count} times"));
        }
    }
    ensure!(problems.is_empty(), "{}", problems.join("; "));
    Ok(report.join(", "))
}

/// The tests a libtest run reported as `ok`, and its one summary line.
pub(crate) fn check_tests(stdout: &str, names: &[&str]) -> Result<String> {
    let passed: Vec<&str> = stdout
        .lines()
        .filter_map(|line| line.strip_prefix("test ")?.strip_suffix(" ... ok"))
        .collect();
    let summaries: Vec<&str> = stdout
        .lines()
        .filter(|line| line.starts_with("test result: "))
        .collect();
    let [summary] = summaries.as_slice() else {
        bail!("expected one test summary, found {}", summaries.len());
    };
    let expected = format!(
        "test result: ok. {} passed; 0 failed; 0 ignored;",
        names.len()
    );
    ensure!(
        summary.starts_with(&expected),
        "summary `{summary}` is not `{expected} ...`"
    );
    let missing: Vec<&&str> = names
        .iter()
        .filter(|name| {
            !passed
                .iter()
                .any(|run| run == *name || run.ends_with(&format!("::{name}")))
        })
        .collect();
    ensure!(missing.is_empty(), "named tests did not pass: {missing:?}");
    Ok(format!("{} named tests ok", names.len()))
}

/// The guest artifact the build's JSON messages name, with its SHA-256.
pub(crate) fn check_artifact(messages: &str) -> Result<String> {
    let wasm = messages
        .lines()
        .filter_map(|line| serde_json::from_str::<Value>(line).ok())
        .filter(|message| {
            message["reason"] == "compiler-artifact"
                && message["target"]["name"] == "rumoca_runtime_fmi_ls"
        })
        .flat_map(|message| message["filenames"].as_array().cloned().unwrap_or_default())
        .filter_map(|name| name.as_str().map(PathBuf::from))
        .find(|path| path.extension().is_some_and(|ext| ext == "wasm"))
        .context("the build messages name no rumoca_runtime_fmi_ls .wasm artifact")?;
    let bytes = std::fs::read(&wasm).with_context(|| format!("read {}", wasm.display()))?;
    let digest = Sha256::digest(&bytes);
    let hex: String = digest.iter().map(|byte| format!("{byte:02x}")).collect();
    Ok(format!(
        "{} sha256 {hex} ({} bytes)",
        wasm.display(),
        bytes.len()
    ))
}

fn verdict(stage: &Stage, stdout: &str) -> Result<String> {
    match &stage.expect {
        Expect::Exit => Ok("exit 0".to_string()),
        Expect::Artifact => check_artifact(stdout),
        Expect::Tree { require, forbid } => check_tree(stdout, require, forbid),
        Expect::Tests(names) => check_tests(stdout, names),
    }
}

pub(crate) fn run(args: &CheckMatrixArgs) -> Result<()> {
    let root = crate::repo_root();
    let jobs = crate::resource_budget::worker_jobs(None, MAX_JOBS)?;
    let all = stages(&rust_lld()?, jobs, args.offline);
    for name in &args.stages {
        ensure!(
            all.iter().any(|stage| stage.name == name),
            "unknown stage `{name}`; known: {}",
            all.iter()
                .map(|stage| stage.name)
                .collect::<Vec<_>>()
                .join(" ")
        );
    }
    let selected: Vec<&Stage> = all
        .iter()
        .filter(|stage| args.stages.is_empty() || args.stages.iter().any(|name| name == stage.name))
        .collect();
    if args.list {
        for stage in &selected {
            println!("{:<10} cargo {}", stage.name, stage.args.join(" "));
        }
        return Ok(());
    }
    let out = root.join(&args.out);
    std::fs::create_dir_all(&out).with_context(|| format!("create {}", out.display()))?;
    let cargo = std::env::var("CARGO").unwrap_or_else(|_| "cargo".to_string());
    let mut results = Vec::new();
    let mut failed = Vec::new();
    for stage in selected {
        let target = root.join(&args.target_root).join(stage.class.dir());
        let started = Instant::now();
        let output = Command::new(&cargo)
            .args(&stage.args)
            .current_dir(&root)
            .env_remove("RUSTFLAGS")
            .env_remove("RUSTC_WRAPPER")
            .env_remove("RUSTC_WORKSPACE_WRAPPER")
            .env("CARGO_ENCODED_RUSTFLAGS", &stage.rustflags)
            .env("CARGO_TARGET_DIR", &target)
            .env("CARGO_BUILD_JOBS", jobs.to_string())
            .env("RUST_TEST_THREADS", jobs.to_string())
            .env("RAYON_NUM_THREADS", jobs.to_string())
            .env("CARGO_INCREMENTAL", "0")
            .env("CARGO_PROFILE_RELEASE_DEBUG", "0")
            .output()
            .with_context(|| format!("run stage {}", stage.name))?;
        let seconds = started.elapsed().as_secs_f64();
        let stdout = String::from_utf8_lossy(&output.stdout);
        std::fs::write(out.join(format!("{}.stdout", stage.name)), &output.stdout)?;
        std::fs::write(out.join(format!("{}.stderr", stage.name)), &output.stderr)?;
        let code = output.status.code();
        let outcome = if output.status.success() {
            verdict(stage, &stdout)
        } else {
            Err(anyhow::anyhow!("exit {code:?}"))
        };
        let (pass, detail) = match outcome {
            Ok(detail) => (true, detail),
            Err(error) => (false, format!("{error:#}")),
        };
        println!(
            "{:<10} {} {:>7.1}s  {detail}",
            stage.name,
            if pass { "PASS" } else { "FAIL" },
            seconds
        );
        results.push(json!({
            "stage": stage.name,
            "command": format!("cargo {}", stage.args.join(" ")),
            "rustflags": stage.rustflags.replace('\u{1f}', " "),
            "target_dir": target,
            "exit": code,
            "seconds": seconds,
            "pass": pass,
            "detail": detail,
        }));
        if !pass {
            failed.push(stage.name);
            if !args.keep_going {
                break;
            }
        }
    }
    std::fs::write(
        out.join("summary.json"),
        serde_json::to_string_pretty(&json!({ "jobs": jobs, "stages": results }))?,
    )?;
    ensure!(failed.is_empty(), "failed stages: {}", failed.join(" "));
    Ok(())
}

#[cfg(test)]
#[path = "check_matrix_tests.rs"]
mod tests;
