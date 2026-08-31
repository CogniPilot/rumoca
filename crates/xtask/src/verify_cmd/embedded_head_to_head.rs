//! Generated-source Cortex-M7 instruction ratchet against external compilers.

// One token list owns both the authenticated suite identity and its executor
// dispatch. Changing an operation method therefore changes the row digest.
macro_rules! embedded_suite_operations {
    ($consumer:ident $(, $argument:tt)*) => {
        $consumer! {
            $($argument),*;
            StageInputs => "stage-inputs" => stage_inputs,
            WriteOracle => "write-oracle" => write_oracle,
            GenerateRumoca => "generate-rumoca" => generate_rumoca,
            GenerateCasadi => "generate-casadi" => generate_casadi,
            CounterStraight => "counter-straight" => counter_straight,
            CounterCalledLeaf => "counter-called-leaf" => counter_called_leaf,
            CounterExitStatus => "counter-exit-status" => validate_exit_status,
            MutationDeletedCall => "mutation-deleted-call" => mutation_deleted_call,
            MutationConstantOutput => "mutation-constant-output" => mutation_constant_output,
            MutationClosedBranches => "mutation-closed-branches" => mutation_closed_branches,
            MutationCasadiRowMajor => "mutation-casadi-row-major" => mutation_casadi_row_major,
            CorrectnessRumoca => "correctness-rumoca" => correctness_rumoca,
            CorrectnessCasadi => "correctness-casadi" => correctness_casadi,
            BuildRumocaMeasured => "build-rumoca-measured" => build_rumoca_measured,
            BuildCasadiMeasured => "build-casadi-measured" => build_casadi_measured,
            MeasureRumoca => "measure-rumoca" => measure_rumoca,
            MeasureCasadi => "measure-casadi" => measure_casadi,
        }
    };
}

mod artifact_bundle;
mod artifact_guard;
mod compiler_deps;
mod compiler_source;
mod cross;
mod efmu_artifact;
mod emit;
mod manifest;
mod process;
mod qemu;
mod snapshot;
mod suite;
mod suite_identity;
#[cfg(test)]
mod tests;
mod tool_closure;
mod typed_path;

use anyhow::{Context, Result, bail, ensure};
use clap::Args;
use serde::Serialize;
use serde::ser::SerializeSeq;
use sha2::{Digest, Sha256};
use std::fs;
#[cfg(test)]
use std::io::Read;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::time::Instant;

use cross::BuildContext;
use manifest::{CorrectnessCase, Entry, Manifest, MeasuredToolPins};
use suite::{CompletedSuite, PartialSuiteEvidence, SuiteFailure, measure_row};
use typed_path::{
    CasadiRowMajorDriver, OracleInclude, RolePath, RumocaCacheDirectory, RumocaClosedBranchDriver,
    RumocaClosedBranchModel, RumocaConstantOutputDriver, RumocaDeletedCallDriver,
};

const ARTIFACT_DIR: &str = "target/verification/embedded-head-to-head";
const SUMMARY_PATH: &str = "target/verification/embedded-head-to-head-summary.json";
const SUMMARY_TEMP_PATH: &str = "target/verification/embedded-head-to-head-summary.json.tmp";
const LOCK_PATH: &str = "target/verification/embedded-head-to-head.lock";
#[derive(Debug, Args, Clone, PartialEq, Eq)]
pub(crate) struct VerifyEmbeddedHeadToHeadArgs {
    /// Root whose bin/ contains arm-none-eabi-gcc and arm-none-eabi-nm.
    #[arg(long, value_name = "PATH")]
    arm_toolchain: PathBuf,
    /// Exact qemu-system-arm executable used for dynamic measurement.
    #[arg(long, value_name = "PATH")]
    qemu: PathBuf,
    /// Python executable carrying the comparator version named by the manifest.
    #[arg(long, value_name = "PATH")]
    python: PathBuf,
    /// Exact util-linux prlimit executable that bounds QEMU trace growth.
    #[arg(long, value_name = "PATH")]
    prlimit: PathBuf,
    /// Reviewed predecessor revision. Xtask authenticates the commit and loads
    /// the manifest from its exact ancestor tree; genuine absence is the
    /// bootstrap case. A closed run cannot be constructed without this proof.
    #[arg(long, value_name = "COMMIT")]
    baseline_git_revision: String,
    /// Measure only the named manifest row (repeatable).
    #[arg(long = "only", value_name = "ID")]
    only: Vec<String>,
}

#[derive(Clone, Copy, Debug, Serialize)]
#[serde(rename_all = "kebab-case")]
enum SetupPhase {
    AuthenticateManifest,
    RejectPendingRows,
    AuthenticateGit,
    AuthenticateBaseline,
    AuthenticateMeasuredTools,
    AuthenticateCompilerToolchain,
    AuthenticateToolClosure,
    CaptureCompilerSource,
    CaptureCompilerDependencies,
    BuildCompiler,
    BindRows,
    PublishFinalReport,
}

struct SetupLedger {
    phase: SetupPhase,
    commands: Vec<process::CommandReceipt>,
    final_report_published: bool,
}

impl SetupLedger {
    fn new() -> Self {
        Self {
            phase: SetupPhase::AuthenticateManifest,
            commands: Vec::new(),
            final_report_published: false,
        }
    }

    fn absorb_attempted(&mut self, error: &anyhow::Error) {
        self.commands
            .extend_from_slice(process::attempted_receipts(error));
    }

    fn extend(&mut self, commands: impl IntoIterator<Item = process::CommandReceipt>) {
        self.commands.extend(commands);
    }
}

pub(super) fn run(root: &Path, args: &VerifyEmbeddedHeadToHeadArgs) -> Result<()> {
    ensure!(root.is_absolute(), "workspace root must be absolute");
    let _lock = RunLock::acquire(root)?;
    invalidate_previous_evidence(root)?;
    let mut ledger = SetupLedger::new();
    let result = run_after_invalidation(root, args, &mut ledger);
    if let Err(error) = result {
        if ledger.final_report_published {
            return Err(error);
        }
        ledger.absorb_attempted(&error);
        publish_setup_failure(root, &ledger, &error)?;
        return Err(error);
    }
    Ok(())
}

fn run_after_invalidation(
    root: &Path,
    args: &VerifyEmbeddedHeadToHeadArgs,
    ledger: &mut SetupLedger,
) -> Result<()> {
    ledger.phase = SetupPhase::AuthenticateManifest;
    let manifest_path = manifest::path(root);
    let current_manifest = manifest::load_authenticated(&manifest_path)?;
    let manifest = current_manifest.manifest();
    let manifest_sha256 = current_manifest.sha256();
    ledger.phase = SetupPhase::RejectPendingRows;
    let selection = select(manifest, &args.only)?;
    let entries = selection.rows();
    let (git, git_tool, history) =
        authenticate_git_and_history(&current_manifest, root, &args.baseline_git_revision, ledger)?;
    ledger.phase = SetupPhase::AuthenticateMeasuredTools;
    let probed_tools = resolve_tools(args, &manifest.measured_tools)?;
    ledger.extend(probed_tools.probe_commands.iter().cloned());
    ledger.phase = SetupPhase::AuthenticateCompilerToolchain;
    let compiler_tools = resolve_compiler_tools(&manifest.measured_tools)?;
    ledger.extend(compiler_tools.commands.iter().cloned());
    let tool_closure = authenticate_run_tool_closure(
        &git,
        &probed_tools,
        &compiler_tools,
        &manifest.measured_tools.execution_tool_closure_sha256,
        ledger,
    )?;
    let artifact_dir = prepare_artifacts(root)?;
    ledger.phase = SetupPhase::CaptureCompilerSource;
    let compiler_source =
        compiler_source::stage(root, &artifact_dir.join("compiler-source"), &git)?;
    let compiler_source_commands = compiler_source.commands().to_vec();
    ledger.extend(compiler_source_commands.iter().cloned());
    ledger.phase = SetupPhase::CaptureCompilerDependencies;
    let compiler_dependencies = compiler_deps::stage(compiler_deps::ResolutionInputs {
        cargo: compiler_tools.cargo.for_execution()?,
        rustc: compiler_tools.rustc.for_execution()?,
        source_root: compiler_source.root(),
        destination: &artifact_dir.join("compiler-vendor"),
        artifact_root: &artifact_dir,
        temporary_dir: &artifact_dir.join("compiler-metadata-tmp"),
        source_cargo_home: &compiler_tools.source_cargo_home,
        nix_path: &compiler_tools.nix_path,
        expected_sha256: &manifest.measured_tools.compiler_dependencies_sha256,
    })?;
    ledger.extend(compiler_dependencies.commands().iter().cloned());
    ledger.phase = SetupPhase::BuildCompiler;
    let (compiler, compiler_build_environment_sha256, compiler_build_command) = build_compiler(
        &compiler_source,
        &compiler_dependencies,
        &compiler_tools,
        &tool_closure,
        &artifact_dir,
    )?;
    ledger.commands.push(compiler_build_command);
    compiler_source.verify_after_build()?;
    compiler_dependencies.verify_after_build()?;
    let (compiler_source_evidence, _) = compiler_source.into_evidence();
    let compiler_dependency_evidence = compiler_dependencies.into_evidence();
    let setup_commands = ledger.commands.clone();
    ledger.phase = SetupPhase::BindRows;
    let (setup, rows) = bind_rows(
        entries,
        BindRowsInputs {
            setup_commands,
            git_tool,
            tools: probed_tools,
            tool_closure,
            compiler,
            compiler_source: compiler_source_evidence,
            compiler_dependencies: compiler_dependency_evidence,
            compiler_toolchain: compiler_tools.evidence,
            compiler_build_environment_sha256,
            comparator_outputs: manifest.comparator_outputs.clone(),
            suite_implementation_sha256: manifest
                .suite_implementation_history
                .active_sha256()
                .to_owned(),
            suite_implementation_history_sha256: manifest.suite_implementation_history.digest()?,
        },
    )?;
    let started = Instant::now();
    let verdicts = rows
        .iter()
        .map(|row| measure_row(root, row, &artifact_dir))
        .collect::<Vec<_>>();
    ledger.phase = SetupPhase::PublishFinalReport;
    let result = report(ReportInputs {
        root,
        manifest: &manifest_path,
        manifest_sha256,
        history: &history,
        setup: &setup,
        scope: selection.scope(),
        verdicts: &verdicts,
        wall_seconds: started.elapsed().as_secs_f64(),
    });
    ledger.final_report_published = root.join(SUMMARY_PATH).is_file();
    result
}

fn authenticate_run_tool_closure(
    git: &typed_path::AuthenticatedExecutable<typed_path::GitExecutable>,
    tools: &ProbedTools,
    compiler: &CompilerTools,
    expected_sha256: &str,
    ledger: &mut SetupLedger,
) -> Result<tool_closure::AuthenticatedToolClosure> {
    ledger.phase = SetupPhase::AuthenticateToolClosure;
    let closure = authenticate_tool_closure(git, tools, compiler, expected_sha256)?;
    ledger.extend(closure.commands().iter().cloned());
    Ok(closure)
}

fn authenticate_git_and_history(
    current_manifest: &manifest::CheckedCurrentManifest,
    root: &Path,
    baseline_git_revision: &str,
    ledger: &mut SetupLedger,
) -> Result<(
    typed_path::AuthenticatedExecutable<typed_path::GitExecutable>,
    GitToolEvidence,
    manifest::AuthenticatedHistory,
)> {
    let manifest = current_manifest.manifest();
    ledger.phase = SetupPhase::AuthenticateGit;
    let git = resolve_git_executable(&manifest.measured_tools)?;
    let mut command = process::HermeticCommand::new(git.for_execution()?);
    command.arg("--version");
    let (git_version, git_probe) = version(command, "git")?;
    ledger.commands.push(git_probe);
    ensure!(
        git_version == manifest.measured_tools.git_version,
        "Git version `{git_version}` does not match reviewed pin `{}`",
        manifest.measured_tools.git_version
    );
    let git_tool = GitToolEvidence {
        version: git_version,
        sha256: manifest.measured_tools.git_sha256.clone(),
    };
    ledger.phase = SetupPhase::AuthenticateBaseline;
    let history =
        manifest::authenticate_git_history(current_manifest, root, &git, baseline_git_revision)?;
    ledger.extend(history.commands().iter().cloned());
    if history.evidence().is_bootstrap() {
        println!(
            "Bootstrapping instruction ratchet: authenticated ancestor commit {} has no predecessor manifest",
            history.evidence().predecessor_commit()
        );
    }
    Ok((git, git_tool, history))
}

struct BoundRow<'a> {
    entry: &'a Entry,
    plan: cross::BoundExecutionPlan,
}

struct ProbedTools {
    gcc: typed_path::AuthenticatedExecutable<typed_path::GccExecutable>,
    nm: typed_path::AuthenticatedExecutable<typed_path::NmExecutable>,
    qemu: typed_path::AuthenticatedExecutable<typed_path::QemuExecutable>,
    python: typed_path::AuthenticatedExecutable<typed_path::PythonExecutable>,
    prlimit: typed_path::AuthenticatedExecutable<typed_path::PrlimitExecutable>,
    versions: ToolVersions,
    sha256: ToolHashes,
    probe_commands: Vec<process::CommandReceipt>,
}

struct CompilerTools {
    cargo: typed_path::AuthenticatedExecutable<typed_path::CargoExecutable>,
    rustc: typed_path::AuthenticatedExecutable<typed_path::RustcExecutable>,
    source_cargo_home: PathBuf,
    nix_path: std::ffi::OsString,
    evidence: CompilerToolchainEvidence,
    commands: Vec<process::CommandReceipt>,
}

fn authenticate_tool_closure(
    git: &typed_path::AuthenticatedExecutable<typed_path::GitExecutable>,
    tools: &ProbedTools,
    compiler: &CompilerTools,
    expected_sha256: &str,
) -> Result<tool_closure::AuthenticatedToolClosure> {
    let mut inputs = vec![
        git.canonical_target().to_path_buf(),
        tools.gcc.canonical_target().to_path_buf(),
        tools.nm.canonical_target().to_path_buf(),
        tools.qemu.canonical_target().to_path_buf(),
        tools.python.canonical_target().to_path_buf(),
        tools.prlimit.canonical_target().to_path_buf(),
        compiler.cargo.canonical_target().to_path_buf(),
        compiler.rustc.canonical_target().to_path_buf(),
        PathBuf::from(&compiler.evidence.rust_sysroot),
    ];
    inputs.extend(std::env::split_paths(&compiler.nix_path));
    tool_closure::AuthenticatedToolClosure::capture(inputs, expected_sha256)
}

#[derive(Serialize)]
struct CompilerToolchainEvidence {
    cargo_version: String,
    cargo_sha256: String,
    rustc_version: String,
    rustc_sha256: String,
    host_target: String,
    rust_sysroot: String,
    rust_sysroot_tree_sha256: String,
    nix_path_sha256: String,
    nix_store_input_roots: Vec<String>,
    resolver_cargo_home: String,
}

#[derive(Clone, Serialize)]
struct ToolVersions {
    arm_gcc: String,
    arm_nm: String,
    qemu: String,
    python: String,
    prlimit: String,
}

#[derive(Serialize)]
struct ToolHashes {
    arm_gcc: String,
    arm_nm: String,
    qemu: String,
    python: String,
    prlimit: String,
}

fn resolve_tools(
    args: &VerifyEmbeddedHeadToHeadArgs,
    expected: &MeasuredToolPins,
) -> Result<ProbedTools> {
    let mut commands = Vec::new();
    match resolve_tools_inner(args, expected, &mut commands) {
        Ok(mut tools) => {
            tools.probe_commands = commands;
            Ok(tools)
        }
        Err(error) => Err(process::attach_prior_receipts(error, commands)),
    }
}

fn resolve_tools_inner(
    args: &VerifyEmbeddedHeadToHeadArgs,
    expected: &MeasuredToolPins,
    commands: &mut Vec<process::CommandReceipt>,
) -> Result<ProbedTools> {
    let ResolvedToolExecutables {
        gcc,
        nm,
        qemu,
        python,
        prlimit,
    } = resolve_tool_executables(args, expected)?;
    let sha256 = ToolHashes {
        arm_gcc: expected.arm_gcc_sha256.clone(),
        arm_nm: expected.arm_nm_sha256.clone(),
        qemu: expected.qemu_sha256.clone(),
        python: expected.python_sha256.clone(),
        prlimit: expected.prlimit_sha256.clone(),
    };
    let (arm_gcc, gcc_probe) = version(
        cross::gcc_version_command(gcc.for_execution()?),
        "arm-none-eabi-gcc",
    )?;
    commands.push(gcc_probe);
    let (arm_nm, nm_probe) = version(
        cross::nm_version_command(nm.for_execution()?),
        "arm-none-eabi-nm",
    )?;
    commands.push(nm_probe);
    let (qemu_version, qemu_probe) = version(
        cross::qemu_version_command(qemu.for_execution()?),
        "qemu-system-arm",
    )?;
    commands.push(qemu_probe);
    let (python_version, python_probe) = version(
        cross::python_version_command(python.for_execution()?),
        "python",
    )?;
    commands.push(python_probe);
    let (prlimit_version, prlimit_probe) = version(
        cross::prlimit_version_command(prlimit.for_execution()?),
        "prlimit",
    )?;
    commands.push(prlimit_probe);
    ensure!(
        prlimit_version == expected.prlimit_version,
        "prlimit version `{prlimit_version}` does not match reviewed pin `{}`",
        expected.prlimit_version
    );
    let versions = ToolVersions {
        arm_gcc,
        arm_nm,
        qemu: qemu_version,
        python: python_version,
        prlimit: prlimit_version,
    };
    Ok(ProbedTools {
        gcc,
        nm,
        qemu,
        python,
        prlimit,
        versions,
        sha256,
        probe_commands: Vec::new(),
    })
}

struct ResolvedToolExecutables {
    gcc: typed_path::AuthenticatedExecutable<typed_path::GccExecutable>,
    nm: typed_path::AuthenticatedExecutable<typed_path::NmExecutable>,
    qemu: typed_path::AuthenticatedExecutable<typed_path::QemuExecutable>,
    python: typed_path::AuthenticatedExecutable<typed_path::PythonExecutable>,
    prlimit: typed_path::AuthenticatedExecutable<typed_path::PrlimitExecutable>,
}

fn resolve_tool_executables(
    args: &VerifyEmbeddedHeadToHeadArgs,
    expected: &MeasuredToolPins,
) -> Result<ResolvedToolExecutables> {
    let caller_directory = std::env::current_dir().context("failed to resolve caller directory")?;
    let arm_toolchain = absolute_from(&caller_directory, &args.arm_toolchain);
    let qemu_path = absolute_from(&caller_directory, &args.qemu);
    let python_path = absolute_from(&caller_directory, &args.python);
    let prlimit_path = absolute_from(&caller_directory, &args.prlimit);
    ensure!(arm_toolchain.is_dir(), "--arm-toolchain is not a directory");
    ensure!(
        qemu_path.is_file(),
        "--qemu is not a file: {}",
        qemu_path.display()
    );
    ensure!(
        python_path.is_file(),
        "--python is not a file: {}",
        python_path.display()
    );
    ensure!(
        prlimit_path.is_file(),
        "--prlimit is not a file: {}",
        prlimit_path.display()
    );
    let bin = arm_toolchain.join("bin");
    let gcc_path = bin.join("arm-none-eabi-gcc");
    let nm_path = bin.join("arm-none-eabi-nm");
    ensure!(
        gcc_path.is_file() && nm_path.is_file(),
        "ARM root lacks required gcc or nm"
    );
    let gcc = typed_path::AuthenticatedExecutable::checked(
        RolePath::checked(gcc_path)?,
        &expected.arm_gcc_sha256,
    )?;
    let nm = typed_path::AuthenticatedExecutable::checked(
        RolePath::checked(nm_path)?,
        &expected.arm_nm_sha256,
    )?;
    let qemu = typed_path::AuthenticatedExecutable::checked(
        RolePath::checked(qemu_path)?,
        &expected.qemu_sha256,
    )?;
    let python = typed_path::AuthenticatedExecutable::checked(
        RolePath::checked(python_path)?,
        &expected.python_sha256,
    )?;
    let prlimit = typed_path::AuthenticatedExecutable::checked(
        RolePath::checked(prlimit_path)?,
        &expected.prlimit_sha256,
    )?;
    Ok(ResolvedToolExecutables {
        gcc,
        nm,
        qemu,
        python,
        prlimit,
    })
}

fn resolve_compiler_tools(expected: &MeasuredToolPins) -> Result<CompilerTools> {
    let mut commands = Vec::new();
    match resolve_compiler_tools_inner(expected, &mut commands) {
        Ok(mut tools) => {
            tools.commands = commands;
            Ok(tools)
        }
        Err(error) => Err(process::attach_prior_receipts(error, commands)),
    }
}

fn resolve_compiler_tools_inner(
    expected: &MeasuredToolPins,
    commands: &mut Vec<process::CommandReceipt>,
) -> Result<CompilerTools> {
    let cargo_path = resolve_cargo_executable()?;
    ensure!(
        cargo_path.starts_with("/nix/store"),
        "compiler Cargo must resolve from the locked Nix store"
    );
    let rustc_path = cargo_path
        .parent()
        .context("Cargo executable has no parent")?
        .join("rustc")
        .canonicalize()
        .context("failed to resolve rustc beside Cargo")?;
    let cargo = typed_path::AuthenticatedExecutable::checked(
        RolePath::checked(cargo_path)?,
        &expected.cargo_sha256,
    )?;
    let rustc = typed_path::AuthenticatedExecutable::checked(
        RolePath::checked(rustc_path)?,
        &expected.rustc_sha256,
    )?;
    let mut cargo_command = process::HermeticCommand::new(cargo.for_execution()?);
    cargo_command.arg("--version");
    let (cargo_version, cargo_receipt) = version(cargo_command, "cargo")?;
    commands.push(cargo_receipt);
    ensure!(
        cargo_version == expected.cargo_version,
        "Cargo version `{cargo_version}` does not match reviewed pin `{}`",
        expected.cargo_version
    );
    let mut rustc_command = process::HermeticCommand::new(rustc.for_execution()?);
    rustc_command.args(["--version", "--verbose"]);
    let (rustc_output, rustc_receipt) = process::output(&mut rustc_command, process::Limit::Probe)?;
    commands.push(rustc_receipt);
    ensure!(
        rustc_output.status.success(),
        "rustc --version --verbose failed"
    );
    let rustc_verbose = process::combined(&rustc_output);
    let rustc_version = rustc_verbose
        .lines()
        .next()
        .context("rustc verbose version output was empty")?
        .to_owned();
    ensure!(
        rustc_version == expected.rustc_version,
        "rustc version `{rustc_version}` does not match reviewed pin `{}`",
        expected.rustc_version
    );
    let host_target = rustc_host_target(&rustc_verbose)?;
    ensure!(
        host_target == process::COMPILER_HOST_TARGET,
        "rustc host target `{host_target}` does not match benchmark compiler target `{}`",
        process::COMPILER_HOST_TARGET
    );
    let mut sysroot_command = process::HermeticCommand::new(rustc.for_execution()?);
    sysroot_command.args(["--print", "sysroot"]);
    let (output, sysroot_receipt) = process::output(&mut sysroot_command, process::Limit::Probe)?;
    commands.push(sysroot_receipt.clone());
    if !output.status.success() {
        bail!("rustc --print sysroot failed");
    }
    let rust_sysroot = process::combined(&output).trim().to_string();
    let rust_sysroot_path = PathBuf::from(&rust_sysroot);
    ensure!(
        rust_sysroot_path.is_absolute()
            && rust_sysroot_path.starts_with("/nix/store")
            && rust_sysroot_path.is_dir(),
        "rustc sysroot is not an immutable Nix store directory"
    );
    let source_cargo_home = resolve_source_cargo_home()?;
    let nix_path = closed_nix_path()?;
    let nix_path_sha256 = format!("{:x}", Sha256::digest(nix_path.as_encoded_bytes()));
    let nix_store_input_roots = nix_store_input_roots(
        &nix_path,
        [
            cargo.for_execution()?.as_path(),
            rustc.for_execution()?.as_path(),
            &rust_sysroot_path,
        ],
    )?;
    let rust_sysroot_tree_sha256 = compiler_deps::toolchain_tree_sha256(&rust_sysroot_path)?;
    Ok(CompilerTools {
        cargo,
        rustc,
        source_cargo_home: source_cargo_home.clone(),
        nix_path,
        evidence: CompilerToolchainEvidence {
            cargo_version,
            cargo_sha256: expected.cargo_sha256.clone(),
            rustc_version,
            rustc_sha256: expected.rustc_sha256.clone(),
            host_target,
            rust_sysroot,
            rust_sysroot_tree_sha256,
            nix_path_sha256,
            nix_store_input_roots,
            resolver_cargo_home: source_cargo_home.display().to_string(),
        },
        commands: Vec::new(),
    })
}

fn rustc_host_target(verbose_version: &str) -> Result<String> {
    let hosts = verbose_version
        .lines()
        .filter_map(|line| line.strip_prefix("host: "))
        .collect::<Vec<_>>();
    ensure!(hosts.len() == 1, "rustc verbose version has no unique host");
    ensure!(
        !hosts[0].is_empty() && !hosts[0].chars().any(char::is_whitespace),
        "rustc verbose host is malformed"
    );
    Ok(hosts[0].to_owned())
}

fn resolve_source_cargo_home() -> Result<PathBuf> {
    std::env::var_os("CARGO_HOME")
        .filter(|value| !value.is_empty())
        .map(PathBuf::from)
        .or_else(|| std::env::var_os("HOME").map(|value| PathBuf::from(value).join(".cargo")))
        .context("CARGO_HOME and HOME are both unavailable")?
        .canonicalize()
        .context("failed to resolve Cargo home")
}

fn absolute_from(base: &Path, path: &Path) -> PathBuf {
    if path.is_absolute() {
        path.to_path_buf()
    } else {
        base.join(path)
    }
}

struct BindRowsInputs {
    setup_commands: Vec<process::CommandReceipt>,
    git_tool: GitToolEvidence,
    tools: ProbedTools,
    tool_closure: tool_closure::AuthenticatedToolClosure,
    compiler: cross::AuthenticatedCompiler,
    compiler_source: compiler_source::CompilerSourceEvidence,
    compiler_dependencies: compiler_deps::DependencyEvidence,
    compiler_toolchain: CompilerToolchainEvidence,
    compiler_build_environment_sha256: String,
    comparator_outputs: manifest::ComparatorOutputPins,
    suite_implementation_sha256: String,
    suite_implementation_history_sha256: String,
}

fn bind_rows<'a>(
    entries: &[&'a Entry],
    inputs: BindRowsInputs,
) -> Result<(SetupEvidence, Vec<BoundRow<'a>>)> {
    let BindRowsInputs {
        setup_commands,
        git_tool,
        tools,
        tool_closure,
        compiler,
        compiler_source,
        compiler_dependencies,
        compiler_toolchain,
        compiler_build_environment_sha256,
        comparator_outputs,
        suite_implementation_sha256,
        suite_implementation_history_sha256,
    } = inputs;
    ensure!(!entries.is_empty(), "manifest selection is empty");
    let root = cross::authenticate_execution_root(cross::ExecutionRootInputs {
        gcc: tools.gcc,
        nm: tools.nm,
        qemu: tools.qemu,
        python: tools.python,
        prlimit: tools.prlimit,
        gcc_version: tools.versions.arm_gcc.clone(),
        nm_version: tools.versions.arm_nm.clone(),
        qemu_version: tools.versions.qemu.clone(),
        python_version: tools.versions.python.clone(),
        prlimit_version: tools.versions.prlimit.clone(),
        compiler,
        comparator_outputs,
        tool_closure,
    })?;
    let setup = SetupEvidence {
        commands: setup_commands,
        versions: tools.versions,
        tool_sha256: tools.sha256,
        git_tool,
        compiler_source,
        compiler_dependencies,
        compiler_toolchain,
        tool_closure: root.tool_closure_evidence().clone(),
        compiler_build_environment_sha256,
        compiler_sha256: root.compiler_sha256().to_string(),
        compiler_artifact: root.compiler_artifact().clone(),
        compiler_runtime_environment_sha256: root.compiler_runtime_environment_sha256(),
        suite_implementation_sha256,
        suite_implementation_history_sha256,
    };
    let rows = entries
        .iter()
        .map(|entry| {
            Ok(BoundRow {
                entry,
                plan: cross::bind_execution_plan(&root, entry)?,
            })
        })
        .collect::<Result<Vec<_>>>()?;
    Ok((setup, rows))
}

fn version(
    mut command: process::HermeticCommand,
    label: &str,
) -> Result<(String, process::CommandReceipt)> {
    let (output, receipt) = process::output(&mut command, process::Limit::Probe)?;
    if !output.status.success() {
        return Err(process::attach_attempted_receipt(
            anyhow::anyhow!("{label} --version failed"),
            receipt,
        ));
    }
    let version = process::combined(&output)
        .lines()
        .next()
        .map(str::to_string)
        .ok_or_else(|| {
            process::attach_attempted_receipt(
                anyhow::anyhow!("tool version output was empty"),
                receipt.clone(),
            )
        })?;
    Ok((version, receipt))
}

fn build_compiler(
    source: &compiler_source::CompilerSourceSnapshot,
    dependencies: &compiler_deps::DependencySnapshot,
    tools: &CompilerTools,
    tool_closure: &tool_closure::AuthenticatedToolClosure,
    artifact_dir: &Path,
) -> Result<(
    cross::AuthenticatedCompiler,
    String,
    process::CommandReceipt,
)> {
    let cargo = tools.cargo.for_execution()?;
    let rustc = tools.rustc.for_execution()?;
    let (mut command, compiler) = prepare_compiler_build(CompilerBuildPreparation {
        source_root: source.root(),
        vendor: dependencies.root(),
        cargo,
        rustc,
        nix_path: &tools.nix_path,
        artifact_dir,
    })?;
    let build_command = process::require_workspace_success(
        &mut command,
        "workspace compiler build",
        process::Limit::WorkspaceBuild,
    )?;
    ensure!(
        compiler.is_file(),
        "workspace build produced no compiler at the closed output {}",
        compiler.display()
    );
    let environment_sha256 = build_command.sha256();
    let retained_command = build_command.clone();
    let compiler = cross::stage_compiler(&compiler, artifact_dir, tool_closure)
        .map_err(|error| process::attach_prior_receipts(error, vec![retained_command.clone()]))?;
    Ok((compiler, environment_sha256, retained_command))
}

struct CompilerBuildPreparation<'a> {
    source_root: &'a Path,
    vendor: &'a Path,
    cargo: &'a RolePath<typed_path::CargoExecutable>,
    rustc: &'a RolePath<typed_path::RustcExecutable>,
    nix_path: &'a std::ffi::OsStr,
    artifact_dir: &'a Path,
}

fn prepare_compiler_build(
    inputs: CompilerBuildPreparation<'_>,
) -> Result<(process::CompilerBuildCommand, PathBuf)> {
    let CompilerBuildPreparation {
        source_root,
        vendor,
        cargo,
        rustc,
        nix_path,
        artifact_dir,
    } = inputs;
    ensure!(
        source_root.is_absolute()
            && artifact_dir.is_absolute()
            && vendor.is_absolute()
            && source_root.starts_with(artifact_dir)
            && vendor.starts_with(artifact_dir),
        "compiler build must use absolute gate-owned source and dependency snapshots"
    );
    let target_dir = artifact_dir.join("compiler-build-target");
    ensure!(
        !target_dir.exists(),
        "fresh gate compiler target unexpectedly exists: {}",
        target_dir.display()
    );
    let compiler = target_dir
        .join(process::COMPILER_HOST_TARGET)
        .join("debug/rumoca");
    let temporary_dir = artifact_dir.join("compiler-build-tmp");
    let cargo_home = artifact_dir.join("compiler-build-cargo-home");
    ensure!(
        !temporary_dir.exists() && !cargo_home.exists(),
        "fresh compiler temporary or Cargo home directory unexpectedly exists"
    );
    fs::create_dir_all(&temporary_dir)?;
    fs::create_dir_all(&cargo_home)?;
    let command = process::CompilerBuildCommand::rumoca(process::CompilerBuildInputs {
        cargo,
        rustc,
        source_root,
        target_dir: &target_dir,
        temporary_dir: &temporary_dir,
        cargo_home: &cargo_home,
        vendor,
        nix_path,
    })?;
    Ok((command, compiler))
}

fn closed_nix_path() -> Result<std::ffi::OsString> {
    let path = std::env::var_os("PATH").context("PATH is unavailable for Nix tool closure")?;
    let mut seen = std::collections::BTreeSet::new();
    let directories = std::env::split_paths(&path)
        .filter(|directory| directory.starts_with("/nix/store") && directory.is_dir())
        .filter(|directory| seen.insert(directory.clone()))
        .collect::<Vec<_>>();
    ensure!(
        !directories.is_empty(),
        "PATH contains no Nix store directories"
    );
    std::env::join_paths(directories).context("failed to encode closed Nix PATH")
}

fn nix_store_input_roots<'a>(
    nix_path: &std::ffi::OsStr,
    additional: impl IntoIterator<Item = &'a Path>,
) -> Result<Vec<String>> {
    let mut paths = std::env::split_paths(nix_path).collect::<Vec<_>>();
    paths.extend(additional.into_iter().map(Path::to_path_buf));
    let mut roots = std::collections::BTreeSet::new();
    for path in paths {
        let relative = path.strip_prefix("/nix/store").with_context(|| {
            format!(
                "compiler tool input escaped the Nix store: {}",
                path.display()
            )
        })?;
        let component = relative
            .components()
            .next()
            .filter(|component| matches!(component, std::path::Component::Normal(_)))
            .context("compiler tool input has no Nix store root")?;
        let root = Path::new("/nix/store").join(component.as_os_str());
        ensure!(root.is_dir(), "Nix store input root is not a directory");
        roots.insert(root.display().to_string());
    }
    ensure!(
        !roots.is_empty(),
        "compiler tool closure has no Nix store inputs"
    );
    Ok(roots.into_iter().collect())
}

fn resolve_cargo_executable() -> Result<PathBuf> {
    let requested = std::env::var_os("CARGO").unwrap_or_else(|| "cargo".into());
    let requested = PathBuf::from(requested);
    if requested.is_absolute() {
        ensure!(
            requested.is_file(),
            "CARGO is not a file: {}",
            requested.display()
        );
        return Ok(requested);
    }
    let path = std::env::var_os("PATH").context("PATH is absent while resolving Cargo")?;
    std::env::split_paths(&path)
        .map(|directory| directory.join(&requested))
        .find(|candidate| candidate.is_file())
        .map(|candidate| candidate.canonicalize())
        .transpose()?
        .context("Cargo executable was not found on PATH")
}

fn resolve_git_executable(
    expected: &MeasuredToolPins,
) -> Result<typed_path::AuthenticatedExecutable<typed_path::GitExecutable>> {
    let path = std::env::var_os("PATH").context("PATH is absent while resolving Git")?;
    let executable = std::env::split_paths(&path)
        .map(|directory| directory.join("git"))
        .find(|candidate| candidate.is_file())
        .context("Git executable was not found on PATH")?
        .canonicalize()
        .context("failed to resolve the Git executable")?;
    ensure!(
        executable.is_absolute(),
        "resolved Git executable is not absolute"
    );
    authenticate_git_candidate(executable, expected)
}

fn authenticate_git_candidate(
    executable: PathBuf,
    expected: &MeasuredToolPins,
) -> Result<typed_path::AuthenticatedExecutable<typed_path::GitExecutable>> {
    let path = RolePath::checked(executable)?;
    typed_path::AuthenticatedExecutable::checked(path, &expected.git_sha256)
}

fn prepare_artifacts(root: &Path) -> Result<PathBuf> {
    let directory = root.join(ARTIFACT_DIR);
    emit::fresh_directory(&directory)?;
    Ok(directory)
}

fn invalidate_previous_evidence(root: &Path) -> Result<()> {
    for relative in [SUMMARY_PATH, SUMMARY_TEMP_PATH] {
        let path = root.join(relative);
        match fs::remove_file(&path) {
            Ok(()) => {}
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => {}
            Err(error) => {
                return Err(error)
                    .with_context(|| format!("failed to invalidate {}", path.display()));
            }
        }
    }
    let artifacts = root.join(ARTIFACT_DIR);
    if artifacts.exists() {
        make_tree_removable(&artifacts)?;
    }
    match fs::remove_dir_all(&artifacts) {
        Ok(()) => {}
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => {}
        Err(error) => {
            return Err(error)
                .with_context(|| format!("failed to invalidate {}", artifacts.display()));
        }
    }
    Ok(())
}

fn make_tree_removable(path: &Path) -> Result<()> {
    let metadata = fs::symlink_metadata(path)?;
    if metadata.file_type().is_symlink() {
        return Ok(());
    }
    let mut permissions = metadata.permissions();
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let added = if metadata.is_dir() { 0o700 } else { 0o200 };
        permissions.set_mode(permissions.mode() | added);
    }
    #[cfg(not(unix))]
    permissions.set_readonly(false);
    fs::set_permissions(path, permissions)?;
    if metadata.is_dir() {
        for entry in fs::read_dir(path)? {
            make_tree_removable(&entry?.path())?;
        }
    }
    Ok(())
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
#[serde(tag = "kind", rename_all = "kebab-case")]
enum SelectionScope {
    Full {
        rows: NonEmptyRowIds,
    },
    Partial {
        selected_rows: NonEmptyRowIds,
        omitted_rows: NonEmptyRowIds,
    },
}

impl SelectionScope {
    fn full(rows: Vec<String>) -> Result<Self> {
        Ok(Self::Full {
            rows: NonEmptyRowIds::new(rows)?,
        })
    }

    fn partial(selected_rows: Vec<String>, omitted_rows: Vec<String>) -> Result<Self> {
        Ok(Self::Partial {
            selected_rows: NonEmptyRowIds::new(selected_rows)?,
            omitted_rows: NonEmptyRowIds::new(omitted_rows)?,
        })
    }

    const fn selected_rows(&self) -> &NonEmptyRowIds {
        match self {
            Self::Full { rows } => rows,
            Self::Partial { selected_rows, .. } => selected_rows,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct NonEmptyRowIds {
    first: String,
    rest: Vec<String>,
}

impl NonEmptyRowIds {
    fn new(rows: Vec<String>) -> Result<Self> {
        for (index, id) in rows.iter().enumerate() {
            ensure!(
                !rows[..index].contains(id),
                "row identity set repeats `{id}`"
            );
        }
        let mut rows = rows.into_iter();
        let first = rows.next().context("row identity set is empty")?;
        Ok(Self {
            first,
            rest: rows.collect(),
        })
    }

    fn len(&self) -> usize {
        1 + self.rest.len()
    }

    fn iter(&self) -> impl Iterator<Item = &String> {
        std::iter::once(&self.first).chain(&self.rest)
    }
}

impl Serialize for NonEmptyRowIds {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut sequence = serializer.serialize_seq(Some(self.len()))?;
        for id in self.iter() {
            sequence.serialize_element(id)?;
        }
        sequence.end()
    }
}

struct SelectedEntries<'a> {
    rows: Vec<&'a Entry>,
    scope: SelectionScope,
}

impl<'a> SelectedEntries<'a> {
    fn rows(&self) -> &[&'a Entry] {
        &self.rows
    }

    const fn scope(&self) -> &SelectionScope {
        &self.scope
    }
}

fn select<'a>(manifest: &'a Manifest, only: &[String]) -> Result<SelectedEntries<'a>> {
    for (index, id) in only.iter().enumerate() {
        ensure!(
            !only[..index].contains(id),
            "row `{id}` was selected more than once"
        );
    }
    let selected = if only.is_empty() {
        manifest.entries.iter().collect::<Vec<_>>()
    } else {
        manifest
            .entries
            .iter()
            .filter(|entry| only.contains(&entry.id))
            .collect::<Vec<_>>()
    };
    for id in only {
        ensure!(
            selected.iter().any(|entry| &entry.id == id),
            "unknown row `{id}`"
        );
    }
    ensure!(!selected.is_empty(), "manifest selection is empty");
    let pending = selected
        .iter()
        .filter(|entry| entry.evidence_state.is_pending())
        .map(|entry| format!("{} {}", entry.id, entry.evidence_state.label()))
        .collect::<Vec<_>>();
    ensure!(
        pending.is_empty(),
        "selected competitor rows are pending and cannot execute or support a claim: {}",
        pending.join(", ")
    );
    let selected_ids = selected
        .iter()
        .map(|entry| entry.id.clone())
        .collect::<Vec<_>>();
    let omitted = manifest
        .entries
        .iter()
        .filter(|entry| !selected_ids.contains(&entry.id))
        .map(|entry| entry.id.clone())
        .collect::<Vec<_>>();
    let scope = if omitted.is_empty() {
        SelectionScope::full(selected_ids)?
    } else {
        SelectionScope::partial(selected_ids, omitted)?
    };
    Ok(SelectedEntries {
        rows: selected,
        scope,
    })
}

fn validate_measured_call_required(
    plan: &cross::BoundExecutionPlan,
    context: &BuildContext<'_>,
    row: &Path,
    rumoca: &emit::RumocaEmission,
    expected_cases: &[CorrectnessCase],
) -> Result<Vec<process::CommandReceipt>> {
    const CALL: &str = "    ExpMixedStep_dostep(&state); /* MEASURED_CALL */\n";
    let source = fs::read_to_string(context.harness.rumoca_measured().as_path())?;
    ensure!(
        source.matches(CALL).count() == 1,
        "Rumoca driver must contain exactly one measured call"
    );
    let mutant_dir = row.join("mutation-deleted-call");
    fs::create_dir_all(&mutant_dir)?;
    let mutant_driver = mutant_dir.join("driver_rumoca.c");
    fs::write(
        &mutant_driver,
        source.replacen(CALL, "    /* measured call deliberately deleted */\n", 1),
    )?;
    let mutant_driver = RolePath::<RumocaDeletedCallDriver>::checked(mutant_driver)?;
    let mutation_guard = artifact_guard::FrozenArtifactSet::capture_tree(&mutant_dir)?;
    let linked = cross::build_rumoca_deleted_call(
        rumoca,
        &mutant_driver,
        &mutation_guard,
        &row.join("build-mutation-deleted-call"),
        context,
    )?;
    let mut commands = linked.commands.clone();
    let validation = qemu::validate_deleted_call_rejected(
        plan,
        &linked,
        "rumoca_exp_mixed",
        &expected_cases[..1],
        row,
    );
    match validation {
        Ok(receipt) => commands.push(receipt),
        Err(error) => return Err(process::attach_prior_receipts(error, commands)),
    }
    Ok(commands)
}

fn validate_constant_output_mutation(
    plan: &cross::BoundExecutionPlan,
    context: &BuildContext<'_>,
    row: &Path,
    rumoca: &emit::RumocaEmission,
    expected_cases: &[CorrectnessCase],
) -> Result<Vec<process::CommandReceipt>> {
    let source = fs::read_to_string(context.harness.rumoca_correctness().as_path())?;
    let source = replace_once(
        source,
        "static ExpMixedStepState state;\n",
        "static ExpMixedStepState state;\nstatic float mutation_constant_output[10];\n",
        "Rumoca state declaration",
    )?;
    let source = replace_once(
        source,
        "    accepted = report_case(0);\n",
        "    accepted = report_case(0);\n    copy_float(mutation_constant_output, state.X1, 10);\n",
        "measured output report",
    )?;
    let source = replace_once(
        source,
        "        ExpMixedStep_dostep(&state); /* COHORT_CALL */\n",
        "        copy_float(state.X1, mutation_constant_output, 10); /* CONSTANT_OUTPUT_MUTATION */\n",
        "unmeasured cohort call",
    )?;
    let mutation = row.join("mutation-constant-output");
    fs::create_dir_all(&mutation)?;
    let driver = mutation.join("driver_rumoca.c");
    fs::write(&driver, source)?;
    let driver = RolePath::<RumocaConstantOutputDriver>::checked(driver)?;
    let mutation_guard = artifact_guard::FrozenArtifactSet::capture_tree(&mutation)?;
    let linked = cross::build_rumoca_constant_output(
        rumoca,
        &driver,
        &mutation_guard,
        &row.join("build-mutation-constant-output"),
        context,
    )?;
    mutation_rejection_commands(
        plan,
        linked,
        "rumoca_exp_mixed",
        expected_cases,
        qemu::MutationExpectation::MeasuredPointPreserved,
        row,
    )
}

fn validate_closed_branch_mutation(
    plan: &cross::BoundExecutionPlan,
    context: &BuildContext<'_>,
    row: &Path,
    rumoca: &emit::RumocaEmission,
    expected_cases: &[CorrectnessCase],
) -> Result<Vec<process::CommandReceipt>> {
    const BRANCH: &str =
        "if (rumoca_galec_compare_lt(&self->rumoca_galec_error_signal_status, theta_sq, eps)) {";
    let source = fs::read_to_string(rumoca.model_c.as_path())?;
    ensure!(
        source.matches(BRANCH).count() == 3,
        "generated model must contain three exp_mixed threshold branches"
    );
    let mutation = row.join("mutation-closed-branches");
    fs::create_dir_all(&mutation)?;
    let model = mutation.join("ExpMixedStep.c");
    fs::write(&model, source.replace(BRANCH, "if (theta_sq < -eps) {"))?;
    let driver = mutation.join("driver_rumoca.c");
    fs::copy(context.harness.rumoca_correctness().as_path(), &driver)?;
    let driver = RolePath::<RumocaClosedBranchDriver>::checked(driver)?;
    let model = RolePath::<RumocaClosedBranchModel>::checked(model)?;
    let mutation_guard = artifact_guard::FrozenArtifactSet::capture_tree(&mutation)?;
    let linked = cross::build_rumoca_closed_branches(
        rumoca,
        &driver,
        &model,
        &mutation_guard,
        &row.join("build-mutation-closed-branches"),
        context,
    )?;
    mutation_rejection_commands(
        plan,
        linked,
        "rumoca_exp_mixed",
        expected_cases,
        qemu::MutationExpectation::MeasuredPointPreserved,
        row,
    )
}

fn validate_casadi_layout_mutation(
    plan: &cross::BoundExecutionPlan,
    context: &BuildContext<'_>,
    row: &Path,
    casadi: &emit::CasadiEmission,
    expected_cases: &[CorrectnessCase],
) -> Result<Vec<process::CommandReceipt>> {
    let source = fs::read_to_string(context.harness.casadi_correctness().as_path())?;
    let source = replace_once(
        source,
        "    input_coupling_column_major[1] = input->B[1][0]; /* COLUMN_MAJOR_1 */\n",
        "    input_coupling_column_major[1] = input->B[0][1]; /* ROW_MAJOR_MUTATION_1 */\n",
        "CasADi column-major element one",
    )?;
    let source = replace_once(
        source,
        "    input_coupling_column_major[2] = input->B[0][1]; /* COLUMN_MAJOR_2 */\n",
        "    input_coupling_column_major[2] = input->B[1][0]; /* ROW_MAJOR_MUTATION_2 */\n",
        "CasADi column-major element two",
    )?;
    let mutation = row.join("mutation-row-major");
    fs::create_dir_all(&mutation)?;
    let driver = mutation.join("driver_casadi.c");
    fs::write(&driver, source)?;
    let driver = RolePath::<CasadiRowMajorDriver>::checked(driver)?;
    let mutation_guard = artifact_guard::FrozenArtifactSet::capture_tree(&mutation)?;
    let linked = cross::build_casadi_row_major(
        casadi,
        &driver,
        &mutation_guard,
        &row.join("build-mutation-row-major"),
        context,
    )?;
    mutation_rejection_commands(
        plan,
        linked,
        "casadi_exp_mixed",
        expected_cases,
        qemu::MutationExpectation::AnyCaseRejected,
        row,
    )
}

fn replace_once(source: String, needle: &str, replacement: &str, label: &str) -> Result<String> {
    ensure!(
        source.matches(needle).count() == 1,
        "mutation anchor `{label}` must occur exactly once"
    );
    Ok(source.replacen(needle, replacement, 1))
}

fn mutation_rejection_commands(
    plan: &cross::BoundExecutionPlan,
    linked: cross::LinkedArtifact,
    name: &str,
    expected_cases: &[CorrectnessCase],
    expectation: qemu::MutationExpectation,
    trace: &Path,
) -> Result<Vec<process::CommandReceipt>> {
    let mut commands = linked.commands.clone();
    let validation = qemu::validate_semantic_mutation_rejected(
        plan,
        &linked,
        name,
        expected_cases,
        expectation,
        trace,
    );
    match validation {
        Ok(receipt) => commands.push(receipt),
        Err(error) => return Err(process::attach_prior_receipts(error, commands)),
    }
    Ok(commands)
}

fn write_oracle(directory: &Path, cases: &[CorrectnessCase]) -> Result<()> {
    let names = cases
        .iter()
        .map(|case| format!("\"{}\"", case.id))
        .collect::<Vec<_>>()
        .join(", ");
    let constants = cases
        .iter()
        .map(|case| {
            let words = case
                .expected_output_bits
                .iter()
                .map(|word| format!("UINT32_C(0x{word})"))
                .collect::<Vec<_>>()
                .join(", ");
            format!("    {{{words}}}")
        })
        .collect::<Vec<_>>()
        .join(",\n");
    fs::write(
        directory.join("expected_output.h"),
        format!(
            "#ifndef EXPECTED_OUTPUT_H\n#define EXPECTED_OUTPUT_H\n#include <stdint.h>\n#define CORRECTNESS_CASE_COUNT UINT32_C({})\nstatic const char *const CORRECTNESS_CASE_NAMES[{}] = {{{names}}};\nstatic const uint32_t EXPECTED_OUTPUT_BITS[{}][10] = {{\n{constants}\n}};\n#endif\n",
            cases.len(),
            cases.len(),
            cases.len()
        ),
    )?;
    Ok(())
}

#[derive(Serialize)]
struct Evidence {
    entry_sha256: String,
    suite_implementation_sha256: String,
    normalized_profile_sha256: String,
    completed_suite_sha256: String,
    authenticated_inputs_sha256: String,
    rumoca_instructions: u64,
    comparator_instructions: u64,
    output_lines: Vec<String>,
    artifact_bundle: artifact_bundle::BundleEvidence,
    commands: Vec<process::CommandReceipt>,
}

impl Evidence {
    fn from_completed(entry: &Entry, suite: CompletedSuite) -> Result<Self> {
        let artifact_bundle = capture_completed_artifacts(&suite)?;
        Ok(Self {
            entry_sha256: format!("{:x}", Sha256::digest(serde_json::to_vec(entry)?)),
            suite_implementation_sha256: suite_identity::sha256(),
            normalized_profile_sha256: entry.normalized_profile_sha256.clone(),
            completed_suite_sha256: suite.suite_sha256,
            authenticated_inputs_sha256: suite.inputs.closure_sha256().to_string(),
            rumoca_instructions: suite.rumoca_trace.instructions,
            comparator_instructions: suite.casadi_trace.instructions,
            output_lines: suite
                .rumoca_trace
                .output_lines
                .into_iter()
                .chain(suite.casadi_trace.output_lines)
                .chain(suite.correctness_output_lines)
                .collect(),
            artifact_bundle,
            commands: suite.commands,
        })
    }
}

fn capture_completed_artifacts(suite: &CompletedSuite) -> Result<artifact_bundle::BundleEvidence> {
    suite.inputs.verify()?;
    suite.oracle_guard.verify()?;
    suite.rumoca.verify()?;
    suite.casadi.verify()?;
    suite.rumoca_link.verify()?;
    suite.casadi_link.verify()?;
    suite.rumoca_trace.verify()?;
    suite.casadi_trace.verify()?;
    let mut sources = suite.inputs.artifact_sources();
    sources.extend([
        artifact_bundle::Source::new(
            "generated/oracle/expected_output.h",
            &suite.oracle.as_path().join("expected_output.h"),
        ),
        artifact_bundle::Source::new(
            "generated/rumoca/.clang-format",
            &suite.rumoca.include.as_path().join(".clang-format"),
        ),
        artifact_bundle::Source::new(
            "generated/rumoca/ExpMixedStep.c",
            suite.rumoca.model_c.as_path(),
        ),
        artifact_bundle::Source::new(
            "generated/rumoca/ExpMixedStep.h",
            suite.rumoca.model_h.as_path(),
        ),
        artifact_bundle::Source::new(
            "generated/rumoca/rumoca_galec_kernels.c",
            suite.rumoca.kernels_c.as_path(),
        ),
        artifact_bundle::Source::new(
            "generated/rumoca/rumoca_galec_kernels.h",
            suite.rumoca.kernels_h.as_path(),
        ),
        artifact_bundle::Source::new(
            "generated/casadi/casadi_exp_mixed.c",
            suite.casadi.generated_c.as_path(),
        ),
        artifact_bundle::Source::new(
            "generated/casadi/casadi_exp_mixed.h",
            suite.casadi.generated_h.as_path(),
        ),
        artifact_bundle::Source::new("linked/rumoca.elf", &suite.rumoca_link.elf),
        artifact_bundle::Source::new("linked/rumoca.symbols.txt", &suite.rumoca_link.symbols),
        artifact_bundle::Source::new("linked/casadi.elf", &suite.casadi_link.elf),
        artifact_bundle::Source::new("linked/casadi.symbols.txt", &suite.casadi_link.symbols),
        artifact_bundle::Source::new("traces/rumoca.trace", &suite.rumoca_trace.trace_log),
        artifact_bundle::Source::new("traces/casadi.trace", &suite.casadi_trace.trace_log),
    ]);
    artifact_bundle::capture(&suite.artifact_root, &suite.bundle_destination, sources)
}

#[cfg(test)]
fn sha256_file(path: &Path) -> Result<String> {
    let mut file = fs::File::open(path)
        .with_context(|| format!("failed to open {} for hashing", path.display()))?;
    let mut digest = Sha256::new();
    let mut buffer = [0_u8; 64 * 1024];
    loop {
        let count = file
            .read(&mut buffer)
            .with_context(|| format!("failed to hash {}", path.display()))?;
        if count == 0 {
            break;
        }
        digest.update(&buffer[..count]);
    }
    Ok(format!("{:x}", digest.finalize()))
}

#[derive(Serialize)]
struct Verdict {
    id: String,
    evidence_state: manifest::RowEvidenceState,
    elapsed_seconds: f64,
    #[serde(flatten)]
    result: VerdictResult,
}

impl Verdict {
    fn unmeasured(entry: &Entry, failure: SuiteFailure, elapsed_seconds: f64) -> Self {
        Self {
            id: entry.id.clone(),
            evidence_state: entry.evidence_state,
            elapsed_seconds,
            result: VerdictResult::Incomplete {
                partial_suite: failure.partial,
                findings: vec![format!("unmeasured: {:#}", failure.error)],
            },
        }
    }
}

fn judge(entry: &Entry, evidence: Evidence, elapsed_seconds: f64) -> Verdict {
    let Some(measured) = entry.measured.accepted() else {
        return Verdict {
            id: entry.id.clone(),
            evidence_state: entry.evidence_state,
            elapsed_seconds,
            result: VerdictResult::Rejected {
                evidence,
                findings: vec![format!(
                    "{}: {} has no accepted baseline and cannot expose a delta or outcome",
                    entry.id,
                    entry.evidence_state.label()
                )],
            },
        };
    };
    let Some(max_delta) = entry.max_delta else {
        return Verdict {
            id: entry.id.clone(),
            evidence_state: entry.evidence_state,
            elapsed_seconds,
            result: VerdictResult::Rejected {
                evidence,
                findings: vec![format!(
                    "{}: accepted measurement without an accepted delta is invalid",
                    entry.id
                )],
            },
        };
    };
    let wide =
        i128::from(evidence.rumoca_instructions) - i128::from(evidence.comparator_instructions);
    let delta = i64::try_from(wide).ok();
    let rumoca_stable = evidence.rumoca_instructions == measured.rumoca;
    let comparator_stable = evidence.comparator_instructions == measured.comparator;
    let measured_calls_nonempty =
        evidence.rumoca_instructions > 0 && evidence.comparator_instructions > 0;
    let mut findings = Vec::new();
    if !rumoca_stable {
        if evidence.rumoca_instructions < measured.rumoca {
            findings.push(format!(
                "unrecorded Rumoca improvement from {} to {}; explicitly promote measured.rumoca and max_delta",
                measured.rumoca, evidence.rumoca_instructions
            ));
        } else {
            findings.push(format!(
                "Rumoca instructions regressed from {} to {}",
                measured.rumoca, evidence.rumoca_instructions
            ));
        }
    }
    if !comparator_stable {
        findings.push(format!(
            "comparator instructions changed from {} to {}; update the pinned comparator row explicitly",
            measured.comparator, evidence.comparator_instructions
        ));
    }
    if !measured_calls_nonempty {
        findings.push("zero-instruction measured call is invalid evidence".into());
    }
    if delta.is_none() {
        findings.push("instruction delta does not fit the signed manifest representation".into());
    }
    let result = match (
        rumoca_stable && comparator_stable && measured_calls_nonempty,
        delta,
    ) {
        (true, Some(delta)) if delta == max_delta => VerdictResult::ExactPin {
            evidence,
            delta,
            comparison_outcome: if delta < entry.match_or_beat_target_delta {
                ComparisonOutcome::Win
            } else if delta == entry.match_or_beat_target_delta {
                ComparisonOutcome::Tie
            } else {
                ComparisonOutcome::Loss
            },
        },
        (true, Some(delta)) => VerdictResult::Rejected {
            evidence,
            findings: vec![format!(
                "measured delta {delta} does not equal accepted delta {max_delta}"
            )],
        },
        _ => VerdictResult::Rejected { evidence, findings },
    };
    Verdict {
        id: entry.id.clone(),
        evidence_state: entry.evidence_state,
        elapsed_seconds,
        result,
    }
}

#[derive(Serialize)]
#[serde(tag = "status", rename_all = "kebab-case")]
enum VerdictResult {
    Incomplete {
        partial_suite: PartialSuiteEvidence,
        findings: Vec<String>,
    },
    Rejected {
        evidence: Evidence,
        findings: Vec<String>,
    },
    ExactPin {
        evidence: Evidence,
        delta: i64,
        comparison_outcome: ComparisonOutcome,
    },
}

impl VerdictResult {
    const fn evidence(&self) -> Option<&Evidence> {
        match self {
            Self::Incomplete { .. } => None,
            Self::Rejected { evidence, .. } | Self::ExactPin { evidence, .. } => Some(evidence),
        }
    }

    fn findings(&self) -> &[String] {
        match self {
            Self::Incomplete { findings, .. } | Self::Rejected { findings, .. } => findings,
            Self::ExactPin { .. } => &[],
        }
    }

    const fn is_rejected(&self) -> bool {
        !matches!(self, Self::ExactPin { .. })
    }
}

#[derive(Clone, Copy, Debug, Serialize, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
enum ComparisonOutcome {
    Loss,
    Tie,
    Win,
}

impl ComparisonOutcome {
    const fn label(self) -> &'static str {
        match self {
            Self::Loss => "loss",
            Self::Tie => "tie",
            Self::Win => "win",
        }
    }
}

#[derive(Serialize)]
struct Summary<'a> {
    schema_version: u32,
    status: SummaryStatus,
    scope: &'a SelectionScope,
    history: &'a manifest::HistoryEvidence,
    manifest: String,
    manifest_sha256: &'a str,
    setup: &'a SetupEvidence,
    measured_comparators: usize,
    rejected_measurements: usize,
    wall_seconds: f64,
    verdicts: &'a [Verdict],
}

#[derive(Debug, Serialize, PartialEq, Eq)]
#[serde(tag = "kind", rename_all = "kebab-case")]
enum SummaryStatus {
    ExactPinNonclaimable {
        label: String,
        rows: Vec<RowReportState>,
    },
    MeasurementFailed {
        rows: Vec<String>,
    },
}

#[derive(Debug, Serialize, PartialEq, Eq)]
struct RowReportState {
    id: String,
    evidence_state: manifest::RowEvidenceState,
    comparison_outcome: ComparisonOutcome,
}

#[derive(Serialize)]
struct SetupEvidence {
    commands: Vec<process::CommandReceipt>,
    versions: ToolVersions,
    tool_sha256: ToolHashes,
    git_tool: GitToolEvidence,
    compiler_source: compiler_source::CompilerSourceEvidence,
    compiler_dependencies: compiler_deps::DependencyEvidence,
    compiler_toolchain: CompilerToolchainEvidence,
    tool_closure: tool_closure::ToolClosureEvidence,
    compiler_build_environment_sha256: String,
    compiler_sha256: String,
    compiler_artifact: artifact_bundle::FileEvidence,
    compiler_runtime_environment_sha256: String,
    suite_implementation_sha256: String,
    suite_implementation_history_sha256: String,
}

#[derive(Serialize)]
struct GitToolEvidence {
    version: String,
    sha256: String,
}

#[derive(Serialize)]
struct SetupFailureSummary<'a> {
    schema_version: u32,
    status: SetupFailureStatus,
    commands: &'a [process::CommandReceipt],
    finding: String,
}

#[derive(Serialize)]
#[serde(tag = "kind", rename_all = "kebab-case")]
enum SetupFailureStatus {
    SetupFailed { phase: SetupPhase },
}

fn publish_setup_failure(root: &Path, ledger: &SetupLedger, error: &anyhow::Error) -> Result<()> {
    publish_summary(
        root,
        &SetupFailureSummary {
            schema_version: manifest::SCHEMA_VERSION,
            status: SetupFailureStatus::SetupFailed {
                phase: ledger.phase,
            },
            commands: &ledger.commands,
            finding: format!("{error:#}"),
        },
    )
}

struct ReportInputs<'a> {
    root: &'a Path,
    manifest: &'a Path,
    manifest_sha256: &'a str,
    history: &'a manifest::AuthenticatedHistory,
    setup: &'a SetupEvidence,
    scope: &'a SelectionScope,
    verdicts: &'a [Verdict],
    wall_seconds: f64,
}

/// Proof that every selected row produced exactly one ordered verdict. The
/// report and summary consume this capability instead of independently
/// rechecking raw vectors.
struct CheckedVerdicts<'a> {
    scope: &'a SelectionScope,
    verdicts: &'a [Verdict],
}

impl<'a> CheckedVerdicts<'a> {
    fn bind(scope: &'a SelectionScope, verdicts: &'a [Verdict]) -> Result<Self> {
        let expected_ids = scope.selected_rows();
        ensure!(
            expected_ids.len() == verdicts.len(),
            "summary scope and verdict cardinality disagree"
        );
        ensure!(
            expected_ids
                .iter()
                .zip(verdicts)
                .all(|(expected, verdict)| expected == &verdict.id),
            "summary scope and verdict identities disagree"
        );
        Ok(Self { scope, verdicts })
    }
}

fn report(inputs: ReportInputs<'_>) -> Result<()> {
    let ReportInputs {
        root,
        manifest,
        manifest_sha256,
        history,
        setup,
        scope,
        verdicts,
        wall_seconds,
    } = inputs;
    let checked = CheckedVerdicts::bind(scope, verdicts)?;
    for evidence in checked
        .verdicts
        .iter()
        .filter_map(|verdict| verdict.result.evidence())
    {
        evidence.artifact_bundle.verify()?;
    }
    for verdict in checked.verdicts {
        match &verdict.result {
            VerdictResult::Incomplete { .. } => println!(
                "  INCOMPLETE {} evidence={}",
                verdict.id,
                verdict.evidence_state.label(),
            ),
            VerdictResult::Rejected { .. } => println!(
                "  REJECTED {} evidence={}",
                verdict.id,
                verdict.evidence_state.label(),
            ),
            VerdictResult::ExactPin {
                delta,
                comparison_outcome,
                ..
            } => println!(
                "  exact-pin {} delta={} outcome={} evidence={}",
                verdict.id,
                delta,
                comparison_outcome.label(),
                verdict.evidence_state.label(),
            ),
        }
    }
    let rejected_measurements = checked
        .verdicts
        .iter()
        .filter(|verdict| verdict.result.is_rejected())
        .count();
    let status = summary_status(&checked);
    let summary = Summary {
        schema_version: manifest::SCHEMA_VERSION,
        status,
        scope: checked.scope,
        history: history.evidence(),
        manifest: manifest.display().to_string(),
        manifest_sha256,
        setup,
        measured_comparators: checked
            .verdicts
            .iter()
            .filter(|verdict| verdict.result.evidence().is_some())
            .count(),
        rejected_measurements,
        wall_seconds,
        verdicts: checked.verdicts,
    };
    publish_summary(root, &summary)?;
    if rejected_measurements == 0 {
        return Ok(());
    }
    let findings = checked
        .verdicts
        .iter()
        .flat_map(|verdict| {
            verdict
                .result
                .findings()
                .iter()
                .map(move |finding| format!("{}: {finding}", verdict.id))
        })
        .collect::<Vec<_>>()
        .join("\n");
    bail!("embedded head-to-head failed:\n{findings}")
}

fn summary_status(checked: &CheckedVerdicts<'_>) -> SummaryStatus {
    let mut rejected = Vec::new();
    let mut rows = Vec::new();
    for verdict in checked.verdicts {
        match &verdict.result {
            VerdictResult::Incomplete { .. } | VerdictResult::Rejected { .. } => {
                rejected.push(verdict.id.clone());
            }
            VerdictResult::ExactPin {
                comparison_outcome, ..
            } => rows.push(RowReportState {
                id: verdict.id.clone(),
                evidence_state: verdict.evidence_state,
                comparison_outcome: *comparison_outcome,
            }),
        }
    }
    if !rejected.is_empty() {
        return SummaryStatus::MeasurementFailed { rows: rejected };
    }
    let label = format!(
        "exact-pin nonclaimable: {}",
        rows.iter()
            .map(|row| format!(
                "{} {} {}",
                row.id,
                row.evidence_state.label(),
                row.comparison_outcome.label()
            ))
            .collect::<Vec<_>>()
            .join(", ")
    );
    SummaryStatus::ExactPinNonclaimable { label, rows }
}

fn publish_summary(root: &Path, summary: &impl Serialize) -> Result<()> {
    let path = root.join(SUMMARY_PATH);
    let temporary = root.join(SUMMARY_TEMP_PATH);
    let bytes = format!("{}\n", serde_json::to_string_pretty(summary)?).into_bytes();
    let mut file = fs::OpenOptions::new()
        .write(true)
        .create_new(true)
        .open(&temporary)
        .with_context(|| format!("failed to create {}", temporary.display()))?;
    file.write_all(&bytes)
        .with_context(|| format!("failed to write {}", temporary.display()))?;
    file.sync_all()
        .with_context(|| format!("failed to sync {}", temporary.display()))?;
    let mut permissions = file.metadata()?.permissions();
    permissions.set_readonly(true);
    file.set_permissions(permissions)?;
    drop(file);
    fs::rename(&temporary, &path).with_context(|| {
        format!(
            "failed to atomically publish {} as {}",
            temporary.display(),
            path.display()
        )
    })?;
    ensure!(
        fs::metadata(&path)?.permissions().readonly() && fs::read(&path)? == bytes,
        "published benchmark summary changed during atomic publication"
    );
    Ok(())
}

struct RunLock {
    _file: fs::File,
}

impl RunLock {
    fn acquire(root: &Path) -> Result<Self> {
        let path = root.join(LOCK_PATH);
        fs::create_dir_all(path.parent().context("lock path has no parent")?)?;
        let file = fs::OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .truncate(false)
            .open(&path)?;
        file.lock()
            .with_context(|| format!("failed to lock {}", path.display()))?;
        Ok(Self { _file: file })
    }
}
