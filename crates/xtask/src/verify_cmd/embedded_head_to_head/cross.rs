//! Symmetric Cortex-M7 compilation and linking for both generated artifacts.

mod plan;

use super::artifact_bundle;
use super::artifact_guard::FrozenArtifactSet;
use super::emit::{CasadiEmission, RumocaEmission};
use super::manifest::{ComparatorOutputPins, CorrectnessCase, CrossProfile, Entry};
use super::process;
use super::qemu;
use super::snapshot::AuthenticatedHarness;
use super::tool_closure::{AuthenticatedToolClosure, ToolClosureEvidence};
use super::typed_path::{
    AuthenticatedExecutable, CasadiEmissionInclude, CasadiRowMajorDriver, ComparatorGenerator,
    CompilerExecutable, FixtureSource, GccExecutable, NmExecutable, OracleInclude,
    PrlimitExecutable, PythonExecutable, QemuExecutable, RolePath, RumocaCacheDirectory,
    RumocaClosedBranchDriver, RumocaClosedBranchModel, RumocaConstantOutputDriver,
    RumocaDeletedCallDriver, RumocaEmissionInclude,
};
use anyhow::{Context, Result, ensure};
use sha2::{Digest, Sha256};
use std::fs;
use std::fs::File;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::sync::Arc;

pub(super) use plan::CounterRecipe;
pub(in crate::verify_cmd::embedded_head_to_head) use plan::{
    CompletedSuite as SuiteCompletionProof, ExecutedSuiteStep, SuiteProgress, SuiteStep,
};

struct ExecutionTools {
    gcc: AuthenticatedExecutable<GccExecutable>,
    nm: AuthenticatedExecutable<NmExecutable>,
    qemu: AuthenticatedExecutable<QemuExecutable>,
    python: AuthenticatedExecutable<PythonExecutable>,
    prlimit: AuthenticatedExecutable<PrlimitExecutable>,
    gcc_version: String,
    nm_version: String,
    qemu_version: String,
    python_version: String,
    prlimit_version: String,
}

impl ExecutionTools {
    fn gcc(&self) -> &AuthenticatedExecutable<GccExecutable> {
        &self.gcc
    }

    fn nm(&self) -> &AuthenticatedExecutable<NmExecutable> {
        &self.nm
    }

    fn qemu(&self) -> &AuthenticatedExecutable<QemuExecutable> {
        &self.qemu
    }

    fn python(&self) -> &AuthenticatedExecutable<PythonExecutable> {
        &self.python
    }

    fn prlimit(&self) -> &AuthenticatedExecutable<PrlimitExecutable> {
        &self.prlimit
    }

    fn gcc_version(&self) -> &str {
        &self.gcc_version
    }

    fn nm_version(&self) -> &str {
        &self.nm_version
    }

    fn qemu_version(&self) -> &str {
        &self.qemu_version
    }

    fn python_version(&self) -> &str {
        &self.python_version
    }

    fn prlimit_version(&self) -> &str {
        &self.prlimit_version
    }
}

pub(super) struct AuthenticatedCompiler {
    path: RolePath<CompilerExecutable>,
    sha256: String,
    artifact: artifact_bundle::FileEvidence,
    runtime_environment: process::CompilerRuntimeEnvironment,
}

#[cfg(test)]
impl AuthenticatedCompiler {
    pub(super) fn path(&self) -> &RolePath<CompilerExecutable> {
        &self.path
    }

    pub(super) fn sha256(&self) -> &str {
        &self.sha256
    }
}

pub(super) struct AuthenticatedExecutionRoot {
    tools: ExecutionTools,
    compiler: AuthenticatedCompiler,
    comparator_outputs: ComparatorOutputPins,
    tool_closure: AuthenticatedToolClosure,
}

impl AuthenticatedExecutionRoot {
    pub(super) fn compiler_sha256(&self) -> &str {
        &self.compiler.sha256
    }

    pub(super) fn compiler_artifact(&self) -> &artifact_bundle::FileEvidence {
        &self.compiler.artifact
    }

    pub(super) fn compiler_runtime_environment_sha256(&self) -> String {
        self.compiler.runtime_environment.sha256()
    }

    pub(super) fn tool_closure_evidence(&self) -> &ToolClosureEvidence {
        self.tool_closure.evidence()
    }

    #[cfg(test)]
    pub(super) fn compiler_path(&self) -> &RolePath<CompilerExecutable> {
        &self.compiler.path
    }
}

pub(super) struct BoundExecutionPlan {
    root: Arc<AuthenticatedExecutionRoot>,
    profile: CrossProfile,
    generation: plan::BoundGenerationPlan,
    measurement: plan::BoundMeasurementPlan,
    suite: plan::BoundSuitePlan,
}

impl BoundExecutionPlan {
    fn profile(&self) -> CrossProfile {
        self.profile
    }

    fn tools(&self) -> &ExecutionTools {
        &self.root.tools
    }

    fn compiler_for_execution(&self) -> Result<&RolePath<CompilerExecutable>> {
        self.root.compiler.verify_for_execution()
    }

    #[cfg(test)]
    pub(super) fn metric_is_test_alternate(&self) -> bool {
        self.measurement == plan::BoundMeasurementPlan::TestAlternate
    }
}

pub(super) fn verify_compiler(plan: &BoundExecutionPlan) -> Result<()> {
    plan.compiler_for_execution().map(|_| ())
}

impl AuthenticatedCompiler {
    fn verify_for_execution(&self) -> Result<&RolePath<CompilerExecutable>> {
        let mut file = File::open(self.path.as_path()).with_context(|| {
            format!(
                "failed to open staged compiler at execution boundary {}",
                self.path.as_path().display()
            )
        })?;
        ensure!(
            file.metadata()?.is_file(),
            "staged compiler is not a regular file: {}",
            self.path.as_path().display()
        );
        let mut digest = Sha256::new();
        let mut buffer = [0_u8; 64 * 1024];
        loop {
            let count = file.read(&mut buffer)?;
            if count == 0 {
                break;
            }
            digest.update(&buffer[..count]);
        }
        let observed = format!("{:x}", digest.finalize());
        ensure!(
            observed == self.sha256,
            "staged compiler digest changed before execution: observed {observed}, expected {}",
            self.sha256
        );
        Ok(&self.path)
    }
}

pub(super) struct GuestInstructionAuthorization<'a> {
    plan: &'a BoundExecutionPlan,
}

impl GuestInstructionAuthorization<'_> {
    pub(super) fn plan(&self) -> &BoundExecutionPlan {
        self.plan
    }
}

pub(super) struct ExecutionRootInputs {
    pub(super) gcc: AuthenticatedExecutable<GccExecutable>,
    pub(super) nm: AuthenticatedExecutable<NmExecutable>,
    pub(super) qemu: AuthenticatedExecutable<QemuExecutable>,
    pub(super) python: AuthenticatedExecutable<PythonExecutable>,
    pub(super) prlimit: AuthenticatedExecutable<PrlimitExecutable>,
    pub(super) gcc_version: String,
    pub(super) nm_version: String,
    pub(super) qemu_version: String,
    pub(super) python_version: String,
    pub(super) prlimit_version: String,
    pub(super) compiler: AuthenticatedCompiler,
    pub(super) comparator_outputs: ComparatorOutputPins,
    pub(super) tool_closure: AuthenticatedToolClosure,
}

pub(super) fn authenticate_execution_root(
    inputs: ExecutionRootInputs,
) -> Result<Arc<AuthenticatedExecutionRoot>> {
    let ExecutionRootInputs {
        gcc,
        nm,
        qemu,
        python,
        prlimit,
        gcc_version,
        nm_version,
        qemu_version,
        python_version,
        prlimit_version,
        compiler,
        comparator_outputs,
        tool_closure,
    } = inputs;
    for path in [
        gcc.canonical_target(),
        nm.canonical_target(),
        qemu.canonical_target(),
        python.canonical_target(),
        prlimit.canonical_target(),
    ] {
        tool_closure.require_member(path)?;
    }
    plan::authenticate_execution_root(
        ExecutionTools {
            gcc,
            nm,
            qemu,
            python,
            prlimit,
            gcc_version,
            nm_version,
            qemu_version,
            python_version,
            prlimit_version,
        },
        compiler,
        comparator_outputs,
        tool_closure,
    )
}

pub(super) fn bind_execution_plan(
    root: &Arc<AuthenticatedExecutionRoot>,
    entry: &Entry,
) -> Result<BoundExecutionPlan> {
    plan::bind_execution_plan(root, entry)
}

pub(super) fn start_suite(plan: &BoundExecutionPlan) -> plan::SuiteCursor {
    plan::start_suite(plan)
}

pub(super) fn execute_suite_step<T>(
    pending: plan::PendingSuiteStep,
    operation: impl FnOnce(SuiteStep) -> Result<T>,
) -> Result<(plan::SuiteCursor, ExecutedSuiteStep, T)> {
    plan::execute_suite_step(pending, operation)
}

pub(super) fn validate_casadi_outputs(
    plan: &BoundExecutionPlan,
    generated_c: &Path,
    generated_h: &Path,
) -> Result<()> {
    for (label, path, expected) in [
        (
            "CasADi generated C",
            generated_c,
            plan.root.comparator_outputs.casadi_c_sha256.as_str(),
        ),
        (
            "CasADi generated header",
            generated_h,
            plan.root.comparator_outputs.casadi_h_sha256.as_str(),
        ),
    ] {
        let bytes = fs::read(path).with_context(|| format!("failed to read {}", path.display()))?;
        let actual = format!("{:x}", Sha256::digest(bytes));
        ensure!(
            actual == expected,
            "{label} SHA-256 {actual} does not match reviewed pin {expected}"
        );
    }
    Ok(())
}

pub(super) fn measure_guest_instructions(
    plan: &BoundExecutionPlan,
    artifact: &LinkedArtifact,
    name: &str,
    expected_cases: &[CorrectnessCase],
    trace_log: &Path,
) -> Result<qemu::TraceMeasurement> {
    match plan.measurement {
        plan::BoundMeasurementPlan::GuestInstructions => qemu::measure(
            &GuestInstructionAuthorization { plan },
            artifact,
            name,
            expected_cases,
            trace_log,
        ),
        #[cfg(test)]
        plan::BoundMeasurementPlan::TestAlternate => {
            anyhow::bail!("test alternate metric has no measurement implementation")
        }
    }
}

pub(super) fn stage_compiler(
    compiler: &Path,
    artifact_dir: &Path,
    tool_closure: &AuthenticatedToolClosure,
) -> Result<AuthenticatedCompiler> {
    ensure!(
        artifact_dir.is_absolute(),
        "compiler artifact directory must be absolute"
    );
    let staged_dir = artifact_dir.join("evidence-bundle/setup/compiler");
    fs::create_dir_all(&staged_dir).with_context(|| {
        format!(
            "failed to create staged compiler directory {}",
            staged_dir.display()
        )
    })?;
    let staged = staged_dir.join("rumoca");
    let mut source = File::open(compiler)
        .with_context(|| format!("failed to open built compiler {}", compiler.display()))?;
    let metadata = source
        .metadata()
        .with_context(|| format!("failed to stat built compiler {}", compiler.display()))?;
    ensure!(
        metadata.is_file(),
        "built compiler is not a regular file: {}",
        compiler.display()
    );
    let mut bytes = Vec::new();
    source
        .read_to_end(&mut bytes)
        .with_context(|| format!("failed to capture built compiler {}", compiler.display()))?;
    let sha256 = format!("{:x}", Sha256::digest(&bytes));
    fs::write(&staged, &bytes).with_context(|| {
        format!(
            "failed to write isolated compiler bytes to {}",
            staged.display()
        )
    })?;
    let mut permissions = metadata.permissions();
    permissions.set_readonly(true);
    fs::set_permissions(&staged, permissions)
        .with_context(|| format!("failed to freeze staged compiler {}", staged.display()))?;
    Ok(AuthenticatedCompiler {
        path: RolePath::checked(staged)?,
        artifact: artifact_bundle::FileEvidence {
            relative_path: "evidence-bundle/setup/compiler/rumoca".to_string(),
            sha256: sha256.clone(),
            bytes: bytes.len() as u64,
        },
        sha256,
        runtime_environment: tool_closure.compiler_runtime_environment()?,
    })
}

pub(super) struct LinkedArtifact {
    pub(super) elf: PathBuf,
    pub(super) symbols: PathBuf,
    pub(super) commands: Vec<process::CommandReceipt>,
    frozen: FrozenArtifactSet,
    execution: plan::BoundExecutionArtifact,
}

impl LinkedArtifact {
    pub(super) fn verify(&self) -> Result<()> {
        self.frozen.verify()
    }
    pub(super) fn trace_log(&self, directory: &Path) -> PathBuf {
        self.execution.trace_log(directory)
    }
}

pub(super) struct BuildContext<'a> {
    pub(super) plan: &'a BoundExecutionPlan,
    pub(super) harness: &'a AuthenticatedHarness,
    pub(super) oracle: &'a RolePath<OracleInclude>,
    pub(super) oracle_guard: &'a FrozenArtifactSet,
}

pub(super) fn build_rumoca(
    emission: &RumocaEmission,
    work: &Path,
    context: &BuildContext<'_>,
) -> Result<LinkedArtifact> {
    plan::rumoca_measured(&plan_context(context, work), emission)
}

pub(super) fn build_rumoca_correctness(
    emission: &RumocaEmission,
    work: &Path,
    context: &BuildContext<'_>,
) -> Result<LinkedArtifact> {
    plan::rumoca_correctness(&plan_context(context, work), emission)
}

pub(super) fn build_rumoca_deleted_call(
    emission: &RumocaEmission,
    driver: &RolePath<RumocaDeletedCallDriver>,
    mutation_guard: &FrozenArtifactSet,
    work: &Path,
    context: &BuildContext<'_>,
) -> Result<LinkedArtifact> {
    plan::rumoca_deleted_call(
        &plan_context(context, work),
        emission,
        driver,
        mutation_guard,
    )
}

pub(super) fn build_rumoca_constant_output(
    emission: &RumocaEmission,
    driver: &RolePath<RumocaConstantOutputDriver>,
    mutation_guard: &FrozenArtifactSet,
    work: &Path,
    context: &BuildContext<'_>,
) -> Result<LinkedArtifact> {
    plan::rumoca_constant_output(
        &plan_context(context, work),
        emission,
        driver,
        mutation_guard,
    )
}

pub(super) fn build_rumoca_closed_branches(
    emission: &RumocaEmission,
    driver: &RolePath<RumocaClosedBranchDriver>,
    model: &RolePath<RumocaClosedBranchModel>,
    mutation_guard: &FrozenArtifactSet,
    work: &Path,
    context: &BuildContext<'_>,
) -> Result<LinkedArtifact> {
    plan::rumoca_closed_branches(
        &plan_context(context, work),
        emission,
        driver,
        model,
        mutation_guard,
    )
}

pub(super) fn build_casadi(
    emission: &CasadiEmission,
    work: &Path,
    context: &BuildContext<'_>,
) -> Result<LinkedArtifact> {
    plan::casadi_measured(&plan_context(context, work), emission)
}

pub(super) fn build_casadi_correctness(
    emission: &CasadiEmission,
    work: &Path,
    context: &BuildContext<'_>,
) -> Result<LinkedArtifact> {
    plan::casadi_correctness(&plan_context(context, work), emission)
}

pub(super) fn build_casadi_row_major(
    emission: &CasadiEmission,
    driver: &RolePath<CasadiRowMajorDriver>,
    mutation_guard: &FrozenArtifactSet,
    work: &Path,
    context: &BuildContext<'_>,
) -> Result<LinkedArtifact> {
    plan::casadi_row_major(
        &plan_context(context, work),
        emission,
        driver,
        mutation_guard,
    )
}

pub(super) fn build_fixture(
    recipe: CounterRecipe,
    work: &Path,
    context: &BuildContext<'_>,
) -> Result<LinkedArtifact> {
    plan::counter(&plan_context(context, work), recipe)
}

#[cfg(test)]
pub(super) fn casadi_measured_bound_paths(
    emission: &CasadiEmission,
    work: &Path,
    context: &BuildContext<'_>,
) -> Result<Vec<PathBuf>> {
    plan::casadi_measured_bound_paths(&plan_context(context, work), emission)
}

#[cfg(test)]
pub(super) fn normalized_profile_sha256(entry: &Entry) -> String {
    plan::normalized_profile_sha256(entry)
}

#[cfg(test)]
pub(super) fn expected_gcc_version(profile: CrossProfile) -> &'static str {
    plan::expected_gcc_version(profile)
}

#[cfg(test)]
pub(super) fn expected_nm_version(profile: CrossProfile) -> &'static str {
    plan::expected_nm_version(profile)
}

#[cfg(test)]
pub(super) fn expected_qemu_version(profile: CrossProfile) -> &'static str {
    plan::expected_qemu_version(profile)
}

#[cfg(test)]
pub(super) fn expected_python_version(profile: CrossProfile) -> &'static str {
    plan::expected_python_version(profile)
}

#[cfg(test)]
pub(super) fn expected_prlimit_version(profile: CrossProfile) -> &'static str {
    plan::expected_prlimit_version(profile)
}

pub(super) fn gcc_version_command(tool: &RolePath<GccExecutable>) -> process::HermeticCommand {
    plan::gcc_version_command(tool)
}

pub(super) fn nm_version_command(tool: &RolePath<NmExecutable>) -> process::HermeticCommand {
    plan::nm_version_command(tool)
}

pub(super) fn qemu_version_command(tool: &RolePath<QemuExecutable>) -> process::HermeticCommand {
    plan::qemu_version_command(tool)
}

pub(super) fn python_version_command(
    tool: &RolePath<PythonExecutable>,
) -> process::HermeticCommand {
    plan::python_version_command(tool)
}

pub(super) fn prlimit_version_command(
    tool: &RolePath<PrlimitExecutable>,
) -> process::HermeticCommand {
    plan::prlimit_version_command(tool)
}

pub(super) fn rumoca_generation_command(
    plan: &BoundExecutionPlan,
    fixture: &RolePath<FixtureSource>,
    cache: &RolePath<RumocaCacheDirectory>,
    output: &RolePath<RumocaEmissionInclude>,
) -> Result<process::HermeticCommand> {
    plan::rumoca_generation_command(plan, fixture, cache, output)
}

pub(super) fn casadi_generation_command(
    plan: &BoundExecutionPlan,
    generator: &RolePath<ComparatorGenerator>,
    output: &RolePath<CasadiEmissionInclude>,
) -> Result<process::HermeticCommand> {
    plan::casadi_generation_command(plan, generator, output)
}

pub(super) fn qemu_command(
    plan: &BoundExecutionPlan,
    artifact: &LinkedArtifact,
    trace_log: &Path,
) -> Result<process::HermeticCommand> {
    plan::qemu_command(plan, artifact, trace_log)
}

fn plan_context<'a>(context: &'a BuildContext<'a>, work: &'a Path) -> plan::Context<'a> {
    plan::Context {
        plan: context.plan,
        harness: context.harness,
        oracle: context.oracle,
        oracle_guard: context.oracle_guard,
        work,
    }
}
