//! Immutable, role-bound cross-build plans.
//!
//! The closed recipe catalog below is the sole owner of source roles,
//! invocation order, flags, tool identities, and output roles. Both the
//! normalized digest and runtime argv are projections of those exact static
//! `RecipeSpec` values. Runtime binding validates every typed path against the
//! catalog role before the first process is executed.

use super::{
    AuthenticatedCompiler, AuthenticatedExecutionRoot, BoundExecutionPlan, ExecutionTools,
    LinkedArtifact,
};
use crate::verify_cmd::embedded_head_to_head::artifact_guard::FrozenArtifactSet;
use crate::verify_cmd::embedded_head_to_head::emit::{CasadiEmission, RumocaEmission};
use crate::verify_cmd::embedded_head_to_head::manifest::{CrossProfile, Entry, Metric};
use crate::verify_cmd::embedded_head_to_head::process;
use crate::verify_cmd::embedded_head_to_head::qemu;
use crate::verify_cmd::embedded_head_to_head::snapshot::AuthenticatedHarness;
use crate::verify_cmd::embedded_head_to_head::tool_closure::AuthenticatedToolClosure;
use crate::verify_cmd::embedded_head_to_head::typed_path::{
    AuthenticatedExecutable, CasadiCorrectnessDriver, CasadiElf, CasadiEmissionInclude,
    CasadiMeasuredDriver, CasadiRowMajorDriver, CasadiSymbols, ComparatorGenerator,
    ComparatorObject, ComparatorWrapper, CounterCalledLeafElf, CounterCalledLeafSymbols,
    CounterExitStatusElf, CounterExitStatusSymbols, CounterObject, CounterSource,
    CounterStraightElf, CounterStraightSymbols, DriverObject, ExecutableRole, FixtureSource,
    GccExecutable, HarnessInclude, KernelsObject, LinkerScript, ModelObject, NmExecutable,
    OracleInclude, PrlimitExecutable, PythonExecutable, QemuExecutable, Role, RolePath,
    RumocaCacheDirectory, RumocaClosedBranchDriver, RumocaClosedBranchModel,
    RumocaConstantOutputDriver, RumocaCorrectnessDriver, RumocaDeletedCallDriver, RumocaElf,
    RumocaEmissionInclude, RumocaKernels, RumocaMeasuredDriver, RumocaModel, RumocaSymbols,
    StartupObject, StartupSource, TraceObject, TraceSource,
};
use anyhow::{Context as _, Result, bail, ensure};
use sha2::{Digest, Sha256};
use std::ffi::OsString;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Arc;

mod generation;
mod suite;

#[cfg(test)]
use crate::verify_cmd::embedded_head_to_head::typed_path::CompilerExecutable;
pub(super) use generation::BoundGenerationPlan;
pub(in crate::verify_cmd::embedded_head_to_head) use suite::{
    BoundSuitePlan, CompletedSuite, ExecutedSuiteStep, PendingSuiteStep, SuiteCursor,
    SuiteProgress, SuiteStep,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(in crate::verify_cmd::embedded_head_to_head) enum CounterRecipe {
    Straight,
    CalledLeaf,
    ExitStatus,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(in crate::verify_cmd::embedded_head_to_head::cross) enum BoundMeasurementPlan {
    GuestInstructions,
    #[cfg(test)]
    TestAlternate,
}

impl BoundMeasurementPlan {
    fn from_manifest(metric: Metric) -> Self {
        match metric {
            Metric::GuestInstructions => Self::GuestInstructions,
            #[cfg(test)]
            Metric::TestAlternate => Self::TestAlternate,
        }
    }

    fn update_digest(self, digest: &mut Sha256) {
        let id = match self {
            Self::GuestInstructions => "guest-instructions-exclusive-markers-v1",
            #[cfg(test)]
            Self::TestAlternate => "test-alternate-metric",
        };
        update_field(digest, "measurement-metric", id);
        update_field(digest, "measurement-rumoca-recipe", RUMOCA_MEASURED_ID);
        update_field(digest, "measurement-comparator-recipe", CASADI_MEASURED_ID);
        update_field(
            digest,
            "measurement-tool-role",
            <QemuExecutable as Role>::ID,
        );
        update_field(digest, "measurement-begin-marker", "trace_begin");
        update_field(digest, "measurement-end-marker", "trace_end");
        update_field(
            digest,
            "measurement-marker-policy",
            "exclusive-single-interval",
        );
    }
}

pub(super) struct Context<'a> {
    pub(super) plan: &'a BoundExecutionPlan,
    pub(super) harness: &'a AuthenticatedHarness,
    pub(super) oracle: &'a RolePath<OracleInclude>,
    pub(super) oracle_guard: &'a FrozenArtifactSet,
    pub(super) work: &'a Path,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum RecipeKind {
    RumocaMeasured,
    RumocaCorrectness,
    RumocaDeletedCall,
    RumocaConstantOutput,
    RumocaClosedBranches,
    CasadiMeasured,
    CasadiCorrectness,
    CasadiRowMajor,
    CounterStraight,
    CounterCalledLeaf,
    CounterExitStatus,
}

impl RecipeKind {
    #[cfg(test)]
    const ALL: [Self; 11] = [
        Self::RumocaMeasured,
        Self::RumocaCorrectness,
        Self::RumocaDeletedCall,
        Self::RumocaConstantOutput,
        Self::RumocaClosedBranches,
        Self::CasadiMeasured,
        Self::CasadiCorrectness,
        Self::CasadiRowMajor,
        Self::CounterStraight,
        Self::CounterCalledLeaf,
        Self::CounterExitStatus,
    ];
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum PathSlot {
    HarnessInclude,
    OracleInclude,
    RumocaEmissionInclude,
    CasadiEmissionInclude,
    LinkerScript,
    StartupSource,
    TraceSource,
    RumocaMeasuredDriver,
    RumocaCorrectnessDriver,
    RumocaDeletedCallDriver,
    RumocaConstantOutputDriver,
    RumocaClosedBranchDriver,
    RumocaModel,
    RumocaClosedBranchModel,
    RumocaKernels,
    CasadiMeasuredDriver,
    CasadiCorrectnessDriver,
    CasadiRowMajorDriver,
    ComparatorWrapper,
    CounterSource,
}

impl PathSlot {
    const fn identity(self) -> &'static str {
        match self {
            Self::HarnessInclude => <HarnessInclude as Role>::ID,
            Self::OracleInclude => <OracleInclude as Role>::ID,
            Self::RumocaEmissionInclude => <RumocaEmissionInclude as Role>::ID,
            Self::CasadiEmissionInclude => <CasadiEmissionInclude as Role>::ID,
            Self::LinkerScript => <LinkerScript as Role>::ID,
            Self::StartupSource => <StartupSource as Role>::ID,
            Self::TraceSource => <TraceSource as Role>::ID,
            Self::RumocaMeasuredDriver => <RumocaMeasuredDriver as Role>::ID,
            Self::RumocaCorrectnessDriver => <RumocaCorrectnessDriver as Role>::ID,
            Self::RumocaDeletedCallDriver => <RumocaDeletedCallDriver as Role>::ID,
            Self::RumocaConstantOutputDriver => <RumocaConstantOutputDriver as Role>::ID,
            Self::RumocaClosedBranchDriver => <RumocaClosedBranchDriver as Role>::ID,
            Self::RumocaModel => <RumocaModel as Role>::ID,
            Self::RumocaClosedBranchModel => <RumocaClosedBranchModel as Role>::ID,
            Self::RumocaKernels => <RumocaKernels as Role>::ID,
            Self::CasadiMeasuredDriver => <CasadiMeasuredDriver as Role>::ID,
            Self::CasadiCorrectnessDriver => <CasadiCorrectnessDriver as Role>::ID,
            Self::CasadiRowMajorDriver => <CasadiRowMajorDriver as Role>::ID,
            Self::ComparatorWrapper => <ComparatorWrapper as Role>::ID,
            Self::CounterSource => <CounterSource as Role>::ID,
        }
    }
}

#[derive(Clone, Copy)]
enum ObjectSlot {
    Startup,
    Trace,
    Driver,
    Model,
    Kernels,
    Comparator,
    Counter,
}

impl ObjectSlot {
    const fn identity(self) -> &'static str {
        match self {
            Self::Startup => <StartupObject as Role>::ID,
            Self::Trace => <TraceObject as Role>::ID,
            Self::Driver => <DriverObject as Role>::ID,
            Self::Model => <ModelObject as Role>::ID,
            Self::Kernels => <KernelsObject as Role>::ID,
            Self::Comparator => <ComparatorObject as Role>::ID,
            Self::Counter => <CounterObject as Role>::ID,
        }
    }

    const fn file_name(self) -> &'static str {
        match self {
            Self::Startup => "startup.o",
            Self::Trace => "trace_output.o",
            Self::Driver => "driver.o",
            Self::Model => "ExpMixedStep.o",
            Self::Kernels => "rumoca_galec_kernels.o",
            Self::Comparator => "casadi_exp_mixed.o",
            Self::Counter => "fixture.o",
        }
    }
}

#[derive(Clone, Copy)]
struct SourceSpec {
    input: PathSlot,
    output: ObjectSlot,
    is_c: bool,
}

#[derive(Clone, Copy)]
enum ArtifactKind {
    Rumoca,
    Casadi,
    CounterStraight,
    CounterCalledLeaf,
    CounterExitStatus,
}

impl ArtifactKind {
    const fn elf_identity(self) -> &'static str {
        match self {
            Self::Rumoca => <RumocaElf as Role>::ID,
            Self::Casadi => <CasadiElf as Role>::ID,
            Self::CounterStraight => <CounterStraightElf as Role>::ID,
            Self::CounterCalledLeaf => <CounterCalledLeafElf as Role>::ID,
            Self::CounterExitStatus => <CounterExitStatusElf as Role>::ID,
        }
    }

    const fn symbol_identity(self) -> &'static str {
        match self {
            Self::Rumoca => <RumocaSymbols as Role>::ID,
            Self::Casadi => <CasadiSymbols as Role>::ID,
            Self::CounterStraight => <CounterStraightSymbols as Role>::ID,
            Self::CounterCalledLeaf => <CounterCalledLeafSymbols as Role>::ID,
            Self::CounterExitStatus => <CounterExitStatusSymbols as Role>::ID,
        }
    }

    const fn stem(self) -> &'static str {
        match self {
            Self::Rumoca => "rumoca_exp_mixed",
            Self::Casadi => "casadi_exp_mixed",
            Self::CounterStraight => "counter-straight",
            Self::CounterCalledLeaf => "counter-called-leaf",
            Self::CounterExitStatus => "counter-exit-status",
        }
    }
}

struct RecipeSpec {
    kind: RecipeKind,
    id: &'static str,
    sources: &'static [SourceSpec],
    includes: &'static [PathSlot],
    definitions: &'static [&'static str],
    artifact: ArtifactKind,
    trace_file: &'static str,
}

#[derive(Clone, Copy)]
enum ToolSlot {
    Gcc,
    Nm,
}

#[derive(Clone)]
enum ArgSpec {
    Literal(&'static str),
    Input(PathSlot),
    Output(ObjectSlot),
    Elf,
}

#[derive(Clone, Copy)]
enum CaptureSpec {
    Status,
    Symbols,
}

struct InvocationSpec {
    id: &'static str,
    tool: ToolSlot,
    args: Vec<ArgSpec>,
    capture: CaptureSpec,
}

impl RecipeSpec {
    fn invocations(&self, profile: &Profile<'_>) -> Vec<InvocationSpec> {
        let mut invocations = self
            .sources
            .iter()
            .map(|source| self.compile_invocation(profile, source))
            .collect::<Vec<_>>();
        invocations.push(self.link_invocation(profile));
        invocations.push(InvocationSpec {
            id: "Cortex-M7 symbol reader",
            tool: ToolSlot::Nm,
            args: vec![
                ArgSpec::Literal("-n"),
                ArgSpec::Literal("-S"),
                ArgSpec::Literal("--defined-only"),
                ArgSpec::Elf,
            ],
            capture: CaptureSpec::Symbols,
        });
        invocations
    }

    fn compile_invocation(&self, profile: &Profile<'_>, source: &SourceSpec) -> InvocationSpec {
        let mut args = literal_specs(if source.is_c {
            profile.c_and_lto_flags
        } else {
            profile.assembly_flags
        });
        if source.is_c {
            args.extend(literal_specs(profile.warnings));
        }
        for include in self.includes {
            args.extend([ArgSpec::Literal("-I"), ArgSpec::Input(*include)]);
        }
        args.extend(self.definitions.iter().map(|value| ArgSpec::Literal(value)));
        args.extend([
            ArgSpec::Literal("-c"),
            ArgSpec::Input(source.input),
            ArgSpec::Literal("-o"),
            ArgSpec::Output(source.output),
        ]);
        InvocationSpec {
            id: "Cortex-M7 compilation",
            tool: ToolSlot::Gcc,
            args,
            capture: CaptureSpec::Status,
        }
    }

    fn link_invocation(&self, profile: &Profile<'_>) -> InvocationSpec {
        let mut args = literal_specs(profile.c_and_lto_flags);
        args.extend(literal_specs(profile.link_options));
        args.extend([
            ArgSpec::Literal("-T"),
            ArgSpec::Input(PathSlot::LinkerScript),
        ]);
        args.extend(
            self.sources
                .iter()
                .map(|source| ArgSpec::Output(source.output)),
        );
        args.extend([ArgSpec::Literal("-o"), ArgSpec::Elf]);
        args.extend(literal_specs(profile.link_libraries));
        InvocationSpec {
            id: "Cortex-M7 link",
            tool: ToolSlot::Gcc,
            args,
            capture: CaptureSpec::Status,
        }
    }
}

fn literal_specs(values: &'static [&'static str]) -> Vec<ArgSpec> {
    values.iter().map(|value| ArgSpec::Literal(value)).collect()
}

const RUMOCA_INCLUDES: &[PathSlot] = &[
    PathSlot::HarnessInclude,
    PathSlot::OracleInclude,
    PathSlot::RumocaEmissionInclude,
];
const RUMOCA_MEASURED_ID: &str = "rumoca-measured";
const CASADI_MEASURED_ID: &str = "casadi-measured";
const CASADI_INCLUDES: &[PathSlot] = &[
    PathSlot::HarnessInclude,
    PathSlot::OracleInclude,
    PathSlot::CasadiEmissionInclude,
];
const COUNTER_INCLUDES: &[PathSlot] = &[PathSlot::HarnessInclude];

const RUMOCA_MEASURED_SOURCES: &[SourceSpec] =
    &rumoca_sources(PathSlot::RumocaMeasuredDriver, PathSlot::RumocaModel);
const RUMOCA_CORRECTNESS_SOURCES: &[SourceSpec] =
    &rumoca_sources(PathSlot::RumocaCorrectnessDriver, PathSlot::RumocaModel);
const RUMOCA_DELETED_CALL_SOURCES: &[SourceSpec] =
    &rumoca_sources(PathSlot::RumocaDeletedCallDriver, PathSlot::RumocaModel);
const RUMOCA_CONSTANT_OUTPUT_SOURCES: &[SourceSpec] =
    &rumoca_sources(PathSlot::RumocaConstantOutputDriver, PathSlot::RumocaModel);
const RUMOCA_CLOSED_BRANCH_SOURCES: &[SourceSpec] = &rumoca_sources(
    PathSlot::RumocaClosedBranchDriver,
    PathSlot::RumocaClosedBranchModel,
);
const CASADI_MEASURED_SOURCES: &[SourceSpec] = &casadi_sources(PathSlot::CasadiMeasuredDriver);
const CASADI_CORRECTNESS_SOURCES: &[SourceSpec] =
    &casadi_sources(PathSlot::CasadiCorrectnessDriver);
const CASADI_ROW_MAJOR_SOURCES: &[SourceSpec] = &casadi_sources(PathSlot::CasadiRowMajorDriver);
const COUNTER_SOURCES: &[SourceSpec] = &[
    SourceSpec {
        input: PathSlot::StartupSource,
        output: ObjectSlot::Startup,
        is_c: false,
    },
    SourceSpec {
        input: PathSlot::CounterSource,
        output: ObjectSlot::Counter,
        is_c: true,
    },
];

const fn rumoca_sources(driver: PathSlot, model: PathSlot) -> [SourceSpec; 5] {
    [
        SourceSpec {
            input: PathSlot::StartupSource,
            output: ObjectSlot::Startup,
            is_c: false,
        },
        SourceSpec {
            input: PathSlot::TraceSource,
            output: ObjectSlot::Trace,
            is_c: true,
        },
        SourceSpec {
            input: driver,
            output: ObjectSlot::Driver,
            is_c: true,
        },
        SourceSpec {
            input: model,
            output: ObjectSlot::Model,
            is_c: true,
        },
        SourceSpec {
            input: PathSlot::RumocaKernels,
            output: ObjectSlot::Kernels,
            is_c: true,
        },
    ]
}

const fn casadi_sources(driver: PathSlot) -> [SourceSpec; 4] {
    [
        SourceSpec {
            input: PathSlot::StartupSource,
            output: ObjectSlot::Startup,
            is_c: false,
        },
        SourceSpec {
            input: PathSlot::TraceSource,
            output: ObjectSlot::Trace,
            is_c: true,
        },
        SourceSpec {
            input: driver,
            output: ObjectSlot::Driver,
            is_c: true,
        },
        SourceSpec {
            input: PathSlot::ComparatorWrapper,
            output: ObjectSlot::Comparator,
            is_c: true,
        },
    ]
}

const RECIPES: &[RecipeSpec] = &[
    RecipeSpec {
        kind: RecipeKind::RumocaMeasured,
        id: RUMOCA_MEASURED_ID,
        sources: RUMOCA_MEASURED_SOURCES,
        includes: RUMOCA_INCLUDES,
        definitions: &[],
        artifact: ArtifactKind::Rumoca,
        trace_file: "rumoca.trace",
    },
    RecipeSpec {
        kind: RecipeKind::RumocaCorrectness,
        id: "rumoca-correctness",
        sources: RUMOCA_CORRECTNESS_SOURCES,
        includes: RUMOCA_INCLUDES,
        definitions: &[],
        artifact: ArtifactKind::Rumoca,
        trace_file: "correctness-rumoca.trace",
    },
    RecipeSpec {
        kind: RecipeKind::RumocaDeletedCall,
        id: "rumoca-mutation-deleted-call",
        sources: RUMOCA_DELETED_CALL_SOURCES,
        includes: RUMOCA_INCLUDES,
        definitions: &[],
        artifact: ArtifactKind::Rumoca,
        trace_file: "mutation-deleted-call.trace",
    },
    RecipeSpec {
        kind: RecipeKind::RumocaConstantOutput,
        id: "rumoca-mutation-constant-output",
        sources: RUMOCA_CONSTANT_OUTPUT_SOURCES,
        includes: RUMOCA_INCLUDES,
        definitions: &[],
        artifact: ArtifactKind::Rumoca,
        trace_file: "mutation-constant-output.trace",
    },
    RecipeSpec {
        kind: RecipeKind::RumocaClosedBranches,
        id: "rumoca-mutation-closed-branches",
        sources: RUMOCA_CLOSED_BRANCH_SOURCES,
        includes: RUMOCA_INCLUDES,
        definitions: &[],
        artifact: ArtifactKind::Rumoca,
        trace_file: "mutation-closed-branches.trace",
    },
    RecipeSpec {
        kind: RecipeKind::CasadiMeasured,
        id: CASADI_MEASURED_ID,
        sources: CASADI_MEASURED_SOURCES,
        includes: CASADI_INCLUDES,
        definitions: &[],
        artifact: ArtifactKind::Casadi,
        trace_file: "casadi.trace",
    },
    RecipeSpec {
        kind: RecipeKind::CasadiCorrectness,
        id: "casadi-correctness",
        sources: CASADI_CORRECTNESS_SOURCES,
        includes: CASADI_INCLUDES,
        definitions: &[],
        artifact: ArtifactKind::Casadi,
        trace_file: "correctness-casadi.trace",
    },
    RecipeSpec {
        kind: RecipeKind::CasadiRowMajor,
        id: "casadi-mutation-row-major",
        sources: CASADI_ROW_MAJOR_SOURCES,
        includes: CASADI_INCLUDES,
        definitions: &[],
        artifact: ArtifactKind::Casadi,
        trace_file: "mutation-row-major.trace",
    },
    RecipeSpec {
        kind: RecipeKind::CounterStraight,
        id: "counter-straight",
        sources: COUNTER_SOURCES,
        includes: COUNTER_INCLUDES,
        definitions: &["-DFIXTURE_STRAIGHT"],
        artifact: ArtifactKind::CounterStraight,
        trace_file: "acceptance-straight.trace",
    },
    RecipeSpec {
        kind: RecipeKind::CounterCalledLeaf,
        id: "counter-called-leaf",
        sources: COUNTER_SOURCES,
        includes: COUNTER_INCLUDES,
        definitions: &["-DFIXTURE_CALLED_LEAF"],
        artifact: ArtifactKind::CounterCalledLeaf,
        trace_file: "acceptance-called-leaf.trace",
    },
    RecipeSpec {
        kind: RecipeKind::CounterExitStatus,
        id: "counter-exit-status",
        sources: COUNTER_SOURCES,
        includes: COUNTER_INCLUDES,
        definitions: &["-DFIXTURE_STRAIGHT", "-DFIXTURE_RETURN_FAILURE"],
        artifact: ArtifactKind::CounterExitStatus,
        trace_file: "acceptance-exit-status.trace",
    },
];

struct Profile<'a> {
    id: &'static str,
    gcc_version: &'a str,
    nm_version: &'a str,
    qemu_version: &'a str,
    python_version: &'a str,
    c_and_lto_flags: &'static [&'static str],
    warnings: &'static [&'static str],
    assembly_flags: &'static [&'static str],
    link_options: &'static [&'static str],
    link_libraries: &'static [&'static str],
}

const C_AND_LTO_FLAGS: &[&str] = &[
    "-O3",
    "-flto",
    "-std=c99",
    "-mcpu=cortex-m7",
    "-mfpu=fpv5-d16",
    "-mfloat-abi=hard",
    "-ffunction-sections",
    "-fno-math-errno",
    "-ffp-contract=off",
    "-g",
    "-fstack-usage",
    "--param=max-inline-insns-auto=700",
    "--param=max-inline-insns-single=700",
];
const WARNINGS: &[&str] = &["-Wall", "-Wextra", "-Werror", "-Wno-unused-parameter"];
const ASSEMBLY_FLAGS: &[&str] = &[
    "-O3",
    "-flto",
    "-mcpu=cortex-m7",
    "-mfpu=fpv5-d16",
    "-mfloat-abi=hard",
    "-ffunction-sections",
    "-g",
    "--param=max-inline-insns-auto=700",
    "--param=max-inline-insns-single=700",
];
const LINK_OPTIONS: &[&str] = &["-nostartfiles", "-Wl,--gc-sections"];
const LINK_LIBRARIES: &[&str] = &["-lm", "-lc", "-lgcc"];
const GCC_VERSION: &str =
    "arm-none-eabi-gcc (Arm GNU Toolchain 15.2.Rel1 (Build arm-15.86)) 15.2.1 20251203";
const NM_VERSION: &str = "GNU nm (Arm GNU Toolchain 15.2.Rel1 (Build arm-15.86)) 2.45.1.20251203";
const QEMU_VERSION: &str = "QEMU emulator version 10.2.2";
const PYTHON_VERSION: &str = "Python 3.12.13";
const PRLIMIT_VERSION: &str = "prlimit from util-linux 2.42";
const QEMU_ARGUMENTS: &[&str] = &[
    "-M",
    "mps2-an500",
    "-cpu",
    "cortex-m7",
    "-accel",
    "tcg,one-insn-per-tb=on",
    "-nographic",
    "-semihosting-config",
    "enable=on,target=native",
    "-kernel",
];
const QEMU_TRACE_ARGUMENTS: &[&str] = &["-d", "exec,nochain", "-D"];
const VERSION_ARGUMENTS: &[&str] = &["--version"];

#[cfg(test)]
pub(super) fn expected_gcc_version(profile: CrossProfile) -> &'static str {
    profile_spec(profile).gcc_version
}

#[cfg(test)]
pub(super) fn expected_nm_version(profile: CrossProfile) -> &'static str {
    profile_spec(profile).nm_version
}

#[cfg(test)]
pub(super) fn expected_qemu_version(profile: CrossProfile) -> &'static str {
    profile_spec(profile).qemu_version
}

#[cfg(test)]
pub(super) fn expected_python_version(profile: CrossProfile) -> &'static str {
    profile_spec(profile).python_version
}

#[cfg(test)]
pub(super) fn expected_prlimit_version(_profile: CrossProfile) -> &'static str {
    PRLIMIT_VERSION
}

#[cfg(test)]
pub(super) fn normalized_profile_sha256(entry: &Entry) -> String {
    let generation = BoundGenerationPlan::normalized(entry)
        .expect("recomputing a generation profile requires an executable row");
    normalized_profile_sha256_with_generation(
        entry,
        GCC_VERSION,
        NM_VERSION,
        QEMU_VERSION,
        PYTHON_VERSION,
        &generation,
    )
}

pub(super) fn authenticate_execution_root(
    tools: ExecutionTools,
    compiler: AuthenticatedCompiler,
    comparator_outputs: crate::verify_cmd::embedded_head_to_head::manifest::ComparatorOutputPins,
    tool_closure: AuthenticatedToolClosure,
) -> Result<Arc<AuthenticatedExecutionRoot>> {
    tools.verify_exact_versions()?;
    Ok(Arc::new(AuthenticatedExecutionRoot {
        tools,
        compiler,
        comparator_outputs,
        tool_closure,
    }))
}

pub(super) fn bind_execution_plan(
    root: &Arc<AuthenticatedExecutionRoot>,
    entry: &Entry,
) -> Result<BoundExecutionPlan> {
    let generation = BoundGenerationPlan::normalized(entry)?;
    let measurement = BoundMeasurementPlan::from_manifest(entry.metric);
    let suite = suite::BoundSuitePlan::closed();
    let observed = normalized_profile_sha256_with_generation(
        entry,
        GCC_VERSION,
        NM_VERSION,
        QEMU_VERSION,
        PYTHON_VERSION,
        &generation,
    );
    ensure!(
        observed == entry.normalized_profile_sha256,
        "bound execution suite digest is {observed}, expected {}",
        entry.normalized_profile_sha256,
    );
    Ok(BoundExecutionPlan {
        root: Arc::clone(root),
        profile: entry.cross_profile,
        generation,
        measurement,
        suite,
    })
}

pub(super) fn start_suite(plan: &BoundExecutionPlan) -> suite::SuiteCursor {
    plan.suite.cursor()
}

pub(super) fn execute_suite_step<T>(
    pending: PendingSuiteStep,
    operation: impl FnOnce(SuiteStep) -> Result<T>,
) -> Result<(SuiteCursor, ExecutedSuiteStep, T)> {
    pending.execute(operation)
}

pub(super) fn gcc_version_command(tool: &RolePath<GccExecutable>) -> process::HermeticCommand {
    version_command(tool)
}

pub(super) fn nm_version_command(tool: &RolePath<NmExecutable>) -> process::HermeticCommand {
    version_command(tool)
}

pub(super) fn qemu_version_command(tool: &RolePath<QemuExecutable>) -> process::HermeticCommand {
    version_command(tool)
}

pub(super) fn python_version_command(
    tool: &RolePath<PythonExecutable>,
) -> process::HermeticCommand {
    version_command(tool)
}

pub(super) fn prlimit_version_command(
    tool: &RolePath<PrlimitExecutable>,
) -> process::HermeticCommand {
    version_command(tool)
}

fn version_command<R: ExecutableRole>(tool: &RolePath<R>) -> process::HermeticCommand {
    let mut command = process::HermeticCommand::new(tool);
    command.args(VERSION_ARGUMENTS);
    command
}

pub(super) fn rumoca_generation_command(
    plan: &BoundExecutionPlan,
    fixture: &RolePath<FixtureSource>,
    cache: &RolePath<RumocaCacheDirectory>,
    output: &RolePath<RumocaEmissionInclude>,
) -> Result<process::HermeticCommand> {
    let compiler = plan.compiler_for_execution()?;
    plan.generation.rumoca_command(
        compiler,
        &plan.root.compiler.runtime_environment,
        fixture,
        cache,
        output,
    )
}

pub(super) fn casadi_generation_command(
    plan: &BoundExecutionPlan,
    generator: &RolePath<ComparatorGenerator>,
    output: &RolePath<CasadiEmissionInclude>,
) -> Result<process::HermeticCommand> {
    plan.generation
        .casadi_command(plan.tools().python().for_execution()?, generator, output)
}

pub(super) fn qemu_command(
    plan: &BoundExecutionPlan,
    artifact: &LinkedArtifact,
    trace_log: &Path,
) -> Result<process::HermeticCommand> {
    let prlimit = plan.tools().prlimit().for_execution()?;
    let qemu = plan.tools().qemu().for_execution()?;
    let mut command = process::HermeticCommand::new(prlimit);
    command
        .arg(format!("--fsize={0}:{0}", qemu::MAX_TRACE_BYTES))
        .arg("--")
        .arg(qemu.as_path())
        .args(QEMU_ARGUMENTS)
        .arg(&artifact.elf)
        .args(QEMU_TRACE_ARGUMENTS)
        .arg(trace_log);
    Ok(command)
}

#[cfg(test)]
fn normalized_profile_sha256_with_identities(
    entry: &Entry,
    gcc_version: &str,
    nm_version: &str,
    qemu_version: &str,
    python_version: &str,
    compiler_role: &'static str,
) -> String {
    let generation = BoundGenerationPlan::with_compiler_role(entry, compiler_role);
    normalized_profile_sha256_with_generation(
        entry,
        gcc_version,
        nm_version,
        qemu_version,
        python_version,
        &generation,
    )
}

fn normalized_profile_sha256_with_generation(
    entry: &Entry,
    gcc_version: &str,
    nm_version: &str,
    qemu_version: &str,
    python_version: &str,
    generation: &BoundGenerationPlan,
) -> String {
    normalized_profile_sha256_with_plan(NormalizedDigestInputs {
        profile: entry.cross_profile,
        gcc_version,
        nm_version,
        qemu_version,
        python_version,
        generation,
        measurement: BoundMeasurementPlan::from_manifest(entry.metric),
    })
}

struct NormalizedDigestInputs<'a> {
    profile: CrossProfile,
    gcc_version: &'a str,
    nm_version: &'a str,
    qemu_version: &'a str,
    python_version: &'a str,
    generation: &'a BoundGenerationPlan,
    measurement: BoundMeasurementPlan,
}

fn normalized_profile_sha256_with_plan(inputs: NormalizedDigestInputs<'_>) -> String {
    let NormalizedDigestInputs {
        profile,
        gcc_version,
        nm_version,
        qemu_version,
        python_version,
        generation,
        measurement,
    } = inputs;
    let profile = profile_spec_with_versions(
        profile,
        gcc_version,
        nm_version,
        qemu_version,
        python_version,
    );
    let mut digest = Sha256::new();
    digest.update(b"embedded-head-to-head-protocol-catalog-v4\0");
    update_profile(&mut digest, &profile);
    update_generation_plans(&mut digest, &profile, generation);
    measurement.update_digest(&mut digest);
    suite::BoundSuitePlan::closed().update_digest(&mut digest);
    for spec in RECIPES {
        update_recipe(&mut digest, &profile, spec);
    }
    format!("{:x}", digest.finalize())
}

pub(super) fn rumoca_measured(
    context: &Context<'_>,
    emission: &RumocaEmission,
) -> Result<LinkedArtifact> {
    execute(
        context,
        RecipeKind::RumocaMeasured,
        Bindings::rumoca(context, emission, RumocaMutation::None, None),
    )
}

pub(super) fn rumoca_correctness(
    context: &Context<'_>,
    emission: &RumocaEmission,
) -> Result<LinkedArtifact> {
    execute(
        context,
        RecipeKind::RumocaCorrectness,
        Bindings::rumoca(context, emission, RumocaMutation::None, None),
    )
}

pub(super) fn rumoca_deleted_call(
    context: &Context<'_>,
    emission: &RumocaEmission,
    driver: &RolePath<RumocaDeletedCallDriver>,
    mutation_guard: &FrozenArtifactSet,
) -> Result<LinkedArtifact> {
    execute(
        context,
        RecipeKind::RumocaDeletedCall,
        Bindings::rumoca(
            context,
            emission,
            RumocaMutation::DeletedCall(driver),
            Some(mutation_guard),
        ),
    )
}

pub(super) fn rumoca_constant_output(
    context: &Context<'_>,
    emission: &RumocaEmission,
    driver: &RolePath<RumocaConstantOutputDriver>,
    mutation_guard: &FrozenArtifactSet,
) -> Result<LinkedArtifact> {
    execute(
        context,
        RecipeKind::RumocaConstantOutput,
        Bindings::rumoca(
            context,
            emission,
            RumocaMutation::ConstantOutput(driver),
            Some(mutation_guard),
        ),
    )
}

pub(super) fn rumoca_closed_branches(
    context: &Context<'_>,
    emission: &RumocaEmission,
    driver: &RolePath<RumocaClosedBranchDriver>,
    model: &RolePath<RumocaClosedBranchModel>,
    mutation_guard: &FrozenArtifactSet,
) -> Result<LinkedArtifact> {
    execute(
        context,
        RecipeKind::RumocaClosedBranches,
        Bindings::rumoca(
            context,
            emission,
            RumocaMutation::ClosedBranches { driver, model },
            Some(mutation_guard),
        ),
    )
}

pub(super) fn casadi_measured(
    context: &Context<'_>,
    emission: &CasadiEmission,
) -> Result<LinkedArtifact> {
    execute(
        context,
        RecipeKind::CasadiMeasured,
        Bindings::casadi(context, emission, None, None),
    )
}

pub(super) fn casadi_correctness(
    context: &Context<'_>,
    emission: &CasadiEmission,
) -> Result<LinkedArtifact> {
    execute(
        context,
        RecipeKind::CasadiCorrectness,
        Bindings::casadi(context, emission, None, None),
    )
}

pub(super) fn casadi_row_major(
    context: &Context<'_>,
    emission: &CasadiEmission,
    driver: &RolePath<CasadiRowMajorDriver>,
    mutation_guard: &FrozenArtifactSet,
) -> Result<LinkedArtifact> {
    execute(
        context,
        RecipeKind::CasadiRowMajor,
        Bindings::casadi(context, emission, Some(driver), Some(mutation_guard)),
    )
}

pub(super) fn counter(context: &Context<'_>, selected: CounterRecipe) -> Result<LinkedArtifact> {
    let kind = match selected {
        CounterRecipe::Straight => RecipeKind::CounterStraight,
        CounterRecipe::CalledLeaf => RecipeKind::CounterCalledLeaf,
        CounterRecipe::ExitStatus => RecipeKind::CounterExitStatus,
    };
    execute(context, kind, Bindings::counter(context))
}

#[cfg(test)]
pub(super) fn casadi_measured_bound_paths(
    context: &Context<'_>,
    emission: &CasadiEmission,
) -> Result<Vec<PathBuf>> {
    let profile = profile_spec(context.plan.profile());
    let bindings = Bindings::casadi(context, emission, None, None);
    let plan = BuildPlan::bind(
        recipe(RecipeKind::CasadiMeasured)?,
        &profile,
        context.plan.tools(),
        &bindings,
        context.work,
    )?;
    Ok(plan
        .invocations
        .iter()
        .flat_map(|invocation| &invocation.args)
        .filter_map(|arg| match arg {
            BoundArg::Path(path) => Some(path.actual.clone()),
            BoundArg::Literal(_) => None,
        })
        .collect())
}

fn execute(
    context: &Context<'_>,
    kind: RecipeKind,
    bindings: Bindings<'_>,
) -> Result<LinkedArtifact> {
    bindings.verify_inputs()?;
    let profile = profile_spec(context.plan.profile());
    let spec = recipe(kind)?;
    fresh_work(context.work)?;
    let result = BuildPlan::bind(
        spec,
        &profile,
        context.plan.tools(),
        &bindings,
        context.work,
    )?
    .run();
    let post_use = bindings.verify_inputs();
    match (result, post_use) {
        (Ok(artifact), Ok(())) => Ok(artifact),
        (Ok(_), Err(error)) => Err(error),
        (Err(error), _) => Err(error),
    }
}

fn recipe(kind: RecipeKind) -> Result<&'static RecipeSpec> {
    RECIPES
        .iter()
        .find(|spec| spec.kind == kind)
        .with_context(|| format!("recipe catalog omitted {kind:?}"))
}

enum EmissionBindings<'a> {
    Rumoca(&'a RumocaEmission),
    Casadi(&'a CasadiEmission),
    Counter,
}

enum RumocaMutation<'a> {
    None,
    DeletedCall(&'a RolePath<RumocaDeletedCallDriver>),
    ConstantOutput(&'a RolePath<RumocaConstantOutputDriver>),
    ClosedBranches {
        driver: &'a RolePath<RumocaClosedBranchDriver>,
        model: &'a RolePath<RumocaClosedBranchModel>,
    },
}

struct Bindings<'a> {
    harness: &'a AuthenticatedHarness,
    oracle: &'a RolePath<OracleInclude>,
    emission: EmissionBindings<'a>,
    rumoca_mutation: RumocaMutation<'a>,
    casadi_mutation: Option<&'a RolePath<CasadiRowMajorDriver>>,
    oracle_guard: &'a FrozenArtifactSet,
    mutation_guard: Option<&'a FrozenArtifactSet>,
}

impl<'a> Bindings<'a> {
    fn rumoca(
        context: &'a Context<'a>,
        emission: &'a RumocaEmission,
        mutation: RumocaMutation<'a>,
        mutation_guard: Option<&'a FrozenArtifactSet>,
    ) -> Self {
        Self {
            harness: context.harness,
            oracle: context.oracle,
            emission: EmissionBindings::Rumoca(emission),
            rumoca_mutation: mutation,
            casadi_mutation: None,
            oracle_guard: context.oracle_guard,
            mutation_guard,
        }
    }

    fn casadi(
        context: &'a Context<'a>,
        emission: &'a CasadiEmission,
        mutation: Option<&'a RolePath<CasadiRowMajorDriver>>,
        mutation_guard: Option<&'a FrozenArtifactSet>,
    ) -> Self {
        Self {
            harness: context.harness,
            oracle: context.oracle,
            emission: EmissionBindings::Casadi(emission),
            rumoca_mutation: RumocaMutation::None,
            casadi_mutation: mutation,
            oracle_guard: context.oracle_guard,
            mutation_guard,
        }
    }

    fn counter(context: &'a Context<'a>) -> Self {
        Self {
            harness: context.harness,
            oracle: context.oracle,
            emission: EmissionBindings::Counter,
            rumoca_mutation: RumocaMutation::None,
            casadi_mutation: None,
            oracle_guard: context.oracle_guard,
            mutation_guard: None,
        }
    }

    fn verify_inputs(&self) -> Result<()> {
        self.harness.verify()?;
        self.oracle_guard.verify()?;
        match self.emission {
            EmissionBindings::Rumoca(emission) => emission.verify()?,
            EmissionBindings::Casadi(emission) => emission.verify()?,
            EmissionBindings::Counter => {}
        }
        if let Some(guard) = self.mutation_guard {
            guard.verify()?;
        }
        Ok(())
    }

    fn resolve(&self, slot: PathSlot) -> Result<BoundPath> {
        let path = match slot {
            PathSlot::HarnessInclude => BoundPath::of(self.harness.include()),
            PathSlot::OracleInclude => BoundPath::of(self.oracle),
            PathSlot::LinkerScript => BoundPath::of(self.harness.linker()),
            PathSlot::StartupSource => BoundPath::of(self.harness.startup()),
            PathSlot::TraceSource => BoundPath::of(self.harness.trace()),
            PathSlot::RumocaMeasuredDriver => BoundPath::of(self.harness.rumoca_measured()),
            PathSlot::RumocaCorrectnessDriver => BoundPath::of(self.harness.rumoca_correctness()),
            PathSlot::CasadiMeasuredDriver => BoundPath::of(self.harness.casadi_measured()),
            PathSlot::CasadiCorrectnessDriver => BoundPath::of(self.harness.casadi_correctness()),
            PathSlot::CounterSource => BoundPath::of(self.harness.counter()),
            PathSlot::RumocaEmissionInclude => {
                let EmissionBindings::Rumoca(emission) = self.emission else {
                    bail!("Rumoca include requested from a non-Rumoca binding set")
                };
                BoundPath::of(&emission.include)
            }
            PathSlot::RumocaModel => {
                let EmissionBindings::Rumoca(emission) = self.emission else {
                    bail!("Rumoca model requested from a non-Rumoca binding set")
                };
                BoundPath::of(&emission.model_c)
            }
            PathSlot::RumocaKernels => {
                let EmissionBindings::Rumoca(emission) = self.emission else {
                    bail!("Rumoca kernels requested from a non-Rumoca binding set")
                };
                BoundPath::of(&emission.kernels_c)
            }
            PathSlot::CasadiEmissionInclude => {
                let EmissionBindings::Casadi(emission) = self.emission else {
                    bail!("CasADi include requested from a non-CasADi binding set")
                };
                BoundPath::of(&emission.include)
            }
            PathSlot::ComparatorWrapper => {
                let EmissionBindings::Casadi(emission) = self.emission else {
                    bail!("CasADi wrapper requested from a non-CasADi binding set")
                };
                BoundPath::of(&emission.wrapper_c)
            }
            PathSlot::RumocaDeletedCallDriver => match self.rumoca_mutation {
                RumocaMutation::DeletedCall(driver) => BoundPath::of(driver),
                _ => bail!("deleted-call recipe lacks its distinct typed driver"),
            },
            PathSlot::RumocaConstantOutputDriver => match self.rumoca_mutation {
                RumocaMutation::ConstantOutput(driver) => BoundPath::of(driver),
                _ => bail!("constant-output recipe lacks its distinct typed driver"),
            },
            PathSlot::RumocaClosedBranchDriver => match self.rumoca_mutation {
                RumocaMutation::ClosedBranches { driver, .. } => BoundPath::of(driver),
                _ => bail!("closed-branch recipe lacks its distinct typed driver"),
            },
            PathSlot::RumocaClosedBranchModel => match self.rumoca_mutation {
                RumocaMutation::ClosedBranches { model, .. } => BoundPath::of(model),
                _ => bail!("closed-branch recipe lacks its distinct typed model"),
            },
            PathSlot::CasadiRowMajorDriver => self
                .casadi_mutation
                .map(BoundPath::of)
                .context("row-major recipe lacks its distinct typed driver")?,
        };
        Ok(path)
    }
}

#[derive(Clone, Debug)]
struct BoundPath {
    actual: PathBuf,
}

impl BoundPath {
    fn of<R: Role>(path: &RolePath<R>) -> Self {
        Self {
            actual: path.as_path().to_path_buf(),
        }
    }
}

#[derive(Clone)]
enum BoundArg {
    Literal(&'static str),
    Path(BoundPath),
}

impl BoundArg {
    fn actual(&self) -> OsString {
        match self {
            Self::Literal(value) => OsString::from(value),
            Self::Path(path) => path.actual.as_os_str().to_owned(),
        }
    }
}

#[derive(Clone)]
enum BoundTool {
    Gcc(AuthenticatedExecutable<GccExecutable>),
    Nm(AuthenticatedExecutable<NmExecutable>),
}

impl BoundTool {
    fn command(&self) -> Result<process::HermeticCommand> {
        match self {
            Self::Gcc(tool) => Ok(process::HermeticCommand::new(tool.for_execution()?)),
            Self::Nm(tool) => Ok(process::HermeticCommand::new(tool.for_execution()?)),
        }
    }
}

struct Invocation {
    id: &'static str,
    tool: BoundTool,
    args: Vec<BoundArg>,
    capture: Option<BoundPath>,
}

pub(super) struct BoundExecutionArtifact {
    trace_file: &'static str,
}

impl BoundExecutionArtifact {
    pub(super) fn trace_log(&self, directory: &Path) -> PathBuf {
        directory
            .join("traces")
            .join(self.trace_file)
            .join("qemu.trace")
    }
}

struct BuildPlan {
    work: PathBuf,
    elf: BoundPath,
    symbols: BoundPath,
    invocations: Vec<Invocation>,
    execution: BoundExecutionArtifact,
}

impl BuildPlan {
    fn bind(
        spec: &'static RecipeSpec,
        profile: &Profile<'_>,
        tools: &ExecutionTools,
        bindings: &Bindings<'_>,
        work: &Path,
    ) -> Result<Self> {
        let outputs = BoundOutputs::actual(spec.artifact, work)?;
        let invocations = spec
            .invocations(profile)
            .into_iter()
            .map(|invocation| Invocation::bind(invocation, tools, bindings, &outputs))
            .collect::<Result<Vec<_>>>()?;
        Ok(Self {
            work: work.to_path_buf(),
            elf: outputs.elf,
            symbols: outputs.symbols,
            invocations,
            execution: BoundExecutionArtifact {
                trace_file: spec.trace_file,
            },
        })
    }

    fn run(self) -> Result<LinkedArtifact> {
        ensure!(self.work.is_dir(), "cross-build work directory disappeared");
        let commands = run_invocations(self.invocations)?;
        let frozen = FrozenArtifactSet::capture_tree(&self.work)?;
        ensure!(
            self.elf.actual.is_file() && self.symbols.actual.is_file(),
            "cross-build did not construct its sealed ELF and symbol artifacts"
        );
        Ok(LinkedArtifact {
            elf: self.elf.actual,
            symbols: self.symbols.actual,
            commands,
            frozen,
            execution: self.execution,
        })
    }
}

fn run_invocations(invocations: Vec<Invocation>) -> Result<Vec<process::CommandReceipt>> {
    let mut commands = Vec::new();
    for invocation in invocations {
        match invocation.run() {
            Ok(receipt) => commands.push(receipt),
            Err(error) => return Err(process::attach_prior_receipts(error, commands)),
        }
    }
    Ok(commands)
}

struct BoundOutputs {
    startup: RolePath<StartupObject>,
    trace: RolePath<TraceObject>,
    driver: RolePath<DriverObject>,
    model: RolePath<ModelObject>,
    kernels: RolePath<KernelsObject>,
    comparator: RolePath<ComparatorObject>,
    counter: RolePath<CounterObject>,
    elf: BoundPath,
    symbols: BoundPath,
}

impl BoundOutputs {
    fn actual(artifact: ArtifactKind, work: &Path) -> Result<Self> {
        let stem = artifact.stem();
        let elf = work.join(format!("{stem}.elf"));
        let symbols = work.join(format!("{stem}.symbols.txt"));
        let (elf, symbols) = match artifact {
            ArtifactKind::Rumoca => (
                BoundPath::of(&RolePath::<RumocaElf>::checked(elf)?),
                BoundPath::of(&RolePath::<RumocaSymbols>::checked(symbols)?),
            ),
            ArtifactKind::Casadi => (
                BoundPath::of(&RolePath::<CasadiElf>::checked(elf)?),
                BoundPath::of(&RolePath::<CasadiSymbols>::checked(symbols)?),
            ),
            ArtifactKind::CounterStraight => (
                BoundPath::of(&RolePath::<CounterStraightElf>::checked(elf)?),
                BoundPath::of(&RolePath::<CounterStraightSymbols>::checked(symbols)?),
            ),
            ArtifactKind::CounterCalledLeaf => (
                BoundPath::of(&RolePath::<CounterCalledLeafElf>::checked(elf)?),
                BoundPath::of(&RolePath::<CounterCalledLeafSymbols>::checked(symbols)?),
            ),
            ArtifactKind::CounterExitStatus => (
                BoundPath::of(&RolePath::<CounterExitStatusElf>::checked(elf)?),
                BoundPath::of(&RolePath::<CounterExitStatusSymbols>::checked(symbols)?),
            ),
        };
        Ok(Self {
            startup: RolePath::checked(work.join(ObjectSlot::Startup.file_name()))?,
            trace: RolePath::checked(work.join(ObjectSlot::Trace.file_name()))?,
            driver: RolePath::checked(work.join(ObjectSlot::Driver.file_name()))?,
            model: RolePath::checked(work.join(ObjectSlot::Model.file_name()))?,
            kernels: RolePath::checked(work.join(ObjectSlot::Kernels.file_name()))?,
            comparator: RolePath::checked(work.join(ObjectSlot::Comparator.file_name()))?,
            counter: RolePath::checked(work.join(ObjectSlot::Counter.file_name()))?,
            elf,
            symbols,
        })
    }

    fn resolve(&self, slot: ObjectSlot) -> Result<BoundPath> {
        Ok(match slot {
            ObjectSlot::Startup => BoundPath::of(&self.startup),
            ObjectSlot::Trace => BoundPath::of(&self.trace),
            ObjectSlot::Driver => BoundPath::of(&self.driver),
            ObjectSlot::Model => BoundPath::of(&self.model),
            ObjectSlot::Kernels => BoundPath::of(&self.kernels),
            ObjectSlot::Comparator => BoundPath::of(&self.comparator),
            ObjectSlot::Counter => BoundPath::of(&self.counter),
        })
    }
}

impl Invocation {
    fn bind(
        spec: InvocationSpec,
        tools: &ExecutionTools,
        bindings: &Bindings<'_>,
        outputs: &BoundOutputs,
    ) -> Result<Self> {
        let tool = match spec.tool {
            ToolSlot::Gcc => BoundTool::Gcc(tools.gcc().clone()),
            ToolSlot::Nm => BoundTool::Nm(tools.nm().clone()),
        };
        let args = spec
            .args
            .into_iter()
            .map(|arg| match arg {
                ArgSpec::Literal(value) => Ok(BoundArg::Literal(value)),
                ArgSpec::Input(slot) => bindings.resolve(slot).map(BoundArg::Path),
                ArgSpec::Output(slot) => outputs.resolve(slot).map(BoundArg::Path),
                ArgSpec::Elf => Ok(BoundArg::Path(outputs.elf.clone())),
            })
            .collect::<Result<Vec<_>>>()?;
        let capture = match spec.capture {
            CaptureSpec::Status => None,
            CaptureSpec::Symbols => Some(outputs.symbols.clone()),
        };
        Ok(Self {
            id: spec.id,
            tool,
            args,
            capture,
        })
    }

    fn run(self) -> Result<process::CommandReceipt> {
        let mut command = self.tool.command()?;
        command.args(self.args.iter().map(BoundArg::actual));
        let Some(destination) = self.capture else {
            return process::require_success(&mut command, self.id, process::Limit::CrossBuild);
        };
        let (output, rendered) = process::output(&mut command, process::Limit::CrossBuild)?;
        if !output.status.success() {
            return Err(process::attach_attempted_receipt(
                anyhow::anyhow!(
                    "{} failed ({})\n{}",
                    self.id,
                    output.status,
                    process::tail(&process::combined(&output))
                ),
                rendered,
            ));
        }
        if let Err(error) = fs::write(&destination.actual, output.stdout)
            .with_context(|| format!("failed to write {}", destination.actual.display()))
        {
            return Err(process::attach_attempted_receipt(error, rendered));
        }
        Ok(rendered)
    }
}

impl ExecutionTools {
    /// The concrete sealed role parameter on every field is the role proof;
    /// this boundary validates only the remaining dynamic version facts.
    fn verify_exact_versions(&self) -> Result<()> {
        ensure!(self.gcc_version() == GCC_VERSION);
        ensure!(self.nm_version() == NM_VERSION);
        ensure!(self.qemu_version() == QEMU_VERSION);
        ensure!(self.python_version() == PYTHON_VERSION);
        ensure!(self.prlimit_version() == PRLIMIT_VERSION);
        Ok(())
    }
}

fn profile_spec(profile: CrossProfile) -> Profile<'static> {
    profile_spec_with_versions(
        profile,
        GCC_VERSION,
        NM_VERSION,
        QEMU_VERSION,
        PYTHON_VERSION,
    )
}

fn profile_spec_with_versions<'a>(
    profile: CrossProfile,
    gcc_version: &'a str,
    nm_version: &'a str,
    qemu_version: &'a str,
    python_version: &'a str,
) -> Profile<'a> {
    match profile {
        CrossProfile::Gcc15CortexM7O3LtoNoFpContractHotAuto700Single700ExactV3 => Profile {
            id: "gcc15-cortex-m7-o3-lto-no-fp-contract-hot-auto700-single700-exact-v3",
            gcc_version,
            nm_version,
            qemu_version,
            python_version,
            c_and_lto_flags: C_AND_LTO_FLAGS,
            warnings: WARNINGS,
            assembly_flags: ASSEMBLY_FLAGS,
            link_options: LINK_OPTIONS,
            link_libraries: LINK_LIBRARIES,
        },
        #[cfg(test)]
        CrossProfile::TestAlternateNoFpContract => Profile {
            id: "test-alternate-no-fp-contract",
            gcc_version,
            nm_version,
            qemu_version,
            python_version,
            c_and_lto_flags: C_AND_LTO_FLAGS,
            warnings: WARNINGS,
            assembly_flags: ASSEMBLY_FLAGS,
            link_options: LINK_OPTIONS,
            link_libraries: LINK_LIBRARIES,
        },
    }
}

fn update_profile(digest: &mut Sha256, profile: &Profile<'_>) {
    update_field(digest, "profile", profile.id);
    update_field(digest, "gcc-tool-role", <GccExecutable as Role>::ID);
    update_field(digest, "gcc-version", profile.gcc_version);
    update_field(digest, "nm-tool-role", <NmExecutable as Role>::ID);
    update_field(digest, "nm-version", profile.nm_version);
    update_field(digest, "qemu-tool-role", <QemuExecutable as Role>::ID);
    update_field(digest, "qemu-version", profile.qemu_version);
    update_field(digest, "python-tool-role", <PythonExecutable as Role>::ID);
    update_field(digest, "python-version", profile.python_version);
    update_field(digest, "prlimit-tool-role", <PrlimitExecutable as Role>::ID);
    update_field(digest, "prlimit-version", PRLIMIT_VERSION);
    update_field(
        digest,
        "environment-policy",
        process::HERMETIC_ENVIRONMENT_ID,
    );
    update_field(
        digest,
        "compiler-environment-policy",
        process::COMPILER_RUNTIME_ENVIRONMENT_ID,
    );
    update_field(
        digest,
        "working-directory",
        process::HERMETIC_WORKING_DIRECTORY_ID,
    );
    update_field(
        digest,
        "qemu-trace-max-bytes",
        &crate::verify_cmd::embedded_head_to_head::qemu::MAX_TRACE_BYTES.to_string(),
    );
}

fn update_recipe(digest: &mut Sha256, profile: &Profile<'_>, spec: &RecipeSpec) {
    update_field(digest, "recipe", spec.id);
    update_field(digest, "elf-role", spec.artifact.elf_identity());
    update_field(digest, "symbol-role", spec.artifact.symbol_identity());
    for invocation in spec.invocations(profile) {
        update_field(digest, "invocation", invocation.id);
        update_field(
            digest,
            "tool-role",
            match invocation.tool {
                ToolSlot::Gcc => <GccExecutable as Role>::ID,
                ToolSlot::Nm => <NmExecutable as Role>::ID,
            },
        );
        for arg in invocation.args {
            match arg {
                ArgSpec::Literal(value) => update_field(digest, "literal", value),
                ArgSpec::Input(slot) => update_field(digest, "path-role", slot.identity()),
                ArgSpec::Output(slot) => update_field(digest, "path-role", slot.identity()),
                ArgSpec::Elf => update_field(digest, "path-role", spec.artifact.elf_identity()),
            }
        }
        update_field(
            digest,
            "capture-role",
            match invocation.capture {
                CaptureSpec::Status => "status-only",
                CaptureSpec::Symbols => spec.artifact.symbol_identity(),
            },
        );
    }
    update_field(digest, "invocation", "QEMU guest execution");
    update_field(digest, "tool-role", <PrlimitExecutable as Role>::ID);
    update_field(
        digest,
        "literal",
        &format!("--fsize={0}:{0}", qemu::MAX_TRACE_BYTES),
    );
    update_field(digest, "literal", "--");
    update_field(digest, "nested-tool-role", <QemuExecutable as Role>::ID);
    for literal in QEMU_ARGUMENTS {
        update_field(digest, "literal", literal);
    }
    update_field(digest, "path-role", spec.artifact.elf_identity());
    for literal in QEMU_TRACE_ARGUMENTS {
        update_field(digest, "literal", literal);
    }
    update_field(digest, "path-role", &format!("trace:{}", spec.trace_file));
}

fn update_generation_plans(
    digest: &mut Sha256,
    profile: &Profile<'_>,
    generation: &BoundGenerationPlan,
) {
    for (role, version) in [
        (<GccExecutable as Role>::ID, profile.gcc_version),
        (<NmExecutable as Role>::ID, profile.nm_version),
        (<QemuExecutable as Role>::ID, profile.qemu_version),
        (<PythonExecutable as Role>::ID, profile.python_version),
    ] {
        update_field(digest, "invocation", "tool version probe");
        update_field(digest, "tool-role", role);
        update_field(digest, "tool-version", version);
        for literal in VERSION_ARGUMENTS {
            update_field(digest, "literal", literal);
        }
    }
    generation.update_digest(digest);
}

fn fresh_work(work: &Path) -> Result<()> {
    if work.exists() {
        fs::remove_dir_all(work).with_context(|| format!("failed to clear {}", work.display()))?;
    }
    fs::create_dir_all(work).with_context(|| format!("failed to create {}", work.display()))
}

fn update_field(digest: &mut Sha256, label: &str, value: &str) {
    digest.update(label.as_bytes());
    digest.update([0]);
    digest.update(value.as_bytes());
    digest.update([0]);
}

#[cfg(test)]
mod tests {
    use super::*;

    fn checked_entry() -> Entry {
        let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
        let raw = std::fs::read_to_string(
            crate::verify_cmd::embedded_head_to_head::manifest::path(&root),
        )
        .unwrap();
        let manifest: serde_json::Value = serde_json::from_str(&raw).unwrap();
        serde_json::from_value(manifest["entries"][0].clone()).unwrap()
    }

    #[test]
    fn catalog_is_exhaustive_and_unique() {
        assert_eq!(RECIPES.len(), RecipeKind::ALL.len());
        for kind in RecipeKind::ALL {
            assert_eq!(RECIPES.iter().filter(|spec| spec.kind == kind).count(), 1);
        }
        for (index, recipe) in RECIPES.iter().enumerate() {
            assert_eq!(
                RECIPES.iter().filter(|item| item.id == recipe.id).count(),
                1
            );
            assert_eq!(recipe.kind, RecipeKind::ALL[index]);
        }
    }

    #[test]
    fn every_c_compile_and_lto_link_forbids_fp_contraction() {
        let profile =
            profile_spec(CrossProfile::Gcc15CortexM7O3LtoNoFpContractHotAuto700Single700ExactV3);
        assert!(profile.c_and_lto_flags.contains(&"-ffp-contract=off"));
        for spec in RECIPES {
            assert!(spec.sources.iter().any(|source| source.is_c));
        }
    }

    #[test]
    fn every_external_tool_identity_is_part_of_normalized_catalog() {
        let entry = checked_entry();
        let expected = normalized_profile_sha256(&entry);
        for (gcc, nm, qemu, python, compiler) in [
            (
                "mutated gcc identity",
                NM_VERSION,
                QEMU_VERSION,
                PYTHON_VERSION,
                <CompilerExecutable as Role>::ID,
            ),
            (
                GCC_VERSION,
                "mutated nm identity",
                QEMU_VERSION,
                PYTHON_VERSION,
                <CompilerExecutable as Role>::ID,
            ),
            (
                GCC_VERSION,
                NM_VERSION,
                "mutated qemu identity",
                PYTHON_VERSION,
                <CompilerExecutable as Role>::ID,
            ),
            (
                GCC_VERSION,
                NM_VERSION,
                QEMU_VERSION,
                "mutated python identity",
                <CompilerExecutable as Role>::ID,
            ),
            (
                GCC_VERSION,
                NM_VERSION,
                QEMU_VERSION,
                PYTHON_VERSION,
                "tool:rebound-compiler",
            ),
        ] {
            assert_ne!(
                expected,
                normalized_profile_sha256_with_identities(&entry, gcc, nm, qemu, python, compiler,)
            );
        }
    }

    #[test]
    fn generation_arg_omission_reordering_and_rebinding_move_the_digest() {
        let entry = checked_entry();
        let expected = normalized_profile_sha256(&entry);
        for mutation in [
            generation::TestMutation::ReorderRumoca,
            generation::TestMutation::OmitCasadi,
            generation::TestMutation::RebindRumocaTarget,
        ] {
            let generation = BoundGenerationPlan::mutated(&entry, mutation);
            assert_ne!(
                expected,
                normalized_profile_sha256_with_plan(NormalizedDigestInputs {
                    profile: entry.cross_profile,
                    gcc_version: GCC_VERSION,
                    nm_version: NM_VERSION,
                    qemu_version: QEMU_VERSION,
                    python_version: PYTHON_VERSION,
                    generation: &generation,
                    measurement: BoundMeasurementPlan::from_manifest(entry.metric),
                })
            );
        }
    }

    #[test]
    fn every_executed_mutant_has_a_distinct_catalog_recipe() {
        let roles = [
            PathSlot::RumocaDeletedCallDriver,
            PathSlot::RumocaConstantOutputDriver,
            PathSlot::RumocaClosedBranchDriver,
            PathSlot::RumocaClosedBranchModel,
            PathSlot::CasadiRowMajorDriver,
        ];
        for role in roles {
            assert_eq!(
                RECIPES
                    .iter()
                    .flat_map(|spec| spec.sources)
                    .filter(|source| source.input == role)
                    .count(),
                1,
                "mutation role {} must appear exactly once",
                role.identity()
            );
        }
    }

    #[cfg(unix)]
    #[test]
    fn nth_invocation_failure_retains_successful_prefix_then_attempt_once() {
        use sha2::{Digest, Sha256};
        use std::os::unix::fs::PermissionsExt;

        let temporary = tempfile::tempdir().unwrap();
        let executable = temporary.path().join("arm-none-eabi-gcc");
        let bytes = b"#!/bin/sh\n[ \"$1\" = fail ] && exit 9\nexit 0\n";
        fs::write(&executable, bytes).unwrap();
        let mut permissions = fs::metadata(&executable).unwrap().permissions();
        permissions.set_mode(0o755);
        fs::set_permissions(&executable, permissions).unwrap();
        let tool = AuthenticatedExecutable::<GccExecutable>::checked(
            RolePath::checked(executable).unwrap(),
            &format!("{:x}", Sha256::digest(bytes)),
        )
        .unwrap();
        let invocation = |id, argument| Invocation {
            id,
            tool: BoundTool::Gcc(tool.clone()),
            args: vec![BoundArg::Literal(argument)],
            capture: None,
        };

        let error = run_invocations(vec![
            invocation("first", "ok-first"),
            invocation("second", "ok-second"),
            invocation("third", "fail"),
        ])
        .expect_err("the third invocation must fail");
        let receipts = process::attempted_receipts(&error);
        assert_eq!(receipts.len(), 3, "each attempted command appears once");
        assert!(receipts[0].display_only().ends_with("ok-first"));
        assert!(receipts[1].display_only().ends_with("ok-second"));
        assert!(receipts[2].display_only().ends_with("fail"));
    }
}
