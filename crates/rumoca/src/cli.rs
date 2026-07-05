//! Reusable CLI argument parsing and dispatch for the `rumoca` binary.
//!
//! This module owns the clap argument types and the command dispatch logic so
//! that both the `rumoca` binary and the Python bindings can run the exact same
//! "verbatim CLI" operations. The binary uses [`run`] (which prints/writes files
//! like the user-facing CLI); the bindings use the value-returning entrypoints
//! ([`compile_to_value`], [`simulate_to_value`]) to get structured data back.
//!
//! The `#[global_allocator]`, `fn main`, and the miette error-*printing* helpers
//! live in `main.rs` (binary-only); the error-*report builders* live here so they
//! can be unit-tested alongside the dispatch logic and reused by `main.rs`.

use std::path::Path;
use std::path::PathBuf;

use crate::cache_cmd;
use crate::fmt_cli;
use crate::main_helpers::{completion_script, discover_workspace_root_for_model_file};
use crate::sim_bench;
use crate::sim_inspect;
use crate::target_manifest;
use crate::targets_cmd;
#[cfg(feature = "runner")]
use anyhow::Context;
use anyhow::{Result, bail};
use clap::{ArgAction, Args, Parser, Subcommand, ValueEnum};
use miette::{LabeledSpan, MietteDiagnostic, NamedSource, Report, Severity};

// Re-export the leaf-command argument types declared in sibling modules so the
// public `Commands`/`SimSubcommand` variants that embed them are nameable as
// `rumoca::cli::FmtArgs` / `rumoca::cli::SimBenchArgs`.
pub use crate::fmt_cli::FmtArgs;
pub use crate::sim_bench::SimBenchArgs;
use crate::{CompilationResult, Compiler, CompilerError, DaeCompilationResult, TemplateIr};
use rumoca_compile::{
    codegen::{render_ast_template_with_name, render_flat_template_with_name},
    compile::core::{Diagnostic as CommonDiagnostic, DiagnosticSeverity, SourceMap},
    compile::{Dae, FlatModel, ResolvedTree},
};
use rumoca_sim::{DiffsolMethod, SimOptions, SimSolverMode};
use rumoca_sim::{SimulationRequestSummary, SimulationRunMetrics};
use rumoca_tool_lint::{LintLevel, LintMessage, LintOptions, PartialLintOptions};

#[path = "cli/model_resolution.rs"]
mod model_resolution;
pub(crate) use model_resolution::{
    collect_modelica_files, compiler_for_source, ensure_model_file_readable, first_path_config_dir,
    infer_model_name, merged_source_root_paths, normalize_target_paths, parent_dir_or_current,
    validate_explicit_target_paths,
};
#[cfg(test)]
pub(crate) use model_resolution::{merge_source_root_path_sources, split_path_list};

#[cfg(test)]
pub(crate) use sim_inspect::parse_eval_at_spec;

/// Git version string
const VERSION: &str = env!("CARGO_PKG_VERSION");
const DEFAULT_DEBUG_TRACE_FILTER: &str = concat!(
    "rumoca_phase_dae=debug,",
    "rumoca_phase_flatten=debug,",
    "rumoca_phase_instantiate=debug,",
    "rumoca_phase_resolve=debug,",
    "rumoca_phase_structural=debug"
);
const PROFILE_TRACE_FILTER: &str = concat!(
    "rumoca_phase_dae::profile=debug,",
    "rumoca_phase_dae::runtime_precompute=debug,",
    "rumoca_phase_resolve::timing=debug"
);

/// Long help for `--trace`, enumerating the phase targets a user can name in a
/// `--trace=<FILTER>` EnvFilter (keep in sync with DEFAULT_DEBUG_TRACE_FILTER /
/// PROFILE_TRACE_FILTER).
const TRACE_LONG_HELP: &str = "\
Trace internal compiler phases (requires --features tracing).

`--trace` alone enables debug tracing for the default set of phases.
`--trace=<FILTER>` takes a comma-separated filter. Phases can be named with a
short alias and an optional level (default `debug`):

  parse         resolve       instantiate   typecheck
  flatten       dae           structural    solve         codegen

e.g.
  --trace=dae                       (dae at debug)
  --trace=dae:trace,structural      (dae at trace, structural at debug)
  --trace=resolve:debug,solve:info

Each alias expands to its `rumoca_phase_<name>` target; any token that is not an
alias is passed through as a raw tracing EnvFilter directive, so
`--trace=rumoca_phase_dae::profile=debug` still works.

Subsystem targets (not compiler phases):
  viewer   debug overlay/logging for the interactive viewer; only applies to
           `sim --config` scenario runs (ignored by compile/check/batch sim)

Runtime diagnostic targets (simulation/solver internals; enable a whole crate
or a `::`-sub-target, e.g. --trace=rumoca_solver_diffsol::bdf):
  rumoca_solver_diffsol       ::bdf (event/root tracing) ::bdf_eval (eval counts)
  rumoca_solver_rk45          ::eval (RK eval counts/events)
  rumoca_solver               ::hotpath (solver step/root counters)
  rumoca_eval_solve           ::refresh (algebraic refresh) ::row (row-eval stats)
  rumoca_eval_dae             ::sim ::introspect ::function_inputs ::function_match
  rumoca_sim                  ::external_interface ::autopilot (child stdio passthrough)
  rumoca_transport_websocket  ::ws ::viewer_input
  rumoca_tool_lsp             ::completion
Short aliases: bdf, rk45, hotpath (e.g. --trace=bdf).

Add --trace-profile for phase timing/profiling targets
(rumoca_phase_dae::profile, rumoca_phase_dae::runtime_precompute,
rumoca_phase_resolve::timing).";

/// `--verbose` help. The "use --trace instead" pointer is only included when the
/// `tracing` feature is on — otherwise `--trace` is `hide`d and pointing at it
/// would dangle.
#[cfg(feature = "tracing")]
const VERBOSE_HELP: &str = "Verbose progress output (always available): friendly `[rumoca] Phase ...` lines. For structured, filterable internals use --trace instead.";
#[cfg(not(feature = "tracing"))]
const VERBOSE_HELP: &str = "Verbose progress output: friendly `[rumoca] Phase ...` lines.";

#[derive(Parser, Debug)]
#[command(name = "rumoca")]
#[command(version = VERSION)]
#[command(about = "Rumoca Modelica Compiler", long_about = None)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Commands,

    /// Override the on-disk cache root (default: the platform cache directory).
    /// Applies to every subcommand. `cache status`/`prune` also accept `--root`.
    // Declared after the subcommand so it sorts last in each subcommand's --help
    // (a low-importance global that shouldn't crowd the primary options).
    #[arg(long, global = true, value_name = "DIR")]
    pub cache_dir: Option<PathBuf>,
}

#[derive(Subcommand, Debug)]
pub enum Commands {
    /// Compile a Modelica file
    Compile(CompileArgs),
    /// Compile and simulate a model/scenario (see subcommands: check, init, bench)
    Sim(Box<SimCommandArgs>),
    /// Format Modelica files
    Fmt(FmtArgs),
    /// Lint Modelica files
    Lint(LintArgs),
    /// Print shell completion scripts
    Completions {
        /// Target shell
        #[arg(value_enum)]
        shell: CompletionShell,
    },
    /// List built-in code generation targets and their declared capabilities
    Targets(TargetsArgs),
    /// Inspect or prune the shared Rumoca cache
    Cache(CacheArgs),
}

#[derive(Args, Debug)]
pub struct TargetsArgs {
    /// Emit the compatibility matrix as JSON
    #[arg(long)]
    pub json: bool,
}

#[derive(Subcommand, Debug)]
pub enum CacheCommand {
    /// Print shared cache size and entry counts
    Status(CacheStatusArgs),
    /// Remove oldest cache files until the cache is under a size limit
    Prune(CachePruneArgs),
}

#[derive(Args, Debug)]
pub struct CacheArgs {
    #[command(subcommand)]
    pub command: CacheCommand,
}

#[derive(Args, Debug)]
pub struct CacheStatusArgs {
    /// Cache root to inspect (defaults to --cache-dir or the platform cache)
    #[arg(long)]
    pub root: Option<PathBuf>,
}

#[derive(Args, Debug)]
pub struct CachePruneArgs {
    /// Cache root to prune (defaults to --cache-dir or the platform cache)
    #[arg(long)]
    pub root: Option<PathBuf>,
    /// Maximum cache size, e.g. 10G, 2048M, or raw bytes
    #[arg(long)]
    pub max_size: Option<String>,
    /// Remove cache files older than this many days
    #[arg(long)]
    pub max_age_days: Option<u64>,
    /// Per-family cache budget as FAMILY=SIZE (SIZE like --max-size: 10G, 2048M,
    /// or raw bytes), repeatable. Example: --family-max msl=4G --family-max omc=2G
    #[arg(long = "family-max", action = ArgAction::Append)]
    pub family_max: Vec<String>,
    /// Preview removals without deleting files
    #[arg(long, default_value_t = false)]
    pub dry_run: bool,
}

/// Diagnostics flags shared by every compiler-driven command (`compile`,
/// `check`, `sim`). `--verbose` controls human-readable output; the `--trace*`
/// flags control structured tracing of internal compiler phases. There is no
/// `--debug`: `--trace` alone is the "default debug tracing" toggle.
// Diagnostics are low user-exposure: flatten this LAST in each leaf command so
// clap's declaration-order rendering places it below the primary options.
#[derive(Args, Debug, Clone, Default)]
pub struct DiagnosticsArgs {
    /// Verbose progress output. Help text comes from the `VERBOSE_HELP`
    /// constant; the `--trace` pointer is only included when that flag is
    /// actually visible.
    #[arg(short, long, help = VERBOSE_HELP)]
    pub verbose: bool,

    /// Structured tracing of internal compiler phases (requires --features
    /// tracing; off by default). The deep-dive counterpart to --verbose. Use
    /// `--trace` alone for the default phase filter, or `--trace=<FILTER>` for a
    /// custom tracing EnvFilter. See --help for the traceable phase targets.
    // Hidden from help in builds without the `tracing` feature: the flag is
    // still accepted (so it warns rather than erroring) but the help no longer
    // advertises a capability the binary doesn't have.
    #[arg(
        long,
        value_name = "FILTER",
        num_args = 0..=1,
        require_equals = true,
        default_missing_value = "",
        long_help = TRACE_LONG_HELP,
        hide = !cfg!(feature = "tracing"),
    )]
    pub trace: Option<String>,

    /// Also include phase timing/profiling targets in the trace output.
    #[arg(long = "trace-profile", hide = !cfg!(feature = "tracing"))]
    pub trace_profile: bool,
}

/// Model-selection options shared by every command that compiles a model
/// (`compile`, `check`, `sim`, `sim bench`) via `#[command(flatten)]`, so the
/// flags are described identically everywhere and can't drift. The model-file
/// positional is NOT here: it is required for `compile`/`check` but optional for
/// `sim`/`bench` (which can take it from `--config`), so each command declares
/// its own. Diagnostics are likewise flattened separately and last.
#[derive(Args, Debug, Clone, Default)]
pub struct ModelOptions {
    /// Main model/class to compile (auto-inferred when omitted)
    #[arg(short, long)]
    pub model: Option<String>,

    /// Source root path (file or directory). Can be specified multiple times.
    /// Example: --source-root ./packages/MSL --source-root helper.mo
    ///
    /// Environment: entries from the `MODELICAPATH` env var (`:`-separated) are
    /// appended after these, so libraries on `MODELICAPATH` resolve without an
    /// explicit flag.
    #[arg(long = "source-root", value_name = "PATH", action = ArgAction::Append)]
    pub source_roots: Vec<String>,
}

/// `compile`/`check` model input: the required model-file positional plus the
/// shared [`ModelOptions`].
#[derive(Args, Debug, Clone)]
pub struct ModelInputArgs {
    /// Modelica file to compile
    #[arg(name = "MODELICA_FILE")]
    pub model_file: String,

    #[command(flatten)]
    pub options: ModelOptions,
}

#[derive(Args, Debug)]
#[command(arg_required_else_help = true)]
pub struct CompileArgs {
    #[command(flatten)]
    pub input: ModelInputArgs,

    /// Dump an intermediate representation: `<stage>-mo` for Modelica or
    /// `<stage>-json` for JSON (stage = ast/flat/dae/solve). See possible values.
    #[arg(long, value_enum, conflicts_with = "target")]
    pub emit: Option<EmitTarget>,

    /// Code-generation target: a built-in target, a raw .jinja template, or a
    /// directory containing target.toml. Run `rumoca targets` to list them. For
    /// an IR/Modelica dump use --emit instead.
    #[arg(long, value_name = "TARGET")]
    pub target: Option<String>,

    /// Pick which IR a raw template `--target` receives (default dae). Only
    /// meaningful when --target is a `.jinja` file, e.g. `--target my.jinja
    /// --phase flat`.
    #[arg(long, value_enum, requires = "target")]
    pub phase: Option<CompilePhase>,

    /// Output path. For an `--emit` IR dump this is a file (defaults to stdout);
    /// for a `--target` codegen run it may be a file or a directory.
    #[arg(short, long)]
    pub output: Option<PathBuf>,

    /// Inspect the lowered model instead of emitting/compiling. `structure`
    /// needs no point; `eval`/`jacobian` take one via `--at` (same as `sim
    /// --inspect`).
    #[arg(long, value_enum, conflicts_with_all = ["emit", "target"])]
    pub inspect: Option<InspectKind>,

    /// Evaluation point for `--inspect eval|jacobian`: `<name=value,...@t>`
    /// (states by name; unset states keep their initial value; default t = 0).
    #[arg(long, value_name = "POINT", requires = "inspect")]
    pub at: Option<String>,

    /// Output format for `--inspect jacobian|objective-gradient` (`human`/`json`).
    #[arg(long, value_enum, default_value_t = InspectFormat::Human, requires = "inspect")]
    pub format: InspectFormat,

    /// Objective variable for `--inspect objective-gradient`: the state or solver
    /// algebraic whose steady gradient `d(objective)/dp` is computed.
    #[arg(long, value_name = "NAME", requires = "inspect")]
    pub objective: Option<String>,

    /// Gradient method for `--inspect objective-gradient` (`forward` or `adjoint`).
    #[arg(long, value_enum, default_value_t = GradMode::Forward, requires = "inspect")]
    pub grad_mode: GradMode,

    #[command(flatten)]
    pub diagnostics: DiagnosticsArgs,
}

/// Compiler stage whose IR a raw `.jinja` `--target` consumes (`compile --phase`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum CompilePhase {
    /// Abstract syntax tree (resolved).
    Ast,
    /// Flattened model.
    Flat,
    /// DAE system.
    Dae,
    /// Solver IR.
    Solve,
}

impl From<CompilePhase> for TemplateIr {
    fn from(phase: CompilePhase) -> Self {
        match phase {
            CompilePhase::Ast => TemplateIr::Ast,
            CompilePhase::Flat => TemplateIr::Flat,
            CompilePhase::Dae => TemplateIr::Dae,
            CompilePhase::Solve => TemplateIr::Solve,
        }
    }
}

/// An IR dump selected by `compile --emit`: a compiler stage plus output format.
/// The solver IR has no Modelica form, so `solve-mo` is intentionally absent.
#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum EmitTarget {
    #[value(name = "ast-mo")]
    AstMo,
    #[value(name = "ast-json")]
    AstJson,
    #[value(name = "flat-mo")]
    FlatMo,
    #[value(name = "flat-json")]
    FlatJson,
    #[value(name = "dae-mo")]
    DaeMo,
    #[value(name = "dae-json")]
    DaeJson,
    #[value(name = "solve-json")]
    SolveJson,
}

impl EmitTarget {
    fn phase(self) -> CompilePhase {
        match self {
            Self::AstMo | Self::AstJson => CompilePhase::Ast,
            Self::FlatMo | Self::FlatJson => CompilePhase::Flat,
            Self::DaeMo | Self::DaeJson => CompilePhase::Dae,
            Self::SolveJson => CompilePhase::Solve,
        }
    }

    fn is_json(self) -> bool {
        matches!(
            self,
            Self::AstJson | Self::FlatJson | Self::DaeJson | Self::SolveJson
        )
    }
}

// clap enforces "MODELICA_FILE or --config" natively (so the error carries the
// `try --help` hint), bare `rumoca sim` prints help, and the check/init/bench
// subcommands are exempt from the requirement.
#[derive(Args, Debug)]
#[command(
    arg_required_else_help = true,
    subcommand_negates_reqs = true,
    group(clap::ArgGroup::new("sim_source").required(true).multiple(true).args(["MODELICA_FILE", "config"])),
)]
pub struct SimCommandArgs {
    #[command(subcommand)]
    pub command: Option<SimSubcommand>,

    /// Modelica file to simulate directly, or override the config's `model.file`
    /// with --config.
    #[arg(name = "MODELICA_FILE")]
    pub model_file: Option<String>,

    /// Run a rumoca-scenario.toml scenario (`rumoca-scenario.toml` /
    /// `rumoca-scenario.<profile>.toml`) instead of a direct sim. Create one with
    /// `rumoca sim init`; validate with `sim check`.
    #[arg(short, long)]
    pub config: Option<String>,

    /// Shared model-selection options (--model / --source-root). For scenario
    /// runs (--config) these override the config's `model` / `source_roots`.
    #[command(flatten)]
    pub model_options: ModelOptions,

    /// Solver: auto (recommended), bdf (stiff/implicit, diffsol), esdirk34 or
    /// trbdf2 (implicit SDIRK tableaus for stiff DAEs, diffsol), or rk-like
    /// (explicit Runge-Kutta-style, non-stiff)
    #[arg(long, value_enum)]
    pub solver: Option<SimulateSolverMode>,

    /// Simulation end time. Direct runs default to 1.0; scenario runs use `sim.t_end`.
    #[arg(long)]
    pub t_end: Option<f64>,

    /// Optional fixed output interval (dt). If omitted, runtime chooses automatically.
    #[arg(long)]
    pub dt: Option<f64>,

    /// Absolute solver tolerance (overrides the model's `experiment(Tolerance=…)`
    /// and the backend default). Pair with --rtol to match a host's tolerance
    /// policy exactly (e.g. lunica's 1e-4 atol/rtol).
    #[arg(long)]
    pub atol: Option<f64>,

    /// Relative solver tolerance (overrides the model's `experiment(Tolerance=…)`
    /// and the backend default).
    #[arg(long)]
    pub rtol: Option<f64>,

    /// Output file path for simulation report (default: `<MODEL>_results.html`)
    #[arg(short, long)]
    pub output: Option<String>,

    /// Inspect the lowered model instead of simulating (see possible values
    /// below). `eval`/`jacobian` take a point via --at. Analyzes only.
    #[arg(long = "inspect", value_enum)]
    pub inspect: Option<InspectKind>,

    /// Evaluation point for `--inspect eval|jacobian`: `<name=value,...@t>`
    /// (states by name; unset states keep their initial value; time after `@`,
    /// default 0). With no --at, evaluates at the model's initial state (which
    /// also discovers the state names).
    #[arg(long = "at", value_name = "NAME=VALUE,...@T", requires = "inspect")]
    pub at: Option<String>,

    /// Output format for `--inspect jacobian|objective-gradient` (`human`/`json`).
    #[arg(long, value_enum, default_value_t = InspectFormat::Human, requires = "inspect")]
    pub format: InspectFormat,

    /// Objective variable for `--inspect objective-gradient`: the state or solver
    /// algebraic whose steady gradient `d(objective)/dp` is computed.
    #[arg(long, value_name = "NAME", requires = "inspect")]
    pub objective: Option<String>,

    /// Gradient method for `--inspect objective-gradient` (`forward` or `adjoint`).
    #[arg(long, value_enum, default_value_t = GradMode::Forward, requires = "inspect")]
    pub grad_mode: GradMode,

    #[command(flatten)]
    pub diagnostics: DiagnosticsArgs,
}

/// Which model inspection `rumoca sim --inspect` performs (all at the solver IR).
#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum InspectKind {
    /// Structural analysis: matching, BLT blocks, coupled SCCs, tearing.
    Structure,
    /// Evaluate solver values + state derivatives at a point; name non-finite.
    Eval,
    /// Dense state Jacobian at a point; flag singular columns / zero pivots.
    Jacobian,
    /// Steady-state gradient `d(objective)/dp` of a chosen variable (needs
    /// `--objective`); forward sensitivity by default, `--grad-mode adjoint` for
    /// reverse mode. Honors `--at` for the steady point and `--format json`.
    ObjectiveGradient,
}

/// How `--inspect objective-gradient` computes `d(objective)/dp`.
#[derive(Copy, Clone, Debug, PartialEq, Eq, ValueEnum)]
pub enum GradMode {
    /// Forward sensitivity (implicit-function theorem, dense per parameter).
    Forward,
    /// Reverse-mode adjoint (matrix-free; one solve for all parameters).
    Adjoint,
}

/// Output format for `--inspect jacobian` (other inspections are human-only).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, ValueEnum)]
pub enum InspectFormat {
    /// Human-readable text report (default).
    #[default]
    Human,
    /// Machine-readable JSON: state and parameter Jacobians with named
    /// rows/columns and dense matrices.
    Json,
}

#[derive(Subcommand, Debug)]
pub enum SimSubcommand {
    /// Validate a rumoca-scenario.toml scenario file without running it
    Check(SimCheckArgs),
    /// Print a commented rumoca-scenario.toml scenario template (e.g. `sim init > rumoca-scenario.toml`)
    Init,
    /// Benchmark compile, preparation, and hot simulation throughput
    Bench(sim_bench::SimBenchArgs),
}

// Accept the scenario as a positional (matching `sim` / `sim bench`, which take
// a positional file) or via -c/--config; clap requires exactly one.
#[derive(Args, Debug)]
#[command(group(
    clap::ArgGroup::new("sim_check_config").required(true).args(["config_positional", "config"])
))]
pub struct SimCheckArgs {
    /// rumoca-scenario.toml scenario to validate (positional form)
    #[arg(value_name = "CONFIG")]
    pub config_positional: Option<String>,

    /// rumoca-scenario.toml scenario to validate (flag form, same as the positional)
    #[arg(short, long, value_name = "CONFIG")]
    pub config: Option<String>,
}

impl SimCheckArgs {
    /// The scenario path from whichever form was supplied (clap's ArgGroup
    /// guarantees exactly one is set).
    pub fn config_path(&self) -> Result<&str> {
        self.config_positional
            .as_deref()
            .or(self.config.as_deref())
            .ok_or_else(|| anyhow::anyhow!("rumoca sim check requires CONFIG or --config"))
    }
}

impl SimCommandArgs {
    fn direct_input(&self) -> Result<ModelInputArgs> {
        let model_file = self
            .model_file
            .clone()
            .ok_or_else(|| anyhow::anyhow!("rumoca sim requires MODELICA_FILE or --config"))?;
        Ok(ModelInputArgs {
            model_file,
            options: self.model_options.clone(),
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum SimulateSolverMode {
    Auto,
    Bdf,
    /// ESDIRK34 — implicit SDIRK tableau on the diffsol path (stiff).
    Esdirk34,
    /// TR-BDF2 — implicit SDIRK tableau on the diffsol path (stiff).
    #[value(name = "trbdf2")]
    TrBdf2,
    #[value(name = "rk-like")]
    RkLike,
}

impl From<SimulateSolverMode> for SimSolverMode {
    fn from(value: SimulateSolverMode) -> Self {
        match value {
            SimulateSolverMode::Auto => SimSolverMode::Auto,
            // ESDIRK34 / TR-BDF2 are implicit tableaus served by the diffsol
            // (BDF-family) path; the specific tableau is carried by the solver
            // label into `SimOptions::diffsol_method`.
            SimulateSolverMode::Bdf | SimulateSolverMode::Esdirk34 | SimulateSolverMode::TrBdf2 => {
                SimSolverMode::Bdf
            }
            SimulateSolverMode::RkLike => SimSolverMode::RkLike,
        }
    }
}

impl SimulateSolverMode {
    pub(crate) fn as_label(self) -> &'static str {
        match self {
            SimulateSolverMode::Auto => "auto",
            SimulateSolverMode::Bdf => "bdf",
            SimulateSolverMode::Esdirk34 => "esdirk34",
            SimulateSolverMode::TrBdf2 => "trbdf2",
            SimulateSolverMode::RkLike => "rk-like",
        }
    }
}

#[derive(Args, Debug)]
pub struct LintArgs {
    /// Files or directories to lint. If empty, lints current directory.
    #[arg()]
    pub paths: Vec<PathBuf>,
    /// Minimum severity level to report.
    #[arg(long, value_enum)]
    pub min_level: Option<LintLevelArg>,
    /// Disable a lint rule (repeatable).
    #[arg(long = "disable-rule", action = ArgAction::Append)]
    pub disable_rules: Vec<String>,
    /// Treat warnings as errors.
    #[arg(long, default_value_t = false)]
    pub warnings_as_errors: bool,
    /// Maximum number of lint messages to print.
    #[arg(long)]
    pub max_messages: Option<usize>,
}

#[derive(Debug, Clone, Copy, ValueEnum)]
pub enum LintLevelArg {
    Help,
    Note,
    Warning,
    Error,
}

impl From<LintLevelArg> for LintLevel {
    fn from(value: LintLevelArg) -> Self {
        match value {
            LintLevelArg::Help => LintLevel::Help,
            LintLevelArg::Note => LintLevel::Note,
            LintLevelArg::Warning => LintLevel::Warning,
            LintLevelArg::Error => LintLevel::Error,
        }
    }
}

#[derive(Debug, Clone, Copy, ValueEnum)]
pub enum CompletionShell {
    Bash,
    Zsh,
    Fish,
    #[value(name = "powershell")]
    PowerShell,
}

/// Parse CLI `args` (without the leading program name) into a [`Cli`] using the
/// exact same clap command tree the binary uses, so callers (e.g. the Python
/// `cli` binding) parse arguments verbatim — there is no second arg spec to
/// drift. On a parse/`--help`/`--version` error, the clap-rendered message is
/// returned as the `Err` string.
pub fn parse_args(args: impl IntoIterator<Item = String>) -> std::result::Result<Cli, String> {
    Cli::try_parse_from(std::iter::once("rumoca".to_string()).chain(args))
        .map_err(|error| error.to_string())
}

/// Dispatch a parsed [`Cli`] exactly like the user-facing binary: print
/// summaries, write IR/codegen files, and run simulations to HTML/CSV reports.
///
/// This is the binary's `try_main`. Parsing has already happened
/// ([`Cli::parse`] in `main`); on error `main` renders the result through the
/// miette CLI hook.
pub fn run(cli: Cli) -> Result<()> {
    if let Some(dir) = cli.cache_dir {
        rumoca_compile::source_roots::set_cache_root_override(dir);
    }
    match cli.command {
        Commands::Compile(args) => run_compile(args),
        Commands::Sim(args) => run_sim(*args),
        Commands::Fmt(args) => fmt_cli::run_fmt(args),
        Commands::Lint(args) => run_lint(args),
        Commands::Completions { shell } => {
            print!("{}", completion_script(shell)?);
            Ok(())
        }
        Commands::Targets(args) => targets_cmd::run(args.json),
        Commands::Cache(args) => cache_cmd::run_cache(args),
    }
}

/// Build a miette [`Report`] for any CLI error, preferring the compiler's own
/// diagnostic codes when the error is a [`CompilerError`].
pub fn build_cli_error_report(error: &anyhow::Error) -> Report {
    if let Some(compiler_error) = error.downcast_ref::<CompilerError>() {
        return Report::new(compiler_error.clone());
    }
    let mut message = error.to_string();
    for cause in error.chain().skip(1) {
        message.push_str("\n\nCaused by:\n  ");
        message.push_str(&cause.to_string());
    }
    Report::msg(message)
}

pub fn build_source_diagnostic_report(
    diagnostic: &CommonDiagnostic,
    source_map: &SourceMap,
) -> Report {
    if !diagnostic.labels.is_empty() {
        return Report::new(diagnostic.to_miette_with_source_map(source_map));
    }

    let severity = match diagnostic.severity {
        DiagnosticSeverity::Error => Severity::Error,
        DiagnosticSeverity::Warning => Severity::Warning,
        DiagnosticSeverity::Note => Severity::Advice,
    };
    let message = diagnostic
        .code
        .as_ref()
        .map(|code| format!("[{code}] {}", diagnostic.message))
        .unwrap_or_else(|| diagnostic.message.clone());
    Report::new(MietteDiagnostic::new(message).with_severity(severity))
}

pub fn build_compile_failure_report(
    failure: &rumoca_compile::compile::ModelFailureDiagnostic,
    source_map: &rumoca_compile::compile::core::SourceMap,
) -> Report {
    let Some(label) = failure.primary_label.as_ref() else {
        return build_compile_failure_fallback_report(
            failure,
            "internal compiler diagnostic is missing a primary source label",
        );
    };
    let Some((file_name, source)) = source_map.get_source(label.span.source) else {
        return build_compile_failure_fallback_report(
            failure,
            "internal compiler diagnostic references a missing source file",
        );
    };
    let start = label.span.start.0.min(source.len());
    let end = label.span.end.0.max(start + 1).min(source.len());
    let label_text = label.message.clone().unwrap_or_else(|| "error".to_string());
    let display_name = display_source_name(file_name);
    let message = if let Some(code) = &failure.error_code {
        format!("\x1b[31m[{code}]\x1b[0m {}", failure.error)
    } else {
        failure.error.clone()
    };
    let diagnostic = MietteDiagnostic::new(message)
        .with_severity(Severity::Error)
        .with_label(LabeledSpan::new_primary_with_span(
            Some(label_text),
            (start, end.saturating_sub(start).max(1)),
        ));
    Report::new(diagnostic).with_source_code(NamedSource::new(display_name, source.to_string()))
}

fn build_compile_failure_fallback_report(
    failure: &rumoca_compile::compile::ModelFailureDiagnostic,
    internal_note: &str,
) -> Report {
    let message = if let Some(code) = &failure.error_code {
        format!("[{code}] {}\n\n{internal_note}", failure.error)
    } else {
        format!("{}\n\n{internal_note}", failure.error)
    };
    Report::new(MietteDiagnostic::new(message).with_severity(Severity::Error))
}

fn display_source_name(file_name: &str) -> String {
    let path = Path::new(file_name);
    if path.is_absolute() {
        return file_name.to_string();
    }
    std::env::current_dir()
        .ok()
        .map(|cwd| cwd.join(path).display().to_string())
        .unwrap_or_else(|| file_name.to_string())
}

#[cfg(feature = "runner")]
pub(crate) fn resolve_path(base: &Path, rel: &str) -> std::path::PathBuf {
    let p = Path::new(rel);
    if p.is_absolute() {
        p.to_path_buf()
    } else {
        base.join(p)
    }
}

#[cfg(feature = "runner")]
pub(crate) fn configured_model_name(
    cli_model: Option<&str>,
    config_model: Option<&rumoca_sim::runner::config::ModelConfig>,
    model_path: &Path,
) -> String {
    cli_model
        .map(str::to_string)
        .or_else(|| config_model.map(|m| m.name.clone()))
        .or_else(|| {
            model_path
                .file_stem()
                .and_then(|s| s.to_str())
                .map(String::from)
        })
        .unwrap_or_else(|| "Model".to_string())
}

#[cfg(feature = "runner")]
/// Resolve the viewer scene script (read from `[transport.http].scene`) and the
/// directory served at `/assets/...`. The asset dir defaults to the scene
/// script's parent, overridden by an explicit `[transport.http].asset_dir`
/// (both resolved relative to the config file), letting several examples share
/// one `/assets/` root (e.g. `examples/assets`).
fn resolve_scene_and_asset_dir(
    config: &rumoca_sim::runner::config::SimulationConfig,
    config_dir: &Path,
) -> Result<(Option<String>, Option<std::path::PathBuf>)> {
    let http = config.transport.as_ref().and_then(|t| t.http.as_ref());

    let mut scene_asset_dir = None;
    let scene_script = match http.and_then(|h| h.scene.clone()) {
        Some(rel) => {
            let scene_full = resolve_path(config_dir, &rel);
            scene_asset_dir = scene_full.parent().map(Path::to_path_buf);
            Some(
                std::fs::read_to_string(&scene_full)
                    .with_context(|| format!("Read scene script: {}", scene_full.display()))?,
            )
        }
        None => None,
    };

    if let Some(asset_dir) = http.and_then(|h| h.asset_dir.as_deref()) {
        scene_asset_dir = Some(resolve_path(config_dir, asset_dir));
    }

    Ok((scene_script, scene_asset_dir))
}

fn run_configured_simulation(args: SimCommandArgs) -> Result<()> {
    let config_path = args.config.as_deref().ok_or_else(|| {
        anyhow::anyhow!("rumoca sim requires MODELICA_FILE or --config <rumoca-scenario.toml>")
    })?;
    let config = rumoca_sim::runner::config::SimulationConfig::load(Path::new(config_path))
        .with_context(|| format!("Load simulation config: {config_path}"))?;

    let config_dir = parent_dir_or_current(Path::new(config_path));

    // Resolve model file: positional override > config [model].file.
    let model_path_str = args
        .model_file
        .clone()
        .or_else(|| config.model.as_ref().map(|m| m.file.clone()))
        .ok_or_else(|| {
            anyhow::anyhow!(
                "no model file specified: provide MODELICA_FILE or a [model].file in the config"
            )
        })?;
    let model_path = resolve_path(config_dir, &model_path_str);
    let model_source = std::fs::read_to_string(&model_path)
        .with_context(|| format!("Read model file: {}", model_path.display()))?;
    let model_name = configured_model_name(
        args.model_options.model.as_deref(),
        config.model.as_ref(),
        &model_path,
    );

    let source_roots = config
        .source_roots
        .iter()
        .map(|source_root| resolve_path(config_dir, source_root))
        .collect::<Vec<_>>();
    let solver_label = configured_solver_label(args.solver, config.sim.solver.as_ref());
    let solver_mode = SimSolverMode::from_external_name(&solver_label);

    if !config.is_interactive_runner() {
        let input = ModelInputArgs {
            model_file: model_path.to_string_lossy().to_string(),
            options: ModelOptions {
                model: Some(model_name.clone()),
                source_roots: source_roots
                    .iter()
                    .map(|path| path.to_string_lossy().to_string())
                    .collect(),
            },
        };
        init_debug_tracing(&args.diagnostics)?;
        let (result, compiled_model) =
            compile_dae_with_inferred_model(&input, args.diagnostics.verbose)?;
        let workspace_root = discover_workspace_root_for_model_file(&input.model_file);
        return run_simulation(SimulationRun {
            dae: result.dae.as_ref(),
            model: &compiled_model,
            t_end: configured_sim_t_end(args.t_end, config.sim.t_end),
            dt: Some(configured_sim_dt(args.dt, config.sim.dt)),
            atol: configured_sim_option(args.atol, config.sim.atol),
            rtol: configured_sim_option(args.rtol, config.sim.rtol),
            solver_mode,
            solver_label: &solver_label,
            output: args.output.as_deref().or(config.sim.output.as_deref()),
            workspace_root: workspace_root.as_deref(),
        });
    }

    let (scene_script, scene_asset_dir) = resolve_scene_and_asset_dir(&config, config_dir)?;

    Ok(rumoca_sim::runner::run(rumoca_sim::runner::SimArgs {
        model_source,
        model_path: Some(model_path),
        model_name,
        solver_mode,
        solver_label,
        atol: configured_sim_option(args.atol, config.sim.atol),
        rtol: configured_sim_option(args.rtol, config.sim.rtol),
        http_port: config.http_port(),
        ws_port: config.websocket_port(),
        config,
        scene_script,
        scene_asset_dir,
        source_roots,
        debug: trace_requests_viewer(&args.diagnostics),
    })?)
}

fn configured_solver_label(
    cli_solver: Option<SimulateSolverMode>,
    config_solver: Option<&String>,
) -> String {
    match cli_solver {
        Some(solver) => solver.as_label().to_string(),
        None => match config_solver {
            Some(solver) => solver.clone(),
            None => "auto".to_string(),
        },
    }
}

fn configured_sim_t_end(cli_t_end: Option<f64>, config_t_end: f64) -> f64 {
    match cli_t_end {
        Some(t_end) => t_end,
        None => config_t_end,
    }
}

fn configured_sim_dt(cli_dt: Option<f64>, config_dt: f64) -> f64 {
    match cli_dt {
        Some(dt) => dt,
        None => config_dt,
    }
}

fn configured_sim_option(cli_value: Option<f64>, config_value: Option<f64>) -> Option<f64> {
    cli_value.or(config_value)
}

#[cfg(feature = "runner")]
fn run_config_check(args: SimCheckArgs) -> Result<()> {
    let config_path = args.config_path()?;
    let _config = rumoca_sim::runner::config::SimulationConfig::load(Path::new(config_path))
        .with_context(|| format!("Load simulation config: {config_path}"))?;
    println!("{config_path}: config OK");
    Ok(())
}

#[cfg(feature = "runner")]
fn run_config_init() -> Result<()> {
    print!("{}", rumoca_sim::runner::CONFIG_TEMPLATE);
    Ok(())
}

fn run_compile(args: CompileArgs) -> Result<()> {
    init_debug_tracing(&args.diagnostics)?;
    if let Some(emit) = args.emit
        && matches!(emit.phase(), CompilePhase::Ast | CompilePhase::Flat)
    {
        let (artifact, model) = compile_early_ir_with_inferred_model(
            &args.input,
            emit.phase(),
            args.diagnostics.verbose,
        )?;
        return run_early_ir_dump(&artifact, &model, emit.is_json(), args.output);
    }

    let (result, model) = compile_with_inferred_model(&args.input, args.diagnostics.verbose)?;

    // Structural / point inspection of the lowered model (shares the `sim
    // --inspect` machinery). Structure is a compile-time artifact, so it belongs
    // on `compile` too; eval/jacobian take a point via `--at`.
    if let Some(kind) = args.inspect {
        let dae = &result.dae;
        let at = inspect_at_spec(args.at.as_deref());
        let solver = SimulateSolverMode::Auto;
        if matches!(args.format, InspectFormat::Json)
            && !matches!(kind, InspectKind::Jacobian | InspectKind::ObjectiveGradient)
        {
            anyhow::bail!(
                "`--format json` is only supported with `--inspect jacobian|objective-gradient`"
            );
        }
        return match kind {
            InspectKind::Structure => sim_inspect::run_structure_dump(dae, &model, solver.into()),
            InspectKind::Eval => sim_inspect::run_eval_at(dae, &model, at, solver.into()),
            InspectKind::Jacobian => sim_inspect::run_jacobian(
                dae,
                &model,
                at,
                solver.into(),
                matches!(args.format, InspectFormat::Json),
            ),
            InspectKind::ObjectiveGradient => sim_inspect::run_objective_gradient(
                dae,
                &model,
                at,
                args.objective.as_deref(),
                matches!(args.grad_mode, GradMode::Adjoint),
                matches!(args.format, InspectFormat::Json),
            ),
        };
    }

    match (args.emit, args.target) {
        // IR dump of one compiler stage (--emit conflicts with --target).
        (Some(emit), _) => run_ir_dump(&result, &model, emit.phase(), emit.is_json(), args.output),
        // Code-gen target; --phase (clap-required to accompany --target) only
        // picks the IR a raw .jinja template receives.
        (None, Some(target)) => target_manifest::compile_target(
            &result,
            &model,
            &target,
            args.output,
            args.phase.map(TemplateIr::from),
        ),
        // Neither: just report the compilation summary. There is no artifact to
        // write here, so `--output` would be a silent no-op — reject it instead
        // of lying by omission.
        (None, None) => {
            if let Some(path) = &args.output {
                bail!(
                    "--output `{}` has nothing to write without --emit or --target; \
                     add --emit <STAGE> to dump an IR or --target <TARGET> for codegen",
                    path.display()
                );
            }
            print_summary(&model, &result);
            Ok(())
        }
    }
}

pub(crate) enum EarlyIrArtifact {
    Ast(Box<ResolvedTree>),
    Flat(Box<FlatModel>),
}

fn run_early_ir_dump(
    artifact: &EarlyIrArtifact,
    model: &str,
    json: bool,
    output: Option<PathBuf>,
) -> Result<()> {
    let rendered = match (artifact, json) {
        (EarlyIrArtifact::Ast(resolved), true) => serde_json::to_string_pretty(resolved.inner())?,
        (EarlyIrArtifact::Ast(resolved), false) => {
            render_early_ir_as_modelica_ast(resolved, model)?
        }
        (EarlyIrArtifact::Flat(flat), true) => serde_json::to_string_pretty(flat)?,
        (EarlyIrArtifact::Flat(flat), false) => render_early_ir_as_modelica_flat(flat, model)?,
    };
    write_ir_dump(
        &rendered,
        match artifact {
            EarlyIrArtifact::Ast(_) => CompilePhase::Ast,
            EarlyIrArtifact::Flat(_) => CompilePhase::Flat,
        },
        json,
        output,
    )
}

/// Dump the IR at `phase` as JSON (`--json`) or Modelica (default) to `output`
/// or stdout.
fn run_ir_dump(
    result: &CompilationResult,
    model: &str,
    phase: CompilePhase,
    json: bool,
    output: Option<PathBuf>,
) -> Result<()> {
    let rendered = if json {
        result.to_ir_json(phase.into())?
    } else {
        render_ir_as_modelica(result, model, phase)?
    };

    write_ir_dump(&rendered, phase, json, output)
}

fn write_ir_dump(
    rendered: &str,
    phase: CompilePhase,
    json: bool,
    output: Option<PathBuf>,
) -> Result<()> {
    match output {
        Some(path) => {
            // An --emit dump is a single file; catch `-o <dir>` with a friendly
            // message instead of a bare OS error 21, matching `sim --output`'s
            // guard.
            if path.is_dir() {
                bail!(
                    "output path `{}` is a directory; --emit must write to a file \
                     (e.g. model.dae.mo)",
                    path.display()
                );
            }
            std::fs::write(&path, rendered).with_context(|| format!("write {}", path.display()))?;
            eprintln!(
                "wrote {:?} IR ({}) to {}",
                phase,
                if json { "json" } else { "modelica" },
                path.display()
            );
        }
        None => {
            print!("{rendered}");
            if !rendered.ends_with('\n') {
                println!();
            }
        }
    }
    Ok(())
}

fn render_early_ir_as_modelica_ast(resolved: &ResolvedTree, model: &str) -> Result<String> {
    let template = rumoca_compile::codegen::templates::builtin_template_source(
        "modelica",
        "modelica.mo.jinja",
    )
    .ok_or_else(|| anyhow::anyhow!("missing built-in modelica template"))?;
    let model_identifier = model.replace('.', "_");
    render_ast_template_with_name(resolved.inner(), template, &model_identifier).map_err(Into::into)
}

fn render_early_ir_as_modelica_flat(flat: &FlatModel, model: &str) -> Result<String> {
    let template = rumoca_compile::codegen::templates::builtin_template_source(
        "flat-modelica",
        "flat_modelica.mo.jinja",
    )
    .ok_or_else(|| anyhow::anyhow!("missing built-in flat-modelica template"))?;
    let model_identifier = model.replace('.', "_");
    render_flat_template_with_name(flat, template, &model_identifier).map_err(Into::into)
}

/// Render the IR at `phase` back to equivalent Modelica via the built-in
/// `*-modelica` templates.
fn render_ir_as_modelica(
    result: &CompilationResult,
    model: &str,
    phase: CompilePhase,
) -> Result<String> {
    let (target, template_file) = match phase {
        CompilePhase::Ast => ("modelica", "modelica.mo.jinja"),
        CompilePhase::Flat => ("flat-modelica", "flat_modelica.mo.jinja"),
        CompilePhase::Dae => ("dae-modelica", "dae_modelica.mo.jinja"),
        CompilePhase::Solve => {
            bail!("the solve IR has no Modelica form; use `--phase solve --json`")
        }
    };
    let template =
        rumoca_compile::codegen::templates::builtin_template_source(target, template_file)
            .ok_or_else(|| anyhow::anyhow!("missing built-in {target} template"))?;
    let model_identifier = model.replace('.', "_");
    result
        .render_template_str_with_name_and_ir(template, &model_identifier, phase.into())
        .map_err(Into::into)
}

fn run_sim(args: SimCommandArgs) -> Result<()> {
    match args.command {
        #[cfg(feature = "runner")]
        Some(SimSubcommand::Check(check_args)) => run_config_check(check_args),
        #[cfg(feature = "runner")]
        Some(SimSubcommand::Init) => run_config_init(),
        Some(SimSubcommand::Bench(bench_args)) => sim_bench::run_sim_bench(bench_args),
        #[cfg(not(feature = "runner"))]
        Some(_) => bail!(
            "this rumoca binary was built without interactive simulation config support; \
             rebuild with --features=runner"
        ),
        None if args.config.is_some() => {
            #[cfg(feature = "runner")]
            {
                run_configured_simulation(args)
            }
            #[cfg(not(feature = "runner"))]
            {
                let _ = args;
                bail!(
                    "this rumoca binary was built without interactive simulation config support; \
                     rebuild with --features=runner"
                )
            }
        }
        None => run_direct_simulation(args),
    }
}

fn run_direct_simulation(args: SimCommandArgs) -> Result<()> {
    let input = args.direct_input()?;
    init_debug_tracing(&args.diagnostics)?;
    let (result, model) = compile_dae_with_inferred_model(&input, args.diagnostics.verbose)?;
    if let Some(kind) = args.inspect {
        let solver = simulate_solver_or_auto(args.solver, result.experiment_solver.as_deref());
        let dae = result.dae.as_ref();
        let at = inspect_at_spec(args.at.as_deref());
        if matches!(args.format, InspectFormat::Json)
            && !matches!(kind, InspectKind::Jacobian | InspectKind::ObjectiveGradient)
        {
            anyhow::bail!(
                "`--format json` is only supported with `--inspect jacobian|objective-gradient`"
            );
        }
        return match kind {
            InspectKind::Structure => sim_inspect::run_structure_dump(dae, &model, solver.into()),
            InspectKind::Eval => sim_inspect::run_eval_at(dae, &model, at, solver.into()),
            InspectKind::Jacobian => sim_inspect::run_jacobian(
                dae,
                &model,
                at,
                solver.into(),
                matches!(args.format, InspectFormat::Json),
            ),
            InspectKind::ObjectiveGradient => sim_inspect::run_objective_gradient(
                dae,
                &model,
                at,
                args.objective.as_deref(),
                matches!(args.grad_mode, GradMode::Adjoint),
                matches!(args.format, InspectFormat::Json),
            ),
        };
    }
    let workspace_root = discover_workspace_root_for_model_file(&input.model_file);
    let solver = simulate_solver_or_auto(args.solver, result.experiment_solver.as_deref());
    run_simulation(SimulationRun {
        dae: result.dae.as_ref(),
        model: &model,
        t_end: direct_sim_t_end(args.t_end),
        dt: args.dt,
        atol: args.atol,
        rtol: args.rtol,
        solver_mode: solver.into(),
        solver_label: solver.as_label(),
        output: args.output.as_deref(),
        workspace_root: workspace_root.as_deref(),
    })
}

fn inspect_at_spec(at: Option<&str>) -> &str {
    at.unwrap_or_default()
}

fn simulate_solver_or_auto(
    solver: Option<SimulateSolverMode>,
    experiment_solver: Option<&str>,
) -> SimulateSolverMode {
    // An explicit `--solver` always wins.
    if let Some(solver) = solver {
        return solver;
    }
    // No `--solver`: honor the model's `experiment(Solver = "...")` annotation when it
    // names an explicit Runge-Kutta-family solver -- e.g. the PDE method-of-lines
    // examples annotated `Solver = "rk-like"`, which are artificial-compressibility /
    // explicit schemes the implicit BDF auto path cannot step. Non-RK annotations
    // (`dassl`, `cvode`, ...) already resolve to the implicit family and so match the
    // `Auto` fallback, leaving their behavior unchanged. This mirrors the WASM/docs
    // runner, which already resolves the annotated solver.
    match experiment_solver {
        Some(name) if SimSolverMode::from_external_name(name) == SimSolverMode::RkLike => {
            SimulateSolverMode::RkLike
        }
        _ => SimulateSolverMode::Auto,
    }
}

fn direct_sim_t_end(t_end: Option<f64>) -> f64 {
    t_end.unwrap_or(1.0)
}

fn run_lint(args: LintArgs) -> Result<()> {
    validate_explicit_target_paths(&args.paths)?;
    let paths = normalize_target_paths(&args.paths);
    let config_dir = first_path_config_dir(&paths);
    let base_options = lint_options_from_config(&config_dir)?;
    let cli_overrides = PartialLintOptions {
        min_level: args.min_level.map(Into::into),
        disabled_rules: (!args.disable_rules.is_empty()).then_some(args.disable_rules.clone()),
        warnings_as_errors: args.warnings_as_errors.then_some(true),
        max_messages: args.max_messages,
    };
    let options = base_options.merge(cli_overrides);

    let files = collect_modelica_files(&paths);
    if files.is_empty() {
        eprintln!("No .mo files found");
        return Ok(());
    }

    let mut total_messages = Vec::<LintMessage>::new();
    let mut io_errors = 0usize;
    for file in &files {
        let source = match std::fs::read_to_string(file) {
            Ok(v) => v,
            Err(e) => {
                eprintln!("Error reading {}: {e}", file.display());
                io_errors += 1;
                continue;
            }
        };
        let file_label = file.to_string_lossy().to_string();
        let messages = rumoca_tool_lint::lint(&source, &file_label, &options);
        total_messages.extend(messages);
    }

    let mut limited = total_messages;
    let total_message_count = limited.len();
    if limited.len() > options.max_messages {
        limited.truncate(options.max_messages);
    }
    for message in &limited {
        let suggestion = lint_suggestion_suffix(message.suggestion.as_deref());
        println!(
            "{}:{}:{} [{}] {} ({}){}",
            message.file,
            message.line,
            message.column,
            message.level,
            message.message,
            message.rule,
            suggestion
        );
    }

    let error_count = limited
        .iter()
        .filter(|m| m.level >= LintLevel::Error)
        .count()
        + io_errors;
    let warning_count = limited
        .iter()
        .filter(|m| m.level == LintLevel::Warning)
        .count();

    eprintln!(
        "{} files linted | {} messages (shown: {}) | errors={} warnings={} io_errors={}",
        files.len(),
        total_message_count,
        limited.len(),
        error_count,
        warning_count,
        io_errors
    );

    if error_count > 0 || (options.warnings_as_errors && warning_count > 0) {
        std::process::exit(1);
    }
    Ok(())
}

fn lint_options_from_config(config_dir: &Path) -> Result<LintOptions> {
    match rumoca_tool_lint::load_config_from_dir(config_dir)
        .map_err(|e| anyhow::anyhow!("Failed to load lint config: {e}"))?
    {
        Some(options) => Ok(options),
        None => Ok(LintOptions::default()),
    }
}

fn lint_suggestion_suffix(suggestion: Option<&str>) -> String {
    match suggestion {
        Some(suggestion) => format!(" | suggestion: {suggestion}"),
        None => String::new(),
    }
}

pub(crate) fn init_debug_tracing(diagnostics: &DiagnosticsArgs) -> Result<()> {
    let Some(filter) = trace_filter_from_diagnostics(diagnostics) else {
        return Ok(());
    };

    #[cfg(feature = "tracing")]
    {
        use tracing_subscriber::EnvFilter;
        let filter = EnvFilter::try_new(&filter)
            .map_err(|error| anyhow::anyhow!("invalid trace filter `{filter}`: {error}"))?;
        tracing_subscriber::fmt()
            .with_env_filter(filter)
            .with_target(true)
            .with_level(true)
            .init();
    }

    #[cfg(not(feature = "tracing"))]
    {
        let _ = filter;
        eprintln!("Warning: tracing flags require --features tracing");
        eprintln!("Rebuild with: cargo build --features tracing");
    }

    Ok(())
}

fn trace_filter_from_diagnostics(diagnostics: &DiagnosticsArgs) -> Option<String> {
    let mut filters = Vec::new();
    // `--trace` present with no value (Some("")) selects the default phase
    // filter; `--trace=<FILTER>` supplies a custom filter (with short phase
    // aliases expanded to their `rumoca_phase_*` targets).
    match diagnostics.trace.as_deref() {
        Some(filter) if !filter.is_empty() => filters.push(expand_trace_filter(filter)),
        Some(_) => filters.push(DEFAULT_DEBUG_TRACE_FILTER.to_string()),
        None => {}
    }
    if diagnostics.trace_profile {
        filters.push(PROFILE_TRACE_FILTER.to_string());
    }
    if filters.is_empty() {
        None
    } else {
        Some(filters.join(","))
    }
}

/// Whether the `--trace` filter names the `viewer` subsystem (e.g.
/// `--trace=viewer`). The interactive viewer is just another trace subsystem:
/// naming it enables the viewer's debug overlay/logging (this replaced the
/// separate `--viewer-debug` flag).
fn trace_requests_viewer(diagnostics: &DiagnosticsArgs) -> bool {
    diagnostics.trace.as_deref().is_some_and(|spec| {
        spec.split(',').map(str::trim).any(|token| {
            let name = token.split_once(':').map_or(token, |(name, _)| name.trim());
            name == "viewer"
        })
    })
}

/// Short aliases accepted in a `--trace` filter, mapping to tracing targets so
/// `--trace=dae:debug` is shorthand for `--trace=rumoca_phase_dae=debug`. Covers
/// the compiler phases plus a few high-traffic runtime diagnostic targets.
const TRACE_PHASE_ALIASES: &[(&str, &str)] = &[
    ("parse", "rumoca_phase_parse"),
    ("resolve", "rumoca_phase_resolve"),
    ("instantiate", "rumoca_phase_instantiate"),
    ("typecheck", "rumoca_phase_typecheck"),
    ("flatten", "rumoca_phase_flatten"),
    ("dae", "rumoca_phase_dae"),
    ("structural", "rumoca_phase_structural"),
    ("solve", "rumoca_phase_solve"),
    ("codegen", "rumoca_phase_codegen"),
    // Runtime diagnostic shortcuts (see TRACE_LONG_HELP for the full target set).
    ("bdf", "rumoca_solver_diffsol::bdf"),
    ("rk45", "rumoca_solver_rk45::eval"),
    ("hotpath", "rumoca_solver::hotpath"),
];

/// Expand short phase aliases in a `--trace` filter. Each comma-separated token
/// of the form `<phase>` or `<phase>:<level>` (e.g. `dae:debug`) expands to
/// `rumoca_phase_<phase>=<level>` (default level `debug`); any token that is not
/// a known alias passes through unchanged, so full tracing EnvFilter directives
/// (e.g. `rumoca_phase_dae::profile=debug`) still work.
fn expand_trace_filter(spec: &str) -> String {
    spec.split(',')
        .map(str::trim)
        .filter(|token| !token.is_empty())
        .map(|token| {
            let (name, level) = match token.split_once(':') {
                Some((name, level)) => (name.trim(), level.trim()),
                None => (token, "debug"),
            };
            match TRACE_PHASE_ALIASES.iter().find(|(alias, _)| *alias == name) {
                Some((_, target)) => format!("{target}={level}"),
                None => token.to_string(),
            }
        })
        .collect::<Vec<_>>()
        .join(",")
}

fn compile_with_inferred_model(
    args: &ModelInputArgs,
    verbose: bool,
) -> Result<(CompilationResult, String)> {
    ensure_model_file_readable(&args.model_file)?;
    let model = match &args.options.model {
        Some(model) => model.clone(),
        None => infer_model_name(&args.model_file)?,
    };

    let source_roots = merged_source_root_paths(&args.options.source_roots);

    let compiler = Compiler::new()
        .model(&model)
        .verbose(verbose)
        .source_roots(&source_roots);
    let result = compiler.compile_file(&args.model_file)?;
    Ok((result, model))
}

fn compile_early_ir_with_inferred_model(
    args: &ModelInputArgs,
    phase: CompilePhase,
    verbose: bool,
) -> Result<(EarlyIrArtifact, String)> {
    ensure_model_file_readable(&args.model_file)?;
    let model = match &args.options.model {
        Some(model) => model.clone(),
        None => infer_model_name(&args.model_file)?,
    };

    let source_roots = merged_source_root_paths(&args.options.source_roots);

    let compiler = Compiler::new()
        .model(&model)
        .verbose(verbose)
        .source_roots(&source_roots);
    let artifact = match phase {
        CompilePhase::Ast => {
            EarlyIrArtifact::Ast(Box::new(compiler.compile_file_ast(&args.model_file)?))
        }
        CompilePhase::Flat => {
            EarlyIrArtifact::Flat(Box::new(compiler.compile_file_flat(&args.model_file)?))
        }
        CompilePhase::Dae | CompilePhase::Solve => {
            bail!("internal error: early IR compile requested for {phase:?}")
        }
    };
    Ok((artifact, model))
}

pub(crate) fn compile_dae_with_inferred_model(
    args: &ModelInputArgs,
    verbose: bool,
) -> Result<(DaeCompilationResult, String)> {
    ensure_model_file_readable(&args.model_file)?;
    let model = match &args.options.model {
        Some(model) => model.clone(),
        None => infer_model_name(&args.model_file)?,
    };

    let source_roots = merged_source_root_paths(&args.options.source_roots);

    let compiler = Compiler::new()
        .model(&model)
        .verbose(verbose)
        .source_roots(&source_roots);
    let result = compiler.compile_file_dae(&args.model_file)?;
    Ok((result, model))
}

/// In-memory counterpart to [`compile_with_inferred_model`]: compile `source`
/// (keyed under `options`/`file_name`) without touching disk, inferring the
/// model when `options.model` is absent. Used by the value-returning entrypoints.
pub(crate) fn compile_str_with_inferred_model(
    source: &str,
    file_name: &str,
    options: &ModelOptions,
    verbose: bool,
) -> Result<(CompilationResult, String)> {
    let (compiler, model) = compiler_for_source(source, file_name, options, verbose)?;
    let result = compiler.compile_str(source, file_name)?;
    Ok((result, model))
}

/// In-memory counterpart to [`compile_early_ir_with_inferred_model`].
pub(crate) fn compile_str_early_ir_with_inferred_model(
    source: &str,
    file_name: &str,
    options: &ModelOptions,
    phase: CompilePhase,
    verbose: bool,
) -> Result<(EarlyIrArtifact, String)> {
    let (compiler, model) = compiler_for_source(source, file_name, options, verbose)?;
    let artifact = match phase {
        CompilePhase::Ast => {
            EarlyIrArtifact::Ast(Box::new(compiler.compile_str_ast(source, file_name)?))
        }
        CompilePhase::Flat => {
            EarlyIrArtifact::Flat(Box::new(compiler.compile_str_flat(source, file_name)?))
        }
        CompilePhase::Dae | CompilePhase::Solve => {
            bail!("internal error: early IR compile requested for {phase:?}")
        }
    };
    Ok((artifact, model))
}

/// In-memory counterpart to [`compile_dae_with_inferred_model`].
pub(crate) fn compile_str_dae_with_inferred_model(
    source: &str,
    file_name: &str,
    options: &ModelOptions,
    verbose: bool,
) -> Result<(DaeCompilationResult, String)> {
    let (compiler, model) = compiler_for_source(source, file_name, options, verbose)?;
    let result = compiler.compile_str_dae(source, file_name)?;
    Ok((result, model))
}

fn print_summary(model: &str, result: &CompilationResult) {
    println!("Compilation successful!");
    println!();
    println!("Model: {}", model);
    println!("States: {}", result.dae.variables.states.len());
    println!("Algebraics: {}", result.dae.variables.algebraics.len());
    println!("Parameters: {}", result.dae.variables.parameters.len());
    println!("Constants: {}", result.dae.variables.constants.len());
    println!("Inputs: {}", result.dae.variables.inputs.len());
    println!("Outputs: {}", result.dae.variables.outputs.len());
    println!();
    println!(
        "Continuous equations (f_x): {}",
        result.dae.continuous.equations.len()
    );
    println!(
        "Initial equations: {}",
        result.dae.initialization.equations.len()
    );
    println!();
    println!("Balance: {} (equations - unknowns)", result.balance());
    if result.is_balanced() {
        println!("Status: BALANCED");
    } else {
        println!("Status: UNBALANCED");
    }
    println!();
    println!(
        "Use `rumoca compile <file> --emit dae-mo` to dump the DAE IR as Modelica (or dae-json)"
    );
    println!("Use `rumoca compile <file> --emit solve-json` to dump the solver IR");
    println!(
        "Use `rumoca compile <file> --target <TARGET>` for code generation (`rumoca targets` to list)"
    );
    println!(
        "Use `rumoca sim <file> --inspect structure` for BLT/tearing/SCC analysis (also `--inspect eval|jacobian`)"
    );
}

struct SimulationRun<'a> {
    dae: &'a Dae,
    model: &'a str,
    t_end: f64,
    dt: Option<f64>,
    atol: Option<f64>,
    rtol: Option<f64>,
    solver_mode: SimSolverMode,
    solver_label: &'a str,
    output: Option<&'a str>,
    workspace_root: Option<&'a Path>,
}

fn run_simulation(run: SimulationRun<'_>) -> Result<()> {
    use rumoca_sim::simulate_with_diagnostics_auto_nan_trace;

    // Validate the report path before spending a full solve on it: `sim`'s
    // --output is the HTML report *file*, not a directory.
    if let Some(output) = run.output
        && Path::new(output).is_dir()
    {
        bail!(
            "output path `{output}` is a directory; sim --output must be a file (e.g. report.html)"
        );
    }

    let mut opts = SimOptions {
        t_end: run.t_end,
        dt: run.dt,
        solver_mode: run.solver_mode,
        // `--solver esdirk34` / `trbdf2` selects an implicit SDIRK tableau on
        // the diffsol path; other names leave the BDF default.
        diffsol_method: diffsol_method_for_solver_label(run.solver_label),
        ..SimOptions::default()
    };
    // Explicit --atol/--rtol override the backend default so a host's tolerance
    // policy can be reproduced exactly from the CLI.
    if let Some(atol) = run.atol {
        opts.atol = atol;
    }
    if let Some(rtol) = run.rtol {
        opts.rtol = rtol;
    }

    eprintln!("Simulating {} to t={}...", run.model, run.t_end);
    // On a non-finite-suggestive failure (e.g. a model divide-by-zero showing up
    // as "step size too small"), this re-runs once with NaN tracing so the
    // offending variable(s) are named for the user.
    let sim =
        simulate_with_diagnostics_auto_nan_trace(run.dae, &opts).map_err(anyhow::Error::msg)?;
    eprintln!(
        "Simulation complete: {} time points, {} variables",
        sim.times.len(),
        sim.names.len()
    );

    let out_path = match run.output {
        Some(p) => PathBuf::from(p),
        None => PathBuf::from(format!("{}_results.html", run.model)),
    };
    // `-o` must honor the requested format: writing HTML into a `.csv` file
    // silently hands the user a broken artifact.
    let extension = out_path
        .extension()
        .map(|ext| ext.to_string_lossy().to_ascii_lowercase());
    match extension.as_deref() {
        Some("html") | None => {}
        Some("csv") => {
            rumoca_sim::report::write_csv_results(&sim, &out_path)?;
            println!("{}", out_path.display());
            return Ok(());
        }
        Some(other) => {
            anyhow::bail!(
                "unsupported simulation output extension `.{other}` for `{}`: \
                 use `.html` for the report or `.csv` for raw results",
                out_path.display()
            );
        }
    }
    let request_summary = SimulationRequestSummary {
        solver: run.solver_label.to_string(),
        t_start: opts.t_start,
        t_end: opts.t_end,
        dt: opts.dt,
        rtol: opts.rtol,
        atol: opts.atol,
    };
    let metrics = SimulationRunMetrics::default();
    rumoca_sim::report::write_html_report(
        &sim,
        run.model,
        &out_path,
        &request_summary,
        &metrics,
        run.workspace_root,
    )?;
    // Human progress lines above went to stderr; the report path is the sole
    // stdout line so `report=$(rumoca sim model.mo)` captures just the artifact
    // path. Keep it bare/unlabeled for that reason.
    println!("{}", out_path.display());

    Ok(())
}

fn diffsol_method_for_solver_label(solver_label: &str) -> DiffsolMethod {
    DiffsolMethod::from_external_name(solver_label).unwrap_or_default()
}

// Structured, value-returning entrypoints (`compile_to_value`,
// `simulate_to_value`) for the Python `cli` binding live in a child module so
// `cli.rs` stays focused on argument parsing + the binary's print/write
// dispatch. The child reuses this module's private compute helpers via `super::`.
mod value;
pub use value::{compile_to_value, simulate_to_value};

#[cfg(test)]
#[path = "cli/cli_tests.rs"]
mod cli_tests;

#[cfg(test)]
#[path = "cli/cli_report_tests.rs"]
mod cli_report_tests;
