//! # Rumoca Modelica Compiler
//!
//! Command-line tool for compiling Modelica files into DAE representations.
//!
//! ## Usage
//!
//! ```sh
//! # Compile and output solve IR JSON
//! rumoca compile model.mo --model MyModel --emit solve-json
//!
//! # Compile and render a target.toml codegen target
//! rumoca compile model.mo --model MyModel --target sympy --output out
//!
//! # Verbose output
//! rumoca compile model.mo --model MyModel --emit dae-mo --verbose
//!
//! # Debug output (requires --features tracing)
//! rumoca compile model.mo --model MyModel --trace
//!
//! # Explicit tracing filter (requires --features tracing)
//! rumoca compile model.mo --model MyModel --trace=rumoca_phase_dae=debug
//! ```

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

mod cache_cmd;
mod fmt_cli;
mod fmu;
mod sim_bench;
mod sim_inspect;
mod target_manifest;
mod targets_cmd;

#[cfg(test)]
pub(crate) use sim_inspect::parse_eval_at_spec;

use std::ffi::OsString;
use std::path::Path;
use std::path::PathBuf;

#[cfg(feature = "runner")]
use anyhow::Context;
use anyhow::{Result, bail};
use clap::{ArgAction, Args, Parser, Subcommand, ValueEnum};
use fmt_cli::FmtArgs;
use miette::{
    GraphicalTheme, LabeledSpan, MietteDiagnostic, MietteHandlerOpts, NamedSource, Report, Severity,
};
use rumoca::{CompilationResult, Compiler, CompilerError, DaeCompilationResult, TemplateIr};
use rumoca_compile::{
    codegen::{render_ast_template_with_name, render_flat_template_with_name},
    compile::core::{Diagnostic as CommonDiagnostic, DiagnosticSeverity, SourceMap},
    compile::{Dae, FlatModel, ResolvedTree, Session, SessionConfig},
    project::{write_last_simulation_result_for_model, write_simulation_run},
};
use rumoca_sim::{SimOptions, SimSolverMode};
use rumoca_sim::{SimulationRequestSummary, SimulationRunMetrics};
use rumoca_tool_lint::{LintLevel, LintMessage, PartialLintOptions};
use walkdir::WalkDir;

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
struct Cli {
    #[command(subcommand)]
    command: Commands,

    /// Override the on-disk cache root (default: the platform cache directory).
    /// Applies to every subcommand. `cache status`/`prune` also accept `--root`.
    // Declared after the subcommand so it sorts last in each subcommand's --help
    // (a low-importance global that shouldn't crowd the primary options).
    #[arg(long, global = true, value_name = "DIR")]
    cache_dir: Option<PathBuf>,
}

#[derive(Subcommand, Debug)]
enum Commands {
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
struct TargetsArgs {
    /// Emit the compatibility matrix as JSON
    #[arg(long)]
    json: bool,
}

#[derive(Subcommand, Debug)]
pub(crate) enum CacheCommand {
    /// Print shared cache size and entry counts
    Status(CacheStatusArgs),
    /// Remove oldest cache files until the cache is under a size limit
    Prune(CachePruneArgs),
}

#[derive(Args, Debug)]
pub(crate) struct CacheArgs {
    #[command(subcommand)]
    pub(crate) command: CacheCommand,
}

#[derive(Args, Debug)]
pub(crate) struct CacheStatusArgs {
    /// Cache root to inspect (defaults to --cache-dir or the platform cache)
    #[arg(long)]
    pub(crate) root: Option<PathBuf>,
}

#[derive(Args, Debug)]
pub(crate) struct CachePruneArgs {
    /// Cache root to prune (defaults to --cache-dir or the platform cache)
    #[arg(long)]
    pub(crate) root: Option<PathBuf>,
    /// Maximum cache size, e.g. 10G, 2048M, or raw bytes
    #[arg(long)]
    pub(crate) max_size: Option<String>,
    /// Remove cache files older than this many days
    #[arg(long)]
    pub(crate) max_age_days: Option<u64>,
    /// Per-family cache budget as FAMILY=SIZE (SIZE like --max-size: 10G, 2048M,
    /// or raw bytes), repeatable. Example: --family-max msl=4G --family-max omc=2G
    #[arg(long = "family-max", action = ArgAction::Append)]
    pub(crate) family_max: Vec<String>,
    /// Preview removals without deleting files
    #[arg(long, default_value_t = false)]
    pub(crate) dry_run: bool,
}

/// Diagnostics flags shared by every compiler-driven command (`compile`,
/// `check`, `sim`). `--verbose` controls human-readable output; the `--trace*`
/// flags control structured tracing of internal compiler phases. There is no
/// `--debug`: `--trace` alone is the "default debug tracing" toggle.
// Diagnostics are low user-exposure: flatten this LAST in each leaf command so
// clap's declaration-order rendering places it below the primary options.
#[derive(Args, Debug, Clone, Default)]
pub(crate) struct DiagnosticsArgs {
    /// Verbose progress output. See [`VERBOSE_HELP`]; the `--trace` pointer is
    /// only included when that flag is actually visible.
    #[arg(short, long, help = VERBOSE_HELP)]
    pub(crate) verbose: bool,

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
    pub(crate) trace: Option<String>,

    /// Also include phase timing/profiling targets in the trace output.
    #[arg(long = "trace-profile", hide = !cfg!(feature = "tracing"))]
    pub(crate) trace_profile: bool,
}

/// Model-selection options shared by every command that compiles a model
/// (`compile`, `check`, `sim`, `sim bench`) via `#[command(flatten)]`, so the
/// flags are described identically everywhere and can't drift. The model-file
/// positional is NOT here: it is required for `compile`/`check` but optional for
/// `sim`/`bench` (which can take it from `--config`), so each command declares
/// its own. Diagnostics are likewise flattened separately and last.
#[derive(Args, Debug, Clone, Default)]
pub(crate) struct ModelOptions {
    /// Main model/class to compile (auto-inferred when omitted)
    #[arg(short, long)]
    pub(crate) model: Option<String>,

    /// Source root path (file or directory). Can be specified multiple times.
    /// Example: --source-root ./packages/MSL --source-root helper.mo
    ///
    /// Environment: entries from the `MODELICAPATH` env var (`:`-separated) are
    /// appended after these, so libraries on `MODELICAPATH` resolve without an
    /// explicit flag.
    #[arg(long = "source-root", value_name = "PATH", action = ArgAction::Append)]
    pub(crate) source_roots: Vec<String>,
}

/// `compile`/`check` model input: the required model-file positional plus the
/// shared [`ModelOptions`].
#[derive(Args, Debug, Clone)]
struct ModelInputArgs {
    /// Modelica file to compile
    #[arg(name = "MODELICA_FILE")]
    model_file: String,

    #[command(flatten)]
    options: ModelOptions,
}

#[derive(Args, Debug)]
#[command(arg_required_else_help = true)]
struct CompileArgs {
    #[command(flatten)]
    input: ModelInputArgs,

    /// Dump an intermediate representation: `<stage>-mo` for Modelica or
    /// `<stage>-json` for JSON (stage = ast/flat/dae/solve). See possible values.
    #[arg(long, value_enum, conflicts_with = "target")]
    emit: Option<EmitTarget>,

    /// Code-generation target: a built-in target, a raw .jinja template, or a
    /// directory containing target.toml. Run `rumoca targets` to list them. For
    /// an IR/Modelica dump use --emit instead.
    #[arg(long, value_name = "TARGET")]
    target: Option<String>,

    /// Pick which IR a raw template `--target` receives (default dae). Only
    /// meaningful when --target is a `.jinja` file, e.g. `--target my.jinja
    /// --phase flat`.
    #[arg(long, value_enum, requires = "target")]
    phase: Option<CompilePhase>,

    /// Output path. For an `--emit` IR dump this is a file (defaults to stdout);
    /// for a `--target` codegen run it may be a file or a directory.
    #[arg(short, long)]
    output: Option<PathBuf>,

    /// Inspect the lowered model instead of emitting/compiling. `structure`
    /// needs no point; `eval`/`jacobian` take one via `--at` (same as `sim
    /// --inspect`).
    #[arg(long, value_enum, conflicts_with_all = ["emit", "target"])]
    inspect: Option<InspectKind>,

    /// Evaluation point for `--inspect eval|jacobian`: `<name=value,...@t>`
    /// (states by name; unset states keep their initial value; default t = 0).
    #[arg(long, value_name = "POINT", requires = "inspect")]
    at: Option<String>,

    #[command(flatten)]
    diagnostics: DiagnosticsArgs,
}

/// Compiler stage whose IR a raw `.jinja` `--target` consumes (`compile --phase`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
enum CompilePhase {
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
enum EmitTarget {
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
struct SimCommandArgs {
    #[command(subcommand)]
    command: Option<SimSubcommand>,

    /// Modelica file to simulate directly, or override [model].file with --config.
    #[arg(name = "MODELICA_FILE")]
    model_file: Option<String>,

    /// Run a rum.toml scenario (rum.toml / rum.<profile>.toml) instead of a
    /// direct sim. Create one with `rumoca sim init`; validate with `sim check`.
    #[arg(short, long)]
    config: Option<String>,

    /// Shared model-selection options (--model / --source-root). For scenario
    /// runs (--config) these override the config's [model] / source_roots.
    #[command(flatten)]
    model_options: ModelOptions,

    /// Solver: auto (recommended), bdf (stiff/implicit, diffsol), or rk-like
    /// (explicit Runge-Kutta-style, non-stiff)
    #[arg(long, value_enum)]
    solver: Option<SimulateSolverMode>,

    /// Simulation end time. Direct runs default to 1.0; scenario runs use [sim].t_end.
    #[arg(long)]
    t_end: Option<f64>,

    /// Optional fixed output interval (dt). If omitted, runtime chooses automatically.
    #[arg(long)]
    dt: Option<f64>,

    /// Output file path for simulation report (default: <MODEL>_results.html)
    #[arg(short, long)]
    output: Option<String>,

    /// Inspect the lowered model instead of simulating (see possible values
    /// below). `eval`/`jacobian` take a point via --at. Analyzes only.
    #[arg(long = "inspect", value_enum)]
    inspect: Option<InspectKind>,

    /// Evaluation point for `--inspect eval|jacobian`: `<name=value,...@t>`
    /// (states by name; unset states keep their initial value; time after `@`,
    /// default 0). With no --at, evaluates at the model's initial state (which
    /// also discovers the state names).
    #[arg(long = "at", value_name = "NAME=VALUE,...@T", requires = "inspect")]
    at: Option<String>,

    #[command(flatten)]
    diagnostics: DiagnosticsArgs,
}

/// Which model inspection `rumoca sim --inspect` performs (all at the solver IR).
#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
enum InspectKind {
    /// Structural analysis: matching, BLT blocks, coupled SCCs, tearing.
    Structure,
    /// Evaluate solver values + state derivatives at a point; name non-finite.
    Eval,
    /// Dense state Jacobian at a point; flag singular columns / zero pivots.
    Jacobian,
}

#[derive(Subcommand, Debug)]
enum SimSubcommand {
    /// Validate a rum.toml scenario file without running it
    Check(SimCheckArgs),
    /// Print a commented rum.toml scenario template (e.g. `sim init > rum.toml`)
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
struct SimCheckArgs {
    /// rum.toml scenario to validate (positional form)
    #[arg(value_name = "CONFIG")]
    config_positional: Option<String>,

    /// rum.toml scenario to validate (flag form, same as the positional)
    #[arg(short, long, value_name = "CONFIG")]
    config: Option<String>,
}

impl SimCheckArgs {
    /// The scenario path from whichever form was supplied (clap's ArgGroup
    /// guarantees exactly one is set).
    fn config_path(&self) -> &str {
        self.config_positional
            .as_deref()
            .or(self.config.as_deref())
            .expect("clap ArgGroup requires a config")
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
enum SimulateSolverMode {
    Auto,
    Bdf,
    #[value(name = "rk-like")]
    RkLike,
}

impl From<SimulateSolverMode> for SimSolverMode {
    fn from(value: SimulateSolverMode) -> Self {
        match value {
            SimulateSolverMode::Auto => SimSolverMode::Auto,
            SimulateSolverMode::Bdf => SimSolverMode::Bdf,
            SimulateSolverMode::RkLike => SimSolverMode::RkLike,
        }
    }
}

impl SimulateSolverMode {
    fn as_label(self) -> &'static str {
        match self {
            SimulateSolverMode::Auto => "auto",
            SimulateSolverMode::Bdf => "bdf",
            SimulateSolverMode::RkLike => "rk-like",
        }
    }
}

#[derive(Args, Debug)]
struct LintArgs {
    /// Files or directories to lint. If empty, lints current directory.
    #[arg()]
    paths: Vec<PathBuf>,
    /// Minimum severity level to report.
    #[arg(long, value_enum)]
    min_level: Option<LintLevelArg>,
    /// Disable a lint rule (repeatable).
    #[arg(long = "disable-rule", action = ArgAction::Append)]
    disable_rules: Vec<String>,
    /// Treat warnings as errors.
    #[arg(long, default_value_t = false)]
    warnings_as_errors: bool,
    /// Maximum number of lint messages to print.
    #[arg(long)]
    max_messages: Option<usize>,
}

#[derive(Debug, Clone, Copy, ValueEnum)]
enum LintLevelArg {
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
enum CompletionShell {
    Bash,
    Zsh,
    Fish,
    #[value(name = "powershell")]
    PowerShell,
}

fn main() {
    // Restore the default SIGPIPE disposition: Rust installs SIG_IGN at startup,
    // which turns a closed reader (`rumoca compile … | head`) into a BrokenPipe
    // error that the `println!`-per-line paths surface as a panic (exit 101).
    // With the default disposition the process terminates quietly on SIGPIPE
    // like a normal Unix tool. No-op on non-Unix.
    sigpipe::reset();
    install_cli_miette_hook();
    if let Err(error) = try_main() {
        print_cli_error(&error);
        std::process::exit(1);
    }
}

fn install_cli_miette_hook() {
    static INIT: std::sync::Once = std::sync::Once::new();
    INIT.call_once(|| {
        let _ = miette::set_hook(Box::new(|_| {
            let mut theme = GraphicalTheme::unicode();
            let strong_error = theme.styles.error.bold();
            theme.styles.highlights = vec![strong_error, strong_error, strong_error];
            theme.characters.error = String::new();
            Box::new(MietteHandlerOpts::new().graphical_theme(theme).build())
        }));
    });
}

fn try_main() -> Result<()> {
    let cli = Cli::parse();
    if let Some(dir) = cli.cache_dir {
        rumoca_compile::source_roots::set_cache_root_override(dir);
    }
    match cli.command {
        Commands::Compile(args) => run_compile(args),
        Commands::Sim(args) => run_sim(*args),
        Commands::Fmt(args) => fmt_cli::run_fmt(args),
        Commands::Lint(args) => run_lint(args),
        Commands::Completions { shell } => {
            print!("{}", completion_script(shell));
            Ok(())
        }
        Commands::Targets(args) => targets_cmd::run(args.json),
        Commands::Cache(args) => cache_cmd::run_cache(args),
    }
}

fn print_cli_error(error: &anyhow::Error) {
    if let Some(CompilerError::CompileDiagnosticsError {
        failures,
        source_map,
        ..
    }) = error.downcast_ref::<CompilerError>()
        && print_compile_failures(failures, source_map.as_deref())
    {
        return;
    }
    if let Some(CompilerError::SourceDiagnosticsError {
        diagnostics,
        source_map,
        ..
    }) = error.downcast_ref::<CompilerError>()
        && print_source_diagnostics(diagnostics, source_map)
    {
        return;
    }
    #[cfg(feature = "runner")]
    if let Some(runner_error) = error.downcast_ref::<rumoca_sim::runner::RunnerError>()
        && print_runner_diagnostic_error(runner_error)
    {
        return;
    }
    eprintln!("{:?}", build_cli_error_report(error));
}

#[cfg(feature = "runner")]
fn print_runner_diagnostic_error(error: &rumoca_sim::runner::RunnerError) -> bool {
    let Some((diagnostic, source_map)) = error.source_diagnostic() else {
        return false;
    };
    print_source_diagnostics(&[diagnostic], &source_map)
}

fn build_cli_error_report(error: &anyhow::Error) -> Report {
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

fn print_compile_failures(
    failures: &[rumoca_compile::compile::ModelFailureDiagnostic],
    source_map: Option<&rumoca_compile::compile::core::SourceMap>,
) -> bool {
    let Some(source_map) = source_map else {
        return false;
    };

    let mut printed_any = false;
    for failure in failures {
        if printed_any {
            eprintln!();
        }
        let report = build_compile_failure_report(failure, source_map);
        eprintln!("{report:?}");
        printed_any = true;
    }
    printed_any
}

fn print_source_diagnostics(diagnostics: &[CommonDiagnostic], source_map: &SourceMap) -> bool {
    if diagnostics.is_empty() {
        return false;
    }

    let mut printed_any = false;
    for diagnostic in diagnostics {
        if printed_any {
            eprintln!();
        }
        let report = build_source_diagnostic_report(diagnostic, source_map);
        eprintln!("{report:?}");
        printed_any = true;
    }
    printed_any
}

fn build_source_diagnostic_report(diagnostic: &CommonDiagnostic, source_map: &SourceMap) -> Report {
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

fn build_compile_failure_report(
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
fn resolve_path(base: &Path, rel: &str) -> std::path::PathBuf {
    let p = Path::new(rel);
    if p.is_absolute() {
        p.to_path_buf()
    } else {
        base.join(p)
    }
}

#[cfg(feature = "runner")]
fn configured_model_name(
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
fn run_configured_simulation(args: SimCommandArgs) -> Result<()> {
    let config_path = args.config.as_deref().ok_or_else(|| {
        anyhow::anyhow!("rumoca sim requires MODELICA_FILE or --config <rum.toml>")
    })?;
    let config = rumoca_sim::runner::config::SimulationConfig::load(Path::new(config_path))
        .with_context(|| format!("Load simulation config: {config_path}"))?;

    let config_dir = Path::new(config_path).parent().unwrap_or(Path::new("."));

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
    let solver_label = args
        .solver
        .map(|solver| solver.as_label().to_string())
        .or_else(|| config.sim.solver.clone())
        .unwrap_or_else(|| "auto".to_string());
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
            t_end: args.t_end.unwrap_or(config.sim.t_end),
            dt: args.dt.or(Some(config.sim.dt)),
            solver_mode,
            solver_label: &solver_label,
            output: args.output.as_deref().or(config.sim.output.as_deref()),
            workspace_root: workspace_root.as_deref(),
        });
    }

    // Scene comes from [transport.http].scene in the scenario config.
    let scene_ref = config
        .transport
        .as_ref()
        .and_then(|t| t.http.as_ref())
        .and_then(|h| h.scene.clone());
    let mut scene_asset_dir = None;
    let scene_script = match scene_ref {
        Some(rel) => {
            let scene_path = Path::new(&rel);
            let scene_full = if scene_path.is_absolute() {
                scene_path.to_path_buf()
            } else {
                config_dir.join(scene_path)
            };
            scene_asset_dir = scene_full.parent().map(Path::to_path_buf);
            Some(
                std::fs::read_to_string(&scene_full)
                    .with_context(|| format!("Read scene script: {}", scene_full.display()))?,
            )
        }
        None => None,
    };

    // An explicit `[transport.http].asset_dir` (resolved relative to the config
    // file) overrides the scene-parent default, letting several examples share
    // one `/assets/` root (e.g. examples/assets).
    if let Some(asset_dir) = config
        .transport
        .as_ref()
        .and_then(|t| t.http.as_ref())
        .and_then(|h| h.asset_dir.as_deref())
    {
        let dir = Path::new(asset_dir);
        let full = if dir.is_absolute() {
            dir.to_path_buf()
        } else {
            config_dir.join(dir)
        };
        scene_asset_dir = Some(full);
    }

    Ok(rumoca_sim::runner::run(rumoca_sim::runner::SimArgs {
        model_source,
        model_path: Some(model_path),
        model_name,
        solver_mode,
        http_port: config.http_port(),
        ws_port: config.websocket_port(),
        config,
        scene_script,
        scene_asset_dir,
        source_roots,
        debug: trace_requests_viewer(&args.diagnostics),
    })?)
}

#[cfg(feature = "runner")]
fn run_config_check(args: SimCheckArgs) -> Result<()> {
    let config_path = args.config_path();
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
        let at = args.at.as_deref().unwrap_or("");
        let solver = SimulateSolverMode::Auto;
        return match kind {
            InspectKind::Structure => sim_inspect::run_structure_dump(dae, &model, solver.into()),
            InspectKind::Eval => sim_inspect::run_eval_at(dae, &model, at, solver.into()),
            InspectKind::Jacobian => sim_inspect::run_jacobian(dae, &model, at, solver.into()),
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

enum EarlyIrArtifact {
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
        let solver = args.solver.unwrap_or(SimulateSolverMode::Auto);
        let dae = result.dae.as_ref();
        let at = args.at.as_deref().unwrap_or("");
        return match kind {
            InspectKind::Structure => sim_inspect::run_structure_dump(dae, &model, solver.into()),
            InspectKind::Eval => sim_inspect::run_eval_at(dae, &model, at, solver.into()),
            InspectKind::Jacobian => sim_inspect::run_jacobian(dae, &model, at, solver.into()),
        };
    }
    let workspace_root = discover_workspace_root_for_model_file(&input.model_file);
    let solver = args.solver.unwrap_or(SimulateSolverMode::Auto);
    run_simulation(SimulationRun {
        dae: result.dae.as_ref(),
        model: &model,
        t_end: args.t_end.unwrap_or(1.0),
        dt: args.dt,
        solver_mode: solver.into(),
        solver_label: solver.as_label(),
        output: args.output.as_deref(),
        workspace_root: workspace_root.as_deref(),
    })
}

fn run_lint(args: LintArgs) -> Result<()> {
    validate_explicit_target_paths(&args.paths)?;
    let paths = normalize_target_paths(&args.paths);
    let config_dir = first_path_config_dir(&paths);
    let base_options = rumoca_tool_lint::load_config_from_dir(&config_dir)
        .map_err(|e| anyhow::anyhow!("Failed to load lint config: {e}"))?
        .unwrap_or_default();
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
        let suggestion = message
            .suggestion
            .as_ref()
            .map(|s| format!(" | suggestion: {s}"))
            .unwrap_or_default();
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

/// Report a missing or non-file model path identically for every command,
/// before either the `--model`-present (compiler E003) or inferred (read in
/// `infer_model_name`) path can produce a less helpful bare `io::Error`.
fn ensure_model_file_readable(model_file: &str) -> Result<()> {
    let path = Path::new(model_file);
    if path.is_dir() {
        bail!("model file `{model_file}` is a directory, not a Modelica file");
    }
    if !path.exists() {
        bail!("model file `{model_file}` not found");
    }
    // A rum.toml scenario fed to the model path otherwise parses as Modelica and
    // dies on `[` with a dead-end syntax error; point at the right command
    // instead.
    if looks_like_scenario_config(path) {
        bail!(
            "`{model_file}` looks like a rum.toml scenario, not a Modelica model; \
             did you mean `rumoca sim check -c {model_file}` (or `rumoca sim -c {model_file}`)?"
        );
    }
    Ok(())
}

/// Whether `path` is a rum.toml scenario rather than a Modelica model: a `.toml`
/// extension, or a first non-blank line of `[rumoca]`/`[model]`.
fn looks_like_scenario_config(path: &Path) -> bool {
    if path.extension().is_some_and(|ext| ext == "toml") {
        return true;
    }
    let Ok(contents) = std::fs::read_to_string(path) else {
        return false;
    };
    contents
        .lines()
        .map(str::trim)
        .find(|line| !line.is_empty())
        .is_some_and(|line| line == "[rumoca]" || line == "[model]")
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

fn compile_dae_with_inferred_model(
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

fn split_path_list(raw: Option<OsString>) -> Vec<String> {
    let Some(raw) = raw else {
        return Vec::new();
    };
    std::env::split_paths(&raw)
        .filter(|entry| !entry.as_os_str().is_empty())
        .map(|entry| entry.to_string_lossy().to_string())
        .collect()
}

fn merged_source_root_paths(cli_paths: &[String]) -> Vec<String> {
    let env_modelica_paths = split_path_list(std::env::var_os("MODELICAPATH"));
    merge_source_root_path_sources(cli_paths, &env_modelica_paths)
}

fn merge_source_root_path_sources(
    cli_paths: &[String],
    env_modelica_paths: &[String],
) -> Vec<String> {
    let mut merged: Vec<String> = Vec::new();
    for path in cli_paths.iter().chain(env_modelica_paths.iter()) {
        let trimmed = path.trim();
        if trimmed.is_empty() {
            continue;
        }
        let key = if cfg!(windows) {
            trimmed.to_ascii_lowercase()
        } else {
            trimmed.to_string()
        };
        let already_seen = merged.iter().any(|existing| {
            let existing_key = if cfg!(windows) {
                existing.to_ascii_lowercase()
            } else {
                existing.clone()
            };
            existing_key == key
        });
        if !already_seen {
            merged.push(trimmed.to_string());
        }
    }
    merged
}

fn infer_model_name(model_file: &str) -> Result<String> {
    let source = std::fs::read_to_string(model_file)
        .with_context(|| format!("read model file `{model_file}`"))?;
    let mut session = Session::new(SessionConfig::default());
    let parse_error = session.update_document(model_file, &source);
    let definition = session
        .get_document(model_file)
        .map(|doc| doc.best_effort().clone())
        .ok_or_else(|| anyhow::anyhow!("failed to load document '{}'", model_file))?;

    let top_level_names = definition
        .classes
        .iter()
        .filter_map(|(name, class)| {
            let class_kind = class.class_type.as_str();
            if class_kind == "model" || class_kind == "block" || class_kind == "class" {
                Some(name.clone())
            } else {
                None
            }
        })
        .collect::<Vec<_>>();

    let mut candidates = rumoca_compile::parsing::collect_model_names(&definition);
    candidates.sort();
    candidates.dedup();
    if candidates.is_empty() {
        if parse_error.is_some()
            && let Some((diagnostics, source_map)) =
                session.document_parse_diagnostics_with_source_map(model_file)
        {
            return Err(anyhow::Error::new(CompilerError::SourceDiagnosticsError {
                summary: format!("failed to infer model from '{}'", model_file),
                diagnostics,
                source_map: Box::new(source_map),
            }));
        }
        bail!(
            "No compilable model/block/class candidates found in '{}'; pass --model <NAME>.",
            model_file
        );
    }

    if top_level_names.len() == 1
        && let Some(model) = choose_single_candidate_by_suffix(&candidates, &top_level_names[0])
    {
        return Ok(model);
    }

    if candidates.len() == 1 {
        return Ok(candidates[0].clone());
    }

    let file_stem = Path::new(model_file)
        .file_stem()
        .and_then(|stem| stem.to_str())
        .unwrap_or_default();
    if !file_stem.is_empty()
        && let Some(model) = choose_single_candidate_by_suffix(&candidates, file_stem)
    {
        return Ok(model);
    }

    if parse_error.is_some()
        && let Some((diagnostics, source_map)) =
            session.document_parse_diagnostics_with_source_map(model_file)
    {
        return Err(anyhow::Error::new(CompilerError::SourceDiagnosticsError {
            summary: format!("failed to infer model from '{}'", model_file),
            diagnostics,
            source_map: Box::new(source_map),
        }));
    }

    let preview = candidates
        .iter()
        .take(15)
        .cloned()
        .collect::<Vec<_>>()
        .join(", ");
    bail!(
        "Unable to infer model from '{}'. Candidates: {}{} . Pass --model <NAME>.",
        model_file,
        preview,
        if candidates.len() > 15 { ", ..." } else { "" }
    );
}

fn normalize_target_paths(paths: &[PathBuf]) -> Vec<PathBuf> {
    if paths.is_empty() {
        vec![PathBuf::from(".")]
    } else {
        paths.to_vec()
    }
}

/// Validate paths the user passed explicitly to `lint`/`fmt`. An explicit path
/// that doesn't exist, or an explicit file that isn't `.mo`, is an error — not a
/// silent "No .mo files found" / exit 0, which reads as a clean pass and lets a
/// `lint`/`fmt` CI gate no-op on a typo. The default `.`-walk
/// (empty `paths`) is exempt: finding nothing there is a legitimate clean run.
fn validate_explicit_target_paths(paths: &[PathBuf]) -> Result<()> {
    for path in paths {
        if !path.exists() {
            bail!("path `{}` not found", path.display());
        }
        if path.is_file() && path.extension().is_none_or(|ext| ext != "mo") {
            bail!(
                "`{}` is not a Modelica (.mo) file; pass a .mo file or a directory",
                path.display()
            );
        }
    }
    Ok(())
}

fn first_path_config_dir(paths: &[PathBuf]) -> PathBuf {
    paths
        .first()
        .map(|p| {
            if p.is_dir() {
                p.clone()
            } else {
                p.parent().unwrap_or(Path::new(".")).to_path_buf()
            }
        })
        .unwrap_or_else(|| PathBuf::from("."))
}

/// Directories never worth walking for source `.mo` files (build output, VCS,
/// caches, deps). Skipping them keeps `lint`/`fmt` on the current directory fast
/// — otherwise a repo's `target/` (which can hold the whole MSL stdlib) makes it
/// appear to hang.
fn is_ignored_walk_dir(name: &str) -> bool {
    matches!(
        name,
        "target" | ".git" | ".rumoca" | "node_modules" | ".venv" | "dist" | "build"
    )
}

fn collect_modelica_files(paths: &[PathBuf]) -> Vec<PathBuf> {
    let mut out = Vec::<PathBuf>::new();
    for path in paths {
        if path.is_file() {
            if path.extension().is_some_and(|ext| ext == "mo") {
                out.push(path.to_path_buf());
            }
            continue;
        }
        let walk = WalkDir::new(path).into_iter().filter_entry(|entry| {
            // Always descend the explicitly-passed root; prune ignored subdirs.
            !entry.file_type().is_dir()
                || entry.depth() == 0
                || entry
                    .file_name()
                    .to_str()
                    .is_none_or(|name| !is_ignored_walk_dir(name))
        });
        for entry in walk.filter_map(Result::ok) {
            let candidate = entry.path();
            if candidate.is_file() && candidate.extension().is_some_and(|ext| ext == "mo") {
                out.push(candidate.to_path_buf());
            }
        }
    }
    out.sort();
    out.dedup();
    out
}

fn choose_single_candidate_by_suffix(candidates: &[String], suffix: &str) -> Option<String> {
    let mut matches = candidates
        .iter()
        .filter(|candidate| last_segment(candidate) == suffix || candidate.as_str() == suffix)
        .collect::<Vec<_>>();
    if matches.len() == 1 {
        return Some(matches[0].clone());
    }
    if matches.is_empty() {
        return None;
    }

    matches.sort_by_key(|candidate| candidate.matches('.').count());
    let min_depth = matches[0].matches('.').count();
    let min_matches = matches
        .into_iter()
        .filter(|candidate| candidate.matches('.').count() == min_depth)
        .collect::<Vec<_>>();
    if min_matches.len() == 1 {
        Some(min_matches[0].clone())
    } else {
        None
    }
}

fn last_segment(qualified_name: &str) -> &str {
    rumoca_core::top_level_last_segment(qualified_name)
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

    let opts = SimOptions {
        t_end: run.t_end,
        dt: run.dt,
        solver_mode: run.solver_mode,
        ..SimOptions::default()
    };

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
    let request_summary = SimulationRequestSummary {
        solver: run.solver_label.to_string(),
        t_start: opts.t_start,
        t_end: opts.t_end,
        dt: opts.dt,
        rtol: opts.rtol,
        atol: opts.atol,
    };
    let metrics = SimulationRunMetrics::default();
    let report = rumoca_sim::report::write_html_report(
        &sim,
        run.model,
        &out_path,
        &request_summary,
        &metrics,
        run.workspace_root,
    )?;
    if let Some(workspace_root) = run.workspace_root {
        write_last_simulation_result_for_model(
            workspace_root,
            run.model,
            &report.payload,
            Some(&report.metrics),
        )?;
        write_simulation_run(
            workspace_root,
            run.model,
            &report.payload,
            Some(&report.metrics),
            Some(&report.views),
        )?;
    }
    // Human progress lines above went to stderr; the report path is the sole
    // stdout line so `report=$(rumoca sim model.mo)` captures just the artifact
    // path. Keep it bare/unlabeled for that reason.
    println!("{}", out_path.display());

    Ok(())
}

mod main_helpers;
use main_helpers::{completion_script, discover_workspace_root_for_model_file};

#[cfg(test)]
mod main_tests;

#[cfg(test)]
mod main_report_tests;
