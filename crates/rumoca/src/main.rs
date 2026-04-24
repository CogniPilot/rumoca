//! # Rumoca Modelica Compiler
//!
//! Command-line tool for compiling Modelica files into DAE representations.
//!
//! ## Usage
//!
//! ```sh
//! # Compile and output JSON
//! rumoca compile model.mo --model MyModel --json
//!
//! # Compile and render with template
//! rumoca compile model.mo --model MyModel --template-file template.j2
//!
//! # Verbose output
//! rumoca compile model.mo --model MyModel --json --verbose
//!
//! # Debug output (requires --features tracing)
//! rumoca check model.mo --model MyModel --debug
//! ```

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

mod sim_report;

use std::ffi::OsString;
use std::path::Path;
use std::path::PathBuf;

#[cfg(feature = "sim-fb")]
use anyhow::Context;
use anyhow::{Result, bail};
use clap::{ArgAction, Args, Parser, Subcommand, ValueEnum};
use miette::{
    GraphicalTheme, LabeledSpan, MietteDiagnostic, MietteHandlerOpts, NamedSource, Report, Severity,
};
use rumoca::{CompilationResult, Compiler, CompilerError};
use rumoca_session::{
    compile::core::{Diagnostic as CommonDiagnostic, DiagnosticSeverity, SourceMap},
    compile::{Session, SessionConfig},
    project::{
        ProjectFileMoveHint, resync_model_sidecars_with_move_hints,
        write_last_simulation_result_for_model, write_simulation_run,
    },
};
use rumoca_sim::results_web::{SimulationRequestSummary, SimulationRunMetrics};
use rumoca_tool_lint::{LintLevel, LintMessage, PartialLintOptions};
use walkdir::WalkDir;

/// Git version string
const VERSION: &str = env!("CARGO_PKG_VERSION");

#[derive(Parser, Debug)]
#[command(name = "rumoca")]
#[command(version = VERSION)]
#[command(about = "Rumoca Modelica Compiler", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand, Debug)]
enum Commands {
    /// Compile a Modelica file
    Compile(CompileArgs),
    /// Simulate a Modelica file and generate an HTML report
    Simulate(SimulateArgs),
    /// Compile and print balance/summary information
    Check(CheckArgs),
    /// Export an FMU (Functional Mock-up Unit)
    ExportFmu(ExportFmuArgs),
    /// Export embedded C (.h and .c files)
    ExportEmbeddedC(ExportEmbeddedCArgs),
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
    /// Manage workspace-side Rumoca project sidecars
    Project(ProjectArgs),
    /// Run FlatBuffer-based SIL simulation with 3D viewer
    #[cfg(feature = "sim-fb")]
    SimFb(SimFbArgs),
}

#[derive(Subcommand, Debug)]
enum ProjectCommand {
    /// Synchronize model sidecar associations (including file-move remaps)
    Sync(ProjectSyncArgs),
}

#[derive(Args, Debug)]
struct ProjectArgs {
    #[command(subcommand)]
    command: ProjectCommand,
}

#[derive(Args, Debug)]
struct ProjectSyncArgs {
    /// Workspace root (defaults to current directory)
    #[arg(long)]
    workspace: Option<PathBuf>,
    /// Preview changes without writing
    #[arg(long, default_value_t = false)]
    dry_run: bool,
    /// Remove orphan sidecars while syncing
    #[arg(long, default_value_t = false)]
    prune_orphans: bool,
    /// Optional explicit move hint formatted as OLD->NEW (repeatable)
    #[arg(long = "move", action = ArgAction::Append)]
    moves: Vec<String>,
}

#[cfg(feature = "sim-fb")]
#[derive(Args, Debug)]
struct SimFbArgs {
    /// Modelica file containing the plant model
    #[arg(name = "MODELICA_FILE")]
    model_file: String,

    /// Model name to simulate (auto-inferred when omitted)
    #[arg(short, long)]
    model: Option<String>,

    /// Path to SIL config TOML (schema paths, UDP ports, field routing)
    #[arg(long)]
    config: String,

    /// Path to a scene script (.js) for 3D visualization (default: quadrotor)
    #[arg(long)]
    scene: Option<String>,

    /// Enable debug overlays, L/Y log download, and P render log in browser
    #[arg(long)]
    debug: bool,

    /// HTTP server port for the 3D viewer
    #[arg(long, default_value = "8080")]
    http_port: u16,

    /// WebSocket proxy port
    #[arg(long, default_value = "8081")]
    ws_port: u16,
}

#[derive(Args, Debug, Clone)]
struct ModelInputArgs {
    /// Modelica file to parse
    #[arg(name = "MODELICA_FILE")]
    model_file: String,

    /// Main model/class to compile (auto-inferred when omitted)
    #[arg(short, long)]
    model: Option<String>,

    /// Source root path (file or directory). Can be specified multiple times.
    /// Example: --source-root ./packages/MSL --source-root helper.mo
    #[arg(long = "source-root", action = ArgAction::Append)]
    source_roots: Vec<String>,

    /// Verbose output
    #[arg(short, long)]
    verbose: bool,

    /// Enable debug tracing (requires --features tracing)
    #[arg(long)]
    debug: bool,
}

#[derive(Args, Debug)]
struct CompileArgs {
    #[command(flatten)]
    input: ModelInputArgs,

    /// Export to JSON (native, recommended)
    #[arg(long, conflicts_with_all = ["template_file", "backend"])]
    json: bool,

    /// Built-in backend for code generation
    #[arg(short, long, value_enum, conflicts_with = "template_file")]
    backend: Option<Backend>,

    /// Template file for custom export (advanced)
    #[arg(short, long)]
    template_file: Option<String>,

    /// Render templates from a structurally prepared DAE instead of raw compile output
    #[arg(long, requires = "template_file")]
    template_prepared: bool,
}

#[derive(Debug, Clone, Copy, ValueEnum)]
enum Backend {
    /// CasADi SX — scalar symbolic expressions (Python)
    #[value(name = "casadi-sx")]
    CasadiSx,
    /// CasADi MX — matrix symbolic with vector variables (Python)
    #[value(name = "casadi-mx")]
    CasadiMx,
    /// SymPy symbolic model (Python)
    Sympy,
    /// FMI 2.0 Model Exchange C source
    Fmi2,
    /// ONNX computational graph (Python)
    Onnx,
    /// DAE Modelica (classified variables and split equations)
    #[value(name = "dae-modelica")]
    DaeModelica,
    /// Flat Modelica
    #[value(name = "flat-modelica")]
    FlatModelica,
    /// Julia ModelingToolkit (Julia)
    #[value(name = "julia-mtk")]
    JuliaMtk,
}

impl Backend {
    fn template(self) -> &'static str {
        use rumoca_session::runtime::templates;
        match self {
            Backend::CasadiSx => templates::CASADI_SX,
            Backend::CasadiMx => templates::CASADI_MX,

            Backend::Sympy => templates::SYMPY,
            Backend::Onnx => templates::ONNX,
            Backend::Fmi2 => templates::FMI2_MODEL,
            Backend::DaeModelica => templates::DAE_MODELICA,
            Backend::FlatModelica => templates::FLAT_MODELICA,
            Backend::JuliaMtk => templates::JULIA_MTK,
        }
    }
}

#[derive(Args, Debug)]
struct SimulateArgs {
    #[command(flatten)]
    input: ModelInputArgs,

    /// Simulation end time (default: 1.0)
    #[arg(long, default_value_t = 1.0)]
    t_end: f64,

    /// Optional fixed output interval (dt). If omitted, runtime chooses automatically.
    #[arg(long)]
    dt: Option<f64>,

    /// Solver mode (auto, bdf, rk-like)
    #[arg(long, value_enum, default_value_t = SimulateSolverMode::Auto)]
    solver: SimulateSolverMode,

    /// Output file path for simulation report (default: <MODEL>_results.html)
    #[arg(short, long)]
    output: Option<String>,
}

#[derive(Debug, Clone, Copy, ValueEnum)]
enum SimulateSolverMode {
    Auto,
    Bdf,
    #[value(name = "rk-like")]
    RkLike,
}

impl From<SimulateSolverMode> for rumoca_session::runtime::SimSolverMode {
    fn from(value: SimulateSolverMode) -> Self {
        match value {
            SimulateSolverMode::Auto => rumoca_session::runtime::SimSolverMode::Auto,
            SimulateSolverMode::Bdf => rumoca_session::runtime::SimSolverMode::Bdf,
            SimulateSolverMode::RkLike => rumoca_session::runtime::SimSolverMode::RkLike,
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
struct CheckArgs {
    #[command(flatten)]
    input: ModelInputArgs,
}

#[derive(Debug, Clone, Copy, ValueEnum)]
enum FmiVersionArg {
    #[value(name = "2")]
    Fmi2,
    #[value(name = "3")]
    Fmi3,
}

#[derive(Args, Debug)]
struct ExportFmuArgs {
    #[command(flatten)]
    input: ModelInputArgs,

    /// Output directory for generated FMU sources (default: <MODEL>.fmu/)
    #[arg(short, long)]
    output: Option<PathBuf>,

    /// FMI version to target (2 or 3, default: 2)
    #[arg(long, value_enum, default_value = "2")]
    fmi_version: FmiVersionArg,

    /// Skip compiling and packaging the .fmu archive (only generate sources)
    #[arg(long, default_value_t = false)]
    no_build: bool,
}

#[derive(Args, Debug)]
struct ExportEmbeddedCArgs {
    #[command(flatten)]
    input: ModelInputArgs,

    /// Output directory for generated .h and .c files (default: <MODEL>/)
    #[arg(short, long)]
    output: Option<PathBuf>,
}

#[derive(Args, Debug)]
struct FmtArgs {
    /// Files or directories to format. If empty, formats current directory.
    #[arg()]
    paths: Vec<PathBuf>,
    /// Check formatting without writing changes.
    #[arg(long, default_value_t = false)]
    check: bool,
    /// Number of spaces per indentation level.
    #[arg(long)]
    indent_size: Option<usize>,
    /// Use tabs instead of spaces.
    #[arg(
        long,
        num_args = 0..=1,
        default_missing_value = "true",
        value_parser = clap::builder::BoolishValueParser::new()
    )]
    use_tabs: Option<bool>,
    /// Formatting profile.
    #[arg(long, value_enum)]
    profile: Option<FmtProfileArg>,
}

#[derive(Debug, Clone, Copy, ValueEnum)]
enum FmtProfileArg {
    Msl,
    Canonical,
}

impl From<FmtProfileArg> for rumoca_tool_fmt::FormatProfile {
    fn from(value: FmtProfileArg) -> Self {
        match value {
            FmtProfileArg::Msl => rumoca_tool_fmt::FormatProfile::Msl,
            FmtProfileArg::Canonical => rumoca_tool_fmt::FormatProfile::Canonical,
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
    match cli.command {
        Commands::Compile(args) => run_compile(args),
        Commands::Simulate(args) => run_simulate(args),
        Commands::Check(args) => run_check(args),
        Commands::ExportFmu(args) => run_export_fmu(args),
        Commands::ExportEmbeddedC(args) => run_export_embedded_c(args),
        Commands::Fmt(args) => run_fmt(args),
        Commands::Lint(args) => run_lint(args),
        Commands::Completions { shell } => {
            print!("{}", completion_script(shell));
            Ok(())
        }
        Commands::Project(args) => run_project(args),
        #[cfg(feature = "sim-fb")]
        Commands::SimFb(args) => run_sim_fb(args),
    }
}

fn print_cli_error(error: &anyhow::Error) {
    if let Some(CompilerError::CompileDiagnosticsError {
        failures,
        source_map,
        ..
    }) = error.downcast_ref::<CompilerError>()
        && print_compile_failures(failures, source_map.as_ref())
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
    eprintln!("{:?}", build_cli_error_report(error));
}

fn build_cli_error_report(error: &anyhow::Error) -> Report {
    if let Some(compiler_error) = error.downcast_ref::<CompilerError>() {
        return Report::new(compiler_error.clone());
    }
    Report::msg(error.to_string())
}

fn print_compile_failures(
    failures: &[rumoca_session::compile::ModelFailureDiagnostic],
    source_map: Option<&rumoca_session::compile::core::SourceMap>,
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
    failure: &rumoca_session::compile::ModelFailureDiagnostic,
    source_map: &rumoca_session::compile::core::SourceMap,
) -> Report {
    let label = failure
        .primary_label
        .as_ref()
        .unwrap_or_else(|| panic!("compile failure must include a primary label"));
    let (file_name, source) = source_map
        .get_source(label.span.source)
        .unwrap_or_else(|| panic!("compile failure label source must exist in source map"));
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

fn run_project(args: ProjectArgs) -> Result<()> {
    match args.command {
        ProjectCommand::Sync(sync_args) => run_project_sync(sync_args),
    }
}

fn run_project_sync(args: ProjectSyncArgs) -> Result<()> {
    let workspace_root = args.workspace.unwrap_or(std::env::current_dir()?);
    let moved_hints = parse_move_hints(&args.moves)?;
    let report = resync_model_sidecars_with_move_hints(
        &workspace_root,
        &[],
        &moved_hints,
        args.dry_run,
        args.prune_orphans,
    )?;
    println!(
        "Project sync: discovered={} parsed_files={} parse_failures={} remapped={} move_hints_applied={} removed_orphans={} dry_run={} prune_orphans={}",
        report.discovered_models,
        report.parsed_model_files,
        report.parse_failures,
        report.remapped_models,
        report.applied_move_hints,
        report.removed_orphans,
        report.dry_run,
        report.prune_orphans,
    );
    for remap in &report.remaps {
        println!(
            "  remap: {} -> {} ({})",
            remap.from_model, remap.to_model, remap.reason
        );
    }
    for orphan in &report.orphans {
        println!(
            "  orphan: {} [{}] ({})",
            orphan.qualified_name, orphan.uuid, orphan.reason
        );
    }
    Ok(())
}

fn parse_move_hints(raw_moves: &[String]) -> Result<Vec<ProjectFileMoveHint>> {
    let mut out = Vec::new();
    for item in raw_moves {
        let Some((old_raw, new_raw)) = item.split_once("->") else {
            bail!("Invalid --move value '{}': expected OLD->NEW", item);
        };
        let old_path = old_raw.trim();
        let new_path = new_raw.trim();
        if old_path.is_empty() || new_path.is_empty() {
            bail!(
                "Invalid --move value '{}': both OLD and NEW must be non-empty",
                item
            );
        }
        out.push(ProjectFileMoveHint {
            old_path: old_path.to_string(),
            new_path: new_path.to_string(),
        });
    }
    Ok(out)
}

#[cfg(feature = "sim-fb")]
fn run_sim_fb(args: SimFbArgs) -> Result<()> {
    let model_source = std::fs::read_to_string(&args.model_file)
        .with_context(|| format!("Read model file: {}", args.model_file))?;

    let model_name = args.model.unwrap_or_else(|| {
        Path::new(&args.model_file)
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("Model")
            .to_string()
    });

    let config = rumoca_sim_fb::config::SilConfig::load(Path::new(&args.config))
        .with_context(|| format!("Load SIL config: {}", args.config))?;

    // Load scene script if provided
    let scene_script = match args.scene {
        Some(path) => Some(
            std::fs::read_to_string(&path)
                .with_context(|| format!("Read scene script: {}", path))?,
        ),
        None => None,
    };

    rumoca_sim_fb::run(rumoca_sim_fb::SimFbArgs {
        model_source,
        model_name,
        config,
        http_port: args.http_port,
        ws_port: args.ws_port,
        scene_script,
        debug: args.debug,
    })
}

fn run_compile(args: CompileArgs) -> Result<()> {
    init_debug_tracing(args.input.debug);
    let (result, model) = compile_with_inferred_model(&args.input)?;
    if args.json {
        println!("{}", result.to_json()?);
        return Ok(());
    }
    if let Some(backend) = args.backend {
        let rendered =
            result.render_template_str_prepared_with_name(backend.template(), &model, true)?;
        print!("{rendered}");
        return Ok(());
    }
    if let Some(template_file) = args.template_file {
        if args.template_prepared {
            print!("{}", result.render_template_prepared(&template_file, true)?);
        } else {
            print!("{}", result.render_template(&template_file)?);
        }
        return Ok(());
    }
    print_summary(&model, &result);
    Ok(())
}

fn run_simulate(args: SimulateArgs) -> Result<()> {
    init_debug_tracing(args.input.debug);
    let (result, model) = compile_with_inferred_model(&args.input)?;
    let workspace_root = discover_workspace_root_for_model_file(&args.input.model_file);
    run_simulation(
        &result,
        &model,
        args.t_end,
        args.dt,
        args.solver,
        args.output.as_deref(),
        workspace_root.as_deref(),
    )
}

fn run_check(args: CheckArgs) -> Result<()> {
    init_debug_tracing(args.input.debug);
    let (result, model) = compile_with_inferred_model(&args.input)?;
    print_summary(&model, &result);
    Ok(())
}

fn run_export_fmu(args: ExportFmuArgs) -> Result<()> {
    use rumoca_session::runtime::{fmi2_templates, fmi3_templates};
    use std::fs;

    init_debug_tracing(args.input.debug);
    let (result, model) = compile_with_inferred_model(&args.input)?;

    // Sanitize model identifier (replace dots with underscores for C compatibility)
    let model_identifier = model.replace('.', "_");

    // Select templates based on FMI version
    let (xml_template, c_template, fmi_label) = match args.fmi_version {
        FmiVersionArg::Fmi2 => (
            fmi2_templates::FMI2_MODEL_DESCRIPTION,
            fmi2_templates::FMI2_MODEL,
            "FMI 2.0",
        ),
        FmiVersionArg::Fmi3 => (
            fmi3_templates::FMI3_MODEL_DESCRIPTION,
            fmi3_templates::FMI3_MODEL,
            "FMI 3.0",
        ),
    };

    eprintln!("Exporting {} FMU for {}", fmi_label, model_identifier);

    let out_dir = args
        .output
        .unwrap_or_else(|| PathBuf::from(format!("{}.fmu", model_identifier)));

    // Create FMU directory structure
    let sources_dir = out_dir.join("sources");
    fs::create_dir_all(&sources_dir)?;

    // Render and write modelDescription.xml from the same prepared DAE used by the
    // native backend so value references stay aligned with fmi2Get/SetReal.
    let xml =
        result.render_template_str_prepared_with_name(xml_template, &model_identifier, true)?;
    let xml_path = out_dir.join("modelDescription.xml");
    fs::write(&xml_path, &xml)?;
    eprintln!("  wrote {}", xml_path.display());

    // Render and write C source (uses prepared DAE for correct equation structure
    // and parameter initialization ordering)
    let c_code =
        result.render_template_str_prepared_with_name(c_template, &model_identifier, true)?;
    let c_path = sources_dir.join(format!("{}.c", model_identifier));
    fs::write(&c_path, &c_code)?;
    eprintln!("  wrote {}", c_path.display());

    // Write CMakeLists.txt and build script
    write_fmu_cmake(&sources_dir, &model_identifier)?;
    write_fmu_build_script(&out_dir, &model_identifier)?;

    if args.no_build {
        eprintln!(
            "\nFMU sources exported to: {}\nRun ./build.sh to compile and package the .fmu",
            out_dir.display()
        );
    } else {
        build_fmu(&out_dir, &model_identifier)?;
    }

    Ok(())
}

fn run_export_embedded_c(args: ExportEmbeddedCArgs) -> Result<()> {
    use rumoca_session::runtime::embedded_c_templates;
    use std::fs;

    init_debug_tracing(args.input.debug);
    let (result, model) = compile_with_inferred_model(&args.input)?;

    let model_identifier = model.replace('.', "_");

    // Validate eFMI constraint: reject continuous derivatives
    // eFMI embedded C only supports discrete states with pre() causality, not continuous ODE dynamics
    if !result.dae.states.is_empty() {
        anyhow::bail!(
            "Embedded C code generation does not support continuous states (der(x)) \
             per eFMI semantics. Model '{}' has {} continuous state(s). \
             Use discrete states with 'when sample()' and 'pre()' references instead.",
            model_identifier,
            result.dae.states.len()
        );
    }

    eprintln!("Exporting embedded C for {}", model_identifier);

    let out_dir = args
        .output
        .unwrap_or_else(|| PathBuf::from(&model_identifier));
    fs::create_dir_all(&out_dir)?;

    // Render header (.h)
    let h_code = result.render_template_str_prepared_with_name(
        embedded_c_templates::EMBEDDED_C_H,
        &model_identifier,
        true,
    )?;
    let h_path = out_dir.join(format!("{}.h", model_identifier));
    fs::write(&h_path, &h_code)?;
    eprintln!("  wrote {}", h_path.display());

    // Render implementation (.c)
    let c_code = result.render_template_str_prepared_with_name(
        embedded_c_templates::EMBEDDED_C_IMPL,
        &model_identifier,
        true,
    )?;
    let c_path = out_dir.join(format!("{}.c", model_identifier));
    fs::write(&c_path, &c_code)?;
    eprintln!("  wrote {}", c_path.display());

    eprintln!(
        "\nEmbedded C sources exported to: {}\nCompile: cc -O2 -Wall -c {}/{}.c",
        out_dir.display(),
        out_dir.display(),
        model_identifier,
    );

    Ok(())
}

/// Write a CMakeLists.txt for building the FMU shared library.
fn write_fmu_cmake(sources_dir: &Path, model_identifier: &str) -> Result<()> {
    let cmake_path = sources_dir.join("CMakeLists.txt");
    let cmake_content = format!(
        r#"cmake_minimum_required(VERSION 3.10)
project({ident} C)

set(CMAKE_C_STANDARD 99)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

add_library({ident} SHARED {ident}.c)
target_compile_options({ident} PRIVATE -Wall -Wextra -pedantic)

if(WIN32)
  set(FMU_PLATFORM "win64")
elseif(APPLE)
  set(FMU_PLATFORM "darwin64")
elseif(UNIX)
  set(FMU_PLATFORM "linux64")
else()
  message(FATAL_ERROR "Unsupported platform for FMI2 packaging")
endif()

# Install into FMU binaries directory
install(TARGETS {ident}
    RUNTIME DESTINATION ${{CMAKE_INSTALL_PREFIX}}/binaries/${{FMU_PLATFORM}}
    LIBRARY DESTINATION ${{CMAKE_INSTALL_PREFIX}}/binaries/${{FMU_PLATFORM}})
"#,
        ident = model_identifier
    );
    std::fs::write(&cmake_path, &cmake_content)?;
    eprintln!("  wrote {}", cmake_path.display());
    Ok(())
}

/// Write a POSIX shell script that compiles and packages the FMU.
fn write_fmu_build_script(out_dir: &Path, model_identifier: &str) -> Result<()> {
    use std::io::Write;

    let build_script_path = out_dir.join("build.sh");
    let mut f = std::fs::File::create(&build_script_path)?;
    write!(
        f,
        r#"#!/bin/sh
# Build script for {ident} FMU
set -e
cd "$(dirname "$0")"

# Detect platform for FMU binaries directory
case "$(uname -s)" in
  Linux*)   PLATFORM=linux64; LIB_EXT=so ;;
  Darwin*)  PLATFORM=darwin64; LIB_EXT=dylib ;;
  MINGW*|MSYS*|CYGWIN*) PLATFORM=win64; LIB_EXT=dll ;;
  *) echo "Unknown platform"; exit 1 ;;
esac

# Compile shared library
mkdir -p binaries/$PLATFORM
cc -shared -fPIC -O2 -o binaries/$PLATFORM/{ident}.$LIB_EXT sources/{ident}.c -lm

# Package as .fmu (ZIP archive)
zip -r {ident}.fmu modelDescription.xml binaries/ sources/
echo "Created {ident}.fmu"
"#,
        ident = model_identifier
    )?;
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        std::fs::set_permissions(&build_script_path, std::fs::Permissions::from_mode(0o755))?;
    }
    eprintln!("  wrote {}", build_script_path.display());
    Ok(())
}

/// Compile the generated C source into a shared library and package as .fmu.
fn build_fmu(out_dir: &Path, model_identifier: &str) -> Result<()> {
    use std::process::Command;

    // Detect platform
    let (platform, lib_ext) = if cfg!(target_os = "linux") {
        ("linux64", "so")
    } else if cfg!(target_os = "macos") {
        ("darwin64", "dylib")
    } else if cfg!(target_os = "windows") {
        ("win64", "dll")
    } else {
        bail!("Unsupported platform for FMU packaging");
    };

    // Compile shared library
    let bin_dir = out_dir.join("binaries").join(platform);
    std::fs::create_dir_all(&bin_dir)?;

    let c_path = out_dir
        .join("sources")
        .join(format!("{model_identifier}.c"));
    let lib_path = bin_dir.join(format!("{model_identifier}.{lib_ext}"));

    eprintln!("  compiling {}", c_path.display());
    let status = Command::new("cc")
        .args(["-shared", "-fPIC", "-O2", "-o"])
        .arg(&lib_path)
        .arg(&c_path)
        .arg("-lm")
        .status()?;

    if !status.success() {
        bail!(
            "C compiler failed with exit code {}",
            status.code().unwrap_or(-1)
        );
    }
    eprintln!("  wrote {}", lib_path.display());

    // Package as .fmu (ZIP archive)
    let fmu_path = out_dir.join(format!("{model_identifier}.fmu"));
    create_fmu_zip(out_dir, &fmu_path)?;
    eprintln!("\nCreated {}", fmu_path.display());

    Ok(())
}

/// Create the .fmu ZIP archive containing modelDescription.xml, binaries/, and sources/.
fn create_fmu_zip(out_dir: &Path, fmu_path: &Path) -> Result<()> {
    use std::io::{Read as _, Write as _};
    use zip::ZipWriter;
    use zip::write::SimpleFileOptions;

    let file = std::fs::File::create(fmu_path)?;
    let mut zip = ZipWriter::new(file);
    let options = SimpleFileOptions::default().compression_method(zip::CompressionMethod::Deflated);

    // Walk the output directory and add relevant files
    for entry in walkdir::WalkDir::new(out_dir) {
        let entry = entry?;
        let path = entry.path();

        // Skip the .fmu file itself and build.sh
        if path == fmu_path || path == out_dir.join("build.sh") {
            continue;
        }

        let rel_path = path.strip_prefix(out_dir)?;
        let rel_str = rel_path.to_string_lossy();

        if rel_str.is_empty() {
            continue;
        }

        if entry.file_type().is_dir() {
            zip.add_directory(format!("{rel_str}/"), options)?;
        } else {
            zip.start_file(rel_str.to_string(), options)?;
            let mut f = std::fs::File::open(path)?;
            let mut buf = Vec::new();
            f.read_to_end(&mut buf)?;
            zip.write_all(&buf)?;
        }
    }

    zip.finish()?;
    Ok(())
}

fn run_fmt(args: FmtArgs) -> Result<()> {
    let paths = normalize_target_paths(&args.paths);
    let config_dir = first_path_config_dir(&paths);
    let mut options = rumoca_tool_fmt::load_config_from_dir(&config_dir)
        .map_err(|e| anyhow::anyhow!("Failed to load formatter config: {e}"))?
        .unwrap_or_default();
    if let Some(indent_size) = args.indent_size {
        options.indent_size = indent_size;
    }
    if let Some(use_tabs) = args.use_tabs {
        options.use_tabs = use_tabs;
    }
    if let Some(profile) = args.profile {
        options.profile = profile.into();
    }

    let files = collect_modelica_files(&paths);
    if files.is_empty() {
        eprintln!("No .mo files found");
        return Ok(());
    }

    let mut needs_formatting = 0usize;
    let mut errors = 0usize;
    for file in &files {
        let source = match std::fs::read_to_string(file) {
            Ok(v) => v,
            Err(e) => {
                eprintln!("Error reading {}: {e}", file.display());
                errors += 1;
                continue;
            }
        };

        let source_name = file.display().to_string();
        let formatted =
            match rumoca_tool_fmt::format_with_source_name(&source, &options, &source_name) {
                Ok(v) => v,
                Err(e) => {
                    eprintln!("Error formatting {}: {e}", file.display());
                    errors += 1;
                    continue;
                }
            };
        if formatted == source {
            continue;
        }
        needs_formatting += 1;
        if args.check {
            eprintln!("Would reformat: {}", file.display());
        } else if let Err(e) = std::fs::write(file, formatted) {
            eprintln!("Error writing {}: {e}", file.display());
            errors += 1;
        } else {
            eprintln!("Formatted: {}", file.display());
        }
    }

    let total = files.len();
    let unchanged = total.saturating_sub(needs_formatting + errors);
    if args.check {
        eprintln!(
            "{total} files checked: {unchanged} ok, {needs_formatting} need formatting, {errors} errors"
        );
        if needs_formatting > 0 || errors > 0 {
            std::process::exit(1);
        }
    } else {
        eprintln!(
            "{total} files processed: {unchanged} unchanged, {needs_formatting} formatted, {errors} errors"
        );
        if errors > 0 {
            std::process::exit(1);
        }
    }

    Ok(())
}

fn run_lint(args: LintArgs) -> Result<()> {
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
        limited.len(),
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

fn init_debug_tracing(debug: bool) {
    // Initialize tracing if enabled (SPEC_0024)
    #[cfg(feature = "tracing")]
    if debug {
        use tracing_subscriber::EnvFilter;
        tracing_subscriber::fmt()
            .with_env_filter(
                EnvFilter::try_from_default_env()
                    .unwrap_or_else(|_| EnvFilter::new("rumoca_phase_flatten=debug")),
            )
            .with_target(true)
            .with_level(true)
            .init();
        eprintln!("Debug tracing enabled");
    }

    #[cfg(not(feature = "tracing"))]
    if debug {
        eprintln!("Warning: --debug flag requires --features tracing");
        eprintln!("Rebuild with: cargo build --features tracing");
    }
}

fn compile_with_inferred_model(args: &ModelInputArgs) -> Result<(CompilationResult, String)> {
    let model = match &args.model {
        Some(model) => model.clone(),
        None => infer_model_name(&args.model_file)?,
    };

    let source_roots = merged_source_root_paths(&args.source_roots);

    let compiler = Compiler::new()
        .model(&model)
        .verbose(args.verbose)
        .source_roots(&source_roots);
    let result = compiler.compile_file(&args.model_file)?;
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
    let mut merged = Vec::new();
    let mut seen = std::collections::HashSet::new();
    for path in env_modelica_paths.iter().chain(cli_paths.iter()) {
        let trimmed = path.trim();
        if trimmed.is_empty() {
            continue;
        }
        let key = if cfg!(windows) {
            trimmed.to_ascii_lowercase()
        } else {
            trimmed.to_string()
        };
        if seen.insert(key) {
            merged.push(trimmed.to_string());
        }
    }
    merged
}

fn infer_model_name(model_file: &str) -> Result<String> {
    let source = std::fs::read_to_string(model_file)?;
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

    let mut candidates = rumoca_session::parsing::collect_model_names(&definition);
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
                source_map,
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
            source_map,
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

fn collect_modelica_files(paths: &[PathBuf]) -> Vec<PathBuf> {
    let mut out = Vec::<PathBuf>::new();
    for path in paths {
        if path.is_file() {
            if path.extension().is_some_and(|ext| ext == "mo") {
                out.push(path.to_path_buf());
            }
            continue;
        }
        for entry in WalkDir::new(path).into_iter().filter_map(|e| e.ok()) {
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
    qualified_name.rsplit('.').next().unwrap_or(qualified_name)
}

fn print_summary(model: &str, result: &CompilationResult) {
    println!("Compilation successful!");
    println!();
    println!("Model: {}", model);
    println!("States: {}", result.dae.states.len());
    println!("Algebraics: {}", result.dae.algebraics.len());
    println!("Parameters: {}", result.dae.parameters.len());
    println!("Constants: {}", result.dae.constants.len());
    println!("Inputs: {}", result.dae.inputs.len());
    println!("Outputs: {}", result.dae.outputs.len());
    println!();
    println!("Continuous equations (f_x): {}", result.dae.f_x.len());
    println!("Initial equations: {}", result.dae.initial_equations.len());
    println!();
    println!("Balance: {} (equations - unknowns)", result.balance());
    if result.is_balanced() {
        println!("Status: BALANCED");
    } else {
        println!("Status: UNBALANCED");
    }
    println!();
    println!("Use `rumoca compile <file> --json` to output the full DAE as JSON");
    println!("Use `rumoca compile <file> --template-file <FILE>` for template rendering");
}

fn run_simulation(
    result: &CompilationResult,
    model: &str,
    t_end: f64,
    dt: Option<f64>,
    solver: SimulateSolverMode,
    output: Option<&str>,
    workspace_root: Option<&Path>,
) -> Result<()> {
    use rumoca_session::runtime::{SimOptions, simulate_dae};

    let opts = SimOptions {
        t_end,
        dt,
        solver_mode: solver.into(),
        ..SimOptions::default()
    };

    eprintln!("Simulating {} to t={}...", model, t_end);
    let sim = simulate_dae(&result.dae, &opts).map_err(anyhow::Error::msg)?;
    eprintln!(
        "Simulation complete: {} time points, {} variables",
        sim.times.len(),
        sim.names.len()
    );

    let out_path = match output {
        Some(p) => PathBuf::from(p),
        None => PathBuf::from(format!("{}_results.html", model)),
    };
    let request_summary = SimulationRequestSummary {
        solver: solver.as_label().to_string(),
        t_start: opts.t_start,
        t_end: opts.t_end,
        dt: opts.dt,
        rtol: opts.rtol,
        atol: opts.atol,
    };
    let metrics = SimulationRunMetrics::default();
    let report = sim_report::write_html_report(
        &sim,
        model,
        &out_path,
        &request_summary,
        &metrics,
        workspace_root,
    )?;
    if let Some(workspace_root) = workspace_root {
        write_last_simulation_result_for_model(
            workspace_root,
            model,
            &report.payload,
            Some(&report.metrics),
        )?;
        write_simulation_run(
            workspace_root,
            model,
            &report.payload,
            Some(&report.metrics),
            Some(&report.views),
        )?;
    }
    println!("{}", out_path.display());

    Ok(())
}

fn discover_workspace_root_for_model_file(model_file: &str) -> Option<PathBuf> {
    let input_path = PathBuf::from(model_file);
    let absolute = if input_path.is_absolute() {
        input_path
    } else {
        std::env::current_dir().ok()?.join(input_path)
    };
    let start_dir = if absolute.is_dir() {
        absolute
    } else {
        absolute.parent()?.to_path_buf()
    };
    for ancestor in start_dir.ancestors() {
        if ancestor.join(".rumoca").join("project.toml").is_file() {
            return Some(ancestor.to_path_buf());
        }
    }
    None
}

fn completion_script(shell: CompletionShell) -> String {
    let top = "compile simulate check export-fmu completions --help -h --version -V";
    let compile_opts =
        "--model --source-root --json --template-file --template-prepared --verbose --debug";
    let simulate_opts = "--model --source-root --t-end --dt --solver --output --verbose --debug";
    let check_opts = "--model --source-root --verbose --debug";
    let export_fmu_opts = "--model --source-root --output --no-build --verbose --debug";
    let completion_opts = "bash zsh fish powershell";
    match shell {
        CompletionShell::Bash => format!(
            r#"_rumoca_completions() {{
  local cur cmd opts
  cur="${{COMP_WORDS[COMP_CWORD]}}"
  cmd="${{COMP_WORDS[1]}}"
  if [[ $COMP_CWORD -eq 1 ]]; then
    COMPREPLY=($(compgen -W "{top}" -- "$cur"))
    return
  fi
  case "$cmd" in
    compile) opts="{compile_opts}" ;;
    simulate) opts="{simulate_opts}" ;;
    check) opts="{check_opts}" ;;
    export-fmu) opts="{export_fmu_opts}" ;;
    completions) opts="{completion_opts}" ;;
    *) opts="{top}" ;;
  esac
  COMPREPLY=($(compgen -W "$opts" -- "$cur"))
}}
complete -F _rumoca_completions rumoca
"#
        ),
        CompletionShell::Zsh => format!(
            r#"#compdef rumoca
_rumoca() {{
  local -a top
  top=({top})
  _arguments '1: :->subcmd' '*::arg:->args'
  case $state in
    subcmd)
      _describe -t commands 'rumoca commands' top
      ;;
    args)
      case $words[2] in
        compile) _values 'options' {compile_opts} ;;
        simulate) _values 'options' {simulate_opts} ;;
        check) _values 'options' {check_opts} ;;
        export-fmu) _values 'options' {export_fmu_opts} ;;
        completions) _values 'shell' {completion_opts} ;;
      esac
      ;;
  esac
}}
compdef _rumoca rumoca
"#
        ),
        CompletionShell::Fish => [
            "complete -c rumoca -n '__fish_use_subcommand' -a 'compile' -d 'Compile a Modelica file'",
            "complete -c rumoca -n '__fish_use_subcommand' -a 'simulate' -d 'Simulate a Modelica file'",
            "complete -c rumoca -n '__fish_use_subcommand' -a 'check' -d 'Compile and print summary'",
            "complete -c rumoca -n '__fish_use_subcommand' -a 'export-fmu' -d 'Export FMI 2.0 FMU'",
            "complete -c rumoca -n '__fish_use_subcommand' -a 'completions' -d 'Print completion script'",
            "complete -c rumoca -n '__fish_seen_subcommand_from compile' -a '--model --source-root --json --template-file --template-prepared --verbose --debug'",
            "complete -c rumoca -n '__fish_seen_subcommand_from simulate' -a '--model --source-root --t-end --output --verbose --debug'",
            "complete -c rumoca -n '__fish_seen_subcommand_from check' -a '--model --source-root --verbose --debug'",
            "complete -c rumoca -n '__fish_seen_subcommand_from export-fmu' -a '--model --source-root --output --verbose --debug'",
            "complete -c rumoca -n '__fish_seen_subcommand_from completions' -a 'bash zsh fish powershell'",
        ]
        .join("\n")
            + "\n",
        CompletionShell::PowerShell => format!(
            r#"Register-ArgumentCompleter -CommandName rumoca -ScriptBlock {{
  param($wordToComplete, $commandAst, $cursorPosition)
  $words = $commandAst.CommandElements | ForEach-Object {{ $_.ToString() }}
  $candidates = @({top_tokens})
  if ($words.Count -ge 2) {{
    switch ($words[1]) {{
      "compile" {{ $candidates = @({compile_tokens}) }}
      "simulate" {{ $candidates = @({simulate_tokens}) }}
      "check" {{ $candidates = @({check_tokens}) }}
      "export-fmu" {{ $candidates = @({export_fmu_tokens}) }}
      "completions" {{ $candidates = @({completion_tokens}) }}
    }}
  }}
  $candidates | Where-Object {{ $_ -like "$wordToComplete*" }} | ForEach-Object {{
    [System.Management.Automation.CompletionResult]::new($_, $_, 'ParameterValue', $_)
  }}
}}
"#,
            top_tokens = to_ps_tokens(top),
            compile_tokens = to_ps_tokens(compile_opts),
            simulate_tokens = to_ps_tokens(simulate_opts),
            check_tokens = to_ps_tokens(check_opts),
            export_fmu_tokens = to_ps_tokens(export_fmu_opts),
            completion_tokens = to_ps_tokens(completion_opts),
        ),
    }
}

fn to_ps_tokens(words: &str) -> String {
    words
        .split_whitespace()
        .map(|word| format!("'{word}'"))
        .collect::<Vec<_>>()
        .join(", ")
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_session::compile::core::PrimaryLabel;
    use std::io::Write;
    use tempfile::NamedTempFile;

    #[test]
    fn infer_model_from_single_top_level_class() {
        let mut file = NamedTempFile::new().expect("temp file");
        writeln!(
            file,
            "model OnlyOne\n  Real x;\nequation\n  der(x)=1;\nend OnlyOne;"
        )
        .expect("write");
        let model = infer_model_name(file.path().to_str().expect("utf8 path")).expect("infer");
        assert_eq!(model, "OnlyOne");
    }

    #[test]
    fn infer_model_by_file_stem_when_multiple_candidates() {
        let dir = tempfile::tempdir().expect("temp dir");
        let path = dir.path().join("Preferred.mo");
        std::fs::write(
            &path,
            "model Alternate\n  Real x;\nend Alternate;\nmodel Preferred\n  Real y;\nend Preferred;",
        )
        .expect("write");
        let model = infer_model_name(path.to_str().expect("utf8 path")).expect("infer");
        assert_eq!(model, "Preferred");
    }

    #[test]
    fn infer_model_errors_when_ambiguous() {
        let mut file = NamedTempFile::new().expect("temp file");
        writeln!(
            file,
            "model Alpha\n  Real x;\nend Alpha;\nmodel Beta\n  Real y;\nend Beta;"
        )
        .expect("write");
        let error = infer_model_name(file.path().to_str().expect("utf8 path"))
            .expect_err("should fail without explicit model");
        assert!(
            error.to_string().contains("Pass --model <NAME>"),
            "unexpected error: {error}"
        );
    }

    #[test]
    fn infer_model_from_recovered_parse_broken_file() {
        let mut file = NamedTempFile::new().expect("temp file");
        writeln!(file, "model Broken").expect("write");
        writeln!(file, "  Real x").expect("write");
        writeln!(file, "end Broken;").expect("write");
        let model = infer_model_name(file.path().to_str().expect("utf8 path"))
            .expect("recovery parser should preserve top-level model name");
        assert_eq!(model, "Broken");
    }

    #[test]
    fn split_path_list_parses_multiple_entries() {
        let joined = std::env::join_paths([PathBuf::from("libA"), PathBuf::from("libB")])
            .expect("join paths");
        let parsed = split_path_list(Some(joined));
        assert_eq!(parsed, vec!["libA".to_string(), "libB".to_string()]);
    }

    #[test]
    fn merged_source_root_paths_prefers_env_then_cli_and_dedups() {
        let cli = vec!["/tmp/rootA".to_string(), "/tmp/rootA".to_string()];
        let env_modelica = vec!["/tmp/rootB".to_string(), "/tmp/rootA".to_string()];
        let merged = merge_source_root_path_sources(&cli, &env_modelica);
        assert_eq!(
            merged,
            vec!["/tmp/rootB".to_string(), "/tmp/rootA".to_string()]
        );
    }

    #[test]
    fn cli_error_report_preserves_compiler_diagnostics() {
        let error = anyhow::Error::new(CompilerError::ParseError("bad package layout".to_string()));
        let report = build_cli_error_report(&error);
        let rendered = format!("{report:?}");
        assert!(
            rendered.contains("E004"),
            "compiler errors should render via miette with their diagnostic code: {rendered}"
        );
        assert!(
            rendered.contains("bad package layout"),
            "compiler error message should be preserved: {rendered}"
        );
    }

    #[test]
    fn cli_error_report_wraps_generic_errors_in_miette() {
        let error = anyhow::anyhow!("plain failure");
        let report = build_cli_error_report(&error);
        let rendered = format!("{report:?}");
        assert!(
            rendered.contains("plain failure"),
            "generic errors should still render through miette: {rendered}"
        );
    }

    #[test]
    fn source_diagnostic_report_preserves_spans() {
        let mut source_map = SourceMap::new();
        let source_id = source_map.add("Pkg/A.mo", "model A end A;");
        let span = rumoca_session::compile::core::Span::from_offsets(source_id, 6, 7);
        let diagnostic = CommonDiagnostic::error(
            "PKG-007",
            "duplicate class name",
            PrimaryLabel::new(span).with_message("duplicate class here"),
        );

        let report = build_source_diagnostic_report(&diagnostic, &source_map);
        let rendered = format!("{report:?}");
        assert!(
            rendered.contains("PKG-007"),
            "code should be preserved: {rendered}"
        );
        assert!(
            rendered.contains("duplicate class name"),
            "message should be preserved: {rendered}"
        );
        assert!(
            rendered.contains("Pkg/A.mo"),
            "source file should be shown: {rendered}"
        );
    }

    #[test]
    fn source_diagnostic_report_formats_global_errors_without_source_blocks() {
        let report = build_source_diagnostic_report(
            &CommonDiagnostic::global_error(
                "PKG-006",
                "directory '/tmp/Pkg' is missing package.mo",
            ),
            &SourceMap::new(),
        );
        let rendered = format!("{report:?}");
        assert!(
            rendered.contains("PKG-006"),
            "code should be preserved: {rendered}"
        );
        assert!(
            rendered.contains("missing package.mo"),
            "message should be preserved: {rendered}"
        );
        assert!(
            !rendered.contains("unknown"),
            "global errors should not invent fake source blocks: {rendered}"
        );
    }
}
