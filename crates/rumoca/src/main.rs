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

use std::path::Path;
use std::path::PathBuf;

use anyhow::{Result, bail};
use clap::{ArgAction, Args, Parser, Subcommand, ValueEnum};
use rumoca::{CompilationResult, Compiler, sim_report};
use rumoca_session::project::{ProjectFileMoveHint, resync_model_sidecars_with_move_hints};
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

#[derive(Args, Debug, Clone)]
struct ModelInputArgs {
    /// Modelica file to parse
    #[arg(name = "MODELICA_FILE")]
    model_file: String,

    /// Main model/class to compile (auto-inferred when omitted)
    #[arg(short, long)]
    model: Option<String>,

    /// Library path (file or directory). Can be specified multiple times.
    /// Example: -l ./libs/MSL -l helper.mo
    #[arg(short = 'l', long = "library", action = ArgAction::Append)]
    libraries: Vec<String>,

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
    #[arg(long, conflicts_with = "template_file")]
    json: bool,

    /// Template file for custom export (advanced)
    #[arg(short, long)]
    template_file: Option<String>,

    /// Render templates from a structurally prepared DAE instead of raw compile output
    #[arg(long, requires = "template_file")]
    template_prepared: bool,
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
    #[arg(long)]
    use_tabs: Option<bool>,
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

fn main() -> Result<()> {
    let cli = Cli::parse();
    match cli.command {
        Commands::Compile(args) => run_compile(args),
        Commands::Simulate(args) => run_simulate(args),
        Commands::Check(args) => run_check(args),
        Commands::Fmt(args) => run_fmt(args),
        Commands::Lint(args) => run_lint(args),
        Commands::Completions { shell } => {
            print!("{}", completion_script(shell));
            Ok(())
        }
        Commands::Project(args) => run_project(args),
    }
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

fn run_compile(args: CompileArgs) -> Result<()> {
    init_debug_tracing(args.input.debug);
    let (result, model) = compile_with_inferred_model(&args.input)?;
    if args.json {
        println!("{}", result.to_json()?);
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
    run_simulation(
        &result,
        &model,
        args.t_end,
        args.dt,
        args.solver,
        args.output.as_deref(),
    )
}

fn run_check(args: CheckArgs) -> Result<()> {
    init_debug_tracing(args.input.debug);
    let (result, model) = compile_with_inferred_model(&args.input)?;
    print_summary(&model, &result);
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

        let formatted = match rumoca_tool_fmt::format(&source, &options) {
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

    let compiler = Compiler::new()
        .model(&model)
        .verbose(args.verbose)
        .libraries(&args.libraries);
    let result = compiler.compile_file(&args.model_file)?;
    Ok((result, model))
}

fn infer_model_name(model_file: &str) -> Result<String> {
    let parsed = rumoca_session::parsing::parse_files_parallel(&[model_file])?;
    let top_level_names = parsed
        .first()
        .map(|(_, def)| {
            def.classes
                .iter()
                .filter_map(|(name, class)| {
                    let class_kind = class.class_type.as_str();
                    if class_kind == "model" || class_kind == "block" || class_kind == "class" {
                        Some(name.clone())
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>()
        })
        .unwrap_or_default();

    let merged = rumoca_session::parsing::merge_stored_definitions(parsed)?;
    let mut candidates = rumoca_session::parsing::collect_model_names(&merged);
    candidates.sort();
    candidates.dedup();
    if candidates.is_empty() {
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
    let header_settings = sim_report::SimulationHeaderSettings {
        solver: solver.as_label().to_string(),
        t_start: opts.t_start,
        t_end_requested: opts.t_end,
        dt: opts.dt,
        rtol: opts.rtol,
        atol: opts.atol,
        compile_seconds: None,
        simulate_seconds: None,
        compile_phase_instantiate_seconds: None,
        compile_phase_typecheck_seconds: None,
        compile_phase_flatten_seconds: None,
        compile_phase_todae_seconds: None,
    };
    sim_report::write_html_report(&sim, model, &out_path, Some(&header_settings))?;
    println!("{}", out_path.display());

    Ok(())
}

fn completion_script(shell: CompletionShell) -> String {
    let top = "compile simulate check completions --help -h --version -V";
    let compile_opts =
        "--model --library --json --template-file --template-prepared --verbose --debug";
    let simulate_opts = "--model --library --t-end --dt --solver --output --verbose --debug";
    let check_opts = "--model --library --verbose --debug";
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
            "complete -c rumoca -n '__fish_use_subcommand' -a 'completions' -d 'Print completion script'",
            "complete -c rumoca -n '__fish_seen_subcommand_from compile' -a '--model --library --json --template-file --template-prepared --verbose --debug'",
            "complete -c rumoca -n '__fish_seen_subcommand_from simulate' -a '--model --library --t-end --output --verbose --debug'",
            "complete -c rumoca -n '__fish_seen_subcommand_from check' -a '--model --library --verbose --debug'",
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
}
