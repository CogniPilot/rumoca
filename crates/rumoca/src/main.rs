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
//!
//! Argument parsing and command dispatch live in [`rumoca::cli`]; this binary is
//! a thin wrapper that sets up the allocator and the miette error hook, parses
//! the arguments, runs [`rumoca::cli::run`], and renders any error.

// mimalloc is gated behind `native-allocator` so pure-Rust
// library consumers of this crate don't pull a C allocator. The binary always
// enables it via the package default.
#[cfg(feature = "native-allocator")]
#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use clap::Parser;
use miette::{GraphicalTheme, MietteHandlerOpts};
use rumoca::CompilerError;
use rumoca::cli::{
    self, Cli, build_cli_error_report, build_compile_failure_report, build_source_diagnostic_report,
};
use rumoca_compile::compile::core::{Diagnostic as CommonDiagnostic, SourceMap};

fn main() {
    // Restore the default SIGPIPE disposition: Rust installs SIG_IGN at startup,
    // which turns a closed reader (`rumoca compile … | head`) into a BrokenPipe
    // error that the `println!`-per-line paths surface as a panic (exit 101).
    // With the default disposition the process terminates quietly on SIGPIPE
    // like a normal Unix tool. No-op on non-Unix.
    sigpipe::reset();
    install_cli_miette_hook();
    let cli = Cli::parse();
    if let Err(error) = cli::run(cli) {
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
    #[cfg(feature = "scheduled-sim")]
    if let Some(scheduled_error) =
        error.downcast_ref::<rumoca_sim::scheduled_sim::ScheduledSimError>()
        && print_scheduled_sim_diagnostic_error(scheduled_error)
    {
        return;
    }
    eprintln!("{:?}", build_cli_error_report(error));
}

#[cfg(feature = "scheduled-sim")]
fn print_scheduled_sim_diagnostic_error(
    error: &rumoca_sim::scheduled_sim::ScheduledSimError,
) -> bool {
    let Some((diagnostic, source_map)) = error.source_diagnostic() else {
        return false;
    };
    print_source_diagnostics(&[diagnostic], &source_map)
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
