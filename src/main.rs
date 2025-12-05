//! # Rumoca Modelica Translator
//!
//! This module provides a command-line tool for parsing, flattening, and translating
//! Modelica files into Differential Algebraic Equations (DAE) representations. It also
//! supports rendering the DAE representation using a user-provided template file.
//!
//! ## Features
//! - **Parsing**: Parses Modelica files into an abstract syntax tree (AST) using a custom grammar.
//! - **Flattening**: Flattens the parsed AST into a simplified representation.
//! - **DAE Creation**: Converts the flattened representation into a DAE format.
//! - **Template Rendering**: Renders the DAE representation using a Jinja2 template.
//!
//! ## Command-Line Arguments
//! - `--template-file` (`-t`): Optional path to a template file for rendering the DAE.
//! - `MODELICA_FILE`: Path to the Modelica file to parse.
//! - `--verbose` (`-v`): Enables verbose output for detailed logging and debugging.
//!
//! ## Usage
//! ```sh
//! rumoca_parol --template-file template.j2 example.mo --verbose
//! ```
//!
//! ## Error Handling
//! Errors encountered during file reading, parsing, or processing are reported using
//! the `anyhow` crate for detailed context. Parsing errors are handled by the custom
//! `ErrorReporter` implementation.
//!
//! ## Dependencies
//! - `parol_runtime`: Used for parsing Modelica files.
//! - `clap`: Command-line argument parsing.
//! - `env_logger`: Logging support.
//! - `anyhow`: Error handling with context.
//! - `rumoca`: Core library for Modelica grammar, parsing, and DAE generation.
use clap::Parser;
use rumoca::Compiler;

use anyhow::Result;

#[derive(Parser, Debug)]
#[command(version, about = "Rumoca Modelica Translator", long_about = None)]
struct Args {
    /// Export to Base Modelica JSON (native, recommended)
    #[arg(long, conflicts_with = "template_file")]
    json: bool,

    /// Template file for custom export (advanced)
    #[arg(short, long)]
    template_file: Option<String>,

    /// Main model/class to simulate (required)
    #[arg(short, long, required = true)]
    model: String,

    /// Modelica file to parse
    #[arg(name = "MODELICA_FILE")]
    model_file: String,

    /// Library search paths (alternative to MODELICAPATH env var)
    /// Can be specified multiple times: -L /path1 -L /path2
    #[arg(short = 'L', long = "lib-path")]
    lib_paths: Vec<String>,

    /// Verbose output
    #[arg(short, long)]
    verbose: bool,
}

fn main() -> Result<()> {
    env_logger::init();
    let args = Args::parse();

    // Use the new Compiler API
    let mut compiler = Compiler::new().verbose(args.verbose);

    // Set main model (required)
    compiler = compiler.model(&args.model);

    // Set library paths if provided (overrides MODELICAPATH env var)
    if !args.lib_paths.is_empty() {
        let paths: Vec<&str> = args.lib_paths.iter().map(|s| s.as_str()).collect();
        compiler = compiler.modelica_path(&paths);
    }

    // Auto-include packages from MODELICAPATH based on model name
    // E.g., "Modelica.Blocks.Continuous.PID" -> include "Modelica" package
    if let Some(root_package) = args.model.split('.').next() {
        // Try to include the root package from library paths
        // This will fail silently if the package is not found (it may be in the main file)
        compiler = match compiler.include_from_modelica_path(root_package) {
            Ok(c) => c,
            Err(_) => Compiler::new()
                .verbose(args.verbose)
                .model(&args.model)
                .modelica_path(
                    &args
                        .lib_paths
                        .iter()
                        .map(|s| s.as_str())
                        .collect::<Vec<_>>(),
                ),
        };
    }

    let mut result = compiler.compile_file(&args.model_file)?;

    // Export using native JSON or template
    if args.json {
        // Native JSON export (recommended)
        let json = result.dae.to_dae_ir_json()?;
        println!("{}", json);
    } else if let Some(template_file) = args.template_file {
        // Template-based export (advanced)
        result.render_template(&template_file)?;
    }

    Ok(())
}
