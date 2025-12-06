//! This build script is responsible for generating the parser and associated files
//! for the Modelica grammar using the `parol` crate. It automates the process of
//! creating the parser, grammar trait, and expanded grammar files based on the
//! provided grammar definition file (`modelica.par`).
//!
//! The script uses the `parol::build::Builder` to configure and generate the necessary
//! files. Key features include:
//! - Specifying the grammar file (`modelica.par`).
//! - Generating the expanded grammar file (`modelica-exp.par`).
//! - Creating the parser implementation file (`modelica_parser.rs`).
//! - Creating the grammar trait file (`modelica_grammar_trait.rs`).
//! - Customizing the user type name and trait module name.
//! - Enabling optimizations like trimming the parse tree and minimizing boxed types.
//!
//! If an error occurs during the generation process, it is reported using the
//! `ParolErrorReporter`, and the script exits with a non-zero status code.
//!
//! This script ensures that the parser and related files are always up-to-date
//! with the grammar definition, streamlining the development process.
// build.rs
use parol::parol_runtime::Report;
use parol::{ParolErrorReporter, build::Builder};
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};
use std::{env, process};

/// Get git version, appending build timestamp for dirty builds.
/// This ensures cache invalidation on rebuild while keeping cache stable between runs.
fn get_git_version_with_timestamp() -> String {
    // Get git describe output
    let output = Command::new("git")
        .args(["describe", "--tags", "--always", "--dirty"])
        .output();

    let base_version = match output {
        Ok(o) if o.status.success() => String::from_utf8_lossy(&o.stdout).trim().to_string(),
        _ => "unknown".to_string(),
    };

    // If dirty, append build timestamp
    if base_version.ends_with("-dirty") {
        let timestamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map(|d| d.as_secs().to_string())
            .unwrap_or_else(|_| "0".to_string());
        format!("{}-{}", base_version, timestamp)
    } else {
        base_version
    }
}

fn main() {
    // Get git version with timestamp for dirty builds
    let git_version = get_git_version_with_timestamp();

    // Write to a marker file and watch it for changes
    // This ensures build.rs reruns when git status changes (dirty <-> clean)
    // or when we rebuild after a previous dirty build
    let out_dir = env::var("OUT_DIR").unwrap_or_else(|_| ".".to_string());
    let marker_file = format!("{}/git_version.txt", out_dir);

    // Check if version changed - only write if different to avoid unnecessary rebuilds
    let needs_update = std::fs::read_to_string(&marker_file)
        .map(|s| s.trim() != git_version)
        .unwrap_or(true);

    if needs_update {
        std::fs::write(&marker_file, &git_version).ok();
    }

    // Watch the marker file - cargo will rerun build.rs if it changes
    println!("cargo:rerun-if-changed={}", marker_file);

    // Set the git version env var for the compiler
    println!("cargo:rustc-env=RUMOCA_GIT_VERSION={}", git_version);

    println!("cargo:rerun-if-changed=src/modelica_grammar/modelica.par");

    // Pedantic rule: only rebuild if explicitly requested.
    let rebuild = env::var_os("CARGO_FEATURE_REGEN_PARSER").is_some();

    if !rebuild {
        return;
    }

    println!("cargo:warning=Regenerating parser (triggered by feature=regen-parser)");

    if let Err(err) = Builder::with_explicit_output_dir("src/modelica_grammar/generated")
        .grammar_file("src/modelica_grammar/modelica.par")
        .expanded_grammar_output_file("modelica-exp.par")
        .parser_output_file("modelica_parser.rs")
        .actions_output_file("modelica_grammar_trait.rs")
        .user_type_name("ModelicaGrammar")
        .user_trait_module_name("modelica_grammar")
        .trim_parse_tree()
        .minimize_boxed_types()
        .generate_parser()
    {
        ParolErrorReporter::report_error(&err, "src/modelica_grammar/modelica.par")
            .unwrap_or_default();
        process::exit(1);
    }
}
