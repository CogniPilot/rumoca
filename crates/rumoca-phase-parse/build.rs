//! Build script for generating the Modelica parser from the grammar.

mod build_support;

use build_support::{generation_is_current, generation_stamp, install_generated_if_changed};
use parol::ParolErrorReporter;
use parol::build::Builder;
use parol::parol_runtime::Report;
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process;

fn main() {
    // Re-run if grammar changes
    println!("cargo:rerun-if-changed=src/modelica.par");
    println!("cargo:rerun-if-changed=build_support.rs");
    println!("cargo:rerun-if-changed=src/generated/grammar.stamp");

    let par_file = "src/modelica.par";

    // Only build if grammar file exists
    if !Path::new(par_file).exists() {
        eprintln!("Warning: Grammar file not found, skipping parser generation");
        return;
    }

    // The checked-in parser is regenerated only when its content inputs change.
    // Generation shells out to the machine's `rustfmt`, whose absence or version
    // would otherwise rewrite tracked files from an unchanged grammar.
    let stamp = generation_stamp(
        &fs::read(par_file).expect("read the Modelica grammar"),
        include_bytes!("build.rs"),
    );
    let stamp_file = Path::new("src/generated/grammar.stamp");
    let outputs = GENERATED_FILES.map(|name| Path::new("src/generated").join(name));
    if generation_is_current(
        stamp_file,
        &stamp,
        &outputs.each_ref().map(PathBuf::as_path),
    ) {
        return;
    }

    let staging_dir = PathBuf::from(
        env::var_os("OUT_DIR").expect("Cargo must provide OUT_DIR to parser generation"),
    )
    .join("modelica-generated");
    if let Err(err) = fs::create_dir_all(&staging_dir) {
        eprintln!(
            "failed to create parser staging directory {}: {err}",
            staging_dir.display()
        );
        process::exit(1);
    }

    if let Err(err) = Builder::with_explicit_output_dir(&staging_dir)
        .grammar_file(par_file)
        .parser_output_file("modelica_parser.rs")
        .actions_output_file("modelica_grammar_trait.rs")
        .user_type_name("ModelicaGrammar")
        .user_trait_module_name("grammar")
        .trim_parse_tree()
        .minimize_boxed_types()
        .generate_parser()
    {
        ParolErrorReporter::report_error(&err, par_file).unwrap_or_default();
        process::exit(1);
    }

    install_generated_files(&staging_dir);
    if let Err(err) = fs::write(stamp_file, stamp) {
        eprintln!("failed to record the grammar stamp: {err}");
        process::exit(1);
    }
}

const GENERATED_FILES: [&str; 2] = ["modelica_parser.rs", "modelica_grammar_trait.rs"];

fn install_generated_files(staging_dir: &Path) {
    let checked_in_dir = Path::new("src/generated");
    for file_name in GENERATED_FILES {
        let staged = staging_dir.join(file_name);
        let checked_in = checked_in_dir.join(file_name);
        if let Err(err) = install_generated_if_changed(&staged, &checked_in) {
            eprintln!(
                "failed to install generated parser {}: {err}",
                checked_in.display()
            );
            process::exit(1);
        }
    }
}
