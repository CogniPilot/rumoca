//! Build script for generating the GALEC parser from the grammar.
//!
//! Regenerates the committed parser in `src/parse/generated` from
//! `src/parse/galec.par` when the grammar or generator settings change.

mod build_support;

use build_support::{generation_is_current, generation_stamp, install_generated_if_changed};
use parol::ParolErrorReporter;
use parol::build::Builder;
use parol::parol_runtime::Report;
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process;

const GENERATED_FILES: [&str; 2] = ["galec_parser.rs", "galec_grammar_trait.rs"];

fn main() {
    // Re-run if the grammar or the generation stamp changes.
    println!("cargo:rerun-if-changed=src/parse/galec.par");
    println!("cargo:rerun-if-changed=build_support.rs");
    println!("cargo:rerun-if-changed=src/parse/generated/grammar.stamp");

    let par_file = "src/parse/galec.par";
    let checked_in_dir = Path::new("src/parse/generated");

    // The checked-in parser is regenerated only when its content inputs change.
    // Generation shells out to the machine's `rustfmt`, whose absence or version
    // would otherwise rewrite tracked files from an unchanged grammar.
    let stamp = generation_stamp(
        &fs::read(par_file).expect("read the GALEC grammar"),
        include_bytes!("build.rs"),
    );
    let stamp_file = checked_in_dir.join("grammar.stamp");
    let outputs = GENERATED_FILES.map(|name| checked_in_dir.join(name));
    if generation_is_current(
        &stamp_file,
        &stamp,
        &outputs.each_ref().map(PathBuf::as_path),
    ) {
        return;
    }

    let staging_dir = PathBuf::from(
        env::var_os("OUT_DIR").expect("Cargo must provide OUT_DIR to parser generation"),
    )
    .join("galec-generated");
    if let Err(err) = fs::create_dir_all(&staging_dir) {
        eprintln!(
            "failed to create parser staging directory {}: {err}",
            staging_dir.display()
        );
        process::exit(1);
    }

    if let Err(err) = Builder::with_explicit_output_dir(&staging_dir)
        .grammar_file(par_file)
        .parser_output_file("galec_parser.rs")
        .actions_output_file("galec_grammar_trait.rs")
        .user_type_name("GalecGrammar")
        .user_trait_module_name("grammar")
        .trim_parse_tree()
        .minimize_boxed_types()
        // Fail-early (SPEC_0008): a syntax error is a typed error, not a
        // recovered partial tree. Error recovery would skip tokens and run
        // semantic actions over a garbage tree, silently producing a wrong AST
        // (and defeating the expression-extraction entry point). A single typed
        // `GalecParseError` is the whole parser's error surface, so recovery
        // buys nothing here.
        .disable_recovery()
        .generate_parser()
    {
        ParolErrorReporter::report_error(&err, par_file).unwrap_or_default();
        process::exit(1);
    }

    for file_name in GENERATED_FILES {
        let checked_in = checked_in_dir.join(file_name);
        if let Err(err) = install_generated_if_changed(&staging_dir.join(file_name), &checked_in) {
            eprintln!(
                "failed to install generated parser {}: {err}",
                checked_in.display()
            );
            process::exit(1);
        }
    }
    if let Err(err) = fs::write(&stamp_file, stamp) {
        eprintln!("failed to record the grammar stamp: {err}");
        process::exit(1);
    }
}
