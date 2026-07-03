//! Build script for generating the GALEC parser from the grammar.
//!
//! Mirrors `crates/rumoca-phase-parse/build.rs`: it regenerates the committed
//! parser (`src/parse/generated/{galec_parser.rs,galec_grammar_trait.rs}`) from
//! `src/parse/galec.par` as a consistency check. `parol` is a build-dependency
//! so this runs unconditionally; the generated *module* only compiles under the
//! default-off `parse` cargo feature.

use parol::ParolErrorReporter;
use parol::build::Builder;
use parol::parol_runtime::Report;
use std::path::Path;
use std::process;

fn main() {
    // Re-run if the grammar changes.
    println!("cargo:rerun-if-changed=src/parse/galec.par");

    let par_file = "src/parse/galec.par";

    // Only build if the grammar file exists.
    if !Path::new(par_file).exists() {
        eprintln!("Warning: Grammar file not found, skipping parser generation");
        return;
    }

    if let Err(err) = Builder::with_explicit_output_dir("src/parse/generated")
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
}
