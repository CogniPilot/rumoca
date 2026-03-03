//! Build script for generating the Modelica parser from the grammar.

use parol::ParolErrorReporter;
use parol::build::Builder;
use parol::parol_runtime::Report;
use std::path::Path;
use std::process;

fn main() {
    // Re-run if grammar changes
    println!("cargo:rerun-if-changed=src/modelica.par");

    let par_file = "src/modelica.par";

    // Only build if grammar file exists
    if !Path::new(par_file).exists() {
        eprintln!("Warning: Grammar file not found, skipping parser generation");
        return;
    }

    if let Err(err) = Builder::with_explicit_output_dir("src/generated")
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
}
