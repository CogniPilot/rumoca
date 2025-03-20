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
use std::process;

use parol::parol_runtime::Report;
use parol::{ParolErrorReporter, build::Builder};

fn main() {
    // CLI equivalent is:
    // parol -f ./modelica.par -e ./modelica-exp.par -p ./src/modelica_parser.rs -a ./src/modelica_grammar_trait.rs -t ModelicaGrammar -m modelica_grammar -g
    if let Err(err) = Builder::with_explicit_output_dir("src")
        .grammar_file("modelica.par")
        .expanded_grammar_output_file("../modelica-exp.par")
        .parser_output_file("modelica_parser.rs")
        .actions_output_file("modelica_grammar_trait.rs")
        .user_type_name("ModelicaGrammar")
        .user_trait_module_name("modelica_grammar")
        .trim_parse_tree()
        .minimize_boxed_types()
        .generate_parser()
    {
        ParolErrorReporter::report_error(&err, "modelica.par").unwrap_or_default();
        process::exit(1);
    }
}
