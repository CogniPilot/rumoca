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
        //.minimize_boxed_types()
        .generate_parser()
    {
        ParolErrorReporter::report_error(&err, "modelica.par").unwrap_or_default();
        process::exit(1);
    }
}
