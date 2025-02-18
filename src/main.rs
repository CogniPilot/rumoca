extern crate parol_runtime;

mod modelica_grammar;
// The output is version controlled
mod modelica_grammar_trait;
mod modelica_parser;

use crate::modelica_grammar::ModelicaGrammar;
use crate::modelica_parser::parse;
use anyhow::{Context, Result, anyhow};
// use parol_runtime::ParseTree;
// use parol_runtime::syntree_layout::Layouter;
use parol_runtime::{Report, log::debug};
use std::{env, fs, time::Instant};

// To generate:
// parol -f ./modelica.par -e ./modelica-exp.par -p ./src/modelica_parser.rs -a ./src/modelica_grammar_trait.rs -t ModelicaGrammar -m modelica_grammar -g

struct ErrorReporter;
impl Report for ErrorReporter {}

fn main() -> Result<()> {
    env_logger::init();
    debug!("env logger started");

    let args: Vec<String> = env::args().collect();
    if args.len() >= 2 {
        let file_name = args[1].clone();
        let input = fs::read_to_string(file_name.clone())
            .with_context(|| format!("Can't read file {}", file_name))?;
        let mut modelica_grammar = ModelicaGrammar::new();
        let now = Instant::now();
        match parse(&input, &file_name, &mut modelica_grammar) {
            Ok(_syntax_tree) => {
                let elapsed_time = now.elapsed();
                println!("Parsing took {} milliseconds.", elapsed_time.as_millis());
                if args.len() > 2 && args[2] == "-q" {
                    Ok(())
                } else {
                    //generate_tree_layout(&syntax_tree, &file_name)?;
                    println!(
                        "Success!\n{:#?}",
                        modelica_grammar.modelica.expect("failed to parse")
                    );
                    Ok(())
                }
            }
            Err(e) => ErrorReporter::report_error(&e, file_name),
        }
    } else {
        Err(anyhow!("Please provide a file name as first parameter!"))
    }
}

// fn generate_tree_layout(
//     syntax_tree: &ParseTree<'_>,
//     input_file_name: &str,
// ) -> parol_runtime::syntree_layout::Result<()> {
//     let mut svg_full_file_name = std::path::PathBuf::from(input_file_name);
//     svg_full_file_name.set_extension("svg");
//     Layouter::new(syntax_tree)
//         .with_file_path(&svg_full_file_name)
//         .embed_with_visualize()?
//         .write()
// }
