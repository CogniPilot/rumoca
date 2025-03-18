extern crate parol_runtime;
use clap::Parser;
use parol_runtime::{Report, log::debug};
use rumoca::modelica_grammar::ModelicaGrammar;
use rumoca::modelica_parser::parse;
use rumoca::{dae, ir::create_dae::create_dae, ir::flatten::flatten};
use std::{fs, time::Instant};

use anyhow::{Context, Result};

#[derive(Parser, Debug)]
#[command(version, about = "Rumoca Modelica Translator", long_about = None)]
struct Args {
    /// Template file to render to
    #[arg(short, long)]
    template_file: Option<String>,

    /// Modelica file to parse
    #[arg(name = "MODELICA_FILE")]
    model_file: String,

    /// Verbose output
    #[arg(short, long, default_value_t = false)]
    verbose: bool,
}

struct ErrorReporter;
impl Report for ErrorReporter {}

fn main() -> Result<()> {
    env_logger::init();
    debug!("env logger started");
    let args = Args::parse();

    let file_name = args.model_file.clone();
    let input = fs::read_to_string(file_name.clone())
        .with_context(|| format!("Can't read file {}", file_name))?;

    let mut modelica_grammar = ModelicaGrammar::new();
    let now = Instant::now();

    match parse(&input, &file_name, &mut modelica_grammar) {
        Ok(_syntax_tree) => {
            let elapsed_time = now.elapsed();

            // parse tree
            let def = modelica_grammar.modelica.expect("failed to parse");
            if args.verbose {
                println!("Parsing took {} milliseconds.", elapsed_time.as_millis());
                println!("Success!\n{:#?}", def);
            }

            // flatten tree
            let mut fclass = flatten(&def)?;
            if args.verbose {
                println!("{:#?}", fclass);
            }

            // create DAE
            let dae = create_dae(&mut fclass)?;
            if args.verbose {
                println!("{:#?}", dae);
            }

            // render template
            if args.template_file.is_some() {
                let s = args.template_file.unwrap();
                dae::jinja::render_template(dae, &s)?;
            }
            Ok(())
        }
        Err(e) => ErrorReporter::report_error(&e, file_name),
    }
}
