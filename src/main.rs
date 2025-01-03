mod ast;
mod flat_ast;
mod generator;
mod lexer;
mod tokens;

use clap::Parser;
use generator::flatten;
use generator::parse_file;

use lalrpop_util::lalrpop_mod;

lalrpop_mod!(
    #[allow(clippy::ptr_arg)]
    #[allow(clippy::vec_box)]
    #[rustfmt::skip]
    parser
);

#[derive(Parser, Debug)]
#[command(version, about = "Rumoca Modelica Translator", long_about = None)]
struct Args {
    /// The template
    #[arg(short, long)]
    template_file: String,

    /// The filename to compile
    #[arg(short, long)]
    filename: String,

    /// Verbose output
    #[arg(short, long, default_value_t = false)]
    verbose: bool,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();
    let def = parse_file(&args.filename).expect("failed to parse");
    let flat_def = flatten(&def).expect("failed to flatten");

    if args.verbose {
        println!("{:#?}", flat_def);
    }
    let s = generator::generate(&flat_def, &args.template_file)?;
    println!("{s:}");
    Ok(())
}
