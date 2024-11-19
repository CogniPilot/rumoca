mod ast;
mod generators;
mod lexer;
mod tokens;

use clap::{Parser, ValueEnum};
use generators::parse_file;

use lalrpop_util::lalrpop_mod;

lalrpop_mod!(
    #[allow(clippy::ptr_arg)]
    #[rustfmt::skip]
    parser
);

#[derive(ValueEnum, Debug, Clone)]
enum Generator {
    Sympy,
    Json,
    CasadiMx,
    CasadiSx,
    Collimator,
}

#[derive(Parser, Debug)]
#[command(version, about = "Modelica Compiler", long_about = None)]
struct Args {
    /// The filename to compile
    #[arg(short, long)]
    filename: String,

    /// Verbose output
    #[arg(short, long, default_value_t = false)]
    verbose: bool,

    /// Generator to Use
    #[arg(short, long, value_enum)]
    generator: Generator,
}

fn main() {
    let args = Args::parse();
    let def = parse_file(&args.filename).expect("failed to parse");
    if args.verbose {
        println!("{:#?}", def);
    }
    match args.generator {
        Generator::Json => generators::json::generate(&def),
        Generator::Sympy => generators::sympy::generate(&def),
        Generator::CasadiMx => generators::casadi_mx::generate(&def),
        Generator::CasadiSx => generators::casadi_sx::generate(&def),
        Generator::Collimator => generators::collimator::generate(&def),
    }
}
