mod ast;
mod generator;
mod lexer;
mod tokens;

use clap::Parser;
use generator::parse_file;

use lalrpop_util::lalrpop_mod;

lalrpop_mod!(
    #[allow(clippy::ptr_arg)]
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
    if args.verbose {
        println!("{:#?}", def);
    }
    let s = generator::generate(&def, &args.template_file)?;
    println!("{s:}");
    Ok(())
}
