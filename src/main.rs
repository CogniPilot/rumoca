mod s0_lexer;
mod s1_parser;
mod s2_analyzer;
mod s3_optimizer;
mod s4_generator;

use clap::Parser;

#[derive(Parser, Debug)]
#[command(version, about = "Rumoca Modelica Translator", long_about = None)]
struct Args {
    /// The template
    #[arg(short, long)]
    template_file: String,

    /// The model file to compile
    #[arg(short, long)]
    model_file: String,

    /// Verbose output
    #[arg(short, long, default_value_t = false)]
    verbose: bool,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();
    let def = s1_parser::parse_file(&args.model_file);
    let mut flat_def = s2_analyzer::flatten(&def).expect("failed to flatten");

    if args.verbose {
        println!("{:#?}", flat_def);
    }
    let s = s4_generator::generate(&mut flat_def, &args.template_file)?;
    println!("{s:}");
    Ok(())
}
