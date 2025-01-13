use clap::Parser;

use rumoca::modelica_printer;
use rumoca::{s2_analyzer, s4_generator};

#[derive(Parser, Debug)]
#[command(version, about = "Rumoca Modelica Translator", long_about = None)]
struct Args {
    /// Renders a template using dae_ast
    #[arg(short, long, default_value = "")]
    template: String,

    /// Renders a template using ast
    #[arg(short, long, default_value = "")]
    ast_template: String,

    /// The modelica *.mo file to compile
    #[arg(name = "MODELICA_FILE")]
    modelica_file: String,

    /// Flatten the model
    #[arg(short, long, default_value_t = false)]
    flatten: bool,

    /// Verbose output
    #[arg(short, long, default_value_t = false)]
    verbose: bool,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();

    let def = rumoca_parser::parse_file(&args.modelica_file);

    if args.verbose {
        println!("\n\n{}", "=".repeat(80));
        println!("PARSE");
        println!("{}", "=".repeat(80));
        modelica_printer(&def);
    }

    let mut flat_def = def.clone();

    if args.flatten {
        flat_def = s2_analyzer::flatten(&def, args.verbose).expect("failed to flatten");
    }

    if !args.ast_template.is_empty() {
        let s = s4_generator::generate_ast(&flat_def, &args.ast_template, args.verbose)?;
        println!("{s:}");
    }

    if !args.template.is_empty() {
        let mut dae_def =
            s2_analyzer::dae_creator::create_dae(&def, args.verbose).expect("failed to create dae");
        let s = s4_generator::generate_dae_ast(&mut dae_def, &args.template, args.verbose)?;
        println!("{s:}");
    }
    Ok(())
}
