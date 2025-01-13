use clap::Parser;

use rumoca::s2_analyzer::parse::node::{Node, Visitable};
use rumoca::s2_analyzer::parse::repr_visitor::ReprVisitor;
use rumoca::s4_generator;

#[derive(Parser, Debug)]
#[command(version, about = "Rumoca Modelica Translator", long_about = None)]
struct Args {
    /// Renders a template using dae_ast
    #[arg(short, long, default_value = "")]
    template: String,

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
    let bar = "=".repeat(40);

    if args.verbose {
        println!("\n\n{}", bar);
        println!("PARSE");
        println!("{}", bar);
        println!("{:#?}", def);
    }

    let mut repr_visitor = ReprVisitor::default();
    def.accept(&mut repr_visitor, None);
    println!(
        "{}",
        repr_visitor.repr.get(&def.node_data().id).expect("no repr")
    );

    if !args.template.is_empty() {
        let s = s4_generator::generate(&def, &args.template, args.verbose)?;
        println!("{s:}");
    }

    Ok(())
}
