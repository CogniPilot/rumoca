//! Example demonstrating file-based compilation
//!
//! This example shows how to compile a Modelica file from disk.
//!
//! Run this example with:
//! ```sh
//! cargo run --example file_compilation
//! ```

use rumoca::Compiler;
use std::env;

fn main() -> anyhow::Result<()> {
    // Get the model file path from command line args, or use a default
    let args: Vec<String> = env::args().collect();
    let model_file = if args.len() > 1 {
        &args[1]
    } else {
        "tests/fixtures/integrator.mo"
    };

    println!("Compiling model file: {}\n", model_file);

    // Compile the file
    let result = Compiler::new()
        .verbose(true) // Enable verbose output to see compilation steps
        .compile_file(model_file)?;

    // Print summary
    println!("\n=== Summary ===");
    println!("States: {}", result.dae.x.len());
    println!("Outputs: {}", result.dae.y.len());
    println!("Parameters: {}", result.dae.p.len());
    println!("Equations: {}", result.dae.fx.len());
    println!("Total compilation time: {:?}", result.total_time());

    Ok(())
}
