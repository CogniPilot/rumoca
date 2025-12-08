//! Basic example demonstrating how to use the rumoca Compiler API
//!
//! This example shows how to compile a simple Modelica model and inspect the resulting DAE.
//!
//! Run this example with:
//! ```sh
//! cargo run --example basic_usage
//! ```

use rumoca::Compiler;

fn main() -> anyhow::Result<()> {
    // Define a simple Modelica model as a string
    let modelica_code = r#"
model Integrator
    "A simple integrator model"
    Real x(start=0.0);
equation
    der(x) = 1.0;
end Integrator;
"#;

    // Compile the model using the Compiler API
    println!("Compiling Modelica model...\n");
    let result = Compiler::new()
        .model("Integrator") // Specify which class to compile
        .verbose(false) // Set to true to see detailed compilation output
        .compile_str(modelica_code, "Integrator.mo")?;

    // Print information about the compiled model
    println!("=== Compilation Results ===");
    println!("Model hash: {}", result.model_hash);
    println!("Rumoca version: {}", result.dae.rumoca_version);
    println!("\nTiming:");
    println!("  Parsing:    {:?}", result.parse_time);
    println!("  Flattening: {:?}", result.flatten_time);
    println!("  DAE:        {:?}", result.dae_time);
    println!("  Total:      {:?}", result.total_time());

    println!("\n=== DAE Structure ===");
    println!("States (x): {} variables", result.dae.x.len());
    for (name, component) in &result.dae.x {
        println!("  - {}: {}", name, component.type_name);
    }

    println!("\nAlgebraic variables (y): {}", result.dae.y.len());
    println!("Inputs (u): {}", result.dae.u.len());
    println!("Parameters (p): {}", result.dae.p.len());
    println!("Constants (cp): {}", result.dae.cp.len());

    println!("\nContinuous equations (fx): {}", result.dae.fx.len());
    for (i, eq) in result.dae.fx.iter().enumerate() {
        println!("  {}: {:?}", i, eq);
    }

    println!("\n=== Export to DAE IR JSON ===");
    match result.to_dae_ir_json() {
        Ok(json) => println!("JSON export successful ({} bytes)", json.len()),
        Err(e) => println!("JSON export failed: {}", e),
    }

    Ok(())
}
