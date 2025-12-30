//! Debug balance for a specific MSL model
//!
//! Run with:
//!   MODELICAPATH=/path/to/msl cargo run --release --example debug_balance

use rumoca::Compiler;

fn main() {
    let model_name = std::env::args().nth(1).unwrap_or_else(|| {
        "Modelica.Mechanics.Rotational.Components.GeneralAngleToTorqueAdaptor".to_string()
    });

    println!("Debugging balance for: {}", model_name);
    println!("============================================================\n");

    let result = Compiler::new()
        .model(&model_name)
        .include_from_modelica_path("Modelica")
        .expect("Failed to load Modelica from MODELICAPATH")
        .compile_str("within;", "<entry>")
        .expect("Compilation failed");

    let dae = result.dae();

    println!(
        "UNKNOWNS ({} total):",
        dae.x.len() + dae.y.len() + dae.z.len() + dae.m.len()
    );
    println!("  States (x): {} variables", dae.x.len());
    for (name, comp) in &dae.x {
        println!("    - {} : {:?} {:?}", name, comp.type_name, comp.shape);
    }
    println!("  Algebraic (y): {} variables", dae.y.len());
    for (name, comp) in &dae.y {
        println!("    - {} : {:?} {:?}", name, comp.type_name, comp.shape);
    }
    println!("  Discrete (z): {} variables", dae.z.len());
    for (name, comp) in &dae.z {
        println!("    - {} : {:?} {:?}", name, comp.type_name, comp.shape);
    }
    println!("  Mixed (m): {} variables", dae.m.len());
    for (name, comp) in &dae.m {
        println!("    - {} : {:?} {:?}", name, comp.type_name, comp.shape);
    }

    println!("\nEQUATIONS:");
    println!("  Differential (fx): {} equations", dae.fx.len());
    for (i, eq) in dae.fx.iter().enumerate() {
        println!("    [{}] {:?}", i, eq);
    }
    println!("  Algebraic (fz): {} equations", dae.fz.len());
    for (i, eq) in dae.fz.iter().enumerate() {
        println!("    [{}] {:?}", i, eq);
    }
    println!("  Event (fr): {} statements", dae.fr.len());
    for (name, stmt) in &dae.fr {
        println!("    {} = {:?}", name, stmt);
    }

    println!("\nPARAMETERS:");
    println!("  Parameters (p): {} values", dae.p.len());
    for (name, comp) in &dae.p {
        println!("    - {} = {:?}", name, comp.start);
    }
    println!("  Computed (cp): {} values", dae.cp.len());
    println!("  Inputs (u): {} values", dae.u.len());
    for (name, comp) in &dae.u {
        println!("    - {} : {:?}", name, comp.type_name);
    }

    println!("\nBALANCE CHECK:");
    let balance = dae.check_balance();
    println!("  Equations: {}", balance.num_equations);
    println!("  Unknowns:  {}", balance.num_unknowns);
    println!(
        "  Difference: {} ({})",
        balance.difference(),
        if balance.difference() > 0 {
            "over-determined"
        } else if balance.difference() < 0 {
            "under-determined"
        } else {
            "balanced"
        }
    );
    println!("  Status: {:?}", balance.status);
}
