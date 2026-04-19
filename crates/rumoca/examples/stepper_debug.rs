//! Quick diagnostic — step the autopilot model and print state.

use rumoca_solver_diffsol::{SimStepper, StepperOptions};

const MODEL_SOURCE: &str = include_str!("../../../examples/QuadrotorAttitude.mo");

fn main() -> anyhow::Result<()> {
    let compiler = rumoca::Compiler::new().model("QuadrotorAttitude");
    let result = compiler.compile_str(MODEL_SOURCE, "QuadrotorAttitude.mo")?;

    let mut stepper = SimStepper::new(&result.dae, StepperOptions::default())?;
    println!("Inputs: {:?}", stepper.input_names());
    println!("Variables: {:?}", stepper.variable_names());

    // Print initial state
    println!("\n=== Initial state ===");
    for name in [
        "px", "py", "pz", "vx", "vy", "vz", "q0", "q1", "q2", "q3", "T",
    ] {
        println!("  {} = {:?}", name, stepper.get(name));
    }

    // Step 1 at a time to find where it hangs
    println!("\n=== Stepping with no input ===");
    for i in 0..10 {
        let start = std::time::Instant::now();
        stepper.step(0.02)?;
        let elapsed = start.elapsed();
        println!(
            "  step {} took {:?}, t={:.4}, pz={:.6}",
            i,
            elapsed,
            stepper.time(),
            stepper.get("pz").unwrap_or(0.0)
        );
    }
    for name in [
        "px", "py", "pz", "vx", "vy", "vz", "q0", "q1", "q2", "q3", "T",
    ] {
        println!("  {} = {:?}", name, stepper.get(name));
    }

    // Hover: thrust = mass * g = 2.0 * 9.80665 = 19.613
    let hover_thrust = 2.0 * 9.80665;
    println!("\n=== Hover for 1s (thrust={:.1}N) ===", hover_thrust);
    stepper.set_input("cmd_thrust", hover_thrust)?;
    for i in 0..50 {
        stepper.step(0.02)?;
        if i % 10 == 0 {
            println!(
                "  t={:.2} pz={:.4} vz={:.4}",
                stepper.time(),
                stepper.get("pz").unwrap_or(0.0),
                stepper.get("vz").unwrap_or(0.0)
            );
        }
    }

    // Climb: more thrust
    println!("\n=== Climb for 1s (thrust={:.1}N) ===", hover_thrust * 1.3);
    stepper.set_input("cmd_thrust", hover_thrust * 1.3)?;
    for i in 0..50 {
        stepper.step(0.02)?;
        if i % 10 == 0 {
            println!(
                "  t={:.2} pz={:.4} vz={:.4}",
                stepper.time(),
                stepper.get("pz").unwrap_or(0.0),
                stepper.get("vz").unwrap_or(0.0)
            );
        }
    }

    // Roll right
    println!("\n=== Roll 15deg right for 1s ===");
    stepper.set_input("cmd_roll", 0.26)?; // ~15 degrees
    stepper.set_input("cmd_thrust", hover_thrust)?;
    for i in 0..50 {
        stepper.step(0.02)?;
        if i % 10 == 0 {
            println!(
                "  t={:.2} px={:.3} py={:.3} pz={:.3} roll={:.2}°",
                stepper.time(),
                stepper.get("px").unwrap_or(0.0),
                stepper.get("py").unwrap_or(0.0),
                stepper.get("pz").unwrap_or(0.0),
                stepper.get("roll").unwrap_or(0.0).to_degrees()
            );
        }
    }

    println!("\nFinal time = {}", stepper.time());
    Ok(())
}
