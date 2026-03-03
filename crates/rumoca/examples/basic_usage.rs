//! Basic usage of the public `rumoca::Compiler` API.
//!
//! Run with:
//! `cargo run --example basic_usage -p rumoca`

use rumoca::Compiler;

fn main() -> anyhow::Result<()> {
    let modelica_code = r#"
model Integrator
    Real x(start=0.0);
equation
    der(x) = 1.0;
end Integrator;
"#;

    let result = Compiler::new()
        .model("Integrator")
        .compile_str(modelica_code, "Integrator.mo")?;

    println!("States (x): {}", result.dae.states.len());
    println!("Algebraics (y): {}", result.dae.algebraics.len());
    println!("Continuous equations (f_x): {}", result.dae.f_x.len());
    println!("Balance (eq - unknown): {}", result.dae.balance());
    println!("DAE JSON bytes: {}", result.to_json()?.len());

    Ok(())
}
