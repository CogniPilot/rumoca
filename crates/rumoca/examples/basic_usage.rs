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

    let (states, algebraics, equations) = result.dae.inspect(|view| {
        (
            view.variables()
                .filter(|(_, variable)| {
                    variable.role() == rumoca_compile::compile::VariableRole::State
                })
                .count(),
            view.variables()
                .filter(|(_, variable)| {
                    variable.role() == rumoca_compile::compile::VariableRole::Algebraic
                })
                .count(),
            view.continuous_owner_count(),
        )
    });
    println!("States (x): {states}");
    println!("Algebraics (y): {algebraics}");
    println!("Continuous equations (f_x): {equations}");
    println!("Balance (eq - unknown): {}", result.balance());
    println!("DAE JSON bytes: {}", result.to_json()?.len());

    Ok(())
}
