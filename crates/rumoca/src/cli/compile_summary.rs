//! Human-readable summary of a compiled model.

use crate::CompilationResult;

pub(super) fn print_summary(model: &str, result: &CompilationResult) {
    let (states, algebraics, parameters, constants, inputs, outputs, continuous, initial) =
        result.dae.inspect(|view| {
            let mut roles = [0usize; 6];
            for (_, variable) in view.variables() {
                match variable.role() {
                    rumoca_compile::compile::VariableRole::State => roles[0] += 1,
                    rumoca_compile::compile::VariableRole::Algebraic => roles[1] += 1,
                    rumoca_compile::compile::VariableRole::Parameter => roles[2] += 1,
                    rumoca_compile::compile::VariableRole::Constant => roles[3] += 1,
                    rumoca_compile::compile::VariableRole::Input => roles[4] += 1,
                    rumoca_compile::compile::VariableRole::Output => roles[5] += 1,
                    rumoca_compile::compile::VariableRole::DiscreteReal
                    | rumoca_compile::compile::VariableRole::DiscreteValue => {}
                }
            }
            (
                roles[0],
                roles[1],
                roles[2],
                roles[3],
                roles[4],
                roles[5],
                view.continuous_owner_count(),
                view.initialization_owner_count(),
            )
        });
    println!("Compilation successful!");
    println!();
    println!("Model: {}", model);
    println!("States: {states}");
    println!("Algebraics: {algebraics}");
    println!("Parameters: {parameters}");
    println!("Constants: {constants}");
    println!("Inputs: {inputs}");
    println!("Outputs: {outputs}");
    println!();
    println!("Continuous equations (f_x): {}", continuous);
    println!("Initial equations: {}", initial);
    println!();
    println!("Balance: {} (equations - unknowns)", result.balance());
    if result.is_balanced() {
        println!("Status: BALANCED");
    } else {
        println!("Status: UNBALANCED");
    }
    println!();
    println!(
        "Use `rumoca compile <file> --emit dae-mo` to dump the DAE IR as Modelica (or dae-json)"
    );
    println!("Use `rumoca compile <file> --emit solve-json` to dump the solver IR");
    println!(
        "Use `rumoca compile <file> --target <TARGET>` for code generation (`rumoca targets` to list)"
    );
    println!(
        "Use `rumoca sim <file> --inspect structure` for BLT/tearing/SCC analysis (also `--inspect eval|jacobian`)"
    );
}
