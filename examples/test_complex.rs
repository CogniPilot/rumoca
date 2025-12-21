//! Test Complex type detection
use rumoca::Compiler;

fn main() {
    let source = r#"
model Test
    Complex y;
    Complex u1;
    Complex u2;
    Complex k1 = Complex(1, 0);
    Complex k2 = Complex(1, 0);
equation
    y = k1 * u1 + k2 * u2;
end Test;
"#;

    // Compile with verbose mode to see debug output
    match Compiler::new()
        .model("Test")
        .verbose(true)
        .compile_str(source, "<test>")
    {
        Ok(result) => {
            println!("\n=== Final result ===");
            println!("Equations: {}", result.balance.num_equations);
            println!("Unknowns: {}", result.balance.num_unknowns);
            println!("Balanced: {}", result.is_balanced());

            println!("\n=== DAE equations ===");
            for (i, eq) in result.dae.fx.iter().enumerate() {
                println!("  Eq {}: {:?}", i, eq);
            }
        }
        Err(e) => {
            println!("Compile error: {}", e);
        }
    }
}
