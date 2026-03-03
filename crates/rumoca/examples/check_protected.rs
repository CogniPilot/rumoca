//! Demonstrate how protected declarations are preserved in flat IR.
//!
//! Run with:
//! `cargo run --example check_protected -p rumoca`

use rumoca::Compiler;

fn main() -> anyhow::Result<()> {
    let source = r#"
model ProtectedDemo
    parameter Real public_gain = 2;
protected
    parameter Real protected_gain = 3;
    Real hidden(start = 0);
equation
    hidden = public_gain + protected_gain;
end ProtectedDemo;
"#;

    let result = Compiler::new()
        .model("ProtectedDemo")
        .compile_str(source, "<protected_demo>")?;

    println!("Protected flags in flat IR:");
    for name in ["public_gain", "protected_gain", "hidden"] {
        if let Some((_, variable)) = result
            .flat
            .variables
            .iter()
            .find(|(var_name, _)| var_name.as_str() == name)
        {
            println!(
                "  {}: is_protected={}, causality={:?}",
                name, variable.is_protected, variable.causality
            );
        }
    }

    Ok(())
}
