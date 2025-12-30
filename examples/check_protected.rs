use rumoca::Compiler;

fn main() {
    let result = Compiler::new()
        .model("Modelica.Mechanics.Rotational.Components.GeneralAngleToTorqueAdaptor")
        .include_from_modelica_path("Modelica")
        .expect("Failed to load")
        .compile_str("within;", "<entry>")
        .expect("Compilation failed");

    println!("Checking protected status of components:");
    for name in ["u1", "u2", "pder", "pder2", "p"] {
        if let Some(comp) = result.expanded_class.components.get(name) {
            println!(
                "  {}: is_protected={}, causality={:?}",
                name, comp.is_protected, comp.causality
            );
        } else {
            println!("  {}: NOT FOUND", name);
        }
    }
}
