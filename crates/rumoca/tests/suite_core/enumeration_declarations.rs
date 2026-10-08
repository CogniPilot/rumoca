//! An enumeration variable carries the declaration of its type (MLS §4.9.5):
//! the qualified name and the literals in declaration order, so a consumer
//! that exposes the variable names its literals. A type derived from another
//! enumeration reaches the declaration of the type it derives from.

use rumoca::Compiler;

const SOURCE: &str = "
package Flight
  type Mode = enumeration(Off, Climb, Cruise);
  type Phase = Mode;
end Flight;
model Declared
  parameter Flight.Mode mode = Flight.Mode.Climb;
  parameter Flight.Phase phase = Flight.Mode.Cruise;
  parameter Integer count = 2;
end Declared;";

fn declaration(variable: &str) -> Option<(String, Vec<String>)> {
    let compiled = Compiler::new()
        .model("Declared")
        .compile_str(SOURCE, "enumeration_declarations.mo")
        .expect("the model compiles");
    compiled.dae.inspect(|view| {
        view.variables()
            .find(|(_, candidate)| candidate.name().as_str() == variable)
            .map(|(_, candidate)| candidate.enumeration().map(declared))
            .unwrap_or_else(|| panic!("{variable} is declared"))
    })
}

fn declared(declaration: &rumoca_core::EnumerationDeclaration) -> (String, Vec<String>) {
    (declaration.name.clone(), declaration.literals.clone())
}

#[test]
fn an_enumeration_variable_carries_its_types_declaration() {
    let expected = (
        "Flight.Mode".to_string(),
        ["Off", "Climb", "Cruise"].map(str::to_string).to_vec(),
    );
    assert_eq!(declaration("mode"), Some(expected.clone()));
    assert_eq!(declaration("phase"), Some(expected));
}

#[test]
fn a_variable_of_another_type_carries_no_declaration() {
    assert_eq!(declaration("count"), None);
}
