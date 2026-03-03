//! Test that array subscripts are correctly preserved in variable names.
//!
//! MLS §10.1: Array subscripts are part of the variable identity.

use rumoca_ir_ast as ast;
use rumoca_phase_flatten::flatten_ref;
use rumoca_phase_instantiate::instantiate_model;
use rumoca_phase_resolve::resolve;
use rumoca_phase_typecheck::typecheck_instanced;

#[test]
fn test_array_subscripts_in_variable_names() {
    // Create a simple test model with arrays of components
    let model_src = r#"
package ArrayTest
  connector Pin
    Real v "Potential at the pin";
    flow Real i "Current flowing into the pin";
  end Pin;

  model Resistor
    Pin p, n;
    parameter Real R = 1 "Resistance";
    Real i;
  equation
    i = p.i;
    p.i + n.i = 0;
    R * i = p.v - n.v;
  end Resistor;

  model Ground
    Pin p;
  equation
    p.v = 0;
  end Ground;

  model TestArray
    Resistor r[3](each R = 100);
    Ground g;
  equation
    for i in 1:3 loop
      connect(r[i].n, g.p);
    end for;
    r[1].p.v = 10;
    r[2].p.v = 10;
    r[3].p.v = 10;
  end TestArray;
end ArrayTest;
"#;

    let def = rumoca_phase_parse::parse_to_ast(model_src, "test.mo").unwrap();
    let tree = ast::ClassTree::from_parsed(def);
    let parsed = rumoca_ir_ast::ParsedTree::new(tree);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let model = "ArrayTest.TestArray";
    let tree = &resolved.0;

    let mut overlay = instantiate_model(tree, model).expect("instantiate should succeed");
    typecheck_instanced(tree, &mut overlay, model).expect("typecheck should succeed");
    let flat = flatten_ref(tree, &overlay, model).expect("flatten should succeed");

    // Check that array-subscripted variables exist with [index] in name
    let has_r1_p_v = flat.variables.iter().any(|(n, _)| n.as_str() == "r[1].p.v");
    let has_r2_p_v = flat.variables.iter().any(|(n, _)| n.as_str() == "r[2].p.v");
    let has_r3_p_v = flat.variables.iter().any(|(n, _)| n.as_str() == "r[3].p.v");

    println!("Variables with array subscripts:");
    for (name, _) in flat.variables.iter() {
        if name.as_str().contains('[') {
            println!("  {}", name);
        }
    }

    println!("\nVariable checks:");
    println!("  r[1].p.v exists: {}", has_r1_p_v);
    println!("  r[2].p.v exists: {}", has_r2_p_v);
    println!("  r[3].p.v exists: {}", has_r3_p_v);

    // Check that equations reference indexed variables properly
    println!("\nEquations:");
    for eq in flat.equations.iter() {
        println!("  {} (scalar_count={})", eq.origin, eq.scalar_count);
    }

    // Assert variables have proper array subscripts
    assert!(has_r1_p_v, "Should have r[1].p.v variable");
    assert!(has_r2_p_v, "Should have r[2].p.v variable");
    assert!(has_r3_p_v, "Should have r[3].p.v variable");
}
