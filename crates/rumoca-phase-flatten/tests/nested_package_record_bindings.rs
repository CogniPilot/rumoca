//! Record declaration bindings retain the concrete instance scope when the
//! record type is selected through a nested package alias.

use rumoca_core::Expression;
use rumoca_ir_ast as ast;

const SOURCE: &str = r#"
package P
  package Components
    record VehicleParameters
      Real mass = 0.063;
      Real gravity = 9.81;
      Real weight = mass * gravity;
    end VehicleParameters;
  end Components;

  model Controller
    import C = P.Components;
    parameter C.VehicleParameters vehicle = C.VehicleParameters();
    output Real weight;
  equation
    weight = vehicle.weight;
  end Controller;
end P;
"#;

const SAME_PACKAGE_SOURCE: &str = r#"
package P
  package Components
    record VehicleParameters
      Real mass = 0.063;
      Real gravity = 9.81;
      Real weight = mass * gravity;
    end VehicleParameters;

    function readWeight
      input VehicleParameters vehicle;
      output Real weight;
    algorithm
      weight := vehicle.weight;
    end readWeight;

    block Controller
      parameter VehicleParameters vehicle = VehicleParameters();
      output Real weight;
    equation
      weight = readWeight(vehicle);
    end Controller;
  end Components;
end P;
"#;

#[test]
fn nested_package_record_binding_uses_concrete_record_instance_scope() {
    let stored = rumoca_phase_parse::parse_to_ast(SOURCE, "nested-package-record.mo")
        .expect("fixture parses");
    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map.add("nested-package-record.mo", SOURCE);
    let resolved =
        rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree)).expect("fixture resolves");
    let ast::InstancedTree { tree, mut overlay } =
        rumoca_phase_instantiate::instantiate(resolved, "P.Controller")
            .expect("fixture instantiates");
    rumoca_phase_typecheck::typecheck_instanced(&tree, &mut overlay, "P.Controller")
        .expect("fixture typechecks");
    let source_map = tree.source_map.clone();
    let flat = rumoca_phase_flatten::flatten_ref(&tree, &overlay, "P.Controller")
        .expect("fixture flattens");

    let binding = flat.variables[&rumoca_core::VarName::new("vehicle.weight")]
        .binding
        .as_ref()
        .expect("record field keeps its declaration binding");
    let Expression::Binary { lhs, rhs, .. } = binding else {
        panic!("weight binding remains a product");
    };
    let Expression::VarRef { name: lhs, .. } = lhs.as_ref() else {
        panic!("mass operand remains a variable reference");
    };
    let Expression::VarRef { name: rhs, .. } = rhs.as_ref() else {
        panic!("gravity operand remains a variable reference");
    };
    assert_eq!(lhs.as_str(), "vehicle.mass");
    assert_eq!(rhs.as_str(), "vehicle.gravity");

    rumoca_phase_dae::to_dae(&flat, source_map)
        .expect("the scoped record binding lowers to checked DAE");
}

#[test]
fn same_package_record_binding_uses_concrete_record_instance_scope() {
    let stored = rumoca_phase_parse::parse_to_ast(SAME_PACKAGE_SOURCE, "same-package-record.mo")
        .expect("fixture parses");
    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map
        .add("same-package-record.mo", SAME_PACKAGE_SOURCE);
    let resolved =
        rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree)).expect("fixture resolves");
    let ast::InstancedTree { tree, mut overlay } =
        rumoca_phase_instantiate::instantiate(resolved, "P.Components.Controller")
            .expect("fixture instantiates");
    rumoca_phase_typecheck::typecheck_instanced(&tree, &mut overlay, "P.Components.Controller")
        .expect("fixture typechecks");
    let source_map = tree.source_map.clone();
    let flat = rumoca_phase_flatten::flatten_ref(&tree, &overlay, "P.Components.Controller")
        .expect("fixture flattens");

    let binding = flat.variables[&rumoca_core::VarName::new("vehicle.weight")]
        .binding
        .as_ref()
        .expect("record field keeps its declaration binding");
    let Expression::Binary { lhs, rhs, .. } = binding else {
        panic!("weight binding remains a product");
    };
    let Expression::VarRef { name: lhs, .. } = lhs.as_ref() else {
        panic!("mass operand remains a variable reference");
    };
    let Expression::VarRef { name: rhs, .. } = rhs.as_ref() else {
        panic!("gravity operand remains a variable reference");
    };
    assert_eq!(lhs.as_str(), "vehicle.mass");
    assert_eq!(rhs.as_str(), "vehicle.gravity");

    rumoca_phase_dae::to_dae(&flat, source_map)
        .expect("the scoped record binding lowers to checked DAE");
}
