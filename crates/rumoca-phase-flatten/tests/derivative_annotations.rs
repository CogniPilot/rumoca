use rumoca_core::FunctionDerivativeInput::{Differentiate, NoDerivative};
use rumoca_ir_ast as ast;

#[test]
fn derivative_dependencies_and_record_input_roles_survive_flattening() {
    let source = r#"
package Kinematics
  record Pair
    Real a;
    Real b;
  end Pair;
  function f
    input Pair moving;
    input Pair constantPair;
    input Real v;
    output Real y;
  algorithm
    y := moving.a + constantPair.b + v;
    annotation(derivative(noDerivative=constantPair)=df);
  end f;
  function df
    input Pair moving;
    input Pair constantPair;
    input Real v;
    input Pair der_moving;
    input Real der_v;
    output Real der_y;
  algorithm
    der_y := der_moving.a + der_v;
  end df;
end Kinematics;
model Probe
  Real u(start=1);
  Real y;
equation
  der(u) = -u;
  y = Kinematics.f(Kinematics.Pair(u,2*u), Kinematics.Pair(3,4), u);
end Probe;
"#;
    let flat = flatten(source, "Probe");
    let function = &flat.functions[&rumoca_core::VarName::new("Kinematics.f")];
    let [annotation] = function.derivatives.as_slice() else {
        panic!("the derivative annotation must survive extraction and collection");
    };
    let target = annotation
        .derivative_function
        .resolved_function()
        .unwrap()
        .instance_id;
    let derivative = flat
        .get_function_instance(target)
        .expect("annotation dependency is retained");
    assert_eq!(function.inputs.len(), 5);
    assert_eq!(
        annotation.inputs,
        [
            Differentiate,
            Differentiate,
            NoDerivative,
            NoDerivative,
            Differentiate
        ]
    );
    assert_eq!(derivative.inputs.len(), 8);
    assert_eq!(
        annotation.derivative_function.target_def_id(),
        derivative.def_id
    );
    assert_eq!(
        Some(
            annotation
                .derivative_function
                .resolved_function()
                .unwrap()
                .instance_id
        ),
        derivative.instance_id,
    );
}

fn flatten(source: &str, name: &str) -> rumoca_ir_flat::Model {
    let stored = rumoca_phase_parse::parse_to_ast(source, "derivative_annotation.mo").unwrap();
    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map.add("derivative_annotation.mo", source);
    let resolved = rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree)).unwrap();
    let instanced = rumoca_phase_instantiate::instantiate(resolved, name).unwrap();
    let ast::InstancedTree { tree, mut overlay } = instanced;
    rumoca_phase_typecheck::typecheck_instanced(&tree, &mut overlay, name).unwrap();
    rumoca_phase_flatten::flatten_ref_with_options(&tree, &overlay, name, Default::default())
        .unwrap()
}
