use rumoca_core::{Expression, ExpressionVisitor};
use rumoca_ir_ast as ast;

const SOURCE: &str = r#"
package Modelica
  extends Modelica.Icons.Package;

  package Icons
    partial package Package
    end Package;

    partial function Conversion
    end Conversion;
  end Icons;

  package Units
    extends Modelica.Icons.Package;

    package Conversions
      extends Modelica.Icons.Package;

      function to_unit1
        extends Modelica.Icons.Conversion;
        input Real r;
        output Real result;
      algorithm
        result := r;
      end to_unit1;
    end Conversions;
  end Units;
end Modelica;

model UsesVectorizedCall
  Real x[3] = {1.0, 2.0, 3.0};
  Real y[3] = Modelica.Units.Conversions.to_unit1(x);
end UsesVectorizedCall;
"#;

fn flatten_source() -> rumoca_ir_flat::Model {
    let file_name = "<function_vectorization_certificate>";
    let stored = rumoca_phase_parse::parse_to_ast(SOURCE, file_name).expect("source parses");
    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map.add(file_name, SOURCE);
    let resolved =
        rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree)).expect("source resolves");
    let ast::InstancedTree { tree, mut overlay } =
        rumoca_phase_instantiate::instantiate(resolved, "UsesVectorizedCall")
            .expect("model instantiates");
    rumoca_phase_typecheck::typecheck_instanced(&tree, &mut overlay, "UsesVectorizedCall")
        .expect("model typechecks");
    rumoca_phase_flatten::flatten_ref(&tree, &overlay, "UsesVectorizedCall")
        .expect("model flattens")
}

#[derive(Default)]
struct Calls(Vec<rumoca_core::Reference>);

impl ExpressionVisitor for Calls {
    fn visit_function_call(
        &mut self,
        name: &rumoca_core::Reference,
        args: &[Expression],
        is_constructor: bool,
    ) {
        self.0.push(name.clone());
        self.walk_function_call(name, args, is_constructor);
    }
}

#[test]
fn direct_nonreplaceable_function_path_carries_func026_certificate() {
    let model = flatten_source();
    let function = model
        .functions
        .values()
        .find(|function| function.name.as_str() == "Modelica.Units.Conversions.to_unit1")
        .expect("selected function is collected");

    let mut calls = Calls::default();
    for variable in model.variables.values() {
        if let Some(binding) = &variable.binding {
            calls.visit_expression(binding);
        }
    }
    let call = calls
        .0
        .iter()
        .find(|call| call.as_str() == "Modelica.Units.Conversions.to_unit1")
        .expect("vectorized call remains in Flat");
    let component = call
        .component_ref()
        .expect("call retains structured identity");
    assert!(function.transitively_non_replaceable);
    assert!(
        call.resolved_function()
            .is_some_and(|resolved| resolved.transitively_non_replaceable)
    );
    assert!(!component.parts().is_empty());
}
