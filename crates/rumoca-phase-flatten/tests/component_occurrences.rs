//! Flattening records which class equation each flat equation instantiates and
//! for which component, so repeated component structure is carried to later
//! phases instead of being rediscovered from names or spans.

use rumoca_ir_ast as ast;
use rumoca_ir_flat as flat;

fn flatten_source(source: &str, model: &str) -> flat::Model {
    let file_name = "<component_occurrences>";
    let stored = rumoca_phase_parse::parse_to_ast(source, file_name).expect("source parses");
    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map.add(file_name, source);
    let resolved =
        rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree)).expect("source resolves");
    let instanced =
        rumoca_phase_instantiate::instantiate(resolved, model).expect("model instantiates");
    let ast::InstancedTree { tree, mut overlay } = instanced;
    rumoca_phase_typecheck::typecheck_instanced(&tree, &mut overlay, model)
        .expect("instanced model typechecks");
    rumoca_phase_flatten::flatten_ref(&tree, &overlay, model).expect("model flattens")
}

const SOURCE: &str = "
model Unit
  Real x(start = 1, fixed = true);
  Real y;
equation
  der(x) = -x;
  y = 2 * x;
end Unit;
model Probe
  Unit a;
  Unit b;
  Real z;
equation
  z = a.y + b.y;
end Probe;
";

fn occurrences_from(model: &flat::Model, component: &str) -> Vec<rumoca_core::ComponentOccurrence> {
    model
        .equations
        .iter()
        .filter(|equation| equation.origin.component_name() == Some(component))
        .map(|equation| {
            equation
                .occurrence
                .expect("component equation records its occurrence")
        })
        .collect()
}

#[test]
fn equations_of_two_components_of_one_class_share_class_equations() {
    let model = flatten_source(SOURCE, "Probe");
    let a = occurrences_from(&model, "a");
    let b = occurrences_from(&model, "b");
    assert_eq!(a.len(), 2);
    assert_eq!(b.len(), 2);
    for (left, right) in a.iter().zip(&b) {
        assert_eq!(left.class_equation, right.class_equation);
        assert_eq!(left.view, right.view);
        assert_ne!(left.instance, right.instance);
    }
    assert_ne!(a[0].class_equation, a[1].class_equation);
    assert!(
        a.iter()
            .all(|occurrence| occurrence.instance == a[0].instance)
    );
}

#[test]
fn the_top_level_equation_is_its_own_class_equation() {
    let model = flatten_source(SOURCE, "Probe");
    let top = occurrences_from(&model, "");
    assert_eq!(top.len(), 1);
    let units = occurrences_from(&model, "a");
    assert!(
        units
            .iter()
            .all(|unit| unit.class_equation != top[0].class_equation)
    );
}
