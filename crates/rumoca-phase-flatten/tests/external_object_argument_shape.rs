//! A scalar function applied to an external object does not broadcast the
//! array dimensions the object was constructed from (MLS §12.4.6 maps a
//! function only over actual dimensions its formal does not declare).

use rumoca_ir_ast as ast;
use rumoca_ir_flat as flat;

const SOURCE: &str = r#"
class Table
  extends ExternalObject;

  function constructor
    input Real data[:, 2];
    output Table handle;
    external "C" handle = make_table(data);
  end constructor;

  function destructor
    input Table handle;
    external "C" free_table(handle);
  end destructor;
end Table;

function lookup
  input Table handle;
  input Real u;
  output Real y;
  external "C" y = table_value(handle, u);
end lookup;

model UsesTable
  parameter Real data[1, 2] = [0, 0.5];
  final parameter Table handle = Table(data);
  final parameter Real gain = lookup(handle, 0);
  Real force(start = 1);
  Real torque;
equation
  force = 2 + time;
  torque = gain * force;
end UsesTable;
"#;

fn flatten_source() -> flat::Model {
    let file_name = "<external_object_argument_shape>";
    let stored = rumoca_phase_parse::parse_to_ast(SOURCE, file_name).expect("source parses");
    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map.add(file_name, SOURCE);
    let resolved =
        rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree)).expect("source resolves");
    let instanced =
        rumoca_phase_instantiate::instantiate(resolved, "UsesTable").expect("model instantiates");
    let ast::InstancedTree { tree, mut overlay } = instanced;
    rumoca_phase_typecheck::typecheck_instanced(&tree, &mut overlay, "UsesTable")
        .expect("model typechecks");
    rumoca_phase_flatten::flatten_ref(&tree, &overlay, "UsesTable").expect("model flattens")
}

#[test]
fn scalar_lookup_through_external_object_keeps_scalar_equation_cardinality() {
    let model = flatten_source();
    let counts = model
        .equations
        .iter()
        .map(|equation| equation.scalar_count)
        .collect::<Vec<_>>();
    assert!(
        counts.iter().all(|count| *count == 1),
        "every equation of the scalar model is scalar: {counts:?}"
    );
}
