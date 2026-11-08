//! Resolved identity through record-valued parameter modifications (MLS §7.2).

use rumoca_ir_ast as ast;

fn flatten(source: &str) -> rumoca_ir_flat::Model {
    let file = "record_member_identity.mo";
    let parsed = rumoca_phase_parse::parse_to_ast(source, file).expect("fixture parses");
    let mut tree = ast::ClassTree::from_parsed(parsed);
    tree.source_map.add(file, source);
    let resolved =
        rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree)).expect("fixture resolves");
    let instanced =
        rumoca_phase_instantiate::instantiate(resolved, "Top").expect("fixture instantiates");
    let ast::InstancedTree { tree, mut overlay } = instanced;
    rumoca_phase_typecheck::typecheck_instanced(&tree, &mut overlay, "Top")
        .expect("fixture typechecks");
    rumoca_phase_flatten::flatten_ref(&tree, &overlay, "Top")
        .unwrap_or_else(|error| panic!("typed flatten failure: {error:?}"))
}

#[test]
fn record_parameter_member_modification_keeps_identity() {
    let flat = flatten(
        r#"
record Data
  parameter Real gain(fixed=false) annotation(Evaluate=false);
  parameter Real limit = gain * 3;
  parameter Real acceleration = limit / 2;
end Data;
model Controller
  parameter Real maximum;
  parameter Real minimum = -maximum;
  Real y = maximum + minimum;
end Controller;
partial model Base
  replaceable parameter Data data constrainedby Data;
end Base;
model Top
  extends Base;
  Controller controller(maximum = data.limit);
end Top;
"#,
    );
    assert!(!flat.variables.is_empty());
}

#[test]
fn forwarded_record_member_modification_keeps_identity() {
    let flat = flatten(
        r#"
record Data
  parameter Real gain(fixed=false) annotation(Evaluate=false);
  parameter Real limit = gain * 3;
end Data;
model Constant
  parameter Real k;
  Real y = k;
end Constant;
model Controller
  parameter Real maximum;
  Constant source(k = maximum);
end Controller;
partial model Base
  replaceable parameter Data data constrainedby Data;
end Base;
model Top
  extends Base;
  Controller controller(maximum = data.limit);
end Top;
"#,
    );
    let forwarded = &flat.variables[&rumoca_core::VarName::new("controller.source.k")];
    let maximum = &flat.variables[&rumoca_core::VarName::new("controller.maximum")];
    let rumoca_core::Expression::VarRef { name, .. } = forwarded.binding.as_ref().unwrap() else {
        panic!("forwarding must retain the controller parameter reference");
    };
    assert_eq!(name.as_str(), "controller.maximum");
    assert_eq!(
        name.component_ref().unwrap().target_def_id(),
        maximum.component_ref.as_ref().unwrap().target_def_id(),
    );
}

#[test]
fn structural_function_keeps_record_and_array_parameter_values() {
    let flat = flatten(
        r#"
record Data
  Real values[2];
end Data;
function positive
  input Data data;
  output Boolean yes;
algorithm
  yes := sum(data.values) > 0;
end positive;
model Top
  parameter Data data(values={2,3});
  parameter Boolean enabled = positive(data);
  Real x;
equation
  if enabled then
    x=1;
  else
    x=2;
  end if;
end Top;
"#,
    );
    assert_eq!(flat.equations.len(), 1);
    assert!(flat.structured_equations.is_empty());
}
