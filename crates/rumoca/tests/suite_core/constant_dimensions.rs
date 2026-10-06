//! MLS 3.7 sections 5.3, 7.1, 10.1, and 12.6: a dimension that names a
//! constant evaluates that constant's binding in the constant's own lexical
//! scope, whichever class declares the array, and an implicit record
//! constructor call carries the constructor's resolved function metadata.

use rumoca::Compiler;
use rumoca_core::VarName;

fn flat(model: &str, source: &str) -> Result<rumoca_ir_flat::Model, String> {
    Compiler::new()
        .model(model)
        .compile_str_flat(source, &format!("{model}.mo"))
        .map_err(|error| format!("{error:#}"))
}

fn compile(model: &str, source: &str) -> Result<rumoca::CompilationResult, String> {
    Compiler::new()
        .model(model)
        .compile_str(source, &format!("{model}.mo"))
        .map_err(|error| format!("{error:#}"))
}

fn dims(flat: &rumoca_ir_flat::Model, name: &str) -> Vec<i64> {
    flat.variables
        .get(&VarName::new(name))
        .unwrap_or_else(|| panic!("flat variable {name}"))
        .dims
        .clone()
}

const PACKAGE_DIMENSIONS: &str = "
package K
  constant Integer n = 3;
end K;
package V
  constant Integer cap = 4;
  constant Integer alias = K.n;
  constant Integer product = K.n * cap;
end V;
model Reader
  Real a[V.cap](each start = 1);
  Real b[V.alias](each start = 1);
  Real c[V.product](each start = 1);
equation
  der(a) = -a;
  der(b) = -b;
  der(c) = -c;
end Reader;
";

#[test]
fn package_constants_and_alias_chains_size_arrays_from_outside_their_package() {
    let model = flat("Reader", PACKAGE_DIMENSIONS).expect("package constants are dimensions");
    assert_eq!(dims(&model, "a"), vec![4]);
    assert_eq!(dims(&model, "b"), vec![3]);
    assert_eq!(dims(&model, "c"), vec![12]);
}

const RECORD_FIELD_DIMENSIONS: &str = "
package K
  constant Integer capacity = 4;
end K;
package V
  constant Integer featureCapacity = K.capacity;
  constant Integer slots = 2;
  record Proposal
    Real rms;
    Boolean inliers[featureCapacity];
  end Proposal;
  record Item
    Real v[featureCapacity];
  end Item;
  record Table
    Item items[slots];
    Integer count;
  end Table;
  function Make
    input Real r;
    output Proposal p;
  algorithm
    p.rms := r;
    p.inliers := fill(true, featureCapacity);
  end Make;
  function Total
    input Table t;
    output Real s;
  algorithm
    s := 0;
    for i in 1:slots loop
      s := s + sum(t.items[i].v);
    end for;
  end Total;
end V;
model Holder
  input Real r;
  V.Proposal proposal;
  V.Table table;
  output Real total;
equation
  proposal = V.Make(r);
  table.count = 1;
  for i in 1:2 loop
    table.items[i].v = fill(r, 4);
  end for;
  total = V.Total(table);
end Holder;
";

#[test]
fn record_field_dimensions_resolve_in_the_record_declaration_scope() {
    let model = flat("Holder", RECORD_FIELD_DIMENSIONS).expect("record field dimensions resolve");
    assert_eq!(dims(&model, "proposal.inliers"), vec![4]);
    // `Table.items` is an array of records whose field `v` is sized by a
    // constant of the declaring package: two items of four elements each.
    let item_scalars: i64 = model
        .variables
        .iter()
        .filter(|(name, _)| {
            name.as_str().starts_with("table.items") && name.as_str().ends_with("v")
        })
        .map(|(_, variable)| variable.dims.iter().product::<i64>().max(1))
        .sum();
    assert_eq!(item_scalars, 8);
}

const CONSTRUCTOR_COPY: &str = "
package V
  constant Integer slots = 4;
  record Snapshot
    Integer nextId = 1;
    Boolean occupied[slots] = fill(false, slots);
    Real point[slots, 3];
    Real level = 0.5;
  end Snapshot;
  function Advance
    input Snapshot previous;
    output Snapshot next;
  algorithm
    next := previous;
    next.nextId := previous.nextId + 1;
  end Advance;
end V;
model Step
  input V.Snapshot previous;
  output V.Snapshot next;
equation
  next = V.Advance(previous);
end Step;
";

#[test]
fn a_whole_record_copy_builds_the_constructor_call_with_resolved_metadata() {
    let model = flat("Step", CONSTRUCTOR_COPY).expect("the record copy flattens");
    assert_eq!(dims(&model, "next.occupied"), vec![4]);
    assert_eq!(dims(&model, "next.point"), vec![4, 3]);
    let advance = model
        .functions
        .values()
        .find(|function| function.name.as_str().ends_with("Advance"))
        .expect("Advance is collected");
    let copies: Vec<_> = advance
        .body
        .iter()
        .filter_map(|statement| match statement {
            rumoca_core::Statement::Assignment {
                value:
                    rumoca_core::Expression::FunctionCall {
                        name,
                        is_constructor: true,
                        ..
                    },
                ..
            } => Some(name),
            _ => None,
        })
        .collect();
    assert!(
        !copies.is_empty(),
        "the whole-record copy is a constructor call"
    );
    for name in copies {
        assert!(
            name.resolved_function().is_some(),
            "constructor `{name}` carries its resolved function metadata"
        );
    }
}

#[test]
fn a_dimension_that_reads_a_time_varying_variable_is_refused() {
    let source = "
model Varying
  Real n(start = 3);
  Real x[integer(n)];
equation
  der(n) = 0;
  der(x) = -x;
end Varying;
";
    assert!(compile("Varying", source).is_err());
}

#[test]
fn cyclic_constant_dimensions_are_refused() {
    let source = "
package P
  constant Integer a = b;
  constant Integer b = a;
end P;
model Cycle
  Real x[P.a];
equation
  der(x) = -x;
end Cycle;
";
    assert!(compile("Cycle", source).is_err());
}
