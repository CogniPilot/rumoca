//! Scalar projection of a field of a record field (MLS §10.6.1 component
//! references): the coordinates one scalar of `s.identity.weight` reads are
//! found through the enclosing record value, including a function result and
//! an array of records.

use std::collections::BTreeSet;

use rumoca::Compiler;
use rumoca_eval_dae::for_each_scalar_coordinate;
use rumoca_ir_dae as dae;

const SOURCE: &str = r#"
within;
package P
  record Identity
    Integer sequence;
    Real weight;
  end Identity;
  record Edge
    Integer id;
    Real r[2];
  end Edge;
  record State
    Identity identity;
    Edge edges[3];
    Real time;
  end State;
  function Seed
    input Real w;
    output State state;
  algorithm
    state.identity := Identity(7, w);
    for slot in 1:3 loop
      state.edges[slot] := Edge(slot, {slot * w, 2 * slot});
    end for;
    state.time := 2 * w;
  end Seed;
end P;
model NestedFieldProjection
  input Real w;
  output P.State s = P.Seed(w);
end NestedFieldProjection;
"#;

/// The field name and the input scalars every scalar of each non-record field
/// selection reads.
fn field_inputs(view: dae::DaeView<'_>) -> Vec<(String, Vec<BTreeSet<usize>>)> {
    let mut projected = Vec::new();
    for index in 0..view.expression_count() {
        let expression = view.expression_id(index).unwrap();
        let node = view.expression(expression).unwrap();
        let dae::ExpressionOperation::Field { base, field } = node.operation() else {
            continue;
        };
        let base = view.expression(base).unwrap();
        let name = base
            .value_type()
            .record_field_name(field as usize)
            .unwrap()
            .to_string();
        let Some(scalars) = node.value_type().scalar_count() else {
            continue;
        };
        let inputs = (0..scalars)
            .map(|scalar| scalar_inputs(view, expression, scalar))
            .collect();
        projected.push((name, inputs));
    }
    projected
}

/// The input scalars one scalar of `expression` reads.
fn scalar_inputs<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    scalar: usize,
) -> BTreeSet<usize> {
    let mut inputs = BTreeSet::new();
    for_each_scalar_coordinate(view, expression, scalar, None, |coordinate, at| {
        if let dae::CoordinateView::Input(_) = coordinate {
            inputs.insert(at);
        }
    })
    .expect("a field of a record field projects through its enclosing record");
    inputs
}

/// Element-major columns of `len` scalars: the even scalars read `w`, the
/// odd ones nothing.
fn alternating_columns(
    len: usize,
    w: &BTreeSet<usize>,
    none: &BTreeSet<usize>,
) -> Vec<BTreeSet<usize>> {
    (0..len)
        .map(|scalar| {
            if scalar % 2 == 0 {
                w.clone()
            } else {
                none.clone()
            }
        })
        .collect()
}

#[test]
fn fields_of_record_fields_project_through_the_enclosing_record() {
    let compiled = Compiler::new()
        .model("NestedFieldProjection")
        .compile_str(SOURCE, "NestedFieldProjection.mo")
        .expect("a record output bound to a function result compiles");
    compiled.dae.inspect(|view| {
        let projected = field_inputs(view);
        let reads = |name: &str| {
            projected
                .iter()
                .filter(|(field, _)| field == name)
                .map(|(_, inputs)| inputs.clone())
                .collect::<Vec<_>>()
        };
        let w = BTreeSet::from([0]);
        let none = BTreeSet::new();
        assert!(!reads("weight").is_empty(), "{projected:?}");
        assert!(
            reads("weight")
                .iter()
                .all(|inputs| inputs == &vec![w.clone()])
        );
        assert!(
            reads("sequence")
                .iter()
                .all(|inputs| inputs == &vec![none.clone()])
        );
        assert!(!reads("r").is_empty(), "{projected:?}");
        for inputs in reads("r") {
            // Element-major columns: r[k, 1] = k * w reads w, r[k, 2] = 2 * k
            // reads nothing.
            let expected = alternating_columns(inputs.len(), &w, &none);
            assert_eq!(inputs, expected);
        }
    });
}

fn value(report: &rumoca_sim::EvalAtReport, name: &str) -> f64 {
    report
        .solver_y
        .iter()
        .find(|slot| slot.name.replace(' ', "") == name)
        .unwrap_or_else(|| panic!("missing solver value {name}: {:?}", report.solver_y))
        .value
}

/// Solve lowering reads each continuous scalar of a record output bound to a
/// function result through the nested field paths, including the columns of
/// an array-of-records field the function fills in a loop.
#[test]
fn nested_record_function_results_lower_field_by_field() {
    let source = SOURCE.replace(
        "  input Real w;\n  output P.State s = P.Seed(w);",
        "  parameter Real w = 1.5;\n  output P.State s = P.Seed(w);\n  output Real y = s.identity.weight + s.edges[3].r[1] + s.edges[1].r[2];",
    );
    let compiled = Compiler::new()
        .model("NestedFieldProjection")
        .compile_str(&source, "NestedFieldProjection.mo")
        .expect("a record output bound to a function result compiles");
    let probe =
        rumoca_sim::eval_dae_at(&compiled.dae, &rumoca_sim::SimOptions::default(), &[], 0.0)
            .expect("nested record fields of a function result lower");
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    assert_eq!(value(&probe.report, "y"), 1.5 + 4.5 + 2.0);
}
