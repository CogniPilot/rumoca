//! MLS §9.4 regression: a `connect` that closes a loop in the virtual
//! connection graph must generate the reduced `equalityConstraint` residual
//! instead of the full element-wise equality.
//!
//! The fixture is a minimal overdetermined connector: record `Ori` carries two
//! elements but only one degree of freedom, and its `equalityConstraint`
//! returns a single residue. Three `Node` elements each declare
//! `Connections.branch(pin_a.R, pin_b.R)`, `Ground` declares
//! `Connections.root(pin.R)`, and four `connect` statements close the loop.
//! The spanning forest therefore has exactly one loop-closing connect, and that
//! one connect must contribute one residual row rather than two equalities.

use rumoca_ir_ast as ast;
use rumoca_ir_flat::EquationOrigin;
use rumoca_phase_flatten::flatten_ref;
use rumoca_phase_instantiate::{InstantiationOutcome, instantiate_model_with_outcome};
use rumoca_phase_resolve::resolve;
use rumoca_phase_typecheck::typecheck_instanced;

const SOURCE: &str = r#"
package OcLoop
  record Ori "overdetermined type: two elements, one degree of freedom"
    Real a;
    Real b;
    function equalityConstraint
      input Ori R1;
      input Ori R2;
      output Real residue[1];
    algorithm
      residue := {R1.a - R2.a};
    end equalityConstraint;
  end Ori;

  connector Pin
    Ori R;
    Real e;
    flow Real f;
  end Pin;

  model Node "one required spanning-tree edge: pin_a -- pin_b"
    Pin pin_a;
    Pin pin_b;
    parameter Real d = 1;
  equation
    Connections.branch(pin_a.R, pin_b.R);
    pin_b.R.a = pin_a.R.a + d;
    pin_b.R.b = pin_a.R.b;
    pin_b.e = pin_a.e + d;
    pin_a.f + pin_b.f = 0;
  end Node;

  model Ground
    Pin pin;
  equation
    Connections.root(pin.R);
    pin.R.a = 0;
    pin.R.b = 0;
    pin.e = 0;
  end Ground;

  model Loop3 "three branch elements closed into a loop"
    Ground g;
    Node n1;
    Node n2;
    Node n3(d = -2);
  equation
    connect(g.pin, n1.pin_a);
    connect(n1.pin_b, n2.pin_a);
    connect(n2.pin_b, n3.pin_a);
    connect(n3.pin_b, g.pin);
  end Loop3;
end OcLoop;
"#;

fn flatten_model(source: &str, model_name: &str) -> rumoca_ir_flat::Model {
    let def = rumoca_phase_parse::parse_to_ast(source, "overdetermined_loop.mo")
        .expect("parse should succeed");
    let mut tree = ast::ClassTree::from_parsed(def);
    tree.source_map.add("overdetermined_loop.mo", source);
    let parsed = ast::ParsedTree::new(tree);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let tree = resolved.inner();
    let mut overlay = match instantiate_model_with_outcome(tree, model_name) {
        InstantiationOutcome::Success(overlay) => overlay,
        InstantiationOutcome::NeedsInner { missing_inners, .. } => {
            panic!("fixture unexpectedly needs inner declarations: {missing_inners:?}")
        }
        InstantiationOutcome::Error(error) => panic!("fixture instantiation failed: {error}"),
    };
    typecheck_instanced(&resolved, &mut overlay, model_name).expect("typecheck should succeed");
    flatten_ref(tree, &overlay, model_name).expect("flatten should succeed")
}

fn record_name(flat: &rumoca_ir_flat::Model, instance: rumoca_core::InstanceId) -> String {
    flat.record_instances
        .iter()
        .find(|(_, record)| record.instance_id == instance)
        .map(|(name, _)| name.as_str().to_string())
        .expect("typed equalityConstraint owner resolves to one record instance")
}

fn function_name(
    flat: &rumoca_ir_flat::Model,
    instance: rumoca_core::FunctionInstanceId,
) -> String {
    flat.functions
        .iter()
        .find(|(_, function)| function.instance_id == Some(instance))
        .map(|(name, _)| name.as_str().to_string())
        .expect("typed equalityConstraint owner resolves to one function")
}

/// Every ordinary generated connection equation as a rendered endpoint pair.
fn connection_origins(flat: &rumoca_ir_flat::Model) -> Vec<(String, String)> {
    flat.equations
        .iter()
        .filter_map(|eq| match &eq.origin {
            EquationOrigin::Connection { lhs, rhs } => Some((lhs.clone(), rhs.clone())),
            _ => None,
        })
        .collect()
}

fn equality_constraint_origins(
    flat: &rumoca_ir_flat::Model,
) -> Vec<(String, String, String, usize)> {
    flat.equations
        .iter()
        .filter_map(|equation| match equation.origin {
            EquationOrigin::EqualityConstraint {
                lhs_record,
                rhs_record,
                function,
            } => Some((
                record_name(flat, lhs_record),
                record_name(flat, rhs_record),
                function_name(flat, function),
                equation.scalar_count,
            )),
            _ => None,
        })
        .collect()
}

#[test]
fn loop_closing_connect_emits_reduced_equality_constraint_residual() {
    let flat = flatten_model(SOURCE, "OcLoop.Loop3");
    let residuals = equality_constraint_origins(&flat);

    assert_eq!(
        residuals.len(),
        1,
        "exactly one loop-closing connect must be replaced by equalityConstraint; got {residuals:?}"
    );
    let (_, _, function, scalar_count) = &residuals[0];
    assert_eq!(
        *scalar_count, 1,
        "the residual width must be the equalityConstraint output size, not the record size"
    );
    assert_eq!(
        function, "OcLoop.Ori.equalityConstraint",
        "residual must call the connector record's own equalityConstraint"
    );
}

#[test]
fn loop_closing_connect_drops_both_element_wise_equalities() {
    let flat = flatten_model(SOURCE, "OcLoop.Loop3");
    let origins = connection_origins(&flat);

    let equality_constraints = equality_constraint_origins(&flat);
    let (lhs_record, rhs_record, _, _) = equality_constraints
        .first()
        .expect("a loop-closing connect must produce a residual");

    for field in ["a", "b"] {
        let lhs = format!("{lhs_record}.{field}");
        let rhs = format!("{rhs_record}.{field}");
        assert!(
            !origins
                .iter()
                .any(|(eq_lhs, eq_rhs)| (eq_lhs == &lhs && eq_rhs == &rhs)
                    || (eq_lhs == &rhs && eq_rhs == &lhs)),
            "broken edge must not also emit the `{field}` equality"
        );
    }
}

#[test]
fn spanning_tree_connects_keep_full_element_wise_equality() {
    let flat = flatten_model(SOURCE, "OcLoop.Loop3");
    let origins = connection_origins(&flat);

    // Four connects join four overdetermined records into one component with a
    // single root, so three edges stay in the spanning tree. Each tree edge
    // keeps one equality per record element (`a` and `b`).
    let element_equalities = origins
        .iter()
        .filter(|(lhs, rhs)| {
            (lhs.ends_with(".R.a") && rhs.ends_with(".R.a"))
                || (lhs.ends_with(".R.b") && rhs.ends_with(".R.b"))
        })
        .count();
    assert_eq!(
        element_equalities, 6,
        "three spanning-tree connects x two record elements; got {origins:?}"
    );
}
