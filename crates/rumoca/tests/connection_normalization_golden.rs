use rumoca_ir_ast as ast;
use rumoca_ir_flat::EquationOrigin;
use rumoca_phase_flatten::flatten_ref;
use rumoca_phase_instantiate::instantiate_model;
use rumoca_phase_resolve::resolve;
use rumoca_phase_typecheck::typecheck_instanced;

fn flatten_model(source: &str, model_name: &str) -> rumoca_ir_flat::Model {
    let def = rumoca_phase_parse::parse_to_ast(source, "connection_golden.mo")
        .expect("parse should succeed");
    let tree = ast::ClassTree::from_parsed(def);
    let parsed = rumoca_ir_ast::ParsedTree::new(tree);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let tree = &resolved.0;
    let mut overlay = instantiate_model(tree, model_name).expect("instantiate should succeed");
    typecheck_instanced(tree, &mut overlay, model_name).expect("typecheck should succeed");
    flatten_ref(tree, &overlay, model_name).expect("flatten should succeed")
}

#[test]
fn connector_normalization_origin_snapshot() {
    let flat = flatten_model(
        r#"
package ConnectionGolden
  connector Pin
    Real v;
    flow Real i;
  end Pin;

  model Source
    Pin p;
  equation
    p.v = 1;
  end Source;

  model Sink
    Pin p;
  equation
    p.v = 0;
  end Sink;

  model Harness
    Source source;
    Sink sink;
  equation
    connect(source.p, sink.p);
  end Harness;
end ConnectionGolden;
"#,
        "ConnectionGolden.Harness",
    );

    let mut origins = flat
        .equations
        .iter()
        .filter_map(|eq| match &eq.origin {
            EquationOrigin::Connection { lhs, rhs } => Some(serde_json::json!({
                "kind": "connection",
                "lhs": lhs,
                "rhs": rhs,
                "scalar_count": eq.scalar_count,
            })),
            EquationOrigin::FlowSum { description } => Some(serde_json::json!({
                "kind": "flow_sum",
                "description": description,
                "scalar_count": eq.scalar_count,
            })),
            _ => None,
        })
        .collect::<Vec<_>>();
    origins.sort_by_key(|entry| entry.to_string());

    assert_eq!(
        origins,
        serde_json::json!([
            {
                "kind": "connection",
                "lhs": "source.p.v",
                "rhs": "sink.p.v",
                "scalar_count": 1
            },
            {
                "kind": "flow_sum",
                "description": "source.p.i + sink.p.i = 0",
                "scalar_count": 1
            }
        ])
        .as_array()
        .expect("expected array")
        .clone()
    );
}
