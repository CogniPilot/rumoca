//! Fail-closed coverage for `connect(..., c.port[i])` where `port` is a
//! compact array of a *simple* connector.
//!
//! A connector whose type is a predefined type (`connector RealInput = input
//! Real;`) has no members to expand, so an array of it stays one flat variable
//! with its dimensions intact — `gate.x`, dims `[2]`. The model owns no
//! declaration named `gate.x[1]`.
//!
//! Flat's connected-state owner is the per-element `ConnectedDomain`, so the
//! connection marks exactly `gate.x[1]`: the equality equation names that
//! element, the declaration stays compact, and `gate.x[2]` remains
//! unconnected.

use rumoca_ir_ast as ast;

const SOURCE_NAME: &str = "<connection_array_element_identity>";
const SOURCE: &str = r#"
connector RealInput = input Real;
connector RealOutput = output Real;

model Gate
    RealInput x[2];
    RealOutput y;
equation
    y = x[1] + x[2];
end Gate;

model Wired
    Gate gate;
    RealOutput a;
    Real probe;
equation
    connect(a, gate.x[1]);
    probe = gate.y;
    a = 1.0;
end Wired;
"#;

fn flatten_model(
    model_name: &str,
) -> Result<rumoca_ir_flat::Model, Box<rumoca_phase_flatten::FlattenError>> {
    let stored = rumoca_phase_parse::parse_to_ast(SOURCE, SOURCE_NAME).expect("source parses");
    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map.add(SOURCE_NAME, SOURCE);
    let resolved =
        rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree)).expect("source resolves");
    let overlay = match rumoca_phase_instantiate::instantiate_model_with_outcome(
        resolved.inner(),
        model_name,
    ) {
        rumoca_phase_instantiate::InstantiationOutcome::Success(overlay) => overlay,
        rumoca_phase_instantiate::InstantiationOutcome::NeedsInner { missing_inners, .. } => {
            panic!("fixture unexpectedly needs inner declarations: {missing_inners:?}")
        }
        rumoca_phase_instantiate::InstantiationOutcome::Error(error) => {
            panic!("fixture instantiation failed: {error}")
        }
    };
    let typed = rumoca_phase_typecheck::typecheck_instanced_tree(&resolved, overlay, model_name)
        .expect("instanced model typechecks");
    rumoca_phase_flatten::flatten_typed(typed, rumoca_phase_flatten::FlattenOptions::default())
        .map_err(Box::new)
}

fn declared_names(model: &rumoca_ir_flat::Model) -> Vec<String> {
    model
        .variables
        .keys()
        .map(|name| name.as_str().to_string())
        .collect()
}

#[test]
fn simple_connector_array_stays_one_declaration() {
    let model = flatten_model("Gate").expect("an unconnected compact connector array flattens");
    let names = declared_names(&model);

    assert!(
        names.iter().any(|name| name == "x"),
        "flat model must declare the connector array itself, got {names:?}"
    );
    assert_eq!(
        model
            .variables
            .get(&rumoca_core::VarName::new("x"))
            .expect("connector array is declared")
            .dims,
        vec![2],
        "the connector array keeps its declared dimension"
    );
    assert!(
        !names.iter().any(|name| name.starts_with("x[")),
        "no element of the connector array is separately declared, got {names:?}"
    );
}

#[test]
fn partial_compact_connector_connection_marks_exactly_the_selected_element() {
    let model = flatten_model("Wired").expect("one element of a compact connector array lowers");
    let names = declared_names(&model);
    assert!(
        !names.iter().any(|name| name.starts_with("gate.x[")),
        "the connection must not split the compact declaration, got {names:?}"
    );

    let connection_rows: Vec<(String, String, usize)> = model
        .equations
        .iter()
        .filter_map(|equation| match &equation.origin {
            rumoca_ir_flat::EquationOrigin::Connection { lhs, rhs } => {
                Some((lhs.clone(), rhs.clone(), equation.scalar_count))
            }
            _ => None,
        })
        .collect();
    assert_eq!(
        connection_rows,
        vec![("a".to_string(), "gate.x[1]".to_string(), 1)],
        "exactly the selected element joins the connection set"
    );
    assert!(
        !model.equations.iter().any(|equation| matches!(
            equation.origin,
            rumoca_ir_flat::EquationOrigin::FlowSum { .. }
                | rumoca_ir_flat::EquationOrigin::UnconnectedFlow { .. }
        )),
        "a causal connector array owns no flow rows"
    );

    let gate_x = model
        .variables
        .get(&rumoca_core::VarName::new("gate.x"))
        .expect("the compact connector array is declared");
    assert_eq!(gate_x.dims, vec![2]);
    assert_eq!(
        gate_x.connected.selections().collect::<Vec<_>>(),
        vec![&[1][..]],
        "only the selected element is connected"
    );
    assert_eq!(
        gate_x.connected.coverage(&[2]),
        Ok(rumoca_ir_flat::ConnectedCoverage::Partial)
    );
    assert_eq!(
        gate_x.connected.unconnected_coordinates(&[2]),
        Ok(vec![vec![2]])
    );
}
