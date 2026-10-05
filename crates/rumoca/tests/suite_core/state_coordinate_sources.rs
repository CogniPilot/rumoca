//! A reduced state selection integrates a generated aggregate state; the Solve
//! model names each of its scalars by the source scalar and formal derivative
//! order its value projection equation equates it to (SPEC_0040 STRUCT-T07),
//! and model wire replay refuses a map that names no distinct source.

use std::collections::{BTreeMap, HashMap};

use rumoca::Compiler;
use rumoca_ir_solve::{SolveModel, SolveStateCoordinate};
use rumoca_sim::{SimOptions, simulate_dae_with_diagnostics};

fn dae(source: &str, model: &str) -> std::sync::Arc<rumoca_ir_dae::Dae> {
    Compiler::new()
        .model(model)
        .compile_str(source, &format!("{model}.mo"))
        .unwrap()
        .dae
}

fn lowered(source: &str, model: &str) -> SolveModel {
    rumoca_phase_solve::lower_solve_model(&dae(source, model), &HashMap::new(), |_| {})
        .unwrap()
        .model()
        .clone()
}

/// Every Solve state scalar that carries a source, keyed by its own name.
fn sources(model: &SolveModel) -> BTreeMap<String, SolveStateCoordinate> {
    model
        .variable_meta
        .iter()
        .filter_map(|meta| {
            let source = meta.state_coordinate.clone()?;
            assert!(meta.is_state, "{} carries a source but no state", meta.name);
            Some((meta.name.clone(), source))
        })
        .collect()
}

fn source_names(model: &SolveModel) -> Vec<String> {
    let mut names = sources(model)
        .values()
        .map(SolveStateCoordinate::source_name)
        .collect::<Vec<_>>();
    names.sort();
    names
}

/// Mass and internal energy are differentiated; temperature and level are
/// preferred, so the selection integrates a generated state equal to them.
const TANK: &str = r#"
model PreferredTank
  Real U, m, u;
  Real T(stateSelect = StateSelect.prefer, start = 290);
  Real level(stateSelect = StateSelect.prefer, start = 0.5);
equation
  m = 2*level;
  U = m*u;
  u = 4184*(T - 298.15);
  der(U) = -1;
  der(m) = -0.001;
initial equation
  T = 300;
  level = 1;
end PreferredTank;
"#;

#[test]
fn each_generated_state_scalar_names_the_source_scalar_it_equals() {
    let model = lowered(TANK, "PreferredTank");
    assert_eq!(source_names(&model), ["T", "level"]);
    assert!(
        sources(&model)
            .values()
            .all(|source| source.derivative_order == 0)
    );
    assert_eq!(
        source_names(&model),
        rumoca_phase_solve::integrated_state_names(&dae(TANK, "PreferredTank"))
            .map(|mut names| {
                names.sort();
                names
            })
            .unwrap(),
        "the integrated basis and the Solve metadata read one map"
    );
    let result = simulate_dae_with_diagnostics(
        &dae(TANK, "PreferredTank"),
        &SimOptions {
            t_end: 1.0,
            dt: Some(0.1),
            ..Default::default()
        },
    )
    .expect("the preferred basis simulates");
    let column = |name: &str| {
        result
            .names
            .iter()
            .position(|candidate| candidate == name)
            .unwrap_or_else(|| panic!("trace exposes {name}"))
    };
    let mapped = result
        .variable_meta
        .iter()
        .filter_map(|meta| Some((meta.name.as_str(), meta.state_coordinate.as_ref()?)))
        .collect::<Vec<_>>();
    assert_eq!(mapped.len(), 2, "the simulation result carries the map");
    for (state, source) in mapped {
        let (state, source) = (column(state), column(&source.variable));
        for row in 0..result.times.len() {
            let (a, b) = (result.data[state][row], result.data[source][row]);
            assert!(
                (a - b).abs() <= 1e-9 * (1.0 + b.abs()),
                "state {a} and source {b} differ at row {row}"
            );
        }
    }
}

/// The source scalar name and formal order of each scalar of the generated
/// state, in state scalar order, of the candidate `select` proposes.
fn candidate_map(
    source: &str,
    model: &str,
    select: impl for<'s, 'f> FnOnce(
        rumoca_phase_structural::FormalDerivativeView<'_, 's, 'f>,
    ) -> Result<
        Vec<rumoca_phase_structural::FormalStateCoordinate<'f>>,
        rumoca_phase_structural::StructuralError,
    >,
) -> Vec<(String, u32)> {
    let source = dae(source, model);
    let quotient = rumoca_phase_structural::quotient_aliases(&source).unwrap();
    let formal =
        rumoca_phase_structural::construct_formal_derivatives(quotient.as_ref().unwrap_or(&source))
            .unwrap();
    let prepared = formal
        .construct_state_candidate(select)
        .unwrap()
        .into_prepared()
        .unwrap();
    prepared.inspect(|system| {
        let map = system
            .state_coordinates
            .expect("a candidate issues its map");
        let state = system
            .view
            .variable(map.state(system.view).unwrap())
            .unwrap();
        assert_eq!(state.role(), rumoca_ir_dae::VariableRole::State);
        assert_eq!(state.scalar_count(), map.coordinates().len());
        map.coordinates()
            .iter()
            .map(|coordinate| {
                (
                    coordinate.source_scalar_name(system.view).unwrap(),
                    coordinate.order,
                )
            })
            .collect()
    })
}

fn named<'d>(view: rumoca_ir_dae::DaeView<'d>, name: &str) -> rumoca_ir_dae::VariableId<'d> {
    view.variables()
        .find(|(_, variable)| variable.name().as_str() == name)
        .unwrap()
        .0
}

#[test]
fn the_candidate_map_follows_the_selected_scalars_and_orders() {
    // Tensor scalars keep the proposal's order, not the declaration's.
    let matrix = "model Matrix Real x[2,2](each start = 1); equation der(x) = -x; end Matrix;";
    let map = candidate_map(matrix, "Matrix", |view| {
        let x = named(view.source, "x");
        [3, 0, 2, 1]
            .into_iter()
            .map(|scalar| view.state_coordinate(x, 0, scalar))
            .collect()
    });
    assert_eq!(
        map,
        ["x[2,2]", "x[1,1]", "x[2,1]", "x[1,2]"].map(|name| (name.to_string(), 0))
    );
}

#[test]
fn a_system_without_a_reduced_selection_carries_no_state_coordinates() {
    let source = "model Plain
  Real x(start = 1, fixed = true);
equation
  der(x) = -x;
end Plain;";
    assert!(sources(&lowered(source, "Plain")).is_empty());
}

fn replay(wire: &serde_json::Value) -> Result<SolveModel, String> {
    let bytes = serde_json::to_vec(wire).unwrap();
    rumoca_phase_solve::deserialize_solve_model(&mut serde_json::Deserializer::from_slice(&bytes))
        .map_err(|error| error.to_string())
}

#[test]
fn model_wire_replay_refuses_a_state_coordinate_without_a_distinct_visible_source() {
    let model = lowered(TANK, "PreferredTank");
    let wire = serde_json::to_value(rumoca_phase_solve::solve_model_wire(&model).unwrap()).unwrap();
    let replayed = replay(&wire).expect("the constructed map replays");
    assert_eq!(source_names(&replayed), ["T", "level"]);

    let state_index = |wire: &serde_json::Value| {
        wire["variable_meta"]
            .as_array()
            .unwrap()
            .iter()
            .position(|meta| meta.get("state_coordinate").is_some())
            .unwrap()
    };
    let index = state_index(&wire);

    let mut absent = wire.clone();
    absent["variable_meta"][index]["state_coordinate"]["variable"] = "absent".into();
    let error = replay(&absent).expect_err("an absent source is refused");
    assert!(error.contains("state coordinate"), "{error}");

    let mut repeated = wire.clone();
    let other = wire["variable_meta"]
        .as_array()
        .unwrap()
        .iter()
        .enumerate()
        .rposition(|(at, meta)| at != index && meta.get("state_coordinate").is_some())
        .unwrap();
    repeated["variable_meta"][other]["state_coordinate"] =
        wire["variable_meta"][index]["state_coordinate"].clone();
    let error = replay(&repeated).expect_err("a repeated source is refused");
    assert!(error.contains("state coordinate"), "{error}");

    let mut not_state = wire.clone();
    let algebraic = wire["variable_meta"]
        .as_array()
        .unwrap()
        .iter()
        .position(|meta| meta["is_state"] == false)
        .unwrap();
    not_state["variable_meta"][algebraic]["state_coordinate"] =
        serde_json::json!({ "variable": "U", "derivative_order": 0 });
    let error = replay(&not_state).expect_err("a non-state scalar carries no source");
    assert!(error.contains("state coordinate"), "{error}");
}
