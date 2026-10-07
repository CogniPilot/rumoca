//! Source compilation plus actual Wasmi execution: one RHS call snapshot,
//! ordered receivers, exact scalar types, and named malformed-shape refusals.
use super::*;
use wasmi::{Engine, Linker, Memory, MemoryType, Module, Store};

fn execute(source: &str, model: &str, inputs: &[(&str, f64)], outputs: &[&str]) -> Vec<f64> {
    let _guard = session_test_guard();
    let raw = crate::native_program_api::prepare_native_program(source, model).unwrap();
    let artifact: serde_json::Value = serde_json::from_str(&raw).unwrap();
    assert_eq!(artifact["profile"], "native-direct-program-f64-v3");
    assert_eq!(artifact["abi"]["transactional_y"], true);
    let bytes = artifact["module_bytes"]
        .as_array()
        .unwrap()
        .iter()
        .map(|value| value.as_u64().unwrap() as u8)
        .collect::<Vec<_>>();
    let engine = Engine::default();
    let module = Module::new(&engine, bytes).unwrap();
    let mut store = Store::new(&engine, ());
    let abi = &artifact["abi"];
    let memory = Memory::new(
        &mut store,
        MemoryType::new(abi["memory_pages"].as_u64().unwrap() as u32, None),
    )
    .unwrap();
    let mut parameters = artifact["parameters"]
        .as_array()
        .unwrap()
        .iter()
        .map(|value| value.as_f64().unwrap())
        .collect::<Vec<_>>();
    // Integer and Boolean inputs are written to their typed input lanes
    // (SOLVE-C69), starting from their declared start values.
    let typed = abi["typed_lanes_offset"].as_u64().unwrap_or(0) as usize;
    let mut lanes = vec![0u8; abi["input_lanes_bytes"].as_u64().unwrap_or(0) as usize];
    let lane_inputs = artifact["input_lanes"]
        .as_array()
        .cloned()
        .unwrap_or_default();
    for lane in &lane_inputs {
        let p_index = lane["p_index"].as_u64().unwrap() as usize;
        write_lane(&mut lanes, lane, parameters[p_index]);
    }
    for &(name, value) in inputs {
        if let Some(lane) = lane_inputs.iter().find(|lane| lane["name"] == name) {
            write_lane(&mut lanes, lane, value);
            continue;
        }
        let index = artifact["var_layout"]["bindings"][name]["P"]["index"]
            .as_u64()
            .unwrap() as usize;
        parameters[index] = value;
    }
    let parameter_bytes = parameters
        .into_iter()
        .flat_map(f64::to_le_bytes)
        .collect::<Vec<_>>();
    memory
        .write(
            &mut store,
            abi["p_offset"].as_u64().unwrap() as usize,
            &parameter_bytes,
        )
        .unwrap();
    memory.write(&mut store, typed, &lanes).unwrap();
    let mut linker = Linker::new(&engine);
    linker.define("env", "memory", memory).unwrap();
    let instance = linker
        .instantiate(&mut store, &module)
        .unwrap()
        .start(&mut store)
        .unwrap();
    let call = instance
        .get_typed_func::<(i32, i32, f64, i32, i32), i32>(&store, "eval_assignments")
        .unwrap();
    assert_eq!(
        call.call(
            &mut store,
            (
                0,
                abi["p_offset"].as_u64().unwrap() as i32,
                0.0,
                abi["scratch_offset"].as_u64().unwrap() as i32,
                typed as i32
            )
        )
        .unwrap(),
        0
    );
    outputs
        .iter()
        .map(|name| {
            let index = artifact["var_layout"]["bindings"][*name]["Y"]["index"]
                .as_u64()
                .unwrap() as usize;
            let mut cell = [0; 8];
            memory.read(&store, index * 8, &mut cell).unwrap();
            f64::from_le_bytes(cell)
        })
        .collect()
}

const STATE_SOURCE: &str = r#"
function StateStep
  input Real previous;
  output Real next;
  output Real draw;
algorithm
  next := previous+1.0;
  draw := next/100.0;
end StateStep;
function StateLoop
  input Real seed;
  output Real total;
protected
  Real state;
  Real u;
algorithm
  total := 0.0;
  state := seed;
  u := 0.0;
  for i in 1:3 loop
    (state,u) := StateStep(state);
    total := total+state+u;
  end for;
end StateLoop;
model TupleStateLoop
  input Real seed = 7.0;
  output Real total;
equation
  total = StateLoop(seed);
end TupleStateLoop;
"#;

#[test]
fn integer_quotient_loop_executes_original_source_and_edited_index_divisor() {
    let source = include_str!("fixtures/quotient_loop.mo");
    let names = (1..=49)
        .map(|index| format!("values[{index}]"))
        .collect::<Vec<_>>();
    let inputs = names
        .iter()
        .enumerate()
        .map(|(index, name)| (name.as_str(), (index + 1) as f64))
        .collect::<Vec<_>>();
    assert_eq!(
        execute(source, "QuotientLoopControl", &inputs, &["total"]),
        vec![196.0]
    );
    let edited = source.replace("div(offset,7)", "div(offset,5)");
    assert_ne!(edited, source);
    assert_eq!(
        execute(&edited, "QuotientLoopControl", &inputs, &["total"]),
        vec![265.0]
    );
}

#[test]
fn tuple_loop_reads_one_prior_state_and_publishes_all_scalar_receivers() {
    for (seed, expected) in [(7.0, 27.27), (-4.0, -6.06), (0.0, 6.06)] {
        let actual = execute(
            STATE_SOURCE,
            "TupleStateLoop",
            &[("seed", seed)],
            &["total"],
        );
        assert!((actual[0] - expected).abs() < 1e-12);
    }
}

const REPEATED_SOURCE: &str = r#"
function ThreeResults
  input Real previous;
  output Real a;
  output Real b;
  output Real c;
algorithm
  a := previous+1.0;
  b := previous+2.0;
  c := previous+3.0;
end ThreeResults;
function RepeatedLoop
  input Real seed;
  output Real x[2];
  output Real state;
algorithm
  x := zeros(2);
  state := seed;
  for i in 1:3 loop
    (x[1],x[2],x[1]) := ThreeResults(state);
    state := x[1];
  end for;
end RepeatedLoop;
model TupleRepeatedLoop
  input Real seed = 7.0;
  output Real x[2];
  output Real state;
equation
  (x,state) = RepeatedLoop(seed);
end TupleRepeatedLoop;
"#;

/// MLS §11.2.1.1 explicitly permits (x[1],x[2],x[1]): the third wins,
/// while the distinct second receiver survives the private aggregate updates.
#[test]
fn tuple_loop_mls_repeated_and_disjoint_array_receivers_keep_source_order() {
    let actual = execute(
        REPEATED_SOURCE,
        "TupleRepeatedLoop",
        &[],
        &["x[1]", "x[2]", "state"],
    );
    assert_eq!(actual, vec![16.0, 15.0, 16.0]);
}

const INDEX_SOURCE: &str = r#"
function IndexStep
  input Integer previous;
  output Integer next;
  output Real value;
algorithm
  next := previous+1;
  value := 10.0+next;
end IndexStep;
function IndexLoop
  input Integer seed;
  output Real x[3];
  output Integer index;
algorithm
  x := zeros(3);
  index := seed;
  for i in 1:3 loop
    (index,x[index]) := IndexStep(index);
  end for;
end IndexLoop;
function IndexObservation
  input Integer seed;
  output Real observed[4];
protected
  Real x[3];
  Integer index;
algorithm
  (x,index) := IndexLoop(seed);
  observed := {x[1],x[2],x[3],index};
end IndexObservation;
model TupleIndexLoop
  input Integer seed = 0;
  output Real observed[4];
equation
  observed = IndexObservation(seed);
end TupleIndexLoop;
"#;

#[test]
fn later_receiver_index_observes_earlier_receiver_but_rhs_uses_old_index() {
    let actual = execute(
        INDEX_SOURCE,
        "TupleIndexLoop",
        &[],
        &["observed[1]", "observed[2]", "observed[3]", "observed[4]"],
    );
    assert_eq!(actual, vec![11.0, 12.0, 13.0, 3.0]);
}

const MIXED_SOURCE: &str = r#"
function MixedStep
  input Integer previous;
  input Boolean enabled;
  output Integer next;
  output Boolean toggled;
  output Real value;
algorithm
  next := previous+1;
  toggled := not enabled;
  value := if enabled then 1.25 else 2.5;
end MixedStep;
function MixedLoop
  input Integer seed;
  output Integer index;
  output Boolean enabled;
  output Real value;
algorithm
  index := seed;
  enabled := false;
  value := 0.0;
  for i in 1:3 loop
    (index,enabled,value) := MixedStep(index,enabled);
  end for;
end MixedLoop;
function MixedObservation
  input Integer seed;
  output Real observed[3];
protected
  Integer index;
  Boolean enabled;
  Real value;
algorithm
  (index,enabled,value) := MixedLoop(seed);
  observed := {index,if enabled then 1.0 else 0.0,value};
end MixedObservation;
model TupleMixedLoop
  input Integer seed = 0;
  output Real observed[3];
equation
  observed = MixedObservation(seed);
end TupleMixedLoop;
"#;

#[test]
fn tuple_loop_retains_integer_boolean_and_real_result_types() {
    let actual = execute(
        MIXED_SOURCE,
        "TupleMixedLoop",
        &[("seed", 2048.0)],
        &["observed[1]", "observed[2]", "observed[3]"],
    );
    assert_eq!(actual, vec![2051.0, 1.0, 2.5]);
}

#[test]
fn tuple_loop_malformed_receiver_types_still_refuse() {
    let _guard = session_test_guard();
    let invalid = STATE_SOURCE
        .replace("Real u;", "Boolean u;")
        .replace("u := 0.0;", "u := false;");
    let failure = crate::native_program_api::prepare_native_program(&invalid, "TupleStateLoop")
        .unwrap_err()
        .to_string();
    assert!(failure.contains("expression shape mismatch"), "{failure}");
}

#[test]
fn tuple_loop_malformed_receiver_shapes_still_refuse() {
    let _guard = session_test_guard();
    let invalid = STATE_SOURCE
        .replace("Real u;", "Real u[2];")
        .replace("u := 0.0;", "u := zeros(2);");
    let failure = crate::native_program_api::prepare_native_program(&invalid, "TupleStateLoop")
        .unwrap_err()
        .to_string();
    assert!(
        failure.contains("shape") || failure.contains("dimension"),
        "{failure}"
    );
}

/// Write one Integer (`i64`) or Boolean (`u8`) typed input lane.
fn write_lane(lanes: &mut [u8], lane: &serde_json::Value, value: f64) {
    let at = lane["byte_offset"].as_u64().unwrap() as usize;
    match lane["representation"].as_str().unwrap() {
        "i64" => lanes[at..at + 8].copy_from_slice(&(value as i64).to_le_bytes()),
        _ => lanes[at] = u8::from(value != 0.0),
    }
}
