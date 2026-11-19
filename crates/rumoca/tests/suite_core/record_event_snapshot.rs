//! MLS 3.7 §3.7.5 permits pre(recordComponent). Record equations retain
//! their function result and field ownership (§8.3.1, §12.4.3).

use super::same_tick_function_chain::REFINE_FUNCTION;
use rumoca::Compiler;

const SNAPSHOT_DECLARATIONS: &str = r#"
  record Snapshot
    Real charge;
    Real wells[2];
    Integer direction;
  end Snapshot;
  function evolve
    input Snapshot previous;
    input Integer amount;
    output Snapshot next;
  algorithm
    next := previous;
    next.charge := previous.charge-amount;
    next.wells[1] := previous.wells[1]+amount;
    next.direction := 1;
  end evolve;
  function inspect
    input Snapshot previous;
    output Real value;
  algorithm
    value := previous.charge+2*previous.wells[1]+3*previous.wells[2]+10*previous.direction;
  end inspect;
"#;

const SNAPSHOT_VARIABLES: &str = r#"
  Snapshot state(charge(start=50,fixed=true),wells(each start=5,each fixed=true),
    direction(start=1,fixed=true));
  discrete Integer index(start=0,fixed=true);
"#;

fn simulate(source: &str) -> rumoca_sim::SimResult {
    let compiled = Compiler::new()
        .model("SnapshotProbe")
        .compile_str(source, "record_event_snapshot.mo")
        .expect("record snapshot event ownership compiles");
    let wire = serde_json::to_vec(&compiled.dae).expect("checked record snapshot DAE encodes");
    let dae: rumoca_compile::compile::Dae =
        serde_json::from_slice(&wire).expect("existing DAE wire reconstructs the snapshot");
    rumoca_sim::simulate_dae_with_diagnostics(
        &dae,
        &rumoca_sim::SimOptions {
            t_end: 2.0,
            dt: Some(1.0),
            ..Default::default()
        },
    )
    .expect("record snapshot history and updates simulate")
}

fn endpoints(result: &rumoca_sim::SimResult, name: &str) -> (f64, f64) {
    let index = result.names.iter().position(|entry| entry == name).unwrap();
    let values = &result.data[index];
    (*values.first().unwrap(), *values.last().unwrap())
}

#[test]
fn record_pre_argument_reads_the_prior_primitive_fields() {
    let source = format!(
        r#"model SnapshotProbe
{SNAPSHOT_DECLARATIONS}
{SNAPSHOT_VARIABLES}
  discrete Real observed;
equation
  when sample(0,1) then
    index = pre(index)+1;
    state.charge = pre(state.charge)-index;
    state.wells = pre(state.wells)+{{index,0}};
    state.direction = 1;
    observed = inspect(pre(state));
  end when;
end SnapshotProbe;"#
    );
    let result = simulate(&source);
    assert_eq!(endpoints(&result, "observed"), (85.0, 88.0));
    assert_eq!(endpoints(&result, "state.charge"), (49.0, 44.0));
}

#[test]
fn whole_record_when_result_preserves_snapshot_and_array_field() {
    let source = format!(
        r#"model SnapshotProbe
{SNAPSHOT_DECLARATIONS}
{SNAPSHOT_VARIABLES}
equation
  when sample(0,1) then
    index = pre(index)+1;
    state = evolve(pre(state),index);
  end when;
end SnapshotProbe;"#
    );
    let result = simulate(&source);
    assert_eq!(endpoints(&result, "state.charge"), (49.0, 44.0));
    assert_eq!(endpoints(&result, "state.wells[1]"), (6.0, 11.0));
    assert_eq!(endpoints(&result, "state.wells[2]"), (5.0, 5.0));
    assert_eq!(endpoints(&result, "state.direction"), (1.0, 1.0));
}

#[test]
fn tuple_record_receivers_keep_one_declared_owner_per_leaf() {
    let declarations = SNAPSHOT_DECLARATIONS
        .replace(
            "output Snapshot next;",
            "output Snapshot next; output Real marker;",
        )
        .replace("next := previous;", "marker := amount; next := previous;");
    let source = format!(
        r#"model SnapshotProbe
{declarations}
{SNAPSHOT_VARIABLES}
  discrete Real marker;
equation
  when sample(0,1) then
    index = pre(index)+1;
    (state,marker) = evolve(pre(state),index);
  end when;
end SnapshotProbe;"#
    );
    let result = simulate(&source);
    assert_eq!(endpoints(&result, "state.charge"), (49.0, 44.0));
    assert_eq!(endpoints(&result, "state.wells[1]"), (6.0, 11.0));
    assert_eq!(endpoints(&result, "marker"), (1.0, 3.0));
}

#[test]
fn record_history_remains_illegal_inside_functions() {
    let source = format!(
        r#"model SnapshotProbe
{SNAPSHOT_DECLARATIONS}
  function illegal
    input Snapshot previous;
    output Real observed;
  algorithm
    observed := inspect(pre(previous));
  end illegal;
  Real observed = illegal(Snapshot(50,{{5,5}},1));
end SnapshotProbe;"#
    );
    assert!(
        Compiler::new()
            .model("SnapshotProbe")
            .compile_str(&source, "illegal_record_history.mo")
            .is_err(),
        "record decomposition must not erase function-local pre()"
    );
}

#[test]
fn independent_producers_straddle_one_mixed_record_result() {
    let declarations = SNAPSHOT_DECLARATIONS
        .replace("input Integer amount;", "input Real amount;")
        .replace(
            "next.direction := 1;",
            "next.direction := if amount > 2 then 1 else -1;",
        );
    let source = format!(
        r#"model SnapshotProbe
{declarations}
{REFINE_FUNCTION}
{SNAPSHOT_VARIABLES}
  discrete Real requested;
  discrete Integer consumer(start=0,fixed=true);
equation
  when sample(0,1) then
    index = pre(index)+1;
    requested = refine(2*index);
    state = evolve(pre(state),requested);
    consumer = if state.charge < 46 then 1 else 0;
  end when;
end SnapshotProbe;"#
    );
    let result = simulate(&source);
    let (first, last) = endpoints(&result, "state.charge");
    assert!((first - (50.0 - 2_f64.sqrt())).abs() < 1e-10);
    assert!((last - (48.0 - 2_f64.sqrt() - 6_f64.sqrt())).abs() < 1e-10);
    assert_eq!(endpoints(&result, "state.direction"), (-1.0, 1.0));
    assert_eq!(endpoints(&result, "consumer"), (0.0, 1.0));
}

#[test]
fn record_history_requires_a_variable_not_a_constructor_expression() {
    let source = format!(
        r#"model SnapshotProbe
{SNAPSHOT_DECLARATIONS}
  Real observed = inspect(pre(Snapshot(50,{{5,5}},1)));
end SnapshotProbe;"#
    );
    assert!(
        Compiler::new()
            .model("SnapshotProbe")
            .compile_str(&source, "nonvariable_record_history.mo")
            .is_err(),
        "record argument decomposition must not accept pre(expression)"
    );
}

#[test]
fn impure_record_result_cannot_be_repeated_per_projection() {
    let declarations = SNAPSHOT_DECLARATIONS.replace("function evolve", "impure function evolve");
    let source = format!(
        r#"model SnapshotProbe
{declarations}
{SNAPSHOT_VARIABLES}
equation
  when sample(0,1) then
    index = pre(index)+1;
    state = evolve(pre(state),index);
  end when;
end SnapshotProbe;"#
    );
    let error = Compiler::new()
        .model("SnapshotProbe")
        .compile_str(&source, "impure_record_result.mo")
        .expect_err("independent projections must not duplicate an impure record call");
    assert!(
        error.to_string().contains("impure Modelica body"),
        "{error}"
    );
}

#[test]
fn whole_record_call_without_record_inputs_uses_the_same_receiver_plan() {
    let record = SNAPSHOT_DECLARATIONS
        .split("  function evolve")
        .next()
        .unwrap();
    let source = format!(
        r#"model SnapshotProbe
{record}
  function create
    input Integer amount;
    output Snapshot next;
  algorithm
    next := Snapshot(50-amount,{{5+amount,5}},1);
  end create;
{SNAPSHOT_VARIABLES}
equation
  when sample(0,1) then
    index = pre(index)+1;
    state = create(index);
  end when;
end SnapshotProbe;"#
    );
    let result = simulate(&source);
    assert_eq!(endpoints(&result, "state.charge"), (49.0, 47.0));
    assert_eq!(endpoints(&result, "state.wells[1]"), (6.0, 8.0));
}

#[test]
fn record_result_and_field_cannot_claim_the_same_when_coordinate() {
    let source = format!(
        r#"model SnapshotProbe
{SNAPSHOT_DECLARATIONS}
{SNAPSHOT_VARIABLES}
equation
  when sample(0,1) then
    index = pre(index)+1;
    state = evolve(pre(state),index);
    state.charge = 12;
  end when;
end SnapshotProbe;"#
    );
    let error = Compiler::new()
        .model("SnapshotProbe")
        .compile_str(&source, "overlapping_record_result.mo")
        .expect_err("record and primitive field must share the overlap checker");
    assert!(
        error.to_string().contains("assigned more than once"),
        "{error}"
    );
}

#[test]
fn sibling_record_calls_keep_distinct_instance_owners() {
    let source = format!(
        r#"model SnapshotProbe
{SNAPSHOT_DECLARATIONS}
  model Cell
    parameter Integer scale = 1;
{SNAPSHOT_VARIABLES}
  equation
    when sample(0,1) then
      index = pre(index)+1;
      state = evolve(pre(state),scale*index);
    end when;
  end Cell;
  Cell first(scale=1);
  Cell second(scale=2);
end SnapshotProbe;"#
    );
    let result = simulate(&source);
    assert_eq!(endpoints(&result, "first.state.charge"), (49.0, 44.0));
    assert_eq!(endpoints(&result, "second.state.charge"), (48.0, 38.0));
    assert_eq!(endpoints(&result, "first.state.wells[1]"), (6.0, 11.0));
    assert_eq!(endpoints(&result, "second.state.wells[1]"), (7.0, 17.0));
}

#[test]
fn inactive_record_call_does_not_execute_its_failing_assertion() {
    let declarations = SNAPSHOT_DECLARATIONS.replace(
        "next := previous;",
        "assert(amount > 0, \"inactive call executed\"); next := previous;",
    );
    let source = format!(
        r#"model SnapshotProbe
{declarations}
{SNAPSHOT_VARIABLES}
  Snapshot dormant(charge(start=50,fixed=true),wells(each start=5,each fixed=true),
    direction(start=1,fixed=true));
equation
  when sample(0,1) then
    index = pre(index)+1;
    state = evolve(pre(state),index);
  end when;
  when time < -1 then
    dormant = evolve(pre(dormant),-1);
  end when;
end SnapshotProbe;"#
    );
    let result = simulate(&source);
    assert_eq!(endpoints(&result, "state.charge"), (49.0, 44.0));
    assert_eq!(endpoints(&result, "dormant.charge"), (50.0, 50.0));
    assert_eq!(endpoints(&result, "dormant.wells[1]"), (5.0, 5.0));
}

fn effectful_record_call_source(indirect: bool) -> String {
    let declarations = if indirect {
        SNAPSHOT_DECLARATIONS.replace("next.direction := 1;", "next.direction := tick(amount);")
    } else {
        SNAPSHOT_DECLARATIONS.to_string()
    };
    let amount = if indirect { "index" } else { "tick(index)" };
    format!(
        r#"model SnapshotProbe
{declarations}
  impure function tick
    input Integer amount;
    output Integer value;
    external "C" value = snapshot_tick(amount) annotation(Library="snapshot_fixture");
  end tick;
{SNAPSHOT_VARIABLES}
equation
  when sample(0,1) then
    index = pre(index)+1;
    state = evolve(pre(state),{amount});
  end when;
end SnapshotProbe;"#
    )
}

#[test]
fn effectful_record_result_cannot_duplicate_an_external_call() {
    let source = effectful_record_call_source(true);
    let Err(error) = Compiler::new()
        .model("SnapshotProbe")
        .compile_str(&source, "effectful_record_result.mo")
    else {
        panic!("record projections cannot duplicate a reachable external effect");
    };
    assert!(error.to_string().contains("impure function"), "{error}");
}

#[test]
fn record_call_projection_cannot_repeat_an_impure_actual_argument() {
    let source = effectful_record_call_source(false);
    let Err(error) = Compiler::new()
        .model("SnapshotProbe")
        .compile_str(&source, "effectful_record_argument.mo")
    else {
        panic!("record projections cannot duplicate an impure argument");
    };
    assert!(error.to_string().contains("impure"), "{error}");
}
