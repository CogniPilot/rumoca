//! MLS §10.4.1, §10.5 and §10.3.4 permit a function to reduce the slice
//! selected by an Integer input. The result is scalar even when the selected
//! extent changes at run time; translation-time input shape does not settle
//! the value of that input (MLS §12.2).

use rumoca::Compiler;

#[test]
fn runtime_maximum_slice_preserves_the_selected_extent() {
    let source = r#"
model RuntimeSliceReduction
  function activeMaximum
    input Real values[:];
    input Integer count;
    output Real result;
  algorithm
    result := max(values[1:count]);
  end activeMaximum;
  Real result;
equation
  result = activeMaximum({-5, -2, -9}, if time < 0.5 then 1 else 2);
end RuntimeSliceReduction;
"#;
    let compiled = Compiler::new()
        .model("RuntimeSliceReduction")
        .compile_str(source, "runtime_slice_reduction.mo")
        .unwrap_or_else(|error| {
            panic!("a scalar reduction of an input-sized slice compiles: {error:?}")
        });
    let result = rumoca_sim::simulate_dae_with_diagnostics(
        &compiled.dae,
        &rumoca_sim::SimOptions {
            t_end: 1.0,
            dt: Some(0.25),
            ..Default::default()
        },
    )
    .expect("the runtime-sized scalar reduction simulates");
    let index = result
        .names
        .iter()
        .position(|name| name == "result")
        .unwrap();
    assert_eq!(result.data[index].first(), Some(&-5.0));
    assert_eq!(result.data[index].last(), Some(&-2.0));
}

fn simulate(source: &str) -> Result<rumoca_sim::SimResult, String> {
    let compiled = Compiler::new()
        .model("SliceProbe")
        .compile_str(source, "slice_probe.mo")
        .unwrap_or_else(|error| panic!("runtime slice reduction compiles: {error:?}"));
    let wire =
        serde_json::to_vec(&compiled.dae).expect("existing DAE wire encodes the normalized slice");
    let decoded: rumoca_compile::compile::Dae =
        serde_json::from_slice(&wire).expect("existing DAE wire reconstructs the normalized slice");
    rumoca_sim::simulate_dae_with_diagnostics(
        &decoded,
        &rumoca_sim::SimOptions {
            t_end: 1.0,
            dt: Some(0.25),
            ..Default::default()
        },
    )
    .map_err(|error| format!("{error:?}"))
}

fn endpoints(result: &rumoca_sim::SimResult, name: &str) -> (f64, f64) {
    let index = result.names.iter().position(|entry| entry == name).unwrap();
    (
        *result.data[index].first().unwrap(),
        *result.data[index].last().unwrap(),
    )
}

#[test]
fn runtime_slice_checks_remain_in_their_active_branch() {
    let source = r#"
model SliceProbe
  function active
    input Real values[:];
    input Integer count;
    input Boolean enabled;
    output Real result;
  algorithm
    if enabled then
      result := max(values[1:count]);
    else
      result := 7;
    end if;
  end active;
  Real result = active({-5,-2,-9},if time < 0.5 then 1 else 4,time < 0.5);
end SliceProbe;
"#;
    let result = simulate(source).expect("inactive invalid slice must not execute or assert");
    assert_eq!(endpoints(&result, "result"), (-5.0, 7.0));
}

#[test]
fn runtime_slice_rejects_invalid_nonempty_bounds() {
    let source = r#"
model SliceProbe
  function active
    input Real values[:];
    input Integer count;
    output Real result;
  algorithm
    result := max(values[1:count]);
  end active;
  Real result = active({-5,-2,-9},if time < 0.5 then 1 else 4);
end SliceProbe;
"#;
    assert!(
        simulate(source).is_err(),
        "a nonempty out-of-bounds slice must fail"
    );
}

#[test]
fn runtime_slice_minimum_and_empty_extrema_preserve_real_identities() {
    let source = r#"
model SliceProbe
  function inspectSlice
    input Real values[:];
    input Integer first;
    input Integer last;
    output Real result;
  protected
    Real smallest;
    Real greatest;
  algorithm
    smallest := min(values[first:last]);
    greatest := max(values[first:last]);
    if first > last then
      result := if smallest > 1e300 and greatest < -1e300 then 1 else -1;
    else
      result := smallest + greatest;
    end if;
  end inspectSlice;
  Real result = inspectSlice({1,4,99},if time < 0.5 then 2 else 4,2);
end SliceProbe;
"#;
    let result = simulate(source).expect("empty slice must use finite Real extrema");
    assert_eq!(endpoints(&result, "result"), (8.0, 1.0));
}

#[test]
fn runtime_slice_reads_changing_integer_locals_inside_a_loop() {
    let source = r#"
model SliceProbe
  function active
    input Real values[:];
    input Integer count;
    output Real result;
  protected
    Integer last;
    Real largest;
  algorithm
    last := count;
    result := 0;
    for j in 1:2 loop
      last := last - 1;
      largest := max(values[1:last]);
      result := result + largest;
    end for;
  end active;
  Real result = active({-5,-2,-9,-1},if time < 0.5 then 3 else 4);
end SliceProbe;
"#;
    let result = simulate(source).expect("loop-local slice bounds remain live");
    assert_eq!(endpoints(&result, "result"), (-7.0, -4.0));
}

#[test]
fn runtime_slice_does_not_erase_real_subscript_type_errors() {
    let source = r#"
model BadSliceProbe
  function active
    input Real values[:];
    input Real count;
    output Real result;
  algorithm
    result := max(values[1:count]);
  end active;
  Real result = active({1,2,3},time);
end BadSliceProbe;
"#;
    assert!(
        Compiler::new()
            .model("BadSliceProbe")
            .compile_str(source, "bad_slice.mo")
            .is_err()
    );
}

#[test]
fn runtime_slice_does_not_borrow_the_type_of_a_shadowed_outer_integer() {
    let source = r#"
model BadSliceProbe
  function active
    input Real values[:];
    input Integer count;
    output Real result;
  algorithm
    result := 0;
    for count in {1.5,2.5} loop
      result := max(values[1:count]);
    end for;
  end active;
  Real result = active({1,2,3},integer(time));
end BadSliceProbe;
"#;
    assert!(
        Compiler::new()
            .model("BadSliceProbe")
            .compile_str(source, "shadowed_slice.mo")
            .is_err()
    );
}
