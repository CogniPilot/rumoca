//! A region body that reads an element directly and then calls a helper that
//! reads another element reports the first read's provenance (SOLVE-C73
//! issues demanded calls without reordering faults), and a call in an
//! unselected `elseif` condition or `else` arm is never issued.

use super::*;

#[test]
fn correlated_conditions_preserve_intervening_read_fault_order() {
    let _lock = session_test_guard();
    let source = r#"
function SeparatedReads
  input Real a[3];
  input Real b[3];
  input Real c[3];
  input Integer k;
  input Integer j;
  input Integer m;
  input Boolean flag;
  input Boolean enabled;
  output Real x;
  output Real z;
  output Real y;
algorithm
  x := 0.0; z := 0.0; y := 0.0;
  for iteration in 1:2 loop
    if enabled then
      x := if flag then a[k] else 0.0;
      z := b[j];
      y := if flag then c[m] else 0.0;
    end if;
  end for;
end SeparatedReads;
model SeparatedReadFaultOrder
  input Real a[3] = {1.0,2.0,3.0};
  input Real b[3] = {4.0,5.0,6.0};
  input Real c[3] = {7.0,8.0,9.0};
  input Integer k = 1;
  input Integer j = 1;
  input Integer m = 1;
  input Boolean flag = true;
  input Boolean enabled = true;
  output Real x;
  output Real z;
  output Real y;
equation
  (x,z,y) = SeparatedReads(a,b,c,k,j,m,flag,enabled);
end SeparatedReadFaultOrder;
"#;
    let artifact = artifact(source, "SeparatedReadFaultOrder");
    let parameters = parameters(&artifact);
    let mut execution = CallExecution::new(&artifact);
    execution.set_input("k", 1);
    execution.set_input("j", 0);
    execution.set_input("m", 0);
    let (status, fault) = run_checked(&artifact, &mut execution, &parameters);
    assert_ne!(status, 0);
    let (start, _) = span_of(fault.unwrap());
    assert_eq!(start, source.find("b[j]").unwrap());
}

const ORDER: &str = r#"
function CheckedSample
  input Real values[:];
  input Integer index;
  output Real value;
algorithm
  value := values[index];
end CheckedSample;

function ConditionalReads
  input Real values[3];
  input Integer firstIndex;
  input Integer secondIndex;
  input Boolean enabled;
  output Real result[2];
algorithm
  result := {-10.0,-20.0};
  for iteration in 1:2 loop
    if enabled then
      result[1] := values[firstIndex+iteration-1];
      result[2] := CheckedSample(values,secondIndex+iteration-1);
    end if;
  end for;
end ConditionalReads;

model ConditionalCallFaultOrder
  input Real values[3] = {11.0,22.0,33.0};
  input Integer firstIndex = 1;
  input Integer secondIndex = 1;
  input Boolean enabled = true;
  output Real result[2];
equation
  result = ConditionalReads(values,firstIndex,secondIndex,enabled);
end ConditionalCallFaultOrder;
"#;

const ARMS: &str = r#"
function Probe
  input Real a[3];
  input Integer j;
  output Real v;
algorithm
  v := a[j];
end Probe;

function Arms
  input Real a[3];
  input Integer sel;
  input Integer j;
  input Integer m;
  output Real r[2];
algorithm
  r := {-1.0,-1.0};
  for k in 1:2 loop
    if sel == 1 then
      r[k] := 100.0 + k;
    elseif sel == 2 then
      r[k] := Probe(a, m);
    elseif Probe(a, j) > 20.0 then
      r[k] := 200.0 + k;
    else
      r[k] := Probe(a, m) + 0.5;
    end if;
  end for;
end Arms;

model ArmLaziness
  input Real a[3] = {11.0,22.0,33.0};
  input Integer sel = 1;
  input Integer j = 1;
  input Integer m = 1;
  output Real r[2];
equation
  r = Arms(a, sel, j, m);
end ArmLaziness;
"#;

fn artifact(source: &str, model: &str) -> serde_json::Value {
    serde_json::from_str(&crate::native_program_api::prepare_native_program(source, model).unwrap())
        .unwrap()
}

fn parameters(artifact: &serde_json::Value) -> Vec<f64> {
    artifact["parameters"]
        .as_array()
        .unwrap()
        .iter()
        .map(|value| value.as_f64().unwrap())
        .collect()
}

/// `(status, fault)` of one checked call, with the published Y bytes and the
/// read-only P bytes checked around it: a refused call must leave the
/// poisoned outputs untouched and P byte-identical.
fn run_checked<'a>(
    artifact: &'a serde_json::Value,
    execution: &mut CallExecution,
    parameters: &[f64],
) -> (i32, Option<&'a serde_json::Value>) {
    let poison = vec![0xa5u8; execution.y * 8];
    execution
        .memory
        .write(&mut execution.store, 0, &poison)
        .unwrap();
    let input = parameters
        .iter()
        .flat_map(|v| v.to_le_bytes())
        .collect::<Vec<_>>();
    let (status, _) = execution.run_typed(parameters);
    let data = execution.memory.data(&execution.store);
    assert_eq!(&data[execution.p..execution.p + input.len()], input);
    let fault = artifact["faults"]
        .as_array()
        .unwrap()
        .iter()
        .find(|fault| fault["status"] == status);
    if status != 0 {
        assert_eq!(&data[..execution.y * 8], poison, "a fault published Y");
        assert!(fault.is_some(), "status {status} has a fault entry");
    }
    (status, fault)
}

fn span_of(fault: &serde_json::Value) -> (usize, usize) {
    (
        fault["provenance"]["start"].as_u64().unwrap() as usize,
        fault["provenance"]["end"].as_u64().unwrap() as usize,
    )
}

#[test]
fn direct_read_fault_precedes_the_helper_read_fault_on_every_iteration_and_rolls_back() {
    let _lock = session_test_guard();
    let artifact = artifact(ORDER, "ConditionalCallFaultOrder");
    assert_eq!(artifact["profile"], "native-direct-program-f64-v3");
    // The helper is one authored occurrence, hence one static site.
    let helper_sites = artifact["call_sites"]
        .as_array()
        .unwrap()
        .iter()
        .filter(|site| {
            let span = site["callee_span"].as_array().unwrap();
            let start = span[0].as_u64().unwrap() as usize;
            ORDER[start..].starts_with("function CheckedSample")
                || ORDER[start..].starts_with("CheckedSample")
        })
        .collect::<Vec<_>>();
    assert_eq!(helper_sites.len(), 1, "{helper_sites:?}");
    assert_eq!(helper_sites[0]["sites"], 1);

    let direct = ORDER.find("values[firstIndex").unwrap();
    let helper = ORDER.find("values[index]").unwrap();
    let mut execution = CallExecution::new(&artifact);
    let parameters = parameters(&artifact);
    let result = slot(&artifact, "result", "Y");
    let read = |execution: &mut CallExecution, first, second| {
        execution.set_input("firstIndex", first);
        execution.set_input("secondIndex", second);
        execution.run(&parameters)
    };
    execution.set_input("enabled", 1);
    // Valid values: iteration 2 overwrites iteration 1.
    let y = read(&mut execution, 1, 1);
    assert_eq!((y[result], y[result + 1]), (22.0, 22.0));
    let y = read(&mut execution, 2, 1);
    assert_eq!((y[result], y[result + 1]), (33.0, 22.0));

    // (first, second): both invalid at iteration 1, then both valid at
    // iteration 1 and invalid at iteration 2, then the helper alone.
    for (first, second) in [(0, 0), (3, 3), (4, 4), (-1, 9)] {
        execution.set_input("firstIndex", first);
        execution.set_input("secondIndex", second);
        let (status, fault) = run_checked(&artifact, &mut execution, &parameters);
        assert_ne!(status, 0, "first={first} second={second}");
        let fault = fault.unwrap();
        assert_eq!(fault["kind"], "IndexBounds");
        let (start, end) = span_of(fault);
        assert_eq!(
            &ORDER[start..end],
            "values",
            "first={first} second={second}"
        );
        assert_eq!(
            start, direct,
            "first={first} second={second}: the direct read, not the helper read at {helper}, is reported"
        );
        assert!(
            !fault["region_path"].as_array().unwrap().is_empty(),
            "the faulting read is inside the loop and conditional regions"
        );
        // The same instance recovers without a reset.
        let y = read(&mut execution, 1, 1);
        assert_eq!((y[result], y[result + 1]), (22.0, 22.0));
    }

    // Only the helper read is invalid: its own provenance is reported.
    execution.set_input("firstIndex", 1);
    execution.set_input("secondIndex", 3);
    let (status, fault) = run_checked(&artifact, &mut execution, &parameters);
    assert_ne!(status, 0);
    let (start, end) = span_of(fault.unwrap());
    assert_eq!(&ORDER[start..end], "values");
    assert_eq!(start, helper);

    // A disabled region reads nothing, so invalid indices are not faults.
    execution.set_input("enabled", 0);
    execution.set_input("firstIndex", 0);
    execution.set_input("secondIndex", 0);
    let (status, _) = run_checked(&artifact, &mut execution, &parameters);
    assert_eq!(status, 0);
    let y = execution.run(&parameters);
    assert_eq!((y[result], y[result + 1]), (-10.0, -20.0));
}

#[test]
fn unselected_call_arms_and_later_elseif_conditions_stay_lazy_and_fault_when_selected() {
    let _lock = session_test_guard();
    let artifact = artifact(ARMS, "ArmLaziness");
    assert_eq!(artifact["profile"], "native-direct-program-f64-v3");
    let mut execution = CallExecution::new(&artifact);
    let parameters = parameters(&artifact);
    let result = slot(&artifact, "r", "Y");
    let set = |execution: &mut CallExecution, sel, j, m| {
        execution.set_input("sel", sel);
        execution.set_input("j", j);
        execution.set_input("m", m);
    };
    // Hand-computed from a = {11, 22, 33}.
    for (sel, j, m, expected) in [
        (1, 0, 0, [101.0, 102.0]),
        (1, 9, -4, [101.0, 102.0]),
        // The earlier arm selects; the later condition's invalid `j` is lazy.
        (2, 0, 2, [22.0, 22.0]),
        (2, 7, 3, [33.0, 33.0]),
        // The condition selects its arm; the else call's invalid `m` is lazy.
        (3, 2, 0, [201.0, 202.0]),
        (3, 3, 9, [201.0, 202.0]),
        // The condition rejects its arm; the else call runs.
        (3, 1, 3, [33.5, 33.5]),
        (4, 1, 1, [11.5, 11.5]),
    ] {
        set(&mut execution, sel, j, m);
        let (status, _) = run_checked(&artifact, &mut execution, &parameters);
        assert_eq!(status, 0, "sel={sel} j={j} m={m}");
        let y = execution.run(&parameters);
        for (cell, value) in expected.iter().copied().map(f64::to_bits).enumerate() {
            assert_eq!(y[result + cell].to_bits(), value, "sel={sel} j={j} m={m}");
        }
    }
    // The same shapes fault when the invalid call is selected.
    for (sel, j, m) in [
        (2, 1, 0),
        (2, 1, 4),
        (3, 0, 1),
        (3, 4, 1),
        (3, 1, 0),
        (4, 1, 4),
    ] {
        set(&mut execution, sel, j, m);
        let (status, fault) = run_checked(&artifact, &mut execution, &parameters);
        assert_ne!(status, 0, "sel={sel} j={j} m={m}");
        let fault = fault.unwrap();
        assert_eq!(fault["kind"], "IndexBounds");
        let (start, end) = span_of(fault);
        assert_eq!(&ARMS[start..end], "a", "sel={sel} j={j} m={m}");
        set(&mut execution, 1, 0, 0);
        assert_eq!(execution.run(&parameters)[result], 101.0);
    }
}
