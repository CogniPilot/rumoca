//! A conditional that returns a large array it was given, in a function that
//! other functions call, names the array by address instead of copying it.
//! Every path still computes the value a copy would: the array a branch
//! passes through, builds, or returns from a nested conditional or a call.

use super::*;

const SOURCE: &str = r#"
function Pick
  input Real flag;
  input Real a[:];
  input Real b[:];
  output Real r[size(a,1)];
algorithm
  if flag > 0.5 then
    r := a;
  else
    r := 2.0 * b;
  end if;
end Pick;

function Nest
  input Real mode;
  input Real a[:];
  input Real b[:];
  output Real r[size(a,1)];
algorithm
  if mode > 0.5 then
    if mode > 1.5 then
      r := a;
    else
      r := Pick(0.0, a, b);
    end if;
  else
    r := Pick(1.0, b, a);
  end if;
end Nest;

function Chain
  input Real mode;
  input Real a[:];
  input Real b[:];
  output Real s;
protected
  Real r[size(a,1)];
algorithm
  r := Nest(mode, a, b);
  s := r[1] + 3.0 * r[size(a,1)];
end Chain;

model Select
  parameter Integer n = @N@;
  input Real mode = 0;
  input Real a[n] = zeros(n);
  input Real b[n] = zeros(n);
  output Real s;
equation
  s = Chain(mode, a, b);
end Select;
"#;

const CELLS: usize = 1024;

fn run(mode: f64) -> f64 {
    let source = SOURCE.replace("@N@", &CELLS.to_string());
    let artifact: serde_json::Value = serde_json::from_str(
        &crate::native_program_api::prepare_native_program(&source, "Select").unwrap(),
    )
    .unwrap();
    let layout: rumoca_ir_solve::VarLayout =
        serde_json::from_value(artifact["var_layout"].clone()).unwrap();
    let binding = |name: &str| match layout.binding(name).unwrap() {
        rumoca_ir_solve::ScalarSlot::Y { index, .. }
        | rumoca_ir_solve::ScalarSlot::P { index, .. } => index,
        _ => panic!("{name} is a scalar slot"),
    };
    let mut parameters = artifact["parameters"]
        .as_array()
        .unwrap()
        .iter()
        .map(|value| value.as_f64().unwrap())
        .collect::<Vec<_>>();
    parameters[binding("mode")] = mode;
    for cell in 0..CELLS {
        parameters[binding(&format!("a[{}]", cell + 1))] = cell as f64 + 1.0;
        parameters[binding(&format!("b[{}]", cell + 1))] = 10.0 * (cell as f64 + 1.0);
    }
    let mut execution = CallExecution::new(&artifact);
    let (status, y) = execution.run_typed(&parameters);
    assert_eq!(status, 0);
    y[binding("s")]
}

#[test]
fn a_large_array_returned_through_nested_conditionals_and_calls_has_the_value_of_the_taken_path() {
    let _lock = session_test_guard();
    let (first, last) = (1.0, CELLS as f64);
    let (b_first, b_last) = (10.0 * first, 10.0 * last);
    // mode 2: Nest returns a.
    assert_eq!(run(2.0), first + 3.0 * last);
    // mode 1: Nest builds 2 * b through Pick.
    assert_eq!(run(1.0), 2.0 * b_first + 3.0 * 2.0 * b_last);
    // mode 0: Nest returns Pick(1, b, a), which passes b through.
    assert_eq!(run(0.0), b_first + 3.0 * b_last);
}
