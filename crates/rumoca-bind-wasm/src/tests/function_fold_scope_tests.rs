//! A conditional region inside a conditional region of a loop update reads the
//! carried values of the enclosing loops through the outer region, so a loop
//! lowered in the inner region finds the tuple of the outermost loop.
use super::function_tuple_loop_tests::execute;

/// The callee loop `weigh` sits in a conditional of the loop `sweepAxes`, which
/// is called in a conditional of the guarded loop `advance`; the callee loop
/// reads the array `advance` carries through its argument.
const SOURCE: &str = r#"
function weigh
  input Real v[3];
  output Real s;
algorithm
  s := 0.0;
  for k in 1:3 loop
    s := s + v[k] * k;
  end for;
end weigh;

function sweepAxes
  input Real v[3];
  input Real gate;
  output Real r;
algorithm
  r := 0.0;
  for axis in 1:3 loop
    if gate > 0.5 then
      if axis > 0 then
        r := r + weigh(v);
      end if;
    end if;
  end for;
end sweepAxes;

function advance
  input Real u;
  input Integer steps;
  output Real carried[3];
  output Real total;
protected
  Boolean running;
algorithm
  carried := {u, 2 * u, 3 * u};
  total := 0.0;
  running := true;
  if true then
    for step in 1:min(steps, 8) loop
      if running then
        if step > 0 then
          total := total + sweepAxes(carried, 1.0);
        end if;
        carried := carried + {1.0, 1.0, 1.0};
        running := total < 1000.0;
      end if;
    end for;
  end if;
end advance;

model Region
  input Real x = 1.0;
  input Integer n = 3;
  Real carried[3];
  Real total;
equation
  (carried, total) = advance(x, n);
end Region;
"#;

#[test]
fn a_callee_loop_in_nested_regions_reads_the_carried_array_of_the_outer_loop() {
    // weigh({1,2,3}) = 14, weigh({2,3,4}) = 20, weigh({3,4,5}) = 26; each is
    // taken once per axis.
    let values = execute(SOURCE, "Region", &[("x", 1.0), ("n", 3.0)], &["total"]);
    assert_eq!(values, [180.0]);
}
