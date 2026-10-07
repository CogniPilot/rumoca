//! MLS 3.7 §11.2.2 / §11.2.3: function loops whose domains are bounded only
//! by value facts the body proves. A counter that indexes an array is
//! bounded by that array's extent after the loop that counts it
//! (MLS §10.5); a Real indicator `valid := if c then 1.0 else 0.0` carries
//! the facts of `c` into every branch that tests `valid > 0.0`, which bounds
//! a `while` stride and its start; a sift-down `while` raises its index by
//! the index itself on every pass that does not end the loop; and a window
//! `max(b, y - r):min(n - b - 1, y + r)` is bounded by its half-bounded
//! operands. Each loop iterates a compact envelope and stops at the pass the
//! source stops at. Expected values are computed by hand.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, eval_dae_at};

const MODELS: &str = r#"
function positiveSum
  input Real x[:];
  output Real s;
protected
  Integer count;
  Integer idx[size(x, 1)];
algorithm
  count := 0;
  s := 0;
  idx := fill(0, size(x, 1));
  for i in 1:size(x, 1) loop
    if x[i] > 0 then
      count := count + 1;
      idx[count] := i;
    end if;
  end for;
  for k in 1:count loop
    s := s + x[idx[k]];
  end for;
end positiveSum;

model PositiveSum
  input Real x[5] = {1, -2, 3, -4, 5};
  output Real s;
equation
  s = positiveSum(x);
end PositiveSum;

function stridedSum
  input Real s[:];
  input Real settings[2] "stride, start";
  input Integer n;
  output Real total;
protected
  Real valid;
  Integer spacing;
  Integer start;
  Integer y;
algorithm
  valid := if n > 0 and size(s, 1) == n and settings[1] >= 1.0
    and settings[2] >= 0.0 and settings[2] < n then 1.0 else 0.0;
  spacing := if valid > 0.0 then integer(settings[1]) else 1;
  start := if valid > 0.0 then integer(settings[2]) else 0;
  total := 0;
  if valid > 0.0 then
    y := start;
    while y < n loop
      total := total + s[y + 1];
      y := y + spacing;
    end while;
  end if;
end stridedSum;

model StridedSum
  input Real s[6] = {1, 2, 3, 4, 5, 6};
  input Real settings[2] = {2.0, 1.0};
  output Real total;
equation
  total = stridedSum(s, settings, 6);
end StridedSum;

model StridedSumRejected
  input Real s[6] = {1, 2, 3, 4, 5, 6};
  input Real settings[2] = {0.5, 1.0};
  output Real total;
equation
  total = stridedSum(s, settings, 6);
end StridedSumRejected;

function unprovenStride
  input Real s[:];
  input Integer step;
  output Real total;
protected
  Integer y;
algorithm
  total := 0;
  y := 0;
  while y < size(s, 1) loop
    total := total + s[y + 1];
    y := y + step;
  end while;
end unprovenStride;

model UnprovenStride
  input Real s[3] = {1, 2, 3};
  input Integer step = 1;
  output Real total;
equation
  total = unprovenStride(s, step);
end UnprovenStride;

function siftDown
  input Real heap[:];
  output Real weighted;
protected
  Real a[size(heap, 1)];
  Integer n;
  Integer root;
  Integer child;
  Real swap;
  Boolean descending;
algorithm
  a := heap;
  n := size(heap, 1);
  root := 1;
  descending := true;
  while root <= div(n, 2) and descending loop
    child := 2*root;
    if child < n then
      if a[child + 1] > a[child] then
        child := child + 1;
      end if;
    end if;
    if a[child] > a[root] then
      swap := a[root];
      a[root] := a[child];
      a[child] := swap;
      root := child;
    else
      descending := false;
    end if;
  end while;
  weighted := sum(a[i]*i for i in 1:n);
end siftDown;

model SiftDown
  input Real heap[5] = {1, 5, 3, 4, 2};
  output Real weighted;
equation
  weighted = siftDown(heap);
end SiftDown;

function windowCells
  input Integer n;
  input Real reach;
  input Integer y;
  output Real cells;
protected
  Real valid;
  Integer border;
  Integer r;
algorithm
  valid := if n > 0 and n <= 16 and reach >= 0.0 and reach <= 4.0 then 1.0 else 0.0;
  border := if valid > 0.0 then 1 else 0;
  r := if valid > 0.0 then integer(reach) else 0;
  cells := 0;
  if valid > 0.0 and y >= 0 and y < n then
    for yy in max(border, y - r):min(n - border - 1, y + r) loop
      cells := cells + 1;
    end for;
  end if;
end windowCells;

model WindowCells
  input Real reach = 2.0;
  input Integer y = 1;
  output Real cells;
equation
  cells = windowCells(8, reach, y);
end WindowCells;
"#;

fn values(model: &str, names: &[&str]) -> Vec<f64> {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(MODELS, "FactBoundedLoops.mo")
        .unwrap_or_else(|error| panic!("{model} should compile: {error}"));
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .unwrap_or_else(|error| panic!("{model} should evaluate: {error}"));
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    names
        .iter()
        .map(|name| {
            probe
                .report
                .solver_y
                .iter()
                .find(|slot| slot.name == *name)
                .unwrap_or_else(|| panic!("{model} has no {name}"))
                .value
        })
        .collect()
}

#[test]
fn a_counter_that_indexes_an_array_bounds_the_loop_it_ends() {
    // The positive elements 1, 3 and 5.
    assert_eq!(values("PositiveSum", &["s"]), vec![9.0]);
}

#[test]
fn an_indicator_carries_its_condition_into_a_while_stride_and_start() {
    // Stride 2 from start 1 reads s[2], s[4], s[6].
    assert_eq!(values("StridedSum", &["total"]), vec![12.0]);
    // A stride below 1 fails the indicator, so the loop never runs.
    assert_eq!(values("StridedSumRejected", &["total"]), vec![0.0]);
}

#[test]
fn a_while_stride_with_no_proven_lower_bound_is_refused() {
    let error = Compiler::new()
        .model("UnprovenStride")
        .compile_str(MODELS, "FactBoundedLoops.mo")
        .map(|_| ())
        .expect_err("nothing proves `step >= 1`, so the passes are unbounded");
    let message = error.to_string();
    assert!(
        message.contains("ToDae") && message.contains("`unprovenStride`"),
        "{error}"
    );
}

#[test]
fn a_sift_down_while_advances_its_index_by_the_index_itself() {
    // {1, 5, 3, 4, 2} sifts to {5, 4, 3, 1, 2}: 5 + 8 + 9 + 4 + 10.
    assert_eq!(values("SiftDown", &["weighted"]), vec![36.0]);
}

#[test]
fn a_window_range_is_bounded_by_its_half_bounded_operands() {
    // n = 8, border 1, r = 2, y = 1: max(1, -1):min(6, 3) = 1:3.
    assert_eq!(values("WindowCells", &["cells"]), vec![3.0]);
}
