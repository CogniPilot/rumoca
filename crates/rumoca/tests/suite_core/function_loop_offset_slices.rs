//! Function-local slices whose bounds share a loop variable, and extent
//! queries on values the algorithm has not defined yet.
//!
//! MLS §10.4.1 sizes `a:b` from `b - a` alone, so `x[i - 1:i]` inside a loop
//! has the exact extent 2 although neither bound is a translation-time value.
//! MLS §12.4.4 still requires every element a slice reads to be defined; with
//! the loop unrolled per point the read indices of `state[i - 2:i - 1]` are
//! exact. MLS §10.3.1 makes `size(y, 1)` the extent of `y`, never its value,
//! so it may be asked before `y` has one. `Modelica.Math.Random.Utilities.
//! initialStateWithXorshift64star` uses all three. A window such as
//! `img[row - radius:row + radius, ...]` over a named constant radius is the
//! same construct; an extent that varies with the loop index is refused.

use rumoca::Compiler;

fn simulate(source: &str, model: &str) -> rumoca_sim::SimResult {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(source, "function_loop_offset_slices.mo")
        .unwrap_or_else(|error| panic!("{model} compiles: {error:?}"));
    rumoca_sim::simulate_dae_with_diagnostics(
        &compiled.dae,
        &rumoca_sim::SimOptions {
            t_end: 0.1,
            ..Default::default()
        },
    )
    .unwrap_or_else(|error| panic!("{model} simulates: {error:?}"))
}

fn final_value(result: &rumoca_sim::SimResult, name: &str) -> f64 {
    let index = result
        .names
        .iter()
        .position(|candidate| candidate == name)
        .unwrap_or_else(|| panic!("{name} in {:?}", result.names));
    *result.data[index].last().expect("a simulated trace")
}

const PAIR_SUMS: &str = r#"
model PairSums
  function pairSum
    input Real p[2];
    output Real s;
  algorithm
    s := p[1] + p[2];
  end pairSum;
  function pairSums
    input Real x[4];
    output Real y[3];
  algorithm
    for i in 2:4 loop
      y[i - 1] := pairSum(x[i - 1:i]);
    end for;
  end pairSums;
  parameter Real a = 1;
  Real y[3] = pairSums({a, 2*a, 3*a, 4*a});
end PairSums;
"#;

#[test]
fn a_slice_whose_bounds_share_a_loop_variable_has_an_exact_extent() {
    let result = simulate(PAIR_SUMS, "PairSums");
    for (index, expected) in [3.0, 5.0, 7.0].into_iter().enumerate() {
        let name = format!("y[{}]", index + 1);
        assert_eq!(final_value(&result, &name), expected, "{name}");
    }
}

const CHAINED_STATE: &str = r#"
model ChainedState
  function step
    input Integer s[2];
    output Real r;
    output Integer t[2];
  algorithm
    t := {s[2] + 1, s[1] + 3};
    r := 0.5;
  end step;
  function chain
    input Integer seed;
    input Integer n;
    output Integer[n] state;
  protected
    Real r;
    Integer aux[2];
    Integer nEven;
  algorithm
    aux := {seed, seed + 1};
    if n >= 2 then
      state[1:2] := aux;
    else
      state[1] := aux[1];
    end if;
    nEven := 2*div(n, 2);
    for i in 3:2:nEven loop
      (r, aux) := step(state[i - 2:i - 1]);
      state[i:i + 1] := aux;
    end for;
    if n >= 3 and n <> nEven then
      (r, aux) := step(state[n - 2:n - 1]);
      state[n] := aux[1];
    end if;
  end chain;
  parameter Integer seed = 3;
  discrete Integer s4[4](each start = 0, each fixed = true);
  discrete Integer s5[5](each start = 0, each fixed = true);
algorithm
  when initial() then
    s4 := chain(seed, size(s4, 1));
    s5 := chain(seed, size(s5, 1));
  end when;
end ChainedState;
"#;

#[test]
fn a_loop_reads_the_slice_its_earlier_iterations_defined() {
    let result = simulate(CHAINED_STATE, "ChainedState");
    for (name, expected) in [
        ("s4[1]", 3.0),
        ("s4[2]", 4.0),
        ("s4[3]", 5.0),
        ("s4[4]", 6.0),
        ("s5[1]", 3.0),
        ("s5[2]", 4.0),
        ("s5[3]", 5.0),
        ("s5[4]", 6.0),
        ("s5[5]", 7.0),
    ] {
        assert_eq!(final_value(&result, name), expected, "{name}");
    }
}

const OWN_EXTENT: &str = r#"
model OwnExtent
  function scaled
    input Real u;
    output Real y[3];
  algorithm
    y := {u, 2*u, 3*u}*size(y, 1);
  end scaled;
  parameter Real a = 2;
  Real y[3] = scaled(a);
end OwnExtent;
"#;

#[test]
fn an_output_may_ask_its_own_extent_before_it_is_defined() {
    let result = simulate(OWN_EXTENT, "OwnExtent");
    for (index, expected) in [6.0, 12.0, 18.0].into_iter().enumerate() {
        let name = format!("y[{}]", index + 1);
        assert_eq!(final_value(&result, &name), expected, "{name}");
    }
}

// A readable image kernel: the window is a slice whose bounds read both loop
// indices and a named constant radius, inside loops bounded by `size`.
const PATCH_SUMS: &str = r#"
model PatchSums
  function weighted
    input Real p[3,3];
    output Real s;
  algorithm
    s := sum(p[i,j]*(10*i + j) for i in 1:3, j in 1:3);
  end weighted;
  function patchSums
    input Real img[:,:];
    output Real weightedSums[size(img,1),size(img,2)];
    output Real stridedSums[size(img,1),size(img,2)];
  protected
    constant Integer radius = 1;
  algorithm
    weightedSums := zeros(size(img,1),size(img,2));
    stridedSums := zeros(size(img,1),size(img,2));
    for row in radius+1:size(img,1)-radius loop
      for column in radius+1:size(img,2)-radius loop
        weightedSums[row,column] :=
          weighted(img[row-radius:row+radius,column-radius:column+radius]);
        stridedSums[row,column] :=
          sum(img[row-radius:row+radius,column-radius:2*radius:column+radius]);
      end for;
    end for;
  end patchSums;
  parameter Real a = 1;
  Real w[4,5];
  Real p[4,5];
equation
  (w, p) = patchSums({{a*(7*r + c*c) for c in 1:5} for r in 1:4});
end PatchSums;
"#;

/// The weighted and column-strided sums of the 3x3 window centred on
/// `(row, column)` of `PatchSums`, accumulated in the same element order.
fn expected_window_sums(row: i64, column: i64) -> (f64, f64) {
    let image = |r: i64, c: i64| (7 * r + c * c) as f64;
    let window = (1..=3_i64).flat_map(|i| (1..=3_i64).map(move |j| (i, j)));
    let (mut weighted, mut strided) = (0.0, 0.0);
    for (i, j) in window {
        let value = image(row - 2 + i, column - 2 + j);
        weighted += value * (10 * i + j) as f64;
        if j != 2 {
            strided += value;
        }
    }
    (weighted, strided)
}

#[test]
fn a_loop_window_slice_with_a_named_radius_is_a_fixed_extent_view() {
    let result = simulate(PATCH_SUMS, "PatchSums");
    for row in 1..=4_i64 {
        for column in 1..=5_i64 {
            let interior = (2..=3).contains(&row) && (2..=4).contains(&column);
            let (weighted, strided) = if interior {
                expected_window_sums(row, column)
            } else {
                (0.0, 0.0)
            };
            let w = format!("w[{row},{column}]");
            let p = format!("p[{row},{column}]");
            assert_eq!(final_value(&result, &w), weighted, "{w}");
            assert_eq!(final_value(&result, &p), strided, "{p}");
        }
    }
}

// The run-time offset `m` cancels from the extent but is never folded: the
// view follows `k` after its event.
const RUNTIME_OFFSET: &str = r#"
model RuntimeOffset
  function weighted
    input Real p[2];
    output Real s;
  algorithm
    s := sum(p[i]*(10*i) for i in 1:2);
  end weighted;
  function pairs
    input Real x[:];
    input Integer m;
    output Real y[3];
  algorithm
    for i in 1:3 loop
      y[i] := weighted(x[i+m:i+m+1]);
    end for;
  end pairs;
  discrete Integer k(start = 1, fixed = true);
  Real y[3] = pairs({1, 2, 3, 4, 5, 6}, k);
equation
  when time > 0.05 then
    k = 2;
  end when;
end RuntimeOffset;
"#;

#[test]
fn a_window_offset_known_only_at_run_time_is_read_at_run_time() {
    let result = simulate(RUNTIME_OFFSET, "RuntimeOffset");
    for i in 1..=3 {
        let name = format!("y[{i}]");
        let expected = 10.0 * (i + 2) as f64 + 20.0 * (i + 3) as f64;
        assert_eq!(final_value(&result, &name), expected, "{name}");
    }
}

#[test]
fn a_slice_whose_extent_varies_with_the_loop_index_is_refused() {
    let error = Compiler::new()
        .model("GrowingSlice")
        .compile_str(
            r#"
model GrowingSlice
  function total
    input Real p[:];
    output Real y;
  algorithm
    y := sum(p);
  end total;
  function prefixes
    input Real x[:];
    output Real y[3];
  algorithm
    for i in 1:3 loop
      y[i] := total(x[i:2*i]);
    end for;
  end prefixes;
  Real y[3] = prefixes({1, 2, 3, 4, 5, 6});
end GrowingSlice;
"#,
            "GrowingSlice.mo",
        )
        .expect_err("`x[i:2*i]` has an extent that changes with `i`");
    let rendered = format!("{error:?}");
    assert!(
        rendered.contains("function shape proof")
            && rendered.contains("extent depends on the value of scalar `i`"),
        "the unproven extent must be refused at its slice: {rendered}"
    );
}
