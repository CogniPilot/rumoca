//! Function-local slices whose bounds share a loop variable, and extent
//! queries on values the algorithm has not defined yet.
//!
//! MLS §10.4.1 sizes `a:b` from `b - a` alone, so `x[i - 1:i]` inside a loop
//! has the exact extent 2 although neither bound is a translation-time value.
//! MLS §12.4.4 still requires every element a slice reads to be defined; with
//! the loop unrolled per point the read indices of `state[i - 2:i - 1]` are
//! exact. MLS §10.3.1 makes `size(y, 1)` the extent of `y`, never its value,
//! so it may be asked before `y` has one. `Modelica.Math.Random.Utilities.
//! initialStateWithXorshift64star` uses all three.

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
