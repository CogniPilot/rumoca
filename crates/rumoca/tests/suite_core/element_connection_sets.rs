//! Discrete connection sets of array elements (MLS §9.2).
//!
//! `connect(b, and.x[1])` and `connect(a, and.x[2])` put the two elements of
//! `and.x` in different connection sets. The half adder of
//! `Modelica.Electrical.Digital` connects each input to one element of each
//! gate, in an order that makes `and.x[1]` follow `xor.x[1]` while `xor.x[2]`
//! follows `and.x[2]`. Each element is oriented from its own set's producer,
//! and neither gate input array depends on the other.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae};

const HALF_ADDER: &str = r#"
package P
  type Logic = enumeration('0', '1');
  connector LogicIn = input Logic;
  connector LogicOut = output Logic;
  block Step
    parameter Real t0;
    LogicOut y;
  algorithm
    y := if time >= t0 then Logic.'1' else Logic.'0';
  end Step;
  block And2
    LogicIn x[2];
    LogicOut y;
  equation
    y = if x[1] == Logic.'1' and x[2] == Logic.'1' then Logic.'1' else Logic.'0';
  end And2;
  block Xor2
    LogicIn x[2];
    LogicOut y;
  equation
    y = if x[1] <> x[2] then Logic.'1' else Logic.'0';
  end Xor2;
  model HalfAdder
    LogicIn a;
    LogicIn b;
    LogicOut s;
    LogicOut c;
    And2 conj;
    Xor2 disj;
  equation
    connect(conj.y, c);
    connect(disj.y, s);
    connect(b, conj.x[1]);
    connect(b, disj.x[1]);
    connect(a, disj.x[2]);
    connect(a, conj.x[2]);
  end HalfAdder;
  model Top
    Step sa(t0 = 0.25);
    Step sb(t0 = 0.5);
    HalfAdder adder;
  equation
    connect(sa.y, adder.a);
    connect(sb.y, adder.b);
  end Top;
end P;
"#;

#[test]
fn interleaved_element_connections_define_every_gate_input() {
    let compiled = Compiler::new()
        .model("P.Top")
        .compile_str(HALF_ADDER, "ElementConnectionSets.mo")
        .unwrap_or_else(|error| panic!("P.Top compiles: {error:?}"));
    let result = simulate_dae(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .expect("P.Top simulates");
    let column = |name: &str| {
        let index = result
            .names
            .iter()
            .position(|candidate| candidate == name)
            .unwrap_or_else(|| panic!("simulation exposes {name}"));
        &result.data[index]
    };
    let (s, c) = (column("adder.s"), column("adder.c"));
    // Enumeration ordinals: '0' = 1, '1' = 2.
    for ((time, s), c) in result.times.iter().zip(s).zip(c) {
        if [0.25, 0.5].iter().any(|edge| (time - edge).abs() < 1.0e-9) {
            continue;
        }
        let a = *time >= 0.25;
        let b = *time >= 0.5;
        let ordinal = |bit: bool| if bit { 2.0 } else { 1.0 };
        assert_eq!(*s, ordinal(a != b), "sum at t = {time}");
        assert_eq!(*c, ordinal(a && b), "carry at t = {time}");
    }
}
