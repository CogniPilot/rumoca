//! A package's constant record read by field, in two packages of one base.
//!
//! `Base` declares `constant Data data` and a model that reads `data.MM`;
//! `A` and `B` extend it with their own `data` (the MSL's ideal gases: each
//! `SingleGasNasa` medium its `data`, `BaseProperties` reads `data.MM` and
//! `data.R_s`). A model with a component of each must read each package's
//! own record. It read one of the two for both, which one differing from
//! run to run: air beside methane was computed as methane, or methane as air.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
package TwoLib
  record Data
    Real MM;
    Real c;
  end Data;
  package Datas
    constant Data Air(MM = 1, c = 10);
    constant Data Gas(MM = 2, c = 20);
  end Datas;
  function twice
    input Data d;
    output Real y;
  algorithm
    y := 2*d.MM;
  end twice;
  partial package Base
    constant Data data;
    model Props
      Real y;
      Real z;
    equation
      y = data.MM;
      z = twice(data);
    end Props;
  end Base;
  package A
    extends Base(data = Datas.Air);
  end A;
  package B
    extends Base(data = Datas.Gas);
  end B;
  model Use
    replaceable package M = Base;
    M.Props p;
  end Use;
end TwoLib;

model Two
  TwoLib.Use a(redeclare package M = TwoLib.A);
  TwoLib.Use b(redeclare package M = TwoLib.B);
end Two;
"#;

fn values() -> [f64; 4] {
    let compiled = Compiler::new()
        .model("Two")
        .compile_str(SOURCE, "two.mo")
        .unwrap_or_else(|error| panic!("Two: {error:#}"));
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..Default::default()
        },
    )
    .unwrap_or_else(|error| panic!("Two: {error}"));
    ["a.p.y", "b.p.y", "a.p.z", "b.p.z"].map(|name| {
        let column = result
            .names
            .iter()
            .position(|n| n == name)
            .unwrap_or_else(|| panic!("no {name} in {:?}", result.names));
        *result.data[column].last().expect("a sample")
    })
}

/// Each component its own package's record, by field and as a whole, every
/// time (which package won depended on a hash map's order: compiled often).
#[test]
fn each_component_reads_its_own_package_s_constant_record() {
    for round in 0..24 {
        assert_eq!(values(), [1.0, 2.0, 2.0, 4.0], "round {round}");
    }
}
