//! Members of package record constants (MLS §7.1, §7.3, §12.6).
//!
//! `Medium.data.R_s` reads the field `R_s` of the record constant `data` that
//! the package selected by `Medium` exposes; the field's declaration in the
//! record class has no value of its own, the record constant's binding gives it
//! one. The same holds for a member read inside a function of that package,
//! where `data` is exposed by the package the function is called through, and
//! for a record constant bound in an extends modification to another one
//! (`extends Base(data = Data.H2O)`, as `IdealGases.SingleGases.H2O` does).

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimResult, simulate_dae_with_diagnostics};

fn series<'a>(result: &'a SimResult, name: &str) -> &'a [f64] {
    let Some(index) = result.names.iter().position(|candidate| candidate == name) else {
        panic!("simulation result missing column {name}");
    };
    result.data[index].as_slice()
}

fn simulate(source: &str, model: &str) -> SimResult {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(source, &format!("{model}.mo"))
        .unwrap_or_else(|error| panic!("{model} compiles: {error:?}"));
    simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("{model} simulates: {error}"))
}

const MEMBERS: &str = r#"
package Members
  record DataRecord
    Real R_s;
    Real cp;
  end DataRecord;
  package Data
    constant DataRecord H2O(R_s = 461.5, cp = 2000.0);
  end Data;
  partial package Base
    constant DataRecord data;
    function cv
      input Real T;
      output Real y;
    algorithm
      y := data.cp * T / 300 - data.R_s;
    end cv;
  end Base;
  package Med
    extends Base(data = Data.H2O);
  end Med;
  package Direct
    constant DataRecord data(R_s = 287.0, cp = 1000.0);
  end Direct;
  model Model
    package Medium = Med;
    Real T = 300 + time;
    Real direct = Direct.data.cp * T + Direct.data.R_s;
    Real exposed = Medium.data.R_s * T;
    Real called = Medium.cv(T);
  end Model;
end Members;
"#;

#[test]
fn record_constant_members_take_the_field_values_of_their_binding() {
    let result = simulate(MEMBERS, "Members.Model");
    let temperature = series(&result, "T");
    let direct = series(&result, "direct");
    let exposed = series(&result, "exposed");
    let called = series(&result, "called");
    for (index, t) in temperature.iter().enumerate() {
        let expected_direct = 1000.0 * t + 287.0;
        let expected_exposed = 461.5 * t;
        let expected_called = 2000.0 * t / 300.0 - 461.5;
        assert!((direct[index] - expected_direct).abs() < 1e-9 * expected_direct);
        assert!((exposed[index] - expected_exposed).abs() < 1e-9 * expected_exposed);
        assert!((called[index] - expected_called).abs() < 1e-9 * expected_called.abs());
    }
}
