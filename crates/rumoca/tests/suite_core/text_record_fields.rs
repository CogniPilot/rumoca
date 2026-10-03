//! Records with `String` fields in pure-call interfaces.
//!
//! A `String` carries no numeric value, so it occupies no leaf of a function's
//! interface: `IdealGases.Common.DataRecord` passes its coefficients to
//! `cp_T(data, T)` and leaves its `name` out. A body that computes with the
//! text is refused at construction instead of reading an empty value.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimResult, simulate_dae_with_diagnostics};

fn series<'a>(result: &'a SimResult, name: &str) -> &'a [f64] {
    let Some(index) = result.names.iter().position(|candidate| candidate == name) else {
        panic!("simulation result missing column {name}");
    };
    result.data[index].as_slice()
}

const NAMED_DATA: &str = r#"
package NamedData
  record DataRecord
    String name;
    Real R_s;
    Real a[3];
  end DataRecord;
  constant DataRecord H2O(name = "H2O", R_s = 461.5, a = {1, 2, 3});
  function cp_T
    input DataRecord d;
    input Real T;
    output Real cp;
  algorithm
    cp := d.R_s * (d.a[1] + d.a[2] * T + d.a[3] * T * T);
  end cp_T;
  function sameName
    input DataRecord d;
    output Boolean same;
  algorithm
    same := d.name == "H2O";
  end sameName;
  function cp
    input Real T;
    output Real y;
  algorithm
    y := cp_T(H2O, T);
  end cp;
  model Model
    Real T = 1 + time;
    Real cp = NamedData.cp(T);
    Real direct = cp_T(H2O, T);
  end Model;
  model ReadsName
    Boolean same = sameName(H2O);
    Real x(start = 0, fixed = true);
  equation
    der(x) = if same then 1 else 0;
  end ReadsName;
end NamedData;
"#;

#[test]
fn text_fields_occupy_no_leaf_of_a_call_interface() {
    let compiled = Compiler::new()
        .model("NamedData.Model")
        .compile_str(NAMED_DATA, "NamedData.mo")
        .expect("the named data record compiles");
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .expect("a call passing a record with a text field simulates");
    for (t, cp) in series(&result, "T").iter().zip(series(&result, "cp")) {
        let expected = 461.5 * (1.0 + 2.0 * t + 3.0 * t * t);
        assert!((cp - expected).abs() < 1e-9 * expected, "cp_T at T = {t}");
    }
    for (cp, direct) in series(&result, "cp").iter().zip(series(&result, "direct")) {
        assert!(
            (cp - direct).abs() < 1e-9 * cp.abs(),
            "model-level cp_T call"
        );
    }
}

#[test]
fn computing_with_a_text_field_is_refused() {
    let compiled = Compiler::new()
        .model("NamedData.ReadsName")
        .compile_str(NAMED_DATA, "NamedData.mo");
    let refused = match compiled {
        Err(_) => true,
        Ok(compiled) => {
            simulate_dae_with_diagnostics(&compiled.dae, &SimOptions::default()).is_err()
        }
    };
    assert!(
        refused,
        "a text comparison inside a function must be refused"
    );
}
