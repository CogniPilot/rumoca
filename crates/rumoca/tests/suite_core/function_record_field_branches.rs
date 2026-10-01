//! MLS §12.2: a function may assign the fields of its record result one at a
//! time inside `if` and `for` statements, as the IF97 `waterBaseProp_*`
//! functions of `Modelica.Media.Water` do for their auxiliary record. Each
//! field is then one function value, and the result is assembled from them.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
package FieldBranches
  record Aux
    Integer region;
    Real h;
    Real cp;
    Real c[2];
  end Aux;
  function props
    input Real p;
    input Real T;
    output Aux aux;
  algorithm
    aux.region := if T < 400 then 1 else 2;
    if aux.region == 1 then
      aux.cp := 4.2;
      aux.h := aux.cp*T;
    elseif aux.region == 2 then
      aux.cp := 2.0;
      aux.h := aux.cp*T + p;
    else
      assert(false, "region");
    end if;
    for i in 1:2 loop
      aux.c[i] := i*aux.cp;
    end for;
  end props;
  function h_pT
    input Real p;
    input Real T;
    output Real h;
  protected
    Aux aux;
  algorithm
    aux := props(p, T);
    h := aux.h + aux.c[2];
  end h_pT;
  model Top
    Real h = h_pT(1, 300 + 200*time);
    Aux aux = props(1, 300 + 200*time);
  end Top;
end FieldBranches;
"#;

#[test]
fn a_record_result_assigned_field_by_field_in_branches_and_loops() {
    let compiled = Compiler::new()
        .model("FieldBranches.Top")
        .compile_str(SOURCE, "FieldBranches.mo")
        .unwrap_or_else(|error| panic!("Top compiles: {error:?}"));
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..Default::default()
        },
    )
    .expect("Top simulates");
    let column = |name: &str| {
        let index = result.names.iter().position(|n| n == name);
        &result.data[index.unwrap_or_else(|| panic!("{name} in {:?}", result.names))]
    };
    for (sample, time) in result.times.iter().enumerate() {
        let temperature = 300.0 + 200.0 * time;
        if (temperature - 400.0).abs() < 1e-6 {
            continue;
        }
        let (cp, h) = if temperature < 400.0 {
            (4.2, 4.2 * temperature)
        } else {
            (2.0, 2.0 * temperature + 1.0)
        };
        let expected = [
            ("h", h + 2.0 * cp),
            ("aux.h", h),
            ("aux.cp", cp),
            ("aux.c[1]", cp),
            ("aux.c[2]", 2.0 * cp),
        ];
        for (name, value) in expected {
            let actual = column(name)[sample];
            assert!(
                (actual - value).abs() < 1e-9,
                "{name}({time}) = {actual}, expected {value}"
            );
        }
    }
}
