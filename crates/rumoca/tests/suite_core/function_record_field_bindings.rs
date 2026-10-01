//! Record fields inside functions, read and written by exact declaration.
//!
//! A protected binding such as `Real k1 = k(if data.atA then data.da else
//! data.db, data.zeta)` is evaluated in the function's own scope (MLS 3.7
//! §12.4.4), so it reads a decomposed record input exactly as the algorithm
//! does. A record result whose field names share a prefix (`zeta1` and
//! `zeta1_at_a`, as in `Modelica.Fluid` loss-factor data) is assembled field
//! by field: an assignment to `data.zeta1` writes `zeta1` only.

use rumoca::Compiler;

const SOURCE: &str = r#"
record Data
  Real da;
  Real db;
  Real zeta1;
  Boolean zeta1_at_a = true;
end Data;

function expansion
  input Real da;
  input Real db;
  output Data data;
protected
  Real rel;
algorithm
  data.da := da;
  data.db := db;
  if da <= db then
    rel := (da/db)^2;
    data.zeta1 := (1 - rel)^2;
    data.zeta1_at_a := true;
  else
    rel := (db/da)^2;
    data.zeta1 := 0.5*(1 - rel)^0.75;
    data.zeta1_at_a := false;
  end if;
end expansion;

function constantOf
  input Real d;
  input Real zeta;
  output Real k;
algorithm
  k := zeta/d^2;
end constantOf;

function loss
  input Real m;
  input Data data;
  output Real dp;
protected
  Real k1 = constantOf(if data.zeta1_at_a then data.da else data.db, data.zeta1);
algorithm
  dp := k1*m;
end loss;

model Fitting
  parameter Data narrowing = expansion(0.1, 0.2);
  parameter Data widening = expansion(0.2, 0.1);
  Real x(start = 1, fixed = true);
  Real dp1;
  Real dp2;
equation
  der(x) = -x;
  dp1 = loss(x, narrowing);
  dp2 = loss(x, widening);
end Fitting;
"#;

#[test]
fn protected_bindings_read_decomposed_record_fields_and_prefixed_fields_stay_distinct() {
    let compiled = Compiler::new()
        .model("Fitting")
        .compile_str(SOURCE, "FunctionRecordFieldBindings.mo")
        .unwrap_or_else(|error| panic!("Fitting compiles: {error:?}"));
    let result = rumoca_sim::simulate_dae_with_diagnostics(
        &compiled.dae,
        &rumoca_sim::SimOptions {
            t_end: 0.5,
            ..Default::default()
        },
    )
    .expect("Fitting simulates");
    let column = |name: &str| {
        let index = result.names.iter().position(|n| n == name);
        &result.data[index.unwrap_or_else(|| panic!("{name} in {:?}", result.names))]
    };
    // rel = 0.25 for both. Narrowing: zeta1 = 0.75^2 on diameter_a = 0.1.
    // Widening: zeta1 = 0.5*0.75^0.75 on diameter_b = 0.1.
    let narrowing = 0.75_f64.powi(2) / 0.01;
    let widening = 0.5 * 0.75_f64.powf(0.75) / 0.01;
    for (index, _) in result.times.iter().enumerate() {
        let x = column("x")[index];
        let dp1 = column("dp1")[index];
        let dp2 = column("dp2")[index];
        assert!(
            (dp1 - narrowing * x).abs() < 1e-9 * narrowing,
            "dp1 = {dp1}"
        );
        assert!((dp2 - widening * x).abs() < 1e-9 * widening, "dp2 = {dp2}");
    }
}
