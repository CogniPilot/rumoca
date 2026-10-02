//! MLS §12.4.4 "It is an error to use or return an uninitialized variable in a
//! function" applied to the executed path.
//!
//! An `if`/`elseif` chain without an `else` may leave a function value
//! unwritten on paths a given call never takes. The IF97 `waterBaseProp_*`
//! functions of `Modelica.Media.Water` leave the `cp` field of their result
//! record unwritten in region 3 and every field unwritten past the region
//! chain. A call on a path that defines every value it uses or returns is
//! well defined; a call on a path that does not fails, exactly where the
//! value is used or returned.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimResult, simulate_dae_with_diagnostics};

/// A record result assembled field by field inside a region chain: region 3
/// never writes `cp`, and no region past 3 writes anything.
const RECORD_FIELDS: &str = r#"
model RecordFields
  record Props
    Real h;
    Real cp;
  end Props;
  function props
    input Real T;
    input Integer region;
    output Props aux;
  algorithm
    if region == 1 then
      aux.h := 4.2*T;
      aux.cp := 4.2;
    elseif region == 2 then
      aux.h := 2.0*T + 1;
      aux.cp := 2.0;
    elseif region == 3 then
      aux.h := 3.0*T;
    end if;
  end props;
  Props p = props(300 + 200*time, if time < 0.5 then 1 elseif time < 0.75 then 2 else REGION);
end RecordFields;
"#;

/// A local written by one arm and read after the conditional, and an output
/// written by one arm and returned.
const SCALARS: &str = r#"
model Scalars
  function f
    input Real u;
    output Real y;
  protected
    Real t;
  algorithm
    if u > 0 then
      t := 2*u;
    end if;
    y := t + 1;
  end f;
  function g
    input Real u;
    output Real y;
  algorithm
    if u > -1 then
      y := u;
    end if;
  end g;
  Real z = CALL;
end Scalars;
"#;

fn simulate(model: &str, source: &str) -> Result<SimResult, String> {
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
    .map_err(|error| format!("{error:?}"))
}

fn column<'result>(result: &'result SimResult, name: &str) -> &'result [f64] {
    let index = result
        .names
        .iter()
        .position(|candidate| candidate == name)
        .unwrap_or_else(|| panic!("{name} is recorded"));
    &result.data[index]
}

#[test]
fn a_call_on_paths_that_define_every_field_returns_the_record() {
    let result = simulate("RecordFields", &RECORD_FIELDS.replace("REGION", "1"))
        .expect("every executed region defines every field");
    let h = column(&result, "p.h");
    let cp = column(&result, "p.cp");
    for (sample, time) in result.times.iter().enumerate() {
        let temperature = 300.0 + 200.0 * time;
        let (expected_h, expected_cp) = if *time < 0.75 && *time >= 0.5 {
            (2.0 * temperature + 1.0, 2.0)
        } else {
            (4.2 * temperature, 4.2)
        };
        assert!(
            (h[sample] - expected_h).abs() < 1e-9,
            "h({time}) = {}",
            h[sample]
        );
        assert!(
            (cp[sample] - expected_cp).abs() < 1e-12,
            "cp({time}) = {}",
            cp[sample]
        );
    }
}

#[test]
fn returning_a_field_the_executed_path_never_wrote_fails_the_call() {
    for region in ["3", "4"] {
        let error = simulate("RecordFields", &RECORD_FIELDS.replace("REGION", region))
            .expect_err("the executed region leaves `cp` unwritten");
        assert!(error.contains("used without a value"), "{error}");
        assert!(error.contains("t=0.75"), "{error}");
    }
}

#[test]
fn a_use_of_a_local_fails_only_on_the_path_that_never_wrote_it() {
    let result = simulate("Scalars", &SCALARS.replace("CALL", "f(2 - time)"))
        .expect("u > 0 on every executed path writes t before its use");
    let z = column(&result, "z");
    for (sample, time) in result.times.iter().enumerate() {
        let expected = 2.0 * (2.0 - time) + 1.0;
        assert!(
            (z[sample] - expected).abs() < 1e-9,
            "z({time}) = {}",
            z[sample]
        );
    }
    let error = simulate("Scalars", &SCALARS.replace("CALL", "f(0.5 - time)"))
        .expect_err("u <= 0 reaches the read of t without a value");
    assert!(error.contains("`t` is used without a value"), "{error}");
    assert!(error.contains("t=0.5"), "{error}");
}

#[test]
fn returning_an_output_the_executed_path_never_wrote_fails_the_call() {
    let error = simulate("Scalars", &SCALARS.replace("CALL", "g(-0.5 - time)"))
        .expect_err("u <= -1 from t = 0.5 leaves y unwritten at return");
    assert!(error.contains("`y` is used without a value"), "{error}");
    assert!(error.contains("t=0.5"), "{error}");
}

/// A value one arm defines that no later statement uses needs no definition
/// past the conditional, so the join leaves it branch-local. The shape is the
/// wall-friction characteristic of `Modelica.Fluid.Pipes`: a slope one arm
/// receives from a multi-result call is read only inside that arm.
const BRANCH_LOCAL_SLOPE: &str = r#"
model BranchLocalSlope
  function g
    input Real x;
    output Real y;
    output Real c;
  algorithm
    assert(x > -10, "x too small");
    y := 2*x;
    c := 3*x;
  end g;
  function wf
    input Real dp;
    output Real m_flow;
  protected
    Real slope;
  algorithm
    if dp >= 10 then
      m_flow := dp;
    else
      (m_flow, slope) := g(dp);
      if dp > 0 then
        m_flow := g(slope);
      else
        m_flow := g(-slope);
      end if;
    end if;
  end wf;
  Real m = wf(4*(time - 0.5));
end BranchLocalSlope;
"#;

#[test]
fn a_value_no_later_statement_uses_stays_branch_local() {
    let result = simulate("BranchLocalSlope", BRANCH_LOCAL_SLOPE)
        .expect("the slope is read only inside the arm that defines it");
    let m = column(&result, "m");
    for (sample, time) in result.times.iter().enumerate() {
        let dp = 4.0 * (time - 0.5);
        let slope = 3.0 * dp;
        let expected = if dp > 0.0 { 2.0 * slope } else { -2.0 * slope };
        assert!(
            (m[sample] - expected).abs() < 1e-9,
            "m({time}) = {}",
            m[sample]
        );
    }
}
