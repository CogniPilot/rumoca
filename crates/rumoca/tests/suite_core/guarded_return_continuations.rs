//! Two guarded returns with a local defined between them (MLS §12.4.4): the
//! continuation after the first return still owns the local's definition when
//! the second continuation reads it, on every path.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, eval_dae_at};

const SOURCE: &str = r#"
within;
function fallback
  input Real u;
  input Real tag;
  output Real y[2];
  output Real path;
algorithm
  y := {u, -u};
  path := tag;
end fallback;
function twoReturns
  input Real u;
  output Real y[2];
  output Real path;
protected
  Real g[4];
  Real s;
algorithm
  path := 0;
  if u < 0 then
    (y, path) := fallback(u, 1);
    return;
  end if;
  g := zeros(4);
  g[1] := u;
  g[2] := 3 * u;
  s := g[1] + g[2];
  if u > 5 then
    (y, path) := fallback(2 * u, 2);
    return;
  end if;
  y := {g[1], s};
  path := 3;
end twoReturns;
model GuardedReturnPaths
  Real early[2];
  Real earlyPath;
  Real late[2];
  Real latePath;
  Real through[2];
  Real throughPath;
equation
  (early, earlyPath) = twoReturns(time - 1);
  (late, latePath) = twoReturns(time + 7);
  (through, throughPath) = twoReturns(time + 2);
end GuardedReturnPaths;
"#;

fn value(report: &rumoca_sim::EvalAtReport, name: &str) -> f64 {
    report
        .solver_y
        .iter()
        .find(|slot| slot.name.replace(' ', "") == name)
        .unwrap_or_else(|| panic!("missing solver value {name}"))
        .value
}

#[test]
fn both_returns_and_the_fall_through_keep_their_own_definitions() {
    let compiled = Compiler::new()
        .model("GuardedReturnPaths")
        .compile_str(SOURCE, "GuardedReturnPaths.mo")
        .expect("guarded returns around a local definition compile");
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("the checked DAE evaluates");
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    let report = probe.report;
    for (name, expected) in [
        ("early[1]", -1.0),
        ("early[2]", 1.0),
        ("earlyPath", 1.0),
        ("late[1]", 14.0),
        ("late[2]", -14.0),
        ("latePath", 2.0),
        ("through[1]", 2.0),
        ("through[2]", 8.0),
        ("throughPath", 3.0),
    ] {
        assert_eq!(value(&report, name), expected, "{name}");
    }
}
