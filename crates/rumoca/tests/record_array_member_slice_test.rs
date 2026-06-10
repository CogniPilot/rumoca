//! Regression: a connector/record-array member slice such as `pin[:].v`
//! must scalarize into the per-element component variables `pin[k].v`
//! (MLS §10.5.1 slice of an array of components).
//!
//! Before the fix, structural scalarization split the array equation into
//! per-element rows but left the colon slice in each residual, and the Solve
//! lowering rejected it with "slice subscript `:` is unsupported". This is
//! the MSL PowerConverters `vAC = ac.pin[:].v` pattern.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, eval_dae_at};

const SLICE_MEMBER_MODEL: &str = r#"
within;
connector Pin
  Real v;
  flow Real i;
end Pin;
model SliceMember
  Pin pin[3];
  Real v[3] = pin[:].v;
  Real x(start = 0, fixed = true);
equation
  for k in 1:3 loop
    pin[k].v = k * 10.0;
  end for;
  der(x) = v[2];
end SliceMember;
"#;

#[test]
fn record_array_member_slice_scalarizes_per_element() {
    let compiled = Compiler::new()
        .model("SliceMember")
        .compile_str(SLICE_MEMBER_MODEL, "SliceMember.mo")
        .expect("member slice model should compile to DAE");

    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("member slice model should lower and evaluate at t=0");
    let report = &probe.report;
    assert!(report.error.is_none(), "eval error: {:?}", report.error);

    let der_x = report
        .derivatives
        .iter()
        .find(|slot| slot.name == "der(x)")
        .unwrap_or_else(|| {
            panic!(
                "missing der(x); have: {:?}",
                report
                    .derivatives
                    .iter()
                    .map(|slot| slot.name.clone())
                    .collect::<Vec<_>>()
            )
        })
        .value;
    // v[2] = pin[2].v = 20.0
    assert_eq!(der_x, 20.0, "v = pin[:].v must select per-element values");
}
