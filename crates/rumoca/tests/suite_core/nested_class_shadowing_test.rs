//! Regression: a nested class's own member must shadow a same-named member
//! of its enclosing class (MLS §5.3.2).
//!
//! Before the fix, instantiation registered every constant/parameter of an
//! enclosing class as a class-qualified import alias for nested classes,
//! overriding the nested class's own declaration. In MSL this turned
//! `Rotor1D.RotorWith3DEffects.cylinder(R = ... planarRotation(e, ...))`
//! into an unresolved `Modelica.Mechanics.MultiBody.Parts.Rotor1D.e`
//! reference (4 Rotational3DEffects examples failed in ToDae). Enclosing
//! parameters of non-package classes are instance members and must never
//! become class-qualified alias targets at all.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, eval_dae_at};

const SHADOWED_NESTED_MODEL: &str = r#"
within;
model EnclosingShadow
  Inner inner1;
  Real x[3](each start = 0.0, each fixed = true);
protected
  parameter Real e[3] = {7, 0, 0};
  model Probe
    input Real k[3];
    output Real y[3];
  equation
    y = 2 * k;
  end Probe;
  model Inner
    Probe probe(k = e);
    Real z[3];
  protected
    parameter Real e[3] = {0, 11, 0};
  equation
    z = probe.y;
  end Inner;
equation
  der(x) = inner1.z;
end EnclosingShadow;
"#;

#[test]
fn nested_class_member_shadows_enclosing_class_member() {
    let compiled = Compiler::new()
        .model("EnclosingShadow")
        .compile_str(SHADOWED_NESTED_MODEL, "EnclosingShadow.mo")
        .expect("nested shadowed member should compile to DAE");

    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("shadowed nested model should lower and evaluate at t=0");
    let report = &probe.report;
    assert!(report.error.is_none(), "eval error: {:?}", report.error);

    let der = |name: &str| {
        report
            .derivatives
            .iter()
            .find(|slot| slot.name == name)
            .unwrap_or_else(|| {
                panic!(
                    "missing derivative {name}; have: {:?}",
                    report
                        .derivatives
                        .iter()
                        .map(|slot| slot.name.clone())
                        .collect::<Vec<_>>()
                )
            })
            .value
    };
    // probe.k must bind to Inner's `e` = {0, 11, 0}, so der(x) = 2*k.
    assert_eq!(
        der("der(x[1])"),
        0.0,
        "inner `e` must shadow the enclosing `e`"
    );
    assert_eq!(
        der("der(x[2])"),
        22.0,
        "inner `e` must shadow the enclosing `e`"
    );
    assert_eq!(der("der(x[3])"), 0.0);
}
