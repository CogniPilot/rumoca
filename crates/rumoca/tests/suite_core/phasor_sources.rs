//! Solve lowering records, from each visible scalar's defining equation, the
//! phasor an angle or power-factor scalar is a function of (SPEC_0040
//! SOLVE-C65), and model wire replay refuses a phasor that names no two
//! distinct visible Real components.

use std::collections::{BTreeMap, HashMap};

use rumoca::Compiler;
use rumoca_ir_solve::{SolveModel, SolvePhasor, SolvePhasorFunction};

fn lowered(source: &str, model: &str) -> SolveModel {
    let dae = Compiler::new()
        .model(model)
        .compile_str(source, &format!("{model}.mo"))
        .unwrap()
        .dae;
    rumoca_phase_solve::lower_solve_model(&dae, &HashMap::new(), |_| {})
        .unwrap()
        .model()
        .clone()
}

fn phasors(model: &SolveModel) -> BTreeMap<String, SolvePhasor> {
    model
        .variable_meta
        .iter()
        .filter_map(|meta| Some((meta.name.clone(), meta.phasor.clone()?)))
        .collect()
}

fn phasor(function: SolvePhasorFunction, re: &str, im: &str) -> SolvePhasor {
    SolvePhasor {
        function,
        re: re.to_string(),
        im: im.to_string(),
    }
}

/// The MSL forms: `Modelica.Math.atan3` (a guarded body, not inlined),
/// `ComplexMath.arg` over it with its default `phi0 = 0`, the
/// `ComplexToPolar` conditional on a parameter, and `cos(arg(...))` power
/// factors, over scalar and record (`Complex`) arguments; plus the forms
/// that must not be recorded.
const PHASORS: &str = r#"
model Phasors
  function atan3
    input Real u1;
    input Real u2;
    input Real y0 = 0;
    output Real y;
  protected
    Real w;
  algorithm
    w := atan2(u1, u2);
    if y0 == 0 then
      y := w;
    else
      y := w + 2*3.141592653589793*integer((3.141592653589793 + y0 - w)/(2*3.141592653589793));
    end if;
  end atan3;
  function arg
    input Real re;
    input Real im;
    input Real phi0 = 0;
    output Real phi;
  algorithm
    phi := atan3(im, re, phi0);
    annotation(Inline = true);
  end arg;
  record Cplx
    Real re;
    Real im;
  end Cplx;
  function argOf
    input Cplx c;
    input Real phi0 = 0;
    output Real phi;
  algorithm
    phi := atan3(c.im, c.re, phi0);
    annotation(Inline = true);
  end argOf;
  parameter Boolean conjugate = false;
  parameter Real reference = 1;
  Real re(start = 1), im(start = 0);
  Real P, Q;
  Real angle, viaAtan3, viaArg, polar, pf;
  Cplx z;
  Real viaRecord, pfRecord;
  Real shifted, referenced, sum;
equation
  der(re) = -im;
  der(im) = re;
  P = 2*re;
  Q = 3*im;
  angle = atan2(im, re);
  viaAtan3 = atan3(im, re, 0);
  viaArg = arg(re, im);
  polar = if conjugate then atan2(-im, re) else atan2(im, re);
  pf = cos(arg(P, Q));
  z = Cplx(re, im);
  viaRecord = argOf(z);
  pfRecord = cos(argOf(Cplx(P, Q)));
  shifted = atan2(im, re) + 1;
  referenced = atan3(im, re, reference);
  sum = atan2(im + P, re);
end Phasors;
"#;

#[test]
fn angles_and_power_factors_record_the_phasor_their_equation_reads() {
    let angle = phasor(SolvePhasorFunction::Angle, "re", "im");
    let power_factor = phasor(SolvePhasorFunction::CosineOfAngle, "P", "Q");
    assert_eq!(
        phasors(&lowered(PHASORS, "Phasors")),
        BTreeMap::from([
            ("angle".to_string(), angle.clone()),
            ("viaAtan3".to_string(), angle.clone()),
            ("viaArg".to_string(), angle.clone()),
            ("polar".to_string(), angle),
            (
                "viaRecord".to_string(),
                phasor(SolvePhasorFunction::Angle, "z.re", "z.im")
            ),
            ("pf".to_string(), power_factor.clone()),
            ("pfRecord".to_string(), power_factor),
        ])
    );
}

fn replay(wire: &serde_json::Value) -> Result<SolveModel, String> {
    let bytes = serde_json::to_vec(wire).unwrap();
    rumoca_phase_solve::deserialize_solve_model(&mut serde_json::Deserializer::from_slice(&bytes))
        .map_err(|error| error.to_string())
}

#[test]
fn model_wire_replay_refuses_a_phasor_without_two_distinct_visible_components() {
    let model = lowered(PHASORS, "Phasors");
    let wire = serde_json::to_value(rumoca_phase_solve::solve_model_wire(&model).unwrap()).unwrap();
    assert_eq!(
        phasors(&replay(&wire).expect("the recorded phasors replay")).len(),
        7
    );
    let index = wire["variable_meta"]
        .as_array()
        .unwrap()
        .iter()
        .position(|meta| meta["name"] == "angle")
        .unwrap();
    for (component, value) in [
        ("re", "absent"),
        ("re", "im"),
        ("im", "angle"),
        ("re", "conjugate"),
    ] {
        let mut refused = wire.clone();
        refused["variable_meta"][index]["phasor"][component] = value.into();
        let error = replay(&refused).expect_err("an invalid phasor is refused");
        assert!(error.contains("phasor"), "{error}");
    }
}
