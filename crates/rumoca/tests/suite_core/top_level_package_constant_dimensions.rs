//! A parameter bound to a top-level package constant sizes a dimension of a
//! top-level model (MLS 3.7 §5.3 lookup through the unnamed scope, §10.1).

use rumoca::Compiler;
use rumoca_sim::{SimOptions, eval_dae_at};

const TOP_LEVEL_PACKAGE_CONSTANT: &str = r#"
within;
package Keyframes
  type Mode = enumeration(Fast, Slow);
  constant Integer imageHeight = 3;
  constant Mode mode = Mode.Slow;
end Keyframes;
model Init
  parameter Integer ih = Keyframes.imageHeight;
  parameter Keyframes.Mode mode = Keyframes.mode;
  Real rgb[ih] = fill(2.0, ih);
  Real s = sum(rgb) + (if mode == Keyframes.Mode.Slow then 10.0 else 0.0);
end Init;
"#;

#[test]
fn top_level_package_constant_sizes_a_top_level_model() {
    let compiled = Compiler::new()
        .model("Init")
        .compile_str(TOP_LEVEL_PACKAGE_CONSTANT, "Init.mo")
        .expect("a top-level package constant is found from a top-level model");
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("the model should evaluate");
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    let s = probe
        .report
        .solver_y
        .iter()
        .find(|slot| slot.name == "s")
        .expect("s is a solver value")
        .value;
    assert_eq!(s, 16.0);
}
