//! Structural selections of equation conditionals (SPEC_0040 DAE-C22, MLS 3.7
//! §8.3.4).
//!
//! An if-equation whose equal-count arms define different unknowns under an
//! ordinary parameter guard is a structural selection: the arm the guard never
//! takes is not part of the system, so each selected row keeps the owner its
//! own target selects (a discrete-valued arm is an Appendix B assignment), and
//! the guard parameter is fixed at translation. Arms that read the same
//! unknowns keep a run-time branch and a settable parameter.

use rumoca::Compiler;
use rumoca_ir_dae as dae;
use rumoca_sim::{SimOptions, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
model ArmSelection
  parameter Boolean stiff = false;
  Real x(start = 1, fixed = true);
  Real z;
  Boolean b(start = false);
equation
  der(x) = -1;
  if stiff then
    z = 0;
    b = false;
  else
    b = pre(b) or x < 0.5;
    z = x;
  end if;
end ArmSelection;

model StiffArmSelection
  extends ArmSelection(stiff = true);
end StiffArmSelection;

type Frame = enumeration(world, frameA, frameB);

record Orientation
  Real T[3,3];
  Real w[3];
end Orientation;

function nullRotation
  output Orientation R;
algorithm
  R := Orientation(T = identity(3), w = zeros(3));
end nullRotation;

model RecordArms
  parameter Frame frameIn = Frame.frameB;
  parameter Frame frameOut = frameIn;
  Real rIn[3] = {time, 2, 3};
  Real rOut[3];
  Orientation R1;
  Orientation Ra = nullRotation();
equation
  if frameOut == frameIn then
    rOut = rIn;
    R1 = nullRotation();
  else
    if frameIn == Frame.world then
      R1 = nullRotation();
    else
      R1 = Ra;
    end if;
    rOut = 2*rIn;
  end if;
end RecordArms;

model RetainedBranch
  parameter Boolean fast = false;
  Real x(start = 1, fixed = true);
equation
  if fast then
    der(x) = -2*x;
  else
    der(x) = -x;
  end if;
end RetainedBranch;
"#;

fn compile(model: &str) -> std::sync::Arc<dae::Dae> {
    Compiler::new()
        .model(model)
        .compile_str(SOURCE, "StructuralBranchSelections.mo")
        .unwrap_or_else(|error| panic!("{model} compiles: {error:?}"))
        .dae
}

fn evaluable(model: &dae::Dae) -> Vec<String> {
    model.inspect(|view| {
        view.variables()
            .filter(|(_, variable)| variable.is_evaluable())
            .map(|(_, variable)| variable.name().to_string())
            .collect()
    })
}

fn value_at(model: &dae::Dae, name: &str, time: f64) -> f64 {
    let result = simulate_dae_with_diagnostics(
        model,
        &SimOptions {
            t_end: 1.0,
            ..Default::default()
        },
    )
    .unwrap_or_else(|error| panic!("simulates: {error:?}"));
    let column = result
        .names
        .iter()
        .position(|candidate| candidate == name)
        .unwrap_or_else(|| panic!("{name} in {:?}", result.names));
    let row = result
        .times
        .iter()
        .rposition(|t| *t <= time)
        .expect("a sample at or before the time");
    result.data[column][row]
}

#[test]
fn a_parameter_guard_over_different_unknowns_selects_its_arm() {
    let selected = compile("ArmSelection");
    assert_eq!(evaluable(&selected), ["stiff"]);
    assert_eq!(value_at(&selected, "b", 0.4), 0.0);
    assert_eq!(value_at(&selected, "b", 0.6), 1.0);
    assert!((value_at(&selected, "z", 0.6) - 0.4).abs() < 1e-6);
    let stiff = compile("StiffArmSelection");
    assert_eq!(value_at(&stiff, "b", 0.9), 0.0);
    assert_eq!(value_at(&stiff, "z", 0.9), 0.0);
}

#[test]
fn a_parameter_guard_over_the_same_unknowns_stays_a_run_time_branch() {
    assert!(evaluable(&compile("RetainedBranch")).is_empty());
}

#[test]
fn a_record_aggregate_arm_reads_its_field_unknowns() {
    // `R1 = Ra` reads every field of both aggregates, so the arms differ in
    // their unknowns and the parameter guard is a structural selection.
    let selected = compile("RecordArms");
    let mut evaluable = evaluable(&selected);
    evaluable.sort();
    assert_eq!(evaluable, ["frameIn", "frameOut"]);
    assert!((value_at(&selected, "rOut[1]", 0.6) - 0.6).abs() < 1e-6);
    assert_eq!(value_at(&selected, "R1.T[1,1]", 0.6), 1.0);
}
