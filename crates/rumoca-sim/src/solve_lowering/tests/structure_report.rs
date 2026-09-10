//! Inspection must consume the structural preparation used by simulation (SPEC_0007).

use super::compile;
use crate::{
    BlockReport, SimOptions, diagnose_structural_singularity, lower_dae_for_simulation,
    simulate_dae, structural_report_for_dae,
};

const PENDULUM: &str = r#"
model Pend
  parameter Real L=1;
  parameter Real g=9.81;
  Real x(start=1);
  Real y(start=0);
  Real vx(start=0);
  Real vy(start=0);
  Real lambda;
equation
  der(x)=vx;
  der(y)=vy;
  der(vx)=-lambda*x;
  der(vy)=-g-lambda*y;
  x*x+y*y=L*L;
end Pend;
"#;

#[test]
fn index_three_inspection_agrees_with_simulation_preparation() {
    let dae = compile(PENDULUM, "Pend");
    let options = SimOptions {
        t_end: 0.1,
        dt: Some(0.01),
        ..SimOptions::default()
    };
    let raw_error = dae.inspect(|view| {
        rumoca_phase_structural::build_structural_report(view)
            .expect_err("the unreduced index-three system requires structural preparation")
    });
    assert!(raw_error.to_string().contains("4 matched out of 5"));
    lower_dae_for_simulation(&dae, &options).expect("the pendulum reduces to a computable system");
    let report = structural_report_for_dae(&dae, &options)
        .expect("inspection must report the reduced system the simulator accepts");
    assert_eq!(report.n_equations, report.n_unknowns);
    assert_eq!(report.matching.len(), report.n_equations);
    assert!(!report.blocks.is_empty());
    assert!(
        diagnose_structural_singularity(&dae, &options)
            .expect("diagnosis uses the same preparation")
            .is_none()
    );

    let trace = simulate_dae(&dae, &options).expect("the inspected pendulum must simulate");
    assert_eq!(trace.times.last(), Some(&0.1));
    let x = trace.names.iter().position(|name| name == "x").unwrap();
    let y = trace.names.iter().position(|name| name == "y").unwrap();
    for (&x, &y) in trace.data[x].iter().zip(&trace.data[y]) {
        assert!((x * x + y * y - 1.0).abs() < 1.0e-8);
    }
}

#[test]
fn structural_inspection_preserves_a_reduction_refusal() {
    let source = PENDULUM.replace("start=1", "start=1, fixed=true");
    let dae = compile(&source, "Pend");
    let options = SimOptions::default();
    let simulation = lower_dae_for_simulation(&dae, &options)
        .expect_err("reduction cannot discard a fixed initial value");
    let inspection = structural_report_for_dae(&dae, &options)
        .expect_err("inspection must retain the simulator's reduction refusal");
    let diagnosis = diagnose_structural_singularity(&dae, &options)
        .expect_err("diagnosis must retain the simulator's reduction refusal");
    for message in [
        simulation.to_string(),
        inspection.to_string(),
        diagnosis.to_string(),
    ] {
        assert!(
            message.contains("would discard the stated initial value of `x`"),
            "{message}"
        );
    }
}

#[test]
fn structural_inspection_reports_a_tearing_that_reaches_the_runtime() {
    let dae = compile(
        "model TearLoop Real x(start=0); Real y(start=0); equation x=2*y+1; y=0.25*x; end TearLoop;",
        "TearLoop",
    );
    let options = SimOptions {
        t_end: 0.02,
        dt: Some(0.01),
        ..SimOptions::default()
    };
    let report =
        structural_report_for_dae(&dae, &options).expect("the loop is structurally regular");
    let [
        BlockReport::Coupled {
            unknowns,
            tearing: Some(tearing),
            ..
        },
    ] = report.blocks.as_slice()
    else {
        panic!("the inspector must expose the coupled block's tearing: {report:?}");
    };
    assert_eq!(unknowns.len(), 2);
    assert_eq!(tearing.tear_vars.len(), 1);
    assert_eq!(tearing.residual_equations.len(), 1);
    assert_eq!(tearing.causal_sequence.len(), 1);
    let solve = lower_dae_for_simulation(&dae, &options).expect("the coupled block lowers");
    let blocks = &solve.problem.continuous.algebraic_projection_plan.blocks;
    assert_eq!(blocks.len(), 1);
    let runtime_tearing = blocks[0]
        .tearing
        .as_ref()
        .expect("Solve must carry the tearing");
    assert_eq!(runtime_tearing.tear_y_indices.len(), 1);
    assert_eq!(runtime_tearing.residual_rows.len(), 1);
    assert_eq!(runtime_tearing.causal_steps.len(), 1);
    let trace = simulate_dae(&dae, &options).expect("the torn algebraic loop must execute");
    for (name, expected) in [("x", 2.0), ("y", 0.5)] {
        let column = trace
            .names
            .iter()
            .position(|candidate| candidate == name)
            .unwrap();
        assert!(
            trace.data[column]
                .iter()
                .all(|value| (*value - expected).abs() < 1.0e-12)
        );
    }
}

#[test]
fn structural_inspection_accepts_an_empty_prepared_system() {
    let dae = compile("model Empty end Empty;", "Empty");
    let report = structural_report_for_dae(&dae, &SimOptions::default())
        .expect("an empty prepared system has an empty report");
    assert_eq!(report.n_equations, 0);
    assert_eq!(report.n_unknowns, 0);
    assert!(report.blocks.is_empty());
    assert!(report.matching.is_empty());
}
