//! MLS §12.4.4 / ALG-019: definedness survives a selected loop and its remainder.

use rumoca::Compiler;
use rumoca_ir_dae::Dae;
use rumoca_sim::{SimExecutionPolicy, SimOptions, SimSolverMode, SimulationSession};

const AUTHORED: &str =
    include_str!("../../../../examples/interactive/reusable_booster/ReusableBoosterLanding.mo");

#[test]
fn original_support_margin_preserves_all_six_source_branches() {
    let start = AUTHORED.find("function triangleSupportMargin\n").unwrap();
    let end = AUTHORED.find("end loadedFootSupportMargin;").unwrap()
        + "end loadedFootSupportMargin;".len();
    let source = format!(
        "{}\nmodel MarginPaths input Real loaded[4](each start=1); output Real margin; Real x(start=0,fixed=true); equation margin=loadedFootSupportMargin([-1,1,1,-1;-1,1,-1,1;0,0,0,0],{{0,0}},loaded); der(x)=margin; end MarginPaths;",
        &AUTHORED[start..end]
    );
    let compiled = Compiler::new()
        .model("MarginPaths")
        .compile_str(&source, "original_support_margin_paths.mo")
        .unwrap();
    // These four triangle diagonals contain the origin; each square edge is
    // two units long and one unit from it. Keep the authored regularization.
    let cases = [
        ([0., 0., 0., 0.], -1.),
        ([0., 1., 1., 1.], 0.),
        ([1., 0., 1., 1.], 0.),
        ([1., 1., 0., 1.], 0.),
        ([1., 1., 1., 0.], 0.),
        ([1., 1., 1., 1.], 2. / (4. + 1e-12_f64).sqrt()),
    ];
    let mut failures = Vec::new();
    for policy in [SimExecutionPolicy::Interpreter, SimExecutionPolicy::Auto] {
        for (loaded, expected) in cases {
            let inputs = loaded
                .into_iter()
                .enumerate()
                .map(|(index, value)| (format!("loaded[{}]", index + 1), value))
                .collect();
            if let Err(error) = check_path(&compiled.dae, policy, inputs, "margin", expected) {
                failures.push(format!("{policy:?} {loaded:?}: {error}"));
            }
        }
    }
    assert!(failures.is_empty(), "{failures:#?}");
}

#[test]
fn selected_fold_definedness_keeps_unassigned_paths_faulting() {
    let source = "function PartialFold input Real u; output Real y; algorithm if u>0 then y:=1; for i in 1:3 loop y:=y+i; end for; end if; if u<0 then y:=2; end if; end PartialFold; model PartialPaths input Real u(start=0); output Real y; Real x(start=0,fixed=true); equation y=PartialFold(u); der(x)=y; end PartialPaths;";
    let compiled = Compiler::new()
        .model("PartialPaths")
        .compile_str(source, "selected_fold_partial_paths.mo")
        .unwrap();
    for policy in [SimExecutionPolicy::Interpreter, SimExecutionPolicy::Auto] {
        check_path(&compiled.dae, policy, vec![("u".into(), 1.)], "y", 7.).unwrap();
        check_path(&compiled.dae, policy, vec![("u".into(), -1.)], "y", 2.).unwrap();
        let failure =
            SimulationSession::new(&compiled.dae, options(policy, vec![("u".into(), 0.)]))
                .err()
                .expect("the source never assigns y on the zero-input path");
        assert!(
            failure.to_string().contains("`y` is used without a value"),
            "{policy:?}: {failure}"
        );
    }
}

fn options(policy: SimExecutionPolicy, inputs: Vec<(String, f64)>) -> SimOptions {
    SimOptions {
        execution_policy: policy,
        solver_mode: SimSolverMode::RkLike,
        dt: Some(0.05),
        initial_inputs: inputs,
        ..Default::default()
    }
}

fn check_path(
    dae: &Dae,
    policy: SimExecutionPolicy,
    inputs: Vec<(String, f64)>,
    output: &str,
    expected: f64,
) -> Result<(), String> {
    let mut session =
        SimulationSession::new(dae, options(policy, inputs)).map_err(|error| error.to_string())?;
    for time in [0., 0.05] {
        session
            .advance_to(time)
            .map_err(|error| error.to_string())?;
        let state = session.state().map_err(|error| error.to_string())?;
        assert!((state.values[output] - expected).abs() < 1e-10, "{state:?}");
        assert!(
            (state.values["x"] - time * expected).abs() < 1e-10,
            "{state:?}"
        );
    }
    Ok(())
}
