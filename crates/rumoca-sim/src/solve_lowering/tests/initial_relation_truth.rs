//! MLS §8.6: relations belong to the simultaneous initialization system.

use super::compile;
use crate::{SimOptions, SimSolverMode, SimulationSession, simulate_dae_with_diagnostics};

const DOMAIN: &str = r#"model RelationInitialization
  Real clock(start=0, fixed=true);
  Real x(start=0, fixed=false);
  Real q;
  Real u;
initial equation
  der(x)=0;
equation
  der(clock)=1;
  der(x)=u-x+q-1;
  u=if clock < 0.5 then 0 else 5;
  q=sqrt(1-x*x);
end RelationInitialization;"#;

fn assert_initial_branch(source: &str) {
    let dae = compile(source, "RelationInitialization");
    for solver_mode in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
        let result = simulate_dae_with_diagnostics(
            &dae,
            &SimOptions {
                solver_mode,
                t_end: 0.01,
                dt: Some(0.01),
                ..Default::default()
            },
        )
        .expect("the current relation selects the regular initialization branch");
        for (name, expected) in [("x", 0.0), ("u", 0.0), ("q", 1.0)] {
            let index = result
                .names
                .iter()
                .position(|candidate| candidate == name)
                .unwrap();
            assert!(
                result.data[index]
                    .iter()
                    .all(|value| (value - expected).abs() < 1e-8)
            );
        }
    }
}

#[test]
fn a_given_state_relation_selects_the_regular_initialization_branch() {
    // Frozen false selects u=5, whose initialization x cannot satisfy q's
    // domain. The exact same starts admit x=0, q=1 when clock<0.5 is current.
    assert_initial_branch(DOMAIN);
    let dae = compile(DOMAIN, "RelationInitialization");
    let mut session = SimulationSession::new(&dae, SimOptions::default()).unwrap();
    session.advance_to(0.25).unwrap();
    assert!((session.get("clock").unwrap().unwrap() - 0.25).abs() < 1e-8);
    session.reset(0.0).unwrap();
    assert_eq!(session.get("clock").unwrap(), Some(0.0));
    assert_eq!(session.get("q").unwrap(), Some(1.0));
}

#[test]
fn a_projected_state_crosses_the_relation_while_initialization_solves() {
    let source = DOMAIN
        .replace("clock(start=0, fixed=true)", "clock(start=1, fixed=false)")
        .replace("  der(x)=0;", "  clock=0;\n  der(x)=0;");
    assert_initial_branch(&source);
}

#[test]
fn an_algebraic_relation_reads_its_current_reconstructed_value() {
    let source = DOMAIN
        .replace("  Real u;", "  Real u;\n  Real measured;")
        .replace("  u=if clock", "  measured=sin(clock);\n  u=if measured");
    assert_initial_branch(&source);
}

#[test]
fn a_model_relation_used_as_a_function_argument_keeps_its_source_context() {
    let source = DOMAIN
        .replace(
            "  Real q;",
            r#"  function choose
    input Boolean low;
    output Real u;
  algorithm
    u := if low then 0 else 5;
  end choose;
  Real q;"#,
        )
        .replace("if clock < 0.5 then 0 else 5", "choose(clock < 0.5)");
    assert_initial_branch(&source);
}

#[test]
fn transient_relation_memory_does_not_execute_an_inactive_pure_call() {
    let source = r#"model UnsafeRelation
  function checked
    input Real u;
    output Real y;
  algorithm
    y := sqrt(u);
  end checked;
  Real x(start=1, fixed=true);
  Boolean b;
equation
  der(x)=0;
  b=checked(x)>0;
end UnsafeRelation;"#;
    let dae = compile(source, "UnsafeRelation");
    let model =
        super::super::entry::lower_dae_for_simulation(&dae, &SimOptions::default()).unwrap();
    let mut y = model.initial_y.to_vec();
    let rumoca_ir_solve::ScalarSlot::Y { index, .. } = model.problem.layout.binding("x").unwrap()
    else {
        panic!("x retains its source state slot")
    };
    y[index] = -1.0;
    let mut p = model.parameters.to_vec();
    p[model.problem.solve_layout.relation_memory_parameter_indices[0]] = 1.0;
    let initial = model
        .problem
        .solve_layout
        .initial_event_parameter_index
        .unwrap();
    let rhs = &model.problem.discrete.rhs;
    let source_target = model.problem.layout.binding("b").unwrap();
    let source_row = model
        .problem
        .discrete
        .update_targets
        .iter()
        .position(|target| *target == source_target)
        .expect("the checked discrete owner retains the source Boolean");
    let mut output = vec![0.0; rhs.stored_output_count()];
    // Deliberately withhold the call table: any executed call must refuse.
    let context = rumoca_eval_solve::RowEvalContext::default();
    p[initial] = 0.0;
    rumoca_eval_solve::eval_scalar_program_block_with_context(
        rhs,
        &y,
        &p,
        0.0,
        context,
        &mut output,
    )
    .expect("held truth skips the raw call");
    assert_eq!(output[source_row], 1.0);
    p[initial] = 1.0;
    rumoca_eval_solve::eval_scalar_program_block_with_context(
        rhs,
        &y,
        &p,
        0.0,
        context,
        &mut output,
    )
    .expect_err("initialization must execute the raw call and require its typed owner");
}
