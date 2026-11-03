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

#[test]
fn ordinary_initialization_reads_relations_at_the_solved_discrete_definition() {
    let dae = compile(
        "model InitialDiscreteRelation
         parameter Real period=1;
         Real width;
         discrete Real deadline(start=-5);
         Real x; Real y;
         equation width=period/2; der(x)=-x; y=if time < deadline+width then 3 else 0;
         when time>0.7 then deadline=time; end when;
         initial algorithm deadline:=0;
         initial equation x=y;
         end InitialDiscreteRelation;",
        "InitialDiscreteRelation",
    );
    let session = SimulationSession::new(
        &dae,
        SimOptions {
            t_end: 0.25,
            ..Default::default()
        },
    )
    .expect("the simultaneous initial solve reads the defined discrete value");
    let state = session.state().unwrap();
    assert_eq!(state.time, 0.0);
    assert_eq!(state.values["x"], 3.0);
    assert_eq!(state.values["y"], 3.0);
}

#[test]
fn structured_boolean_relations_retain_raw_initialization_and_held_transient_truth() {
    let dae = compile(
        "model ArrayRelations Real x[3](each start=1, each fixed=true); Boolean b[3];\n\
         equation der(x)=zeros(3); b={sqrt(x[i])>0 for i in 1:3}; end ArrayRelations;",
        "ArrayRelations",
    );
    let model = super::super::entry::lower_dae_for_simulation(&dae, &SimOptions::default())
        .expect("structured relation wrappers have one proven affine program");
    assert!(!model.problem.discrete.structured_rhs.is_empty());
    let mut y = model.initial_y.to_vec();
    let mut p = model.parameters.to_vec();
    let initial = model
        .problem
        .solve_layout
        .initial_event_parameter_index
        .unwrap();
    let rhs =
        rumoca_eval_solve::to_scalar_program_block(&model.problem.discrete.structured_rhs).unwrap();
    let mut output = vec![0.0; rhs.stored_output_count()];
    for &memory in &model.problem.solve_layout.relation_memory_parameter_indices {
        p[memory] = 0.0;
    }
    p[initial] = 1.0;
    rumoca_eval_solve::eval_scalar_program_block(&rhs, &y, &p, 0.0, None, &mut output)
        .expect("initialization reads raw relations despite false memory seeds");
    assert_eq!(output, vec![1.0; 3]);
    y.fill(-1.0);
    for &memory in &model.problem.solve_layout.relation_memory_parameter_indices {
        p[memory] = 1.0;
    }
    p[initial] = 0.0;
    rumoca_eval_solve::eval_scalar_program_block(&rhs, &y, &p, 0.0, None, &mut output)
        .expect("held truth skips raw sqrt at the negative trial coordinate");
    assert_eq!(output, vec![1.0; 3]);
}

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
