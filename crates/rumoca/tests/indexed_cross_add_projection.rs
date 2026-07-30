use rumoca_compile::{Session, SessionConfig};
use rumoca_eval_solve::{eval_scalar_program_block, to_scalar_program_block};
use rumoca_phase_solve::lower_solve_problem;
use rumoca_sim::{SimOptions, structurally_prepared_dae_for_simulation_artifact};

#[test]
fn modelica_cross_sum_preserves_each_indexed_component_through_solve_ir() {
    let source = r#"
model IndexedCrossSum
  Real r[3, 4];
  Real f[3, 4];
  Real m[3];
equation
  r = [1, 0, 0, 2; 0, 1, 0, 1; 0, 0, 1, 0];
  f = [0, 0, 1, 1; 1, 0, 0, 0; 0, 1, 0, 3];
  m = cross(r[:, 1], f[:, 1])
    + cross(r[:, 2], f[:, 2])
    + cross(r[:, 3], f[:, 3])
    + cross(r[:, 4], f[:, 4]);
end IndexedCrossSum;
"#;
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("indexed_cross_sum.mo", source)
        .expect("fixture should parse and resolve");
    let dae = session
        .compile_model("IndexedCrossSum")
        .expect("fixture should compile through Modelica to DAE")
        .dae;
    let prepared = structurally_prepared_dae_for_simulation_artifact(&dae, &SimOptions::default())
        .expect("fixture DAE should pass structural preparation");
    let solve = lower_solve_problem(&prepared).expect("prepared DAE should lower to Solve IR");
    let residuals = to_scalar_program_block(&solve.continuous.residual)
        .expect("residual ComputeBlock should have a scalar fallback");
    let mut y = vec![0.0; solve.solve_layout.solver_scalar_count()];
    for (name, value) in [
        ("r[1,1]", 1.0),
        ("r[2,2]", 1.0),
        ("r[3,3]", 1.0),
        ("r[1,4]", 2.0),
        ("r[2,4]", 1.0),
        ("f[2,1]", 1.0),
        ("f[3,2]", 1.0),
        ("f[1,3]", 1.0),
        ("f[1,4]", 1.0),
        ("f[3,4]", 3.0),
    ] {
        let index = solve
            .solve_layout
            .solver_maps
            .names
            .iter()
            .position(|candidate| candidate == name)
            .unwrap_or_else(|| panic!("missing solver slot {name}"));
        y[index] = value;
    }
    let mut outputs = vec![0.0; residuals.output_count()];
    eval_scalar_program_block(&residuals, &y, &[], 0.0, None, &mut outputs)
        .expect("residual scalar programs should evaluate");

    assert!(
        outputs.windows(3).any(|window| window == [-4.0, 5.0, 0.0]),
        "expected the three contact-moment residuals to stay distinct, got {outputs:?}"
    );
}
