use rumoca::Compiler;
use rumoca_core::VarName;
use rumoca_ir_solve::{LinearOp, ScalarSlot};
use rumoca_sim::{
    SimOptions, SimSolverMode, lower_for_simulation_with_overrides, simulate_dae_with_diagnostics,
};

const SOURCE: &str = r#"
model Arr
  parameter Real a = 1;
  parameter Real arr[3] = {a, 2*a, 3*a};
  Real x[3](each start = 0);
equation
  for i in 1:3 loop
    der(x[i]) = arr[i];
  end for;
end Arr;
"#;

fn compile_arr() -> rumoca::CompilationResult {
    Compiler::new()
        .model("Arr")
        .compile_str(SOURCE, "array_dependent_parameter.mo")
        .expect("Arr should compile")
}

fn p_index(model: &rumoca_ir_solve::SolveModel, name: &str) -> usize {
    match model.problem.layout.binding(name) {
        Some(ScalarSlot::P { index, .. }) => index,
        other => panic!("{name} must have a P slot, got {other:?}"),
    }
}

fn parameter_values(model: &rumoca_ir_solve::SolveModel) -> [f64; 4] {
    ["a", "arr[1]", "arr[2]", "arr[3]"].map(|name| model.parameters[p_index(model, name)])
}

#[test]
fn array_dependent_parameters_preserve_dae_slots_and_derivative_lanes() {
    let compiled = compile_arr();
    let a = compiled
        .dae
        .variables
        .parameters
        .get(&VarName::new("a"))
        .expect("DAE parameter a");
    let arr = compiled
        .dae
        .variables
        .parameters
        .get(&VarName::new("arr"))
        .expect("DAE parameter arr");
    assert!(a.start.is_some(), "DAE parameter a must retain its binding");
    assert_eq!(arr.dims, vec![3], "DAE arr must retain its declared shape");
    let arr_binding = arr.start.as_ref().expect("DAE arr binding");
    let mut binding_refs = Vec::new();
    arr_binding.collect_var_refs(&mut binding_refs);
    assert_eq!(binding_refs, vec![VarName::new("a")]);

    let artifact_opts = SimOptions {
        solver_mode: SimSolverMode::Bdf,
        ..SimOptions::default()
    };
    let prepared = rumoca_sim::structurally_prepared_dae_for_simulation_artifact(
        &compiled.dae,
        &artifact_opts,
    )
    .expect("prepared DAE");
    let boundary =
        rumoca_sim::boundary_reduced_dae_for_simulation_artifact(&compiled.dae, &artifact_opts)
            .expect("boundary-reduced DAE");
    assert_eq!(prepared.continuous.structured_equations.len(), 1);
    assert_eq!(boundary.continuous.structured_equations.len(), 1);
    assert!(
        !boundary.continuous.structured_equations[0].interiors_materialized,
        "boundary elimination must retain the compact family that owns interior lanes"
    );

    let base = lower_for_simulation_with_overrides(&compiled.dae, &SimOptions::default())
        .expect("base solve model");
    assert_eq!(parameter_values(&base), [1.0, 1.0, 2.0, 3.0]);

    let override_opts = SimOptions {
        param_overrides: vec![("a".to_string(), 10.0)],
        ..SimOptions::default()
    };
    let overridden = lower_for_simulation_with_overrides(&compiled.dae, &override_opts)
        .expect("override solve model");
    assert_eq!(parameter_values(&overridden), [10.0, 10.0, 20.0, 30.0]);

    let scalar_rows =
        rumoca_eval_solve::to_scalar_program_block(&base.problem.continuous.derivative_rhs)
            .expect("derivative tensor nodes should have a scalar view");
    assert_eq!(scalar_rows.programs.len(), 3);
    for (lane, program) in scalar_rows.programs.iter().enumerate() {
        let expected = p_index(&base, &format!("arr[{}]", lane + 1));
        let loaded = program
            .iter()
            .filter_map(|op| match op {
                LinearOp::LoadP { index, .. } => Some(*index),
                _ => None,
            })
            .collect::<Vec<_>>();
        assert_eq!(
            loaded,
            vec![expected],
            "derivative lane {} must read arr[{}], nodes={:?}, rows={:?}, outputs={:?}",
            lane + 1,
            lane + 1,
            base.problem.continuous.derivative_rhs.nodes,
            scalar_rows.programs,
            scalar_rows.output_indices,
        );
    }
    for name in ["x[1]", "x[2]", "x[3]"] {
        assert!(
            base.visible_names.iter().any(|candidate| candidate == name),
            "runtime-visible names must contain {name}: {:?}",
            base.visible_names
        );
    }
}

fn final_values(dae: &rumoca_ir_dae::Dae, opts: SimOptions) -> [f64; 3] {
    let sim = simulate_dae_with_diagnostics(dae, &opts).expect("Arr should simulate");
    ["x[1]", "x[2]", "x[3]"].map(|name| {
        let index = sim
            .names
            .iter()
            .position(|candidate| candidate == name)
            .unwrap_or_else(|| panic!("simulation names must contain {name}: {:?}", sim.names));
        sim.data[index]
            .last()
            .copied()
            .expect("simulation output row")
    })
}

#[test]
fn array_dependent_parameter_trajectories_follow_base_and_override() {
    let compiled = compile_arr();
    let base = final_values(
        &compiled.dae,
        SimOptions {
            t_end: 0.5,
            dt: Some(0.01),
            solver_mode: SimSolverMode::Bdf,
            ..SimOptions::default()
        },
    );
    for (actual, expected) in base.into_iter().zip([0.5, 1.0, 1.5]) {
        assert!((actual - expected).abs() < 1.0e-8, "{actual} != {expected}");
    }

    let overridden = final_values(
        &compiled.dae,
        SimOptions {
            t_end: 0.5,
            dt: Some(0.01),
            solver_mode: SimSolverMode::Bdf,
            param_overrides: vec![("a".to_string(), 10.0)],
            ..SimOptions::default()
        },
    );
    for (actual, expected) in overridden.into_iter().zip([5.0, 10.0, 15.0]) {
        assert!((actual - expected).abs() < 1.0e-8, "{actual} != {expected}");
    }
}
