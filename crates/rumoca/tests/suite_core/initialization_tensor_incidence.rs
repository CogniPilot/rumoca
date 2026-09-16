use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimSolverMode, simulate_dae_with_diagnostics};

#[test]
fn a_demoted_observation_retains_its_fixed_initial_equation() {
    let source = include_str!("../fixtures/index_reduction/ProjectedInitialCoordinate.mo");
    check_trace(
        source,
        "ProjectedInitialCoordinate",
        &[
            ("x[1]", -3.0),
            ("x[2]", 2.0),
            ("u", 1.0),
            ("pin", 1.0),
            ("second", 2.0),
            ("rate", -1.0),
        ],
    );
    let compiled = Compiler::new()
        .model("ProjectedInitialCoordinate")
        .compile_str(source, "ProjectedInitialCoordinate.mo")
        .unwrap();
    let prepared = rumoca_phase_structural::prepare_for_solve(&compiled.dae).unwrap();
    prepared.as_dae().inspect(|view| {
        let states = view
            .variables()
            .filter(|(_, variable)| variable.role() == rumoca_ir_dae::VariableRole::State);
        assert_eq!(
            states
                .map(|(_, variable)| variable.scalar_count())
                .sum::<usize>(),
            2,
            "the fixed observation must constrain x without adding a redundant integrated state"
        );
    });
}

#[test]
fn a_demoted_observation_cannot_hide_conflicting_fixed_states() {
    let source = include_str!("../fixtures/index_reduction/ProjectedInitialCoordinate.mo")
        .replace("each start=0", "each start=0, each fixed=true");
    let compiled = Compiler::new()
        .model("ProjectedInitialCoordinate")
        .compile_str(&source, "ProjectedInitialCoordinate.mo")
        .unwrap();
    for solver_mode in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
        let error = simulate_dae_with_diagnostics(
            &compiled.dae,
            &SimOptions {
                solver_mode,
                t_end: 0.05,
                ..Default::default()
            },
        )
        .expect_err("x=0 cannot satisfy pin=1 and second=2 at initialization");
        assert!(error.to_string().contains("initial"), "{error}");
    }
}

#[test]
fn a_direct_reduction_cannot_discard_a_conflicting_initial_equation() {
    for value in ["time", "5"] {
        let source = format!(
            "model ConflictingInitial Real x(start=1,fixed=true); Real v;
             equation x={value}; der(x)=v; end ConflictingInitial;"
        );
        let compiled = Compiler::new()
            .model("ConflictingInitial")
            .compile_str(&source, "conflicting_initial.mo")
            .unwrap();
        for solver_mode in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
            let error = simulate_dae_with_diagnostics(
                &compiled.dae,
                &SimOptions {
                    solver_mode,
                    t_end: 0.05,
                    ..Default::default()
                },
            )
            .expect_err("the continuous value contradicts x(0)=1");
            assert!(error.to_string().contains("initial"), "{error}");
        }
    }
}

#[test]
fn partial_array_pin_transfer_retains_unprojected_initial_equations() {
    for (selected, values) in [(1, "x,0,0"), (2, "0,2*x,0"), (3, "0,0,3*x")] {
        reject_partial_array_pin(&partial_array_pin_source(values, selected));
    }
}

fn reject_partial_array_pin(source: &str) {
    let compiled = Compiler::new()
        .model("PartialPin")
        .compile_str(source, "partial_pin.mo")
        .unwrap();
    for solver_mode in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
        let error = simulate_dae_with_diagnostics(
            &compiled.dae,
            &SimOptions {
                t_end: 0.05,
                solver_mode,
                ..Default::default()
            },
        )
        .expect_err(
            "unprojected elements contradict their fixed starts even when the selected element agrees",
        );
        assert!(
            error.to_string().contains("initial variable projection"),
            "{error}"
        );
    }
}

#[test]
fn partial_array_pin_transfer_preserves_consistent_initialization() {
    for selected in 1..=3 {
        check_trace(
            &partial_array_pin_source("x,2*x,3*x", selected),
            "PartialPin",
            &[
                ("x", 1.0),
                ("q", f64::from(selected)),
                ("a[1]", 1.0),
                ("a[2]", 2.0),
                ("a[3]", 3.0),
            ],
        );
    }
}

fn partial_array_pin_source(values: &str, selected: u32) -> String {
    format!(
        "model PartialPin Real x(start=1,fixed=true);
        Real a[3](start={{1,2,3}},each fixed=true); Real q,rate;
        equation der(x)=-x; a={{{values}}}; q=a[{selected}]; der(q)=rate;
        end PartialPin;"
    )
}

#[test]
fn computed_fixed_array_attributes_constrain_algebraic_initialization() {
    let source = r#"
model FixedAlgebraicArray
  parameter Boolean pinned = true;
  Real x[3](each start=0);
  Real y[3](start={1,2,3}, fixed=fill(pinned,3));
equation
  der(x) = -x;
  y = 2*x;
end FixedAlgebraicArray;
"#;
    check_trace(
        source,
        "FixedAlgebraicArray",
        &[
            ("x[1]", 0.5),
            ("x[2]", 1.0),
            ("x[3]", 1.5),
            ("y[1]", 1.0),
            ("y[2]", 2.0),
            ("y[3]", 3.0),
        ],
    );
}

#[test]
fn matrix_initial_equations_determine_each_state_coordinate() {
    check_matrix_initialization("x = {{1,2},{3,4}};");
}

#[test]
fn structured_initial_equations_preserve_their_index_coordinates() {
    check_matrix_initialization(
        "for i in 1:2 loop\n  for j in 1:2 loop\n    x[i,j] = 2*(i-1)+j;\n  end for;\nend for;",
    );
}

#[test]
fn matrix_derivative_initial_equations_follow_the_matched_scalar_rows() {
    check_matrix_initialization("der(x) = -{{1,2},{3,4}};");
}

#[test]
fn array_parameter_substitutions_preserve_permuted_scalar_dependencies() {
    let source = r#"
model PermutedParameters
  parameter Real p[2](each fixed=false);
  parameter Real q[2] = {p[2]+1,p[1]-1};
  Real x[2](start={-10,-20});
initial equation
  p = {3,4};
  x = q;
equation
  der(x) = -x;
end PermutedParameters;
"#;
    check_trace(
        source,
        "PermutedParameters",
        &[("x[1]", 5.0), ("x[2]", 2.0)],
    );
}

#[test]
fn a_fixed_matrix_coordinate_cannot_be_moved_to_satisfy_an_initial_equation() {
    let source = r#"
model ContradictoryMatrix
  Real x[2,2](start={{1,2},{3,4}},each fixed=true);
initial equation
  x[1,2] = 99;
equation
  der(x) = -x;
end ContradictoryMatrix;
"#;
    let compiled = Compiler::new()
        .model("ContradictoryMatrix")
        .compile_str(source, "contradictory_matrix.mo")
        .expect("the initial contradiction is independent of DAE construction");
    for solver_mode in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
        let error = simulate_dae_with_diagnostics(
            &compiled.dae,
            &SimOptions {
                solver_mode,
                ..Default::default()
            },
        )
        .expect_err("the declaration pins x[1,2] to 2, so 99 cannot be certified");
        assert!(
            error.to_string().contains("initial variable projection"),
            "{error}"
        );
    }
}

#[test]
fn source_initial_equations_follow_array_algebraics_to_independent_states() {
    let compiled = Compiler::new()
        .model("InitArrayAlias")
        .compile_str(
            include_str!("../fixtures/index_reduction/InitArrayAlias.mo"),
            "InitArrayAlias.mo",
        )
        .unwrap();
    let model =
        rumoca_sim::lower_dae_for_simulation(&compiled.dae, &SimOptions::default()).unwrap();
    let initial = &model.problem.initialization;
    assert_eq!(initial.projection_plan().blocks.len(), 2);
    for (row, block) in initial.projection_plan().blocks.iter().enumerate() {
        assert_eq!(block.rows, [row]);
        assert_eq!(block.unknowns, [rumoca_ir_solve::scalar_slot_y(row)]);
    }
    assert!(
        initial
            .row_roles()
            .iter()
            .all(|role| *role
                == rumoca_ir_solve::InitializationRowRole::SolvedThroughAlgebraicRefresh)
    );
    for solver_mode in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
        let trace = simulate_dae_with_diagnostics(
            &compiled.dae,
            &SimOptions {
                solver_mode,
                t_end: 0.05,
                dt: Some(0.01),
                ..Default::default()
            },
        )
        .unwrap();
        for (name, slope, offset) in [
            ("z[1]", -2.4, 0.0),
            ("q[2]", -2.4, 0.0),
            ("z[2]", 0.0, -2.4),
            ("w[2]", 0.0, -2.4),
        ] {
            let column = trace.names.iter().position(|value| value == name).unwrap();
            for (&time, &actual) in trace.times.iter().zip(&trace.data[column]) {
                assert!(
                    (actual - (slope * time + offset)).abs() < 1e-8,
                    "{solver_mode:?} {name} at {time}: {actual}"
                );
            }
        }
    }
}

#[test]
fn initial_algebraic_cycles_reach_only_their_external_tensor_coordinates() {
    check_trace(
        r#"
model CoupledInitialArray
  Real x[2]; Real a[2], b[2];
initial equation
  a={2,6};
equation
  der(x)=-x;
  a=2*b+{x[2],x[1]};
  b=0.25*a;
end CoupledInitialArray;
"#,
        "CoupledInitialArray",
        &[("x[1]", 3.0), ("x[2]", 1.0), ("a[1]", 2.0), ("a[2]", 6.0)],
    );
}

#[test]
fn initial_algebraic_dependencies_retain_matched_family_domain_points() {
    check_trace(
        r#"
model StructuredInitialAlias
  Real x[2]; Real a[2];
initial equation
  for i in 1:2 loop
    a[i]=2*i;
  end for;
equation
  der(x)=-x;
  for i in 1:2 loop
    a[i]=2*x[3-i];
  end for;
end StructuredInitialAlias;
"#,
        "StructuredInitialAlias",
        &[("x[1]", 2.0), ("x[2]", 1.0), ("a[1]", 2.0), ("a[2]", 4.0)],
    );
}

#[test]
fn an_algebraic_initial_equation_solves_a_parameter_through_its_bound_dependent() {
    check_trace(
        r#"
model InitialAlgebraicParameter
  parameter Real p(start=1, fixed=false);
  parameter Real q=2*p;
  Real x(start=4, fixed=true); Real a;
initial equation
  a=24;
equation
  der(x)=-x;
  a=q*x;
end InitialAlgebraicParameter;
"#,
        "InitialAlgebraicParameter",
        &[("x", 4.0), ("a", 24.0)],
    );
}

#[test]
fn an_algebraic_initial_equation_cannot_override_a_fixed_state() {
    let source = include_str!("../fixtures/index_reduction/InitArrayAlias.mo")
        .replace("each fixed=false", "each start=0, each fixed=true");
    let compiled = Compiler::new()
        .model("InitArrayAlias")
        .compile_str(&source, "InitArrayAlias.mo")
        .unwrap();
    for solver_mode in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
        let error = simulate_dae_with_diagnostics(
            &compiled.dae,
            &SimOptions {
                solver_mode,
                ..Default::default()
            },
        )
        .unwrap_err();
        assert!(
            error.to_string().contains("initial variable projection"),
            "{error}"
        );
    }
}

#[test]
fn an_unowned_algebraic_dependency_cannot_certify_its_zero_seed() {
    let source = r#"
model DiscreteInitialAlgebraic
  Real x(start=0, fixed=false);
  Real a(start=0);
  discrete Real d(start=2, fixed=true);
initial equation
  x=a;
equation
  der(x)=0;
  a=2*d;
  when time>1 then d=3; end when;
end DiscreteInitialAlgebraic;
"#;
    let compiled = Compiler::new()
        .model("DiscreteInitialAlgebraic")
        .compile_str(source, "DiscreteInitialAlgebraic.mo")
        .unwrap();
    for solver_mode in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
        let error = simulate_dae_with_diagnostics(
            &compiled.dae,
            &SimOptions {
                solver_mode,
                t_end: 0.05,
                ..Default::default()
            },
        )
        .expect_err("unowned x=a must observe a=4, never the zero declaration seed");
        assert!(
            error.to_string().contains("initial variable projection"),
            "{error}"
        );
    }
}

fn check_matrix_initialization(initial_equations: &str) {
    // MLS §8.6 solves every initial equation, including derivatives, over
    // the scalar coordinates of the compact matrix declaration.
    let source = format!(
        "model MatrixInitialization\n  Real x[2,2](start=fill(-1,2,2));\ninitial equation\n  {initial_equations}\nequation\n  der(x) = -x;\nend MatrixInitialization;"
    );
    check_trace(
        &source,
        "MatrixInitialization",
        &[
            ("x[1,1]", 1.0),
            ("x[1,2]", 2.0),
            ("x[2,1]", 3.0),
            ("x[2,2]", 4.0),
        ],
    );
}

fn check_trace(source: &str, model: &str, channels: &[(&str, f64)]) {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(source, "tensor_initialization.mo")
        .expect("tensor initialization constructs checked DAE");
    for solver_mode in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
        let result = simulate_dae_with_diagnostics(
            &compiled.dae,
            &SimOptions {
                t_end: 0.05,
                dt: Some(0.01),
                solver_mode,
                ..Default::default()
            },
        )
        .unwrap_or_else(|error| panic!("matrix initialization on {solver_mode:?}: {error}"));
        for &(name, initial) in channels {
            let channel = result.names.iter().position(|value| value == name).unwrap();
            for (&time, &actual) in result.times.iter().zip(&result.data[channel]) {
                let expected = initial * (-time).exp();
                assert!(
                    (actual - expected).abs() < 1.0e-6,
                    "{name} at {time} on {solver_mode:?}: {actual} != {expected}"
                );
            }
        }
    }
}
