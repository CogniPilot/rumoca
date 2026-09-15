use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimSolverMode, simulate_dae_with_diagnostics};

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
