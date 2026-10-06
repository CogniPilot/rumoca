//! A structured initial-equation nest stays one compact owner through Solve
//! (SPEC_0032 §4): its residual is one `Map` node, its Jacobian is that node's
//! tensor JVP, and both views equal the per-point scalar rows exactly; beside
//! scalar rows its Jacobian relation stays exact (SPEC_0039 `Stacked`).

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae_with_diagnostics};

const GRID_SOURCE: &str = r#"
model GridInit
  parameter Integer nx = 4;
  parameter Integer ny = 3;
  parameter Real a = 0.3;
  parameter Real b = 1.7;
  Real s[nx, ny];
initial equation
  for i in 1:nx loop
    for j in 1:ny loop
      s[i, j] = ((i - 0.5) * a - b) * cos(a) - ((j - 0.5) * b) * sin(a);
    end for;
  end for;
equation
  der(s) = -s;
end GridInit;
"#;

fn lowered() -> rumoca_ir_solve::SolveProblem {
    let compiled = Compiler::new()
        .model("GridInit")
        .compile_str(GRID_SOURCE, "GridInit.mo")
        .unwrap_or_else(|error| panic!("GridInit compiles: {error:?}"));
    rumoca_sim::lower_solve_problem(&compiled.dae)
        .unwrap_or_else(|error| panic!("GridInit lowers: {error:?}"))
}

#[test]
fn a_structured_initial_nest_lowers_to_one_map_node() {
    let problem = lowered();
    let residual = problem.initialization.residual();
    let counts = residual.compute_node_counts();
    assert_eq!(counts.map, 1, "the 4 x 3 nest is one Map node");
    let view = rumoca_eval_solve::to_scalar_program_block(residual).expect("scalar view");
    assert_eq!(view.programs().len(), 12);
    // The native backend runs the residual and its Jacobian as one loop
    // kernel each, with no row compiled per point.
    let artifacts = rumoca_sim::lower_solve_artifacts(&problem).expect("artifacts");
    let (residual, jacobian) =
        rumoca_sim::native_initialization_inventory(&problem, &artifacts).expect("native compile");
    let kernel = rumoca_sim::NativeComputeInventory {
        kernels: 1,
        compiled_rows: 0,
    };
    assert_eq!((residual, jacobian), (kernel, kernel));
}

#[test]
fn the_tensor_jvp_of_the_initial_family_equals_the_per_row_jvp() {
    let problem = lowered();
    let residual = problem.initialization.residual();
    let seeds = problem.solve_layout.solver_scalar_count();
    let tensor =
        rumoca_phase_solve::lower_compute_block_full_jvp(residual, seeds).expect("tensor JVP");
    let rows = rumoca_phase_solve::ad::lower_scalar_program_block_full_jvp(
        &rumoca_eval_solve::to_scalar_program_block(residual).expect("scalar view"),
        seeds,
    )
    .expect("per-row JVP");
    let tensor = rumoca_eval_solve::to_scalar_program_block(&tensor).expect("JVP view");
    assert_eq!(tensor.programs(), rows.programs());
    assert_eq!(tensor.output_indices(), rows.output_indices());
}

#[test]
fn the_compact_initial_family_sets_every_start_value() {
    let compiled = Compiler::new()
        .model("GridInit")
        .compile_str(GRID_SOURCE, "GridInit.mo")
        .unwrap_or_else(|error| panic!("GridInit compiles: {error:?}"));
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 0.1,
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("GridInit simulates: {error}"));
    let (a, b) = (0.3f64, 1.7f64);
    for i in 1..=4 {
        for j in 1..=3 {
            let index = result
                .names
                .iter()
                .position(|name| *name == format!("s[{i},{j}]"))
                .expect("s is recorded");
            let expected = ((i as f64 - 0.5) * a - b) * a.cos() - ((j as f64 - 0.5) * b) * a.sin();
            assert!(
                (result.data[index][0] - expected).abs() <= 1e-12 * expected.abs().max(1.0),
                "s[{i},{j}](0) = {} != {expected}",
                result.data[index][0]
            );
        }
    }
}

const MIXED_SOURCE: &str = r#"
model MixedInit
  parameter Integer nx = 4;
  parameter Integer ny = 3;
  parameter Real a = 0.3;
  Real s[nx, ny];
  Real z;
initial equation
  z = 2 * a;
  for i in 1:nx loop
    for j in 1:ny loop
      s[i, j] = (i - 0.5) * a + j;
    end for;
  end for;
equation
  der(s) = -s;
  der(z) = -z;
end MixedInit;
"#;

/// An initialization residual mixing scalar rows with a compact family keeps
/// an exact row relation (SPEC_0039 `Stacked`), so each row's settled read
/// cone stays its own: the relation equals the one its scalar view derives.
#[test]
fn a_mixed_initial_residual_keeps_an_exact_row_relation() {
    let compiled = Compiler::new()
        .model("MixedInit")
        .compile_str(MIXED_SOURCE, "MixedInit.mo")
        .unwrap_or_else(|error| panic!("MixedInit compiles: {error:?}"));
    let problem = rumoca_sim::lower_solve_problem(&compiled.dae)
        .unwrap_or_else(|error| panic!("MixedInit lowers: {error:?}"));
    let counts = problem.initialization.residual().compute_node_counts();
    assert_eq!((counts.scalar_programs, counts.map), (1, 1));
    let artifacts = rumoca_sim::lower_solve_artifacts(&problem).expect("artifacts");
    let pattern = artifacts
        .initialization
        .structural
        .residual()
        .expect("initialization residual pattern")
        .pattern();
    assert!(matches!(
        pattern.view(),
        rumoca_ir_solve::StructuralPatternView::Stacked { .. }
    ));
    let scalar = rumoca_eval_solve::derive_jacobian_pattern_from_scalar_jvp(
        &rumoca_eval_solve::to_scalar_program_block(&artifacts.initialization.residual_jacobian_v)
            .expect("JVP view"),
        pattern.rows() as usize,
        pattern.columns() as usize,
        pattern.provenance().span(),
    )
    .expect("scalar-view relation");
    assert_eq!(pattern.nonzero_coordinates(), scalar.nonzero_coordinates());
}
