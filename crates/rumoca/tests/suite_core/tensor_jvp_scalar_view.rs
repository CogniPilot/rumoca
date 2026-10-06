//! The scalar Jacobian artifacts are the scalar view of the tensor JVP: a
//! stencil's base program is differentiated once, and its view equals
//! differentiating every scalar row of the primal view (SPEC_0032 §4).

use rumoca::Compiler;

const STENCIL_SOURCE: &str = r#"
model StencilJvp
  parameter Integer n = 7;
  parameter Real k = 0.4;
  parameter Real c = 1.3;
  Real u[n, n](each start = 0.1);
  Real w[n, n](each start = 0.2);
equation
  for i in 1:n loop
    der(u[i, 1]) = -u[i, 1];
    der(u[i, n]) = -u[i, n];
    der(w[i, 1]) = 0.0;
    der(w[i, n]) = 0.0;
  end for;
  for j in 2:n - 1 loop
    der(u[1, j]) = -u[1, j];
    der(u[n, j]) = -u[n, j];
    der(w[1, j]) = 0.0;
    der(w[n, j]) = 0.0;
  end for;
  for i in 2:n - 1 loop
    for j in 2:n - 1 loop
      der(u[i, j]) = k * (u[i + 1, j] - 2.0 * u[i, j] + u[i - 1, j])
        - tanh(abs(w[i, j]) - sqrt(max(u[i, j - 1], 0.0) ^ 2 + c ^ 2))
        * noEvent(if u[i, j + 1] < c then u[i, j + 1] ^ 3 else c * u[i, j + 1]);
      der(w[i, j]) = c * (w[i, j + 1] - w[i, j - 1]) / k - exp(-u[i, j]);
    end for;
  end for;
end StencilJvp;
"#;

#[test]
fn the_derivative_jacobian_rows_are_the_view_of_the_tensor_jvp() {
    let compiled = Compiler::new()
        .model("StencilJvp")
        .compile_str(STENCIL_SOURCE, "StencilJvp.mo")
        .unwrap_or_else(|error| panic!("StencilJvp compiles: {error:?}"));
    let problem = rumoca_sim::lower_solve_problem(&compiled.dae)
        .unwrap_or_else(|error| panic!("StencilJvp lowers: {error:?}"));
    let block = &problem.continuous.derivative_rhs;
    let counts = block.compute_node_counts();
    assert!(
        counts.map + counts.affine_stencil >= 2,
        "the grid keeps compact tensor nodes: {counts:?}"
    );
    let native = rumoca_sim::native_compute_inventory(block).expect("native compile");
    assert_eq!(native.kernels, counts.map + counts.affine_stencil);
    let primal = rumoca_eval_solve::to_scalar_program_block(block).expect("primal view");
    let per_row = rumoca_phase_solve::lower_scalar_program_block_full_ad_with_spans(
        primal.programs(),
        primal.program_spans(),
        &problem.layout,
    )
    .expect("per-row JVP");
    let tensor =
        rumoca_phase_solve::lower_compute_block_full_jvp(block, problem.layout.y_scalars())
            .expect("tensor JVP");
    let view = rumoca_eval_solve::to_scalar_program_block(&tensor).expect("JVP view");
    assert_eq!(view.programs(), per_row.as_slice());
    assert_eq!(view.output_indices(), primal.output_indices());
    assert_eq!(view.program_spans(), primal.program_spans());
}

/// The grid's derivative block holds several compact nodes (boundary
/// families and interior stencils). The structural relation of its tensor JVP stacks the
/// nodes' exact relations (SPEC_0039 `Stacked`) and equals, entry for entry,
/// the relation derived from the JVP's scalar view.
#[test]
fn the_mixed_derivative_jacobian_relation_equals_its_scalar_view() {
    let compiled = Compiler::new()
        .model("StencilJvp")
        .compile_str(STENCIL_SOURCE, "StencilJvp.mo")
        .unwrap_or_else(|error| panic!("StencilJvp compiles: {error:?}"));
    let problem = rumoca_sim::lower_solve_problem(&compiled.dae)
        .unwrap_or_else(|error| panic!("StencilJvp lowers: {error:?}"));
    let block = &problem.continuous.derivative_rhs;
    let counts = block.compute_node_counts();
    assert!(
        counts.scalar_programs + counts.map + counts.affine_stencil >= 2,
        "the derivative block holds several nodes: {counts:?}"
    );
    let jvp = rumoca_phase_solve::lower_compute_block_full_jvp(block, problem.layout.y_scalars())
        .expect("tensor JVP");
    let view = rumoca_eval_solve::to_scalar_program_block(&jvp).expect("JVP view");
    let rows = view.output_count();
    let columns = problem.layout.y_scalars() + problem.layout.p_scalars();
    let span = view.first_source_span().expect("source-backed JVP");
    let pattern = rumoca_eval_solve::derive_jacobian_pattern_from_jvp(&jvp, rows, columns, span)
        .expect("compact relation");
    assert!(matches!(
        pattern.view(),
        rumoca_ir_solve::StructuralPatternView::Stacked { .. }
    ));
    let scalar =
        rumoca_eval_solve::derive_jacobian_pattern_from_scalar_jvp(&view, rows, columns, span)
            .expect("scalar-view relation");
    assert_eq!(pattern.nonzero_coordinates(), scalar.nonzero_coordinates());
    assert_eq!(pattern.column_coloring(), scalar.column_coloring());
}
