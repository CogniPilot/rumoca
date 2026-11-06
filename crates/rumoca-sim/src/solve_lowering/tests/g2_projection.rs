//! Actual sim-host projection qualification, identical in both source arms.
use super::{SimOptions, SimSolverMode, compile, simulate_dae};

#[test]
fn nonlinear_sparse_projection_trajectory_for_g2() {
    // A sparse nonlinear block with more tears than the existing promotion
    // capacity. BDF needs its algebraic sensitivity to differentiate the state
    // RHS, so that actual host path uses sparse LU instead of torn elimination.
    // The cycle has 2*N structural entries; no policy is overridden.
    const N: usize = 140;
    let mut source = String::from("model G2Projection\n Real x(start=1, fixed=true);\n");
    for i in 0..N {
        source.push_str(&format!(" Real z{i}(start=1);\n"));
    }
    source.push_str("equation\n der(x)=-x+0.01*z0;\n");
    for i in 0..N {
        source.push_str(&format!(
            " z{i}^3 + z{i} + 0.01*(z{0}^3 + z{0}) = 1+x;\n",
            (i + 1) % N
        ));
    }
    source.push_str("end G2Projection;\n");
    let dae = compile(&source, "G2Projection");
    let model = super::lower_dae_for_simulation(&dae, &SimOptions::default()).unwrap();
    let blocks = model
        .problem
        .continuous
        .algebraic_projection_plan
        .blocks
        .scalar_blocks();
    println!(
        "G2_PLAN {}",
        serde_json::json!(
            blocks
                .iter()
                .map(|block| serde_json::json!({
                    "rows": block.rows.len(), "variables": block.y_indices.len(),
                    "tears": block.tearing.as_ref().map(|t| t.tear_y_indices.len()),
                }))
                .collect::<Vec<_>>()
        )
    );
    let structures = model.artifacts.continuous.structural.algebraic_projection();
    let structure = &structures[0];
    let kernel =
        rumoca_eval_solve::tensor_policy::select_linear_solve_kernel(N, structure.pattern())
            .unwrap();
    let elimination = structure.affine_elimination();
    let capacity =
        elimination.and_then(rumoca_eval_solve::projection_policy::affine_elimination_capacity);
    println!(
        "G2_ADMISSION {}",
        serde_json::json!({
            "rows": structure.pattern().rows(),
            "nonzero_upper_bound": structure.pattern().nonzero_upper_bound(),
            "sparse_candidate": kernel == rumoca_eval_solve::tensor_policy::LinearSolveKernel::SparseCandidate,
            "layout_tears": elimination.map(|v| v.tears().len()),
            "elimination_capacity": capacity,
        })
    );
    assert_eq!(
        kernel,
        rumoca_eval_solve::tensor_policy::LinearSolveKernel::SparseCandidate
    );
    assert!(
        capacity.is_none(),
        "the existing elimination policy must decline"
    );
    let options = SimOptions {
        t_end: 0.02,
        dt: Some(0.005),
        solver_mode: SimSolverMode::Bdf,
        ..SimOptions::default()
    };
    let result = simulate_dae(&dae, &options).expect("nonlinear projection simulation succeeds");
    assert_eq!(result.names.len(), N + 1);
    assert!(
        result
            .data
            .iter()
            .all(|column| !column.is_empty() && column.iter().all(|v| v.is_finite()))
    );
    // Machine-readable numerical after-image; the request compares complete
    // labels/times/data, not merely success or a checksum of rounded values.
    println!(
        "G2_TRAJECTORY {}",
        serde_json::json!({
            "names": result.names,
            "time_bits": result.times.iter().map(|value| format!("{:016x}", value.to_bits())).collect::<Vec<_>>(),
            "data_bits": result.data.iter().map(|column| column.iter().map(|value| format!("{:016x}", value.to_bits())).collect::<Vec<_>>()).collect::<Vec<_>>(),
        })
    );
}
