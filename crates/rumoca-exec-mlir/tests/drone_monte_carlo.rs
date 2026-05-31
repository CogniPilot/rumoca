/// Phase 5.4 — Drone Monte Carlo simulation on NVIDIA RTX 3090.
///
/// Planar quadrotor — small-angle linearisation (valid near hover, θ << 1 rad):
///   States  y = [x, y, theta, vx, vy, omega]
///   Params  p = [m, J, F, g]
///
/// Linearised equations (sin θ ≈ θ, cos θ ≈ 1):
///   der(x)     = vx
///   der(y)     = vy
///   der(theta) = omega
///   der(vx)    = -(F/m) * theta
///   der(vy)    = F/m - g
///   der(omega) = 0
///
/// The linearisation removes all trig functions, which NVPTX cannot lower for
/// f64 without CUDA's libdevice.  For near-hover Monte Carlo (theta ~ 0) the
/// linearisation is accurate.
///
/// Monte Carlo: vary mass m ~ U[0.8, 1.2] kg and thrust F ~ U[9.0, 11.0] N
/// across N trajectories.  When F/m > g the drone rises.  We verify:
///   - mean final altitude y[1] is positive (above start)
///   - GPU results match CPU reference for each trajectory to 1e-4
use rumoca_exec_mlir::{
    GpuCompiledBlob, MlirBackendOptions, MlirError, MlirTarget, batch_euler_cuda,
    batch_euler_cuda_device, compile_euler_update_ptx,
    compile_to_gpu_blob as backend_compile_to_gpu_blob, cuda_driver::CudaDriver,
};
use rumoca_ir_solve::{
    BinaryOp,
    ComputeBlock,
    ContinuousSolveSystem,
    DiscreteSolveSystem,
    InitializationSolveSystem,
    LinearOp,
    ScalarProgramBlock,
    SolveClockPartition,
    SolveEventPartition,
    SolveLayout,
    SolveProblem,
    SolverNameIndexMaps,
    UnaryOp, // Neg still used
};

fn compile_to_gpu_blob(
    solve: &SolveProblem,
    model_name: &str,
    opts: &MlirBackendOptions,
) -> Result<GpuCompiledBlob, MlirError> {
    let artifacts =
        rumoca_phase_solve::lower_solve_artifacts(solve).expect("test solve artifacts lower");
    backend_compile_to_gpu_blob(solve, &artifacts, model_name, opts)
}

// ─── Model construction ───────────────────────────────────────────────────────
//
// State indices: x=0, y=1, theta=2, vx=3, vy=4, omega=5
// Param indices: m=0, J=1, F=2, g=3

fn drone_solve() -> SolveProblem {
    use LinearOp::*;

    // Row 0: der(x) = vx   → LoadY[3]
    let row0 = vec![LoadY { dst: 0, index: 3 }, StoreOutput { src: 0 }];

    // Row 1: der(y) = vy   → LoadY[4]
    let row1 = vec![LoadY { dst: 0, index: 4 }, StoreOutput { src: 0 }];

    // Row 2: der(theta) = omega   → LoadY[5]
    let row2 = vec![LoadY { dst: 0, index: 5 }, StoreOutput { src: 0 }];

    // Row 3: der(vx) = -(F/m)*theta   (small-angle: sin θ ≈ θ)
    //   r0 = F, r1 = m, r2 = F/m, r3 = theta, r4 = (F/m)*theta, r5 = -(F/m)*theta
    let row3 = vec![
        LoadP { dst: 0, index: 2 }, // r0 = F
        LoadP { dst: 1, index: 0 }, // r1 = m
        Binary {
            dst: 2,
            op: BinaryOp::Div,
            lhs: 0,
            rhs: 1,
        }, // r2 = F/m
        LoadY { dst: 3, index: 2 }, // r3 = theta
        Binary {
            dst: 4,
            op: BinaryOp::Mul,
            lhs: 2,
            rhs: 3,
        }, // r4 = (F/m)*theta
        Unary {
            dst: 5,
            op: UnaryOp::Neg,
            arg: 4,
        }, // r5 = -(F/m)*theta
        StoreOutput { src: 5 },
    ];

    // Row 4: der(vy) = F/m - g   (small-angle: cos θ ≈ 1)
    //   r0 = F, r1 = m, r2 = F/m, r3 = g, r4 = F/m - g
    let row4 = vec![
        LoadP { dst: 0, index: 2 },
        LoadP { dst: 1, index: 0 },
        Binary {
            dst: 2,
            op: BinaryOp::Div,
            lhs: 0,
            rhs: 1,
        }, // r2 = F/m
        LoadP { dst: 3, index: 3 }, // r3 = g
        Binary {
            dst: 4,
            op: BinaryOp::Sub,
            lhs: 2,
            rhs: 3,
        }, // r4 = F/m - g
        StoreOutput { src: 4 },
    ];

    // Row 5: der(omega) = 0
    let row5 = vec![Const { dst: 0, value: 0.0 }, StoreOutput { src: 0 }];

    SolveProblem::with_derivative_rhs(ComputeBlock::from_scalar_program_block(
        ScalarProgramBlock::new(vec![row0, row1, row2, row3, row4, row5]),
    ))
}

fn drone_prepared_model(m: f64, j: f64, f: f64, g: f64) -> rumoca_ir_solve::SolveModel {
    let solve = drone_solve();
    let zero_rb = ScalarProgramBlock::new(vec![vec![
        LinearOp::Const { dst: 0, value: 0.0 },
        LinearOp::StoreOutput { src: 0 },
    ]]);
    let zero_block = ComputeBlock::from_scalar_program_block(zero_rb.clone());
    let names: Vec<String> = ["x", "y", "theta", "vx", "vy", "omega"]
        .iter()
        .map(|s| s.to_string())
        .collect();

    rumoca_ir_solve::SolveModel {
        problem: SolveProblem {
            schema_version: rumoca_ir_solve::SOLVE_SCHEMA_VERSION,
            layout: rumoca_ir_solve::VarLayout::from_parts(Default::default(), 6, 6),
            continuous: ContinuousSolveSystem {
                implicit_rhs: zero_block.clone(),
                implicit_row_targets: (0..6)
                    .map(|i| Some(rumoca_ir_solve::scalar_slot_y(i)))
                    .collect(),
                residual: zero_rb.clone(),
                derivative_rhs: solve.continuous.derivative_rhs.clone(),
                algebraic_projection_plan: rumoca_ir_solve::AlgebraicProjectionPlan::default(),
            },
            initialization: InitializationSolveSystem {
                residual: zero_rb.clone(),
                row_targets: Vec::new(),
                projection_plan: rumoca_ir_solve::AlgebraicProjectionPlan::default(),
                update_rhs: ScalarProgramBlock::default(),
                update_targets: Vec::new(),
            },
            discrete: DiscreteSolveSystem {
                rhs: zero_rb.clone(),
                ..Default::default()
            },
            events: SolveEventPartition::default(),
            clocks: SolveClockPartition::default(),
            solve_layout: SolveLayout {
                solver_maps: SolverNameIndexMaps {
                    names: names.clone(),
                    name_to_idx: names
                        .iter()
                        .cloned()
                        .enumerate()
                        .map(|(i, n)| (n, i))
                        .collect(),
                    base_to_indices: names
                        .iter()
                        .cloned()
                        .enumerate()
                        .map(|(i, n)| (n, vec![i]))
                        .collect(),
                },
                state_scalar_count: 6,
                algebraic_scalar_count: 0,
                output_scalar_count: 0,
                parameter_count: 4,
                compiled_parameter_len: 4,
                input_scalar_names: Vec::new(),
                discrete_real_scalar_names: Vec::new(),
                discrete_valued_scalar_names: Vec::new(),
                relation_memory_parameter_indices: Vec::new(),
                initial_event_parameter_index: None,
                pre_param_bindings: Vec::new(),
            },
        },
        artifacts: rumoca_ir_solve::SolveArtifacts {
            continuous: rumoca_ir_solve::ContinuousSolveArtifacts {
                mass_matrix: vec![vec![1.0; 6]],
                implicit_jacobian_v: zero_block,
                implicit_jacobian_v_scalar: zero_rb.clone(),
                full_jacobian_v: zero_rb.clone(),
            },
        },
        initial_y: vec![0.0; 6], // start at origin, hover
        parameters: vec![m, j, f, g],
        external_tables: rumoca_ir_solve::ExternalTables::default(),
        visible_names: names,
        visible_value_rows: ScalarProgramBlock::default(),
        variable_meta: Vec::new(),
    }
}

// ─── CPU reference integrator ─────────────────────────────────────────────────

fn cpu_euler(m: f64, f_thrust: f64, g: f64, dt: f64, n_steps: usize) -> Vec<f64> {
    let mut y = vec![0.0f64; 6]; // x, y, theta, vx, vy, omega
    for _s in 0..n_steps {
        let theta = y[2];
        let xdot = [
            y[3],
            y[4],
            y[5],
            -(f_thrust / m) * theta, // linearised: sin θ ≈ θ
            (f_thrust / m) - g,      // linearised: cos θ ≈ 1
            0.0,
        ];
        for i in 0..6 {
            y[i] += dt * xdot[i];
        }
    }
    y
}

// ─── Test ─────────────────────────────────────────────────────────────────────

#[test]
fn drone_monte_carlo_gpu() {
    const N: usize = 100;
    const N_STEPS: usize = 1000;
    const DT: f64 = 1e-3;
    const G: f64 = 9.81;
    const J: f64 = 0.01;

    // Build and compile the drone model for sm_86 (RTX 3090).
    let model = drone_prepared_model(1.0, J, 10.0, G);
    let opts = MlirBackendOptions {
        target: MlirTarget::GpuCuda,
        gpu_chip: Some("sm_86".to_string()),
        ..Default::default()
    };

    let blob = match compile_to_gpu_blob(&model.problem, "drone", &opts) {
        Ok(b) => b,
        Err(MlirError::ToolNotFound { tool, .. }) => {
            eprintln!("SKIP: {tool} not found");
            return;
        }
        Err(e) => panic!("GPU compile failed: {e}"),
    };

    let driver = match CudaDriver::new() {
        Ok(d) => d,
        Err(MlirError::ToolNotFound { tool, .. }) => {
            eprintln!("SKIP: {tool} not available");
            return;
        }
        Err(e) => panic!("CUDA init failed: {e}"),
    };

    let cuda_module = driver.load_ptx(&blob.device_ir).expect("load PTX");
    let func = driver
        .get_function(cuda_module, &blob.entry_point)
        .expect("get function");

    // Generate Monte Carlo parameter samples (deterministic LCG for reproducibility).
    let _ = J; // used in p_batch below
    let mut rng_state: u64 = 0xdeadbeef;
    let mut lcg = || -> f64 {
        rng_state = rng_state
            .wrapping_mul(6364136223846793005)
            .wrapping_add(1442695040888963407);
        (rng_state >> 33) as f64 / (u32::MAX as f64)
    };

    let mut y0_batch: Vec<Vec<f64>> = Vec::with_capacity(N);
    let mut p_batch: Vec<Vec<f64>> = Vec::with_capacity(N);
    let mut m_vals: Vec<f64> = Vec::with_capacity(N);
    let mut f_vals: Vec<f64> = Vec::with_capacity(N);

    for _ in 0..N {
        let m = 0.8 + lcg() * 0.4; // U[0.8, 1.2]
        let f = 9.0 + lcg() * 2.0; // U[9.0, 11.0]
        y0_batch.push(vec![0.0; 6]);
        p_batch.push(vec![m, J, f, G]);
        m_vals.push(m);
        f_vals.push(f);
    }

    // Run batch on GPU.
    let t_start = std::time::Instant::now();
    let gpu_results = batch_euler_cuda(&driver, func, &y0_batch, &p_batch, DT, N_STEPS)
        .expect("batch GPU Euler failed");
    let gpu_elapsed = t_start.elapsed();

    // Run CPU reference (sequential).
    let t_start = std::time::Instant::now();
    let cpu_results: Vec<Vec<f64>> = (0..N)
        .map(|i| cpu_euler(m_vals[i], f_vals[i], G, DT, N_STEPS))
        .collect();
    let cpu_elapsed = t_start.elapsed();

    // Verify GPU vs CPU agreement.
    let mut max_err = 0.0f64;
    for i in 0..N {
        for s in 0..6 {
            let err = (gpu_results[i][s] - cpu_results[i][s]).abs();
            if err > max_err {
                max_err = err;
            }
        }
    }
    assert!(
        max_err < 1e-4,
        "GPU vs CPU max state error {max_err:.2e} exceeds 1e-4"
    );

    // Monte Carlo statistics: mean final altitude (state index 1 = y).
    let mean_alt: f64 = gpu_results.iter().map(|y| y[1]).sum::<f64>() / N as f64;
    let std_alt: f64 = {
        let var = gpu_results
            .iter()
            .map(|y| (y[1] - mean_alt).powi(2))
            .sum::<f64>()
            / N as f64;
        var.sqrt()
    };

    println!(
        "Drone Monte Carlo (N={N}, steps={N_STEPS}, dt={DT}):\n\
         GPU time:  {gpu_elapsed:?}\n\
         CPU time:  {cpu_elapsed:?}\n\
         Mean final altitude: {mean_alt:.4} m\n\
         Std  final altitude: {std_alt:.4} m\n\
         GPU vs CPU max error: {max_err:.2e}"
    );

    // With F ~ U[9,11] and m ~ U[0.8,1.2], F/m > g holds for F/m > 9.81.
    // Mean F = 10, mean m = 1.0 → F/m = 10 > 9.81 → drones net rise.
    assert!(
        mean_alt > 0.0,
        "expected positive mean altitude, got {mean_alt:.4}"
    );
}

/// Phase 6.2 — device-side Euler: verify `batch_euler_cuda_device` produces
/// identical results to `batch_euler_cuda` (host-side update reference).
///
/// The device-side variant eliminates all per-step h2d/d2h round-trips by
/// running the Euler update kernel (`euler_update_kernel`) on device.  Only
/// the final state is copied back.
#[test]
fn drone_monte_carlo_gpu_device_euler() {
    const N: usize = 100;
    const N_STEPS: usize = 1000;
    const DT: f64 = 1e-3;
    const G: f64 = 9.81;
    const J: f64 = 0.01;
    const CHIP: &str = "sm_86";

    let model = drone_prepared_model(1.0, J, 10.0, G);
    let opts = MlirBackendOptions {
        target: MlirTarget::GpuCuda,
        gpu_chip: Some(CHIP.to_string()),
        ..Default::default()
    };

    // Compile derivative kernel.
    let deriv_blob = match compile_to_gpu_blob(&model.problem, "drone", &opts) {
        Ok(b) => b,
        Err(MlirError::ToolNotFound { tool, .. }) => {
            eprintln!("SKIP: {tool} not found");
            return;
        }
        Err(e) => panic!("GPU compile failed: {e}"),
    };

    // Compile device-side Euler update kernel.
    let update_blob = match compile_euler_update_ptx(CHIP) {
        Ok(b) => b,
        Err(MlirError::ToolNotFound { tool, .. }) => {
            eprintln!("SKIP: {tool} not found");
            return;
        }
        Err(e) => panic!("Euler kernel compile failed: {e}"),
    };

    let driver = match CudaDriver::new() {
        Ok(d) => d,
        Err(MlirError::ToolNotFound { tool, .. }) => {
            eprintln!("SKIP: {tool} not available");
            return;
        }
        Err(e) => panic!("CUDA init failed: {e}"),
    };

    let deriv_module = driver
        .load_ptx(&deriv_blob.device_ir)
        .expect("load derivative PTX");
    let deriv_func = driver
        .get_function(deriv_module, &deriv_blob.entry_point)
        .expect("get deriv fn");

    let update_module = driver
        .load_ptx(&update_blob.device_ir)
        .expect("load euler update PTX");
    let update_func = driver
        .get_function(update_module, &update_blob.entry_point)
        .expect("get update fn");

    // Generate deterministic Monte Carlo samples.
    let mut rng_state: u64 = 0xdeadbeef;
    let mut lcg = || -> f64 {
        rng_state = rng_state
            .wrapping_mul(6364136223846793005)
            .wrapping_add(1442695040888963407);
        (rng_state >> 33) as f64 / (u32::MAX as f64)
    };

    let mut y0_batch: Vec<Vec<f64>> = Vec::with_capacity(N);
    let mut p_batch: Vec<Vec<f64>> = Vec::with_capacity(N);

    for _ in 0..N {
        let m = 0.8 + lcg() * 0.4;
        let f = 9.0 + lcg() * 2.0;
        y0_batch.push(vec![0.0; 6]);
        p_batch.push(vec![m, J, f, G]);
    }

    // Host-side reference.
    let ref_results = batch_euler_cuda(&driver, deriv_func, &y0_batch, &p_batch, DT, N_STEPS)
        .expect("host-side batch Euler failed");

    // Device-side update.
    let t_start = std::time::Instant::now();
    let dev_results = batch_euler_cuda_device(
        &driver,
        deriv_func,
        update_func,
        &y0_batch,
        &p_batch,
        DT,
        N_STEPS,
    )
    .expect("device-side batch Euler failed");
    let dev_elapsed = t_start.elapsed();

    // Device results must match host reference to 1e-10 (same f64 arithmetic).
    let mut max_err = 0.0f64;
    for i in 0..N {
        for s in 0..6 {
            let err = (dev_results[i][s] - ref_results[i][s]).abs();
            if err > max_err {
                max_err = err;
            }
        }
    }
    assert!(
        max_err < 1e-10,
        "device Euler vs host Euler max error {max_err:.2e} exceeds 1e-10"
    );

    let mean_alt: f64 = dev_results.iter().map(|y| y[1]).sum::<f64>() / N as f64;
    println!(
        "device-side Euler (N={N}, steps={N_STEPS}, dt={DT}):\n\
         device time: {dev_elapsed:?}\n\
         Mean final altitude: {mean_alt:.4} m\n\
         device vs host max error: {max_err:.2e}"
    );
    assert!(mean_alt > 0.0, "expected positive mean altitude");
}
