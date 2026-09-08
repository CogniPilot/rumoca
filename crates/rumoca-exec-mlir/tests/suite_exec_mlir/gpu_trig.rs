use rumoca_core::{SourceId, Span};
/// GPU trigonometric evaluation through libdevice.
///
/// Tests the full nonlinear planar quadrotor with actual sin/cos (no small-angle approx).
/// Requires `libdevice.10.bc` (from `nvidia-cuda-toolkit`) and the MLIR/LLVM toolchain.
/// The test skips gracefully when either is absent.
///
/// Nonlinear equations:
///   der(x)     = vx
///   der(y)     = vy
///   der(theta) = omega
///   der(vx)    = -(F/m) * sin(theta)
///   der(vy)    =  (F/m) * cos(theta) - g
///   der(omega) = 0
///
/// At theta=0 the nonlinear model reduces exactly to the linearised one, so we
/// can compare against the linearised CPU reference and expect exact agreement.
use rumoca_exec_mlir::{
    GpuCompiledBlob, MlirBackendOptions, MlirError, MlirTarget,
    compile_to_gpu_blob as backend_compile_to_gpu_blob,
};
use rumoca_ir_solve::{BinaryOp, ComputeBlock, LinearOp, ScalarProgramBlock, UnaryOp};

use super::support::{derivative, fixture};

fn spb(rows: Vec<Vec<LinearOp>>, label: &str) -> ScalarProgramBlock {
    ScalarProgramBlock::with_source_span(
        rows,
        Span::from_offsets(SourceId::from_source_name(label), 0, label.len())
            .require_provenance("MLIR GPU trig fixture")
            .expect("fixture span is source-backed"),
    )
    .expect("fixture program is computable")
}

fn compile_to_gpu_blob(
    solve: std::sync::Arc<rumoca_ir_solve::SolveModel>,
    model_name: &str,
    opts: &MlirBackendOptions,
) -> Result<GpuCompiledBlob, MlirError> {
    backend_compile_to_gpu_blob(solve, model_name, opts)
}

// ─── Full nonlinear drone model ───────────────────────────────────────────────
//
// State indices: x=0, y=1, theta=2, vx=3, vy=4, omega=5
// Param indices: m=0, J=1, F=2, g=3

fn nonlinear_drone_derivative_rhs() -> ComputeBlock {
    use LinearOp::*;

    // Row 0: der(x) = vx
    let row0 = vec![LoadY { dst: 0, index: 3 }, StoreOutput { src: 0 }];

    // Row 1: der(y) = vy
    let row1 = vec![LoadY { dst: 0, index: 4 }, StoreOutput { src: 0 }];

    // Row 2: der(theta) = omega
    let row2 = vec![LoadY { dst: 0, index: 5 }, StoreOutput { src: 0 }];

    // Row 3: der(vx) = -(F/m) * sin(theta)
    //   r0 = F, r1 = m, r2 = F/m, r3 = theta, r4 = sin(theta), r5 = (F/m)*sin(theta), r6 = -(...)
    let row3 = vec![
        LoadP { dst: 0, index: 2 },
        LoadP { dst: 1, index: 0 },
        Binary {
            dst: 2,
            op: BinaryOp::Div,
            lhs: 0,
            rhs: 1,
        },
        LoadY { dst: 3, index: 2 },
        Unary {
            dst: 4,
            op: UnaryOp::Sin,
            arg: 3,
        },
        Binary {
            dst: 5,
            op: BinaryOp::Mul,
            lhs: 2,
            rhs: 4,
        },
        Unary {
            dst: 6,
            op: UnaryOp::Neg,
            arg: 5,
        },
        StoreOutput { src: 6 },
    ];

    // Row 4: der(vy) = (F/m)*cos(theta) - g
    //   r0 = F, r1 = m, r2 = F/m, r3 = theta, r4 = cos(theta), r5 = (F/m)*cos(theta), r6 = g, r7 = r5-g
    let row4 = vec![
        LoadP { dst: 0, index: 2 },
        LoadP { dst: 1, index: 0 },
        Binary {
            dst: 2,
            op: BinaryOp::Div,
            lhs: 0,
            rhs: 1,
        },
        LoadY { dst: 3, index: 2 },
        Unary {
            dst: 4,
            op: UnaryOp::Cos,
            arg: 3,
        },
        Binary {
            dst: 5,
            op: BinaryOp::Mul,
            lhs: 2,
            rhs: 4,
        },
        LoadP { dst: 6, index: 3 },
        Binary {
            dst: 7,
            op: BinaryOp::Sub,
            lhs: 5,
            rhs: 6,
        },
        StoreOutput { src: 7 },
    ];

    // Row 5: der(omega) = 0
    let row5 = vec![Const { dst: 0, value: 0.0 }, StoreOutput { src: 0 }];

    ComputeBlock::from_scalar_program_block(spb(
        vec![row0, row1, row2, row3, row4, row5],
        "gpu_trig_nonlinear_drone.mo",
    ))
}

fn nonlinear_drone_prepared(
    m: f64,
    j: f64,
    f: f64,
    g: f64,
) -> std::sync::Arc<rumoca_ir_solve::SolveModel> {
    let problem = derivative::derivative_problem(
        fixture::ContinuousInventory::new(vec![
            fixture::FixtureScalar::state("x", 0.0, 1.0),
            fixture::FixtureScalar::state("y", 0.0, 1.0),
            fixture::FixtureScalar::state("theta", 0.0, 1.0),
            fixture::FixtureScalar::state("vx", 0.0, 1.0),
            fixture::FixtureScalar::state("vy", 0.0, 1.0),
            fixture::FixtureScalar::state("omega", 0.0, 1.0),
            fixture::FixtureScalar::parameter("m", m),
            fixture::FixtureScalar::parameter("J", j),
            fixture::FixtureScalar::parameter("F", f),
            fixture::FixtureScalar::parameter("g", g),
        ]),
        nonlinear_drone_derivative_rhs(),
    );
    fixture::complete_model(problem)
}

// ─── Tests ────────────────────────────────────────────────────────────────────

/// Verify the nonlinear drone MLIR template renders (includes sin/cos ops) and
/// that the GPU PTX compilation either succeeds (with libdevice) or fails with
/// a clear `LibdeviceNotFound` error (without it).  Never panics on tool absence.
#[test]
fn nonlinear_drone_ptx_or_libdevice_error() {
    let model = nonlinear_drone_prepared(1.0, 0.01, 10.0, 9.81);
    let opts = MlirBackendOptions {
        target: MlirTarget::GpuCuda,
        gpu_chip: Some("sm_86".to_string()),
        ..Default::default()
    };

    match backend_compile_to_gpu_blob(model, "nonlinear_drone", &opts) {
        Ok(blob) => {
            // Libdevice found and linked — PTX should contain the kernel entry.
            let ptx = blob.device_ir_text();
            assert!(
                ptx.contains(".visible .entry") || ptx.contains(".entry"),
                "PTX missing kernel entry:\n{ptx}"
            );
            eprintln!(
                "PTX compiled with libdevice ({} bytes)",
                blob.device_ir_text().len()
            );
        }
        Err(MlirError::LibdeviceNotFound { searched }) => {
            eprintln!(
                "SKIP: libdevice not found (expected without nvidia-cuda-toolkit).\nSearched: {searched}"
            );
        }
        Err(MlirError::ToolNotFound { tool, .. }) => {
            eprintln!("SKIP: {tool} not on PATH");
        }
        Err(e) => panic!("Unexpected GPU compile error: {e}"),
    }
}

/// When the user provides a non-existent explicit libdevice path the error is
/// `LibdeviceNotFound`, not a generic IO error or panic.
#[test]
fn explicit_bad_libdevice_path_gives_clear_error() {
    let model = nonlinear_drone_prepared(1.0, 0.01, 10.0, 9.81);
    let opts = MlirBackendOptions {
        target: MlirTarget::GpuCuda,
        gpu_chip: Some("sm_86".to_string()),
        libdevice_path: Some(std::path::PathBuf::from("/nonexistent/libdevice.10.bc")),
        ..Default::default()
    };

    match backend_compile_to_gpu_blob(model, "nonlinear_drone", &opts) {
        Err(MlirError::LibdeviceNotFound { searched }) => {
            assert!(
                searched.contains("nonexistent"),
                "Error should name the searched path: {searched}"
            );
        }
        Err(MlirError::ToolNotFound { tool, .. }) => {
            eprintln!("SKIP: {tool} not on PATH (libdevice path check is pre-compile)");
        }
        Ok(_) => panic!("Should not succeed with a bad libdevice path"),
        Err(e) => panic!("Unexpected error variant: {e}"),
    }
}

fn linear_drone_prepared() -> std::sync::Arc<rumoca_ir_solve::SolveModel> {
    use LinearOp::*;

    // Same model as drone_monte_carlo.rs but inline — no sin/cos
    let row_vx = vec![
        LoadP { dst: 0, index: 2 },
        LoadP { dst: 1, index: 0 },
        Binary {
            dst: 2,
            op: BinaryOp::Div,
            lhs: 0,
            rhs: 1,
        },
        LoadY { dst: 3, index: 2 },
        Binary {
            dst: 4,
            op: BinaryOp::Mul,
            lhs: 2,
            rhs: 3,
        },
        Unary {
            dst: 5,
            op: UnaryOp::Neg,
            arg: 4,
        },
        StoreOutput { src: 5 },
    ];
    let row_vy = vec![
        LoadP { dst: 0, index: 2 },
        LoadP { dst: 1, index: 0 },
        Binary {
            dst: 2,
            op: BinaryOp::Div,
            lhs: 0,
            rhs: 1,
        },
        LoadP { dst: 3, index: 3 },
        Binary {
            dst: 4,
            op: BinaryOp::Sub,
            lhs: 2,
            rhs: 3,
        },
        StoreOutput { src: 4 },
    ];

    let solve = derivative::derivative_problem(
        fixture::ContinuousInventory::new(vec![
            fixture::FixtureScalar::state("x", 0.0, 1.0),
            fixture::FixtureScalar::state("y", 0.0, 1.0),
            fixture::FixtureScalar::state("theta", 0.0, 1.0),
            fixture::FixtureScalar::state("vx", 0.0, 1.0),
            fixture::FixtureScalar::state("vy", 0.0, 1.0),
            fixture::FixtureScalar::state("omega", 0.0, 1.0),
            fixture::FixtureScalar::parameter("m", 1.0),
            fixture::FixtureScalar::parameter("J", 0.01),
            fixture::FixtureScalar::parameter("F", 10.0),
            fixture::FixtureScalar::parameter("g", 9.81),
        ]),
        ComputeBlock::from_scalar_program_block(spb(
            vec![
                vec![LoadY { dst: 0, index: 3 }, StoreOutput { src: 0 }],
                vec![LoadY { dst: 0, index: 4 }, StoreOutput { src: 0 }],
                vec![LoadY { dst: 0, index: 5 }, StoreOutput { src: 0 }],
                row_vx,
                row_vy,
                vec![
                    LinearOp::Const { dst: 0, value: 0.0 },
                    StoreOutput { src: 0 },
                ],
            ],
            "gpu_trig_linear_reference.mo",
        )),
    );
    fixture::complete_model(solve)
}

/// Linearised model (no trig) should compile to PTX without libdevice.
/// This guards that the libdevice detection (`ll_needs_libdevice`) correctly
/// identifies trig-free IR.
#[test]
fn linear_drone_ptx_no_libdevice_needed() {
    let solve = linear_drone_prepared();

    let opts = MlirBackendOptions {
        target: MlirTarget::GpuCuda,
        gpu_chip: Some("sm_86".to_string()),
        ..Default::default()
    };

    match compile_to_gpu_blob(solve, "linear_drone", &opts) {
        Ok(blob) => {
            let ptx = blob.device_ir_text();
            assert!(
                ptx.contains(".entry") || ptx.contains(".visible"),
                "PTX missing kernel entry"
            );
            // No libdevice calls should appear in a trig-free model.
            assert!(
                !ptx.contains("__nv_sin") && !ptx.contains("__nv_cos"),
                "Trig-free model should not reference libdevice trig symbols"
            );
        }
        Err(MlirError::ToolNotFound { tool, .. }) => {
            eprintln!("SKIP: {tool} not on PATH");
        }
        Err(MlirError::LibdeviceNotFound { .. }) => {
            panic!("Linear model should not need libdevice");
        }
        Err(e) => panic!("Unexpected error: {e}"),
    }
}
