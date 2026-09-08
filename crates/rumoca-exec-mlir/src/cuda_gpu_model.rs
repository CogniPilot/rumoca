use crate::compiled::validate_abi_len;
use crate::cuda_driver::{
    CUdeviceptr, CUfunction, CUmodule, CUstream, CudaDriver, EvalDerivativeLaunch,
};
use crate::error::{MlirAbiArgument, MlirError};
use crate::gpu_blob::{
    DerivativeRuntimeInputs, DerivativeRuntimeParts, GpuBlobKind, GpuCompiledBlob,
    PureExplicitDerivativeRuntimeInputs,
};
use crate::options::MlirTarget;
use rumoca_ir_solve::PureExplicitStateCount;
use std::rc::Rc;

/// A single-trajectory ODE model whose derivative function runs on the GPU.
///
/// Wraps the `CudaDriver`, a loaded PTX module, and per-call device buffers.
/// `eval_state_derivatives` copies `y` and `p` to device, launches the kernel,
/// and copies `xdot` back in one blocking call.
pub struct CudaGpuOdeModel {
    driver: Rc<CudaDriver>,
    _module: CUmodule,
    func: CUfunction,
    parameters: Vec<f64>,
    pure_explicit_state: PureExplicitStateCount,
    param_count: usize,
    chip: String,
    pub initial_y: Vec<f64>,
    pub visible_names: Vec<String>,
}

impl CudaGpuOdeModel {
    /// Evaluate `xdot = f(y, p, t)` by launching the GPU kernel.
    ///
    /// `y` is the complete solver-Y vector retained by the compiled artifact,
    /// not only its leading state prefix.
    pub fn eval_state_derivatives(&self, t: f64, y: &[f64]) -> Result<Vec<f64>, MlirError> {
        let state_count = self.pure_explicit_state.get();
        validate_cuda_y(state_count, y)?;
        let drv = &self.driver;
        // Allocate device buffers.
        let d_y = drv.alloc_f64(y.len().max(1))?;
        let d_p = drv.alloc_f64(self.param_count.max(1))?;
        let d_out = drv.alloc_f64(state_count)?;

        drv.copy_h2d(d_y, y)?;
        drv.copy_h2d(d_p, &self.parameters)?;

        drv.launch_eval_derivative(EvalDerivativeLaunch {
            func: self.func,
            d_y,
            len_y: y.len() as i64,
            d_p,
            len_p: self.param_count as i64,
            t,
            d_out,
            len_out: state_count as i64,
            stream: CUstream::null(),
        })?;

        drv.synchronize()?;

        let mut out = vec![0.0f64; state_count];
        drv.copy_d2h(&mut out, d_out)?;

        drv.free(d_y)?;
        drv.free(d_p)?;
        drv.free(d_out)?;
        Ok(out)
    }

    pub fn state_count(&self) -> usize {
        self.pure_explicit_state.get()
    }

    /// Integrate a rectangular batch through this model's retained derivative ABI.
    pub fn batch_euler(
        &self,
        y0_batch: &[Vec<f64>],
        p_batch: &[Vec<f64>],
        dt: f64,
        n_steps: usize,
    ) -> Result<Vec<Vec<f64>>, MlirError> {
        batch_euler_cuda(
            &self.driver,
            self.func,
            BatchEulerInputs {
                y0_batch,
                p_batch,
                dt,
                n_steps,
                abi: self.batch_abi(),
            },
        )
    }

    /// Integrate with a correlated, typed Euler-update blob on the device.
    pub fn batch_euler_device(
        &self,
        update_blob: GpuCompiledBlob,
        y0_batch: &[Vec<f64>],
        p_batch: &[Vec<f64>],
        dt: f64,
        n_steps: usize,
    ) -> Result<Vec<Vec<f64>>, MlirError> {
        let (device_ir, entry_point, target, chip, kind) = update_blob.into_parts();
        if target != MlirTarget::GpuCuda || chip != self.chip {
            return invalid_batch_input(
                "CudaGpuOdeModel::batch_euler_device",
                "Euler-update blob target/chip does not match the derivative model",
            );
        }
        if !matches!(kind, GpuBlobKind::EulerUpdate) {
            return invalid_batch_input(
                "CudaGpuOdeModel::batch_euler_device",
                "blob is not an Euler-update kernel",
            );
        }
        let module = self.driver.load_ptx(device_ir.as_bytes())?;
        let update_func = self.driver.get_function(module, &entry_point)?;
        batch_euler_cuda_device(
            &self.driver,
            self.func,
            update_func,
            BatchEulerInputs {
                y0_batch,
                p_batch,
                dt,
                n_steps,
                abi: self.batch_abi(),
            },
        )
    }

    const fn batch_abi(&self) -> BatchAbi {
        BatchAbi {
            y_count: self.pure_explicit_state.get(),
            parameter_count: self.param_count,
            state_count: self.pure_explicit_state.get(),
        }
    }
}

fn validate_cuda_y(expected: usize, y: &[f64]) -> Result<(), MlirError> {
    validate_abi_len("eval_derivative", MlirAbiArgument::Y, expected, y.len())
}

/// Build a `CudaGpuOdeModel` from an already-compiled `GpuCompiledBlob`.
///
/// `blob` must have been compiled with `MlirTarget::GpuCuda`.  Opens
/// `libcuda.so.1` (fails gracefully when absent), loads the PTX, and resolves
/// the `eval_derivative` entry point.
pub fn build_cuda_ode_model(blob: GpuCompiledBlob) -> Result<CudaGpuOdeModel, MlirError> {
    let (device_ir, entry_point, target, chip, kind) = blob.into_parts();
    if target != MlirTarget::GpuCuda {
        return Err(MlirError::ToolNotFound {
            tool: "build_cuda_ode_model",
            source: std::io::Error::new(
                std::io::ErrorKind::InvalidInput,
                "blob target must be GpuCuda",
            ),
        });
    }

    let GpuBlobKind::SolveDerivative(runtime) = kind else {
        return Err(MlirError::InvalidInput {
            operation: "build_cuda_ode_model",
            message: "blob must carry a Solve derivative launch contract".to_string(),
        });
    };
    let runtime = require_cuda_ode_layout(runtime)?;

    let driver = Rc::new(CudaDriver::new()?);
    let module = driver.load_ptx(device_ir.as_bytes())?;
    let func = driver.get_function(module, &entry_point)?;

    Ok(cuda_model_from_runtime(driver, module, func, runtime, chip))
}

fn require_cuda_ode_layout(
    runtime: DerivativeRuntimeInputs,
) -> Result<PureExplicitDerivativeRuntimeInputs, MlirError> {
    runtime
        .admit_pure_explicit()
        .map_err(|diagnostic| MlirError::InvalidInput {
            operation: "build_cuda_ode_model",
            message: format!(
                "CUDA ODE integration requires a positive pure explicit state-only Y layout: {diagnostic}"
            ),
        })
}

fn cuda_model_from_runtime(
    driver: Rc<CudaDriver>,
    module: CUmodule,
    func: CUfunction,
    runtime: PureExplicitDerivativeRuntimeInputs,
    chip: String,
) -> CudaGpuOdeModel {
    let pure_explicit_state = runtime.state_count();
    let DerivativeRuntimeParts {
        abi,
        parameters,
        initial_y,
        visible_names,
    } = runtime.into_runtime_parts();
    CudaGpuOdeModel {
        driver,
        _module: module,
        func,
        parameters: parameters.into_vec(),
        pure_explicit_state,
        param_count: abi.parameter_count(),
        chip,
        initial_y: initial_y.into_vec(),
        visible_names: visible_names.into_vec(),
    }
}

/// Run a batch of N ODE trajectories (each with its own parameter set) in
/// parallel using one CUDA stream per trajectory.
///
/// `y0_batch[i]` is the initial state vector for trajectory `i`.
/// `p_batch[i]` is the parameter vector for trajectory `i`.
/// `n_steps` fixed-step forward Euler steps of size `dt`.
///
/// Returns `final_y[i]` — the state of each trajectory at `t = dt * n_steps`.
struct BatchEulerInputs<'a> {
    y0_batch: &'a [Vec<f64>],
    p_batch: &'a [Vec<f64>],
    dt: f64,
    n_steps: usize,
    abi: BatchAbi,
}

#[derive(Clone, Copy)]
struct BatchAbi {
    y_count: usize,
    parameter_count: usize,
    state_count: usize,
}

fn batch_euler_cuda(
    driver: &CudaDriver,
    func: CUfunction,
    inputs: BatchEulerInputs<'_>,
) -> Result<Vec<Vec<f64>>, MlirError> {
    let BatchEulerInputs {
        y0_batch,
        p_batch,
        dt,
        n_steps,
        abi,
    } = inputs;
    let BatchShape {
        trajectories: n,
        y: n_y,
        states: n_states,
        parameters: n_params,
    } = validate_batch_inputs("CudaGpuOdeModel::batch_euler", y0_batch, p_batch, abi)?;

    // Allocate per-trajectory device buffers and streams.
    let mut d_y: Vec<CUdeviceptr> = Vec::with_capacity(n);
    let mut d_p: Vec<CUdeviceptr> = Vec::with_capacity(n);
    let mut d_out: Vec<CUdeviceptr> = Vec::with_capacity(n);
    let mut streams: Vec<CUstream> = Vec::with_capacity(n);
    let mut host_y: Vec<Vec<f64>> = y0_batch.to_vec();

    for i in 0..n {
        d_y.push(driver.alloc_f64(n_y.max(1))?);
        d_p.push(driver.alloc_f64(n_params.max(1))?);
        d_out.push(driver.alloc_f64(n_states.max(1))?);
        streams.push(driver.stream_create()?);
        driver.copy_h2d(d_y[i], &host_y[i])?;
        driver.copy_h2d(d_p[i], &p_batch[i])?;
    }

    // Euler integration loop.
    let mut xdot_buf: Vec<Vec<f64>> = vec![vec![0.0; n_states]; n];
    for _step in 0..n_steps {
        // Launch all kernels — each on its own stream (true parallel on GPU).
        let t = _step as f64 * dt;
        for i in 0..n {
            driver.launch_eval_derivative(EvalDerivativeLaunch {
                func,
                d_y: d_y[i],
                len_y: n_y as i64,
                d_p: d_p[i],
                len_p: n_params as i64,
                t,
                d_out: d_out[i],
                len_out: n_states as i64,
                stream: streams[i],
            })?;
        }
        // Synchronize all streams, then update y on host.
        for i in 0..n {
            driver.stream_synchronize(streams[i])?;
            driver.copy_d2h(&mut xdot_buf[i], d_out[i])?;
            for s in 0..n_states {
                host_y[i][s] += dt * xdot_buf[i][s];
            }
            driver.copy_h2d(d_y[i], &host_y[i])?;
        }
    }

    // Free device resources.
    for i in 0..n {
        driver.free(d_y[i])?;
        driver.free(d_p[i])?;
        driver.free(d_out[i])?;
        driver.stream_destroy(streams[i])?;
    }

    Ok(host_y)
}

/// Batch forward Euler — **device-side update** variant.
///
/// Eliminates all per-step host–device round-trips by running both the
/// derivative kernel and the Euler update kernel on device.  Only the final
/// state is copied back to the host.  Expected speedup vs `batch_euler_cuda`:
/// >>10× for large N and many steps.
///
/// `update_func` must be the `euler_update_kernel` compiled by
/// `compile_euler_update_ptx(chip)`.
///
/// Integration loop per step:
///   1. Launch `deriv_func(d_y, d_p, t, d_xdot)` on each stream.
///   2. Launch `euler_update_kernel(d_y, d_xdot, dt, n_states)` on same stream.
///      (No sync between steps — streams serialize automatically within each stream.)
///      After all steps: sync all streams → d2h d_y.
fn batch_euler_cuda_device(
    driver: &CudaDriver,
    deriv_func: CUfunction,
    update_func: CUfunction,
    inputs: BatchEulerInputs<'_>,
) -> Result<Vec<Vec<f64>>, MlirError> {
    let BatchEulerInputs {
        y0_batch,
        p_batch,
        dt,
        n_steps,
        abi,
    } = inputs;
    let BatchShape {
        trajectories: n,
        y: n_y,
        states: n_states,
        parameters: n_params,
    } = validate_batch_inputs(
        "CudaGpuOdeModel::batch_euler_device",
        y0_batch,
        p_batch,
        abi,
    )?;

    // Allocate per-trajectory device buffers and streams.
    let mut d_y: Vec<CUdeviceptr> = Vec::with_capacity(n);
    let mut d_p: Vec<CUdeviceptr> = Vec::with_capacity(n);
    let mut d_xdot: Vec<CUdeviceptr> = Vec::with_capacity(n);
    let mut streams: Vec<CUstream> = Vec::with_capacity(n);

    for i in 0..n {
        let dy = driver.alloc_f64(n_y.max(1))?;
        let dp = driver.alloc_f64(n_params.max(1))?;
        let dx = driver.alloc_f64(n_states.max(1))?;
        let st = driver.stream_create()?;
        driver.copy_h2d(dy, &y0_batch[i])?;
        driver.copy_h2d(dp, &p_batch[i])?;
        d_y.push(dy);
        d_p.push(dp);
        d_xdot.push(dx);
        streams.push(st);
    }

    // Integration loop — no d2h or h2d inside.
    for step in 0..n_steps {
        let t = step as f64 * dt;
        for i in 0..n {
            // Compute derivatives → d_xdot[i]
            driver.launch_eval_derivative(EvalDerivativeLaunch {
                func: deriv_func,
                d_y: d_y[i],
                len_y: n_y as i64,
                d_p: d_p[i],
                len_p: n_params as i64,
                t,
                d_out: d_xdot[i],
                len_out: n_states as i64,
                stream: streams[i],
            })?;
            // Update y[i] += dt * xdot[i] on device (enqueued after deriv on same stream)
            driver.launch_euler_update(
                update_func,
                d_y[i],
                d_xdot[i],
                dt,
                n_states as i64,
                streams[i],
            )?;
        }
    }

    // Sync all streams, copy final y back, free.
    let mut final_y: Vec<Vec<f64>> = vec![vec![0.0; n_y]; n];
    for i in 0..n {
        driver.stream_synchronize(streams[i])?;
        driver.copy_d2h(&mut final_y[i], d_y[i])?;
        driver.free(d_y[i])?;
        driver.free(d_p[i])?;
        driver.free(d_xdot[i])?;
        driver.stream_destroy(streams[i])?;
    }

    Ok(final_y)
}

#[derive(Debug)]
struct BatchShape {
    trajectories: usize,
    y: usize,
    states: usize,
    parameters: usize,
}

fn validate_batch_inputs(
    operation: &'static str,
    y0_batch: &[Vec<f64>],
    p_batch: &[Vec<f64>],
    abi: BatchAbi,
) -> Result<BatchShape, MlirError> {
    let BatchAbi {
        y_count: expected_y,
        parameter_count: expected_parameters,
        state_count: expected_states,
    } = abi;
    let trajectories = y0_batch.len();
    if trajectories == 0 {
        return invalid_batch_input(operation, "batch must contain at least one trajectory");
    }
    if trajectories != p_batch.len() {
        return invalid_batch_input(
            operation,
            format!(
                "y0_batch length {} does not match p_batch length {}",
                trajectories,
                p_batch.len()
            ),
        );
    }

    let states = expected_states;
    let parameters = expected_parameters;
    for (index, y0) in y0_batch.iter().enumerate() {
        if y0.len() != expected_y {
            return invalid_batch_input(
                operation,
                format!(
                    "y0_batch[{index}] length {} does not match derivative ABI Y length {expected_y}",
                    y0.len()
                ),
            );
        }
    }
    for (index, params) in p_batch.iter().enumerate() {
        if params.len() != expected_parameters {
            return invalid_batch_input(
                operation,
                format!(
                    "p_batch[{index}] length {} does not match derivative ABI P length {expected_parameters}",
                    params.len()
                ),
            );
        }
    }

    Ok(BatchShape {
        trajectories,
        y: expected_y,
        states,
        parameters,
    })
}

fn invalid_batch_input<T>(
    operation: &'static str,
    message: impl Into<String>,
) -> Result<T, MlirError> {
    Err(MlirError::InvalidInput {
        operation,
        message: message.into(),
    })
}

#[cfg(test)]
mod tests {
    use super::{
        BatchAbi, BatchShape, require_cuda_ode_layout, validate_batch_inputs, validate_cuda_y,
    };
    use crate::gpu_blob::DerivativeRuntimeInputs;
    use crate::{MlirAbiArgument, MlirError};

    #[test]
    fn cuda_public_y_boundary_refuses_missing_and_surplus_solver_y() {
        for actual in [2, 4] {
            let y = vec![0.0; actual];
            assert!(matches!(
                validate_cuda_y(3, &y),
                Err(MlirError::AbiLengthMismatch {
                    function: "eval_derivative",
                    argument: MlirAbiArgument::Y,
                    expected: 3,
                    actual: error_actual,
                }) if error_actual == actual
            ));
        }
    }

    #[test]
    fn batch_shape_rejects_empty_batches() {
        let err = validate_batch_inputs("test", &[], &[], batch_abi(1, 0, 1))
            .expect_err("empty batch should fail");
        assert!(err.to_string().contains("at least one trajectory"));
    }

    #[test]
    fn batch_shape_rejects_mismatched_batch_lengths() {
        let err = validate_batch_inputs("test", &[vec![1.0]], &[], batch_abi(1, 0, 1))
            .expect_err("mismatched batch lengths should fail");
        assert!(err.to_string().contains("does not match"));
    }

    #[test]
    fn batch_shape_rejects_ragged_states() {
        let err = validate_batch_inputs(
            "test",
            &[vec![1.0], vec![1.0, 2.0]],
            &[vec![], vec![]],
            batch_abi(1, 0, 1),
        )
        .expect_err("ragged state vectors should fail");
        assert!(err.to_string().contains("y0_batch[1]"));
    }

    #[test]
    fn batch_shape_accepts_rectangular_batches() {
        let BatchShape {
            trajectories,
            y,
            states,
            parameters,
        } = validate_batch_inputs(
            "test",
            &[vec![1.0, 2.0], vec![3.0, 4.0]],
            &[vec![5.0], vec![6.0]],
            batch_abi(2, 1, 2),
        )
        .expect("rectangular batch should pass");
        assert_eq!(trajectories, 2);
        assert_eq!(y, 2);
        assert_eq!(states, 2);
        assert_eq!(parameters, 1);
    }

    #[test]
    fn cuda_ode_layout_accepts_only_positive_pure_explicit_state_storage() {
        let layout = |state, algebraic, output, y| rumoca_ir_solve::SolveLayout {
            state_scalar_count: state,
            algebraic_scalar_count: algebraic,
            output_scalar_count: output,
            solver_maps: rumoca_ir_solve::SolverNameIndexMaps {
                names: (0..y).map(|index| format!("y{index}")).collect(),
                ..Default::default()
            },
            ..Default::default()
        };
        require_cuda_ode_layout(DerivativeRuntimeInputs::fixture(&layout(2, 0, 0, 2)))
            .expect("positive pure explicit layout is supported");
        for (state, algebraic, output, y) in
            [(0, 0, 0, 0), (1, 1, 0, 2), (1, 0, 1, 2), (1, 0, 0, 2)]
        {
            let runtime = DerivativeRuntimeInputs::fixture(&layout(state, algebraic, output, y));
            let Err(error) = require_cuda_ode_layout(runtime) else {
                panic!("unsupported CUDA integration layout must fail before loading");
            };
            assert!(matches!(
                error,
                MlirError::InvalidInput {
                    operation: "build_cuda_ode_model",
                    ..
                }
            ));
        }
    }

    const fn batch_abi(y_count: usize, parameter_count: usize, state_count: usize) -> BatchAbi {
        BatchAbi {
            y_count,
            parameter_count,
            state_count,
        }
    }
}
