use crate::cuda_driver::{
    CUdeviceptr, CUfunction, CUmodule, CUstream, CudaDriver, EvalDerivativeLaunch,
};
use crate::error::MlirError;
use crate::gpu_blob::GpuCompiledBlob;
use crate::options::MlirTarget;
use rumoca_ir_solve::SolveModel;
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
    state_count: usize,
    param_count: usize,
    pub initial_y: Vec<f64>,
    pub visible_names: Vec<String>,
}

impl CudaGpuOdeModel {
    /// Evaluate `xdot = f(y, p, t)` by launching the GPU kernel.
    pub fn eval_state_derivatives(&self, t: f64, y: &[f64]) -> Result<Vec<f64>, MlirError> {
        let drv = &self.driver;
        // Allocate device buffers.
        let d_y = drv.alloc_f64(y.len().max(1))?;
        let d_p = drv.alloc_f64(self.param_count.max(1))?;
        let d_out = drv.alloc_f64(self.state_count.max(1))?;

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
            len_out: self.state_count as i64,
            stream: CUstream::null(),
        })?;

        drv.synchronize()?;

        let mut out = vec![0.0f64; self.state_count];
        drv.copy_d2h(&mut out, d_out)?;

        drv.free(d_y)?;
        drv.free(d_p)?;
        drv.free(d_out)?;
        Ok(out)
    }

    pub fn state_count(&self) -> usize {
        self.state_count
    }
}

/// Build a `CudaGpuOdeModel` from an already-compiled `GpuCompiledBlob`.
///
/// `blob` must have been compiled with `MlirTarget::GpuCuda`.  Opens
/// `libcuda.so.1` (fails gracefully when absent), loads the PTX, and resolves
/// the `eval_derivative` entry point.
pub fn build_cuda_ode_model(
    blob: GpuCompiledBlob,
    model: &SolveModel,
) -> Result<CudaGpuOdeModel, MlirError> {
    if blob.target != MlirTarget::GpuCuda {
        return Err(MlirError::ToolNotFound {
            tool: "build_cuda_ode_model",
            source: std::io::Error::new(
                std::io::ErrorKind::InvalidInput,
                "blob target must be GpuCuda",
            ),
        });
    }

    let driver = Rc::new(CudaDriver::new()?);
    let module = driver.load_ptx(&blob.device_ir)?;
    let func = driver.get_function(module, &blob.entry_point)?;

    Ok(CudaGpuOdeModel {
        driver,
        _module: module,
        func,
        parameters: model.parameters.clone(),
        state_count: model.state_scalar_count(),
        param_count: model.parameters.len(),
        initial_y: model.initial_y.clone(),
        visible_names: model.visible_names.clone(),
    })
}

// ─── Batch / Monte Carlo support ─────────────────────────────────────────────

/// Run a batch of N ODE trajectories (each with its own parameter set) in
/// parallel using one CUDA stream per trajectory.
///
/// `y0_batch[i]` is the initial state vector for trajectory `i`.
/// `p_batch[i]` is the parameter vector for trajectory `i`.
/// `n_steps` fixed-step forward Euler steps of size `dt`.
///
/// Returns `final_y[i]` — the state of each trajectory at `t = dt * n_steps`.
pub fn batch_euler_cuda(
    driver: &CudaDriver,
    func: CUfunction,
    y0_batch: &[Vec<f64>],
    p_batch: &[Vec<f64>],
    dt: f64,
    n_steps: usize,
) -> Result<Vec<Vec<f64>>, MlirError> {
    let n = y0_batch.len();
    assert_eq!(
        n,
        p_batch.len(),
        "y0_batch and p_batch must have the same length"
    );

    let n_states = y0_batch[0].len();
    let n_params = p_batch[0].len();

    // Allocate per-trajectory device buffers and streams.
    let mut d_y: Vec<CUdeviceptr> = Vec::with_capacity(n);
    let mut d_p: Vec<CUdeviceptr> = Vec::with_capacity(n);
    let mut d_out: Vec<CUdeviceptr> = Vec::with_capacity(n);
    let mut streams: Vec<CUstream> = Vec::with_capacity(n);
    let mut host_y: Vec<Vec<f64>> = y0_batch.to_vec();

    for i in 0..n {
        d_y.push(driver.alloc_f64(n_states.max(1))?);
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
                len_y: n_states as i64,
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
pub fn batch_euler_cuda_device(
    driver: &CudaDriver,
    deriv_func: CUfunction,
    update_func: CUfunction,
    y0_batch: &[Vec<f64>],
    p_batch: &[Vec<f64>],
    dt: f64,
    n_steps: usize,
) -> Result<Vec<Vec<f64>>, MlirError> {
    let n = y0_batch.len();
    assert_eq!(n, p_batch.len());

    let n_states = y0_batch[0].len();
    let n_params = p_batch[0].len();

    // Allocate per-trajectory device buffers and streams.
    let mut d_y: Vec<CUdeviceptr> = Vec::with_capacity(n);
    let mut d_p: Vec<CUdeviceptr> = Vec::with_capacity(n);
    let mut d_xdot: Vec<CUdeviceptr> = Vec::with_capacity(n);
    let mut streams: Vec<CUstream> = Vec::with_capacity(n);

    for i in 0..n {
        let dy = driver.alloc_f64(n_states.max(1))?;
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
                len_y: n_states as i64,
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
    let mut final_y: Vec<Vec<f64>> = vec![vec![0.0; n_states]; n];
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
