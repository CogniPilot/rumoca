/// CUDA Driver API loaded at runtime via libloading.
///
/// Opens `libcuda.so.1` and resolves only the symbols we need.  No CUDA SDK
/// or compile-time link against libcuda is required — fails gracefully at
/// runtime with `MlirError::ToolNotFound` when the library is absent.
use crate::error::MlirError;
use std::ffi::{CString, c_void};

// ─── CUDA opaque handle types ─────────────────────────────────────────────────
pub type CUresult = i32;
pub type CUdevice = i32;
pub type CUdeviceptr = u64;
type CUcontext = *mut c_void;
type CUmoduleRaw = *mut c_void;
type CUfunctionRaw = *mut c_void;
type CUstreamRaw = *mut c_void;

#[derive(Clone, Copy)]
pub struct CUmodule(CUmoduleRaw);

#[derive(Clone, Copy)]
pub struct CUfunction(CUfunctionRaw);

#[derive(Clone, Copy)]
pub struct CUstream(CUstreamRaw);

impl CUstream {
    pub const fn null() -> Self {
        Self(std::ptr::null_mut())
    }
}

pub const CUDA_SUCCESS: CUresult = 0;

// ─── Raw function-pointer types ───────────────────────────────────────────────
type FnCuInit = unsafe extern "C" fn(u32) -> CUresult;
type FnCuDeviceGet = unsafe extern "C" fn(*mut CUdevice, i32) -> CUresult;
type FnCuCtxCreate = unsafe extern "C" fn(*mut CUcontext, u32, CUdevice) -> CUresult;
type FnCuModuleLoadData = unsafe extern "C" fn(*mut CUmoduleRaw, *const c_void) -> CUresult;
type FnCuModuleGetFunction =
    unsafe extern "C" fn(*mut CUfunctionRaw, CUmoduleRaw, *const i8) -> CUresult;
type FnCuMemAlloc = unsafe extern "C" fn(*mut CUdeviceptr, usize) -> CUresult;
type FnCuMemcpyHtoD = unsafe extern "C" fn(CUdeviceptr, *const c_void, usize) -> CUresult;
type FnCuMemcpyDtoH = unsafe extern "C" fn(*mut c_void, CUdeviceptr, usize) -> CUresult;
type FnCuMemFree = unsafe extern "C" fn(CUdeviceptr) -> CUresult;
type FnCuLaunchKernel = unsafe extern "C" fn(
    CUfunctionRaw,
    u32,
    u32,
    u32, // grid dims
    u32,
    u32,
    u32, // block dims
    u32,
    CUstreamRaw,
    *mut *mut c_void,
    *mut *mut c_void,
) -> CUresult;
type FnCuCtxSynchronize = unsafe extern "C" fn() -> CUresult;
type FnCuStreamCreate = unsafe extern "C" fn(*mut CUstreamRaw, u32) -> CUresult;
type FnCuStreamSynchronize = unsafe extern "C" fn(CUstreamRaw) -> CUresult;
type FnCuStreamDestroy = unsafe extern "C" fn(CUstreamRaw) -> CUresult;

pub struct EvalDerivativeLaunch {
    pub func: CUfunction,
    pub d_y: CUdeviceptr,
    pub len_y: i64,
    pub d_p: CUdeviceptr,
    pub len_p: i64,
    pub t: f64,
    pub d_out: CUdeviceptr,
    pub len_out: i64,
    pub stream: CUstream,
}

// ─── CudaDriver ───────────────────────────────────────────────────────────────

/// Holds the dynamically-loaded CUDA driver library and all resolved symbols.
pub struct CudaDriver {
    _lib: libloading::Library,
    pub cu_module_load_data: FnCuModuleLoadData,
    pub cu_module_get_function: FnCuModuleGetFunction,
    pub cu_mem_alloc: FnCuMemAlloc,
    pub cu_memcpy_h2d: FnCuMemcpyHtoD,
    pub cu_memcpy_d2h: FnCuMemcpyDtoH,
    pub cu_mem_free: FnCuMemFree,
    pub cu_launch_kernel: FnCuLaunchKernel,
    pub cu_ctx_synchronize: FnCuCtxSynchronize,
    pub cu_stream_create: FnCuStreamCreate,
    pub cu_stream_synchronize: FnCuStreamSynchronize,
    pub cu_stream_destroy: FnCuStreamDestroy,
    pub ctx: CUcontext,
}

impl CudaDriver {
    /// Open `libcuda.so.1`, initialise CUDA, and create a context on device 0.
    pub fn new() -> Result<Self, MlirError> {
        let lib = unsafe { libloading::Library::new("libcuda.so.1") }.map_err(|e| {
            MlirError::ToolNotFound {
                tool: "libcuda.so.1",
                source: std::io::Error::new(std::io::ErrorKind::NotFound, e.to_string()),
            }
        })?;

        macro_rules! sym {
            ($name:expr, $ty:ty) => {{
                let s: libloading::Symbol<$ty> = unsafe { lib.get($name) }.map_err(|e| {
                    MlirError::MissingSymbol(format!(
                        "{}: {e}",
                        std::str::from_utf8($name).unwrap_or("?")
                    ))
                })?;
                *s
            }};
        }

        let cu_init: FnCuInit = sym!(b"cuInit\0", FnCuInit);
        let cu_device_get: FnCuDeviceGet = sym!(b"cuDeviceGet\0", FnCuDeviceGet);
        let cu_ctx_create: FnCuCtxCreate = sym!(b"cuCtxCreate\0", FnCuCtxCreate);

        let cu_module_load_data = sym!(b"cuModuleLoadData\0", FnCuModuleLoadData);
        let cu_module_get_function = sym!(b"cuModuleGetFunction\0", FnCuModuleGetFunction);
        let cu_mem_alloc = sym!(b"cuMemAlloc\0", FnCuMemAlloc);
        let cu_memcpy_h2d = sym!(b"cuMemcpyHtoD\0", FnCuMemcpyHtoD);
        let cu_memcpy_d2h = sym!(b"cuMemcpyDtoH\0", FnCuMemcpyDtoH);
        let cu_mem_free = sym!(b"cuMemFree\0", FnCuMemFree);
        let cu_launch_kernel = sym!(b"cuLaunchKernel\0", FnCuLaunchKernel);
        let cu_ctx_synchronize = sym!(b"cuCtxSynchronize\0", FnCuCtxSynchronize);
        let cu_stream_create = sym!(b"cuStreamCreate\0", FnCuStreamCreate);
        let cu_stream_synchronize = sym!(b"cuStreamSynchronize\0", FnCuStreamSynchronize);
        let cu_stream_destroy = sym!(b"cuStreamDestroy\0", FnCuStreamDestroy);

        cuda_check(unsafe { cu_init(0) }, "cuInit")?;

        let mut device: CUdevice = 0;
        cuda_check(unsafe { cu_device_get(&mut device, 0) }, "cuDeviceGet")?;

        let mut ctx: CUcontext = std::ptr::null_mut();
        cuda_check(unsafe { cu_ctx_create(&mut ctx, 0, device) }, "cuCtxCreate")?;

        Ok(Self {
            _lib: lib,
            cu_module_load_data,
            cu_module_get_function,
            cu_mem_alloc,
            cu_memcpy_h2d,
            cu_memcpy_d2h,
            cu_mem_free,
            cu_launch_kernel,
            cu_ctx_synchronize,
            cu_stream_create,
            cu_stream_synchronize,
            cu_stream_destroy,
            ctx,
        })
    }

    /// Load a PTX blob (null-terminated text) and return the module handle.
    pub fn load_ptx(&self, ptx: &[u8]) -> Result<CUmodule, MlirError> {
        // cuModuleLoadData requires a null-terminated string.
        let mut ptx_with_nul = ptx.to_vec();
        if ptx_with_nul.last() != Some(&0) {
            ptx_with_nul.push(0);
        }
        let mut module: CUmoduleRaw = std::ptr::null_mut();
        cuda_check(
            unsafe {
                (self.cu_module_load_data)(&mut module, ptx_with_nul.as_ptr() as *const c_void)
            },
            "cuModuleLoadData",
        )?;
        Ok(CUmodule(module))
    }

    /// Resolve a kernel function by name within a loaded module.
    pub fn get_function(&self, module: CUmodule, name: &str) -> Result<CUfunction, MlirError> {
        let cname = CString::new(name).unwrap();
        let mut func: CUfunctionRaw = std::ptr::null_mut();
        cuda_check(
            unsafe { (self.cu_module_get_function)(&mut func, module.0, cname.as_ptr()) },
            "cuModuleGetFunction",
        )?;
        Ok(CUfunction(func))
    }

    /// Allocate `n` f64 values on the device and return the device pointer.
    pub fn alloc_f64(&self, n: usize) -> Result<CUdeviceptr, MlirError> {
        let mut ptr: CUdeviceptr = 0;
        cuda_check(
            unsafe { (self.cu_mem_alloc)(&mut ptr, n * 8) },
            "cuMemAlloc",
        )?;
        Ok(ptr)
    }

    /// Copy a host f64 slice to device memory.
    pub fn copy_h2d(&self, dst: CUdeviceptr, src: &[f64]) -> Result<(), MlirError> {
        cuda_check(
            unsafe { (self.cu_memcpy_h2d)(dst, src.as_ptr() as *const c_void, src.len() * 8) },
            "cuMemcpyHtoD",
        )?;
        Ok(())
    }

    /// Copy device memory to a host f64 slice.
    pub fn copy_d2h(&self, dst: &mut [f64], src: CUdeviceptr) -> Result<(), MlirError> {
        cuda_check(
            unsafe { (self.cu_memcpy_d2h)(dst.as_mut_ptr() as *mut c_void, src, dst.len() * 8) },
            "cuMemcpyDtoH",
        )?;
        Ok(())
    }

    /// Free a device allocation.
    pub fn free(&self, ptr: CUdeviceptr) -> Result<(), MlirError> {
        cuda_check(unsafe { (self.cu_mem_free)(ptr) }, "cuMemFree")?;
        Ok(())
    }

    /// Launch the `eval_derivative` kernel (expanded memref descriptor ABI, 16 args).
    ///
    /// The kernel signature (after MLIR lowering) is:
    ///   `(y_alloc, y_align, y_off, y_size, y_stride,
    ///     p_alloc, p_align, p_off, p_size, p_stride,
    ///     t,
    ///     out_alloc, out_align, out_off, out_size, out_stride)`
    /// We pass device pointers as u64 with stride=1, offset=0.
    pub fn launch_eval_derivative(&self, launch: EvalDerivativeLaunch) -> Result<(), MlirError> {
        let EvalDerivativeLaunch {
            func,
            d_y,
            len_y,
            d_p,
            len_p,
            t,
            d_out,
            len_out,
            stream,
        } = launch;
        let zero: i64 = 0;
        let one: i64 = 1;
        // We need stable stack addresses for each argument.
        let args: [*mut c_void; 16] = [
            &d_y as *const _ as *mut c_void,
            &d_y as *const _ as *mut c_void,
            &zero as *const _ as *mut c_void,
            &len_y as *const _ as *mut c_void,
            &one as *const _ as *mut c_void,
            &d_p as *const _ as *mut c_void,
            &d_p as *const _ as *mut c_void,
            &zero as *const _ as *mut c_void,
            &len_p as *const _ as *mut c_void,
            &one as *const _ as *mut c_void,
            &t as *const _ as *mut c_void,
            &d_out as *const _ as *mut c_void,
            &d_out as *const _ as *mut c_void,
            &zero as *const _ as *mut c_void,
            &len_out as *const _ as *mut c_void,
            &one as *const _ as *mut c_void,
        ];
        let mut args = args; // make mutable for the void** cast
        cuda_check(
            unsafe {
                (self.cu_launch_kernel)(
                    func.0,
                    1,
                    1,
                    1, // grid: 1 block
                    1,
                    1,
                    1, // block: 1 thread
                    0,
                    stream.0,
                    args.as_mut_ptr(),
                    std::ptr::null_mut(),
                )
            },
            "cuLaunchKernel",
        )?;
        Ok(())
    }

    /// Launch the `euler_update_kernel` — one thread per state element.
    ///
    /// Kernel signature: `(y: ptr, xdot: ptr, dt: f64, n: i64)`
    ///   - `d_y`:   device buffer to update in-place (`y[i] += dt * xdot[i]`)
    ///   - `d_xdot`: device buffer with derivatives (read-only)
    ///   - `dt`:    step size (scalar, by value)
    ///   - `n`:     number of state elements
    ///
    /// Thread layout: `blockDim.x = 256`, `gridDim.x = ceil(n / 256)`.
    pub fn launch_euler_update(
        &self,
        func: CUfunction,
        d_y: CUdeviceptr,
        d_xdot: CUdeviceptr,
        dt: f64,
        n: i64,
        stream: CUstream,
    ) -> Result<(), MlirError> {
        const BLOCK: u32 = 256;
        let grid: u32 = (n as u32).div_ceil(BLOCK);
        let args: [*mut c_void; 4] = [
            &d_y as *const _ as *mut c_void,
            &d_xdot as *const _ as *mut c_void,
            &dt as *const _ as *mut c_void,
            &n as *const _ as *mut c_void,
        ];
        let mut args = args;
        cuda_check(
            unsafe {
                (self.cu_launch_kernel)(
                    func.0,
                    grid,
                    1,
                    1,
                    BLOCK,
                    1,
                    1,
                    0,
                    stream.0,
                    args.as_mut_ptr(),
                    std::ptr::null_mut(),
                )
            },
            "cuLaunchKernel(euler_update)",
        )?;
        Ok(())
    }

    pub fn synchronize(&self) -> Result<(), MlirError> {
        cuda_check(unsafe { (self.cu_ctx_synchronize)() }, "cuCtxSynchronize")
    }

    pub fn stream_create(&self) -> Result<CUstream, MlirError> {
        let mut stream: CUstreamRaw = std::ptr::null_mut();
        cuda_check(
            unsafe { (self.cu_stream_create)(&mut stream, 0) },
            "cuStreamCreate",
        )?;
        Ok(CUstream(stream))
    }

    pub fn stream_synchronize(&self, stream: CUstream) -> Result<(), MlirError> {
        cuda_check(
            unsafe { (self.cu_stream_synchronize)(stream.0) },
            "cuStreamSynchronize",
        )
    }

    pub fn stream_destroy(&self, stream: CUstream) -> Result<(), MlirError> {
        cuda_check(
            unsafe { (self.cu_stream_destroy)(stream.0) },
            "cuStreamDestroy",
        )
    }
}

pub fn cuda_check(result: CUresult, op: &'static str) -> Result<(), MlirError> {
    if result == CUDA_SUCCESS {
        Ok(())
    } else {
        Err(MlirError::ToolFailed {
            tool: op,
            code: result,
            stderr: format!("CUDA error code {result}"),
        })
    }
}
