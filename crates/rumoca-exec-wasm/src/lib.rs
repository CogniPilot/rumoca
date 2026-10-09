//! WASM execution adapter for prepared Solve-IR row kernels.

mod component_build;
pub use component_build::CompiledWasmComponent;
#[cfg(all(not(target_arch = "wasm32"), feature = "component-build"))]
pub use component_build::build_wasm_component;

mod emit;
mod exact_assignments;
mod native_program;
#[cfg(any(target_arch = "wasm32", test))]
mod private_arena;
mod private_program;
mod scratch_report;
mod typed_call;
#[cfg(target_arch = "wasm32")]
mod wasm_runtime;
pub use exact_assignments::{CompiledExactAssignmentWasm, compile_exact_assignment_schedule_wasm};
pub use native_program::{
    CompiledNativeCallProgramWasm, NativeGatherFault,
    compile_native_assignment_schedule_with_calls_wasm,
};
pub use private_program::{CompiledPrivateProgramWasm, compile_private_program_wasm};
pub use scratch_report::{ScratchCall, ScratchFrame, ScratchOwner, ScratchRegion, ScratchReport};
#[cfg(target_arch = "wasm32")]
use wasm_runtime::WasmKernelRuntime;

pub use typed_call::{
    CompiledTypedCallWasm, TypedCallCompileError, TypedCallFault, TypedCallFaultKind,
    TypedCallLayout, compile_pure_call_wasm,
};

use rumoca_ir_solve::{ScalarProgramBlock, VarLayout};

#[derive(Debug)]
pub enum WasmCompileError {
    Backend(String),
    Input(String),
    TypedSource(TypedCallFault),
    GatherSource(NativeGatherFault),
}

impl std::fmt::Display for WasmCompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Backend(msg) => write!(f, "wasm backend error: {msg}"),
            Self::Input(msg) => write!(f, "invalid input: {msg}"),
            Self::TypedSource(fault) => write!(f, "WASM source operation fault: {fault:?}"),
            Self::GatherSource(fault) => write!(f, "WASM source gather fault: {fault:?}"),
        }
    }
}

impl std::error::Error for WasmCompileError {}

struct CompiledKernelWasm {
    module_bytes: Vec<u8>,
    rows: usize,
    required_y_len: usize,
    required_p_len: usize,
    #[cfg(target_arch = "wasm32")]
    runtime: WasmKernelRuntime,
}

impl CompiledKernelWasm {
    fn from_rows(
        rows: Vec<Vec<rumoca_ir_solve::LinearOp>>,
        required_y_len: usize,
        required_p_len: usize,
    ) -> Result<Self, WasmCompileError> {
        let row_count = rows.len();
        let module_bytes = emit::emit_residual_module(&rows).map_err(WasmCompileError::Backend)?;
        Self::from_module(module_bytes, row_count, required_y_len, required_p_len)
    }

    fn from_module(
        module_bytes: Vec<u8>,
        row_count: usize,
        required_y_len: usize,
        required_p_len: usize,
    ) -> Result<Self, WasmCompileError> {
        #[cfg(target_arch = "wasm32")]
        let runtime = WasmKernelRuntime::new(&module_bytes)?;
        Ok(Self {
            module_bytes,
            rows: row_count,
            required_y_len,
            required_p_len,
            #[cfg(target_arch = "wasm32")]
            runtime,
        })
    }

    fn module_bytes(&self) -> &[u8] {
        &self.module_bytes
    }

    fn into_module_bytes(self) -> Vec<u8> {
        self.module_bytes
    }

    fn rows(&self) -> usize {
        self.rows
    }

    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        seed: Option<&[f64]>,
        out: &mut [f64],
    ) -> Result<(), WasmCompileError> {
        let mut y_scratch = Vec::new();
        let y_slice = if y.len() < self.required_y_len {
            resize_zeroed_scratch(&mut y_scratch, self.required_y_len, "y scratch")?;
            y_scratch[..y.len()].copy_from_slice(y);
            y_scratch.as_slice()
        } else {
            y
        };

        let mut p_scratch = Vec::new();
        let p_slice = if p.len() < self.required_p_len {
            resize_zeroed_scratch(&mut p_scratch, self.required_p_len, "p scratch")?;
            p_scratch[..p.len()].copy_from_slice(p);
            p_scratch.as_slice()
        } else {
            p
        };

        let mut seed_scratch = Vec::new();
        let seed_slice = match seed {
            Some(seed_values) if seed_values.len() < self.required_y_len => {
                resize_zeroed_scratch(&mut seed_scratch, self.required_y_len, "seed scratch")?;
                seed_scratch[..seed_values.len()].copy_from_slice(seed_values);
                Some(seed_scratch.as_slice())
            }
            Some(seed_values) => Some(seed_values),
            None => None,
        };

        let out_len = out.len();
        let out_short = out_len < self.rows;
        let mut out_scratch = Vec::new();
        if out_short {
            resize_zeroed_scratch(&mut out_scratch, self.rows, "output scratch")?;
        }

        {
            let out_slice: &mut [f64] = if out_short {
                out_scratch.as_mut_slice()
            } else {
                out
            };

            #[cfg(target_arch = "wasm32")]
            {
                self.runtime
                    .call(y_slice, p_slice, t, seed_slice, out_slice)?;
            }
            #[cfg(not(target_arch = "wasm32"))]
            {
                let _ = (y_slice, p_slice, t, seed_slice, out_slice);
                Err(WasmCompileError::Input(
                    "compiled WASM kernels can only be executed on wasm32 targets".to_string(),
                ))?;
            }
        }

        #[cfg(target_arch = "wasm32")]
        if out_short {
            out.copy_from_slice(&out_scratch[..out_len]);
        }
        Ok(())
    }
}

fn resize_zeroed_scratch(
    values: &mut Vec<f64>,
    len: usize,
    kind: &'static str,
) -> Result<(), WasmCompileError> {
    if values.len() >= len {
        values.resize(len, 0.0);
        return Ok(());
    }
    let additional = len - values.len();
    values.try_reserve(additional).map_err(|_| {
        WasmCompileError::Backend(format!("{kind} allocation overflow for {len} values"))
    })?;
    values.resize(len, 0.0);
    Ok(())
}

pub struct CompiledResidualWasm {
    kernel: CompiledKernelWasm,
}

impl CompiledResidualWasm {
    pub fn module_bytes(&self) -> &[u8] {
        self.kernel.module_bytes()
    }

    pub fn into_module_bytes(self) -> Vec<u8> {
        self.kernel.into_module_bytes()
    }

    pub fn rows(&self) -> usize {
        self.kernel.rows()
    }

    pub fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Result<(), WasmCompileError> {
        self.kernel.call(y, p, t, None, out)
    }
}

pub struct CompiledJacobianVWasm {
    kernel: CompiledKernelWasm,
}

impl CompiledJacobianVWasm {
    pub fn module_bytes(&self) -> &[u8] {
        self.kernel.module_bytes()
    }

    pub fn rows(&self) -> usize {
        self.kernel.rows()
    }

    pub fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        v: &[f64],
        out: &mut [f64],
    ) -> Result<(), WasmCompileError> {
        self.kernel.call(y, p, t, Some(v), out)
    }
}

pub struct CompiledExpressionRowsWasm {
    kernel: CompiledKernelWasm,
}

impl CompiledExpressionRowsWasm {
    pub fn module_bytes(&self) -> &[u8] {
        self.kernel.module_bytes()
    }

    pub fn rows(&self) -> usize {
        self.kernel.rows()
    }

    pub fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Result<(), WasmCompileError> {
        self.kernel.call(y, p, t, None, out)
    }
}

pub fn compile_residual_scalar_program_block_wasm(
    rows: &ScalarProgramBlock,
    layout: &VarLayout,
) -> Result<CompiledResidualWasm, WasmCompileError> {
    let kernel = CompiledKernelWasm::from_rows(
        rows.programs().to_vec(),
        layout.y_scalars(),
        layout.p_scalars(),
    )?;
    Ok(CompiledResidualWasm { kernel })
}

pub fn compile_jacobian_scalar_program_block_wasm(
    rows: &ScalarProgramBlock,
    layout: &VarLayout,
) -> Result<CompiledJacobianVWasm, WasmCompileError> {
    let kernel = CompiledKernelWasm::from_rows(
        rows.programs().to_vec(),
        layout.y_scalars(),
        layout.p_scalars(),
    )?;
    Ok(CompiledJacobianVWasm { kernel })
}

pub fn compile_expression_scalar_program_block_wasm(
    rows: &ScalarProgramBlock,
    layout: &VarLayout,
) -> Result<CompiledExpressionRowsWasm, WasmCompileError> {
    compile_expression_rows_wasm(
        layout.y_scalars(),
        layout.p_scalars(),
        rows.programs().to_vec(),
    )
}

/// Compile checked scalar, compact affine, and direct-input matrix expressions.
/// Affine domains and dense product reductions become loops. Matrix operands
/// must be independent direct input views; computed/coupled setups fail.
/// Matrix output buffers must not overlap either input buffer; the emitted
/// module traps before mutation on aliasing or pointer-range overflow.
/// Packed TensorLoad scalar registers are limited to 4096 values. Outputs use
/// the ComputeBlock's logical slots;
/// sparse holes are cleared. This evaluates expressions, not a model's causal
/// refresh schedule: assignment commits remain the caller's checked contract.
pub fn compile_expression_compute_block_wasm(
    block: &rumoca_ir_solve::ComputeBlock,
    layout: &VarLayout,
) -> Result<CompiledExpressionRowsWasm, WasmCompileError> {
    let module_bytes = compile_expression_compute_block_wasm_bytes(block, layout)?;
    let outputs = block
        .len()
        .map_err(|error| WasmCompileError::Input(error.to_string()))?;
    let kernel = CompiledKernelWasm::from_module(
        module_bytes,
        outputs,
        layout.y_scalars(),
        layout.p_scalars(),
    )?;
    Ok(CompiledExpressionRowsWasm { kernel })
}

/// Emit the same checked portable expression module without instantiating it.
///
/// Artifact generation does not use or change the compiler's WASM memory.
/// Callers instantiate with an unshared wasm32 `env.memory`, provide any math
/// imports, and call `eval_residual(y_ptr, p_ptr, time, seed_ptr, out_ptr)`.
/// The instantiated wrapper above remains available for immediate execution.
pub fn compile_expression_compute_block_wasm_bytes(
    block: &rumoca_ir_solve::ComputeBlock,
    layout: &VarLayout,
) -> Result<Vec<u8>, WasmCompileError> {
    emit::emit_compute_module(block, layout).map_err(WasmCompileError::Backend)
}

/// Emit one direct-write function from a construction-issued native schedule.
///
/// The module imports unshared wasm32 `env.memory` and the same supported math
/// intrinsics as expression kernels. Call `eval_assignments(y_ptr,p_ptr,time,0,0)`
/// once: every issued value is written directly into its owned Y target range.
/// Y/P must be aligned, disjoint and wholly in memory; guards trap before writes.
/// No compiler memory is instantiated or mutated while producing these bytes.
pub fn compile_native_assignment_schedule_wasm_bytes(
    schedule: &rumoca_ir_solve::NativeRefreshAssignmentSchedule,
    layout: &VarLayout,
) -> Result<Vec<u8>, WasmCompileError> {
    emit::emit_native_assignment_module(schedule, layout).map_err(WasmCompileError::Backend)
}

fn compile_expression_rows_wasm(
    required_y_len: usize,
    required_p_len: usize,
    rows: Vec<Vec<rumoca_ir_solve::LinearOp>>,
) -> Result<CompiledExpressionRowsWasm, WasmCompileError> {
    let kernel = CompiledKernelWasm::from_rows(rows, required_y_len, required_p_len)?;
    Ok(CompiledExpressionRowsWasm { kernel })
}

#[cfg(test)]
mod tangent_lane_refusal_tests;

#[cfg(test)]
mod tests {
    use rumoca_ir_solve::{BinaryOp, LinearOp, ScalarProgramBlock, UnaryOp, VarLayout};
    use wasmparser::FunctionBody;
    use wasmparser::Parser;
    use wasmparser::Payload;
    use wasmparser::Validator;

    fn fixture_program_block() -> (ScalarProgramBlock, VarLayout) {
        let rows = vec![
            vec![
                LinearOp::LoadY { dst: 0, index: 0 },
                LinearOp::LoadY { dst: 1, index: 1 },
                LinearOp::Unary {
                    dst: 2,
                    op: UnaryOp::Sin,
                    arg: 1,
                },
                LinearOp::Binary {
                    dst: 3,
                    op: BinaryOp::Add,
                    lhs: 0,
                    rhs: 2,
                },
                LinearOp::StoreOutput { src: 3 },
            ],
            vec![
                LinearOp::LoadY { dst: 0, index: 0 },
                LinearOp::Unary {
                    dst: 1,
                    op: UnaryOp::Exp,
                    arg: 0,
                },
                LinearOp::LoadP { dst: 2, index: 0 },
                LinearOp::LoadY { dst: 3, index: 1 },
                LinearOp::Binary {
                    dst: 4,
                    op: BinaryOp::Mul,
                    lhs: 2,
                    rhs: 3,
                },
                LinearOp::Binary {
                    dst: 5,
                    op: BinaryOp::Sub,
                    lhs: 1,
                    rhs: 4,
                },
                LinearOp::StoreOutput { src: 5 },
            ],
        ];
        let span = rumoca_ir_solve::source_span_from_offsets(1, 0, 1);
        let block = ScalarProgramBlock::with_program_spans(rows, vec![span; 2])
            .expect("fixture programs satisfy the Solve-IR contract");
        let layout = VarLayout::from_parts(Default::default(), 2, 1);
        (block, layout)
    }

    fn compile_fixture_model() -> super::CompiledResidualWasm {
        let (block, layout) = fixture_program_block();
        super::compile_residual_scalar_program_block_wasm(&block, &layout)
            .expect("compile residual WASM through the public adapter")
    }

    #[derive(Default)]
    struct ModuleStats {
        saw_eval_export: bool,
        saw_memory_export: bool,
        function_bodies: usize,
        op_count: usize,
    }

    fn collect_module_stats(module_bytes: &[u8]) -> ModuleStats {
        let mut stats = ModuleStats::default();
        for payload in Parser::new(0).parse_all(module_bytes) {
            let payload = payload.expect("parse payload");
            collect_payload_stats(payload, &mut stats);
        }
        stats
    }

    fn collect_payload_stats(payload: Payload<'_>, stats: &mut ModuleStats) {
        match payload {
            Payload::ExportSection(reader) => update_export_stats(reader, stats),
            Payload::CodeSectionEntry(body) => update_code_stats(body, stats),
            _ => {}
        }
    }

    fn update_export_stats(reader: wasmparser::ExportSectionReader<'_>, stats: &mut ModuleStats) {
        for export in reader {
            let export = export.expect("read export");
            if export.name == "eval_residual" {
                stats.saw_eval_export = true;
            } else if export.name == "memory" {
                stats.saw_memory_export = true;
            }
        }
    }

    fn update_code_stats(body: FunctionBody<'_>, stats: &mut ModuleStats) {
        stats.function_bodies += 1;
        stats.op_count += count_operators(body);
    }

    fn count_operators(body: FunctionBody<'_>) -> usize {
        let mut ops = body.get_operators_reader().expect("operators reader");
        let mut count = 0usize;
        while !ops.eof() {
            let _ = ops.read().expect("read operator");
            count += 1;
        }
        count
    }

    #[test]
    fn emitted_module_validates_and_exports_eval_function() {
        let compiled = compile_fixture_model();
        let module_bytes = compiled.module_bytes();
        Validator::new()
            .validate_all(module_bytes)
            .expect("validate emitted wasm");

        let stats = collect_module_stats(module_bytes);
        assert!(stats.saw_eval_export);
        assert!(stats.saw_memory_export);
        assert_eq!(stats.function_bodies, 1);
        assert!(stats.op_count > 20);
    }

    #[test]
    fn every_public_compile_surface_consumes_a_checked_solve_block() {
        let (block, layout) = fixture_program_block();
        let residual = super::compile_residual_scalar_program_block_wasm(&block, &layout)
            .expect("compile residual adapter");
        let jacobian = super::compile_jacobian_scalar_program_block_wasm(&block, &layout)
            .expect("compile Jacobian adapter");
        let expressions = super::compile_expression_scalar_program_block_wasm(&block, &layout)
            .expect("compile expression adapter");

        for bytes in [
            residual.module_bytes(),
            jacobian.module_bytes(),
            expressions.module_bytes(),
        ] {
            Validator::new()
                .validate_all(bytes)
                .expect("public adapter emits a valid WASM module");
        }
        assert_eq!(residual.rows(), 2);
        assert_eq!(jacobian.rows(), 2);
        assert_eq!(expressions.rows(), 2);
    }

    #[test]
    fn compile_rejects_linear_solve_register_range_overflow() {
        let rows = vec![vec![
            LinearOp::LinearSolveComponent {
                dst: 0,
                matrix_start: u32::MAX,
                rhs_start: 0,
                n: usize::MAX,
                component: 0,
            },
            LinearOp::StoreOutput { src: 0 },
        ]];

        let err = match super::CompiledKernelWasm::from_rows(rows, 0, 0) {
            Ok(_) => panic!("oversized linear solve metadata should fail"),
            Err(err) => err,
        };

        assert!(
            matches!(err, super::WasmCompileError::Backend(message) if message.contains("LinearSolveComponent op 0 register range"))
        );
    }
}
