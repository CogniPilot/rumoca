//! WASM execution adapter for prepared Solve-IR row kernels.

mod emit;

use rumoca_ir_solve::{ScalarProgramBlock, VarLayout};

#[derive(Debug)]
pub enum WasmCompileError {
    Backend(String),
    Input(String),
}

impl std::fmt::Display for WasmCompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Backend(msg) => write!(f, "wasm backend error: {msg}"),
            Self::Input(msg) => write!(f, "invalid input: {msg}"),
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
        rows.programs.clone(),
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
        rows.programs.clone(),
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
        rows.programs.clone(),
    )
}

fn compile_expression_rows_wasm(
    required_y_len: usize,
    required_p_len: usize,
    rows: Vec<Vec<rumoca_ir_solve::LinearOp>>,
) -> Result<CompiledExpressionRowsWasm, WasmCompileError> {
    let kernel = CompiledKernelWasm::from_rows(rows, required_y_len, required_p_len)?;
    Ok(CompiledExpressionRowsWasm { kernel })
}

#[cfg(target_arch = "wasm32")]
struct WasmKernelRuntime {
    eval_function: js_sys::Function,
}

#[cfg(target_arch = "wasm32")]
impl WasmKernelRuntime {
    fn new(module_bytes: &[u8]) -> Result<Self, WasmCompileError> {
        use js_sys::Object;
        use js_sys::Reflect;
        use js_sys::Uint8Array;
        use js_sys::WebAssembly;
        use wasm_bindgen::JsCast;
        use wasm_bindgen::JsValue;

        let wasm_bytes = Uint8Array::from(module_bytes);
        let module = WebAssembly::Module::new(&wasm_bytes.into())
            .map_err(|err| WasmCompileError::Backend(format!("module create failed: {err:?}")))?;

        let imports = Object::new();
        let env = Object::new();
        Reflect::set(&env, &JsValue::from_str("memory"), &wasm_bindgen::memory()).map_err(
            |err| WasmCompileError::Backend(format!("memory import set failed: {err:?}")),
        )?;
        install_math_import(&env, "abs")?;
        install_math_import(&env, "sign")?;
        install_math_import(&env, "sin")?;
        install_math_import(&env, "cos")?;
        install_math_import(&env, "tan")?;
        install_math_import(&env, "asin")?;
        install_math_import(&env, "acos")?;
        install_math_import(&env, "atan")?;
        install_math_import(&env, "sinh")?;
        install_math_import(&env, "cosh")?;
        install_math_import(&env, "tanh")?;
        install_math_import(&env, "exp")?;
        install_math_import(&env, "log")?;
        install_math_import(&env, "log10")?;
        install_math_import(&env, "pow")?;
        install_math_import(&env, "atan2")?;

        Reflect::set(&imports, &JsValue::from_str("env"), &env)
            .map_err(|err| WasmCompileError::Backend(format!("env import set failed: {err:?}")))?;

        let instance = WebAssembly::Instance::new(&module, &imports).map_err(|err| {
            WasmCompileError::Backend(format!("module instantiate failed: {err:?}"))
        })?;
        let exports = instance.exports();
        let eval = Reflect::get(&exports, &JsValue::from_str("eval_residual"))
            .map_err(|err| WasmCompileError::Backend(format!("missing eval export: {err:?}")))?;
        let eval_function = eval.dyn_into::<js_sys::Function>().map_err(|_| {
            WasmCompileError::Backend("eval_residual export is not a callable function".to_string())
        })?;
        Ok(Self { eval_function })
    }

    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        seed: Option<&[f64]>,
        out: &mut [f64],
    ) -> Result<(), WasmCompileError> {
        use wasm_bindgen::JsValue;

        let y_ptr = ptr_to_wasm_i32(y.as_ptr())?;
        let p_ptr = ptr_to_wasm_i32(p.as_ptr())?;
        let seed_ptr = match seed {
            Some(values) => ptr_to_wasm_i32(values.as_ptr())?,
            None => 0u32,
        };
        let out_ptr = ptr_to_wasm_i32(out.as_ptr())?;

        self.eval_function
            .call5(
                &JsValue::NULL,
                &JsValue::from_f64(y_ptr as f64),
                &JsValue::from_f64(p_ptr as f64),
                &JsValue::from_f64(t),
                &JsValue::from_f64(seed_ptr as f64),
                &JsValue::from_f64(out_ptr as f64),
            )
            .map_err(|err| WasmCompileError::Backend(format!("kernel call failed: {err:?}")))?;
        Ok(())
    }
}

#[cfg(target_arch = "wasm32")]
fn install_math_import(env: &js_sys::Object, name: &str) -> Result<(), WasmCompileError> {
    use js_sys::Reflect;
    use wasm_bindgen::JsValue;

    let global = js_sys::global();
    let math = Reflect::get(&global, &JsValue::from_str("Math"))
        .map_err(|err| WasmCompileError::Backend(format!("Math global missing: {err:?}")))?;
    let function = Reflect::get(&math, &JsValue::from_str(name))
        .map_err(|err| WasmCompileError::Backend(format!("Math.{name} missing: {err:?}")))?;
    Reflect::set(env, &JsValue::from_str(name), &function)
        .map(|_| ())
        .map_err(|err| {
            WasmCompileError::Backend(format!("failed setting import Math.{name}: {err:?}"))
        })
}

#[cfg(target_arch = "wasm32")]
fn ptr_to_wasm_i32<T>(ptr: *const T) -> Result<u32, WasmCompileError> {
    u32::try_from(ptr as usize)
        .map_err(|_| WasmCompileError::Backend("pointer offset does not fit wasm32".to_string()))
}

#[cfg(test)]
mod tests {
    use rumoca_ir_solve::{BinaryOp, ExternalFunctionKind, LinearOp, UnaryOp};
    use wasmparser::FunctionBody;
    use wasmparser::Parser;
    use wasmparser::Payload;
    use wasmparser::Validator;

    fn compile_fixture_model() -> super::CompiledResidualWasm {
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
        let kernel = super::CompiledKernelWasm::from_rows(rows, 2, 1).expect("compile wasm rows");
        super::CompiledResidualWasm { kernel }
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
            matches!(err, super::WasmCompileError::Backend(message) if message.contains("linear solve matrix size overflow"))
        );
    }
}
