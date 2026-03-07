//! WASM backend for compiled residual/Jacobian/expression-row evaluation.

mod emit;

use crate::compiled::VarLayout;
use crate::compiled::ad::lower_residual_ad;
use crate::compiled::lower::{
    LowerError, lower_discrete_rhs, lower_expression_rows_from_expressions, lower_initial_residual,
    lower_residual, lower_root_conditions,
};
use rumoca_ir_dae as dae;

#[derive(Debug)]
pub enum WasmCompileError {
    Lower(LowerError),
    Backend(String),
    Input(String),
}

impl std::fmt::Display for WasmCompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Lower(err) => write!(f, "{err}"),
            Self::Backend(msg) => write!(f, "wasm backend error: {msg}"),
            Self::Input(msg) => write!(f, "invalid input: {msg}"),
        }
    }
}

impl std::error::Error for WasmCompileError {}

impl From<LowerError> for WasmCompileError {
    fn from(value: LowerError) -> Self {
        Self::Lower(value)
    }
}

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
        rows: Vec<Vec<crate::compiled::linear_op::LinearOp>>,
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
            y_scratch.resize(self.required_y_len, 0.0);
            y_scratch[..y.len()].copy_from_slice(y);
            y_scratch.as_slice()
        } else {
            y
        };

        let mut p_scratch = Vec::new();
        let p_slice = if p.len() < self.required_p_len {
            p_scratch.resize(self.required_p_len, 0.0);
            p_scratch[..p.len()].copy_from_slice(p);
            p_scratch.as_slice()
        } else {
            p
        };

        let mut seed_scratch = Vec::new();
        let seed_slice = match seed {
            Some(seed_values) if seed_values.len() < self.required_y_len => {
                seed_scratch.resize(self.required_y_len, 0.0);
                seed_scratch[..seed_values.len()].copy_from_slice(seed_values);
                Some(seed_scratch.as_slice())
            }
            Some(seed_values) => Some(seed_values),
            None => None,
        };

        let out_len = out.len();
        let out_short = out_len < self.rows;
        let mut out_scratch = if out_short {
            vec![0.0; self.rows]
        } else {
            Vec::new()
        };

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

fn compile_expression_rows_wasm(
    required_y_len: usize,
    required_p_len: usize,
    rows: Vec<Vec<crate::compiled::linear_op::LinearOp>>,
) -> Result<CompiledExpressionRowsWasm, WasmCompileError> {
    let kernel = CompiledKernelWasm::from_rows(rows, required_y_len, required_p_len)?;
    Ok(CompiledExpressionRowsWasm { kernel })
}

pub fn compile_residual_wasm(
    dae_model: &dae::Dae,
) -> Result<CompiledResidualWasm, WasmCompileError> {
    let layout = VarLayout::from_dae(dae_model);
    let rows = lower_residual(dae_model, &layout)?;
    let kernel = CompiledKernelWasm::from_rows(rows, layout.y_scalars(), layout.p_scalars())?;
    Ok(CompiledResidualWasm { kernel })
}

pub fn compile_jacobian_v_wasm(
    dae_model: &dae::Dae,
) -> Result<CompiledJacobianVWasm, WasmCompileError> {
    let layout = VarLayout::from_dae(dae_model);
    let rows = lower_residual_ad(dae_model, &layout)?;
    let kernel = CompiledKernelWasm::from_rows(rows, layout.y_scalars(), layout.p_scalars())?;
    Ok(CompiledJacobianVWasm { kernel })
}

pub fn compile_root_conditions_wasm(
    dae_model: &dae::Dae,
) -> Result<CompiledExpressionRowsWasm, WasmCompileError> {
    let layout = VarLayout::from_dae(dae_model);
    let rows = lower_root_conditions(dae_model, &layout)?;
    compile_expression_rows_wasm(layout.y_scalars(), layout.p_scalars(), rows)
}

pub fn compile_expressions_wasm(
    dae_model: &dae::Dae,
    expressions: &[dae::Expression],
) -> Result<CompiledExpressionRowsWasm, WasmCompileError> {
    let layout = VarLayout::from_dae(dae_model);
    let rows = lower_expression_rows_from_expressions(expressions, &layout, &dae_model.functions)?;
    compile_expression_rows_wasm(layout.y_scalars(), layout.p_scalars(), rows)
}

pub fn compile_discrete_rhs_wasm(
    dae_model: &dae::Dae,
) -> Result<CompiledExpressionRowsWasm, WasmCompileError> {
    let layout = VarLayout::from_dae(dae_model);
    let rows = lower_discrete_rhs(dae_model, &layout)?;
    compile_expression_rows_wasm(layout.y_scalars(), layout.p_scalars(), rows)
}

pub fn compile_initial_residual_wasm(
    dae_model: &dae::Dae,
) -> Result<CompiledExpressionRowsWasm, WasmCompileError> {
    let layout = VarLayout::from_dae(dae_model);
    let rows = lower_initial_residual(dae_model, &layout)?;
    compile_expression_rows_wasm(layout.y_scalars(), layout.p_scalars(), rows)
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
    use super::compile_residual_wasm;
    use rumoca_ir_dae as dae;
    use wasmparser::FunctionBody;
    use wasmparser::Parser;
    use wasmparser::Payload;
    use wasmparser::Validator;

    fn scalar_var(name: &str) -> dae::Variable {
        dae::Variable::new(dae::VarName::new(name))
    }

    fn expr_var(name: &str) -> dae::Expression {
        dae::Expression::VarRef {
            name: dae::VarName::new(name),
            subscripts: vec![],
        }
    }

    fn compile_fixture_model() -> super::CompiledResidualWasm {
        let mut dae_model = dae::Dae::default();
        dae_model
            .states
            .insert(dae::VarName::new("x"), scalar_var("x"));
        dae_model
            .algebraics
            .insert(dae::VarName::new("z"), scalar_var("z"));
        dae_model
            .parameters
            .insert(dae::VarName::new("p"), scalar_var("p"));
        dae_model.f_x.push(dae::Equation::residual(
            dae::Expression::Binary {
                op: rumoca_ir_core::OpBinary::Add(Default::default()),
                lhs: Box::new(expr_var("x")),
                rhs: Box::new(dae::Expression::BuiltinCall {
                    function: dae::BuiltinFunction::Sin,
                    args: vec![expr_var("z")],
                }),
            },
            Default::default(),
            "row0",
        ));
        dae_model.f_x.push(dae::Equation::residual(
            dae::Expression::Binary {
                op: rumoca_ir_core::OpBinary::Sub(Default::default()),
                lhs: Box::new(dae::Expression::BuiltinCall {
                    function: dae::BuiltinFunction::Exp,
                    args: vec![expr_var("x")],
                }),
                rhs: Box::new(dae::Expression::Binary {
                    op: rumoca_ir_core::OpBinary::Mul(Default::default()),
                    lhs: Box::new(expr_var("p")),
                    rhs: Box::new(expr_var("z")),
                }),
            },
            Default::default(),
            "row1",
        ));
        compile_residual_wasm(&dae_model).expect("compile wasm residual")
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
}
