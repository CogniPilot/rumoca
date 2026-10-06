//! Direct browser-engine execution sharing the caller's wasm_bindgen memory.
mod pool;

use crate::WasmCompileError;

pub(super) struct WasmKernelRuntime {
    eval_function: js_sys::Function,
    // Retain the pool until every function referring to its regions is dropped.
    _arena: Option<pool::ArenaLease>,
}

impl WasmKernelRuntime {
    pub(super) fn new(module_bytes: &[u8]) -> Result<Self, WasmCompileError> {
        Self::new_named(module_bytes, "eval_residual")
    }

    pub(super) fn new_named(module_bytes: &[u8], export: &str) -> Result<Self, WasmCompileError> {
        Self::new_with_arena(module_bytes, export, None)
    }

    pub(super) fn new_with_arena(
        module_bytes: &[u8],
        export: &str,
        arena_bytes: Option<u32>,
    ) -> Result<Self, WasmCompileError> {
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
        let arena = arena_bytes.map(pool::ArenaLease::allocate).transpose()?;
        if let Some(arena) = &arena {
            arena.install(&env)?;
        }
        Reflect::set(&env, &JsValue::from_str("memory"), &wasm_bindgen::memory()).map_err(
            |err| WasmCompileError::Backend(format!("memory import set failed: {err:?}")),
        )?;
        install_math_import(&env, "abs")?;
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
        let eval = Reflect::get(&exports, &JsValue::from_str(export))
            .map_err(|err| WasmCompileError::Backend(format!("missing eval export: {err:?}")))?;
        let eval_function = eval.dyn_into::<js_sys::Function>().map_err(|_| {
            WasmCompileError::Backend(format!("{export} export is not a callable function"))
        })?;
        Ok(Self {
            eval_function,
            _arena: arena,
        })
    }

    pub(super) fn call(
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
    pub(super) fn call_private(
        &self,
        y: &[f64],
        p: &[f64],
        time: f64,
        scratch: &mut [f64],
    ) -> Result<u32, WasmCompileError> {
        self.call_status(y, p, time, scratch)
    }

    pub(super) fn call_assignments(
        &self,
        y: &mut [f64],
        p: &[f64],
        time: f64,
        scratch: &mut [f64],
    ) -> Result<u32, WasmCompileError> {
        self.call_status(y, p, time, scratch)
    }

    fn call_status(
        &self,
        y: &[f64],
        p: &[f64],
        time: f64,
        scratch: &mut [f64],
    ) -> Result<u32, WasmCompileError> {
        use wasm_bindgen::JsValue;
        let y_ptr = ptr_to_wasm_i32(y.as_ptr())?;
        let p_ptr = ptr_to_wasm_i32(p.as_ptr())?;
        let scratch_ptr = ptr_to_wasm_i32(scratch.as_ptr())?;
        let result = self
            .eval_function
            .call5(
                &JsValue::NULL,
                &JsValue::from_f64(f64::from(y_ptr)),
                &JsValue::from_f64(f64::from(p_ptr)),
                &JsValue::from_f64(time),
                &JsValue::from_f64(f64::from(scratch_ptr)),
                &JsValue::from_f64(0.0),
            )
            .map_err(|error| {
                WasmCompileError::Backend(format!("assignment call failed: {error:?}"))
            })?;
        let value = result
            .as_f64()
            .filter(|value| {
                value.is_finite()
                    && value.trunc() == *value
                    && *value >= f64::from(i32::MIN)
                    && *value <= f64::from(i32::MAX)
            })
            .ok_or_else(|| {
                WasmCompileError::Backend("assignment export returned invalid i32 status".into())
            })?;
        Ok(value as i32 as u32)
    }
}

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

fn ptr_to_wasm_i32<T>(ptr: *const T) -> Result<u32, WasmCompileError> {
    u32::try_from(ptr as usize)
        .map_err(|_| WasmCompileError::Backend("pointer offset does not fit wasm32".to_string()))
}
