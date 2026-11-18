//! Direct Wasm-to-Wasm links; exact SDK memory and math ABI are shared.
use super::call::{FmiDerivativeCall, FmiKinematicCall};
use super::{PrecompiledFmiDerivativeOperation, WasmCompileError};
use crate::wasm_runtime::{
    exported_function, instantiate, instantiate_with_operations, ptr_to_wasm_i32, status_value,
};
use wasm_bindgen::{JsCast, JsValue, prelude::wasm_bindgen};

// This is exactly the fixed signature printed by the source-issued entry.
#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(extends = js_sys::Function, js_name = Function)]
    type DerivativeEntry;
    #[wasm_bindgen(method, catch, js_name = call)]
    fn invoke(
        this: &DerivativeEntry,
        context: &JsValue,
        y: u32,
        parameters: u32,
        time: f64,
        output: u32,
        workspace: u32,
        cached: u32,
        present: u32,
        keys: u32,
        valid: u32,
    ) -> Result<JsValue, JsValue>;
    #[wasm_bindgen(extends = js_sys::Function, js_name = Function)]
    type KinematicEntry;
    #[wasm_bindgen(method, catch, js_name = call)]
    fn invoke_kinematic(
        this: &KinematicEntry,
        context: &JsValue,
        y: u32,
        parameters: u32,
        time: f64,
        output: u32,
        workspace: u32,
    ) -> Result<JsValue, JsValue>;
}

pub(super) struct DerivativeRuntime {
    entry: DerivativeEntry,
    kinematic: Option<KinematicEntry>,
}
impl DerivativeRuntime {
    pub(super) fn new(code: &PrecompiledFmiDerivativeOperation) -> Result<Self, WasmCompileError> {
        let operations = js_sys::Object::new();
        for (name, payload, export) in [
            (
                "static_refresh",
                code.static_program.as_ref(),
                "eval_assignments",
            ),
            (
                "dynamic_refresh",
                code.dynamic_program.as_ref(),
                "eval_assignments",
            ),
            ("values", Some(&code.values), "eval_values"),
            ("kinematic_values", code.kinematic.as_ref(), "eval_values"),
        ] {
            if let Some(payload) = payload {
                let instance = instantiate(payload.module_bytes())?;
                let entry = exported_function(&instance, export)?;
                js_sys::Reflect::set(&operations, &JsValue::from_str(name), &entry).map_err(
                    |e| WasmCompileError::Backend(format!("derivative import {name}: {e:?}")),
                )?;
            }
        }
        let instance = instantiate_with_operations(&code.bytes, &operations)?;
        let kinematic = code
            .kinematic
            .as_ref()
            .map(|_| exported_function(&instance, "eval_kinematic"))
            .transpose()?
            .map(JsCast::unchecked_into);
        Ok(Self {
            entry: exported_function(&instance, "eval_derivatives")?.unchecked_into(),
            kinematic,
        })
    }

    pub(super) fn call(&self, frame: &mut FmiDerivativeCall<'_>) -> Result<u32, WasmCompileError> {
        let _span = rumoca_core::browser_profile::Span::new("complete_derivative_bridge_call");
        let result = self
            .entry
            .invoke(
                &JsValue::NULL,
                ptr_to_wasm_i32(frame.y.as_ptr())?,
                ptr_to_wasm_i32(frame.parameters.as_ptr())?,
                frame.time,
                ptr_to_wasm_i32(frame.output.as_ptr())?,
                ptr_to_wasm_i32(frame.workspace.as_ptr())?,
                ptr_to_wasm_i32(frame.cache_values.as_ptr())?,
                ptr_to_wasm_i32(frame.cache_present.as_ptr())?,
                ptr_to_wasm_i32(frame.cache_parameters.as_ptr())?,
                u32::from(*frame.cache_valid),
            )
            .map_err(|error| {
                WasmCompileError::Backend(format!("complete derivative call failed: {error:?}"))
            })?;
        status_value(result)
    }
    pub(super) fn call_kinematic(
        &self,
        frame: &mut FmiKinematicCall<'_>,
    ) -> Result<u32, WasmCompileError> {
        let entry = self
            .kinematic
            .as_ref()
            .ok_or_else(|| WasmCompileError::Input("no issued kinematic entry".into()))?;
        let result = entry
            .invoke_kinematic(
                &JsValue::NULL,
                ptr_to_wasm_i32(frame.y.as_ptr())?,
                ptr_to_wasm_i32(frame.parameters.as_ptr())?,
                frame.time,
                ptr_to_wasm_i32(frame.output.as_ptr())?,
                ptr_to_wasm_i32(frame.workspace.as_ptr())?,
            )
            .map_err(|error| {
                WasmCompileError::Backend(format!("kinematic derivative call failed: {error:?}"))
            })?;
        status_value(result)
    }
}

/// The issuer's active phase is the only authority to request this recovery.
pub(super) fn restore_coordinate(
    frame: &mut FmiDerivativeCall<'_>,
    workspace: rumoca_ir_solve::fmi::FmiDerivativeWorkspace,
) -> Result<(), WasmCompileError> {
    use wasm_bindgen::JsCast;
    let y = ptr_to_wasm_i32(frame.y.as_ptr())?;
    let work = ptr_to_wasm_i32(frame.workspace.as_ptr())?;
    let start = work
        .checked_add(workspace.snapshot())
        .ok_or_else(|| WasmCompileError::Input("rollback start overflow".into()))?;
    let bytes = workspace.y_bytes();
    let end = start
        .checked_add(bytes)
        .ok_or_else(|| WasmCompileError::Input("rollback end overflow".into()))?;
    let memory = wasm_bindgen::memory().unchecked_into::<js_sys::WebAssembly::Memory>();
    let view = js_sys::Uint8Array::new(&memory.buffer());
    let saved = view.subarray(start, end);
    view.set(&saved, y);
    Ok(())
}
