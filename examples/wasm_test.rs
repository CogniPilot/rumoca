//! WASM compilation test stub.
//!
//! This example tests that rumoca compiles correctly for WASM targets.
//!
//! ## Building
//! ```sh
//! # Using wasm-pack (recommended):
//! RUSTFLAGS='-C target-feature=+atomics,+bulk-memory,+mutable-globals' \
//!   wasm-pack build --target web \
//!   --no-default-features --features wasm \
//!   -- -Z build-std=std,panic_abort
//!
//! # Or using cargo directly:
//! RUSTFLAGS='-C target-feature=+atomics,+bulk-memory,+mutable-globals --cfg getrandom_backend="wasm_js"' \
//!   cargo +nightly build --example wasm_test --target wasm32-unknown-unknown \
//!   --no-default-features --features wasm -Z build-std=std,panic_abort
//! ```
//!
//! ## Usage from JavaScript
//! ```js
//! import init, { wasm_init, parse_modelica, compile_to_json } from './pkg/rumoca.js';
//!
//! await init();
//! await wasm_init(navigator.hardwareConcurrency || 4);
//!
//! const ok = parse_modelica("model M Real x; equation der(x) = 1; end M;");
//! const json = compile_to_json("model M Real x; equation der(x) = 1; end M;", "M");
//! ```

use rumoca::{Compiler, parse_source_simple};

#[cfg(feature = "wasm")]
use wasm_bindgen::prelude::*;

/// Initialize the thread pool for parallel processing.
///
/// Call this once from JavaScript before using other functions.
/// The `num_threads` parameter specifies how many Web Workers to spawn.
///
/// Note: Requires SharedArrayBuffer support, which needs these HTTP headers:
/// - Cross-Origin-Opener-Policy: same-origin
/// - Cross-Origin-Embedder-Policy: require-corp
#[cfg(feature = "wasm")]
#[wasm_bindgen]
pub fn wasm_init(num_threads: usize) -> Result<(), JsValue> {
    // Initialize rayon thread pool for WASM
    wasm_bindgen_rayon::init_thread_pool(num_threads)
}

/// Parse Modelica source and return whether it's valid.
#[cfg_attr(feature = "wasm", wasm_bindgen)]
pub fn parse_modelica(source: &str) -> bool {
    parse_source_simple(source, "<wasm>").is_some()
}

/// Compile a Modelica model and return DAE as JSON.
///
/// # Arguments
/// * `source` - Modelica source code
/// * `model_name` - Name of the model to compile (e.g., "Ball")
///
/// # Returns
/// JSON string containing the DAE representation, or error message.
#[cfg_attr(feature = "wasm", wasm_bindgen)]
pub fn compile_to_json(source: &str, model_name: &str) -> Result<String, String> {
    let result = Compiler::new()
        .model(model_name)
        .cache(false) // No filesystem in WASM
        .compile_str(source, "<wasm>")
        .map_err(|e| e.to_string())?;

    result.to_dae_ir_json().map_err(|e| e.to_string())
}

// For non-WASM testing
#[cfg(not(target_arch = "wasm32"))]
fn main() {
    let source = r#"
        model Integrator
            Real x(start = 0);
        equation
            der(x) = 1;
        end Integrator;
    "#;

    println!("Testing parse_modelica...");
    let parsed = parse_modelica(source);
    println!("  Parse result: {}", parsed);

    println!("Testing compile_to_json...");
    match compile_to_json(source, "Integrator") {
        Ok(json) => println!(
            "  Compiled successfully!\n  JSON length: {} bytes",
            json.len()
        ),
        Err(e) => println!("  Error: {}", e),
    }
}

// WASM entry point (no main needed)
#[cfg(target_arch = "wasm32")]
fn main() {}
