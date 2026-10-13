//! Unchanged source-function control under MLS §11.2.2/§11.2.6 and SPEC_0043 §4.

use super::assert_runtime_call_retained;
use rumoca::Compiler;

const SOURCE: &str = include_str!("../../fixtures/function_guarded_loops/ExactRaster.mo");

#[test]
fn unchanged_raster_function_compiles_and_retains_its_source_call() {
    let compiled = Compiler::new()
        .model("ExactRaster")
        .compile_str(SOURCE, "ExactRaster.mo")
        .unwrap_or_else(|error| {
            panic!("the original selected raster function stays defined: {error}")
        });
    assert_runtime_call_retained(&compiled.dae);
}
