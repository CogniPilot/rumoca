/// Report an unavailable CPU tool in portable test runs and fail closed in the
/// required MLIR CPU lane selected by the `required-mlir-cpu` Cargo feature.
pub(crate) fn missing_cpu_tool(tool: &str) {
    #[cfg(feature = "required-mlir-cpu")]
    panic!("required MLIR CPU tool is unavailable: {tool}");

    #[cfg(not(feature = "required-mlir-cpu"))]
    eprintln!("SKIP: {tool} not found");
}
