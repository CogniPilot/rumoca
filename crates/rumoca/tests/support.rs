use rumoca::{CompilationResult, Compiler};

pub fn compile_model_result(
    source: &str,
    model: &str,
    filename: &str,
) -> Result<CompilationResult, String> {
    Compiler::new()
        .model(model)
        .compile_str(source, filename)
        .map_err(|err| format!("{err:?}"))
}

pub fn compile_model_ok(source: &str, model: &str, filename: &str) -> CompilationResult {
    compile_model_result(source, model, filename)
        .unwrap_or_else(|err| panic!("Compilation failed for model {model} in {filename}: {err}"))
}
