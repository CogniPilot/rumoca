//! Compile a Modelica model from a file.
//!
//! Usage:
//! - Default demo model:
//!   `cargo run --example file_compilation -p rumoca`
//! - Custom file/model:
//!   `cargo run --example file_compilation -p rumoca -- path/to/model.mo ModelName`

use std::env;
use std::fs;
use std::path::{Path, PathBuf};

use rumoca::Compiler;

const DEFAULT_MODEL_NAME: &str = "FileExample";
const DEFAULT_MODEL_SOURCE: &str = r#"
model FileExample
    Real x(start = 0.0);
equation
    der(x) = 2.0;
end FileExample;
"#;

fn write_default_model_file() -> anyhow::Result<PathBuf> {
    let path = env::temp_dir().join(format!("rumoca_file_example_{}.mo", std::process::id()));
    fs::write(&path, DEFAULT_MODEL_SOURCE)?;
    Ok(path)
}

fn path_to_str(path: &Path) -> anyhow::Result<&str> {
    path.to_str()
        .ok_or_else(|| anyhow::anyhow!("non-utf8 path: {}", path.display()))
}

fn main() -> anyhow::Result<()> {
    let args: Vec<String> = env::args().collect();

    let (model_file, cleanup_file) = if let Some(path) = args.get(1) {
        (PathBuf::from(path), None)
    } else {
        let generated = write_default_model_file()?;
        (generated.clone(), Some(generated))
    };

    let model_name = args
        .get(2)
        .map(String::as_str)
        .unwrap_or(DEFAULT_MODEL_NAME);

    println!(
        "Compiling {} from {}",
        model_name,
        model_file.to_string_lossy()
    );

    let result = Compiler::new()
        .model(model_name)
        .compile_file(path_to_str(&model_file)?)?;

    println!("States: {}", result.dae.states.len());
    println!("Algebraics: {}", result.dae.algebraics.len());
    println!("f_x equations: {}", result.dae.f_x.len());
    println!("Balanced: {}", result.dae.is_balanced());

    if let Some(path) = cleanup_file {
        let _ = fs::remove_file(path);
    }

    Ok(())
}
