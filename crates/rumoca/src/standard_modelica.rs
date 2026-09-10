//! `compile --emit-standard-modelica`: the portable expansion of one source.
//!
//! This is a source-to-source step. It runs the expansion the compiler runs
//! when it stores a document, then writes the result instead of continuing to
//! flatten, so the artifact is exactly the program the compiler compiled and
//! any Modelica tool can elaborate it.

use std::path::Path;

use anyhow::{Context, Result, bail};

/// Write the expanded standard Modelica of `model_file` to `output`, or stdout.
pub(crate) fn run(model_file: &str, output: Option<&Path>) -> Result<()> {
    let path = Path::new(model_file);
    let source = std::fs::read_to_string(path)
        .with_context(|| format!("Read Modelica input '{}'", path.display()))?;
    let expanded =
        rumoca_compile::parsing::expand_source_to_standard_modelica(&source, model_file)?;
    let Some(output) = output else {
        print!("{expanded}");
        return Ok(());
    };
    if crate::cli::output_names_input_file(output, path)? {
        bail!(
            "output path `{}` is the Modelica input file; refusing to overwrite the source",
            output.display()
        );
    }
    std::fs::write(output, &expanded).with_context(|| format!("write {}", output.display()))?;
    eprintln!("wrote standard Modelica to {}", output.display());
    Ok(())
}
