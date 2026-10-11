//! Issue one actual source variant through the canonical compiler and Solve owner.
//! `regenerate_source_tables.mjs` records the build/source/binary evidence.

use std::{fs, path::PathBuf, time::Instant};

use clap::Parser;

#[derive(Parser)]
#[command(about = "Issue a canonical source table with measured producer provenance")]
struct Arguments {
    /// Actual Modelica source variant to compile.
    source_path: PathBuf,
    /// Model entrypoint in the source.
    model: String,
    /// Variant identity retained by the regeneration orchestrator.
    variant: String,
    /// Destination for the exact Rust-serialized table.
    output_path: PathBuf,
    /// SHA-256 of the actual Modelica source bytes.
    #[arg(long)]
    source_sha256: String,
    /// Git revision of the source-sealed compiler checkout.
    #[arg(long)]
    compiler_revision: String,
    /// SHA-256 of the actual tracked compiler-source manifest.
    #[arg(long)]
    compiler_source_sha256: String,
    /// SHA-256 of this actual built producer executable.
    #[arg(long)]
    producer_binary_sha256: String,
}

fn main() -> anyhow::Result<()> {
    let arguments = Arguments::parse();
    let source = fs::read_to_string(&arguments.source_path)?;
    let document = arguments
        .source_path
        .file_name()
        .and_then(|name| name.to_str())
        .ok_or_else(|| anyhow::anyhow!("source filename must be UTF-8"))?;
    let started = Instant::now();
    let mut session = rumoca_compile::Session::default();
    session.add_document(document, &source)?;
    let result = session.compile_model(&arguments.model)?;
    let table = rumoca_phase_solve::lower_solve_package(&result.dae)?.pure_calls;
    let artifact = serde_json::json!({
        "variant": arguments.variant,
        "model": arguments.model,
        "source": source,
        "sourceSha256": arguments.source_sha256,
        "compilerRevision": arguments.compiler_revision,
        "compilerSourceSha256": arguments.compiler_source_sha256,
        "producerBinarySha256": arguments.producer_binary_sha256,
        "daeSchemaVersion": result.dae.schema_version(),
        "solveSchemaVersion": rumoca_ir_solve::SOLVE_SCHEMA_VERSION,
        "elapsedMs": started.elapsed().as_secs_f64() * 1000.0,
        "table": table,
    });
    // Rust serializes exact i64 payloads. The orchestrator only hashes/copies
    // these bytes and never parses and reserializes the typed table in JS.
    fs::write(arguments.output_path, serde_json::to_vec(&artifact)?)?;
    Ok(())
}
