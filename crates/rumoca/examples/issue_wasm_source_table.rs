//! Issue one actual source variant through the canonical compiler and Solve owner.
//! `regenerate_source_tables.mjs` records the build/source/binary evidence.

use std::{env, fs, path::Path, time::Instant};

fn main() -> anyhow::Result<()> {
    let arguments = env::args().skip(1).collect::<Vec<_>>();
    let [source_path, model, variant, output_path] = arguments.as_slice() else {
        anyhow::bail!("SOURCE MODEL VARIANT OUTPUT_JSON required");
    };
    let source = fs::read_to_string(source_path)?;
    let document = Path::new(source_path)
        .file_name()
        .and_then(|name| name.to_str())
        .ok_or_else(|| anyhow::anyhow!("source filename must be UTF-8"))?;
    let started = Instant::now();
    let mut session = rumoca_compile::Session::default();
    session.add_document(document, &source)?;
    let result = session.compile_model(model)?;
    let table = rumoca_phase_solve::lower_solve_package(&result.dae)?.pure_calls;
    let artifact = serde_json::json!({
        "variant": variant,
        "model": model,
        "source": source,
        "sourceSha256": env::var("RUMOCA_MODEL_SOURCE_SHA256")?,
        "compilerRevision": env::var("RUMOCA_PRODUCER_REVISION")?,
        "compilerSourceSha256": env::var("RUMOCA_PRODUCER_SOURCE_SHA256")?,
        "producerBinarySha256": env::var("RUMOCA_PRODUCER_BINARY_SHA256")?,
        "daeSchemaVersion": result.dae.schema_version(),
        "solveSchemaVersion": rumoca_ir_solve::SOLVE_SCHEMA_VERSION,
        "elapsedMs": started.elapsed().as_secs_f64() * 1000.0,
        "table": table,
    });
    // Rust serializes exact i64 payloads. The orchestrator only hashes/copies
    // these bytes and never parses and reserializes the typed table in JS.
    fs::write(output_path, serde_json::to_vec(&artifact)?)?;
    Ok(())
}
