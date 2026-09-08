//! Streaming CSV trace capture for scheduled simulation frames.

use std::fs::File;
use std::io::{BufWriter, Write};
use std::path::PathBuf;

use anyhow::{Context, Result};
use rumoca_input::{InputEngine, RuntimeContext};

use crate::scenario_config::SimulationConfig;

pub(super) struct TraceLogger {
    writer: BufWriter<File>,
    fields: Vec<String>,
    path: PathBuf,
}

impl TraceLogger {
    fn open(path: PathBuf, fields: Vec<String>) -> Result<Self> {
        let file =
            File::create(&path).with_context(|| format!("Open trace log {}", path.display()))?;
        let mut writer = BufWriter::new(file);
        let header = fields.join(",");
        writeln!(writer, "{header}")?;
        eprintln!("  Trace log: {} ({} columns)", path.display(), fields.len());
        Ok(Self {
            writer,
            fields,
            path,
        })
    }

    pub(super) fn field_names(&self) -> impl Iterator<Item = &str> {
        self.fields.iter().map(String::as_str)
    }

    pub(super) fn record(&mut self, engine: &InputEngine, rt: &RuntimeContext<'_>) -> Result<()> {
        let mut first = true;
        for name in &self.fields {
            if !first {
                self.writer.write_all(b",")?;
            }
            first = false;
            let v = resolve_trace_field(name, engine, rt)?;
            write!(self.writer, "{v}")?;
        }
        self.writer.write_all(b"\n")?;
        Ok(())
    }
}

pub(super) fn open_trace_logger(cfg: &SimulationConfig) -> Result<Option<TraceLogger>> {
    let Some(dbg) = cfg.debug_log.as_ref() else {
        return Ok(None);
    };
    // Default: drop `rumoca_trace.csv` in the cwd so you always have a log to
    // share with no setup. Override with `path = "/path/other.csv"` under the
    // scenario's [debug_log] config.
    let logger = TraceLogger::open(PathBuf::from(dbg.path.clone()), dbg.capture.clone())?;
    Ok(Some(logger))
}

impl Drop for TraceLogger {
    fn drop(&mut self) {
        let _flush_result = self.writer.flush();
        eprintln!("[trace] flushed to {}", self.path.display());
    }
}

/// Resolve a `debug_log.capture` field to an f64 using the same prefix
/// scheme as signal mapper: `model:`, `local:` (supports `.idx`),
/// `runtime:frame_num|wall_ms|input_connected|model_time`. Missing
fn resolve_trace_field(name: &str, engine: &InputEngine, rt: &RuntimeContext<'_>) -> Result<f64> {
    if let Some(rest) = name.strip_prefix("model:") {
        if rest == "time" {
            return Ok(rt.model_time);
        }
        return (rt.model_get)(rest)?
            .ok_or_else(|| anyhow::anyhow!("trace field model:{rest} did not resolve"));
    }
    if let Some(rest) = name.strip_prefix("local:") {
        return engine
            .get(rest)
            .ok_or_else(|| anyhow::anyhow!("trace field local:{rest} did not resolve"));
    }
    if let Some(rest) = name.strip_prefix("runtime:") {
        return match rest {
            "frame_num" => Ok(rt.frame_num as f64),
            "wall_ms" => Ok(rt.wall_ms),
            "input_connected" => Ok(f64::from(u8::from(rt.input_connected))),
            "model_time" => Ok(rt.model_time),
            _ => Err(anyhow::anyhow!("unknown trace runtime field '{rest}'")),
        };
    }
    Err(anyhow::anyhow!(
        "trace field '{name}' must use model:, local:, or runtime:"
    ))
}
