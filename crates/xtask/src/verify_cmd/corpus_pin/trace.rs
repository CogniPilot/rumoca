//! Reading a pinned observation back out of a simulation trace.
//!
//! `rumoca sim -o <file>.csv` writes the raw result table: a `time` column plus
//! one column per reported variable. Variable names contain commas (a matrix
//! element is `a[1,2]`), so the header is quoted CSV and cannot be split on
//! commas, because doing that silently shifts every column after the first matrix
//! entry, which would make a pinned reading compare the wrong variable and
//! still look green. Hence the small quote-aware splitter here.

use anyhow::{Context, Result, bail};
use std::fs;
use std::path::Path;

/// Slack for landing a probe on a sample stamp. Sample stamps come back as
/// `0.0009999999999999998` rather than `0.001`, so an exact comparison would
/// miss the sample the probe names.
const PROBE_EPSILON: f64 = 1.0e-9;

/// A simulation result table.
#[derive(Debug)]
pub(crate) struct Trace {
    names: Vec<String>,
    times: Vec<f64>,
    /// Column-major: `columns[variable][sample]`.
    columns: Vec<Vec<f64>>,
}

impl Trace {
    pub(crate) fn read(path: &Path) -> Result<Self> {
        let raw = fs::read_to_string(path)
            .with_context(|| format!("failed to read simulation trace {}", path.display()))?;
        Self::parse(&raw).with_context(|| format!("failed to parse {}", path.display()))
    }

    pub(crate) fn parse(raw: &str) -> Result<Self> {
        let mut lines = raw.lines().filter(|line| !line.trim().is_empty());
        let Some(header) = lines.next() else {
            bail!("trace is empty");
        };
        let header = split_csv_record(header);
        if header.first().map(String::as_str) != Some("time") {
            bail!(
                "trace's first column is `{}`, expected `time`",
                header.first().map_or("<none>", String::as_str)
            );
        }
        let names = header[1..].to_vec();
        let mut times = Vec::new();
        let mut columns = vec![Vec::new(); names.len()];
        for line in lines {
            push_sample(line, &names, &mut times, &mut columns)?;
        }
        if times.is_empty() {
            bail!("trace carries a header but no samples");
        }
        Ok(Self {
            names,
            times,
            columns,
        })
    }

    pub(crate) fn variable_names(&self) -> &[String] {
        &self.names
    }

    pub(crate) fn stop_time(&self) -> f64 {
        self.times.last().copied().unwrap_or(f64::NAN)
    }

    /// The span between the largest and the smallest finite sample of `name`
    /// over `[0, t_end]`, or `None` when the trace does not carry `name`.
    ///
    /// This is how [`super::observability`] tells a run that moved from one
    /// that stood still, so it reads every sample rather than the two endpoints
    /// a pin happens to probe: a variable that leaves its start value and comes
    /// back has moved, and a trace truncated to one sample has not.
    pub(crate) fn spread_until(&self, name: &str, t_end: f64) -> Option<f64> {
        let index = self.names.iter().position(|candidate| candidate == name)?;
        let column = &self.columns[index];
        let mut span: Option<(f64, f64)> = None;
        for (sample, &stamp) in self.times.iter().enumerate() {
            if stamp > t_end + PROBE_EPSILON {
                break;
            }
            let value = column[sample];
            if !value.is_finite() {
                continue;
            }
            span = Some(match span {
                None => (value, value),
                Some((low, high)) => (low.min(value), high.max(value)),
            });
        }
        span.map(|(low, high)| high - low)
    }

    /// The reported value of `name` at `time`, held right-continuously: the last
    /// sample at or before the probe.
    ///
    /// A probe before the first sample, or a name the trace does not carry, is
    /// an error rather than a default. Both mean the pin no longer describes
    /// this model, which is a finding.
    pub(crate) fn value_at(&self, name: &str, time: f64) -> Result<f64> {
        let Some(index) = self.names.iter().position(|candidate| candidate == name) else {
            bail!(
                "trace does not carry `{name}` (it has {} variables)",
                self.names.len()
            );
        };
        let column = &self.columns[index];
        let mut found = None;
        for (sample, &stamp) in self.times.iter().enumerate() {
            if stamp <= time + PROBE_EPSILON {
                found = Some(column[sample]);
            } else {
                break;
            }
        }
        found.with_context(|| {
            format!(
                "no sample at or before t={time} for `{name}`; the trace starts at t={}",
                self.times.first().copied().unwrap_or(f64::NAN)
            )
        })
    }
}

fn push_sample(
    line: &str,
    names: &[String],
    times: &mut Vec<f64>,
    columns: &mut [Vec<f64>],
) -> Result<()> {
    let fields = split_csv_record(line);
    if fields.len() != names.len() + 1 {
        bail!(
            "sample row has {} fields but the header declares {}",
            fields.len(),
            names.len() + 1
        );
    }
    times.push(parse_field(&fields[0], "time")?);
    for (index, name) in names.iter().enumerate() {
        columns[index].push(parse_field(&fields[index + 1], name)?);
    }
    Ok(())
}

fn parse_field(field: &str, name: &str) -> Result<f64> {
    field
        .trim()
        .parse::<f64>()
        .with_context(|| format!("`{name}` sample `{field}` is not a number"))
}

/// Split one CSV record into fields, honoring double quotes and `""` escapes.
pub(crate) fn split_csv_record(line: &str) -> Vec<String> {
    let mut fields = Vec::new();
    let mut current = String::new();
    let mut quoted = false;
    let mut characters = line.chars().peekable();
    while let Some(character) = characters.next() {
        match (character, quoted) {
            ('"', true) if characters.peek() == Some(&'"') => {
                characters.next();
                current.push('"');
            }
            ('"', _) => quoted = !quoted,
            (',', false) => fields.push(std::mem::take(&mut current)),
            (other, _) => current.push(other),
        }
    }
    fields.push(current);
    fields
}
