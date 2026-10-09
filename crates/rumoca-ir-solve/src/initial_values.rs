//! Source-issued initialization runs and their explicit dense runtime view.

#[cfg(test)]
mod tests;

use std::ops::Deref;
use std::sync::{Arc, OnceLock};

use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, thiserror::Error)]
#[error("invalid initialization buffer: {0}")]
pub struct SolveInitialValuesError(&'static str);

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case", deny_unknown_fields)]
enum Run {
    Repeat {
        start: usize,
        count: usize,
        bits: u64,
    },
    Literal {
        start: usize,
        bits: Vec<u64>,
    },
}

impl Run {
    fn start(&self) -> usize {
        match self {
            Self::Repeat { start, .. } | Self::Literal { start, .. } => *start,
        }
    }

    fn count(&self) -> usize {
        match self {
            Self::Repeat { count, .. } => *count,
            Self::Literal { bits, .. } => bits.len(),
        }
    }

    fn value(&self, ordinal: usize) -> f64 {
        f64::from_bits(match self {
            Self::Repeat { bits, .. } => *bits,
            Self::Literal { bits, .. } => bits[ordinal],
        })
    }

    fn append_dense(&self, values: &mut Vec<f64>) {
        match self {
            Self::Repeat { count, bits, .. } => {
                values.resize(values.len() + count, f64::from_bits(*bits))
            }
            Self::Literal { bits, .. } => values.extend(bits.iter().copied().map(f64::from_bits)),
        }
    }

    fn slice(&self, start: usize, count: usize, target: usize) -> Self {
        match self {
            Self::Repeat { bits, .. } => Self::Repeat {
                start: target,
                count,
                bits: *bits,
            },
            Self::Literal { bits, .. } => Self::Literal {
                start: target,
                bits: bits[start..start + count].to_vec(),
            },
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
struct Plan {
    count: usize,
    runs: Vec<Run>,
}

impl Plan {
    fn check(&self) -> Result<(), SolveInitialValuesError> {
        self.count
            .checked_mul(std::mem::size_of::<f64>())
            .filter(|bytes| *bytes <= isize::MAX as usize)
            .ok_or(SolveInitialValuesError("capacity overflow"))?;
        let mut end = 0usize;
        for run in &self.runs {
            if run.start() != end || run.count() == 0 {
                return Err(SolveInitialValuesError(
                    "runs are not a complete ordered partition",
                ));
            }
            end = end
                .checked_add(run.count())
                .ok_or(SolveInitialValuesError("run extent overflow"))?;
        }
        if end != self.count {
            return Err(SolveInitialValuesError("run capacity mismatch"));
        }
        Ok(())
    }
}

/// The sole initialization-value owner. Runs come from source evaluation or
/// explicit literal data; dense compatibility reads never infer repetition.
#[derive(Debug)]
pub struct SolveInitialValues {
    plan: Arc<Plan>,
    dense: OnceLock<Vec<f64>>,
}

impl Clone for SolveInitialValues {
    fn clone(&self) -> Self {
        Self {
            plan: Arc::clone(&self.plan),
            dense: OnceLock::new(),
        }
    }
}

impl Default for SolveInitialValues {
    fn default() -> Self {
        Self {
            plan: Arc::new(Plan {
                count: 0,
                runs: Vec::new(),
            }),
            dense: OnceLock::new(),
        }
    }
}

impl SolveInitialValues {
    fn construct(plan: Plan) -> Result<Self, SolveInitialValuesError> {
        plan.check()?;
        Ok(Self {
            plan: Arc::new(plan),
            dense: OnceLock::new(),
        })
    }

    pub fn repeat(value: f64, count: usize) -> Result<Self, SolveInitialValuesError> {
        let runs = if count == 0 {
            Vec::new()
        } else {
            vec![Run::Repeat {
                start: 0,
                count,
                bits: value.to_bits(),
            }]
        };
        Self::construct(Plan { count, runs })
    }

    pub fn len(&self) -> usize {
        self.plan.count
    }
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
    pub fn has_dense_view(&self) -> bool {
        self.dense.get().is_some()
    }

    /// Check finite initialization admission without forming a dense view.
    pub fn require_finite(&self) -> Result<(), SolveInitialValuesError> {
        let finite = self.plan.runs.iter().all(|run| match run {
            Run::Repeat { bits, .. } => f64::from_bits(*bits).is_finite(),
            Run::Literal { bits, .. } => bits.iter().all(|bits| f64::from_bits(*bits).is_finite()),
        });
        finite
            .then_some(())
            .ok_or(SolveInitialValuesError("non-finite initialization value"))
    }

    pub fn value(&self, index: usize) -> Option<f64> {
        let position = self
            .plan
            .runs
            .partition_point(|run| run.start() <= index)
            .checked_sub(1)?;
        let run = self.plan.runs.get(position)?;
        let ordinal = index.checked_sub(run.start())?;
        (ordinal < run.count()).then(|| run.value(ordinal))
    }

    pub fn as_slice(&self) -> &[f64] {
        self.dense.get_or_init(|| {
            let mut values = Vec::with_capacity(self.len());
            for run in &self.plan.runs {
                run.append_dense(&mut values);
            }
            values
        })
    }

    pub fn to_vec(&self) -> Vec<f64> {
        self.as_slice().to_vec()
    }

    /// Apply a source-issued update, retaining untouched runs and their order.
    pub fn replace(&mut self, start: usize, values: &Self) -> Result<(), SolveInitialValuesError> {
        let end = start
            .checked_add(values.len())
            .filter(|end| *end <= self.len())
            .ok_or(SolveInitialValuesError(
                "source write is outside buffer capacity",
            ))?;
        if values.is_empty() {
            return Ok(());
        }
        let mut runs = Vec::new();
        for run in &self.plan.runs {
            if run.start() >= start {
                break;
            }
            let count = run.count().min(start - run.start());
            runs.push(run.slice(0, count, run.start()));
        }
        for run in &values.plan.runs {
            runs.push(run.slice(0, run.count(), start + run.start()));
        }
        for run in &self.plan.runs {
            let run_end = run.start() + run.count();
            if run_end <= end {
                continue;
            }
            let first = run.start().max(end);
            runs.push(run.slice(first - run.start(), run_end - first, first));
        }
        *self = Self::construct(Plan {
            count: self.len(),
            runs,
        })?;
        Ok(())
    }

    pub fn set(&mut self, index: usize, value: f64) -> Result<(), SolveInitialValuesError> {
        self.replace(index, &Self::repeat(value, 1)?)
    }

    pub fn concatenate(
        values: impl IntoIterator<Item = Self>,
    ) -> Result<Self, SolveInitialValuesError> {
        let mut count = 0usize;
        let mut runs = Vec::new();
        for value in values {
            let next = count
                .checked_add(value.len())
                .ok_or(SolveInitialValuesError("source extent overflow"))?;
            for run in &value.plan.runs {
                runs.push(run.slice(0, run.count(), count + run.start()));
            }
            count = next;
        }
        Self::construct(Plan { count, runs })
    }

    pub fn runs(&self) -> impl ExactSizeIterator<Item = SolveInitialValueRun<'_>> {
        self.plan
            .runs
            .iter()
            .map(|run| SolveInitialValueRun { run })
    }

    pub fn run_count(&self) -> usize {
        self.plan.runs.len()
    }
    pub fn run(&self, index: usize) -> Option<SolveInitialValueRun<'_>> {
        self.plan
            .runs
            .get(index)
            .map(|run| SolveInitialValueRun { run })
    }
}

/// A borrowed issued run; offsets and extents cannot be supplied separately.
#[derive(Clone, Copy, Debug)]
pub struct SolveInitialValueRun<'a> {
    run: &'a Run,
}

impl<'a> SolveInitialValueRun<'a> {
    pub fn start(self) -> usize {
        self.run.start()
    }
    pub fn count(self) -> usize {
        self.run.count()
    }
    pub fn repeated_bits(self) -> Option<u64> {
        match self.run {
            Run::Repeat { bits, .. } => Some(*bits),
            Run::Literal { .. } => None,
        }
    }
    pub fn literal_bits(self) -> Option<&'a [u64]> {
        match self.run {
            Run::Literal { bits, .. } => Some(bits),
            Run::Repeat { .. } => None,
        }
    }
}

impl Deref for SolveInitialValues {
    type Target = [f64];
    fn deref(&self) -> &Self::Target {
        self.as_slice()
    }
}

impl From<Vec<f64>> for SolveInitialValues {
    fn from(values: Vec<f64>) -> Self {
        let count = values.len();
        let runs = if count == 0 {
            Vec::new()
        } else {
            vec![Run::Literal {
                start: 0,
                bits: values.into_iter().map(f64::to_bits).collect(),
            }]
        };
        Self {
            plan: Arc::new(Plan { count, runs }),
            dense: OnceLock::new(),
        }
    }
}

impl PartialEq<Vec<f64>> for SolveInitialValues {
    fn eq(&self, other: &Vec<f64>) -> bool {
        self.as_slice() == other
    }
}

impl PartialEq for SolveInitialValues {
    fn eq(&self, other: &Self) -> bool {
        self.plan == other.plan
    }
}

impl Eq for SolveInitialValues {}

impl Serialize for SolveInitialValues {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        self.plan.serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for SolveInitialValues {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        Self::construct(Plan::deserialize(deserializer)?).map_err(serde::de::Error::custom)
    }
}
