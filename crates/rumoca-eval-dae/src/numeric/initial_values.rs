#[cfg(test)]
mod tests;

use std::sync::Arc;

/// A source-issued numeric value: fill and scalar broadcast retain repetition;
/// authored or computed literal data retains its original element cost.
#[derive(Clone, Debug, Default)]
pub struct NumericInitialValues {
    runs: Vec<Run>,
    count: usize,
}

#[derive(Clone, Debug)]
enum Run {
    Repeat { value: f64, count: usize },
    Literal(Arc<[f64]>),
}

/// Read-only source runs consumed by the checked Solve storage construction.
pub enum NumericInitialRun<'a> {
    Repeat { value: f64, count: usize },
    Literal(&'a [f64]),
}

impl NumericInitialValues {
    pub(super) fn literal(values: Vec<f64>) -> Self {
        let count = values.len();
        let runs = if count == 0 {
            Vec::new()
        } else {
            vec![Run::Literal(values.into())]
        };
        Self { runs, count }
    }

    pub(super) fn repeat(value: f64, count: usize) -> Self {
        let runs = if count == 0 {
            Vec::new()
        } else {
            vec![Run::Repeat { value, count }]
        };
        Self { runs, count }
    }

    pub fn len(&self) -> usize {
        self.count
    }

    pub fn is_empty(&self) -> bool {
        self.count == 0
    }

    pub fn runs(&self) -> impl Iterator<Item = NumericInitialRun<'_>> {
        self.runs.iter().map(|run| match run {
            Run::Repeat { value, count } => NumericInitialRun::Repeat {
                value: *value,
                count: *count,
            },
            Run::Literal(values) => NumericInitialRun::Literal(values),
        })
    }

    /// The explicit dense compatibility boundary; it never discovers runs.
    pub fn materialize(&self) -> Vec<f64> {
        let mut values = Vec::with_capacity(self.count);
        for run in &self.runs {
            match run {
                Run::Repeat { value, count } => {
                    values.resize(values.len() + count, *value);
                }
                Run::Literal(elements) => values.extend_from_slice(elements),
            }
        }
        values
    }

    pub(super) fn broadcast(&mut self, count: usize) {
        if self.count == 1 && count > 1 {
            let value = self.value(0).expect("one source value");
            *self = Self::repeat(value, count);
        }
    }

    pub(super) fn value(&self, index: usize) -> Option<f64> {
        let mut remaining = index;
        for run in &self.runs {
            let count = match run {
                Run::Repeat { count, .. } => *count,
                Run::Literal(values) => values.len(),
            };
            if remaining < count {
                return Some(match run {
                    Run::Repeat { value, .. } => *value,
                    Run::Literal(values) => values[remaining],
                });
            }
            remaining = remaining.checked_sub(count)?;
        }
        None
    }

    pub(super) fn all_finite(&self) -> bool {
        self.runs.iter().all(|run| match run {
            Run::Repeat { value, .. } => value.is_finite(),
            Run::Literal(values) => values.iter().all(|value| value.is_finite()),
        })
    }

    pub(super) fn apply_source_overrides(&mut self, overrides: &[(usize, f64)]) {
        let mut changed = Vec::new();
        let mut next = overrides.iter().copied().peekable();
        let mut start = 0usize;
        for run in &self.runs {
            match run {
                Run::Repeat { value, count } => {
                    append_overridden_repeat(&mut changed, *value, start, *count, &mut next);
                    start += count;
                }
                Run::Literal(values) => {
                    let end = start + values.len();
                    changed.push(Run::Literal(overridden_literal(values, start, &mut next)));
                    start = end;
                }
            }
        }
        self.runs = changed;
    }
}

fn overridden_literal(
    values: &Arc<[f64]>,
    start: usize,
    next: &mut std::iter::Peekable<impl Iterator<Item = (usize, f64)>>,
) -> Arc<[f64]> {
    let end = start + values.len();
    let mut values = Arc::clone(values);
    while let Some((index, value)) = next.peek().copied().filter(|(index, _)| *index < end) {
        Arc::make_mut(&mut values)[index - start] = value;
        next.next();
    }
    values
}

fn append_overridden_repeat(
    runs: &mut Vec<Run>,
    value: f64,
    start: usize,
    count: usize,
    next: &mut std::iter::Peekable<impl Iterator<Item = (usize, f64)>>,
) {
    let end = start + count;
    let mut cursor = start;
    while let Some((index, _)) = next.peek().copied().filter(|(index, _)| *index < end) {
        if index > cursor {
            runs.push(Run::Repeat {
                value,
                count: index - cursor,
            });
        }
        let mut literals = Vec::new();
        cursor = index;
        while let Some((_, value)) = next
            .peek()
            .copied()
            .filter(|(index, _)| *index == cursor && *index < end)
        {
            literals.push(value);
            cursor += 1;
            next.next();
        }
        runs.push(Run::Literal(literals.into()));
    }
    if cursor < end {
        runs.push(Run::Repeat {
            value,
            count: end - cursor,
        });
    }
}
