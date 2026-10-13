#[cfg(test)]
mod tests;

use std::sync::Arc;

/// A source-issued numeric value: fill and scalar broadcast retain repetition;
/// authored or computed literal data retains its original element cost.
#[derive(Clone, Debug)]
pub struct NumericInitialValues<T = f64> {
    runs: Vec<Run<T>>,
    count: usize,
}
impl<T> Default for NumericInitialValues<T> {
    fn default() -> Self {
        Self {
            runs: Vec::new(),
            count: 0,
        }
    }
}

#[derive(Clone, Debug)]
enum Run<T> {
    Repeat { value: T, count: usize },
    Literal(Arc<[T]>),
}

/// Read-only source runs consumed by the checked Solve storage construction.
pub enum NumericInitialRun<'a, T = f64> {
    Repeat { value: T, count: usize },
    Literal(&'a [T]),
}

impl NumericInitialValues<super::Scalar> {
    /// Numerical view of already issued exact source values, never its inverse.
    pub fn numerical_values(&self) -> NumericInitialValues {
        self.map(super::Scalar::real)
    }
    /// Exact Integer attributes share this one checked numerical projection.
    pub fn numerical_attribute_values(
        &self,
        kind: rumoca_ir_dae::ScalarType,
        span: rumoca_core::Span,
    ) -> Result<NumericInitialValues, super::NumericEvaluationError> {
        if matches!(
            kind,
            rumoca_ir_dae::ScalarType::Integer | rumoca_ir_dae::ScalarType::Enumeration
        ) {
            let representable = |value: super::Scalar| {
                value
                    .integer()
                    .is_some_and(rumoca_core::integer_within_binary64_exact_range)
            };
            if !self.runs().all(|run| match run {
                NumericInitialRun::Repeat { value, .. } => representable(value),
                NumericInitialRun::Literal(values) => values.iter().copied().all(representable),
            }) {
                return Err(super::failure(
                    super::NumericEvaluationErrorKind::UnsupportedOperation,
                    "Integer bound has no exact projection in the numerical attribute profile",
                    span,
                ));
            }
        }
        Ok(self.numerical_values())
    }
}

impl<T: Copy> NumericInitialValues<T> {
    pub(super) fn map<U: Copy>(&self, mut convert: impl FnMut(T) -> U) -> NumericInitialValues<U> {
        NumericInitialValues {
            count: self.count,
            runs: self
                .runs
                .iter()
                .map(|run| match run {
                    Run::Repeat { value, count } => Run::Repeat {
                        value: convert(*value),
                        count: *count,
                    },
                    Run::Literal(values) => Run::Literal(
                        values
                            .iter()
                            .copied()
                            .map(&mut convert)
                            .collect::<Vec<_>>()
                            .into(),
                    ),
                })
                .collect(),
        }
    }
    pub(super) fn append(&mut self, mut other: Self) {
        self.count += other.count;
        self.runs.append(&mut other.runs);
    }
    pub(super) fn slice(&self, start: usize, count: usize) -> Option<Self> {
        let end = start.checked_add(count)?;
        if end > self.count {
            return None;
        }
        let mut result = Self {
            count: 0,
            runs: Vec::new(),
        };
        let mut base = 0;
        for run in &self.runs {
            let extent = match run {
                Run::Repeat { count, .. } => *count,
                Run::Literal(values) => values.len(),
            };
            let from = start.max(base);
            let to = end.min(base + extent);
            if from < to {
                result.append(match run {
                    Run::Repeat { value, .. } => Self::repeat(*value, to - from),
                    Run::Literal(values) => Self::literal(values[from - base..to - base].to_vec()),
                });
            }
            base += extent;
        }
        Some(result)
    }
    pub(super) fn literal(values: Vec<T>) -> Self {
        let count = values.len();
        let runs = if count == 0 {
            Vec::new()
        } else {
            vec![Run::Literal(values.into())]
        };
        Self { runs, count }
    }

    pub(super) fn repeat(value: T, count: usize) -> Self {
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

    pub fn runs(&self) -> impl Iterator<Item = NumericInitialRun<'_, T>> {
        self.runs.iter().map(|run| match run {
            Run::Repeat { value, count } => NumericInitialRun::Repeat {
                value: *value,
                count: *count,
            },
            Run::Literal(values) => NumericInitialRun::Literal(values),
        })
    }

    /// The explicit dense compatibility boundary; it never discovers runs.
    pub fn materialize(&self) -> Vec<T> {
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

    /// Apply source scalar broadcast; other cardinalities remain unchanged.
    pub fn broadcast(&mut self, count: usize) {
        if self.count == 1 && count > 1 {
            let value = self.value(0).expect("one source value");
            *self = Self::repeat(value, count);
        }
    }

    pub(super) fn value(&self, index: usize) -> Option<T> {
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

    pub(super) fn apply_source_overrides(&mut self, overrides: &[(usize, T)]) {
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

fn overridden_literal<T: Copy>(
    values: &Arc<[T]>,
    start: usize,
    next: &mut std::iter::Peekable<impl Iterator<Item = (usize, T)>>,
) -> Arc<[T]> {
    let end = start + values.len();
    let mut values = Arc::clone(values);
    while let Some((index, value)) = next.peek().copied().filter(|(index, _)| *index < end) {
        Arc::make_mut(&mut values)[index - start] = value;
        next.next();
    }
    values
}

fn append_overridden_repeat<T: Copy>(
    runs: &mut Vec<Run<T>>,
    value: T,
    start: usize,
    count: usize,
    next: &mut std::iter::Peekable<impl Iterator<Item = (usize, T)>>,
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
