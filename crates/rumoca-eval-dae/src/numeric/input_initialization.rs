//! Export defaults retain source absence and actual supply before numeric projection.

use super::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InputOrigin {
    Binding,
    Start,
    Supplied,
    MlsFallback,
}

#[derive(Debug, Clone)]
pub struct InputOriginRun {
    start: usize,
    count: usize,
    origin: InputOrigin,
    span: Span,
}

impl InputOriginRun {
    pub fn start(&self) -> usize {
        self.start
    }
    pub fn count(&self) -> usize {
        self.count
    }
    pub fn origin(&self) -> InputOrigin {
        self.origin
    }
    pub fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone)]
pub struct InputFallbackBounds {
    minimum: Option<SourceValues>,
    maximum: Option<SourceValues>,
    minimum_span: Option<Span>,
    maximum_span: Option<Span>,
}

impl InputFallbackBounds {
    pub fn minimum(&self) -> Option<&SourceValues> {
        self.minimum.as_ref()
    }
    pub fn maximum(&self) -> Option<&SourceValues> {
        self.maximum.as_ref()
    }
    pub fn minimum_span(&self) -> Option<Span> {
        self.minimum_span
    }
    pub fn maximum_span(&self) -> Option<Span> {
        self.maximum_span
    }
}

/// Issued only by the original numeric source owner; fields cannot be forged.
#[derive(Debug, Clone)]
pub struct InputInitialization {
    source: Option<SourceValues>,
    effective: SourceValues,
    origins: Vec<InputOriginRun>,
    fallback: Option<InputFallbackBounds>,
}

impl InputInitialization {
    pub fn source(&self) -> Option<&SourceValues> {
        self.source.as_ref()
    }
    pub fn effective(&self) -> &SourceValues {
        &self.effective
    }
    pub fn origins(&self) -> &[InputOriginRun] {
        &self.origins
    }
    pub fn fallback(&self) -> Option<&InputFallbackBounds> {
        self.fallback.as_ref()
    }
    pub fn numerical_values(&self) -> NumericInitialValues {
        self.effective.map(Scalar::real)
    }
}

impl<'dae, F> NumericEvaluator<'dae, F>
where
    F: FnMut(dae::VariableView<'dae>, usize) -> Option<f64>,
{
    /// Retain the exact source and effective values in the same invocation/cache.
    pub fn input_initialization(
        &mut self,
        id: dae::VariableId<'dae>,
    ) -> Result<InputInitialization, NumericEvaluationError> {
        let variable = self.view.variable(id).expect("branded variable resolves");
        if variable.role() != dae::VariableRole::Input
            || self.input_policy != InputInitializationPolicy::FmiExportDefaults
        {
            return Err(failure(
                NumericEvaluationErrorKind::UnsupportedOperation,
                "effective export defaults require the explicit FMI input policy",
                variable.declaration().span(),
            ));
        }
        let index = id.index() as usize;
        if let Some(value) = &self.input_initializations[index] {
            return Ok(value.clone());
        }
        if self.evaluating[index] {
            return Err(failure(
                NumericEvaluationErrorKind::CyclicDependency,
                "cyclic input default dependency",
                variable.declaration().span(),
            ));
        }
        self.evaluating[index] = true;
        let result = self.issue_input_initialization(variable);
        self.evaluating[index] = false;
        let value = result?;
        self.input_initializations[index] = Some(value.clone());
        Ok(value)
    }

    fn issue_input_initialization(
        &mut self,
        variable: dae::VariableView<'dae>,
    ) -> Result<InputInitialization, NumericEvaluationError> {
        let expression = declaration_initial_expression(variable, self.input_policy);
        let source = expression
            .map(|expr| self.variable_source_expression(variable, expr))
            .transpose()?;
        let overrides = self.checked_overrides(variable, variable.scalar_count())?;
        let (mut effective, fallback) =
            self.input_default_base(variable, source.as_ref(), &overrides)?;
        effective.apply_source_overrides(&overrides);
        let span = expression.map_or(variable.declaration().span(), |expr| {
            self.expression_span(expr)
        });
        require_finite(&effective, span)?;
        let origin = if variable.binding().is_some() {
            InputOrigin::Binding
        } else if source.is_some() {
            InputOrigin::Start
        } else {
            InputOrigin::MlsFallback
        };
        let origins = origin_runs(
            variable.scalar_count(),
            origin,
            span,
            variable.declaration().span(),
            &overrides,
        );
        Ok(InputInitialization {
            source,
            effective,
            origins,
            fallback,
        })
    }

    fn input_default_base(
        &mut self,
        variable: dae::VariableView<'dae>,
        source: Option<&SourceValues>,
        overrides: &[(usize, Scalar)],
    ) -> Result<(SourceValues, Option<InputFallbackBounds>), NumericEvaluationError> {
        let bounds = InputFallbackBounds {
            minimum: self.input_bound(variable, variable.minimum())?,
            maximum: self.input_bound(variable, variable.maximum())?,
            minimum_span: variable.minimum().map(|e| self.expression_span(e)),
            maximum_span: variable.maximum().map(|e| self.expression_span(e)),
        };
        if let Some(values) = source {
            return Ok((values.clone(), Some(bounds)));
        }
        if overrides.len() == variable.scalar_count() {
            return Ok((
                SourceValues::literal(overrides.iter().map(|(_, v)| *v).collect()),
                Some(bounds),
            ));
        }
        let values = fallback_values(variable, &bounds)?;
        Ok((values, Some(bounds)))
    }

    fn input_bound(
        &mut self,
        variable: dae::VariableView<'dae>,
        expression: Option<dae::ExprId<'dae>>,
    ) -> Result<Option<SourceValues>, NumericEvaluationError> {
        let Some(expression) = expression else {
            return Ok(None);
        };
        if variable.scalar_count() == 0 {
            return Ok(Some(SourceValues::default()));
        }
        let mut values = self.source_values(expression)?;
        values.broadcast(variable.scalar_count());
        if values.len() != variable.scalar_count() {
            return Err(failure(
                NumericEvaluationErrorKind::ShapeMismatch,
                "input bound extent disagrees with declaration",
                self.expression_span(expression),
            ));
        }
        Ok(Some(values))
    }
}

fn origin_runs(
    count: usize,
    origin: InputOrigin,
    span: Span,
    supplied_span: Span,
    overrides: &[(usize, Scalar)],
) -> Vec<InputOriginRun> {
    let mut runs = Vec::new();
    let mut cursor = 0;
    for &(index, _) in overrides {
        if cursor < index {
            runs.push(InputOriginRun {
                start: cursor,
                count: index - cursor,
                origin,
                span,
            });
        }
        if let Some(last) = runs
            .last_mut()
            .filter(|run| run.origin == InputOrigin::Supplied && run.start + run.count == index)
        {
            last.count += 1;
        } else {
            runs.push(InputOriginRun {
                start: index,
                count: 1,
                origin: InputOrigin::Supplied,
                span: supplied_span,
            });
        }
        cursor = index + 1;
    }
    if cursor < count {
        runs.push(InputOriginRun {
            start: cursor,
            count: count - cursor,
            origin,
            span,
        });
    }
    runs
}

fn fallback_values(
    variable: dae::VariableView<'_>,
    bounds: &InputFallbackBounds,
) -> Result<SourceValues, NumericEvaluationError> {
    let runs = [bound_runs(bounds.minimum()), bound_runs(bounds.maximum())];
    let mut boundaries = vec![0, variable.scalar_count()];
    for bound in &runs {
        boundaries.extend(
            bound
                .iter()
                .map(|(start, run)| start + bound_run_count(run)),
        );
    }
    boundaries.sort_unstable();
    boundaries.dedup();
    let mut cursors = [0; 2];
    let mut result = SourceValues::default();
    for interval in boundaries.windows(2) {
        let start = interval[0];
        let count = interval[1] - start;
        for (runs, cursor) in runs.iter().zip(&mut cursors) {
            while runs
                .get(*cursor)
                .is_some_and(|(base, run)| base + bound_run_count(run) <= start)
            {
                *cursor += 1;
            }
        }
        let current = [runs[0].get(cursors[0]), runs[1].get(cursors[1])];
        let repeated = current
            .iter()
            .all(|run| run.is_none_or(|(_, run)| matches!(run, NumericInitialRun::Repeat { .. })));
        let value = |ordinal| {
            fallback_scalar(
                variable,
                bound_run_value(current[0], ordinal),
                bound_run_value(current[1], ordinal),
            )
        };
        result.append(if repeated {
            SourceValues::repeat(value(start)?, count)
        } else {
            SourceValues::literal(
                (start..start + count)
                    .map(value)
                    .collect::<Result<_, _>>()?,
            )
        });
    }
    Ok(result)
}

fn bound_runs(values: Option<&SourceValues>) -> Vec<(usize, NumericInitialRun<'_, Scalar>)> {
    let mut base = 0;
    values
        .into_iter()
        .flat_map(|v| v.runs())
        .map(|run| {
            let start = base;
            base += bound_run_count(&run);
            (start, run)
        })
        .collect()
}

fn bound_run_count(run: &NumericInitialRun<'_, Scalar>) -> usize {
    match run {
        NumericInitialRun::Repeat { count, .. } => *count,
        NumericInitialRun::Literal(values) => values.len(),
    }
}

fn bound_run_value(
    run: Option<&(usize, NumericInitialRun<'_, Scalar>)>,
    ordinal: usize,
) -> Option<Scalar> {
    match run? {
        (_, NumericInitialRun::Repeat { value, .. }) => Some(*value),
        (start, NumericInitialRun::Literal(values)) => values.get(ordinal - start).copied(),
    }
}

fn fallback_scalar(
    variable: dae::VariableView<'_>,
    minimum: Option<Scalar>,
    maximum: Option<Scalar>,
) -> Result<Scalar, NumericEvaluationError> {
    let invalid = || {
        failure(
            NumericEvaluationErrorKind::InvalidValue,
            "input fallback has invalid or infeasible bounds",
            variable.declaration().span(),
        )
    };
    match variable.value_type().scalar_type() {
        dae::ScalarType::Real => {
            let lower = minimum.map_or(f64::NEG_INFINITY, Scalar::real);
            let upper = maximum.map_or(f64::INFINITY, Scalar::real);
            if lower.is_nan() || upper.is_nan() || lower > upper {
                return Err(invalid());
            }
            let value = if lower > 0.0 {
                lower
            } else if upper < 0.0 {
                upper
            } else {
                0.0
            };
            if !value.is_finite() {
                return Err(invalid());
            }
            Ok(Scalar::Real(value))
        }
        dae::ScalarType::Integer => {
            let lower = integer_bound(minimum, i64::MIN).ok_or_else(invalid)?;
            let upper = integer_bound(maximum, i64::MAX).ok_or_else(invalid)?;
            if lower > upper {
                return Err(invalid());
            }
            Ok(Scalar::Integer(0_i64.clamp(lower, upper)))
        }
        dae::ScalarType::Boolean if minimum.is_none() && maximum.is_none() => {
            Ok(Scalar::Boolean(false))
        }
        dae::ScalarType::Enumeration => {
            let size = variable
                .enumeration()
                .map(|domain| domain.literals.len() as i64)
                .ok_or_else(invalid)?;
            let lower = integer_bound(minimum, 1).ok_or_else(invalid)?;
            let upper = integer_bound(maximum, size).ok_or_else(invalid)?;
            if lower < 1 || lower > upper || upper > size {
                return Err(invalid());
            }
            Ok(Scalar::Enumeration(lower))
        }
        _ => Err(invalid()),
    }
}

fn integer_bound(value: Option<Scalar>, absent: i64) -> Option<i64> {
    match value {
        None => Some(absent),
        Some(Scalar::Integer(v) | Scalar::Enumeration(v)) => Some(v),
        _ => None,
    }
}
