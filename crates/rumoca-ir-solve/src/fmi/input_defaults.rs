//! Immutable effective input defaults remain distinct from declaration starts.

use super::*;
use crate::{SolveDeclarationStart as Start, SolveExactInitialRun, SolveVariableValueKind};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum FmiInputDefaultOrigin {
    Binding,
    Start,
    Supplied,
    MlsFallback,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct FmiInputDefaultOriginRun {
    pub start: usize,
    pub count: usize,
    pub origin: FmiInputDefaultOrigin,
    pub span: Span,
}

/// Unchecked ingredients; only the correlated component can issue a default.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct FmiInputDefaultInput {
    pub declaration: usize,
    pub values: Start,
    pub origins: Vec<FmiInputDefaultOriginRun>,
    pub fallback_minimum: Option<Start>,
    pub fallback_maximum: Option<Start>,
    pub fallback_minimum_span: Option<Span>,
    pub fallback_maximum_span: Option<Span>,
}

#[derive(Debug, Clone)]
pub struct FmiInputInstantiationDefault {
    input: FmiInputDefaultInput,
    declaration: Span,
}

impl FmiInputInstantiationDefault {
    pub fn values(&self) -> &Start {
        &self.input.values
    }
    pub fn origins(&self) -> &[FmiInputDefaultOriginRun] {
        &self.input.origins
    }
    /// Source declaration span proven when the default was issued.
    pub fn declaration(&self) -> Span {
        self.declaration
    }
    pub(super) fn input(&self) -> FmiInputDefaultInput {
        self.input.clone()
    }

    pub(super) fn construct(
        input: FmiInputDefaultInput,
        declaration: usize,
        source: &FmiVariableInput,
    ) -> Result<Self, FmiComponentError> {
        let bad = |message| error(source, message);
        let count = source.start.len();
        if source.role != SolveVariableStorageRole::ExternalInput
            || input.declaration != declaration
            || input.values.is_missing()
            || input.values.value_kind() != source.value_kind
            || input.values.len() != count
        {
            return Err(bad("default identity, kind or extent mismatch"));
        }
        crate::initial_values::check_initial_partition(
            count,
            1,
            input.origins.iter().map(|run| (run.start, run.count)),
        )
        .map_err(|_| bad("default origins are not a complete ordered partition"))?;
        for bound in [&input.fallback_minimum, &input.fallback_maximum]
            .into_iter()
            .flatten()
        {
            if bound.is_missing()
                || bound.len() != count
                || !bound_kind_matches(source.value_kind, bound.value_kind())
            {
                return Err(bad("fallback bound kind or extent mismatch"));
            }
        }
        for (bound, span) in [
            (&input.fallback_minimum, input.fallback_minimum_span),
            (&input.fallback_maximum, input.fallback_maximum_span),
        ] {
            if bound.is_some() != span.is_some() || span.is_some_and(|s| s.is_dummy()) {
                return Err(bad("fallback bound has no matching source provenance"));
            }
        }
        if !bound_projection_matches(input.fallback_minimum.as_ref(), source.minimum.as_ref())
            || !bound_projection_matches(input.fallback_maximum.as_ref(), source.maximum.as_ref())
        {
            return Err(bad("exact bound witness disagrees with metadata attribute"));
        }
        let result = Self {
            input,
            declaration: source.declaration,
        };
        result.check_origins(source)?;
        Ok(result)
    }

    fn check_origins(&self, source: &FmiVariableInput) -> Result<(), FmiComponentError> {
        let starts = [
            Some(self.values()),
            Some(&source.start),
            self.input.fallback_minimum.as_ref(),
            self.input.fallback_maximum.as_ref(),
        ];
        let boundaries = origin_boundaries(&self.input.origins);
        visit_intervals(&starts, &boundaries, |index, _| {
            self.check_origin_at(source, index)
        })
    }

    fn check_origin_at(
        &self,
        source: &FmiVariableInput,
        index: usize,
    ) -> Result<(), FmiComponentError> {
        let position = self
            .input
            .origins
            .partition_point(|run| run.start <= index)
            .checked_sub(1);
        let origin = position
            .and_then(|position| self.input.origins.get(position))
            .ok_or_else(|| error(source, "default origin does not cover its value"))?;
        if origin.span.is_dummy() {
            return Err(error(source, "default origin has no source provenance"));
        }
        let value =
            scalar(self.values(), index).ok_or_else(|| error(source, "default value is absent"))?;
        if !value.valid(source.enumeration.as_ref()) {
            return Err(error(source, "invalid typed default value"));
        }
        match origin.origin {
            FmiInputDefaultOrigin::Binding | FmiInputDefaultOrigin::Start => {
                if scalar(&source.start, index) != Some(value) {
                    return Err(error(
                        source,
                        "source default origin disagrees with declaration start",
                    ));
                }
            }
            FmiInputDefaultOrigin::MlsFallback => self.check_fallback_at(source, index, value)?,
            FmiInputDefaultOrigin::Supplied => {}
        }
        Ok(())
    }

    fn check_fallback_at(
        &self,
        source: &FmiVariableInput,
        index: usize,
        value: Scalar,
    ) -> Result<(), FmiComponentError> {
        let bound = |bound: &Option<Start>| bound.as_ref().and_then(|v| scalar(v, index));
        if !source.start.is_missing()
            || !fallback_witness(
                value,
                bound(&self.input.fallback_minimum),
                bound(&self.input.fallback_maximum),
                source.enumeration.as_ref(),
            )
        {
            return Err(error(source, "invalid language fallback witness"));
        }
        Ok(())
    }
}

fn origin_boundaries(origins: &[FmiInputDefaultOriginRun]) -> Vec<(usize, usize)> {
    origins.iter().map(|run| (run.start, run.count)).collect()
}

fn bound_kind_matches(kind: SolveVariableValueKind, bound: SolveVariableValueKind) -> bool {
    kind == bound
        || (kind == SolveVariableValueKind::Enumeration && bound == SolveVariableValueKind::Integer)
}

fn bound_projection_matches(
    bound: Option<&Start>,
    attribute: Option<&crate::SolveInitialValues>,
) -> bool {
    match (bound, attribute) {
        (None, None) => true,
        (Some(bound), Some(attribute)) => {
            let projected = Start::Real(attribute.clone());
            if bound.len() != projected.len() {
                return false;
            }
            visit_intervals(&[Some(bound), Some(&projected)], &[], |index, _| {
                if scalar(bound, index)
                    .and_then(Scalar::attribute_projection)
                    .map(f64::to_bits)
                    == attribute.value(index).map(f64::to_bits)
                {
                    Ok(())
                } else {
                    Err(())
                }
            })
            .is_ok()
        }
        _ => false,
    }
}

fn error(source: &FmiVariableInput, message: &'static str) -> FmiComponentError {
    FmiComponentError::InputDefault {
        name: source.name.clone(),
        message,
        span: source.declaration,
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Scalar {
    Real(u64),
    Integer(i64),
    Enumeration(i64),
    Boolean(bool),
}

impl Scalar {
    fn attribute_projection(self) -> Option<f64> {
        match self {
            Self::Integer(v) | Self::Enumeration(v)
                if !rumoca_core::integer_within_binary64_exact_range(v) =>
            {
                None
            }
            value => Some(value.numerical()),
        }
    }
    fn numerical(self) -> f64 {
        match self {
            Self::Real(bits) => f64::from_bits(bits),
            Self::Integer(v) | Self::Enumeration(v) => v as f64,
            Self::Boolean(v) => f64::from(u8::from(v)),
        }
    }
    fn valid(self, domain: Option<&rumoca_core::EnumerationDeclaration>) -> bool {
        match self {
            Self::Real(v) => f64::from_bits(v).is_finite(),
            Self::Enumeration(v) => domain.is_some_and(|d| d.contains_ordinal(v)),
            _ => true,
        }
    }
}

fn scalar(values: &Start, index: usize) -> Option<Scalar> {
    match values {
        Start::Real(v) => v.value(index).map(|v| Scalar::Real(v.to_bits())),
        Start::Integer(v) => v.value(index).copied().map(Scalar::Integer),
        Start::Enumeration(v) => v.value(index).copied().map(Scalar::Enumeration),
        Start::Boolean(v) => v.value(index).copied().map(Scalar::Boolean),
        Start::Missing { .. } => None,
    }
}

fn integer_bound(value: Option<Scalar>, absent: i64) -> Option<i64> {
    match value {
        None => Some(absent),
        Some(Scalar::Integer(v) | Scalar::Enumeration(v)) => Some(v),
        _ => None,
    }
}

/// Check the retained choice witness, without evaluating or selecting a value.
fn fallback_witness(
    value: Scalar,
    minimum: Option<Scalar>,
    maximum: Option<Scalar>,
    domain: Option<&rumoca_core::EnumerationDeclaration>,
) -> bool {
    match value {
        Scalar::Real(bits) => {
            let lower = minimum.map_or(f64::NEG_INFINITY, Scalar::numerical);
            let upper = maximum.map_or(f64::INFINITY, Scalar::numerical);
            let value = f64::from_bits(bits);
            lower <= value
                && value <= upper
                && ((lower <= 0.0 && upper >= 0.0 && bits == 0.0_f64.to_bits())
                    || (lower > 0.0 && bits == lower.to_bits())
                    || (upper < 0.0 && bits == upper.to_bits()))
        }
        Scalar::Integer(value) => {
            let (Some(lower), Some(upper)) = (
                integer_bound(minimum, i64::MIN),
                integer_bound(maximum, i64::MAX),
            ) else {
                return false;
            };
            lower <= value
                && value <= upper
                && ((lower <= 0 && upper >= 0 && value == 0)
                    || (lower > 0 && value == lower)
                    || (upper < 0 && value == upper))
        }
        Scalar::Enumeration(value) => {
            let Some(domain) = domain else {
                return false;
            };
            let (Some(lower), Some(upper)) = (
                integer_bound(minimum, 1),
                integer_bound(maximum, domain.literals.len() as i64),
            ) else {
                return false;
            };
            value == lower
                && lower <= upper
                && domain.contains_ordinal(lower)
                && domain.contains_ordinal(upper)
        }
        Scalar::Boolean(value) => !value && minimum.is_none() && maximum.is_none(),
    }
}

fn run_intervals(values: &Start) -> Vec<(usize, usize, bool)> {
    match values {
        Start::Real(v) => v
            .runs()
            .map(|run| (run.start(), run.count(), run.repeated_bits().is_some()))
            .collect(),
        Start::Integer(v) | Start::Enumeration(v) => exact_intervals(v),
        Start::Boolean(v) => exact_intervals(v),
        Start::Missing { .. } => vec![],
    }
}

fn exact_intervals<T>(values: &crate::SolveExactInitialValues<T>) -> Vec<(usize, usize, bool)> {
    values
        .runs()
        .map(|run| {
            (
                run.start(),
                run.count(),
                matches!(run, SolveExactInitialRun::Repeat { .. }),
            )
        })
        .collect()
}

fn visit_intervals<E>(
    values: &[Option<&Start>],
    origins: &[(usize, usize)],
    mut visit: impl FnMut(usize, usize) -> Result<(), E>,
) -> Result<(), E> {
    let runs = values
        .iter()
        .flatten()
        .map(|v| run_intervals(v))
        .collect::<Vec<_>>();
    let mut boundaries = origins
        .iter()
        .flat_map(|(start, count)| [*start, start + count])
        .collect::<Vec<_>>();
    boundaries.extend(
        runs.iter()
            .flatten()
            .flat_map(|(start, count, _)| [*start, start + count]),
    );
    boundaries.sort_unstable();
    boundaries.dedup();
    let mut cursors = vec![0; runs.len()];
    for pair in boundaries.windows(2) {
        let repeated = runs.iter().zip(&mut cursors).all(|(runs, cursor)| {
            while runs
                .get(*cursor)
                .is_some_and(|(start, count, _)| start + count <= pair[0])
            {
                *cursor += 1;
            }
            runs.get(*cursor).is_none_or(|(_, _, repeated)| *repeated)
        });
        if repeated {
            visit(pair[0], pair[1] - pair[0])?;
        } else {
            for index in pair[0]..pair[1] {
                visit(index, 1)?;
            }
        }
    }
    Ok(())
}

/// Why a storage check failed: a structural defect, or a stored value that
/// differs from the checked default.
enum StorageCheck {
    Invalid(FmiComponentError),
    Disagrees,
}

fn invalid_storage(message: &str) -> StorageCheck {
    StorageCheck::Invalid(FmiComponentError::InvalidSolve(message.into()))
}

/// Check every issued default once and derive public-input completeness.
/// A host component may retain an authored start that differs from its supplied
/// storage; that narrows publication without invalidating the host component.
pub(super) fn check_model_defaults(
    model: &crate::SolveModel,
    metadata: &FmiMetadata,
) -> Result<bool, FmiComponentError> {
    let mut complete = true;
    for variable in metadata.variables() {
        if let Some(default) = &variable.effective_input_default {
            match check_storage(model, variable, default.values()) {
                Ok(()) => {}
                Err(StorageCheck::Invalid(error)) => return Err(error),
                Err(StorageCheck::Disagrees) => {
                    return Err(FmiComponentError::InputDefault {
                        name: variable.name().into(),
                        message: "default disagrees with original instantiation storage",
                        span: default.declaration(),
                    });
                }
            }
        } else if variable.causality() == FmiCausality::Input {
            complete &= !variable.instantiation_start().is_missing()
                && check_storage(model, variable, variable.instantiation_start()).is_ok();
        }
    }
    Ok(complete)
}

fn check_storage(
    model: &crate::SolveModel,
    variable: &FmiVariable,
    values: &Start,
) -> Result<(), StorageCheck> {
    let storage = variable
        .storage()
        .ok_or_else(|| invalid_storage("input default has no storage"))?;
    let column = match storage.column() {
        FmiStorageColumn::P => &model.parameters,
        FmiStorageColumn::Y => &model.initial_y,
    };
    visit_intervals(&[Some(values)], &[(0, values.len())], |index, count| {
        let bits = scalar(values, index)
            .ok_or_else(|| invalid_storage("input default value is absent"))?
            .numerical()
            .to_bits();
        if covered_matching(column, &storage, index, count, bits)? == count {
            Ok(())
        } else {
            Err(invalid_storage("input default is outside runtime storage"))
        }
    })
}

/// Scalars of `index..index + count` stored with `bits`; a covered scalar
/// holding other bits is a disagreement.
fn covered_matching(
    column: &crate::SolveInitialValues,
    storage: &FmiStorageRun,
    index: usize,
    count: usize,
    bits: u64,
) -> Result<usize, StorageCheck> {
    let begin = storage.base() + index;
    let end = begin
        .checked_add(count)
        .ok_or_else(|| invalid_storage("input default extent overflow"))?;
    let runs = column
        .runs_covering(begin, count)
        .ok_or_else(|| invalid_storage("input default is outside runtime storage"))?;
    let mut covered = 0;
    for run in runs {
        let first = run.start().max(begin);
        let last = (run.start() + run.count()).min(end);
        if first >= last {
            continue;
        }
        if !run_holds(&run, first, last, bits) {
            return Err(StorageCheck::Disagrees);
        }
        covered += last - first;
    }
    Ok(covered)
}

fn run_holds(run: &crate::SolveInitialValueRun<'_>, first: usize, last: usize, bits: u64) -> bool {
    match run.repeated_bits() {
        Some(actual) => actual == bits,
        None => run.literal_bits().is_some_and(|actual| {
            actual[first - run.start()..last - run.start()]
                .iter()
                .all(|v| *v == bits)
        }),
    }
}
