//! Construction-owned declaration and runtime-value catalog for one Solve model.
//!
//! Entries are issued in the dense declaration order owned by `SolveLayout`.
//! The construction operation supplies the exact declaration and storage run;
//! callers provide only source-owned metadata and evaluated values.

use crate::{
    SolveProblem, SolveStorageCoordinate, SolveVariableDeclaration, SolveVariableStorageRole,
    SolveVariableStorageRun, SolveVariableTimeDomain, SolveVariableValueKind,
};
use rumoca_core::{Fixity, SourceOccurrenceId, Span};
use serde::{Deserialize, Serialize};
use std::collections::BTreeSet;

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct SolveVariableId(u32);

impl SolveVariableId {
    #[must_use]
    pub const fn index(self) -> usize {
        self.0 as usize
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum SolveVariableCausality {
    Input,
    Output,
    Parameter,
    CalculatedParameter,
    Independent,
    Local,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum SolveVariableVariability {
    Constant,
    Fixed,
    Tunable,
    Discrete,
    Continuous,
}

/// Construction-issued initialization policy for a continuous state.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum SolveStateInitialization {
    NotState,
    Exact,
    Approximate,
}

/// Source-owned variable facts transferred into Solve construction.
///
/// `source_occurrence` is the only identity field. Names and provenance are
/// presentation/diagnostic data and never authorize a semantic join.
#[derive(Clone, Debug)]
pub struct SolveVariableSource {
    source_occurrence: SourceOccurrenceId,
    name: String,
    dimensions: Box<[u32]>,
    scalar_names: Box<[String]>,
    provenance: Span,
}

impl SolveVariableSource {
    #[must_use]
    pub fn new(
        source_occurrence: SourceOccurrenceId,
        name: String,
        dimensions: Vec<u32>,
        scalar_names: Vec<String>,
        provenance: Span,
    ) -> Self {
        Self {
            source_occurrence,
            name,
            dimensions: dimensions.into_boxed_slice(),
            scalar_names: scalar_names.into_boxed_slice(),
            provenance,
        }
    }
}

#[derive(Clone, Debug)]
pub struct SolveVariableSourceAttributes {
    causality: SolveVariableCausality,
    variability: SolveVariableVariability,
    tunable: bool,
    unit: Option<String>,
    description: Option<String>,
    fixed: Fixity,
}

impl SolveVariableSourceAttributes {
    #[must_use]
    pub fn new(
        causality: SolveVariableCausality,
        variability: SolveVariableVariability,
        tunable: bool,
        unit: Option<String>,
        description: Option<String>,
        fixed: Fixity,
    ) -> Self {
        Self {
            causality,
            variability,
            tunable,
            unit,
            description,
            fixed,
        }
    }
}

#[derive(Clone, Debug)]
pub struct SolveVariableEvaluatedValues {
    start: Option<Box<[f64]>>,
    minimum: Option<Box<[f64]>>,
    maximum: Option<Box<[f64]>>,
    /// Explicit source `nominal` values. Absence retains the presentation fact
    /// that the attribute was omitted; its effective numerical value is still
    /// exactly `1.0` and catalog construction correlates that value with every
    /// corresponding solver-Y nominal slot.
    nominal: Option<Box<[f64]>>,
}

impl SolveVariableEvaluatedValues {
    #[must_use]
    pub fn new(
        start: Option<Vec<f64>>,
        minimum: Option<Vec<f64>>,
        maximum: Option<Vec<f64>>,
        nominal: Option<Vec<f64>>,
    ) -> Self {
        Self {
            start: start.map(Vec::into_boxed_slice),
            minimum: minimum.map(Vec::into_boxed_slice),
            maximum: maximum.map(Vec::into_boxed_slice),
            nominal: nominal.map(Vec::into_boxed_slice),
        }
    }
}

#[derive(Debug)]
pub struct SolveVariableCatalogEntry {
    id: SolveVariableId,
    source: SolveVariableSource,
    attributes: SolveVariableSourceAttributes,
    values: SolveVariableEvaluatedValues,
    declaration: SolveVariableDeclaration,
    storage: SolveVariableStorageRun,
    state_initialization: SolveStateInitialization,
}

impl SolveVariableCatalogEntry {
    #[must_use]
    pub const fn id(&self) -> SolveVariableId {
        self.id
    }

    #[must_use]
    pub const fn source_occurrence(&self) -> SourceOccurrenceId {
        self.source.source_occurrence
    }

    #[must_use]
    pub fn name(&self) -> &str {
        &self.source.name
    }

    #[must_use]
    pub fn dimensions(&self) -> &[u32] {
        std::ops::Deref::deref(&self.source.dimensions)
    }

    #[must_use]
    pub fn scalar_names(&self) -> &[String] {
        &self.source.scalar_names
    }

    #[must_use]
    pub const fn provenance(&self) -> Span {
        self.source.provenance
    }

    #[must_use]
    pub const fn causality(&self) -> SolveVariableCausality {
        self.attributes.causality
    }

    #[must_use]
    pub const fn variability(&self) -> SolveVariableVariability {
        self.attributes.variability
    }

    #[must_use]
    pub const fn is_tunable(&self) -> bool {
        self.attributes.tunable
    }

    #[must_use]
    pub fn unit(&self) -> Option<&str> {
        self.attributes.unit.as_deref()
    }

    #[must_use]
    pub fn description(&self) -> Option<&str> {
        self.attributes.description.as_deref()
    }

    #[must_use]
    pub const fn fixed(&self) -> Fixity {
        self.attributes.fixed
    }

    #[must_use]
    pub fn start(&self) -> Option<&[f64]> {
        match &self.values.start {
            Some(values) => Some(std::ops::Deref::deref(values)),
            None => None,
        }
    }

    #[must_use]
    pub fn minimum(&self) -> Option<&[f64]> {
        self.values.minimum.as_deref()
    }

    #[must_use]
    pub fn maximum(&self) -> Option<&[f64]> {
        self.values.maximum.as_deref()
    }

    #[must_use]
    pub fn nominal(&self) -> Option<&[f64]> {
        self.values.nominal.as_deref()
    }

    #[must_use]
    pub const fn declaration(&self) -> SolveVariableDeclaration {
        self.declaration
    }

    #[must_use]
    pub const fn storage(&self) -> SolveVariableStorageRun {
        self.storage
    }

    #[must_use]
    pub const fn role(&self) -> SolveVariableStorageRole {
        self.declaration.role()
    }

    #[must_use]
    pub const fn value_kind(&self) -> SolveVariableValueKind {
        self.declaration.value_kind()
    }

    #[must_use]
    pub const fn time_domain(&self) -> SolveVariableTimeDomain {
        self.declaration.time_domain()
    }

    #[must_use]
    pub const fn state_initialization(&self) -> SolveStateInitialization {
        self.state_initialization
    }
}

#[derive(Debug)]
pub struct SolveVariableCatalog {
    entries: Box<[SolveVariableCatalogEntry]>,
}

impl SolveVariableCatalog {
    #[must_use]
    pub fn entries(&self) -> &[SolveVariableCatalogEntry] {
        std::ops::Deref::deref(&self.entries)
    }

    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    #[must_use]
    pub const fn len(&self) -> usize {
        self.entries.len()
    }

    #[must_use]
    pub fn visible_scalar_count(&self) -> usize {
        self.entries
            .iter()
            .filter(|entry| is_visible_role(entry.role()))
            .map(|entry| entry.scalar_names().len())
            .sum()
    }

    pub fn visible_scalar_names(&self) -> impl Iterator<Item = &str> {
        self.entries
            .iter()
            .filter(|entry| is_visible_role(entry.role()))
            .flat_map(|entry| entry.scalar_names().iter().map(String::as_str))
    }

    pub fn begin<'model>(
        problem: &'model SolveProblem,
        initial_y: &'model [f64],
        solver_nominals: &'model [f64],
        parameters: &'model [f64],
    ) -> SolveVariableCatalogConstruction<'model> {
        SolveVariableCatalogConstruction {
            problem,
            initial_y,
            solver_nominals,
            parameters,
            entries: Vec::with_capacity(problem.solve_layout.variable_declarations.len()),
            source_occurrences: BTreeSet::new(),
            declaration_names: BTreeSet::new(),
            scalar_names: BTreeSet::new(),
        }
    }
}

/// Name-free Solve variable facts exposed to a cross-stage refinement
/// checker.
///
/// It has no display-name, scalar-label, provenance, runtime-value, or
/// construction API. Effective fixity, construction-issued state
/// initialization, variability, and tunability are semantic facts, not
/// runtime values.
/// `SolveVariableCatalogConstruction` already proves that every entry agrees
/// with the sealed layout, so repeating layout arrays here would enlarge the
/// cross-stage checker's trusted input without strengthening its DAE-to-Solve
/// refinement claim.
pub struct SolveVariableRefinementView {
    entries: Box<[SolveVariableRefinementEntry]>,
}

pub struct SolveVariableRefinementEntry {
    source_occurrence: SourceOccurrenceId,
    fixed: Fixity,
    state_initialization: SolveStateInitialization,
    variability: SolveVariableVariability,
    tunable: bool,
    causality: SolveVariableCausality,
    dimensions: Box<[u32]>,
    role: SolveVariableStorageRole,
    value_kind: SolveVariableValueKind,
    storage: SolveVariableStorageRun,
}

impl SolveVariableRefinementView {
    pub(crate) fn new(entries: &[SolveVariableCatalogEntry]) -> Self {
        let mut projected = Vec::with_capacity(entries.len());
        let mut index = 0;
        while index < entries.len() {
            let entry = &entries[index];
            projected.push(SolveVariableRefinementEntry {
                source_occurrence: entry.source_occurrence(),
                fixed: entry.fixed(),
                state_initialization: entry.state_initialization(),
                variability: entry.variability(),
                tunable: entry.is_tunable(),
                causality: entry.causality(),
                dimensions: entry.dimensions().to_vec().into(),
                role: entry.role(),
                value_kind: entry.value_kind(),
                storage: entry.storage(),
            });
            index += 1;
        }
        Self {
            entries: projected.into(),
        }
    }

    #[must_use]
    pub fn entries(&self) -> &[SolveVariableRefinementEntry] {
        std::ops::Deref::deref(&self.entries)
    }
}

impl SolveVariableRefinementEntry {
    #[must_use]
    pub const fn source_occurrence(&self) -> SourceOccurrenceId {
        self.source_occurrence
    }

    #[must_use]
    pub const fn fixed(&self) -> Fixity {
        self.fixed
    }

    #[must_use]
    pub const fn state_initialization(&self) -> SolveStateInitialization {
        self.state_initialization
    }

    #[must_use]
    pub const fn variability(&self) -> SolveVariableVariability {
        self.variability
    }

    #[must_use]
    pub const fn tunable(&self) -> bool {
        self.tunable
    }

    #[must_use]
    pub const fn causality(&self) -> SolveVariableCausality {
        self.causality
    }

    #[must_use]
    pub fn dimensions(&self) -> &[u32] {
        std::ops::Deref::deref(&self.dimensions)
    }

    #[must_use]
    pub const fn role(&self) -> SolveVariableStorageRole {
        self.role
    }

    #[must_use]
    pub const fn value_kind(&self) -> SolveVariableValueKind {
        self.value_kind
    }

    #[must_use]
    pub const fn storage(&self) -> SolveVariableStorageRun {
        self.storage
    }
}

const fn is_visible_role(role: SolveVariableStorageRole) -> bool {
    matches!(
        role,
        SolveVariableStorageRole::ExternalInput
            | SolveVariableStorageRole::State
            | SolveVariableStorageRole::Algebraic
            | SolveVariableStorageRole::Output
            | SolveVariableStorageRole::DiscreteReal
            | SolveVariableStorageRole::DiscreteValue
    )
}

#[must_use = "a variable catalog construction must be finished or discarded"]
pub struct SolveVariableCatalogConstruction<'problem> {
    problem: &'problem SolveProblem,
    initial_y: &'problem [f64],
    solver_nominals: &'problem [f64],
    parameters: &'problem [f64],
    entries: Vec<SolveVariableCatalogEntry>,
    source_occurrences: BTreeSet<SourceOccurrenceId>,
    declaration_names: BTreeSet<String>,
    scalar_names: BTreeSet<String>,
}

impl SolveVariableCatalogConstruction<'_> {
    pub fn issue(
        &mut self,
        source: SolveVariableSource,
        attributes: SolveVariableSourceAttributes,
        values: SolveVariableEvaluatedValues,
    ) -> Result<SolveVariableId, SolveVariableCatalogError> {
        let index = self.entries.len();
        let declaration = self.expected_declaration(index, source.provenance)?;
        let storage = self.expected_storage(index, source.provenance)?;
        check_source(&source, storage.scalar_count)?;
        check_solver_storage_names(self.problem, &source, storage)?;
        let state_initialization = check_attributes(&source, declaration, &attributes)?;
        check_values(&source, declaration.value_kind(), &values)?;
        check_runtime_values(self, &source, storage, &values)?;
        if self.source_occurrences.contains(&source.source_occurrence) {
            return Err(SolveVariableCatalogError::DuplicateSourceOccurrence {
                occurrence: source.source_occurrence,
                span: source.provenance,
            });
        }
        if self.declaration_names.contains(&source.name) {
            return Err(SolveVariableCatalogError::DuplicateName {
                name: source.name,
                span: source.provenance,
            });
        }
        let mut local_scalar_names = BTreeSet::new();
        for name in &source.scalar_names {
            if self.scalar_names.contains(name) || !local_scalar_names.insert(name.as_str()) {
                return Err(SolveVariableCatalogError::DuplicateScalarName {
                    name: name.clone(),
                    span: source.provenance,
                });
            }
        }
        let id = SolveVariableId(
            u32::try_from(index).map_err(|_| SolveVariableCatalogError::IdentityOverflow)?,
        );
        self.entries.push(SolveVariableCatalogEntry {
            id,
            source,
            attributes,
            values,
            declaration,
            storage,
            state_initialization,
        });
        self.source_occurrences
            .insert(self.entries[index].source.source_occurrence);
        self.declaration_names
            .insert(self.entries[index].source.name.clone());
        self.scalar_names
            .extend(self.entries[index].source.scalar_names.iter().cloned());
        Ok(id)
    }

    pub fn finish(self) -> Result<SolveVariableCatalog, SolveVariableCatalogError> {
        let expected = self.problem.solve_layout.variable_declarations.len();
        if self.entries.len() != expected {
            return Err(SolveVariableCatalogError::Incomplete {
                actual: self.entries.len(),
                expected,
            });
        }
        Ok(SolveVariableCatalog {
            entries: self.entries.into_boxed_slice(),
        })
    }

    fn expected_declaration(
        &self,
        index: usize,
        span: Span,
    ) -> Result<SolveVariableDeclaration, SolveVariableCatalogError> {
        self.problem
            .solve_layout
            .variable_declarations
            .get(index)
            .copied()
            .ok_or(SolveVariableCatalogError::TooManyEntries { index, span })
    }

    fn expected_storage(
        &self,
        index: usize,
        span: Span,
    ) -> Result<SolveVariableStorageRun, SolveVariableCatalogError> {
        self.problem
            .solve_layout
            .variable_storage_runs
            .get(index)
            .copied()
            .ok_or(SolveVariableCatalogError::TooManyEntries { index, span })
    }
}

fn check_runtime_values(
    construction: &SolveVariableCatalogConstruction<'_>,
    identity: &SolveVariableSource,
    storage: SolveVariableStorageRun,
    values: &SolveVariableEvaluatedValues,
) -> Result<(), SolveVariableCatalogError> {
    if let Some(start) = values.start.as_deref() {
        let (runtime, base) = match storage.base {
            SolveStorageCoordinate::Y(index) => (construction.initial_y, index),
            SolveStorageCoordinate::P(index) => (construction.parameters, index),
        };
        let runtime_start = checked_runtime_range(identity, runtime, base, storage.scalar_count)?;
        if !same_float_bits(start, runtime_start) {
            return Err(SolveVariableCatalogError::RuntimeStartMismatch {
                name: identity.name.clone(),
                span: identity.provenance,
            });
        }
    }

    // The catalog retains whether `nominal` was stated so FMI rendering can
    // omit the defaulted attribute, while the solver vector necessarily holds
    // an effective value for every Y lane. Correlate both representations here
    // once: absence is the Modelica effective value 1.0, never permission for
    // an independently chosen solver scale baseline.
    if let SolveStorageCoordinate::Y(index) = storage.base {
        let runtime_nominal = checked_runtime_range(
            identity,
            construction.solver_nominals,
            index,
            storage.scalar_count,
        )?;
        let agrees = match values.nominal.as_deref() {
            Some(nominal) => same_float_bits(nominal, runtime_nominal),
            None => runtime_nominal
                .iter()
                .all(|nominal| nominal.to_bits() == 1.0f64.to_bits()),
        };
        if !agrees {
            return Err(SolveVariableCatalogError::RuntimeNominalMismatch {
                name: identity.name.clone(),
                span: identity.provenance,
            });
        }
    }
    Ok(())
}

fn checked_runtime_range<'values>(
    identity: &SolveVariableSource,
    values: &'values [f64],
    base: usize,
    count: usize,
) -> Result<&'values [f64], SolveVariableCatalogError> {
    let end = base
        .checked_add(count)
        .ok_or(SolveVariableCatalogError::RuntimeRange {
            name: identity.name.clone(),
            span: identity.provenance,
        })?;
    values
        .get(base..end)
        .ok_or(SolveVariableCatalogError::RuntimeRange {
            name: identity.name.clone(),
            span: identity.provenance,
        })
}

fn same_float_bits(lhs: &[f64], rhs: &[f64]) -> bool {
    lhs.len() == rhs.len()
        && lhs
            .iter()
            .zip(rhs)
            .all(|(lhs, rhs)| lhs.to_bits() == rhs.to_bits())
}

fn check_source(
    identity: &SolveVariableSource,
    expected: usize,
) -> Result<(), SolveVariableCatalogError> {
    if identity.provenance.is_dummy() {
        return Err(SolveVariableCatalogError::MissingProvenance);
    }
    if identity.name.is_empty() || identity.scalar_names.iter().any(String::is_empty) {
        return Err(SolveVariableCatalogError::EmptyName {
            span: identity.provenance,
        });
    }
    let scalar_count = identity
        .dimensions
        .iter()
        .try_fold(1usize, |count, extent| count.checked_mul(*extent as usize))
        .ok_or(SolveVariableCatalogError::ShapeOverflow {
            span: identity.provenance,
        })?;
    if scalar_count != expected || identity.scalar_names.len() != expected {
        return Err(SolveVariableCatalogError::ScalarCount {
            name: identity.name.clone(),
            actual: identity.scalar_names.len(),
            shape: scalar_count,
            expected,
            span: identity.provenance,
        });
    }
    Ok(())
}

fn check_solver_storage_names(
    problem: &SolveProblem,
    source: &SolveVariableSource,
    storage: SolveVariableStorageRun,
) -> Result<(), SolveVariableCatalogError> {
    let SolveStorageCoordinate::Y(index) = storage.base else {
        return Ok(());
    };
    let end = index.checked_add(storage.scalar_count).ok_or_else(|| {
        SolveVariableCatalogError::StorageNameMismatch {
            name: source.name.clone(),
            span: source.provenance,
        }
    })?;
    let Some(storage_names) = problem.solve_layout.solver_maps.names.get(index..end) else {
        return Err(SolveVariableCatalogError::StorageNameMismatch {
            name: source.name.clone(),
            span: source.provenance,
        });
    };
    if storage_names != source.scalar_names.as_ref() {
        return Err(SolveVariableCatalogError::StorageNameMismatch {
            name: source.name.clone(),
            span: source.provenance,
        });
    }
    Ok(())
}

fn check_values(
    identity: &SolveVariableSource,
    kind: SolveVariableValueKind,
    values: &SolveVariableEvaluatedValues,
) -> Result<(), SolveVariableCatalogError> {
    if kind == SolveVariableValueKind::String {
        for (attribute, values) in value_attributes(values) {
            if values.is_some() {
                return Err(SolveVariableCatalogError::NumericStringAttribute {
                    name: identity.name.clone(),
                    attribute,
                    span: identity.provenance,
                });
            }
        }
        return Ok(());
    }
    let expected = identity.scalar_names.len();
    check_value_count(identity, "start", values.start.as_deref(), Some(expected))?;
    check_value_count(identity, "minimum", values.minimum.as_deref(), None)?;
    check_value_count(identity, "maximum", values.maximum.as_deref(), None)?;
    check_value_count(identity, "nominal", values.nominal.as_deref(), None)?;
    for (attribute, values) in value_attributes(values) {
        if let Some(values) = values {
            check_value_domain(identity, kind, attribute, values)?;
        }
    }
    Ok(())
}

fn check_attributes(
    identity: &SolveVariableSource,
    declaration: SolveVariableDeclaration,
    attributes: &SolveVariableSourceAttributes,
) -> Result<SolveStateInitialization, SolveVariableCatalogError> {
    if attributes.tunable != (attributes.variability == SolveVariableVariability::Tunable) {
        return Err(SolveVariableCatalogError::TunableVariability {
            name: identity.name.clone(),
            span: identity.provenance,
        });
    }
    let role = declaration.role();
    if attributes.causality == SolveVariableCausality::Independent {
        return Err(SolveVariableCatalogError::IndependentStorage {
            name: identity.name.clone(),
            role,
            span: identity.provenance,
        });
    }
    let input_pair = role == SolveVariableStorageRole::ExternalInput
        && attributes.causality == SolveVariableCausality::Input;
    if (role == SolveVariableStorageRole::ExternalInput
        || attributes.causality == SolveVariableCausality::Input)
        && !input_pair
    {
        return Err(SolveVariableCatalogError::InputCausality {
            name: identity.name.clone(),
            role,
            causality: attributes.causality,
            span: identity.provenance,
        });
    }
    // `fixed` is total from DAE construction onward, so a state's
    // initialization strength is a projection, never a recovery from absence.
    Ok(match (role, attributes.fixed) {
        (SolveVariableStorageRole::State, Fixity::Fixed) => SolveStateInitialization::Exact,
        (SolveVariableStorageRole::State, Fixity::Free) => SolveStateInitialization::Approximate,
        (_, _) => SolveStateInitialization::NotState,
    })
}

fn value_attributes(values: &SolveVariableEvaluatedValues) -> [(&'static str, Option<&[f64]>); 4] {
    [
        ("start", values.start.as_deref()),
        ("minimum", values.minimum.as_deref()),
        ("maximum", values.maximum.as_deref()),
        ("nominal", values.nominal.as_deref()),
    ]
}

fn check_value_domain(
    identity: &SolveVariableSource,
    kind: SolveVariableValueKind,
    attribute: &'static str,
    values: &[f64],
) -> Result<(), SolveVariableCatalogError> {
    for (scalar, value) in values.iter().copied().enumerate() {
        let valid_kind = match kind {
            SolveVariableValueKind::Real => value.is_finite(),
            SolveVariableValueKind::Integer => value.is_finite() && value.fract() == 0.0,
            SolveVariableValueKind::Boolean => value == 0.0 || value == 1.0,
            SolveVariableValueKind::Enumeration => {
                value.is_finite() && value.fract() == 0.0 && value >= 1.0
            }
            SolveVariableValueKind::String => false,
        };
        let valid_nominal = attribute != "nominal" || value > 0.0;
        if !valid_kind || !valid_nominal {
            return Err(SolveVariableCatalogError::InvalidNumericValue {
                name: identity.name.clone(),
                attribute,
                scalar,
                kind,
                span: identity.provenance,
            });
        }
    }
    Ok(())
}

fn check_value_count(
    identity: &SolveVariableSource,
    attribute: &'static str,
    values: Option<&[f64]>,
    required: Option<usize>,
) -> Result<(), SolveVariableCatalogError> {
    let expected = identity.scalar_names.len();
    let actual = values.map(<[f64]>::len);
    if actual == required || (required.is_none() && actual.is_none()) {
        return Ok(());
    }
    if required.is_none() && actual == Some(expected) {
        return Ok(());
    }
    Err(SolveVariableCatalogError::AttributeCount {
        name: identity.name.clone(),
        attribute,
        actual,
        expected: required.or(Some(expected)),
        span: identity.provenance,
    })
}

#[derive(Clone, Debug, PartialEq, thiserror::Error)]
pub enum SolveVariableCatalogError {
    #[error("Solve variable catalog contains entry {index} beyond the dense declaration catalog")]
    TooManyEntries { index: usize, span: Span },
    #[error("Solve variable catalog contains {actual} entries; expected {expected}")]
    Incomplete { actual: usize, expected: usize },
    #[error("Solve variable identity exceeds u32")]
    IdentityOverflow,
    #[error("Solve variable identity has no source or generated provenance")]
    MissingProvenance,
    #[error("shape arithmetic overflows for Solve variable at {span:?}")]
    ShapeOverflow { span: Span },
    #[error(
        "Solve variable `{name}` shape contains {shape} scalars and names contain {actual}; expected {expected}"
    )]
    ScalarCount {
        name: String,
        actual: usize,
        shape: usize,
        expected: usize,
        span: Span,
    },
    #[error("source occurrence {occurrence:?} is assigned to more than one Solve variable")]
    DuplicateSourceOccurrence {
        occurrence: SourceOccurrenceId,
        span: Span,
    },
    #[error("Solve variable `{name}` has a duplicate presentation label")]
    DuplicateName { name: String, span: Span },
    #[error("Solve variable scalar `{name}` has a duplicate presentation label")]
    DuplicateScalarName { name: String, span: Span },
    #[error("Solve variable identity contains an empty declaration or scalar name")]
    EmptyName { span: Span },
    #[error("Solve variable `{name}` has inconsistent tunable and variability attributes")]
    TunableVariability { name: String, span: Span },
    #[error("Solve variable `{name}` has incompatible role {role:?} and causality {causality:?}")]
    InputCausality {
        name: String,
        role: SolveVariableStorageRole,
        causality: SolveVariableCausality,
        span: Span,
    },
    #[error("Solve variable `{name}` cannot represent independent causality in role {role:?}")]
    IndependentStorage {
        name: String,
        role: SolveVariableStorageRole,
        span: Span,
    },
    #[error("Solve variable `{name}` runtime storage range overflows or is out of bounds")]
    RuntimeRange { name: String, span: Span },
    #[error("Solve variable `{name}` start values differ from the final runtime slots")]
    RuntimeStartMismatch { name: String, span: Span },
    #[error("Solve variable `{name}` nominal values differ from the final solver slots")]
    RuntimeNominalMismatch { name: String, span: Span },
    #[error("Solve variable `{name}` scalar names differ from its canonical solver storage names")]
    StorageNameMismatch { name: String, span: Span },
    #[error(
        "Solve variable `{name}` attribute `{attribute}` has {actual:?} scalars; expected {expected:?}"
    )]
    AttributeCount {
        name: String,
        attribute: &'static str,
        actual: Option<usize>,
        expected: Option<usize>,
        span: Span,
    },
    #[error("String Solve variable `{name}` cannot carry numeric `{attribute}` values")]
    NumericStringAttribute {
        name: String,
        attribute: &'static str,
        span: Span,
    },
    #[error("Solve variable `{name}` has invalid {kind:?} `{attribute}` value at scalar {scalar}")]
    InvalidNumericValue {
        name: String,
        attribute: &'static str,
        scalar: usize,
        kind: SolveVariableValueKind,
        span: Span,
    },
}

impl SolveVariableCatalogError {
    #[must_use]
    pub const fn span(&self) -> Option<Span> {
        match self {
            Self::TooManyEntries { span, .. }
            | Self::ShapeOverflow { span }
            | Self::ScalarCount { span, .. }
            | Self::DuplicateSourceOccurrence { span, .. }
            | Self::DuplicateName { span, .. }
            | Self::DuplicateScalarName { span, .. }
            | Self::EmptyName { span }
            | Self::TunableVariability { span, .. }
            | Self::InputCausality { span, .. }
            | Self::IndependentStorage { span, .. }
            | Self::RuntimeRange { span, .. }
            | Self::RuntimeStartMismatch { span, .. }
            | Self::RuntimeNominalMismatch { span, .. }
            | Self::StorageNameMismatch { span, .. }
            | Self::AttributeCount { span, .. }
            | Self::NumericStringAttribute { span, .. }
            | Self::InvalidNumericValue { span, .. } => Some(*span),
            Self::Incomplete { .. } | Self::IdentityOverflow | Self::MissingProvenance => None,
        }
    }
}
