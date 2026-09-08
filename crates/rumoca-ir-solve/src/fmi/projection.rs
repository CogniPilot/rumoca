//! Checked event-free Binary64 interface projections for FMI 2 and FMI 3.

use super::write_modes::{Fmi2WriteModeBits, Fmi3WriteModeBits};
use super::{
    Fmi2WriteModes, Fmi3WriteModes, FmiCausality, FmiInitial, FmiMetadata, FmiStorageRun,
    FmiVariability, FmiVariable,
};
use crate::{SolveModel, SolveRealFormat, SolveVariableId, SolveVariableValueKind};
use rumoca_core::Span;
use serde::Serialize;
use std::collections::BTreeSet;

macro_rules! scalar_accessors {
    () => {
        #[must_use]
        pub const fn source_id(&self) -> SolveVariableId {
            self.source_id
        }
        #[must_use]
        pub const fn scalar_index(&self) -> usize {
            self.scalar_index
        }
        #[must_use]
        pub fn name(&self) -> &str {
            &self.name
        }
        #[must_use]
        pub const fn value_reference(&self) -> u32 {
            self.value_reference
        }
        #[must_use]
        pub const fn model_index(&self) -> u32 {
            self.model_index
        }
        #[must_use]
        pub const fn storage(&self) -> FmiStorageRun {
            self.storage
        }
        #[must_use]
        pub const fn derivative(&self) -> Option<FmiDerivativeLink> {
            self.derivative
        }
        #[must_use]
        pub const fn model_structure(&self) -> FmiModelStructureMembership {
            self.model_structure
        }
        #[must_use]
        pub const fn initial(&self) -> Option<FmiInitial> {
            self.initial
        }
        #[must_use]
        pub const fn write_modes(&self) -> Fmi2WriteModes {
            self.write_modes
        }
        #[must_use]
        pub const fn causality(&self) -> FmiCausality {
            self.causality
        }
        #[must_use]
        pub const fn variability(&self) -> FmiVariability {
            self.variability
        }
        #[must_use]
        pub const fn runtime_start(&self) -> f64 {
            self.runtime_start
        }
        #[must_use]
        pub const fn start(&self) -> Option<f64> {
            self.start
        }
        #[must_use]
        pub const fn minimum(&self) -> Option<f64> {
            self.minimum
        }
        #[must_use]
        pub const fn maximum(&self) -> Option<f64> {
            self.maximum
        }
        #[must_use]
        pub const fn nominal(&self) -> Option<f64> {
            self.nominal
        }
        #[must_use]
        pub fn unit(&self) -> Option<&str> {
            self.unit.as_deref()
        }
        #[must_use]
        pub fn description(&self) -> Option<&str> {
            self.description.as_deref()
        }
    };
}

macro_rules! derivative_accessors {
    () => {
        #[must_use]
        pub const fn source_id(&self) -> SolveVariableId {
            self.source_id
        }
        #[must_use]
        pub fn name(&self) -> &str {
            &self.name
        }
        #[must_use]
        pub const fn link(&self) -> FmiDerivativeLink {
            self.link
        }
        #[must_use]
        pub const fn model_index(&self) -> u32 {
            self.model_index
        }
        #[must_use]
        pub const fn storage(&self) -> FmiDerivativeStorageRange {
            self.storage
        }
        #[must_use]
        pub const fn model_structure(&self) -> FmiModelStructureMembership {
            self.model_structure
        }
        #[must_use]
        pub const fn initial(&self) -> FmiInitial {
            self.initial
        }
        #[must_use]
        pub const fn causality(&self) -> FmiCausality {
            self.causality
        }
        #[must_use]
        pub const fn variability(&self) -> FmiVariability {
            self.variability
        }
    };
}

macro_rules! projection_accessors {
    ($variable:ty, $derivative:ty) => {
        #[must_use]
        pub fn variables(&self) -> &[$variable] {
            &self.variables
        }
        #[must_use]
        pub fn derivatives(&self) -> &[$derivative] {
            &self.derivatives
        }
        #[must_use]
        pub fn unit_definitions(&self) -> &[FmiUnitDefinition] {
            &self.unit_definitions
        }
        #[must_use]
        pub fn output_model_indices(&self) -> &[u32] {
            &self.output_model_indices
        }
        #[must_use]
        pub fn derivative_model_indices(&self) -> &[u32] {
            &self.derivative_model_indices
        }
        #[must_use]
        pub fn initial_unknown_model_indices(&self) -> &[u32] {
            &self.initial_unknown_model_indices
        }
    };
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize)]
pub struct FmiModelStructureMembership {
    output: bool,
    continuous_state_derivative: bool,
    initial_unknown: bool,
}

impl FmiModelStructureMembership {
    /// Whether this entry appears in the ModelStructure output list.
    #[must_use]
    pub const fn output(self) -> bool {
        self.output
    }

    /// Whether this entry appears in the ModelStructure continuous-state
    /// derivative list.
    #[must_use]
    pub const fn continuous_state_derivative(self) -> bool {
        self.continuous_state_derivative
    }

    /// Whether this entry appears in the ModelStructure initial-unknown list.
    #[must_use]
    pub const fn initial_unknown(self) -> bool {
        self.initial_unknown
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize)]
pub struct FmiDerivativeLink {
    state_value_reference: u32,
    derivative_value_reference: u32,
    state_model_index: u32,
    derivative_model_index: u32,
}

impl FmiDerivativeLink {
    #[must_use]
    pub const fn state_value_reference(self) -> u32 {
        self.state_value_reference
    }

    #[must_use]
    pub const fn derivative_value_reference(self) -> u32 {
        self.derivative_value_reference
    }

    #[must_use]
    pub const fn state_model_index(self) -> u32 {
        self.state_model_index
    }

    #[must_use]
    pub const fn derivative_model_index(self) -> u32 {
        self.derivative_model_index
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize)]
pub struct FmiDerivativeStorageRange {
    base: usize,
    scalar_count: usize,
}

impl FmiDerivativeStorageRange {
    #[must_use]
    pub const fn base(self) -> usize {
        self.base
    }

    #[must_use]
    pub const fn scalar_count(self) -> usize {
        self.scalar_count
    }
}

/// One canonical unit definition already selected for FMI XML rendering.
///
/// An absent seconds exponent is an honest name-only FMI definition: Solve IR
/// does not yet own checked Modelica unit algebra from which another BaseUnit
/// could be derived. The renderer is never allowed to infer one from `name`.
#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub struct FmiUnitDefinition {
    name: String,
    base_unit_seconds_exponent: Option<i8>,
}

impl FmiUnitDefinition {
    #[must_use]
    pub fn name(&self) -> &str {
        &self.name
    }

    #[must_use]
    pub const fn base_unit_seconds_exponent(&self) -> Option<i8> {
        self.base_unit_seconds_exponent
    }
}

#[derive(Debug, Serialize)]
pub struct Fmi2ScalarVariable {
    #[serde(skip)]
    source_id: SolveVariableId,
    scalar_index: usize,
    name: String,
    value_reference: u32,
    model_index: u32,
    storage: FmiStorageRun,
    derivative: Option<FmiDerivativeLink>,
    continuous_state_storage: Option<FmiDerivativeStorageRange>,
    model_structure: FmiModelStructureMembership,
    initial: Option<FmiInitial>,
    write_modes: Fmi2WriteModes,
    causality: FmiCausality,
    variability: FmiVariability,
    runtime_start: f64,
    start: Option<f64>,
    minimum: Option<f64>,
    maximum: Option<f64>,
    nominal: Option<f64>,
    unit: Option<String>,
    description: Option<String>,
}

impl Fmi2ScalarVariable {
    scalar_accessors!();
}

#[derive(Debug, Serialize)]
pub struct Fmi2DerivativeVariable {
    #[serde(skip)]
    source_id: SolveVariableId,
    name: String,
    state_scalar_index: usize,
    link: FmiDerivativeLink,
    model_index: u32,
    storage: FmiDerivativeStorageRange,
    model_structure: FmiModelStructureMembership,
    initial: FmiInitial,
    causality: FmiCausality,
    variability: FmiVariability,
}

impl Fmi2DerivativeVariable {
    derivative_accessors!();

    #[must_use]
    pub const fn state_scalar_index(&self) -> usize {
        self.state_scalar_index
    }
}

#[derive(Debug, Serialize)]
pub struct Fmi2Projection {
    write_mode_bits: Fmi2WriteModeBits,
    variables: Box<[Fmi2ScalarVariable]>,
    derivatives: Box<[Fmi2DerivativeVariable]>,
    unit_definitions: Box<[FmiUnitDefinition]>,
    output_model_indices: Box<[u32]>,
    derivative_model_indices: Box<[u32]>,
    initial_unknown_model_indices: Box<[u32]>,
    continuous_state_scalar_count: usize,
}

impl Fmi2Projection {
    projection_accessors!(Fmi2ScalarVariable, Fmi2DerivativeVariable);

    #[must_use]
    pub const fn continuous_state_scalar_count(&self) -> usize {
        self.continuous_state_scalar_count
    }
}

#[derive(Debug, Serialize)]
pub struct Fmi3TensorVariable {
    #[serde(skip)]
    source_id: SolveVariableId,
    name: String,
    dimensions: Box<[u32]>,
    value_reference: u32,
    model_index: u32,
    storage: FmiStorageRun,
    derivative: Option<FmiDerivativeLink>,
    continuous_state_storage: Option<FmiDerivativeStorageRange>,
    model_structure: FmiModelStructureMembership,
    initial: Option<FmiInitial>,
    write_modes: Fmi3WriteModes,
    causality: FmiCausality,
    variability: FmiVariability,
    runtime_start: Box<[f64]>,
    start: Option<Box<[f64]>>,
    minimum: Option<Box<[f64]>>,
    maximum: Option<Box<[f64]>>,
    nominal: Option<Box<[f64]>>,
    unit: Option<String>,
    description: Option<String>,
}

impl Fmi3TensorVariable {
    #[must_use]
    pub const fn source_id(&self) -> SolveVariableId {
        self.source_id
    }
    #[must_use]
    pub fn name(&self) -> &str {
        &self.name
    }
    #[must_use]
    pub fn dimensions(&self) -> &[u32] {
        &self.dimensions
    }
    #[must_use]
    pub const fn value_reference(&self) -> u32 {
        self.value_reference
    }
    #[must_use]
    pub const fn model_index(&self) -> u32 {
        self.model_index
    }
    #[must_use]
    pub const fn storage(&self) -> FmiStorageRun {
        self.storage
    }
    #[must_use]
    pub const fn derivative(&self) -> Option<FmiDerivativeLink> {
        self.derivative
    }
    #[must_use]
    pub const fn model_structure(&self) -> FmiModelStructureMembership {
        self.model_structure
    }
    #[must_use]
    pub const fn initial(&self) -> Option<FmiInitial> {
        self.initial
    }
    #[must_use]
    pub const fn write_modes(&self) -> Fmi3WriteModes {
        self.write_modes
    }
    #[must_use]
    pub const fn causality(&self) -> FmiCausality {
        self.causality
    }
    #[must_use]
    pub const fn variability(&self) -> FmiVariability {
        self.variability
    }
    #[must_use]
    pub fn runtime_start(&self) -> &[f64] {
        &self.runtime_start
    }
    #[must_use]
    pub fn start(&self) -> Option<&[f64]> {
        self.start.as_deref()
    }
    #[must_use]
    pub fn minimum(&self) -> Option<&[f64]> {
        self.minimum.as_deref()
    }
    #[must_use]
    pub fn maximum(&self) -> Option<&[f64]> {
        self.maximum.as_deref()
    }
    #[must_use]
    pub fn nominal(&self) -> Option<&[f64]> {
        self.nominal.as_deref()
    }
    #[must_use]
    pub fn unit(&self) -> Option<&str> {
        self.unit.as_deref()
    }
    #[must_use]
    pub fn description(&self) -> Option<&str> {
        self.description.as_deref()
    }
}

#[derive(Debug, Serialize)]
pub struct Fmi3DerivativeVariable {
    #[serde(skip)]
    source_id: SolveVariableId,
    name: String,
    dimensions: Box<[u32]>,
    link: FmiDerivativeLink,
    model_index: u32,
    storage: FmiDerivativeStorageRange,
    model_structure: FmiModelStructureMembership,
    initial: FmiInitial,
    causality: FmiCausality,
    variability: FmiVariability,
}

impl Fmi3DerivativeVariable {
    derivative_accessors!();

    #[must_use]
    pub fn dimensions(&self) -> &[u32] {
        &self.dimensions
    }
}

#[derive(Debug, Serialize)]
pub struct Fmi3Projection {
    write_mode_bits: Fmi3WriteModeBits,
    variables: Box<[Fmi3TensorVariable]>,
    derivatives: Box<[Fmi3DerivativeVariable]>,
    unit_definitions: Box<[FmiUnitDefinition]>,
    output_value_references: Box<[u32]>,
    derivative_value_references: Box<[u32]>,
    initial_unknown_value_references: Box<[u32]>,
    continuous_state_scalar_count: usize,
}

impl Fmi3Projection {
    #[must_use]
    pub fn variables(&self) -> &[Fmi3TensorVariable] {
        &self.variables
    }
    #[must_use]
    pub fn derivatives(&self) -> &[Fmi3DerivativeVariable] {
        &self.derivatives
    }
    #[must_use]
    pub fn unit_definitions(&self) -> &[FmiUnitDefinition] {
        &self.unit_definitions
    }
    #[must_use]
    pub fn output_value_references(&self) -> &[u32] {
        &self.output_value_references
    }
    #[must_use]
    pub fn derivative_value_references(&self) -> &[u32] {
        &self.derivative_value_references
    }
    #[must_use]
    pub fn initial_unknown_value_references(&self) -> &[u32] {
        &self.initial_unknown_value_references
    }
    #[must_use]
    pub const fn continuous_state_scalar_count(&self) -> usize {
        self.continuous_state_scalar_count
    }
}

#[derive(Debug)]
pub(super) struct FmiVersionProjections {
    pub(super) fmi2: Fmi2Projection,
    pub(super) fmi3: Fmi3Projection,
}

impl FmiVersionProjections {
    pub(super) fn construct(
        metadata: &FmiMetadata,
        model: &SolveModel,
    ) -> Result<Self, FmiProjectionError> {
        let real_format = model.pure_calls().arithmetic().real_format();
        if real_format != SolveRealFormat::Binary64 {
            return Err(FmiProjectionError::UnsupportedRealFormat {
                actual: real_format,
            });
        }
        for variable in metadata.variables() {
            require_binary64_variable(variable)?;
        }
        let unit_definitions = construct_unit_definitions(metadata.variables())?;
        Ok(Self {
            fmi2: construct_fmi2(metadata.variables(), model, unit_definitions.clone())?,
            fmi3: construct_fmi3(metadata.variables(), model, unit_definitions)?,
        })
    }
}

fn construct_unit_definitions(
    variables: &[FmiVariable],
) -> Result<Box<[FmiUnitDefinition]>, FmiProjectionError> {
    let mut names = BTreeSet::from(["s"]);
    for variable in variables {
        let Some(unit) = variable.unit() else {
            continue;
        };
        if !valid_unit_name(unit) {
            return Err(FmiProjectionError::InvalidUnitName {
                variable: variable.name().to_string(),
                unit: unit.to_string(),
                span: variable.declaration(),
            });
        }
        names.insert(unit);
    }
    Ok(names
        .into_iter()
        .map(|name| FmiUnitDefinition {
            name: name.to_string(),
            base_unit_seconds_exponent: (name == "s").then_some(1),
        })
        .collect::<Vec<_>>()
        .into_boxed_slice())
}

fn valid_unit_name(unit: &str) -> bool {
    !unit.is_empty()
        && unit.trim() == unit
        && unit.chars().all(|character| {
            matches!(
                character,
                '\u{20}'..='\u{d7ff}' | '\u{e000}'..='\u{fffd}' | '\u{10000}'..='\u{10ffff}'
            )
        })
}

fn require_binary64_variable(variable: &FmiVariable) -> Result<(), FmiProjectionError> {
    if variable.value_kind() != SolveVariableValueKind::Real {
        return Err(FmiProjectionError::UnsupportedValueKind {
            variable: variable.name().to_string(),
            actual: variable.value_kind(),
            span: variable.declaration(),
        });
    }
    let storage = variable
        .storage()
        .ok_or_else(|| unsupported_backing(variable))?;
    if storage.scalar_count() == 0
        || variable.scalar_names().is_empty()
        || variable.dimensions().contains(&0)
    {
        return Err(FmiProjectionError::ZeroExtentVariable {
            variable: variable.name().to_string(),
            span: variable.declaration(),
        });
    }
    let missing_required_start =
        variable.start().is_none() && variable.initial() != Some(FmiInitial::Calculated);
    if variable.source_id().is_none() || missing_required_start {
        return Err(FmiProjectionError::UnsupportedBacking {
            variable: variable.name().to_string(),
            span: variable.declaration(),
        });
    }
    Ok(())
}

fn construct_fmi2(
    variables: &[FmiVariable],
    model: &SolveModel,
    unit_definitions: Box<[FmiUnitDefinition]>,
) -> Result<Fmi2Projection, FmiProjectionError> {
    let source_scalar_count = variables.iter().try_fold(0usize, |count, variable| {
        checked_add(count, variable.scalar_names().len())
    })?;
    let state_scalar_count = variables.iter().try_fold(0usize, |count, variable| {
        let add = if variable.role() == Some(crate::SolveVariableStorageRole::State) {
            variable.scalar_names().len()
        } else {
            0
        };
        checked_add(count, add)
    })?;
    let derivative_vr_base = checked_vr(source_scalar_count)?;
    let derivative_model_base = checked_model_index(source_scalar_count)?;
    let mut projection =
        Fmi2Builder::new(source_scalar_count, state_scalar_count, unit_definitions);
    for variable in variables {
        projection.issue(
            variable,
            runtime_start(model, variable)?,
            derivative_vr_base,
            derivative_model_base,
        )?;
    }
    projection.finish()
}

struct Fmi2Builder {
    variables: Vec<Fmi2ScalarVariable>,
    derivatives: Vec<Fmi2DerivativeVariable>,
    unit_definitions: Box<[FmiUnitDefinition]>,
    output_model_indices: Vec<u32>,
    derivative_model_indices: Vec<u32>,
    initial_unknown_model_indices: Vec<u32>,
    state_scalar_ordinal: usize,
}

#[derive(Clone, Copy)]
struct Fmi2SourceScalar<'a> {
    source_id: SolveVariableId,
    scalar: usize,
    name: &'a str,
    value_reference: u32,
    model_index: u32,
}

#[derive(Clone, Copy)]
struct FmiDerivativeBases {
    value_reference: u32,
    model_index: u32,
}

impl Fmi2Builder {
    fn new(
        variable_count: usize,
        derivative_count: usize,
        unit_definitions: Box<[FmiUnitDefinition]>,
    ) -> Self {
        Self {
            variables: Vec::with_capacity(variable_count),
            derivatives: Vec::with_capacity(derivative_count),
            unit_definitions,
            output_model_indices: Vec::new(),
            derivative_model_indices: Vec::with_capacity(derivative_count),
            initial_unknown_model_indices: Vec::with_capacity(derivative_count),
            state_scalar_ordinal: 0,
        }
    }

    fn issue(
        &mut self,
        variable: &FmiVariable,
        runtime_start: &[f64],
        derivative_vr_base: u32,
        derivative_model_base: u32,
    ) -> Result<(), FmiProjectionError> {
        let storage = variable
            .storage()
            .ok_or_else(|| unsupported_backing(variable))?;
        let source_id = variable
            .source_id()
            .ok_or_else(|| unsupported_backing(variable))?;
        let derivative_bases = FmiDerivativeBases {
            value_reference: derivative_vr_base,
            model_index: derivative_model_base,
        };
        for (scalar, name) in variable.scalar_names().iter().enumerate() {
            let value_reference = checked_vr(self.variables.len())?;
            let model_index = checked_model_index(self.variables.len())?;
            let scalar_storage = scalar_storage(storage, scalar)?;
            let continuous_state_base = self.state_scalar_ordinal;
            let derivative = self.issue_fmi2_derivative(
                variable,
                Fmi2SourceScalar {
                    source_id,
                    scalar,
                    name,
                    value_reference,
                    model_index,
                },
                derivative_bases,
            )?;
            let continuous_state_storage = derivative.map(|_| FmiDerivativeStorageRange {
                base: continuous_state_base,
                scalar_count: 1,
            });
            let structure = source_structure(variable);
            if structure.output {
                self.output_model_indices.push(model_index);
            }
            if structure.initial_unknown {
                self.initial_unknown_model_indices.push(model_index);
            }
            self.variables.push(Fmi2ScalarVariable {
                source_id,
                scalar_index: scalar,
                name: name.clone(),
                value_reference,
                model_index,
                storage: scalar_storage,
                derivative,
                continuous_state_storage,
                model_structure: structure,
                initial: variable.initial(),
                write_modes: Fmi2WriteModes::of(variable.write_policy()),
                causality: variable.causality(),
                variability: variable.variability(),
                runtime_start: scalar_required_value(
                    variable,
                    "runtime_start",
                    runtime_start,
                    scalar,
                )?,
                start: scalar_attribute(variable, "start", variable.start(), scalar)?,
                minimum: scalar_attribute(variable, "minimum", variable.minimum(), scalar)?,
                maximum: scalar_attribute(variable, "maximum", variable.maximum(), scalar)?,
                nominal: scalar_attribute(variable, "nominal", variable.nominal(), scalar)?,
                unit: variable.unit().map(str::to_string),
                description: variable.description().map(str::to_string),
            });
        }
        Ok(())
    }

    fn issue_fmi2_derivative(
        &mut self,
        variable: &FmiVariable,
        source: Fmi2SourceScalar<'_>,
        derivative_bases: FmiDerivativeBases,
    ) -> Result<Option<FmiDerivativeLink>, FmiProjectionError> {
        if variable.role() != Some(crate::SolveVariableStorageRole::State) {
            return Ok(None);
        }
        let ordinal = self.state_scalar_ordinal;
        self.state_scalar_ordinal = checked_add(ordinal, 1)?;
        let derivative_value_reference =
            checked_u32_add(derivative_bases.value_reference, ordinal)?;
        let model_index = checked_u32_add(derivative_bases.model_index, ordinal)?;
        let link = FmiDerivativeLink {
            state_value_reference: source.value_reference,
            derivative_value_reference,
            state_model_index: source.model_index,
            derivative_model_index: model_index,
        };
        let structure = derivative_structure();
        self.derivative_model_indices.push(model_index);
        self.initial_unknown_model_indices.push(model_index);
        self.derivatives.push(Fmi2DerivativeVariable {
            source_id: source.source_id,
            name: format!("der({})", source.name),
            state_scalar_index: source.scalar,
            link,
            model_index,
            storage: FmiDerivativeStorageRange {
                base: ordinal,
                scalar_count: 1,
            },
            model_structure: structure,
            initial: FmiInitial::Calculated,
            causality: FmiCausality::Local,
            variability: FmiVariability::Continuous,
        });
        Ok(Some(link))
    }

    fn finish(mut self) -> Result<Fmi2Projection, FmiProjectionError> {
        self.initial_unknown_model_indices.sort_unstable();
        let continuous_state_scalar_count = self.derivatives.len();
        Ok(Fmi2Projection {
            write_mode_bits: Fmi2WriteModeBits,
            variables: self.variables.into_boxed_slice(),
            derivatives: self.derivatives.into_boxed_slice(),
            unit_definitions: self.unit_definitions,
            output_model_indices: self.output_model_indices.into_boxed_slice(),
            derivative_model_indices: self.derivative_model_indices.into_boxed_slice(),
            initial_unknown_model_indices: self.initial_unknown_model_indices.into_boxed_slice(),
            continuous_state_scalar_count,
        })
    }
}

fn construct_fmi3(
    variables: &[FmiVariable],
    model: &SolveModel,
    unit_definitions: Box<[FmiUnitDefinition]>,
) -> Result<Fmi3Projection, FmiProjectionError> {
    let state_count = variables
        .iter()
        .filter(|variable| variable.role() == Some(crate::SolveVariableStorageRole::State))
        .count();
    let derivative_vr_base = checked_vr(variables.len())?;
    let derivative_model_base = checked_model_index(variables.len())?;
    let mut result = Fmi3Builder::new(variables.len(), state_count, unit_definitions);
    for variable in variables {
        result.issue(
            variable,
            runtime_start(model, variable)?,
            derivative_vr_base,
            derivative_model_base,
        )?;
    }
    Ok(result.finish())
}

struct Fmi3Builder {
    variables: Vec<Fmi3TensorVariable>,
    derivatives: Vec<Fmi3DerivativeVariable>,
    unit_definitions: Box<[FmiUnitDefinition]>,
    output_value_references: Vec<u32>,
    derivative_value_references: Vec<u32>,
    initial_unknown_value_references: Vec<u32>,
    derivative_scalar_base: usize,
}

impl Fmi3Builder {
    fn new(
        variable_count: usize,
        derivative_count: usize,
        unit_definitions: Box<[FmiUnitDefinition]>,
    ) -> Self {
        Self {
            variables: Vec::with_capacity(variable_count),
            derivatives: Vec::with_capacity(derivative_count),
            unit_definitions,
            output_value_references: Vec::new(),
            derivative_value_references: Vec::with_capacity(derivative_count),
            initial_unknown_value_references: Vec::with_capacity(derivative_count),
            derivative_scalar_base: 0,
        }
    }

    fn issue(
        &mut self,
        variable: &FmiVariable,
        runtime_start: &[f64],
        derivative_vr_base: u32,
        derivative_model_base: u32,
    ) -> Result<(), FmiProjectionError> {
        let index = self.variables.len();
        let value_reference = checked_vr(index)?;
        let model_index = checked_model_index(index)?;
        let continuous_state_base = self.derivative_scalar_base;
        let derivative = self.issue_derivative(
            variable,
            value_reference,
            model_index,
            derivative_vr_base,
            derivative_model_base,
        )?;
        let storage = variable
            .storage()
            .ok_or_else(|| unsupported_backing(variable))?;
        let continuous_state_storage = derivative.map(|_| FmiDerivativeStorageRange {
            base: continuous_state_base,
            scalar_count: storage.scalar_count(),
        });
        let structure = source_structure(variable);
        if structure.output {
            self.output_value_references.push(value_reference);
        }
        if structure.initial_unknown {
            self.initial_unknown_value_references.push(value_reference);
        }
        self.variables.push(Fmi3TensorVariable {
            source_id: variable
                .source_id()
                .ok_or_else(|| unsupported_backing(variable))?,
            name: variable.name().to_string(),
            dimensions: variable.dimensions().to_vec().into_boxed_slice(),
            value_reference,
            model_index,
            storage,
            derivative,
            continuous_state_storage,
            model_structure: structure,
            initial: variable.initial(),
            write_modes: Fmi3WriteModes::of(variable.write_policy()),
            causality: variable.causality(),
            variability: variable.variability(),
            runtime_start: runtime_start.to_vec().into_boxed_slice(),
            start: variable
                .start()
                .map(<[f64]>::to_vec)
                .map(Vec::into_boxed_slice),
            minimum: variable
                .minimum()
                .map(<[f64]>::to_vec)
                .map(Vec::into_boxed_slice),
            maximum: variable
                .maximum()
                .map(<[f64]>::to_vec)
                .map(Vec::into_boxed_slice),
            nominal: variable
                .nominal()
                .map(<[f64]>::to_vec)
                .map(Vec::into_boxed_slice),
            unit: variable.unit().map(str::to_string),
            description: variable.description().map(str::to_string),
        });
        Ok(())
    }

    fn issue_derivative(
        &mut self,
        variable: &FmiVariable,
        state_value_reference: u32,
        state_model_index: u32,
        derivative_vr_base: u32,
        derivative_model_base: u32,
    ) -> Result<Option<FmiDerivativeLink>, FmiProjectionError> {
        if variable.role() != Some(crate::SolveVariableStorageRole::State) {
            return Ok(None);
        }
        let ordinal = self.derivatives.len();
        let derivative_value_reference = checked_u32_add(derivative_vr_base, ordinal)?;
        let model_index = checked_u32_add(derivative_model_base, ordinal)?;
        let scalar_count = variable
            .storage()
            .ok_or_else(|| unsupported_backing(variable))?
            .scalar_count();
        let base = self.derivative_scalar_base;
        self.derivative_scalar_base = checked_add(base, scalar_count)?;
        let link = FmiDerivativeLink {
            state_value_reference,
            derivative_value_reference,
            state_model_index,
            derivative_model_index: model_index,
        };
        self.derivative_value_references
            .push(derivative_value_reference);
        self.initial_unknown_value_references
            .push(derivative_value_reference);
        self.derivatives.push(Fmi3DerivativeVariable {
            source_id: variable
                .source_id()
                .ok_or_else(|| unsupported_backing(variable))?,
            name: format!("der({})", variable.name()),
            dimensions: variable.dimensions().to_vec().into_boxed_slice(),
            link,
            model_index,
            storage: FmiDerivativeStorageRange { base, scalar_count },
            model_structure: derivative_structure(),
            initial: FmiInitial::Calculated,
            causality: FmiCausality::Local,
            variability: FmiVariability::Continuous,
        });
        Ok(Some(link))
    }

    fn finish(mut self) -> Fmi3Projection {
        self.initial_unknown_value_references.sort_unstable();
        Fmi3Projection {
            write_mode_bits: Fmi3WriteModeBits,
            variables: self.variables.into_boxed_slice(),
            derivatives: self.derivatives.into_boxed_slice(),
            unit_definitions: self.unit_definitions,
            output_value_references: self.output_value_references.into_boxed_slice(),
            derivative_value_references: self.derivative_value_references.into_boxed_slice(),
            initial_unknown_value_references: self
                .initial_unknown_value_references
                .into_boxed_slice(),
            continuous_state_scalar_count: self.derivative_scalar_base,
        }
    }
}

fn scalar_storage(
    storage: FmiStorageRun,
    scalar: usize,
) -> Result<FmiStorageRun, FmiProjectionError> {
    let base = checked_add(storage.base(), scalar)?;
    Ok(FmiStorageRun {
        column: storage.column(),
        base,
        scalar_count: 1,
    })
}

fn scalar_attribute(
    variable: &FmiVariable,
    attribute: &'static str,
    values: Option<&[f64]>,
    scalar: usize,
) -> Result<Option<f64>, FmiProjectionError> {
    let Some(values) = values else {
        return Ok(None);
    };
    values
        .get(scalar)
        .copied()
        .map(Some)
        .ok_or_else(|| FmiProjectionError::AttributeRange {
            variable: variable.name().to_string(),
            attribute,
            scalar,
            span: variable.declaration(),
        })
}

fn scalar_required_value(
    variable: &FmiVariable,
    attribute: &'static str,
    values: &[f64],
    scalar: usize,
) -> Result<f64, FmiProjectionError> {
    values
        .get(scalar)
        .copied()
        .ok_or_else(|| FmiProjectionError::AttributeRange {
            variable: variable.name().to_string(),
            attribute,
            scalar,
            span: variable.declaration(),
        })
}

fn runtime_start<'model>(
    model: &'model SolveModel,
    variable: &FmiVariable,
) -> Result<&'model [f64], FmiProjectionError> {
    let storage = variable
        .storage()
        .ok_or_else(|| unsupported_backing(variable))?;
    let column = match storage.column() {
        super::SolveStorageColumn::Y => model.initial_y(),
        super::SolveStorageColumn::P => model.parameters(),
    };
    let end = checked_add(storage.base(), storage.scalar_count())?;
    column
        .get(storage.base()..end)
        .ok_or_else(|| FmiProjectionError::RuntimeRange {
            variable: variable.name().to_string(),
            span: variable.declaration(),
        })
}

fn source_structure(variable: &FmiVariable) -> FmiModelStructureMembership {
    let initial_unknown = matches!(
        variable.initial(),
        Some(FmiInitial::Approx | FmiInitial::Calculated)
    ) && (matches!(
        variable.causality(),
        super::FmiCausality::Output | super::FmiCausality::CalculatedParameter
    ) || variable.role()
        == Some(crate::SolveVariableStorageRole::State));
    FmiModelStructureMembership {
        output: variable.causality() == super::FmiCausality::Output,
        continuous_state_derivative: false,
        initial_unknown,
    }
}

const fn derivative_structure() -> FmiModelStructureMembership {
    FmiModelStructureMembership {
        output: false,
        continuous_state_derivative: true,
        initial_unknown: true,
    }
}

fn unsupported_backing(variable: &FmiVariable) -> FmiProjectionError {
    FmiProjectionError::UnsupportedBacking {
        variable: variable.name().to_string(),
        span: variable.declaration(),
    }
}

fn checked_add(lhs: usize, rhs: usize) -> Result<usize, FmiProjectionError> {
    lhs.checked_add(rhs)
        .ok_or(FmiProjectionError::ArithmeticOverflow)
}

/// The synthetic time variable owns value reference zero, so source variables
/// begin at the immediately following checked value reference.
const FIRST_SOURCE_VALUE_REFERENCE: u32 = 1;

/// FMI 2 ModelStructure indices are one-based and the synthetic time variable
/// is model variable one, so source variables begin at model index two.
const FIRST_SOURCE_MODEL_INDEX: u32 = 2;

fn checked_vr(zero_based: usize) -> Result<u32, FmiProjectionError> {
    u32::try_from(zero_based)
        .ok()
        .and_then(|value| value.checked_add(FIRST_SOURCE_VALUE_REFERENCE))
        .ok_or(FmiProjectionError::ArithmeticOverflow)
}

fn checked_model_index(zero_based_source: usize) -> Result<u32, FmiProjectionError> {
    u32::try_from(zero_based_source)
        .ok()
        .and_then(|value| value.checked_add(FIRST_SOURCE_MODEL_INDEX))
        .ok_or(FmiProjectionError::ArithmeticOverflow)
}

fn checked_u32_add(base: u32, offset: usize) -> Result<u32, FmiProjectionError> {
    let offset = u32::try_from(offset).map_err(|_| FmiProjectionError::ArithmeticOverflow)?;
    base.checked_add(offset)
        .ok_or(FmiProjectionError::ArithmeticOverflow)
}

#[derive(Debug, Clone, PartialEq, thiserror::Error)]
pub enum FmiProjectionError {
    #[error("FMI Float64 projection requires Binary64 Solve arithmetic, not {actual:?}")]
    UnsupportedRealFormat { actual: SolveRealFormat },
    #[error("FMI Float64 projection cannot represent `{variable}` with value kind {actual:?}")]
    UnsupportedValueKind {
        variable: String,
        actual: SolveVariableValueKind,
        span: Option<Span>,
    },
    #[error("FMI Float64 projection cannot represent the backing of `{variable}`")]
    UnsupportedBacking {
        variable: String,
        span: Option<Span>,
    },
    #[error("FMI projection cannot render invalid unit name {unit:?} on `{variable}`")]
    InvalidUnitName {
        variable: String,
        unit: String,
        span: Option<Span>,
    },
    #[error("FMI Float64 projection rejects zero-scalar exposed variable `{variable}`")]
    ZeroExtentVariable {
        variable: String,
        span: Option<Span>,
    },
    #[error("FMI Float64 projection runtime storage for `{variable}` is out of range")]
    RuntimeRange {
        variable: String,
        span: Option<Span>,
    },
    #[error("FMI projection attribute `{attribute}` for `{variable}` has no scalar {scalar}")]
    AttributeRange {
        variable: String,
        attribute: &'static str,
        scalar: usize,
        span: Option<Span>,
    },
    #[error("FMI projection arithmetic overflows")]
    ArithmeticOverflow,
}

impl FmiProjectionError {
    #[must_use]
    pub const fn span(&self) -> Option<Span> {
        match self {
            Self::UnsupportedValueKind { span, .. }
            | Self::UnsupportedBacking { span, .. }
            | Self::InvalidUnitName { span, .. }
            | Self::ZeroExtentVariable { span, .. }
            | Self::RuntimeRange { span, .. }
            | Self::AttributeRange { span, .. } => *span,
            Self::UnsupportedRealFormat { .. } | Self::ArithmeticOverflow => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn value_reference_and_storage_arithmetic_fail_closed_on_overflow() {
        assert_eq!(
            checked_vr(usize::MAX),
            Err(FmiProjectionError::ArithmeticOverflow)
        );
        assert_eq!(
            checked_model_index(usize::MAX),
            Err(FmiProjectionError::ArithmeticOverflow)
        );
        assert_eq!(
            checked_add(usize::MAX, 1),
            Err(FmiProjectionError::ArithmeticOverflow)
        );
        assert_eq!(
            checked_u32_add(u32::MAX, 1),
            Err(FmiProjectionError::ArithmeticOverflow)
        );
    }
}
